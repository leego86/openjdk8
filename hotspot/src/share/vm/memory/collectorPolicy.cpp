/*
 * Copyright (c) 2001, 2015, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 *
 */

#include "precompiled.hpp"
#include "gc_implementation/shared/adaptiveSizePolicy.hpp"
#include "gc_implementation/shared/gcPolicyCounters.hpp"
#include "gc_implementation/shared/vmGCOperations.hpp"
#include "memory/cardTableRS.hpp"
#include "memory/collectorPolicy.hpp"
#include "memory/gcLocker.inline.hpp"
#include "memory/genCollectedHeap.hpp"
#include "memory/generationSpec.hpp"
#include "memory/space.hpp"
#include "memory/universe.hpp"
#include "runtime/arguments.hpp"
#include "runtime/globals_extension.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/java.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/vmThread.hpp"
#include "utilities/macros.hpp"
#if INCLUDE_ALL_GCS
#include "gc_implementation/concurrentMarkSweep/cmsAdaptiveSizePolicy.hpp"
#include "gc_implementation/concurrentMarkSweep/cmsGCAdaptivePolicyCounters.hpp"
#endif // INCLUDE_ALL_GCS

// CollectorPolicy methods.

CollectorPolicy::CollectorPolicy() :
    _space_alignment(0),
    _heap_alignment(0),
    _initial_heap_byte_size(InitialHeapSize),
    _max_heap_byte_size(MaxHeapSize),
    _min_heap_byte_size(Arguments::min_heap_size()),
    _max_heap_size_cmdline(false),
    _size_policy(NULL),
    _should_clear_all_soft_refs(false),
    _all_soft_refs_clear(false)
{}

#ifdef ASSERT
void CollectorPolicy::assert_flags() {
  assert(InitialHeapSize <= MaxHeapSize, "Ergonomics decided on incompatible initial and maximum heap sizes");
  assert(InitialHeapSize % _heap_alignment == 0, "InitialHeapSize alignment");
  assert(MaxHeapSize % _heap_alignment == 0, "MaxHeapSize alignment");
}

void CollectorPolicy::assert_size_info() {
  assert(InitialHeapSize == _initial_heap_byte_size, "Discrepancy between InitialHeapSize flag and local storage");
  assert(MaxHeapSize == _max_heap_byte_size, "Discrepancy between MaxHeapSize flag and local storage");
  assert(_max_heap_byte_size >= _min_heap_byte_size, "Ergonomics decided on incompatible minimum and maximum heap sizes");
  assert(_initial_heap_byte_size >= _min_heap_byte_size, "Ergonomics decided on incompatible initial and minimum heap sizes");
  assert(_max_heap_byte_size >= _initial_heap_byte_size, "Ergonomics decided on incompatible initial and maximum heap sizes");
  assert(_min_heap_byte_size % _heap_alignment == 0, "min_heap_byte_size alignment");
  assert(_initial_heap_byte_size % _heap_alignment == 0, "initial_heap_byte_size alignment");
  assert(_max_heap_byte_size % _heap_alignment == 0, "max_heap_byte_size alignment");
}
#endif // ASSERT

void CollectorPolicy::initialize_flags() {
  assert(_space_alignment != 0, "Space alignment not set up properly");
  assert(_heap_alignment != 0, "Heap alignment not set up properly");
  assert(_heap_alignment >= _space_alignment,
         err_msg("heap_alignment: " SIZE_FORMAT " less than space_alignment: " SIZE_FORMAT,
                 _heap_alignment, _space_alignment));
  assert(_heap_alignment % _space_alignment == 0,
         err_msg("heap_alignment: " SIZE_FORMAT " not aligned by space_alignment: " SIZE_FORMAT,
                 _heap_alignment, _space_alignment));

  if (FLAG_IS_CMDLINE(MaxHeapSize)) {
    if (FLAG_IS_CMDLINE(InitialHeapSize) && InitialHeapSize > MaxHeapSize) {
      vm_exit_during_initialization("Initial heap size set to a larger value than the maximum heap size");
    }
    if (_min_heap_byte_size != 0 && MaxHeapSize < _min_heap_byte_size) {
      vm_exit_during_initialization("Incompatible minimum and maximum heap sizes specified");
    }
    _max_heap_size_cmdline = true;
  }

  // Check heap parameter properties
  if (InitialHeapSize < M) {
    vm_exit_during_initialization("Too small initial heap");
  }
  if (_min_heap_byte_size < M) {
    vm_exit_during_initialization("Too small minimum heap");
  }

  // User inputs from -Xmx and -Xms must be aligned
  _min_heap_byte_size = align_size_up(_min_heap_byte_size, _heap_alignment);
  uintx aligned_initial_heap_size = align_size_up(InitialHeapSize, _heap_alignment);
  uintx aligned_max_heap_size = align_size_up(MaxHeapSize, _heap_alignment);

  // Write back to flags if the values changed
  if (aligned_initial_heap_size != InitialHeapSize) {
    FLAG_SET_ERGO(uintx, InitialHeapSize, aligned_initial_heap_size);
  }
  if (aligned_max_heap_size != MaxHeapSize) {
    FLAG_SET_ERGO(uintx, MaxHeapSize, aligned_max_heap_size);
  }

  if (FLAG_IS_CMDLINE(InitialHeapSize) && _min_heap_byte_size != 0 &&
      InitialHeapSize < _min_heap_byte_size) {
    vm_exit_during_initialization("Incompatible minimum and initial heap sizes specified");
  }
  if (!FLAG_IS_DEFAULT(InitialHeapSize) && InitialHeapSize > MaxHeapSize) {
    FLAG_SET_ERGO(uintx, MaxHeapSize, InitialHeapSize);
  } else if (!FLAG_IS_DEFAULT(MaxHeapSize) && InitialHeapSize > MaxHeapSize) {
    FLAG_SET_ERGO(uintx, InitialHeapSize, MaxHeapSize);
    if (InitialHeapSize < _min_heap_byte_size) {
      _min_heap_byte_size = InitialHeapSize;
    }
  }

  _initial_heap_byte_size = InitialHeapSize;
  _max_heap_byte_size = MaxHeapSize;

  FLAG_SET_ERGO(uintx, MinHeapDeltaBytes, align_size_up(MinHeapDeltaBytes, _space_alignment));

  DEBUG_ONLY(CollectorPolicy::assert_flags();)
}

void CollectorPolicy::initialize_size_info() {
  if (PrintGCDetails && Verbose) {
    gclog_or_tty->print_cr("Minimum heap " SIZE_FORMAT "  Initial heap "
      SIZE_FORMAT "  Maximum heap " SIZE_FORMAT,
      _min_heap_byte_size, _initial_heap_byte_size, _max_heap_byte_size);
  }

  DEBUG_ONLY(CollectorPolicy::assert_size_info();)
}

bool CollectorPolicy::use_should_clear_all_soft_refs(bool v) {
  bool result = _should_clear_all_soft_refs;
  set_should_clear_all_soft_refs(false);
  return result;
}

GenRemSet* CollectorPolicy::create_rem_set(MemRegion whole_heap,
                                           int max_covered_regions) {
  return new CardTableRS(whole_heap, max_covered_regions);
}

void CollectorPolicy::cleared_all_soft_refs() {
  // If near gc overhear limit, continue to clear SoftRefs.  
  // 如果接近gc偷听？限制，续集清理软引用对象
  // SoftRefs may have been cleared in the last collection but if the gc overhear
  // 软引用可能在最后一次收集中被清除，但如果继续接近gc偷听限制，软引用依然会被清除
  // limit continues to be near, SoftRefs should still be cleared.
  if (size_policy() != NULL) {
    _should_clear_all_soft_refs = size_policy()->gc_overhead_limit_near();
  }
  _all_soft_refs_clear = true;
}

size_t CollectorPolicy::compute_heap_alignment() {
  // The card marking array and the offset arrays for old generations are
  // committed in os pages as well. 
  // 老年代的卡片标记集合和偏移量集合也在os页面提交
  // Make sure they are entirely full (to avoid partial page problems), e.g. 
  // 确保他们都满了（避免局部页面问题）
  // if 512 bytes heap corresponds to 1 byte entry and the os page size is 4096, 
  // 如果512字节堆对应一字节条目，os页大小为4096
  // the maximum heap size should be 512*4096 = 2MB aligned.
  // 最大堆大小应该是512×4096 = 2MB对齐

  // There is only the GenRemSet in Hotspot and only the GenRemSet::CardTable is supported.
  // Hotspot虚拟机只支持GenRemSet，并且只支持GenRemSet::CardTable
  // Requirements of any new remembered set implementations must be added here.
  // 对于任何新记忆集实现的要求都必须添加到这里
  size_t alignment = GenRemSet::max_alignment_constraint(GenRemSet::CardTable);

  if (UseLargePages) {
      // In presence of large pages we have to make sure that our
      // alignment is large page aware.
      // 在出现大页时，我们必须确保对齐是大页感知的
      alignment = lcm(os::large_page_size(), alignment);
  }

  return alignment;
}

// GenCollectorPolicy methods.

GenCollectorPolicy::GenCollectorPolicy() :
    _min_gen0_size(0),
    _initial_gen0_size(0),
    _max_gen0_size(0),
    _gen_alignment(0),
    _generations(NULL)
{}

size_t GenCollectorPolicy::scale_by_NewRatio_aligned(size_t base_size) {
  return align_size_down_bounded(base_size / (NewRatio + 1), _gen_alignment);
}

size_t GenCollectorPolicy::bound_minus_alignment(size_t desired_size,
                                                 size_t maximum_size) {
  size_t max_minus = maximum_size - _gen_alignment;
  return desired_size < max_minus ? desired_size : max_minus;
}


void GenCollectorPolicy::initialize_size_policy(size_t init_eden_size,
                                                size_t init_promo_size,
                                                size_t init_survivor_size) {
  const double max_gc_pause_sec = ((double) MaxGCPauseMillis) / 1000.0;
  _size_policy = new AdaptiveSizePolicy(init_eden_size,
                                        init_promo_size,
                                        init_survivor_size,
                                        max_gc_pause_sec,
                                        GCTimeRatio);
}

size_t GenCollectorPolicy::young_gen_size_lower_bound() {
  // The young generation must be aligned and have room for eden + two survivors
  // 新生代必须对齐，而且预留eden+2个survivors区的空间
  return align_size_up(3 * _space_alignment, _gen_alignment);
}

#ifdef ASSERT
void GenCollectorPolicy::assert_flags() {
  CollectorPolicy::assert_flags();
  assert(NewSize >= _min_gen0_size, "Ergonomics decided on a too small young gen size");
  assert(NewSize <= MaxNewSize, "Ergonomics decided on incompatible initial and maximum young gen sizes");
  assert(FLAG_IS_DEFAULT(MaxNewSize) || MaxNewSize < MaxHeapSize, "Ergonomics decided on incompatible maximum young gen and heap sizes");
  assert(NewSize % _gen_alignment == 0, "NewSize alignment");
  assert(FLAG_IS_DEFAULT(MaxNewSize) || MaxNewSize % _gen_alignment == 0, "MaxNewSize alignment");
}

void TwoGenerationCollectorPolicy::assert_flags() {
  GenCollectorPolicy::assert_flags();
  assert(OldSize + NewSize <= MaxHeapSize, "Ergonomics decided on incompatible generation and heap sizes");
  assert(OldSize % _gen_alignment == 0, "OldSize alignment");
}

void GenCollectorPolicy::assert_size_info() {
  CollectorPolicy::assert_size_info();
  // GenCollectorPolicy::initialize_size_info may update the MaxNewSize
  assert(MaxNewSize < MaxHeapSize, "Ergonomics decided on incompatible maximum young and heap sizes");
  assert(NewSize == _initial_gen0_size, "Discrepancy between NewSize flag and local storage");
  assert(MaxNewSize == _max_gen0_size, "Discrepancy between MaxNewSize flag and local storage");
  assert(_min_gen0_size <= _initial_gen0_size, "Ergonomics decided on incompatible minimum and initial young gen sizes");
  assert(_initial_gen0_size <= _max_gen0_size, "Ergonomics decided on incompatible initial and maximum young gen sizes");
  assert(_min_gen0_size % _gen_alignment == 0, "_min_gen0_size alignment");
  assert(_initial_gen0_size % _gen_alignment == 0, "_initial_gen0_size alignment");
  assert(_max_gen0_size % _gen_alignment == 0, "_max_gen0_size alignment");
}

void TwoGenerationCollectorPolicy::assert_size_info() {
  GenCollectorPolicy::assert_size_info();
  assert(OldSize == _initial_gen1_size, "Discrepancy between OldSize flag and local storage");
  assert(_min_gen1_size <= _initial_gen1_size, "Ergonomics decided on incompatible minimum and initial old gen sizes");
  assert(_initial_gen1_size <= _max_gen1_size, "Ergonomics decided on incompatible initial and maximum old gen sizes");
  assert(_max_gen1_size % _gen_alignment == 0, "_max_gen1_size alignment");
  assert(_initial_gen1_size % _gen_alignment == 0, "_initial_gen1_size alignment");
  assert(_max_heap_byte_size <= (_max_gen0_size + _max_gen1_size), "Total maximum heap sizes must be sum of generation maximum sizes");
}
#endif // ASSERT

void GenCollectorPolicy::initialize_flags() {
  CollectorPolicy::initialize_flags();

  assert(_gen_alignment != 0, "Generation alignment not set up properly");
  assert(_heap_alignment >= _gen_alignment,
         err_msg("heap_alignment: " SIZE_FORMAT " less than gen_alignment: " SIZE_FORMAT,
                 _heap_alignment, _gen_alignment));
  assert(_gen_alignment % _space_alignment == 0,
         err_msg("gen_alignment: " SIZE_FORMAT " not aligned by space_alignment: " SIZE_FORMAT,
                 _gen_alignment, _space_alignment));
  assert(_heap_alignment % _gen_alignment == 0,
         err_msg("heap_alignment: " SIZE_FORMAT " not aligned by gen_alignment: " SIZE_FORMAT,
                 _heap_alignment, _gen_alignment));

  // All generational heaps have a youngest gen; handle those flags here
  // 所有的堆世代都有一个最年轻的世代，处理这些标记
  // Make sure the heap is large enough for two generations
  // 确保堆足够打，可以容纳两个世代
  uintx smallest_new_size = young_gen_size_lower_bound();
  uintx smallest_heap_size = align_size_up(smallest_new_size + align_size_up(_space_alignment, _gen_alignment),
                                           _heap_alignment);
  if (MaxHeapSize < smallest_heap_size) {
    FLAG_SET_ERGO(uintx, MaxHeapSize, smallest_heap_size);
    _max_heap_byte_size = MaxHeapSize;
  }
  // If needed, synchronize _min_heap_byte size and _initial_heap_byte_size
  // 如果需要，同步设置最小堆字节数和初始化堆字节数
  if (_min_heap_byte_size < smallest_heap_size) {
    _min_heap_byte_size = smallest_heap_size;
    if (InitialHeapSize < _min_heap_byte_size) {
      FLAG_SET_ERGO(uintx, InitialHeapSize, smallest_heap_size);
      _initial_heap_byte_size = smallest_heap_size;
    }
  }

  // Now take the actual NewSize into account. 
  // 现在考虑实际的NewSize
  // We will silently increase NewSize if the user specified a smaller or unaligned value.
  // 如果用户设置一个更小或者没有对齐的值，则将无声的增加NewSize
  smallest_new_size = MAX2(smallest_new_size, (uintx)align_size_down(NewSize, _gen_alignment));
  if (smallest_new_size != NewSize) {
    // Do not use FLAG_SET_ERGO to update NewSize here, 
    // 不要使用FLAG_SET_ERGO修改NewSize
    // since this will override if NewSize was set on the command line or not. 
    // 因为无论是否在命令行上设置NewSize，这会被覆盖它
    // This information is needed later when setting the initial and minimum young generation size.
    // 稍后在设置初始和最小年轻代大小时需要此信息
    NewSize = smallest_new_size;
  }
  _initial_gen0_size = NewSize;

  if (!FLAG_IS_DEFAULT(MaxNewSize)) {
    uintx min_new_size = MAX2(_gen_alignment, _min_gen0_size);

    if (MaxNewSize >= MaxHeapSize) {
      // Make sure there is room for an old generation
      //确保老年代的空间
      uintx smaller_max_new_size = MaxHeapSize - _gen_alignment;
      if (FLAG_IS_CMDLINE(MaxNewSize)) {
        warning("MaxNewSize (" SIZE_FORMAT "k) is equal to or greater than the entire "
                "heap (" SIZE_FORMAT "k).  A new max generation size of " SIZE_FORMAT "k will be used.",
                MaxNewSize/K, MaxHeapSize/K, smaller_max_new_size/K);
      }
      FLAG_SET_ERGO(uintx, MaxNewSize, smaller_max_new_size);
      if (NewSize > MaxNewSize) {
        FLAG_SET_ERGO(uintx, NewSize, MaxNewSize);
        _initial_gen0_size = NewSize;
      }
    } else if (MaxNewSize < min_new_size) {
      FLAG_SET_ERGO(uintx, MaxNewSize, min_new_size);
    } else if (!is_size_aligned(MaxNewSize, _gen_alignment)) {
      FLAG_SET_ERGO(uintx, MaxNewSize, align_size_down(MaxNewSize, _gen_alignment));
    }
    _max_gen0_size = MaxNewSize;
  }

  if (NewSize > MaxNewSize) {
    // At this point this should only happen if the user specifies a large NewSize and/or
    // a small (but not too small) MaxNewSize.
    // 此时，只有在用户设置一个大的新生代和/或一个小的（但不是特别小的）最大新生代时才会发生这种情况
    if (FLAG_IS_CMDLINE(MaxNewSize)) {
      warning("NewSize (" SIZE_FORMAT "k) is greater than the MaxNewSize (" SIZE_FORMAT "k). "
              "A new max generation size of " SIZE_FORMAT "k will be used.",
              NewSize/K, MaxNewSize/K, NewSize/K);
    }
    FLAG_SET_ERGO(uintx, MaxNewSize, NewSize);
    _max_gen0_size = MaxNewSize;
  }

  if (SurvivorRatio < 1 || NewRatio < 1) {
    vm_exit_during_initialization("Invalid young gen ratio specified");
  }

  DEBUG_ONLY(GenCollectorPolicy::assert_flags();)
}

void TwoGenerationCollectorPolicy::initialize_flags() {
  GenCollectorPolicy::initialize_flags();

  if (!is_size_aligned(OldSize, _gen_alignment)) {
    FLAG_SET_ERGO(uintx, OldSize, align_size_down(OldSize, _gen_alignment));
  }

  if (FLAG_IS_CMDLINE(OldSize) && FLAG_IS_DEFAULT(MaxHeapSize)) {
    // NewRatio will be used later to set the young generation size so we use
    // it to calculate how big the heap should be based on the requested OldSize
    // and NewRatio.
    // 新生代比例将稍后在设置新生代大小时被使用，所以我们使用它来根据老年代和新生代比例计算堆需要多大
    assert(NewRatio > 0, "NewRatio should have been set up earlier");
    size_t calculated_heapsize = (OldSize / NewRatio) * (NewRatio + 1);

    calculated_heapsize = align_size_up(calculated_heapsize, _heap_alignment);
    FLAG_SET_ERGO(uintx, MaxHeapSize, calculated_heapsize);
    _max_heap_byte_size = MaxHeapSize;
    FLAG_SET_ERGO(uintx, InitialHeapSize, calculated_heapsize);
    _initial_heap_byte_size = InitialHeapSize;
  }

  // adjust max heap size if necessary
  // 如果需要，调整最大堆大小
  if (NewSize + OldSize > MaxHeapSize) {
    if (_max_heap_size_cmdline) {
      // somebody set a maximum heap size with the intention that we should not
      // exceed it. Adjust New/OldSize as necessary.
      // 有人设置最大堆大小，目的是不让我们超过它。根据需要调整新/老大小
      uintx calculated_size = NewSize + OldSize;
      double shrink_factor = (double) MaxHeapSize / calculated_size;
      uintx smaller_new_size = align_size_down((uintx)(NewSize * shrink_factor), _gen_alignment);
      FLAG_SET_ERGO(uintx, NewSize, MAX2(young_gen_size_lower_bound(), smaller_new_size));
      _initial_gen0_size = NewSize;

      // OldSize is already aligned because above we aligned MaxHeapSize to _heap_alignment, 
      // 老年代大小已经被对齐，因为上面我们把MaxHeapSize对齐到了_heap_alignment
      // and we just made sure that NewSize is aligned to _gen_alignment. 
      // 并且我们刚刚确保NewSize对齐到了_gen_alignment。
      // In initialize_flags() we verified that _heap_alignment is a multiple of _gen_alignment.
      // 在initialize_flags()中我们校验了_heap_alignment是_gen_alignment的倍数
      FLAG_SET_ERGO(uintx, OldSize, MaxHeapSize - NewSize);
    } else {
      FLAG_SET_ERGO(uintx, MaxHeapSize, align_size_up(NewSize + OldSize, _heap_alignment));
      _max_heap_byte_size = MaxHeapSize;
    }
  }

  always_do_update_barrier = UseConcMarkSweepGC;

  DEBUG_ONLY(TwoGenerationCollectorPolicy::assert_flags();)
}

// Values set on the command line win over any ergonomically set command line parameters.
// 命令行上设置的值胜过任何设计的命令行参数 
// Ergonomic choice of parameters are done before this method is called.  
// 在调用此方法之前完成参数的设计选择
// Values for command line parameters such as NewSize and MaxNewSize feed those ergonomic choices into this method.
// 诸如NewSize和MaxNewSize等命令行设置的值将这些符合设计的选择提供给这个方法
// This method makes the final generation sizings consistent with themselves and with overall heap sizings.
// 这个方法使最终生成的大小与其本身和总体堆大小保持一致
// In the absence of explicitly set command line flags, 
// policies such as the use of NewRatio are used to size the generation.
// 像使用NewRatio这样的策略，被用于设定代纪大小
void GenCollectorPolicy::initialize_size_info() {
  CollectorPolicy::initialize_size_info();

  // _space_alignment is used for alignment within a generation.
  // _space_alignment用于代内对齐
  // There is additional alignment done down stream for some collectors that sometimes causes unwanted rounding up of generations sizes.
  // 对于某些收集器，还需要在下游进行额外的对齐，这有时会导致不必要的代大小舍入

  // Determine maximum size of gen0
  // 决定gen0的最大大小

  size_t max_new_size = 0;
  if (!FLAG_IS_DEFAULT(MaxNewSize)) {
    max_new_size = MaxNewSize;
  } else {
    max_new_size = scale_by_NewRatio_aligned(_max_heap_byte_size);
    // Bound the maximum size by NewSize below (since it historically
    // 根据下面的newSize绑定最大大小（因为历史上它应该是NewSize，而且NewRatio计算可能会产生一个太小的大小）
    // would have been NewSize and because the NewRatio calculation could
    // yield a size that is too small) and bound it by MaxNewSize above.
    //并且根据上面的MaxNewSize绑定它
    // Ergonomics plays here by previously calculating the desired NewSize and MaxNewSize.
    // 预设在这里发挥作用，通过之前计算所需的NewSize和MaxNewSize
    max_new_size = MIN2(MAX2(max_new_size, NewSize), MaxNewSize);
  }
  assert(max_new_size > 0, "All paths should set max_new_size");

  // Given the maximum gen0 size, determine the initial and minimum gen0 sizes.
  // 提供最大gen0大小，决定初始化和最小gen0大小

  if (_max_heap_byte_size == _min_heap_byte_size) {
    // The maximum and minimum heap sizes are the same so
    // 最大和最小对相等
    // the generations minimum and initial must be the same as its maximum.
    // 所以代纪的最小和初始化大小必须和最大相等
    _min_gen0_size = max_new_size;
    _initial_gen0_size = max_new_size;
    _max_gen0_size = max_new_size;
  } else {
    size_t desired_new_size = 0;
    if (FLAG_IS_CMDLINE(NewSize)) {
      // If NewSize is set on the command line, we must use it as
      // 如果NewSize在命令行设置的，我们必须用它作为初始化大小
      // the initial size and it also makes sense to use it as the lower limit.
      // 并且使用它作为下限也是有意义的
      _min_gen0_size = NewSize;
      desired_new_size = NewSize;
      max_new_size = MAX2(max_new_size, NewSize);
    } else if (FLAG_IS_ERGO(NewSize)) {
      // If NewSize is set ergonomically, we should use it as a lower
      // 如果NewSize是最优设置，我们应该用它作为最低限制
      // limit, but use NewRatio to calculate the initial size.
      // 但是使用NewRatio计算初始化大小
      _min_gen0_size = NewSize;
      desired_new_size =
        MAX2(scale_by_NewRatio_aligned(_initial_heap_byte_size), NewSize);
      max_new_size = MAX2(max_new_size, NewSize);
    } else {
      // For the case where NewSize is the default, use NewRatio
      // 当NewSize是默认值时，使用NewRatio限制最小值和初始化代纪大小
      // to size the minimum and initial generation sizes.
      // Use the default NewSize as the floor for these values.  If
      // 使用默认NewSize作为这些参数的最小值
      // NewRatio is overly large, the resulting sizes can be too small.
      // 如果NewRatio过大，结果大小可能会太小
      _min_gen0_size = MAX2(scale_by_NewRatio_aligned(_min_heap_byte_size), NewSize);
      desired_new_size =
        MAX2(scale_by_NewRatio_aligned(_initial_heap_byte_size), NewSize);
    }

    assert(_min_gen0_size > 0, "Sanity check");
    _initial_gen0_size = desired_new_size;
    _max_gen0_size = max_new_size;

    // At this point the desirable initial and minimum sizes have been
    // determined without regard to the maximum sizes.
    // 此时，已经确定了理想的初始尺寸和最小尺寸，而不考虑最大尺寸
    // Bound the sizes by the corresponding overall heap sizes.
    // 根据相应的总堆大小限制这些尺寸
    _min_gen0_size = bound_minus_alignment(_min_gen0_size, _min_heap_byte_size);
    _initial_gen0_size = bound_minus_alignment(_initial_gen0_size, _initial_heap_byte_size);
    _max_gen0_size = bound_minus_alignment(_max_gen0_size, _max_heap_byte_size);

    // At this point all three sizes have been checked against the
    // maximum sizes but have not been checked for consistency
    // among the three.
    // 此时，所有三个尺寸都与最大尺寸进行了比较
    // Final check min <= initial <= max
    // 最后检测最小值<=初始值<=最大值
    _min_gen0_size = MIN2(_min_gen0_size, _max_gen0_size);
    _initial_gen0_size = MAX2(MIN2(_initial_gen0_size, _max_gen0_size), _min_gen0_size);
    _min_gen0_size = MIN2(_min_gen0_size, _initial_gen0_size);
  }

  // Write back to flags if necessary
  // 必要时回写标记
  if (NewSize != _initial_gen0_size) {
    FLAG_SET_ERGO(uintx, NewSize, _initial_gen0_size);
  }

  if (MaxNewSize != _max_gen0_size) {
    FLAG_SET_ERGO(uintx, MaxNewSize, _max_gen0_size);
  }

  if (PrintGCDetails && Verbose) {
    gclog_or_tty->print_cr("1: Minimum gen0 " SIZE_FORMAT "  Initial gen0 "
      SIZE_FORMAT "  Maximum gen0 " SIZE_FORMAT,
      _min_gen0_size, _initial_gen0_size, _max_gen0_size);
  }

  DEBUG_ONLY(GenCollectorPolicy::assert_size_info();)
}

// Call this method during the sizing of the gen1 to make
// 在第一代大小调整期间调用此方法
// adjustments to gen0 because of gen1 sizing policy.  gen0 initially has
// 以便根据第一代大小调整策略对第0代进行调整。第0代初始化调整更自由
// the most freedom in sizing because it is done before the
// 因为它在应用第1代策略前完成的
// policy for gen1 is applied.  Once gen1 policies have been applied,
// 一旦第一代策略应用完成
// there may be conflicts in the shape of the heap and this method
// 堆的形状可能会有冲突，此方法用于进行所需的调整
// is used to make the needed adjustments.  The application of the
// policies could be more sophisticated (iterative for example) but
// 这些策略的应用可能更复杂（例如迭代）但是保持它简单似乎也是一个有价值的目标
// keeping it simple also seems a worthwhile goal.
bool TwoGenerationCollectorPolicy::adjust_gen0_sizes(size_t* gen0_size_ptr,
                                                     size_t* gen1_size_ptr,
                                                     const size_t heap_size) {
  bool result = false;

  if ((*gen0_size_ptr + *gen1_size_ptr) > heap_size) {
    uintx smallest_new_size = young_gen_size_lower_bound();
    if ((heap_size < (*gen0_size_ptr + _min_gen1_size)) &&
        (heap_size >= _min_gen1_size + smallest_new_size)) {
      // Adjust gen0 down to accommodate _min_gen1_size
      //向下调整gen0以适应_min_gen1_size
      *gen0_size_ptr = align_size_down_bounded(heap_size - _min_gen1_size, _gen_alignment);
      result = true;
    } else {
      *gen1_size_ptr = align_size_down_bounded(heap_size - *gen0_size_ptr, _gen_alignment);
    }
  }
  return result;
}

// Minimum sizes of the generations may be different than
// 最小代纪尺寸可能和初始化尺寸不同
// the initial sizes.  An inconsistently is permitted here
// 这里允许总大小不一致，可以通过命令行规范OldSize和NewSize以及命令行规范-Xms显式地指定 
// in the total size that can be specified explicitly by
// command line specification of OldSize and NewSize and
// also a command line specification of -Xms.  Issue a warning
// but allow the values to pass.
// 发出警告，但允许传递值

void TwoGenerationCollectorPolicy::initialize_size_info() {
  GenCollectorPolicy::initialize_size_info();

  // At this point the minimum, initial and maximum sizes
  // of the overall heap and of gen0 have been determined.
  // 此时，整个堆和gen0的最小，初始化和最大大小已经确定
  // The maximum gen1 size can be determined from the maximum gen0
  // 最大gen1大小可以根据最大gen0和最大堆大小决定
  // and maximum heap size since no explicit flags exits
  // 因为没有设置gen1最大值的显示标志
  // for setting the gen1 maximum.
  _max_gen1_size = MAX2(_max_heap_byte_size - _max_gen0_size, _gen_alignment);

  // If no explicit command line flag has been set for the
  // 如果没显式的在命令行设置gen1大小
  // gen1 size, use what is left for gen1.
  // 使用gen1剩下的部分？
  if (!FLAG_IS_CMDLINE(OldSize)) {
    // The user has not specified any value but the ergonomics
    // 如果用户没有设置任何值，工效学可能选择一个值（这可能与总堆大小一致，也可能不一致）
    // may have chosen a value (which may or may not be consistent
    // with the overall heap size).  In either case make
    // 在两种情况下，使最小，最大和初始化大小与gen0大小和总堆大小一致
    // the minimum, maximum and initial sizes consistent
    // with the gen0 sizes and the overall heap sizes.
    _min_gen1_size = MAX2(_min_heap_byte_size - _min_gen0_size, _gen_alignment);
    _initial_gen1_size = MAX2(_initial_heap_byte_size - _initial_gen0_size, _gen_alignment);
    // _max_gen1_size has already been made consistent above
    // 上面已经将_max_gen1_size保持一致了
    FLAG_SET_ERGO(uintx, OldSize, _initial_gen1_size);
  } else {
    // It's been explicitly set on the command line.  Use the
    // 它已经在命令行显式设置了
    // OldSize and then determine the consequences.
    // 使用OldSize，然后确定结果
    _min_gen1_size = MIN2(OldSize, _min_heap_byte_size - _min_gen0_size);
    _initial_gen1_size = OldSize;

    // If the user has explicitly set an OldSize that is inconsistent
    // 如果用户显式设置了与其他命令行不一致的OldSize
    // with other command line flags, issue a warning.
    // 发出一个警告
    // The generation minimums and the overall heap mimimum should
    // 代纪最小和总堆最小值应该和一个代对齐
    // be within one generation alignment.
    if ((_min_gen1_size + _min_gen0_size + _gen_alignment) < _min_heap_byte_size) {
      warning("Inconsistency between minimum heap size and minimum "
              "generation sizes: using minimum heap = " SIZE_FORMAT,
              _min_heap_byte_size);
    }
    if (OldSize > _max_gen1_size) {
      warning("Inconsistency between maximum heap size and maximum "
              "generation sizes: using maximum heap = " SIZE_FORMAT
              " -XX:OldSize flag is being ignored",
              _max_heap_byte_size);
    }
    // If there is an inconsistency between the OldSize and the minimum and/or
    // 如果OldSize和最小值和/或gen0初始大小不一致，
    // initial size of gen0, since OldSize was explicitly set, OldSize wins.
    // 因为OldSize是明确设置，以OldSize为准
    if (adjust_gen0_sizes(&_min_gen0_size, &_min_gen1_size, _min_heap_byte_size)) {
      if (PrintGCDetails && Verbose) {
        gclog_or_tty->print_cr("2: Minimum gen0 " SIZE_FORMAT "  Initial gen0 "
              SIZE_FORMAT "  Maximum gen0 " SIZE_FORMAT,
              _min_gen0_size, _initial_gen0_size, _max_gen0_size);
      }
    }
    // Initial size
    if (adjust_gen0_sizes(&_initial_gen0_size, &_initial_gen1_size,
                          _initial_heap_byte_size)) {
      if (PrintGCDetails && Verbose) {
        gclog_or_tty->print_cr("3: Minimum gen0 " SIZE_FORMAT "  Initial gen0 "
          SIZE_FORMAT "  Maximum gen0 " SIZE_FORMAT,
          _min_gen0_size, _initial_gen0_size, _max_gen0_size);
      }
    }
  }
  // Enforce the maximum gen1 size.
  _min_gen1_size = MIN2(_min_gen1_size, _max_gen1_size);

  // Check that min gen1 <= initial gen1 <= max gen1
  _initial_gen1_size = MAX2(_initial_gen1_size, _min_gen1_size);
  _initial_gen1_size = MIN2(_initial_gen1_size, _max_gen1_size);

  // Write back to flags if necessary
  if (NewSize != _initial_gen0_size) {
    FLAG_SET_ERGO(uintx, NewSize, _initial_gen0_size);
  }

  if (MaxNewSize != _max_gen0_size) {
    FLAG_SET_ERGO(uintx, MaxNewSize, _max_gen0_size);
  }

  if (OldSize != _initial_gen1_size) {
    FLAG_SET_ERGO(uintx, OldSize, _initial_gen1_size);
  }

  if (PrintGCDetails && Verbose) {
    gclog_or_tty->print_cr("Minimum gen1 " SIZE_FORMAT "  Initial gen1 "
      SIZE_FORMAT "  Maximum gen1 " SIZE_FORMAT,
      _min_gen1_size, _initial_gen1_size, _max_gen1_size);
  }

  DEBUG_ONLY(TwoGenerationCollectorPolicy::assert_size_info();)
}

HeapWord* GenCollectorPolicy::mem_allocate_work(size_t size,
                                        bool is_tlab,
                                        bool* gc_overhead_limit_was_exceeded) {
  GenCollectedHeap *gch = GenCollectedHeap::heap();

  debug_only(gch->check_for_valid_allocation_state());
  assert(gch->no_gc_in_progress(), "Allocation during gc not allowed");

  // In general gc_overhead_limit_was_exceeded should be false so
  // 一般 gc_overhead_limit_was_exceeded应该设置为false
  // set it so here and reset it to true only if the gc time
  // 所以在这里设置它，只有在超过gc时间限制时才将其重置为true，如下所示
  // limit is being exceeded as checked below.
  *gc_overhead_limit_was_exceeded = false;

  HeapWord* result = NULL;

  // Loop until the allocation is satisified,
  // 循环，知道满足分配
  // or unsatisfied after GC.
  // 或者在GC之后不满足
  for (uint try_count = 1, gclocker_stalled_count = 0; /* return or throw */; try_count += 1) {
    HandleMark hm; // discard any handles allocated in each iteration
                   // 丢弃在每次迭代中分配的句柄

    // First allocation attempt is lock-free.
    // 第一次分配尝试是无锁的
    Generation *gen0 = gch->get_gen(0);
    assert(gen0->supports_inline_contig_alloc(),
      "Otherwise, must do alloc within heap lock");
    if (gen0->should_allocate(size, is_tlab)) {
      result = gen0->par_allocate(size, is_tlab);
      if (result != NULL) {
        assert(gch->is_in_reserved(result), "result not in heap");
        return result;
      }
    }
    uint gc_count_before;  // read inside the Heap_lock locked region 读取堆锁锁定的内部区域
    {
      MutexLocker ml(Heap_lock);
      if (PrintGC && Verbose) {
        gclog_or_tty->print_cr("TwoGenerationCollectorPolicy::mem_allocate_work:"
                      " attempting locked slow path allocation");
      }
      // Note that only large objects get a shot at being
      // allocated in later generations.
      // 注意，只有大对象才有机会在以后的代中分配
      bool first_only = ! should_try_older_generation_allocation(size);

      result = gch->attempt_allocation(size, is_tlab, first_only);
      if (result != NULL) {
        assert(gch->is_in_reserved(result), "result not in heap");
        return result;
      }

      if (GC_locker::is_active_and_needs_gc()) {
        if (is_tlab) {
          return NULL;  // Caller will retry allocating individual object 调用者将会重试分配独立对象
        }
        if (!gch->is_maximal_no_gc()) {
          // Try and expand heap to satisfy request
          // 尝试扩展堆以满足请求
          result = expand_heap_and_allocate(size, is_tlab);
          // result could be null if we are out of space
          // 如果超出空间，结果可能为null
          if (result != NULL) {
            return result;
          }
        }

        if (gclocker_stalled_count > GCLockerRetryAllocationCount) {
          return NULL; // we didn't get to do a GC and we didn't get any memory
                       // 没有发起GC，也没有任何内存
        }

        // If this thread is not in a jni critical section, we stall
        // 如果这个线程不在jni临界区
        // the requestor until the critical section has cleared and
        // 我们暂停请求者，直到临界区被清理，并且GC允许
        // GC allowed. When the critical section clears, a GC is
        // 当临界区清理时，退出临界区的最后一个线程启动GC
        // initiated by the last thread exiting the critical section; so
        // we retry the allocation sequence from the beginning of the loop,
        // 因此，我们从循环的开始重试分配序列
        // rather than causing more, now probably unnecessary, GC attempts.
        // 而不是导致更多，现在可能是不必要的，GC尝试
        JavaThread* jthr = JavaThread::current();
        if (!jthr->in_critical()) {
          MutexUnlocker mul(Heap_lock);
          // Wait for JNI critical section to be exited
          GC_locker::stall_until_clear();
          gclocker_stalled_count += 1;
          continue;
        } else {
          if (CheckJNICalls) {
            fatal("Possible deadlock due to allocating while"
                  " in jni critical section");
          }
          return NULL;
        }
      }

      // Read the gc count while the heap lock is held.
      // 当持有堆锁时读gc数
      gc_count_before = Universe::heap()->total_collections();
    }

    VM_GenCollectForAllocation op(size, is_tlab, gc_count_before);
    VMThread::execute(&op);
    if (op.prologue_succeeded()) {
      result = op.result();
      if (op.gc_locked()) {
         assert(result == NULL, "must be NULL if gc_locked() is true");
         continue;  // retry and/or stall as necessary 根据需要重试和/或暂停
      }

      // Allocation has failed and a collection
      // 分配失败并且收集完成
      // has been done.  If the gc time limit was exceeded the
      // 如果gc时间限制超过这个时间
      // this time, return NULL so that an out-of-memory
      // 如果返回NULL将会抛出内存溢出异常
      // will be thrown.  Clear gc_overhead_limit_exceeded
      // 清理gc_overhead_limit_exceeded
      // so that the overhead exceeded does not persist.
      // 这样超出的开销就不会持续存在

      const bool limit_exceeded = size_policy()->gc_overhead_limit_exceeded();
      const bool softrefs_clear = all_soft_refs_clear();

      if (limit_exceeded && softrefs_clear) {
        *gc_overhead_limit_was_exceeded = true;
        size_policy()->set_gc_overhead_limit_exceeded(false);
        if (op.result() != NULL) {
          CollectedHeap::fill_with_object(op.result(), size);
        }
        return NULL;
      }
      assert(result == NULL || gch->is_in_reserved(result),
             "result not in heap");
      return result;
    }

    // Give a warning if we seem to be looping forever.
    // 如果似乎要死循环，就给出警告
    if ((QueuedAllocationWarningCount > 0) &&
        (try_count % QueuedAllocationWarningCount == 0)) {
          warning("TwoGenerationCollectorPolicy::mem_allocate_work retries %d times \n\t"
                  " size=" SIZE_FORMAT " %s", try_count, size, is_tlab ? "(TLAB)" : "");
    }
  }
}

HeapWord* GenCollectorPolicy::expand_heap_and_allocate(size_t size,
                                                       bool   is_tlab) {
  GenCollectedHeap *gch = GenCollectedHeap::heap();
  HeapWord* result = NULL;
  for (int i = number_of_generations() - 1; i >= 0 && result == NULL; i--) {
    Generation *gen = gch->get_gen(i);
    if (gen->should_allocate(size, is_tlab)) {
      result = gen->expand_and_allocate(size, is_tlab);
    }
  }
  assert(result == NULL || gch->is_in_reserved(result), "result not in heap");
  return result;
}

HeapWord* GenCollectorPolicy::satisfy_failed_allocation(size_t size,
                                                        bool   is_tlab) {
  GenCollectedHeap *gch = GenCollectedHeap::heap();
  GCCauseSetter x(gch, GCCause::_allocation_failure);
  HeapWord* result = NULL;

  assert(size != 0, "Precondition violated");
  if (GC_locker::is_active_and_needs_gc()) {
    // GC locker is active; instead of a collection we will attempt
    // GC锁是占用的;如果有空间可以扩展，我们将尝试扩展堆，而入不是收集
    // to expand the heap, if there's room for expansion.
    if (!gch->is_maximal_no_gc()) {
      result = expand_heap_and_allocate(size, is_tlab);
    }
    return result;   // could be null if we are out of space 如果超出空间，可能为null
  } else if (!gch->incremental_collection_will_fail(false /* don't consult_young */)) {
    // Do an incremental collection.
    // 执行增量收集 
    gch->do_collection(false            /* full */,
                       false            /* clear_all_soft_refs */,
                       size             /* size */,
                       is_tlab          /* is_tlab */,
                       number_of_generations() - 1 /* max_level */);
  } else {
    if (Verbose && PrintGCDetails) {
      gclog_or_tty->print(" :: Trying full because partial may fail :: ");
    }
    // Try a full collection; see delta for bug id 6266275
    // 尝试全量收集;
    // for the original code and why this has been simplified
    // with from-space allocation criteria modified and
    // such allocation moved out of the safepoint path.
    // 请参阅delta查看原始代码的bug id 6266275，以及为什么通过修改from-space分配标准和将这种分配移出safepoint路径来简化它 
    gch->do_collection(true             /* full */,
                       false            /* clear_all_soft_refs */,
                       size             /* size */,
                       is_tlab          /* is_tlab */,
                       number_of_generations() - 1 /* max_level */);
  }

  result = gch->attempt_allocation(size, is_tlab, false /*first_only*/);

  if (result != NULL) {
    assert(gch->is_in_reserved(result), "result not in heap");
    return result;
  }

  // OK, collection failed, try expansion.
  // 收集失败，尝试扩展
  result = expand_heap_and_allocate(size, is_tlab);
  if (result != NULL) {
    return result;
  }

  // If we reach this point, we're really out of memory. Try every trick
  // 如果我们达到这个点，我们可能内存溢出。
  // we can to reclaim memory. Force collection of soft references. Force
  // 想尽一切办法回收内存。强制收集软引用。强制对堆进行完全压缩
  // a complete compaction of the heap. Any additional methods for finding
  // 任何用于寻找空闲内存的其他方法都应该在这里，尤其是当它们很昂贵的时候。
  // free memory should be here, especially if they are expensive. If this
  // attempt fails, an OOM exception will be thrown.
  //  如果此尝试失败，将抛出一个OOM异常。
  {
    UIntFlagSetting flag_change(MarkSweepAlwaysCompactCount, 1); // Make sure the heap is fully compacted

    gch->do_collection(true             /* full */,
                       true             /* clear_all_soft_refs */,
                       size             /* size */,
                       is_tlab          /* is_tlab */,
                       number_of_generations() - 1 /* max_level */);
  }

  result = gch->attempt_allocation(size, is_tlab, false /* first_only */);
  if (result != NULL) {
    assert(gch->is_in_reserved(result), "result not in heap");
    return result;
  }

  assert(!should_clear_all_soft_refs(),
    "Flag should have been handled and cleared prior to this point");

  // What else?  We might try synchronous finalization later.  If the total
  // 其他？稍后我们可能尝试同步结束
  // space available is large enough for the allocation, then a more
  // 如果可用的总空间足够大，那么比我们目前尝试的更完整的压缩阶段可能是合适的 
  // complete compaction phase than we've tried so far might be
  // appropriate.
  return NULL;
}

MetaWord* CollectorPolicy::satisfy_failed_metadata_allocation(
                                                 ClassLoaderData* loader_data,
                                                 size_t word_size,
                                                 Metaspace::MetadataType mdtype) {
  uint loop_count = 0;
  uint gc_count = 0;
  uint full_gc_count = 0;

  assert(!Heap_lock->owned_by_self(), "Should not be holding the Heap_lock");

  do {
    MetaWord* result = NULL;
    if (GC_locker::is_active_and_needs_gc()) {
      // If the GC_locker is active, just expand and allocate.
      // 如果gc锁是活动的，只需扩展和分配
      // If that does not succeed, wait if this thread is not
      // 如果没有成功，如果这个线程本身不在临界区，则等待
      // in a critical section itself.
      result =
        loader_data->metaspace_non_null()->expand_and_allocate(word_size,
                                                               mdtype);
      if (result != NULL) {
        return result;
      }
      JavaThread* jthr = JavaThread::current();
      if (!jthr->in_critical()) {
        // Wait for JNI critical section to be exited
        // 等待JNI临界区退出
        GC_locker::stall_until_clear();
        // The GC invoked by the last thread leaving the critical
        // 离开临界区的最后一个线程调用GC，将是年轻代收集，卸载类（当前）需要一个全量收集
        // section will be a young collection and a full collection
        // is (currently) needed for unloading classes so continue
        // to the next iteration to get a full GC.
        // 因此需要继续到下一个迭代获得full GC
        continue;
      } else {
        if (CheckJNICalls) {
          fatal("Possible deadlock due to allocating while"
                " in jni critical section");
        }
        return NULL;
      }
    }

    {  // Need lock to get self consistent gc_count's
    //需要锁定来获得一致的gc计数
      MutexLocker ml(Heap_lock);
      gc_count      = Universe::heap()->total_collections();
      full_gc_count = Universe::heap()->total_full_collections();
    }

    // Generate a VM operation
    // 生成虚拟机操作
    VM_CollectForMetadataAllocation op(loader_data,
                                       word_size,
                                       mdtype,
                                       gc_count,
                                       full_gc_count,
                                       GCCause::_metadata_GC_threshold);
    VMThread::execute(&op);

    // If GC was locked out, try again.  Check
    // 如果GC被锁定，重试
    // before checking success because the prologue
    // 在检查成功前进行检测，因为序言可能已经成功，而GC仍然被锁定
    // could have succeeded and the GC still have
    // been locked out.
    if (op.gc_locked()) {
      continue;
    }

    if (op.prologue_succeeded()) {
      return op.result();
    }
    loop_count++;
    if ((QueuedAllocationWarningCount > 0) &&
        (loop_count % QueuedAllocationWarningCount == 0)) {
      warning("satisfy_failed_metadata_allocation() retries %d times \n\t"
              " size=" SIZE_FORMAT, loop_count, word_size);
    }
  } while (true);  // Until a GC is done  知道GC完成
}

// Return true if any of the following is true:
// 以下任一为true则返回true
// . the allocation won't fit into the current young gen heap
//   这个分配将不适合当前的年轻代堆
// . gc locker is occupied (jni critical section)
//   gc锁被占用（jni临界区）
// . heap memory is tight -- the most recent previous collection
//   堆内存紧张  
//   was a full collection because a partial collection (would
//   最近的前一次收集是全量收集，因为部分收集（已经）失败了，而且很可能再次失败
//   have) failed and is likely to fail again
bool GenCollectorPolicy::should_try_older_generation_allocation(
        size_t word_size) const {
  GenCollectedHeap* gch = GenCollectedHeap::heap();
  size_t gen0_capacity = gch->get_gen(0)->capacity_before_gc();
  return    (word_size > heap_word_size(gen0_capacity))
         || GC_locker::is_active_and_needs_gc()
         || gch->incremental_collection_failed();
}


//
// MarkSweepPolicy methods
//

void MarkSweepPolicy::initialize_alignments() {
  _space_alignment = _gen_alignment = (uintx)Generation::GenGrain;
  _heap_alignment = compute_heap_alignment();
}

void MarkSweepPolicy::initialize_generations() {
  _generations = NEW_C_HEAP_ARRAY3(GenerationSpecPtr, number_of_generations(), mtGC, CURRENT_PC,
    AllocFailStrategy::RETURN_NULL);
  if (_generations == NULL) {
    vm_exit_during_initialization("Unable to allocate gen spec");
  }

  if (UseParNewGC) {
    _generations[0] = new GenerationSpec(Generation::ParNew, _initial_gen0_size, _max_gen0_size);
  } else {
    _generations[0] = new GenerationSpec(Generation::DefNew, _initial_gen0_size, _max_gen0_size);
  }
  _generations[1] = new GenerationSpec(Generation::MarkSweepCompact, _initial_gen1_size, _max_gen1_size);

  if (_generations[0] == NULL || _generations[1] == NULL) {
    vm_exit_during_initialization("Unable to allocate gen spec");
  }
}

void MarkSweepPolicy::initialize_gc_policy_counters() {
  // initialize the policy counters - 2 collectors, 3 generations
  // 初始化策略计数器 - 2个收集器，3个代
  if (UseParNewGC) {
    _gc_policy_counters = new GCPolicyCounters("ParNew:MSC", 2, 3);
  } else {
    _gc_policy_counters = new GCPolicyCounters("Copy:MSC", 2, 3);
  }
}

/////////////// Unit tests ///////////////

#ifndef PRODUCT
// Testing that the NewSize flag is handled correct is hard because it
// 很难测试NewSize标志是否被正确使用，因为它依赖于很多其他可配置变量
// depends on so many other configurable variables. This test only tries to
// verify that there are some basic rules for NewSize honored by the policies.
// 这个测试只尝试验证策略遵守了一些NewSize的基础规格
class TestGenCollectorPolicy {
public:
  static void test() {
    size_t flag_value;

    save_flags();

    // Set some limits that makes the math simple.
    // 设置一些限制，使运算简单
    FLAG_SET_ERGO(uintx, MaxHeapSize, 180 * M);
    FLAG_SET_ERGO(uintx, InitialHeapSize, 120 * M);
    Arguments::set_min_heap_size(40 * M);

    // If NewSize is set on the command line, it should be used
    // 如果NewSize在命令行上设置，如果小于最小堆，它将同时被最小和初始化年轻代大小使用
    // for both min and initial young size if less than min heap.
    flag_value = 20 * M;
    FLAG_SET_CMDLINE(uintx, NewSize, flag_value);
    verify_min(flag_value);
    verify_initial(flag_value);

    // If NewSize is set on command line, but is larger than the min
    // 如果NewSize在命令行上设置，但是比最小堆大
    // heap size, it should only be used for initial young size.
    // 它只被用作初始化年轻代大小
    flag_value = 80 * M;
    FLAG_SET_CMDLINE(uintx, NewSize, flag_value);
    verify_initial(flag_value);

    // If NewSize has been ergonomically set, the collector policy
    // 如果NewSize已按工学设置，收集器策略应该被用在最小，但是根据NewRatio计算初始化年轻代大小
    // should use it for min but calculate the initial young size
    // using NewRatio.
    flag_value = 20 * M;
    FLAG_SET_ERGO(uintx, NewSize, flag_value);
    verify_min(flag_value);
    verify_scaled_initial(InitialHeapSize);

    restore_flags();

  }

  static void verify_min(size_t expected) {
    MarkSweepPolicy msp;
    msp.initialize_all();

    assert(msp.min_gen0_size() <= expected, err_msg("%zu  > %zu", msp.min_gen0_size(), expected));
  }

  static void verify_initial(size_t expected) {
    MarkSweepPolicy msp;
    msp.initialize_all();

    assert(msp.initial_gen0_size() == expected, err_msg("%zu != %zu", msp.initial_gen0_size(), expected));
  }

  static void verify_scaled_initial(size_t initial_heap_size) {
    MarkSweepPolicy msp;
    msp.initialize_all();

    size_t expected = msp.scale_by_NewRatio_aligned(initial_heap_size);
    assert(msp.initial_gen0_size() == expected, err_msg("%zu != %zu", msp.initial_gen0_size(), expected));
    assert(FLAG_IS_ERGO(NewSize) && NewSize == expected,
        err_msg("NewSize should have been set ergonomically to %zu, but was %zu", expected, NewSize));
  }

private:
  static size_t original_InitialHeapSize;
  static size_t original_MaxHeapSize;
  static size_t original_MaxNewSize;
  static size_t original_MinHeapDeltaBytes;
  static size_t original_NewSize;
  static size_t original_OldSize;

  static void save_flags() {
    original_InitialHeapSize   = InitialHeapSize;
    original_MaxHeapSize       = MaxHeapSize;
    original_MaxNewSize        = MaxNewSize;
    original_MinHeapDeltaBytes = MinHeapDeltaBytes;
    original_NewSize           = NewSize;
    original_OldSize           = OldSize;
  }

  static void restore_flags() {
    InitialHeapSize   = original_InitialHeapSize;
    MaxHeapSize       = original_MaxHeapSize;
    MaxNewSize        = original_MaxNewSize;
    MinHeapDeltaBytes = original_MinHeapDeltaBytes;
    NewSize           = original_NewSize;
    OldSize           = original_OldSize;
  }
};

size_t TestGenCollectorPolicy::original_InitialHeapSize   = 0;
size_t TestGenCollectorPolicy::original_MaxHeapSize       = 0;
size_t TestGenCollectorPolicy::original_MaxNewSize        = 0;
size_t TestGenCollectorPolicy::original_MinHeapDeltaBytes = 0;
size_t TestGenCollectorPolicy::original_NewSize           = 0;
size_t TestGenCollectorPolicy::original_OldSize           = 0;

void TestNewSize_test() {
  TestGenCollectorPolicy::test();
}

#endif
