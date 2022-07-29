/*
 * Copyright (c) 2001, 2013, Oracle and/or its affiliates. All rights reserved.
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
#include "classfile/javaClasses.hpp"
#include "classfile/symbolTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "code/codeCache.hpp"
#include "code/icBuffer.hpp"
#include "gc_implementation/shared/gcHeapSummary.hpp"
#include "gc_implementation/shared/gcTimer.hpp"
#include "gc_implementation/shared/gcTrace.hpp"
#include "gc_implementation/shared/gcTraceTime.hpp"
#include "gc_interface/collectedHeap.inline.hpp"
#include "memory/genCollectedHeap.hpp"
#include "memory/genMarkSweep.hpp"
#include "memory/genOopClosures.inline.hpp"
#include "memory/generation.inline.hpp"
#include "memory/modRefBarrierSet.hpp"
#include "memory/referencePolicy.hpp"
#include "memory/space.hpp"
#include "oops/instanceRefKlass.hpp"
#include "oops/oop.inline.hpp"
#include "prims/jvmtiExport.hpp"
#include "runtime/fprofiler.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/vmThread.hpp"
#include "utilities/copy.hpp"
#include "utilities/events.hpp"

void GenMarkSweep::invoke_at_safepoint(int level, ReferenceProcessor* rp, bool clear_all_softrefs) {
  guarantee(level == 1, "We always collect both old and young.");
  assert(SafepointSynchronize::is_at_safepoint(), "must be at a safepoint");

  GenCollectedHeap* gch = GenCollectedHeap::heap();
#ifdef ASSERT
  if (gch->collector_policy()->should_clear_all_soft_refs()) {
    assert(clear_all_softrefs, "Policy should have been checked earlier");
  }
#endif

  // hook up weak ref data so it can be used during Mark-Sweep
  assert(ref_processor() == NULL, "no stomping");
  assert(rp != NULL, "should be non-NULL");
  _ref_processor = rp;
  rp->setup_policy(clear_all_softrefs);

  GCTraceTime t1(GCCauseString("Full GC", gch->gc_cause()), PrintGC && !PrintGCDetails, true, NULL, _gc_tracer->gc_id());

//检查堆概要，发送相应的gc event
  gch->trace_heap_before_gc(_gc_tracer);

  // When collecting the permanent generation Method*s may be moving,
  // 当收集永久代的时候方法可能移动
  // so we either have to flush all bcp data or convert it into bci.
  // 所以我们要么刷新所有bcp数据，要么将其转换成bci
  CodeCache::gc_prologue();
  Threads::gc_prologue();

  // Increment the invocation count
  //增加调用计数器
  _total_invocations++;

  // Capture heap size before collection for printing.
  // 在收集前为打印捕获堆大小
  size_t gch_prev_used = gch->used();

  // Capture used regions for each generation that will be subject to collection,
  // 捕获将被收集的每一代的区域
  //  so that card table adjustments can be made intelligently (see clear / invalidate further below).
  //那样卡表的调整将可以智能化
  gch->save_used_regions(level);

//分配栈内存
  allocate_stacks();
//标记清除阶段1
  mark_sweep_phase1(level, clear_all_softrefs);

//标记清除阶段2
  mark_sweep_phase2();

  // Don't add any more derived pointers during phase3
  COMPILER2_PRESENT(assert(DerivedPointerTable::is_active(), "Sanity"));
  COMPILER2_PRESENT(DerivedPointerTable::set_active(false));

//标记清除阶段3
  mark_sweep_phase3(level);

//标记清除阶段4
  mark_sweep_phase4();

//恢复标记
  restore_marks();

  // Set saved marks for allocation profiler (and other things? -- dld)
  // 为分配分析器设置保存标记
  // (Should this be in general part?)
  //这应该是一般部分吗？
  gch->save_marks();

//释放栈
  deallocate_stacks();

  // If compaction completely evacuated all generations younger than this
  // one, then we can clear the card table.  Otherwise, we must invalidate
  // it (consider all cards dirty).  In the future, we might consider doing
  // compaction within generations only, and doing card-table sliding.
  bool all_empty = true;
  for (int i = 0; all_empty && i < level; i++) {
    Generation* g = gch->get_gen(i);
    all_empty = all_empty && gch->get_gen(i)->used() == 0;
  }
  GenRemSet* rs = gch->rem_set();
  Generation* old_gen = gch->get_gen(level);
  // Clear/invalidate below make use of the "prev_used_regions" saved earlier.
  if (all_empty) {
    // We've evacuated all generations below us.
    rs->clear_into_younger(old_gen);
  } else {
    // Invalidate the cards corresponding to the currently used
    // region and clear those corresponding to the evacuated region.
    rs->invalidate_or_clear(old_gen);
  }

  Threads::gc_epilogue();
  CodeCache::gc_epilogue();
  JvmtiExport::gc_epilogue();

  if (PrintGC && !PrintGCDetails) {
    gch->print_heap_change(gch_prev_used);
  }

  // refs processing: clean slate
  _ref_processor = NULL;

  // Update heap occupancy information which is used as
  // input to soft ref clearing policy at the next gc.
  Universe::update_heap_info_at_gc();

  // Update time of last gc for all generations we collected
  // (which curently is all the generations in the heap).
  // We need to use a monotonically non-deccreasing time in ms
  // or we will see time-warp warnings and os::javaTimeMillis()
  // does not guarantee monotonicity.
  jlong now = os::javaTimeNanos() / NANOSECS_PER_MILLISEC;
  gch->update_time_of_last_gc(now);

  gch->trace_heap_after_gc(_gc_tracer);
}

void GenMarkSweep::allocate_stacks() {
  GenCollectedHeap* gch = GenCollectedHeap::heap();
  // Scratch request on behalf of oldest generation; will do no allocation.
  //划出请求代表老年代，不会进行分配
  ScratchBlock* scratch = gch->gather_scratch(gch->_gens[gch->_n_gens-1], 0);

  // $$$ To cut a corner, we'll only use the first scratch block, 
  //为了省事，我们只使用第一次刮擦块
  //and then revert to malloc.
  //然后恢复成malloc
  if (scratch != NULL) {
    _preserved_count_max =
      scratch->num_words * HeapWordSize / sizeof(PreservedMark);
  } else {
    _preserved_count_max = 0;
  }

  _preserved_marks = (PreservedMark*)scratch;
  _preserved_count = 0;
}


void GenMarkSweep::deallocate_stacks() {
  if (!UseG1GC) {
    GenCollectedHeap* gch = GenCollectedHeap::heap();
    gch->release_scratch();
  }

  _preserved_mark_stack.clear(true);
  _preserved_oop_stack.clear(true);
  _marking_stack.clear();
  _objarray_stack.clear(true);
}


void GenMarkSweep::mark_sweep_phase1(int level,
                                  bool clear_all_softrefs) {
  // Recursively traverse all live objects and mark them
  //递归便利所有存活对象并标记它们
  GCTraceTime tm("phase 1", PrintGC && Verbose, true, _gc_timer, _gc_tracer->gc_id());
  trace(" 1");

  GenCollectedHeap* gch = GenCollectedHeap::heap();

  // Because follow_root_closure is created statically, 
  //因为follow_root_closure是静态创建的
  // cannot use OopsInGenClosure constructor which takes a generation,
  //不能使用需要生成的OopsInGenClosure构造函数
  // as the Universe has not been created when the static constructors are run.
  //因为在运行静态构造函数时，宇宙（内存空间？）还没有被创建
  follow_root_closure.set_orig_generation(gch->get_gen(level));

  // Need new claim bits before marking starts.
  //需要新的声明位用于指针调整跟踪
  ClassLoaderDataGraph::clear_claimed_marks();

//标记GC roots?
  gch->gen_process_roots(level,
                         false, // Younger gens are not roots.
                         true,  // activate StrongRootsScope
                         GenCollectedHeap::SO_None,
                         ClassUnloading,
                         &follow_root_closure,
                         &follow_root_closure,
                         &follow_cld_closure);

  // Process reference objects found during marking
  //处理标记过程中找到的引用对象
  {
    ref_processor()->setup_policy(clear_all_softrefs);
    const ReferenceProcessorStats& stats =
      ref_processor()->process_discovered_references(
        &is_alive, &keep_alive, &follow_stack_closure, NULL, _gc_timer, _gc_tracer->gc_id());
    gc_tracer()->report_gc_reference_stats(stats);
  }

  // This is the point where the entire marking should have completed.
  //这是整个标记应该完成的地方
  assert(_marking_stack.is_empty(), "Marking should have completed");

  // Unload classes and purge the SystemDictionary.
  //卸载类并且清除系统字典
  bool purged_class = SystemDictionary::do_unloading(&is_alive);

  // Unload nmethods.
  //卸载本地?方法
  CodeCache::do_unloading(&is_alive, purged_class);

  // Prune dead klasses from subklass/sibling/implementor lists.
  //从子类/兄弟类/实现类列表中删除死类
  Klass::clean_weak_klass_links(&is_alive);

  // Delete entries for dead interned strings.
  //删除死掉的字符串条目
  StringTable::unlink(&is_alive);

  // Clean up unreferenced symbols in symbol table.
  //从符号表中清除没有引用的符号
  SymbolTable::unlink();

  gc_tracer()->report_object_count_after_gc(&is_alive);
}


void GenMarkSweep::mark_sweep_phase2() {
  // Now all live objects are marked, compute the new object addresses.
  // 现在所有的存活对象都已经被标记，计算新对象的地址
  // It is imperative that we traverse perm_gen LAST. 
  // 我们必须最后遍历永久代.
  // If dead space is allowed a range of dead object may get overwritten by a dead int array.
  // 如果允许死空间，则死对象的范围可能会被死int数组覆盖 
  // If perm_gen is not traversed last a Klass* may get overwritten.
  // 如果永久代没有最后遍历，一个类指针可能被覆盖
  // This is fine since it is dead, but if the class has dead
  // 这是好的，因为它是死的，但是如果类存在死的实例，我们需要忽略它们，为了找到它们的大小，我们需要类指针
  // instances we have to skip them, and in order to find their size we
  // need the Klass*!
  //
  // It is not required that we traverse spaces in the same order in phase2, phase3 and phase4, 
  // 我们不需要在阶段2,阶段3和阶段4中以相同的顺序便利空间
  // but the ValidateMarkSweep live oops tracking expects us to do so. 
  // 但是"校验标记清楚"实时跟踪希望我们这样做
  // See comment under phase4.
  // 参见阶段4的评论

  GenCollectedHeap* gch = GenCollectedHeap::heap();

  GCTraceTime tm("phase 2", PrintGC && Verbose, true, _gc_timer, _gc_tracer->gc_id());
  trace("2");

//准备压缩
  gch->prepare_for_compaction();
}

//调整指针实现
class GenAdjustPointersClosure: public GenCollectedHeap::GenClosure {
public:
  void do_generation(Generation* gen) {
    gen->adjust_pointers();
  }
};

void GenMarkSweep::mark_sweep_phase3(int level) {
  GenCollectedHeap* gch = GenCollectedHeap::heap();

  // Adjust the pointers to reflect the new locations
  // 调整指针以反映新的地址
  GCTraceTime tm("phase 3", PrintGC && Verbose, true, _gc_timer, _gc_tracer->gc_id());
  trace("3");

  // Need new claim bits for the pointer adjustment tracing.
  //需要新的声明位用于指针调整跟踪
  ClassLoaderDataGraph::clear_claimed_marks();

  // Because the closure below is created statically, 
  // 因为下面的闭包是静态创建的
  // we cannot use OopsInGenClosure constructor which takes a generation,
  // 我们不能使用需要生成的OopsInGenClosure构造函数
  // as the Universe has not been created when the static constructors are run.
  // 因为在运行静态构造函数时，宇宙（内存空间？）还没有被创建
  adjust_pointer_closure.set_orig_generation(gch->get_gen(level));

//处理GC roots？
  gch->gen_process_roots(level,
                         false, // Younger gens are not roots.
                         true,  // activate StrongRootsScope
                         GenCollectedHeap::SO_AllCodeCache,
                         GenCollectedHeap::StrongAndWeakRoots,
                         &adjust_pointer_closure,
                         &adjust_pointer_closure,
                         &adjust_cld_closure);

//处理弱引用GC roots？
  gch->gen_process_weak_roots(&adjust_pointer_closure);

//调整标记
  adjust_marks();
  GenAdjustPointersClosure blk;
  gch->generation_iterate(&blk, true);
}

//压缩实现
class GenCompactClosure: public GenCollectedHeap::GenClosure {
public:
  void do_generation(Generation* gen) {
    gen->compact();
  }
};

void GenMarkSweep::mark_sweep_phase4() {
  // All pointers are now adjusted, move objects accordingly
  // 现在所有的指针都被调整了，相应的移动对象
  // It is imperative that we traverse perm_gen first in phase4. 
  // 在阶段4我们必须先遍历永久代
  // All classes must be allocated earlier than their instances, 
  // 所有的类必须比实例先分配
  // and traversing perm_gen first makes sure that all Klass*s have moved to their new
  // 并且先遍历永久代可以保证所有的类在任何实例通过它的类分配之前，已经移动到了他们新的位置
  // location before any instance does a dispatch through it's klass!

  // The ValidateMarkSweep live oops tracking expects us to traverse spaces in the same order in phase2, phase3 and phase4
  // ValidateMarkSweep存活对象跟踪希望我们在阶段2,阶段3和阶段4使用相同的顺序遍历空间
  // We don't quite do that here (perm_gen first rather than last), so we tell the validate code
  // 我们这里不这么做（永久代第一个而不是最后一个），所以我们告诉校验代码，校验永久代的时候使用更高的索引（从阶段2保存下来的）
  // to use a higher index (saved from phase2) when verifying perm_gen.
  GenCollectedHeap* gch = GenCollectedHeap::heap();

  GCTraceTime tm("phase 4", PrintGC && Verbose, true, _gc_timer, _gc_tracer->gc_id());
  trace("4");

  GenCompactClosure blk;
  gch->generation_iterate(&blk, true);
}
