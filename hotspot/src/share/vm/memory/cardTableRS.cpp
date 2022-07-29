/*
 * Copyright (c) 2001, 2014, Oracle and/or its affiliates. All rights reserved.
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
#include "memory/allocation.inline.hpp"
#include "memory/cardTableRS.hpp"
#include "memory/genCollectedHeap.hpp"
#include "memory/generation.hpp"
#include "memory/space.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/java.hpp"
#include "runtime/os.hpp"
#include "utilities/macros.hpp"
#if INCLUDE_ALL_GCS
#include "gc_implementation/g1/concurrentMark.hpp"
#include "gc_implementation/g1/g1SATBCardTableModRefBS.hpp"
#endif // INCLUDE_ALL_GCS

CardTableRS::CardTableRS(MemRegion whole_heap,
                         int max_covered_regions) :
  GenRemSet(),
  _cur_youngergen_card_val(youngergenP1_card),
  _regions_to_iterate(max_covered_regions - 1)
{
#if INCLUDE_ALL_GCS
  if (UseG1GC) {
      _ct_bs = new G1SATBCardTableLoggingModRefBS(whole_heap,
                                                  max_covered_regions);
  } else {
    _ct_bs = new CardTableModRefBSForCTRS(whole_heap, max_covered_regions);
  }
#else
  _ct_bs = new CardTableModRefBSForCTRS(whole_heap, max_covered_regions);
#endif
  _ct_bs->initialize();
  set_bs(_ct_bs);
  _last_cur_val_in_gen = NEW_C_HEAP_ARRAY3(jbyte, GenCollectedHeap::max_gens + 1,
                         mtGC, CURRENT_PC, AllocFailStrategy::RETURN_NULL);
  if (_last_cur_val_in_gen == NULL) {
    vm_exit_during_initialization("Could not create last_cur_val_in_gen array.");
  }
  for (int i = 0; i < GenCollectedHeap::max_gens + 1; i++) {
    _last_cur_val_in_gen[i] = clean_card_val();
  }
  _ct_bs->set_CTRS(this);
}

CardTableRS::~CardTableRS() {
  if (_ct_bs) {
    delete _ct_bs;
    _ct_bs = NULL;
  }
  if (_last_cur_val_in_gen) {
    FREE_C_HEAP_ARRAY(jbyte, _last_cur_val_in_gen, mtInternal);
  }
}

void CardTableRS::resize_covered_region(MemRegion new_region) {
  _ct_bs->resize_covered_region(new_region);
}

jbyte CardTableRS::find_unused_youngergenP_card_value() {
  for (jbyte v = youngergenP1_card;
       v < cur_youngergen_and_prev_nonclean_card;
       v++) {
    bool seen = false;
    for (int g = 0; g < _regions_to_iterate; g++) {
      if (_last_cur_val_in_gen[g] == v) {
        seen = true;
        break;
      }
    }
    if (!seen) return v;
  }
  ShouldNotReachHere();
  return 0;
}

void CardTableRS::prepare_for_younger_refs_iterate(bool parallel) {
  // Parallel or sequential, we must always set the prev to equal the
  // 无论是并行还是顺序，我们必须始终将prev设置为与写入的最后一个相同
  // last one written.
  if (parallel) {
    // Find a parallel value to be used next.
    // 找到下一个会使用的并行值
    jbyte next_val = find_unused_youngergenP_card_value();
    set_cur_youngergen_card_val(next_val);

  } else {
    // In an sequential traversal we will always write youngergen, so that
    // 在顺序扫描中，我们将总是写更年轻的代，这样内联屏障是正确的
    // the inline barrier is  correct.
    set_cur_youngergen_card_val(youngergen_card);
  }
}

void CardTableRS::younger_refs_iterate(Generation* g,
                                       OopsInGenClosure* blk) {
  _last_cur_val_in_gen[g->level()+1] = cur_youngergen_card_val();
  g->younger_refs_iterate(blk);
}

inline bool ClearNoncleanCardWrapper::clear_card(jbyte* entry) {
  if (_is_par) {
    return clear_card_parallel(entry);
  } else {
    return clear_card_serial(entry);
  }
}

inline bool ClearNoncleanCardWrapper::clear_card_parallel(jbyte* entry) {
  while (true) {
    // In the parallel case, we may have to do this several times.
    // 在并行场景，我们可能需要多次这么做
    jbyte entry_val = *entry;
    assert(entry_val != CardTableRS::clean_card_val(),
           "We shouldn't be looking at clean cards, and this should "
           "be the only place they get cleaned.");
    if (CardTableRS::card_is_dirty_wrt_gen_iter(entry_val)
        || _ct->is_prev_youngergen_card_val(entry_val)) {
      jbyte res =
        Atomic::cmpxchg(CardTableRS::clean_card_val(), entry, entry_val);
      if (res == entry_val) {
        break;
      } else {
        assert(res == CardTableRS::cur_youngergen_and_prev_nonclean_card,
               "The CAS above should only fail if another thread did "
               "a GC write barrier.");
      }
    } else if (entry_val ==
               CardTableRS::cur_youngergen_and_prev_nonclean_card) {
      // Parallelism shouldn't matter in this case.  Only the thread
      // 这种情况下并行性应该是无关紧要的。
      // assigned to scan the card should change this value.
      // 只有分配给扫描卡的线程才会改变这个值
      *entry = _ct->cur_youngergen_card_val();
      break;
    } else {
      assert(entry_val == _ct->cur_youngergen_card_val(),
             "Should be the only possibility.");
      // In this case, the card was clean before, and become
      // 这种情况下，卡在之前是干净的，只是因为处理了一个提升的对象而变成cur_youngergen
      // cur_youngergen only because of processing of a promoted object.
      // We don't have to look at the card.
      // 我们不需要关注卡
      return false;
    }
  }
  return true;
}


inline bool ClearNoncleanCardWrapper::clear_card_serial(jbyte* entry) {
  jbyte entry_val = *entry;
  assert(entry_val != CardTableRS::clean_card_val(),
  //我们不应该看干净的卡牌，这应该是他们唯一被清理过的地方。 
         "We shouldn't be looking at clean cards, and this should "
         "be the only place they get cleaned.");
  assert(entry_val != CardTableRS::cur_youngergen_and_prev_nonclean_card,
         "This should be possible in the sequential case.");
  *entry = CardTableRS::clean_card_val();
  return true;
}

ClearNoncleanCardWrapper::ClearNoncleanCardWrapper(
  DirtyCardToOopClosure* dirty_card_closure, CardTableRS* ct) :
    _dirty_card_closure(dirty_card_closure), _ct(ct) {
    // Cannot yet substitute active_workers for n_par_threads
    // in the case where parallelism is being turned off by
    // setting n_par_threads to 0.
    // 在通过将n_par_threads设置为0来关闭并行性的情况下，还不能用active_workers替换n_par_threads 
    _is_par = (SharedHeap::heap()->n_par_threads() > 0);
    assert(!_is_par ||
           (SharedHeap::heap()->n_par_threads() ==
            SharedHeap::heap()->workers()->active_workers()), "Mismatch");
}

bool ClearNoncleanCardWrapper::is_word_aligned(jbyte* entry) {
  return (((intptr_t)entry) & (BytesPerWord-1)) == 0;
}

void ClearNoncleanCardWrapper::do_MemRegion(MemRegion mr) {
  assert(mr.word_size() > 0, "Error");
  assert(_ct->is_aligned(mr.start()), "mr.start() should be card aligned");
  // mr.end() may not necessarily be card aligned.
  // mr.end()不一定是对齐的
  jbyte* cur_entry = _ct->byte_for(mr.last());
  const jbyte* limit = _ct->byte_for(mr.start());
  HeapWord* end_of_non_clean = mr.end();
  HeapWord* start_of_non_clean = end_of_non_clean;
  while (cur_entry >= limit) {
    HeapWord* cur_hw = _ct->addr_for(cur_entry);
    if ((*cur_entry != CardTableRS::clean_card_val()) && clear_card(cur_entry)) {
      // Continue the dirty range by opening the
      // dirty window one card to the left.
      // 通过打开左侧的一张卡的脏窗口，继续脏范围
      start_of_non_clean = cur_hw;
    } else {
      // We hit a "clean" card; process any non-empty
      // 我们命中一个干净卡;
      // "dirty" range accumulated so far.
      // 处理到目前为止的任何非”脏“范围
      if (start_of_non_clean < end_of_non_clean) {
        const MemRegion mrd(start_of_non_clean, end_of_non_clean);
        _dirty_card_closure->do_MemRegion(mrd);
      }

      // fast forward through potential continuous whole-word range of clean cards beginning at a word-boundary
      // 快速前进，通过潜在的连续全字范围的干净卡片，从字边界开始
      if (is_word_aligned(cur_entry)) {
        jbyte* cur_row = cur_entry - BytesPerWord;
        while (cur_row >= limit && *((intptr_t*)cur_row) ==  CardTableRS::clean_card_row()) {
          cur_row -= BytesPerWord;
        }
        cur_entry = cur_row + BytesPerWord;
        cur_hw = _ct->addr_for(cur_entry);
      }

      // Reset the dirty window, while continuing to look
      // for the next dirty card that will start a
      // new dirty window.
      // 重置脏窗口，同时继续查找将启动新脏窗口的下一个脏卡
      end_of_non_clean = cur_hw;
      start_of_non_clean = cur_hw;
    }
    // Note that "cur_entry" leads "start_of_non_clean" in
    // its leftward excursion after this point
    // in the loop and, when we hit the left end of "mr",
    // will point off of the left end of the card-table
    // for "mr".
    //请注意，“cur_entry”在循环中的这一点之后，在向左偏移中导致“start_of_non_clean”，
    //并且当我们点击“mr”的左端时，将指向“mr”的卡表的左端
    cur_entry--;
  }
  // If the first card of "mr" was dirty, we will have
  // been left with a dirty window, co-initial with "mr",
  // which we now process.
  if (start_of_non_clean < end_of_non_clean) {
    const MemRegion mrd(start_of_non_clean, end_of_non_clean);
    _dirty_card_closure->do_MemRegion(mrd);
  }
}

// clean (by dirty->clean before) ==> cur_younger_gen
// dirty                          ==> cur_youngergen_and_prev_nonclean_card
// precleaned                     ==> cur_youngergen_and_prev_nonclean_card
// prev-younger-gen               ==> cur_youngergen_and_prev_nonclean_card
// cur-younger-gen                ==> cur_younger_gen
// cur_youngergen_and_prev_nonclean_card ==> no change.
void CardTableRS::write_ref_field_gc_par(void* field, oop new_val) {
  jbyte* entry = ct_bs()->byte_for(field);
  do {
    jbyte entry_val = *entry;
    // We put this first because it's probably the most common case.
    //我们把这个放到第一位，因为它可能是最常见的情况
    if (entry_val == clean_card_val()) {
      // No threat of contention with cleaning threads.
      //使清理线程不会产生争用威胁
      *entry = cur_youngergen_card_val();
      return;
    } else if (card_is_dirty_wrt_gen_iter(entry_val)
               || is_prev_youngergen_card_val(entry_val)) {
      // Mark it as both cur and prev youngergen; card cleaning thread will
      // 标记它为当前和上一个年轻代;卡清理线程最终将清理以前的东西
      // eventually remove the previous stuff.
      jbyte new_val = cur_youngergen_and_prev_nonclean_card;
      jbyte res = Atomic::cmpxchg(new_val, entry, entry_val);
      // Did the CAS succeed?
      if (res == entry_val) return;
      // Otherwise, retry, to see the new value.
      continue;
    } else {
      assert(entry_val == cur_youngergen_and_prev_nonclean_card
             || entry_val == cur_youngergen_card_val(),
             "should be only possibilities.");
      return;
    }
  } while (true);
}

void CardTableRS::younger_refs_in_space_iterate(Space* sp,
                                                OopsInGenClosure* cl) {
  const MemRegion urasm = sp->used_region_at_save_marks();
#ifdef ASSERT
  // Convert the assertion check to a warning if we are running
  // 如果我们运行CMS+ParNew，将断言转换为警告，直到修复相关bug
  // CMS+ParNew until related bug is fixed.
  MemRegion ur    = sp->used_region();
  assert(ur.contains(urasm) || (UseConcMarkSweepGC && UseParNewGC),
         err_msg("Did you forget to call save_marks()? "
                 "[" PTR_FORMAT ", " PTR_FORMAT ") is not contained in "
                 "[" PTR_FORMAT ", " PTR_FORMAT ")",
                 p2i(urasm.start()), p2i(urasm.end()), p2i(ur.start()), p2i(ur.end())));
  // In the case of CMS+ParNew, issue a warning
  if (!ur.contains(urasm)) {
    assert(UseConcMarkSweepGC && UseParNewGC, "Tautology: see assert above");
    warning("CMS+ParNew: Did you forget to call save_marks()? "
            "[" PTR_FORMAT ", " PTR_FORMAT ") is not contained in "
            "[" PTR_FORMAT ", " PTR_FORMAT ")",
             p2i(urasm.start()), p2i(urasm.end()), p2i(ur.start()), p2i(ur.end()));
    MemRegion ur2 = sp->used_region();
    MemRegion urasm2 = sp->used_region_at_save_marks();
    if (!ur.equals(ur2)) {
      warning("CMS+ParNew: Flickering used_region()!!");
    }
    if (!urasm.equals(urasm2)) {
      warning("CMS+ParNew: Flickering used_region_at_save_marks()!!");
    }
    ShouldNotReachHere();
  }
#endif
  _ct_bs->non_clean_card_iterate_possibly_parallel(sp, urasm, cl, this);
}

void CardTableRS::clear_into_younger(Generation* old_gen) {
  assert(old_gen->level() == 1, "Should only be called for the old generation");
  // The card tables for the youngest gen need never be cleared.
  // 最年轻的代的卡表不需要清理
  // There's a bit of subtlety in the clear() and invalidate()
  // methods that we exploit here and in invalidate_or_clear()
  // below to avoid missing cards at the fringes. If clear() or
  // invalidate() are changed in the future, this code should
  // be revisited. 20040107.ysr
  // 我们在这里和下面的invalidate_or_clear（）中利用的clear（）和invalate（）方法中有一些微妙之处，
  // 以避免在边缘丢失卡片。如果 clear（） 或 invalidate（） 将来发生更改，则应重新访问此代码
  clear(old_gen->prev_used_region());
}

void CardTableRS::invalidate_or_clear(Generation* old_gen) {
  assert(old_gen->level() == 1, "Should only be called for the old generation");
  // Invalidate the cards for the currently occupied part of
  // 使老年代中当前被占用部分的卡无效，
  // the old generation and clear the cards for the
  // 并为该代未被占用的部分清理卡
  // unoccupied part of the generation (if any, making use
  //（如果有的话，利用该代的pre_used_region来确定该区域）
  // of that generation's prev_used_region to determine that
  // region). No need to do anything for the youngest
  // 不需要为最年轻一代做任何事情
  // generation. Also see note#20040107.ysr above.
  MemRegion used_mr = old_gen->used_region();
  MemRegion to_be_cleared_mr = old_gen->prev_used_region().minus(used_mr);
  if (!to_be_cleared_mr.is_empty()) {
    clear(to_be_cleared_mr);
  }
  invalidate(used_mr);
}


class VerifyCleanCardClosure: public OopClosure {
private:
  HeapWord* _boundary;
  HeapWord* _begin;
  HeapWord* _end;
protected:
  template <class T> void do_oop_work(T* p) {
    HeapWord* jp = (HeapWord*)p;
    assert(jp >= _begin && jp < _end,
           err_msg("Error: jp " PTR_FORMAT " should be within "
                   "[_begin, _end) = [" PTR_FORMAT "," PTR_FORMAT ")",
                   p2i(jp), p2i(_begin), p2i(_end)));
    oop obj = oopDesc::load_decode_heap_oop(p);
    guarantee(obj == NULL || (HeapWord*)obj >= _boundary,
              err_msg("pointer " PTR_FORMAT " at " PTR_FORMAT " on "
                      "clean card crosses boundary" PTR_FORMAT,
                      p2i((HeapWord*)obj), p2i(jp), p2i(_boundary)));
  }

public:
  VerifyCleanCardClosure(HeapWord* b, HeapWord* begin, HeapWord* end) :
    _boundary(b), _begin(begin), _end(end) {
    assert(b <= begin,
           err_msg("Error: boundary " PTR_FORMAT " should be at or below begin " PTR_FORMAT,
                   p2i(b), p2i(begin)));
    assert(begin <= end,
           err_msg("Error: begin " PTR_FORMAT " should be strictly below end " PTR_FORMAT,
                   p2i(begin), p2i(end)));
  }

  virtual void do_oop(oop* p)       { VerifyCleanCardClosure::do_oop_work(p); }
  virtual void do_oop(narrowOop* p) { VerifyCleanCardClosure::do_oop_work(p); }
};

class VerifyCTSpaceClosure: public SpaceClosure {
private:
  CardTableRS* _ct;
  HeapWord* _boundary;
public:
  VerifyCTSpaceClosure(CardTableRS* ct, HeapWord* boundary) :
    _ct(ct), _boundary(boundary) {}
  virtual void do_space(Space* s) { _ct->verify_space(s, _boundary); }
};

class VerifyCTGenClosure: public GenCollectedHeap::GenClosure {
  CardTableRS* _ct;
public:
  VerifyCTGenClosure(CardTableRS* ct) : _ct(ct) {}
  void do_generation(Generation* gen) {
    // Skip the youngest generation.
    // 忽略最年轻代
    if (gen->level() == 0) return;
    // Normally, we're interested in pointers to younger generations.
    // 一般，我们对年轻代的指针感兴趣
    VerifyCTSpaceClosure blk(_ct, gen->reserved().start());
    gen->space_iterate(&blk, true);
  }
};

void CardTableRS::verify_space(Space* s, HeapWord* gen_boundary) {
  // We don't need to do young-gen spaces.
  // 不需要处理年轻代空间
  if (s->end() <= gen_boundary) return;
  MemRegion used = s->used_region();

  jbyte* cur_entry = byte_for(used.start());
  jbyte* limit = byte_after(used.last());
  while (cur_entry < limit) {
    if (*cur_entry == CardTableModRefBS::clean_card) {
      jbyte* first_dirty = cur_entry+1;
      while (first_dirty < limit &&
             *first_dirty == CardTableModRefBS::clean_card) {
        first_dirty++;
      }
      // If the first object is a regular object, and it has a
      // 如果第一个对象是常规对象，并且它有年轻到老年的字段，
      // young-to-old field, that would mark the previous card.
      // 需要标记上一个卡片
      HeapWord* boundary = addr_for(cur_entry);
      HeapWord* end = (first_dirty >= limit) ? used.end() : addr_for(first_dirty);
      HeapWord* boundary_block = s->block_start(boundary);
      HeapWord* begin = boundary;             // Until proven otherwise. 除非证明不是这样
      HeapWord* start_block = boundary_block; // Until proven otherwise.
      if (boundary_block < boundary) {
        if (s->block_is_obj(boundary_block) && s->obj_is_alive(boundary_block)) {
          oop boundary_obj = oop(boundary_block);
          if (!boundary_obj->is_objArray() &&
              !boundary_obj->is_typeArray()) {
            guarantee(cur_entry > byte_for(used.start()),
                      "else boundary would be boundary_block");
            if (*byte_for(boundary_block) != CardTableModRefBS::clean_card) {
              begin = boundary_block + s->block_size(boundary_block);
              start_block = begin;
            }
          }
        }
      }
      // Now traverse objects until end.
      // 现在遍历对象直到结束
      if (begin < end) {
        MemRegion mr(begin, end);
        VerifyCleanCardClosure verify_blk(gen_boundary, begin, end);
        for (HeapWord* cur = start_block; cur < end; cur += s->block_size(cur)) {
          if (s->block_is_obj(cur) && s->obj_is_alive(cur)) {
            oop(cur)->oop_iterate_no_header(&verify_blk, mr);
          }
        }
      }
      cur_entry = first_dirty;
    } else {
      // We'd normally expect that cur_youngergen_and_prev_nonclean_card
      // 我们通常期望cur_youngergen_and_prev_nonclean_card是个临时值
      // is a transient value, that cannot be in the card table
      // 除了GC过程中，否则不能存在于卡表中
      // except during GC, and thus assert that:
      // 因此断言：
      // guarantee(*cur_entry != cur_youngergen_and_prev_nonclean_card,
      //        "Illegal CT value");
      // 保证(*cur_entry != cur_youngergen_and_prev_nonclean_card, "Illegal CT value");
      // That however, need not hold, as will become clear in the
      // 然而，这不需要成立，正如下面将要明确的那样
      // following...

      // We'd normally expect that if we are in the parallel case,
      // 在并行场景，我们通常期望
      // we can't have left a prev value (which would be different
      // 我们不能在卡表中留下一个prev值（可能与当前值不同）
      // from the current value) in the card table, and so we'd like to
      // 因此，我们将会断言：
      // assert that:
      // guarantee(cur_youngergen_card_val() == youngergen_card
      //           || !is_prev_youngergen_card_val(*cur_entry),
      //           "Illegal CT value");
      // 保证(cur_youngergen_card_val() == youngergen_card
      //           || !is_prev_youngergen_card_val(*cur_entry),
      //           "Illegal CT value");
      // That, however, may not hold occasionally, because of
      // 然而，因为老年代的CMS或者MSC，偶尔可能hold不住
      // CMS or MSC in the old gen. To wit, consider the
      // 即，考虑以下两种简单的说明性场景：
      // following two simple illustrative scenarios:
      // (a) CMS: Consider the case where a large object L
      // （a）CMS：考虑这种情况，即在老年代分配跨越若干卡片的大对象L
      //     spanning several cards is allocated in the old
      //     gen, and has a young gen reference stored in it, dirtying
      //     并且有持有一个年轻代引用，弄脏了一下内部卡片
      //     some interior cards. A young collection scans the card,
      //     年轻代收集器扫描这个卡片
      //     finds a young ref and installs a youngergenP_n value.
      //     找到一个年轻代引用并且安装youngergenP_n值
      //     L then goes dead. Now a CMS collection starts,
      //     然后L死掉。现在CMS收集启动
      //     finds L dead and sweeps it up. Assume that L is
      //     找到L死亡并清除它。假设L与_unallocated_blk相邻
      //     abutting _unallocated_blk, so _unallocated_blk is
      //     所以_unallocated_blk被调整为L
      //     adjusted down to (below) L. Assume further that
      //     no young collection intervenes during this CMS cycle.
      //     进一步假设，在CMS周期内没有年轻代收集干预
      //     The next young gen cycle will not get to look at this
      //     下一次年轻代周期将无法看到这个youngergenP_n卡
      //     youngergenP_n card since it lies in the unoccupied
      //     因为它位于空间的未占用部分
      //     part of the space.
      //     Some young collections later the blocks on this
      //     一些年轻代收集后，由于直接分配或由于吸收晋升，此卡上的块可以重新分配
      //     card can be re-allocated either due to direct allocation
      //     or due to absorbing promotions. At this time, the
      //     此时
      //     before-gc verification will fail the above assert.
      //     gc前校验将无法通过上述断言
      // (b) MSC: In this case, an object L with a young reference
      // （b）MSC：这种情况，具有年轻代引用的对象L位于（因此）具有youngergen_n值的卡表上
      //     is on a card that (therefore) holds a youngergen_n value.
      //     Suppose also that L lies towards the end of the used
      //     还假设L位于GC之前已用空间的末尾
      //     the used space before GC. An MSC collection
      //     occurs that compacts to such an extent that this
      //     发生MSC收集，压缩到这样的程度，此卡不再是在占用的部分空间
      //     card is no longer in the occupied part of the space.
      //     Since current code in MSC does not always clear cards
      //     因为当前的代码在MSC不是总会清理老年代部分没有使用的卡片
      //     in the unused part of old gen, this stale youngergen_n
      //     value is left behind and can later be covered by
      //     这个陈旧的youngergen_n值被留在后面，稍后当提升或直接分配，重新分配堆的那一部分时，它可以被对象覆盖
      //     an object when promotion or direct allocation
      //     re-allocates that part of the heap.
      //
      // Fortunately, the presence of such stale card values is
      // 幸运的，这些陈旧的卡值的存在”只是“一个小麻烦，因为随后的年轻代收集可能会不必要的扫描这些卡
      // "only" a minor annoyance in that subsequent young collections
      // might needlessly scan such cards, but would still never corrupt
      // 但是仍然不会破坏堆
      // the heap as a result. However, it's likely not to be a significant
      // 然而，在实践中，它可能不会成为一个显著的性能抑制因素
      // performance inhibitor in practice. For instance,
      // 例如，
      // some recent measurements with unoccupied cards eagerly cleared
      // 最近一些为占用卡的测量结果被清空以保持这个不变
      // out to maintain this invariant, showed next to no
      // 显示几乎没有变化的年轻代收集时间
      // change in young collection times; of course one can construct
      // degenerate examples where the cost can be significant.)
      // 当然，我们可以构造一些代价很大的恶化例子
      // Note, in particular, that if the "stale" card is modified
      // 特别要注意的是，如果在更新分配然后修改了”过期“卡，那么它将是脏的，而不是”过期“的
      // after re-allocation, it would be dirty, not "stale". Thus,
      // we can never have a younger ref in such a card and it is
      // 因此，我们不能在这样的卡片中有一个年轻代的引用，在任何收集中不扫描这张卡片是安全的
      // safe not to scan that card in any collection. [As we see
      // below, we do some unnecessary scanning
      // 就像我们下面看到的，在当前的并行扫描算法中，我们在某些情况下做了一些不必要的扫描
      // in some cases in the current parallel scanning algorithm.]
      //
      // The main point below is that the parallel card scanning code
      // 下面要点是，并行卡片扫描代码正确的处理这些陈旧的卡值
      // deals correctly with these stale card values. There are two main
      // cases to consider where we have a stale "younger gen" value and a
      // 我们需要考虑两种主要情况，即陈旧的”年轻代“值和”衍生“情况
      // "derivative" case to consider, where we have a stale
      // "cur_younger_gen_and_prev_non_clean" value, as will become
      // 在这里我们有一个陈旧的”cur_younger_gen_and_prev_non_clean“值，这将在下面的案例分析中变得明显
      // apparent in the case analysis below.
      // o Case 1. If the stale value corresponds to a younger_gen_n
      //   情况1.如果陈旧值对应于cur_younger_gen值以外的younger_gen_n值，则代码将其视为等同于prev_younger_gen卡
      //   value other than the cur_younger_gen value then the code
      //   treats this as being tantamount to a prev_younger_gen
      //   card. This means that the card may be unnecessarily scanned.
      //   There are two sub-cases to consider:
      //   o Case 1a. Let us say that the card is in the occupied part
      //   情况1a。假设卡片在收集开始时处于代中被占用的部分
      //     of the generation at the time the collection begins. In
      //     that case the card will be either cleared when it is scanned
      //     这种情况下，卡片将在扫描完年轻代指针后被清理，
      //     for young pointers, or will be set to cur_younger_gen as a
      //     或被设置为cur_younger_gen作为晋升的结果
      //     result of promotion. (We have elided the normal case where
      //     我们已经省略了扫描线程和提升线程交错可能导致cur_younger_gen_and_prev_non_clean值在确定为cur_younger_gen之前的一种正常情况。 (例1结束。) 
      //     the scanning thread and the promoting thread interleave
      //     possibly resulting in a transient
      //     cur_younger_gen_and_prev_non_clean value before settling
      //     to cur_younger_gen. [End Case 1a.]
      //   o Case 1b. Consider now the case when the card is in the unoccupied
      //     情况1b。现在考虑这样一种情况:当卡处于空间的未占用部分时，由于在当前年轻GC期间升级到它而被占用 
      //     part of the space which becomes occupied because of promotions
      //     into it during the current young GC. In this case the card
      //     这种情况
      //     will never be scanned for young references. The current
      //     卡将不会因为年轻代引用被扫描到
      //     code will set the card value to either
      //     当前代码会设置卡值为cur_younger_gen_and_prev_non_clean或让它保留旧值
      //     cur_younger_gen_and_prev_non_clean or leave
      //     it with its stale value -- because the promotions didn't
      //   因为这张卡上没有出现更年轻的晋升
      //     result in any younger refs on that card. Of these two
      //     cases, the latter will be covered in Case 1a during
      //    这两种情况下，后一种情况将在后续扫描过程中在案例1a中进行介绍
      //     a subsequent scan. To deal with the former case, we need
      //    处理前一种情况，在下面的案例分析中，我们需要进一步考虑如何处理cur_youngger_gen_and_prev_non_clean的旧值 
      //     to further consider how we deal with a stale value of
      //     cur_younger_gen_and_prev_non_clean in our case analysis
      //     below. This we do in Case 3 below. [End Case 1b]
      //    我们将在下面的案例3中这样做
      //   [End Case 1]
      // o Case 2. If the stale value corresponds to cur_younger_gen being
      //   案例2.如果旧值对应的cur_younger_gen值不是当前晋升写入的值
      //   a value not necessarily written by a current promotion, the
      //   card will not be scanned by the younger refs scanning code.
      //   卡片不会被年轻代引用扫描代码扫描到
      //   (This is OK since as we argued above such cards cannot contain
      //   （这是可以的，因为我们上面讨论了卡不会包含任何年轻代引用）
      //   any younger refs.) The result is that this value will be
      //    结果是，此值将在后续集合中被视为prev_younger_gen值，
      //   treated as a prev_younger_gen value in a subsequent collection,
      //   which is addressed in Case 1 above. [End Case 2]
      //   这在上面的案例 1 中得到解决
      // o Case 3. We here consider the "derivative" case from Case 1b. above
      //  案例3. 我们在这里考虑案例1b中的”衍生“案例
      //   because of which we may find a stale
      //   由于我们在表里发现一个旧的cur_younger_gen_and_prev_non_clean卡值
      //   cur_younger_gen_and_prev_non_clean card value in the table.
      //   Once again, as in Case 1, we consider two subcases, depending
      //   再次，就像案例1中那样，我们考虑两种子案例，
      //   on whether the card lies in the occupied or unoccupied part
      //  取决于卡是否位于年轻代收集开始时空间被占用或未占用的部分
      //   of the space at the start of the young collection.
      //   o Case 3a. Let us say the card is in the occupied part of
      //    案例3a。假设年轻代回收开始时卡在被占用的老年代
      //     the old gen at the start of the young collection. In that
      //     case, the card will be scanned by the younger refs scanning
      //   这种情况下，该卡将被年轻代引用扫描代码扫描，该扫描代码将其设置为cur_youngger_gen 
      //     code which will set it to cur_younger_gen. In a subsequent
      //     scan, the card will be considered again and get its final
      //在随后的扫描中，将再次考虑该卡片并得到其最终的正确值
      //     correct value. [End Case 3a]
      //   o Case 3b. Now consider the case where the card is in the
      //     案例3b。现在考虑卡位于老年代未占用的部分时的场景
      //     unoccupied part of the old gen, and is occupied as a result
      //     并且它作为在年轻代gc期间被晋升的结果
      //     of promotions during thus young gc. In that case,
      //    这种情况下
      //     the card will not be scanned for younger refs. The presence
      //     此卡将不会被年轻代引用扫描到
      //     of newly promoted objects on the card will then result in
      //     卡片上新提升的对象的存在将导致它保持cur_youngger_gen_and_prev_non_clean值，
      //     its keeping the value cur_younger_gen_and_prev_non_clean
      //     value, which we have dealt with in Case 3 here. [End Case 3b]
      //      我们在这里的案例3中已经处理了这个值 
      //   [End Case 3]
      //
      // (Please refer to the code in the helper class
      // ClearNonCleanCardWrapper and in CardTableModRefBS for details.)
      // 详细信息请参考助手类ClearNonCleanCardWrapper和CardTableModRefBS中的代码
      //
      // The informal arguments above can be tightened into a formal
      // correctness proof and it behooves us to write up such a proof,
      // or to use model checking to prove that there are no lingering
      // concerns.
      // 上面的非正式论证可以被压缩成正式的正确性证明，我们有必要写出这样的证明，或者使用模型检查来证明没有残留的问题 
      //
      // Clearly because of Case 3b one cannot bound the time for
      // 显然，由于案例3b，我们无法限制一张卡保留我们所说的”过期“值的时间
      // which a card will retain what we have called a "stale" value.
      // However, one can obtain a Loose upper bound on the redundant
      // 然而，由于这样的旧值，可以获得冗余工作的松散上界
      // work as a result of such stale values. Note first that any
      // time a stale card lies in the occupied part of the space at
      // 首先要注意的是，在收集开始的任何时候，过期的卡位于已占用的空间部分 
      // the start of the collection, it is scanned by younger refs
      // 它被年轻代引用代码扫描到，并且我们可以根据卡值定义一个等级函数，
      // code and we can define a rank function on card values that
      // declines when this is so. Note also that when a card does not
      // 在这种情况下卡值会下降。也要注意，当卡没有位于年轻代收集开始时已占用的部分
      // lie in the occupied part of the space at the beginning of a
      // young collection, its rank can either decline or stay unchanged.
      // 他的排名要么下降，要么保持不变
      // In this case, no extra work is done in terms of redundant
      // 这个案例中，不需要额外的冗余做年轻代引用扫描卡
      // younger refs scanning of that card.
      // Then, the case analysis above reveals that, in the worst case,
      // 然后，上面的案例分析表明，在最坏的情况下
      // any such stale card will be scanned unnecessarily at most twice.
      // 任何这类旧值卡将被扫描两次
      // It is nonethelss advisable to try and get rid of some of this
      // 
      // redundant work in a subsequent (low priority) re-design of
      // the card-scanning code, if only to simplify the underlying
      // state machine analysis/proof. ysr 1/28/2002. XXX
      // 尽管如此，在后续的(低优先级)卡片扫描代码重新设计中，如果只是为了简化底层的状态机分析/证明，尝试消除一些冗余工作是可取的 
      cur_entry++;
    }
  }
}

void CardTableRS::verify() {
  // At present, we only know how to verify the card table RS for
  // 目前，我们只知道如何验证卡表 RS的分代堆
  // generational heaps.
  VerifyCTGenClosure blk(this);
  CollectedHeap* ch = Universe::heap();

  if (ch->kind() == CollectedHeap::GenCollectedHeap) {
    GenCollectedHeap::heap()->generation_iterate(&blk, false);
    _ct_bs->verify();
    }
  }


void CardTableRS::verify_aligned_region_empty(MemRegion mr) {
  if (!mr.is_empty()) {
    jbyte* cur_entry = byte_for(mr.start());
    jbyte* limit = byte_after(mr.last());
    // The region mr may not start on a card boundary so
    // mar区域不能从一个卡边界开始
    // the first card may reflect a write to the space
    // 所以，第一张卡可以反映在mr之前的空间中写的内容
    // just prior to mr.
    if (!is_aligned(mr.start())) {
      cur_entry++;
    }
    for (;cur_entry < limit; cur_entry++) {
      guarantee(*cur_entry == CardTableModRefBS::clean_card,
                "Unexpected dirty card found");
    }
  }
}
