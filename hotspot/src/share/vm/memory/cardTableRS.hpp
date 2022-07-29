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

#ifndef SHARE_VM_MEMORY_CARDTABLERS_HPP
#define SHARE_VM_MEMORY_CARDTABLERS_HPP

#include "memory/cardTableModRefBS.hpp"
#include "memory/genRemSet.hpp"
#include "memory/memRegion.hpp"

class Space;
class OopsInGenClosure;

// This kind of "GenRemSet" uses a card table both as shared data structure
// 这类的“GenRemSet”使用卡表同时用作mod ref屏障和记忆集信息的共享数据结构
// for a mod ref barrier set and for the rem set information.

class CardTableRS: public GenRemSet {
  friend class VMStructs;
  // Below are private classes used in impl.
  // 下面都是实现中使用的私有类
  friend class VerifyCTSpaceClosure;
  friend class ClearNoncleanCardWrapper;

  static jbyte clean_card_val() {
    return CardTableModRefBS::clean_card;
  }

  static intptr_t clean_card_row() {
    return CardTableModRefBS::clean_card_row;
  }

  static bool
  card_is_dirty_wrt_gen_iter(jbyte cv) {
    return CardTableModRefBS::card_is_dirty_wrt_gen_iter(cv);
  }

  CardTableModRefBSForCTRS* _ct_bs;

  virtual void younger_refs_in_space_iterate(Space* sp, OopsInGenClosure* cl);

  void verify_space(Space* s, HeapWord* gen_start);

  enum ExtendedCardValue {
    youngergen_card   = CardTableModRefBS::CT_MR_BS_last_reserved + 1,
    // These are for parallel collection.
    // 这些给并行收集器使用
    // There are three P (parallel) youngergen card values.  In general, this
    // 有三个P（并行）年轻代卡表值，一般，
    // needs to be more than the number of generations (including the perm、
    // 这需要比可能单独调用younger_refs_do的代（包括永久代，gen）的数量更多
    // gen) that might have younger_refs_do invoked on them separately.  So
    // if we add more gens, we have to add more values.
    // 所以如果我们添加更多代，我们需要增加跟多值
    youngergenP1_card  = CardTableModRefBS::CT_MR_BS_last_reserved + 2,
    youngergenP2_card  = CardTableModRefBS::CT_MR_BS_last_reserved + 3,
    youngergenP3_card  = CardTableModRefBS::CT_MR_BS_last_reserved + 4,
    cur_youngergen_and_prev_nonclean_card =
      CardTableModRefBS::CT_MR_BS_last_reserved + 5
  };

  // An array that contains, for each generation, the card table value last
  // 每个代包含的数组
  // used as the current value for a younger_refs_do iteration of that
  // 卡表值最后用作表中该部分的younger_refs_do迭代的当前值
  // portion of the table.  (The perm gen is index 0; other gens are at
  //（永久代在索引0;其他代在他们的级别+1
  // their level plus 1.  They youngest gen is in the table, but will
  // 他们是表里最年轻的一代
  // always have the value "clean_card".)
  // 但是会有“clean_card”的值）
  jbyte* _last_cur_val_in_gen;

  jbyte _cur_youngergen_card_val;

  int _regions_to_iterate;

  jbyte cur_youngergen_card_val() {
    return _cur_youngergen_card_val;
  }
  void set_cur_youngergen_card_val(jbyte v) {
    _cur_youngergen_card_val = v;
  }
  bool is_prev_youngergen_card_val(jbyte v) {
    return
      youngergen_card <= v &&
      v < cur_youngergen_and_prev_nonclean_card &&
      v != _cur_youngergen_card_val;
  }
  // Return a youngergen_card_value that is not currently in use.
  // 返回一个当前没有被使用的youngergen_card_value
  jbyte find_unused_youngergenP_card_value();

public:
  CardTableRS(MemRegion whole_heap, int max_covered_regions);
  ~CardTableRS();

  // *** GenRemSet functions.
  GenRemSet::Name rs_kind() { return GenRemSet::CardTable; }

  CardTableRS* as_CardTableRS() { return this; }

  CardTableModRefBS* ct_bs() { return _ct_bs; }

  // Override.
  void prepare_for_younger_refs_iterate(bool parallel);

  // Card table entries are cleared before application; "blk" is
  // 卡表条目在申请前被清除
  // responsible for dirtying if the oop is still older-to-younger after
  // 如果在闭包申请之后，oop仍然是老年代到年轻代，”blk“负责脏
  // closure application.
  void younger_refs_iterate(Generation* g, OopsInGenClosure* blk);

  void inline_write_ref_field_gc(void* field, oop new_val) {
    jbyte* byte = _ct_bs->byte_for(field);
    *byte = youngergen_card;
  }
  void write_ref_field_gc_work(void* field, oop new_val) {
    inline_write_ref_field_gc(field, new_val);
  }

  // Override.  Might want to devirtualize this in the same fashion as
  // 可能需要以上面相同的方式对其进行反虚拟化
  // above.  Ensures that the value of the card for field says that it's
  // 确保卡中字段的值表明它是当前收集中更年轻的卡
  // a younger card in the current collection.
  virtual void write_ref_field_gc_par(void* field, oop new_val);

  void resize_covered_region(MemRegion new_region);

  bool is_aligned(HeapWord* addr) {
    return _ct_bs->is_card_aligned(addr);
  }

  void verify();
  void verify_aligned_region_empty(MemRegion mr);

  void clear(MemRegion mr) { _ct_bs->clear(mr); }
  void clear_into_younger(Generation* old_gen);

  void invalidate(MemRegion mr, bool whole_heap = false) {
    _ct_bs->invalidate(mr, whole_heap);
  }
  void invalidate_or_clear(Generation* old_gen);

  static uintx ct_max_alignment_constraint() {
    return CardTableModRefBS::ct_max_alignment_constraint();
  }

  jbyte* byte_for(void* p)     { return _ct_bs->byte_for(p); }
  jbyte* byte_after(void* p)   { return _ct_bs->byte_after(p); }
  HeapWord* addr_for(jbyte* p) { return _ct_bs->addr_for(p); }

  bool is_prev_nonclean_card_val(jbyte v) {
    return
      youngergen_card <= v &&
      v <= cur_youngergen_and_prev_nonclean_card &&
      v != _cur_youngergen_card_val;
  }

  static bool youngergen_may_have_been_dirty(jbyte cv) {
    return cv == CardTableRS::cur_youngergen_and_prev_nonclean_card;
  }

};

class ClearNoncleanCardWrapper: public MemRegionClosure {
  DirtyCardToOopClosure* _dirty_card_closure;
  CardTableRS* _ct;
  bool _is_par;
private:
  // Clears the given card, return true if the corresponding card should be
  // 清除传递的卡，如果需要处理相应的卡片，则返回true
  // processed.
  inline bool clear_card(jbyte* entry);
  // Work methods called by the clear_card()
  inline bool clear_card_serial(jbyte* entry);
  inline bool clear_card_parallel(jbyte* entry);
  // check alignment of pointer
  //检测指针对齐
  bool is_word_aligned(jbyte* entry);

public:
  ClearNoncleanCardWrapper(DirtyCardToOopClosure* dirty_card_closure, CardTableRS* ct);
  void do_MemRegion(MemRegion mr);
};

#endif // SHARE_VM_MEMORY_CARDTABLERS_HPP
