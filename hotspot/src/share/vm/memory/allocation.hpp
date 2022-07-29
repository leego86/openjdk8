/*
 * Copyright (c) 1997, 2014, Oracle and/or its affiliates. All rights reserved.
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

#ifndef SHARE_VM_MEMORY_ALLOCATION_HPP
#define SHARE_VM_MEMORY_ALLOCATION_HPP

#include "runtime/globals.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#ifdef COMPILER1
#include "c1/c1_globals.hpp"
#endif
#ifdef COMPILER2
#include "opto/c2_globals.hpp"
#endif

#include <new>

#define ARENA_ALIGN_M1 (((size_t)(ARENA_AMALLOC_ALIGNMENT)) - 1)
#define ARENA_ALIGN_MASK (~((size_t)ARENA_ALIGN_M1))
#define ARENA_ALIGN(x) ((((size_t)(x)) + ARENA_ALIGN_M1) & ARENA_ALIGN_MASK)


// noinline attribute
#ifdef _WINDOWS
  #define _NOINLINE_  __declspec(noinline)
#else
  #if __GNUC__ < 3    // gcc 2.x does not support noinline attribute
    #define _NOINLINE_
  #else
    #define _NOINLINE_ __attribute__ ((noinline))
  #endif
#endif

class AllocFailStrategy {
public:
  enum AllocFailEnum { EXIT_OOM, RETURN_NULL };
};
typedef AllocFailStrategy::AllocFailEnum AllocFailType;

// All classes in the virtual machine must be subclassed
// 所有虚拟机中的类必须是下面这里分配的中的一个的子类
// by one of the following allocation classes:
//
// For objects allocated in the resource area (see resourceArea.hpp).
// 对于资源区域的分配对象
// - ResourceObj
//
// For objects allocated in the C-heap (managed by: free & malloc).
// 对于C堆的分配对象
// - CHeapObj
//
// For objects allocated on the stack.
// 对于栈上分配对象
// - StackObj
//
// For embedded objects.
// 对于嵌入式对象
// - ValueObj
//
// For classes used as name spaces.
// 对于用作命名空间的类
// - AllStatic
//
// For classes in Metaspace (class data)
// 对于元空间的类
// - MetaspaceObj
//
// The printable subclasses are used for debugging and define virtual
// 可打印子类用于调试和定义用于打印的虚拟成员函数
// member functions for printing. Classes that avoid allocating the
// 因此，避免在对象中分配vtbl条目的类不应该是可以打印的子类
// vtbl entries in the objects should therefore not be the printable
// subclasses.
//
// The following macros and function should be used to allocate memory
// 下面的宏命令和函数应该用于在资源区域或C堆直接分配内存
// directly in the resource area or in the C-heap, The _OBJ variants
// of the NEW/FREE_C_HEAP macros are used for alloc/dealloc simple
// NEW/FREE_C_HEAP宏的变体_OBJ用于alloc/dealloc不从CHeapObj继承的简单对象 
// objects which are not inherited from CHeapObj, note constructor and
// 注意:不调用构造函数和析构函数
// destructor are not called. The preferable way to allocate objects
// 更好的分配对象的方法是使用new操作符 
// is using the new operator.
//
// WARNING: The array variant must only be used for a homogenous array
// 警告：数组变体只能用于所有对象都是指定的精确类型的同质数组
// where all objects are of the exact type specified. If subtypes are
// stored in the array then must pay attention to calling destructors
// 如果子类型存储在数组中，必须注意在需要时调用析够函数
// at needed.
//
//   NEW_RESOURCE_ARRAY(type, size)
//   NEW_RESOURCE_OBJ(type)
//   NEW_C_HEAP_ARRAY(type, size)
//   NEW_C_HEAP_OBJ(type, memflags)
//   FREE_C_HEAP_ARRAY(type, old, memflags)
//   FREE_C_HEAP_OBJ(objname, type, memflags)
//   char* AllocateHeap(size_t size, const char* name);
//   void  FreeHeap(void* p);
//
// C-heap allocation can be traced using +PrintHeapAllocation.
// C-堆分配可以使用+PrintHeapAllocation跟踪
// malloc and free should therefore never called directly.
// 因此，不应该直接调用Malloc和free 

// Base class for objects allocated in the C-heap.
// 对象的基础类在C堆中被分配

// In non product mode we introduce a super class for all allocation classes
//在非产品模式下，我们为所有支持打印的分配类引入了一个超类
// that supports printing.
// We avoid the superclass in product mode since some C++ compilers add
// 我们避免产品模式下超类，因为一些C++编译器为空的超类添加一个字的开销
// a word overhead for empty super classes.

#ifdef PRODUCT
#define ALLOCATION_SUPER_CLASS_SPEC
#else
#define ALLOCATION_SUPER_CLASS_SPEC : public AllocatedObj
class AllocatedObj {
 public:
  // Printing support
  void print() const;
  void print_value() const;

  virtual void print_on(outputStream* st) const;
  virtual void print_value_on(outputStream* st) const;
};
#endif


/*
 * Memory types
 */
enum MemoryType {
  // Memory type by sub systems. It occupies lower byte.
  // 各子系统的内存类型，它将占用更低的字节
  mtJavaHeap          = 0x00,  // Java heap
  mtClass             = 0x01,  // memory class for Java classes  java类的内存
  mtThread            = 0x02,  // memory for thread objects 线程对象的内存
  mtThreadStack       = 0x03,
  mtCode              = 0x04,  // memory for generated code 生成代码的内存
  mtGC                = 0x05,  // memory for GC  GC的内存
  mtCompiler          = 0x06,  // memory for compiler 编译的内存
  mtInternal          = 0x07,  // memory used by VM, but does not belong to
                               // 虚拟机用的内存，但是不属于任何上面的分类，
                                 // any of above categories, and not used for
                                 // 也不用于本地内存跟踪
                                 // native memory tracking
  mtOther             = 0x08,  // memory not used by VM 不被虚拟机使用的内存
  mtSymbol            = 0x09,  // symbol  符号
  mtNMT               = 0x0A,  // memory used by native memory tracking  本地内存跟踪使用的内存
  mtClassShared       = 0x0B,  // class data sharing  类数据共享
  mtChunk             = 0x0C,  // chunk that holds content of arenas 保持竞技场？内容的块
  mtTest              = 0x0D,  // Test type for verifying NMT  用于验证NMT的测试类型
  mtTracing           = 0x0E,  // memory used for Tracing  跟踪使用的内存
  mtNone              = 0x0F,  // undefined  未定义
  mt_number_of_types  = 0x10   // number of memory types (mtDontTrack 内存类型数量（mtDontTrack 不包含在验证类型中）
                                 // is not included as validate type)
};

typedef MemoryType MEMFLAGS;


#if INCLUDE_NMT

extern bool NMT_track_callsite;

#else

const bool NMT_track_callsite = false;

#endif // INCLUDE_NMT

class NativeCallStack;


template <MEMFLAGS F> class CHeapObj ALLOCATION_SUPER_CLASS_SPEC {
 public:
  _NOINLINE_ void* operator new(size_t size, const NativeCallStack& stack) throw();
  _NOINLINE_ void* operator new(size_t size) throw();
  _NOINLINE_ void* operator new (size_t size, const std::nothrow_t&  nothrow_constant,
                               const NativeCallStack& stack) throw();
  _NOINLINE_ void* operator new (size_t size, const std::nothrow_t&  nothrow_constant)
                               throw();
  _NOINLINE_ void* operator new [](size_t size, const NativeCallStack& stack) throw();
  _NOINLINE_ void* operator new [](size_t size) throw();
  _NOINLINE_ void* operator new [](size_t size, const std::nothrow_t&  nothrow_constant,
                               const NativeCallStack& stack) throw();
  _NOINLINE_ void* operator new [](size_t size, const std::nothrow_t&  nothrow_constant)
                               throw();
  void  operator delete(void* p);
  void  operator delete [] (void* p);
};

// Base class for objects allocated on the stack only.
// 只在栈上分配对象的基础类
// Calling new or delete will result in fatal error.
// 调用new或者delete将造成致命错误

class StackObj ALLOCATION_SUPER_CLASS_SPEC {
 private:
  void* operator new(size_t size) throw();
  void* operator new [](size_t size) throw();
#ifdef __IBMCPP__
 public:
#endif
  void  operator delete(void* p);
  void  operator delete [](void* p);
};

// Base class for objects used as value objects.
// 用作值对象的对象的基类
// Calling new or delete will result in fatal error.
// 调用new或者delete将造成致命错误
//
// Portability note: Certain compilers (e.g. gcc) will
// 可移植行注意：某些编译器(例如gcc)总是会把有超类的类做大 
// always make classes bigger if it has a superclass, even
// if the superclass does not have any virtual methods or
// 即使超类没有虚拟方法或实例字段
// instance fields. The HotSpot implementation relies on this
// HotSpot实现依赖于这种情况不会发生
// not to happen. So never make a ValueObj class a direct subclass
// 所以不要让ValueObj类成为这个对象的直接子类，而是使用VALUE_OBJ_CLASS_SPEC类代替
// of this object, but use the VALUE_OBJ_CLASS_SPEC class instead, e.g.,
// like this:
//
//   class A VALUE_OBJ_CLASS_SPEC {
//     ...
//   }
//
// With gcc and possible other compilers the VALUE_OBJ_CLASS_SPEC can
// 使用gcc和其他可能的编译器VALUE_OBJ_CLASS_SPEC会被定义为一个空字符串”“
// be defined as a an empty string "".
//
class _ValueObj {
 private:
  void* operator new(size_t size) throw();
  void  operator delete(void* p);
  void* operator new [](size_t size) throw();
  void  operator delete [](void* p);
};


// Base class for objects stored in Metaspace.
// 存储在元空间中的对象的基类
// Calling delete will result in fatal error.
//
// Do not inherit from something with a vptr because this class does
// not introduce one.  This class is used to allocate both shared read-only
// and shared read-write classes.
//

class ClassLoaderData;

class MetaspaceObj {
 public:
  bool is_metaspace_object() const;
  bool is_shared() const;
  void print_address_on(outputStream* st) const;  // nonvirtual address printing

#define METASPACE_OBJ_TYPES_DO(f) \
  f(Unknown) \
  f(Class) \
  f(Symbol) \
  f(TypeArrayU1) \
  f(TypeArrayU2) \
  f(TypeArrayU4) \
  f(TypeArrayU8) \
  f(TypeArrayOther) \
  f(Method) \
  f(ConstMethod) \
  f(MethodData) \
  f(ConstantPool) \
  f(ConstantPoolCache) \
  f(Annotation) \
  f(MethodCounters) \
  f(Deallocated)

#define METASPACE_OBJ_TYPE_DECLARE(name) name ## Type,
#define METASPACE_OBJ_TYPE_NAME_CASE(name) case name ## Type: return #name;

  enum Type {
    // Types are MetaspaceObj::ClassType, MetaspaceObj::SymbolType, etc
    METASPACE_OBJ_TYPES_DO(METASPACE_OBJ_TYPE_DECLARE)
    _number_of_types
  };

  static const char * type_name(Type type) {
    switch(type) {
    METASPACE_OBJ_TYPES_DO(METASPACE_OBJ_TYPE_NAME_CASE)
    default:
      ShouldNotReachHere();
      return NULL;
    }
  }

  static MetaspaceObj::Type array_type(size_t elem_size) {
    switch (elem_size) {
    case 1: return TypeArrayU1Type;
    case 2: return TypeArrayU2Type;
    case 4: return TypeArrayU4Type;
    case 8: return TypeArrayU8Type;
    default:
      return TypeArrayOtherType;
    }
  }

  void* operator new(size_t size, ClassLoaderData* loader_data,
                     size_t word_size, bool read_only,
                     Type type, Thread* thread) throw();
                     // can't use TRAPS from this header file.
  void operator delete(void* p) { ShouldNotCallThis(); }
};

// Base class for classes that constitute name spaces.
// 构成命名空间的类的基类
class AllStatic {
 public:
  AllStatic()  { ShouldNotCallThis(); }
  ~AllStatic() { ShouldNotCallThis(); }
};


//------------------------------Chunk------------------------------------------
// Linked list of raw memory chunks
// 原始内存块链表
class Chunk: CHeapObj<mtChunk> {
  friend class VMStructs;-/

 protected:
  Chunk*       _next;     // Next Chunk in list
  const size_t _len;      // Size of this Chunk
 public:
  void* operator new(size_t size, AllocFailType alloc_failmode, size_t length) throw();
  void  operator delete(void* p);
  Chunk(size_t length);

  enum {
    // default sizes; make them slightly smaller than 2**k to guard against
    // 默认大小;让它们稍微小于2**k，以防止好友系统风格的malloc实现 
    // buddy-system style malloc implementations
#ifdef _LP64
    slack      = 40,            // [RGV] Not sure if this is right, but make it
                                //  不确定这是对的，但是让它是8的倍数
                                //       a multiple of 8.
#else
    slack      = 20,            // suspected sizeof(Chunk) + internal malloc headers
#endif

    tiny_size  =  256  - slack, // Size of first chunk (tiny)
    init_size  =  1*K  - slack, // Size of first chunk (normal aka small)
    medium_size= 10*K  - slack, // Size of medium-sized chunk
    size       = 32*K  - slack, // Default size of an Arena chunk (following the first)
    non_pool_size = init_size + 32 // An initial size which is not one of above
  };

  void chop();                  // Chop this chunk
  void next_chop();             // Chop next chunk
  static size_t aligned_overhead_size(void) { return ARENA_ALIGN(sizeof(Chunk)); }
  static size_t aligned_overhead_size(size_t byte_size) { return ARENA_ALIGN(byte_size); }

  size_t length() const         { return _len;  }
  Chunk* next() const           { return _next;  }
  void set_next(Chunk* n)       { _next = n;  }
  // Boundaries of data area (possibly unused)
  char* bottom() const          { return ((char*) this) + aligned_overhead_size();  }
  char* top()    const          { return bottom() + _len; }
  bool contains(char* p) const  { return bottom() <= p && p <= top(); }

  // Start the chunk_pool cleaner task
  static void start_chunk_pool_cleaner_task();

  static void clean_chunk_pool();
};

//------------------------------Arena------------------------------------------
// Fast allocation of memory
// 快速分配内存
class Arena : public CHeapObj<mtNone> {
protected:
  friend class ResourceMark;
  friend class HandleMark;
  friend class NoHandleMark;
  friend class VMStructs;

  MEMFLAGS    _flags;           // Memory tracking flags

  Chunk *_first;                // First chunk
  Chunk *_chunk;                // current chunk
  char *_hwm, *_max;            // High water mark and max in current chunk  当前区块的高水位和最大水位
  // Get a new Chunk of at least size x
  void* grow(size_t x, AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM);
  size_t _size_in_bytes;        // Size of arena (used for native memory tracking)

  NOT_PRODUCT(static julong _bytes_allocated;) // total #bytes allocated since start
  friend class AllocStats;
  debug_only(void* malloc(size_t size);)
  debug_only(void* internal_malloc_4(size_t x);)
  NOT_PRODUCT(void inc_bytes_allocated(size_t x);)

  void signal_out_of_memory(size_t request, const char* whence) const;

  bool check_for_overflow(size_t request, const char* whence,
      AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM) const {
    if (UINTPTR_MAX - request < (uintptr_t)_hwm) {
      if (alloc_failmode == AllocFailStrategy::RETURN_NULL) {
        return false;
      }
      signal_out_of_memory(request, whence);
    }
    return true;
 }

 public:
  Arena(MEMFLAGS memflag);
  Arena(MEMFLAGS memflag, size_t init_size);
  ~Arena();
  void  destruct_contents();
  char* hwm() const             { return _hwm; }

  // new operators
  void* operator new (size_t size) throw();
  void* operator new (size_t size, const std::nothrow_t& nothrow_constant) throw();

  // dynamic memory type tagging
  // 动态内存类型标记
  void* operator new(size_t size, MEMFLAGS flags) throw();
  void* operator new(size_t size, const std::nothrow_t& nothrow_constant, MEMFLAGS flags) throw();
  void  operator delete(void* p);

  // Fast allocate in the arena.  Common case is: pointer test + increment.
  // 竞技场中快速内存分配。通常情况是：指针 test+增量
  void* Amalloc(size_t x, AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM) {
    assert(is_power_of_2(ARENA_AMALLOC_ALIGNMENT) , "should be a power of 2");
    x = ARENA_ALIGN(x);
    debug_only(if (UseMallocOnly) return malloc(x);)
    if (!check_for_overflow(x, "Arena::Amalloc", alloc_failmode))
      return NULL;
    NOT_PRODUCT(inc_bytes_allocated(x);)
    if (_hwm + x > _max) {
      return grow(x, alloc_failmode);
    } else {
      char *old = _hwm;
      _hwm += x;
      return old;
    }
  }
  // Further assume size is padded out to words
  // 进一步假设大小被填充到词
  void *Amalloc_4(size_t x, AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM) {
    assert( (x&(sizeof(char*)-1)) == 0, "misaligned size" );
    debug_only(if (UseMallocOnly) return malloc(x);)
    if (!check_for_overflow(x, "Arena::Amalloc_4", alloc_failmode))
      return NULL;
    NOT_PRODUCT(inc_bytes_allocated(x);)
    if (_hwm + x > _max) {
      return grow(x, alloc_failmode);
    } else {
      char *old = _hwm;
      _hwm += x;
      return old;
    }
  }

  // Allocate with 'double' alignment. It is 8 bytes on sparc.
  // 使用”双“对齐方式分配。在工作站上是8字节
  // In other cases Amalloc_D() should be the same as Amalloc_4().
  // 在其他情况下，Amalloc_D()应与Amalloc_4()相同
  void* Amalloc_D(size_t x, AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM) {
    assert( (x&(sizeof(char*)-1)) == 0, "misaligned size" );
    debug_only(if (UseMallocOnly) return malloc(x);)
#if defined(SPARC) && !defined(_LP64)
#define DALIGN_M1 7
    size_t delta = (((size_t)_hwm + DALIGN_M1) & ~DALIGN_M1) - (size_t)_hwm;
    x += delta;
#endif
    if (!check_for_overflow(x, "Arena::Amalloc_D", alloc_failmode))
      return NULL;
    NOT_PRODUCT(inc_bytes_allocated(x);)
    if (_hwm + x > _max) {
      return grow(x, alloc_failmode); // grow() returns a result aligned >= 8 bytes.
    } else {
      char *old = _hwm;
      _hwm += x;
#if defined(SPARC) && !defined(_LP64)
      old += delta; // align to 8-bytes
#endif
      return old;
    }
  }

  // Fast delete in area.  Common case is: NOP (except for storage reclaimed)
  //区域内快速删除。通常是：NOP（除了回收的存储）
  void Afree(void *ptr, size_t size) {
#ifdef ASSERT
    if (ZapResourceArea) memset(ptr, badResourceValue, size); // zap freed memory
    if (UseMallocOnly) return;
#endif
    if (((char*)ptr) + size == _hwm) _hwm = (char*)ptr;
  }

  void *Arealloc( void *old_ptr, size_t old_size, size_t new_size,
      AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM);

  // Move contents of this arena into an empty arena
  // 移动这个竞技场的内容到一个空的竞技场 
  Arena *move_contents(Arena *empty_arena);

  // Determine if pointer belongs to this Arena or not.
  // 确定指针是否属于这个竞技场
  bool contains( const void *ptr ) const;

  // Total of all chunks in use (not thread-safe)
  //正在使用的所有块总数（非线程安全）
  size_t used() const;

  // Total # of bytes used
  //被使用的字节总数
  size_t size_in_bytes() const         {  return _size_in_bytes; };
  void set_size_in_bytes(size_t size);

  static void free_malloced_objects(Chunk* chunk, char* hwm, char* max, char* hwm2)  PRODUCT_RETURN;
  static void free_all(char** start, char** end)                                     PRODUCT_RETURN;

private:
  // Reset this Arena to empty, access will trigger grow if necessary
  // 重置当前竞技场为空，有必要时，访问将触发增长
  void   reset(void) {
    _first = _chunk = NULL;
    _hwm = _max = NULL;
    set_size_in_bytes(0);
  }
};

// One of the following macros must be used when allocating
// 从竞技场分配一个数组或对象时，必须使用下面的宏之一
// an array or object from an arena
#define NEW_ARENA_ARRAY(arena, type, size) \
  (type*) (arena)->Amalloc((size) * sizeof(type))

#define REALLOC_ARENA_ARRAY(arena, type, old, old_size, new_size)    \
  (type*) (arena)->Arealloc((char*)(old), (old_size) * sizeof(type), \
                            (new_size) * sizeof(type) )

#define FREE_ARENA_ARRAY(arena, type, old, size) \
  (arena)->Afree((char*)(old), (size) * sizeof(type))

#define NEW_ARENA_OBJ(arena, type) \
  NEW_ARENA_ARRAY(arena, type, 1)


//%note allocation_1
extern char* resource_allocate_bytes(size_t size,
    AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM);
extern char* resource_allocate_bytes(Thread* thread, size_t size,
    AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM);
extern char* resource_reallocate_bytes( char *old, size_t old_size, size_t new_size,
    AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM);
extern void resource_free_bytes( char *old, size_t size );

//----------------------------------------------------------------------
// Base class for objects allocated in the resource area per default.
// 每个默认值在资源区域中分配的对象的基类 
// Optionally, objects may be allocated on the C heap with
//例子，对象可能使用new(ResourceObj::C_HEAP) Foo(...)分配到C堆上或使用new (&arena)分配到竞技场上
// new(ResourceObj::C_HEAP) Foo(...) or in an Arena with new (&arena)
// ResourceObj's can be allocated within other objects, but don't use
// ResourceObj可以被其他对象分配，但不能使用new或delete（分配类型为未知）
// new or delete (allocation_type is unknown).  If new is used to allocate,
// use delete to deallocate.
// 如果分配使用的new，解除分配使用delete
class ResourceObj ALLOCATION_SUPER_CLASS_SPEC {
 public:
  enum allocation_type { STACK_OR_EMBEDDED = 0, RESOURCE_AREA, C_HEAP, ARENA, allocation_mask = 0x3 };
  static void set_allocation_type(address res, allocation_type type) NOT_DEBUG_RETURN;
#ifdef ASSERT
 private:
  // When this object is allocated on stack the new() operator is not
  // 当这个对象被分配到栈上，new操作符没有被调用，但是栈上的垃圾可能看起来想一个有效的分配类型
  // called but garbage on stack may look like a valid allocation_type.
  // Store negated 'this' pointer when new() is called to distinguish cases.
  // 当调用new来区分大小写时，存储将‘this’指针置空
  // Use second array's element for verification value to distinguish garbage.
  // 使用第二个数组的元素作为验证值来区分垃圾
  uintptr_t _allocation_t[2];
  bool is_type_set() const;
 public:
  allocation_type get_allocation_type() const;
  bool allocated_on_stack()    const { return get_allocation_type() == STACK_OR_EMBEDDED; }
  bool allocated_on_res_area() const { return get_allocation_type() == RESOURCE_AREA; }
  bool allocated_on_C_heap()   const { return get_allocation_type() == C_HEAP; }
  bool allocated_on_arena()    const { return get_allocation_type() == ARENA; }
  ResourceObj(); // default construtor
  ResourceObj(const ResourceObj& r); // default copy construtor
  ResourceObj& operator=(const ResourceObj& r); // default copy assignment
  ~ResourceObj();
#endif // ASSERT

 public:
  void* operator new(size_t size, allocation_type type, MEMFLAGS flags) throw();
  void* operator new [](size_t size, allocation_type type, MEMFLAGS flags) throw();
  void* operator new(size_t size, const std::nothrow_t&  nothrow_constant,
      allocation_type type, MEMFLAGS flags) throw();
  void* operator new [](size_t size, const std::nothrow_t&  nothrow_constant,
      allocation_type type, MEMFLAGS flags) throw();

  void* operator new(size_t size, Arena *arena) throw() {
      address res = (address)arena->Amalloc(size);
      DEBUG_ONLY(set_allocation_type(res, ARENA);)
      return res;
  }

  void* operator new [](size_t size, Arena *arena) throw() {
      address res = (address)arena->Amalloc(size);
      DEBUG_ONLY(set_allocation_type(res, ARENA);)
      return res;
  }

  void* operator new(size_t size) throw() {
      address res = (address)resource_allocate_bytes(size);
      DEBUG_ONLY(set_allocation_type(res, RESOURCE_AREA);)
      return res;
  }

  void* operator new(size_t size, const std::nothrow_t& nothrow_constant) throw() {
      address res = (address)resource_allocate_bytes(size, AllocFailStrategy::RETURN_NULL);
      DEBUG_ONLY(if (res != NULL) set_allocation_type(res, RESOURCE_AREA);)
      return res;
  }

  void* operator new [](size_t size) throw() {
      address res = (address)resource_allocate_bytes(size);
      DEBUG_ONLY(set_allocation_type(res, RESOURCE_AREA);)
      return res;
  }

  void* operator new [](size_t size, const std::nothrow_t& nothrow_constant) throw() {
      address res = (address)resource_allocate_bytes(size, AllocFailStrategy::RETURN_NULL);
      DEBUG_ONLY(if (res != NULL) set_allocation_type(res, RESOURCE_AREA);)
      return res;
  }

  void  operator delete(void* p);
  void  operator delete [](void* p);
};

// One of the following macros must be used when allocating an array
// 在分配数组或对象时，必须使用下列宏之一来确定它是否应该驻留在资源区域的C堆中
// or object to determine whether it should reside in the C heap on in
// the resource area.

#define NEW_RESOURCE_ARRAY(type, size)\
  (type*) resource_allocate_bytes((size) * sizeof(type))

#define NEW_RESOURCE_ARRAY_RETURN_NULL(type, size)\
  (type*) resource_allocate_bytes((size) * sizeof(type), AllocFailStrategy::RETURN_NULL)

#define NEW_RESOURCE_ARRAY_IN_THREAD(thread, type, size)\
  (type*) resource_allocate_bytes(thread, (size) * sizeof(type))

#define NEW_RESOURCE_ARRAY_IN_THREAD_RETURN_NULL(thread, type, size)\
  (type*) resource_allocate_bytes(thread, (size) * sizeof(type), AllocFailStrategy::RETURN_NULL)

#define REALLOC_RESOURCE_ARRAY(type, old, old_size, new_size)\
  (type*) resource_reallocate_bytes((char*)(old), (old_size) * sizeof(type), (new_size) * sizeof(type))

#define REALLOC_RESOURCE_ARRAY_RETURN_NULL(type, old, old_size, new_size)\
  (type*) resource_reallocate_bytes((char*)(old), (old_size) * sizeof(type),\
                                    (new_size) * sizeof(type), AllocFailStrategy::RETURN_NULL)

#define FREE_RESOURCE_ARRAY(type, old, size)\
  resource_free_bytes((char*)(old), (size) * sizeof(type))

#define FREE_FAST(old)\
    /* nop */

#define NEW_RESOURCE_OBJ(type)\
  NEW_RESOURCE_ARRAY(type, 1)

#define NEW_RESOURCE_OBJ_RETURN_NULL(type)\
  NEW_RESOURCE_ARRAY_RETURN_NULL(type, 1)

#define NEW_C_HEAP_ARRAY3(type, size, memflags, pc, allocfail)\
  (type*) AllocateHeap((size) * sizeof(type), memflags, pc, allocfail)

#define NEW_C_HEAP_ARRAY2(type, size, memflags, pc)\
  (type*) (AllocateHeap((size) * sizeof(type), memflags, pc))

#define NEW_C_HEAP_ARRAY(type, size, memflags)\
  (type*) (AllocateHeap((size) * sizeof(type), memflags))

#define NEW_C_HEAP_ARRAY2_RETURN_NULL(type, size, memflags, pc)\
  NEW_C_HEAP_ARRAY3(type, (size), memflags, pc, AllocFailStrategy::RETURN_NULL)

#define NEW_C_HEAP_ARRAY_RETURN_NULL(type, size, memflags)\
  NEW_C_HEAP_ARRAY3(type, (size), memflags, CURRENT_PC, AllocFailStrategy::RETURN_NULL)

#define REALLOC_C_HEAP_ARRAY(type, old, size, memflags)\
  (type*) (ReallocateHeap((char*)(old), (size) * sizeof(type), memflags))

#define REALLOC_C_HEAP_ARRAY_RETURN_NULL(type, old, size, memflags)\
  (type*) (ReallocateHeap((char*)(old), (size) * sizeof(type), memflags, AllocFailStrategy::RETURN_NULL))

#define FREE_C_HEAP_ARRAY(type, old, memflags) \
  FreeHeap((char*)(old), memflags)

// allocate type in heap without calling ctor
// 不调用构造函数在堆中分配类型
#define NEW_C_HEAP_OBJ(type, memflags)\
  NEW_C_HEAP_ARRAY(type, 1, memflags)

#define NEW_C_HEAP_OBJ_RETURN_NULL(type, memflags)\
  NEW_C_HEAP_ARRAY_RETURN_NULL(type, 1, memflags)

// deallocate obj of type in heap without calling dtor
// 不调用析够函数在堆中解除分配对象类型
#define FREE_C_HEAP_OBJ(objname, memflags)\
  FreeHeap((char*)objname, memflags);

// for statistics
#ifndef PRODUCT
class AllocStats : StackObj {
  julong start_mallocs, start_frees;
  julong start_malloc_bytes, start_mfree_bytes, start_res_bytes;
 public:
  AllocStats();

  julong num_mallocs();    // since creation of receiver
  julong alloc_bytes();
  julong num_frees();
  julong free_bytes();
  julong resource_bytes();
  void   print();
};
#endif


//------------------------------ReallocMark---------------------------------
// Code which uses REALLOC_RESOURCE_ARRAY should check an associated
// 使用REALLOC_RESOURCE_ARRAY的代码应该检查关联的ReallocMark 
// ReallocMark, which is declared in the same scope as the reallocated
// 该ReallocMark声明在与重新分配的指针相同的作用域中 
// pointer.  Any operation that could __potentially__ cause a reallocation
// should check the ReallocMark.
// 任何可能导致重新分配的操作都应该检查ReallocMark 
class ReallocMark: public StackObj {
protected:
  NOT_PRODUCT(int _nesting;)

public:
  ReallocMark()   PRODUCT_RETURN;
  void check()    PRODUCT_RETURN;
};

// Helper class to allocate arrays that may become large.
// 帮助类分配可能变大的数组 
// Uses the OS malloc for allocations smaller than ArrayAllocatorMallocLimit
// 对于小于ArrayAllocatorMallocLimit的分配使用OS malloc，而对于较大的分配使用映射内存 
// and uses mapped memory for larger allocations.
// Most OS mallocs do something similar but Solaris malloc does not revert
// 大多数操作系统分配做法是相似的，但是对于较大的分配，Solaris分配不会恢复到映射内存
// to mapped memory for large allocations. By default ArrayAllocatorMallocLimit
// 默认ArrayAllocatorMallocLimit是设置的
// is set so that we always use malloc except for Solaris where we set the
// 所以我们总是使用malloc，除了在Solaris中我们设置获取映射内存的设置
// limit to get mapped memory.
template <class E, MEMFLAGS F>
class ArrayAllocator VALUE_OBJ_CLASS_SPEC {
  char* _addr;
  bool _use_malloc;
  size_t _size;
  bool _free_in_destructor;
 public:
  ArrayAllocator(bool free_in_destructor = true) :
    _addr(NULL), _use_malloc(false), _size(0), _free_in_destructor(free_in_destructor) { }

  ~ArrayAllocator() {
    if (_free_in_destructor) {
      free();
    }
  }

  E* allocate(size_t length);
  void free();
};

#endif // SHARE_VM_MEMORY_ALLOCATION_HPP
