#ifndef LANGSAM_H
#define LANGSAM_H

#include <float.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

// core types

typedef struct LangsamVM LangsamVM;

#if INTPTR_MAX == LLONG_MAX
typedef long long LangsamSize;
#define LANGSAM_INTEGER_MIN LLONG_MIN
#define LANGSAM_INTEGER_MAX LLONG_MAX
#define LANGSAM_INTEGER_FMT "%lld"
#elif INTPTR_MAX == LONG_MAX
typedef long LangsamSize;
#define LANGSAM_INTEGER_MIN LONG_MIN
#define LANGSAM_INTEGER_MAX LONG_MAX
#define LANGSAM_INTEGER_FMT "%ld"
#else
#error "Cannot find a suitable type for LangsamSize"
#endif

typedef LangsamSize LangsamIndex;

typedef LangsamSize LangsamBoolean;
typedef LangsamSize LangsamInteger;

#if UINTPTR_MAX == 0xfffffffful
typedef float LangsamFloat;
#define LANGSAM_FLOAT_MANT_DIG FLT_MANT_DIG
#define LANGSAM_FLOAT_FMT "%f"
#elif UINTPTR_MAX == 0xffffffffffffffffull
typedef double LangsamFloat;
#define LANGSAM_FLOAT_MANT_DIG DBL_MANT_DIG
#define LANGSAM_FLOAT_FMT "%f"
#else
#error "Cannot find a suitable type for LangsamFloat"
#endif

#if UINTPTR_MAX == 0xfffffffful
typedef uint32_t LangsamHash;
#elif UINTPTR_MAX == 0xffffffffffffffffull
typedef uint64_t LangsamHash;
#else
#error "Cannot find a suitable type for LangsamHash"
#endif

#define LANGSAM_SIZEOF(x) ((LangsamSize)(sizeof(x)))

typedef struct LangsamValue LangsamValue;

#define LV LangsamValue

struct LangsamT {
  char *typename;
  bool gcmanaged;
  LangsamSize (*gcmark)(LangsamVM *vm, void *p);
  LangsamSize (*gcfree)(LangsamVM *vm, void *p);
  bool (*truthy)(LangsamVM *vm, LV self);
  LangsamHash (*hash)(LangsamVM *vm, LV self, LangsamHash prevhash);
  LV (*cast)(LangsamVM *vm, LV other);
  LV (*clone)(LangsamVM *vm, LV self);
  LV (*equal)(LangsamVM *vm, LV self, LV other);
  LV (*cmp)(LangsamVM *vm, LV self, LV other);
  LV (*add)(LangsamVM *vm, LV self, LV other);
  LV (*sub)(LangsamVM *vm, LV self, LV other);
  LV (*mul)(LangsamVM *vm, LV self, LV other);
  LV (*div)(LangsamVM *vm, LV self, LV other);
  LV (*mod)(LangsamVM *vm, LV self, LV other);
  LV (*get)(LangsamVM *vm, LV self, LV key);
  LV (*put)(LangsamVM *vm, LV self, LV key, LV value);
  LV (*del)(LangsamVM *vm, LV self, LV key);
  LV (*len)(LangsamVM *vm, LV self);
  LV (*iter)(LangsamVM *vm, LV self);
  LV (*deref)(LangsamVM *vm, LV self);
  LV (*invoke)(LangsamVM *vm, LV self, LV args);
  LV (*eval)(LangsamVM *vm, LV self);
  LV (*name)(LangsamVM *vm, LV self);
  LV (*repr)(LangsamVM *vm, LV self);
  LV (*str)(LangsamVM *vm, LV self);
};

typedef struct LangsamT *LangsamType;

bool langsam_truthy(LangsamVM *vm, LV self);
LangsamHash langsam_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_cast(LangsamVM *vm, LV type, LV other);
LV langsam_clone(LangsamVM *vm, LV self);
LV langsam_equal(LangsamVM *vm, LV self, LV other);
LV langsam_cmp(LangsamVM *vm, LV self, LV other);
LV langsam_add(LangsamVM *vm, LV self, LV other);
LV langsam_sub(LangsamVM *vm, LV self, LV other);
LV langsam_mul(LangsamVM *vm, LV self, LV other);
LV langsam_div(LangsamVM *vm, LV self, LV other);
LV langsam_mod(LangsamVM *vm, LV self, LV other);
LV langsam_get(LangsamVM *vm, LV self, LV key);
LV langsam_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_del(LangsamVM *vm, LV self, LV key);
LV langsam_len(LangsamVM *vm, LV self);
LV langsam_iter(LangsamVM *vm, LV self);
LV langsam_deref(LangsamVM *vm, LV self);
LV langsam_invoke(LangsamVM *vm, LV self, LV args);
LV langsam_eval(LangsamVM *vm, LV self);
LV langsam_name(LangsamVM *vm, LV self);
LV langsam_repr(LangsamVM *vm, LV self);
LV langsam_str(LangsamVM *vm, LV self);

extern const LangsamType LT_TYPE;

extern const LangsamType LT_NIL;
extern const LangsamType LT_EXCEPTION;
extern const LangsamType LT_BOOLEAN;
extern const LangsamType LT_INTEGER;
extern const LangsamType LT_FLOAT;
extern const LangsamType LT_STRING;
extern const LangsamType LT_SYMBOL;
extern const LangsamType LT_KEYWORD;
extern const LangsamType LT_OPWORD;
extern const LangsamType LT_CONS;
extern const LangsamType LT_VECTOR;
extern const LangsamType LT_VECTORSLICE;
extern const LangsamType LT_STRINGSLICE;
extern const LangsamType LT_MAP;
extern const LangsamType LT_FUNCTION;

extern const LangsamType LT_CONSITERATOR;
extern const LangsamType LT_VECTORITERATOR;
extern const LangsamType LT_VECTORSLICEITERATOR;
extern const LangsamType LT_MAPITERATOR;

struct LangsamValue {
  LangsamType type;
  union {
    LangsamBoolean b;
    LangsamInteger i;
    LangsamFloat f;
    void *p;
  };
};

#define LVCMP(v1, v2) (memcmp(&(v1), &(v2), sizeof(LV)))
#define LVEQ(v1, v2) (LVCMP(v1, v2) == 0)

typedef struct {
  LV payload;
} LangsamException;

typedef struct {
  char *p;
  LangsamSize len;
} LangsamString;

typedef struct {
  LV s;
  LangsamIndex start;
  LangsamSize len;
} LangsamStringSlice;

typedef struct {
  LV car;
  LV cdr;
} LangsamCons;

typedef struct {
  LV cur;
} LangsamConsIterator;

typedef struct {
  LV *items;
  LangsamSize len;
} LangsamVector;

typedef struct {
  LV v;
  LangsamIndex i;
} LangsamVectorIterator;

typedef struct {
  LV v;
  LangsamIndex start;
  LangsamSize len;
} LangsamVectorSlice;

typedef struct {
  LV s;
  LangsamIndex i;
} LangsamVectorSliceIterator;

typedef struct {
  LV *buckets;
  LangsamSize nbuckets;
  LangsamSize nitems;
  LangsamFloat loadfactor;
  LV proto;
} LangsamMap;

typedef struct {
  LV m;
  LV items;
} LangsamMapIterator;

typedef LV (*LangsamNativeFn)(LangsamVM *vm, LV args);

typedef struct {
  LV name;
  LV params;
  LV doc;
  LV funclet;
  LV body;
  LangsamNativeFn fn;
  bool evalargs;
  bool evalresult;
} LangsamFunction;

// Type

LangsamHash langsam_Type_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_Type_invoke(LangsamVM *vm, LV self, LV args);
LV langsam_Type_repr(LangsamVM *vm, LV self);

LV langsam_type(LangsamType t);
char *langsam_ctypename(LangsamVM *vm, LangsamType t);

// Nil

bool langsam_Nil_truthy(LangsamVM *vm, LV self);
LangsamHash langsam_Nil_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_Nil_len(LangsamVM *vm, LV self);
LV langsam_Nil_repr(LangsamVM *vm, LV self);

extern const LV langsam_nil;

bool langsam_nilp(LV v);
bool langsam_somep(LV v);

// Exception

LangsamSize langsam_Exception_gcmark(LangsamVM *vm, void *p);
LangsamHash langsam_Exception_hash(LangsamVM *vm, LV self,
                                   LangsamHash prevhash);
LV langsam_Exception_cast(LangsamVM *vm, LV other);
LV langsam_Exception_deref(LangsamVM *vm, LV self);
LV langsam_Exception_repr(LangsamVM *vm, LV self);
LV langsam_Exception_str(LangsamVM *vm, LV self);

LV langsam_exception(LangsamVM *vm, LV payload);
LV langsam_exceptionf(LangsamVM *vm, char *kind, const char *fmt, ...);

bool langsam_exceptionp(LV v);
bool langsam_exceptionpk(LangsamVM *vm, LV v, char *kind);

#define LANGSAM_CHECK(result)                                                  \
  {                                                                            \
    LV checked_value = result;                                                 \
    if (langsam_exceptionp(checked_value)) {                                   \
      return checked_value;                                                    \
    }                                                                          \
  }

// Boolean

bool langsam_Boolean_truthy(LangsamVM *vm, LV self);
LangsamHash langsam_Boolean_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_Boolean_cast(LangsamVM *vm, LV other);
LV langsam_Boolean_repr(LangsamVM *vm, LV self);

extern const LV langsam_true;
extern const LV langsam_false;

LV langsam_boolean(LangsamBoolean b);

bool langsam_truep(LV v);
bool langsam_falsep(LV v);

// Integer

LangsamHash langsam_Integer_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_Integer_cast(LangsamVM *vm, LV other);
LV langsam_Integer_cmp(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_add(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_sub(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_mul(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_div(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_mod(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_repr(LangsamVM *vm, LV self);

LV langsam_integer(LangsamInteger i);

// Float

LangsamHash langsam_Float_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_Float_cast(LangsamVM *vm, LV other);
LV langsam_Float_cmp(LangsamVM *vm, LV self, LV other);
LV langsam_Float_add(LangsamVM *vm, LV self, LV other);
LV langsam_Float_sub(LangsamVM *vm, LV self, LV other);
LV langsam_Float_mul(LangsamVM *vm, LV self, LV other);
LV langsam_Float_div(LangsamVM *vm, LV self, LV other);
LV langsam_Float_mod(LangsamVM *vm, LV self, LV other);
LV langsam_Float_repr(LangsamVM *vm, LV self);

LV langsam_float(LangsamFloat f);

// String

LangsamSize langsam_String_gcmark(LangsamVM *vm, void *p);
LangsamSize langsam_String_gcfree(LangsamVM *vm, void *p);
LangsamHash langsam_String_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_String_cast(LangsamVM *vm, LV other);
LV langsam_String_cmp(LangsamVM *vm, LV self, LV other);
LV langsam_String_add(LangsamVM *vm, LV self, LV other);
LV langsam_String_get(LangsamVM *vm, LV self, LV key);
LV langsam_String_len(LangsamVM *vm, LV self);
LV langsam_String_repr(LangsamVM *vm, LV self);
LV langsam_String_str(LangsamVM *vm, LV self);

LV langsam_string(LangsamVM *vm, const char *s);
LV langsam_stringn(LangsamVM *vm, const char *s, LangsamSize len);

LV langsam_string_wrap(LangsamVM *vm, char *s);
LV langsam_stringn_wrap(LangsamVM *vm, char *s, LangsamSize len);

LV langsam_istring(LangsamVM *vm, char *s);
LV langsam_istringn(LangsamVM *vm, char *s, LangsamSize len);

LV langsam_vformat(LangsamVM *vm, const char *fmt, va_list args);
LV langsam_format(LangsamVM *vm, const char *fmt, ...);

// StringSlice

LangsamSize langsam_StringSlice_gcmark(LangsamVM *vm, void *p);
LangsamHash langsam_StringSlice_hash(LangsamVM *vm, LV self,
                                     LangsamHash prevhash);
LV langsam_StringSlice_cast(LangsamVM *vm, LV other);
LV langsam_StringSlice_cmp(LangsamVM *vm, LV self, LV other);
LV langsam_StringSlice_add(LangsamVM *vm, LV self, LV other);
LV langsam_StringSlice_get(LangsamVM *vm, LV self, LV key);
LV langsam_StringSlice_len(LangsamVM *vm, LV self);
LV langsam_StringSlice_repr(LangsamVM *vm, LV self);
LV langsam_StringSlice_str(LangsamVM *vm, LV self);

LV langsam_stringslice(LangsamVM *vm, LV string, LangsamIndex start,
                       LangsamSize len);

// Symbol

LV langsam_Symbol_cast(LangsamVM *vm, LV other);
LV langsam_Symbol_eval(LangsamVM *vm, LV self);
LV langsam_Symbol_name(LangsamVM *vm, LV self);
LV langsam_Symbol_repr(LangsamVM *vm, LV self);

LV langsam_symbol(LangsamVM *vm, char *name);
LV langsam_symboln(LangsamVM *vm, char *name, LangsamSize len);

// Keyword

LV langsam_Keyword_cast(LangsamVM *vm, LV other);
LV langsam_Keyword_invoke(LangsamVM *vm, LV self, LV args);
LV langsam_Keyword_name(LangsamVM *vm, LV self);
LV langsam_Keyword_repr(LangsamVM *vm, LV self);

LV langsam_keyword(LangsamVM *vm, char *name);

// Opword

LV langsam_Opword_cast(LangsamVM *vm, LV other);
LV langsam_Opword_name(LangsamVM *vm, LV self);
LV langsam_Opword_repr(LangsamVM *vm, LV self);

LV langsam_opword(LangsamVM *vm, char *name);

// Cons

LangsamSize langsam_Cons_gcmark(LangsamVM *vm, void *p);
LangsamHash langsam_Cons_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_Cons_cast(LangsamVM *vm, LV other);
LV langsam_Cons_equal(LangsamVM *vm, LV self, LV other);
LV langsam_Cons_get(LangsamVM *vm, LV self, LV key);
LV langsam_Cons_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_Cons_len(LangsamVM *vm, LV self);
LV langsam_Cons_iter(LangsamVM *vm, LV self);
LV langsam_Cons_deref(LangsamVM *vm, LV self);
LV langsam_Cons_invoke(LangsamVM *vm, LV self, LV args);
LV langsam_Cons_eval(LangsamVM *vm, LV self);
LV langsam_Cons_repr(LangsamVM *vm, LV self);

LV langsam_cons(LangsamVM *vm, LV car, LV cdr);

bool langsam_consp(LV v);

#define langsam_car(cons) (((LangsamCons *)cons.p)->car)
#define langsam_cdr(cons) (((LangsamCons *)cons.p)->cdr)

#define langsam_setcar(cons, newcar) (langsam_car(cons) = newcar)
#define langsam_setcdr(cons, newcdr) (langsam_cdr(cons) = newcdr)

LV langsam_nreverse(LV cons);
LV langsam_nreverse_with_last(LV cons, LV last);

LangsamSize langsam_ConsIterator_gcmark(LangsamVM *vm, void *p);
LV langsam_ConsIterator_deref(LangsamVM *vm, LV self);
LV langsam_ConsIterator_invoke(LangsamVM *vm, LV self, LV args);

// Vector

LangsamSize langsam_Vector_gcmark(LangsamVM *vm, void *p);
LangsamSize langsam_Vector_gcfree(LangsamVM *vm, void *p);
LangsamHash langsam_Vector_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_Vector_cast(LangsamVM *vm, LV other);
LV langsam_Vector_clone(LangsamVM *vm, LV self);
LV langsam_Vector_equal(LangsamVM *vm, LV self, LV other);
LV langsam_Vector_add(LangsamVM *vm, LV self, LV other);
LV langsam_Vector_get(LangsamVM *vm, LV self, LV key);
LV langsam_Vector_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_Vector_len(LangsamVM *vm, LV self);
LV langsam_Vector_iter(LangsamVM *vm, LV self);
LV langsam_Vector_invoke(LangsamVM *vm, LV self, LV args);
LV langsam_Vector_eval(LangsamVM *vm, LV self);
LV langsam_Vector_repr(LangsamVM *vm, LV self);

LV langsam_vector_uninitialized(LangsamVM *vm, LangsamSize len);
LV langsam_vector(LangsamVM *vm, LangsamSize len);

LangsamSize langsam_VectorIterator_gcmark(LangsamVM *vm, void *p);
LV langsam_VectorIterator_deref(LangsamVM *vm, LV self);
LV langsam_VectorIterator_invoke(LangsamVM *vm, LV self, LV args);

// VectorSlice

LangsamSize langsam_VectorSlice_gcmark(LangsamVM *vm, void *p);
LangsamHash langsam_VectorSlice_hash(LangsamVM *vm, LV self,
                                     LangsamHash prevhash);
LV langsam_VectorSlice_cast(LangsamVM *vm, LV other);
LV langsam_VectorSlice_equal(LangsamVM *vm, LV self, LV other);
LV langsam_VectorSlice_add(LangsamVM *vm, LV self, LV other);
LV langsam_VectorSlice_get(LangsamVM *vm, LV self, LV key);
LV langsam_VectorSlice_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_VectorSlice_len(LangsamVM *vm, LV self);
LV langsam_VectorSlice_iter(LangsamVM *vm, LV self);
LV langsam_VectorSlice_invoke(LangsamVM *vm, LV self, LV args);
LV langsam_VectorSlice_eval(LangsamVM *vm, LV self);
LV langsam_VectorSlice_repr(LangsamVM *vm, LV self);

LV langsam_vectorslice(LangsamVM *vm, LV vector, LangsamIndex start,
                       LangsamSize len);

LangsamSize langsam_VectorSliceIterator_gcmark(LangsamVM *vm, void *p);
LV langsam_VectorSliceIterator_deref(LangsamVM *vm, LV self);
LV langsam_VectorSliceIterator_invoke(LangsamVM *vm, LV self, LV args);

// Map

LangsamSize langsam_Map_gcmark(LangsamVM *vm, void *p);
LangsamSize langsam_Map_gcfree(LangsamVM *vm, void *p);
LangsamHash langsam_Map_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_Map_cast(LangsamVM *vm, LV other);
LV langsam_Map_clone(LangsamVM *vm, LV self);
LV langsam_Map_equal(LangsamVM *vm, LV self, LV other);
LV langsam_Map_add(LangsamVM *vm, LV self, LV other);
LV langsam_Map_get(LangsamVM *vm, LV self, LV key);
LV langsam_Map_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_Map_del(LangsamVM *vm, LV self, LV key);
LV langsam_Map_len(LangsamVM *vm, LV self);
LV langsam_Map_iter(LangsamVM *vm, LV self);
LV langsam_Map_deref(LangsamVM *vm, LV self);
LV langsam_Map_invoke(LangsamVM *vm, LV self, LV args);
LV langsam_Map_eval(LangsamVM *vm, LV self);
LV langsam_Map_repr(LangsamVM *vm, LV self);

LV langsam_Map_gep(LangsamVM *vm, LV self, LV key);

LV langsam_Map_items(LangsamVM *vm, LV self);
LV langsam_Map_keys(LangsamVM *vm, LV self);
LV langsam_Map_values(LangsamVM *vm, LV self);

LV langsam_Map_getproto(LangsamVM *vm, LV self);
LV langsam_Map_setproto(LangsamVM *vm, LV self, LV proto);

LV langsam_map(LangsamVM *vm, LV proto, LangsamSize nitems);

LangsamSize langsam_MapIterator_gcmark(LangsamVM *vm, void *p);
LV langsam_MapIterator_deref(LangsamVM *vm, LV self);
LV langsam_MapIterator_invoke(LangsamVM *vm, LV self, LV args);

// Function

LangsamSize langsam_Function_gcmark(LangsamVM *vm, void *p);
LangsamHash langsam_Function_hash(LangsamVM *vm, LV self, LangsamHash prevhash);
LV langsam_Function_cast(LangsamVM *vm, LV other);
LV langsam_Function_get(LangsamVM *vm, LV self, LV key);
LV langsam_Function_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_Function_invoke(LangsamVM *vm, LV self, LV args);
LV langsam_Function_name(LangsamVM *vm, LV self);
LV langsam_Function_repr(LangsamVM *vm, LV self);

// core functions and helpers

LV langsam_bind(LangsamVM *vm, LV env, LV lhs, LV rhs);
LV langsam_quote(LangsamVM *vm, LV obj);
LV langsam_quasiquote(LangsamVM *vm, LV obj);
LV langsam_do(LangsamVM *vm, LV forms);
LV langsam_next(LangsamVM *vm, LV it);

#define LANGSAM_ARG(name, args)                                                \
  LV name = langsam_nil;                                                       \
  if (langsam_consp(args)) {                                                   \
    name = langsam_car(args);                                                  \
    args = langsam_cdr(args);                                                  \
  } else {                                                                     \
    return langsam_exceptionf(vm, "syntax", "missing argument: %s", #name);    \
  }

#define LANGSAM_ARG_OPT(name, args)                                            \
  LV name = langsam_nil;                                                       \
  if (langsam_consp(args)) {                                                   \
    name = langsam_car(args);                                                  \
    args = langsam_cdr(args);                                                  \
  }

#define LANGSAM_ARG_TYPE(name, expected_type)                                  \
  if (name.type != expected_type) {                                            \
    return langsam_exceptionf(vm, "type", "argument %s is %s, expected %s",    \
                              #name, langsam_ctypename(vm, name.type),         \
                              langsam_ctypename(vm, expected_type));           \
  }

// allocator

typedef struct {
  void *self;
  void *(*realloc)(void *self, void *ptr, LangsamSize size);
} LangsamAllocator;

void *langsam_alloc(LangsamVM *vm, LangsamSize size);
void *langsam_calloc(LangsamVM *vm, LangsamSize size);
void *langsam_realloc(LangsamVM *vm, void *ptr, LangsamSize size);
void langsam_free(LangsamVM *vm, void *ptr);

// GC

typedef enum {
  LANGSAM_GC_BLACK,
  LANGSAM_GC_WHITE,
} LangsamGCColor;

typedef struct {
  LangsamSize size;
  LangsamType type;
  LangsamGCColor gccolor;
  void *next;
} LangsamGCHeader;

void *langsam_gcalloc(LangsamVM *vm, LangsamType type, LangsamSize size);
LangsamSize langsam_mark(LangsamVM *vm, LV self);
LV langsam_gc(LangsamVM *vm);

// logger

typedef enum {
  LANGSAM_EMERGENCY = 0,
  LANGSAM_ALERT = 1,
  LANGSAM_CRITICAL = 2,
  LANGSAM_ERROR = 3,
  LANGSAM_WARNING = 4,
  LANGSAM_NOTICE = 5,
  LANGSAM_INFO = 6,
  LANGSAM_DEBUG = 7,
} LangsamLogLevel;

void langsam_loglevel(LangsamVM *vm, LangsamLogLevel level);

void langsam_log(LangsamVM *vm, LangsamLogLevel level, const char *fmt, ...);

#define langsam_emergency(vm, fmt, ...)                                        \
  if (vm->loglevel >= LANGSAM_EMERGENCY) {                                     \
    langsam_log(vm, LANGSAM_EMERGENCY, fmt, ##__VA_ARGS__);                    \
  }

#define langsam_alert(vm, fmt, ...)                                            \
  if (vm->loglevel >= LANGSAM_ALERT) {                                         \
    langsam_log(vm, LANGSAM_ALERT, fmt, ##__VA_ARGS__);                        \
  }

#define langsam_critical(vm, fmt, ...)                                         \
  if (vm->loglevel >= LANGSAM_CRITICAL) {                                      \
    langsam_log(vm, LANGSAM_CRITICAL, fmt, ##__VA_ARGS__);                     \
  }

#define langsam_error(vm, fmt, ...)                                            \
  if (vm->loglevel >= LANGSAM_ERROR) {                                         \
    langsam_log(vm, LANGSAM_ERROR, fmt, ##__VA_ARGS__);                        \
  }

#define langsam_warning(vm, fmt, ...)                                          \
  if (vm->loglevel >= LANGSAM_WARNING) {                                       \
    langsam_log(vm, LANGSAM_WARNING, fmt, ##__VA_ARGS__);                      \
  }

#define langsam_notice(vm, fmt, ...)                                           \
  if (vm->loglevel >= LANGSAM_NOTICE) {                                        \
    langsam_log(vm, LANGSAM_NOTICE, fmt, ##__VA_ARGS__);                       \
  }

#define langsam_info(vm, fmt, ...)                                             \
  if (vm->loglevel >= LANGSAM_INFO) {                                          \
    langsam_log(vm, LANGSAM_INFO, fmt, ##__VA_ARGS__);                         \
  }

#define langsam_debug(vm, fmt, ...)                                            \
  if (vm->loglevel >= LANGSAM_DEBUG) {                                         \
    langsam_log(vm, LANGSAM_DEBUG, fmt, ##__VA_ARGS__);                        \
  }

// VM

typedef struct {
  LangsamAllocator *allocator;
} LangsamVMOpts;

typedef struct {
  LV nil;
  LV quote;
  LV quasiquote;
  LV unquote;
  LV unquote_splicing;
  LV cons;
  LV and_;
  LV or_;
  LV pred;
  LV guard;
  LV amp;
  LV opt;
  LV isap;
  LV dot;
  LV deref;
  LV modules;
  LV loaders;
  LV eq;
} LangsamCoreSymbols;

typedef struct {
  LV keys;
  LV name;
  LV params;
  LV doc;
  LV body;
  LV evalargs;
  LV evalresult;
  LV funclet;
  LV marked;
  LV swept;
} LangsamCoreKeywords;

typedef struct {
  LV splice;
} LangsamCoreOpwords;

struct LangsamVM {
  LangsamAllocator *allocator;
  LV strings;
  LangsamCoreSymbols sym;
  LangsamCoreKeywords kw;
  LangsamCoreOpwords op;
  LV *roots;
  int numroots;
  LangsamGCHeader *gcobjects;
  LangsamGCColor gcmarkcolor;
  LV rootlet;
  LV curlet;
  LV *lets;
  int numlets;
  bool repl;
  LangsamLogLevel loglevel;
  int evaldepth;
  int reprdepth;
};

typedef void (*LangsamImportFn)(LangsamVM *vm, LV env);

// LangsamImportFn for langsam core
void langsam_module_langsam_load(LangsamVM *vm, LV env);

LV langsam_init(LangsamVM *vm, LangsamVMOpts *opts);
void langsam_enable_repl_mode(LangsamVM *vm);
void langsam_close(LangsamVM *vm);

LV langsam_pushroot(LangsamVM *vm, LV root);
LV langsam_poproot(LangsamVM *vm);

LV langsam_pushlet(LangsamVM *vm, LV env);
LV langsam_poplet(LangsamVM *vm);

void langsam_def(LangsamVM *vm, LV env, char *name, LV value);
void langsam_defn(LangsamVM *vm, LV env, char *name, LangsamNativeFn fn);
void langsam_defspecial(LangsamVM *vm, LV env, char *name, LangsamNativeFn fn);

// returned string is managed by GC, no need to free
char *langsam_cstr(LangsamVM *vm, LV v);

typedef LV (*ByteReadFunc)(LangsamVM *vm, void *data);

LV langsam_load_bytes(LangsamVM *vm, LV env, ByteReadFunc readbyte,
                      void *readbyte_data);

LV langsam_readstring(LangsamVM *vm, char *s);
LV langsam_readstringn(LangsamVM *vm, char *s, LangsamSize len);

LV langsam_loadstring(LangsamVM *vm, LV env, char *s);
LV langsam_loadstringn(LangsamVM *vm, LV env, char *s, LangsamSize len);

void langsam_register_module_loader(LangsamVM *vm, char *name,
                                    LangsamNativeFn loader);

LV langsam_require(LangsamVM *vm, char *module_name);

#endif // LANGSAM_H
