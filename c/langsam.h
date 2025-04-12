#ifndef LANGSAM_H
#define LANGSAM_H

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

typedef struct LangsamVM LangsamVM;

typedef bool LangsamBoolean;
typedef int64_t LangsamInteger;
typedef double LangsamFloat;

static_assert(sizeof(void *) >= sizeof(LangsamBoolean),
              "ERROR: sizeof(void*) < sizeof(LangsamBoolean)");

static_assert(sizeof(void *) >= sizeof(LangsamInteger),
              "ERROR: sizeof(void*) < sizeof(LangsamInteger)");

static_assert(sizeof(void *) >= sizeof(LangsamFloat),
              "ERROR: sizeof(void*) < sizeof(LangsamFloat)");

typedef struct LangsamValue LangsamValue;

#define LV LangsamValue

struct LangsamT {
  const char *name;
  LV (*cast)(LangsamVM *vm, LV other);
  LV (*eq)(LangsamVM *vm, LV self, LV other);
  LV (*cmp)(LangsamVM *vm, LV self, LV other);
  LV (*add)(LangsamVM *vm, LV self, LV other);
  LV (*sub)(LangsamVM *vm, LV self, LV other);
  LV (*mul)(LangsamVM *vm, LV self, LV other);
  LV (*div)(LangsamVM *vm, LV self, LV other);
  LV (*get)(LangsamVM *vm, LV self, LV key);
  LV (*put)(LangsamVM *vm, LV self, LV key, LV value);
  LV (*len)(LangsamVM *vm, LV self);
  LV (*deref)(LangsamVM *vm, LV self);
  LV (*call)(LangsamVM *vm, LV self, LV args);
  LV (*eval)(LangsamVM *vm, LV self);
  LV (*str)(LangsamVM *vm, LV self);
  LV (*repr)(LangsamVM *vm, LV self);
};

typedef struct LangsamT *LangsamType;

LV langsam_cast(LangsamVM *vm, LV type, LV other);
LV langsam_eq(LangsamVM *vm, LV self, LV other);
LV langsam_cmp(LangsamVM *vm, LV self, LV other);
LV langsam_add(LangsamVM *vm, LV self, LV other);
LV langsam_sub(LangsamVM *vm, LV self, LV other);
LV langsam_mul(LangsamVM *vm, LV self, LV other);
LV langsam_div(LangsamVM *vm, LV self, LV other);
LV langsam_get(LangsamVM *vm, LV self, LV key);
LV langsam_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_len(LangsamVM *vm, LV self);
LV langsam_deref(LangsamVM *vm, LV self);
LV langsam_call(LangsamVM *vm, LV self, LV args);
LV langsam_eval(LangsamVM *vm, LV self);
LV langsam_repr(LangsamVM *vm, LV self);
LV langsam_str(LangsamVM *vm, LV self);

extern const LangsamType LANGSAM_TYPE_TYPE;

extern const LangsamType LANGSAM_TYPE_NIL;
extern const LangsamType LANGSAM_TYPE_ERROR;
extern const LangsamType LANGSAM_TYPE_BOOLEAN;
extern const LangsamType LANGSAM_TYPE_INTEGER;
extern const LangsamType LANGSAM_TYPE_FLOAT;
extern const LangsamType LANGSAM_TYPE_STRING;
extern const LangsamType LANGSAM_TYPE_SYMBOL;
extern const LangsamType LANGSAM_TYPE_CONS;
extern const LangsamType LANGSAM_TYPE_VECTOR;
extern const LangsamType LANGSAM_TYPE_MAP;
extern const LangsamType LANGSAM_TYPE_FUNCTION;
extern const LangsamType LANGSAM_TYPE_NATIVEFN;

struct LangsamValue {
  LangsamType type;
  union {
    LangsamBoolean b;
    LangsamInteger i;
    LangsamFloat f;
    void *p;
  };
};

#define LVEQ(v1, v2) (memcmp(&(v1), &(v2), sizeof(LV)))

typedef struct {
  LV payload;
} LangsamError;

typedef struct {
  char *p;
  int len;
} LangsamString;

typedef struct {
  char sigil;
  LangsamString *name;
} LangsamSymbol;

typedef struct {
  LV car;
  LV cdr;
} LangsamCons;

typedef struct {
  LV *items;
  int len;
} LangsamVector;

typedef struct {
  LV *buckets;
  int len;
} LangsamMap;

typedef struct {
  LV name;
  LV params;
  LV restname;
  bool evalargs;
  bool evalresult;
  LV body;
} LangsamFunction;

typedef LangsamValue (*LangsamNativeFn)(LangsamVM *vm, LV args);

// Type

LV langsam_Type_call(LangsamVM *vm, LV self, LV args);
LV langsam_Type_repr(LangsamVM *vm, LV self);

LV langsam_type(LangsamVM *vm, LangsamType t);

// Nil

LV langsam_Nil_cast(LangsamVM *vm, LV other);
LV langsam_Nil_repr(LangsamVM *vm, LV self);

extern const LV langsam_nil;

bool langsam_nilp(LV v);

// Error

LV langsam_Error_cast(LangsamVM *vm, LV other);
LV langsam_Error_deref(LangsamVM *vm, LV self);
LV langsam_Error_repr(LangsamVM *vm, LV self);
LV langsam_Error_str(LangsamVM *vm, LV self);

bool langsam_errorp(LV v);
bool langsam_exceptionp(LV v);

LV langsam_errorf(LangsamVM *vm, const char *fmt, ...);
LV langsam_exceptionf(LangsamVM *vm, const char *kind, const char *fmt, ...);

// Boolean

LV langsam_Boolean_cast(LangsamVM *vm, LV other);
LV langsam_Boolean_repr(LangsamVM *vm, LV self);

extern const LV langsam_true;
extern const LV langsam_false;

LV langsam_boolean(LangsamVM *vm, LangsamBoolean b);

bool langsam_truep(LV v);
bool langsam_falsep(LV v);

// Integer

LV langsam_Integer_cast(LangsamVM *vm, LV other);
LV langsam_Integer_cmp(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_add(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_sub(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_mul(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_div(LangsamVM *vm, LV self, LV other);
LV langsam_Integer_repr(LangsamVM *vm, LV self);

LV langsam_integer(LangsamVM *vm, LangsamInteger i);

// Float

LV langsam_Float_cast(LangsamVM *vm, LV other);
LV langsam_Float_cmp(LangsamVM *vm, LV self, LV other);
LV langsam_Float_add(LangsamVM *vm, LV self, LV other);
LV langsam_Float_sub(LangsamVM *vm, LV self, LV other);
LV langsam_Float_mul(LangsamVM *vm, LV self, LV other);
LV langsam_Float_div(LangsamVM *vm, LV self, LV other);
LV langsam_Float_repr(LangsamVM *vm, LV self);

LV langsam_float(LangsamVM *vm, LangsamFloat f);

// String

LV langsam_String_cast(LangsamVM *vm, LV other);
LV langsam_String_cmp(LangsamVM *vm, LV self, LV other);
LV langsam_String_add(LangsamVM *vm, LV self, LV other);
LV langsam_String_get(LangsamVM *vm, LV self, LV key);
LV langsam_String_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_String_len(LangsamVM *vm, LV self);
LV langsam_String_repr(LangsamVM *vm, LV self);
LV langsam_String_str(LangsamVM *vm, LV self);

LV langsam_string(LangsamVM *vm, const char *s);
LV langsam_format(LangsamVM *vm, const char *fmt, ...);

// Symbol

LV langsam_Symbol_cast(LangsamVM *vm, LV other);
LV langsam_Symbol_eq(LangsamVM *vm, LV self, LV other);
LV langsam_Symbol_eval(LangsamVM *vm, LV self);
LV langsam_Symbol_repr(LangsamVM *vm, LV self);

LV langsam_symbol(LangsamVM *vm, const char *name);

// Cons

LV langsam_Cons_cast(LangsamVM *vm, LV other);
LV langsam_Cons_eq(LangsamVM *vm, LV self, LV other);
LV langsam_Cons_get(LangsamVM *vm, LV self, LV key);
LV langsam_Cons_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_Cons_eval(LangsamVM *vm, LV self);
LV langsam_Cons_repr(LangsamVM *vm, LV self);

LV langsam_cons(LangsamVM *vm, LV car, LV cdr);

bool langsam_consp(LV v);

LV langsam_car(LV cons);
LV langsam_cdr(LV cons);

LV langsam_setcar(LV cons, LV value);
LV langsam_setcdr(LV cons, LV value);

// List

LV langsam_List_cast(LangsamVM *vm, LV other);
LV langsam_List_eq(LangsamVM *vm, LV self, LV other);
LV langsam_List_len(LangsamVM *vm, LV self);
LV langsam_List_eval(LangsamVM *vm, LV self);
LV langsam_List_repr(LangsamVM *vm, LV self);

LV langsam_list(LangsamVM *vm, LV cons);

// Vector

LV langsam_Vector_cast(LangsamVM *vm, LV other);
LV langsam_Vector_eq(LangsamVM *vm, LV self, LV other);
LV langsam_Vector_add(LangsamVM *vm, LV self, LV other);
LV langsam_Vector_get(LangsamVM *vm, LV self, LV key);
LV langsam_Vector_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_Vector_len(LangsamVM *vm, LV self);
LV langsam_Vector_call(LangsamVM *vm, LV self, LV args);
LV langsam_Vector_eval(LangsamVM *vm, LV self);
LV langsam_Vector_repr(LangsamVM *vm, LV self);

LV langsam_vector(LangsamVM *vm, int len);

// Map

LV langsam_Map_cast(LangsamVM *vm, LV other);
LV langsam_Map_eq(LangsamVM *vm, LV self, LV other);
LV langsam_Map_add(LangsamVM *vm, LV self, LV other);
LV langsam_Map_get(LangsamVM *vm, LV self, LV key);
LV langsam_Map_put(LangsamVM *vm, LV self, LV key, LV value);
LV langsam_Map_len(LangsamVM *vm, LV self);
LV langsam_Map_call(LangsamVM *vm, LV self, LV args);
LV langsam_Map_eval(LangsamVM *vm, LV self);
LV langsam_Map_repr(LangsamVM *vm, LV self);

LV langsam_map(LangsamVM *vm, int len);

// Function

LV langsam_Function_cast(LangsamVM *vm, LV other);
LV langsam_Function_call(LangsamVM *vm, LV self, LV args);
LV langsam_Function_repr(LangsamVM *vm, LV self);

typedef struct {
  void *self;
  void *(*realloc)(void *self, void *ptr, size_t size);
} LangsamAllocator;

void *langsam_alloc(LangsamVM *vm, size_t size);
void *langsam_calloc(LangsamVM *vm, size_t size);
void *langsam_realloc(LangsamVM *vm, void *ptr, size_t size);
void langsam_free(LangsamVM *vm, void *ptr);

typedef enum {
  LANGSAM_GC_WHITE,
  LANGSAM_GC_BLACK,
  LANGSAM_GC_IGNORE,
} LangsamGCColor;

void *langsam_gc_alloc(LangsamVM *vm, size_t size);
void *langsam_gc_calloc(LangsamVM *vm, size_t size);
void *langsam_gc_realloc(LangsamVM *vm, void *ptr, size_t size);
void langsam_gc_free(LangsamVM *vm, void *ptr);

struct LangsamVM {
  LangsamAllocator allocator;
  LV strings;
  LV gcobjects;
  LangsamGCColor gcmarkcolor;
  LV rootlet;
  LV curlet;
};

typedef enum {
  LANGSAM_OK,
  LANGSAM_IMPORT_ERROR,
} LangsamStatus;

typedef LV (*LangsamImportFn)(LangsamVM *vm);

void langsam_register_module(const char *name, LangsamImportFn import);

LangsamStatus langsam_init(LangsamVM *vm);
void langsam_close(LangsamVM *vm);

void langsam_defvalue(LangsamVM *vm, const char *name, LV value);
void langsam_defnative(LangsamVM *vm, const char *name, LangsamNativeFn fn);
void langsam_defspecial(LangsamVM *vm, const char *name, LangsamNativeFn fn);

LV langsam_sublet(LangsamVM *vm, int len);

char *langsam_cstr(LangsamVM *vm, LV v);
char *langsam_crepr(LangsamVM *vm, LV v);

LV langsam_loadstring(LangsamVM *vm, const char *s);
LV langsam_loadfile(LangsamVM *vm, const char *path);
LV langsam_loadfd(LangsamVM *vm, int fd);

#endif // LANGSAM_H
