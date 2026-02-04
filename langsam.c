#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "langsam.h"

#define LANGSAM_MAX_ROOTS 4096
#define LANGSAM_MAX_LETS 256

#define LANGSAM_MAX_EVAL_DEPTH 256
#define LANGSAM_MAX_REPR_DEPTH 16

// hash functions

#if UINTPTR_MAX == 0xfffffffful

#define FNV1A_OFFSET_BASIS UINT32_C(0x811c9dc5)
#define FNV1A_PRIME UINT32_C(0x01000193)

#define FNV1A_NIL UINT32_C(0x321a0a5b)
#define FNV1A_TRUE UINT32_C(0xa9f37ed7)
#define FNV1A_FALSE UINT32_C(0xa3f79810)

#elif UINTPTR_MAX == 0xffffffffffffffffull

#define FNV1A_OFFSET_BASIS UINT64_C(0xcbf29ce484222325)
#define FNV1A_PRIME UINT64_C(0x00000100000001b3)

#define FNV1A_NIL UINT64_C(0x2146ba19257dc6ac)
#define FNV1A_TRUE UINT64_C(0x5b5c98ef514dbfa5)
#define FNV1A_FALSE UINT64_C(0xb5fae2c14238b978)

#else
#error "Cannot find a suitable implementation for value hashing"
#endif

#define HASH_SEED FNV1A_OFFSET_BASIS

static LangsamHash fnv1a_mix(LangsamHash hash, const uint8_t *p, size_t len) {
  for (size_t i = 0; i < len; i++) {
    hash ^= (LangsamHash)p[i];
    hash *= FNV1A_PRIME;
  }
  return hash;
}

static LangsamHash hash_ptr(LangsamHash hash, void *p) {
  uintptr_t u = (uintptr_t)p;
  return fnv1a_mix(hash, (uint8_t *)&u, sizeof(u));
}

static LangsamHash hash_boolean(LangsamHash hash, LangsamBoolean b) {
  return fnv1a_mix(hash, (uint8_t *)&b, sizeof(b));
}
static LangsamHash hash_integer(LangsamHash hash, LangsamInteger i) {
  return fnv1a_mix(hash, (uint8_t *)&i, sizeof(i));
}
static LangsamHash hash_float(LangsamHash hash, LangsamFloat f) {
  return fnv1a_mix(hash, (uint8_t *)&f, sizeof(f));
}
static LangsamHash hash_string(LangsamHash hash, char *s, LangsamSize len) {
  return fnv1a_mix(hash, (uint8_t *)s, (size_t)len);
}

// core API

bool langsam_truthy(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  return t->truthy ? t->truthy(vm, self) : true;
}

LangsamHash langsam_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  LangsamType t = self.type;
  hash = hash_ptr(hash, t);
  hash = t->hash ? t->hash(vm, self, hash) : hash_ptr(hash, self.p);
  return hash;
}

LV langsam_cast(LangsamVM *vm, LV type, LV other) {
  LangsamType t = type.p;
  if (t == other.type) {
    return other;
  }
  if (t->cast == NULL) {
    char *type_name = langsam_cstr(vm, type);
    char *other_type_name = langsam_ctypename(vm, other.type);
    return langsam_exceptionf(vm, "cast",
                              "cannot cast %s to %s: %s does not "
                              "support cast",
                              other_type_name, type_name, type_name);
  }
  return t->cast(vm, other);
}

LV langsam_equal(LangsamVM *vm, LV self, LV other) {
  bool eq = LVEQ(self, other);
  if (eq) {
    return langsam_true;
  }
  LangsamType t1 = self.type;
  LangsamType t2 = other.type;
  if (t1 != t2) {
    return langsam_false;
  }
  if (t1->equal) {
    return t1->equal(vm, self, other);
  }
  if (t1->cmp) {
    LV cmp_result = t1->cmp(vm, self, other);
    LANGSAM_CHECK(cmp_result);
    return langsam_boolean(cmp_result.i == 0);
  }
  return eq ? langsam_true : langsam_false;
}

LV langsam_cmp(LangsamVM *vm, LV self, LV other) {
  if (LVEQ(self, other)) {
    return langsam_integer(0);
  }
  LangsamType t1 = self.type;
  if (t1->cmp == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "cmp", "%s does not support cmp",
                              self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    return langsam_integer(t1 - t2);
  }
  return t1->cmp(vm, self, other);
}

LV langsam_add(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->add == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "add", "%s does not support add",
                              self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    LANGSAM_CHECK(other);
  }
  return t1->add(vm, self, other);
}

LV langsam_sub(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->sub == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "sub", "%s does not support sub",
                              self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    LANGSAM_CHECK(other);
  }
  return t1->sub(vm, self, other);
}

LV langsam_mul(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->mul == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "mul", "%s does not support mul",
                              self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    LANGSAM_CHECK(other);
  }
  return t1->mul(vm, self, other);
}

LV langsam_div(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->div == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "div", "%s does not support div",
                              self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    LANGSAM_CHECK(other);
  }
  return t1->div(vm, self, other);
}

LV langsam_mod(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->mod == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "mod", "%s does not support mod",
                              self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    LANGSAM_CHECK(other);
  }
  return t1->mod(vm, self, other);
}

LV langsam_get(LangsamVM *vm, LV self, LV key) {
  LangsamType t = self.type;
  if (t->get == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "get", "%s does not support get",
                              self_type_name);
  }
  return t->get(vm, self, key);
}

LV langsam_put(LangsamVM *vm, LV self, LV key, LV value) {
  LangsamType t = self.type;
  if (t->put == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "put", "%s does not support put",
                              self_type_name);
  }
  return t->put(vm, self, key, value);
}

LV langsam_del(LangsamVM *vm, LV self, LV key) {
  LangsamType t = self.type;
  if (t->del == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "del", "%s does not support del",
                              self_type_name);
  }
  return t->del(vm, self, key);
}

LV langsam_len(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->len == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "len", "%s does not support len",
                              self_type_name);
  }
  return t->len(vm, self);
}

LV langsam_iter(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->iter == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "iter", "%s does not support iter",
                              self_type_name);
  }
  return t->iter(vm, self);
}

LV langsam_deref(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->deref == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "deref", "%s does not support deref",
                              self_type_name);
  }
  return t->deref(vm, self);
}

LV langsam_invoke(LangsamVM *vm, LV self, LV args) {
  LangsamType t = self.type;
  if (t->invoke == NULL) {
    char *self_type_name = langsam_ctypename(vm, self.type);
    return langsam_exceptionf(vm, "invoke", "%s does not support invoke",
                              self_type_name);
  }
  return t->invoke(vm, self, args);
}

LV langsam_eval(LangsamVM *vm, LV self) {
  if (vm->evaldepth == LANGSAM_MAX_EVAL_DEPTH) {
    return langsam_exceptionf(vm, "eval", "infinite recursion");
  }
  vm->evaldepth++;
  LANGSAM_CHECK(langsam_pushroot(vm, self));
  LangsamType t = self.type;
  LV result = t->eval ? t->eval(vm, self) : self;
  langsam_debug(vm, "EVAL: %s -> %s", langsam_cstr(vm, self),
                langsam_cstr(vm, result));
  langsam_poproot(vm);
  vm->evaldepth--;
  return result;
}

LV langsam_repr(LangsamVM *vm, LV self) {
  if (vm->reprdepth == LANGSAM_MAX_REPR_DEPTH) {
    return langsam_exceptionf(vm, "repr", "infinite recursion");
  }
  LangsamType t = self.type;
  if (t->repr) {
    vm->reprdepth++;
    LV result = t->repr(vm, self);
    vm->reprdepth--;
    LANGSAM_CHECK(result);
    return langsam_str(vm, result);
  }
  char *self_type_name = langsam_ctypename(vm, self.type);
  return langsam_format(vm, "%s", self_type_name);
}

LV langsam_str(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->str) {
    return t->str(vm, self);
  }
  return langsam_repr(vm, self);
}

// Type

LangsamHash langsam_Type_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  return hash_ptr(hash, self.p);
}

LV langsam_Type_invoke(LangsamVM *vm, LV self, LV args) {
  if (langsam_nilp(args)) {
    return langsam_exceptionf(vm, "syntax", "%s constructor with no arguments",
                              langsam_cstr(vm, self));
  }
  LV tail = langsam_cdr(args);
  if (langsam_consp(tail)) {
    return langsam_exceptionf(vm, "syntax",
                              "%s constructor requires a single argument",
                              langsam_cstr(vm, self));
  }
  LV arg = langsam_car(args);
  arg = langsam_eval(vm, arg);
  LANGSAM_CHECK(arg);
  return langsam_cast(vm, self, arg);
}

LV langsam_Type_repr(LangsamVM *vm, LV self) {
  LangsamType t = self.p;
  return langsam_format(vm, "%s", t->name);
}

LV langsam_type(LangsamType t) { return (LV){.type = LT_TYPE, .p = t}; }

char *langsam_ctypename(LangsamVM *vm, LangsamType t) {
  return langsam_cstr(vm, langsam_type(t));
}

static struct LangsamT LANGSAM_T_TYPE = {
    .name = "Type",
    .gcmanaged = false,
    .hash = langsam_Type_hash,
    .invoke = langsam_Type_invoke,
    .repr = langsam_Type_repr,
};

const LangsamType LT_TYPE = &LANGSAM_T_TYPE;

// Nil

bool langsam_Nil_truthy(LangsamVM *vm, LV self) { return false; }

LangsamHash langsam_Nil_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  return hash_integer(hash, (LangsamInteger)FNV1A_NIL);
}

LV langsam_Nil_repr(LangsamVM *vm, LV self) {
  return langsam_symbol(vm, "nil");
}

static struct LangsamT LANGSAM_T_NIL = {
    .name = "Nil",
    .gcmanaged = false,
    .truthy = langsam_Nil_truthy,
    .hash = langsam_Nil_hash,
    .iter = langsam_Cons_iter,
    .repr = langsam_Nil_repr,
};

const LangsamType LT_NIL = &LANGSAM_T_NIL;

const LV langsam_nil = {.type = LT_NIL, .p = NULL};

bool langsam_nilp(LV v) { return v.type == LT_NIL; }
bool langsam_somep(LV v) { return v.type != LT_NIL; }

// Exception

LangsamSize langsam_Exception_gcmark(LangsamVM *vm, void *p) {
  LangsamException *ex = p;
  return langsam_mark(vm, ex->payload);
}

LangsamHash langsam_Exception_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  LangsamException *ex = self.p;
  return langsam_hash(vm, ex->payload, hash);
}

LV langsam_Exception_cast(LangsamVM *vm, LV other) {
  LangsamException *ex =
      langsam_gcalloc(vm, LT_EXCEPTION, LANGSAM_SIZEOF(LangsamException));
  ex->payload = other;
  return (LV){
      .type = LT_EXCEPTION,
      .p = ex,
  };
}

LV langsam_Exception_deref(LangsamVM *vm, LV self) {
  LangsamException *ex = self.p;
  return ex->payload;
}

LV langsam_Exception_repr(LangsamVM *vm, LV self) {
  LangsamException *ex = self.p;
  return langsam_repr(vm, ex->payload);
}

LV langsam_Exception_str(LangsamVM *vm, LV self) {
  LangsamException *ex = self.p;
  return langsam_str(vm, ex->payload);
}

LV langsam_exception(LangsamVM *vm, LV payload) {
  return langsam_Exception_cast(vm, payload);
}

LV langsam_exceptionf(LangsamVM *vm, char *kind, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  LV error_message = langsam_vformat(vm, fmt, args);
  va_end(args);
  LANGSAM_CHECK(error_message);
  LV payload = langsam_cons(vm, langsam_symbol(vm, kind), error_message);
  return langsam_exception(vm, payload);
}

bool langsam_exceptionp(LV v) { return (v.type == LT_EXCEPTION); }

bool langsam_exceptionpk(LangsamVM *vm, LV v, char *kind) {
  if (!langsam_exceptionp(v)) {
    return false;
  }
  LangsamException *ex = v.p;
  if (!langsam_consp(ex->payload)) {
    return false;
  }
  LV ex_kind = langsam_car(ex->payload);
  LV in_kind = langsam_symbol(vm, kind);
  return LVEQ(ex_kind, in_kind);
}

static struct LangsamT LANGSAM_T_EXCEPTION = {
    .name = "Exception",
    .gcmanaged = true,
    .gcmark = langsam_Exception_gcmark,
    .hash = langsam_Exception_hash,
    .cast = langsam_Exception_cast,
    .deref = langsam_Exception_deref,
    .repr = langsam_Exception_repr,
    .str = langsam_Exception_str,
};

const LangsamType LT_EXCEPTION = &LANGSAM_T_EXCEPTION;

// Boolean

bool langsam_Boolean_truthy(LangsamVM *vm, LV self) { return self.b; }

LangsamHash langsam_Boolean_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  return hash_boolean(hash, self.b);
}

LV langsam_Boolean_cast(LangsamVM *vm, LV other) {
  return langsam_boolean(langsam_truthy(vm, other));
}

LV langsam_Boolean_repr(LangsamVM *vm, LV self) {
  if (self.b) {
    return langsam_istring(vm, "true");
  } else {
    return langsam_istring(vm, "false");
  }
}

LV langsam_boolean(LangsamBoolean b) {
  return (LV){.type = LT_BOOLEAN, .b = b};
}

static struct LangsamT LANGSAM_T_BOOLEAN = {
    .name = "Boolean",
    .gcmanaged = false,
    .hash = langsam_Boolean_hash,
    .cast = langsam_Boolean_cast,
    .truthy = langsam_Boolean_truthy,
    .repr = langsam_Boolean_repr,
};

const LangsamType LT_BOOLEAN = &LANGSAM_T_BOOLEAN;

const LV langsam_true = {.type = LT_BOOLEAN, .b = true};
const LV langsam_false = {.type = LT_BOOLEAN, .b = false};

bool langsam_truep(LV v) { return v.type == LT_BOOLEAN && v.b; }
bool langsam_falsep(LV v) { return v.type == LT_BOOLEAN && !v.b; }

// Integer

bool langsam_Integer_truthy(LangsamVM *vm, LV self) { return self.i != 0; }

LangsamHash langsam_Integer_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  return hash_integer(hash, self.i);
}

LV langsam_Integer_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_NIL) {
    return langsam_integer(0);
  } else if (other.type == LT_BOOLEAN) {
    return langsam_integer(other.b ? 1 : 0);
  } else if (other.type == LT_FLOAT) {
    LangsamFloat f = other.f;
    if (isnan(f) || isinf(f) || (f < (LangsamFloat)LANGSAM_INTEGER_MIN) ||
        (f > (LangsamFloat)LANGSAM_INTEGER_MAX)) {
      return langsam_exceptionf(vm, "cast", "Float->Integer conversion failed");
    }
#if INTPTR_MAX == LLONG_MAX
    long long result = llround(f);
#elif INTPTR_MAX == LONG_MAX
    long result = lround(f);
#else
#error "Cannot find a suitable implementation for Float->Integer conversion"
#endif
    return langsam_integer(result);
  } else if (other.type == LT_STRING) {
    LangsamString *s = other.p;
    LangsamInteger i;
    errno = 0;
    char *endptr;
#if INTPTR_MAX == LLONG_MAX
    i = strtoll(s->p, &endptr, 0);
#elif INTPTR_MAX == LONG_MAX
    i = strtol(s->p, &endptr, 0);
#else
#error "Cannot find a suitable implementation for String->Integer conversion"
#endif
    if (endptr == s->p || errno != 0) {
      return langsam_exceptionf(
          vm, "cast", "String->Integer conversion failed for: %s", s->p);
    }
    return langsam_integer(i);
  }
  return langsam_exceptionf(vm, "cast", "Cannot cast %s to Integer",
                            langsam_ctypename(vm, other.type));
}

LV langsam_Integer_cmp(LangsamVM *vm, LV self, LV other) {
  if (self.i < other.i) {
    return langsam_integer(-1);
  } else if (self.i > other.i) {
    return langsam_integer(1);
  } else {
    return langsam_integer(0);
  }
}

LV langsam_Integer_add(LangsamVM *vm, LV self, LV other) {
  return langsam_integer(self.i + other.i);
}

LV langsam_Integer_sub(LangsamVM *vm, LV self, LV other) {
  return langsam_integer(self.i - other.i);
}

LV langsam_Integer_mul(LangsamVM *vm, LV self, LV other) {
  return langsam_integer(self.i * other.i);
}

LV langsam_Integer_div(LangsamVM *vm, LV self, LV other) {
  if (other.i == 0) {
    return langsam_exceptionf(vm, "div", "division by zero");
  }
  return langsam_integer(self.i / other.i);
}

LV langsam_Integer_mod(LangsamVM *vm, LV self, LV other) {
  if (other.i == 0) {
    return langsam_exceptionf(vm, "mod", "modulo by zero");
  }
  return langsam_integer(self.i % other.i);
}

LV langsam_Integer_repr(LangsamVM *vm, LV self) {
  return langsam_format(vm, LANGSAM_INTEGER_FMT, self.i);
}

LV langsam_integer(LangsamInteger i) {
  return (LV){.type = LT_INTEGER, .i = i};
}

static struct LangsamT LANGSAM_T_INTEGER = {
    .name = "Integer",
    .gcmanaged = false,
    .truthy = langsam_Integer_truthy,
    .hash = langsam_Integer_hash,
    .cast = langsam_Integer_cast,
    .cmp = langsam_Integer_cmp,
    .add = langsam_Integer_add,
    .sub = langsam_Integer_sub,
    .mul = langsam_Integer_mul,
    .div = langsam_Integer_div,
    .mod = langsam_Integer_mod,
    .repr = langsam_Integer_repr,
};

const LangsamType LT_INTEGER = &LANGSAM_T_INTEGER;

// Float

bool langsam_Float_truthy(LangsamVM *vm, LV self) { return self.f != 0.0; }

LangsamHash langsam_Float_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  return hash_float(hash, self.f);
}

LV langsam_Float_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_NIL) {
    return langsam_float(0);
  } else if (other.type == LT_BOOLEAN) {
    return langsam_float(other.b ? 1 : 0);
  } else if (other.type == LT_INTEGER) {
    const LangsamFloat limit = (LangsamFloat)(1ULL << LANGSAM_FLOAT_MANT_DIG);
    LangsamInteger i = other.i;
    LangsamFloat f = (LangsamFloat)i;
    if (f > limit || f < -limit) {
      return langsam_exceptionf(vm, "cast", "Integer->Float conversion failed");
    }
    return langsam_float(f);
  } else if (other.type == LT_STRING) {
    LangsamString *s = other.p;
    LangsamFloat f;
    errno = 0;
    char *endptr;
#if LANGSAM_FLOAT_MANT_DIG == FLT_MANT_DIG
    f = strtof(s->p, &endptr);
#elif LANGSAM_FLOAT_MANT_DIG == DBL_MANT_DIG
    f = strtod(s->p, &endptr);
#else
#error "Cannot find a suitable implementation for String->Float conversion"
#endif
    if (endptr == s->p || errno != 0) {
      return langsam_exceptionf(
          vm, "cast", "String->Float conversion failed for: %s", s->p);
    }
    return langsam_float(f);
  }
  return langsam_exceptionf(vm, "cast", "Cannot cast %s to Float",
                            langsam_ctypename(vm, other.type));
}

LV langsam_Float_cmp(LangsamVM *vm, LV self, LV other) {
  if (self.f < other.f) {
    return langsam_integer(-1);
  } else if (self.f > other.f) {
    return langsam_integer(1);
  } else {
    return langsam_integer(0);
  }
}

LV langsam_Float_add(LangsamVM *vm, LV self, LV other) {
  return langsam_float(self.f + other.f);
}

LV langsam_Float_sub(LangsamVM *vm, LV self, LV other) {
  return langsam_float(self.f - other.f);
}

LV langsam_Float_mul(LangsamVM *vm, LV self, LV other) {
  return langsam_float(self.f * other.f);
}

LV langsam_Float_div(LangsamVM *vm, LV self, LV other) {
  if (other.f == 0) {
    return langsam_exceptionf(vm, "div", "division by zero");
  }
  return langsam_float(self.f / other.f);
}

LV langsam_Float_mod(LangsamVM *vm, LV self, LV other) {
  if (other.f == 0) {
    return langsam_exceptionf(vm, "mod", "modulo by zero");
  }
#if LANGSAM_FLOAT_MANT_DIG == FLT_MANT_DIG
  return langsam_float(fmodf(self.f, other.f));
#elif LANGSAM_FLOAT_MANT_DIG == DBL_MANT_DIG
  return langsam_float(fmod(self.f, other.f));
#else
#error "Cannot find a suitable implementation for Float modulo operation"
#endif
}

LV langsam_Float_repr(LangsamVM *vm, LV self) {
  return langsam_format(vm, "%.17g", self.f);
}

LV langsam_float(LangsamFloat f) { return (LV){.type = LT_FLOAT, .f = f}; }

static struct LangsamT LANGSAM_T_FLOAT = {
    .name = "Float",
    .gcmanaged = false,
    .truthy = langsam_Float_truthy,
    .hash = langsam_Float_hash,
    .cast = langsam_Float_cast,
    .cmp = langsam_Float_cmp,
    .add = langsam_Float_add,
    .sub = langsam_Float_sub,
    .mul = langsam_Float_mul,
    .div = langsam_Float_div,
    .mod = langsam_Float_mod,
    .repr = langsam_Float_repr,
};

const LangsamType LT_FLOAT = &LANGSAM_T_FLOAT;

// String

LangsamSize langsam_String_gcmark(LangsamVM *vm, void *p) {
  LangsamString *s = p;
  return s->len + 1;
}

LangsamSize langsam_String_gcfree(LangsamVM *vm, void *p) {
  LangsamString *s = p;
  langsam_free(vm, s->p);
  return s->len + 1;
}

bool langsam_String_truthy(LangsamVM *vm, LV self) {
  LangsamString *s = self.p;
  return s->len != 0;
}

LangsamHash langsam_String_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  LangsamString *s = self.p;
  return hash_string(hash, s->p, s->len);
}

LV langsam_String_cast(LangsamVM *vm, LV other) {
  return langsam_str(vm, other);
}

LV langsam_String_cmp(LangsamVM *vm, LV self, LV other) {
  LangsamString *s1 = (LangsamString *)self.p;
  LangsamString *s2 = (LangsamString *)other.p;
  if (s1->len < s2->len) {
    return langsam_integer(-1);
  }
  if (s1->len > s2->len) {
    return langsam_integer(1);
  }
  size_t cmplen = (size_t)s1->len;
  return langsam_integer(memcmp(s1->p, s2->p, cmplen));
}

LV langsam_String_add(LangsamVM *vm, LV self, LV other) {
  LangsamString *s1 = self.p;
  LangsamString *s2 = other.p;
  LangsamSize len = s1->len + s2->len;
  char *p = langsam_alloc(vm, len + 1);
  memcpy(p, s1->p, (size_t)s1->len);
  memcpy(p + s1->len, s2->p, (size_t)s2->len);
  p[len] = 0;
  return langsam_stringn_wrap(vm, p, len);
}

LV langsam_String_get(LangsamVM *vm, LV self, LV key) {
  if (key.type != LT_INTEGER) {
    char *key_type_name = langsam_ctypename(vm, key.type);
    return langsam_exceptionf(vm, "get", "attempt to index String with %s",
                              key_type_name);
  }
  LangsamIndex index = key.i;
  LangsamString *s = self.p;
  if (index >= s->len) {
    return langsam_exceptionf(vm, "get",
                              "String index " LANGSAM_INTEGER_FMT
                              " out of range (0.." LANGSAM_INTEGER_FMT ")",
                              index, s->len - 1);
  }
  return langsam_integer(s->p[index]);
}

static LangsamSize string_length(LV self) {
  LangsamString *s = self.p;
  return s->len;
}

LV langsam_String_len(LangsamVM *vm, LV self) {
  return langsam_integer(string_length(self));
}

static int charreprwidth(char c) {
  if (c == '\\')
    return 2;
  if (c == '"')
    return 2;
  if (c >= 0x7f)
    return 4;
  if (c >= 0x20)
    return 1;
  if (c == '\a')
    return 2;
  if (c == '\b')
    return 2;
  if (c == '\f')
    return 2;
  if (c == '\n')
    return 2;
  if (c == '\r')
    return 2;
  if (c == '\t')
    return 2;
  if (c == '\v')
    return 2;
  return 4;
}

static int hexdigit(int nybble) {
  if (nybble < 10) {
    return '0' + nybble;
  } else {
    return 'a' + (nybble - 10);
  }
}

static char *writecharrepr(char *p, char c) {
  if (c == '\\' || c == '"') {
    *p++ = '\\';
    *p++ = c;
  } else if (c >= 0x7f) {
    *p++ = '\\';
    *p++ = 'x';
    *p++ = (char)(hexdigit(c >> 4));
    *p++ = (char)(hexdigit(c & 0x0f));
  } else if (c >= 0x20) {
    *p++ = c;
  } else if (c == '\a') {
    *p++ = '\\';
    *p++ = 'a';
  } else if (c == '\b') {
    *p++ = '\\';
    *p++ = 'b';
  } else if (c == '\f') {
    *p++ = '\\';
    *p++ = 'f';
  } else if (c == '\n') {
    *p++ = '\\';
    *p++ = 'n';
  } else if (c == '\r') {
    *p++ = '\\';
    *p++ = 'r';
  } else if (c == '\t') {
    *p++ = '\\';
    *p++ = 't';
  } else if (c == '\v') {
    *p++ = '\\';
    *p++ = 'v';
  } else {
    *p++ = '\\';
    *p++ = 'x';
    *p++ = (char)(hexdigit(c >> 4));
    *p++ = (char)(hexdigit(c & 0x0f));
  }
  return p;
}

LV langsam_String_repr(LangsamVM *vm, LV self) {
  LangsamString *s = self.p;
  LangsamSize reprlen = 2;
  for (LangsamSize i = 0; i < s->len; i++) {
    reprlen += charreprwidth(s->p[i]);
  }
  char *p = langsam_alloc(vm, reprlen + 1);
  char *dst = p;
  *dst++ = '"';
  for (LangsamSize i = 0; i < s->len; i++) {
    dst = writecharrepr(dst, s->p[i]);
  }
  *dst++ = '"';
  *dst = 0;
  return langsam_stringn_wrap(vm, p, reprlen);
}

LV langsam_String_str(LangsamVM *vm, LV self) { return self; }

LV langsam_string(LangsamVM *vm, const char *s) {
  LangsamSize len = (LangsamSize)strlen(s);
  return langsam_stringn(vm, s, len);
}

LV langsam_stringn(LangsamVM *vm, const char *s, LangsamSize len) {
  char *p = langsam_alloc(vm, len + 1);
  memcpy(p, s, (size_t)len);
  p[len] = 0;
  return langsam_stringn_wrap(vm, p, len);
}

LV langsam_string_wrap(LangsamVM *vm, char *p) {
  LangsamSize len = (LangsamSize)strlen(p);
  return langsam_stringn_wrap(vm, p, len);
}

LV langsam_stringn_wrap(LangsamVM *vm, char *p, LangsamSize len) {
  LangsamString *s =
      langsam_gcalloc(vm, LT_STRING, LANGSAM_SIZEOF(LangsamString));
  s->p = p;
  s->len = len;
  return (LV){
      .type = LT_STRING,
      .p = s,
  };
}

static LV intern_stringn(LangsamVM *vm, char *s, LangsamSize len) {
  LV vs = langsam_stringn(vm, s, len);
  langsam_put(vm, vm->strings, vs, vs);
  return vs;
}

LV langsam_istring(LangsamVM *vm, char *s) {
  LangsamSize len = (LangsamSize)strlen(s);
  return langsam_istringn(vm, s, len);
}

LV langsam_istringn(LangsamVM *vm, char *s, LangsamSize len) {
  LangsamString stmp = {
      .p = s,
      .len = len,
  };
  LV vtmp = {
      .type = LT_STRING,
      .p = &stmp,
  };
  LV vs = langsam_get(vm, vm->strings, vtmp);
  if (langsam_somep(vs)) {
    return vs;
  }
  return intern_stringn(vm, s, len);
}

LV langsam_vformat(LangsamVM *vm, const char *fmt, va_list args) {
  char *result;
  int len = vasprintf(&result, fmt, args);
  if (len == -1) {
    return langsam_exceptionf(vm, "format",
                              "vasprintf failed with format string: %s", fmt);
  }
  LV vs = langsam_stringn(vm, result, len);
  free(result);
  return vs;
}

LV langsam_format(LangsamVM *vm, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  LV vs = langsam_vformat(vm, fmt, args);
  va_end(args);
  return vs;
}

char *langsam_cstr(LangsamVM *vm, LV v) {
  LV vs = langsam_str(vm, v);
  LangsamString *s = vs.p;
  return s->p;
}

static struct LangsamT LANGSAM_T_STRING = {
    .name = "String",
    .gcmanaged = true,
    .gcmark = langsam_String_gcmark,
    .gcfree = langsam_String_gcfree,
    .truthy = langsam_String_truthy,
    .hash = langsam_String_hash,
    .cast = langsam_String_cast,
    .cmp = langsam_String_cmp,
    .add = langsam_String_add,
    .get = langsam_String_get,
    .len = langsam_String_len,
    .repr = langsam_String_repr,
    .str = langsam_String_str,
};

const LangsamType LT_STRING = &LANGSAM_T_STRING;

// Symbol

LV langsam_Symbol_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_STRING) {
    LangsamString *s = other.p;
    return langsam_symbol(vm, s->p);
  }
  return langsam_exceptionf(vm, "cast", "Cannot cast %s to Symbol",
                            langsam_ctypename(vm, other.type));
}

LV langsam_Symbol_eval(LangsamVM *vm, LV self) {
  return langsam_get(vm, vm->curlet, self);
}

LV langsam_Symbol_repr(LangsamVM *vm, LV self) {
  return (LV){
      .type = LT_STRING,
      .p = self.p,
  };
}

LV langsam_symbol(LangsamVM *vm, char *name) {
  LangsamSize len = (LangsamSize)strlen(name);
  return langsam_symboln(vm, name, len);
}

LV langsam_symboln(LangsamVM *vm, char *name, LangsamSize len) {
  LV vs = langsam_istringn(vm, name, len);
  vs.type = LT_SYMBOL;
  return vs;
}

static struct LangsamT LANGSAM_T_SYMBOL = {
    .name = "Symbol",
    .gcmanaged = false,
    .hash = langsam_String_hash,
    .cast = langsam_Symbol_cast,
    .eval = langsam_Symbol_eval,
    .repr = langsam_Symbol_repr,
};

const LangsamType LT_SYMBOL = &LANGSAM_T_SYMBOL;

// Keyword

LV langsam_Keyword_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_STRING) {
    LangsamString *s = other.p;
    return langsam_keyword(vm, s->p);
  }
  if (other.type == LT_SYMBOL) {
    return (LV){
        .type = LT_KEYWORD,
        .p = other.p,
    };
  }
  return langsam_exceptionf(vm, "cast", "Cannot cast %s to Keyword",
                            langsam_ctypename(vm, other.type));
}

LV langsam_Keyword_invoke(LangsamVM *vm, LV self, LV args) {
  LANGSAM_ARG(coll, args);
  coll = langsam_eval(vm, coll);
  LANGSAM_CHECK(coll);
  return langsam_get(vm, coll, self);
}

LV langsam_Keyword_repr(LangsamVM *vm, LV self) {
  LangsamString *s = self.p;
  return langsam_format(vm, ":%s", s->p);
}

LV langsam_keyword(LangsamVM *vm, char *name) {
  LV vs = langsam_istring(vm, name);
  vs.type = LT_KEYWORD;
  return vs;
}

static struct LangsamT LANGSAM_T_KEYWORD = {
    .name = "Keyword",
    .gcmanaged = false,
    .hash = langsam_String_hash,
    .cast = langsam_Keyword_cast,
    .invoke = langsam_Keyword_invoke,
    .repr = langsam_Keyword_repr,
};

const LangsamType LT_KEYWORD = &LANGSAM_T_KEYWORD;

// Opword

LV langsam_Opword_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_STRING) {
    LangsamString *s = other.p;
    return langsam_opword(vm, s->p);
  }
  if (other.type == LT_SYMBOL) {
    return (LV){
        .type = LT_OPWORD,
        .p = other.p,
    };
  }
  return langsam_exceptionf(vm, "cast", "Cannot cast %s to Opword",
                            langsam_ctypename(vm, other.type));
}

LV langsam_Opword_repr(LangsamVM *vm, LV self) {
  LangsamString *s = self.p;
  return langsam_format(vm, "%%%s", s->p);
}

LV langsam_opword(LangsamVM *vm, char *name) {
  LV vs = langsam_istring(vm, name);
  vs.type = LT_OPWORD;
  return vs;
}

static struct LangsamT LANGSAM_T_OPWORD = {
    .name = "Opword",
    .gcmanaged = false,
    .hash = langsam_String_hash,
    .cast = langsam_Opword_cast,
    .invoke = langsam_Keyword_invoke,
    .repr = langsam_Opword_repr,
};

const LangsamType LT_OPWORD = &LANGSAM_T_OPWORD;

// Cons

LangsamSize langsam_Cons_gcmark(LangsamVM *vm, void *p) {
  LangsamCons *cons = p;
  LangsamSize total = 0;
  total += langsam_mark(vm, cons->car);
  total += langsam_mark(vm, cons->cdr);
  return total;
}

LangsamHash langsam_Cons_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  LangsamCons *cons = self.p;
  hash = langsam_hash(vm, cons->car, hash);
  hash = langsam_hash(vm, cons->cdr, hash);
  return hash;
}

LV langsam_Cons_cast(LangsamVM *vm, LV other) {
  LV it = langsam_iter(vm, other);
  LANGSAM_CHECK(it);
  LV items = langsam_nil;
  int len = 0;
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    items = langsam_cons(vm, item, items);
    it = langsam_next(vm, it);
    len++;
  }
  if (len < 2) {
    return langsam_exceptionf(
        vm, "cast", "Cons requires an iterable with at least 2 elements");
  }
  return langsam_nreverse_with_last(langsam_cdr(items), langsam_car(items));
}

LV langsam_Cons_equal(LangsamVM *vm, LV self, LV other) {
  LangsamCons *c1 = self.p;
  LangsamCons *c2 = other.p;
  LV careq = langsam_equal(vm, c1->car, c2->car);
  LV cdreq = langsam_equal(vm, c1->cdr, c2->cdr);
  return langsam_boolean(careq.b && cdreq.b);
}

LV langsam_Cons_get(LangsamVM *vm, LV self, LV key) {
  if (key.type != LT_INTEGER) {
    char *key_type_name = langsam_ctypename(vm, key.type);
    return langsam_exceptionf(vm, "get", "attempt to index Cons with %s",
                              key_type_name);
  }
  LangsamCons *cons = self.p;
  switch (key.i) {
  case 0:
    return cons->car;
  case 1:
    return cons->cdr;
  default:
    return langsam_exceptionf(
        vm, "get", "Cons index " LANGSAM_INTEGER_FMT " out of range (0..1)",
        key.i);
  }
}

LV langsam_Cons_put(LangsamVM *vm, LV self, LV key, LV value) {
  if (key.type != LT_INTEGER) {
    char *key_type_name = langsam_ctypename(vm, key.type);
    return langsam_exceptionf(vm, "put", "attempt to index Cons with %s",
                              key_type_name);
  }
  LangsamCons *cons = self.p;
  switch (key.i) {
  case 0:
    cons->car = value;
    break;
  case 1:
    cons->cdr = value;
    break;
  default:
    return langsam_exceptionf(
        vm, "put", "Cons index " LANGSAM_INTEGER_FMT " out of range (0..1)",
        key.i);
  }
  return self;
}

LV langsam_Cons_iter(LangsamVM *vm, LV self) {
  LangsamConsIterator *p =
      langsam_gcalloc(vm, LT_CONSITERATOR, LANGSAM_SIZEOF(LangsamConsIterator));
  p->cur = self;
  return (LV){
      .type = LT_CONSITERATOR,
      .p = p,
  };
}

LV langsam_Cons_deref(LangsamVM *vm, LV self) { return langsam_cdr(self); }

LV langsam_Cons_invoke(LangsamVM *vm, LV self, LV args) {
  LV obj = langsam_car(self);
  LV key = langsam_cdr(self);
  LV method = langsam_get(vm, obj, key);
  LANGSAM_CHECK(method);
  if (langsam_nilp(method)) {
    return langsam_exceptionf(vm, "invoke", "cannot find method `%s` in %s",
                              langsam_cstr(vm, key),
                              langsam_ctypename(vm, obj.type));
  }
  args = langsam_cons(vm, langsam_quote(vm, obj), args);
  return langsam_invoke(vm, method, args);
}

LV langsam_Cons_eval(LangsamVM *vm, LV self) {
  LV head = langsam_car(self);
  LV op = langsam_eval(vm, head);
  LANGSAM_CHECK(op);
  if (langsam_nilp(op)) {
    return langsam_exceptionf(vm, "eval", "unknown operator: %s",
                              langsam_cstr(vm, head));
  }
  return langsam_invoke(vm, op, langsam_cdr(self));
}

LV langsam_Cons_repr(LangsamVM *vm, LV self) {
  LV reprs = langsam_nil;
  LangsamSize total_length = 0;
  LV cur = self;
  while (langsam_consp(cur)) {
    LV repr = langsam_repr(vm, langsam_car(cur));
    LANGSAM_CHECK(repr);
    LV reprlen = langsam_len(vm, repr);
    LANGSAM_CHECK(reprlen);
    if (total_length > 0) {
      total_length++; // separator
    }
    total_length += reprlen.i;
    reprs = langsam_cons(vm, repr, reprs);
    cur = langsam_cdr(cur);
  }
  if (langsam_somep(cur)) {
    total_length += 2;
    reprs = langsam_cons(vm, langsam_istring(vm, "."), reprs);
    LV repr = langsam_repr(vm, cur);
    LANGSAM_CHECK(repr);
    LV reprlen = langsam_len(vm, repr);
    LANGSAM_CHECK(reprlen);
    total_length++;
    total_length += reprlen.i;
    reprs = langsam_cons(vm, repr, reprs);
  }
  LangsamIndex index = total_length + 2 + 1;
  char *result = langsam_alloc(vm, index);
  result[--index] = 0;
  result[--index] = ')';
  for (LV r = reprs; langsam_consp(r); r = langsam_cdr(r)) {
    LV repr = langsam_car(r);
    LangsamString *reprstr = (LangsamString *)repr.p;
    index -= reprstr->len;
    strncpy(result + index, reprstr->p, (size_t)reprstr->len);
    if (index > 1) {
      result[--index] = ' ';
    }
  }
  result[--index] = '(';
  return langsam_stringn_wrap(vm, result, total_length + 2);
}

LV langsam_nreverse_with_last(LV cons, LV last) {
  LV prev = last;
  LV cur = cons;
  while (langsam_consp(cur)) {
    LV next = langsam_cdr(cur);
    langsam_setcdr(cur, prev);
    prev = cur;
    cur = next;
  }
  return prev;
}

LV langsam_nreverse(LV cons) {
  return langsam_nreverse_with_last(cons, langsam_nil);
}

LV langsam_cons(LangsamVM *vm, LV car, LV cdr) {
  LangsamCons *cons = langsam_gcalloc(vm, LT_CONS, LANGSAM_SIZEOF(LangsamCons));
  cons->car = car;
  cons->cdr = cdr;
  return (LV){
      .type = LT_CONS,
      .p = cons,
  };
}

bool langsam_consp(LV v) { return v.type == LT_CONS; }

static struct LangsamT LANGSAM_T_CONS = {
    .name = "Cons",
    .gcmanaged = true,
    .gcmark = langsam_Cons_gcmark,
    .hash = langsam_Cons_hash,
    .cast = langsam_Cons_cast,
    .equal = langsam_Cons_equal,
    .get = langsam_Cons_get,
    .put = langsam_Cons_put,
    .iter = langsam_Cons_iter,
    .deref = langsam_Cons_deref,
    .invoke = langsam_Cons_invoke,
    .eval = langsam_Cons_eval,
    .repr = langsam_Cons_repr,
};

const LangsamType LT_CONS = &LANGSAM_T_CONS;

// ConsIterator

LangsamSize langsam_ConsIterator_gcmark(LangsamVM *vm, void *p) {
  LangsamConsIterator *it = p;
  return langsam_mark(vm, it->cur);
}

bool langsam_ConsIterator_truthy(LangsamVM *vm, LV self) {
  LangsamConsIterator *it = self.p;
  return langsam_consp(it->cur);
}

LV langsam_ConsIterator_deref(LangsamVM *vm, LV self) {
  LangsamConsIterator *it = self.p;
  if (!langsam_consp(it->cur)) {
    return langsam_exceptionf(vm, "deref",
                              "attempt to deref consumed ConsIterator");
  }
  return langsam_car(it->cur);
}

LV langsam_ConsIterator_invoke(LangsamVM *vm, LV self, LV args) {
  LangsamConsIterator *it = self.p;
  if (!langsam_consp(it->cur)) {
    return langsam_exceptionf(vm, "invoke",
                              "attempt to advance consumed ConsIterator");
  }
  it->cur = langsam_cdr(it->cur);
  return self;
}

static struct LangsamT LANGSAM_T_CONSITERATOR = {
    .name = "ConsIterator",
    .gcmanaged = true,
    .gcmark = langsam_ConsIterator_gcmark,
    .truthy = langsam_ConsIterator_truthy,
    .deref = langsam_ConsIterator_deref,
    .invoke = langsam_ConsIterator_invoke,
};

const LangsamType LT_CONSITERATOR = &LANGSAM_T_CONSITERATOR;

// Vector

LangsamSize langsam_Vector_gcmark(LangsamVM *vm, void *p) {
  LangsamVector *v = p;
  LangsamSize total = v->len * LANGSAM_SIZEOF(LV);
  for (LangsamIndex i = 0; i < v->len; i++) {
    total += langsam_mark(vm, v->items[i]);
  }
  return total;
}

LangsamSize langsam_Vector_gcfree(LangsamVM *vm, void *p) {
  LangsamVector *v = p;
  langsam_free(vm, v->items);
  return v->len * LANGSAM_SIZEOF(LV);
}

bool langsam_Vector_truthy(LangsamVM *vm, LV self) {
  LangsamVector *v = self.p;
  return v->items != 0;
}

LangsamHash langsam_Vector_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  LangsamVector *v = self.p;
  for (LangsamIndex i = 0; i < v->len; i++) {
    hash = langsam_hash(vm, v->items[i], hash);
  }
  return hash;
}

LV langsam_Vector_cast(LangsamVM *vm, LV other) {
  LV it = langsam_iter(vm, other);
  LANGSAM_CHECK(it);
  LV l = langsam_nil;
  LangsamSize len = 0;
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    l = langsam_cons(vm, item, l);
    it = langsam_next(vm, it);
    len++;
  }
  LV self = langsam_vector_uninitialized(vm, len);
  LangsamVector *v = self.p;
  for (LangsamIndex i = len - 1; i >= 0; i--) {
    v->items[i] = langsam_car(l);
    l = langsam_cdr(l);
  }
  return self;
}

LV langsam_Vector_equal(LangsamVM *vm, LV self, LV other) {
  LangsamVector *v1 = self.p;
  LangsamVector *v2 = other.p;
  if (v1->len != v2->len) {
    return langsam_false;
  }
  for (LangsamIndex i = 0; i < v1->len; i++) {
    if (langsam_falsep(langsam_equal(vm, v1->items[i], v2->items[i]))) {
      return langsam_false;
    }
  }
  return langsam_true;
}

LV langsam_Vector_add(LangsamVM *vm, LV self, LV other) {
  LangsamVector *v1 = self.p;
  LangsamVector *v2 = other.p;
  LangsamSize len = v1->len + v2->len;
  LV result = langsam_vector_uninitialized(vm, len);
  LV *items = ((LangsamVector *)result.p)->items;
  LangsamIndex index = 0;
  for (LangsamIndex i = 0; i < v1->len; i++) {
    items[index++] = v1->items[i];
  }
  for (LangsamIndex i = 0; i < v2->len; i++) {
    items[index++] = v2->items[i];
  }
  return result;
}

LV langsam_Vector_get(LangsamVM *vm, LV self, LV key) {
  if (key.type != LT_INTEGER) {
    char *key_type_name = langsam_ctypename(vm, key.type);
    return langsam_exceptionf(vm, "get", "attempt to index Vector with %s",
                              key_type_name);
  }
  LangsamIndex index = key.i;
  LangsamVector *v = self.p;
  if (index >= v->len) {
    return langsam_exceptionf(vm, "get",
                              "Vector index " LANGSAM_INTEGER_FMT
                              " out of range (0.." LANGSAM_INTEGER_FMT ")",
                              index, v->len - 1);
  }
  return v->items[index];
}

LV langsam_Vector_put(LangsamVM *vm, LV self, LV key, LV value) {
  if (key.type != LT_INTEGER) {
    char *key_type_name = langsam_ctypename(vm, key.type);
    return langsam_exceptionf(vm, "put", "attempt to index Vector with %s",
                              key_type_name);
  }
  LangsamIndex index = key.i;
  LangsamVector *v = self.p;
  if (index >= v->len) {
    return langsam_exceptionf(vm, "put",
                              "Vector index " LANGSAM_INTEGER_FMT
                              " out of range (0.." LANGSAM_INTEGER_FMT ")",
                              index, v->len - 1);
  }
  v->items[index] = value;
  return self;
}

LV langsam_Vector_len(LangsamVM *vm, LV self) {
  LangsamVector *v = self.p;
  return langsam_integer(v->len);
}

LV langsam_Vector_iter(LangsamVM *vm, LV self) {
  LangsamVectorIterator *p = langsam_gcalloc(
      vm, LT_VECTORITERATOR, LANGSAM_SIZEOF(LangsamVectorIterator));
  p->v = self;
  p->i = 0;
  return (LV){
      .type = LT_VECTORITERATOR,
      .p = p,
  };
}

LV langsam_Vector_invoke(LangsamVM *vm, LV self, LV args) {
  LANGSAM_ARG(index, args);
  index = langsam_eval(vm, index);
  LANGSAM_CHECK(index);
  return langsam_Vector_get(vm, self, index);
}

LV langsam_Vector_eval(LangsamVM *vm, LV self) {
  LangsamVector *v = self.p;
  LV result = langsam_vector_uninitialized(vm, v->len);
  LV *result_items = ((LangsamVector *)result.p)->items;
  for (LangsamIndex i = 0; i < v->len; i++) {
    LV result_item = langsam_eval(vm, v->items[i]);
    LANGSAM_CHECK(result_item);
    result_items[i] = result_item;
  }
  return result;
}

LV langsam_Vector_repr(LangsamVM *vm, LV self) {
  LangsamVector *v = self.p;
  LV reprs = langsam_nil;
  LangsamSize total_length = 0;
  for (int i = 0; i < v->len; i++) {
    LV repr = langsam_repr(vm, v->items[i]);
    LANGSAM_CHECK(repr);
    LV reprlen = langsam_len(vm, repr);
    LANGSAM_CHECK(reprlen);
    if (total_length > 0) {
      total_length++; // separator
    }
    total_length += reprlen.i;
    reprs = langsam_cons(vm, repr, reprs);
  }
  LangsamIndex index = total_length + 2 + 1;
  char *result = langsam_alloc(vm, index);
  result[--index] = 0;
  result[--index] = ']';
  for (LV r = reprs; langsam_consp(r); r = langsam_cdr(r)) {
    LV repr = langsam_car(r);
    char *repr_p = ((LangsamString *)repr.p)->p;
    LangsamSize repr_len = ((LangsamString *)repr.p)->len;
    index -= repr_len;
    strncpy(result + index, repr_p, (size_t)repr_len);
    if (index > 1) {
      result[--index] = ' ';
    }
  }
  result[--index] = '[';
  return langsam_stringn_wrap(vm, result, total_length + 2);
}

LV langsam_vector_uninitialized(LangsamVM *vm, LangsamSize len) {
  LangsamVector *v =
      langsam_gcalloc(vm, LT_VECTOR, LANGSAM_SIZEOF(LangsamVector));
  v->items = langsam_alloc(vm, LANGSAM_SIZEOF(LV) * len);
  v->len = len;
  return (LV){
      .type = LT_VECTOR,
      .p = v,
  };
}

LV langsam_vector(LangsamVM *vm, LangsamSize len) {
  LV result = langsam_vector_uninitialized(vm, len);
  LangsamVector *v = result.p;
  for (LangsamIndex i = 0; i < len; i++) {
    v->items[i] = langsam_nil;
  }
  return result;
}

static struct LangsamT LANGSAM_T_VECTOR = {
    .name = "Vector",
    .gcmanaged = true,
    .gcmark = langsam_Vector_gcmark,
    .gcfree = langsam_Vector_gcfree,
    .truthy = langsam_Vector_truthy,
    .hash = langsam_Vector_hash,
    .cast = langsam_Vector_cast,
    .equal = langsam_Vector_equal,
    .add = langsam_Vector_add,
    .get = langsam_Vector_get,
    .put = langsam_Vector_put,
    .len = langsam_Vector_len,
    .iter = langsam_Vector_iter,
    .invoke = langsam_Vector_invoke,
    .eval = langsam_Vector_eval,
    .repr = langsam_Vector_repr,
};

const LangsamType LT_VECTOR = &LANGSAM_T_VECTOR;

// VectorIterator

LangsamSize langsam_VectorIterator_gcmark(LangsamVM *vm, void *p) {
  LangsamVectorIterator *it = p;
  return langsam_mark(vm, it->v);
}

bool langsam_VectorIterator_truthy(LangsamVM *vm, LV self) {
  LangsamVectorIterator *it = self.p;
  LangsamVector *v = it->v.p;
  return it->i < v->len;
}

LV langsam_VectorIterator_deref(LangsamVM *vm, LV self) {
  LangsamVectorIterator *it = self.p;
  LangsamVector *v = it->v.p;
  if (it->i >= v->len) {
    return langsam_exceptionf(vm, "deref",
                              "attempt to deref consumed VectorIterator");
  }
  return v->items[it->i];
}

LV langsam_VectorIterator_invoke(LangsamVM *vm, LV self, LV args) {
  LangsamVectorIterator *it = self.p;
  LangsamVector *v = it->v.p;
  if (it->i >= v->len) {
    return langsam_exceptionf(vm, "invoke",
                              "attempt to advance consumed VectorIterator");
  }
  it->i++;
  return self;
}

static struct LangsamT LANGSAM_T_VECTORITERATOR = {
    .name = "VectorIterator",
    .gcmanaged = true,
    .gcmark = langsam_VectorIterator_gcmark,
    .truthy = langsam_VectorIterator_truthy,
    .deref = langsam_VectorIterator_deref,
    .invoke = langsam_VectorIterator_invoke,
};

const LangsamType LT_VECTORITERATOR = &LANGSAM_T_VECTORITERATOR;

// Map

#define langsam_MapIteratorItem_k(item) (((LangsamVector *)item.p)->items[0])
#define langsam_MapIteratorItem_v(item) (((LangsamVector *)item.p)->items[1])

LangsamSize langsam_Map_gcmark(LangsamVM *vm, void *p) {
  LangsamMap *m = p;
  LangsamSize total = m->nbuckets * LANGSAM_SIZEOF(LV);
  for (LangsamIndex i = 0; i < m->nbuckets; i++) {
    total += langsam_mark(vm, m->buckets[i]);
  }
  total += langsam_mark(vm, m->proto);
  return total;
}

LangsamSize langsam_Map_gcfree(LangsamVM *vm, void *p) {
  LangsamMap *m = p;
  langsam_free(vm, m->buckets);
  return m->nbuckets * LANGSAM_SIZEOF(LV);
}

bool langsam_Map_truthy(LangsamVM *vm, LV self) {
  LangsamMap *m = self.p;
  return m->nitems != 0;
}

LangsamHash langsam_Map_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  LangsamMap *m = self.p;
  for (LangsamIndex i = 0; i < m->nbuckets; i++) {
    hash = langsam_hash(vm, m->buckets[i], hash);
  }
  return hash;
}

LV langsam_Map_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_INTEGER) {
    return langsam_map(vm, langsam_nil, other.i);
  }
  LV it = langsam_iter(vm, other);
  LANGSAM_CHECK(it);
  LV l = langsam_nil;
  LangsamSize nitems = 0;
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    l = langsam_cons(vm, item, l);
    it = langsam_next(vm, it);
    nitems++;
  }
  LV self = langsam_map(vm, langsam_nil, nitems);
  for (LangsamIndex i = 0; i < nitems; i++) {
    LV item = langsam_car(l);
    LV key = langsam_get(vm, item, langsam_integer(0));
    LANGSAM_CHECK(key);
    LV value = langsam_get(vm, item, langsam_integer(1));
    LANGSAM_CHECK(value);
    langsam_put(vm, self, key, value);
    l = langsam_cdr(l);
  }
  return self;
}

LV langsam_Map_equal(LangsamVM *vm, LV self, LV other) {
  LangsamMap *m1 = self.p;
  LangsamMap *m2 = other.p;
  if (m1->nitems != m2->nitems) {
    return langsam_false;
  }
  LV it = langsam_iter(vm, self);
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    LV k = langsam_MapIteratorItem_k(item);
    LV v1 = langsam_MapIteratorItem_v(item);
    LV v2item = langsam_Map_gep(vm, other, k);
    LANGSAM_CHECK(v2item);
    if (langsam_nilp(v2item)) {
      return langsam_false;
    }
    LV v2 = langsam_cdr(v2item);
    LV eq = langsam_equal(vm, v1, v2);
    LANGSAM_CHECK(eq);
    if (langsam_falsep(eq)) {
      return langsam_false;
    }
    it = langsam_next(vm, it);
  }
  return langsam_true;
}

LV langsam_Map_add(LangsamVM *vm, LV self, LV other) {
  LangsamMap *m1 = self.p;
  LangsamMap *m2 = other.p;
  LangsamSize nitems = m1->nitems + m2->nitems;
  LV result = langsam_map(vm, m1->proto, nitems);
  LV it1 = langsam_iter(vm, self);
  while (langsam_truthy(vm, it1)) {
    LV item = langsam_deref(vm, it1);
    LV k = langsam_MapIteratorItem_k(item);
    LV v = langsam_MapIteratorItem_v(item);
    langsam_put(vm, result, k, v);
    it1 = langsam_next(vm, it1);
  }
  LV it2 = langsam_iter(vm, other);
  while (langsam_truthy(vm, it2)) {
    LV otheritem = langsam_deref(vm, it2);
    LV k = langsam_MapIteratorItem_k(otheritem);
    LV v = langsam_MapIteratorItem_v(otheritem);
    langsam_put(vm, result, k, v);
    it2 = langsam_next(vm, it2);
  }
  return result;
}

static LV langsam_Map_rawgep(LangsamVM *vm, LV self, LV key) {
  LangsamMap *m = self.p;
  LangsamHash hash = langsam_hash(vm, key, HASH_SEED);
  LangsamIndex bucket_index = (LangsamIndex)(hash % (LangsamHash)m->nbuckets);
  LV bucket = m->buckets[bucket_index];
  while (langsam_consp(bucket)) {
    LV item = langsam_car(bucket);
    LV k = langsam_car(item);
    LV eq = langsam_equal(vm, k, key);
    LANGSAM_CHECK(eq);
    if (langsam_truep(eq)) {
      return item;
    }
    bucket = langsam_cdr(bucket);
  }
  return langsam_nil;
}

LV langsam_Map_gep(LangsamVM *vm, LV self, LV key) {
  LV item = langsam_Map_rawgep(vm, self, key);
  if (langsam_somep(item)) {
    return item;
  }
  LV proto = langsam_Map_getproto(vm, self);
  if (proto.type == LT_MAP) {
    return langsam_Map_gep(vm, proto, key);
  }
  return langsam_nil;
}

LV langsam_Map_get(LangsamVM *vm, LV self, LV key) {
  LV item = langsam_Map_gep(vm, self, key);
  LANGSAM_CHECK(item);
  if (langsam_consp(item)) {
    return langsam_cdr(item);
  }
  return langsam_nil;
}

static void resize_map(LangsamVM *vm, LV self, LangsamSize nbuckets) {
  LangsamMap *m = self.p;
  LangsamMap tmp;
  tmp.nbuckets = nbuckets;
  tmp.buckets = langsam_alloc(vm, LANGSAM_SIZEOF(LV) * tmp.nbuckets);
  for (LangsamIndex i = 0; i < tmp.nbuckets; i++) {
    tmp.buckets[i] = langsam_nil;
  }
  tmp.nitems = 0;
  tmp.load_factor = m->load_factor;
  LV newm = (LV){
      .type = LT_MAP,
      .p = &tmp,
  };
  LV it = langsam_iter(vm, self);
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    LV k = langsam_MapIteratorItem_k(item);
    LV v = langsam_MapIteratorItem_v(item);
    langsam_put(vm, newm, k, v);
    it = langsam_next(vm, it);
  }
  langsam_free(vm, m->buckets);
  m->buckets = tmp.buckets;
  m->nbuckets = tmp.nbuckets;
  m->nitems = tmp.nitems;
}

static LangsamInteger next_power_of_two(LangsamInteger n) {
  LangsamInteger p = 1;
  while (p < n) {
    p <<= 1;
  }
  return p;
}

LV langsam_Map_put(LangsamVM *vm, LV self, LV key, LV value) {
  LV item = langsam_Map_rawgep(vm, self, key);
  LANGSAM_CHECK(item);
  if (langsam_consp(item)) {
    langsam_setcdr(item, value);
    return self;
  }
  LangsamMap *m = self.p;
  LangsamSize limit = (LangsamSize)(m->load_factor * (LangsamFloat)m->nbuckets);
  if (m->nitems + 1 > limit) {
    resize_map(vm, self, next_power_of_two(m->nbuckets * 2));
  }
  LangsamHash hash = langsam_hash(vm, key, HASH_SEED);
  LangsamIndex bucket_index = (LangsamIndex)(hash % (LangsamHash)m->nbuckets);
  item = langsam_cons(vm, key, value);
  m->buckets[bucket_index] = langsam_cons(vm, item, m->buckets[bucket_index]);
  m->nitems++;
  return self;
}

LV langsam_Map_del(LangsamVM *vm, LV self, LV key) {
  LangsamMap *m = self.p;
  LangsamHash hash = langsam_hash(vm, key, HASH_SEED);
  LangsamIndex bucket_index = (LangsamIndex)(hash % (LangsamHash)m->nbuckets);
  LV bucket = m->buckets[bucket_index];
  LV prev = langsam_nil;
  while (langsam_consp(bucket)) {
    LV item = langsam_car(bucket);
    LV k = langsam_car(item);
    LV eq = langsam_equal(vm, k, key);
    LANGSAM_CHECK(eq);
    if (langsam_truep(eq)) {
      if (langsam_consp(prev)) {
        langsam_setcdr(prev, langsam_cdr(bucket));
      } else {
        m->buckets[bucket_index] = langsam_cdr(bucket);
      }
      m->nitems--;
      return langsam_nil;
    }
    prev = bucket;
    bucket = langsam_cdr(bucket);
  }
  return langsam_nil;
}

LV langsam_Map_len(LangsamVM *vm, LV self) {
  LangsamMap *m = self.p;
  return langsam_integer(m->nitems);
}

LV langsam_Map_iter(LangsamVM *vm, LV self) {
  LangsamMapIterator *p =
      langsam_gcalloc(vm, LT_MAPITERATOR, LANGSAM_SIZEOF(LangsamMapIterator));
  p->m = self;
  p->items = langsam_Map_items(vm, self);
  return (LV){
      .type = LT_MAPITERATOR,
      .p = p,
  };
}

LV langsam_Map_invoke(LangsamVM *vm, LV self, LV args) {
  LANGSAM_ARG(key, args);
  key = langsam_eval(vm, key);
  LANGSAM_CHECK(key);
  return langsam_Map_get(vm, self, key);
}

LV langsam_Map_eval(LangsamVM *vm, LV self) {
  LangsamMap *m = self.p;
  LV result = langsam_map(vm, m->proto, m->nitems);
  LV it = langsam_iter(vm, self);
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    LV k = langsam_MapIteratorItem_k(item);
    k = langsam_eval(vm, k);
    LANGSAM_CHECK(k);
    LV v = langsam_MapIteratorItem_v(item);
    v = langsam_eval(vm, v);
    LANGSAM_CHECK(v);
    langsam_put(vm, result, k, v);
    it = langsam_next(vm, it);
  }
  return result;
}

LV langsam_Map_repr(LangsamVM *vm, LV self) {
  LV reprs = langsam_nil;
  LangsamSize total_length = 0;
  LV it = langsam_iter(vm, self);
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    LV k = langsam_MapIteratorItem_k(item);
    LV krepr = langsam_repr(vm, k);
    LANGSAM_CHECK(krepr);
    LV kreprlen = langsam_len(vm, krepr);
    LANGSAM_CHECK(kreprlen);
    LV v = langsam_MapIteratorItem_v(item);
    LV vrepr = langsam_repr(vm, v);
    LANGSAM_CHECK(vrepr);
    LV vreprlen = langsam_len(vm, vrepr);
    LANGSAM_CHECK(vreprlen);
    if (total_length > 0) {
      total_length++; // separator
    }
    total_length += kreprlen.i + 1 + vreprlen.i;
    reprs = langsam_cons(vm, krepr, reprs);
    reprs = langsam_cons(vm, vrepr, reprs);
    it = langsam_next(vm, it);
  }
  LangsamIndex index = total_length + 2 + 1;
  char *result = langsam_alloc(vm, index);
  result[--index] = 0;
  result[--index] = '}';
  for (LV r = reprs; langsam_consp(r); r = langsam_cdr(r)) {
    LV repr = langsam_car(r);
    char *repr_p = ((LangsamString *)repr.p)->p;
    LangsamSize repr_len = ((LangsamString *)repr.p)->len;
    index -= repr_len;
    strncpy(result + index, repr_p, (size_t)repr_len);
    if (index > 1) {
      result[--index] = ' ';
    }
  }
  result[--index] = '{';
  return langsam_stringn_wrap(vm, result, total_length + 2);
}

LV langsam_Map_items(LangsamVM *vm, LV self) {
  LV result = langsam_nil;
  LangsamMap *m = self.p;
  for (LangsamIndex i = 0; i < m->nbuckets; i++) {
    LV bucket = m->buckets[i];
    while (langsam_consp(bucket)) {
      LV item = langsam_car(bucket);
      LV v = langsam_vector_uninitialized(vm, 2);
      langsam_put(vm, v, langsam_integer(0), langsam_car(item));
      langsam_put(vm, v, langsam_integer(1), langsam_cdr(item));
      result = langsam_cons(vm, v, result);
      bucket = langsam_cdr(bucket);
    }
  }
  return result;
}

LV langsam_Map_keys(LangsamVM *vm, LV self) {
  LV result = langsam_nil;
  LangsamMap *m = self.p;
  for (LangsamIndex i = 0; i < m->nbuckets; i++) {
    LV bucket = m->buckets[i];
    while (langsam_consp(bucket)) {
      LV item = langsam_car(bucket);
      result = langsam_cons(vm, langsam_car(item), result);
      bucket = langsam_cdr(bucket);
    }
  }
  return result;
}

LV langsam_Map_values(LangsamVM *vm, LV self) {
  LV result = langsam_nil;
  LangsamMap *m = self.p;
  for (LangsamIndex i = 0; i < m->nbuckets; i++) {
    LV bucket = m->buckets[i];
    while (langsam_consp(bucket)) {
      LV item = langsam_car(bucket);
      result = langsam_cons(vm, langsam_cdr(item), result);
      bucket = langsam_cdr(bucket);
    }
  }
  return result;
}

LV langsam_Map_getproto(LangsamVM *vm, LV self) {
  if (self.type != LT_MAP) {
    return langsam_exceptionf(vm, "getproto",
                              "value of %s does not have a prototype",
                              langsam_ctypename(vm, self.type));
  }
  LangsamMap *m = self.p;
  return m->proto;
}

LV langsam_Map_setproto(LangsamVM *vm, LV self, LV proto) {
  if (self.type != LT_MAP) {
    return langsam_exceptionf(vm, "setproto",
                              "value of %s does not have a prototype",
                              langsam_ctypename(vm, self.type));
  }
  if (proto.type != LT_MAP && proto.type != LT_NIL) {
    return langsam_exceptionf(vm, "setproto",
                              "prototype must be Map or Nil, got %s",
                              langsam_ctypename(vm, proto.type));
  }
  LangsamMap *m = self.p;
  m->proto = proto;
  return proto;
}

LV langsam_map(LangsamVM *vm, LV proto, LangsamSize nitems) {
  LangsamSize nbuckets = next_power_of_two(nitems);
  LangsamMap *m = langsam_gcalloc(vm, LT_MAP, LANGSAM_SIZEOF(LangsamMap));
  m->proto = proto;
  m->buckets = langsam_alloc(vm, LANGSAM_SIZEOF(LV) * nbuckets);
  for (LangsamIndex i = 0; i < nbuckets; i++) {
    m->buckets[i] = langsam_nil;
  }
  m->nbuckets = nbuckets;
  m->nitems = 0;
  m->load_factor = 0.75;
  return (LV){
      .type = LT_MAP,
      .p = m,
  };
}

static struct LangsamT LANGSAM_T_MAP = {
    .name = "Map",
    .gcmanaged = true,
    .gcmark = langsam_Map_gcmark,
    .gcfree = langsam_Map_gcfree,
    .truthy = langsam_Map_truthy,
    .hash = langsam_Map_hash,
    .cast = langsam_Map_cast,
    .equal = langsam_Map_equal,
    .add = langsam_Map_add,
    .get = langsam_Map_get,
    .put = langsam_Map_put,
    .del = langsam_Map_del,
    .len = langsam_Map_len,
    .iter = langsam_Map_iter,
    .invoke = langsam_Map_invoke,
    .eval = langsam_Map_eval,
    .repr = langsam_Map_repr,
};

const LangsamType LT_MAP = &LANGSAM_T_MAP;

// MapIterator

LangsamSize langsam_MapIterator_gcmark(LangsamVM *vm, void *p) {
  LangsamMapIterator *it = p;
  LangsamSize total = 0;
  total += langsam_mark(vm, it->m);
  total += langsam_mark(vm, it->items);
  return total;
}

bool langsam_MapIterator_truthy(LangsamVM *vm, LV self) {
  LangsamMapIterator *it = self.p;
  return langsam_consp(it->items);
}

LV langsam_MapIterator_deref(LangsamVM *vm, LV self) {
  LangsamMapIterator *it = self.p;
  if (langsam_nilp(it->items)) {
    return langsam_exceptionf(vm, "deref",
                              "attempt to deref consumed MapIterator");
  }
  return langsam_car(it->items);
}

LV langsam_MapIterator_invoke(LangsamVM *vm, LV self, LV args) {
  LangsamMapIterator *it = self.p;
  if (langsam_nilp(it->items)) {
    return langsam_exceptionf(vm, "invoke",
                              "attempt to advance consumed MapIterator");
  }
  it->items = langsam_cdr(it->items);
  return self;
}

static struct LangsamT LANGSAM_T_MAPITERATOR = {
    .name = "MapIterator",
    .gcmanaged = true,
    .gcmark = langsam_MapIterator_gcmark,
    .truthy = langsam_MapIterator_truthy,
    .deref = langsam_MapIterator_deref,
    .invoke = langsam_MapIterator_invoke,
};

const LangsamType LT_MAPITERATOR = &LANGSAM_T_MAPITERATOR;

// bind

static LV collect_rest(LangsamVM *vm, LV it) {
  LV items = langsam_nil;
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    items = langsam_cons(vm, item, items);
    it = langsam_next(vm, it);
  }
  return langsam_nreverse(items);
}

static bool bind_exceptionp(LangsamVM *vm, LV obj) {
  return langsam_exceptionpk(vm, obj, "bind");
}

static LV bind_quoted(LangsamVM *vm, LV env, LV lhs, LV rhs) {
  LV result = langsam_equal(vm, lhs, rhs);
  LANGSAM_CHECK(result);
  if (langsam_falsep(result)) {
    return langsam_exceptionf(vm, "bind", "literal mismatch: lhs=%s rhs=%s",
                              langsam_cstr(vm, lhs), langsam_cstr(vm, rhs));
  }
  return env;
}

static LV bind_quasiquoted(LangsamVM *vm, LV env, LV lhs, LV rhs);

static LV bind_quasiquoted_seq(LangsamVM *vm, LV env, LV lhs, LV rhs) {
  LV it_lhs = langsam_iter(vm, lhs);
  LANGSAM_CHECK(it_lhs);
  LV it_rhs = langsam_iter(vm, rhs);
  LANGSAM_CHECK(it_rhs);
  LV unquote_splicing_symbol = langsam_symbol(vm, "unquote-splicing");
  while (langsam_truthy(vm, it_lhs)) {
    LV item = langsam_deref(vm, it_lhs);
    if (langsam_consp(item) &&
        LVEQ(langsam_car(item), unquote_splicing_symbol)) {
      LV tail = langsam_cdr(item);
      if (!langsam_consp(tail)) {
        return langsam_exceptionf(vm, "syntax",
                                  "malformed unquote-splicing form: %s",
                                  langsam_cstr(vm, item));
      }
      LV pat = langsam_car(tail);
      LV rhs_items = collect_rest(vm, it_rhs);
      return langsam_bind(vm, env, pat, rhs_items);
    }
    if (!langsam_truthy(vm, it_rhs)) {
      return langsam_exceptionf(vm, "bind",
                                "not enough values on the right side to bind "
                                "quasiquoted seq: lhs=%s rhs=%s",
                                langsam_cstr(vm, lhs), langsam_cstr(vm, rhs));
    }
    LV value = langsam_deref(vm, it_rhs);
    LV bind_result = bind_quasiquoted(vm, env, item, value);
    LANGSAM_CHECK(bind_result);
    it_lhs = langsam_next(vm, it_lhs);
    it_rhs = langsam_next(vm, it_rhs);
  }
  return env;
}

static LV bind_quasiquoted(LangsamVM *vm, LV env, LV lhs, LV rhs) {
  if (lhs.type == LT_CONS) {
    LV head = langsam_car(lhs);
    LV unquote_symbol = langsam_symbol(vm, "unquote");
    if (LVEQ(head, unquote_symbol)) {
      LV tail = langsam_cdr(lhs);
      if (!langsam_consp(tail)) {
        return langsam_exceptionf(vm, "syntax", "malformed unquote form: %s",
                                  langsam_cstr(vm, lhs));
      }
      return langsam_bind(vm, env, langsam_car(tail), rhs);
    }
    if (rhs.type != LT_CONS && rhs.type != LT_NIL) {
      return langsam_exceptionf(
          vm, "bind", "attempt to bind value of %s to `%s",
          langsam_ctypename(vm, rhs.type), langsam_cstr(vm, lhs));
    }
    return bind_quasiquoted_seq(vm, env, lhs, rhs);
  } else if (lhs.type == LT_VECTOR) {
    if (rhs.type != LT_VECTOR) {
      return langsam_exceptionf(
          vm, "bind", "attempt to bind value of %s to `%s",
          langsam_ctypename(vm, rhs.type), langsam_cstr(vm, lhs));
    }
    return bind_quasiquoted_seq(vm, env, lhs, rhs);
  } else {
    return bind_quoted(vm, env, lhs, rhs);
  }
}

static LV bind_cons(LangsamVM *vm, LV env, LV lhs, LV rhs) {
  LV head = langsam_car(lhs);
  LV quote_symbol = langsam_symbol(vm, "quote");
  if (LVEQ(head, quote_symbol)) {
    LV tail = langsam_cdr(lhs);
    if (!langsam_consp(tail)) {
      return langsam_exceptionf(vm, "syntax", "malformed quote form: %s",
                                langsam_cstr(vm, lhs));
    }
    return bind_quoted(vm, env, langsam_car(tail), rhs);
  }
  LV quasiquote_symbol = langsam_symbol(vm, "quasiquote");
  if (LVEQ(head, quasiquote_symbol)) {
    LV tail = langsam_cdr(lhs);
    if (!langsam_consp(tail)) {
      return langsam_exceptionf(vm, "syntax", "malformed quasiquote form: %s",
                                langsam_cstr(vm, lhs));
    }
    return bind_quasiquoted(vm, env, langsam_car(tail), rhs);
  }
  LV cons_symbol = langsam_symbol(vm, "cons");
  if (LVEQ(head, cons_symbol)) {
    if (rhs.type != LT_CONS) {
      return langsam_exceptionf(vm, "bind", "attempt to bind value of %s to %s",
                                langsam_ctypename(vm, rhs.type),
                                langsam_cstr(vm, lhs));
    }
    LV pat = langsam_Cons_cast(vm, langsam_cdr(lhs));
    LANGSAM_CHECK(pat);
    LV pat_items = pat;
    LV rhs_items = rhs;
    while (langsam_consp(pat_items)) {
      LV pat_head = langsam_car(pat_items);
      if (!langsam_consp(rhs_items)) {
        return langsam_exceptionf(vm, "bind",
                                  "not enough values on the right side: "
                                  "lhs=%s rhs=%s",
                                  langsam_cstr(vm, lhs), langsam_cstr(vm, rhs));
      }
      LV rhs_head = langsam_car(rhs_items);
      LV bind_result = langsam_bind(vm, env, pat_head, rhs_head);
      LANGSAM_CHECK(bind_result);
      pat_items = langsam_cdr(pat_items);
      rhs_items = langsam_cdr(rhs_items);
    }
    if (!langsam_nilp(pat_items)) {
      LV bind_result = langsam_bind(vm, env, pat_items, rhs_items);
      LANGSAM_CHECK(bind_result);
    }
    return env;
  }
  LV and_symbol = langsam_symbol(vm, "and");
  if (LVEQ(head, and_symbol)) {
    LV pats = langsam_cdr(lhs);
    while (langsam_consp(pats)) {
      LV pat = langsam_car(pats);
      LV bind_result = langsam_bind(vm, env, pat, rhs);
      if (langsam_exceptionp(bind_result)) {
        return langsam_exceptionf(vm, "bind", "pattern failed: %s",
                                  langsam_cstr(vm, lhs));
      }
      pats = langsam_cdr(pats);
    }
    return env;
  }
  LV or_symbol = langsam_symbol(vm, "or");
  if (LVEQ(head, or_symbol)) {
    LV pats = langsam_cdr(lhs);
    while (langsam_consp(pats)) {
      LV pat = langsam_car(pats);
      LV bind_result = langsam_bind(vm, env, pat, rhs);
      if (!langsam_exceptionp(bind_result)) {
        return env;
      }
      pats = langsam_cdr(pats);
    }
    return langsam_exceptionf(vm, "bind", "pattern failed: %s",
                              langsam_cstr(vm, lhs));
  }
  LV pred_symbol = langsam_symbol(vm, "pred");
  if (LVEQ(head, pred_symbol)) {
    LV tail = langsam_cdr(lhs);
    LANGSAM_ARG(pred, tail);
    LV pred_fn = langsam_eval(vm, pred);
    LANGSAM_CHECK(pred_fn);
    LV pred_form = tail;
    pred_form = langsam_cons(vm, rhs, pred_form);
    pred_form = langsam_cons(vm, pred_fn, pred_form);
    LV result = langsam_eval(vm, pred_form);
    LANGSAM_CHECK(result);
    if (!langsam_truthy(vm, result)) {
      return langsam_exceptionf(vm, "bind", "pattern failed: %s",
                                langsam_cstr(vm, lhs));
    }
    return env;
  }
  LV when_symbol = langsam_symbol(vm, "when");
  if (LVEQ(head, when_symbol)) {
    LV tail = langsam_cdr(lhs);
    LANGSAM_ARG(expr, tail);
    LANGSAM_CHECK(langsam_pushlet(vm, env));
    LV result = langsam_eval(vm, expr);
    langsam_poplet(vm);
    LANGSAM_CHECK(result);
    if (!langsam_truthy(vm, result)) {
      return langsam_exceptionf(vm, "bind", "pattern failed: %s",
                                langsam_cstr(vm, lhs));
    }
    return env;
  }
  if (head.type == LT_SYMBOL) {
    LV ev_head = langsam_eval(vm, head);
    if (ev_head.type == LT_TYPE) {
      if (ev_head.p != rhs.type) {
        return langsam_exceptionf(vm, "bind", "type mismatch: %s",
                                  langsam_cstr(vm, lhs));
      }
      LV tail = langsam_cdr(lhs);
      LANGSAM_ARG(pat, tail);
      LV bind_result = langsam_bind(vm, env, pat, rhs);
      LANGSAM_CHECK(bind_result);
      return env;
    }
  }
  return langsam_exceptionf(vm, "syntax",
                            "invalid cons pattern in bind form: %s",
                            langsam_cstr(vm, lhs));
}

typedef enum {
  LANGSAM_BIND_POS,
  LANGSAM_BIND_OPT,
  LANGSAM_BIND_REST,
} LangsamBindVectorState;

static LV bind_vector(LangsamVM *vm, LV env, LV lhs, LV rhs) {
  LV it_lhs = langsam_iter(vm, lhs);
  LANGSAM_CHECK(it_lhs);
  LV it_rhs = langsam_iter(vm, rhs);
  if (langsam_exceptionpk(vm, it_rhs, "iter")) {
    return langsam_exceptionf(
        vm, "bind", "attempt to bind non-iterable %s to %s",
        langsam_ctypename(vm, rhs.type), langsam_ctypename(vm, lhs.type));
  }
  LANGSAM_CHECK(it_rhs);
  LV opt_symbol = langsam_symbol(vm, "&opt");
  LV amp_symbol = langsam_symbol(vm, "&");
  LangsamBindVectorState bs = LANGSAM_BIND_POS;
  LV rest = langsam_nil;
  while (langsam_truthy(vm, it_lhs)) {
    LV pat = langsam_deref(vm, it_lhs);
    switch (bs) {
    case LANGSAM_BIND_POS:
      if (LVEQ(pat, opt_symbol)) {
        bs = LANGSAM_BIND_OPT;
      } else if (LVEQ(pat, amp_symbol)) {
        bs = LANGSAM_BIND_REST;
      } else {
        if (!langsam_truthy(vm, it_rhs)) {
          return langsam_exceptionf(
              vm, "bind", "not enough values on the right side: lhs=%s rhs=%s",
              langsam_cstr(vm, lhs), langsam_cstr(vm, rhs));
        }
        LV value = langsam_deref(vm, it_rhs);
        LV result = langsam_bind(vm, env, pat, value);
        LANGSAM_CHECK(result);
        it_rhs = langsam_next(vm, it_rhs);
      }
      break;
    case LANGSAM_BIND_OPT:
      if (LVEQ(pat, amp_symbol)) {
        bs = LANGSAM_BIND_REST;
      } else {
        LV sym, val;
        LV symsetp = langsam_nil;
        if (pat.type == LT_SYMBOL) {
          sym = pat;
          val = langsam_nil;
        } else if (pat.type == LT_CONS) {
          LV head = langsam_car(pat);
          LV tail = langsam_cdr(pat);
          if (head.type == LT_SYMBOL && tail.type == LT_CONS) {
            sym = head;
            val = langsam_car(tail);
            tail = langsam_cdr(tail);
            if (tail.type == LT_CONS) {
              symsetp = langsam_car(tail);
              if (symsetp.type != LT_SYMBOL) {
                return langsam_exceptionf(vm, "syntax",
                                          "symsetp should be Symbol, got %s",
                                          langsam_ctypename(vm, symsetp.type));
              }
            }
          } else {
            return langsam_exceptionf(
                vm, "syntax",
                "&opt parameter with default value should look like (sym "
                "default) or (sym default symsetp), got %s",
                langsam_cstr(vm, pat));
          }
        } else {
          return langsam_exceptionf(
              vm, "syntax", "&opt parameter must be Symbol or Cons, got %s",
              langsam_cstr(vm, pat));
        }
        bool argsetp = false;
        if (langsam_truthy(vm, it_rhs)) {
          argsetp = true;
          val = langsam_deref(vm, it_rhs);
          it_rhs = langsam_next(vm, it_rhs);
        } else if (langsam_somep(val)) {
          LANGSAM_CHECK(langsam_pushlet(vm, env));
          val = langsam_eval(vm, val);
          langsam_poplet(vm);
          LANGSAM_CHECK(val);
        }
        LV bind_value_result = langsam_bind(vm, env, sym, val);
        LANGSAM_CHECK(bind_value_result);
        if (langsam_somep(symsetp)) {
          LV bind_symsetp_result =
              langsam_bind(vm, env, symsetp, langsam_boolean(argsetp));
          LANGSAM_CHECK(bind_symsetp_result);
        }
      }
      break;
    case LANGSAM_BIND_REST:
      if (langsam_somep(rest)) {
        return langsam_exceptionf(vm, "syntax",
                                  "& must be followed by a single form");
      }
      rest = collect_rest(vm, it_rhs);
      LV result = langsam_bind(vm, env, pat, rest);
      LANGSAM_CHECK(result);
      break;
    }
    it_lhs = langsam_next(vm, it_lhs);
  }
  return env;
}

static LV bind_map(LangsamVM *vm, LV env, LV lhs, LV rhs) {
  LV it_lhs = langsam_iter(vm, lhs);
  LANGSAM_CHECK(it_lhs);
  while (langsam_truthy(vm, it_lhs)) {
    LV item = langsam_deref(vm, it_lhs);
    LV pat = langsam_MapIteratorItem_k(item);
    LV key = langsam_MapIteratorItem_v(item);
    if (pat.type == LT_KEYWORD) {
      LV keys = langsam_keyword(vm, "keys");
      if (LVEQ(pat, keys)) {
        LV it_key = langsam_iter(vm, key);
        LANGSAM_CHECK(it_key);
        while (langsam_truthy(vm, it_key)) {
          LV sym = langsam_deref(vm, it_key);
          if (sym.type != LT_SYMBOL) {
            return langsam_exceptionf(
                vm, "syntax", "found value of %s in iterable passed to :keys",
                langsam_ctypename(vm, sym.type));
          }
          LV k = langsam_Keyword_cast(vm, sym);
          LV v = langsam_get(vm, rhs, k);
          if (langsam_exceptionpk(vm, v, "get")) {
            return langsam_exceptionf(
                vm, "bind", "attempt to bind non-associative %s to %s",
                langsam_ctypename(vm, rhs.type),
                langsam_ctypename(vm, lhs.type));
          }
          LANGSAM_CHECK(v);
          LV result = langsam_bind(vm, env, sym, v);
          LANGSAM_CHECK(result);
          it_key = langsam_next(vm, it_key);
        }
      } else {
        return langsam_exceptionf(vm, "syntax",
                                  "invalid key in map pattern: %s",
                                  langsam_cstr(vm, pat));
      }
    } else {
      LV value = langsam_get(vm, rhs, key);
      if (langsam_exceptionpk(vm, value, "get")) {
        return langsam_exceptionf(
            vm, "bind", "attempt to bind non-associative %s to %s",
            langsam_ctypename(vm, rhs.type), langsam_ctypename(vm, lhs.type));
      }
      LANGSAM_CHECK(value);
      LV result = langsam_bind(vm, env, pat, value);
      LANGSAM_CHECK(result);
    }
    it_lhs = langsam_next(vm, it_lhs);
  }
  return env;
}

LV langsam_bind(LangsamVM *vm, LV env, LV lhs, LV rhs) {
  if (lhs.type == LT_SYMBOL) {
    langsam_put(vm, env, lhs, rhs);
  } else if (lhs.type == LT_CONS) {
    return bind_cons(vm, env, lhs, rhs);
  } else if (lhs.type == LT_VECTOR) {
    return bind_vector(vm, env, lhs, rhs);
  } else if (lhs.type == LT_MAP) {
    return bind_map(vm, env, lhs, rhs);
  } else {
    return bind_quoted(vm, env, lhs, rhs);
  }
  return env;
}

LV langsam_quote(LangsamVM *vm, LV obj) {
  LV quote = langsam_get(vm, vm->rootlet, langsam_symbol(vm, "quote"));
  LV result = langsam_nil;
  result = langsam_cons(vm, obj, result);
  result = langsam_cons(vm, quote, result);
  return result;
}

static LV quasiquote_collect_item(LangsamVM *vm, LV item, LV *result,
                                  LV splice) {
  LV qqitem = langsam_quasiquote(vm, item);
  if (langsam_exceptionp(qqitem)) {
    bool throw = true;
    LV payload = langsam_deref(vm, qqitem);
    if (langsam_consp(payload)) {
      LV head = langsam_car(payload);
      if (LVEQ(head, splice)) {
        throw = false;
        LV splice_items = langsam_cdr(payload);
        while (langsam_consp(splice_items)) {
          LV splice_item = langsam_car(splice_items);
          *result = langsam_cons(vm, splice_item, *result);
          splice_items = langsam_cdr(splice_items);
        }
      }
    }
    if (throw) {
      return qqitem;
    }
  } else {
    *result = langsam_cons(vm, qqitem, *result);
  }
  return langsam_nil;
}

static LV quasiquote_collect_cons(LangsamVM *vm, LV list) {
  LV splice = langsam_opword(vm, "splice");
  LV result = langsam_nil;
  while (langsam_consp(list)) {
    LV item = langsam_car(list);
    LV item_result = quasiquote_collect_item(vm, item, &result, splice);
    LANGSAM_CHECK(item_result);
    list = langsam_cdr(list);
  }
  return langsam_nreverse_with_last(result, list);
}

static LV quasiquote_collect_iterable(LangsamVM *vm, LV coll) {
  LV splice = langsam_opword(vm, "splice");
  LV result = langsam_nil;
  LV it = langsam_iter(vm, coll);
  LANGSAM_CHECK(it);
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    LV item_result = quasiquote_collect_item(vm, item, &result, splice);
    LANGSAM_CHECK(item_result);
    it = langsam_next(vm, it);
  }
  return langsam_nreverse(result);
}

LV langsam_quasiquote(LangsamVM *vm, LV obj) {
  if (obj.type == LT_CONS) {
    LV head = langsam_car(obj);
    LV unquote = langsam_symbol(vm, "unquote");
    LV unquote_splicing = langsam_symbol(vm, "unquote-splicing");
    if (LVEQ(head, unquote)) {
      LV tail = langsam_cdr(obj);
      if (!langsam_consp(tail)) {
        return langsam_exceptionf(vm, "syntax", "unquote needs argument");
      }
      LV form = langsam_car(tail);
      return langsam_eval(vm, form);
    } else if (LVEQ(head, unquote_splicing)) {
      LV tail = langsam_cdr(obj);
      if (!langsam_consp(tail)) {
        return langsam_exceptionf(vm, "syntax",
                                  "unquote-splicing needs argument");
      }
      LV form = langsam_car(tail);
      LV evaluated_form = langsam_eval(vm, form);
      LANGSAM_CHECK(evaluated_form);
      LV it = langsam_iter(vm, evaluated_form);
      LANGSAM_CHECK(it);
      LV result = collect_rest(vm, it);
      LV splice = langsam_opword(vm, "splice");
      return langsam_exception(vm, langsam_cons(vm, splice, result));
    } else {
      return quasiquote_collect_cons(vm, obj);
    }
  } else if (obj.type == LT_VECTOR || obj.type == LT_MAP) {
    LV items = quasiquote_collect_iterable(vm, obj);
    LANGSAM_CHECK(items);
    return langsam_cast(vm, langsam_type(obj.type), items);
  } else {
    return obj;
  }
}

LV langsam_do(LangsamVM *vm, LV forms) {
  LV result = langsam_nil;
  while (langsam_consp(forms)) {
    LV form = langsam_car(forms);
    result = langsam_eval(vm, form);
    LANGSAM_CHECK(result);
    forms = langsam_cdr(forms);
  }
  return result;
}

LV langsam_next(LangsamVM *vm, LV it) {
  return langsam_invoke(vm, it, langsam_nil);
}

// Function

LangsamSize langsam_Function_gcmark(LangsamVM *vm, void *p) {
  LangsamFunction *f = p;
  LangsamSize total = 0;
  total += langsam_mark(vm, f->name);
  total += langsam_mark(vm, f->params);
  total += langsam_mark(vm, f->doc);
  total += langsam_mark(vm, f->funclet);
  total += langsam_mark(vm, f->body);
  return total;
}

LangsamHash langsam_Function_hash(LangsamVM *vm, LV self, LangsamHash hash) {
  LangsamFunction *f = self.p;
  hash = langsam_hash(vm, f->name, hash);
  hash = langsam_hash(vm, f->params, hash);
  hash = langsam_hash(vm, f->doc, hash);
  hash = langsam_hash(vm, f->body, hash);
  hash = hash_boolean(hash, f->evalargs);
  hash = hash_boolean(hash, f->evalresult);
  hash = hash_boolean(hash, f->dynamic);
  return hash;
}

LV langsam_Function_cast(LangsamVM *vm, LV other) {
  if (other.type != LT_MAP) {
    return langsam_exceptionf(vm, "cast", "Cannot cast %s to <Function>",
                              langsam_ctypename(vm, other.type));
  }
  LangsamFunction *f =
      langsam_gcalloc(vm, LT_FUNCTION, LANGSAM_SIZEOF(LangsamFunction));
  f->name = langsam_get(vm, other, langsam_keyword(vm, "name"));
  if (langsam_somep(f->name)) {
    if (f->name.type != LT_SYMBOL) {
      return langsam_exceptionf(
          vm, "syntax", "Function name should be symbol, got %s: %s",
          langsam_ctypename(vm, f->name.type), langsam_cstr(vm, f->name));
    }
  }
  f->params = langsam_get(vm, other, langsam_keyword(vm, "params"));
  if (f->params.type != LT_VECTOR && f->params.type != LT_NIL) {
    return langsam_exceptionf(
        vm, "syntax", "Function parameters should be Vector or Nil, got %s",
        langsam_ctypename(vm, f->params.type));
  }
  f->doc = langsam_get(vm, other, langsam_keyword(vm, "doc"));
  f->funclet = vm->curlet;
  f->body = langsam_get(vm, other, langsam_keyword(vm, "body"));
  if (f->body.type != LT_CONS && f->body.type != LT_NIL) {
    return langsam_exceptionf(vm, "syntax",
                              "Function body should be Cons or Nil, got %s",
                              langsam_ctypename(vm, f->body.type));
  }
  f->fn = NULL;
  f->evalargs = langsam_truthy(
      vm, langsam_get(vm, other, langsam_keyword(vm, "evalargs")));
  f->evalresult = langsam_truthy(
      vm, langsam_get(vm, other, langsam_keyword(vm, "evalresult")));
  f->dynamic = langsam_truthy(
      vm, langsam_get(vm, other, langsam_keyword(vm, "dynamic")));
  return (LV){
      .type = LT_FUNCTION,
      .p = f,
  };
}

LV langsam_Function_get(LangsamVM *vm, LV self, LV key) {
  LangsamFunction *f = self.p;
  LV name_key = langsam_keyword(vm, "name");
  if (LVEQ(key, name_key)) {
    return f->name;
  }
  LV params_key = langsam_keyword(vm, "params");
  if (LVEQ(key, params_key)) {
    return f->params;
  }
  LV doc_key = langsam_keyword(vm, "doc");
  if (LVEQ(key, doc_key)) {
    return f->doc;
  }
  LV funclet_key = langsam_keyword(vm, "funclet");
  if (LVEQ(key, funclet_key)) {
    return f->funclet;
  }
  LV body_key = langsam_keyword(vm, "body");
  if (LVEQ(key, body_key)) {
    return f->body;
  }
  LV evalargs_key = langsam_keyword(vm, "evalargs");
  if (LVEQ(key, evalargs_key)) {
    return langsam_boolean(f->evalargs);
  }
  LV evalresult_key = langsam_keyword(vm, "evalresult");
  if (LVEQ(key, evalresult_key)) {
    return langsam_boolean(f->evalresult);
  }
  LV dynamic_key = langsam_keyword(vm, "dynamic");
  if (LVEQ(key, dynamic_key)) {
    return langsam_boolean(f->dynamic);
  }
  return langsam_nil;
}

LV langsam_Function_put(LangsamVM *vm, LV self, LV key, LV value) {
  LangsamFunction *f = self.p;
  LV name_key = langsam_keyword(vm, "name");
  if (LVEQ(key, name_key)) {
    f->name = value;
  }
  LV params_key = langsam_keyword(vm, "params");
  if (LVEQ(key, params_key)) {
    f->params = value;
  }
  LV doc_key = langsam_keyword(vm, "doc");
  if (LVEQ(key, doc_key)) {
    f->doc = value;
  }
  LV funclet_key = langsam_keyword(vm, "funclet");
  if (LVEQ(key, funclet_key)) {
    f->funclet = value;
  }
  LV body_key = langsam_keyword(vm, "body");
  if (LVEQ(key, body_key)) {
    f->body = value;
  }
  LV evalargs_key = langsam_keyword(vm, "evalargs");
  if (LVEQ(key, evalargs_key)) {
    f->evalargs = langsam_truep(value);
  }
  LV evalresult_key = langsam_keyword(vm, "evalresult");
  if (LVEQ(key, evalresult_key)) {
    f->evalresult = langsam_truep(value);
  }
  LV dynamic_key = langsam_keyword(vm, "dynamic");
  if (LVEQ(key, dynamic_key)) {
    f->dynamic = langsam_truep(value);
  }
  return self;
}

static LV fn_evalargs(LangsamVM *vm, LV args) {
  LV l = args;
  LV ev_args = langsam_nil;
  while (langsam_consp(l)) {
    LV arg = langsam_car(l);
    LV ev_arg = langsam_eval(vm, arg);
    LANGSAM_CHECK(ev_arg);
    ev_args = langsam_cons(vm, ev_arg, ev_args);
    l = langsam_cdr(l);
  }
  if (langsam_somep(l)) {
    LV rest_arg = langsam_eval(vm, l);
    LANGSAM_CHECK(rest_arg);
    LV it = langsam_iter(vm, rest_arg);
    if (langsam_exceptionp(it)) {
      return langsam_exceptionf(vm, "invoke",
                                "rest arg should be iterable, got %s",
                                langsam_ctypename(vm, rest_arg.type));
    }
    while (langsam_truthy(vm, it)) {
      LV ev_arg = langsam_deref(vm, it);
      LANGSAM_CHECK(ev_arg);
      ev_args = langsam_cons(vm, ev_arg, ev_args);
      it = langsam_next(vm, it);
    }
  }
  return langsam_nreverse(ev_args);
}

static LV fn_invoke(LangsamVM *vm, LangsamFunction *f, LV args) {
  if (f->fn != NULL) {
    return f->fn(vm, args);
  } else {
    LV body = f->body;
    LV fenv = langsam_map(vm, f->dynamic ? vm->curlet : f->funclet, 64);
    LANGSAM_CHECK(langsam_pushlet(vm, fenv));
    LV bind_result = langsam_bind(vm, vm->curlet, f->params, args);
    if (langsam_exceptionp(bind_result)) {
      langsam_poplet(vm);
      return bind_result;
    }
    LV result = langsam_do(vm, body);
    langsam_poplet(vm);
    return result;
  }
}

LV langsam_Function_invoke(LangsamVM *vm, LV self, LV args) {
  LangsamFunction *f = self.p;
  if (f->evalargs) {
    args = fn_evalargs(vm, args);
    LANGSAM_CHECK(args);
  }
  LV result = fn_invoke(vm, f, args);
  LANGSAM_CHECK(result);
  if (f->evalresult) {
    result = langsam_eval(vm, result);
    LANGSAM_CHECK(result);
  }
  return result;
}

LV langsam_Function_repr(LangsamVM *vm, LV self) {
  LangsamFunction *f = self.p;
  char *type;
  if (f->evalargs) {
    type = "Function";
  } else if (f->evalresult) {
    type = "Macro";
  } else {
    type = "Special";
  }
  if (langsam_somep(f->name)) {
    return langsam_format(vm, "<%s:%s>", type, langsam_cstr(vm, f->name));
  } else {
    return langsam_format(vm, "<%s>", type);
  }
}

static struct LangsamT LANGSAM_T_FUNCTION = {
    .name = "Function",
    .gcmanaged = true,
    .gcmark = langsam_Function_gcmark,
    .hash = langsam_Function_hash,
    .cast = langsam_Function_cast,
    .get = langsam_Function_get,
    .put = langsam_Function_put,
    .invoke = langsam_Function_invoke,
    .repr = langsam_Function_repr,
};

const LangsamType LT_FUNCTION = &LANGSAM_T_FUNCTION;

// allocator

static void *langsam_default_realloc(void *self, void *ptr, LangsamSize size) {
  if (size) {
    if (ptr) {
      return realloc(ptr, (size_t)size);
    } else {
      return malloc((size_t)size);
    }
  } else {
    free(ptr);
    return NULL;
  }
}

static LangsamAllocator langsam_default_allocator = {
    .self = NULL,
    .realloc = langsam_default_realloc,
};

void *langsam_alloc(LangsamVM *vm, LangsamSize size) {
  return vm->allocator->realloc(vm->allocator, NULL, size);
}

void *langsam_calloc(LangsamVM *vm, LangsamSize size) {
  void *p = langsam_alloc(vm, size);
  return memset(p, 0, (size_t)size);
}

void *langsam_realloc(LangsamVM *vm, void *ptr, LangsamSize size) {
  return vm->allocator->realloc(vm->allocator, ptr, size);
}

void langsam_free(LangsamVM *vm, void *ptr) {
  vm->allocator->realloc(vm->allocator, ptr, 0);
}

// GC

#define langsam_gcheader(p) ((LangsamGCHeader *)p - 1)

static LangsamGCColor langsam_gcaltcolor(LangsamVM *vm) {
  if (vm->gcmarkcolor == LANGSAM_GC_BLACK) {
    return LANGSAM_GC_WHITE;
  } else {
    return LANGSAM_GC_BLACK;
  }
}

void *langsam_gcalloc(LangsamVM *vm, LangsamType type, LangsamSize size) {
  LangsamSize object_size = LANGSAM_SIZEOF(LangsamGCHeader) + size;
  LangsamGCHeader *gch = langsam_alloc(vm, object_size);
  gch->size = object_size;
  gch->type = type;
  gch->gccolor = langsam_gcaltcolor(vm);
  gch->next = vm->gcobjects;
  vm->gcobjects = gch;
  void *p = gch + 1;
  return p;
}

LangsamSize langsam_mark(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (!t->gcmanaged) {
    return 0;
  }
  LangsamGCHeader *gch = langsam_gcheader(self.p);
  if (gch->gccolor == vm->gcmarkcolor) {
    return 0;
  }
  gch->gccolor = vm->gcmarkcolor;
  return gch->size + (t->gcmark ? t->gcmark(vm, self.p) : 0);
}

static LangsamSize langsam_gcfree(LangsamVM *vm, LangsamGCHeader *gch) {
  LangsamType t = gch->type;
  LangsamSize size = gch->size;
  if (t->gcfree) {
    size += t->gcfree(vm, gch + 1);
  }
  langsam_free(vm, gch);
  return size;
}

static LangsamSize langsam_gcfree_all(LangsamVM *vm) {
  LangsamGCHeader *gch = vm->gcobjects;
  LangsamSize total = 0;
  while (gch) {
    LangsamGCHeader *next = gch->next;
    total += langsam_gcfree(vm, gch);
    gch = next;
  }
  vm->gcobjects = NULL;
  return total;
}

LV langsam_gc(LangsamVM *vm) {
  LangsamSize mark_total = 0;
  for (int i = 0; i < vm->numroots; i++) {
    mark_total += langsam_mark(vm, vm->roots[i]);
  }
  for (int i = 0; i < vm->numlets; i++) {
    mark_total += langsam_mark(vm, vm->lets[i]);
  }
  mark_total += langsam_mark(vm, vm->strings);
  LangsamGCHeader *prevgch = NULL;
  LangsamGCHeader *gch = vm->gcobjects;
  LangsamSize free_total = 0;
  while (gch) {
    LangsamGCHeader *next = gch->next;
    if (gch->gccolor == vm->gcmarkcolor) {
      // marked: keep
      prevgch = gch;
      gch = next;
    } else {
      // !marked: sweep
      free_total += langsam_gcfree(vm, gch);
      if (prevgch == NULL) {
        vm->gcobjects = next;
      } else {
        prevgch->next = next;
      }
      gch = next;
    }
  }
  vm->gcmarkcolor = langsam_gcaltcolor(vm);
  LV result = langsam_map(vm, langsam_nil, 2);
  langsam_put(vm, result, langsam_keyword(vm, "marked"),
              langsam_integer(mark_total));
  langsam_put(vm, result, langsam_keyword(vm, "swept"),
              langsam_integer(free_total));
  return result;
}

// core

static LV eval_hash(LangsamVM *vm, LV args) {
  LANGSAM_ARG(obj, args);
  LANGSAM_ARG_OPT(prev, args);
  if (langsam_somep(prev)) {
    LANGSAM_ARG_TYPE(prev, LT_INTEGER);
  }
  LangsamHash prevhash = langsam_somep(prev) ? (LangsamHash)prev.i : HASH_SEED;
  LangsamHash hash = langsam_hash(vm, obj, prevhash);
  return langsam_integer((LangsamInteger)hash);
}

static LV eval_eq(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  while (langsam_consp(args)) {
    LANGSAM_ARG(rhs, args);
    if (!LVEQ(lhs, rhs)) {
      return langsam_false;
    }
  }
  return langsam_true;
}

static LV eval_equal(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  while (langsam_consp(args)) {
    LANGSAM_ARG(rhs, args);
    LV is_eq = langsam_equal(vm, lhs, rhs);
    LANGSAM_CHECK(is_eq);
    if (langsam_falsep(is_eq)) {
      return langsam_false;
    }
  }
  return langsam_true;
}

static LV eval_cmp(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LANGSAM_ARG(rhs, args);
  return langsam_cmp(vm, lhs, rhs);
}

static LV eval_lt(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LANGSAM_ARG(rhs, args);
  LV result = langsam_cmp(vm, lhs, rhs);
  LANGSAM_CHECK(result);
  return langsam_boolean(result.i < 0);
}

static LV eval_le(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LANGSAM_ARG(rhs, args);
  LV result = langsam_cmp(vm, lhs, rhs);
  LANGSAM_CHECK(result);
  return langsam_boolean(result.i <= 0);
}

static LV eval_ge(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LANGSAM_ARG(rhs, args);
  LV result = langsam_cmp(vm, lhs, rhs);
  LANGSAM_CHECK(result);
  return langsam_boolean(result.i >= 0);
}

static LV eval_gt(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LANGSAM_ARG(rhs, args);
  LV result = langsam_cmp(vm, lhs, rhs);
  LANGSAM_CHECK(result);
  return langsam_boolean(result.i > 0);
}

static LV eval_add(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LV result = lhs;
  while (langsam_consp(args)) {
    LANGSAM_ARG(rhs, args);
    result = langsam_add(vm, result, rhs);
    LANGSAM_CHECK(result);
  }
  return result;
}

static LV eval_sub(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  if (langsam_nilp(args)) {
    return langsam_mul(vm, lhs, langsam_integer(-1));
  }
  LV result = lhs;
  while (langsam_consp(args)) {
    LANGSAM_ARG(rhs, args);
    result = langsam_sub(vm, result, rhs);
    LANGSAM_CHECK(result);
  }
  return result;
}

static LV eval_mul(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LV result = lhs;
  while (langsam_consp(args)) {
    LANGSAM_ARG(rhs, args);
    result = langsam_mul(vm, result, rhs);
    LANGSAM_CHECK(result);
  }
  return result;
}

static LV eval_div(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LV result = lhs;
  while (langsam_consp(args)) {
    LANGSAM_ARG(rhs, args);
    result = langsam_div(vm, result, rhs);
    LANGSAM_CHECK(result);
  }
  return result;
}

static LV eval_mod(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LV result = lhs;
  while (langsam_consp(args)) {
    LANGSAM_ARG(rhs, args);
    result = langsam_mod(vm, result, rhs);
    LANGSAM_CHECK(result);
  }
  return result;
}

static LV eval_get(LangsamVM *vm, LV args) {
  LANGSAM_ARG(coll, args);
  LANGSAM_ARG(key, args);
  return langsam_get(vm, coll, key);
}

static LV eval_put(LangsamVM *vm, LV args) {
  LANGSAM_ARG(coll, args);
  LANGSAM_ARG(key, args);
  LANGSAM_ARG(value, args);
  return langsam_put(vm, coll, key, value);
}

static LV eval_del(LangsamVM *vm, LV args) {
  LANGSAM_ARG(coll, args);
  LANGSAM_ARG(key, args);
  return langsam_del(vm, coll, key);
}

static LV eval_len(LangsamVM *vm, LV args) {
  LANGSAM_ARG(obj, args);
  return langsam_len(vm, obj);
}

static LV eval_iter(LangsamVM *vm, LV args) {
  LANGSAM_ARG(obj, args);
  return langsam_iter(vm, obj);
}

static LV eval_deref(LangsamVM *vm, LV args) {
  LANGSAM_ARG(obj, args);
  return langsam_deref(vm, obj);
}

static LV eval_eval(LangsamVM *vm, LV args) {
  LANGSAM_ARG(form, args);
  LANGSAM_ARG_OPT(env, args);
  if (langsam_nilp(env)) {
    env = vm->curlet;
  }
  LANGSAM_CHECK(langsam_pushlet(vm, env));
  LV result = langsam_eval(vm, form);
  langsam_poplet(vm);
  return result;
}

static LV eval_repr(LangsamVM *vm, LV args) {
  LANGSAM_ARG(obj, args);
  return langsam_repr(vm, obj);
}

static LV eval_str(LangsamVM *vm, LV args) {
  LANGSAM_ARG(head, args);
  LV head_str = langsam_str(vm, head);
  LANGSAM_CHECK(head_str);
  if (langsam_nilp(args)) {
    return head_str;
  }
  LV ss = langsam_cons(vm, head_str, langsam_nil);
  LangsamSize len = string_length(head_str);
  while (langsam_consp(args)) {
    LANGSAM_ARG(item, args);
    LV item_str = langsam_str(vm, item);
    LANGSAM_CHECK(item_str);
    ss = langsam_cons(vm, item_str, ss);
    len += string_length(item_str);
  }
  ss = langsam_nreverse(ss);
  char *p0 = langsam_alloc(vm, len + 1);
  char *p = p0;
  while (langsam_consp(ss)) {
    LV s = langsam_car(ss);
    LangsamString *ls = s.p;
    memcpy(p, ls->p, (size_t)ls->len);
    p += ls->len;
    ss = langsam_cdr(ss);
  }
  *p = 0;
  return langsam_stringn_wrap(vm, p0, len);
}

static LV eval_quote(LangsamVM *vm, LV args) {
  LANGSAM_ARG(obj, args);
  return obj;
}

static LV eval_quasiquote(LangsamVM *vm, LV args) {
  LANGSAM_ARG(obj, args);
  return langsam_quasiquote(vm, obj);
}

static LV eval_apply(LangsamVM *vm, LV args) {
  LANGSAM_ARG(f, args);
  f = langsam_eval(vm, f);
  LANGSAM_CHECK(f);
  LV reversed_args = langsam_nil;
  while (langsam_consp(args)) {
    LV arg = langsam_car(args);
    reversed_args = langsam_cons(vm, arg, reversed_args);
    args = langsam_cdr(args);
  }
  LANGSAM_ARG(rest, reversed_args);
  LV dotted_args = langsam_nreverse_with_last(reversed_args, rest);
  return langsam_invoke(vm, f, dotted_args);
}

static LV eval_def(LangsamVM *vm, LV args) {
  LANGSAM_ARG(lhs, args);
  LANGSAM_ARG(rhs, args);
  rhs = langsam_eval(vm, rhs);
  LANGSAM_CHECK(rhs);
  LV bind_result = langsam_bind(vm, vm->curlet, lhs, rhs);
  LANGSAM_CHECK(bind_result);
  return lhs;
}

static LV make_function(LangsamVM *vm, LV args, bool evalargs,
                        bool evalresult) {
  if (langsam_nilp(args)) {
    return langsam_exceptionf(vm, "syntax", "missing function spec");
  }
  LV tail = args;
  LV name = langsam_nil;
  LV params = langsam_nil;
  LV doc = langsam_nil;
  LV attrs = langsam_nil;
  while (langsam_nilp(params) && langsam_consp(tail)) {
    LV head = langsam_car(tail);
    if (head.type == LT_SYMBOL) {
      if (langsam_nilp(name)) {
        name = head;
      } else {
        break;
      }
    } else if (head.type == LT_STRING) {
      if (langsam_nilp(doc)) {
        doc = head;
      } else {
        break;
      }
    } else if (head.type == LT_VECTOR) {
      params = head;
    } else if (head.type == LT_MAP) {
      attrs = head;
    } else {
      break;
    }
    tail = langsam_cdr(tail);
  }
  if (langsam_nilp(params)) {
    return langsam_exceptionf(vm, "syntax", "missing function params");
  }
  LV body = tail;
  LV desc = langsam_map(vm, langsam_nil, 6);
  langsam_put(vm, desc, langsam_keyword(vm, "name"), name);
  langsam_put(vm, desc, langsam_keyword(vm, "params"), params);
  langsam_put(vm, desc, langsam_keyword(vm, "doc"), doc);
  langsam_put(vm, desc, langsam_keyword(vm, "body"), body);
  langsam_put(vm, desc, langsam_keyword(vm, "evalargs"),
              langsam_boolean(evalargs));
  langsam_put(vm, desc, langsam_keyword(vm, "evalresult"),
              langsam_boolean(evalresult));
  if (attrs.type == LT_MAP) {
    LV dynamic_keyword = langsam_keyword(vm, "dynamic");
    langsam_put(
        vm, desc, dynamic_keyword,
        langsam_Boolean_cast(vm, langsam_get(vm, attrs, dynamic_keyword)));
  }
  return langsam_Function_cast(vm, desc);
}

static LV eval_defn(LangsamVM *vm, LV args) {
  LV function = make_function(vm, args, true, false);
  LANGSAM_CHECK(function);
  LV name = langsam_get(vm, function, langsam_keyword(vm, "name"));
  if (langsam_nilp(name)) {
    return langsam_exceptionf(vm, "syntax", "missing function name");
  }
  langsam_put(vm, vm->curlet, name, function);
  return function;
}

static LV eval_fn(LangsamVM *vm, LV args) {
  return make_function(vm, args, true, false);
}

static LV eval_defmacro(LangsamVM *vm, LV args) {
  LV macro = make_function(vm, args, false, true);
  LANGSAM_CHECK(macro);
  LV name = langsam_get(vm, macro, langsam_keyword(vm, "name"));
  if (langsam_nilp(name)) {
    return langsam_exceptionf(vm, "syntax", "missing macro name");
  }
  langsam_put(vm, vm->curlet, name, macro);
  return macro;
}

static LV eval_macro(LangsamVM *vm, LV args) {
  return make_function(vm, args, false, true);
}

static LangsamFunction *is_macro(LV obj) {
  if (obj.type != LT_FUNCTION) {
    return NULL;
  }
  LangsamFunction *f = obj.p;
  if (f->evalargs == true || f->evalresult == false) {
    return NULL;
  }
  return f;
}

static LV eval_macrop(LangsamVM *vm, LV args) {
  LANGSAM_ARG(obj, args);
  return langsam_boolean(is_macro(obj) != NULL);
}

static LV eval_macroexpand_1(LangsamVM *vm, LV args) {
  LANGSAM_ARG(macro_call, args);
  LANGSAM_ARG_OPT(env, args);
  if (langsam_nilp(env)) {
    env = vm->curlet;
  }
  LANGSAM_ARG_TYPE(env, LT_MAP);
  LANGSAM_ARG(macro_sym, macro_call);
  LANGSAM_CHECK(langsam_pushlet(vm, env));
  LV macro = langsam_eval(vm, macro_sym);
  if (langsam_exceptionp(macro)) {
    langsam_poplet(vm);
    return macro;
  }
  LangsamFunction *f = is_macro(macro);
  if (f == NULL) {
    langsam_poplet(vm);
    return macro_call;
  }
  LV result = fn_invoke(vm, f, macro_call);
  langsam_poplet(vm);
  return result;
}

static LV eval_throw(LangsamVM *vm, LV args) {
  LANGSAM_ARG(payload, args);
  return langsam_exception(vm, payload);
}

static LV eval_do(LangsamVM *vm, LV args) { return langsam_do(vm, args); }

static LV eval_if(LangsamVM *vm, LV args) {
  LANGSAM_ARG(cond, args);
  LANGSAM_ARG(if_expr, args);
  LANGSAM_ARG_OPT(else_expr, args);
  LV cond_result = langsam_eval(vm, cond);
  LANGSAM_CHECK(cond_result);
  if (langsam_truthy(vm, cond_result)) {
    return langsam_eval(vm, if_expr);
  } else {
    return langsam_eval(vm, else_expr);
  }
}

static LV eval_while(LangsamVM *vm, LV args) {
  LANGSAM_ARG(cond, args);
  LV result = langsam_nil;
  while (true) {
    LV cond_result = langsam_eval(vm, cond);
    LANGSAM_CHECK(cond_result);
    if (langsam_truthy(vm, cond_result)) {
      result = langsam_do(vm, args);
      LANGSAM_CHECK(result);
    } else {
      break;
    }
  }
  return result;
}

static LV process_bindings(LangsamVM *vm, LV bindings) {
  LV it = langsam_iter(vm, bindings);
  LANGSAM_CHECK(it);
  while (langsam_truthy(vm, it)) {
    LV k = langsam_deref(vm, it);
    it = langsam_next(vm, it);
    if (!langsam_truthy(vm, it)) {
      return langsam_exceptionf(vm, "syntax", "incomplete let bindings");
    }
    LV v = langsam_deref(vm, it);
    v = langsam_eval(vm, v);
    LANGSAM_CHECK(v);
    it = langsam_next(vm, it);
    LV bind_result = langsam_bind(vm, vm->curlet, k, v);
    LANGSAM_CHECK(bind_result);
  }
  return langsam_nil;
}

static LV eval_let(LangsamVM *vm, LV args) {
  LANGSAM_ARG(bindings, args);
  LV letenv = langsam_map(vm, vm->curlet, 64);
  LANGSAM_CHECK(langsam_pushlet(vm, letenv));
  LV bind_result = process_bindings(vm, bindings);
  if (langsam_exceptionp(bind_result)) {
    langsam_poplet(vm);
    return bind_result;
  }
  LV result = langsam_do(vm, args);
  langsam_poplet(vm);
  return result;
}

static LV eval_if_let(LangsamVM *vm, LV args) {
  LANGSAM_ARG(bindings, args);
  LANGSAM_ARG(if_expr, args);
  LANGSAM_ARG_OPT(else_expr, args);
  LV letenv = langsam_map(vm, vm->curlet, 64);
  LANGSAM_CHECK(langsam_pushlet(vm, letenv));
  LV bind_result = process_bindings(vm, bindings);
  if (langsam_exceptionp(bind_result)) {
    langsam_poplet(vm);
    if (!bind_exceptionp(vm, bind_result)) {
      return bind_result;
    } else {
      return langsam_eval(vm, else_expr);
    }
  } else {
    LV result = langsam_eval(vm, if_expr);
    langsam_poplet(vm);
    return result;
  }
}

static LV process_catch_clauses(LangsamVM *vm, LV clauses, LV payload) {
  LV it = langsam_iter(vm, clauses);
  LANGSAM_CHECK(it);
  while (langsam_truthy(vm, it)) {
    LV pat = langsam_deref(vm, it);
    it = langsam_next(vm, it);
    if (!langsam_truthy(vm, it)) {
      return langsam_exceptionf(vm, "syntax", "incomplete catch clause");
    }
    LV catchenv = langsam_map(vm, vm->curlet, 64);
    LANGSAM_CHECK(langsam_pushlet(vm, catchenv));
    LV bind_result = langsam_bind(vm, vm->curlet, pat, payload);
    if (!langsam_exceptionp(bind_result)) {
      LV expr = langsam_deref(vm, it);
      LV result = langsam_eval(vm, expr);
      langsam_poplet(vm);
      return result;
    } else if (!bind_exceptionp(vm, bind_result)) {
      langsam_poplet(vm);
      return bind_result;
    }
    langsam_poplet(vm);
    it = langsam_next(vm, it);
  }
  return langsam_exception(vm, payload);
}

static LV eval_catch(LangsamVM *vm, LV args) {
  LANGSAM_ARG(clauses, args);
  LANGSAM_ARG_TYPE(clauses, LT_VECTOR);
  LV result = langsam_do(vm, args);
  if (langsam_exceptionp(result)) {
    LangsamException *ex = result.p;
    result = process_catch_clauses(vm, clauses, ex->payload);
  }
  return result;
}

static LV eval_assert(LangsamVM *vm, LV args) {
  LANGSAM_ARG(expr, args);
  LV result = langsam_eval(vm, expr);
  LANGSAM_CHECK(result);
  if (!langsam_truthy(vm, result)) {
    if (langsam_consp(expr)) {
      LV head = langsam_car(expr);
      LV symeq = langsam_symbol(vm, "=");
      if (LVEQ(head, symeq)) {
        LV tail = langsam_cdr(expr);
        LANGSAM_ARG(actual_form, tail);
        LV actual = langsam_eval(vm, actual_form);
        LANGSAM_ARG(expected_form, tail);
        LV expected = langsam_eval(vm, expected_form);
        return langsam_exceptionf(
            vm, "assert", "assertion failed: %s: expected %s, got %s",
            langsam_cstr(vm, expr), langsam_cstr(vm, expected),
            langsam_cstr(vm, actual));
      }
    }
    return langsam_exceptionf(vm, "assert", "assertion failed: %s",
                              langsam_cstr(vm, expr));
  }
  return result;
}

static LV eval_debug(LangsamVM *vm, LV args) {
  LANGSAM_ARG_OPT(arg, args);
  if (!langsam_nilp(arg)) {
    LV result = langsam_eval(vm, arg);
    langsam_log(vm, vm->loglevel, "DEBUG: %s -> %s", langsam_cstr(vm, arg),
                langsam_cstr(vm, result));
    return result;
  }
  return langsam_nil;
}

static LV eval_curlet(LangsamVM *vm, LV args) { return vm->curlet; }
static LV eval_rootlet(LangsamVM *vm, LV args) { return vm->rootlet; }

static LV eval_bind(LangsamVM *vm, LV args) {
  LANGSAM_ARG(env, args);
  LANGSAM_ARG_TYPE(env, LT_MAP);
  LANGSAM_ARG(pattern, args);
  LANGSAM_ARG(value, args);
  return langsam_bind(vm, env, pattern, value);
}

static LV eval_getproto(LangsamVM *vm, LV args) {
  LANGSAM_ARG(map, args);
  LANGSAM_ARG_TYPE(map, LT_MAP);
  return langsam_Map_getproto(vm, map);
}

static LV eval_setproto(LangsamVM *vm, LV args) {
  LANGSAM_ARG(map, args);
  LANGSAM_ARG_TYPE(map, LT_MAP);
  LANGSAM_ARG(proto, args);
  return langsam_Map_setproto(vm, map, proto);
}

static LV eval_gep(LangsamVM *vm, LV args) {
  LANGSAM_ARG(map, args);
  LANGSAM_ARG_TYPE(map, LT_MAP);
  LANGSAM_ARG(key, args);
  return langsam_Map_gep(vm, map, key);
}

static LV eval_next(LangsamVM *vm, LV args) {
  LANGSAM_ARG(it, args);
  return langsam_next(vm, it);
}

static LV eval_type(LangsamVM *vm, LV args) {
  LANGSAM_ARG(x, args);
  return langsam_type(x.type);
}

static LV eval_cons(LangsamVM *vm, LV args) {
  return langsam_Cons_cast(vm, args);
}

static LV eval_list(LangsamVM *vm, LV args) {
  LV it = langsam_iter(vm, args);
  LANGSAM_CHECK(it);
  return collect_rest(vm, it);
}

static LV eval_car(LangsamVM *vm, LV args) {
  LANGSAM_ARG(arg, args);
  LANGSAM_ARG_TYPE(arg, LT_CONS);
  return langsam_car(arg);
}

static LV eval_cdr(LangsamVM *vm, LV args) {
  LANGSAM_ARG(arg, args);
  LANGSAM_ARG_TYPE(arg, LT_CONS);
  return langsam_cdr(arg);
}

static LV eval_setcar(LangsamVM *vm, LV args) {
  LANGSAM_ARG(cons, args);
  LANGSAM_ARG_TYPE(cons, LT_CONS);
  LANGSAM_ARG(value, args);
  langsam_setcar(cons, value);
  return langsam_nil;
}

static LV eval_setcdr(LangsamVM *vm, LV args) {
  LANGSAM_ARG(cons, args);
  LANGSAM_ARG_TYPE(cons, LT_CONS);
  LANGSAM_ARG(value, args);
  langsam_setcdr(cons, value);
  return langsam_nil;
}

static LV eval_nreverse(LangsamVM *vm, LV args) {
  LANGSAM_ARG(arg, args);
  if (langsam_nilp(arg)) {
    return arg;
  }
  LANGSAM_ARG_TYPE(arg, LT_CONS);
  return langsam_nreverse(arg);
}

static LV eval_require(LangsamVM *vm, LV args) {
  LANGSAM_ARG(module_name, args);
  LANGSAM_ARG_TYPE(module_name, LT_STRING);
  LangsamString *ls = (LangsamString *)module_name.p;
  return langsam_require(vm, ls->p);
}

static LV eval_read_string(LangsamVM *vm, LV args) {
  LANGSAM_ARG(source, args);
  LANGSAM_ARG_TYPE(source, LT_STRING);
  LangsamString *ls = (LangsamString *)source.p;
  return langsam_readstringn(vm, ls->p, ls->len);
}

static LV eval_gc(LangsamVM *vm, LV args) { return langsam_gc(vm); }

extern int langsam_l_len;
extern char langsam_l_bytes[];

static LV langsam_import_core(LangsamVM *vm, LV env) {
  langsam_def(vm, env, "true", langsam_true);
  langsam_def(vm, env, "false", langsam_false);
  langsam_def(vm, env, "Type", langsam_type(LT_TYPE));
  langsam_def(vm, env, "Nil", langsam_type(LT_NIL));
  langsam_def(vm, env, "Exception", langsam_type(LT_EXCEPTION));
  langsam_def(vm, env, "Boolean", langsam_type(LT_BOOLEAN));
  langsam_def(vm, env, "Integer", langsam_type(LT_INTEGER));
  langsam_def(vm, env, "Float", langsam_type(LT_FLOAT));
  langsam_def(vm, env, "String", langsam_type(LT_STRING));
  langsam_def(vm, env, "Symbol", langsam_type(LT_SYMBOL));
  langsam_def(vm, env, "Keyword", langsam_type(LT_KEYWORD));
  langsam_def(vm, env, "Opword", langsam_type(LT_OPWORD));
  langsam_def(vm, env, "Cons", langsam_type(LT_CONS));
  langsam_def(vm, env, "Vector", langsam_type(LT_VECTOR));
  langsam_def(vm, env, "Map", langsam_type(LT_MAP));
  langsam_def(vm, env, "Function", langsam_type(LT_FUNCTION));
  langsam_def(vm, env, "ConsIterator", langsam_type(LT_CONSITERATOR));
  langsam_def(vm, env, "VectorIterator", langsam_type(LT_VECTORITERATOR));
  langsam_def(vm, env, "MapIterator", langsam_type(LT_MAPITERATOR));
  langsam_defn(vm, env, "hash", eval_hash);
  langsam_defn(vm, env, "eq", eval_eq);
  langsam_defn(vm, env, "=", eval_equal);
  langsam_defn(vm, env, "cmp", eval_cmp);
  langsam_defn(vm, env, "<", eval_lt);
  langsam_defn(vm, env, "<=", eval_le);
  langsam_defn(vm, env, ">=", eval_ge);
  langsam_defn(vm, env, ">", eval_gt);
  langsam_defn(vm, env, "+", eval_add);
  langsam_defn(vm, env, "-", eval_sub);
  langsam_defn(vm, env, "*", eval_mul);
  langsam_defn(vm, env, "/", eval_div);
  langsam_defn(vm, env, "mod", eval_mod);
  langsam_defn(vm, env, "get", eval_get);
  langsam_defn(vm, env, "put", eval_put);
  langsam_defn(vm, env, "del", eval_del);
  langsam_defn(vm, env, "len", eval_len);
  langsam_defn(vm, env, "iter", eval_iter);
  langsam_defn(vm, env, "deref", eval_deref);
  langsam_defn(vm, env, "eval", eval_eval);
  langsam_defn(vm, env, "repr", eval_repr);
  langsam_defn(vm, env, "str", eval_str);
  langsam_defn(vm, env, "getproto", eval_getproto);
  langsam_defn(vm, env, "setproto", eval_setproto);
  langsam_defn(vm, env, "gep", eval_gep);
  langsam_defn(vm, env, "next", eval_next);
  langsam_defspecial(vm, env, "quote", eval_quote);
  langsam_defspecial(vm, env, "quasiquote", eval_quasiquote);
  langsam_defspecial(vm, env, "apply", eval_apply);
  langsam_defspecial(vm, env, "def", eval_def);
  langsam_defspecial(vm, env, "defn", eval_defn);
  langsam_defspecial(vm, env, "fn", eval_fn);
  langsam_defspecial(vm, env, "defmacro", eval_defmacro);
  langsam_defspecial(vm, env, "macro", eval_macro);
  langsam_defspecial(vm, env, "do", eval_do);
  langsam_defspecial(vm, env, "if", eval_if);
  langsam_defspecial(vm, env, "while", eval_while);
  langsam_defspecial(vm, env, "let", eval_let);
  langsam_defspecial(vm, env, "if-let", eval_if_let);
  langsam_defspecial(vm, env, "catch", eval_catch);
  langsam_defspecial(vm, env, "assert", eval_assert);
  langsam_defspecial(vm, env, "debug", eval_debug);
  langsam_defspecial(vm, env, "curlet", eval_curlet);
  langsam_defspecial(vm, env, "rootlet", eval_rootlet);
  langsam_defn(vm, env, "bind", eval_bind);
  langsam_defn(vm, env, "macro?", eval_macrop);
  langsam_defn(vm, env, "macroexpand-1", eval_macroexpand_1);
  langsam_defn(vm, env, "throw", eval_throw);
  langsam_defn(vm, env, "type", eval_type);
  langsam_defn(vm, env, "cons", eval_cons);
  langsam_defn(vm, env, "list", eval_list);
  langsam_defn(vm, env, "car", eval_car);
  langsam_defn(vm, env, "cdr", eval_cdr);
  langsam_defn(vm, env, "setcar", eval_setcar);
  langsam_defn(vm, env, "setcdr", eval_setcdr);
  langsam_defn(vm, env, "nreverse", eval_nreverse);
  langsam_defn(vm, env, "require", eval_require);
  langsam_defn(vm, env, "read-string", eval_read_string);
  langsam_defn(vm, env, "gc", eval_gc);
  return langsam_loadstringn(vm, env, langsam_l_bytes, langsam_l_len);
}

// VM

typedef struct LangsamModule {
  const char *name;
  LangsamImportFn import;
  struct LangsamModule *next;
} LangsamModule;

static LangsamModule *registered_modules = NULL;

LV langsam_require(LangsamVM *vm, char *module_name) {
  LV module_iname = langsam_istring(vm, module_name);
  LV modules = langsam_get(vm, vm->rootlet, langsam_symbol(vm, "modules"));
  LV module = langsam_get(vm, modules, module_iname);
  if (langsam_somep(module)) {
    return module;
  }
  LangsamModule *m = registered_modules;
  while (m) {
    if (strcmp(m->name, module_name) == 0) {
      module = langsam_map(vm, vm->rootlet, 64);
      LV import_result = m->import(vm, module);
      LANGSAM_CHECK(import_result);
      langsam_put(vm, modules, module_iname, module);
      return module;
    }
    m = m->next;
  }
  return langsam_exceptionf(vm, "require", "cannot find module: %s",
                            module_name);
}

void langsam_register_module(const char *name, LangsamImportFn import) {
  LangsamModule *m = malloc(sizeof(LangsamModule));
  m->name = name;
  m->import = import;
  m->next = registered_modules;
  registered_modules = m;
}

static void langsam_unregister_modules(void) {
  LangsamModule *m = registered_modules;
  while (m) {
    LangsamModule *p = m;
    m = m->next;
    free(p);
  }
  registered_modules = NULL;
}

LV langsam_pushroot(LangsamVM *vm, LV root) {
  if (vm->numroots == LANGSAM_MAX_ROOTS) {
    return langsam_exceptionf(vm, "pushroot",
                              "number of roots reached maximum (%d)",
                              LANGSAM_MAX_ROOTS);
  }
  vm->roots[vm->numroots++] = root;
  return langsam_nil;
}

LV langsam_poproot(LangsamVM *vm) {
  if (vm->numroots == 0) {
    return langsam_exceptionf(vm, "poproot", "root stack underflow");
  }
  vm->numroots--;
  return langsam_nil;
}

LV langsam_pushlet(LangsamVM *vm, LV env) {
  if (vm->numlets == LANGSAM_MAX_LETS) {
    return langsam_exceptionf(vm, "pushlet", "env stack overflow",
                              LANGSAM_MAX_LETS);
  }
  vm->lets[vm->numlets++] = env;
  vm->curlet = env;
  return env;
}

LV langsam_poplet(LangsamVM *vm) {
  if (vm->numlets < 2) {
    return langsam_exceptionf(vm, "poplet", "env stack underflow");
  }
  vm->numlets--;
  vm->curlet = vm->lets[vm->numlets - 1];
  return vm->curlet;
}

void langsam_def(LangsamVM *vm, LV env, char *name, LV value) {
  langsam_put(vm, env, langsam_symbol(vm, name), value);
}

static void define_nativefn(LangsamVM *vm, LV env, char *name,
                            LangsamNativeFn fn, bool evalargs,
                            bool evalresult) {
  LV namesym = langsam_symbol(vm, name);
  LV desc = langsam_map(vm, langsam_nil, 4);
  langsam_put(vm, desc, langsam_keyword(vm, "name"), namesym);
  langsam_put(vm, desc, langsam_keyword(vm, "evalargs"),
              langsam_boolean(evalargs));
  langsam_put(vm, desc, langsam_keyword(vm, "evalresult"),
              langsam_boolean(evalresult));
  LV function = langsam_Function_cast(vm, desc);
  LangsamFunction *f = function.p;
  f->fn = fn;
  langsam_put(vm, env, namesym, function);
}

void langsam_defn(LangsamVM *vm, LV env, char *name, LangsamNativeFn fn) {
  define_nativefn(vm, env, name, fn, true, false);
}

void langsam_defspecial(LangsamVM *vm, LV env, char *name, LangsamNativeFn fn) {
  define_nativefn(vm, env, name, fn, false, false);
}

// logging

void langsam_loglevel(LangsamVM *vm, LangsamLogLevel level) {
  vm->loglevel = level;
}

void langsam_log(LangsamVM *vm, LangsamLogLevel level, const char *fmt, ...) {
  if (vm->loglevel < level) {
    return;
  }
  va_list args;
  va_start(args, fmt);
  char *result;
  int len = vasprintf(&result, fmt, args);
  if (len >= 0) {
    fprintf(stderr, "%s\n", result);
    free(result);
  }
  va_end(args);
}

// VM

LV langsam_init(LangsamVM *vm, LangsamVMOpts *opts) {
  vm->allocator = &langsam_default_allocator;
  if (opts) {
    if (opts->allocator) {
      vm->allocator = opts->allocator;
    }
  }
  vm->gcobjects = NULL;
  vm->gcmarkcolor = LANGSAM_GC_BLACK;
  vm->roots = langsam_alloc(vm, LANGSAM_MAX_ROOTS * sizeof(LV));
  vm->numroots = 0;
  vm->lets = langsam_alloc(vm, LANGSAM_MAX_LETS * sizeof(LV));
  vm->numlets = 0;
  vm->strings = langsam_map(vm, langsam_nil, 4096);
  vm->rootlet = langsam_map(vm, langsam_nil, 4096);
  langsam_pushlet(vm, vm->rootlet);
  LV modules = langsam_map(vm, langsam_nil, 64);
  langsam_def(vm, vm->curlet, "modules", modules);
  vm->repl = false;
  vm->loglevel = LANGSAM_INFO;
  vm->evaldepth = 0;
  vm->reprdepth = 0;
  LV result = langsam_import_core(vm, vm->curlet);
  LANGSAM_CHECK(result);
  LV mainlet = langsam_map(vm, vm->curlet, 4096);
  langsam_pushlet(vm, mainlet);
  return langsam_nil;
}

void langsam_enable_repl_mode(LangsamVM *vm) { vm->repl = true; }

typedef struct {
  LangsamVM *vm;
  void *buf;
  LangsamSize len;
  LangsamSize cap;
} StringBuilder;

static void StringBuilder_init(StringBuilder *sb, LangsamVM *vm) {
  sb->vm = vm;
  sb->buf = NULL;
  sb->len = 0;
  sb->cap = 0;
}

static void StringBuilder_reset(StringBuilder *sb) {
  if (sb->buf) {
    langsam_free(sb->vm, sb->buf);
  }
  sb->buf = NULL;
  sb->len = 0;
  sb->cap = 0;
}

static void StringBuilder_write_byte(StringBuilder *sb, uint8_t b) {
  if (sb->cap == 0) {
    sb->buf = langsam_alloc(sb->vm, 4096);
    sb->cap = 4096;
  } else if (sb->len == sb->cap) {
    sb->cap *= 2;
    sb->buf = langsam_realloc(sb->vm, sb->buf, sb->cap);
  }
  uint8_t *buf = (uint8_t *)sb->buf;
  buf[sb->len++] = b;
}

static LV StringBuilder_result_as_string(StringBuilder *sb) {
  LV result = langsam_stringn(sb->vm, sb->buf, sb->len);
  StringBuilder_reset(sb);
  return result;
}

static LV StringBuilder_result_as_symbol(StringBuilder *sb) {
  LV result = langsam_symboln(sb->vm, sb->buf, sb->len);
  StringBuilder_reset(sb);
  return result;
}

typedef LV (*ByteReadFunc)(LangsamVM *vm, void *data);

typedef struct {
  LangsamVM *vm;
  ByteReadFunc readbyte;
  void *readbyte_data;
  uint8_t buffer[1];
  LangsamSize bufsize;
  LangsamIndex pos;
} Reader;

static void Reader_init(Reader *r, LangsamVM *vm, ByteReadFunc readbyte,
                        void *readbyte_data) {
  r->vm = vm;
  r->readbyte = readbyte;
  r->readbyte_data = readbyte_data;
  r->bufsize = 0;
  r->pos = 0;
}

static LV Reader_readbyte(Reader *r) {
  LV result;
  if (r->bufsize > 0) {
    r->bufsize = 0;
    result = langsam_integer(r->buffer[0]);
  } else {
    result = r->readbyte(r->vm, r->readbyte_data);
  }
  if (result.type == LT_INTEGER) {
    r->pos++;
  }
  return result;
}

static void Reader_unreadbyte(Reader *r, uint8_t c) {
  r->buffer[0] = c;
  r->bufsize = 1;
  r->pos--;
}

static bool is_whitespace(uint8_t c) { return c <= 0x20; }

static LV Reader_readbyte_skipws(Reader *r) {
  uint8_t c;
  LV result;
start:
  result = Reader_readbyte(r);
  if (result.type == LT_INTEGER) {
    c = (uint8_t)result.i;
  } else {
    return result;
  }
  if (is_whitespace(c)) {
    goto start;
  } else if (c == ';') {
    while (1) {
      LV comment_result = Reader_readbyte(r);
      if (comment_result.type == LT_INTEGER) {
        if (comment_result.i == '\n') {
          goto start;
        }
      } else {
        return comment_result;
      }
    }
  } else if (c == '#' && r->pos == 1) {
    LV shebang_result = Reader_readbyte(r);
    if (shebang_result.type == LT_INTEGER && shebang_result.i == '!') {
      while (1) {
        shebang_result = Reader_readbyte(r);
        if (shebang_result.type == LT_INTEGER) {
          if (shebang_result.i == '\n') {
            goto start;
          }
        } else {
          return shebang_result;
        }
      }
    } else {
      Reader_unreadbyte(r, (uint8_t)shebang_result.i);
      return result;
    }
  } else {
    return result;
  }
}

static bool is_symbol_char(uint8_t c) {
  if (is_whitespace(c)) {
    return false;
  }
  if (c == '(' || c == ')') {
    return false;
  }
  if (c == '[' || c == ']') {
    return false;
  }
  if (c == '{' || c == '}') {
    return false;
  }
  return true;
}

static LV Reader_read_symbol(Reader *r, uint8_t first) {
  StringBuilder sb;
  StringBuilder_init(&sb, r->vm);
  StringBuilder_write_byte(&sb, first);
  LV lhs = langsam_nil;
  uint8_t seen_sep = 0;
  while (1) {
    LV result = Reader_readbyte(r);
    if (langsam_exceptionp(result)) {
      StringBuilder_reset(&sb);
      return result;
    } else if (langsam_nilp(result)) {
      break;
    }
    uint8_t c = (uint8_t)result.i;
    if (!is_symbol_char(c)) {
      Reader_unreadbyte(r, c);
      break;
    }
    if (c == '/' || c == '.') {
      if (seen_sep) {
        if (seen_sep == c) {
          return langsam_exceptionf(
              r->vm, "read", "symbol contains multiple %c characters", c);
        } else {
          return langsam_exceptionf(r->vm, "read",
                                    "symbol contains both / and . characters");
        }
      }
      seen_sep = c;
      lhs = StringBuilder_result_as_symbol(&sb);
    } else {
      StringBuilder_write_byte(&sb, c);
    }
  }
  if (seen_sep) {
    LV rhs = StringBuilder_result_as_symbol(&sb);
    LV result = langsam_nil;
    if (seen_sep == '/') {
      LV get = langsam_get(r->vm, r->vm->rootlet, langsam_symbol(r->vm, "get"));
      result = langsam_cons(r->vm, langsam_quote(r->vm, rhs), result);
      result = langsam_cons(r->vm, lhs, result);
      result = langsam_cons(r->vm, get, result);
    } else {
      LV cons =
          langsam_get(r->vm, r->vm->rootlet, langsam_symbol(r->vm, "cons"));
      result = langsam_cons(r->vm, langsam_quote(r->vm, rhs), result);
      result = langsam_cons(r->vm, lhs, result);
      result = langsam_cons(r->vm, cons, result);
    }
    return result;
  } else {
    return StringBuilder_result_as_symbol(&sb);
  }
}

static LV Reader_read_keyword(Reader *r) {
  StringBuilder sb;
  StringBuilder_init(&sb, r->vm);
  while (1) {
    LV result = Reader_readbyte(r);
    if (langsam_exceptionp(result)) {
      StringBuilder_reset(&sb);
      return result;
    } else if (langsam_nilp(result)) {
      break;
    }
    uint8_t c = (uint8_t)result.i;
    if (!is_symbol_char(c)) {
      Reader_unreadbyte(r, c);
      break;
    }
    StringBuilder_write_byte(&sb, c);
  }
  LV sym = StringBuilder_result_as_symbol(&sb);
  return (LV){
      .type = LT_KEYWORD,
      .p = sym.p,
  };
}

static LV Reader_read_opword(Reader *r) {
  StringBuilder sb;
  StringBuilder_init(&sb, r->vm);
  while (1) {
    LV result = Reader_readbyte(r);
    if (langsam_exceptionp(result)) {
      StringBuilder_reset(&sb);
      return result;
    } else if (langsam_nilp(result)) {
      break;
    }
    uint8_t c = (uint8_t)result.i;
    if (!is_symbol_char(c)) {
      Reader_unreadbyte(r, c);
      break;
    }
    StringBuilder_write_byte(&sb, c);
  }
  LV sym = StringBuilder_result_as_symbol(&sb);
  return (LV){
      .type = LT_OPWORD,
      .p = sym.p,
  };
}

static LV Reader_read_integer_in_radix(Reader *r, int radix) {
  LangsamInteger value = 0;
  int digits_read = 0;
  while (1) {
    LV result = Reader_readbyte(r);
    if (langsam_exceptionp(result)) {
      return result;
    } else if (langsam_nilp(result)) {
      if (digits_read > 0) {
        return langsam_integer(value);
      } else {
        return langsam_exceptionf(r->vm, "read", "incomplete integer constant");
      }
    }
    uint8_t c = (uint8_t)result.i;
    if (c >= 'a') {
      c = c - ('a' - 'A');
    }
    if (c >= 'A') {
      c = c - ('A' - '9') + 1;
    }
    LangsamInteger digit = c - '0';
    if (digit < 0 || digit >= radix) {
      Reader_unreadbyte(r, c);
      return langsam_integer(value);
    }
    value = (value * radix) + digit;
    digits_read++;
  }
}

static LV Reader_read_float(Reader *r, LangsamInteger integer_part) {
  LangsamFloat value = (LangsamFloat)integer_part;
  LangsamFloat divisor = 1;
  while (1) {
    LV result = Reader_readbyte(r);
    if (langsam_exceptionp(result)) {
      return result;
    } else if (langsam_nilp(result)) {
      return langsam_float(value / divisor);
    }
    uint8_t c = (uint8_t)result.i;
    if (c < '0' || c > '9') {
      Reader_unreadbyte(r, c);
      return langsam_float(value / divisor);
    }
    LangsamFloat digit = (LangsamFloat)(c - '0');
    value = value * 10 + digit;
    divisor *= 10;
  }
}

static LV Reader_read_number(Reader *r, uint8_t first) {
  LangsamInteger value = first - '0';
  bool check_prefix = true;
  while (1) {
    LV result = Reader_readbyte(r);
    if (langsam_exceptionp(result)) {
      return result;
    } else if (langsam_nilp(result)) {
      return langsam_integer(value);
    }
    uint8_t c = (uint8_t)result.i;
    if (check_prefix && value == 0) {
      switch (c) {
      case 'b':
        return Reader_read_integer_in_radix(r, 2);
      case 'o':
        return Reader_read_integer_in_radix(r, 8);
      case 'x':
        return Reader_read_integer_in_radix(r, 16);
      }
    }
    if (c == '.') {
      return Reader_read_float(r, value);
    }
    if (c < '0' || c > '9') {
      Reader_unreadbyte(r, c);
      return langsam_integer(value);
    }
    LangsamInteger digit = c - '0';
    value = (value * 10) + digit;
    check_prefix = false;
  }
}

static LV Reader_read_string(Reader *r) {
  StringBuilder sb;
  StringBuilder_init(&sb, r->vm);
  while (1) {
    LV result = Reader_readbyte(r);
    if (langsam_exceptionp(result)) {
      StringBuilder_reset(&sb);
      return result;
    } else if (langsam_nilp(result)) {
      return StringBuilder_result_as_string(&sb);
    }
    uint8_t c = (uint8_t)result.i;
    if (c == '"') {
      return StringBuilder_result_as_string(&sb);
    }
    if (c == '\\') {
      LV escape_result = Reader_readbyte(r);
      if (langsam_exceptionp(escape_result)) {
        StringBuilder_reset(&sb);
        return escape_result;
      } else if (langsam_nilp(escape_result)) {
        return langsam_exceptionf(r->vm, "read",
                                  "incomplete character escape sequence");
      }
      c = (uint8_t)escape_result.i;
      switch (c) {
      case 'a':
        c = 0x07;
        break;
      case 'b':
        c = 0x08;
        break;
      case 'f':
        c = 0x0c;
        break;
      case 'n':
        c = 0x0a;
        break;
      case 'r':
        c = 0x0d;
        break;
      case 't':
        c = 0x09;
        break;
      case 'v':
        c = 0x0b;
        break;
      }
    }
    StringBuilder_write_byte(&sb, c);
  }
}

static LV Reader_read(Reader *r);

static LV Reader_read_cons(Reader *r) {
  LV value = langsam_nil;
  LangsamSize len = 0;
  while (1) {
    LV result = Reader_readbyte_skipws(r);
    if (langsam_exceptionp(result)) {
      return result;
    } else if (langsam_nilp(result)) {
      return langsam_exceptionf(r->vm, "read", "incomplete list");
    }
    uint8_t c = (uint8_t)result.i;
    if (c == ')') {
      if (len >= 3) {
        LV second = langsam_car(langsam_cdr(value));
        LV dot_symbol = langsam_symbol(r->vm, ".");
        if (LVEQ(second, dot_symbol)) {
          LV head = langsam_car(value);
          LV tail = langsam_cdr(langsam_cdr(value));
          return langsam_nreverse_with_last(tail, head);
        }
      }
      return langsam_nreverse(value);
    } else {
      Reader_unreadbyte(r, c);
    }
    LV form = Reader_read(r);
    if (langsam_exceptionp(form)) {
      return form;
    } else if (langsam_nilp(form)) {
      return langsam_exceptionf(r->vm, "read", "incomplete list");
    }
    value = langsam_cons(r->vm, form, value);
    len++;
  }
}

static LV Reader_read_vector(Reader *r) {
  LV items = langsam_nil;
  LangsamSize len = 0;
  while (1) {
    LV result = Reader_readbyte_skipws(r);
    if (langsam_exceptionp(result)) {
      return result;
    } else if (langsam_nilp(result)) {
      return langsam_exceptionf(r->vm, "read", "incomplete vector");
    }
    uint8_t c = (uint8_t)result.i;
    if (c == ']') {
      LV vec = langsam_vector(r->vm, len);
      LV index = langsam_integer(len - 1);
      for (LV list = items; langsam_consp(list); list = langsam_cdr(list)) {
        langsam_put(r->vm, vec, index, langsam_car(list));
        index.i--;
      }
      return vec;
    } else {
      Reader_unreadbyte(r, c);
    }
    LV form = Reader_read(r);
    if (langsam_exceptionp(form)) {
      return form;
    } else if (langsam_nilp(form)) {
      return langsam_exceptionf(r->vm, "read", "incomplete vector");
    }
    items = langsam_cons(r->vm, form, items);
    len++;
  }
}

static LV Reader_read_map(Reader *r) {
  LV items = langsam_nil;
  LangsamSize len = 0;
  while (1) {
    LV result = Reader_readbyte_skipws(r);
    if (langsam_exceptionp(result)) {
      return result;
    } else if (langsam_nilp(result)) {
      return langsam_exceptionf(r->vm, "read", "incomplete map");
    }
    uint8_t c = (uint8_t)result.i;
    if (c == '}') {
      if (len % 2 != 0) {
        return langsam_exceptionf(r->vm, "read",
                                  "map literal with odd number of elements");
      }
      LV map = langsam_map(r->vm, langsam_nil, len / 2);
      for (LV list = items; langsam_consp(list);) {
        LV v = langsam_car(list);
        list = langsam_cdr(list);
        LV k = langsam_car(list);
        list = langsam_cdr(list);
        langsam_put(r->vm, map, k, v);
      }
      return map;
    } else {
      Reader_unreadbyte(r, c);
    }
    LV form = Reader_read(r);
    if (langsam_exceptionp(form)) {
      return form;
    } else if (langsam_nilp(form)) {
      return langsam_exceptionf(r->vm, "read", "incomplete map");
    }
    items = langsam_cons(r->vm, form, items);
    len++;
  }
}

static LV Reader_read(Reader *r) {
  LV result = Reader_readbyte_skipws(r);
  if (result.type != LT_INTEGER) {
    return result;
  }
  uint8_t c = (uint8_t)result.i;
  if (c == '-') {
    LV first = Reader_readbyte(r);
    if (first.type == LT_INTEGER && first.i >= '0' && first.i <= '9') {
      LV number_result = Reader_read_number(r, (uint8_t)first.i);
      if (number_result.type == LT_INTEGER) {
        return langsam_integer(-1 * number_result.i);
      } else if (number_result.type == LT_FLOAT) {
        return langsam_float(-1 * number_result.f);
      } else {
        return number_result;
      }
    } else {
      Reader_unreadbyte(r, (uint8_t)first.i);
      return Reader_read_symbol(r, '-');
    }
  } else if (c >= '0' && c <= '9') {
    return Reader_read_number(r, c);
  } else if (c == '"') {
    return Reader_read_string(r);
  } else if (c == '(') {
    return Reader_read_cons(r);
  } else if (c == '[') {
    return Reader_read_vector(r);
  } else if (c == '{') {
    return Reader_read_map(r);
  } else if (c == ':') {
    return Reader_read_keyword(r);
  } else if (c == '%') {
    return Reader_read_opword(r);
  } else if (c == '@') {
    LV form = Reader_read(r);
    if (langsam_exceptionp(form)) {
      return form;
    } else if (langsam_nilp(form)) {
      return langsam_exceptionf(r->vm, "read",
                                "missing argument to @ (deref) operator");
    }
    LV tail = langsam_cons(r->vm, form, langsam_nil);
    return langsam_cons(r->vm, langsam_symbol(r->vm, "deref"), tail);
  } else if (c == '\'') {
    LV form = Reader_read(r);
    if (langsam_exceptionp(form)) {
      return form;
    }
    LV tail = langsam_cons(r->vm, form, langsam_nil);
    return langsam_cons(r->vm, langsam_symbol(r->vm, "quote"), tail);
  } else if (c == '`') {
    LV form = Reader_read(r);
    if (langsam_exceptionp(form)) {
      return form;
    }
    LV tail = langsam_cons(r->vm, form, langsam_nil);
    return langsam_cons(r->vm, langsam_symbol(r->vm, "quasiquote"), tail);
  } else if (c == ',') {
    LV unquote_result = Reader_readbyte(r);
    if (langsam_exceptionp(unquote_result)) {
      return unquote_result;
    } else if (langsam_nilp(unquote_result)) {
      return langsam_exceptionf(r->vm, "read",
                                "missing argument to , (unquote) operator");
    }
    uint8_t uc = (uint8_t)unquote_result.i;
    if (uc == '@') {
      LV form = Reader_read(r);
      if (langsam_exceptionp(form)) {
        return form;
      }
      LV tail = langsam_cons(r->vm, form, langsam_nil);
      return langsam_cons(r->vm, langsam_symbol(r->vm, "unquote-splicing"),
                          tail);
    } else {
      Reader_unreadbyte(r, uc);
      LV form = Reader_read(r);
      if (langsam_exceptionp(form))
        return form;
      LV tail = langsam_cons(r->vm, form, langsam_nil);
      return langsam_cons(r->vm, langsam_symbol(r->vm, "unquote"), tail);
    }
  } else {
    return Reader_read_symbol(r, c);
  }
}

static LV langsam_read(LangsamVM *vm, ByteReadFunc readbyte,
                       void *readbyte_data) {
  Reader r;
  Reader_init(&r, vm, readbyte, readbyte_data);
  return Reader_read(&r);
}

static LV langsam_load(LangsamVM *vm, LV env, ByteReadFunc readbyte,
                       void *readbyte_data) {
  if (langsam_nilp(env)) {
    env = vm->curlet;
  }
  LANGSAM_CHECK(langsam_pushlet(vm, env));
  Reader r;
  Reader_init(&r, vm, readbyte, readbyte_data);
  LV result = langsam_nil;
  while (1) {
    if (vm->repl) {
      fprintf(stdout, "@L> ");
      fflush(stdout);
    }
    LV form = Reader_read(&r);
    if (langsam_exceptionp(form)) {
      if (vm->repl) {
        fprintf(stderr, "%s\n", langsam_cstr(vm, form));
        fflush(stderr);
        continue;
      } else {
        langsam_poplet(vm);
        return form;
      }
    }
    if (langsam_nilp(form)) {
      break;
    }
    langsam_debug(vm, "> %s", langsam_cstr(vm, form));
    result = langsam_eval(vm, form);
    if (langsam_exceptionp(result)) {
      if (vm->repl) {
        fprintf(stderr, "%s\n", langsam_cstr(vm, result));
        fflush(stderr);
        result = langsam_nil;
      } else {
        langsam_poplet(vm);
        return result;
      }
    } else if (vm->repl) {
      fprintf(stdout, "%s\n", langsam_cstr(vm, result));
    }
  }
  langsam_poplet(vm);
  return result;
}

static LV readbyte_fd(LangsamVM *vm, void *data) {
  int fd = (int)(intptr_t)data;
  uint8_t c;
  ssize_t bytes_read = read(fd, &c, 1);
  if (bytes_read < 0) {
    return langsam_exceptionf(vm, "io", "%s", strerror(errno));
  }
  if (bytes_read == 0) {
    return langsam_nil;
  }
  return langsam_integer(c);
}

LV langsam_loadfd(LangsamVM *vm, LV env, int fd) {
  return langsam_load(vm, env, readbyte_fd, (void *)(intptr_t)fd);
}

LV langsam_loadfile(LangsamVM *vm, LV env, const char *path) {
  langsam_debug(vm, "loading %s", path);
  int fd = open(path, O_RDONLY);
  if (fd == -1) {
    return langsam_exceptionf(vm, "io", "cannot open %s: %s", path,
                              strerror(errno));
  }
  LV result = langsam_loadfd(vm, env, fd);
  close(fd);
  return result;
}

typedef struct {
  uint8_t *data;
  LangsamSize len;
  LangsamSize index;
} ReadByteStringState;

static LV readbyte_string(LangsamVM *vm, void *data) {
  ReadByteStringState *state = (ReadByteStringState *)data;
  if (state->index == state->len) {
    return langsam_nil;
  }
  return langsam_integer(state->data[state->index++]);
}

LV langsam_readstring(LangsamVM *vm, char *s) {
  LangsamSize len = (LangsamSize)strlen(s);
  return langsam_readstringn(vm, s, len);
}

LV langsam_readstringn(LangsamVM *vm, char *s, LangsamSize len) {
  ReadByteStringState state = {
      .data = (uint8_t *)s,
      .len = len,
      .index = 0,
  };
  return langsam_read(vm, readbyte_string, &state);
}

LV langsam_loadstring(LangsamVM *vm, LV env, char *s) {
  LangsamSize len = (LangsamSize)strlen(s);
  return langsam_loadstringn(vm, env, s, len);
}

LV langsam_loadstringn(LangsamVM *vm, LV env, char *s, LangsamSize len) {
  ReadByteStringState state = {
      .data = (uint8_t *)s,
      .len = len,
      .index = 0,
  };
  return langsam_load(vm, env, readbyte_string, &state);
}

void langsam_close(LangsamVM *vm) {
  vm->strings = langsam_nil;
  vm->rootlet = langsam_nil;
  vm->curlet = langsam_nil;
  langsam_gcfree_all(vm);
  langsam_unregister_modules();
  langsam_free(vm, vm->roots);
  langsam_free(vm, vm->lets);
}
