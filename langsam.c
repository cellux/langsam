#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "langsam.h"

#define LANGSAM_MAX_ROOTS 4096

// hash functions

#define FNV1A_OFFSET_BASIS 0xcbf29ce484222325
#define FNV1A_PRIME 0x00000100000001b3

static uint64_t fnv1a_64_mix(uint64_t hash, uint8_t *p, size_t len) {
  for (size_t i = 0; i < len; i++) {
    hash = hash ^ *p++;
    hash = hash * FNV1A_PRIME;
  }
  return hash;
}

#define HASH_SEED FNV1A_OFFSET_BASIS

#define FNV1A_NIL 0x2146ba19257dc6ac
#define FNV1A_TRUE 0x5b5c98ef514dbfa5
#define FNV1A_FALSE 0xb5fae2c14238b978

static uint64_t hash_ptr(uint64_t hash, void *p) {
  return fnv1a_64_mix(hash, p, sizeof(void *));
}
static uint64_t hash_uint64(uint64_t hash, uint64_t u) {
  return fnv1a_64_mix(hash, (uint8_t *)&u, sizeof(uint64_t));
}

static uint64_t hash_boolean(uint64_t hash, LangsamBoolean b) {
  uint64_t bhash = b ? FNV1A_TRUE : FNV1A_FALSE;
  return fnv1a_64_mix(hash, (uint8_t *)&bhash, sizeof(LangsamBoolean));
}
static uint64_t hash_integer(uint64_t hash, LangsamInteger i) {
  return fnv1a_64_mix(hash, (uint8_t *)&i, sizeof(LangsamInteger));
}
static uint64_t hash_float(uint64_t hash, LangsamFloat f) {
  return fnv1a_64_mix(hash, (uint8_t *)&f, sizeof(LangsamFloat));
}
static uint64_t hash_string(uint64_t hash, char *s, LangsamSize len) {
  return fnv1a_64_mix(hash, (uint8_t *)s, (size_t)len);
}

// core API

bool langsam_truthy(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  return t->truthy ? t->truthy(vm, self) : true;
}

uint64_t langsam_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  LangsamType t = self.type;
  uint64_t hash = hash_ptr(prevhash, t);
  return t->hash ? t->hash(vm, self, hash) : hash_ptr(hash, self.p);
}

LV langsam_cast(LangsamVM *vm, LV type, LV other) {
  LangsamType t = type.p;
  if (t == other.type) {
    return other;
  }
  if (t->cast == NULL) {
    char *type_name = langsam_cstr(vm, type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "cast",
                              "cannot cast %s to %s: type `%s` does not "
                              "support cast",
                              other_type_name, type_name, type_name);
  }
  return t->cast(vm, other);
}

LV langsam_equal(LangsamVM *vm, LV self, LV other) {
  if (LVEQ(self, other)) {
    return langsam_true;
  }
  LangsamType t1 = self.type;
  if (t1->equal == NULL && t1->cmp == NULL) {
    return LVEQ(self, other) ? langsam_true : langsam_false;
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    LANGSAM_CHECK(other);
  }
  if (t1->equal) {
    return t1->equal(vm, self, other);
  }
  if (t1->cmp) {
    LV cmp_result = t1->cmp(vm, self, other);
    LANGSAM_CHECK(cmp_result);
    return langsam_boolean(cmp_result.i == 0);
  }
  return langsam_false;
}

LV langsam_cmp(LangsamVM *vm, LV self, LV other) {
  if (LVEQ(self, other)) {
    return langsam_integer(0);
  }
  LangsamType t1 = self.type;
  if (t1->cmp == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "cmp", "type `%s` does not support cmp",
                              self_type_name, other_type_name, self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    LANGSAM_CHECK(other);
  }
  return t1->cmp(vm, self, other);
}

LV langsam_add(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->add == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "add", "type `%s` does not support add",
                              other_type_name, self_type_name, self_type_name);
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
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "sub", "type `%s` does not support sub",
                              other_type_name, self_type_name, self_type_name);
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
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "mul", "type `%s` does not support mul",
                              self_type_name, other_type_name, self_type_name);
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
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "div", "type `%s` does not support div",
                              self_type_name, other_type_name, self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    LANGSAM_CHECK(other);
  }
  return t1->div(vm, self, other);
}

LV langsam_get(LangsamVM *vm, LV self, LV key) {
  LangsamType t = self.type;
  if (t->get == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "get", "type `%s` does not support get",
                              self_type_name);
  }
  return t->get(vm, self, key);
}

LV langsam_put(LangsamVM *vm, LV self, LV key, LV value) {
  LangsamType t = self.type;
  if (t->put == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "put", "type `%s` does not support put",
                              self_type_name);
  }
  return t->put(vm, self, key, value);
}

LV langsam_del(LangsamVM *vm, LV self, LV key) {
  LangsamType t = self.type;
  if (t->del == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "del", "type `%s` does not support del",
                              self_type_name);
  }
  return t->del(vm, self, key);
}

LV langsam_len(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->len == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "len", "type `%s` does not support len",
                              self_type_name);
  }
  return t->len(vm, self);
}

LV langsam_iter(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->iter == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "iter", "type `%s` does not support iter",
                              self_type_name);
  }
  return t->iter(vm, self);
}

LV langsam_deref(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->deref == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "deref", "type `%s` does not support deref",
                              self_type_name);
  }
  return t->deref(vm, self);
}

LV langsam_apply(LangsamVM *vm, LV self, LV args) {
  LangsamType t = self.type;
  if (t->apply == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "apply", "type `%s` does not support apply",
                              self_type_name);
  }
  return t->apply(vm, self, args);
}

LV langsam_eval(LangsamVM *vm, LV self) {
  LANGSAM_CHECK(langsam_pushroot(vm, self));
  LangsamType t = self.type;
  LV result = t->eval ? t->eval(vm, self) : self;
  langsam_debug(vm, "EVAL: %s -> %s", langsam_cstr(vm, self),
                langsam_cstr(vm, result));
  langsam_poproot(vm);
  return result;
}

LV langsam_repr(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->repr) {
    return langsam_str(vm, t->repr(vm, self));
  }
  char *self_type_name = langsam_typename(vm, self.type);
  return langsam_format(vm, "<%s>", self_type_name);
}

LV langsam_str(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->str) {
    return t->str(vm, self);
  }
  return langsam_repr(vm, self);
}

// Type

uint64_t langsam_Type_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  return hash_ptr(prevhash, self.p);
}

LV langsam_Type_apply(LangsamVM *vm, LV self, LV args) {
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

char *langsam_typename(LangsamVM *vm, LangsamType t) {
  return langsam_cstr(vm, langsam_type(t));
}

static struct LangsamT LANGSAM_T_TYPE = {
    .name = "Type",
    .gcmanaged = false,
    .hash = langsam_Type_hash,
    .apply = langsam_Type_apply,
    .repr = langsam_Type_repr,
};

const LangsamType LT_TYPE = &LANGSAM_T_TYPE;

// Nil

bool langsam_Nil_truthy(LangsamVM *vm, LV self) { return false; }

uint64_t langsam_Nil_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  return hash_uint64(prevhash, FNV1A_NIL);
}

LV langsam_Nil_repr(LangsamVM *vm, LV self) {
  return langsam_symbol(vm, "nil");
}

static struct LangsamT LANGSAM_T_NIL = {
    .name = "Nil",
    .gcmanaged = false,
    .truthy = langsam_Nil_truthy,
    .hash = langsam_Nil_hash,
    .repr = langsam_Nil_repr,
};

const LangsamType LT_NIL = &LANGSAM_T_NIL;

const LV langsam_nil = {.type = LT_NIL, .p = NULL};

bool langsam_nilp(LV v) { return v.type == LT_NIL; }

// Exception

LangsamSize langsam_Exception_gcmark(LangsamVM *vm, void *p) {
  LangsamException *ex = p;
  return langsam_mark(vm, ex->payload);
}

uint64_t langsam_Exception_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  LangsamException *ex = self.p;
  return langsam_hash(vm, ex->payload, prevhash);
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

uint64_t langsam_Boolean_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  return hash_boolean(prevhash, self.b);
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

uint64_t langsam_Integer_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  return hash_integer(prevhash, self.i);
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
#if INTPTR_MAX == LLONG_MAX
    i = strtoll(s->p, NULL, 0);
#elif INTPTR_MAX == LONG_MAX
    i = strtol(s->p, NULL, 0);
#else
#error "Cannot find a suitable implementation for String->Integer conversion"
#endif
    if (errno) {
      return langsam_exceptionf(
          vm, "cast", "String->Integer conversion failed for: %s", s->p);
    }
    return langsam_integer(i);
  }
  return langsam_exceptionf(vm, "cast",
                            "Cannot cast value of type `%s` to Integer",
                            langsam_typename(vm, other.type));
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

LV langsam_Integer_repr(LangsamVM *vm, LV self) {
  return langsam_format(vm, "%lld", self.i);
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
    .repr = langsam_Integer_repr,
};

const LangsamType LT_INTEGER = &LANGSAM_T_INTEGER;

// Float

bool langsam_Float_truthy(LangsamVM *vm, LV self) { return self.f != 0.0; }

uint64_t langsam_Float_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  return hash_float(prevhash, self.f);
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
#if LANGSAM_FLOAT_MANT_DIG == FLT_MANT_DIG
    f = strtof(s->p, NULL);
#elif LANGSAM_FLOAT_MANT_DIG == DBL_MANT_DIG
    f = strtod(s->p, NULL);
#else
#error "Cannot find a suitable implementation for String->Float conversion"
#endif
    if (errno) {
      return langsam_exceptionf(
          vm, "cast", "String->Float conversion failed for: %s", s->p);
    }
    return langsam_float(f);
  }
  return langsam_exceptionf(vm, "cast",
                            "Cannot cast value of type `%s` to Float",
                            langsam_typename(vm, other.type));
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

LV langsam_Float_repr(LangsamVM *vm, LV self) {
  return langsam_format(vm, "%g", self.f);
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

uint64_t langsam_String_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  LangsamString *s = self.p;
  return hash_string(prevhash, s->p, s->len);
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
    char *key_type_name = langsam_typename(vm, key.type);
    return langsam_exceptionf(
        vm, "get",
        "attempt to index String with non-integer index of type `%s`",
        key_type_name);
  }
  LangsamIndex index = key.i;
  LangsamString *s = self.p;
  if (index >= s->len) {
    return langsam_exceptionf(vm, "get",
                              "String index %lld out of range (0..%lld)", index,
                              s->len - 1);
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
  if (!langsam_nilp(vs)) {
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
  return langsam_exceptionf(vm, "cast",
                            "Cannot cast value of type `%s` to Symbol",
                            langsam_typename(vm, other.type));
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
  return langsam_exceptionf(vm, "cast",
                            "Cannot cast value of type `%s` to Keyword",
                            langsam_typename(vm, other.type));
}

LV langsam_Keyword_apply(LangsamVM *vm, LV self, LV args) {
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
    .apply = langsam_Keyword_apply,
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
  return langsam_exceptionf(vm, "cast",
                            "Cannot cast value of type `%s` to Opword",
                            langsam_typename(vm, other.type));
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
    .apply = langsam_Keyword_apply,
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

bool langsam_Cons_truthy(LangsamVM *vm, LV self) { return !langsam_nilp(self); }

uint64_t langsam_Cons_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  LangsamCons *cons = self.p;
  uint64_t hash = prevhash;
  hash = langsam_hash(vm, cons->car, hash);
  hash = langsam_hash(vm, cons->cdr, hash);
  return hash;
}

LV langsam_Cons_cast(LangsamVM *vm, LV other) {
  LV it = langsam_iter(vm, other);
  LANGSAM_CHECK(it);
  LV result = langsam_nil;
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    result = langsam_cons(vm, item, result);
    it = langsam_next(vm, it);
  }
  return langsam_nreverse(result);
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
    char *key_type_name = langsam_typename(vm, key.type);
    return langsam_exceptionf(
        vm, "get", "attempt to index Cons with non-integer index of type `%s`",
        key_type_name);
  }
  LangsamCons *cons = self.p;
  switch (key.i) {
  case 0:
    return cons->car;
  case 1:
    return cons->cdr;
  default:
    return langsam_exceptionf(vm, "get", "Cons index %lld out of range (0..1)",
                              key.i);
  }
}

LV langsam_Cons_put(LangsamVM *vm, LV self, LV key, LV value) {
  if (key.type != LT_INTEGER) {
    char *key_type_name = langsam_typename(vm, key.type);
    return langsam_exceptionf(
        vm, "put", "attempt to index Cons with non-integer index of type `%s`",
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
    return langsam_exceptionf(vm, "put", "Cons index %lld out of range (0..1)",
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

LV langsam_Cons_apply(LangsamVM *vm, LV self, LV args) {
  LV obj = langsam_car(self);
  LV key = langsam_cdr(self);
  LV method = langsam_get(vm, obj, key);
  LANGSAM_CHECK(method);
  if (langsam_nilp(method)) {
    return langsam_exceptionf(
        vm, "apply", "cannot get method `%s` from value of type `%s`",
        langsam_cstr(vm, key), langsam_typename(vm, obj.type));
  }
  args = langsam_cons(vm, langsam_quote(vm, obj), args);
  return langsam_apply(vm, method, args);
}

LV langsam_Cons_eval(LangsamVM *vm, LV self) {
  LV head = langsam_car(self);
  LV op = langsam_eval(vm, head);
  LANGSAM_CHECK(op);
  if (langsam_nilp(op)) {
    return langsam_exceptionf(
        vm, "eval", "expression in operator position evaluated to nil: %s",
        langsam_cstr(vm, head));
  }
  return langsam_apply(vm, op, langsam_cdr(self));
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
  if (!langsam_nilp(cur)) {
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
  for (LV r = reprs; !langsam_nilp(r); r = langsam_cdr(r)) {
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
    .truthy = langsam_Cons_truthy,
    .hash = langsam_Cons_hash,
    .cast = langsam_Cons_cast,
    .equal = langsam_Cons_equal,
    .get = langsam_Cons_get,
    .put = langsam_Cons_put,
    .iter = langsam_Cons_iter,
    .deref = langsam_Cons_deref,
    .apply = langsam_Cons_apply,
    .eval = langsam_Cons_eval,
    .repr = langsam_Cons_repr,
};

const LangsamType LT_CONS = &LANGSAM_T_CONS;

// iterators

LV langsam_next(LangsamVM *vm, LV it) {
  return langsam_apply(vm, it, langsam_nil);
}

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
                              "attempt to deref consumed list iterator");
  }
  return langsam_car(it->cur);
}

LV langsam_ConsIterator_apply(LangsamVM *vm, LV self, LV args) {
  LangsamConsIterator *it = self.p;
  if (!langsam_consp(it->cur)) {
    return langsam_exceptionf(vm, "apply",
                              "attempt to advance consumed list iterator");
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
    .apply = langsam_ConsIterator_apply,
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

uint64_t langsam_Vector_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  LangsamVector *v = self.p;
  uint64_t hash = prevhash;
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
    l = langsam_cons(vm, langsam_deref(vm, it), l);
    it = langsam_next(vm, it);
    len++;
  }
  LV self = langsam_vector0(vm, len);
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
  LV result = langsam_vector0(vm, len);
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
    char *key_type_name = langsam_typename(vm, key.type);
    return langsam_exceptionf(
        vm, "get",
        "attempt to index Vector with non-integer index of type `%s`",
        key_type_name);
  }
  LangsamIndex index = key.i;
  LangsamVector *v = self.p;
  if (index >= v->len) {
    return langsam_exceptionf(vm, "get",
                              "Vector index %lld out of range (0..%lld)", index,
                              v->len - 1);
  }
  return v->items[index];
}

LV langsam_Vector_put(LangsamVM *vm, LV self, LV key, LV value) {
  if (key.type != LT_INTEGER) {
    char *key_type_name = langsam_typename(vm, key.type);
    return langsam_exceptionf(
        vm, "put",
        "attempt to index Vector with non-integer index of type `%s`",
        key_type_name);
  }
  LangsamIndex index = key.i;
  LangsamVector *v = self.p;
  if (index >= v->len) {
    return langsam_exceptionf(vm, "put",
                              "Vector index %lld out of range (0..%lld)", index,
                              v->len - 1);
  }
  v->items[index] = value;
  return langsam_nil;
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

LV langsam_Vector_apply(LangsamVM *vm, LV self, LV args) {
  LANGSAM_ARG(index, args);
  index = langsam_eval(vm, index);
  LANGSAM_CHECK(index);
  LANGSAM_ARG_TYPE(index, LT_INTEGER);
  return langsam_Vector_get(vm, self, index);
}

LV langsam_Vector_eval(LangsamVM *vm, LV self) {
  LangsamVector *v = self.p;
  LV result = langsam_vector0(vm, v->len);
  LV *result_items = ((LangsamVector *)result.p)->items;
  for (LangsamIndex i = 0; i < v->len; i++) {
    result_items[i] = langsam_eval(vm, v->items[i]);
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
  for (LV r = reprs; !langsam_nilp(r); r = langsam_cdr(r)) {
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

LV langsam_vector0(LangsamVM *vm, LangsamSize len) {
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
  LV result = langsam_vector0(vm, len);
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
    .apply = langsam_Vector_apply,
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
                              "attempt to deref consumed vector iterator");
  }
  return v->items[it->i];
}

LV langsam_VectorIterator_apply(LangsamVM *vm, LV self, LV args) {
  LangsamVectorIterator *it = self.p;
  LangsamVector *v = it->v.p;
  if (it->i >= v->len) {
    return langsam_exceptionf(vm, "deref",
                              "attempt to advance consumed vector iterator");
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
    .apply = langsam_VectorIterator_apply,
};

const LangsamType LT_VECTORITERATOR = &LANGSAM_T_VECTORITERATOR;

// Map

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

uint64_t langsam_Map_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  LangsamMap *m = self.p;
  uint64_t hash = prevhash;
  for (LangsamIndex i = 0; i < m->nbuckets; i++) {
    LV bucket = m->buckets[i];
    while (!langsam_nilp(bucket)) {
      LV item = langsam_car(bucket);
      LV k = langsam_car(item);
      hash = langsam_hash(vm, k, hash);
      LV v = langsam_cdr(item);
      hash = langsam_hash(vm, v, hash);
      bucket = langsam_cdr(bucket);
    }
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
    l = langsam_cons(vm, langsam_deref(vm, it), l);
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
  LV items = langsam_Map_items(vm, self);
  while (!langsam_nilp(items)) {
    LV item = langsam_car(items);
    LV k = langsam_car(item);
    LV v1 = langsam_cdr(item);
    LV v2 = langsam_get(vm, other, k);
    LANGSAM_CHECK(v2);
    LV eq = langsam_equal(vm, v1, v2);
    LANGSAM_CHECK(eq);
    if (langsam_falsep(eq)) {
      return langsam_false;
    }
    items = langsam_cdr(items);
  }
  return langsam_true;
}

LV langsam_Map_add(LangsamVM *vm, LV self, LV other) {
  LangsamMap *m1 = self.p;
  LangsamMap *m2 = other.p;
  LangsamSize nitems = m1->nitems + m2->nitems;
  LV result = langsam_map(vm, m1->proto, nitems);
  LV items = langsam_Map_items(vm, self);
  while (!langsam_nilp(items)) {
    LV item = langsam_car(items);
    LV k = langsam_car(item);
    LV v = langsam_cdr(item);
    langsam_put(vm, result, k, v);
    items = langsam_cdr(items);
  }
  LV otheritems = langsam_Map_items(vm, other);
  while (!langsam_nilp(otheritems)) {
    LV otheritem = langsam_car(otheritems);
    LV k = langsam_car(otheritem);
    LV v = langsam_cdr(otheritem);
    langsam_put(vm, result, k, v);
    otheritems = langsam_cdr(otheritems);
  }
  return result;
}

static LV langsam_Map_rawgep(LangsamVM *vm, LV self, LV key) {
  LangsamMap *m = self.p;
  uint64_t hash = langsam_hash(vm, key, HASH_SEED);
  LangsamIndex bucket_index = (LangsamIndex)(hash % (uint64_t)m->nbuckets);
  LV bucket = m->buckets[bucket_index];
  while (!langsam_nilp(bucket)) {
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

static LV langsam_Map_gep(LangsamVM *vm, LV self, LV key) {
  LV item = langsam_Map_rawgep(vm, self, key);
  if (!langsam_nilp(item)) {
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
  if (!langsam_nilp(item)) {
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
    LV k = langsam_car(item);
    LV v = langsam_cdr(item);
    langsam_put(vm, newm, k, v);
    it = langsam_next(vm, it);
  }
  langsam_free(vm, m->buckets);
  m->buckets = tmp.buckets;
  m->nbuckets = tmp.nbuckets;
  m->nitems = tmp.nitems;
}

LV langsam_Map_put(LangsamVM *vm, LV self, LV key, LV value) {
  if (langsam_nilp(key)) {
    return langsam_exceptionf(vm, "put", "attempt to use nil as map key");
  }
  if (langsam_nilp(value)) {
    return langsam_Map_del(vm, self, key);
  }
  LV item = langsam_Map_rawgep(vm, self, key);
  LANGSAM_CHECK(item);
  if (!langsam_nilp(item)) {
    langsam_setcdr(item, value);
    return langsam_nil;
  }
  LangsamMap *m = self.p;
  LangsamSize limit = (LangsamSize)(m->load_factor * (LangsamFloat)m->nbuckets);
  if (m->nitems + 1 > limit) {
    resize_map(vm, self, m->nbuckets * 2);
  }
  uint64_t hash = langsam_hash(vm, key, HASH_SEED);
  LangsamIndex bucket_index = (LangsamIndex)(hash % (uint64_t)m->nbuckets);
  item = langsam_cons(vm, key, value);
  m->buckets[bucket_index] = langsam_cons(vm, item, m->buckets[bucket_index]);
  m->nitems++;
  return langsam_nil;
}

LV langsam_Map_del(LangsamVM *vm, LV self, LV key) {
  LangsamMap *m = self.p;
  uint64_t hash = langsam_hash(vm, key, HASH_SEED);
  LangsamIndex bucket_index = (LangsamIndex)(hash % (uint64_t)m->nbuckets);
  LV bucket = m->buckets[bucket_index];
  LV prev = langsam_nil;
  while (!langsam_nilp(bucket)) {
    LV item = langsam_car(bucket);
    LV k = langsam_car(item);
    LV eq = langsam_equal(vm, k, key);
    LANGSAM_CHECK(eq);
    if (langsam_truep(eq)) {
      if (langsam_nilp(prev)) {
        m->buckets[bucket_index] = langsam_cdr(bucket);
      } else {
        langsam_setcdr(prev, langsam_cdr(bucket));
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

LV langsam_Map_apply(LangsamVM *vm, LV self, LV args) {
  LANGSAM_ARG(key, args);
  key = langsam_eval(vm, key);
  LANGSAM_CHECK(key);
  return langsam_Map_get(vm, self, key);
}

LV langsam_Map_eval(LangsamVM *vm, LV self) {
  LangsamMap *m = self.p;
  LV result = langsam_map(vm, m->proto, m->nitems);
  LV items = langsam_Map_items(vm, self);
  while (!langsam_nilp(items)) {
    LV item = langsam_car(items);
    LV k = langsam_car(item);
    LV v = langsam_cdr(item);
    k = langsam_eval(vm, k);
    LANGSAM_CHECK(k);
    v = langsam_eval(vm, v);
    LANGSAM_CHECK(v);
    langsam_put(vm, result, k, v);
    items = langsam_cdr(items);
  }
  return result;
}

LV langsam_Map_repr(LangsamVM *vm, LV self) {
  LV items = langsam_Map_items(vm, self);
  LV reprs = langsam_nil;
  LangsamSize total_length = 0;
  while (!langsam_nilp(items)) {
    LV item = langsam_car(items);
    LV k = langsam_car(item);
    LV v = langsam_cdr(item);
    LV krepr = langsam_repr(vm, k);
    LANGSAM_CHECK(krepr);
    LV kreprlen = langsam_len(vm, krepr);
    LANGSAM_CHECK(kreprlen);
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
    items = langsam_cdr(items);
  }
  LangsamIndex index = total_length + 2 + 1;
  char *result = langsam_alloc(vm, index);
  result[--index] = 0;
  result[--index] = '}';
  for (LV r = reprs; !langsam_nilp(r); r = langsam_cdr(r)) {
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
    while (!langsam_nilp(bucket)) {
      LV item = langsam_car(bucket);
      result = langsam_cons(vm, item, result);
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
    while (!langsam_nilp(bucket)) {
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
    while (!langsam_nilp(bucket)) {
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
                              "values of type `%s` do not have a prototype",
                              langsam_typename(vm, self.type));
  }
  LangsamMap *m = self.p;
  return m->proto;
}

LV langsam_Map_setproto(LangsamVM *vm, LV self, LV proto) {
  if (self.type != LT_MAP) {
    return langsam_exceptionf(vm, "setproto",
                              "values of type `%s` do not have a prototype",
                              langsam_typename(vm, self.type));
  }
  if (proto.type != LT_MAP) {
    return langsam_exceptionf(vm, "setproto",
                              "prototype must be a Map, got `%s`",
                              langsam_typename(vm, proto.type));
  }
  LangsamMap *m = self.p;
  m->proto = proto;
  return proto;
}

static LangsamInteger next_power_of_two(LangsamInteger n) {
  LangsamInteger p = 1;
  while (p < n) {
    p <<= 1;
  }
  return p;
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
    .apply = langsam_Map_apply,
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
  return !langsam_nilp(it->items);
}

LV langsam_MapIterator_deref(LangsamVM *vm, LV self) {
  LangsamMapIterator *it = self.p;
  if (langsam_nilp(it->items)) {
    return langsam_exceptionf(vm, "deref",
                              "attempt to deref consumed map iterator");
  }
  return langsam_car(it->items);
}

LV langsam_MapIterator_apply(LangsamVM *vm, LV self, LV args) {
  LangsamMapIterator *it = self.p;
  if (langsam_nilp(it->items)) {
    return langsam_exceptionf(vm, "deref",
                              "attempt to advance consumed map iterator");
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
    .apply = langsam_MapIterator_apply,
};

const LangsamType LT_MAPITERATOR = &LANGSAM_T_MAPITERATOR;

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

uint64_t langsam_Function_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  LangsamFunction *f = self.p;
  uint64_t hash = prevhash;
  hash = langsam_hash(vm, f->name, hash);
  hash = langsam_hash(vm, f->params, hash);
  hash = langsam_hash(vm, f->doc, hash);
  hash = langsam_hash(vm, f->body, hash);
  hash = hash_boolean(hash, f->evalargs);
  hash = hash_boolean(hash, f->evalresult);
  return hash;
}

LV langsam_Function_cast(LangsamVM *vm, LV other) {
  if (other.type != LT_MAP) {
    return langsam_exceptionf(vm, "cast",
                              "Cannot cast value of type `%s` to Function",
                              langsam_typename(vm, other.type));
  }
  LangsamFunction *f =
      langsam_gcalloc(vm, LT_FUNCTION, LANGSAM_SIZEOF(LangsamFunction));
  f->name = langsam_get(vm, other, langsam_keyword(vm, "name"));
  if (!langsam_nilp(f->name)) {
    if (f->name.type != LT_SYMBOL) {
      return langsam_exceptionf(
          vm, "syntax", "Function name should be symbol, got %s: %s",
          langsam_typename(vm, f->name.type), langsam_cstr(vm, f->name));
    }
  }
  f->params = langsam_get(vm, other, langsam_keyword(vm, "params"));
  if (f->params.type != LT_VECTOR && f->params.type != LT_NIL) {
    return langsam_exceptionf(
        vm, "syntax", "Function parameters should be Vector or Nil, got %s",
        langsam_typename(vm, f->params.type));
  }
  f->doc = langsam_get(vm, other, langsam_keyword(vm, "doc"));
  f->funclet = vm->curlet;
  f->body = langsam_get(vm, other, langsam_keyword(vm, "body"));
  if (f->body.type != LT_CONS && f->body.type != LT_NIL) {
    return langsam_exceptionf(vm, "syntax",
                              "Function body should be Cons or Nil, got %s",
                              langsam_typename(vm, f->body.type));
  }
  f->fn = NULL;
  f->evalargs = langsam_truthy(
      vm, langsam_get(vm, other, langsam_keyword(vm, "evalargs")));
  f->evalresult = langsam_truthy(
      vm, langsam_get(vm, other, langsam_keyword(vm, "evalresult")));
  return (LV){
      .type = LT_FUNCTION,
      .p = f,
  };
}

static LV collect_rest(LangsamVM *vm, LV it) {
  LV rest = langsam_nil;
  while (langsam_truthy(vm, it)) {
    LV value = langsam_deref(vm, it);
    rest = langsam_cons(vm, value, rest);
    it = langsam_next(vm, it);
  }
  return langsam_nreverse(rest);
}

static LV langsam_bind(LangsamVM *vm, LV env, LV lhs, LV rhs) {
  if (lhs.type == LT_SYMBOL) {
    langsam_put(vm, env, lhs, rhs);
  } else if (lhs.type == LT_VECTOR) {
    LV it_lhs = langsam_iter(vm, lhs);
    LANGSAM_CHECK(it_lhs);
    LV it_rhs = langsam_iter(vm, rhs);
    LANGSAM_CHECK(it_rhs);
    LV amp_symbol = langsam_symbol(vm, "&");
    bool seen_amp = false;
    while (langsam_truthy(vm, it_lhs)) {
      LV pat = langsam_deref(vm, it_lhs);
      if (!seen_amp && LVEQ(pat, amp_symbol)) {
        seen_amp = true;
      } else if (seen_amp) {
        LV rest = collect_rest(vm, it_rhs);
        LANGSAM_CHECK(rest);
        LV result = langsam_bind(vm, env, pat, rest);
        LANGSAM_CHECK(result);
        break;
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
      it_lhs = langsam_next(vm, it_lhs);
    }
  } else if (lhs.type == LT_MAP) {
    LV it_lhs = langsam_iter(vm, lhs);
    LANGSAM_CHECK(it_lhs);
    while (langsam_truthy(vm, it_lhs)) {
      LV item = langsam_deref(vm, it_lhs);
      LV pat = langsam_car(item);
      LV key = langsam_cdr(item);
      if (pat.type == LT_KEYWORD) {
        LV keys = langsam_keyword(vm, "keys");
        if (LVEQ(pat, keys)) {
          LV it_key = langsam_iter(vm, key);
          LANGSAM_CHECK(it_key);
          while (langsam_truthy(vm, it_key)) {
            LV sym = langsam_deref(vm, it_key);
            if (sym.type != LT_SYMBOL) {
              return langsam_exceptionf(
                  vm, "bind",
                  "found value of type `%s` in iterable passed to :keys",
                  langsam_typename(vm, sym.type));
            }
            LV k = langsam_Keyword_cast(vm, sym);
            LV v = langsam_get(vm, rhs, k);
            LV result = langsam_bind(vm, env, sym, v);
            LANGSAM_CHECK(result);
            it_key = langsam_next(vm, it_key);
          }
        } else {
          return langsam_exceptionf(vm, "bind",
                                    "invalid key in map pattern: %s",
                                    langsam_cstr(vm, pat));
        }
      } else {
        LV value = langsam_get(vm, rhs, key);
        LV result = langsam_bind(vm, env, pat, value);
        LANGSAM_CHECK(result);
      }
      it_lhs = langsam_next(vm, it_lhs);
    }
  } else {
    return langsam_exceptionf(vm, "bind", "cannot bind pattern of type %s",
                              langsam_typename(vm, lhs.type));
  }
  return env;
}

LV langsam_quote(LangsamVM *vm, LV arg) {
  LV quote = langsam_get(vm, vm->rootlet, langsam_symbol(vm, "quote"));
  LV result = langsam_nil;
  result = langsam_cons(vm, arg, result);
  result = langsam_cons(vm, quote, result);
  return result;
}

LV langsam_do(LangsamVM *vm, LV forms) {
  LV oldlet = vm->curlet;
  vm->curlet = langsam_map(vm, vm->curlet, 64);
  LV result = langsam_nil;
  while (langsam_consp(forms)) {
    LV form = langsam_car(forms);
    result = langsam_eval(vm, form);
    if (langsam_exceptionp(result)) {
      vm->curlet = oldlet;
      return result;
    }
    forms = langsam_cdr(forms);
  }
  vm->curlet = oldlet;
  return result;
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
  return langsam_nil;
}

static LV eval_list(LangsamVM *vm, LV list) {
  LV result = langsam_nil;
  while (langsam_consp(list)) {
    LV item = langsam_car(list);
    LV evaluated_item = langsam_eval(vm, item);
    LANGSAM_CHECK(evaluated_item);
    result = langsam_cons(vm, evaluated_item, result);
    list = langsam_cdr(list);
  }
  return langsam_nreverse(result);
}

LV langsam_Function_apply(LangsamVM *vm, LV self, LV args) {
  LangsamFunction *f = self.p;
  LV result = langsam_nil;
  if (f->evalargs) {
    args = eval_list(vm, args);
    LANGSAM_CHECK(args);
  }
  if (f->fn != NULL) {
    result = f->fn(vm, args);
    LANGSAM_CHECK(result);
  } else {
    LV body = f->body;
    LV oldlet = vm->curlet;
    vm->curlet = langsam_map(vm, f->funclet, 64);
    LV bind_result = langsam_bind(vm, vm->curlet, f->params, args);
    if (langsam_exceptionp(bind_result)) {
      vm->curlet = oldlet;
      return bind_result;
    }
    result = langsam_do(vm, body);
    if (langsam_exceptionp(result)) {
      vm->curlet = oldlet;
      return result;
    }
    vm->curlet = oldlet;
  }
  if (f->evalresult) {
    result = langsam_eval(vm, result);
  }
  return result;
}

LV langsam_Function_repr(LangsamVM *vm, LV self) {
  LangsamFunction *f = self.p;
  if (!langsam_nilp(f->name)) {
    return langsam_format(vm, "<Function:%s>", langsam_cstr(vm, f->name));
  } else {
    return langsam_string(vm, "<Function>");
  }
}

static struct LangsamT LANGSAM_T_FUNCTION = {
    .name = "Function",
    .gcmanaged = true,
    .gcmark = langsam_Function_gcmark,
    .hash = langsam_Function_hash,
    .cast = langsam_Function_cast,
    .get = langsam_Function_get,
    .apply = langsam_Function_apply,
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
  mark_total += langsam_mark(vm, vm->strings);
  mark_total += langsam_mark(vm, vm->curlet);
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

static LV eval_apply(LangsamVM *vm, LV args) {
  LV tail = args;
  LANGSAM_ARG(obj, tail);
  LV patch_target = args;
  LV patch_source = tail;
  while (langsam_consp(tail)) {
    LV next_tail = langsam_cdr(tail);
    if (langsam_consp(next_tail)) {
      patch_target = tail;
      patch_source = next_tail;
    }
    tail = langsam_cdr(tail);
  }
  if (!langsam_nilp(tail)) {
    return langsam_exceptionf(vm, "apply", "invoked with improper list: %s",
                              args);
  }
  if (langsam_consp(patch_source)) {
    LV car = langsam_car(patch_source);
    if (car.type != LT_CONS) {
      return langsam_exceptionf(vm, "apply",
                                "last element should be list, got %s",
                                langsam_typename(vm, car.type));
    }
    langsam_setcdr(patch_target, car);
  }
  return langsam_apply(vm, obj, langsam_cdr(args));
}

static LV eval_eval(LangsamVM *vm, LV args) {
  LANGSAM_ARG(form, args);
  return langsam_eval(vm, form);
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

static LV quasiquote(LangsamVM *vm, LV obj);

static LV quasiquote_collect(LangsamVM *vm, LV coll) {
  LV splice = langsam_opword(vm, "splice");
  LV result = langsam_nil;
  LV it = langsam_iter(vm, coll);
  LANGSAM_CHECK(it);
  while (langsam_truthy(vm, it)) {
    LV item = langsam_deref(vm, it);
    LV qqitem = quasiquote(vm, item);
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
            result = langsam_cons(vm, splice_item, result);
            splice_items = langsam_cdr(splice_items);
          }
        }
      }
      if (throw) {
        return qqitem;
      }
    } else {
      result = langsam_cons(vm, qqitem, result);
    }
    it = langsam_next(vm, it);
  }
  return langsam_nreverse(result);
}

static LV quasiquote(LangsamVM *vm, LV obj) {
  if (obj.type == LT_CONS) {
    LV head = langsam_car(obj);
    LV unquote = langsam_symbol(vm, "unquote");
    LV unquote_splicing = langsam_symbol(vm, "unquote-splicing");
    if (LVEQ(head, unquote)) {
      LV tail = langsam_cdr(obj);
      if (!langsam_consp(tail)) {
        return langsam_exceptionf(vm, "quasiquote", "unquote needs argument");
      }
      LV form = langsam_car(tail);
      return langsam_eval(vm, form);
    } else if (LVEQ(head, unquote_splicing)) {
      LV tail = langsam_cdr(obj);
      if (!langsam_consp(tail)) {
        return langsam_exceptionf(vm, "quasiquote",
                                  "unquote-splicing needs argument");
      }
      LV form = langsam_car(tail);
      LV evaluated_form = langsam_eval(vm, form);
      LANGSAM_CHECK(evaluated_form);
      LV it = langsam_iter(vm, evaluated_form);
      LANGSAM_CHECK(it);
      LV result = langsam_nil;
      while (langsam_truthy(vm, it)) {
        LV item = langsam_deref(vm, it);
        result = langsam_cons(vm, item, result);
        it = langsam_next(vm, it);
      }
      result = langsam_nreverse(result);
      LV splice = langsam_opword(vm, "splice");
      return langsam_exception(vm, langsam_cons(vm, splice, result));
    } else {
      return quasiquote_collect(vm, obj);
    }
  } else if (obj.type == LT_VECTOR || obj.type == LT_MAP) {
    LV items = quasiquote_collect(vm, obj);
    LANGSAM_CHECK(items);
    return langsam_cast(vm, langsam_type(obj.type), items);
  } else {
    return obj;
  }
}

static LV eval_quasiquote(LangsamVM *vm, LV args) {
  LANGSAM_ARG(obj, args);
  return quasiquote(vm, obj);
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

static LV process_bindings(LangsamVM *vm, LV bindings) {
  LV it = langsam_iter(vm, bindings);
  LANGSAM_CHECK(it);
  while (langsam_truthy(vm, it)) {
    LV k = langsam_deref(vm, it);
    it = langsam_next(vm, it);
    if (!langsam_truthy(vm, it)) {
      return langsam_exceptionf(vm, "let", "incomplete bindings");
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
  LV oldlet = vm->curlet;
  vm->curlet = langsam_map(vm, vm->curlet, 64);
  LV bind_result = process_bindings(vm, bindings);
  if (langsam_exceptionp(bind_result)) {
    vm->curlet = oldlet;
    return bind_result;
  }
  LV result = langsam_do(vm, args);
  vm->curlet = oldlet;
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

static LV eval_int3(LangsamVM *vm, LV args) {
  LANGSAM_ARG_OPT(arg, args);
  raise(SIGTRAP);
  return langsam_eval(vm, arg);
}

static LV eval_curlet(LangsamVM *vm, LV args) { return vm->curlet; }

static LV eval_destructure(LangsamVM *vm, LV args) {
  LANGSAM_ARG(pattern, args);
  LANGSAM_ARG(value, args);
  LV bindlet = langsam_map(vm, vm->curlet, 64);
  return langsam_bind(vm, bindlet, pattern, value);
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
  LANGSAM_ARG_TYPE(proto, LT_MAP);
  return langsam_Map_setproto(vm, map, proto);
}

static LV eval_gep(LangsamVM *vm, LV args) {
  LANGSAM_ARG(map, args);
  LANGSAM_ARG_TYPE(map, LT_MAP);
  LANGSAM_ARG(key, args);
  return langsam_Map_gep(vm, map, key);
}

static LV eval_type(LangsamVM *vm, LV args) {
  LANGSAM_ARG(x, args);
  return langsam_type(x.type);
}

static LV eval_cons(LangsamVM *vm, LV args) {
  LANGSAM_ARG(car, args);
  LANGSAM_ARG(cdr, args);
  return langsam_cons(vm, car, cdr);
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

static LV eval_require(LangsamVM *vm, LV args) {
  LANGSAM_ARG(module_name, args);
  LANGSAM_ARG_TYPE(module_name, LT_STRING);
  LangsamString *ls = (LangsamString *)module_name.p;
  return langsam_require(vm, ls->p);
}

static LV eval_gc(LangsamVM *vm, LV args) { return langsam_gc(vm); }

extern int langsam_l_len;
extern char langsam_l_bytes[];

static LV import_langsam_core(LangsamVM *vm) {
  LV env = vm->rootlet;
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
  langsam_defn(vm, env, "eq", eval_eq);
  langsam_defn(vm, env, "=", eval_equal);
  langsam_defn(vm, env, "cmp", eval_cmp);
  langsam_defn(vm, env, "+", eval_add);
  langsam_defn(vm, env, "-", eval_sub);
  langsam_defn(vm, env, "*", eval_mul);
  langsam_defn(vm, env, "/", eval_div);
  langsam_defn(vm, env, "get", eval_get);
  langsam_defn(vm, env, "put", eval_put);
  langsam_defn(vm, env, "del", eval_del);
  langsam_defn(vm, env, "len", eval_len);
  langsam_defn(vm, env, "iter", eval_iter);
  langsam_defn(vm, env, "deref", eval_deref);
  langsam_defn(vm, env, "apply", eval_apply);
  langsam_defn(vm, env, "eval", eval_eval);
  langsam_defn(vm, env, "repr", eval_repr);
  langsam_defn(vm, env, "str", eval_str);
  langsam_defn(vm, env, "getproto", eval_getproto);
  langsam_defn(vm, env, "setproto", eval_setproto);
  langsam_defn(vm, env, "gep", eval_gep);
  langsam_defspecial(vm, env, "quote", eval_quote);
  langsam_defspecial(vm, env, "quasiquote", eval_quasiquote);
  langsam_defspecial(vm, env, "def", eval_def);
  langsam_defspecial(vm, env, "defn", eval_defn);
  langsam_defspecial(vm, env, "fn", eval_fn);
  langsam_defspecial(vm, env, "defmacro", eval_defmacro);
  langsam_defspecial(vm, env, "macro", eval_macro);
  langsam_defspecial(vm, env, "do", eval_do);
  langsam_defspecial(vm, env, "if", eval_if);
  langsam_defspecial(vm, env, "let", eval_let);
  langsam_defspecial(vm, env, "assert", eval_assert);
  langsam_defspecial(vm, env, "int3", eval_int3);
  langsam_defn(vm, env, "curlet", eval_curlet);
  langsam_defn(vm, env, "destructure", eval_destructure);
  langsam_defn(vm, env, "type", eval_type);
  langsam_defn(vm, env, "cons", eval_cons);
  langsam_defn(vm, env, "car", eval_car);
  langsam_defn(vm, env, "cdr", eval_cdr);
  langsam_defn(vm, env, "require", eval_require);
  langsam_defn(vm, env, "gc", eval_gc);
  return langsam_loadstringn(vm, langsam_l_bytes, langsam_l_len);
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
  if (!langsam_nilp(module)) {
    return module;
  }
  LangsamModule *m = registered_modules;
  while (m) {
    if (strcmp(m->name, module_name) == 0) {
      module = m->import(vm);
      if (!langsam_exceptionp(module)) {
        langsam_put(vm, modules, module_iname, module);
      }
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
                              "number of roots reached maximum of %d",
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
  vm->strings = langsam_map(vm, langsam_nil, 4096);
  vm->rootlet = langsam_map(vm, langsam_nil, 4096);
  vm->curlet = vm->rootlet;
  vm->repl = false;
  vm->loglevel = LANGSAM_INFO;
  LV result = import_langsam_core(vm);
  LANGSAM_CHECK(result);
  LV modules = langsam_map(vm, langsam_nil, 64);
  langsam_def(vm, vm->rootlet, "modules", modules);
  LV mainlet = langsam_map(vm, vm->rootlet, 4096);
  vm->curlet = mainlet;
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
} Reader;

static void Reader_init(Reader *r, LangsamVM *vm, ByteReadFunc readbyte,
                        void *readbyte_data) {
  r->vm = vm;
  r->readbyte = readbyte;
  r->readbyte_data = readbyte_data;
  r->bufsize = 0;
}

static LV Reader_readbyte(Reader *r) {
  if (r->bufsize > 0) {
    r->bufsize = 0;
    return langsam_integer(r->buffer[0]);
  } else {
    return r->readbyte(r->vm, r->readbyte_data);
  }
}

static void Reader_unreadbyte(Reader *r, uint8_t c) {
  r->buffer[0] = c;
  r->bufsize = 1;
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
      result = langsam_cons(r->vm, langsam_Opword_cast(r->vm, rhs), result);
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
      return langsam_float(value);
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
    if (result.type != LT_INTEGER) {
      return result;
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
      for (LV list = items; !langsam_nilp(list); list = langsam_cdr(list)) {
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
      for (LV list = items; !langsam_nilp(list);) {
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

static LV langsam_load(LangsamVM *vm, ByteReadFunc readbyte,
                       void *readbyte_data) {
  Reader r;
  Reader_init(&r, vm, readbyte, readbyte_data);
  LV result = langsam_nil;
  while (1) {
    if (vm->repl) {
      fprintf(stdout, "@L> ");
      fflush(stdout);
    }
    LV form = Reader_read(&r);
    LANGSAM_CHECK(form);
    if (langsam_nilp(form)) {
      break;
    }
    langsam_debug(vm, "> %s", langsam_cstr(vm, form));
    result = langsam_eval(vm, form);
    LANGSAM_CHECK(result);
    if (vm->repl) {
      fprintf(stdout, "%s\n", langsam_cstr(vm, result));
    }
  }
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

LV langsam_loadfd(LangsamVM *vm, int fd) {
  return langsam_load(vm, readbyte_fd, (void *)(intptr_t)fd);
}

LV langsam_loadfile(LangsamVM *vm, const char *path) {
  langsam_debug(vm, "loading %s", path);
  int fd = open(path, O_RDONLY);
  if (fd == -1) {
    return langsam_exceptionf(vm, "io", "cannot open %s: %s", path,
                              strerror(errno));
  }
  LV result = langsam_loadfd(vm, fd);
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

LV langsam_loadstring(LangsamVM *vm, char *s) {
  LangsamSize len = (LangsamSize)strlen(s);
  return langsam_loadstringn(vm, s, len);
}

LV langsam_loadstringn(LangsamVM *vm, char *s, LangsamSize len) {
  ReadByteStringState state = {
      .data = (uint8_t *)s,
      .len = len,
      .index = 0,
  };
  return langsam_load(vm, readbyte_string, &state);
}

void langsam_close(LangsamVM *vm) {
  vm->strings = langsam_nil;
  vm->rootlet = langsam_nil;
  vm->curlet = langsam_nil;
  langsam_gcfree_all(vm);
  langsam_unregister_modules();
  langsam_free(vm, vm->roots);
}
