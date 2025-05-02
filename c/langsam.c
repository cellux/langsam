#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "langsam.h"

#define langsam_typename(vm, typeptr) langsam_cstr(vm, langsam_type(typeptr))

LV langsam_cast(LangsamVM *vm, LV type, LV other) {
  LangsamType t = type.p;
  if (t == other.type) {
    return other;
  }
  if (t->cast == NULL) {
    char *type_name = langsam_cstr(vm, type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "cannot cast %s to %s: %s does not "
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
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  if (t1->equal) {
    return t1->equal(vm, self, other);
  }
  LV cmp_result = t1->cmp(vm, self, other);
  if (langsam_exceptionp(cmp_result)) {
    return cmp_result;
  }
  return langsam_boolean(cmp_result.i == 0);
}

LV langsam_cmp(LangsamVM *vm, LV self, LV other) {
  if (LVEQ(self, other)) {
    return langsam_integer(0);
  }
  LangsamType t1 = self.type;
  if (t1->cmp == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "cannot compare %s to %s: %s does not "
                              "support cmp",
                              self_type_name, other_type_name, self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->cmp(vm, self, other);
}

LV langsam_add(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->add == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "cannot add %s to %s: %s does not "
                              "support add",
                              other_type_name, self_type_name, self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->add(vm, self, other);
}

LV langsam_sub(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->sub == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "cannot subtract %s from %s: %s does not "
                              "support sub",
                              other_type_name, self_type_name, self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->sub(vm, self, other);
}

LV langsam_mul(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->mul == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "cannot multiply %s by %s: %s does not "
                              "support mul",
                              self_type_name, other_type_name, self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->mul(vm, self, other);
}

LV langsam_div(LangsamVM *vm, LV self, LV other) {
  LangsamType t1 = self.type;
  if (t1->div == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    char *other_type_name = langsam_typename(vm, other.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "cannot divide %s by %s: %s does not "
                              "support div",
                              self_type_name, other_type_name, self_type_name);
  }
  LangsamType t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->div(vm, self, other);
}

LV langsam_get(LangsamVM *vm, LV self, LV key) {
  LangsamType t = self.type;
  if (t->get == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "%s does not support get", self_type_name);
  }
  return t->get(vm, self, key);
}

LV langsam_put(LangsamVM *vm, LV self, LV key, LV value) {
  LangsamType t = self.type;
  if (t->put == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "%s does not support put", self_type_name);
  }
  return t->put(vm, self, key, value);
}

LV langsam_len(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->len == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "%s does not support len", self_type_name);
  }
  return t->len(vm, self);
}

LV langsam_deref(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->deref == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "%s does not support deref", self_type_name);
  }
  return t->deref(vm, self);
}

LV langsam_call(LangsamVM *vm, LV self, LV args) {
  LangsamType t = self.type;
  if (t->call == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "%s does not support call", self_type_name);
  }
  return t->call(vm, self, args);
}

LV langsam_eval(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->eval == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "%s does not support eval", self_type_name);
  }
  return t->eval(vm, self);
}

LV langsam_repr(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->repr == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "%s does not support repr", self_type_name);
  }
  return langsam_str(vm, t->repr(vm, self));
}

LV langsam_str(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->str != NULL) {
    return t->str(vm, self);
  }
  return langsam_repr(vm, self);
}

LV langsam_hash(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (t->hash == NULL) {
    char *self_type_name = langsam_typename(vm, self.type);
    return langsam_exceptionf(vm, "unsupported-operation",
                              "%s does not support hash", self_type_name);
  }
  LV hash = t->hash(vm, self);
  hash.i = hash_ptr(hash.i, self.type);
  return hash;
}

// hash functions

#define FNV1A_OFFSET_BASIS 0xcbf29ce484222325
#define FNV1A_PRIME 0x00000100000001b3

static uint64_t fnv1a_64_mix(uint64_t hash, uint8_t *p, size_t len) {
  for (int i = 0; i < len; i++) {
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
static uint64_t hash_string(uint64_t hash, char *s, size_t len) {
  return fnv1a_64_mix(hash, (uint8_t *)s, len);
}

// Type

LV langsam_Type_call(LangsamVM *vm, LV self, LV args) {
  LangsamType t = self.p;
  if (langsam_nilp(langsam_cdr(args))) {
    return t->cast(vm, langsam_car(args));
  } else {
    return t->cast(vm, args);
  }
}

LV langsam_Type_repr(LangsamVM *vm, LV self) {
  LangsamType t = self.p;
  return langsam_symbol(vm, t->name);
}

LV langsam_Type_hash(LangsamVM *vm, LV self) {
  return langsam_integer(hash_ptr(HASH_SEED, self.p));
}

LV langsam_type(LangsamType t) { return (LV){.type = LT_TYPE, .p = t}; }

static struct LangsamT LANGSAM_T_TYPE = {
    .name = "Type",
    .gcmanaged = false,
    .call = langsam_Type_call,
    .repr = langsam_Type_repr,
    .hash = langsam_Type_hash,
};

const LangsamType LT_TYPE = &LANGSAM_T_TYPE;

// Nil

LV langsam_Nil_cast(LangsamVM *vm, LV other) { return langsam_nil; }

LV langsam_Nil_repr(LangsamVM *vm, LV self) {
  return langsam_symbol(vm, "nil");
}

LV langsam_Nil_hash(LangsamVM *vm, LV self) {
  return langsam_integer(FNV1A_NIL);
}

static struct LangsamT LANGSAM_T_NIL = {
    .name = "Nil",
    .gcmanaged = false,
    .cast = langsam_Nil_cast,
    .repr = langsam_Nil_repr,
    .hash = langsam_Nil_hash,
};

const LangsamType LT_NIL = &LANGSAM_T_NIL;

const LV langsam_nil = (LV){.type = LT_NIL, .p = 0};

bool langsam_nilp(LV v) { return v.type == LT_NIL; }

// Error

void langsam_Error_gcmark(LangsamVM *vm, void *p) {
  LangsamBox *box = p;
  langsam_mark(vm, box->payload);
}

LV langsam_Error_cast(LangsamVM *vm, LV other) {
  LangsamBox *err = langsam_gcalloc(vm, LT_ERROR, sizeof(LangsamBox));
  err->payload = other;
  return (LV){
      .type = LT_ERROR,
      .p = err,
  };
}

LV langsam_Error_deref(LangsamVM *vm, LV self) {
  LangsamBox *err = self.p;
  return err->payload;
}

LV langsam_Error_repr(LangsamVM *vm, LV self) {
  LangsamBox *err = self.p;
  return langsam_repr(vm, err->payload);
}

LV langsam_Error_str(LangsamVM *vm, LV self) {
  LangsamBox *err = self.p;
  return langsam_str(vm, err->payload);
}

LV langsam_Error_hash(LangsamVM *vm, LV self) {
  LangsamBox *err = self.p;
  return langsam_hash(vm, err->payload);
}

LV langsam_error(LangsamVM *vm, LV payload) {
  return langsam_Error_cast(vm, payload);
}

LV langsam_errorf(LangsamVM *vm, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  LV error_message = langsam_vformat(vm, fmt, args);
  va_end(args);
  if (langsam_exceptionp(error_message)) {
    return error_message;
  }
  return langsam_error(vm, error_message);
}

bool langsam_errorp(LV v) { return v.type == LT_ERROR; }

static struct LangsamT LANGSAM_T_ERROR = {
    .name = "Error",
    .gcmanaged = true,
    .gcmark = langsam_Error_gcmark,
    .cast = langsam_Error_cast,
    .deref = langsam_Error_deref,
    .repr = langsam_Error_repr,
    .str = langsam_Error_str,
    .hash = langsam_Error_hash,
};

const LangsamType LT_ERROR = &LANGSAM_T_ERROR;

// Exception

LV langsam_Exception_cast(LangsamVM *vm, LV other) {
  LangsamBox *ex = langsam_gcalloc(vm, LT_EXCEPTION, sizeof(LangsamBox));
  ex->payload = other;
  return (LV){
      .type = LT_EXCEPTION,
      .p = ex,
  };
}

LV langsam_exception(LangsamVM *vm, LV payload) {
  return langsam_Exception_cast(vm, payload);
}

LV langsam_exceptionf(LangsamVM *vm, const char *kind, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  LV error_message = langsam_vformat(vm, fmt, args);
  va_end(args);
  if (langsam_exceptionp(error_message)) {
    return error_message;
  }
  return langsam_exception(
      vm, langsam_cons(vm, langsam_symbol(vm, kind), error_message));
}

bool langsam_exceptionp(LV v) { return (v.type == LT_EXCEPTION); }

static struct LangsamT LANGSAM_T_EXCEPTION = {
    .name = "Exception",
    .gcmanaged = true,
    .gcmark = langsam_Error_gcmark,
    .cast = langsam_Exception_cast,
    .deref = langsam_Error_deref,
    .repr = langsam_Error_repr,
    .str = langsam_Error_str,
    .hash = langsam_Error_hash,
};

const LangsamType LT_EXCEPTION = &LANGSAM_T_EXCEPTION;

// Boolean

LV langsam_Boolean_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_NIL) {
    return langsam_false;
  } else if (other.type == LT_INTEGER) {
    return langsam_boolean(other.i != 0);
  } else if (other.type == LT_FLOAT) {
    return langsam_boolean(other.f != 0);
  } else if (other.type == LT_STRING) {
    LangsamString *s = other.p;
    return langsam_boolean(s->len != 0);
  } else if (other.type == LT_VECTOR) {
    LangsamVector *v = other.p;
    return langsam_boolean(v->len != 0);
  } else if (other.type == LT_MAP) {
    LangsamMap *m = other.p;
    return langsam_boolean(m->len != 0);
  }
  return langsam_true;
}

LV langsam_Boolean_repr(LangsamVM *vm, LV self) {
  if (self.b) {
    return langsam_istring(vm, "true");
  } else {
    return langsam_istring(vm, "false");
  }
}

LV langsam_Boolean_hash(LangsamVM *vm, LV self) {
  return langsam_integer(hash_boolean(HASH_SEED, self.b));
}

LV langsam_boolean(LangsamBoolean b) {
  return (LV){.type = LT_BOOLEAN, .b = b};
}

static struct LangsamT LANGSAM_T_BOOLEAN = {
    .name = "Boolean",
    .gcmanaged = false,
    .cast = langsam_Boolean_cast,
    .repr = langsam_Boolean_repr,
    .hash = langsam_Boolean_hash,
};

const LangsamType LT_BOOLEAN = &LANGSAM_T_BOOLEAN;

const LV langsam_true = (LV){.type = LT_BOOLEAN, .b = true};
const LV langsam_false = (LV){.type = LT_BOOLEAN, .b = false};

bool langsam_truep(LV v) { return LVEQ(v, langsam_true); }
bool langsam_falsep(LV v) { return LVEQ(v, langsam_false); }

// Integer

LV langsam_Integer_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_NIL) {
    return langsam_integer(0);
  } else if (other.type == LT_BOOLEAN) {
    return langsam_integer(other.b ? 1 : 0);
  } else if (other.type == LT_FLOAT) {
    return langsam_integer(other.f);
  } else if (other.type == LT_STRING) {
    LangsamString *s = other.p;
    errno = 0;
    long long l = strtoll(s->p, NULL, 0);
    if (errno) {
      return langsam_exceptionf(
          vm, "cast-error", "String->Integer conversion failed for: %s", s->p);
    }
    return langsam_integer(l);
  }
  return langsam_exceptionf(vm, "cast-error",
                            "Cannot cast value of type %s to Integer",
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
    return langsam_exceptionf(vm, "math-error", "division by zero");
  }
  return langsam_integer(self.i / other.i);
}

LV langsam_Integer_repr(LangsamVM *vm, LV self) {
  return langsam_format(vm, "%lld", self.i);
}

LV langsam_Integer_hash(LangsamVM *vm, LV self) {
  return langsam_integer(hash_integer(HASH_SEED, self.i));
}

LV langsam_integer(LangsamInteger i) {
  return (LV){.type = LT_INTEGER, .i = i};
}

static struct LangsamT LANGSAM_T_INTEGER = {
    .name = "Integer",
    .gcmanaged = false,
    .cast = langsam_Integer_cast,
    .cmp = langsam_Integer_cmp,
    .add = langsam_Integer_add,
    .sub = langsam_Integer_sub,
    .mul = langsam_Integer_mul,
    .div = langsam_Integer_div,
    .repr = langsam_Integer_repr,
    .hash = langsam_Integer_hash,
};

const LangsamType LT_INTEGER = &LANGSAM_T_INTEGER;

// Float

LV langsam_Float_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_NIL) {
    return langsam_float(0);
  } else if (other.type == LT_BOOLEAN) {
    return langsam_float(other.b ? 1 : 0);
  } else if (other.type == LT_INTEGER) {
    return langsam_float(other.i);
  } else if (other.type == LT_STRING) {
    LangsamString *s = other.p;
    errno = 0;
    double d = strtod(s->p, NULL);
    if (errno) {
      return langsam_exceptionf(
          vm, "cast-error", "String->Float conversion failed for: %s", s->p);
    }
    return langsam_float(d);
  }
  return langsam_exceptionf(vm, "cast-error",
                            "Cannot cast value of type %s to Float",
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
    return langsam_exceptionf(vm, "math-error", "division by zero");
  }
  return langsam_float(self.f / other.f);
}

LV langsam_Float_repr(LangsamVM *vm, LV self) {
  return langsam_format(vm, "%g", self.f);
}

LV langsam_Float_hash(LangsamVM *vm, LV self) {
  return langsam_integer(hash_float(HASH_SEED, self.f));
}

LV langsam_float(LangsamFloat f) { return (LV){.type = LT_FLOAT, .f = f}; }

static struct LangsamT LANGSAM_T_FLOAT = {
    .name = "Float",
    .gcmanaged = false,
    .cast = langsam_Float_cast,
    .cmp = langsam_Float_cmp,
    .add = langsam_Float_add,
    .sub = langsam_Float_sub,
    .mul = langsam_Float_mul,
    .div = langsam_Float_div,
    .repr = langsam_Float_repr,
    .hash = langsam_Float_hash,
};

const LangsamType LT_FLOAT = &LANGSAM_T_FLOAT;

// String

void langsam_String_gcfree(LangsamVM *vm, void *p) {
  LangsamString *s = p;
  langsam_free(vm, s->p);
}

LV langsam_String_cast(LangsamVM *vm, LV other) {
  return langsam_str(vm, other);
}

LV langsam_String_cmp(LangsamVM *vm, LV self, LV other) {
  char *s1 = ((LangsamString *)self.p)->p;
  char *s2 = ((LangsamString *)other.p)->p;
  return langsam_integer(strcmp(s1, s2));
}

LV langsam_String_add(LangsamVM *vm, LV self, LV other) {
  LangsamString *s1 = self.p;
  LangsamString *s2 = other.p;
  size_t len = s1->len + s2->len;
  char *p = langsam_alloc(vm, len + 1);
  memcpy(p, s1->p, s1->len);
  memcpy(p + s1->len, s2->p, s2->len);
  p[len] = 0;
  return langsam_stringn_wrap(vm, p, len);
}

LV langsam_String_get(LangsamVM *vm, LV self, LV key) {
  if (key.type != LT_INTEGER) {
    char *key_type_name = langsam_typename(vm, key.type);
    return langsam_exceptionf(
        vm, "type-error",
        "attempt to index String with non-integer index of type %s",
        key_type_name);
  }
  LangsamInteger index = key.i;
  LangsamString *s = self.p;
  if (index >= s->len) {
    return langsam_exceptionf(vm, "range-error",
                              "String index %lld out of bounds (0..%lld)",
                              index, s->len - 1);
  }
  return langsam_integer(s->p[index]);
}

LV langsam_String_put(LangsamVM *vm, LV self, LV key, LV value) {
  if (key.type != LT_INTEGER) {
    char *key_type_name = langsam_typename(vm, key.type);
    return langsam_exceptionf(
        vm, "type-error",
        "attempt to index String with non-integer index of type %s",
        key_type_name);
  }
  if (value.type != LT_INTEGER) {
    char *value_type_name = langsam_typename(vm, value.type);
    return langsam_exceptionf(vm, "type-error",
                              "attempt to store value of type %s into String",
                              value_type_name);
  }
  LangsamInteger index = key.i;
  LangsamString *s = self.p;
  if (index >= s->len) {
    return langsam_exceptionf(vm, "range-error",
                              "String index %lld out of bounds (0..%lld)",
                              index, s->len - 1);
  }
  s->p[index] = value.i;
  return langsam_nil;
}

LV langsam_String_len(LangsamVM *vm, LV self) {
  LangsamString *s = self.p;
  return langsam_integer(s->len);
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

static char hexdigit(int nybble) {
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
    *p++ = hexdigit(c >> 4);
    *p++ = hexdigit(c & 0x0f);
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
    *p++ = hexdigit(c >> 4);
    *p++ = hexdigit(c & 0x0f);
  }
  return p;
}

LV langsam_String_repr(LangsamVM *vm, LV self) {
  LangsamString *s = self.p;
  size_t reprlen = 0;
  for (size_t i = 0; i < s->len; i++) {
    reprlen += charreprwidth(s->p[i]);
  }
  char *p = langsam_alloc(vm, reprlen + 1);
  char *dst = p;
  for (size_t i = 0; i < s->len; i++) {
    dst = writecharrepr(dst, s->p[i]);
  }
  *dst = 0;
  return langsam_stringn_wrap(vm, dst, reprlen);
}

LV langsam_String_str(LangsamVM *vm, LV self) { return self; }

LV langsam_String_hash(LangsamVM *vm, LV self) {
  LangsamString *s = self.p;
  return langsam_integer(hash_string(HASH_SEED, s->p, s->len));
}

LV langsam_string(LangsamVM *vm, const char *s) {
  return langsam_stringn(vm, s, strlen(s));
}

LV langsam_stringn(LangsamVM *vm, const char *s, size_t len) {
  char *p = langsam_alloc(vm, len + 1);
  memcpy(p, s, len);
  p[len] = 0;
  return langsam_stringn_wrap(vm, p, len);
}

LV langsam_string_wrap(LangsamVM *vm, char *p) {
  return langsam_stringn_wrap(vm, p, strlen(p));
}

LV langsam_stringn_wrap(LangsamVM *vm, char *p, size_t len) {
  LangsamString *s = langsam_gcalloc(vm, LT_STRING, sizeof(LangsamString));
  s->p = p;
  s->len = len;
  return (LV){
      .type = LT_STRING,
      .p = s,
  };
}

LV langsam_istring(LangsamVM *vm, char *s) {
  LangsamString stmp = {
      .p = s,
      .len = strlen(s),
  };
  LV vtmp = {
      .type = LT_STRING,
      .p = &stmp,
  };
  LV vs = langsam_get(vm, vm->strings, vtmp);
  if (!langsam_nilp(vs)) {
    return vs;
  }
  vs = langsam_string(vm, s);
  langsam_put(vm, vm->strings, vs, vs);
  return vs;
}

LV langsam_vformat(LangsamVM *vm, const char *fmt, va_list args) {
  char *result;
  int len = vasprintf(&result, fmt, args);
  if (len == -1) {
    return langsam_exceptionf(vm, "format-error",
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

static struct LangsamT LANGSAM_T_STRING = {
    .name = "String",
    .gcmanaged = true,
    .gcfree = langsam_String_gcfree,
    .cast = langsam_String_cast,
    .cmp = langsam_String_cmp,
    .add = langsam_String_add,
    .get = langsam_String_get,
    .put = langsam_String_put,
    .len = langsam_String_len,
    .repr = langsam_String_repr,
    .str = langsam_String_str,
    .hash = langsam_String_hash,
};

const LangsamType LT_STRING = &LANGSAM_T_STRING;

// Symbol

LV langsam_Symbol_cast(LangsamVM *vm, LV other) {
  if (other.type == LT_STRING) {
    LangsamString *s = other.p;
    return langsam_symbol(vm, s->p);
  }
  return langsam_exceptionf(vm, "cast-error",
                            "Cannot cast value of type %s to Symbol",
                            langsam_typename(vm, other.type));
}

LV langsam_Symbol_eval(LangsamVM *vm, LV self) {
  LV result = langsam_get(vm, vm->curlet, self);
  if (langsam_nilp(result)) {
    return langsam_exceptionf(vm, "eval-error", "Cannot resolve symbol: %s",
                              langsam_cstr(vm, self));
  }
  return result;
}

LV langsam_symbol(LangsamVM *vm, char *name) {
  LV vs = langsam_istring(vm, name);
  vs.type = LT_SYMBOL;
  return vs;
}

static struct LangsamT LANGSAM_T_SYMBOL = {
    .name = "Symbol",
    .gcmanaged = false,
    .cast = langsam_Symbol_cast,
    .eval = langsam_Symbol_eval,
    .repr = langsam_String_str,
    .hash = langsam_String_hash,
};

const LangsamType LT_SYMBOL = &LANGSAM_T_SYMBOL;

// GC

#define langsam_gcheader(p) ((LangsamGCHeader *)p - 1)

void langsam_mark(LangsamVM *vm, LV self) {
  LangsamType t = self.type;
  if (!t->gcmanaged) {
    return;
  }
  LangsamGCHeader *gch = langsam_gcheader(self.p);
  if (gch->color == vm->gcmarkcolor) {
    return;
  }
  gch->color = vm->gcmarkcolor;
  if (t->gcmark != NULL) {
    t->gcmark(vm, self.p);
  }
}

static LangsamGCColor langsam_gcaltcolor(LangsamVM *vm) {
  if (vm->gcmarkcolor == LANGSAM_GC_BLACK) {
    return LANGSAM_GC_WHITE;
  } else {
    return LANGSAM_GC_BLACK;
  }
}

void *langsam_gcalloc(LangsamVM *vm, LangsamType type, size_t size) {
  LangsamGCHeader *gch = langsam_alloc(vm, sizeof(LangsamGCHeader) + size);
  gch->color = langsam_gcaltcolor(vm);
  void *p = gch + 1;
  LV lv = {
      .type = type,
      .p = p,
  };
  vm->gcobjects = langsam_cons(vm, lv, vm->gcobjects);
  return p;
}

void langsam_gc(LangsamVM *vm) {
  langsam_mark(vm, vm->curlet);
  LV prev = langsam_nil;
  LV cur = vm->gcobjects;
  while (!langsam_nilp(cur)) {
    LV next = ((LangsamCons *)cur.p)->cdr;
    LV lv = langsam_car(cur);
    LangsamGCHeader *gcheader = langsam_gcheader(lv.p);
    if (gcheader->color == vm->gcmarkcolor) {
      // marked: keep
      prev = cur;
      cur = next;
    } else {
      // !marked: sweep
      LangsamType t = lv.type;
      if (t->gcfree) {
        t->gcfree(vm, lv.p);
      }
      langsam_free(vm, lv.p);
      if (langsam_nilp(prev)) {
        vm->gcobjects = next;
      } else {
        ((LangsamCons *)prev.p)->cdr = next;
      }
      cur = next;
    }
  }
  vm->gcmarkcolor = langsam_gcaltcolor(vm);
}

// VM

typedef struct Module {
  const char *name;
  LV (*import)(LangsamVM *vm);
  struct Module *next;
} Module;

static Module *registered_modules;

void langsam_register_module(const char *name, LangsamImportFn import) {
  Module *m = malloc(sizeof(Module));
  m->name = name;
  m->import = import;
  m->next = registered_modules;
  registered_modules = m;
}

static void import_langsam_core(LangsamVM *vm) {
  langsam_defvalue(vm, "Nil", langsam_type(LT_NIL));
}

static void *langsam_default_realloc(void *self, void *ptr, size_t size) {
  if (size) {
    if (ptr) {
      return realloc(ptr, size);
    } else {
      return malloc(size);
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

LangsamStatus langsam_init(LangsamVM *vm, LangsamVMOpts *opts) {
  vm->allocator = &langsam_default_allocator;
  if (opts) {
    if (opts->allocator) {
      vm->allocator = opts->allocator;
    }
  }
  vm->strings = langsam_map(vm, 4096);
  vm->gcobjects = langsam_nil;
  vm->gcmarkcolor = LANGSAM_GC_BLACK;
  vm->rootlet = langsam_map(vm, 4096);
  vm->curlet = vm->rootlet;
  import_langsam_core(vm);
  Module *m = registered_modules;
  while (m) {
    LV result = m->import(vm);
    if (langsam_errorp(result)) {
      return LANGSAM_IMPORT_ERROR;
    }
    m = m->next;
  }
  return LANGSAM_OK;
}
