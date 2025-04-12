#include <stdlib.h>

#include "langsam.h"

LV langsam_cast(LangsamVM *vm, LV type, LV other) {
  LangsamType t = type.p;
  if (t == other.type) {
    return other;
  }
  if (t->cast == NULL) {
    return langsam_errorf(vm,
                          "cannot cast %s to %s: %s does not "
                          "support cast operation",
                          *other.type, type, type);
  }
  return t->cast(vm, other);
}

LV langsam_eq(LangsamVM *vm, LV self, LV other) {
  if (LVEQ(self, other)) {
    return langsam_true;
  }
  struct LangsamT *t1 = self.type;
  if (t1->eq == NULL) {
    return langsam_errorf(vm,
                          "cannot compare %s to %s: %s does not "
                          "support eq operation",
                          *self.type, *other.type, *self.type);
  }
  struct LangsamT *t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(vm, self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->eq(vm, self, other);
}

LV langsam_cmp(LangsamVM *vm, LV self, LV other) {
  if (LVEQ(self, other)) {
    return langsam_integer(vm, 0);
  }
  struct LangsamT *t1 = self.type;
  if (t1->cmp == NULL) {
    return langsam_errorf(vm,
                          "cannot compare %s to %s: %s does not "
                          "support cmp operation",
                          *self.type, *other.type, *self.type);
  }
  struct LangsamT *t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(vm, self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->cmp(vm, self, other);
}

LV langsam_add(LangsamVM *vm, LV self, LV other) {
  struct LangsamT *t1 = self.type;
  if (t1->add == NULL) {
    return langsam_errorf(vm,
                          "cannot add %s to %s: %s does not "
                          "support add operation",
                          *other.type, *self.type, *self.type);
  }
  struct LangsamT *t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(vm, self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->add(vm, self, other);
}

LV langsam_sub(LangsamVM *vm, LV self, LV other) {
  struct LangsamT *t1 = self.type;
  if (t1->sub == NULL) {
    return langsam_errorf(vm,
                          "cannot subtract %s from %s: %s does not "
                          "support sub operation",
                          *other.type, *self.type, *self.type);
  }
  struct LangsamT *t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(vm, self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->sub(vm, self, other);
}

LV langsam_mul(LangsamVM *vm, LV self, LV other) {
  struct LangsamT *t1 = self.type;
  if (t1->mul == NULL) {
    return langsam_errorf(vm,
                          "cannot multiply %s by %s: %s does not "
                          "support mul operation",
                          *self.type, *other.type, *self.type);
  }
  struct LangsamT *t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(vm, self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->mul(vm, self, other);
}

LV langsam_div(LangsamVM *vm, LV self, LV other) {
  struct LangsamT *t1 = self.type;
  if (t1->div == NULL) {
    return langsam_errorf(vm,
                          "cannot divide %s by %s: %s does not "
                          "support div operation",
                          *self.type, *other.type, *self.type);
  }
  struct LangsamT *t2 = other.type;
  if (t1 != t2) {
    other = langsam_cast(vm, langsam_type(vm, self.type), other);
    if (langsam_exceptionp(other)) {
      return other;
    }
  }
  return t1->div(vm, self, other);
}

LV langsam_get(LangsamVM *vm, LV self, LV key) {
  struct LangsamT *t = self.type;
  if (t->get == NULL) {
    return langsam_errorf(vm, "%s does not support get operation", *self.type);
  }
  return t->get(vm, self, key);
}

LV langsam_put(LangsamVM *vm, LV self, LV key, LV value) {
  struct LangsamT *t = self.type;
  if (t->put == NULL) {
    return langsam_errorf(vm, "%s does not support put operation", *self.type);
  }
  return t->put(vm, self, key, value);
}

LV langsam_len(LangsamVM *vm, LV self) {
  struct LangsamT *t = self.type;
  if (t->len == NULL) {
    return langsam_errorf(vm, "%s does not support len operation", *self.type);
  }
  return t->len(vm, self);
}

LV langsam_deref(LangsamVM *vm, LV self) {
  struct LangsamT *t = self.type;
  if (t->deref == NULL) {
    return langsam_errorf(vm, "%s does not support deref operation",
                          *self.type);
  }
  return t->deref(vm, self);
}

LV langsam_call(LangsamVM *vm, LV self, LV args) {
  struct LangsamT *t = self.type;
  if (t->call == NULL) {
    return langsam_errorf(vm, "%s does not support call operation", *self.type);
  }
  return t->call(vm, self, args);
}

LV langsam_eval(LangsamVM *vm, LV self) {
  struct LangsamT *t = self.type;
  if (t->eval == NULL) {
    return langsam_errorf(vm, "%s does not support eval operation", *self.type);
  }
  return t->eval(vm, self);
}

LV langsam_repr(LangsamVM *vm, LV self) {
  struct LangsamT *t = self.type;
  if (t->repr == NULL) {
    return langsam_errorf(vm, "%s does not support repr operation", *self.type);
  }
  return t->repr(vm, self);
}

LV langsam_str(LangsamVM *vm, LV self) {
  struct LangsamT *t = self.type;
  if (t->str != NULL) {
    return t->str(vm, self);
  }
  return langsam_repr(vm, self);
}

// Type

LV langsam_Type_call(LangsamVM *vm, LV self, LV args) {
  struct LangsamT *t = self.p;
  return t->cast(vm, langsam_car(args));
}

LV langsam_Type_repr(LangsamVM *vm, LV self) {
  struct LangsamT *t = self.p;
  return langsam_symbol(vm, t->name);
}

LV langsam_type(LangsamVM *vm, LangsamType t) {
  return (LV){.type = LANGSAM_TYPE_TYPE, .p = t};
}

static struct LangsamT LANGSAM_T_TYPE = {
    .name = "Type",
    .call = langsam_Type_call,
    .repr = langsam_Type_repr,
};

const LangsamType LANGSAM_TYPE_TYPE = &LANGSAM_T_TYPE;

// Nil

LV langsam_Nil_cast(LangsamVM *vm, LV other) { return langsam_nil; }

LV langsam_Nil_repr(LangsamVM *vm, LV self) {
  return langsam_symbol(vm, "nil");
}

static struct LangsamT LANGSAM_T_NIL = {
    .name = "Nil",
    .cast = langsam_Nil_cast,
    .repr = langsam_Nil_repr,
};

const LangsamType LANGSAM_TYPE_NIL = &LANGSAM_T_NIL;

const LV langsam_nil = (LV){.type = LANGSAM_TYPE_NIL, .p = 0};

bool langsam_nilp(LV v) { return v.type == LANGSAM_TYPE_NIL; }

// Error

LV langsam_Error_cast(LangsamVM *vm, LV other) {
  LangsamError *err = langsam_gc_alloc(vm, sizeof(LangsamError));
  err->payload = other;
  return (LV){
      .type = LANGSAM_TYPE_ERROR,
      .p = err,
  };
}

LV langsam_Error_deref(LangsamVM *vm, LV self) {
  LangsamError *err = self.p;
  return err->payload;
}

LV langsam_Error_repr(LangsamVM *vm, LV self) {
  LangsamError *err = self.p;
  return langsam_repr(vm, err->payload);
}

LV langsam_Error_str(LangsamVM *vm, LV self) {
  LangsamError *err = self.p;
  return langsam_str(vm, err->payload);
}

bool langsam_errorp(LV v) { return v.type == LANGSAM_TYPE_ERROR; }

static bool exception_symbolp(LV v) {
  if (v.type != LANGSAM_TYPE_SYMBOL) {
    return false;
  }
  LangsamSymbol *sym = v.p;
  return sym->sigil == '!';
}

bool langsam_exceptionp(LV v) {
  if (v.type != LANGSAM_TYPE_ERROR) {
    return false;
  }
  LangsamError *err = v.p;
  if (exception_symbolp(err->payload)) {
    return true;
  }
  if (!langsam_consp(err->payload)) {
    return false;
  }
  return exception_symbolp(langsam_car(err->payload));
}

LV langsam_errorf(LangsamVM *vm, const char *fmt, ...) {}

// Boolean

const LangsamType LANGSAM_TYPE_BOOLEAN = &(struct LangsamT){};

const LV langsam_true = (LV){.type = LANGSAM_TYPE_BOOLEAN, .b = true};
const LV langsam_false = (LV){.type = LANGSAM_TYPE_BOOLEAN, .b = false};

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
  langsam_defvalue(vm, "Nil", langsam_type(LANGSAM_TYPE_NIL));
}

LangsamStatus langsam_init(LangsamVM *vm) {
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
