#include <errno.h>
#include <sys/stat.h>

#include "langsam.h"

static struct LangsamT os_File_T;

typedef struct {
  LV path;
  LV fd;
} os_File;

static void os_File_gcmark(LangsamVM *vm, void *p) {
  os_File *f = p;
  langsam_mark(vm, f->path);
}

static uint64_t os_File_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  os_File *f = self.p;
  uint64_t hash = prevhash;
  hash = langsam_hash(vm, f->path, hash);
  hash = langsam_hash(vm, f->fd, hash);
  return hash;
}

static LV os_File_cast(LangsamVM *vm, LV other) {
  os_File *f = langsam_gcalloc(vm, &os_File_T, sizeof(os_File));
  f->path = langsam_nil;
  f->fd = langsam_integer(-1);
  if (other.type == LT_INTEGER) {
    f->fd = other;
  } else if (other.type == LT_STRING) {
    f->path = other;
  } else {
    return langsam_exceptionf(vm, "cast",
                              "Cannot cast value of type %s to File",
                              langsam_typename(vm, other.type));
  }
  return (LV){
      .type = &os_File_T,
      .p = f,
  };
}

static LV os_File_len(LangsamVM *vm, LV self) {
  os_File *f = self.p;
  struct stat st;
  int stat_result;
  if (f->fd.i >= 0) {
    stat_result = fstat(f->fd.i, &st);
  } else {
    stat_result = stat(langsam_cstr(vm, f->path), &st);
  }
  if (stat_result != 0) {
    return langsam_exceptionf(vm, "os", "cannot determine file size: %s",
                              strerror(errno));
  }
  return langsam_integer(st.st_size);
}

static struct LangsamT os_File_T = {
    .name = "File",
    .gcmanaged = true,
    .gcmark = os_File_gcmark,
    .hash = os_File_hash,
    .cast = os_File_cast,
    .len = os_File_len,
};

LV langsam_os_module(LangsamVM *vm) {
  LV module = langsam_map(vm, 64);
  langsam_put(vm, module, langsam_symbol(vm, "File"), langsam_type(&os_File_T));
  return module;
}
