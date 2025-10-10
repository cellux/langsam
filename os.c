#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

#include "langsam.h"

static struct LangsamT os_File_T;

typedef struct {
  LV fd;
} os_File;

static uint64_t os_File_hash(LangsamVM *vm, LV self, uint64_t prevhash) {
  os_File *f = self.p;
  uint64_t hash = prevhash;
  hash = langsam_hash(vm, f->fd, hash);
  return hash;
}

static LV os_File_cast(LangsamVM *vm, LV other) {
  if (other.type != LT_INTEGER) {
    return langsam_exceptionf(vm, "cast",
                              "File expects an integer file descriptor, got %s",
                              langsam_typename(vm, other.type));
  }
  os_File *f = langsam_gcalloc(vm, &os_File_T, sizeof(os_File));
  f->fd = other;
  return (LV){
      .type = &os_File_T,
      .p = f,
  };
}

static LV os_File_len(LangsamVM *vm, LV self) {
  os_File *f = self.p;
  struct stat st;
  int stat_result = fstat(f->fd.i, &st);
  if (stat_result != 0) {
    return langsam_exceptionf(vm, "len", "cannot determine file size: %s",
                              strerror(errno));
  }
  return langsam_integer(st.st_size);
}

static struct LangsamT os_File_T = {
    .name = "File",
    .gcmanaged = true,
    .hash = os_File_hash,
    .cast = os_File_cast,
    .len = os_File_len,
};

static LV os_open(LangsamVM *vm, LV args) {
  LANGSAM_ARG(path, args);
  LANGSAM_ARG_TYPE(path, LT_STRING);
  LANGSAM_ARG_OPT(flags, args);
  if (!langsam_nilp(flags)) {
    LANGSAM_ARG_TYPE(flags, LT_INTEGER);
  } else {
    flags = langsam_integer(0);
  }
  LANGSAM_ARG_OPT(mode, args);
  if (!langsam_nilp(mode)) {
    LANGSAM_ARG_TYPE(mode, LT_INTEGER);
  }
  int fd;
  if (langsam_nilp(mode)) {
    fd = open(langsam_cstr(vm, path), flags.i);
  } else {
    fd = open(langsam_cstr(vm, path), flags.i, mode.i);
  }
  if (fd < 0) {
    return langsam_exceptionf(vm, "open", "%s", strerror(errno));
  }
  fprintf(stderr, "open: fd=%d\n", fd);
  return os_File_cast(vm, langsam_integer(fd));
}

static LV os_read(LangsamVM *vm, LV args) {
  LANGSAM_ARG(file, args);
  LANGSAM_ARG_TYPE(file, &os_File_T);
  os_File *f = (os_File *)file.p;
  LANGSAM_ARG(size, args);
  LANGSAM_ARG_TYPE(size, LT_INTEGER);
  char *buf = langsam_alloc(vm, size.i + 1);
  size_t remaining = size.i;
  size_t index = 0;
  while (remaining > 0) {
    ssize_t bytes_read = read(f->fd.i, buf + index, remaining);
    if (bytes_read == 0) {
      langsam_free(vm, buf);
      return langsam_nil;
    } else if (bytes_read < 0) {
      langsam_free(vm, buf);
      return langsam_exceptionf(vm, "read", "%s", strerror(errno));
    }
    index += bytes_read;
    remaining -= bytes_read;
  }
  buf[size.i] = 0;
  return langsam_stringn_wrap(vm, buf, size.i);
}

static LV os_write(LangsamVM *vm, LV args) {
  LANGSAM_ARG(file, args);
  LANGSAM_ARG_TYPE(file, &os_File_T);
  os_File *f = (os_File *)file.p;
  fprintf(stderr, "write: fd=%zd\n", f->fd.i);
  LANGSAM_ARG(buf, args);
  LANGSAM_ARG_TYPE(buf, LT_STRING);
  LangsamString *ls = (LangsamString *)buf.p;
  size_t remaining = ls->len;
  size_t index = 0;
  while (remaining > 0) {
    ssize_t bytes_written = write(f->fd.i, ls->p + index, remaining);
    if (bytes_written < 0) {
      return langsam_exceptionf(vm, "write", "%s", strerror(errno));
    }
    index += bytes_written;
    remaining -= bytes_written;
  }
  return langsam_nil;
}

static LV os_close(LangsamVM *vm, LV args) {
  LANGSAM_ARG(file, args);
  LANGSAM_ARG_TYPE(file, &os_File_T);
  os_File *f = (os_File *)file.p;
  int result = close(f->fd.i);
  if (result < 0) {
    return langsam_exceptionf(vm, "close", "cannot close file: %s",
                              strerror(errno));
  }
  return langsam_nil;
}

static LV os_unlink(LangsamVM *vm, LV args) {
  LANGSAM_ARG(path, args);
  LANGSAM_ARG_TYPE(path, LT_STRING);
  LangsamString *ls = (LangsamString *)path.p;
  int result = unlink(ls->p);
  if (result < 0) {
    return langsam_exceptionf(vm, "unlink", "cannot unlink %s: %s", ls->p,
                              strerror(errno));
  }
  return langsam_nil;
}

LV langsam_os_module(LangsamVM *vm) {
  LV module = langsam_map(vm, langsam_nil, 64);
  langsam_def(vm, module, "File", langsam_type(&os_File_T));
  langsam_defn(vm, module, "open", os_open);
  langsam_def(vm, module, "O_APPEND", langsam_integer(O_APPEND));
  langsam_def(vm, module, "O_ASYNC", langsam_integer(O_ASYNC));
  langsam_def(vm, module, "O_CLOEXEC", langsam_integer(O_CLOEXEC));
  langsam_def(vm, module, "O_CREAT", langsam_integer(O_CREAT));
  langsam_def(vm, module, "O_DIRECTORY", langsam_integer(O_DIRECTORY));
  langsam_def(vm, module, "O_DSYNC", langsam_integer(O_DSYNC));
  langsam_def(vm, module, "O_EXCL", langsam_integer(O_EXCL));
  langsam_def(vm, module, "O_NOCTTY", langsam_integer(O_NOCTTY));
  langsam_def(vm, module, "O_NOFOLLOW", langsam_integer(O_NOFOLLOW));
  langsam_def(vm, module, "O_NONBLOCK", langsam_integer(O_NONBLOCK));
  langsam_def(vm, module, "O_RDONLY", langsam_integer(O_RDONLY));
  langsam_def(vm, module, "O_RDWR", langsam_integer(O_RDWR));
  langsam_def(vm, module, "O_SYNC", langsam_integer(O_SYNC));
  langsam_def(vm, module, "O_TRUNC", langsam_integer(O_TRUNC));
  langsam_def(vm, module, "O_WRONLY", langsam_integer(O_WRONLY));
  langsam_defn(vm, module, "read", os_read);
  langsam_defn(vm, module, "write", os_write);
  langsam_defn(vm, module, "close", os_close);
  langsam_defn(vm, module, "unlink", os_unlink);
  return module;
}
