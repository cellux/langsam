#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

#include "langsam.h"

LV langsam_os_module(LangsamVM *vm, LV env);

extern int os_l_len;
extern char os_l_bytes[];

static struct LangsamT os_File_T;

typedef struct {
  LV fd;
} os_File;

static LangsamHash os_File_hash(LangsamVM *vm, LV self, LangsamHash prevhash) {
  os_File *f = self.p;
  LangsamHash hash = prevhash;
  hash = langsam_hash(vm, f->fd, hash);
  return hash;
}

static LV os_File_cast(LangsamVM *vm, LV other) {
  if (other.type != LT_INTEGER) {
    return langsam_exceptionf(vm, "cast",
                              "File expects an integer file descriptor, got %s",
                              langsam_typename(vm, other.type));
  }
  os_File *f = langsam_gcalloc(vm, &os_File_T, LANGSAM_SIZEOF(os_File));
  f->fd = other;
  return (LV){
      .type = &os_File_T,
      .p = f,
  };
}

static LV os_File_len(LangsamVM *vm, LV self) {
  os_File *f = self.p;
  struct stat st;
  int stat_result = fstat((int)f->fd.i, &st);
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
  if (langsam_somep(flags)) {
    LANGSAM_ARG_TYPE(flags, LT_INTEGER);
  } else {
    flags = langsam_integer(0);
  }
  LANGSAM_ARG_OPT(mode, args);
  if (langsam_somep(mode)) {
    LANGSAM_ARG_TYPE(mode, LT_INTEGER);
  }
  int fd;
  if (langsam_nilp(mode)) {
    fd = open(langsam_cstr(vm, path), (int)flags.i);
  } else {
    fd = open(langsam_cstr(vm, path), (int)flags.i, mode.i);
  }
  if (fd < 0) {
    return langsam_exceptionf(vm, "open", "%s", strerror(errno));
  }
  return os_File_cast(vm, langsam_integer(fd));
}

static LV os_read(LangsamVM *vm, LV args) {
  LANGSAM_ARG(file, args);
  LANGSAM_ARG_TYPE(file, &os_File_T);
  os_File *f = (os_File *)file.p;
  LANGSAM_ARG(size, args);
  LANGSAM_ARG_TYPE(size, LT_INTEGER);
  char *buf = langsam_alloc(vm, size.i + 1);
  LangsamSize remaining = size.i;
  LangsamIndex index = 0;
  while (remaining > 0) {
    ssize_t bytes_read = read((int)f->fd.i, buf + index, (size_t)remaining);
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
  LANGSAM_ARG(buf, args);
  LANGSAM_ARG_TYPE(buf, LT_STRING);
  LangsamString *ls = (LangsamString *)buf.p;
  LangsamSize remaining = ls->len;
  LangsamIndex index = 0;
  while (remaining > 0) {
    ssize_t bytes_written =
        write((int)f->fd.i, ls->p + index, (size_t)remaining);
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
  int result = close((int)f->fd.i);
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

LV langsam_os_module(LangsamVM *vm, LV env) {
  langsam_def(vm, env, "File", langsam_type(&os_File_T));
  langsam_defn(vm, env, "open", os_open);
  langsam_def(vm, env, "stdin", os_File_cast(vm, langsam_integer(0)));
  langsam_def(vm, env, "stdout", os_File_cast(vm, langsam_integer(1)));
  langsam_def(vm, env, "stderr", os_File_cast(vm, langsam_integer(2)));
  langsam_def(vm, env, "O_APPEND", langsam_integer(O_APPEND));
  langsam_def(vm, env, "O_ASYNC", langsam_integer(O_ASYNC));
  langsam_def(vm, env, "O_CLOEXEC", langsam_integer(O_CLOEXEC));
  langsam_def(vm, env, "O_CREAT", langsam_integer(O_CREAT));
  langsam_def(vm, env, "O_DIRECTORY", langsam_integer(O_DIRECTORY));
  langsam_def(vm, env, "O_DSYNC", langsam_integer(O_DSYNC));
  langsam_def(vm, env, "O_EXCL", langsam_integer(O_EXCL));
  langsam_def(vm, env, "O_NOCTTY", langsam_integer(O_NOCTTY));
  langsam_def(vm, env, "O_NOFOLLOW", langsam_integer(O_NOFOLLOW));
  langsam_def(vm, env, "O_NONBLOCK", langsam_integer(O_NONBLOCK));
  langsam_def(vm, env, "O_RDONLY", langsam_integer(O_RDONLY));
  langsam_def(vm, env, "O_RDWR", langsam_integer(O_RDWR));
  langsam_def(vm, env, "O_SYNC", langsam_integer(O_SYNC));
  langsam_def(vm, env, "O_TRUNC", langsam_integer(O_TRUNC));
  langsam_def(vm, env, "O_WRONLY", langsam_integer(O_WRONLY));
  langsam_defn(vm, env, "read", os_read);
  langsam_defn(vm, env, "write", os_write);
  langsam_defn(vm, env, "close", os_close);
  langsam_defn(vm, env, "unlink", os_unlink);
  return langsam_loadstringn(vm, env, os_l_bytes, os_l_len);
}
