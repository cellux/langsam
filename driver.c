#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "langsam.h"

static void set_os_args(LangsamVM *vm, int argc, char **argv) {
  LV args = langsam_nil;
  for (int i = 0; i < argc; i++) {
    LV arg = langsam_string(vm, argv[i]);
    args = langsam_cons(vm, arg, args);
  }
  args = langsam_nreverse(args);
  LV os = langsam_require(vm, "os");
  langsam_put(vm, os, langsam_symbol(vm, "args"), args);
}

static LV loadfile(LangsamVM *vm, char *path) {
  LV os = langsam_require(vm, "os");
  LANGSAM_CHECK(os);
  LV loadfile = langsam_get(vm, os, langsam_symbol(vm, "loadfile"));
  LANGSAM_CHECK(loadfile);
  LV args = langsam_nil;
  args = langsam_cons(vm, langsam_string(vm, path), args);
  args = langsam_cons(vm, vm->curlet, args);
  return langsam_invoke(vm, loadfile, args);
}

static LV loadfd(LangsamVM *vm, int fd) {
  LV os = langsam_require(vm, "os");
  LANGSAM_CHECK(os);
  LV loadfd = langsam_get(vm, os, langsam_symbol(vm, "loadfd"));
  LANGSAM_CHECK(loadfd);
  LV args = langsam_nil;
  args = langsam_cons(vm, langsam_integer(fd), args);
  args = langsam_cons(vm, vm->curlet, args);
  return langsam_invoke(vm, loadfd, args);
}

static void write_stderr_all(const char *s, size_t len) {
  while (len > 0) {
    ssize_t wrote = write(STDERR_FILENO, s, len);
    if (wrote <= 0) {
      return;
    }
    s += (size_t)wrote;
    len -= (size_t)wrote;
  }
}

static void write_stderr_cstr(const char *s) {
  write_stderr_all(s, strlen(s));
}

static void print_error_value(LangsamVM *vm, const char *source, LV value) {
  LV error_message = langsam_str(vm, value);
  if (langsam_exceptionp(error_message) || error_message.type != LT_STRING) {
    write_stderr_cstr(source);
    write_stderr_cstr(": <error while converting value to string>\n");
    return;
  }
  LangsamString *message = error_message.p;
  write_stderr_cstr(source);
  write_stderr_cstr(": ");
  if (message->len > 0) {
    size_t message_len = (size_t)message->len;
    write_stderr_all(message->p, message_len);
  }
  write_stderr_cstr("\n");
}

int main(int argc, char **argv) {
  LangsamVM vm;
  LV init_result = langsam_init(&vm, NULL);
  if (langsam_exceptionp(init_result)) {
    char *error_message = langsam_cstr(&vm, init_result);
    fprintf(stderr, "VM creation failed: %s\n", error_message);
    langsam_close(&vm);
    return 1;
  }
  const char *debug = getenv("DEBUG");
  if (debug && *debug) {
    langsam_loglevel(&vm, LANGSAM_DEBUG);
  }
  set_os_args(&vm, argc, argv);
  if (argc > 1) {
    bool opt_e = false;
    for (int i = 1; i < argc; i++) {
      if (strcmp(argv[i], "-e") == 0) {
        opt_e = true;
        continue;
      }
      LV result;
      if (opt_e) {
        result = langsam_readstring(&vm, argv[i]);
        if (!langsam_exceptionp(result)) {
          result = langsam_eval(&vm, result);
        }
        opt_e = false;
      } else {
        result = loadfile(&vm, argv[i]);
      }
      if (langsam_exceptionp(result)) {
        print_error_value(&vm, argv[i], result);
        langsam_close(&vm);
        return 1;
      }
    }
  } else {
    langsam_enable_repl_mode(&vm);
    LV result = loadfd(&vm, 0);
    if (langsam_exceptionp(result)) {
      print_error_value(&vm, "Error while loading from <stdin>", result);
      langsam_close(&vm);
      return 1;
    }
  }
  langsam_close(&vm);
  return 0;
}
