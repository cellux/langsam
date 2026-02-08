#include <stdio.h>
#include <stdlib.h>

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
        char *error_message = langsam_cstr(&vm, result);
        fprintf(stderr, "%s: %s\n", argv[i], error_message);
        langsam_close(&vm);
        return 1;
      }
    }
  } else {
    langsam_enable_repl_mode(&vm);
    LV result = loadfd(&vm, 0);
    if (langsam_exceptionp(result)) {
      char *error_message = langsam_cstr(&vm, result);
      fprintf(stderr, "Error while loading from <stdin>: %s\n", error_message);
      langsam_close(&vm);
      return 1;
    }
  }
  langsam_close(&vm);
  return 0;
}
