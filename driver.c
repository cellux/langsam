#include <stdio.h>

#include "langsam.h"

LV langsam_os_module(LangsamVM *vm);

int main(int argc, char **argv) {
  langsam_register_module("os", langsam_os_module);
  LangsamVM vm;
  LV result = langsam_init(&vm, NULL);
  if (langsam_exceptionp(result)) {
    char *error_message = langsam_cstr(&vm, result);
    fprintf(stderr, "VM creation failed: %s\n", error_message);
    langsam_close(&vm);
    return 1;
  }
  if (argc > 1) {
    for (int i = 1; i < argc; i++) {
      LV result = langsam_loadfile(&vm, argv[i]);
      if (langsam_exceptionp(result)) {
        char *error_message = langsam_cstr(&vm, result);
        fprintf(stderr, "Error while loading %s: %s\n", argv[i], error_message);
        langsam_close(&vm);
        return 1;
      }
    }
  } else {
    LV result = langsam_loadfd(&vm, 0);
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
