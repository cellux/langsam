#include <stdio.h>
#include <stdlib.h>

#include "langsam.h"

int main(int argc, char **argv) {
  LangsamVM vm;
  if (langsam_init(&vm) != LANGSAM_OK) {
    fprintf(stderr, "VM creation failed\n");
    exit(1);
  }
  if (argc > 1) {
    for (int i = 1; i < argc; i++) {
      LV result = langsam_loadfile(&vm, argv[i]);
      if (langsam_exceptionp(result)) {
        char *error_message = langsam_cstr(&vm, result);
        fprintf(stderr, "Error while loading %s: %s\n", argv[i], error_message);
        langsam_close(&vm);
        exit(1);
      }
    }
  } else {
    LangsamValue result = langsam_loadfd(&vm, 0);
    if (langsam_exceptionp(result)) {
      char *error_message = langsam_cstr(&vm, result);
      fprintf(stderr, "Error while loading <stdin>: %s\n", error_message);
      langsam_close(&vm);
      exit(1);
    }
  }
  langsam_close(&vm);
  return 0;
}
