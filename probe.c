#include <stdio.h>

static void print_langsam_os() {
  fputs("LANGSAM_OS=", stdout);
#if defined(_WIN32)
  puts("windows");
#elif defined(__APPLE__) && defined(__MACH__)
#include <TargetConditionals.h>
#if TARGET_OS_IPHONE
  puts("ios");
#else
  puts("macos");
#endif
#elif defined(__linux__)
  puts("linux");
#elif defined(__unix__)
  puts("unix");
#elif defined(__FreeBSD__)
  puts("freebsd");
#elif defined(__ANDROID__)
  puts("android");
#else
  puts("unknown");
#endif
}

static void print_langsam_arch() {
  fputs("LANGSAM_ARCH=", stdout);
#if defined(__x86_64__) || defined(_M_X64)
  puts("x86_64");
#elif defined(__i386__) || defined(_M_IX86)
  puts("x86");
#elif defined(__aarch64__) || defined(_M_ARM64)
  puts("arm64");
#elif defined(__arm__) || defined(_M_ARM)
  puts("arm");
#elif defined(__riscv)
  puts("riscv");
#elif defined(__mips__) || defined(__mips) || defined(_M_MRX000)
  puts("mips");
#elif defined(__powerpc64__) || defined(__ppc64__) || defined(_M_PPC)
  puts("ppc64");
#elif defined(__powerpc__) || defined(__ppc__)
  puts("ppc");
#else
  puts("unknown");
#endif
}

int main(int argc, char **argv) {
  print_langsam_os();
  print_langsam_arch();
  return 0;
}
