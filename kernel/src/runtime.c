#define NANOPRINTF_IMPLEMENTATION
#define NANOPRINTF_USE_FIELD_WIDTH_FORMAT_SPECIFIERS 1
#define NANOPRINTF_USE_PRECISION_FORMAT_SPECIFIERS 1
#define NANOPRINTF_USE_FLOAT_FORMAT_SPECIFIERS 0
#define NANOPRINTF_USE_LARGE_FORMAT_SPECIFIERS 1
#define NANOPRINTF_USE_BINARY_FORMAT_SPECIFIERS 0
#define NANOPRINTF_USE_WRITEBACK_FORMAT_SPECIFIERS 0

#include <nanoprintf.h>

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

// stdio.h

int snprintf(char *s, size_t n, const char *format, ...) {
  va_list args;
  va_start(args, format);
  int r = npf_vsnprintf(s, n, format, args);
  va_end(args);
  return r;
}

// string.h

size_t strlen(const char *s) {
  const char *p = s;
  while (*p != '\0')
    ++p;
  return p - s;
}

size_t strnlen(const char *s, size_t n) {
  const char *p = s;
  while (n > 0 && *p != '\0')
    --n, ++p;
  return p - s;
}

int strcmp(const char *s1, const char *s2) {
  while (*s1 != '\0' && *s1 == *s2)
    ++s1, ++s2;
  return *s1 - *s2;
}

int strncmp(const char *s1, const char *s2, size_t n) {
  while (n > 0 && *s1 != '\0' && *s1 == *s2)
    ++s1, ++s2, --n;
  return n == 0 ? 0 : *s1 - *s2;
}
