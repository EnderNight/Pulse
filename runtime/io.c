#include "io.h"

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

void print(int64_t c) { putchar(c); }

void print_int(int64_t i) { printf("%" PRId64, i); }
