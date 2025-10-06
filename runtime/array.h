#ifndef ARRAY_H_
#define ARRAY_H_

#include <stddef.h>
#include <stdint.h>

typedef struct {
  size_t length;
  int64_t *data;
} Array;

Array *array_alloc(int64_t);
int64_t array_get(Array *, int64_t);
void array_set(Array *, int64_t, int64_t);

#endif // !ARRAY_H_
