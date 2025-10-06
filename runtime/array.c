#include "array.h"

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

Array *array_alloc(int64_t length) {
  if (length < 0) {
    fputs("[Pulse runtime] - negative array allocation length", stderr);
    exit(1);
  }

  int64_t *data = calloc(length, sizeof(int64_t));
  if (data == NULL) {
    perror("[Pulse runtime] - array_alloc()");
    exit(1);
  }

  Array *array = malloc(sizeof(Array));
  if (data == NULL) {
    perror("[Pulse runtime] - array_alloc()");
    exit(1);
  }

  array->length = length;
  array->data = data;

  return array;
}

int64_t array_get(Array *array, int64_t index) {
  if (index < 0 || (size_t)index >= array->length) {
    fputs("[Pulse runtime] - index out of bounds", stderr);
    exit(1);
  }

  return array->data[index];
}

void array_set(Array *array, int64_t index, int64_t value) {
  if (index < 0 || (size_t)index >= array->length) {
    fputs("[Pulse runtime] - index out of bounds", stderr);
    exit(1);
  }

  array->data[index] = value;
}
