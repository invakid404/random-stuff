#include <stdio.h>
#include <stdint.h>

int64_t* dynamic_array_new();
void dynamic_array_reserve(int64_t**, int64_t);
void dynamic_array_push(int64_t**, int64_t);
int64_t dynamic_array_size(int64_t*);
void dynamic_array_free(int64_t*);

#define SIZE 10

int main() {
  int64_t* arr = dynamic_array_new();
  dynamic_array_reserve(&arr, SIZE);

  for (int i = 0; i < SIZE; ++i) {
    dynamic_array_push(&arr, i);
  }

  int64_t size = dynamic_array_size(arr);
  for (int64_t i = 0; i < size; ++i) {
    printf("%lu ", arr[i]);
  }
  printf("\n");

  dynamic_array_free(arr);

  return 0;
}
