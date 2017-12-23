#include "nausparse.h"

// a struct to store permutations in
typedef struct permutations {
  int** perms;
  int perms_size;
  int count;
  int nv;
} permutations;

permutations* perms;

permutations* use_nauty(int nv, size_t nde, size_t *v, int *d, int *e, int *lab, int *ptn);

void free_perms(void);
