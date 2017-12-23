#include "NT.h"

void free_perms(void) {
  /*
  int i = 0;
  for (; i < perms->count; i++) {
    free(perms->perms[i]);
  }
  */
  free(perms->perms);
  free(perms);
}

// Save automorphisms
void saveautom_traces(int count, int *perm, int n){
  if (perms->perms_size <= count) {
    perms->perms = realloc(perms->perms, sizeof (int *) * (((size_t) count) + 5));
    perms->perms_size = count + 5;
  }
  if (perms->perms == NULL) {
    // realloc or malloc failed at some point
    return;
  }

  perms->count++;

  int* save_perm = malloc(n * sizeof(int));
  if (save_perm == NULL) {
    // malloc failed
    perms->perms = NULL;
    return;
  }
  save_perm = memcpy(save_perm, perm, n * sizeof(int));
  if (save_perm == NULL) {
    // memcpy failed
    perms->perms = NULL;
    return;
  }

  // save the permutation
  perms->perms[count - 1] = save_perm;
}

void saveautom_nauty(int count, int *perm, int *orbits, int numorbits, int stabvertex, int n) {
  saveautom_traces(count, perm, n);
}

permutations* use_nauty(int nv, size_t nde, size_t *v, int *d, int *e, int *lab, int *ptn) {
  // allocate orbits
  DYNALLSTAT(int, orbits, orbits_sz);
  DYNALLOC1(int, orbits, orbits_sz, nv, "Orbit allocation failed");
  // set options
  static DEFAULTOPTIONS_SPARSEDIGRAPH(options);
  options.writeautoms=TRUE; // print automorphisms as they are found
  options.defaultptn=FALSE; // use the provided lab and ptn for coloring
  options.userautomproc = saveautom_nauty;
  statsblk stats;
  // make the sparse graph
  sparsegraph sg;
  SG_INIT(sg);
  SG_ALLOC(sg, nv, nde, "Sparse graph allocation failed");
  sg.nv = nv;
  sg.nde = nde;
  memcpy(sg.v, v, nv * sizeof(size_t));
  memcpy(sg.d, d, nv * sizeof(int));
  memcpy(sg.e, e, nde * sizeof(int));
  put_sg(stdout, &sg, 1, 30); // print graph
  // allocate space for perms
  perms = malloc(sizeof(permutations));
  if (perms == NULL) {
    // malloc failed
    return NULL;
  }
  perms->perms = malloc(sizeof(int **) * 3);
  if (perms == NULL) {
    // malloc failed
    return NULL;
  }
  perms->perms_size = 3;
  perms->count = 0;
  perms->nv = nv;
  // call sparse nauty
  sparsenauty (&sg, lab, ptn, orbits, &options, &stats, NULL);
  printf("Automorphism group size = ");
  writegroupsize(stdout,stats.grpsize1,stats.grpsize2);
  printf("\n");
  DYNFREE(orbits, orbits_sz);
  SG_FREE(sg);
  int i = 0, j = 0;
  for (; i < perms->count; i++) {
    for (; j < nv; j++) {
      printf("%d -> %d\n", j, perms->perms[i][j]);
    }
  }
  return perms;
}
