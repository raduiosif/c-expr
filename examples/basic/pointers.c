void main(void) {
  int *x = malloc(10); 
  x->y = 3;
  (x*x+3)->y->z = 5;
  free(x); 
}
