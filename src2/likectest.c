#include <stdio.h>

extern void  readcatalogue();
extern float likelihood( float xval[8] );
extern float priorprob( float xval[8] );

int main() {
  /*float params[9] = { 1.21, 1.01, 2.38, 44.25, 2.49, 4.62, -1.15, 0.20, 45.74 };*/
  float params[8] = { 1.21, 1.01, 2.38, 44.25, 2.49, 4.62, -1.15, 0.20 };
  float l;

  printf("%s\n", "Let's try and call Fortran..");
  readcatalogue();
  
  l = priorprob( params );
  printf("I got priorprob = %f\n",l);
  l = priorprob( params );
  printf("I got priorprob (2nd) = %f\n",l);

  l = likelihood( params );
  printf("I got likelihood = %f\n",l);

}
