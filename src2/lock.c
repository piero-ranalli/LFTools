/* file lock */

/* two simple functions to be called from Fortran */

#include <fcntl.h>

#define MY_LOCK_FILE "likelihood.lock"

static int fh;    /* only visible to this file */
static struct flock fl;


int c_set_lock() {
  fh = open(MY_LOCK_FILE, O_RDWR | O_CREAT, 0600);
  if (fh < 0)  return(1);  /* could not open file */

  fl.l_start = 0;
  fl.l_len = 0;
  fl.l_type = F_WRLCK;
  fl.l_whence = SEEK_SET;
  if (fcntl(fh, F_SETLK, &fl) < 0)    /* put an exclusive lock */
    return(2);                       /* return if didn't succeed */

  return(0);
}


int c_release_lock() {
  if (fcntl(fh, F_UNLCK, &fl) < 0)    /* release lock */
    return(1);            /* could not release lock */
  
  return(0);
}
