/* MemSize.C

   By Michael W. Maher
   Ver 1.0  6/21/91

   A program to find out how much memory an application requires.  The
   program will report available system memory.

*/

void main(int argc, char **argv)
  {
  char *base, *temp, *next, *prev;

  do
    {
    if ((base = (char *)halloc(BLOCK_SIZE, sizeof(char *))) != NULL)
      {

      }
    else
      done = TRUE;
    }
  while (done == FALSE);
  return;
  }
