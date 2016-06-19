/* TSRMon.C
*/


/*  Description    : C module which is turned into a TSR program
		     with the help of an assembly language routines.

    Author         : MICHAEL W. MAHER
    developed on   : 11/27/1990
    last update    : 11/27/1990                                          */
/*-----------------------------------------------------------------------
     (MICROSOFT C)
      creation       : CL /AS /c keymon.C
		       LINK keymon TSRCA;
      call           : keymon                                            */

/* Include files */
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <dos.h>

/* Typedefs */
typedef unsigned char BYTE;            /* build ourselves a byte */
typedef unsigned int WORD;
typedef BYTE BOOL;                     /* like BOOLEAN in Pascal */
typedef union vel far * VP;            /* VP is a FAR ptr into the VRAM */

/* Macros */
#ifndef MK_FP                          /* was MK_FP already defined? */
#define MK_FP(seg, ofs) ((void far *) ((unsigned long) (seg)<<16|(ofs)))
#endif
#define VOFS(x,y) (80 * ( y ) + ( x ) )
#define VPOS(x,y) (VP) ( vptr + VOFS( x, y ) )
#define NO_END_FTN ((void (*)(void)) -1)     /* don't call an end ftn. */

/*== Structures and unions ===========================================*/

struct velb {                          /* describes a screen pos as 2 bytes */
	     BYTE character;           /* the ASCII code */
	     BYTE attribute;           /* corresponding attribute */
            };

struct velw {                          /* describes a screen pos as 1 word */
	    WORD contents;             /* stores ASCII charctr & attribute */
            };

union vel {                            /* describes a screen position */
	  struct velb h;
	  struct velw x;
          };

/* Link the functions from the assembly module */
extern int is_inst( char * id_string );
extern void uninst( void (*fkt)(void) );
extern int tsr_init(BOOL TC, void (*fkt)(void), unsigned maxkey,
                    unsigned heap, char * id_string);

/* Constants */
#ifdef __TURBOC__                      /* are we compiling with TURBO-C? */
  #define TC TRUE                      /* yes */
#else                                  /* we are using Microsoft C */
  #define TC FALSE
#endif


/* codes of the individual control keys for building the hotkey mask */
#define RSHIFT       1                 /* right SHIFT key pressed */
#define LSHIFT       2                 /* left SHIFT key pressed */
#define CTRL         4                 /* CTRL key pressed */
#define ALT          8                 /* ALT key pressed */
#define SCRL_AN     16                 /* Scroll Lock ON */
#define NUML_AN     32                 /* Num Lock ON */
#define CAPL_AN     64                 /* Caps Lock ON */
#define INS_AN     128                 /* Insert ON */
#define SCR_LOCK  4096                 /* Scroll Lock pressed */
#define NUM_LOCK  8192                 /* Num Lock pressed */
#define CAP_LOCK 16384                 /* Caps Lock pressed */
#define INSERT   32768                 /* INSERT key pressed */
#define NOF      0x07                  /* normal color */
#define INV      0x70                  /* inverse color */
#define HNOF     0x0f                  /* bright normal color */
#define HINV     0xf0                  /* bright inverse color */

#define HEAP_FREE 1024                 /* leave 1K space on the heap */
#define TRUE  1                        /* constants for working with BOOL */
#define FALSE 0


/* Global variables */
char id_string[] = "BB";               /* identification string */
char keymonfile[128];                  /* path+file name for monitor file */
unsigned keynum;                       /* num of keys_hit*2 before disk write*/
VP vptr;                               /* ptr to the 1st char in video RAM */
unsigned long atimes = 0;              /* num of key strikes */
union vel * scrbuf;                    /* ptr to buffer w/ screen contents */
char * blank_line;                     /* ptr to a blank line */
FILE *keymonfileptr;


/***********************************************************************
*  Function         : E N D F T N                                      *
**--------------------------------------------------------------------**
*  Description      : Called when the TSR program is reinstalled.      *
*  Input parameters : none                                             *
*  Return value     : none                                             *
***********************************************************************/

void endftn(void)
  {
  /* release the allocated buffers */
  free( blank_line );                  /* release the allocated buffer */
  free( (void *) scrbuf );             /* release the buffer */
  printf("A total of %lu keys have been hit.\n", atimes);
  }

/**************************************************************************
 * Function:             t s r k e y m o n                                *
 * by Michael W. Maher                                                    *
 **------------------------------------------------------------------------
 * Description      : This function will write out a time-date stamp,     *
 *                    key-count to a file.                                *
 * Input parameters : none                                                *
 * Return Value     : none                                                *
 **************************************************************************/

void tsrkeymon(void)
  {
  char open_ok = FALSE, rec[27], d_buf[9], t_buf[9];

  atimes += keynum;                    /* inc the total num of key strikes */
  if ((keymonfileptr = fopen(keymonfile, "at")) == NULL)
    {
    if ((keymonfileptr = fopen(keymonfile, "wt")) == NULL)
      printf("ERROR: can't open %s\n", keymonfile);
    else
      open_ok = TRUE;
    }
  else
    open_ok = TRUE;
  if (open_ok == TRUE)
    {
    sprintf(rec,"%-10lu%8s%8s", atimes/2, _strdate(d_buf), _strtime(t_buf));
    fwrite(rec, sizeof(rec), 1, keymonfileptr);
    fclose(keymonfileptr);
    }
  return;
  }


/**************************************************************************
 **                           MAIN PROGRAM                               **
 **************************************************************************
 **------------------------------------------------------------------------
 * Input parameters : path+filename,                                      *
 *                    number of keys to be hit before writing to file.    *
 * Return Value     : none                                                *
 **************************************************************************/


void main(int argc, char **argv)
  {
  /* set values of command-line parameters */
  if (argc < 2)
    strcpy(keymonfile, "keymon.dat");
  else
    strcpy(keymonfile, argv[1]);
  if (argc < 3)
    keynum = 20 * 2;
  else
    keynum = (unsigned) (2*atoi(argv[2]));
  if (keynum == 0)
    keynum = 20 * 2;

  printf("Keyboard Activity Monitor - By Michael W. Maher\n\n");
  if (is_inst(id_string))              /* is the program already installed? */
    {                                  /* yes */
    printf("TSRMon was already installed--now disabling.\n");
    uninst( endftn );                  /* reinstall prg., call ftn. ENDFKT */
				       /* if no end function is to be called,
				       the call is: */
    /* uninst( NO_END_FTN ); */
    }
  else                                 /* no, program hasn't installed yet */
    {                                  /* w/ MSC the heap buffers must be allocated now */
    scrbuf = (union vel *) malloc(80 * 25 * sizeof(union vel));
    blank_line = (char *)sbrk(80 + 1); /* allocate buffer */
    *(blank_line + 80) = '\0';         /* terminate buffer with NUL */
    memset(blank_line, ' ', 80);       /* fill the buffer with spaces */
    printf("TSRMon now enabled - To view key count and disable run TSRmon\n");
    printf("useage - A> TSRMON [%s] [%u]\n", keymonfile, keynum/2);
    tsr_init(TC, tsrkeymon, keynum, HEAP_FREE, id_string);
    }
  }

