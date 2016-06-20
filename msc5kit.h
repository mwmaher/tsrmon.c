/* MSC5KIT.h

   By Michael W. Maher

   Ver 1.0 4/5/91

   This is the header file for MSC5Kit.C.
   Contains misc. programming tools written in Microsoft C ver 5.1.
*/

#ifndef TOOLKITH
  #define TOOLKITH

  /* includes */
  #include<dos.h>


  /* macros */
  #ifndef BOOLEAN_DEFINE
    #define BOOLEAN_DEFINE
    #define TRUE 1
    #define FALSE 0
    #define ON 1
    #define OFF 0
    #define YES 1
    #define NO 0
    #define HOT 1
    #define COLD 0
  #endif

  #ifndef YELLOW
     #define YELLOW 14
  #endif
  #ifndef BRIGHTWHITE
    #define BRIGHTWHITE 15
  #endif


  /* function prototypes */

  /* input/output functions */
  char are_you_sure(short x, short y);

  /* 80X86 interrupt control functions */
  void install_hand(char int_number,
		    void (interrupt far *handler)(),
		    void (interrupt far *old_handler)());

#endif
