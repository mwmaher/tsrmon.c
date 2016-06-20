/* MSC5Kit.C

   By Michael W. Maher

   Ver 1.0 4/5/91


   Miscellaneous programming tools written in Microsoft C ver 5.1.  All of
   the following functions do not conform to ANSI C standards.  Be careful
   on how they are used.
*/

/* input and output */
/***************************************************************************
 function: are_you_sure

  returns: The function returns a YES or NO char value.

  purpose: This function will pop a message "Are you sure" and force the
	   user to respond "yes" or "no".
****************************************************************************/
char are_you_sure(short x, short y)
  {
  unsigned key;
  char response_flag = YES;
  short yes_color = YELLOW,
	 no_color = BRIGHTWHITE;
  struct rccoord original_pos;

  original_pos = _settextposition(y, x);
  _outtext("Are you sure?   [YES] [NO]");
  do
    {
    _settextposition(y, x+17);
    _settextcolor(yes_color);
    _outtext("YES");
    _settextposition(y, x+23);
    _settextcolor(no_color);
    _outtext("NO");
    key = _bios_keybrd(_KEYBRD_READ);
    if ((key == LEFT_ARROW) || (key == RIGHT_ARROW) ||
	(key == UP_ARROW) || (key == DOWN_ARROW))
      {
      if (response_flag == YES)
	{
	response_flag = NO;
	yes_color = BRIGHTWHITE;
	no_color = YELLOW;
	}
      else
	{
	response_flag = YES;
	yes_color = YELLOW;
	no_color = BRIGHTWHITE;
	}
      }
    if ((key == YKEY) || (key == yKEY))
      response_flag = YES;
    if ((key == NKEY) || (key == nKEY) || (key == ESCAPE))
      response_flag = NO;
    }
  while ((key != ENTER) && (key != yKEY) && (key != YKEY) &&
	 (key != ESCAPE) && (key != nKEY) && (key != NKEY));
  _settextcolor(BRIGHTWHITE);
  _settextposition(y, x);
  _outtext("                               ");
  _settextposition(original_pos.row, original_pos.col);
  return(response_flag);
  }


/* 80X86 interrupt control */
/************************************************************************
   function: install_hand
 parameters: vector_number - vector in table to replace
	     handler       - ptr to new function
	     old_vector    - ptr to old function
    returns: none

    purpose: replaces the handler in the vector table with a new handler.
*************************************************************************/
void install_hand(char int_number,
		  void (interrupt far *handler)(),
		  void (interrupt far *old_handler)())
  {
  void (interrupt far *temp_handler)();

  temp_handler = _dos_getvect(int_number);
  _disable();
  _dos_setvect(int_number, handler);
  _enable();
  old_handler = temp_handler;
  return;
  }



/* a template for function header */
/****************************************************************************
   function:
 parameters:
    returns:
    purpose:
****************************************************************************/

