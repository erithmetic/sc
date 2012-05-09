/*	SC	A Spreadsheet Calculator
 *
 *	One line vi emulation
 *	$Revision: 7.13 $
 */

#include <sys/types.h>
#ifdef BSD42
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif

#include <signal.h>
#include <curses.h>
#include <ctype.h>
#include "sc.h"

#define istext(a) (isalnum(a) || ((a) == '_'))

static void append_line();
static void back_hist();
static int  back_line(int arg);
static int  back_word(int arg);
static void back_space();
static void col_0();
static void cr_line();
static void del_in_line(int arg, int back_null);
static void del_to_end();
static void dogoto();
static void dotab();
static void dotcmd();
static int  find_char(int arg, int n);
static void for_hist();
static int  for_line(int arg, int stop_null);
static int  for_word(int arg, int end_word, int stop_null);
static void last_col();
static void change_case(int arg);
static void rep_char();
static void replace_in_line(int c);
static void replace_mode();
static void restore_it();
static void savedot(int c);
static void save_hist();
static void search_again();
static void search_hist();
static void search_mode();
static void stop_edit();
static int  to_char(int arg, int n);
static void u_save(int c);
static void yank_cmd(int delete, int change);
static void yank_chars(register int first, register int last, int delete);

extern int framerows;		/* Rows in current frame */
extern char mode_ind;		/* Mode indicator */
extern int lcols;		/* Spreadsheet Column the cursor was in last */
char *completethis = NULL;

/* values for mode below */

#define INSERT_MODE	0	/* Insert mode */
#define EDIT_MODE       1	/* Edit mode */
#define REP_MODE        2	/* Replace mode */
#define SEARCH_MODE	3	/* Get arguments for '/' command */
#define NAVIGATE_MODE	4	/* Navigate the spreadsheet while editing
					a line */

#define	DOTLEN		200

static int mode = INSERT_MODE;
struct	hist {
	unsigned int	len;
	char		*histline;
} history[HISTLEN + 1];

static int	histp = 0;
static int	lasthist = 0;
static int	endhist = -1;
static char	*last_search = NULL;
static char	*undo_line = NULL;
static int	undo_lim;
static char	dotb[DOTLEN];
static int	doti = 0;
static int	do_dot = 0;
static char	putbuf[FBUFLEN];
static int	findfunc = '0';
static int	findchar = 1;
static int	finddir = 0;

void
write_line(int c)
{
    struct frange *fr;
    struct crange *cr;

    if (c != ctl('i')) completethis = NULL;
    if (mode == EDIT_MODE) {
	switch (c) {
	case KEY_BACKSPACE:
	case (ctl('h')):	linelim = back_line(arg);		break;
	case (ctl('i')):	dotab();				break;
	case (ctl('m')):	cr_line();				break;
	case 'v':
	case (ctl('v')):	toggle_navigate_mode();			break;
	case ESC:	stop_edit();					break;
	case '+':	for_hist();					break;
	case '-':	back_hist();					break;
	case KEY_END:
	case (ctl('e')):
	case '$':	last_col();					break;
	case '.':	dotcmd();					break;
	case ';':	if (findfunc)
			    ungetch(findchar);
			else
			    break;
			findchar = 0;
			if (findfunc == 'f')
			    linelim = find_char(arg, finddir);
			else
			    linelim = to_char(arg, finddir);
									break;
	case ',':	if (findfunc)
			    ungetch(findchar);
			else
			    break;
			findchar = 0;
			if (findfunc == 'f')
			    linelim = find_char(arg, -(finddir));
			else
			    linelim = to_char(arg, -(finddir));
									break;
	case '~':	u_save(c); change_case(arg);			break;
#ifdef KEY_FIND
	case KEY_FIND:
#endif
	case '/':	search_mode();					break;
#ifdef KEY_HOME
	case KEY_HOME:
#endif
	case (ctl('a')):
	case '0':	col_0();					break;
	case 'A':	u_save(c); last_col(); append_line();		break;
	case 'C':	u_save(c); del_to_end(); append_line();		break;
	case 'D':	u_save(c); del_to_end();			break;
	case 'F':	linelim = find_char(arg, -1);			break;
	case 'G':	if (histp > 0) histp = lasthist; for_hist();	break;
	case 'I':	u_save(c); col_0(); insert_mode();		break;
	case 'P':	u_save(c);
			ins_string(putbuf);
			linelim = back_line(1);				break;
	case 'R':	u_save(c); replace_mode();			break;
	case 'T':	linelim = to_char(arg, -1);			break;
	case 'X':	u_save(c); back_space();			break;
	case 'Y':	yank_chars(linelim, strlen(line), 0);		break;
	case 'a':	u_save(c); append_line();			break;
	case 'b':	linelim = back_word(arg);			break;
	case 'c':	u_save(c); yank_cmd(1, 1); insert_mode();	break;
	case 'd':	u_save(c); yank_cmd(1, 0);			break;
	case 'e':	linelim = for_word(arg, 1, 0);			break;
	case 'f':	linelim = find_char(arg, 1);			break;
	case KEY_LEFT:
	case (ctl('b')):
	case 'h':	linelim = back_line(arg);			break;
	case KEY_IC:
	case 'i':	u_save(c); insert_mode();			break;
	case KEY_DOWN:
	case 'j':	for_hist();					break;
	case KEY_UP:
	case 'k':	back_hist();					break;
	case KEY_RIGHT:
	case (ctl('f')):
	case ' ':
	case 'l':	linelim = for_line(arg, 0);			break;
	case 'n':	search_again();					break;
	case 'p':	u_save(c);
			linelim = for_line(1, 1);
			ins_string(putbuf);
			linelim = back_line(1);				break;
	case 'q':	stop_edit();					break;
	case 'r':	u_save(c); rep_char();				break;
	case 's':	u_save(c); del_in_line(arg, 0); insert_mode();	break;
	case 't':	linelim = to_char(arg, 1);			break;
	case 'u':	restore_it();					break;
	case 'w':	linelim = for_word(arg, 0, 0);			break;
	case KEY_DC:
	case 'x':	u_save(c); del_in_line(arg, 1);			break;
	case 'y':	yank_cmd(0, 0);					break;
	default:							break;
	}
    } else if (mode == INSERT_MODE) { 
	if (c == (ctl('m')))
	    savedot(ESC);
	else
	    savedot(c);
	switch (c) {
	case KEY_BACKSPACE:
	case (ctl('h')):	back_space();				break;
	case (ctl('i')):	dotab();				break;
	case (ctl('m')):	cr_line();				break;
	case (ctl('v')):	toggle_navigate_mode();			break;
	case KEY_LEFT:
	case (ctl('b')):	linelim = back_line(arg);		break;
	case KEY_RIGHT:
	case (ctl('f')):	linelim = for_line(arg, 1);		break;
	case KEY_DOWN:
	case (ctl('n')):	for_hist();				break;
	case KEY_UP:
	case (ctl('p')):	back_hist();				break;
	case KEY_HOME:
	case (ctl('a')):	col_0();				break;
	case KEY_END:
	case (ctl('e')):	last_col();				break;
	case ESC:		edit_mode();				break;
	default:		ins_in_line(c);				break;
	}
    } else if (mode == SEARCH_MODE) {
	switch (c) {
	case KEY_BACKSPACE:
	case (ctl('h')):	back_space();				break;
	case (ctl('m')):	search_hist();				break;
	case ESC:		edit_mode();				break;
	default:		ins_in_line(c);				break;
	}
    } else if (mode == REP_MODE) {
	savedot(c);
	switch (c) {
	case KEY_BACKSPACE:
	case (ctl('h')):	if (linelim > strlen(undo_line))
				    back_space();
				else {
				    linelim = back_line(1);
				    *(line+linelim) = *(undo_line+linelim);
				}					break;
	case (ctl('m')):	cr_line();				break;
	case ESC:		edit_mode();				break;
	default:		replace_in_line(c);			break;
	}
    } else if (mode == NAVIGATE_MODE) {
	switch (c) {
	case '.':
	case ':':
	case (ctl('i')):	if (!showrange) {
				    toggle_navigate_mode();
				    startshow();
				} else {
				    showdr();
				    ins_in_line(' ');
				} 					break;
	case '+':
	case '-':		if (!showrange) {
				    ins_string(v_name(currow, curcol));
				    ins_in_line(c);
				}					break;
	case (ctl('m')):	if (!showrange) {
				    ins_string(v_name(currow, curcol));
				    toggle_navigate_mode();
				} else {
				    toggle_navigate_mode();
				    cr_line();
				}					break;
	case (ctl('a')):	{   /* insert variable value */
				    struct ent *p = *ATBL(tbl, currow, curcol);
				    char temp[100];

				    if (p && p->flags & is_valid) {
					(void) sprintf(temp, "%.*f",
						precision[curcol], p->v);
					ins_string(temp);
				    }
				}					break;
	case 'v':
	case (ctl('v')):	ins_string(v_name(currow, curcol));
				toggle_navigate_mode();			break;
	case 'c':		if ((cr = find_crange(currow, curcol))) {
				    ins_string(r_name(cr->r_left->row,
					    cr->r_left->col,
					    cr->r_right->row,
					    cr->r_right->col));
				    toggle_navigate_mode();
				    ins_in_line(' ');
				    showrange = 0;
				}					break;
	case 'f':		if ((fr = find_frange(currow, curcol))) {
				    ins_string(r_name(fr->or_left->row,
					    fr->or_left->col,
					    fr->or_right->row,
					    fr->or_right->col));
				    toggle_navigate_mode();
				    ins_in_line(' ');
				    showrange = 0;
				}					break;
	case 'r':		if ((fr = find_frange(currow, curcol))) {
				    ins_string(r_name(fr->ir_left->row,
					    fr->ir_left->col,
					    fr->ir_right->row,
					    fr->ir_right->col));
				    toggle_navigate_mode();
				    ins_in_line(' ');
				    showrange = 0;
				}					break;
	case KEY_LEFT:
	case 'h':		backcol(arg);				break;
	case KEY_RIGHT:
	case ' ':
	case 'l':		forwcol(arg);				break;
	case KEY_DOWN:
	case (ctl('n')):
	case 'j':		forwrow(arg);				break;
	case KEY_UP:
	case (ctl('p')):
	case 'k':		backrow(arg);				break;
	case 'q':
	case ctl('g'):
	case ESC:		toggle_navigate_mode();
				showrange = 0;				break;
	case 'H':		backcol(curcol - stcol + 2);
									break;
	case KEY_NPAGE:			/* next page */
	case (ctl('f')):
	case 'J':
				{
				int ps;

				ps = pagesize ? pagesize :
				    (LINES - RESROW - framerows)/2;
				forwrow(arg * ps);
				strow = strow + (arg * ps);
				FullUpdate++;
				}
									break;
	case KEY_PPAGE:			/* previous page */
	case (ctl('b')):
	case 'K':
				{
				int ps;

				ps = pagesize ? pagesize :
				    (LINES - RESROW - framerows)/2;
				backrow(arg * ps);
				strow = strow - (arg * ps);
				if (strow < 0) strow = 0;
				FullUpdate++;
				}
									break;
	case 'L':
				forwcol(lcols -(curcol-stcol)+1);
									break;
	case KEY_HOME:
				/* Remember the current position */
				savedrow[0] = currow;
				savedcol[0] = curcol;
				savedstrow[0] = strow;
				savedstcol[0] = stcol;

				currow = 0;
				curcol = 0;
				rowsinrange = 1;
				colsinrange = fwidth[curcol];
				FullUpdate++;
									break;
	case '0':
				/* Remember the current position */
				savedrow[0] = currow;
				savedcol[0] = curcol;
				savedstrow[0] = strow;
				savedstcol[0] = stcol;

				curcol = 0;
		   		rowsinrange = 1;
		   		colsinrange = fwidth[curcol];		break;
	case '$':		{
				register struct ent *p;

				/* Remember the current position */
				savedrow[0] = currow;
				savedcol[0] = curcol;
				savedstrow[0] = strow;
				savedstcol[0] = stcol;

				curcol = maxcols - 1;
				while (!VALID_CELL(p, currow, curcol) &&
					curcol > 0)
				    curcol--;
				rowsinrange = 1;
				colsinrange = fwidth[curcol];
				break;
				}
	case '^':
				/* Remember the current position */
				savedrow[0] = currow;
				savedcol[0] = curcol;
				savedstrow[0] = strow;
				savedstcol[0] = stcol;

				currow = 0;
				rowsinrange = 1;
				colsinrange = fwidth[curcol];		break;
	case '#':		{
				register struct ent *p;

				/* Remember the current position */
				savedrow[0] = currow;
				savedcol[0] = curcol;
				savedstrow[0] = strow;
				savedstcol[0] = stcol;

				currow = maxrows - 1;
				while (!VALID_CELL(p, currow, curcol) &&
					currow > 0)
				    currow--;
				rowsinrange = 1;
				colsinrange = fwidth[curcol];		break;
				}
	case 'm':
				markcell();				break;
	case '`': case '\'':
				dotick(c);				break;
	case '*':
				gotonote();				break;
	case 'g':
				dogoto();				break;
	case 'w':		{
				register struct ent *p;

				while (--arg>=0) {
				    do {
					if (curcol < maxcols - 1)
					    curcol++;
					else {
					    if (currow < maxrows - 1) {
						while(++currow < maxrows - 1 &&
							row_hidden[currow])
						    ;
						curcol = 0;
					    } else {
						error("At end of table");
						break;
					    }
					}
				    } while(col_hidden[curcol] ||
					    !VALID_CELL(p, currow, curcol));
				}
				rowsinrange = 1;
				colsinrange = fwidth[curcol];
				break;
				}
	case 'b':		{
				register struct ent *p;

				while (--arg>=0) {
				    do {
					if (curcol) 
					    curcol--;
					else {
					    if (currow) {
						while(--currow && row_hidden[currow])
						    ;
						curcol = maxcols - 1;
					    } else {
						error("At start of table");
						break;
					    }
					}
				    } while (col_hidden[curcol] ||
					    !VALID_CELL(p, currow, curcol));
				}
				rowsinrange = 1;
				colsinrange = fwidth[curcol];
				break;
				}
	}
    }
}

void
edit_mode()
{
    mode = EDIT_MODE;
    mode_ind = 'e';
    if (linelim < 0)	/* -1 says stop editing, ...so we still aren't */
	return;
    linelim = back_line(1);
}

void
insert_mode()
{
    mode_ind = 'i';
    mode = INSERT_MODE;
}

static void
search_mode()
{
    line[0] = '/';
    line[1] = '\0';
    linelim = 1;
    mode_ind = '/';
    mode = SEARCH_MODE;
}

static void
replace_mode()
{
    mode_ind = 'R';
    mode = REP_MODE;
}

void
toggle_navigate_mode()
{
    static char prev_mode = NAVIGATE_MODE;
    int limtmp;

    switch (prev_mode) {
	case INSERT_MODE:
				if (mode == NAVIGATE_MODE) {
				    prev_mode = NAVIGATE_MODE;
				    insert_mode();
				    break;
				} else
				    return;
	case EDIT_MODE:
				if (mode == NAVIGATE_MODE) {
				    prev_mode = NAVIGATE_MODE;
				    limtmp = linelim;
				    edit_mode();
				    linelim = limtmp;
				    break;
				} else
				    return;
	case NAVIGATE_MODE:
				prev_mode = mode;
				mode_ind = 'v';
				mode = NAVIGATE_MODE;
				break;
	default:
				prev_mode = NAVIGATE_MODE;
				break;
    }
}

void
dotab()
{
    static struct range *firstmatch;
    static struct range *lastmatch;
    static struct range *nextmatch;
    int len;

    if (linelim > 1 && isalnum(line[linelim-1]) || line[linelim-1] == '_' ||
	    (completethis && line[linelim-1] == ' ')) {
	if (!completethis) {
	    for (completethis = line + linelim - 1; isalnum(*completethis) ||
		    *completethis == '_'; completethis--) /* */;
	    completethis++;
	    len = line + linelim - completethis;
	    if (!find_range(completethis, len, NULL, NULL, &lastmatch)) {
		firstmatch = lastmatch;
		while (firstmatch->r_next &&
			!strncmp(completethis, firstmatch->r_next->r_name, len))
		    firstmatch = firstmatch->r_next;
		nextmatch = firstmatch;
	    } else
		nextmatch = NULL;
	}
	if (nextmatch) {
	    len = line + linelim - completethis;
	    memmove(completethis, line + linelim, strlen(line) - linelim + 1);
	    linelim -= len;
	    ins_string(nextmatch->r_name);
	    if (*(completethis - 1) == ' ' && *(line + linelim) != ' ')
		ins_in_line(' ');
	    if (nextmatch == lastmatch)
		nextmatch = firstmatch;
	    else
		nextmatch = nextmatch->r_prev;
	}
    } else
	startshow();
}

/* show the current range (see ^I), we are moving around to define a range */
void
startshow()
{
    showrange = 1;
    showsr = currow;
    showsc = curcol;
    toggle_navigate_mode();
}

/* insert the range we defined by moving around the screen, see startshow() */

#define SHOWROWS 2
#define SHOWCOLS 3

void
showdr()
{
    int			minsr, minsc, maxsr, maxsc;
    char		*p;
    char		r[12];
    struct frange	*fr = find_frange(currow, curcol);

    if (showrange == SHOWROWS) {
	minsr = showsr < currow ? showsr : currow;
	minsc = fr ? fr->or_left->col : 0;
	maxsr = showsr > currow ? showsr : currow;
	maxsc = fr ? fr->or_right->col : maxcols;

	toggle_navigate_mode();
	sprintf(r, "%d:%d", minsr, maxsr);
	ins_string(r);
    } else if (showrange == SHOWCOLS) {
	minsr = 0;
	minsc = showsc < curcol ? showsc : curcol;
	maxsr = maxrows;
	maxsc = showsc > curcol ? showsc : curcol;

	strcpy(r, coltoa(minsc));
	strcat(r, ":");
	strcat(r, coltoa(maxsc));

	toggle_navigate_mode();
	ins_string(r);
    } else {
	minsr = showsr < currow ? showsr : currow;
	minsc = showsc < curcol ? showsc : curcol;
	maxsr = showsr > currow ? showsr : currow;
	maxsc = showsc > curcol ? showsc : curcol;

	toggle_navigate_mode();
	ins_string(r_name(minsr, minsc, maxsr, maxsc));
    }
    showrange = 0;
}

/* dot command functions.  Saves info so we can redo on a '.' command */

static void
savedot(int c)
{
    if (do_dot || (c == '\n'))
	return;

    if (doti < DOTLEN-1) {
	if (c > 255) {
	    if (doti < DOTLEN-2) {
		dotb[doti++] = c/256;
		c %= 256;
	    } else
		return;
	}
	dotb[doti++] = c;
	dotb[doti] = '\0';
    }
}

static int dotcalled = 0;

static void
dotcmd()
{
    int c;

    if (dotcalled)	/* stop recursive calling of dotcmd() */
	return;
    do_dot = 1;
    doti = 0;
    while (dotb[doti] != '\0') {
	if ((c = dotb[doti++]) < 4)
	    c = c * 256 + dotb[doti++];
	dotcalled = 1;
	write_line(c);
    }
    do_dot = 0;
    doti = 0;
    dotcalled = 0;
}

int
vigetch()
{
    int c;

    if (do_dot) {
	if (dotb[doti] != '\0') {
	    return (dotb[doti++]);
	} else {
	    do_dot = 0;
	    doti = 0;
	    return (nmgetch());
	}
    }
    c = nmgetch();
    return (c);
}

/* saves the current line for possible use by an undo cmd */
static void
u_save(int c)
{
    static unsigned	undolen = 0;

    if (strlen(line)+1 > undolen) {
    	undolen = strlen(line)+40;

	undo_line = scxrealloc(undo_line, undolen);
    }
    (void) strcpy(undo_line, line);

    undo_lim = linelim;

    /* reset dot command if not processing it. */

    if (!do_dot) {
        doti = 0;
	savedot(c);
    }
}

/* Restores the current line saved by u_save() */
static void
restore_it()
{
    static char		*tempc = NULL;
    static unsigned	templen = 0;
    int			tempi;

    if ((undo_line == NULL) || (*undo_line == '\0')) 
	return;

    if (strlen(line)+1 > templen) {
    	templen = strlen(line) + 40;
	tempc = scxrealloc(tempc, templen);
    }

    strcpy(tempc, line);
    tempi = linelim;
    (void) strcpy(line, undo_line);
    linelim = undo_lim;
    strcpy(undo_line, tempc);
    undo_lim = tempi;
}

/* This command stops the editing process. */
static void
stop_edit()
{
    showrange = 0;
    numeric_field = 0;
    linelim = -1;
    (void) move(1, 0);
    (void) clrtoeol();
}

/*
 * Motion commands.  Forward motion commands take an argument
 * which, when set, cause the forward motion to continue onto
 * the null at the end of the line instead of stopping at the
 * the last character of the line.
 */
static int
for_line(int arg, int stop_null)
{
    int cpos = linelim;

    if (linelim < 0)
	return (linelim);
    else if (linelim + arg <= strlen(line))
	cpos += arg;
    else
	cpos = strlen(line);

    if (cpos == strlen(line) && cpos > 0 && !stop_null)
	return (cpos - 1);
    else
	return (cpos);
}

/* If end_word is non-zero, go to next end-of-word.  Otherwise, go to next
 * beginning-of-word.
 */

static int
for_word(int arg, int end_word, int stop_null)
{
    register int c;
    register int cpos;

    cpos = linelim;

    while (cpos < strlen(line) && arg--) {
	if (end_word)
	    cpos++;

	if (line[cpos] == ' ') {
	    while (line[cpos] == ' ')
		cpos++;
	    if (cpos > 0 && line[cpos] == '\0')
		--cpos;
	    if (!end_word)
		continue;
	}

	if (istext(line[cpos])) {
	    while ((c = line[cpos]) && istext(c)) 
		cpos++;
	} else {
	    while ((c = line[cpos]) && !istext(c) && c != ' ')
		cpos++;
	}

	if (end_word)
	    cpos--;
	else while (line[cpos] == ' ')
	    cpos++;

	if (cpos > 0 && line[cpos] == '\0' && !stop_null) 
	    --cpos;
    }

    return (cpos);
}

static int
back_line(int arg)
{
    if (linelim > arg)
        return (linelim - arg);
    else
	return (0);
}

static int
back_word(int arg)
{
    register int c;
    register int cpos;

    cpos = linelim;

    while (cpos > 0 && arg--) {
	if (line[cpos] == ' ') {
	    /* Skip white space */
	    while (cpos > 0 && line[cpos] == ' ')
		--cpos;
	} else if (cpos > 0 && (line[cpos-1] == ' ' 
			|| ( istext(line[cpos]) && !istext(line[cpos-1]))
			|| (!istext(line[cpos]) &&  istext(line[cpos-1])))) {
	    /* Started on the first char of a word - back up to prev. word */
	    --cpos;
	    while (cpos > 0 && line[cpos] == ' ')
		--cpos;
	}

	/* Skip across the word - goes 1 too far */
	if (istext(line[cpos])) {
	    while (cpos > 0 && (c = line[cpos]) && istext(c)) 
		    --cpos;
	} else {
	    while (cpos > 0 && (c = line[cpos]) && !istext(c) && c != ' ')
		    --cpos;
	}

	/* We are done - fix up the one too far */
	if (cpos > 0 && line[cpos] && line[cpos+1]) 
	    cpos++;
    }

    return(cpos);
}

/* Text manipulation commands */

/* If back_null is set, back up if the deletion leaves you at the null
 * line terminator.  Otherwise, don't.
 */
static void
del_in_line(int arg, int back_null)
{
    register int len, i;

    if (linelim >= 0) {
	len = strlen(line);
	if (arg > len - linelim)
	    arg = len - linelim;
	if (linelim == len && linelim > 0)
	    linelim--;
	strncpy(putbuf, line + linelim, arg);
	putbuf[arg] = '\0';
	for (i = linelim; i < len; i++)
	    line[i] = line[i+arg];
    }
    if (back_null && linelim > 0 && line[linelim] == '\0')
	--linelim;
}

void
ins_in_line(int c)
{
    register int i, len;

    if (c < 256) {
	if (linelim < 0) {
	    *line = '\0';
	    linelim = 0;
	}
	len = strlen(line);
	for (i = len; i >= linelim; --i)
	    line[i+1] = line[i];
	line[linelim++] = c;
	line[len+1] = '\0';
    }
}

void
ins_string(char *s)
{
    while (*s)
	ins_in_line(*s++);
}

static void
append_line()
{
    register int i;

    i = linelim;
    if (i >= 0 && line[i])
	linelim++;
    insert_mode();
}

static void
change_case(arg)
{
    if (linelim < 0) {
    	linelim = 0;
	*line = '\0';
    }
    while (arg--) {
	if (islower(line[linelim]))
	    line[linelim] = toupper(line[linelim]);
	else if (isupper(line[linelim]))
	    line[linelim] = tolower(line[linelim]);
	linelim = for_line(1, 0);
    }
}

static void
rep_char()
{
    int c;

    if (linelim < 0) {
    	linelim = 0;
	*line = '\0';
    }
    c = vigetch();
    savedot(c);
    if (c < 256 && c!= ESC && c != ctl('g')) {
	if (line[linelim] == '\0')
	    line[linelim+1] = '\0';
	line[linelim] = c;
    }
}

static void
replace_in_line(int c)
{
    register int len;

    if (c < 256) {
	if (linelim < 0) {
	    linelim = 0;
	    *line = '\0';
	}
	len = strlen(line);
	line[linelim++] = c;
	if (linelim > len)
	    line[linelim] = '\0';
    }
}

static void
back_space()
{
    if (linelim == 0)
	return;

    if (line[linelim] == '\0') {
	linelim = back_line(1);
	del_in_line(1, 1);
	linelim = strlen(line);
    } else {
	linelim = back_line(1);
	del_in_line(1, 1);
    }
}

/* Setting change to 1 makes `w' act like `e' so that `cw' will act like
 * `ce', just as in vi.  Setting change to 0 causes `w' to act as usual.
 */

int
get_motion(int change)
{
    int c;
    int arg2 = 0;

    c = vigetch();
    savedot(c);
    if (c == 0)		return (0);
    while (c >= '0' && c <= '9') {
	arg2 = 10 * arg2 + c - '0';
	c = vigetch();
	savedot(c);
    }
    if (!arg2)
	arg2++;
    arg *= arg2;
    switch (c) {
	case '$':	return (strlen(line));
	case 'b':	return (back_word(arg));
	case 'c':	return (change ? -1 : linelim);
	case 'd':	return (!change ? -1 : linelim);
	case 'e':	return (for_word(arg, 1, 1) + 1);
	case 'f':	return ((c = find_char(arg, 1)) == linelim ? c : c + 1);
	case 'F':	return (find_char(arg, -1));
	case 'h':	return (back_line(arg));
	case 'l':	return (for_line(arg, 1));
	case 't':	return ((c = to_char(arg, 1)) == linelim ? c : c + 1);
	case 'T':	return (to_char(arg, -1));
	case 'w':	return (for_word(arg, change, 1) + change);
	default:	return (linelim);
    }
}

static void
yank_cmd(int delete, int change)
{
    int cpos;

    if ((cpos = get_motion(change)) == -1) {
	cpos++;
	linelim = strlen(line);
    }
    yank_chars(cpos, linelim, delete);
}

static void
yank_chars(register int first, register int last, int delete)
{
    int temp;

    if (first == last)
	return;

    if (last < first) {
	temp = last; last = first; first = temp;
    }

    linelim = first;
    strncpy(putbuf, line + first, last - first);
    putbuf[last - first] = '\0';
    if (delete)
	memmove(line + first, line + last, strlen(line + last) + 1);
}

static void
del_to_end()
{
    if (linelim < 0)
	return;
    strcpy(putbuf, line + linelim);
    line[linelim] = '\0';
    linelim = back_line(1);
}

static void
cr_line()
{
    struct frange *fr;

    insert_mode();
    if (linelim != -1) {
	showrange = 0;
	save_hist();
	linelim = 0;
	(void) yyparse();
	linelim = -1;
	if (cellassign) {
	    cellassign = 0;
	    switch (craction) {
		case CRROWS:
		    if ((rowlimit >= 0) && (currow >= rowlimit)) {
			forwcol(1);
			currow = 0;
		    } else {
			if (autoinsert && (fr = find_frange(currow, curcol))) {
			    forwrow(1);
			    if (currow > fr->ir_right->row) {
				backrow(1);
				insertrow(1, 1);
			    }
			} else
			    forwrow(1);
		    }
		    break;
		case CRCOLS:
		    if ((collimit >= 0) && (curcol >= collimit)) {
			forwrow(1);
			curcol = 0;
		    } else {
			if (autoinsert && (fr = find_frange(currow, curcol))) {
			    forwcol(1);
			    if (curcol > fr->ir_right->col) {
				backcol(1);
				insertcol(1, 1);
			    }
			} else
			    forwcol(1);
		    }
		    break;
		default:
		    break;
	    }
	}
    }
    else {	/* '\n' alone will put you into insert mode */
    	*line = '\0';
	linelim = 0;
    }
}

/* History functions */

static void
save_hist()
{
    if (lasthist < 0)
	lasthist = 1;
    else
	lasthist = lasthist % HISTLEN + 1;

    if (lasthist > endhist)
	endhist = lasthist;

    if (history[lasthist].len < strlen(line)+1) {
    	history[lasthist].len = strlen(line)+40;
	history[lasthist].histline = scxrealloc(history[lasthist].histline,
		history[lasthist].len);
    }
    (void) strcpy(history[lasthist].histline, line);
    if (history[0].histline) {
	scxfree(history[0].histline);
	history[0].histline = (void *)0;
	history[0].len = 0;
    }
    histp = 0;
}

static void
back_hist()
{
    if (histp == 0) {
	if (history[0].len < strlen(line) + 1) {
	    history[0].len = strlen(line) + 40;
	    history[0].histline = scxrealloc(history[0].histline,
		    history[0].len);
	}
	(void) strcpy(history[0].histline, line);

	if (lasthist >= 0)
	    histp = lasthist;
    } else if (histp == 1) {
    	if (endhist != lasthist)
	    histp = endhist;
    } else if (histp != ((lasthist + 1) % (endhist + 1)))
	histp--;

    if (lasthist >= 0) {
    	(void) strcpy(line, history[histp].histline);
	last_col();
    }
}

static void
search_hist()
{
    static	unsigned lastsrchlen = 0;

    if(linelim < 1) {
	linelim = 0;
	edit_mode();
	return;
    }

    if (strlen(line)+1 > lastsrchlen) {
    	lastsrchlen = strlen(line)+40;
	last_search = scxrealloc(last_search, lastsrchlen);
    }
    (void)strcpy(last_search, line+1);
    search_again();
    mode = EDIT_MODE;
}

static void
search_again()
{
    int found_it;
    int do_next;
    int prev_histp;
    char *look_here;

    prev_histp = histp;
    if ((last_search == NULL) || (*last_search == '\0'))
	return;

    do {
	back_hist();
	if (prev_histp == histp)
	    break;
	prev_histp = histp;
	look_here = line;
	found_it = do_next = 0;
	while ((look_here = (char *)strchr(look_here, *last_search)) != NULL &&
						!found_it && !do_next) {

	    if (strncmp(look_here, last_search, strlen(last_search)) == 0)
		found_it++;
	    else if (look_here < line + strlen(line) - 1)
	        look_here++;
	    else
		do_next++;
	}
    } while (!found_it);
}

static void
for_hist()
{
    if (histp == 0)
    	return;

    if (histp == lasthist)
	histp = 0;
    else
	histp = histp % endhist + 1;

    if (lasthist >= 0) {
	(void) strcpy(line, history[histp].histline);
	last_col();
    }
}

static void
col_0()
{
    linelim = 0;
}

static void
last_col()
{
    linelim = strlen(line);
    if (linelim > 0 && mode_ind == 'e')
	--linelim;
}

static int
find_char(int arg, int n)
{
    register int i;

    if (findchar)
	finddir = n;
    findchar = vigetch();
    switch (dotb[doti - 1]) {
	case 'f': case 'F': case 't': case 'T':
	    savedot(findchar);
	default:
	    break;
    }
    i = linelim;
    while (arg--) {
	i += n;
	while (i >= 0 && line[i] && line[i] != findchar)
	    i += n;
	if (i < 0 || !line[i]) {
	    i = linelim;
	    break;
	}
    }
    findfunc = 'f';
    return (i);
}

static int
to_char(int arg, int n)
{
    register int i;
    int tmp = linelim;

    if (linelim + n >= 0 && linelim + n < strlen(line))
	linelim += n;
    i = find_char(arg, n);
    if (i != linelim)
	i -= n;
    linelim = tmp;
    findfunc = 't';

    return (i);
}

static void
dogoto()
{
    static char		*tempc = NULL;
    static unsigned	templen = 0;
    int			tempi;

    if (strlen(line) + 1 > templen) {
    	templen = strlen(line) + 40;
	tempc = scxrealloc(tempc, templen);
    }

    strcpy(tempc, line);
    tempi = linelim;

    /* Can't switch back to navigate mode if insert_mode() is used here
     * instead of toggle_navigate_mode(), which is what we want when doing
     * a goto from within navigate mode.
     */
    insert_mode();
    /* Tempted as I was, I had to resist making this "Where would you like
     * to go today?" - CRM :)
     */
    query("goto where?", NULL);
    if (linelim >= 0) {
	memmove(line + 5, line, strlen(line) + 1);
	strncpy(line, "goto ", 5);
	linelim = 0;
	yyparse();
    }

    strcpy(line, tempc);
    linelim = tempi;
    /* Now we need to change back to navigate mode ourselves so that
     * toggle_navigate_mode() will work properly again.
     */
    mode_ind = 'v';
    mode = NAVIGATE_MODE;
    if (!showrange)
	toggle_navigate_mode();
}

void
query(char *s, char *data)
{
    int c;

    insert_mode();
    if (data != NULL) {
	strcpy(line, data);
	linelim = strlen(line);
    } else {
    	*line = '\0';
	linelim = 0;
    }
    if (s != NULL) error(s);

    while (linelim >= 0) {
	update(0);
	switch (c = nmgetch()) {
	    case ctl('m'):
		error("");
		return;
	    case ctl('g'):
		line[0] = '\0';
		linelim = -1;
		error("");
		update(0);
		return;
	    case ctl('l'):
		FullUpdate++;
		clearok(stdscr,1);
		update(1);
		if (s != NULL) error(s);
		break;
	    default:
		write_line(c);
	}
    }
}
