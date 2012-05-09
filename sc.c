/*	SC	A Spreadsheet Calculator
 *		Main driver
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		$Revision: 7.13 $
 *
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <curses.h>
#include <ctype.h>
#include <getopt.h>

#ifdef BSD42
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif
#if defined(BSD42) || defined(BSD43) || defined(VMS)
#include <sys/file.h>
#else
#include <fcntl.h>
#endif
#ifndef MSDOS
#include <unistd.h>
#endif

#include <stdlib.h>
#include "sc.h"

#ifndef SAVENAME
#define	SAVENAME "SC.SAVE" /* file name to use for emergency saves */
#endif /* SAVENAME */

#ifndef DFLT_PAGER
#define	DFLT_PAGER "more"	/* more is probably more widespread than less */
#endif /* DFLT_PAGER */

#define MAXCMD 160	/* for ! command below */

/* Globals defined in sc.h */

struct ent ***tbl;
int arg = 1;
int strow = 0, stcol = 0;
int currow = 0, curcol = 0;
int savedrow[27], savedcol[27];
int savedstrow[27], savedstcol[27];
int FullUpdate = 0;
int maxrow, maxcol;
int maxrows, maxcols;
int *fwidth;
int *precision;
int *realfmt;
char *col_hidden;
char *row_hidden;
char line[FBUFLEN];
int changed;
struct ent *delbuf[4];
int dbidx;
int modflg;
int cellassign;
int numeric;
char *mdir;
char *autorun;
char *fkey[FKEYS];
char *scext;
char *ascext;
char *tbl0ext;
char *tblext;
char *latexext;
char *slatexext;
char *texext;
int scrc = 0;
int showsc, showsr;	/* Starting cell for highlighted range */
int usecurses = TRUE;	/* Use curses unless piping/redirection or using -q */
int brokenpipe = FALSE;	/* Set to true if SIGPIPE is received */
#ifdef RIGHT_CBUG
int	wasforw	= FALSE;
#endif

void	update();
void	repaint();

char curfile[PATHLEN];
char    revmsg[80];

int  linelim = -1;

int  showtop   = 1;	/* Causes current cell value display in top line  */
int  showcell  = 1;	/* Causes current cell to be highlighted	  */
int  showrange = 0;	/* Causes ranges to be highlighted		  */
int  showneed  = 0;	/* Causes cells needing values to be highlighted  */
int  showexpr  = 0;	/* Causes cell exprs to be displayed, highlighted */
int  shownote  = 0;	/* Causes cells with attached notes to be
			   highlighted					  */

int  autocalc  = 1;	/* 1 to calculate after each update */
int  autolabel = 1;     /* If room, causes label to be created after a define */
int  autoinsert = 0;    /* Causes rows to be inserted if craction is non-zero
			   and the last cell in a row/column of the scrolling
			   portion of a framed range has been filled	  */
int  calc_order = BYROWS;
int  optimize  = 0;	/* Causes numeric expressions to be optimized */
int  tbl_style = 0;	/* headers for T command output */
int  rndtoeven = 0;
int  color     = 0;	/* Use color */
int  colorneg  = 0;	/* Increment color number for cells with negative
			   numbers */
int  colorerr  = 0;	/* Color cells with errors with color 3 */
int  numeric_field = 0; /* Started the line editing with a number */
int  craction = 0;	/* 1 for down, 2 for right */
int  pagesize = 0;	/* If nonzero, use instead of 1/2 screen height */
int  rowlimit = -1;
int  collimit = -1;
int  rowsinrange = 1;
int  colsinrange = DEFWIDTH;

extern	int lastmx, lastmy;	/* Screen address of the cursor */
extern	int lastcol, lcols;	/* Spreadsheet Column the cursor was in last */
extern	int lastendrow;		/* Last bottom row of screen */
extern	int framerows;		/* Rows in current frame */
extern	int framecols;		/* Columns in current frame */
extern	char mode_ind;		/* Mode indicator */

/* a linked list of free [struct ent]'s, uses .next as the pointer */
struct ent *freeents = NULL;

extern	int	seenerr;
extern	char	*rev;

#ifdef VMS
int VMS_read_raw = 0;
#endif

/* return a pointer to a cell's [struct ent *], creating if needed */
struct ent *
lookat(int row, int col)
{
    register struct ent **pp;

    checkbounds(&row, &col);
    pp = ATBL(tbl, row, col);
    if (*pp == (struct ent *)0) {
        if (freeents != NULL) {
	    *pp = freeents;
	    (*pp)->flags &= ~(is_cleared);
	    freeents = freeents->next;
	} else
	    *pp = (struct ent *) scxmalloc((unsigned)sizeof(struct ent));
	if (row>maxrow) maxrow = row;
	if (col>maxcol) maxcol = col;
	(*pp)->label = (char *)0;
	(*pp)->row = row;
	(*pp)->col = col;
	(*pp)->nrow = -1;
	(*pp)->ncol = -1;
	(*pp)->flags = 0;
	(*pp)->expr = (struct enode *)0;
	(*pp)->v = (double) 0.0;
	(*pp)->format = (char *)0;
	(*pp)->cellerror = CELLOK;
	(*pp)->next = NULL;
    }
    return (*pp);
}

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */
void
free_ent(register struct ent *p)
{
    p->next = delbuf[dbidx];
    delbuf[dbidx] = p;
    p->flags |= is_deleted;
    p->flags &= ~is_locked;
}

/* free deleted cells */
void
flush_saved()
{
    register struct ent *p;
    register struct ent *q;

    if (dbidx < 0)
	return;
    (p = delbuf[dbidx]);
    while (p) {
	(void) clearent(p);
	q = p->next;
	p->next = freeents;	/* put this ent on the front of freeents */
	freeents = p;
	p = q;
    }
    delbuf[dbidx--] = NULL;
}

char	*progname;
int	Vopt;

int
main (int argc, char  **argv)
{
    int     inloop = 1;
    register int   c;
    int     edistate = -1;
    int     narg;
    int     nedistate;
    int	    running;
    char    *revi;
    char    *home;
    int	    anychanged = FALSE;
    int     tempx, tempy; 	/* Temp versions of curx, cury */

    /*
     * Keep command line options around until the file is read so the
     * command line overrides file options
     */

    int Mopt = 0;
    int Oopt = 0;
    int Nopt = 0;
    int Copt = 0; 
    int Ropt = 0;
    int Eopt = 0;
    int Popt = 0;
    int Qopt = 0;

    Vopt = 0;

#ifdef MSDOS
    if ((revi = strrchr(argv[0], '\\')) != NULL)
#else
#ifdef VMS
    if ((revi = strrchr(argv[0], ']')) != NULL)
#else
    if ((revi = strrchr(argv[0], '/')) != NULL)
#endif
#endif
	progname = revi+1;
    else
	progname = argv[0];

    while ((c = getopt(argc, argv, "xmoncrCReP:vq")) != EOF) {
    	switch (c) {
	    case 'x':
#if defined(VMS) || defined(MSDOS) || !defined(CRYPT_PATH)
		    (void) fprintf(stderr, "Crypt not available\n");
		    exit (1);
#else 
		    Crypt = 1;
#endif
		    break;
	    case 'm':
		    Mopt = 1;
		    break;
	    case 'o':
		    Oopt = 1;
		    break;
	    case 'n':
		    Nopt = 1;
		    break;
	    case 'c':
		    Copt = 1;
		    break;
	    case 'r':
		    Ropt = 1;
		    break;
	    case 'C':
		    craction = CRCOLS;
		    break;
	    case 'R':
		    craction = CRROWS;
		    break;
	    case 'e':
		    rndtoeven = 1;
		    Eopt = 1;
		    break;
	    case 'P':
		    Popt = 1;
	    case 'v':
		    break;
	    case 'q':
		    Qopt = 1;
		    break;
	    default:
		    (void) fprintf(stderr,"Usage: %s [-cemnoqrxCR] [file...]\n",
			progname);
		    exit (1);
	}
    }

    if (!isatty(STDOUT_FILENO) || Popt || Qopt) usecurses = FALSE;
    startdisp();
    signals();

    /* setup the spreadsheet arrays, initscr() will get the screen size */
    if (!growtbl(GROWNEW, 0, 0)) {
     	stopdisp();
	exit (1);
    }

    /*
     * Build revision message for later use:
     */

    if (Popt)
	*revmsg = '\0';
    else {
	(void) strcpy(revmsg, progname);
	for (revi = rev; (*revi++) != ':'; );	/* copy after colon */
	(void) strcat(revmsg, revi);
	revmsg[strlen(revmsg) - 2] = 0;		/* erase last character */
	(void) strcat(revmsg, ":  Type '?' for help.");
    }

#ifndef MSDOS
    if (optind < argc && !strcmp(argv[optind], "--"))
	optind++;
    if (optind < argc && argv[optind][0] != '|' &&
	    !strcmp(argv[optind], "-"))
#else 
    if (optind < argc)
#endif /* MSDOS */
	(void) strcpy(curfile, argv[optind]);
    if (optind < argc) {
	readfile(argv[optind], 1);
	EvalAll();
	optind++;
    } else
	erasedb();
    while (optind < argc) {
	readfile(argv[optind], 0);
	optind++;
    }
    EvalAll();

    if (!(Popt || isatty(STDIN_FILENO)))
	readfile("-", 0);

    if (Qopt) {
	stopdisp();
	exit (0);
    }

    if (usecurses && has_colors())
	initcolor(0);

    clearok(stdscr, TRUE);
    EvalAll();

    if (Mopt)
	autocalc = 0;
    if (Oopt)
	optimize = 1;
    if (Nopt)
	numeric = 1;
    if (Copt)
	calc_order = BYCOLS;
    if (Ropt)
	calc_order = BYROWS;
    if (Eopt)
	rndtoeven = 1;
    if (Popt) {
	char *redraw = NULL;
	int o;

	optind = 0;
	stopdisp();
	while ((o = getopt(argc, argv, "xmoncrCReP:vq")) != EOF) {
	    switch (o) {
		case 'v':
		    Vopt = 1;
		    break;
		case 'P':
		    if (*optarg == '/') {
			int in, out;

			in = dup(STDIN_FILENO);
			out = dup(STDOUT_FILENO);
			freopen("/dev/tty", "r", stdin);
			freopen("/dev/tty", "w", stdout);
			usecurses = TRUE;
			startdisp();
			if (has_colors()) {
			    initcolor(0);
			    bkgd(COLOR_PAIR(1) | ' ');
			}
			clearok(stdscr, TRUE);
			FullUpdate++;
			linelim = 0;
			*line = '\0';
			if (mode_ind != 'v')
			    write_line(ctl('v'));
			error("Select range:");
			update(1);
			while (!linelim) {
			    int c;

			    switch (c = nmgetch()) {
				case '.':
				case ':':
				case ctl('i'):
				    if (!showrange) {
					write_line(c);
					break;
				    }
		    		    /* else drop through */
				case ctl('m'):
				    strcpy(line, "put ");
				    linelim = 4;
				    write_line('.');
				    if (showrange)
					write_line('.');
				    strcat(line, optarg);
				    break;
				case ESC:
				case ctl('g'):
				case 'q':
				    linelim = -1;
				    break;
				case ctl('l'):
				    FullUpdate++;
				    clearok(stdscr,1);
				    break;
				default:
				    write_line(c);
				    break;
			    }
			    /* goto switches to insert mode when done, so we
			     * have to switch back.
			     */
			    if (mode_ind == 'i')
				write_line(ctl('v'));
			    error("Select range:");
			    update(1);
			}
			stopdisp();
			dup2(in, STDIN_FILENO);
			dup2(out, STDOUT_FILENO);
			close(in);
			close(out);
			redraw = "recalc\nredraw\n";
		    } else {
			strcpy(line, "put ");
			linelim = 4;
			strcat(line, optarg);
		    }
		    if (linelim > 0) {
			linelim = 0;
			yyparse();
		    }
		    Vopt = 0;
		    break;
		default:
		    break;
	    }
	}
	if (redraw) printf(redraw);
	exit (0);
    }

    if (!isatty(STDOUT_FILENO)) {
	stopdisp();
	write_fd(stdout, 0, 0, maxrow, maxcol);
	exit (0);
    }

    modflg = 0;
    cellassign = 0;
#ifdef VENIX
    setbuf(stdin, NULL);
#endif

    while (inloop) { running = 1;
    while (running) {
	nedistate = -1;
	narg = 1;
	if (edistate < 0 && linelim < 0 && autocalc && (changed || FullUpdate))
	{
	    EvalAll();
	    if (changed)		/* if EvalAll changed or was before */
		anychanged = TRUE;
	    changed = 0;
	}
	else		/* any cells change? */
	if (changed)
	    anychanged = TRUE;

	update(anychanged);
	anychanged = FALSE;
#ifndef SYSV3	/* HP/Ux 3.1 this may not be wanted */
	(void) refresh(); /* 5.3 does a refresh in getch */ 
#endif
	c = nmgetch();
	getyx(stdscr, tempy, tempx);
	(void) move(1, 0);
	(void) clrtoeol();
	(void) move(tempy, tempx);
	seenerr = 0;
	showneed = 0;	/* reset after each update */
	showexpr = 0;
	shownote = 0;

	/*
	 * there seems to be some question about what to do w/ the iscntrl
	 * some BSD systems are reportedly broken as well
	 */
	/* if ((c < ' ') || ( c == DEL ))   how about international here ? PB */
#if	pyr
	    if(iscntrl(c) || (c >= 011 && c <= 015))	/* iscntrl broken in OSx4.1 */
#else
	    if ((isascii(c) && (iscntrl(c) || (c == 020))) ||	/* iscntrl broken in OSx4.1 */
			c == KEY_END || c == KEY_BACKSPACE)
#endif
	    switch(c) {
#ifdef SIGTSTP
		case ctl('z'):
		    (void) deraw(1);
		    (void) kill(0, SIGTSTP); /* Nail process group */

		    /* the pc stops here */

		    (void) goraw();
		    break;
#endif
		case ctl('r'):
		    showneed = 1;
		case ctl('l'):
		    FullUpdate++;
		    (void) clearok(stdscr,1);
		    break;
		case ctl('x'):
		    FullUpdate++;
		    showexpr = 1;
		    (void) clearok(stdscr,1);
		    break;
		default:
		    error ("No such command (^%c)", c + 0100);
		    break;
		case ctl('b'):
		    {
		    int ps;

		    ps = pagesize ? pagesize : (LINES - RESROW - framerows)/2;
		    backrow(arg * ps);
		    strow = strow - (arg * ps);
		    if (strow < 0) strow = 0;
		    FullUpdate++;
		    }
		    break;
		case ctl('c'):
		    running = 0;
		    break;

		case KEY_END:
		case ctl('e'):
		    if (linelim < 0 || mode_ind == 'v') {
			switch (c = nmgetch()) {
			    case KEY_UP:
			    case ctl('p'): case 'k':	doend(-1, 0);	break;

			    case KEY_DOWN:
			    case ctl('n'): case 'j':	doend( 1, 0);	break;

			    case KEY_LEFT:
			    case KEY_BACKSPACE:
			    case ctl('h'): case 'h':	doend( 0,-1);	break;

			    case KEY_RIGHT:
			    case ' ':
			    case ctl('i'): case 'l':	doend( 0, 1);	break;

			    case ctl('e'):
			    case ctl('y'):
				while (c == ctl('e') || c == ctl('y')) {
				    int x = arg;

				    while (arg) {
					if (c == ctl('e')) {
					    strow++;
					    while (strow && row_hidden[strow])
						strow++;
					    if (currow < strow)
						currow = strow;
					} else {
					    strow--;
					    while (row_hidden[strow])
						strow--;
					    forwrow(x);
					    if (currow >= lastendrow)
						backrow(1);
					    backrow(x);
					}
					arg--;
				    }
				    FullUpdate++;
				    update(0);
				    arg++;
				    c = nmgetch();
				}
				ungetch(c);
				break;

			    case ESC:
			    case ctl('g'):
				break;

			    default:
				error("Invalid ^E command");
				break;
			}
		    } else
			write_line(ctl('e'));
		    break;

		case ctl('y'):
		    while (c == ctl('e') || c == ctl('y')) {
			int x = arg;

			while (arg) {
			    if (c == ctl('e')) {
				strow++;
				while (strow && row_hidden[strow])
				    strow++;
				if (currow < strow)
				    currow = strow;
			    } else {
				strow--;
				while (row_hidden[strow])
				    strow--;
				forwrow(x);
				if (currow >= lastendrow)
				    backrow(1);
				backrow(x);
			    }
			    arg--;
			}
			FullUpdate++;
			update(0);
			arg++;
			c = nmgetch();
		    }
		    ungetch(c);
		    break;

		case ctl('f'):
		    {
		    int ps;

		    ps = pagesize ? pagesize : (LINES - RESROW - framerows)/2;
		    forwrow(arg * ps);
		    strow = strow + (arg * ps);
		    FullUpdate++;
		    }
		    break;

		case ctl('g'):
		    showrange = 0;
		    linelim = -1;
		    (void) move(1, 0);
		    (void) clrtoeol();
		    break;

		case ESC:	/* ctl('[') */
		    write_line(ESC);
		    break;

		case ctl('d'):
		    write_line(ctl('d'));
		    break;

		case KEY_BACKSPACE:
		case DEL:
		case ctl('h'):
		    if (linelim < 0) {	/* not editing line */
			backcol(arg);	/* treat like ^B    */
			break;
		    }
		    write_line(ctl('h'));
		    break;

		case ctl('i'): 		/* tab */
		    if (linelim < 0) {	/* not editing line */
			forwcol(arg);
			break;
		    }
		    write_line(ctl('i'));
		    break;

		case ctl('m'):
		case ctl('j'):
		    numeric_field = 0;
		    write_line(ctl('m'));
		    break;

		case ctl('n'):
		    c = craction;
		    if (numeric_field) {
			craction = 0;
			write_line(ctl('m'));
			numeric_field = 0;
		    }
		    craction = c;
		    if (linelim < 0) {
			forwrow(arg);
			break;
		    }
		    write_line(ctl('n'));
		    break;

		case ctl('p'):
		    c = craction;
		    if (numeric_field) {
			craction = 0;
			write_line(ctl('m'));
			numeric_field = 0;
		    }
		    craction = c;
		    if (linelim < 0) {
			backrow(arg);
			break;
		    }
		    write_line(ctl('p'));
		    break;

		case ctl('q'):
		    break;	/* ignore flow control */

		case ctl('s'):
		    break;	/* ignore flow control */

		case ctl('t'):
#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
		    error(
"Toggle: a:auto,c:cell,e:ext funcs,n:numeric,t:top,x:encrypt,$:pre-scale,<MORE>");
#else 				/* no encryption available */
		    error(
"Toggle: a:auto,c:cell,e:ext funcs,n:numeric,t:top,$:pre-scale,<MORE>");
#endif
		    (void) refresh();

		    switch (nmgetch()) {
			case 'a': case 'A':
			case 'm': case 'M':
			    autocalc ^= 1;
			    error("Automatic recalculation %sabled.",
				autocalc ? "en":"dis");
			    break;
			case 'o': case 'O':
			    optimize ^= 1;
			    error("%sptimize expressions upon entry.",
				optimize ? "O":"Do not o");
			    break;
			case 'n':
			    numeric = (!numeric);
			    error("Numeric input %sabled.",
				    numeric ? "en" : "dis");
			    break;
			case 't': case 'T':
			    showtop = (!showtop);
			    error("Top line %sabled.", showtop ? "en" : "dis");
			    break;
			case 'c':
			    showcell = (!showcell);
			    repaint(lastmx, lastmy, fwidth[lastcol]);
			    error("Cell highlighting %sabled.",
				    showcell ? "en" : "dis");
			    break;
			case 's':
			    cslop ^= 1;
			    error("Color slop %sabled.",
				    cslop ? "en" : "dis");
			    break;
			case 'C':
			    color = !color;
			    if (has_colors())
				if (color) {
				    attron(COLOR_PAIR(1));
				    bkgd(COLOR_PAIR(1) | ' ');
				} else {
				    attron(COLOR_PAIR(0));
				    bkgd(COLOR_PAIR(0) | ' ');
				}
			    error("Color %sabled.", color ? "en" : "dis");
			    break;
			case 'N':
			    colorneg = !colorneg;
			    error("Color changing of negative numbers %sabled.",
				    colorneg ? "en" : "dis");
			    break;
			case 'E':
			    colorerr = !colorerr;
			    error("Color changing of cells with errors %sabled.",
				    colorerr ? "en" : "dis");
			    break;
			case 'x': case 'X':
#if defined(VMS) || defined(MSDOS) || !defined(CRYPT_PATH)
			    error("Encryption not available.");
#else 
			    Crypt = (! Crypt);
			    error("Encryption %sabled.", Crypt? "en" : "dis");
#endif
			    break;
			case 'l': case 'L':
			    autolabel = (!autolabel);
			    error("Autolabel %sabled.",
				   autolabel? "en" : "dis");
			    break;
			case '$':
			    if (prescale == 1.0) {
				error("Prescale enabled.");
				prescale = 0.01;
			    } else {
				prescale = 1.0;
				error("Prescale disabled.");
			    }
			    break;
			case 'e':
			    extfunc = (!extfunc);
			    error("External functions %sabled.",
				    extfunc? "en" : "dis");
			    break;
			case ESC:
			case ctl('g'):
			    --modflg;	/* negate the modflg++ */
			    break;
			case 'r': case 'R':
			    error("Which direction after return key?");
			    switch(nmgetch()) {
				case ctl('m'):
				    craction = 0;
				    error("No action after new line");
				    break;
				case 'j':
				case ctl('n'):
				    craction = CRROWS;
				    error("Down row after new line");
				    break;
				case 'l':
				case ' ':
				    craction = CRCOLS;
				    error("Right column after new line");
				    break;
				case ESC:
				case ctl('g'):
				    error("");
				    break;
				default:
				    error("Not a valid direction");
			    }
			    break;
			case 'i': case 'I':
			    autoinsert = (!autoinsert);
			    error("Autoinsert %sabled.",
				   autoinsert? "en" : "dis");
			    break;
			case 'z': case 'Z':
			    rowlimit = currow;
			    collimit = curcol;
			    error("Row and column limits set");
			    break;
			default:
			    error("Invalid toggle command");
			    --modflg;	/* negate the modflg++ */
		    }
		    FullUpdate++;
		    modflg++;
		    break;

		case ctl('u'):
		    narg = arg * 4;
		    nedistate = 1;
		    break;

		case ctl('v'):	/* switch to navigate mode, or if already *
				 * in navigate mode, insert variable name */
		    if (linelim >= 0)
		        write_line(ctl('v'));
		    break;

		case ctl('w'):	/* insert variable expression */
		    if (linelim >= 0)  {
			static	char *temp = NULL, *temp1 = NULL;
			static	unsigned	templen = 0;
			int templim;

			/* scxrealloc will scxmalloc if needed */
			if (strlen(line)+1 > templen) {
			    templen = strlen(line)+40;

			    temp = scxrealloc(temp, templen);
			    temp1= scxrealloc(temp1, templen);
			}
			strcpy(temp, line);
			templim = linelim;
			linelim = 0;		/* reset line to empty	*/
			editexp(currow,curcol);
			strcpy(temp1, line);
			strcpy(line, temp);
			linelim = templim;
			ins_string(temp1);
		    }
		    break;

		case ctl('a'):
		    if (linelim >= 0)
			write_line(c);
		    else {
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
		    }
		    break;

	    } /* End of the control char switch stmt */
	else if (isascii(c) && isdigit(c) && ((!numeric && linelim < 0) ||
		(linelim >= 0 && (mode_ind == 'e' || mode_ind == 'v')) ||
		edistate >= 0)) {
	    /* we got a leading number */
	    if (edistate != 0) {
		/* First char of the count */
		if (c == '0') {    /* just a '0' goes to left col */
		    if (linelim >= 0)
			write_line(c);
		    else {
			/* Remember the current position */
			savedrow[0] = currow;
			savedcol[0] = curcol;
			savedstrow[0] = strow;
			savedstcol[0] = stcol;

			curcol = 0;
			rowsinrange = 1;
			colsinrange = fwidth[curcol];
		    }
		} else {
		    nedistate = 0;
		    narg = c - '0';
		}
	    } else {
		/* Succeeding count chars */
		nedistate = 0;
		narg = arg * 10 + (c - '0');
	    }
	} else if (c == KEY_F(1) && !fkey[c - KEY_F0 - 1]) {
	    deraw(1);
	    system("man sc");
	    goraw();
	} else if (linelim >= 0) {
	    /* Editing line */
	    switch (c) {
		case ')':
		case ',':
		    if (showrange)
			showdr();
		    break;
		default:
		    break;
	    }
	    write_line(c);

	} else if (!numeric && ( c == '+' || c == '-' )) {
	    /* increment/decrement ops */
	    register struct ent *p = *ATBL(tbl, currow, curcol);
	    if (!p || !(p->flags & is_valid))
		continue;
	    if (p->expr && !(p->flags & is_strexpr)) {
		error("Can't increment/decrement a formula\n");
		continue;
	    }
	    FullUpdate++;
	    modflg++;
	    if (c == '+')
	    	p->v += (double) arg;
	    else
		p->v -= (double) arg;
	} else if (c > KEY_F0 && c <= KEY_F(FKEYS)) {
	    /* a function key was pressed */
	    if (fkey[c - KEY_F0 - 1]) {
		char *tpp;

		write_line(ctl('m'));	/* switch to insert mode */
		strcpy(line, fkey[c - KEY_F0 - 1]);
		for (tpp = line; *tpp != '\0'; tpp++)
		    if (*tpp == '\\' && *(tpp + 1) == '"')
			memmove(tpp, tpp + 1, strlen(tpp));
		for (tpp = line; *tpp != '\0'; tpp++) {
		    char mycell[9];
		    strcpy(mycell, coltoa(curcol));
		    sprintf(mycell + strlen(mycell), "%d", currow);
		    if (*tpp == '$' && *(tpp + 1) == '$') {
			memmove(tpp + strlen(mycell), tpp + 2, strlen(tpp + 1));
			memcpy(tpp, mycell, strlen(mycell));
			tpp += strlen(mycell);
		    }
		}
		write_line(ctl('m'));
	    }
	} else
	    /* switch on a normal command character */
	    switch (c) {
		case ':':
		    if (linelim >= 0)
			write_line(':');
		    break;	/* Be nice to vi users */

		case '@':
		    EvalAll();
		    changed = 0;
		    anychanged = TRUE;
		    break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		case '.':
		    if (locked_cell(currow, curcol))
			break;
		    numeric_field = 1;
		    (void) sprintf(line,"let %s = %c",
			    v_name(currow, curcol), c);
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;

		case '+':
		case '-':
		    if (!locked_cell(currow, curcol)) {
			numeric_field = 1;
			editv(currow, curcol);
			edit_mode();
			write_line('A');
			write_line(c);
		    }
		    break;

		case '=':
		    if (locked_cell(currow, curcol))
			break;
		    (void) sprintf(line,"let %s = ", v_name(currow, curcol));
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;

		case '!':
		    {
		    /*
		     *  "! command"  executes command
		     *  "!"	forks a shell
		     *  "!!" repeats last command
		     */
#if VMS || MSDOS
		    error("Not implemented on VMS or MS-DOS");
#else /* VMS */
		    char *shl;
		    int pid, temp;
		    char cmd[MAXCMD];
		    static char lastcmd[MAXCMD];

		    if (!(shl = getenv("SHELL")))
			shl = "/bin/sh";

		    deraw(1);
		    (void) fputs("! ", stdout);
		    (void) fflush(stdout);
		    (void) fgets(cmd, MAXCMD, stdin);
		    cmd[strlen(cmd) - 1] = '\0';	/* clobber \n */
		    if (strcmp(cmd,"!") == 0)		/* repeat? */
			(void) strcpy(cmd, lastcmd);
		    else
			(void) strcpy(lastcmd, cmd);

		    if (modflg) {
			(void) puts ("[No write since last change]");
			(void) fflush(stdout);
		    }

		    if (!(pid = fork())) {
			(void) signal(SIGINT, SIG_DFL);  /* reset */
			if(strlen(cmd))
			    (void) execl(shl,shl,"-c",cmd,(char *)0);
			else
			    (void) execl(shl, shl, (char *)0);
			exit (-127);
		    }

		    while (pid != wait(&temp));

		    (void) printf("Press any key to continue ");
		    fflush(stdout);
		    cbreak();
		    (void)getch();
		    goraw();
#endif /* VMS */
		    break;
		    }

		/*
		 * Range commands:
		 */

		case '/':
		    edit_mode();
		    error(
"Range: x:erase v:value c:copy f:fill d:def l:lock U:unlock S:show u:undef F:fmt");
		    (void) refresh();

		    switch (nmgetch()) {
		    case 'l':
			(void) sprintf(line,"lock [range] ");
			write_line('A');
			startshow();
			break;
		    case 'U':
			(void) sprintf(line,"unlock [range] ");
			write_line('A');
			startshow();
			break;
		    case 'c':
			(void) sprintf(line,"copy [dest_range src_range] ");
			write_line('A');
			startshow();
			break;
		    case 'm':
			error("");
			(void) sprintf(line,"move [destination src_range] %s ",
			    v_name(currow, curcol));
			write_line('A');
		        write_line(ctl('v'));
			break;
		    case 'x':
			(void) sprintf(line,"erase [range] ");
			write_line('A');
			startshow();
			break;
		    case 'y':
			(void) sprintf(line,"yank [range] ");
			write_line('A');
			startshow();
			break;
		    case 'v':
			(void) sprintf(line, "value [range] ");
			write_line('A');
			startshow();
			break;
		    case 'f':
			(void) sprintf(line,"fill [range start inc] ");
			write_line('A');
			startshow();
			break;
		    case 'd':
			(void) sprintf(line,"define [string range] \"");
			write_line('A');
			break;
		    case 'u':
			(void) sprintf(line,"undefine [range] ");
			write_line('A');
			break;
		    case 'r':
			error("frame (top/bottom/left/right/all/unframe)");
			refresh();
			linelim = 0;
			switch (nmgetch()) {
			case 't':
			    sprintf(line,"frametop [<outrange> rows] ");
			    break;
			case 'b':
			    sprintf(line,"framebottom [<outrange> rows] ");
			    break;
			case 'l':
			    sprintf(line,"frameleft [<outrange> cols] ");
			    break;
			case 'r':
			    sprintf(line,"frameright [<outrange> cols] ");
			    break;
			case 'a':
			    sprintf(line,"frame [<outrange> inrange] ");
			    write_line('A');
			    startshow();
			    break;
			case 'u':
			    sprintf(line,"unframe [<range>] ");
			    write_line('A');
			    startshow();
			    break;
			case ESC:
			case ctl('g'):
			    error("");
			    linelim = -1;
			    break;
			default:
			    error("Invalid frame command");
			    linelim = -1;
			    break;
			}
			if (linelim == 0) {
			    error("");
			    write_line('A');
			}
			break;
		    case 's':
			(void) sprintf(line, "sort [range \"criteria\"] ");
			write_line('A');
			startshow();
			break;
		    case 'C':
			(void) sprintf(line,"color [range color#] ");
			write_line('A');
			startshow();
			break;
		    case 'S':	/* Show color definitions and ranges */
			if (!are_ranges() && !are_frames() && !are_colors())
			    error("Nothing to show");
			else {
			    FILE *f;
			    int pid;
			    char px[MAXCMD];
			    char *pager;

			    (void) strcpy(px, "| ");
			    if (!(pager = getenv("PAGER")))
				pager = DFLT_PAGER;
			    (void) strcat(px, pager);
			    f = openfile(px, &pid, NULL);
			    if (!f) {
				error("Can't open pipe to %s", pager);
				break;
			    }
			    fprintf(f, "Named Ranges:\n=============\n\n");
			    if (!brokenpipe) list_ranges(f);
			    if (!brokenpipe)
				fprintf(f, "\n\nFrames:\n=======\n\n");
			    if (!brokenpipe) list_frames(f);
			    if (!brokenpipe)
				fprintf(f, "\n\nColors:\n=======\n\n");
			    if (!brokenpipe) list_colors(f);
			    closefile(f, pid, 0);
			}
			break;
		    case 'F':
			(void) sprintf(line, "fmt [range \"format\"] ");
			write_line('A');
			startshow();
			break;
		    case ESC:
		    case ctl('g'):
			error("");
			break;
		    default:
			error("Invalid region command");
			break;
		    }
		    break;

		/*
		 * Row/column commands:
		 */

		case KEY_IC:
		case 'i':
		case 'o':
		case 'a':
		case 'd':
		case 'y':
		case 'p':
		case 'v':
		case 's':
		case 'Z':
		    {
			int rcqual;

			if (!(rcqual = get_rcqual(c))) {
			    error("Invalid row/column command");
			    break;
			}

			error("");	/* clear line */

			if (rcqual == ESC || rcqual == ctl('g'))
			    break;

			switch (c) {

			    case 'i':
				if (rcqual == 'r')	insertrow(arg, 0);
				else			insertcol(arg, 0);
				break;

			    case 'o':
				if (rcqual == 'r')	insertrow(arg, 1);
				else			insertcol(arg, 1);
				break;

			    case 'a':
				if (rcqual == 'r')	while (arg--) duprow();
				else			while (arg--) dupcol();
				break;

			    case 'd':
				if (rcqual == 'r')	deleterow(arg);
				else			closecol(arg);
				break;

			    case 'y':
				if (rcqual == 'r')	yankrow(arg);
				else			yankcol(arg);
				break;

			    case 'p':
				while (arg--)		pullcells(rcqual);
				break;

			    /*
			     * turn an area starting at currow/curcol into
			     * constants vs expressions - not reversable
			     */
			    case 'v':
				if (rcqual == 'r') {
				    struct frange *fr;

				    if ((fr = find_frange(currow, curcol)))
					valueize_area(currow, fr->or_left->col,
						currow + arg - 1,
						fr->or_right->col);
				    else
					valueize_area(currow, 0,
						currow + arg - 1, maxcol);
				} else
				    valueize_area(0, curcol,
					    maxrow, curcol + arg - 1);
				modflg = 1;
				break;

			    case 'Z':
				switch (rcqual) {
				    case 'r':	hiderow(arg);		break;
				    case 'c':	hidecol(arg);		break;
				    case 'Z':	if (modflg && curfile[0]) {
						    writefile(curfile, 0, 0,
							    maxrow, maxcol);
						    running = 0;
						} else if (modflg)
						    error("No file name.");
						else
						    running = 0;
						break;
				}
				break;

			    case 's':
			    /* special case; no repeat count */

				if (rcqual == 'r')	rowshow_op();
				else			colshow_op();
				break;
			}
			break;
		    }

		case '$':
		    {
		    register struct ent *p;

		    /* Remember the current position */
		    savedrow[0] = currow;
		    savedcol[0] = curcol;
		    savedstrow[0] = strow;
		    savedstcol[0] = stcol;

		    curcol = maxcols - 1;
		    while (!VALID_CELL(p, currow, curcol) && curcol > 0)
			curcol--;
		    rowsinrange = 1;
		    colsinrange = fwidth[curcol];
		    break;
		    }
		case '#':
		    {
		    register struct ent *p;

		    /* Remember the current position */
		    savedrow[0] = currow;
		    savedcol[0] = curcol;
		    savedstrow[0] = strow;
		    savedstcol[0] = stcol;

		    currow = maxrows - 1;
		    while (!VALID_CELL(p, currow, curcol) && currow > 0)
			currow--;
		    rowsinrange = 1;
		    colsinrange = fwidth[curcol];
		    break;
		    }
		case 'w':
		    {
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
		case 'b':
		    {
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
		case '^':
		    /* Remember the current position */
		    savedrow[0] = currow;
		    savedcol[0] = curcol;
		    savedstrow[0] = strow;
		    savedstcol[0] = stcol;

		    currow = 0;
		    rowsinrange = 1;
		    colsinrange = fwidth[curcol];
		    break;
#ifdef KEY_HELP
		case KEY_HELP:
#endif
		case '?':
		    help();
		    break;
		case '\\':
		    if (!locked_cell(currow, curcol)) {
			(void) sprintf(line, "label %s = \"",
				v_name(currow, curcol));
			linelim = strlen(line);
			edit_mode();
			write_line('A');
		    }
		    break;

		case '<':
		    if (!locked_cell(currow, curcol)) {
			(void) sprintf(line, "leftstring %s = \"",
				v_name(currow, curcol));
			linelim = strlen(line);
			edit_mode();
			write_line('A');
		    }
		    break;

		case '>':
		    if (!locked_cell(currow, curcol)) {
		       (void) sprintf(line, "rightstring %s = \"",
			      v_name(currow, curcol));
		       linelim = strlen(line);
		       edit_mode();
		       write_line('A');
		    }
		    break;
		case '{':
		    {
			struct ent *p = lookat(currow, curcol);
			if (p->label) {
			    p->flags &= ~is_label;
			    p->flags |= is_leftflush | is_changed;
			    changed++;
			    modflg++;
			} else
			    error("Nothing to justify");
			break;
		    }
		case '}':
		    {
			struct ent *p = lookat(currow, curcol);
			if (p->label) {
			    p->flags &= ~(is_label | is_leftflush);
			    p->flags |= is_changed;
			    changed++;
			    modflg++;
			} else
			    error("Nothing to justify");
			break;
		    }
		case '|':
		    {
			struct ent *p = lookat(currow, curcol);
			if (p->label) {
			    p->flags &= ~is_leftflush;
			    p->flags |= is_label | is_changed;
			    changed++;
			    modflg++;
			} else
			    error("Nothing to center");
			break;
		    }
		case 'e':
		    if (!locked_cell(currow, curcol)) {
			struct ent *p = lookat(currow, curcol);
			editv(currow, curcol);
			edit_mode();
			if (!(p->flags & is_valid))
			    write_line('A');
		    }
		    break;
		case 'E':
		    if (!locked_cell(currow, curcol)) {
			edits(currow, curcol);
			edit_mode();
		    }
		    break;
		case 'f':
		    if (arg == 1)
			(void) sprintf(line, "format [for column] %s ",
				coltoa(curcol));
		    else {
			(void) sprintf(line, "format [for columns] %s:",
				coltoa(curcol));
			(void) sprintf(line+strlen(line), "%s ",
				coltoa(curcol+arg-1));
		    }
		    error("Current format is %d %d %d",
			fwidth[curcol],precision[curcol],realfmt[curcol]);
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'F': {
		    register struct ent *p = *ATBL(tbl, currow, curcol);
		    if (p && p->format) {
			(void) sprintf(line, "fmt [format] %s \"%s",
			    v_name(currow, curcol), p->format);
			edit_mode();
			linelim = strlen(line) - 1;
		    } else {
			(void) sprintf(line, "fmt [format] %s \"",
				   v_name(currow, curcol));
			edit_mode();
			write_line('A');
		    }
		    break;
		}
		case 'C': {
		    error("Color number to set (1-8)?");
		    if ((c=nmgetch()) == ESC || c == ctl('g')) {
			error("");
			break;
		    }
		    if ((c -= ('1' - 1)) < 1 || c > 8) {
			error("Invalid color number.");
			break;
		    }
		    error("");
		    sprintf(line, "color %d = ", c);
		    linelim = strlen(line);
		    if (cpairs[c-1] && cpairs[c-1]->expr) {
			decompile(cpairs[c-1]->expr, 0);
			line[linelim] = '\0';
			edit_mode();
		    } else {
			edit_mode();
			write_line('A');
		    }
		    break;
		}
#ifdef KEY_FIND
		case KEY_FIND:
#endif
		case 'g':
		    (void) sprintf(line, "goto [v] ");
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'P':
		    (void) sprintf(line, "put [\"dest\" range] \"");

/* See the comments under "case 'W':" below for an explanation of the
 * logic here.
 */
		    curfile[strlen(curfile) + 1] = '\0';
		    if (strrchr(curfile, '.') != NULL) {
			if (!strcmp((strrchr(curfile, '.')), ".sc")) {
			    *strrchr(curfile, '.') = '\0';
			    strcpy(curfile + strlen(curfile) + 3, ".\0");
			} else if (scext != NULL &&
				!strcmp((strrchr(curfile, '.') + 1), scext)) {
			    *strrchr(curfile, '.') = '\0';
			    strcpy(curfile + strlen(curfile) +
				    strlen(scext) + 1, ".\0");
			}
		    }
		    if (*curfile)
			error("Default path is \"%s.%s\"", curfile,
				scext == NULL ? "sc" : scext);
		    c = *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1));
		    *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1)) = '\0';
		    curfile[strlen(curfile)] = c;
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'M':
		    (void) sprintf(line, "merge [\"source\"] \"");
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'R':
		    if (mdir)
			(void) sprintf(line,"merge [\"macro_file\"] \"%s", mdir);
		    else
			(void) sprintf(line,"merge [\"macro_file\"] \"");
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'D':
		    (void) sprintf(line, "mdir [\"macro_directory\"] \"");
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'A':
		    if (autorun)
			(void) sprintf(line,"autorun [\"macro_file\"] \"%s", autorun);
		    else
			(void) sprintf(line, "autorun [\"macro_file\"] \"");
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'G':
		    (void) sprintf(line, "get [\"source\"] \"");
		    if (*curfile)
			error("Default file is \"%s\"", curfile);
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'W':
		    (void) sprintf(line, "write [\"dest\" range] \"");

/* First, append an extra null byte to curfile.  Then, if curfile ends in
 * ".sc" (or '.' followed by the string in scext), move the '.' to the
 * end and replace it with a null byte.  This results in two consecutive
 * null-terminated strings, the first being curfile with the ".sc" (or '.'
 * and scext) removed, if present, and the second being either "sc." (or
 * scext and '.') or "", depending on whether the ".sc" (or '.' and scext)
 * was present or not.
 */
		    curfile[strlen(curfile) + 1] = '\0';
		    if (strrchr(curfile, '.') != NULL) {
			if (!strcmp((strrchr(curfile, '.')), ".sc")) {
			    *strrchr(curfile, '.') = '\0';
			    strcpy(curfile + strlen(curfile) + 3, ".\0");
			} else if (scext != NULL &&
				!strcmp((strrchr(curfile, '.') + 1), scext)) {
			    *strrchr(curfile, '.') = '\0';
			    strcpy(curfile + strlen(curfile) +
				    strlen(scext) + 1, ".\0");
			}
		    }

/* Now append ".asc" (or '.' and the value of ascext) to the possibly
 * truncated curfile.
 */
		    if (*curfile)
			error("Default file is \"%s.%s\"", curfile,
				ascext == NULL ? "asc" : ascext);

/* Now swap the '.' and null bytes again.  If there is no '.', swap a
 * null byte with itself.  This may seem convoluted, but it works well,
 * and obviates the need for a 1024 byte temporary buffer. - CRM
 */
		    c = *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1));
		    *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1)) = '\0';
		    curfile[strlen(curfile)] = c;
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'S':	/* set options */
		    (void) sprintf (line, "set ");
		    error("Options:byrows,bycols,iterations=n,tblstyle=(0|tbl|latex|slatex|tex|frame),<MORE>");
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
		case 'T':	/* tbl output */
		    (void) sprintf (line, "tbl [\"dest\" range] \"");

/* See the comments under "case 'W':" above for an explanation of the
 * logic here.
 */
		    curfile[strlen(curfile) + 1] = '\0';
		    if (strrchr(curfile, '.') != NULL) {
			if (!strcmp((strrchr(curfile, '.')), ".sc")) {
			    *strrchr(curfile, '.') = '\0';
			    strcpy(curfile + strlen(curfile) + 3, ".\0");
			} else if (scext != NULL &&
				!strcmp((strrchr(curfile, '.') + 1), scext)) {
			    *strrchr(curfile, '.') = '\0';
			    strcpy(curfile + strlen(curfile) +
				    strlen(scext) + 1, ".\0");
			}
		    }
		    if (*curfile && tbl_style == 0)
			error("Default file is \"%s.%s\"", curfile,
				tbl0ext == NULL ? "cln" : tbl0ext);
		    else if (*curfile && tbl_style == TBL)
			error("Default file is \"%s.%s\"", curfile,
				tblext == NULL ? "tbl" : tblext);
		    else if (*curfile && tbl_style == LATEX)
			error("Default file is \"%s.%s\"", curfile,
				latexext == NULL ? "lat" : latexext);
		    else if (*curfile && tbl_style == SLATEX)
			error("Default file is \"%s.%s\"", curfile,
				slatexext == NULL ? "stx" : slatexext);
		    else if (*curfile && tbl_style == TEX)
			error("Default file is \"%s.%s\"", curfile,
				texext == NULL ? "tex" : texext);
		    c = *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1));
		    *(curfile + strlen(curfile) +
			    strlen(curfile + strlen(curfile) + 1)) = '\0';
		    curfile[strlen(curfile)] = c;
		    linelim = strlen(line);
		    edit_mode();
		    write_line('A');
		    break;
#ifdef KEY_DC
		case KEY_DC:
#endif
		case 'x':
		    {
		    register struct ent **pp;
		    register int c1;

		    flush_saved();
		    if (arg == 1) {
			pp = ATBL(tbl, currow, curcol);
			if ((*pp) && !locked_cell(currow, curcol)) {
			    dbidx++;
			    free_ent(*pp);
			    *pp = NULL;
			}
		    } else if (calc_order == BYROWS)
			erase_area(currow, curcol, currow, curcol + arg - 1);
		    else
			erase_area(currow, curcol, currow + arg - 1, curcol);
		    sync_refs();
		    modflg++;
		    FullUpdate++;
		    }
		    break;
		case 'Q':
		case 'q':
		    running = 0;
		    break;
		case KEY_LEFT:
		case 'h':
		    backcol(arg);
		    break;
		case KEY_DOWN:
		case 'j':
		    forwrow(arg);
		    break;
		case KEY_UP:
		case 'k':
		    backrow(arg);
		    break;
		case 'H':
			backcol(curcol - stcol + 2);
			break;
#ifdef KEY_NPAGE
		case KEY_NPAGE:			/* next page */
#endif
		case 'J':
		    {
		    int ps;

		    ps = pagesize ? pagesize : (LINES - RESROW - framerows)/2;
		    forwrow(arg * ps);
		    strow = strow + (arg * ps);
		    FullUpdate++;
		    }
		    break;
#ifdef	KEY_PPAGE
		case KEY_PPAGE:			/* previous page */
#endif
		case 'K':
		    {
		    int ps;

		    ps = pagesize ? pagesize : (LINES - RESROW - framerows)/2;
		    backrow(arg * ps);
		    strow = strow - (arg * ps);
		    if (strow < 0) strow = 0;
		    FullUpdate++;
		    }
		    break;
#ifdef KEY_HOME
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
#endif
		case 'L':
		    forwcol(lcols - (curcol - stcol) + 1);
		    break;
		case KEY_RIGHT:
		case ' ':
		case 'l':
		    forwcol(arg);
		    break;
		case 'm':
		    markcell();
		    break;
		case 'c':
		    error("Copy marked cell:");
		    if ((c=nmgetch()) == ESC || c == ctl('g')) {
			error("");
			break;
		    }
		    if ((c -= ('a' - 1)) < 1 || c > 26) {
			error("Invalid character for mark (must be a-z)");
			break;
		    }
		    if (savedrow[c] == -1) {
			error("Mark not set");
			break;
		    }
		    error("");
		    {
			register struct ent *p = *ATBL(tbl, savedrow[c], savedcol[c]);
			int c1;
			register struct ent *n;
			if (!p)
			    break;
			FullUpdate++;
			modflg++;
			for (c1 = curcol; arg-- && c1 < maxcols; c1++) {
			    n = lookat(currow, c1);
			    (void) clearent(n);
			    copyent(n, p, currow - savedrow[c], c1 - savedcol[c]);
			}
		    break;
		    }
		case '`':
		case '\'':
			dotick(c);
			break;
		case 'n':
		    {
			register struct ent *p;

			error("Note: Add/Delete/Show?");
			if ((c = nmgetch()) == ESC || c == ctl('g')) {
			    error("");
			    break;
			}
			if (c == 'a' || c == 'A') {
			    sprintf(line, "addnote [target range] %s ", 
				    v_name(currow, curcol));
			    linelim = strlen(line);
			    edit_mode();
			    write_line('A');
			    write_line(ctl('v'));
			    error("");
			    break;
			}
			if (c == 'd' || c == 'D') {
			    p = lookat(currow, curcol);
			    p->nrow = p->ncol = -1;
			    error("");
			    break;
			}
			if (c == 's' || c == 'S') {
			    FullUpdate++;
			    shownote = 1;
			    clearok(stdscr,1);
			    error("Highlighted cells have attached notes.");
			    break;
			}
			error("Invalid command");
			break;
		    }
		case '*':
			gotonote();
			break;
		case 'z':
		    switch (c = nmgetch()) {
			case ctl('m'):
			    strow = currow;
			    FullUpdate++;
			    (void) clearok(stdscr,1);
			    break;
			case '.':
			    strow = -1;
			    FullUpdate++;
			    (void) clearok(stdscr,1);
			    break;
			case '|':
			    stcol = -1;
			    FullUpdate++;
			    (void) clearok(stdscr,1);
			    break;
			case 'c':
			    /* Force centering of current cell (or range, if
			     * we've just jumped to a new range with the goto
			     * command).
			     */
			    strow = -1;
			    stcol = -1;
			    FullUpdate++;
			    (void) clearok(stdscr,1);
			    break;
			default:
			    break;
		    }
		    break;
		default:
		    if ((toascii(c)) != c)
			error ("Weird character, decimal %d\n",
				(int) c);
		    else
			    error ("No such command (%c)", c);
		    break;
	    }
	edistate = nedistate;
	arg = narg;
    }				/* while (running) */
    inloop = modcheck(" before exiting");
    }				/*  while (inloop) */
    stopdisp();
#ifdef VMS	/* Until VMS "fixes" exit we should say 1 here */
    exit (1);
#else
    exit (0);
#endif
    /*NOTREACHED*/
}

/* set the calculation order */
void
setorder(int i)
{
	if ((i == BYROWS) || (i == BYCOLS))
	    calc_order = i;
}

void
setauto(int i)
{
	autocalc = i;
}

void
signals()
{
#ifdef SIGVOID
    void doquit();
    void time_out();
    void dump_me();
    void nopipe();
#ifdef	SIGWINCH
    void winchg();
#endif
#else
    int doquit();
    int time_out();
    int dump_me();
    int nopipe();
#ifdef	SIGWINCH
    int winchg();
#endif
#endif

    (void) signal(SIGINT, doquit);
#if !defined(MSDOS)
    (void) signal(SIGQUIT, dump_me);
    (void) signal(SIGPIPE, nopipe);
    (void) signal(SIGALRM, time_out);
    (void) signal(SIGBUS, doquit);
#endif
    (void) signal(SIGTERM, doquit);
    (void) signal(SIGFPE, doquit);
#ifdef	SIGWINCH
    (void) signal(SIGWINCH, winchg);
#endif
}

#ifdef SIGVOID
void
#else
int
#endif
nopipe()
{
    brokenpipe = TRUE;
}

#ifdef	SIGWINCH
#ifdef SIGVOID
void
#else
int
#endif
winchg()
{
    stopdisp();
    startdisp();
    refresh();
    FullUpdate++;
    (void) clearok(stdscr, TRUE);
    update(1);
    refresh();
    (void) signal(SIGWINCH, winchg);
}
#endif

#ifdef SIGVOID
void
#else
int
#endif
doquit()
{
    if (usecurses) {
	diesave();
	stopdisp();
    }
    exit (1);
}

#ifdef SIGVOID
void
#else
int
#endif
dump_me()
{
    if (usecurses)
	diesave();
    deraw(1);
    abort();
}

/* try to save the current spreadsheet if we can */
void
diesave()
{   char	path[PATHLEN];

    if (modcheck(" before Spreadsheet dies") == 1)
    {	(void) sprintf(path, "~/%s", SAVENAME);
	if (writefile(path, 0, 0, maxrow, maxcol) < 0)
	{
	    (void) sprintf(path, "/tmp/%s", SAVENAME);
	    if (writefile(path, 0, 0, maxrow, maxcol) < 0)
		error("Couldn't save current spreadsheet, Sorry");
	}
    }
}

/* check if tbl was modified and ask to save */
int
modcheck(char *endstr)
{
    if (modflg && curfile[0]) {
	int	yn_ans;
	char	lin[100];

	(void) sprintf(lin,"File \"%s\" is modified, save%s? ",curfile,endstr);
	if ((yn_ans = yn_ask(lin)) < 0)
		return(1);
	else
	if (yn_ans == 1) {
	    if (writefile(curfile, 0, 0, maxrow, maxcol) < 0)
 		return (1);
	}
    } else if (modflg) {
	int	yn_ans;

	if ((yn_ans = yn_ask("Do you want a chance to save the data? ")) < 0)
		return(1);
	else
		return(yn_ans);
    }
    return(0);
}

/* Returns 1 if cell is locked, 0 otherwise */
int
locked_cell(int r, int c)
{
    struct ent *p = *ATBL(tbl, r, c);
    if (p && (p->flags & is_locked)) {
	error("Cell %s%d is locked", coltoa(c), r) ;
	return(1);
    }
    return(0);
}

/* Check if area contains locked cells */
int
any_locked_cells(int r1, int c1, int r2, int c2)
{
    int r, c;
    struct ent *p ;

    for (r=r1; r<=r2; r++)
	for (c=c1; c<=c2; c++) {
	    p = *ATBL(tbl, r, c);
	    if (p && (p->flags&is_locked))
		return(1);
	}
    return(0);
}
