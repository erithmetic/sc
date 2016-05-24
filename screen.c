/*	SC	A Spreadsheet Calculator
 *		Curses based Screen driver
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		$Revision: 7.16 $
 *
 */


#include <curses.h>
#include <time.h>
#include "sc.h"

#ifndef MSDOS
#include <unistd.h>
#endif

#ifdef VMS
extern int VMS_read_raw;   /*sigh*/
    VMS_read_raw = 1;
#endif

#ifdef BROKENCURSES
		/* nl/nonl bug fix */
#undef nl
#undef nonl
#define nl()	 (_tty.sg_flags |= CRMOD,_pfast = _rawmode,stty(_tty_ch, &_tty))
#define nonl()	 (_tty.sg_flags &= ~CRMOD, _pfast = TRUE, stty(_tty_ch, &_tty))
#endif

void	repaint(int x, int y, int len, int attron, int attroff);

char	under_cursor = ' '; /* Data under the < cursor */
char	mode_ind = 'i';
char	search_ind = ' ';
extern	char    revmsg[];

int	lines, cols;
int	rows, lcols;
int	lastmx, lastmy;	/* Screen address of the cursor */
int	lastrow = -1;	/* Spreadsheet Row the cursor was in last */
int	lastcol = -1;	/* Spreadsheet Column the cursor was in last */
int	lastendrow = -1;	/* Last bottom row of screen */
int	lastftoprows = 0;	/* Rows in top of frame cursor was in last */
int	lastfbottomrows = 0;	/* Rows in bottom of frame cursor was in last */
int	lastfleftcols = 0;	/* Columns in left side of frame cursor was
				    in last */
int	lastfrightcols = 0;
struct	frange *lastfr = 0;	/* Last framed range we were in */
bool	frTooLarge = 0;	/* If set, either too many rows or too many columns
			    exist in frame to allow room for the scrolling
			    portion of the framed range */
int	framerows;	/* Rows in current frame */
int	framecols;	/* Columns in current frame */
int	rescol = 4;	/* Columns reserved for row numbers */
extern	int *fwidth;
extern	int showneed;	/* Causes cells needing values to be highlighted */
extern	int showexpr;	/* Causes cell exprs to be displayed, highlighted */
extern	int shownote;	/* Causes cells with attached notes to be highlighted */
#ifdef RIGHT_CBUG
extern	int wasforw;	/* Causes screen to be redisplay if on lastcol */
#endif
extern struct go_save gs;

/*
 * update() does general screen update
 *
 * standout last time in update()?
 *	At this point we will let curses do work
 */
int	standlast	= FALSE;

void
update(int anychanged)		/* did any cell really change in value? */
{
    int				row, col;
    struct ent			**pp;
    int				mxrow, mxcol;
    int				minsr = 0, minsc = 0, maxsr = 0, maxsc = 0;
    int				r, i;
    struct frange		*fr;
    struct crange		*cr;
    int				ftoprows, fbottomrows, fleftcols, frightcols;
    int				ftrows, fbrows, flcols, frcols;
    bool			message;

#ifndef MSDOS
    /*
     * If receiving input from a pipeline, don't display spreadsheet data
     * on screen.
     */
    if (!usecurses) return;
#endif

    getmaxyx(stdscr, lines, cols);
    fr = lastfr;
    if (!(fr && fr->or_left->row <= currow &&	/* If we've left the	    */
	    fr->or_left->col <= curcol &&	/* previous framed range... */
	    fr->or_right->row >= currow &&
	    fr->or_right->col >= curcol)) {
	fr = find_frange(currow, curcol);
	if (fr != lastfr)
	    FullUpdate++;
    }
    if (fr) {
	ftoprows = fr->ir_left->row - fr->or_left->row;
	fbottomrows = fr->or_right->row - fr->ir_right->row;
	fleftcols = fr->ir_left->col - fr->or_left->col;
	frightcols = fr->or_right->col - fr->ir_right->col;
	framerows = RESROW + ftoprows + fbottomrows;
	framecols = rescol;
	for (r = fr->or_left->row - 1, i = ftoprows; i; i--)
	    if (row_hidden[r+i])
		framerows--;
	for (r = fr->or_right->row - 1, i = fbottomrows; i; i--)
	    if (row_hidden[r-i])
		framerows--;
	for (r = fr->or_left->col - 1, i = fleftcols; i; i--) {
	    if (col_hidden[r+i])
		continue;
	    framecols += fwidth[r+i];
	}
	for (r = fr->or_right->col - 1, i = frightcols; i; i--) {
	    if (col_hidden[r-i])
		continue;
	    framecols += fwidth[r-i];
	}
	if (framerows >= lines || framecols >= cols) {
	    frTooLarge = TRUE;
	    if (FullUpdate) {
		error("Frame too large for screen size - ignoring");
	    }
	    ftoprows = fbottomrows = fleftcols = frightcols = 0;
	    strow -= lastftoprows;
	    stcol -= lastfleftcols;
	} else {
	    frTooLarge = FALSE;
	    if (strow >= fr->or_left->row) {
		if (fr == lastfr && strow < fr->or_left->row + ftoprows)
		    strow = fr->or_left->row;
		else if (strow > fr->ir_right->row) {
		    strow = fr->ir_right->row;
		    FullUpdate++;
		}
	    }
	    if (stcol >= fr->or_left->col) {
		if (stcol < fr->or_left->col + fleftcols)
		    stcol = fr->or_left->col;
		else if (stcol > fr->ir_right->col) {
		    stcol = fr->ir_right->col;
		    FullUpdate++;
		}
	    }
	    if (fr == lastfr && currow == lastrow)
		fbottomrows = lastfbottomrows;
	    else if (currow < fr->ir_right->row)
		fbottomrows = fr->or_right->row - fr->ir_right->row;
	    else
		fbottomrows = fr->or_right->row - currow;
	    if (fr == lastfr && curcol == lastcol)
		frightcols = lastfrightcols;
	    else if (curcol < fr->ir_right->col)
		frightcols = fr->or_right->col - fr->ir_right->col;
	    else
		frightcols = fr->or_right->col - curcol;
	}
    } else {
	ftoprows = fbottomrows = fleftcols = frightcols = 0;
	framerows = framecols = 0;
    }
    if (fr != lastfr && !gs.stflag && lastfr) {
	if (strow >= lastfr->ir_left->row)
	    strow -= lastftoprows;
	if (stcol >= lastfr->ir_left->col)
	    stcol -= lastfleftcols;
    }

    ftrows = ftoprows;
    fbrows = fbottomrows;
    flcols = frcols = 0;
    if (fr) {
	for (r = fr->or_left->row - 1, i = ftrows; i; i--)
	    if (row_hidden[r+i])
		ftrows--;
	for (r = fr->or_right->row + 1, i = fbrows; i; i--)
	    if (row_hidden[r-i])
		fbrows--;
	for (r = fr->or_left->col - 1, i = fleftcols; i; i--) {
	    if (col_hidden[r+i])
		continue;
	    flcols += fwidth[r+i];
	}
	for (r = fr->or_right->col + 1, i = frightcols; i; i--) {
	    if (col_hidden[r-i])
		continue;
	    frcols += fwidth[r-i];
	}
    }

    /*
     * Place the cursor on the screen.  Set col, curcol, stcol, lastcol as
     * needed.  If strow and stcol are negative, centering is forced.
     */
    if ((curcol != lastcol) || FullUpdate) {
	while (col_hidden[curcol])   /* You can't hide the last row or col */
	    curcol++;
	if (fwidth[curcol] > cols - rescol - 2) {
	    error("column %s too wide - resizing", coltoa(curcol));
	    doformat(curcol, curcol, cols - rescol - 2,
		    precision[curcol], realfmt[curcol]);
	}

	/* First see if the last display still covers curcol */
	if (stcol >= 0 && stcol <= curcol) {
	    int c = 0;

	    if (fr) {
		if (fr != lastfr) {
		    if (stcol == fr->or_left->col)
			stcol += fleftcols;
		    else if (stcol >= fr->or_left->col && !gs.stflag) {
			stcol += fleftcols;
			if (stcol > fr->ir_right->col)
			    stcol = fr->ir_right->col + 1;
		    }
		} else if (stcol == fr->or_left->col)
		    stcol += fleftcols;
	    }
	    i = stcol;
	    lcols = 0;
	    col = rescol + frcols;
	    if (fr && stcol >= fr->or_left->col)
		if (stcol < fr->ir_left->col)
		    i = fr->or_left->col;
		else
		    col += flcols;
	    for (; (col + fwidth[i] < cols-1 || col_hidden[i] || i < curcol) &&
		    i < maxcols; i++) {
		lcols++;
		if (fr && i == fr->ir_right->col + 1) {
		    col -= frcols;
		    frcols = frightcols = 0;
		}
		if (col_hidden[i])
		    continue;

		/* If there isn't room for more columns, and we haven't yet
		 * reached the current column, start removing columns from
		 * the left.
		 */
		while (col + fwidth[i] > cols - 2) {
		    lcols--;
		    col -= fwidth[stcol];
		    while (col_hidden[++stcol]) /**/ ;
		    FullUpdate++;
		    c++;
		}
		col += fwidth[i];
	    }
	    if (!frTooLarge && fr && curcol <= stcol + lcols &&
		    fr->ir_left->col >= stcol + lcols) {
		while (stcol + lcols < fr->ir_left->col) {
		    col -= fwidth[stcol];
		    lcols--;
		    while (col_hidden[++stcol])
			lcols--;
		    while (col + fwidth[stcol + lcols] < cols - 1) {
			col += fwidth[stcol + lcols];
			lcols++;
		    }
		}
	    } else if (c)
		stcol = -1;
	}

	while (stcol < 0 || curcol < stcol || stcol + lcols - 1 < curcol ||
		(colsinrange != fwidth[curcol] && stcol != curcol &&
		stcol + lcols - 1 < gs.g_lastcol)) {

	    FullUpdate++;

		/* How about back one? */
	    if (stcol - 1 == curcol) {
		stcol--;
		/* Forward one? */
	    } else if (stcol >= 0 && stcol + lcols == curcol) {
		stcol++;
	    } else if (stcol >= 0 && fr && curcol >= fr->or_left->col &&
		    curcol <= fr->ir_left->col && stcol < curcol &&
		    curcol <= stcol + lcols + fr->ir_left->col -
			    fr->or_left->col) {
		while ((stcol + lcols < fr->ir_left->col && !frTooLarge) ||
			(colsinrange != fwidth[curcol] && stcol != curcol &&
			stcol + lcols - 1 < gs.g_lastcol)) {
		    if (col_hidden[++stcol]) lcols--;
		}
	    } else {
		/* Try to put the cursor in the center of the screen.
		 * If we've just jumped to a range using the goto command,
		 * center the range instead.
		 */
		colsinrange = (colsinrange > cols - rescol -
			flcols - frcols - 2 ?
			cols - rescol - flcols - frcols - 2 : colsinrange);
		col = (cols - rescol - flcols - frcols - colsinrange)/2; 
		stcol = curcol;
		for (i = curcol - 1;
			i >= (fr ? fr->or_left->col + fleftcols : 0) &&
			(col - fwidth[i] > 0 || col_hidden[i]);
			i--) {
		    stcol--;
		    if (col_hidden[i])
			continue;
		    col -= fwidth[i];
		}
		if (fr && stcol < fr->or_left->col + fleftcols) {
		    stcol = fr->or_left->col + fleftcols;
		    if (curcol < stcol)
			stcol = curcol;
		}
	    }
	    /* Now pick up the counts again */
	    i = stcol;
	    lcols = 0;
	    col = rescol + frcols;
	    if (fr && stcol >= fr->or_left->col)
		if (stcol < fr->ir_left->col)
		    i = fr->or_left->col;
		else
		    col += flcols;
	    for (; (col + fwidth[i] < cols-1 || col_hidden[i] || i < curcol) &&
		    i < maxcols; i++) {
		lcols++;
		if (fr && i == fr->ir_right->col + 1) {
		    col -= frcols;
		    frcols = frightcols = 0;
		}
		if (col_hidden[i])
		    continue;

		col += fwidth[i];
	    }
	}
    }
    if (fleftcols && stcol >= fr->or_left->col &&
	    stcol < fr->or_left->col + fleftcols) {
	lcols += (fr->or_left->col - stcol);
	stcol = fr->or_left->col + fleftcols;
	if (curcol < stcol)
	    stcol = curcol;
    }

    /* Now - same process on the rows as the columns */
    if ((currow != lastrow) || FullUpdate) {
	while (row_hidden[currow])   /* You can't hide the last row or col */
	    currow++;
	if (strow >= 0 && strow <= currow) {
	    int c = 0;

	    if (fr) {
		if (fr != lastfr) {
		    if (strow == fr->or_left->row)
			strow += ftoprows;
		    else if (strow >= fr->or_left->row && !gs.stflag) {
			strow += ftoprows;
			if (strow > fr->ir_right->row)
			    strow = fr->ir_right->row + 1;
		    }
		} else if (strow == fr->or_left->row)
		    strow += ftoprows;
	    }
	    i = strow;
	    rows = 0;
	    row = RESROW + fbrows;
	    if (fr && strow >= fr->or_left->row)
		if (strow < fr->ir_left->row)
		    i = fr->or_left->row;
		else
		    row += ftrows;
	    for (; (row < lines || row_hidden[i] || i < currow) && i < maxrows;
		    i++) {
		rows++;
		if (fr && i == fr->ir_right->row + 1) {
		    row -= fbrows;
		    fbrows = fbottomrows = 0;
		}
		if (row_hidden[i])
		    continue;

		/* If there isn't room for more rows, and we haven't yet
		 * reached the current row, start removing rows from the
		 * top.
		 */
		if (row >= lines) {
		    rows--;
		    row--;
		    while (row_hidden[++strow]) /**/;
		    FullUpdate++;
		    c++;
		}
		row++;
	    }
	    if (!frTooLarge && fr && currow <= strow + rows &&
		    fr->ir_left->row >= strow + rows) {
		while (strow + rows < fr->ir_left->row) {
		    while (row_hidden[++strow]) /**/;
		}
	    } else if (c && currow > lastendrow)
		strow = -1;
	}

	while (strow < 0 || currow < strow || strow + rows - 1 < currow ||
		strow + rows < currow + rowsinrange) {

	    FullUpdate++;

		/* How about up one? */
	    if (strow - 1 == currow) {
		strow--;
		/* Down one? */
	    } else if (strow >= 0 && strow + rows == currow) {
		strow++;
	    } else if (strow >= 0 && fr && currow >= fr->or_left->row &&
		    currow <= fr->ir_left->row && strow < currow &&
		    currow <= strow + rows + fr->ir_left->row -
			    fr->or_left->row) {
		while ((strow + rows < fr->ir_left->row && !frTooLarge) ||
			(rowsinrange > 1 && strow != currow &&
			strow + rows - 1 < gs.g_lastrow)) {
		    if (row_hidden[++strow]) rows--;
		}
	    } else {
		/* Try to put the cursor in the center of the screen.
		 * If we've just jumped to a range using the goto command,
		 * center the range instead.
		 */
		rowsinrange = (rowsinrange > lines - RESROW - ftrows - fbrows ?
			lines - RESROW - ftrows - fbrows : rowsinrange);
		row = (lines - RESROW - ftrows - fbrows - rowsinrange)/2; 
		strow = currow;
		for (i = currow - 1;
			i >= (fr ? fr->or_left->row + ftoprows : 0) &&
			(row > 0 || row_hidden[i]); i--) {
		    strow--;
		    if (row_hidden[i])
			continue;
		    row--;
		}
		if (fr && strow < fr->or_left->row + ftoprows)
		    strow = fr->or_left->row + ftoprows;
		    if (currow < strow)
			strow = currow;
	    }
	    /* Now pick up the counts again */
	    i = strow;
	    rows = 0;
	    row = RESROW + fbrows;
	    if (fr && strow >= fr->or_left->row)
		if (strow < fr->ir_left->row)
		    i = fr->or_left->row;
		else
		    row += ftrows;
	    for (; (row < lines || row_hidden[i] || i < currow) && i < maxrows;
		    i++) {
		rows++;
		if (fr && i == fr->ir_right->row + 1) {
		    row -= fbrows;
		    fbrows = fbottomrows = 0;
		}
		if (row_hidden[i])
		    continue;

		row++;
	    }
	}
    }
    if (ftoprows && strow >= fr->or_left->row &&
	    strow < fr->or_left->row + ftoprows) {
	rows += (fr->or_left->row - strow);
	strow = fr->or_left->row + ftoprows;
	if (currow < strow)
	    strow = currow;
    }

    mxcol = frightcols ? fr->or_right->col : stcol + lcols - 1;
    mxrow = fbottomrows ? fr->or_right->row : strow + rows - 1;
    gs.stflag = 0;
    lastfr = fr;
    lastftoprows = ftoprows;
    lastfbottomrows = fbottomrows;
    lastfleftcols = fleftcols;
    lastfrightcols = frightcols;

    /* Get rid of cursor standout on the cell at previous cursor position */
    if (!FullUpdate) {
	if (showcell) {
	    pp = ATBL(tbl, lastrow, lastcol);
	    if (color && has_colors()) {
		if ((cr = find_crange(lastrow, lastcol)))
		    color_set(cr->r_color, NULL);
		else
		    color_set(1, NULL);
		if (*pp) {
		    if (colorneg && (*pp)->flags & is_valid && (*pp)->v < 0) {
			if (cr)
			    color_set(((cr->r_color) % CPAIRS) + 1, NULL);
			else
			    color_set(2, NULL);
		    }
		    else if (colorerr && (*pp)->cellerror)
			color_set(3, NULL);
		}
	    }
	    repaint(lastmx, lastmy, fwidth[lastcol], 0, A_STANDOUT);
	}

	(void) move(lastmy, lastmx+fwidth[lastcol]);

	if ((inch() & A_CHARTEXT) == '<')
	    (void) addch(under_cursor | (inch() & A_ATTRIBUTES));

	repaint(lastmx, RESROW - 1, fwidth[lastcol], A_STANDOUT, 0);
	repaint(0, lastmy, rescol - 1, A_STANDOUT, 0);
	if (color && has_colors())
	    color_set(1, NULL);
    }
    lastrow = currow;
    lastcol = curcol;
    lastendrow = strow + rows;

    /* where is the the cursor now? */
    lastmy =  RESROW;
    if (fr && strow >= fr->or_left->row)
	if (strow < fr->ir_left->row)
	    row = fr->or_left->row;
	else {
	    row = strow;
	    lastmy += ftrows;
	}
    else
	row = strow;
    for (; row < currow; row++)
	if (!row_hidden[row])
	    lastmy++;

    lastmx = rescol;
    if (fr && stcol >= fr->or_left->col)
	if (stcol < fr->ir_left->col)
	    col = fr->or_left->col;
	else {
	    col = stcol;
	    lastmx += flcols;
	}
    else
	col = stcol;
    for (; col < curcol; col++)
	if (!col_hidden[col])
	    lastmx += fwidth[col];

    if (color && has_colors())
	color_set(1, NULL);

    if (FullUpdate || standlast) {
	(void) move(2, 0);
	(void) clrtobot();
	(void) standout();

	for (row = RESROW, i = (ftoprows && strow >= fr->or_left->row ?
		fr->or_left->row : strow);
		i <= mxrow; i++) {
	    if (ftoprows && strow >= fr->or_left->row &&
		    row == RESROW + ftrows)
		i = (strow < i ? i : strow);
	    if (fbottomrows && row == lines - fbrows)
		i = fr->or_right->row - fbottomrows + 1;
	    if (row_hidden[i]) 
		continue;
	    (void) move(row, 0);
	    (void) printw("%*d", rescol - 1, i);
	    row++;
	}
#ifdef RIGHT_CBUG
	if (wasforw) {
	    clearok(stdscr, TRUE);
	    wasforw = 0;
	}
#endif
	(void) move(2, 0);
	(void) printw("%*s", rescol, " ");

	for (col = rescol, i = (fleftcols && stcol >= fr->or_left->col ?
		fr->or_left->col : stcol);
		i <= mxcol; i++) {
	    register int k;
	    if (fleftcols && stcol >= fr->or_left->col &&
		    col == rescol + flcols)
		i = (stcol < i ? i : stcol);
	    if (frightcols && col + fwidth[i] >= cols - 1 - frcols &&
		    i < fr->or_right->col - frightcols + 1)
		i = fr->or_right->col - frightcols + 1;
	    if (col_hidden[i])
		continue;
	    (void) move(2, col);
	    k = (fwidth[i] - strlen(coltoa(i)))/2;
	    if (fwidth[i] == 1)
		(void) printw("%1s", coltoa(i%26));
	    else if (braille)
	        (void) printw("%-*s", fwidth[i], coltoa(i));
	    else
	        (void) printw("%*s%-*s", k, "", fwidth[i]-k, coltoa(i));
	    col += fwidth[i];
	}
	(void) standend();
    }

    (void) move(1, 0);
    message = (inch() & A_CHARTEXT) != ' ';
    if (showrange) {
	if (showrange == SHOWROWS) {
	    minsr = showsr < currow ? showsr : currow;
	    minsc = fr ? fr->or_left->col : 0;
	    maxsr = showsr > currow ? showsr : currow;
	    maxsc = fr ? fr->or_right->col : maxcols;

	    if (showtop && !message) {
		(void) clrtoeol();
		(void) printw("Default range:  %d:%d", minsr, maxsr);
	    }
	} else if (showrange == SHOWCOLS) {
	    minsr = 0;
	    minsc = showsc < curcol ? showsc : curcol;
	    maxsr = maxrows;
	    maxsc = showsc > curcol ? showsc : curcol;

	    if (showtop && !message) {
		char r[6];

		strcpy(r, coltoa(minsc));
		strcat(r, ":");
		strcat(r, coltoa(maxsc));
		(void) clrtoeol();
		(void) printw("Default range:  %s", r);
	    }
	} else {
	    minsr = showsr < currow ? showsr : currow;
	    minsc = showsc < curcol ? showsc : curcol;
	    maxsr = showsr > currow ? showsr : currow;
	    maxsc = showsc > curcol ? showsc : curcol;

	    if (showtop && !message) {
		(void) clrtoeol();
		(void) printw("Default range:  %s",
			    r_name(minsr, minsc, maxsr, maxsc));
	    }
	}
    } else if (braille && braillealt && !message && mode_ind == 'v') {
	(void) clrtoeol();
	(void) printw("Current cell:   %s%d ", coltoa(curcol), currow);
    }

    /* Repaint the visible screen */
    if (showrange || anychanged || FullUpdate || standlast) {
	/* may be reset in loop, if not next time we will do a FullUpdate */
      if (standlast) {
      	FullUpdate = TRUE;
	standlast = FALSE;
      }

      for (row = (ftoprows && strow >= fr->or_left->row ?
		  fr->or_left->row : strow), r = RESROW;
	    row <= mxrow; row++) {
	int c = rescol;
	int do_stand = 0;
	int fieldlen;
	int nextcol;

	if (row_hidden[row])
	    continue;
	if (ftoprows && strow >= fr->or_left->row && r == RESROW + ftrows)
	    row = (strow < row ? row : strow);
	if (fbottomrows && r == lines - fbrows)
	    row = fr->or_right->row - fbottomrows + 1;
	for (pp = ATBL(tbl, row, col = (fleftcols && stcol >= fr->or_left->col ?
		fr->or_left->col : stcol));
		col <= mxcol;
		pp += nextcol - col,  col = nextcol, c += fieldlen) {

	    if (fleftcols && stcol >= fr->or_left->col &&
		    c == rescol + flcols) {
		col = (stcol < col ? col : stcol);
		pp = ATBL(tbl, row, col);
	    }
	    if (frightcols && c + fwidth[col] >= cols - 1 - frcols &&
		    col < fr->or_right->col - frightcols + 1) {
		col = fr->or_right->col - frightcols + 1;
		pp = ATBL(tbl, row, col);
	    }
	    nextcol = col + 1;
	    if (col_hidden[col]) {
		fieldlen = 0;
		continue;
	    }

	    fieldlen = fwidth[col];

	    /*
	     * Set standout if:
	     *
	     * - showing ranges, and not showing cells which need to be filled
	     *	 in, and not showing cell expressions, and in a range, OR
	     *
	     * - showing cells which need to be filled in and this one is
	     *	 of that type (has a value and doesn't have an expression,
	     *	 or it is a string expression), OR
	     *
	     * - showing cells which have expressions and this one does.
	     */
	    if ((showrange && (!showneed) && (!showexpr)
			&& (row >= minsr) && (row <= maxsr)
			&& (col >= minsc) && (col <= maxsc))
		    || (showneed && (*pp) && ((*pp)->flags & is_valid) &&
			(((*pp)->flags & is_strexpr) || !((*pp)->expr)))
		    || (showexpr && (*pp) && ((*pp)->expr))
		    || (shownote && (*pp) && ((*pp)->nrow >= 0))) {

		(void) move(r, c);
		(void) standout();
		if (color && has_colors() && (cr = find_crange(row, col)))
		    color_set(cr->r_color, NULL);
		standlast++;
		if (!*pp) {	/* no cell, but standing out */
		    (void) printw("%*s", fwidth[col], " ");
		    (void) standend();
		    if (color && has_colors())
			color_set(1, NULL);
		    continue;
		} else
		    do_stand = 1;
	    } else
		do_stand = 0;

	    if ((cr = find_crange(row, col)) && color && has_colors())
		color_set(cr->r_color, NULL);

	    if ((*pp) && (((*pp)->flags & is_changed || FullUpdate) ||
		    do_stand)) {
		if (do_stand) {
		    (*pp)->flags |= is_changed; 
		} else {
		    (void) move(r, c);
		    (*pp)->flags &= ~is_changed;
		}

		/*
		 * Show expression; takes priority over other displays:
		 */

		if ((*pp)->cellerror) {
		    if (color && colorerr && has_colors())
			color_set(3, NULL);
		    (void) printw("%*.*s", fwidth[col], fwidth[col],
			(*pp)->cellerror == CELLERROR ? "ERROR" : "INVALID");
		} else
		if (showexpr && ((*pp)->expr)) {
		    linelim = 0;
		    editexp(row, col);		/* set line to expr */
		    linelim = -1;
		    showstring(line, /* leftflush = */ 1, /* hasvalue = */ 0,
			    row, col, &nextcol, mxcol, &fieldlen, r, c,
			    fr, frightcols, flcols, frcols);
		} else {
		    /*
		     * Show cell's numeric value:
                     */

		    if ((*pp)->flags & is_valid) {
			char field[FBUFLEN];
			char *cfmt;
			int note;

			*field = '\0';
			note = (*pp)->nrow >= 0 ? 1 : 0;
			cfmt = (*pp)->format ? (*pp)->format :
			    (realfmt[col] >= 0 && realfmt[col] < COLFORMATS &&
			    colformat[realfmt[col]]) ?
			    colformat[realfmt[col]] : NULL;
			if (color && has_colors() && colorneg && (*pp)->v < 0) {
			    if (cr)
				color_set(((cr->r_color) % CPAIRS) + 1, NULL);
			    else
				color_set(2, NULL);
			}
			if (cfmt) {
			    if (*cfmt == ctl('d')) {
				time_t v = (time_t) ((*pp)->v);
				strftime(field, sizeof(field),
					cfmt + 1, localtime(&v));
			    } else
				(void) format(cfmt, precision[col], (*pp)->v,
					field, sizeof(field));
			} else {
			    (void) engformat(realfmt[col], fwidth[col] - note,
				    precision[col], (*pp)->v, 
				    field, sizeof(field));
			}
			if (strlen(field) > fwidth[col]) {
			    for (i = 0; i < fwidth[col]; i++) {
				if (note) {
				    attr_t attr;
				    short curcolor;
				    if (!i && color && has_colors()) {
					attr_get(&attr, &curcolor, NULL);
					color_set(4, NULL);
				    }
				    (void)addch('*');
				    i++;
				    if (!i && color && has_colors())
					color_set(curcolor, NULL);
				}
				(void)addch('*');
			    }
			} else {
			    if (cfmt && *cfmt != ctl('d'))
				for (i = 0;
					i < fwidth[col] - strlen(field) - note;
					i++)
				    (void)addch(' ');
			    if (note) {
				attr_t attr;
				short curcolor;
				if (color && has_colors()) {
				    attr_get(&attr, &curcolor, NULL);
				    color_set(4, NULL);
				}
				(void)addch('*');
				if (color && has_colors())
				    color_set(curcolor, NULL);
			    }
			    (void)addstr(field);
			    if (cfmt && *cfmt == ctl('d'))
				for (i = 0;
					i < fwidth[col] - strlen(field) - note;
					i++)
				    (void)addch(' ');
			}
		    }

		    /*
		     * Show cell's label string:
		     */

		    if ((*pp)->label) {
			showstring((*pp)->label,
				    (*pp)->flags & (is_leftflush|is_label),
				    (*pp)->flags & is_valid,
				    row, col, &nextcol, mxcol, &fieldlen,
				    r, c, fr, frightcols, flcols, frcols);
		    } else	/* repaint a blank cell: */
		    if ((((do_stand || !FullUpdate) &&
			    ((*pp)->flags & is_changed)) ||
			    (color && has_colors() &&
			    cr && cr->r_color != 1)) &&
			    !((*pp)->flags & is_valid) && !(*pp)->label) {
			(void) printw("%*s", fwidth[col], " ");
		    }
		} /* else */
	    } else
	    if (!*pp && color && has_colors && cr && cr->r_color != 1) {
		move(r, c);
		color_set(cr->r_color, NULL);
		printw("%*s", fwidth[col], " ");
	    }
	    if (color && has_colors())
		color_set(1, NULL);
	    if (do_stand) {
		(void) standend();
		do_stand = 0;
	    }
	}
	r++;
      }
    }

    /* place 'cursor marker' */
    if (showcell && (!showneed) && (!showexpr) && (!shownote)) {
	(void) move(lastmy, lastmx);
	pp = ATBL(tbl, currow, curcol);
	if (color && has_colors()) {
	    if ((cr = find_crange(currow, curcol)))
		color_set(cr->r_color, NULL);
	    else
		color_set(1, NULL);
	    if (*pp) {
		if (colorneg && (*pp)->flags & is_valid && (*pp)->v < 0) {
		    if (cr)
			color_set(((cr->r_color) % CPAIRS) + 1, NULL);
		    else
			color_set(2, NULL);
		} else if (colorerr && (*pp)->cellerror)
		    color_set(3, NULL);
	    }
	}
        repaint(lastmx, lastmy, fwidth[lastcol], A_STANDOUT, 0);
	if (color && has_colors())
	    color_set(1, NULL);
    }

    repaint(lastmx, RESROW - 1, fwidth[lastcol], 0, A_STANDOUT);
    repaint(0, lastmy, rescol - 1, 0, A_STANDOUT);

    (void) move(lastmy, lastmx+fwidth[lastcol]);
    under_cursor = (inch() & A_CHARTEXT);
    if (!showcell)
	(void) addch('<' | (inch() & A_ATTRIBUTES));

    (void) move(0, 0);
    (void) clrtoeol();

    if (linelim >= 0) {
	int ctlchars;

	for (i = ctlchars = 0; i < linelim; i++)
	    if ((unsigned char) line[i] < ' ')
		ctlchars++;
	(void) addch(mode_ind);
	(void) addch('>');
	(void) addch(search_ind);
	(void) addstr(line);
	if (!braille || (!message && mode_ind != 'v'))
	    (void) move((linelim+3+ctlchars)/cols, (linelim+3+ctlchars)%cols);
	else if (message)
	    move(1, 0);
	else if (braillealt)
	    move(1, 16);
	else
	    move(lastmy, lastmx);
    } else {
	if (showtop) {			/* show top line */
	    register struct ent *p1;
	    int printed = 0;		/* printed something? */

	    (void) printw("%s%d ", coltoa(curcol), currow);

	    if ((p1 = *ATBL(tbl, currow, curcol)) && p1->nrow > -1)
		printw("{*%s} ", r_name(p1->nrow, p1->ncol,
			p1->nlastrow, p1->nlastcol));

	    /* show the current cell's format */
	    if ((p1) && p1->format)
		printw("(%s) ", p1->format);
	    else
		printw("(%d %d %d) ", fwidth[curcol], precision[curcol],
				realfmt[curcol]);

	    if (p1) {
		if (p1->expr) {
		    /* has expr of some type */
		    linelim = 0;
		    editexp(currow, curcol);	/* set line to expr */
		    linelim = -1;
		}

		/*
		 * Display string part of cell:
		 */

		if ((p1->expr) && (p1->flags & is_strexpr)) {
 		    if (p1->flags & is_label)
			(void) addstr("|{");
		    else
			(void) addstr((p1->flags & is_leftflush) ? "<{" : ">{");
		    (void) addstr(line);
		    (void) addstr("} ");	/* and this '}' is for vi % */
		    printed = 1;

		} else if (p1->label) {
		    /* has constant label only */
		    if (p1->flags & is_label)
			(void) addstr("|\"");
		    else
			(void) addstr((p1->flags & is_leftflush) ? "<\"" : ">\"");
		    (void) addstr(p1->label);
		    (void) addstr("\" ");
		    printed = 1;
		}

		/*
		 * Display value part of cell:
		 */

		if (p1->flags & is_valid) {
		    /* has value or num expr */
		    if ((!(p1->expr)) || (p1->flags & is_strexpr))
			(void) sprintf(line, "%.15g", p1->v);

		    (void) addch('[');
		    (void) addstr(line);
		    (void) addch(']');
		    *line = '\0'; /* this is the input buffer ! */
		    printed = 1;
		}
	    }
	    if (!printed)
		(void) addstr("[]");
	    /* Display if cell is locked */
	    if (p1 && p1->flags&is_locked)
		(void) addstr(" locked");
	}
	if (braille)
	    if (message)
		move(1, 0);
	    else if (braillealt)
		move(0, 0);
	    else
		move(lastmy, lastmx);
	else if (showcell)
	    move(lines - 1, cols - 1);
	else
	    (void) move(lastmy, lastmx+fwidth[lastcol]);
    }

    if (color && has_colors())
	color_set(1, NULL);

    if (revmsg[0]) {
	(void) move(0, 0);
	(void) clrtoeol();	/* get rid of topline display */
	(void) printw(revmsg);
	*revmsg = '\0';		/* don't show it again */
	if (braille)
	    if (message)
		move(1, 0);
	    else if (braillealt)
		move(0, 0);
	    else
		move(lastmy, lastmx);
	else if (showcell)
	    move(lines - 1, cols - 1);
	else
	    (void) move(lastmy, lastmx+fwidth[lastcol]);
    }

    if (color && has_colors())
	color_set(1, NULL);

    if (revmsg[0]) {
	(void) move(0, 0);
	(void) clrtoeol();	/* get rid of topline display */
	(void) printw(revmsg);
	*revmsg = '\0';		/* don't show it again */
	if (braille)
	    if (message)
		move(1, 0);
	    else
		move(lastmy, lastmx);
	else if (showcell)
	    move(lines - 1, cols - 1);
	else
	    (void) move(lastmy, lastmx + fwidth[lastcol]);
    }

    FullUpdate = FALSE;
}

/* redraw what is under the cursor from curses' idea of the screen */
void
repaint(int x, int y, int len, int attron, int attroff)
{
    while (len-- > 0) {
	(void) move(y, x);
	addch((inch() | attron) & ~attroff);
	x++;
    }
}

int seenerr;

/* error routine for yacc (gram.y) */
void
yyerror(char *err)
{
    if (usecurses) {
	if (seenerr) return;
	seenerr++;
	(void) move(1, 0);
	(void) clrtoeol();
	(void) printw("%s: %.*s<=%s", err, linelim, line, line + linelim);
    } else
	(void) fprintf(stderr, "%s: %.*s<=%s\n", err, linelim, line,
		       line + linelim);
}

#ifdef XENIX2_3
struct termio tmio;
#endif

void
startdisp()
{
#if sun
    int	 fd;
    fd = dup(0);
#endif
#ifndef MSDOS
    if (usecurses) {
#endif
	int i;
#ifdef TIOCGSIZE
	{   struct ttysize size;
	    if (ioctl(0, TIOCGSIZE, &size) == 0) { 
		lines = size.ts_lines;
		cols = size.ts_cols;
	    }
	}
#endif

#ifdef XENIX2_3
	(void) ioctl(fileno(stdin), TCGETA, & tmio);
#endif
	(void) initscr();
	start_color();
	for (i = 0; i < 8; i++)
	    if (cpairs[i])
		init_pair(i + 1, cpairs[i]->fg, cpairs[i]->bg);
	if (color && has_colors())
	    bkgdset(COLOR_PAIR(1) | ' ');
#if sun
	close(0);
	dup(fd);
	close(fd);
#endif
	(void) clear();
#ifdef VMS
	VMS_read_raw = 1;
#else
	nonl();
	noecho();
	cbreak();
#endif
	initkbd();
	scrollok(stdscr, 1);

#if defined(SYSV3) && !defined(NOIDLOK)
# ifndef IDLOKBAD
	/*
	 * turn hardware insert/delete on, if possible.
	 * turn on scrolling for systems with SYSVr3.{1,2} (SYSVr3.0 has
	 * this set as the default)
	 */
	idlok(stdscr,TRUE);
# else	/*
	 * This seems to fix (with an empty spreadsheet):
	 *	a) Redrawing the bottom half of the screen when you
	 *		move between row 9 <-> 10
	 *	b) the highlighted row labels being trash when you
	 *		move between row 9 <-> 10
	 *	c) On an xterm on Esix Rev. D+ from eating lines
	 *	 -goto (or move) a few lines (or more) past the bottom
	 *	 of the screen, goto (or move) to the top line on the
	 *	 screen, move upward and the current line is deleted, the
	 *	 others move up even when they should not, check by
	 *	 noticing the rows become 2, 3, 40, 41, 42... (etc).
	 */
	idlok(stdscr,FALSE);
# endif
#endif

	FullUpdate++;
#ifndef MSDOS
    }
#endif
}

void
stopdisp()
{
#ifndef MSDOS
    if (usecurses) {
#endif
	deraw(1);
	resetkbd();
	endwin();
#ifdef XENIX2_3
	(void) ioctl(fileno(stdin), TCSETAW, & tmio);
#endif
#ifndef MSDOS
    }
#endif
}

/* init curses */
#ifdef VMS

void
goraw()
{
    if (usecurses) {
	VMS_read_raw = 1;
	if (color && has_colors())
	    bkgdset(COLOR_PAIR(1) | ' ');
	FullUpdate++;
    }
}

void
deraw(int ClearLastLine)
{
    if (usecurses) {
	if (ClearLastLine) {
	    if (color && has_colors())
		bkgdset(COLOR_PAIR(0) | ' ');
	    (void) move(lines - 1, 0);
	    (void) clrtoeol();
	    (void) refresh();
	}
	VMS_read_raw = 0;
    }
}

#else /* VMS */
void
goraw()
{
    if (usecurses) {
#if SYSV2 || SYSV3
	fixterm();
#else /* SYSV2 || SYSV3 */
	cbreak();
	nonl();
	noecho ();
#endif /* SYSV2 || SYSV3 */
	kbd_again();
	if (color && has_colors())
	    bkgdset(COLOR_PAIR(1) | ' ');
	FullUpdate++;
    }
}

/* clean up curses */
void
deraw(int ClearLastLine)
{
    if (usecurses) {
	if (ClearLastLine) {
	    if (color && has_colors())
		bkgdset(COLOR_PAIR(0) | ' ');
	    (void) move(lines - 1, 0);
	    (void) clrtoeol();
	    (void) refresh();
	}
#if SYSV2 || SYSV3
	resetterm();
#else
	nocbreak();
	nl();
	echo();
#endif
	resetkbd();
    }
}

#endif /* VMS */
