/*	SC	A Spreadsheet Calculator
 *		Command routines
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *
 *		$Revision: 7.13 $
 */

#include <sys/types.h>
#include <sys/wait.h>
#if defined(BSD42) || defined(BSD43)
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif

#include <curses.h>
#include <time.h>
#if defined(BSD42) || defined(BSD43) || defined(VMS)
#include <sys/file.h>
#else
#include <fcntl.h>
#endif
#include "sc.h"
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#ifndef MSDOS
#include <unistd.h>
#endif

void	syncref(register struct enode *e);
void	unspecial(FILE *f, char *str, int delim);

/* a linked list of free [struct ent]'s, uses .next as the pointer */
extern	struct ent *freeents;

/* a linked list of free [struct enodes]'s, uses .e.o.left as the pointer */
extern	struct enode *freeenodes;

#define DEFCOLDELIM ':'

extern	char *scext;
extern	char *ascext;
extern	char *tbl0ext;
extern	char *tblext;
extern	char *latexext;
extern	char *slatexext;
extern	char *texext;
extern	int  Vopt;
extern	struct go_save gs;
int macrofd;
int cslop;

/* copy the current row (currow) and place the cursor in the new row */
void
duprow()
{
    int c1, c2, coltmp = curcol;
    struct frange *fr;

    fr = find_frange(currow, curcol);
    c1 = fr ? fr->or_left->col : 0;
    c2 = fr ? fr->or_right->col : maxcol;

    if (currow >= maxrows - 1 || (!fr && maxrow >= maxrows - 1) ||
	    (fr && fr->or_right->row >= maxrows - 1)) {
	if (!growtbl(GROWROW, 0, 0))
		return;
    }
    modflg++;
    insertrow(1, 1);
    for (curcol = c1; curcol <= c2; curcol++) {
	register struct ent *p = *ATBL(tbl, currow - 1, curcol);
	if (p) {
	    register struct ent *n;
	    n = lookat(currow, curcol);
	    (void) copyent(n, p, 1, 0);
	}
    }
    for (curcol = c1; curcol <= c2; curcol++) {
	register struct ent *p = *ATBL(tbl, currow, curcol);
	if (p && (p->flags & is_valid) && !p->expr)
	    break;
    }
    curcol = coltmp;
}

/* copy the current column (curcol) and place the cursor in the new column */
void
dupcol() 
{
    int rowtmp = currow;

    if (curcol >= maxcols - 1 || maxcol >= maxcols - 1) {
	if (!growtbl(GROWCOL, 0, 0))
		return;
    }
    modflg++;
    insertcol(1, 1);
    for (currow = 0; currow <= maxrow; currow++) {
	register struct ent *p = *ATBL(tbl, currow, curcol - 1);
	if (p) {
	    register struct ent *n;
	    n = lookat(currow, curcol);
	    copyent(n, p, 0, 1);
	}
    }
    for (currow = 0; currow <= maxrow; currow++) {
	register struct ent *p = *ATBL(tbl, currow, curcol);
	if (p && (p -> flags & is_valid) && !p -> expr)
	    break;
    }
    currow = rowtmp;
}

/* Insert 'arg' rows.  The row(s) will be inserted before currow if delta
 * is 0; after if it is 1.
 */
void
insertrow(int arg, int delta)
{
    int	r, c, i;
    struct ent	**tmprow, **pp;
    int lim = maxrow - currow + 1;
    struct frange *fr;

    if (currow > maxrow)
	maxrow = currow;
    maxrow += arg;
    lim = maxrow - lim + delta;

    if ((maxrow >= maxrows) && !growtbl(GROWROW, maxrow, 0))
	return;

    if ((fr = find_frange(currow + delta, curcol))) {
	move_area(currow + arg + delta, fr->or_left->col, currow + delta,
		fr->or_left->col, fr->or_right->row, fr->or_right->col);
	flush_saved();
	if (!delta && fr->ir_left->row == currow + arg)
	    fr->ir_left = lookat(fr->ir_left->row - arg, fr->ir_left->col);
	if (delta && fr->ir_right->row == currow)
	    fr->ir_right = lookat(fr->ir_right->row + arg, fr->ir_right->col);

	for (i=0; i < 27; i++) {	/* update all marked cells */
	    if (savedrow[i] >= currow + delta &&
		    savedcol[i] >= fr->or_left->col &&
		    savedcol[i] <= fr->or_right->col)
		savedrow[i] += arg;
	    if (savedstrow[i] >= currow + delta &&
		    savedstcol[i] >= fr->or_left->col &&
		    savedstcol[i] <= fr->or_right->col)
		savedstrow[i] += arg;
	}
	if (gs.g_row >= currow + delta &&
		gs.g_col >= fr->or_left->col &&
		gs.g_col <= fr->or_right->col)
	    gs.g_row += arg;
	if (gs.g_lastrow >= currow + delta &&
		gs.g_lastcol >= fr->or_left->col &&
		gs.g_lastcol <= fr->or_right->col)
	    gs.g_lastrow += arg;
	if (gs.strow >= currow + delta &&
		gs.stcol >= fr->or_left->col &&
		gs.stcol <= fr->or_right->col)
	    gs.strow += arg;
    } else {
	
	/*
	 * save the last active row+1, shift the rows downward, put the last
	 * row in place of the first
	 */
	tmprow = tbl[maxrow];
	for (r = maxrow; r > lim; r--) {
	    row_hidden[r] = row_hidden[r-arg];
	    row_hidden[r-arg] = row_hidden[r-1];
	    tbl[r] = tbl[r-arg];
	    tbl[r-arg] = tbl[r-1];
	    pp = ATBL(tbl, r, 0);
	    for (c = 0; c < maxcols; c++, pp++)
		if (*pp)
		    (*pp)->row = r;
	}
	tbl[r] = tmprow;		/* the last row was never used.... */

	for (i=0; i < 27; i++) {	/* update all marked cells */
	    if (savedrow[i] >= currow + delta)
		savedrow[i] += arg;
	    if (savedstrow[i] >= currow + delta)
		savedstrow[i] += arg;
	}
	if (gs.g_row >= currow + delta)
	    gs.g_row += arg;
	if (gs.g_lastrow >= currow + delta)
	    gs.g_lastrow += arg;
	if (gs.strow >= currow + delta)
	    gs.strow += arg;
	for (r = 0; r <= maxrow; r++) {
	    for (c = 0; c <= maxcol; c++) {
		pp = ATBL(tbl, r, c);
		if (*pp) {
		    if ((*pp)->nrow >= currow + delta)
			((*pp)->nrow) += arg;
		    if ((*pp)->nlastrow >= currow + delta)
			((*pp)->nlastrow) += arg;
		}
	    }
	}
    }
    fix_ranges(currow + arg * (1 - delta), -1, currow + arg * (1 - delta), -1,
	    delta?0:arg, delta?arg:0);
    currow += delta;
    FullUpdate++;
    modflg++;
}

/* Insert 'arg' cols.  The col(s) will be inserted before curcol if delta
 * is 0; after if it is 1.
 */
void
insertcol(int arg, int delta)
{
    int r, c;
    register struct ent **pp;
    int lim = maxcol - curcol - delta + 1;
    struct frange *fr;

    if (curcol + delta > maxcol)
	maxcol = curcol + delta;
    maxcol += arg;

    if ((maxcol >= maxcols) && !growtbl(GROWCOL, 0, maxcol))
	return;

    for (c = maxcol; c >= curcol + delta + arg; c--) {
	fwidth[c] = fwidth[c-arg];
	precision[c] = precision[c-arg];
	realfmt[c] = realfmt[c-arg];
	col_hidden[c] = col_hidden[c-arg];
    }
    for (c = curcol + delta; c - curcol - delta < arg; c++) {
    	fwidth[c] = DEFWIDTH;
	precision[c] =  DEFPREC;
	realfmt[c] = DEFREFMT;
	col_hidden[c] = FALSE;
    }
	
    for (r=0; r <= maxrow; r++) {
	pp = ATBL(tbl, r, maxcol);
	for (c = lim; --c >= 0; pp--)
	    if ((pp[0] = pp[-arg]))
		pp[0]->col += arg;

	pp = ATBL(tbl, r, curcol + delta);
	for (c = curcol + delta; c - curcol - delta < arg; c++, pp++)
	    *pp = (struct ent *)0;
    }

    /* Update all marked cells. */
    for (c=0; c < 27; c++) {
	if (savedcol[c] >= curcol + delta)
	    savedcol[c] += arg;
	if (savedstcol[c] >= curcol + delta)
	    savedstcol[c] += arg;
    }
    if (gs.g_col >= curcol + delta)
	gs.g_col += arg;
    if (gs.g_lastcol >= curcol + delta)
	gs.g_lastcol += arg;
    if (gs.stcol >= curcol + delta)
	gs.stcol += arg;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
	for (c = 0; c <= maxcol; c++) {
	    pp = ATBL(tbl, r, c);
	    if (*pp) {
		if ((*pp)->ncol >= curcol + delta)
		    ((*pp)->ncol) += arg;
		if ((*pp)->nlastcol >= curcol + delta)
		    ((*pp)->nlastcol) += arg;
	    }
	}
    }
    if (!delta && (fr = find_frange(currow, curcol)) &&
	    fr->ir_left->col == curcol + arg)
	fr->ir_left = lookat(fr->ir_left->row, fr->ir_left->col - arg);
    if (delta && (fr = find_frange(currow, curcol)) &&
	    fr->ir_right->col == curcol)
	fr->ir_right = lookat(fr->ir_right->row, fr->ir_right->col + arg);
    fix_ranges(-1, curcol + arg * (1 - delta), -1, curcol + arg * (1 - delta),
	    delta?0:arg, delta?arg:0);
    curcol += delta;
    FullUpdate++;
    modflg++;
}

/* delete 'arg' rows starting at currow (deletes from currow downward) */
void
deleterow(register int arg)
{
    int rs = maxrow - currow + 1;
    int i;
    char buf[50];
    struct frange *fr;

    if ((fr = find_frange(currow, curcol)))
	rs = fr->or_right->row - currow + 1;
    if (rs - arg < 0) {
	rs = rs > 0 ? rs : 0;
	(void) sprintf(buf, "Can't delete %d row%s %d row%s left", arg,
		(arg != 1 ? "s," : ","), rs, (rs != 1 ? "s" : ""));
	error(buf);
	return;
    }
    if (fr) {
	if (any_locked_cells(currow, fr->or_left->col,
		currow + arg - 1, fr->or_right->col))
	    error("Locked cells encountered. Nothing changed");
	else {
	    FullUpdate++;
	    modflg++;
	    flush_saved();
	    sync_refs();
	    erase_area(currow, fr->or_left->col, currow + arg - 1,
		    fr->or_right->col);
	    fix_ranges(currow, -1, currow + arg - 1, -1, -1, -1);
	    if (currow + arg > fr->or_right->row)
		fr->or_right = lookat(currow - 1, fr->or_right->col);
	    else {
		move_area(currow, fr->or_left->col, currow + arg,
			fr->or_left->col, fr->or_right->row, fr->or_right->col);
		flush_saved();
	    }
	    if (currow + arg > fr->ir_right->row && fr->ir_right->row >= currow)
		fr->ir_right = lookat(currow - 1, fr->ir_right->col);

	    /* Update all marked cells. */
	    for (i=0; i < 27; i++) {
		if (savedcol[i] >= fr->or_left->col &&
			savedcol[i] <= fr->or_right->col) {
		    if (savedrow[i] >= currow && savedrow[i] < currow + arg)
			savedrow[i] = savedcol[i] = -1;
		    if (savedrow[i] >= currow + arg)
			savedrow[i] -= arg;
		}
		if (savedstcol[i] >= fr->or_left->col &&
			savedstcol[i] <= fr->or_right->col) {
		    if (savedstrow[i] >= currow && savedstrow[i] < currow + arg)
			savedstrow[i] = currow;
		    if (savedstrow[i] >= currow + arg)
			savedstrow[i] -= arg;
		}
	    }
	    if (gs.g_col >= fr->or_left->col &&
		    gs.g_col <= fr->or_right->col) {
		if (gs.g_row >= currow && gs.g_row < currow + arg)
		    gs.g_row = currow;
		if (gs.g_row >= currow + arg)
		    gs.g_row -= arg;
	    }
	    if (gs.g_lastcol >= fr->or_left->col &&
		    gs.g_lastcol <= fr->or_right->col) {
		if (gs.g_lastrow >= currow && gs.g_lastrow < currow + arg)
		    gs.g_lastrow = currow - 1;
		if (gs.g_lastrow >= currow + arg)
		    gs.g_lastrow -= arg;
	    }
	    if (gs.g_row > gs.g_lastrow)
		gs.g_row = gs.g_col = -1;
	    if (gs.stcol >= fr->or_left->col &&
		    gs.stcol <= fr->or_right->col) {
		if (gs.strow >= currow && gs.strow < currow + arg)
		    gs.strow = currow;
		if (gs.strow >= currow + arg)
		    gs.strow -= arg;
	    }
	}
    } else {
	
	if (any_locked_cells(currow, 0, currow + arg - 1, maxcol))
	    error("Locked cells encountered. Nothing changed");
	else {
	    flush_saved();
	    sync_refs();
	    erase_area(currow, 0, currow + arg - 1, maxcol);
	    fix_ranges(currow, -1, currow + arg - 1, -1, -1, -1);
	    closerow(currow, arg);
	}
    }
}

void
yankrow(int arg)
{
    int rs = maxrow - currow + 1;
    char buf[50];
    struct frange *fr;

    if ((fr = find_frange(currow, curcol)))
	rs = fr->or_right->row - currow + 1;
    if (rs - arg < 0) {
	rs = rs > 0 ? rs : 0;
	(void) sprintf(buf, "Can't yank %d row%s %d row%s left", arg,
		(arg != 1 ? "s," : ","), rs, (rs != 1 ? "s" : ""));
	error(buf);
	return;
    }
    if (fr) {
	sync_refs();
	flush_saved();
	yank_area(currow, fr->or_left->col, currow + arg - 1,
		fr->or_right->col);
    } else {
	sync_refs();
	flush_saved();
	yank_area(currow, 0, currow + arg - 1, maxcol);
    }
}

void
yankcol(int arg)
{
    int cs = maxcol - curcol + 1;
    char buf[50];

    if (cs - arg < 0) {
    	cs = cs > 0 ? cs : 0;
	(void) sprintf(buf, "Can't yank %d column%s %d column%s left", arg,
		(arg != 1 ? "s," : ","), cs, (cs != 1 ? "s" : ""));
	error(buf);
	return;
    }
    sync_refs();
    flush_saved();
    yank_area(0, curcol, maxrow, curcol + arg - 1);
}

void
erase_area(int sr, int sc, int er, int ec)
{
    int r, c;
    struct ent **pp;

    if (sr > er) {
	r = sr; sr = er; er = r;	
    }

    if (sc > ec) {
	c = sc; sc = ec; ec = c;	
    }

    if (sr < 0)
	sr = 0; 
    if (sc < 0)
	sc = 0;
    checkbounds(&er, &ec);

    /* Do a lookat() for the upper left and lower right cells of the range
     * being erased to make sure they are included in the delete buffer so
     * that pulling cells always works correctly even if the cells at one
     * or more edges of the range are all empty.
     */
    (void) lookat(sr, sc);
    (void) lookat(er, ec);

    dbidx++;
    for (r = sr; r <= er; r++) {
	for (c = sc; c <= ec; c++) {
	    pp = ATBL(tbl, r, c);
	    if (*pp && !((*pp)->flags&is_locked)) {
		free_ent(*pp);
		*pp = NULL;
	    }
	}
    }
}

void
yank_area(int sr, int sc, int er, int ec)
{
    int r, c;

    if (sr > er) {
	r = sr; sr = er; er = r;	
    }

    if (sc > ec) {
	c = sc; sc = ec; ec = c;	
    }

    if (sr < 0)
	sr = 0; 
    if (sc < 0)
	sc = 0;
    checkbounds(&er, &ec);

    r = currow;
    currow = sr;
    c = curcol;
    curcol = sc;

    erase_area(sr, sc, er, ec);
    pullcells('m');

    currow = r;
    curcol = c;
}

void
move_area(int dr, int dc, int sr, int sc, int er, int ec)
{
    struct ent *p;
    struct ent **pp;
    int deltar, deltac;
    int r, c;

    if (sr > er) {
	r = sr; sr = er; er = r;	
    }

    if (sc > ec) {
	c = sc; sc = ec; ec = c;	
    }

    if (sr < 0)
	sr = 0; 
    if (sc < 0)
	sc = 0;
    checkbounds(&er, &ec);

    r = currow;
    currow = sr;
    c = curcol;
    curcol = sc;

    /* First we erase the source range, which puts the cells on the delete
     * buffer stack.  The pullcells(0) then makes a copy in the original
     * location, which we then erase, putting the copy on the delete buffer
     * stack, too.
     */
    erase_area(sr, sc, er, ec);
    pullcells(0);
    erase_area(sr, sc, er, ec);

    currow = r;
    curcol = c;
    deltar = dr - sr;
    deltac = dc - sc;

    /* Now we erase the destination range, which adds it to the delete buffer
     * stack, but then we flush it off.  We then move the original source
     * range from the stack to the destination range, adjusting the addresses
     * as we go, which leaves only the copy on the stack for future pulls.
     */
    erase_area(dr, dc, er + deltar, ec + deltac);
    flush_saved();
    for (p = delbuf[dbidx - 1]; p; p = p->next) {
	pp = ATBL(tbl, p->row + deltar, p->col + deltac);
	*pp = p;
	p->row += deltar;
	p->col += deltac;
	p->flags &= ~is_deleted;
    }

    delbuf[dbidx - 1] = delbuf[dbidx];
    delbuf[dbidx--] = NULL;
}

/*
 * deletes the expression associated w/ a cell and turns it into a constant
 * containing whatever was on the screen
 */
void
valueize_area(int sr, int sc, int er, int ec)
{
    int r, c;
    struct ent *p;

    if (sr > er) {
	r = sr; sr = er; er= r;	
    }

    if (sc > ec) {
	c = sc; sc = ec; ec= c;	
    }

    if (sr < 0)
	sr = 0; 
    if (sc < 0)
	sc = 0;
    checkbounds(&er, &ec);

    for (r = sr; r <= er; r++) {
	for (c = sc; c <= ec; c++) {
	    p = *ATBL(tbl, r, c);
	    if (p && p->flags&is_locked) {
		error(" Cell %s%d is locked", coltoa(c), r);
		continue;
	    }
	    if (p && p->expr) {
		efree(p->expr);
		p->expr = (struct enode *)0;
		p->flags &= ~is_strexpr;
	    }
	}
    }
}

void
pullcells(int to_insert)
{
    struct ent *p, *n;
    struct ent **pp;
    int deltar, deltac;
    int minrow, mincol;
    int mxrow, mxcol;
    int numrows, numcols;
    struct frange *fr;

    if (dbidx < 0) {
	error("No data to pull");
	return;
    }

    minrow = maxrows; 
    mincol = maxcols;
    mxrow = 0;
    mxcol = 0;

    for (p = delbuf[dbidx]; p; p = p->next) {
	if (p->row < minrow)
	    minrow = p->row;
	if (p->row > mxrow)
	    mxrow = p->row;
	if (p->col < mincol)
	    mincol = p->col;
	if (p->col > mxcol)
	    mxcol = p->col;
    }

    numrows = mxrow - minrow + 1;
    numcols = mxcol - mincol + 1;
    deltar = currow - minrow;
    deltac = curcol - mincol;
    

    if (to_insert == 'r') {
	insertrow(numrows, 0);
	if (fr = find_frange(currow, curcol))
	    deltac = fr->or_left->col - mincol;
	else
	    deltac = 0;
    } else if (to_insert == 'c') {
	insertcol(numcols, 0);
	deltar = 0;
    } else if (to_insert == 'x') {	/* Do an exchange. */
	struct ent *tmp;

	/* Save the original contents of the destination range in preparation
	 * for the exchange.
	 */
	erase_area(minrow + deltar, mincol + deltac, mxrow + deltar,
		mxcol + deltac);
	tmp = delbuf[dbidx];
	delbuf[dbidx] = delbuf[dbidx - 1];
	delbuf[dbidx - 1] = tmp;
    }

    FullUpdate++;
    modflg++;

    /* First copy the cells from the delete buffer into the destination
     * range.
     */
    for (p = delbuf[dbidx]; p; p = p->next) {
	if (to_insert == 't')
	    /* Transpose rows and columns while pulling. */
	    n = lookat(minrow + deltar + p->col - mincol,
		    mincol + deltac + p->row - minrow);
	else
	    n = lookat(p->row + deltar, p->col + deltac);
	(void) clearent(n);
	copyent(n, p, 0, 0);
	n->flags = p->flags & ~is_deleted;
    }

    /* Now exchange them so that the original cells from the delete buffer
     * are in the destination range instead of the copies.  When doing a
     * "pull exchange" ("px" or "pullxchg"), exchange the original contents
     * of the destination range with the contents of the delete buffer
     * instead.  Don't do this if called from move_area() or when transposing.
     */
    if (to_insert && to_insert != 't') {
	if (to_insert == 'x') {
	    struct ent *tmp = delbuf[dbidx];
	    delbuf[dbidx] = delbuf[dbidx - 1];
	    delbuf[dbidx - 1] = tmp;
	} else
	    erase_area(minrow + deltar, mincol + deltac, mxrow + deltar,
		    mxcol + deltac);
	for (p = delbuf[dbidx - 1]; p; p = p->next) {
	    pp = ATBL(tbl, p->row + deltar, p->col + deltac);
	    *pp = p;
	    p->row += deltar;
	    p->col += deltac;
	    p->flags &= ~is_deleted;
	}
	delbuf[dbidx - 1] = delbuf[dbidx];
	delbuf[dbidx--] = NULL;

	sync_refs();
	/*
	 * Now change the cell addresses in the delete buffer to match
	 * where the original cells came from.
	 */
	for (p = delbuf[dbidx]; p; p = p->next) {
	    p->row -= deltar;
	    p->col -= deltac;
	}
    }
}

void
colshow_op()
{
    register int i,j;
    for (i=0; i<maxcols; i++)
	if (col_hidden[i]) 
	    break;
    for(j=i; j<maxcols; j++)
	if (!col_hidden[j])
	    break;
    j--;
    if (i>=maxcols)
	error("No hidden columns to show");
    else {
	(void) sprintf(line,"show %s:", coltoa(i));
	(void) sprintf(line + strlen(line),"%s",coltoa(j));
	linelim = strlen(line);
    }
}

void
rowshow_op()
{
    register int i,j;
    for (i=0; i<maxrows; i++)
	if (row_hidden[i]) 
	    break;
    for(j=i; j<maxrows; j++)
	if (!row_hidden[j]) {
	    break;
	}
    j--;

    if (i>=maxrows)
	error("No hidden rows to show");
    else {
	(void)sprintf(line,"show %d:%d", i, j);
        linelim = strlen(line);
    }
}

/*
 * Given a row/column command letter, emit a small menu, then read a qualifier
 * character for a row/column command and convert it to 'r' (row), 'c'
 * (column), or 0 (unknown).  If ch is 'p', three extra qualifiers, 'm', 'x',
 * and 't', are allowed.  If ch is 'Z', an extra qualifier 'Z' is allowed.
 */

#define SHOWROWS 2
#define SHOWCOLS 3

int
get_rcqual(int ch)
{
    int c;

    error("%sow/column:  r: row  c: column%s",

#ifdef KEY_IC
	    (ch == KEY_IC)	? "Insert r" :
#endif
	    (ch == 'i')		? "Insert r" :
	    (ch == 'o')		? "Open r" :
	    (ch == 'a')		? "Append r" :
	    (ch == 'd')		? "Delete r" :
	    (ch == 'y')		? "Yank r" :
	    (ch == 'p')		? "Pull r" :
	    (ch == 'v')		? "Values r" :
	    (ch == 'Z')		? "Zap r" :
	    (ch == 's')		? "Show r" : "R",

	    (ch == 'p') ? "  m: merge  x: exchange  t: transpose" : "");

    (void) refresh();

    switch (c = nmgetch())
    {
	case 'r':	return ('r');

	case 'c':	return ('c');

	case 'm':	return ((ch == 'p') ? 'm' : 0);

	case 'x':	return ((ch == 'p') ? 'x' : 0);

	case 't':	return ((ch == 'p') ? 't' : 0);

	case 'Z':	return ((ch == 'Z') ? 'Z' : 0);

	case ESC:
	case ctl('g'):	return (ESC);

	case 'd':	if (ch == 'd') {
			    ungetch('x');
			    return (ESC);
			} else
			    return (0);

	case 'y':	if (ch == 'y') {
			    flush_saved();
			    yank_area(currow, curcol, currow, curcol);
			    return (ESC);
			} else
			    return (0);

	case 'v':	if (ch == 'v') {
			    valueize_area(currow, curcol, currow, curcol);
			    return (ESC);
			} else
			    return (0);

	case KEY_UP:
	case KEY_DOWN:
	case KEY_PPAGE:
	case KEY_NPAGE:
	case 'j':
	case 'k':
	case 'J':
	case 'K':
	case ctl('f'):
	case ctl('b'):
	case ctl('n'):
	case ctl('p'):	if (ch == 'd')
			    (void) sprintf(line,"deleterow [range] ");
			else if (ch == 'y')
			    (void) sprintf(line,"yankrow [range] ");
			else
			    return (0);
			edit_mode();
			write_line('A');
			startshow();
			showrange = SHOWROWS;
			showsr = currow;
			ungetch(c);
			return (ESC);

	case KEY_BACKSPACE:
	case KEY_LEFT:
	case KEY_RIGHT:
	case ' ':
	case 'h':
	case 'l':
	case 'H':
	case 'L':	if (ch == 'd')
			    (void) sprintf(line,"deletecol [range] ");
			else if (ch == 'y')
			    (void) sprintf(line,"yankcol [range] ");
			else
			    return (0);
			edit_mode();
			write_line('A');
			startshow();
			showrange = SHOWCOLS;
			showsc = curcol;
			ungetch(c);
			return (ESC);

	default:	return (0);
    }
    /*NOTREACHED*/
}

/* delete numrow rows, starting with rs */
void
closerow(int rs, int numrow)
{
    register struct ent **pp;
    int			r, c, i;
    struct ent		**tmprow;

    if (rs + numrow - 1 > maxrow) return;
    r = rs;

/*
 * Rows are dealt with in numrow groups, each group of rows spaced numrow
 * rows apart.
 */
    for (i = 0; i < numrow; i++) {
	r = rs + i;

	/* save the first row of the group and empty it out */
	tmprow = tbl[r];
	pp = ATBL(tbl, r, 0);
	for (c = maxcol + 1; --c >= 0; pp++) {
	    if (*pp) {
		free_ent(*pp);
		*pp = (struct ent *)0;
	    }
	}

	/* move the rows, put the deleted, but now empty, row at the end */
	for (; r + numrow < maxrows - 1; r += numrow) {
	    row_hidden[r] = row_hidden[r + numrow];
	    tbl[r] = tbl[r + numrow];
	    pp = ATBL(tbl, r, 0);
	    for (c = 0; c < maxcols; c++, pp++)
		if (*pp)
		    (*pp)->row = r;
	}
	tbl[r] = tmprow;
    }

    /* Update all marked cells. */
    for (i=0; i < 27; i++) {
	if (savedrow[i] >= rs && savedrow[i] < rs + numrow)
	    savedrow[i] = savedcol[i] = -1;
	if (savedrow[i] >= rs + numrow)
	    savedrow[i] -= numrow;
	if (savedstrow[i] >= rs && savedstrow[i] < rs + numrow)
	    savedstrow[i] = rs;
	if (savedstrow[i] >= rs + numrow)
	    savedstrow[i] -= numrow;
    }
    if (gs.g_row >= rs && gs.g_row < rs + numrow)
	gs.g_row = rs;
    if (gs.g_row >= rs + numrow)
	gs.g_row -= numrow;
    if (gs.g_lastrow >= rs && gs.g_lastrow < rs + numrow)
	gs.g_lastrow = rs - 1;
    if (gs.g_lastrow >= rs + numrow)
	gs.g_lastrow -= numrow;
    if (gs.g_row > gs.g_lastrow)
	gs.g_row = gs.g_col = -1;
    if (gs.strow >= rs && gs.strow < rs + numrow)
	gs.strow = rs;
    if (gs.strow >= rs + numrow)
	gs.strow -= numrow;

    maxrow -= numrow;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
	for (c = 0; c <= maxcol; c++) {
	    pp = ATBL(tbl, r, c);
	    if (*pp) {
		if ((*pp)->nrow >= rs && (*pp)->nrow < rs + numrow)
		    (*pp)->nrow = rs;
		if ((*pp)->nrow >= rs + numrow)
		    ((*pp)->nrow) -= numrow;
		if ((*pp)->nlastrow >= rs && (*pp)->nlastrow < rs + numrow)
		    (*pp)->nlastrow = rs - 1;
		if ((*pp)->nlastrow >= rs + numrow)
		    ((*pp)->nlastrow) -= numrow;
		if ((*pp)->nlastrow < (*pp)->nrow)
		    (*pp)->nrow = (*pp)->ncol = -1;
	    }
	}
    }
    FullUpdate++;
    modflg++;
}

/* delete group of columns (1 or more) */
void
closecol(int arg)
{
    int r, c, i;
    register struct ent **pp;
    register struct ent *q;
    int cs = maxcol - curcol + 1;
    char buf[50];

    if (cs - arg < 0) {
    	cs = cs > 0 ? cs : 0;
	(void) sprintf(buf, "Can't delete %d column%s %d column%s left", arg,
		(arg != 1 ? "s," : ","), cs, (cs != 1 ? "s" : ""));
	error(buf);
	return;
    }
    if (any_locked_cells(0, curcol, maxrow, curcol + arg - 1)) {
	error("Locked cells encountered. Nothing changed");
	return;
    }
    flush_saved();
    sync_refs();
    erase_area(0, curcol, maxrow, curcol + arg - 1);
    fix_ranges(-1, curcol, -1, curcol + arg - 1, -1, -1);

    /* clear then copy the block left */
    cs = maxcols - arg - 1;
    for (r=0; r<=maxrow; r++) {
	for (c = curcol; c - curcol < arg; c++)
	    if ((q = *ATBL(tbl, r, c)))
		free_ent(q);

	pp = ATBL(tbl, r, curcol);
	for (c=curcol; c <= cs; c++, pp++) {
	    if (c > cs)
		*pp = (struct ent *)0;
	    else if ((pp[0] = pp[arg]))
		pp[0]->col -= arg;
	}

	c = arg;
	for (; --c >= 0; pp++)		
	    *pp = (struct ent *)0;
    }

    for (i = curcol; i < maxcols - arg - 1; i++) {
	fwidth[i] = fwidth[i+arg];
	precision[i] = precision[i+arg];
	realfmt[i] = realfmt[i+arg];
	col_hidden[i] = col_hidden[i+arg];
    }
    for (; i < maxcols - 1; i++) {
	fwidth[i] = DEFWIDTH;
	precision[i] = DEFPREC;
	realfmt[i] = DEFREFMT;
	col_hidden[i] = FALSE;
    }

    /* Update all marked cells. */
    for (i=0; i < 27; i++) {
	if (savedcol[i] >= curcol && savedcol[i] < curcol + arg)
	    savedrow[i] = savedcol[i] = -1;
	if (savedcol[i] >= curcol + arg)
	    savedcol[i] -= arg;
	if (savedstcol[i] >= curcol && savedstcol[i] < curcol + arg)
	    savedstcol[i] = curcol;
	if (savedstcol[i] >= curcol + arg)
	    savedstcol[i] -= arg;
    }
    if (gs.g_col >= curcol && gs.g_col < curcol + arg)
	gs.g_col = curcol;
    if (gs.g_col >= curcol + arg)
	gs.g_col -= arg;
    if (gs.g_lastcol >= curcol && gs.g_lastcol < curcol + arg)
	gs.g_lastcol = curcol - 1;
    if (gs.g_lastcol >= curcol + arg)
	gs.g_lastcol -= arg;
    if (gs.g_col > gs.g_lastcol)
	gs.g_row = gs.g_col = -1;
    if (gs.stcol >= curcol && gs.stcol < curcol + arg)
	gs.stcol = curcol;
    if (gs.stcol >= curcol + arg)
	gs.stcol -= arg;

    maxcol -= arg;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
	for (c = 0; c <= maxcol; c++) {
	    pp = ATBL(tbl, r, c);
	    if (*pp) {
		if ((*pp)->ncol >= curcol && (*pp)->ncol < curcol + arg)
		    (*pp)->ncol = curcol;
		if ((*pp)->ncol >= curcol + arg)
		    ((*pp)->ncol) -= arg;
		if ((*pp)->nlastcol >= curcol && (*pp)->nlastcol < curcol + arg)
		    (*pp)->nlastcol = curcol - 1;
		if ((*pp)->nlastcol >= curcol + arg)
		    ((*pp)->nlastcol) -= arg;
		if ((*pp)->nlastcol < (*pp)->ncol)
		    (*pp)->nrow = (*pp)->ncol = -1;
	    }
	}
    }
    FullUpdate++;
    modflg++;
}

void
doend(int rowinc, int colinc)
{
    register struct ent *p;
    int r, c;

    /* Remember the current position */
    savedrow[0] = currow;
    savedcol[0] = curcol;
    savedstrow[0] = strow;
    savedstcol[0] = stcol;

    if (VALID_CELL(p, currow, curcol)) {
	r = currow + rowinc;
	c = curcol + colinc;
	if (r >= 0 && r < maxrows && 
	    c >= 0 && c < maxcols &&
	    !VALID_CELL(p, r, c)) {
		currow = r;
		curcol = c;
	}
    }

    if (!VALID_CELL(p, currow, curcol)) {
        switch (rowinc) {
        case -1:
	    while (!VALID_CELL(p, currow, curcol) && currow > 0)
		currow--;
	    break;
        case  1:
	    while (!VALID_CELL(p, currow, curcol) && currow < maxrows-1)
		currow++;
	    break;
        case  0:
            switch (colinc) {
 	    case -1:
	        while (!VALID_CELL(p, currow, curcol) && curcol > 0)
		    curcol--;
	        break;
 	    case  1:
	        while (!VALID_CELL(p, currow, curcol) && curcol < maxcols-1)
		    curcol++;
	        break;
	    }
            break;
        }
	rowsinrange = 1;
	colsinrange = fwidth[curcol];

	error ("");	/* clear line */
	return;
    }

    switch (rowinc) {
    case -1:
	while (VALID_CELL(p, currow, curcol) && currow > 0)
	    currow--;
	break;
    case  1:
	while (VALID_CELL(p, currow, curcol) && currow < maxrows-1)
	    currow++;
	break;
    case  0:
	switch (colinc) {
	case -1:
	    while (VALID_CELL(p, currow, curcol) && curcol > 0)
		curcol--;
	    break;
	case  1:
	    while (VALID_CELL(p, currow, curcol) && curcol < maxcols-1)
		curcol++;
	    break;
	}
	break;
    }
    if (!VALID_CELL(p, currow, curcol)) {
        currow -= rowinc;
        curcol -= colinc;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* Modified 9/17/90 THA to handle more formats */
void
doformat(int c1, int c2, int w, int p, int r)
{
    register int i;
    int crows = 0;
    int ccols = c2;

    if (c1 >= maxcols && !growtbl(GROWCOL, 0, c1)) c1 = maxcols-1 ;
    if (c2 >= maxcols && !growtbl(GROWCOL, 0, c2)) c2 = maxcols-1 ;

    if (usecurses && w > COLS - rescol - 2) {
	error("Format too large - Maximum = %d", COLS - rescol - 2);
	w = COLS - rescol - 2;
    }

    if (p > w) {
	error("Precision too large");
	p = w;
    }

    checkbounds(&crows, &ccols);
    if (ccols < c2) {
	error("Format statement failed to create implied column %d", c2);
	return;
    }

    for (i = c1; i <= c2; i++)
	fwidth[i] = w, precision[i] = p, realfmt[i] = r;

    rowsinrange = 1;
    colsinrange = fwidth[curcol];

    FullUpdate++;
    modflg++;
}

void
print_options(FILE *f)
{
    if (
	autocalc &&
	!autoinsert &&
	!cslop &&
	!optimize &&
	!rndtoeven &&
	propagation == 10 &&
	calc_order == BYROWS &&
	!numeric &&
	prescale == 1.0 &&
	!extfunc &&
	showcell &&
	showtop &&
	tbl_style == 0 &&
	craction == 0 &&
	pagesize == 0 &&
	rowlimit == -1 &&
	collimit == -1 &&
	!color &&
	!colorneg &&
	!colorerr
       )
	return;		/* No reason to do this */

    (void) fprintf(f, "set");
    if (!autocalc) 
	(void) fprintf(f," !autocalc");
    if (autoinsert) 
	(void) fprintf(f," autoinsert");
    if (cslop)
	(void) fprintf(f," cslop");
    if (optimize) 
	(void) fprintf(f," optimize");
    if (rndtoeven)
	(void) fprintf(f, " rndtoeven");
    if (propagation != 10)
	(void) fprintf(f, " iterations = %d", propagation);
    if (calc_order != BYROWS )
	(void) fprintf(f, " bycols");
    if (numeric)
	(void) fprintf(f, " numeric");
    if (prescale != 1.0)
	(void) fprintf(f, " prescale");
    if (extfunc)
	(void) fprintf(f, " extfun");
    if (!showcell)
	(void) fprintf(f, " !cellcur");
    if (!showtop)
	(void) fprintf(f, " !toprow");
    if (tbl_style)
	(void) fprintf(f, " tblstyle = %s", tbl_style == TBL ? "tbl" :
					tbl_style == LATEX ? "latex" :
					tbl_style == SLATEX ? "slatex" :
					tbl_style == TEX ? "tex" :
					tbl_style == FRAME ? "frame" : "0" );
    if (craction)
	(void) fprintf(f, " craction = %d", craction);
    if (pagesize)
	(void) fprintf(f, " pagesize = %d", pagesize);
    if (rowlimit >= 0)
	(void) fprintf(f, " rowlimit = %d", rowlimit);
    if (collimit >= 0)
	(void) fprintf(f, " collimit = %d", collimit);
    if (color) 
	(void) fprintf(f," color");
    if (colorneg) 
	(void) fprintf(f," colorneg");
    if (colorerr) 
	(void) fprintf(f," colorerr");
    (void) fprintf(f, "\n");
}

void
printfile(char *fname, int r0, int c0, int rn, int cn)
{
    FILE *f;
    static char *pline = NULL;		/* only malloc once, malloc is slow */
    static unsigned fbufs_allocated = 0;
    int plinelim;
    int pid;
    int fieldlen, nextcol;
    long namelen;
    int row, col;
    register struct ent **pp;
    char file[256];
    char path[1024];
    char *tpp;

    /* printfile will be the [path/]file ---> [path/]file.out */
    if (*fname == '\0') {
	strcpy(path, curfile);

#ifdef MSDOS
	namelen = 12;
	if ((tpp = strrchr(path, '\\')) == NULL) {
#else
	if ((tpp = strrchr(path, '/')) == NULL) {
	    namelen = pathconf(".", _PC_NAME_MAX);
#endif
	    tpp = path;
	} else {
#ifndef MSDOS
	    *tpp = '\0';
	    namelen = pathconf(path, _PC_NAME_MAX);
	    *tpp = '/';
#endif
	    tpp++;
	}
	strcpy(file, tpp);

	if (!strcmp(file + strlen(file) - 3, ".sc"))
	    file[strlen(file) - 3] = '\0';
	else if (scext != NULL && file[strlen(file) - strlen(scext) - 1] == '.'
		&& !strcmp(file + strlen(file) - strlen(scext), scext))
	    file[strlen(file) - strlen(scext)] = '\0';

	if (ascext == NULL)
	    file[namelen - 4] = '\0';
	else
	    file[namelen - strlen(ascext) - 1] = '\0';
	sprintf(tpp, "%s.%s", file, ascext == NULL ? "asc" : ascext);
	fname = path;
    }

    if ((strcmp(fname, curfile) == 0) &&
	!yn_ask("Confirm that you want to destroy the data base: (y,n)")) {
	return;
    }

    if (!pline && (pline = scxmalloc((unsigned)(FBUFLEN *
	    ++fbufs_allocated))) == (char *)NULL) {
	error("Malloc failed in printfile()");
        return;
    }

    if ((f = openfile(fname, &pid, NULL)) == (FILE *)0) {
	error("Can't create file \"%s\"", fname);
	return;
    }
    for (row = r0; row <= rn; row++) {
	int c = 0;

	if (row_hidden[row])
	    continue;

	pline[plinelim=0] = '\0';
	for (pp = ATBL(tbl, row, col=c0); col<=cn;
	        pp += nextcol-col, col = nextcol, c += fieldlen) {

	    nextcol = col+1;
	    if (col_hidden[col]) {
		fieldlen = 0;
		continue;
	    }

	    fieldlen = fwidth[col];
	    if (*pp) {
		char *s;

		/* 
		 * dynamically allocate pline, making sure we are not 
		 * attempting to write 'out of bounds'.
		 */
		while (c > (fbufs_allocated * FBUFLEN)) {
		    if ((pline = scxrealloc ((char *)pline, 
			    (unsigned)(FBUFLEN * ++fbufs_allocated))) == NULL) {
			error ("Realloc failed in printfile()");
			return;
		    }
		}		  
		while (plinelim<c) pline[plinelim++] = ' ';
		plinelim = c;
		if ((*pp)->flags&is_valid) {
		    while(plinelim + fwidth[col] > 
			  (fbufs_allocated * FBUFLEN)) {
		      if((pline = ((char *)scxrealloc
				   ((char *)pline, 
				    (unsigned)(FBUFLEN * ++fbufs_allocated))))
			 == NULL) {
			error("Realloc failed in printfile()");
			return;
		      }
		    }
		    if ((*pp)->cellerror)
			(void) sprintf(pline+plinelim, "%*s",
				fwidth[col], ((*pp)->cellerror == CELLERROR ?
				"ERROR " : "INVALID "));
		    else {
		      char *cfmt;

		      cfmt = (*pp)->format ? (*pp)->format :
			    (realfmt[col] >= 0 && realfmt[col] < COLFORMATS &&
			    colformat[realfmt[col]]) ?
			    colformat[realfmt[col]] : 0;
		      if (cfmt) {
	   	        char field[FBUFLEN];

			if (*cfmt == ctl('d')) {
			    time_t v = (time_t) ((*pp)->v);
			    strftime(field, sizeof(field), cfmt + 1,
				    localtime(&v));
			    sprintf(pline+plinelim, "%-*s", fwidth[col],
				    field);
			} else {
			    format(cfmt, precision[col], (*pp)->v, field,
				    sizeof(field));
			    (void) sprintf(pline+plinelim, "%*s", fwidth[col],
				    field);
			}
		      } else {
	   	        char field[FBUFLEN];
			(void) engformat(realfmt[col], fwidth[col],
                                             precision[col], (*pp) -> v,
                                             field, sizeof(field));
			(void) sprintf(pline+plinelim, "%*s", fwidth[col],
				       field);
		      }
		    }
		    plinelim += strlen(pline+plinelim);
		}
		if ((s = (*pp)->label)) {
		    int slen;
		    char *start, *last;
		    register char *fp;
		    struct ent *nc;

		    /*
		     * Figure out if the label slops over to a blank field.
		     * A string started with backslash is defining repetition
		     * char
		     */
		    slen = strlen(s);
		    if (*s == '\\' && *(s+1) != '\0')
			slen = fwidth[col];
		    while (slen > fieldlen && nextcol <= cn &&
			    !((nc = lookat(row,nextcol))->flags & is_valid) &&
			    !(nc->label)) {
			
	                if (!col_hidden[nextcol])
		 	    fieldlen += fwidth[nextcol];

			nextcol++;
		    }
		    if (slen > fieldlen)
			slen = fieldlen;
		    
		    while(c + fieldlen + 2 > (fbufs_allocated * FBUFLEN)) {
		      if((pline = ((char *)scxrealloc
				   ((char *)pline, 
				    (unsigned)(FBUFLEN * ++fbufs_allocated))))
			 == NULL) {
			error ("scxrealloc failed in printfile()");
			return;
		      }
		    }		  

		    /* Now justify and print */
		    start = (*pp)->flags & is_leftflush ? pline + c
					: pline + c + fieldlen - slen;
		    if( (*pp)->flags & is_label )
			start = pline + (c + ((fwidth[col]>slen)?(fwidth[col]-slen)/2:0));
		    last = pline + c + fieldlen;
		    fp = plinelim < c ? pline + plinelim : pline + c;
		    while (fp < start)
			*fp++ = ' ';
		    if( *s == '\\' && *(s+1)!= '\0' ) {
			char *strt;
			strt = ++s;

			while(slen--) {
				*fp++ = *s++; if( *s == '\0' ) s = strt;
			}
		    }
		    else
		    while (slen--)
			*fp++ = *s++;

		    if (!((*pp)->flags & is_valid) || fieldlen != fwidth[col])
			while(fp < last)
			    *fp++ = ' ';
		    if (plinelim < fp - pline)
			plinelim = fp - pline;
		}
	    }
	}
	pline[plinelim++] = '\n';
	pline[plinelim] = '\0';
	(void) fputs (pline, f);
    }

    closefile(f, pid, 0);
}

void
tblprintfile(char *fname, int r0, int c0, int rn, int cn)
{
    FILE *f;
    int pid;
    long namelen;
    int row, col;
    register struct ent **pp;
    char coldelim = DEFCOLDELIM;
    char file[256];
    char path[1024];
    char *tpp;

    /* tblprintfile will be the [path/]file ---> [path/]file.out */
    if (*fname == '\0') {
	strcpy(path, curfile);

#ifdef MSDOS
	namelen = 12;
	if ((tpp = strrchr(path, '\\')) == NULL) {
#else
	if ((tpp = strrchr(path, '/')) == NULL) {
	    namelen = pathconf(".", _PC_NAME_MAX);
#endif
	    tpp = path;
	} else {
#ifndef MSDOS
	    *tpp = '\0';
	    namelen = pathconf(path, _PC_NAME_MAX);
	    *tpp = '/';
#endif
	    tpp++;
	}
	strcpy(file, tpp);

	if (!strcmp(file + strlen(file) - 3, ".sc"))
	    file[strlen(file) - 3] = '\0';
	else if (scext != NULL && file[strlen(file) - strlen(scext) - 1] == '.'
		&& !strcmp(file + strlen(file) - strlen(scext), scext))
	    file[strlen(file) - strlen(scext)] = '\0';

	if (tbl_style == 0) {
	    if (tbl0ext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(tbl0ext) - 1] = '\0';
	    sprintf(tpp, "%s.%s", file, tbl0ext == NULL ? "cln" : tbl0ext);
	}
	else if (tbl_style == TBL) {
	    if (tblext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(tblext) - 1] = '\0';
	    sprintf(tpp, "%s.%s", file, tblext == NULL ? "tbl" : tblext);
	}
	else if (tbl_style == LATEX) {
	    if (latexext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(latexext) - 1] = '\0';
	    sprintf(tpp, "%s.%s", file, latexext == NULL ? "lat" : latexext);
	}
	else if (tbl_style == SLATEX) {
	    if (slatexext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(slatexext) - 1] = '\0';
	    sprintf(tpp, "%s.%s", file, slatexext == NULL ? "stx" : slatexext);
	}
	else if (tbl_style == TEX) {
	    if (texext == NULL)
		file[namelen - 4] = '\0';
	    else
		file[namelen - strlen(texext) - 1] = '\0';
	    sprintf(tpp, "%s.%s", file, texext == NULL ? "tex" : texext);
	}
	fname = path;
    }

    if ((strcmp(fname, curfile) == 0) &&
	!yn_ask("Confirm that you want to destroy the data base: (y,n)"))
	    return;

    if ((f = openfile(fname, &pid, NULL)) == (FILE *)0) {
	error ("Can't create file \"%s\"", fname);
	return;
    }

    if ( tbl_style == TBL ) {
	fprintf(f,".\\\" ** %s spreadsheet output \n.TS\n",progname);
	fprintf(f,"tab(%c);\n",coldelim);
	for (col=c0;col<=cn; col++) fprintf(f," n");
	fprintf(f, ".\n");
    }
    else if ( tbl_style == LATEX ) {
	fprintf(f,"%% ** %s spreadsheet output\n\\begin{tabular}{",progname);
	for (col=c0;col<=cn; col++) fprintf(f,"c");
	fprintf(f, "}\n");
	coldelim = '&';
    }
    else if ( tbl_style == SLATEX ) {
	fprintf(f,"%% ** %s spreadsheet output\n!begin<tabular><",progname);
	for (col=c0;col<=cn; col++) fprintf(f,"c");
	fprintf(f, ">\n");
	coldelim = '&';
    }
    else if ( tbl_style == TEX ) {
	fprintf(f,"{\t%% ** %s spreadsheet output\n\\settabs %d \\columns\n",
		progname, cn-c0+1);
	coldelim = '&';
    }
    else if ( tbl_style == FRAME ) {
	fprintf(f,"<MIFFile 3.00> # generated by the sc spreadsheet calculator\n");
	fprintf(f,"<Tbls\n");
	fprintf(f," <Tbl \n");
	fprintf(f,"  <TblID 1> # This table's ID is 1\n");
	fprintf(f,"  <TblFormat \n");
	fprintf(f,"   <TblTag `Format A'> # Table Format Catalog\n");
	fprintf(f,"  > # end of TblFormat\n");
	fprintf(f,"  <TblNumColumns %d> # Has %d columns\n",cn-c0+1,cn-c0+1);
	fprintf(f,"  <TblTitleContent\n");
	fprintf(f,"   <Para\n");
	fprintf(f,"    <PgfTag `TableTitle'> # Forces lookup in Paragraph Format Catalog\n");
	fprintf(f,"    <ParaLine\n");
	fprintf(f,"     <String `%s'>\n",fname);
	fprintf(f,"    > # end of ParaLine\n");
	fprintf(f,"   > # end of Para\n");
	fprintf(f,"  > # end of TblTitleContent\n");
	fprintf(f,"  <TblH # The heading\n");
	fprintf(f,"   <Row # The heading row\n");
	for (col=c0; col <= cn; col++) {
	    fprintf(f,"    <Cell <CellContent <Para # Cell in column \n");
	    fprintf(f,"       <PgfTag `CellHeading'> # in Paragraph Format Catalog\n");
	    fprintf(f,"       <ParaLine <String `%c'>>\n",'A'+col);
	    fprintf(f,"    >>> # end of Cell\n");
	}
	fprintf(f,"   > # end of Row\n");
	fprintf(f,"  > # end of TblH\n");
	fprintf(f,"  <TblBody # The body\n");
    }

    for (row=r0; row<=rn; row++) {
	if ( tbl_style == TEX )
	    (void) fprintf (f, "\\+");
	else if (tbl_style == FRAME) {
	    fprintf(f,"   <Row # The next body row\n");
	}
	
	for (pp = ATBL(tbl, row, col=c0); col<=cn; col++, pp++) {
	    if ( tbl_style == FRAME ) {
		fprintf(f,"    <Cell <CellContent <Para\n");
		fprintf(f,"       <PgfTag `CellBody'> # in Paragraph Format Catalog\n");
		fprintf(f,"       <ParaLine <String `");
	    } 
	    if (*pp) {
		char *s;
		if ((*pp)->flags&is_valid) {
		    if ((*pp)->cellerror) {
			(void) fprintf (f, "%*s",
					fwidth[col],
			((*pp)->cellerror == CELLERROR ? "ERROR" : "INVALID"));
		    } else if ((*pp)->format) {
		        char field[FBUFLEN];
			if (*((*pp)->format) == ctl('d')) {
			    time_t v = (time_t) ((*pp)->v);
			    strftime(field, sizeof(field), ((*pp)->format)+1,
				    localtime(&v));
			} else
			    format((*pp)->format, precision[col], (*pp)->v,
				    field, sizeof(field));
			unspecial(f, field, coldelim);
		    } else {
		        char field[FBUFLEN];
                        (void) engformat(realfmt[col], fwidth[col],
				precision[col], (*pp) -> v,
				field, sizeof(field));
			unspecial(f, field, coldelim);
		    }
		}
		if ((s = (*pp)->label)) {
	            unspecial(f, s, coldelim);
		}
	    }
	    if (tbl_style == FRAME) {
		fprintf(f, "'>>\n");
		fprintf(f,"    >>> # end of Cell\n");
	    }
	    if (col < cn)
		if (tbl_style != FRAME)
		    (void) fprintf(f,"%c", coldelim);
	}
	if (tbl_style == LATEX) {
		if (row < rn) (void) fprintf (f, "\\\\");
	}
	else if (tbl_style == SLATEX) {
		if (row < rn) (void) fprintf(f, "!!");
	}
	else if (tbl_style == TEX) {
		(void) fprintf (f, "\\cr");
	}
	else if (tbl_style == FRAME) {
	    fprintf(f,"   > # end of Row\n");
	}
	(void) fprintf(f,"\n");
    }

    if (tbl_style == TBL)
    (void) fprintf (f,".TE\n.\\\" ** end of %s spreadsheet output\n", progname);
    else if (tbl_style == LATEX)
	(void) fprintf(f,"\\end{tabular}\n%% ** end of %s spreadsheet output\n", progname);
    else if (tbl_style == SLATEX)
	(void) fprintf (f,"!end<tabular>\n%% ** end of %s spreadsheet output\n", progname);
    else if (tbl_style == TEX)
	(void) fprintf (f,"}\n%% ** end of %s spreadsheet output\n", progname);
    else if (tbl_style == FRAME) {
	fprintf(f,"  > # end of TblBody\n");
	fprintf(f," ># end of Tbl\n");
	fprintf(f,"> # end of Tbls\n");
	fprintf(f,"<TextFlow <Para \n");
	fprintf(f,"  <PgfTag Body> \n");
	fprintf(f,"  <ParaLine <ATbl 1>> # Reference to table ID 1\n");
	fprintf(f,">>\n");
    }

    closefile(f, pid, 0);
}

/* unspecial (backquote) things that are special chars in a table */
void
unspecial(FILE *f, char *str, int delim)
{
    if (*str == '\\') str++; /* delete wheeling string operator, OK? */
    while (*str) {
    	if (((tbl_style == LATEX) || (tbl_style == SLATEX) ||
		(tbl_style == TEX)) &&
		((*str == delim) || (*str == '$') || (*str == '#') ||
		(*str == '%') || (*str == '{') || (*str == '}') ||
		(*str == '&')))
	    putc('\\', f);
	putc(*str, f);
	str++;
    }
}

struct enode *
copye(register struct enode *e, int Rdelta, int Cdelta)
{
    register struct enode *ret;

    if (e == (struct enode *)0)
	ret = (struct enode *)0;
    else if (e->op & REDUCE) {
	int newrow, newcol;
	if (freeenodes) {
	    ret = freeenodes;
	    freeenodes = ret->e.o.left;
	} else
	    ret = (struct enode *) scxmalloc((unsigned) sizeof (struct enode));
	ret->op = e->op;
	newrow=e->e.r.left.vf & FIX_ROW ? e->e.r.left.vp->row :
					  e->e.r.left.vp->row+Rdelta;
	newcol=e->e.r.left.vf & FIX_COL ? e->e.r.left.vp->col :
					  e->e.r.left.vp->col+Cdelta;
	ret->e.r.left.vp = lookat(newrow, newcol);
	ret->e.r.left.vf = e->e.r.left.vf;
	newrow=e->e.r.right.vf & FIX_ROW ? e->e.r.right.vp->row :
					   e->e.r.right.vp->row+Rdelta;
	newcol=e->e.r.right.vf & FIX_COL ? e->e.r.right.vp->col :
					   e->e.r.right.vp->col+Cdelta;
	ret->e.r.right.vp = lookat(newrow, newcol);
	ret->e.r.right.vf = e->e.r.right.vf;
    } else {
	if (freeenodes) {
	    ret = freeenodes;
	    freeenodes = ret->e.o.left;
	} else
	    ret = (struct enode *) scxmalloc((unsigned) sizeof (struct enode));
	ret->op = e->op;
	switch (ret->op) {
	    case 'v':
		{
		    int newrow, newcol;
		    newrow=e->e.v.vf & FIX_ROW ? e->e.v.vp->row :
						 e->e.v.vp->row+Rdelta;
		    newcol=e->e.v.vf & FIX_COL ? e->e.v.vp->col :
						 e->e.v.vp->col+Cdelta;
		    ret->e.v.vp = lookat(newrow, newcol);
		    ret->e.v.vf = e->e.v.vf;
		    break;
		}
	    case 'k':
		ret->e.k = e->e.k;
		break;
	    case 'f':
		ret->e.o.right = copye(e->e.o.right,0,0);
		ret->e.o.left = (struct enode *)0;
 		break;
	    case '$':
		ret->e.s = scxmalloc((unsigned) strlen(e->e.s)+1);
		(void) strcpy(ret->e.s, e->e.s);
		break;
	    default:
		ret->e.o.right = copye(e->e.o.right,Rdelta,Cdelta);
		ret->e.o.left = copye(e->e.o.left,Rdelta,Cdelta);
		break;
	}
    }
    return ret;
}

/*
 * sync_refs and syncref are used to remove references to
 * deleted struct ents.  Note that the deleted structure must still
 * be hanging around before the call, but not referenced by an entry
 * in tbl.  Thus the free_ent calls in sc.c
 */
void
sync_refs()
{
    int i, j;
    register struct ent *p;
    sync_ranges();
    for (i=0; i<=maxrow; i++)
	for (j=0; j<=maxcol; j++)
	    if ((p = *ATBL(tbl, i, j)) && p->expr)
		syncref(p->expr);
}

void
syncref(register struct enode *e)
{
    if (e == (struct enode *)0)
	return;
    else if (e->op & REDUCE) {
 	e->e.r.right.vp = lookat(e->e.r.right.vp->row, e->e.r.right.vp->col);
 	e->e.r.left.vp = lookat(e->e.r.left.vp->row, e->e.r.left.vp->col);
    } else {
	switch (e->op) {
	    case 'v':
		if (!(e->e.v.vp->flags & is_cleared))
		    e->e.v.vp = lookat(e->e.v.vp->row, e->e.v.vp->col);
		else {
		    e->op = ERR_;
		    e->e.o.left = NULL;
		    e->e.o.right = NULL;
		}
		break;
	    case 'k':
		break;
	    case '$':
		break;
	    default:
		syncref(e->e.o.right);
		syncref(e->e.o.left);
		break;
	}
    }
}

/* mark a row as hidden */
void
hiderow(int arg)
{
    register int r1;
    register int r2;

    r1 = currow;
    r2 = r1 + arg - 1;
    if (r1 < 0 || r1 > r2) {
	error("Invalid range");
	return;
    }
    if (r2 >= maxrows-1) {
    	if (!growtbl(GROWROW, arg+1, 0)) {
	    error("You can't hide the last row");
	    return;
	}
    }
    FullUpdate++;
    modflg++;
    while (r1 <= r2)
	row_hidden[r1++] = 1;
}

/* mark a column as hidden */
void
hidecol(int arg)
{
    int c1;
    int c2;

    c1 = curcol;
    c2 = c1 + arg - 1;
    if (c1 < 0 || c1 > c2) {
	error ("Invalid range");
	return;
    }
    if (c2 >= maxcols-1) {
    	if ((arg >= ABSMAXCOLS-1) || !growtbl(GROWCOL, 0, arg+1)) {
	    error("You can't hide the last col");
	    return;
	}
    }
    FullUpdate++;
    modflg++;
    while (c1 <= c2)
	col_hidden[c1++] = TRUE;
}

/* mark a row as not-hidden */
void
showrow(int r1, int r2)
{
    if (r1 < 0 || r1 > r2) {
	error ("Invalid range");
	return;
    }
    if (r2 > maxrows-1) {
	r2 = maxrows-1;
    }
    FullUpdate++;
    modflg++;
    while (r1 <= r2)
	row_hidden[r1++] = 0;
}

/* mark a column as not-hidden */
void
showcol(int c1, int c2)
{
    if (c1 < 0 || c1 > c2) {
	error ("Invalid range");
	return;
    }
    if (c2 > maxcols-1) {
	c2 = maxcols-1;
    }
    FullUpdate++;
    modflg++;
    while (c1 <= c2)
	col_hidden[c1++] = FALSE;
}

/* Open the input or output file, setting up a pipe if needed */
FILE *
openfile(char *fname, int *rpid, int *rfd)
{
    int pipefd[4];
    int pid;
    FILE *f;
    char *efname;

    while (*fname && (*fname == ' '))	/* Skip leading blanks */
	fname++;

    if (*fname != '|') {		/* Open file if not pipe */
	*rpid = 0;
	if (rfd != NULL)
	    *rfd = 1;			/* Set to stdout just in case */
	
	efname = findhome(fname);
#ifdef DOBACKUPS
	if (rfd == NULL && !backup_file(efname) &&
	    (yn_ask("Could not create backup copy.  Save anyway?: (y,n)") != 1))
		return (0);
#endif
	return (fopen(efname, rfd == NULL ? "w" : "r"));
    }

#if defined(MSDOS)
    error("Piping not available under MS-DOS\n");
    return (0);
#else
    fname++;				/* Skip | */
    efname = findhome(fname);
    if (pipe(pipefd) < 0 || (rfd != NULL && pipe(pipefd+2) < 0)) {
	error("Can't make pipe to child");
	*rpid = 0;
	return (0);
    }

    deraw(rfd==NULL);
#ifdef VMS
    fprintf(stderr, "No son tasks available yet under VMS--sorry\n");
#else /* VMS */

    if ((pid=fork()) == 0) {		/* if child */
	(void) close(0);		/* close stdin */
	(void) close(pipefd[1]);
	(void) dup(pipefd[0]);		/* connect to first pipe */
	if (rfd != NULL) {		/* if opening for read */
	    (void) close(1);		/* close stdout */
	    (void) close(pipefd[2]);
	    (void) dup(pipefd[3]);	/* connect to second pipe */
	}
	(void) signal(SIGINT, SIG_DFL);	/* reset */
	(void) execl("/bin/sh", "sh", "-c", efname, 0);
	exit (-127);
    } else {				/* else parent */
	*rpid = pid;
	if ((f = fdopen(pipefd[(rfd==NULL?1:2)], rfd==NULL?"w":"r")) == NULL) {
	    (void) kill(pid, 9);
	    error("Can't fdopen %sput", rfd==NULL?"out":"in");
	    (void) close(pipefd[1]);
	    if (rfd != NULL)
		(void) close(pipefd[3]);
	    *rpid = 0;
	    return (0);
	}
    }
    (void) close(pipefd[0]);
    if (rfd != NULL) {
	(void) close(pipefd[3]);
	*rfd = pipefd[1];
    }
#endif /* VMS */
    return (f);
#endif /* MSDOS */
}

/* close a file opened by openfile(), if process wait for return */
void
closefile(FILE *f, int pid, int rfd)
{
    int temp;

    (void) fclose(f);
#ifndef MSDOS
    if (pid) {
	while (pid != wait(&temp)) /**/;
	if (rfd==0) {
	    (void) printf("Press any key to continue ");
	    (void) fflush(stdout);
	    cbreak();
	    (void) nmgetch();
	    goraw();
	} else {
	    close(rfd);
	    if (usecurses) {
#ifdef VMS
		VMS_read_raw = 1;
#else /* VMS */
#if SYSV2 || SYSV3
		fixterm();
#else /* SYSV2 || SYSV3 */
		cbreak();
		nonl();
		noecho ();
#endif /* SYSV2 || SYSV3 */
		kbd_again();
#endif /* VMS */
		if (color && has_colors())
		    bkgdset(COLOR_PAIR(1) | ' ');
	    }
	}
    }
#endif /* MSDOS */
    if (brokenpipe) {
	error("Broken pipe");
	brokenpipe = FALSE;
    }
}

void
copyent(register struct ent *n, register struct ent *p, int dr, int dc)
{
    if (!n || !p) {
	error("internal error");
	return;
    }
    n->v = p->v;
    n->flags = p->flags;
    n->expr = copye(p->expr, dr, dc);
    n->label = (char *)0;
    if (p->label) {
	n->label = scxmalloc((unsigned) (strlen(p->label) + 1));
	(void) strcpy(n->label, p->label);
    }
    n->format = 0;
    if (p->format) {
        n->format = scxmalloc((unsigned) (strlen(p->format) + 1));
	(void) strcpy(n->format, p->format);
    }
}

void
write_fd(register FILE *f, int r0, int c0, int rn, int cn)
{
    register struct ent **pp;
    int r, c;

    (void) fprintf(f, "# This data file was generated by the Spreadsheet ");
    (void) fprintf(f, "Calculator.\n");
    (void) fprintf(f, "# You almost certainly shouldn't edit it.\n\n");
    print_options(f);
    for (c = 0; c < COLFORMATS; c++)
	if (colformat[c])
	    (void) fprintf (f, "format %d = \"%s\"\n", c, colformat[c]);
    for (c = c0; c < cn; c++)
	if (fwidth[c] != DEFWIDTH || precision[c] != DEFPREC ||
		realfmt[c] != DEFREFMT)
	    (void) fprintf (f, "format %s %d %d %d\n", coltoa(c), fwidth[c],
		    precision[c], realfmt[c]);
    for (c = c0; c < cn; c++)
        if (col_hidden[c])
            (void) fprintf(f, "hide %s\n", coltoa(c));
    for (r = r0; r <= rn; r++)
	if (row_hidden[r])
	    (void) fprintf(f, "hide %d\n", r);

    write_ranges(f);
    write_franges(f);
    write_colors(f, 0);
    write_cranges(f);

    if (mdir) 
	(void) fprintf(f, "mdir \"%s\"\n", mdir);
    if (autorun) 
	(void) fprintf(f, "autorun \"%s\"\n", autorun);
    for (c = 0; c < FKEYS; c++)
	if (fkey[c])
	    (void) fprintf(f, "fkey %d = \"%s\"\n", c + 1, fkey[c]);

    write_cells(f, r0, c0, rn, cn, r0, c0);
    for (r = r0; r <= rn; r++) {
	pp = ATBL(tbl, r, c0);
	for (c = c0; c <= cn; c++, pp++)
	    if (*pp) {
		if ((*pp)->flags&is_locked)
		    (void) fprintf(f, "lock %s%d\n", coltoa((*pp)->col),
						     (*pp)->row);
		if ((*pp)->nrow >= 0) {
		    (void) fprintf(f, "addnote %s ",
			    v_name((*pp)->row, (*pp)->col));
		    (void) fprintf(f, "%s\n", r_name((*pp)->nrow, (*pp)->ncol,
			    (*pp)->nlastrow, (*pp)->nlastcol));
		}
	    }
    }
    /* xxx
    if (rndtoeven)
	fprintf(f, "set rndtoeven\n");
    */
    /*
     * Don't try to combine these into a single fprintf().  v_name() has
     * a single buffer that is overwritten on each call, so the first part
     * needs to be written to the file before making the second call.
     */
    fprintf(f, "goto %s", v_name(currow, curcol));
    fprintf(f, " %s\n", v_name(strow, stcol));
}

void
write_cells(register FILE *f, int r0, int c0, int rn, int cn, int dr, int dc)
{
    register struct ent **pp;
    int r, c, rs, cs, mf;

    mf = modflg;
    if (dr != r0 || dc != c0) {
	yank_area(r0, c0, rn, cn);
	rn += dr - r0;
	cn += dc - c0;
	rs = currow;
	cs = curcol;
	currow = dr;
	curcol = dc;
	pullcells('x');
    }
    if (Vopt) valueize_area(dr, dc, rn, cn);
    for (r = dr; r <= rn; r++) {
	pp = ATBL(tbl, r, dc);
	for (c = dc; c <= cn; c++, pp++)
	    if (*pp) {
		if ((*pp)->label) {
		    edits(r, c);
		    (void) fprintf(f, "%s\n", line);
		}
		if ((*pp)->flags&is_valid) {
		    editv(r, c);
		    (void) fprintf(f, "%s\n", line);
		}
		if ((*pp)->format) {
		    editfmt(r, c);
		    (void) fprintf(f, "%s\n",line);
		}
	    }
    }
    if (dr != r0 || dc != c0) {
	pullcells('x');
	currow = rs;
	curcol = cs;
	flush_saved();
    }
    modflg = mf;
}

int
writefile(char *fname, int r0, int c0, int rn, int cn)
{
    register FILE *f;
    char save[PATHLEN];
    char tfname[PATHLEN];
    long namelen;
    char *tpp;
    int pid;

#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
    if (Crypt) {
	return (cwritefile(fname, r0, c0, rn, cn));
    }
#endif /* VMS */

    if (*fname == '\0')
	if (isatty(STDOUT_FILENO) || *curfile != '\0')
	    fname = curfile;
	else {
	    write_fd(stdout, r0, c0, rn, cn);
	    return (0);
	}

#ifdef MSDOS
    namelen = 12;
#else
    if ((tpp = strrchr(fname, '/')) == NULL)
	namelen = pathconf(".", _PC_NAME_MAX);
    else {
	*tpp = '\0';
	namelen = pathconf(fname, _PC_NAME_MAX);
	*tpp = '/';
    }
#endif /* MSDOS */

    (void) strcpy(tfname, fname);
    for (tpp = tfname; *tpp != '\0'; tpp++)
	if (*tpp == '\\' && *(tpp + 1) == '"')
	    (void) memmove(tpp, tpp + 1, strlen(tpp));

    if (scext != NULL) {
	if (strlen(tfname) > 3 && !strcmp(tfname + strlen(tfname) - 3, ".sc"))
	    tfname[strlen(tfname) - 3] = '\0';
	else if (strlen(tfname) > strlen(scext) + 1 &&
		tfname[strlen(tfname) - strlen(scext) - 1] == '.' &&
		!strcmp(tfname + strlen(tfname) - strlen(scext), scext))
	    tfname[strlen(tfname) - strlen(scext) - 1] = '\0';
	tfname[namelen - strlen(scext) - 1] = '\0';
	strcat(tfname, ".");
	strcat(tfname, scext);
    }

    (void) strcpy(save, tfname);
    for (tpp = save; *tpp != '\0'; tpp++)
	if (*tpp == '"') {
	    (void) memmove(tpp + 1, tpp, strlen(tpp) + 1);
	    *tpp++ = '\\';
	}

    if ((f = openfile(tfname, &pid, NULL)) == NULL) {
	error("Can't create file \"%s\"", save);
	return (-1);
    }

    if (usecurses) {
	error("Writing file \"%s\"...", save);
	refresh();
    }
    write_fd(f, r0, c0, rn, cn);
    
    closefile(f, pid, 0);

    if (!pid) {
        (void) strcpy(curfile, save);
        modflg = 0;
        error("File \"%s\" written.", curfile);
    }

    return (0);
}

void
readfile(char *fname, int eraseflg)
{
    register FILE *f;
    char save[PATHLEN];
    int tempautolabel;
    int pid = 0;
    int rfd = STDOUT_FILENO, savefd;

    tempautolabel = autolabel;		/* turn off auto label when */
    autolabel = 0;			/* reading a file */

    if (*fname == '*' && mdir) { 
       (void) strcpy(save, mdir);
       (void) strcat(save, fname);
    } else {
        if (*fname == '\0')
	    fname = curfile;
	(void) strcpy(save, fname);
    }

#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
    if (Crypt) {
	if (*save == '-' && strlen(fname) == 1)
	    error("Can't use encryption in a pipeline.");
	else
	    if (*save == '|')
		error("Can't use encryption with advanced macros.");
	else
	    creadfile(save, eraseflg);
	autolabel = tempautolabel;
	return;
    }
#endif /* VMS */

    if (eraseflg && strcmp(fname, curfile) && modcheck(" first")) return;

#ifndef MSDOS
    if (fname[0] == '-' && fname[1] == '\0') {
	f = stdin;
	*save = '\0';
    } else {
#endif /* MSDOS */
	if ((f = openfile(save, &pid, &rfd)) == NULL) {
	    error("Can't read file \"%s\"", save);
	    autolabel = tempautolabel;
	    return;
	} else if (eraseflg) {
	    if (usecurses) {
		error("Reading file \"%s\"", save);
		refresh();
	    }
	}
#ifndef MSDOS
    }
    if (*fname == '|')
	*save = '\0';
#endif /* MSDOS */

    if (eraseflg) erasedb();

    loading++;
    savefd = macrofd;
    macrofd = rfd;
    while (!brokenpipe && fgets(line, sizeof(line), f)) {
#ifndef MSDOS
	if (line[0] == '|' && pid != 0) {
	    line[0] = ' ';
	}
#endif /* MSDOS */
	linelim = 0;
	if (line[0] != '#') (void) yyparse();
    }
    macrofd = savefd;
    --loading;
    closefile(f, pid, rfd);
#ifndef MSDOS
    if (f == stdin) {
	freopen("/dev/tty", "r", stdin);
	goraw();
    }
#endif /* MSDOS */
    linelim = -1;
    if (eraseflg) {
	(void) strcpy(curfile, save);
	modflg = 0;
	cellassign = 0;
	if (autorun) readfile(autorun, 0);
	EvalAll();
    }
    autolabel = tempautolabel;
}

/* erase the database (tbl, etc.) */
void
erasedb()
{
    int  r, c;
    char *home;

    for (c = 0; c<=maxcol; c++) {
	fwidth[c] = DEFWIDTH;
	precision[c] = DEFPREC;
	realfmt[c] = DEFREFMT;
    }

    for (r = 0; r<=maxrow; r++) {
	register struct ent **pp = ATBL(tbl, r, 0);
	for (c=0; c++<=maxcol; pp++)
	    if (*pp) {
		if ((*pp)->expr)  efree((*pp) -> expr);
		if ((*pp)->label) scxfree((char *)((*pp) -> label));
		(*pp)->next = freeents;	/* save [struct ent] for reuse */
		freeents = *pp;
		*pp = (struct ent *)0;
	    }
    }
    for (dbidx = 3; dbidx >= 0; )
	delbuf[dbidx--] = NULL;

    maxrow = 0;
    maxcol = 0;
    clean_range();
    clean_frange();
    clean_crange();

    propagation = 10;
    calc_order = BYROWS;
    prescale = 1.0;
    tbl_style = 0;
    craction = 0;
    rowlimit = collimit = -1;

    autocalc=showcell=showtop=1;
    autoinsert=optimize=numeric=extfunc=color=colorneg=colorerr=cslop=0;
    currow=curcol=strow=stcol=0;
    if (usecurses && has_colors())
	color_set(0, NULL);
    /* unset all marks */
    for (c = 1; c < 27; c++)
	savedrow[c] = savedcol[c] = savedstrow[c] = savedstcol[c] = -1;

    /* set location to return to with `` or '' */
    savedrow[0] = currow;
    savedcol[0] = curcol;
    savedstrow[0] = strow;
    savedstcol[0] = stcol;

    if (mdir) {
	scxfree(mdir);
	mdir = NULL;
    }
    if (autorun) {
	scxfree(autorun);
	autorun = NULL;
    }
    for (c = 0; c < FKEYS; c++)
	if (fkey[c]) {
	    scxfree(fkey[c]);
	    fkey[c] = NULL;
	}

#ifndef MSDOS
    /*
     * Load $HOME/.scrc if present.
     */
    if ((home = getenv("HOME"))) {
	strcpy(curfile, home);
	strcat(curfile, "/.scrc");
	if ((c = open(curfile, O_RDONLY)) > -1) {
	    close(c);
	    readfile(curfile, 0);
	}
    }

    /*
     * Load ./.scrc if present and $HOME/.scrc contained `set scrc'.
     */
    if (scrc && strcmp(home, getcwd(curfile, PATHLEN)) &&
	    (c = open(".scrc", O_RDONLY)) > -1) {
	close(c);
	readfile(".scrc", 0);
    }
#endif /* MSDOS */

    *curfile = '\0';
    FullUpdate++;
}

/* moves curcol back one displayed column */
void
backcol(int arg)
{
    while (--arg >= 0) {
	if (curcol)
	    curcol--;
	else
	    {error ("At column A"); break;}
	while(col_hidden[curcol] && curcol)
	    curcol--;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* moves curcol forward one displayed column */
void
forwcol(int arg)
{
    while (--arg>=0) {
	if (curcol < maxcols - 1)
	    curcol++;
	else
	if (!growtbl(GROWCOL, 0, arg))	/* get as much as needed */
		break;
	else
		curcol++;
	while (col_hidden[curcol] && (curcol < maxcols - 1))
	    curcol++;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* moves currow forward one displayed row */
void
forwrow(int arg)
{
    while (--arg>=0) {
	if (currow < maxrows - 1)
	    currow++;
	else
	if (!growtbl(GROWROW, arg, 0))	/* get as much as needed */
		break;
	else
		currow++;
	while (row_hidden[currow]&&(currow<maxrows-1))
	    currow++;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* moves currow backward one displayed row */
void
backrow(int arg)
{
    while (--arg>=0) {
	if (currow)
	    currow--;
	else
	    {error("At row zero"); break;}
	while (row_hidden[currow] && currow)
	    currow--;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

void
markcell()
{
    int c;

    error("Mark cell:");
    if ((c=nmgetch()) == ESC || c == ctl('g')) {
	error("");
	return;
    }
    if ((c -= ('a' - 1)) < 1 || c > 26) {
	error("Invalid character for mark (must be a-z)");
	return;
    }
    error("");
    savedrow[c] = currow;
    savedcol[c] = curcol;
    savedstrow[c] = strow;
    savedstcol[c] = stcol;
}

void
dotick(int tick)
{
    int c;

    /* Remember the current position */
    int tmprow = currow;
    int tmpcol = curcol;
    int tmpstrow = strow;
    int tmpstcol = stcol;

    error("Go to marked cell:");
    if ((c = nmgetch()) == ESC || c == ctl('g')) {
	error("");
	return;
    }
    if ((c -= ('a' - 1)) < 1 || c > 26) {
	if (tick == '`' || tick == '\'')
	    c = 0;
	else {
	    error("Invalid character for mark (must be a-z, ` or \')");
	    return;
	}
    }
    if (savedrow[c] == -1) {
	error("Mark not set");
	return;
    }
    error("");
    currow = savedrow[c];
    curcol = savedcol[c];
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    if (tick == '`') {
	strow = savedstrow[c];
	stcol = savedstcol[c];
	gs.stflag = 1;
    } else
	gs.stflag = 0;
    savedrow[0] = tmprow;
    savedcol[0] = tmpcol;
    savedstrow[0] = tmpstrow;
    savedstcol[0] = tmpstcol;

    FullUpdate++;
}

void
gotonote()
{
    register struct ent *p;

    p = lookat(currow, curcol);
    if (p->nrow == -1)
	error("No note attached");
    else
	moveto(p->nrow, p->ncol, p->nlastrow, p->nlastcol, -1, -1);
}

/*
 * Show a cell's label string or expression value.  May overwrite value if
 * there is one already displayed in the cell.  Created from old code in
 * update(), copied with minimal changes.
 */

void
showstring(char *string,	/* to display */
    int dirflush,		/* or rightflush or centered */
    int hasvalue,		/* is there a numeric value? */
    int row, int col,		/* spreadsheet location */
    int *nextcolp,		/* value returned through it */
    int mxcol,			/* last column displayed? */
    int *fieldlenp,		/* value returned through it */
    int r, int c,		/* screen row and column */
    struct frange *fr,		/* frame range we're currently in, if any */
    int frightcols,		/* number of frame columns to the right */
    int flcols, int frcols)	/* width of left and right sides of frame */
{
    int nextcol  = *nextcolp;
    int fieldlen = *fieldlenp;
    char field[FBUFLEN];
    int  slen;
    char *start, *last;
    register char *fp;
    struct ent *nc;
    struct crange *cr;

    cr = find_crange(row, col);

    /* This figures out if the label is allowed to
       slop over into the next blank field */

    slen = strlen(string);
    if (*string == '\\' && *(string+1) != '\0')
	slen = fwidth[col];
    if (c + fieldlen == rescol + flcols && nextcol < stcol)
	nextcol = stcol;
    if (frightcols &&
	    c + fieldlen + fwidth[nextcol] >= COLS - 1 - frcols &&
	    nextcol < fr->or_right->col - frightcols + 1)
	nextcol = fr->or_right->col - frightcols + 1;
    while ((slen > fieldlen) && (nextcol <= mxcol) &&
	    !((nc = lookat(row, nextcol))->flags & is_valid) && !(nc->label) &&
	    (cslop || find_crange(row, nextcol) == cr)) {

	if (!col_hidden[nextcol])
	    fieldlen += fwidth[nextcol];
	nextcol++;
	if (c + fieldlen == rescol + flcols && nextcol < stcol)
	    nextcol = stcol;
	if (frightcols &&
		c + fieldlen + fwidth[nextcol] >= COLS - 1 - frcols &&
		nextcol < fr->or_right->col - frightcols + 1)
	    nextcol = fr->or_right->col - frightcols + 1;
    }
    if (slen > fieldlen)
	slen = fieldlen;

    /* Now justify and print */
    start = (dirflush&is_leftflush) ? field : field + fieldlen - slen;
    if (dirflush & is_label)
	start = field + ((slen<fwidth[col])?(fieldlen-slen)/2:0);
    last = field+fieldlen;
    fp = field;
    if (slen)
	while (fp < start)
	    *fp++ = ' ';
    if (*string == '\\' && *(string+1) != '\0') {
	char *strt;
	strt = ++string;

	while (slen--) {
	    *fp++ = *string++;
	    if (*string == '\0')
		string = strt;
	}
    }
    else
    while (slen--)
	*fp++ = *string++;

    if ((!hasvalue) || fieldlen != fwidth[col]) 
	while (fp < last)
	    *fp++ = ' ';
    *fp = '\0';
#ifdef VMS
    mvaddstr(r, c, field);	/* this is a macro */
#else
    (void) mvaddstr(r, c, field);
#endif

    *nextcolp  = nextcol;
    *fieldlenp = fieldlen;
}

int
etype(register struct enode *e)
{
    if (e == (struct enode *)0)
	return NUM;
    switch (e->op) {
	case UPPER: case LOWER: case CAPITAL:
	case O_SCONST: case '#': case DATE: case FMT: case STINDEX:
	case EXT: case SVAL: case SUBSTR:
	    return (STR);

	case '?':
	case IF:
	    return (etype(e->e.o.right->e.o.left));

	case 'f':
	    return (etype(e->e.o.right));

	case O_VAR: {
	    register struct ent *p;
	    p = e->e.v.vp;
	    if (p->expr) 
		return (p->flags & is_strexpr ? STR : NUM);
	    else if (p->label)
		return (STR);
	    else
		return (NUM);
	    }

	default:
	    return (NUM);
    }
}

/* return 1 if yes given, 0 otherwise */
int
yn_ask(char *msg)
{	char ch;

	(void) move(0, 0);
	(void) clrtoeol();
	(void) addstr(msg);
	(void) refresh();
	while ((ch = nmgetch()) != 'y' && ch != 'Y' && ch != 'n' && ch != 'N') {
	    if (ch == ctl('g') || ch == ESC)
		return (-1);
	}
	if (ch == 'y' || ch == 'Y')
	    return (1);
	else
	    return (0);
}

/* expand a ~ in a path to your home directory */
#if !defined(MSDOS) && !defined(VMS)
#include <pwd.h>
#endif
char *
findhome(char *path)
{
    static	char	*HomeDir = NULL;

    if (*path == '~') {
    	char	*pathptr;
	char	tmppath[PATHLEN];

	if (HomeDir == NULL) {
	    HomeDir = getenv("HOME");
	    if (HomeDir == NULL)
		HomeDir = "/";
	}
	pathptr = path + 1;
	if ((*pathptr == '/') || (*pathptr == '\0'))
	    strcpy(tmppath, HomeDir);
#if !defined(MSDOS) && !defined(VMS)
	else {
	    struct	passwd *pwent;
	    char	*namep;
	    char	name[50];

	    namep = name;
	    while ((*pathptr != '\0') && (*pathptr != '/'))
		    *(namep++) = *(pathptr++);
	    *namep = '\0';
	    if ((pwent = getpwnam(name)) == NULL) {
	    	(void) sprintf(path, "Can't find user %s", name);
		return (NULL);
	    }
	    strcpy(tmppath, pwent->pw_dir);
	}
#endif
	strcat(tmppath, pathptr);
	strcpy(path, tmppath);
    }
    return (path);
}

#ifdef DOBACKUPS
#include <sys/stat.h>

/*
 * make a backup copy of a file, use the same mode and name in the format
 * [path/]#file~
 * return 1 if we were successful, 0 otherwise
 */
int
backup_file(char *path)
{
	struct	stat	statbuf;
	char	fname[PATHLEN];
	char	tpath[PATHLEN];
#ifdef sequent
	static	char	*buf = NULL;
	static	unsigned buflen = 0;
#else
	char	buf[BUFSIZ];
#endif
	char	*tpp;
	int	infd, outfd;
	int	count;

	/* tpath will be the [path/]file ---> [path/]#file~ */
	strcpy(tpath, path);
	if ((tpp = strrchr(tpath, '/')) == NULL)
		tpp = tpath;
	else
		tpp++;
	strcpy(fname, tpp);
	(void) sprintf(tpp, "#%s~", fname);

	if (stat(path, &statbuf) == 0)
	{
		/* if we know the optimum block size, use it */
#ifdef sequent
		if ((statbuf.st_blksize > buflen) || (buf == NULL))
		{	buflen = statbuf.st_blksize;
			if ((buf = scxrealloc(buf, buflen)) == (char *)0)
			{	buflen = 0;
				return(0);
			}
		}
#endif

		if ((infd = open(path, O_RDONLY, 0)) < 0)
			return (0);

		if ((outfd = open(tpath, O_TRUNC|O_WRONLY|O_CREAT,
					statbuf.st_mode)) < 0)
			return (0);

#ifdef sequent
		while ((count = read(infd, buf, statbuf.st_blksize)) > 0)
#else
		while ((count = read(infd, buf, sizeof(buf))) > 0)
#endif
		{	if (write(outfd, buf, count) != count)
			{	count = -1;
				break;
			}
		}
		close(infd);
		close(outfd);

		return ((count < 0) ? 0 : 1);
	}
	else
	if (errno == ENOENT)
		return (1);
	return (0);
}
#endif
