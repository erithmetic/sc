
/*	SC	A Spreadsheet Calculator
 *		Range Manipulations
 *
 *              Robert Bond, 4/87
 *
 *		$Revision: 7.16 $
 */

#include <sys/types.h>
#ifdef BSD42
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif

#include <stdio.h>
#include <ctype.h>
#include "sc.h"

static	struct range *rng_base;
void	sync_enode(struct enode *e);
void	fix_enode(struct enode *e, int row1, int col1, int row2, int col2,
	int delta1, int delta2);

void
add_range(char *name, struct ent_ptr left, struct ent_ptr right, int is_range)
{
    struct range *r;
    register char *p;
    int minr, minc, maxr, maxc;
    int minrf, mincf, maxrf, maxcf;
    register struct ent *rcp;
    struct range *prev = 0;

    if (left.vp->row < right.vp->row) {
	minr = left.vp->row; minrf = left.vf & FIX_ROW;
	maxr = right.vp->row; maxrf = right.vf & FIX_ROW;
    } else {
	minr = right.vp->row; minrf = right.vf & FIX_ROW;
	maxr = left.vp->row; maxrf = right.vf & FIX_ROW;
    } 

    if (left.vp->col < right.vp->col) {
	minc = left.vp->col; mincf = left.vf & FIX_COL;
	maxc = right.vp->col; maxcf = right.vf & FIX_COL;
    } else {
	minc = right.vp->col; mincf = right.vf & FIX_COL;
	maxc = left.vp->col; maxcf = left.vf & FIX_COL;
    } 

    left.vp = lookat(minr, minc);
    left.vf = minrf | mincf;
    right.vp = lookat(maxr, maxc);
    right.vf = maxrf | maxcf;

    if (!find_range(name, strlen(name), (struct ent *)0, (struct ent *)0,
	    &prev)) {
	error("Error: range name \"%s\" already defined", name);
	scxfree(name);
	return;
    }

    for (p = name; *p; p++)
	if (!(isalpha(*p) || isdigit(*p) || *p == '_')) {
	    error("Invalid range name \"%s\" - illegal combination", name);
	    scxfree(name);
	    return;
	}
 
    p = name;
    if (isdigit(*p) || (isalpha(*p++) && (isdigit(*p) ||
		(isalpha(*p++) && isdigit(*p))))) {
	if (*name == '0' && (name[1] == 'x' || name[1] == 'X')) {
	    ++p;
	    while (isxdigit(*++p)) /* */;
	    if (*p == 'p' || *p == 'P')
		while (isxdigit(*++p)) /* */;
	} else {
	    while (isdigit(*++p)) /* */;
	    if (isdigit(*name) && (*p == 'e' || *p == 'E'))
		while (isdigit(*++p)) /* */;
	}
	if (!(*p)) {
	    error("Invalid range name \"%s\" - ambiguous", name);
	    scxfree(name);
	    return;
	}
    }
 
    if (autolabel && minc>0 && !is_range) {
	rcp = lookat(minr, minc-1);
	if (rcp->label==0 && rcp->expr==0 && rcp->v==0)
		label(rcp, name, 0);
    }

    r = (struct range *)scxmalloc((unsigned)sizeof(struct range));
    r->r_name = name;
    r->r_left = left;
    r->r_right = right;
    r->r_is_range = is_range;
    if (prev) {
	r->r_next = prev->r_next;
	r->r_prev = prev;
	prev->r_next = r;
	if (r->r_next)
	    r->r_next->r_prev = r;
    } else {
	r->r_next = rng_base;
	r->r_prev = (struct range *)0;
	if (rng_base)
	    rng_base->r_prev = r;
	rng_base = r;
    }
    modflg++;
}

void
del_range(struct ent *left, struct ent *right)
{
    struct range *r;
    int minr, minc, maxr, maxc;

    minr = left->row < right->row ? left->row : right->row;
    minc = left->col < right->col ? left->col : right->col;
    maxr = left->row > right->row ? left->row : right->row;
    maxc = left->col > right->col ? left->col : right->col;

    left = lookat(minr, minc);
    right = lookat(maxr, maxc);

    if (find_range((char *)0, 0, left, right, &r)) 
	return;

    if (r->r_next)
        r->r_next->r_prev = r->r_prev;
    if (r->r_prev)
        r->r_prev->r_next = r->r_next;
    else
	rng_base = r->r_next;
    scxfree((char *)(r->r_name));
    scxfree((char *)r);
    modflg++;
}

void
clean_range()
{
    register struct range *r;
    register struct range *nextr;

    r = rng_base;
    rng_base = (struct range *)0;

    while (r) {
	nextr = r->r_next;
	scxfree((char *)(r->r_name));
	scxfree((char *)r);
	r = nextr;
    }
}

/* Match on name or lmatch, rmatch */

int
find_range(char *name, int len, struct ent *lmatch, struct ent *rmatch,
	struct range **rng)
{
    struct range *r;
    int cmp;
    int exact = TRUE;
    
    if (len < 0) {
	exact = FALSE;
	len = -len;
    }

    if (name) {
	for (r = rng_base; r; r = r->r_next) {
	    if ((cmp = strncmp(name, r->r_name, len)) > 0)
		return (cmp);
	    *rng = r;
	    if (cmp == 0)
		if (!exact || strlen(r->r_name) == len)
		    return (cmp);
	}
	return (-1);
    }

    for (r = rng_base; r; r = r->r_next) {
	if ((lmatch == r->r_left.vp) && (rmatch == r->r_right.vp)) {
	    *rng = r;
	    return (0);
	}
    }
    return (-1);
}

void
sync_ranges()
{
    int i, j;
    struct range *r;
    struct ent *p;

    for (r = rng_base; r; r = r->r_next) {
	r->r_left.vp = lookat(r->r_left.vp->row, r->r_left.vp->col);
	r->r_right.vp = lookat(r->r_right.vp->row, r->r_right.vp->col);
    }
    for (i=0; i<=maxrow; i++)
	for (j=0; j<=maxcol; j++)
	    if ((p = *ATBL(tbl,i,j)) && p->expr)
		sync_enode(p->expr);
    sync_franges();
    sync_cranges();
}

void
sync_enode(struct enode *e)
{
    if (e) {
	if ((e->op & REDUCE)) {
	    e->e.r.left.vp = lookat(e->e.r.left.vp->row, e->e.r.left.vp->col);
	    e->e.r.right.vp = lookat(e->e.r.right.vp->row, e->e.r.right.vp->col);
	} else if (e->op != O_VAR && e->op !=O_CONST && e->op != O_SCONST) {
	    sync_enode(e->e.o.left);
	    sync_enode(e->e.o.right);
	}
    }
}

void
write_ranges(FILE *f)
{
    register struct range *r;
    register struct range *nextr;

    for (r = nextr = rng_base; nextr; r = nextr, nextr = r->r_next) /* */ ;
    while (r) {
	(void) fprintf(f, "define \"%s\" %s%s%s%d",
			r->r_name,
			r->r_left.vf & FIX_COL ? "$":"",
			coltoa(r->r_left.vp->col), 
			r->r_left.vf & FIX_ROW ? "$":"",
			r->r_left.vp->row);
	if (r->r_is_range)
	    (void) fprintf(f, ":%s%s%s%d\n",
			    r->r_right.vf & FIX_COL ? "$":"",
			    coltoa(r->r_right.vp->col), 
			    r->r_right.vf & FIX_ROW ? "$":"",
			    r->r_right.vp->row);
	else
	    (void) fprintf(f, "\n");
	r = r->r_prev;
    }
}

void
list_ranges(FILE *f)
{
    register struct range *r;
    register struct range *nextr;

    if (!are_ranges()) {
	fprintf(f, "  No ranges defined");
	return;
    }

    (void) fprintf(f, "  %-30s %s\n","Name","Definition");
    if (!brokenpipe) (void) fprintf(f, "  %-30s %s\n","----","----------");

    for (r = nextr = rng_base; nextr; r = nextr, nextr = r->r_next) /* */ ;
    while (r) {
	(void) fprintf(f, "  %-30s %s%s%s%d",
			    r->r_name,
			    r->r_left.vf & FIX_COL ? "$":"",
			    coltoa(r->r_left.vp->col), 
			    r->r_left.vf & FIX_ROW ? "$":"",
			    r->r_left.vp->row);
	if (brokenpipe) return;
	if (r->r_is_range)
	    (void) fprintf(f, ":%s%s%s%d\n",
			    r->r_right.vf & FIX_COL ? "$":"",
			    coltoa(r->r_right.vp->col), 
			    r->r_right.vf & FIX_ROW ? "$":"",
			    r->r_right.vp->row);
	else
	    (void) fprintf(f, "\n");
	if (brokenpipe) return;
	r = r->r_prev;
    }
}

char *
v_name(int row, int col)
{
    struct ent *v;
    struct range *r;
    static char buf[20];

    v = lookat(row, col);
    if (!find_range((char *)0, 0, v, v, &r)) {
	return (r->r_name);
    } else {
        (void) sprintf(buf, "%s%d", coltoa(col), row);
	return (buf);
    }
}

char *
r_name(int r1, int c1, int r2, int c2)
{
    struct ent *v1, *v2;
    struct range *r;
    static char buf[100];

    v1 = lookat(r1, c1);
    v2 = lookat(r2, c2);
    if (!find_range((char *)0, 0, v1, v2, &r)) {
	return (r->r_name);
    } else {
        (void) sprintf(buf, "%s", v_name(r1, c1));
	(void) sprintf(buf+strlen(buf), ":%s", v_name(r2, c2));
	return (buf);
    }
}

int
are_ranges()
{
    return (rng_base != 0);
}

void
fix_ranges(int row1, int col1, int row2, int col2, int delta1, int delta2)
{
    int r1, r2, c1, c2, i, j;
    struct range *r;
    struct frange *fr;
    struct ent *p;

    fr = find_frange(currow, curcol);

    /* First we fix all of the named ranges. */
    if (rng_base)
	for (r = rng_base; r; r = r->r_next) {
	    r1 = r->r_left.vp->row;
	    c1 = r->r_left.vp->col;
	    r2 = r->r_right.vp->row;
	    c2 = r->r_right.vp->col;

	    if (!(fr && (c1 < fr->or_left->col || c1 > fr->or_right->col))) {
		if (r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
		if (c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
	    }

	    if (!(fr && (c2 < fr->or_left->col || c2 > fr->or_right->col))) {
		if (r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
		if (c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
	    }
	    r->r_left.vp = lookat(r1, c1);
	    r->r_right.vp = lookat(r2, c2);
	}

    /* Next, we go through all valid cells with expressions and fix any ranges
     * that need fixing.
     */
    for (i=0; i<=maxrow; i++)
	for (j=0; j<=maxcol; j++)
	    if ((p = *ATBL(tbl,i,j)) && p->expr)
		fix_enode(p->expr, row1, col2, row2, col2, delta1, delta2);
    fix_frames(row1, col1, row2, col2, delta1, delta2);
    fix_colors(row1, col1, row2, col2, delta1, delta2);
}

void
fix_enode(struct enode *e, int row1, int col1, int row2, int col2,
	int delta1, int delta2)
{
    if (e) {
	if ((e->op & REDUCE)) {
	    int r, c;
	    int r1, c1, r2, c2;
	    struct frange *fr;

	    fr = find_frange(currow, curcol);
	    r1 = e->e.r.left.vp->row;
	    c1 = e->e.r.left.vp->col;
	    r2 = e->e.r.right.vp->row;
	    c2 = e->e.r.right.vp->col;
	    if (r1>r2) r = r2, r2 = r1, r1 = r;
	    if (c1>c2) c = c2, c2 = c1, c1 = c;

	    if (!(fr && (c1 < fr->or_left->col || c1 > fr->or_right->col))) {
		if (r1 != r2 && r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
		if (c1 != c2 && c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
	    }

	    if (!(fr && (c2 < fr->or_left->col || c2 > fr->or_right->col))) {
		if (r1 != r2 && r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
		if (c1 != c2 && c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
	    }
	    e->e.r.left.vp = lookat(r1, c1);
	    e->e.r.right.vp = lookat(r2, c2);

	} else if (e->op != O_VAR && e->op !=O_CONST && e->op != O_SCONST) {
	    fix_enode(e->e.o.left, row1, col1, row2, col2, delta1, delta2);
	    fix_enode(e->e.o.right, row1, col1, row2, col2, delta1, delta2);
	}
    }
}
