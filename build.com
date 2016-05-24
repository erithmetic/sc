$! VMS command file to build SC and PSC (requires bison) on VMS
$! SC:
$! $Revision: 7.16 $
$! bison -d gram.y
$! ren gram_tab.c gram.c
$ cc'p1'  /define=("SIMPLE","SIGVOID") sc.c
$ cc'p1'  /define=("SIMPLE","SIGVOID") gram.c
$ cc'p1'  /define=("SIMPLE","SIGVOID") lex.c
$ cc'p1'  /define=("SIMPLE","SIGVOID","RINT") interp
$ cc'p1'  /define=("SIMPLE","SIGVOID") cmds
$ cc'p1'  /define=("SIMPLE","SIGVOID") xmalloc
$ cc'p1'  /define=("SIMPLE","SIGVOID") range
$ cc'p1'  /define=("SIMPLE","SIGVOID") help
$ cc'p1'  /define=("SIMPLE","SIGVOID") vmtbl
$ cc'p1'  /define=("SIMPLE","SIGVOID") screen
$ cc'p1'  /define=("SIMPLE","SIGVOID") vi
$ cc'p1'  /define=("SIMPLE","SIGVOID") format
$ link'p1' sc.obj,lex.obj,gram.obj,interp.obj,cmds.obj,xmalloc.obj,-    
       range.obj,help.obj,vmtbl.obj,screen.obj,vi.obj,format.obj,-
       sys$library:vaxccurse.olb/lib,-
       sys$library:vaxcrtl/shar
$ !
$ ! Create VMS foreign command symbol to test SC
$ !
$ sc == "$" + f$logical("SYS$DISK") + f$directory() + "SC.EXE"
$!
$! Now PSC
$!
!$ cc'p1' psc.c
!$ cc'p1' getopt.c
$ link'p1' psc,getopt,vmtbl.obj,xmalloc.obj,screen.obj,vi.obj,format.obj,-    
       sys$library:vaxccurse.olb/lib,-
       sys$library:vaxcrtl/shar
$ !
$ ! Create VMS foreign command symbol to test PSC (Note that
$ ! PSC reads SYS$INPUT and writes to SYS$OUTPUT, so use
$ ! DEFINE/USER to redirect.)
$ !
$ psc == "$" + f$logical("SYS$DISK") + f$directory() + "PSC.EXE"
