/* 18 November 1999. Daniel Hellerstein (danielh@crosslink.net)
    RsyncTst: example of how to use the RxRSYNC procedures 
    This version uses macrospace, and use REXXLIB procedures.
   See RxRSYNC.DOC for the details

  Note: this version uses the GDIFF format for the difference file
        (earlier releases of rxrsync used a non-standard format)
        It is quite possible that the format for the "synopsis"
        file will also change (in order to more closely resemble
        what the unix rsync'ers are doing)

*/


/* load the various DLL and macrospace libraries */
a=load_libs()
if a<>0 then do
   say a                /* an error message */
   exit   /* 0 means "okay" */
end

synopfile='synop.rsy'    /* name to use for synopsis file */
difffile='diff.rsy'     /* name to use for difference file */
quiet=0                /* set to 1 suppress some output */

parse arg oldfile newfile outfile .

say "Test of the RxRSYNC procedures (macrospace version) "
if oldfile='' | oldfile='?' | newfile='' then do
   say "Call using: "
   say "   RSYNCtst oldfile newfile outfile "
   say " where: "
   say "     oldfile: the old version  of a file"
   say "     newfile: the new version of a file "
   say "     outfile: name to use for the the duplicate of the newfile"
   say " In addition, 'synopfile' and 'difffile ' will be created "
   exit
end /* do */

if outfile='' then outfile='RSYNCTST.OUT'

say " "
say "Step1: create the synopsis file ("synopfile
status=rsync_synopsis(oldfile,synopfile,'This is a test of rsync',quiet)
say "rsync client status: " status
if abbrev(status,'OK')<>1 then exit

say ' '
say "Step 2: create the difference file ("difffile
status=rsync_gdiff(synopfile,newfile,difffile,quiet)
say "rsync server status: "status
if abbrev(status,'OK')<>1 then exit
parse var status ok smd4
say ' '

say "Step 3: duplicate the new file ("outfile
status=rsync_ungdiff(oldfile,difffile,outfile,smd4,quiet)
say "rsync undiff status: "status

exit


/****************************************************************/
/* read in some useful procedures */
load_libs:procedure

if rxfuncquery('rx_rsync32')=1  then do
  call RXFuncAdd 'RXRsyncLoad', 'RXRSYNC', 'RxRsyncLoad'
  call RxRsyncLoad
end
if rxfuncquery('rx_md4')=1  then do
  return "ERROR could not load  RxRsync.DLL"
end

/* Load up advanced REXX functions */
foo=rxfuncquery('sysloadfuncs')
if foo=1 then do
  call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
  call SysLoadFuncs
end
if rxfuncquery('sysfiledelete')=1  then do
  return "ERROR could not load  REXXUTIL.DLL"
end

foo=rxfuncquery('rexxlibregister')
if foo=1 then do
 call rxfuncadd 'rexxlibregister','rexxlib', 'rexxlibregister'
 call rexxlibregister
end
foo=rxfuncquery('rexxlibregister')
if foo=1 then do
  return "ERROR could not load  REXXLIB.DLL"
end

if macroquery('rsync_synopsis')='' then do
   foo=macroload('rxrsync.rxl')
   if foo=0 then return "ERROR could not load RxRSYNC.RXL"
end /* do */

return 0




