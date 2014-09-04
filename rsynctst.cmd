/* 18 November 1999. Daniel Hellerstein (danielh@crosslink.net)
    RsyncTst: example of how to use the RxRSYNC procedures 
   See RxRSYNC.DOC, or RxRSYNC.CMD, for the details

  Note: this version uses the GDIFF format for the difference file
        (earlier releases of rxrsync used a non-standard format)
        It is quite possible that the format for the "synopsis"
        file will also change in the near future
        (in order to more closely resemble what the unix rsync'ers are doing)
*/

/********** user changable parameters ****/

synopfile='synop.rsy'    /* name to use for synopsis file */
difffile='diff.rsy'     /* name to use for difference file */
quiet=0                /* set to 1 suppress some output */


/********** End of user changable parameters ****/

parse arg oldfile newfile outfile .

say "Test of the RxRSYNC procedures "
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
say "Step 2: create the gdiff-formatted difference file ("difffile
status=rsync_gdiff(synopfile,newfile,difffile,quiet)
say "rsync_gdiff status: "status
if abbrev(status,'OK')<>1 then exit
parse var status aok server_md4

say ' '
say "Step 3: duplicate the new file "outfile
status=rsync_ungdiff(oldfile,difffile,outfile,server_md4,quiet)
say "rsync undiff status: "status

call rxrsyncdrop

exit


/****************** 
   --------- what follows is a copy of RxRsync.REX
******************/

/*******************************************************************/
/*  rsync_synopsis: creates a synopsis of an old version.

       status=rsync_synopsis(oldver_file,synopsis_file,comment,quiet,blocksize)
       quiet, blocksize, and comment are optional parameters
*/

rsync_synopsis:procedure

/*********  USER changeable parameters  ************/
/* (larger blocksizes means smaller synopsis, but possibly larger difference */
blocksize=500

quiet=0         /*  default status messages: 1-terse, 0=normal */


/**** END of USER changeable parameters  ************/

parse arg afile,outfile,comment,verbo,bsize

a=rsync_load_dlls()
if a<>0 then return a

if afile='' then return "ERROR no old-version file specified"
if outfile='' then return "ERROR no synopsis-file specified "

if verbo<>'' then verbose=verbo
if bsize<>'' then do
   if datatype(bsize)='NUM' then blocksize=bsize
end /* do */

select
   when verbose=0 then verbose=1
   when verbose=1 then verbose=0
   otherwise nop
end

reportat=verbose
if verbose<2 then reportat=250000

crlf='0d0a'x
a=time('r')

if comment='' then comment=date('n')||' '||time('n')

/* read "Afile" */
aa=translate(stream(afile,'c','open read'))
if  abbrev(aa,'READY')=0 then return "ERROR could not open "afile
isize=stream(afile,'c','query size')
if isize='' | isize=0 then do
    return 'ERROR 'afile " is unaccessible"
    exit
end
astuff=charin(afile,1,isize)
aa=stream(afile,'c','close')

amd4=rx_md4(astuff)
if verbose>0 then  do
  say "Rsync Client: read "isize" bytes from "afile
end

/* break into chunks of size blocksize, and compute "adler" and md4 checksums */
ifoo=trunc(isize/blocksize+0.999999)

/* Structure of client request file
   Comment  -- 80 characters (i.e.; requested file name)
   1 space
   Blocksize  -- 6 digit integer
   1 space
   #Blocks    -- 8 digit character
   1 space
   md4        -- 32 digit md4
   3 spaces
   rsync1||md41||..||rsyncN||md4||N -- rsync and md4 values (machine format)
*/

ac1=left(comment,80)||' '||left(blocksize,6)||' '||left(ifoo,8)||' '||amd4||'   '
iat=1
do mm=1 to ifoo
  if mm=ifoo then
     ablock=substr(astuff,iat)
  else
     ablock=substr(astuff,iat,blocksize)
  ac1=ac1||x2c(rx_rsync32_md4(ablock))
  iat=iat+blocksize
end
foo=time('e')
if verbose>0 then say '   Done creating hashes for  'ifoo' blocks'

foo=sysfiledelete(outfile)
foo=charout(outfile,ac1,1)          /* create the message the client send to the server*/
if foo<>0 then do
  foo=stream(outfile,'c','close')
  return "ERROR problem writing synopsis file "outfile
end /* do */
foo=stream(outfile,'c','close')
b=time('e')
if verbose>0 then 
   say '   Saving synopsis file to 'outfile' [elapsed time='||strip(b,'t','0')
nn=length(ac1)
drop ac1
return 'OK 'nn ' bytes written to synopsis file 'outfile


/*******************************************************************/
/*     rsync_gdiff: uses a synopsis, and the new version, to create
                   a gdiff formatted difference file

          status=rsync_gdiff(synopsis_string,newver_file,diff_file,quiet)
          quiet is an optional parameter

*****************************************/

rsync_gdiff: procedure

parse arg sfile,newverfile,outfile,verbo

a=rsync_load_dlls()
if a<>0 then return a

if sfile='' then return "ERROR no synopsis file specified "
if newverfile='' then return "ERROR no new_version file specified"
if outfile='' then return "ERROR no difference file specified "

if verbo<>'' then verbose=verbo
select
   when verbose=0 then verbose=1
   when verbose=1 then verbose=0
   otherwise nop
end

crlf='0d0a'x
a=time('r')

/* read "synopsis file" */
aa=translate(stream(sfile,'c','open read'))
if  abbrev(aa,'READY')=0 then return "ERROR could not open "sfile
issize=stream(sfile,'c','query size')
if issize='' | issize=0 then do
    return 'ERROR 'sfile " is unaccessible"
    exit
end
synopsis=charin(sfile,1,issize)
aa=stream(sfile,'c','close')
if verbose>0 then  do
    say "Rsync server: read "issize" bytes in the synopsis file"
end

in1=left(synopsis,132)
parse var in1 comment +80 iblock numblocks amd4 gotcts .


if datatype(iblock)<>'NUM' | datatype(numblocks)<>'NUM' then do
    return 'ERROR not a proper synopsis 'iblock numblocks
end

if verbose>0 then do
  say "   Comment: "||left(comment,64)
  say "            (client used blocksize=" iblock ', and sent 'numblocks' blocks '
end

if stream(newverfile,'c','query exists')='' then  do
  return 'ERROR no such new_version file '||newverfile
end

if verbose>0 then  do
   nn=stream(newverfile,'c','query size')
    say "   Read "nn" bytes in "newverfile
end

/* call the integrated rsync_gdiff procedure */
foo=sysfiledelete(outfile)
if foo>2 then return "ERROR could not delete old version of "outfile

status=rx_rsync_gdiff(newverfile,synopsis,outfile)

b=time('e')

if verbose>0 then do
    nn=stream(outfile,'c','query size')
    say '   Saving difference file to 'outfile '[elapsed time='||strip(b,'t','0')
end

return status


/***************************************************/
/*  rsync_ungdiff: undifference, using a gdiff difference file
                   (that may have have been produced by rsync_gdiff

     status=rsync_ungdiff(oldver_file,diff_file,newfile,GDMD4,verbo)
        OLDVER_FILE -- old version of the file
        diff_file  -- the gdiff file
        newfile -- name to use for duplicate of "new" file
        gdmd4 -- md4 of "server side" new file -- use as an error check
                 if not specified, then error check will not be done

        verbo -- verbosity of status messages

**************************************/

rsync_ungdiff:procedure
  parse arg afile,dfile,outfile,smd4,verbo

a=rsync_load_dlls()
if a<>0 then return a

if afile='' then return "ERROR no old_version file specified"
if dfile='' then return "ERROR no difference-file specified "
if newfile='' then return "ERROR no new_version file specified "

if verbo<>'' then verbose=verbo
select
   when verbose=0 then verbose=1
   when verbose=1 then verbose=0
   otherwise nop
end

crlf='0d0a'x
a=time('r')

/* read "diff Afile" */
aa=translate(stream(dfile,'c','open read'))
if  abbrev(aa,'READY')=0 then return "ERROR could not open "dfile
idsize=stream(dfile,'c','query size')
if idsize='' | idsize=0 then do
    return 'ERROR 'dfile " is unaccessible"
    exit
end
s1=charin(dfile,1,idsize)
aa=stream(dfile,'c','close')
if verbose>0 then  do
    say "Rsync undiff: read "idsize" bytes in the difference file"
end

/* read "Afile" */
aa=translate(stream(afile,'c','open read'))
if  abbrev(aa,'READY')=0 then return "ERROR could not open "afile
iasize=stream(afile,'c','query size')
if iasize='' | iasize=0 then do
    return 'ERROR 'afile " is unaccessible"
    exit
end
astuff=charin(afile,1,iasize)
aa=stream(afile,'c','close')
if verbose>0 then  do
    say "   "iasize" bytes in the old_version file"
end

amd4=rx_md4(astuff)            /* md4 of old version */

/* get md4 and blocksize from s1 */
if smd4<>'' then do
 if strip(translate(amd4))=strip(translate(smd4)) then do
   if verbose>0 then
       say "   File has not changed! "
   bstuff=astuff
   signal writeme1    
 end 
end

/* start building, according to the gdiff format */

/* check for gdiff header (char value of 'd1ffd1ff'x, and version # i.e; '04'x)*/
parse var s1 gheader +4 foover +1  s1
ggh=x2c('d1ffd1ff')
if gheader<>ggh then do
         return 'ERROR in difference file: not a gdiff file: 'c2x(gheader)
end
/* ignore version number */

bstuff=''
noted=0
do forever
   if length(s1)=0 | s1='' then do
       return 'ERROR: eof marker not found '
   end

   mbl=lengtH(bstuff)
   if  mbl-noted> reportat then do
      if verbose>0 then say "... Rsync client: # characters recovered "mbl
      noted=mbl
   end /* do */
   
   parse var s1 atype +1 s1
   itype=c2d(atype)
   select
      when itype=0 then leave   /* 0 signals eof */
      when itype=255 then return 'ERROR gdiff copy operation too large'
      when  itype>0 & itype < 247 then do  /* get itype bytes */
         parse var s1 ccs +(itype) s1
         bstuff=bstuff||ccs
      end /* do */
      when itype=247 then do    /* get <65k bytes */
          parse var s1 jget +2 s1
          jget=c2d(jget)

          parse var s1 ccs +(jget) s1
          bstuff=bstuff||ccs
      end /* do */
      when itype=248 then do    /* get < 2billion bytes */
          parse var s1 jget +4 s1
          jget=c2d(jget)

          parse var s1 ccs +(jget) s1
          bstuff=bstuff||ccs
      end /* do */
      otherwise do   /* copy range of bytes */
         select
           when itype=249 then parse var s1 istart +2 iget +1 s1  
           when itype=250 then parse var s1 istart +2 iget +2 s1
           when itype=251 then parse var s1 istart +2 iget +4 s1
           when itype=252 then parse var s1 istart +4 iget +1 s1
           when itype=253 then parse var s1 istart +4 iget +2 s1
           when itype=254 then parse var s1 istart +4 iget +4 s1
           otherwise return 'ERROR: impossible code in gdiff '
         end
         istart=c2d(istart); iget=c2d(iget)

         bstuff=bstuff||substr(astuff,istart,iget)
      end               /* otherwise range */
   end                  /* gdiff codes */
end                     /* parsing gdiff file */

/* compute md4 of this constructed file */
if smd4<>'' then do 
  b2md4=rx_md4(bstuff)
  if strip(translate(b2md4)) <> strip(translate(smd4)) then do
    say "ERROR md4 does not match: "b2md4', 'smd4
  end /* do */
end

writeme1:   nop                 /* jump here if no change */
foo=sysfiledelete(outfile)
if foo>2 then return 'ERROR unable to delete output file 'outfile
foo=charout(outfile,bstuff,1)          /* save the computed new file */
foo=stream(outfile,'c','close')
b=time('e')
if verbose>0 then
  say '   Saving duplicate to 'outfile ' [elapsed time='||strip(b,'t','0')

nn=length(bstuff)
drop bstuff; drop astuff

return 'OK 'nn ' bytes written to  'outfile


/****************************************************************/
/* read in some useful dlls */
rsync_load_dlls:procedure
if rxfuncquery('rx_md4')=1  then do
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
return 0

