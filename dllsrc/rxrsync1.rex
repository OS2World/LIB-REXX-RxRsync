/***** RxRSYNC REXX procedures  *****************************************/
/*  rsync_synopsis: creates a synopsis of an old version.

       status=rsync_synopsis(oldver_file,synopsis_file,comment,quiet,blocksize)
       quiet, blocksize, and comment are optional parameters
*/

rsync_synopsis:

/*********  USER changeable parameters  ************/
/* (larger blocksizes means smaller synopsis, but possibly larger difference */
blocksize=500

quiet=0         /*  default status messages: 1-terse, 0=normal */


/**** END of USER changeable parameters  ************/

parse arg afile,outfile,comment,verbo,bsize

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

