/**** RxRsync REXX procedures   **************************************/
/*     rsync_gdiff: uses this synopsis, and the new version, to create
                   and rsync difference file

          status=rsync_gdiff(synopsis_string,newver_file,diff_file,quiet)
          quiet is an optional parameter
*****************************************/

rsync_gdiff: 

parse arg sfile,newverfile,outfile,verbo,only32

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

b=time('r')

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

status=rx_rsync_gdiff(newverfile,synopsis,outfile,only32)

b=time('e')

if verbose>0 then do
    nn=stream(outfile,'c','query size')
    say '   Saving difference file to 'outfile '[elapsed time='||strip(b,'t','0')
end

return status  /* if success, status=OK md4_value */


