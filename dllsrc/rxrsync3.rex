/***************************************************/
/*  rsync_ungdiff: undifference, using a difference file

     status=rsync_undiff(oldver_file,diff_file,newfile,GDMD4,verbo)
        OLDVER_FILE -- old version of the file
        diff_file  -- the gdiff file
        newfile -- name to use for duplicate of "new" file
        gdmd4 -- md4 of "server side" new file -- use as an error check
                 if not specified, then error check will not be done
        verbo -- verbosity of status messages (1 for more)

       gdmd4 and verbo are optional

**************************************/

rsync_ungdiff:
  parse arg afile,dfile,outfile,smd4,verbo

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


if smd4<>'' then do              /* no error check info */
  amd4=rx_md4(astuff)            /* md4 of old version */
/* get md4 and blocksize from s1 */
   if strip(translate(amd4))=strip(translate(smd4)) then do
      if verbose>0 then say "   File has not changed! "
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
     return "ERROR md4 does not match: "b2md4', 'smd4
  end 
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



