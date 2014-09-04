/* The ALL rexx rsync.  This is a lot slower then the OS/2 mixed
 versions that use rxsync.dll (as demoed in rsynctst.cmd and rsyncts2.cmd),
 but it should run on most rexx systems */

/*********  USER changeable parameters  ************/
/* (larger blocksizes mean less info sent in c1, but  more in s1. */
blocksize=500

afile='tst\dir.doc'             

bfile='tst\dirnew.doc'             /* the client's old version */

verbose=1                      /* status messages: 0-terse, 1=normal, 2=verbose */

reportat=250000                 /* how often to report some status messages */


/**** END of USER changeable parameters  ************/

parse arg infile outfile



if infile<>'' then afile=infile
if outfile<>'' then bfile=outfile

s1file='rxsync.dif'         /* server's response */
c1file='rxsync.syn'         /* clients requeset */
B1file='rxsync.out'         /* client side reconstruction of B1 (i.e.; a copy of B */

numeric digits 11

if infile='?' then do
    say "RxSYNC: an all REXX rsync (for demonstration purposes). "
    say "Usage: x:>rxsync infile outfile  "
    say " Will generate: "
    say "   Client side 'synopsis': "c1file
    say "   Server side 'difference':" s1file
    say "   Client side duplicate of outfile: "b1file
    exit
end /* do */



/* Load up advanced REXX functions -- for non os/2 systems, you
may have to provide a different library name */
call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
call SysLoadFuncs




/******** The client's request, along with the c1 message ***/

a=time('r')
/* read "Afile" */
isize=stream(afile,'c','query size')
if isize='' | isize=0 then do
    say 'Rsync Client: 'afile " is unaccessible"
    exit
end

astuff=charin(afile,1,isize)
say "Rsync Client: read "isize" bytes from "afile
say " ... computing md4 "
amd4=rexx_md4(astuff)

crlf='0d0a'x
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


ac1=left(bfile,80)||' '||left(blocksize,6)||' '||left(ifoo,8)||' '||amd4||' '
ctcs='  '               /* may use this "parameter" later */
ac1=ac1||ctcs
iat=1
do mm=1 to ifoo
  if mm=ifoo then
     ablock=substr(astuff,iat)
  else
     ablock=substr(astuff,iat,blocksize)
  foo=rsync32_md4(ablock)
  ac1=ac1||foo
  iat=iat+blocksize
  if (iat//20000)=1 then say "processing " iat
end
foo=time('e')
say 'Rsync Client: Done creating hashes for  'ifoo' blocks'

foo=sysfiledelete(c1file)
foo=charout(c1file,ac1,1)          /* create the message the client send to the server*/
foo=stream(c1file,'c','close')
b=time('e')
say 'Rsync client: Saving client request to 'c1file' [elapsed time='||strip(b,'t','0')
drop ac1

/***************** This is the server's action ***/
/* Read in in C1 */
aaa:

b=time('r')

csz=stream(c1file,'c','query size')
foo=stream(c1file,'c','open read')
in1=charin(c1file,1,132)
parse var in1 getfile iblock numblocks amd4 gotcts .

say ' '
say "Rsync Server: reads "c1file", and sees that client requested: "
say "     "getfile
say "     (client used blocksize=" iblock ', and sent 'numblocks' blocks '

/* read in Bfile */
isize=stream(bfile,'c','query size')
if isize='' | isize=0 then do
    say 'Rsync server: ' bfile " is unaccessible"
    exit
end
astuff=charin(bfile,1,isize)
bmd4=rexx_md4(astuff)
say 'Rsync server: 'isize' bytes from requested file read '
if strip(translate(amd4))=translate(strip(bmd4)) then do
   say "Rsync Server: Files have not changed."
   saveb=0
   signal jump1
end /* do */

/* store 32bit hashes in stem.
  Note that the rsync specs suggest a 16 bit hash table, which
  provides an index into a sorted (by 32 bit checksum) array.  
  However, here we take advantage of the balanced tree architecture
  uses in rexx stem variables */
hasht.=0
mxlen=1
nhash=0
do mm=1 to numblocks
   aline=charin(c1file,,20)
   adler=left(aline,4) ; md4=right(aline,16)
   akey='!'||adler
   ijk=hasht.akey+1
   if ijk=1 then nhash=nhash+1
   hasht.akey=ijk
   hasht.akey.!m.ijk=md4
   hasht.akey.!i.ijk=mm
   mxlen=max(mxlen,ijk)
end /* do */

say 'Rsync server: created ' nhash||' entries in hash table with max keys= 'mxlen


/* commence rolling through astuff, starting from first character */
/* for now, if not 500 chars left in file, then stop */

nom1=0      /* last character accounted for (explicitily, or as a block */
nom2=0     /* last character unmatched, and not written to s1 */
lenstuff=length(astuff)

ib1=1 ; ib2=ib1+iblock          /* start of this block, first char after this block */
ablock=substr(astuff,ib1,iblock)         /* initialize Bfile stuff */
adler=rsync32(ablock)         /* alpha and beta are expose */
adler=d2c(adler)

s1=bmd4' 'iblock||crlf                /* the server's report --start with md4 of Bfile */
blockwrites=0; charwrites=0

noted=0
bad32s=0
saveb=0
do while ib1 <= 1+lenstuff-iblock   /* stop when can't grab a block */

  if ib1-noted>reportat=1 then do
    if verbose>0 then say " ... Rsync server: At character # " ib1 '('charwrites','blockwrites')'
    noted=ib1
  end
   
  akey='!'||adler            /* check hash table */
  matchblock=0                          /* assume no matching block */
  do jkk=1 to hasht.akey                /* check all entries with this 32bit hash */
     if jkk=1 then ablock=substr(astuff,ib1,iblock)
     tmd4=x2c(strip(rexx_md4(ablock)))
     if tmd4=hasht.akey.!m.jkk then do       /* md4 match. use this block */
         matchblock=hasht.akey.!i.jkk
         blockwrites=blockwrites+1
         leave
     end             /* md4 check */
     else do
         bad32s=bad32s+1
     end /* do */

  end                   /* all matches of 16 bit hash of adler 32 */

/* if no match, slide block over 1, compute it's adler, and back to top of loop */
  if matchblock=0 then do         /* no match....try next block of Bfile */
      nom2=ib1                    /* "end" of current set of unmatched characters*/
      newchar=substr(astuff,ib2,1)   /* character to add to create next block */

      oldchar=substr(astuff,ib1,1)
      ib1=ib1+1                     /* begin next block here */
      ib2=ib2+1                     /* first char after this block */
/* COMPUTE ROLLING CHECKSUM */
      IOLDCHAR=C2D(OLDCHAR); INEWCHAR=C2D(NEWCHAR)
      alpha=alpha- Ioldchar + Inewchar
      if alpha<0 then  alpha=65536+alpha
      xalpha=right(d2x(alpha),4,0)
      alpha=x2d(xalpha)
      beta=beta-(iblock*ioldchar)+alpha
      if beta<0 then  beta=65536+(beta//65536)
      iadler=right(d2x(beta),4,0)||xalpha
      ADLER=right(x2c(iadler),4)

/*      adler=rexx_add_sync(c2d(oldchar),c2d(newchar))  alpha beta exposed */

      iterate                   /* try next comparision, using new adler checksum */

  end /* do */

/* if here, got a match. */
/* Record set of unmatched chars (that precede this  block) */
   if nom2>0 then do    /* there are some unmatched character before this matching block */
      ndo=nom2-nom1
      charwrites=charwrites+ndo
      if verbose> 1 then say "... Rsync server: record "ndo " chars starting after "nom1
      saveb=saveb+1
      saves.saveb.1='C'
      saves.saveb=ndo':'||substr(astuff,nom1+1,ndo)  /* record unmatched chars */
   end
/* record this matching block */
   saveb=saveb+1
   saves.saveb.1='B'
   saves.saveb=matchblock                     /* record matching block id */
   if verbose>1 then say "... Rsync server: recording block: "matchblock '(with adler 'adler
/* skip past this block, and start searching again */
   ib1=ib1+iblock
   nom1=ib1-1                   /* last "matched" character */
   nom2=0                       /* end of "unmatched characters */
   ablock=substr(astuff,ib1,iblock)         /* get block after */
   adler=rsync32(ablock)         
   adler=d2c(strip(adler))
   ib2=ib1+iblock         

end /* do */

say 'Rsync server:  done comparing blocks '
/* add any "unmatched characters */
if nom1<lenstuff then do
     ndo=lenstuff-nom1
     charwrites=charwrites+ndo
     saveb=saveb+1
     saves.saveb.1='C'
     saves.saveb=ndo':'||substr(astuff,nom1+1)  /* record unmatched chars */
end

say "Rsync server: "charwrites "characters, "blockwrites " blocks"
if verbose>1 then say "Rsync server: # of nonmatching 32bit checksums: "bad32s

jump1: nop
foo=sysfiledelete(s1file)

/* now write saves. array to output file; possibly appending B blocks */
mm=0
foo=stream(s1file,'c','open write')
oout=bmd4' 'iblock||crlf
foo=charout(s1file,oout,1)
do until mm >= saveb
  mm=mm+1
  if saves.mm.1='C' then do
     oout='C'||saves.mm||crlf
     foo=charout(s1file,oout)
  end
  if saves.mm.1='B' then do 
     ib1=saves.mm ; ib2=ib1 
     imm0=mm+1
     do mm2=imm0 to saveb
        if saves.mm2.1<>'B'  then leave
        if saves.mm2-1<>ib2 then leave   /* not next in squesnce */
        ib2=saves.mm2 
        mm=mm2
     end
     oout='B'||ib1':'ib2||crlf
     foo=charout(s1file,oout)
  end /* if */
end /* do */

foo=stream(s1file,'c','close')
b=time('e')

say 'Rsync server: Writing the 's1file ' server response file [elapsed time='||strip(b,'t','0')
drop astuff
drop saves.

/*********** Client recieves response, and assembles Bfile */

b=time('r')

say " "
/* read in s1 */
cisize=stream(s1file,'c','query size')
if isize='' | isize=0 then do
    say 'Rsync Client: Server response file ('s1file' is unaccessible'
    exit
end
s1=charin(s1file,1,cisize)

say "Rsync Client: read "cisize" bytes of server response "
/* read "Afile" */
iasize=stream(afile,'c','query size')
if isize='' | isize=0 then do
    say afile "Rsync Client: (the client's 'A' file) is unaccessible!"
    exit
end
astuff=charin(afile,1,iasize)
amd4=rexx_md4(astuff)

/* get md4 and blocksize from s1 */
parse var s1 smd4 bsize (crlf) s1

if strip(translate(amd4))=strip(translate(smd4)) then do
   say "Rsync Client: file has not changed "
   exit
end /* do */


/* start building. Each records starts with a single character identifier,
either a B or a C. Following the identifier is:
   B: a block start: block end number, and then a crlf
   C: a count of bytes (nnn), a ":", a string of length nnn, and a crlf
*/
bstuff=''
noted=0
do forever
   if length(s1)=0 | s1='' then leave
   mbl=lengtH(bstuff)
   if  mbl-noted> reportat then do
      if verbose>0 then say "... Rsync client: # characters recovered "mbl
      noted=mbl
   end /* do */
   parse var s1 atype +1 s1  ; atype=translate(atype)
   select
      when atype='C' then do
         parse var s1 nnn ':' s1
         parse var s1 ccs +(nnn) (crlf) s1
         bstuff=bstuff||ccs
      end /* do */
      when atype='B' then do
         parse var s1 idb1 ':' idb2 (crlf)  s1
         i1=((idb1-1)*bsize)+1
         i2=idb2*bsize
         i2=min(i2,iasize)
         bstuff=bstuff||substr(astuff,i1,1+i2-i1)
      end /* do */
      otherwise do
         say "ERROR in rsync response: unknown type= "atype
         exit
     end
   end  /* select */
end /* do */

/* compute md4 of this constructed file */
b2md4=rexx_md4(bstuff)
if strip(translate(b2md4)) <> strip(translate(smd4)) then do
   say "Rsync Client: ERROR: md4 does not match: "b2md4', 'smd4
end /* do */
foo=sysfiledelete(bb1file)
foo=charout(b1file,bstuff,1)          /* save the computed Afile */
foo=stream(b1file,'c','close')
b=time('e')
say 'Rsync client: ' length(bstuff) ' bytes written to  'b1file ' [elapsed time='||strip(b,'t','0')
exit


/**********************************/
/* some useful procedures */


/*  ------------------------------ */
/* this is an "all rexx" md4 procedure. It works, but it is slow */
rexx_md4:procedure             /* if called externally, remove the "procedure" */
parse arg stuff
lenstuff=length(stuff)

c0=d2c(0)
c1=d2c(128)
c1a=d2c(255)
c1111=c1a||c1a||c1a||c1a
slen=length(stuff)*8
slen512=slen//512

const1=c2d('5a827999'x)
const2=c2d('6ed9eba1'x)


/* pad message to multiple of 512 bits.  Last 2 words are 64 bit # bits in message*/
if slen512=448 then  addme=512
if slen512<448 then addme=448-slen512
if slen512>448 then addme=960-slen512
addwords=addme/8

apad=c1||copies(c0,addwords-1)

xlen=reverse(right(d2c(lenstuff*8),4,c0))||c0||c0||c0||c0  /* 2**32 max bytes in message */

/* NEWSTUFF is the message to be md4'ed */
newstuff=stuff||apad||xlen

/* starting values of registers */
 a ='67452301'x;
 b ='efcdab89'x;
 c ='98badcfe'x;
 d ='10325476'x;

lennews=length(newstuff)/4

/* loop through entire message */
do i1 = 0 to ((lennews/16)-1)
  i16=i1*64
  do j=1 to 16
     j4=((j-1)*4)+1
     jj=i16+j4
     m.j=reverse(substr(newstuff,jj,4))
  end /* do */

/* transform this block of 16 chars to 4 values. Save prior values first */
 aa=a;bb=b;cc=c;dd=d

/* do 3 rounds, 16 operations per round (rounds differ in bit'ing functions */
S11=3
S12=7 
S13=11
S14=19
  a=round1( a, b, c, d,   0 , S11); /* 1 */
  d=round1( d, a, b, c,   1 , S12); /* 2 */
  c=round1( c, d, a, b,   2 , S13); /* 3 */
  b=round1( b, c, d, a,   3 , S14); /* 4 */
  a=round1( a, b, c, d,   4 , S11); /* 5 */
  d=round1( d, a, b, c,   5 , S12); /* 6 */
  c=round1( c, d, a, b,   6 , S13); /* 7 */
  b=round1( b, c, d, a,   7 , S14); /* 8 */
  a=round1( a, b, c, d,   8 , S11); /* 9 */
  d=round1( d, a, b, c,   9 , S12); /* 10 */
  c=round1( c, d, a, b,  10 , S13); /* 11 */
  b=round1( b, c, d, a,  11 , S14); /* 12 */
  a=round1( a, b, c, d,  12 , S11); /* 13 */
  d=round1( d, a, b, c,  13 , S12); /* 14 */
  c=round1( c, d, a, b,  14 , S13); /* 15 */
  b=round1( b, c, d, a,  15 , S14); /* 16 */

  /* Round 2 */
S21=3
S22=5
S23=9 
S24=13
a= round2( a, b, c, d,   0 ,  S21 ); /* 17 */
d= round2( d, a, b, c,   4 ,  S22 ); /* 18 */
c=  round2( c, d, a, b,  8 , S23); /* 19 */
b=  round2( b, c, d, a,  12 , S24); /* 20 */
a=  round2( a, b, c, d,   1 , S21); /* 21 */
d=  round2( d, a, b, c,  5  , S22); /* 22 */
c=  round2( c, d, a, b,  9  , S23); /* 23 */
 b= round2( b, c, d, a,   13,  S24); /* 24 */
a= round2( a, b, c, d,   2 ,  S21); /* 25 */
d= round2( d, a, b, c,  6  ,  S22); /* 26 */
c=  round2( c, d, a, b,  10 , S23); /* 27 */
b=  round2( b, c, d, a,  14 , S24); /* 28 */
a=  round2( a, b, c, d,   3 , S21); /* 29 */
d=  round2( d, a, b, c,   7 , S22); /* 30 */
c=  round2( c, d, a, b,  11 , S23); /* 31 */
b= round2( b, c, d, a,  15 ,  S24) ; /* 32 */

  /* Round 3 */
S31= 3
S32= 9 
S33= 11
S34= 15
a= round3( a, b, c, d,   0 , S31) ; /* 33 */
d=  round3( d, a, b, c,   8 , S32); /* 34 */
c=  round3( c, d, a, b,  4  , S33); /* 35 */
b=  round3( b, c, d, a,  12 , S34); /* 36 */
a=  round3( a, b, c, d,   2 , S31); /* 37 */
d=  round3( d, a, b, c,  10 , S32); /* 38 */
c=  round3( c, d, a, b,   6 , S33); /* 39 */
b=  round3( b, c, d, a,  14 , S34); /* 40 */
a=  round3( a, b, c, d,  1  , S31); /* 41 */
d=  round3( d, a, b, c,   9 , S32); /* 42 */
c=  round3( c, d, a, b,   5 , S33); /* 43 */
b=  round3( b, c, d, a,  13 , S34); /* 44 */
a=  round3( a, b, c, d,   3 , S31); /* 45 */
d=  round3( d, a, b, c,  11 , S32); /* 46 */
c=  round3( c, d, a, b,   7 , S33); /* 47 */
b=  round3( b, c, d, a,  15 , S34); /* 48 */


a=m32add(aa,a) ; b=m32add(bb,b) ; c=m32add(cc,c) ; d=m32add(dd,d)

end

aa=c2x(reverse(a))||c2x(reverse(b))||c2x(reverse(C))||c2x(reverse(D))
return aa

/* round 1 to 3 functins */

round1:procedure expose m. c1111 c0 c1
parse arg a1,b1,c1,d1,kth,shift
kth=kth+1
t1=c2d(a1)+c2d(f(b1,c1,d1))+ c2d(m.kth) 
t1a=right(d2c(t1),4,c0)
t2=rotleft(t1a,shift)
return t2

round2:procedure expose m. c1111 c0 c1 const1
parse arg a1,b1,c1,d1,kth,shift
kth=kth+1
t1=c2d(a1)+c2d(g(b1,c1,d1))+ c2d(m.kth) + const1
t1a=right(d2c(t1),4,c0)
t2=rotleft(t1a,shift)
return t2

round3:procedure expose m. c1111 c0 c1 const2
parse arg a1,b1,c1,d1,kth,shift
kth=kth+1
t1=c2d(a1)+c2d(h(b1,c1,d1))+ c2d(m.kth) + const2 
t1a=right(d2c(t1),4,c0)
t2=rotleft(t1a,shift)
return t2

/* add to "char" numbers, modulo 2**32, return as char */
m32add:procedure expose c0 c1 c1111
parse arg v1,v2
t1=c2d(v1)+c2d(v2)
t2=d2c(t1)
t3=right(t2,4,c0)
return t3



/*********** Basic functions */
/* F(x, y, z) == (((x) & (y)) | ((~x) & (z))) */
f:procedure expose c0 c1 c1111 
parse arg x,y,z
t1=bitand(x,y)
notx=bitxor(x,c1111)
t2=bitand(notx,z)
return bitor(t1,t2)

/* G(x, y, z) == (((x) & (y)) | ((x) & (Z))|  ((y) & (z)) */
g:procedure expose c0 c1 c1111
parse arg x,y,z
t1=bitand(x,y)
t2=bitand(x,z)
t3=bitand(y,z)
t4=bitor(t1,t2)
return bitor(t4,t3)

/* H(x, y, z) == ((x) ^ (y) ^ (z)) */
h:procedure expose c0 c1 c1111
parse arg x,y,z
t1=bitxor(x,y)
return bitxor(t1,z)


/* bit rotate to the left by s positions */
rotleft:procedure 
parse arg achar,s
if s=0 then return achar

bits=x2b(c2x(achar))
lb=length(bits)
t1=left(bits,s)
t2=bits||t1
yib=right(t2,lb)
return x2c(b2x(yib))


/***/
/* comupte checksum using alpha and beta */
rsync32:procedure expose alpha beta 
parse arg mess
asum=0 ; asum2=0
l=length(mess)
do i=1 to l
   v1=c2d(substr(mess,i,1))
   asum=asum+v1
   asum2=((1+l-i)*v1) + asum2
end /* do */
alpha=asum // 65536
beta=asum2 // 65536
chek=alpha+ (65536*beta)

return chek


/*********************/
/* return checksum|md4, in character format */
rsync32_md4:procedure 
parse arg ablock

aa=rsync32(ablocK)
aa=d2c(aa)
c32=right(aa,4)

cmd4=x2c(rexx_md4(ablock))

return c32||cmd4


