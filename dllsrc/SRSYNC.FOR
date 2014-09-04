c ntegrated server component of rsync; for use as a rexx callable dll 
c call as:
c  sfile=rx_rsync_GDIFF(targetfile,synopsis,diffile,[first32])
c     where: 
c       targetfile: the desired file 
c       synopsis: a string containing the "synopsis" as provided by the client
c       diffile: filename to use for for the "difference" file (the
c                  difference between targetfile and synposis)
c       first_32: optional. If 1, then only compare first 32bits (4 bytes)
c                           of the md4 (this is used by the http variant
c                           of rsync.
c    returns
c         OK status message -- if  success
c         ERROR error message -- if failure

c$define INCL_REXXSAA
c$include rexxsaa.fap

! Declare our exported function.  Export it!

c$pragma aux (RexxFunctionHandler) RX_RSYNC_GDIFF "RX_RSYNC_GDIFF"

! RX_RSYNC_GDIFF: compute a server response to an rsync request


c$noreference
	integer function RX_RSYNC_GDIFF( name, numargs, args,
     &                            queuename, retstr )
c$reference
	character*(*) name, queuename
	integer numargs
	record /RXSTRING/ retstr, args(numargs)
	
	include 'rxsutils.fi'
	
	integer memstat
	character*(*) desfile,cc1buffer,outfile

        include 'fsublib.fi'
        character *128 acomment
        character *1 asp1
        character *2 crlf
        character *4 ctmp4
        character  *8 ctmp8
        character *7 absize
        character *9 anblocks
        character *32 amd4,srx_md4_rsynca,desmd4
        character *32 ctmp32
        character *16 ctmp16

        character *20 aline20
        integer mm,ierr,ierrout,ioerror,iserror,usebsize

        character *(*) allstuff,tmp,tmp2

        integer hasht(0:65536)

        character *10 t1     
        character *5 t2      
                
        integer rsyncs16(:)
        integer rsyncs(:)
        integer index_rsyncs16(:)
        character *16 md4s(:)

        integer nblocks,blocksize,ll
        integer dessize,matchblock
        integer ib1,ib2,ialpha,ibeta,chksum
        integer rsync_increment_s
        integer ihasht,i1

        logical qfirst_32        

        character *1 wastype
        integer was0,was1,blockwrites,charwrites
        common /ccache/wastype,was0,was1,crlf,blockwrites,charwrites


c allocate space for status messages
	allocate( tmp*256, stat=memstat )
	if( memstat .ne. 0 ) then
 	    RX_RSYNC_GDIFF = 445
            deallocate( tmp,stat=ierr )
	    return
	endif

        tmp='O'
        rx_rsync_GDIFF=0
        wastype=' '
        blockwrites=charwrites=0
        qfirst_32=.false.

c read arguments
        if (numargs.lt.3.or.numargs.gt.4) then 
c	    RX_RSYNC_GDIFF = 443
            TMP='ERROR must specify 3 or 4 arguments'
            goto 500
	endif

        if (numargs.eq.4) then 
  	    allocate(tmp2*args(4).strlength, location=args(4).strptr )
            if (tmp2.eq.'1') then 
               qfirst_32=.true.
            endif
            deallocate(tmp2)
        endif

c read the "targetfile" 
	allocate( desfile*args(1).strlength, location=args(1).strptr )
        if (desfile.eq.' ') then
c	    RX_RSYNC_GDIFF = 443
            tmp='ERROR desired file not specified '
            goto 500
	endif

c read the "targetfile" 
	allocate(outfile*args(3).strlength, location=args(3).strptr )
        if (outfile.eq.' ') then
c	    RX_RSYNC_GDIFF = 443
            tmp='ERROR output file not specified '
            goto 500
	endif

c read the client request info
       allocate(cc1buffer*args(2).strlength, location=args(2).strptr )
                     
c do some initializations
        do mm=0,65535
               hasht(mm)=0
        enddo
        crlf(1:1)=char(13)
        crlf(2:2)=char(10)

c read in the parametrs
        acomment=cc1buffer(1:80)
        absize=cc1buffer(82:88)
        anblocks=cc1buffer(89:97)
        amd4=cc1buffer(98:129)
c 130=space, 131 132=cr lf

        read(absize,*,err=2024)blocksize
        read(anblocks,*,err=2024)Nblocks

c create some storage space
        allocate(rsyncs(1:nblocks))
        allocate(index_rsyncs16(1:nblocks))
        allocate(md4s(1:nblocks))
        allocate(rsyncs16(1:nblocks))

c read in nblocks rsync32~md4 pairs
        do mm=1,nblocks
            i1=133+ ((mm-1)*20)
            aline20=cc1buffer(i1:i1+19)

            write(ctmp8,68,err=2024)aline20(1:4)
 68         format(z8)
            read(ctmp8,68,err=2024)rsyncs(mm)
           
            md4s(mm)=aline20(5:20)

            write(ctmp4,66,err=2024)aline20(1:2)
 66         format(z4)
            read(ctmp4,66,err=2024)rsyncs16(mm)
        enddo

c sort each record (sort by leftmost 16 bit hash, as stored in rsyncs16) 
        call rsync_indexsort(rsyncs16,index_rsyncs16,nblocks)

c  now make the 65k hash table 
        call make_hashtable(nblocks,index_rsyncs16,rsyncs16,hasht)

c now read the inputfile
        open(unit=21,file=desfile,access='sequential',status='old',
     1 form='unformatted',recordtype='fixed',err=2000,action='read')
        rewind(unit=21,iostat=ierr)
        if (ierr.ne.0) then
           tmp='ERROR could not rewind desired file '//
     1            desfile(1:lentrim(desfile))
           goto 500
        endif

        dessize=filesize(21)
        allocate (allstuff*dessize)
        read(21,end=2040)allstuff            !read everythin into a big string

        desmd4=srx_md4_rsynca(allstuff)  !md4 of desired file

c open unit 22 for output
        open(unit=22,file=outfile(1:lentrim(outfile)),
     1     access='sequential',
     1     form='unformatted',recordtype='fixed',err=2110,iostat=mm,
     1     action='write')

c write gdidff header

       t1='d1ffd1ff04'      
       read(t1,13)t2        
13     format(z10)          

          asp1=' '
        write(22,err=2130,iostat=ioerror)t2


c set up stuff to prance through
        ib1=1
        ib2=ib1+blocksize
        call rsync_compute(allstuff(1:min(dessize,blocksize)),
     1                     chksum,ialpha,ibeta)

c now start the waltz
c from ith char, look for matching block
c if not found, write the character
c if found, write the character
c note that writeit will "cache" results

        do while (ib1.le.dessize)  ! stop when all chars examined
           matchblock=0
c check the check16 against the hashtable, etc.
          ihasht=hasht(ibeta)
          if (ihasht.Ne.0) then
              DO LL=IHASHT,NBLOCKS
                 mm=index_rsyncs16(LL)
                 IF (RSYNCS16(MM).NE.IBETA) EXIT  !NO MATCH
                 IF (RSYNCS(MM).NE.CHKSUM) CYCLE  !TRY NEXT 32BIT HASH
                ctmp32=srx_md4_rsynca(allstuff(ib1:min(dessize,ib2-1)))
                 read(ctmp32,133,err=2024)ctmp16      !try the 128 bit hash
 133             format(z32)
                 if (qfirst_32) then     ! just look at  first 32 bits?
                     if (ctmp16(1:4).ne.md4s(mm)(1:4)) cycle
                 else 
                     if (ctmp16.ne.md4s(mm)) cycle    !look at 128 bits  
                 endif
                 matchblock=mm
                 usebsize=1+min(dessize,ib2-1)-ib1
                 exit
              enddo
          endif                 !hash table match

c no match? record this character, and roll the rsync ...
          if (matchblock.eq.0) then            !not a matching block 
               call rsync_writeit('C',ib1,allstuff,iserror,0,0)
               if (iserror.ne.0) goto 2230
            
c COMPUTE ROLLING CHECKSUM (also save alpha,beta
            if (ib2.le.dessize) then
                chksum=rsync_increment_s(allstuff(ib1:ib1),
     1                          allstuff(ib2:ib2),
     1                          ialpha,ibeta,blocksize)
               ib1=ib1+1                   ! begin next block here 
               ib2=ib2+1
            else
                ib1=ib1+1                   ! begin next block here 
                call rsync_compute(allstuff(ib1:dessize),chksum,
     1                               ialpha,ibeta)
                ib2=dessize+1
            endif
            cycle                       !get next block
         endif                          !  not a match

c if here, got a rsync16, 32, and md4 match. So write some results

         call rsync_writeit('B',matchblock,allstuff,iserror,blocksize,
     1                       usebsize)
               if (iserror.ne.0) goto 2230


         ib1=ib1+blocksize     ! skip past this block, and start searching again 
         ib2=ib1+blocksize      
         call rsync_compute(allstuff(ib1:min(dessize,ib2-1)),
     1                      chksum,ialpha,ibeta)

       enddo            !scan over all characters 

c if here, eof (or within a block of eof )
c        print *,' ...   done comparing blocks '
        
c add any "unmatched characters 
        call rsync_writeit('F',0,allstuff,iserror,0,0)
               if (iserror.ne.0) goto 2230

        WRITE(tmp,313)desmd4
 313    FORMAT('OK ',a)

	RX_RSYNC_GDIFF = VALID_ROUTINE
 500    continue

        call write_int(0,1)   ! close gdiff file
        
        close (unit=22,iostat=ierrout)


        deallocate (cc1buffer,stat=ierr )  
        deallocate(allstuff,stat=ierr )
        deallocate(rsyncs,stat=ierr )
        deallocate(md4s,stat=ierr )
        deallocate(outfile,stat=ierr)
        deallocate(desfile,stat=ierr)
        deallocate(rsyncs16,stat=ierr )
        deallocate(index_rsyncs16,stat=ierr )

        if (ierrout.ne.0) then
          write(tmp,171)ierrout,outfile(1:lentrim(outfile))
 171     format('ERROR  error ',i6,' writing output file 'a)
c           RX_RSYNC_GDIFF=4466
        endif
	call CopyResult( tmp,lentrim(tmp), retstr )
	deallocate( tmp ,stat=ierr )
	

        return


c ------ status and errors
 2000   continue
       write(TMP,*)'ERROR Could not read desired file:',
     1       desfile(1:lentrim(desfile))
c       RX_RSYNC_GDIFF = 4461
       goto 500

 2024   continue
       write(TMP,*)'ERROR error reading '
c       RX_RSYNC_GDIFF = 4461
       goto 500


 2040   continue
         write(TMP,*)'ERROR Premature end of desired file: ',
     1       desfile(1:lentrim(desfile))

c       RX_RSYNC_GDIFF = 4462
       goto 500

 2110   continue
       write(TMP,*)'ERROR could not open output file: ',
     1        outfile(1:lentrim(outfile))
c       RX_RSYNC_GDIFF = 4463
       goto 500

 2130   continue
        write(tmp,*)'ERROR writing to output file ',ioerror
c       RX_RSYNC_GDIFF = 4463
       goto 500


 2230   continue
        write(TMP,*)'ERROR writing to output file. ',iserror
c       RX_RSYNC_GDIFF = 4463
       goto 500

      end       


c-------------------------------------------
c write to output file using gdiff format
        subroutine rsync_writeit(atype,iat,dastring,iserror,
     1                           bsize,bsize1)
        character *(*)dastring,atype
        integer iat,iserror ,bsize,bsize1  
        integer ndo,ns1
        integer jas1,jassize

        character *2 crlf
        character *1 wastype
        integer was0,was1
        integer blockwrites,charwrites
        common /ccache/wastype,was0,was1,crlf,blockwrites,charwrites

        iserror=0

c       print *,atype,iat,bsize
        if (wastype.eq.' ') then          ! first call  
          if (atype.eq.'C') then
              was0=was1=iat
          else
              was0=((iat-1)*bsize)+1
              was1=was0+bsize1-1
           endif
           wastype=atype
           return
        endif

c case 1: C,B
        if (atype.ne.'C'.and.wastype.eq.'C') then  ! flush chas, start new block 
          ndo=1+was1-was0               
          if (ndo.lt.247) then
             call write_int(ndo,1)
c       print *,' <247',ndo

          else
             if (ndo.lt.65536) then      ! 2 bytes
                call write_int(247,1)
                call write_int(ndo,2)
c       print *,' 247 ',ndo
             else
                call write_int(248,1)
                call write_int(ndo,4)
c       print *,' 248 ',ndo

             endif
          endif
          write(22,err=10,iostat=iserror)dastring(was0:was1)
          CHARWRITES=CHARWRITES+ndo
          wastype=atype         !not strictly needed if "F"
          was0=((iat-1)*bsize)+1
          was1=was0+bsize1-1
          return
      endif

c case 2: C,C
      if (atype.eq.'C'.and.wastype.eq.'C') then        !augment a characters run
             was1=iat
             return
      endif
        
c case 3: B,B
      if (atype.eq.'B'.and.wastype.eq.'B') then    !augment a block run, or flush 
         ns1=(iat-1)*bsize
         if (ns1.eq.was1) then               !augment
             was1=was1+bsize1
             return
         endif

         jas1=was0       ! record a copy range
         jassize=1+was1-was0
         
         if (jas1.lt.65536) then
            if (jassize.lt.256) then 
                call write_int(249,1)
                call write_int(jas1,2)
                call write_int(jassize,1)
            elseif (jassize.lt.65536) then
                call write_int(250,1)
                call write_int(jas1,2)
                call write_int(jassize,2)
            else
                call write_int(251,1)
                call write_int(jas1,2)
                call write_int(jassize,4)
            endif
         else                           !position > 65535
            if (jassize.lt.256) then 
                call write_int(252,1)
                call write_int(jas1,4)
                call write_int(jassize,1)
            elseif (jassize.lt.65536) then
                call write_int(253,1)
                call write_int(jas1,4)
                call write_int(jassize,2)
            else
                call write_int(254,1)
                call write_int(jas1,4)
                call write_int(jassize,4)
            endif
         endif
c       print *,' jas  ',jas1,jassize
         blockwrites=blockwrites+1+was1-was0

         was0=ns1+1
         was1=was0+bsize1-1

         return
       endif

c case 4: B,C
      if (atype.ne.'B'.and.wastype.eq.'B') then    !flush a block run, start char run

         jas1=was0       ! record a copy range
         jassize=1+was1-was0

         if (jas1.lt.65536) then
            if (jassize.lt.256) then 
                call write_int(249,1)
                call write_int(jas1,2)
                call write_int(jassize,1)
            elseif (jassize.lt.65536) then
                call write_int(250,1)
                call write_int(jas1,2)
                call write_int(jassize,2)
            else
                call write_int(251,1)
                call write_int(jas1,2)
                call write_int(jassize,4)
            endif
         else                           !position > 65536
            if (jassize.lt.256) then 
                call write_int(252,1)
                call write_int(jas1,4)
                call write_int(jassize,1)
            elseif (jassize.lt.65536) then
                call write_int(253,1)
                call write_int(jas1,4)
                call write_int(jassize,2)
            else
                call write_int(254,1)
                call write_int(jas1,4)
                call write_int(jassize,4)
            endif
         endif

c       print *,' 2jas ',jas1,jassize

         blockwrites=blockwrites+1+was1-was0
         was0=was1=iat
         wastype=atype          !not strictly needed if "F"
         return
      endif
      
         
 111   format(i9)
 10     continue
        return

        end
        
cc--------------
c write a 1, 2 or 4 byte integer to unit
        subroutine write_int(ival,nbytes)
        integer ival,iserror,nbytes
        character *1 c1
        character *2 c2
        character *4 c4
        character *8 c8

 111       format(z4)
 1111       format(z8)

        if (nbytes.eq.1) then
             c1=char(ival)
             write(22,err=10,iostat=iserror)c1
             return
       endif
     
       if (nbytes.eq.2) then
            write(c4,111)ival
            read(c4,111)c2
            write(22,err=10,iostat=iserror)c2
           return
        endif

c otherwise use 4bytes (assume value < 2billion 
        write(c8,1111)ival
        read(c8,1111)c4
        write(22,err=10,iostat=iserror)c4
        return

 10     print *,'ERROR writing integer: ',ival
        stop

        end      

        subroutine make_hashtable(nblocks,index_rsyncs16, 
     1                     rsyncs16,hasht)
        integer rsyncs16(1)
        integer index_rsyncs16(1)
        integer  hasht(0:65535)
        integer ink,mm,ll,nblocks

        do mm=1,nblocks
           ink=index_rsyncs16(mm)
           ll=rsyncs16(ink)
           if (hasht(ll).eq.0) then
                 hasht(ll)=mm
           endif
        enddo
c        print *,nblocks,' entries written to hash table.'
        return
        end


c---------------
        subroutine rsync_compute(astring,chksum,ialpha,ibeta)
        integer chksum,ialpha,ibeta
        character *(*)astring
        integer isum,i2,i1,ilen

        ilen=len(astring)

c compute alpha
        isum=0
        do i1=1,ilen
          i2=ichar(astring(i1:i1))
          isum=isum+i2
        enddo 
        isum=mod(isum,65536)
        if (isum.lt.0)isum=isum+65536
        ialpha=isum


c compute beta     
        isum=0
        do i1=1,ilen
          i2=ichar(astring(i1:i1))
          isum=((ilen-i1+1)*i2) + isum
        enddo 
        isum=mod(isum,65536)
        if (isum.lt.0)isum=isum+65536
        ibeta=isum

        chksum=ialpha +  (65536*ibeta)

        return
        end


c----------------
c COMPUTE ROLLING CHECKSUM */
        integer function rsync_increment_s(oldchar,newchar,ialpha,
     1                          ibeta,ib)
        character *1 oldchar,newchar
        integer ialpha,ibeta,ioldchar,inewchar,ib
        IOLDCHAR=ichar(oldchar)
        INEWCHAR=ichar(newchar)
        ialpha=ialpha- Ioldchar + Inewchar
         ialpha=mod(ialpha,65536)
         if (ialpha.lt.0) ialpha=65536+ialpha
         ibeta=ibeta-(ib*ioldchar)+ialpha
         ibeta=mod(ibeta,65536)
        if (ibeta.lt.0) ibeta=65536+ibeta
        rsync_increment_s=ialpha+(ibeta*65536)
        return
        end

c==============================================================
c  sort to and index.  Index is integer, array to sort is integer
c Based on QUIKSORT

        SUBROUTINE rsync_INDEXSORT(X,xindex,nx)

        integer  x(1),pivotv
        integer    xindex(1),nx
        integer stack(500)
        integer segbeg,segend,segsize,ep,bp,mid
        integer lenend,lenbeg,newpivot,nspt,ij

        logical QPopSort_rsync

        nspt=0

c initialize stack

        do 2 ij=1,500
           stack(ij)=0
  2     continue

        do 3 ij=1,nx
           xindex(ij)=ij
 3      continue
        
        segbeg=1
        segend=nx

c  first "sort"
        call indqsort3_rsync(segbeg,mid,segend,pivotv,x,xindex)
        if (nx.le.3) goto 2000

 100    continue
        bp=segbeg
        ep=segend

 200    continue

        if (x(xindex(bp)).le.pivotv .and.bp.lt.segend) then
          bp=bp+1
          goto 200
        endif

 300    continue
        if (x(xindex(ep)).ge.pivotv.and.ep.gt.segbeg) then
            ep=ep-1
            goto 300
        endif

        if (ep.gt.bp) then
           call intswap_rsync(xindex(bp),xindex(ep))
           bp=bp+1
           ep=ep-1
           goto 200
        endif


        if (bp.gt.mid) then
          if (ep.gt.mid) then
              call intswap_rsync(xindex(ep),xindex(mid))
              newpivot=ep
          else
              newpivot=mid
          endif
        else
           call intswap_rsync(xindex(bp),xindex(mid))
           newpivot=bp
        endif

        lenend=segend-newpivot
        lenbeg=newpivot-segbeg

        if (lenend.gt.lenbeg) then
           call pushsort_rsync(newpivot+1,segend,stack,nspt)
           segend=newpivot-1
           segsize=lenbeg
        else
           call pushsort_rsync(segbeg,newpivot-1,stack,nspt)
           segbeg=newpivot+1
           segsize=lenend
        endif

        call indqsort3_rsync(segbeg,mid,segend,pivotv,x,xindex)

        if (segsize.gt.3) goto 100

 1000   continue
        if (.not.QPopSort_rsync(segbeg,segend,stack,nspt)) then
            goto 2000
        else
           segsize=segend-segbeg+1
           call indqsort3_rsync(segbeg,mid,segend,pivotv,x,xindex)
           if (segsize.le.3) then
             goto 1000
           else
             goto 100
           endif
        endif

 2000   continue
         return
        end





c--------
        subroutine indqsort3_rsync(segbeg,mid,segend,pivotv,x,xindex)

        integer x(1),pivotv
        integer segbeg,mid,segend
        integer xindex(1)

        mid=(segbeg+segend)/2
        if (segbeg+1.eq.segend) then
            if (x(xindex(segbeg)).gt.x(xindex(segend))) then
               call intswap_rsync(xindex(segbeg),xindex(segend))
            endif
            return
        endif

        if (x(xindex(segbeg)).gt.x(xindex(mid))) then
            if (x(xindex(segbeg)).gt.x(xindex(segend))) then
              call intswap_rsync(xindex(segbeg),xindex(segend))
              if (x(xindex(segbeg)).gt.x(xindex(mid))) then
                call intswap_rsync(xindex(segbeg),xindex(mid))
              endif
            else
              call intswap_rsync(xindex(segbeg),xindex(mid))
            endif
        else
            if (x(xindex(mid)).gt.x(xindex(segend))) then
               call intswap_rsync(xindex(mid),xindex(segend))
               if (x(xindex(segbeg)).gt.x(xindex(mid))) then
                 call intswap_rsync(xindex(segbeg),xindex(mid))
               endif
            else
               continue
            endif
        endif

        pivotv=x(xindex(mid))
        return
        end


        subroutine intswap_rsync(ix,iy)
        integer it,ix,iy
        it=ix
        ix=iy
        iy=it
        return
        end


        subroutine pushsort_rsync(sb,se,stack,stackct)
        integer se,sb,stack(1),stackct


        if (stackct+2.gt.500) then
c           print *,' % Error in QUIKSORT: ARRAY TOO LONG '
           stop 0
        endif

        stackct=stackct+1
        stack(stackct)=sb

        stackct=stackct+1
        stack(stackct)=se

        return
        end


        logical function QPopSort_rsync(sb,se,stack,stkpoint)
        integer sb,se,stack(1),stkpoint

        if (stkpoint-1.le.0)then
           qpopsort_rsync=.false.
           return
        endif

        se=stack(stkpoint)
        stkpoint=stkpoint-1
        sb=stack(stkpoint)
        stkpoint=stkpoint-1
        qpopsort_rsync =.true.
        return
        end


c --------------------
c SRX_MD4 should be declared as *32 (at least)
c astring can be any length

        character *(*) function srx_md4_rsynca(astring)

        character *(*) astring

        integer addme,lenorig,lenorig8

        integer *1 pads(64)
        data pads/'80'x,63*'00'x/

        integer *1 i1a(4)               !use this for working with 
        integer  i4a
        equivalence (i4a,i1a)

        integer mess16(16),x(0:15)               !16 word chunk to digest
        integer *1 mess64(64)
        equivalence(mess16,mess64,x)

        integer lenbits(2)              !# bits in message
        integer *1 lenbits_1(8)
        equivalence(lenbits,lenbits_1)
       
        character *32 ans
        integer leftover,addbytes
        integer a,b,c,d,aa,bb,cc,dd,k,i16,i1,len2,len3,j,il,icc

        integer s11,s12,s13,s14,s21,s22,s23,s24,s31,s32,s33,s34

        character *1 a1lc(6)
        data a1lc/'a','b','c','d','e','f'/

        lenorig=len(astring)
        lenorig8=8*lenorig
        lenbits(1)=lenorig8
        
        leftover=mod(lenorig8,512)
c pad message to multiple of 512 bits. 
c Last 2 words are 64 bit # bits in message
        if (leftover.eq.448) addme=512
        if (leftover.lt.448) addme=448-leftover
        if (leftover.gt.448) addme=960-leftover
        addBYTES=addme/8

        
c  starting values of registers   
       a ='67452301'x 
       b ='efcdab89'x 
       c ='98badcfe'x 
       d ='10325476'x 

       len2=lenorig+ADDBYTES
       len3=len2+8            ! FINAL LENGTH IN BYTES, must be multiple of 64

c  loop through entire message -- 16 words at a time   
       do i1 = 0,((len3/64)-1)
          i16=i1*64
          do j=1,64
             k=i16+j    !add this byte 

C add byte from message, padding, or length 
             if (k.le.lenorig) then
                 mess64(j)=ichar(astring(k:k))
             else
                if (k.le.len2) then
                 mess64(j)=pads(k-lenorig)
                else
                  mess64(j)=lenbits_1(k-len2)
                ENDIF
             ENDIF
          ENDDO                 !64 BYTES OF BLOCK

c  transform this block of 16 chars to 4 values. Save prior values first */
        aa=a
        bb=b
        cc=c
        dd=d


c do 4 rounds, 16 operations per round (rounds differ in bit'ing functions 

        S11=3
        S12=7 
        S13=11
        S14=19

          call round1_4a(a, b, c, d, x( 0), S11)  ! /* 1 */
          call round1_4a(d, a, b, c, x( 1), S12) ! /* 2 */
          call round1_4a(c, d, a, b, x( 2), S13) !  /* 3 */
          call round1_4a(b, c, d, a, x( 3), S14) !  /* 4 */
          call round1_4a(a, b, c, d, x( 4), S11) !  /* 5 */
          call round1_4a(d, a, b, c, x( 5), S12) !  /* 6 */
          call round1_4a(c, d, a, b, x( 6), S13) !  /* 7 */
          call round1_4a(b, c, d, a, x( 7), S14) !  /* 8 */
          call round1_4a(a, b, c, d, x( 8), S11) !  /* 9 */
          call round1_4a(d, a, b, c, x( 9), S12) !  /* 10 */
          call round1_4a(c, d, a, b, x(10), S13) !  /* 11 */
          call round1_4a(b, c, d, a, x(11), S14) !  /* 12 */
          call round1_4a(a, b, c, d, x(12), S11) !  /* 13 */
          call round1_4a(d, a, b, c, x(13), S12) !  /* 14 */
          call round1_4a(c, d, a, b, x(14), S13) !  /* 15 */
          call round1_4a(b, c, d, a, x(15), S14) !  /* 16 */


c /* Round 2 */
        S21=3
        S22=5
        S23=9 
        S24=13

          call round2_4a(a, b, c, d, x( 0), S21) !  /* 17 */
          call round2_4a(d, a, b, c, x( 4), S22) !  /* 18 */
          call round2_4a(c, d, a, b, x( 8), S23) !  /* 19 */
          call round2_4a(b, c, d, a, x(12), S24) !  /* 20 */
          call round2_4a(a, b, c, d, x( 1), S21) !  /* 21 */
          call round2_4a(d, a, b, c, x( 5), S22) !  /* 22 */
          call round2_4a(c, d, a, b, x( 9), S23) !  /* 23 */
          call round2_4a(b, c, d, a, x(13), S24) !  /* 24 */
          call round2_4a(a, b, c, d, x( 2), S21) !  /* 25 */
          call round2_4a(d, a, b, c, x( 6), S22) !  /* 26 */
          call round2_4a(c, d, a, b, x(10), S23) !  /* 27 */
          call round2_4a(b, c, d, a, x(14), S24) !  /* 28 */
          call round2_4a(a, b, c, d, x( 3), S21) !  /* 29 */
          call round2_4a(d, a, b, c, x( 7), S22) !  /* 30 */
          call round2_4a(c, d, a, b, x(11), S23) !  /* 31 */
          call round2_4a(b, c, d, a, x(15), S24)!  /* 32 */

c  /* Round 3 */
        S31= 3
        S32= 9 
        S33= 11
        S34= 15

          call round3_4a(a, b, c, d, x( 0), S31) !  /* 33 */
          call round3_4a(d, a, b, c, x( 8), S32) !  /* 34 */
          call round3_4a(c, d, a, b, x( 4), S33) !  /* 35 */
          call round3_4a(b, c, d, a, x(12), S34) !  /* 36 */
          call round3_4a(a, b, c, d, x( 2), S31) !  /* 37 */
          call round3_4a(d, a, b, c, x(10), S32) !  /* 38 */
          call round3_4a(c, d, a, b, x( 6), S33) !  /* 39 */
          call round3_4a(b, c, d, a, x(14), S34) !  /* 40 */
          call round3_4a(a, b, c, d, x( 1), S31) !  /* 41 */
          call round3_4a(d, a, b, c, x( 9), S32) !  /* 42 */
          call round3_4a(c, d, a, b, x( 5), S33) !  /* 43 */
          call round3_4a(b, c, d, a, x(13), S34) !  /* 44 */
          call round3_4a(a, b, c, d, x( 3), S31) !  /* 45 */
          call round3_4a(d, a, b, c, x(11), S32) !  /* 46 */
          call round3_4a(c, d, a, b, x( 7), S33) !  /* 47 */
          call round3_4a(b, c, d, a, x(15), S34) !  /* 48 */


        A=AA+A
        B=BB+B
        C=CC+C
        D=D+DD


      enddo

      i4a=a
      write(ans(1:8),'(4(z2))')i1a
   
      i4a=b
      write(ans(9:16),'(4(z2))')i1a

      i4a=c
      write(ans(17:24),'(4(z2))')i1a

      i4a=d
      write(ans(25:32),'(4(z2))')i1a

c convert to lower case
       do il=1, 32
          icc=index('ABCDEF',ANS(IL:il))
          if (icc.gt.0) ans(il:il)=a1lc(icc)
       enddo
      
  
      srx_md4_rsynca=ans
      return
      end


C  /* round 1 to 4 functins */

        SUBROUTINE round1_4a(a1,b1,c1,d1,kk,shift)
        INTEGER A1,B1,C1,D1,Kk,SHIFT,T1,T2,f_4a
        
        t1=a1+f_4a(b1,c1,d1)+ kk 

        t2=ISHC(t1,shift)
        A1=T2
        return 
        END


        SUBROUTINE round2_4a(a1,b1,c1,d1,kk,shift)
        INTEGER A1,B1,C1,D1,Kk,SHIFT,T1,T2,g_4a,aconst

        aconst='5a827999'x

        t1=a1+G_4a(b1,c1,d1)+ kk + aconst
        t2=ISHC(t1,shift)
        A1=T2
        return 
        END

        SUBROUTINE round3_4a(a1,b1,c1,d1,kk,shift)
        INTEGER A1,B1,C1,D1,Kk,SHIFT,aconst,T1,T2,h_4a

        aconst='6ed9eba1'x

        t1=a1+H_4a(b1,c1,d1)+ kk + aconst
        t2=ISHC(t1,shift)
        A1=T2
        return 
        END




c*********** Basic functions */
c* F_4a(x, y, z) == (((x) & (y)) | ((~x) & (z))) */
        INTEGER FUNCTION f_4a(X,Y,Z)
        INTEGER X,Y,Z,T1,NOTX,t2
        t1=Iand(x,y)
        notx=NOT(x)
        t2=Iand(notx,z)
        t2=Ior(t1,t2)
        f_4a=t2
        RETURN
        END

c* G_4a(x, y, z) == (((x) & (z)) | ((x) & (y)) | ((y) & (z)) ) */
        INTEGER FUNCTION G_4a(X,Y,Z)
        INTEGER X,Y,Z,T1,T2,t3,t4

        T1=Iand(x,y)
        t2=Iand(x,z)
        t3=iand(y,z)
        t4=Ior(t1,t2)
        g_4a=ior(t3,t4)
        RETURN
        END

c* H_4a(x, y, z) == ((x) ^ (y) ^ (z)) */
        INTEGER FUNCTION H_4a(X,Y,Z)
        INTEGER X,Y,Z,T1

        t1=IEor(x,y)
        H_4a=IEor(t1,z)
        RETURN
        END



C------------
C ROUTINE TO LOOK FOR FIRST NON TAB/SPACE CHARACTER,, AFTER
C POSITION NPOS . RETURNS LOCATION OF THIS NON TAB/SPACE CHAR.

      SUBROUTINE EATSPC_rsync(ASTRG,NPOS)

        integer nlen,npos,is,ij,ic
      CHARACTER *(*)ASTRG
      CHARACTER *1 ACHAR

       nlen=len(astrg)

      IF (NPOS.ge.NLEN) THEN
         NPOS=NLEN+1
         RETURN
      ENDIF

      IS=NPOS+1
      DO 100 IJ=IS,NLEN
         ACHAR=ASTRG(IJ:IJ)
         IC=ICHAR(ACHAR)
         IF (IC.le.32) THEN
            CONTINUE
         ELSE
            NPOS=IJ
            RETURN
         ENDIF
 100  CONTINUE

      NPOS=NLEN+1
      RETURN                                                                            
      END

