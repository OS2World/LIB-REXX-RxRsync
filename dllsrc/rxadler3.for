c Compute adler-32 checksum
c return 8byte value (possibly padded with 0s on the left)
c Call as:
c  adler32=RX_ADLER32(message)
c or, to cumulatively build a checksum (say, by reading N 
c pieces of a huge file
c adler32=rx_adler32(piece1)
c adler32-rx_adler32(piece2,adler32)
c ...
c adler32=rx_adler32(pieceN,adler32)


c$define INCL_REXXSAA
c$include rexxsaa.fap

! Declare our exported function.  Export it!

c$pragma aux (RexxFunctionHandler) RX_ADLER32 "RX_ADLER32"

! Roll_chk -- Returns a rolling checksum of a string


c$noreference
	integer function RX_ADLER32( name, numargs, args,
     &                            queuename, retstr )
c$reference
	character*(*) name, queuename
	integer numargs
	record /RXSTRING/ retstr, args(numargs)
	
	include 'rxsutils.fi'
	
	character*(*)  arg1,arg2

        integer s1,s2,jj,lentr,len_arg1,ict,i1
        integer idid,ineed,ierr
    
        character *8 oldval,oldval2

c at least one argument
	if( numargs .lt.1.or.numargs.gt.2) then
	    RX_ADLER32 = 443
	    return
	endif

	allocate( arg1*args(1).strlength, location=args(1).strptr )
        len_arg1=len(arg1)

        s1=1
        s2=0

c possibly use add to old checksum value?
        if (numargs.eq.2) then
   	   allocate( arg2*args(2).strlength, location=args(2).strptr )
           oldval=arg2
           deallocate(arg2)
           lentr=lentrim(oldval)
           idid=0
           ineed=8-lentr
           if (lentr.ne.8) then
              do jj=1,ineed
                oldval2(jj:jj)='0'
              enddo
              oldval2(ineed+1:8)=oldval(1:lentr)
              oldval=oldval2
            endif
            read(oldval,99,iostat=ierr)s2,s1
 99         format(z4,z4)
            if (ierr.ne.0) then
  	       RX_ADLER32 = 443
 	       return
             endif
         endif

        s1=mod(s1,65521)
        s2=mod(s2,65521)
c --- compute checksum
        ict=0
        do i1=1,len_arg1
          ict=ict+1
          s1=s1+ichar(arg1(i1:i1))
          s2=s2+s1
          if (ict.gt.2500)then   !5552 in spec, but wrapping is a 
            s1=mod(s1,65521)
            s2=mod(s2,65521)
            ict=0
          endif
        enddo 
        s1=mod(s1,65521)
        s2=mod(s2,65521)
        write(oldval,101)s2,s1
 101    format(z4,z4)    

 1000   call CopyResult(oldval,lentrim(oldval), retstr )
       deallocate(arg1)
       RX_ADLER32 = VALID_ROUTINE
       return

       end

