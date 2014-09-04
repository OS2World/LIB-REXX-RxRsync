c Compute adler-32 style rolling checksum for rsync, and md4
c return 8byte hex adler | 32byte md4
c Call as:
c  adler_md4=RX_RSYNC32_MD4(message)

c$define INCL_REXXSAA
c$include rexxsaa.fap

! Declare our exported function.  Export it!

c$pragma aux (RexxFunctionHandler) RX_RSYNC32_MD4 "RX_RSYNC32_MD4"

! Roll_chk -- Returns a rolling checksum of a string


c$noreference
	integer function RX_RSYNC32_MD4( name, numargs, args,
     &                            queuename, retstr )
c$reference
	character*(*) name, queuename
	integer numargs
	record /RXSTRING/ retstr, args(numargs)
	
	include 'rxsutils.fi'
	
	integer memstat, len,klen
	character*(*) tmp, arg1

        character *32 amd4
        integer chksum,ialpha,ibeta
        integer i2,isum

        integer len_arg1

c md4 stuff	
        integer *1 pads(64)
        data pads/'80'x,63*'00'x/

        integer *1 i1a(4)               !use this for working with 
        integer  i4a
        equivalence (i4a,i1a)

        integer mess16(16),x(0:15)                 !16 word chunk to digest
        integer *1 mess64(64)
        equivalence(mess16,mess64,x)

        integer lenbits(2)              !# bits in message
        integer lenorig,lenorig8,addme,i16,k
        integer *1 lenbits_1(8)
        equivalence(lenbits,lenbits_1)
       
        character *32 ans
        integer leftover,addbytes
        integer a,b,c,d,aa,bb,cc,dd,i1,len2,len3,j

        integer s11,s12,s13,s14,s21,s22,s23,s24,s31,s32,s33,s34

c at least one argument
	if( numargs .ne.1) then
	    RX_RSYNC32_MD4 = 443
	    return
	endif

	allocate( arg1*args(1).strlength, location=args(1).strptr )
        len_arg1=len(arg1)

        klen=40

	! Get some buffer space for result ....
	allocate( tmp*klen, stat=memstat )
	if( memstat .ne. 0 )then
	    RX_RSYNC32_MD4 = 445
	    return
	endif

        len_arg1=len(arg1)

c --- compute checksum
c compute alpha
        isum=0
        do i1=1,len_arg1
          i2=ichar(arg1(i1:i1))
          isum=isum+i2
        enddo 
        ialpha=mod(isum, 65536)
c compute beta
        isum=0
        do i1=1,len_arg1
          i2=ichar(arg1(i1:i1))
          isum=((len_arg1-i1+1)*i2) + isum
        enddo 
        isum=mod(isum,65536)
        ibeta=isum

        chksum=ialpha +  (65536*ibeta)  !the checksum


c --- now do md4 
	if (len_arg1.eq.0) then
            amd4='d41d8cd98f00b204e9800998ecf8427e'  !null string value
        else

          lenorig=len_arg1
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
         len3=len2+8                     ! FINAL LENGTH IN BYTES, must be multiple of 64

c  loop through entire message -- 16 words at a time   
         do i1 = 0,((len3/64)-1)
            i16=i1*64
            do j=1,64
               k=i16+j    !add this byte 

C add byte from message, padding, or length 
             if (k.le.lenorig) then
                 mess64(j)=ichar(arg1(k:k))
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

          call round1_4b(a, b, c, d, x( 0), S11)  ! /* 1 */
          call round1_4b(d, a, b, c, x( 1), S12) ! /* 2 */
          call round1_4b(c, d, a, b, x( 2), S13) !  /* 3 */
          call round1_4b(b, c, d, a, x( 3), S14) !  /* 4 */
          call round1_4b(a, b, c, d, x( 4), S11) !  /* 5 */
          call round1_4b(d, a, b, c, x( 5), S12) !  /* 6 */
          call round1_4b(c, d, a, b, x( 6), S13) !  /* 7 */
          call round1_4b(b, c, d, a, x( 7), S14) !  /* 8 */
          call round1_4b(a, b, c, d, x( 8), S11) !  /* 9 */
          call round1_4b(d, a, b, c, x( 9), S12) !  /* 10 */
          call round1_4b(c, d, a, b, x(10), S13) !  /* 11 */
          call round1_4b(b, c, d, a, x(11), S14) !  /* 12 */
          call round1_4b(a, b, c, d, x(12), S11) !  /* 13 */
          call round1_4b(d, a, b, c, x(13), S12) !  /* 14 */
          call round1_4b(c, d, a, b, x(14), S13) !  /* 15 */
          call round1_4b(b, c, d, a, x(15), S14) !  /* 16 */

c /* Round 2 */
        S21=3
        S22=5
        S23=9 
        S24=13

          call round2_4b(a, b, c, d, x( 0), S21) !  /* 17 */
          call round2_4b(d, a, b, c, x( 4), S22) !  /* 18 */
          call round2_4b(c, d, a, b, x( 8), S23) !  /* 19 */
          call round2_4b(b, c, d, a, x(12), S24) !  /* 20 */
          call round2_4b(a, b, c, d, x( 1), S21) !  /* 21 */
          call round2_4b(d, a, b, c, x( 5), S22) !  /* 22 */
          call round2_4b(c, d, a, b, x( 9), S23) !  /* 23 */
          call round2_4b(b, c, d, a, x(13), S24) !  /* 24 */
          call round2_4b(a, b, c, d, x( 2), S21) !  /* 25 */
          call round2_4b(d, a, b, c, x( 6), S22) !  /* 26 */
          call round2_4b(c, d, a, b, x(10), S23) !  /* 27 */
          call round2_4b(b, c, d, a, x(14), S24) !  /* 28 */
          call round2_4b(a, b, c, d, x( 3), S21) !  /* 29 */
          call round2_4b(d, a, b, c, x( 7), S22) !  /* 30 */
          call round2_4b(c, d, a, b, x(11), S23) !  /* 31 */
          call round2_4b(b, c, d, a, x(15), S24)!  /* 32 */

c  /* Round 3 */
        S31= 3
        S32= 9 
        S33= 11
        S34= 15

          call round3_4b(a, b, c, d, x( 0), S31) !  /* 33 */
          call round3_4b(d, a, b, c, x( 8), S32) !  /* 34 */
          call round3_4b(c, d, a, b, x( 4), S33) !  /* 35 */
          call round3_4b(b, c, d, a, x(12), S34) !  /* 36 */
          call round3_4b(a, b, c, d, x( 2), S31) !  /* 37 */
          call round3_4b(d, a, b, c, x(10), S32) !  /* 38 */
          call round3_4b(c, d, a, b, x( 6), S33) !  /* 39 */
          call round3_4b(b, c, d, a, x(14), S34) !  /* 40 */
          call round3_4b(a, b, c, d, x( 1), S31) !  /* 41 */
          call round3_4b(d, a, b, c, x( 9), S32) !  /* 42 */
          call round3_4b(c, d, a, b, x( 5), S33) !  /* 43 */
          call round3_4b(b, c, d, a, x(13), S34) !  /* 44 */
          call round3_4b(a, b, c, d, x( 3), S31) !  /* 45 */
          call round3_4b(d, a, b, c, x(11), S32) !  /* 46 */
          call round3_4b(c, d, a, b, x( 7), S33) !  /* 47 */
          call round3_4b(b, c, d, a, x(15), S34) !  /* 48 */


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

        
         amd4=ans
      endif



c --- write results
        write(tmp,883)chksum,amd4
 
 883    format(z8,a32)

c return results/
 1000       call CopyResult( tmp, lentrim( tmp ), retstr )
       deallocate( tmp )
       deallocate(arg1)
       RX_RSYNC32_MD4 = VALID_ROUTINE
       return

        end

C  /* round 1 to 4 functins */

        SUBROUTINE round1_4b(a1,b1,c1,d1,kk,shift)
        INTEGER A1,B1,C1,D1,Kk,SHIFT,T1,T2,f_4b
        

        t1=a1+f_4b(b1,c1,d1)+ kk 

        t2=ISHC(t1,shift)
        A1=T2
        return 
        END


        SUBROUTINE round2_4b(a1,b1,c1,d1,kk,shift)
        INTEGER A1,B1,C1,D1,Kk,SHIFT,T1,T2,g_4b,aconst

        aconst='5a827999'x

        t1=a1+g_4b(b1,c1,d1)+ kk + aconst
        t2=ISHC(t1,shift)
        A1=T2
        return 
        END

        SUBROUTINE round3_4b(a1,b1,c1,d1,kk,shift)
        INTEGER A1,B1,C1,D1,Kk,SHIFT,aconst,T1,T2,h_4b

        aconst='6ed9eba1'x

        t1=a1+h_4b(b1,c1,d1)+ kk + aconst
        t2=ISHC(t1,shift)
        A1=T2
        return 
        END




c*********** Basic functions */
c* f_4b(x, y, z) == (((x) & (y)) | ((~x) & (z))) */
        INTEGER FUNCTION f_4b(X,Y,Z)
        INTEGER X,Y,Z,T1,NOTX,t2
        t1=Iand(x,y)
        notx=NOT(x)
        t2=Iand(notx,z)
        t2=Ior(t1,t2)
        f_4b=t2
        RETURN
        END

c* g_4b(x, y, z) == (((x) & (z)) | ((x) & (y)) | ((y) & (z)) ) */
        INTEGER FUNCTION g_4b(X,Y,Z)
        INTEGER X,Y,Z,T1,T2,t3,t4

        T1=Iand(x,y)
        t2=Iand(x,z)
        t3=iand(y,z)
        t4=Ior(t1,t2)
        g_4b=ior(t3,t4)
        RETURN
        END

c* h_4b(x, y, z) == ((x) ^ (y) ^ (z)) */
        INTEGER FUNCTION h_4b(X,Y,Z)
        INTEGER X,Y,Z,T1

        t1=IEor(x,y)
        h_4b=IEor(t1,z)
        RETURN
        END




