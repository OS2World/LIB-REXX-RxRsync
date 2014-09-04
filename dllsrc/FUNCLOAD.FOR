! funcload.for -- Defines the two functions 'RxRsyncLoad' and 'RxRsyncDrop',
!                 which load/drop all the functions defined in this DLL.

c$define INCL_REXXSAA
c$include rexxsaa.fap

! Declare our functions.  Make sure to export these in the .LNK file!

c$pragma aux (RexxFunctionHandler) RxRsyncLoad "RxRsyncLoad"
c$pragma aux (RexxFunctionHandler) RxRsyncDrop "RxRsyncDrop"

! Define the table that lists REXX function names and the corresponding
! DLL entry point.  You must change this table whenever you add/remove
! a function or entry point.

	blockdata
	include 'funcload.fi'
	data RxFncTable/
		! function	entry point
     &		'RxRsyncLoad'c,  'RxRsyncLoad'c,
     &		'RxRsyncDrop'c,  'RxRsyncDrop'c,
     &		'RX_MD4'c,      'RX_MD4'c,
     &		'RX_RSYNC32'c,       'RX_RSYNC32'c,
     &		'RX_ADLER32'c,       'RX_ADLER32'c,
     &		'RX_RSYNC32_MD4'c,       'RX_RSYNC32_MD4'c,
     &		'RX_RSYNC_GDIFF'c,       'RX_RSYNC_GDIFF'c/
	end

! RxRsyncLoad -- Register all the functions with REXX.

c$noreference
	integer function RxRsyncLoad( name, numargs, args,
     &				      queuename, retstr )
c$reference
	integer numargs, name, queuename
	record /RXSTRING/ args(numargs), retstr
	
	include 'rxsutils.fi'
	include 'funcload.fi'
	
    	integer j
	character*9 DLLNAME ! Change this if you're changing the DLL name...
	parameter (DLLNAME = 'RXRSYNC'c)
	
	retstr.strlength = 0

	if( numargs .gt. 0 )then
	    RxRsyncLoad = INVALID_ROUTINE
	    return
	endif

	do j = 1, ENTRIES
	    call RexxRegisterFunctionDll( RxFncTable(j).rxName, DLLNAME,
     &					  RxFncTable(j).cName )
	enddo

	RxRsyncLoad = VALID_ROUTINE

	end

* RxRsyncDrop -- Deregister all the functions with REXX.

c$noreference
	integer function RxRsyncDrop( name, numargs, args,
     &				      queuename, retstr )
c$reference
	integer numargs, name, queuename
	record /RXSTRING/ args(numargs), retstr
	
	include 'rxsutils.fi'
	include 'funcload.fi'
	
	integer j

	retstr.strlength = 0

	if( numargs .gt. 0 )then
	    RxRsyncDrop = INVALID_ROUTINE
	    return
	endif

	do j = 1, ENTRIES
	    call RexxDeregisterFunction( RxFncTable(j).rxName )
	enddo

	RxRsyncDrop = VALID_ROUTINE

	end
