    SUBROUTINE DATETIME(adate,time)
!****************************************************************************
!	F77 interface to f90 routine date_and_time
!****************************************************************************
	character(len=8) :: adate
	character(len=10) ::  time          	
	call date_and_time(adate,time)
	return
   END SUBROUTINE DATETIME
