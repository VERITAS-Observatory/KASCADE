!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GTR_MEMORY  
!-------------------------------------------------------------------------------
!     generate trigger pattern memory file 
!-------------------------------------------------------------------------------

!	Modified:
!	22/11/99 GHS
!		Modified for standalone operation under linux.

      IMPLICIT NONE 
      
      character(len=80) :: msg
	integer :: ierr,id
!---- misc 
      INTEGER I,J,K,N,M,n2,n3,n4  
      INTEGER I19                                        ! 19-bit pattern 

!----
      INTEGER,pointer :: P2(:)       ! pattern of 2 ajacent pixels 
      INTEGER,pointer :: P3(:)       ! pattern of 3 ajacent pixels 
      INTEGER,pointer :: P4(:)       ! pattern of 4 ajacent pixels 
      INTEGER,pointer :: PX(:)       ! pattern temporary storage 

!---- pixel positions 
      INTEGER :: U(19), V(19)      
      DATA U / -0,-1,-2,-2,-2,+1,-0,-1,-1,-1,+2,+1,-0,-0,-0,+2,+1,+1,+2 /
      DATA V / -2,-1,-0,+1,+2,-2,-1,-0,+1,+2,-2,-1,-0,+1,+2,-1,-0,+1,-0 /

!-------------------------------------------------------------------------------
!     check (u,v) pixel coordinates are correct  
!-------------------------------------------------------------------------------
!---- check pixel coordinates are unique 
      DO I=1,19                                  ! loop over all (i,j) pairs
        DO J=I+1,19                              !   of two pixels 
          IF (U(I).EQ.U(J)) THEN 
          IF (V(I).EQ.V(J)) THEN 
            MSG  = "Found two identical pixels" 
		print*,msg
            RETURN 
          ENDIF 
          ENDIF 
        ENDDO 
      ENDDO                              
!---- check distance from center 
      DO I=1,19                                    ! loop over all pixels
        IF (ABS(U(I)).GT.2) IERR = 10              ! 
        IF (ABS(V(I)).GT.2) IERR = 11              ! 
        IF (ABS(V(I)+U(I)).GT.2) IERR = 12         ! max distance from center 
        IF (IERR.GT.0) THEN 
          MSG  = "Found illegal pixel coordinates" ! error message 
	print*,msg
          RETURN 
        ENDIF 
      ENDDO                              
!-------------------------------------------------------------------------------
!     find all patterns with two adjacent pixels out of 19 
!-------------------------------------------------------------------------------
      ALLOCATE(PX(9999))                         ! create temp storage 
      PX = 0                                     ! clear all data words 
      N  = 0                                     ! reset 2-adjacent count 
      DO I=1,19                                  ! loop over all (i,j) pairs
        DO J=I+1,19                              !   of two pixels 
          IF (ABS(U(I)-U(J)).GT.1) CYCLE         ! check difference in (u,v)
          IF (ABS(V(I)-V(J)).GT.1) CYCLE         ! to eliminate non-neighbours 
          IF (ABS((U(I)-U(J))-(V(I)-V(J))).LT.1) CYCLE  ! SMB 13.9.98 
          N = N + 1                              ! increment count 
          PX(N) = IBSET(PX(N),I-1)               ! switch on bit I
          PX(N) = IBSET(PX(N),J-1)               ! switch on bit J 
!!GHS Debug print
	write(6,4300)N,i,j,px(n)
4300	format(' N,I,J,PX(octal):',3i5,o10)
        ENDDO 
      ENDDO                              
      n2=n
	ALLOCATE(P2(N2))                            ! create storage 
      P2 = PX(1:N)                               ! copy 2-fold patterns 
      PX(1:N) = 0                                ! clear temp storage 
      PRINT*,"Number of 2-folds",N 
!-------------------------------------------------------------------------------
!     find all patterns with three adjacent pixels 
!-------------------------------------------------------------------------------
      N  = 0                                     ! reset pattern count 
      DO I=1,SIZE(P2)                            ! loop over all pairs 
        DO J=I+1,SIZE(P2)                        !   of 2-folds 
          I19 = IOR(P2(I),P2(J))                 ! take overlap 
          M   = 0                                ! reset bit count 
          DO K=0,18                              ! for lower 19 bits 
            IF (BTEST(I19,K)) M = M + 1          ! count bits sets 
          ENDDO                                  ! 
          IF (M.EQ.3) THEN                       ! new pattern with 3-bits?
           IF (.NOT.ANY(PX(1:N).EQ.I19)) THEN    ! SMB 13.9.98 not identical one?
            N = N + 1                            ! count patterns 
            PX(N) = I19                          ! store pattern 
           ENDIF
          ENDIF 
        ENDDO 
      ENDDO 
      n3=n
	ALLOCATE(P3(N3))                            ! create storage 
      P3 = PX(1:N)                               ! copy 3-fold patterns 
      PX(1:N) = 0                                ! clear temp storage 
      PRINT*,"Number of 3-folds",N 
!-------------------------------------------------------------------------------
!     find all patterns with four adjacent pixels 
!-------------------------------------------------------------------------------
      N  = 0                                     ! reset pattern count 
      DO I=1,SIZE(P2)                            ! loop over all 2-folds 
        DO J=1,SIZE(P3)                          ! loop over all 3-folds 
          I19 = IOR(P2(I),P3(J))                 ! take overlap 
          M   = 0                                ! reset bit count 
          DO K=0,18                              ! for lower 19 bits 
            IF (BTEST(I19,K)) M = M + 1          ! count bits sets 
          ENDDO                                  ! 
          IF (M.EQ.4) THEN                       ! new pattern with 4-bits?
            IF (.NOT.ANY(PX(1:N).EQ.I19)) THEN   ! no identical ones yet?
              N = N + 1                          ! count new patterns 
              PX(N) = I19                        ! store new pattern 
            ENDIF 
          ENDIF 
        ENDDO 
      ENDDO 
      n4=n
	ALLOCATE(P4(N4))                            ! create storage 
      P4 = PX(1:N)                               ! copy 4-fold patterns 
      PX(1:N) = 0                                ! clear temp storage 
      PRINT*,"Number of 4-folds",N 
!-------------------------------------------------------------------------------
!     write trigger data files  
!-------------------------------------------------------------------------------
	id=12
	call gtr_write(P2,n2,id)
	id=13
	call gtr_write(p3,n3,id)
	id=14
	call gtr_write(p4,n4,id)

!-------------------------------------------------------------------------------
!     deallocate memory 
!-------------------------------------------------------------------------------
      DEALLOCATE(P2,P3,P4,PX) 

      END SUBROUTINE GTR_MEMORY 

      SUBROUTINE GTR_WRITE(PN,n,id)  
!-------------------------------------------------------------------------------
!     write trigger patterns to file 
!-------------------------------------------------------------------------------
!	Modified:

!	22/11/99 GHS
!		Modified for standalone LINUX use.

      IMPLICIT NONE 

      INTEGER  :: IERR,id,n 
      INTEGER  :: PN(N)

!----
      LOGICAL*2,ALLOCATABLE :: I16(:)              ! 16-bit integer 
      INTEGER, PARAMETER    :: N19 = 2**19         ! number of data words
      INTEGER I,J 
!----
      ALLOCATE(I16(0:N19-1)) 
      I16 = '0000'X
	print*,'size:',size(pn)
      DO I=0,N19-1 
        DO J=1,SIZE(PN) 
          IF (PN(J).EQ.IAND(I,PN(J))) THEN 
            I16(I) = '001F'X
            EXIT 
          ENDIF 
        ENDDO 
      ENDDO 
!----
      WRITE(id) I16 
      CLOSE(id)      
      DEALLOCATE(I16) 

      END SUBROUTINE GTR_WRITE  

    PROGRAM PST_FILE_GEN
!*****************************************************************************
!	THE PROGRAM GENERATES THE 2,3,AND 4 MULTIPLICITY .DAT FOR THE PST'S
!	Its taken from gr_trigger.f90 from taurus.sao.arizona, observer
!	account, [observer.granite.v155]. Origanlly the subroutine GTR_MEMORY
!	was written by Jaochim Rose of Leeds for use in the GRANITE data
!	acquisiton software. 
!*****************************************************************************

!	written by:
!	Glenn Sembroski
!	22/11/99

!	Modified:

!	22/11/99 GHS
!		Modify Jaochims subroutine to work standalone on LINUX to
!		generate the PST_MULT2.DAT,PST_MUL3.DAT AND PST_MUL4.DAT
!		files.  Fort linux use write them out as form='binary'

	open(12,file='pst_mul2.dat',form='binary')
	open(13,file='pst_mul3.dat',form='binary')
	open(14,file='pst_mul4.dat',form='binary')
	call gtr_memory
	stop
    END PROGRAM PST_FILE_GEN
