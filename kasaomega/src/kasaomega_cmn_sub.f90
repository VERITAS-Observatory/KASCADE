MODULE KASAOMEGA_CMN_SUB

CONTAINS

    SUBROUTINE CMSUM_FILE_OPEN
!*****************************************************************************
!	Open the binary version of the Summary file for input. Read in 
!	the MHEAD record. Open the ouput binary hillas parameter file.
!*****************************************************************************

!	Modified:

!	17/3/00 GHS V:1:1:6:1:2:3.4
!		Write out Michells Mfile. This is a binary version of the 
!		cmsum_in.dat file for use by MIchelle D'Vale to use for 
!		template image analysis. Uses option 'MICHELLE'. Writes to
!		new file michelle.dat. Use subroutine MICHELLE.

!      20/06/02 GHS:1:1:6:1:2:4.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               We use cmsum_in_dat_file here.


 	use structures
	use record_defs
	use kasaomega_def !for MIchelle
        use kasaomega_command_line
	IMPLICIT NONE

 	integer :: i,j,k,m,n,ios

		!open the binary cmsum file. Use the  file name
                !specified on command line (which defaults to cmsum_in.dat)
       
	if(index(options,'CM')==0)then
           OPEN(8,ACCESS='sequential',STATUS='old',FORM='UNFORMATTED', &
		& file=cmsum_in_dat_file,iostat=ios)
        else
           OPEN(8,ACCESS='sequential',STATUS='old',FORM='BINARY', &
		& file=cmsum_in_dat_file,iostat=ios)
        endif
        if(ios>0)then
           print*,' error opening cmsum_in.dat'
           stop 'file open error'
        endif

!	Read in the mfile header.
	read(8)mhead
        return

		
    END SUBROUTINE CMSUM_FILE_OPEN
!******************************************************************************


    SUBROUTINE CMSUM_HDF5_FILE_OPEN
!*************************************************************************
!                         Open HDF5 input cmsum file.
!*************************************************************************

!      20/06/02 GHS:1:1:6:1:2:4.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               We use cmsum_in_hdf5_file  here.

	use structures
	use record_defs
	use kasaomega_def !for MIchelle
        use kasaomega_command_line

	IMPLICIT NONE

        integer :: ierr,ios


                                         !Set up a sting for C. Ends with null.
        call mhdf5_open(trim(cmsum_in_hdf5_file)//char(0),cmsum_in,ierr)
        if(ierr<0)then
           print*,' MHDF5_open failure for file',trim(cmsum_in_hdf5_file),&
           & ':ierr:',ierr
           stop  'CMSUM_IN.HDF5 File open failure'
        endif
        call mhdf5_make_types
        call mhdf5_mhead_read(mhead,cmsum_in)

       return
    END SUBROUTINE CMSUM_HDF5_FILE_OPEN
!******************************************************************************

    SUBROUTINE MASS_NUMBER2CHARGE(ia,qz,xmass)
!******************************************************************************
!   Determine charge Z of most stable nuclei of nuclear number A
!   Determine mass from Z and A using semi-empirical mass formula
!******************************************************************************
!   Uses fromulae from "Physics of Nuclei and Particles, Vol 1", Marmier and 
!   Sheldon,1969, Acedemic Press, pg 36-38, formula: 2-35,2-38,2-39,2-40
!******************************************************************************
!   This is from the liquid drop model of stable nuclei.

!  Written by:
!  Glenn Sembroski
!  Physics Dept.
!  Purdue Univ.
!  W. Lafayette, IN USA 47907

! Modified:



	IMPLICIT NONE
	real a
	integer qz,ia
	real  xmass,pmass
	logical  first_argon

	real mp,mn,ap,av,as,ac,aa
	parameter (mp=938.256e-6)   !Mass of proton (TeV)
	parameter (mn=939.550e-6)   !Mass of neutron(TeV)
	parameter (ap=33.5e-6)     !Pairing coef.(TeV)
	parameter (av=14.1e-6)     !Volume coef (TeV)
	parameter (as=13.0e-6)     !Surface coef(TeV)
	parameter (ac=0.595e-6)    !Coulmb coef.(TeV)
	parameter (aa=19e-6)       !Assymetry coef(TeV)
	data first_argon /.true./


         a=ia
                         !Correct our formula for elements up to a=56(Fe)
                         !which is as high as NUC_LIB goes.
         if(ia==18)then	  !Force Oxygen isotope
	    qz=8
	 elseif(ia==24)then
            qz=12         !Force Magnesium
         elseif(ia==28)then
            qz=14         !Force silicon
         elseif(ia==32)then
            qz=16         !Force Sulpher
         elseif(ia==33)then
            qz=16         !Force Sulpher
         elseif(ia==35)then
            qz=17         !Force Chlorine
         elseif(ia==39)then
            qz=19         !Force Potassium
         elseif(ia==40)then
            qz=18         !Force Argon !Could have been calcium 40.
            if(first_argon)then
               print*,'Warning--Forcing Argon for all atomic masses of 40'
               first_argon=.false.
            endif
         elseif(a==56)then
            qz=26         !Force Iron.
         else
            qz=anint(a/(1.98+0.0155*(a**(2./3.)))) 	!Use nearest integer 
							!function
         endif

!First determine pairing mass term
         if(mod(qz,2)==0)then
            if(mod(ia,2)==0)then
               pmass=-ap*a**(-.75)   !even-even nuclei
            else
               pmass=0.              !even-odd nuclei
            endif
         else
            if(mod(ia,2)==0)then
               pmass=0.              !Odd-even nuclei
            else
               pmass=ap*a**(-.75)   !Odd-odd  nuclei
            endif
         endif

         xmass = qz*mp + (a-qz)*mn - av*a + as*(a**(2./3.)) + &
              &ac*(qz**2)/(a**(1./3.)) + aa*((a-2*qz)**2)/a + pmass
 	return
    END SUBROUTINE MASS_NUMBER2CHARGE
!******************************************************************************


   SUBROUTINE MICHELLE(NWORDS,r4event,wptr)
!******************************************************************************
!	Write out biunary compressed version of cmsum.in data for Michelle
!	D'vali template image analysis. Include shower weighting.
!******************************************************************************
	use structures
	use kasaomega_def
	use record_defs
	use sum_init
	use wcamera_def

	IMPLICIT NONE

	integer nwords
 	real*4,dimension(:) :: r4event
	type(camera) ::       wptr

	real,dimension(60000),target :: disc_cm
	type(sum_rec) :: srec

	real :: x,y,x_g,y_g
	integer :: nxx,nyy
	logical :: ifirst=.true.

	real weight
        integer:: j,i,npes
!        integer :: icount
!debug
!        icount=icount+1
!        if(icount>100)then
!           stop
!        endif


	if(ifirst)then
              call michelle_dist(g_num,g_gev,g_nshowers,g_alpha, &
                   & g_area_solid,g_sparse,g_weight)
              call michelle_dist(p_num,p_gev,p_nshowers,p_alpha, &
                   & p_area_solid,p_sparse,p_weight)
              call michelle_dist(fe_num,fe_gev,fe_nshowers,fe_alpha, &
                   & fe_area_solid,fe_sparse,fe_weight)
		ifirst=.false.
	endif

!Unpack some.
	srec=transfer(r4event(1:srec_size),srec)
        disc_cm(1:nwords-srec_size)=r4event(srec_size+1:nwords)

!Find core location.
    	if(index(mhead%pe_head%petype,'VT')/=0)then
 	     if(index(options,'NS')==0)then
		            				!EW long axis
	      	y=srec%mrec%ny*mhead%pe_head%yseg   !Y(s-n) position
	    	nyy=srec%mrec%ny    	!Convert to integer from real
		if(mod(nyy,2)== 0)then
			x=srec%mrec%nx*mhead%pe_head%xseg 
		else
			x=(srec%mrec%nx+.5)*mhead%pe_head%xseg !X(e-w)
	    	endif
	     else				!NS axis is long
	        x=srec%mrec%nx*mhead%pe_head%xseg  !X(e-w) position
		nxx=srec%mrec%nx    	!Convert to integer from real
	    	if(mod(nxx,2)== 0)then
	    		y=srec%mrec%ny*mhead%pe_head%yseg 
	    	else
		   	y=(srec%mrec%ny+.5)*mhead%pe_head%yseg !Y(n-s)
		endif
    	     endif
    	else
		y=srec%mrec%ny*mhead%pe_head%yseg      !Y(s-n) position
		x=srec%mrec%nx*mhead%pe_head%xseg       !X(e-w) position
       	endif
	x_g=x-srec%x_offset		!- sign is correct here.
	y_g=y-srec%y_offset

        !determine event weight.
        if(mhead%pe_head%segment_head%itype==1)then
           call michelle_weight(g_weight,g_gev,srec%tep,weight)
        elseif(mhead%pe_head%segment_head%itype==76)then
           call michelle_weight(fe_weight,fe_gev,srec%tep,weight)
        elseif(mhead%pe_head%segment_head%itype==13)then
           call michelle_weight(p_weight,p_gev,srec%tep,weight)
        else
           print*,' No weights defined for itype:', &
                & mhead%pe_head%segment_head%itype
           stop 'MICHELLE-No weights for this particle type'
        endif

!Write out the event header line.
	write(33)srec%tep,srec%idfile,weight,x_g,y_g

				!UnCompress the disc array.
	j=2			!Start of compressed data
	i=1
	do
	    if(j>wptr%npmt+1)then
                npes=0                !end of event(pixel=-(npmt+1))
                write(33)-i,npes
	    	exit       			!special exit(all pmts have
						!data)
	    endif
	    if(disc_cm(j)<0)then		!Start on non zero data
	     	i=-disc_cm(j)			!Pixel number
		if(i/=(wptr%npmt+1))then     !look for end of process  
			j=j+1
			cycle
		else
                        npes=0                !end of event(pixel=-(npmt+1))
			write(33)-i,npes
			exit
		endif
	    else
	    	npes=disc_cm(j)
                write(33)i,npes

                i=i+1							
		j=j+1
		cycle
	    endif
	enddo
	return
    END SUBROUTINE MICHELLE
!******************************************************************************

    SUBROUTINE MICHELLE_DIST(num,gev,nshowers,alpha,area_solid,sparse, &
         & weight)

!******************************************************************************
!	Determines weighting factors for getting a 'representative' M file.
!******************************************************************************
!	Modified:

!	10/6/97 GHS
!		Fix problem of badly chosen energies. Problem arisies when
!		or centered bin schem ends up with negative or 0 weights.
!		In the case revert to uncentered bin for that energy.

!	17/3/00 GHS
!		Convert from dist.ftn (used by paw) to f90 for MICHELLE file
!		creation.
!       22/8/00 GHS
!               Add changes for use of ln(e) const spacing of energies.
!               Mainly use first to second energy to calculate first channel
!               width. That is don't go down to 0.
!       06/05/01 GHS
!               For the weight use integral of E**alpha from wlow to whi
!	23/07/01 GHS V:1:2:6:1:2:3.12
!               Upgrade for use with Heavy nuclei. Make this a generic(as far
!               as particle type is concerned) function.
!******************************************************************************
	use sum_init

	IMPLICIT NONE

	integer :: num
        real,dimension(:) :: gev,nshowers,weight
        real :: alpha,area_solid,sparse

        real :: wlow,whi,w,wmax
	real :: w_half,wl,wh
	integer :: i,j

!		First width is halfway up to second and the same down.
	if(num==1)then		!single shower only.
		weight(1)=1.0
		return
	endif

!	Since we now use delta(ln(E)) spaceing as a const, make for gap half 
!	way to second energy and down the same.
	wlow=gev(1)-(gev(2)-gev(1))*.5 
	whi=gev(1)+(gev(2)-gev(1))*.5 
        w=(whi-wlow)

	weight(1)=(wlow**(alpha+1.0))-(whi**(alpha+1.0))

	j=num 
	do i=2,j
!		Go down to match previous and go up by the same amount.
		wlow=whi
		whi=gev(i)+(gev(i)-wlow)
		w=whi-wlow
		if(w.le.0)then
	print*,' --WARNING--(dist.ftn)--Bad Shower Energy Spacing'
	print*,' --between energies',gev(i-1),'GeV and',gev(i),' GeV'
	print*,' --WARNING--To compensate for Bad spacing'
	print*,' --Use uncentered bin at shower energy',gev(i-1), 'GeV'

!		Have to first redo energy i-1. Find bin half width.
			w_half=wlow-gev(i-1)
			wl=gev(i-1)-w_half
!		High side if halfway between
			wh=gev(i-1)+((gev(i)-gev(i-1))/2)
			w=wh-wl
                        weight(i-1)=(wl**(alpha+1.0))-(wh**(alpha+1.0))

			wlow=wh
			whi=gev(i)+(gev(i)-wlow)
			w=whi-wlow
		endif

       		weight(i)=(wlow**(alpha+1.0))-(whi**(alpha+1.0))
	enddo

!	Now scale to the number of showers
	wmax=0.

			     ! Weight as if we only had 1 shower at each energy
	weight = weight/nshowers                                  !Vector arith

	do i=1,j
!	Adjust weights for energies greater then sparse TeV
!	We use a  partial sampling above sparse to reduce file size.
		if(gev(i)>sparse*1000)then
			weight(i)=4.*weight(i)
		endif
	enddo

!	Now re-scale so maximum weight is 1.0
        wmax=maxval(weight)
        weight=weight/wmax                                        !Vector arith
	return
    END SUBROUTINE MICHELLE_DIST
!******************************************************************************

    SUBROUTINE MICHELLE_WEIGHT(weights,energies,tep,weight)
!******************************************************************************
!	Find weight for this energy.
!******************************************************************************

!       There is probably a clever way to do this using intrinsic functions.
	
	IMPLICIT NONE

	real,dimension(:) :: weights,energies
	real :: tep,weight,ener
	integer :: i,icount=0
	
        do i=1,size(energies)
              if(abs(energies(i)-tep*1000)<1)then
			weight=weights(i)
                        return
              endif
	enddo
	print*,' Michelle_weight failed to find tep:',tep
	stop
    END SUBROUTINE MICHELLE_WEIGHT
!**************************************************************************
END MODULE KASAOMEGA_CMN_SUB
