MODULE WCAMERA_SUBS
	implicit none
	real,parameter :: pi=3.141592654

	private
	public :: WCAMERA_ADJACENCY_BUILD
	public :: WCAMERA_gen
	public :: WCAMERA_gen109
	public :: WCAMERA_gen151
	PUBLIC :: WCAMERA_GEN499
	PUBLIC :: WCAMERA_GEN490
	public :: WCAMERA_gen541
	public :: WCAMERA_gen1024
	public :: WCAMERA_PIX
	public :: WCAMERA_NOISE_THRESH_GEN
	public :: WCAMERA_CLEANUP
CONTAINS

   SUBROUTINE WCAMERA_ADJACENCY_BUILD(wptr,pptr)
!*****************************************************************************
!	Set up the adjacency table . Used for image cleanup.
!*****************************************************************************
!	Use a stupid brute
!	force dumb distance algorithum that has the advantage of being easy to
!	understand(and write).
!	With our funny gaps in some of our cameras we can have 7 adjacent pmts
!	Use 0 as flag for no more.

!	28/1/98 GHS	Converted to F90.
	use wcamera_def
	IMPLICIT NONE
	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	
	real :: radius_center,radius_neighbor,w_max_sep2,dist2
	integer :: iadj,i,j


!	Init wadjacent(vector arithritic)
	pptr%wadjacent=0

	do i=1,wptr%npmt		!start with tube i
		iadj=0		!Test everyone else. Init pointer.
		radius_center=pptr%pmt_spacing(i)
		do j=1,wptr%npmt !see if tube j is close enough.
				 !define close enough as less then 2.25 times
				 !pmt seperation.
			radius_neighbor=pptr%pmt_spacing(j)
			if(radius_center==radius_neighbor)then
			    w_max_sep2=(2.25*radius_center)**2
			elseif(radius_center<radius_neighbor)then
			    w_max_sep2=(2.25*radius_center+radius_neighbor)**2
			else
			    w_max_sep2=(radius_center+2.5*radius_neighbor)**2
			endif
				!Square max seperation of
				!centers of pmts to be considered adjacent.
			if(j/=i)then	!Skip on self test.
			    dist2=(pptr%camera_x(i)-pptr%camera_x(j))**2+  &
				 &(pptr%camera_y(i)-pptr%camera_y(j))**2
			    if(dist2<w_max_sep2)then
			      	iadj=iadj+1
			      	pptr%wadjacent(iadj,i)=j !Save adjacent index.
			    endif
			endif
		enddo
	enddo
	return
   END SUBROUTINE WCAMERA_ADJACENCY_BUILD
!*****************************************************************************


    SUBROUTINE WCAMERA_FILL_PST(mhead,npmts)
!*****************************************************************************
!	Load up PST lookup tables
!*****************************************************************************

!Modified:

!       22/9/00 GHS
!               Convert to HDF5 format for pst patterns file.

!       24/01/02 GHS
!               Add Veritas camera (499 pixels,463 in pst). Rename call PST to
!               have 331 or 499 in name. Use npmts to tell which to call.
	use structures
	use wcamera_pst_def
	implicit none

	type(m_head) :: mhead
        integer npmts
	integer :: i,j,error,ios,ierr
	integer,parameter :: n19=2**19
        character(len=9) :: dataset_name
	if(npmts==499)then
           patches_pst=patches_pst_499
           call wcamera_499_pst_make_tables !Makes and fills pst_pixels,
					    !pst-patches, and pst_module
                                            !for VERITAS 499 pixel camera
        else
           patches_pst=patches_pst_331
           call wcamera_331_pst_make_tables !Makes and fills pst_pixels,
					    !pst-patches, and pst_module
                                            !for Whipple 331+ pixel camera
        endif
!Allocate for PST 
       allocate(pst_times(pixels_patch_pst,patches_pst),stat=error)     !*
	if(error/= 0)then
		print*,"Program could not allocte space for pst_times."
	    	stop 'Failure to allocate space for pst_times'
	endif
       allocate(pst_trig_index(pixels_patch_pst,patches_pst),stat=error)     !*
	if(error/= 0)then
		print*,"Program could not allocte space for pst_trig_index."
	    	stop 'Failure to allocate space for pst_trig_index'
	endif
       allocate(patch_trigger_times(patches_pst),stat=error)     !*
	if(error/= 0)then
	    print*,"Program could not allocte space for patch_trigger_times."
	    	stop 'Failure to allocate space for patch_trigger_times'
	endif
       allocate(patch_trigger_times(patches_pst),stat=error)     !*
	if(error/= 0)then
	    print*,"Program could not allocte space for patch_trigger_times."
	    	stop 'Failure to allocate space for patch_trigger_times'
	endif
       allocate(patch_trigger_index(patches_pst),stat=error)     !*
	if(error/= 0)then
	    print*,"Program could not allocte space for patch_trigger_index."
	    	stop 'Failure to allocate space for patch_trigger_index'
	endif
       allocate(patch_trigger_pattern(patches_pst),stat=error)     !*
	if(error/= 0)then
	    print*,"Program could not allocte space for patch_trigger_patytern"
	    	stop 'Failure to allocate space for patch_trigger_pattern'
	endif

 	allocate(pst_patterns(0:n19-1),stat=error)
	if(error/= 0)then
		print*,"Program could not allocte space for pst_patterns."
	    	stop 'PST_PATTERNS allocate failure'
	endif

!   19/9/00 GHS
!   Read PST pattern array from HDF5 file. Do this for portability.
!             First figure out which data set to read
        if (index(mhead%options,'PST2').ne.0)then
               dataset_name='PST_MULT2'
	elseif (index(mhead%options,'PST3').ne.0)then
               dataset_name='PST_MULT3'
	elseif (index(mhead%options,'PST4').ne.0)then
               dataset_name='PST_MULT4'
	else
	    print*,' Unknown PST level requested from options(not 2,3 or 4)'
		stop 'Bad PST level requested'
	endif
!     Call the C routine that opens pst_mult.hdf file and gets pattern array
     print*,' Using HDF5 version of input PST_MULT.HFD:dataset:',dataset_name
        call wcamera_psthdf5_get(dataset_name,pst_patterns,ierr)
        if(ierr<0)then
           print*,' Open file failure for file PST_PATTERNS.HDF'
           stop 'PST_PATTERNS.HDF file open failure'
        endif


!debug

        print*,'Debug:pst3:',(pst_patterns(i),i=0,15)
!enddebug

	close(unit=41)		

	pst_filled=.true.    !Flag that were done this.

	return
    END SUBROUTINE WCAMERA_FILL_PST
!****************************************************************************

    SUBROUTINE WCAMERA_FIND_RUN_NUMBER(unit,options,opt)
!****************************************************************************
!	Search through the file on UNIT for the requested run number sepcified
!	in the RUN=GT****** option from the .par file. Use non-advancing I/O
!	Stop program if run number not found.
!****************************************************************************
	IMPLICIT NONE

	integer :: unit
	character(*) :: options,opt

	integer :: i,istat,idx
	character(len=10) :: run_line
	character(len=80) :: line

	i=index(options,opt)
	i=i+len(opt)+1		!Get to start of run name
    	rewind(unit)

        do
	  			!Using non-advancing i/o here.
				!All run numbers ar 8 char.
    	  read(unit,fmt="(a10)",advance="no",iostat=istat)run_line
				!See if we've reached the end of the file and
				!didn't find the specified run.
                              		!EOF check
         if(istat==-1)then
             print*,' Run # ',options(i:i+7),' not found in &
                  & file assigned to unit#',unit
             print*,'File type:',opt
             stop 'Run not found'
          elseif(istat/=0)then
             print*,'Error reading file assigned to unit#',unit,'istat:',istat
             print*,'File type:',opt,'Run:',options(i:i+7)
             stop 'HRC file read error'
          endif
				!See if this line has the run number on it.
				!Run numbers are 8 char long.
          idx=index(run_line,options(i:i+7))
          if(index(run_line,options(i:i+7))==0)then
		    				!Read the rest of the line so
						!we can start at the beginiing
						!of the next one.
             read(unit,fmt="(a)",advance="yes",iostat=istat)line
             cycle		!Try another
          else
             !We found it.
             return
          endif
       enddo
   END SUBROUTINE WCAMERA_FIND_RUN_NUMBER
!*****************************************************************************


   SUBROUTINE WCAMERA_GEN(wptr,pptr,mhead)
!******************************************************************************
!	Get PMT locations in a standard Generic (hex pattern) Whipple 10m Camera
!	focal plane.  Good for 37,151,270,331 cameras with constant pixel size.
!******************************************************************************

!	All parameters are picked up from WPTR.

!	DEFINITION OF ARRAY LINES:
!	Set up lines,and sector table
!	(Note: the terms lines and RINGS are used somewhat interchangeably in 
!	this code.)
!	We Divide our hexagonal pixel array into 6 sectors. In each sector
!	we have lines of pmts(or light cones) perpendicular to the bisecter
!	of the sector.	The array lines(6,line,sector) tells us for each line
!	in each sector:
!	First index:
!	1:perpendicular distance from the center of the line to the center of
!		the field of view.
!	2:Half length of line from center of line to end.
!	3:1/2 width of the pixel seperation(1/2 width of the line of PMTs etc.)
!	  divided by cos(30deg). This cos(30) term is for use with lightcones.
!	  It reaches into interseces bewtween pixesl when we close pack them.

!	4:PMT index for the first pixel on the line(counting clockwise)
!	5:PMT index for the lastt pixel on the line(counting clockwise)
!		Note that in sector 1 only this number is less then the
!		others on that line(which increase each by one from the first).
!		This is because it is also the first pixel in sector 6.
!		The last pixel in the lines in sector 1 dosen't follow
!		consecutivly with the rest of the pixels in the line.(pixel
!		number otherwise increases in the clockwise direction along
!		the line).
!	6:Number of pixels in the line. 

!	Set up adjacent pixel table. This table is a lookup table of those
!	pmts considered adjacent to the particular pmt. Adjacency is 
!	defined	as only having 1 lightcone boundry between adjacent pixels.
!	

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	02/9/94

!	Modified:

!	27/1/98 GHS
!		Converted to F90.

!	18/5/98 GHS
!		3:Add MHEAD%CONCENTRATION paramerter to m_head in
!		  STRUCTURES.F90. Used to calculate efficiency along with
!		  MHEAD%REFLECTIVITY. Read in from parameter file in DATAIN.
!		  Add pptr%efficiency array. Fill with efficiency factor in
!		  WCAMERA_GEN with better light cone model.

!	20/5/98 GHS
!		For speed reasons in WCAMERA_PIX redefine lines(3,..) to
!		alread have the /cosd(30) term in it.

!	02/3/99 GHS
!		Add stuff to initalize lookup tables from a disk file
!		for PST processing.
!		Includes pst_patches,pst_pixels and pst_module. Add a call to
!		WCAMERA_FILL_PST to do this IF we have at least 331 pixels.
!		and the 'PST' option is chosen.
!		Only do this once in a multi camera evnrionment.
!      03/03/04 GHS V:1:3:8:3:4:4.14
!                Add in allocation and filling of pptr%pe_dc and pptr%ped
!      30/03/04 GHS V:1:3:8:3:4:4.17
!                Add in allocation and filling of pptr%timing_offset
!      31/03/04 GHS V:1:3:8:3:4:4.18
!                Add in allocation and filling of pptr%single_pe_sigmas


	use structures
	use wcamera_def
	use whipple_telescope
	use wcamera_pst_def
	IMPLICIT NONE

	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	type(m_head) :: mhead

	integer,dimension(:),pointer :: NXX,NYY !pixel grid location arrays
	integer :: nsize,error
	integer :: pix,i,j
	real ::	dist_line,dist_fov	
        real :: gauss,xdummy
        real :: pe_dc_width
!Flag this camers is active
	wptr%on=.true.

!Allocate arrays
	allocate(pptr%wadjacent(wptr%neighbor,wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"wadjacent."
	    	stop ' wadjacent allocate failure'
	endif
	allocate(pptr%camera_x(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"camera_x."
	    	stop 'Failure to allocate space for pptr%camera_x'
	endif
	allocate(pptr%camera_y(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"camera_y."
	    	stop 'Failure to allocate space for pptr%camera_y'
	endif
	allocate(pptr%pmt_spacing(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"pmt_spacing."
	    	stop 'Failure to allocate space forpptr%pmt_spacing' 
	endif
	allocate(pptr%pmt_radius(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"pmt_radius."
	    	stop 'Failure to allocate space for pptr%pmt_radius' 
	endif
	allocate(pptr%efficiency(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"efficiency."
	    	stop 'Failure to allocate space for pptr%efficency' 
	endif
	allocate(pptr%threshold(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"threshold."
	    	stop 'Failure to allocate space for pptr%threshold' 
	endif
	allocate(pptr%wnoise(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"wnoise."
	    	stop 'Failure to allocate space for pptr%wnoise' 
	endif
	allocate(pptr%sky_mean_disc(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"sky_mean_disc."
	    	stop 'Failure to allocate space for pptr%sky_mean_disc' 
	endif

	allocate(pptr%mean_time_gap(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id, &
					& "mean_time_gap."
	    	stop 'Failure to allocate space for pptr%mean_time_gap' 
	endif

	allocate(pptr%high(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"high."
	    	stop 'Failure to allocate space for pptr%high' 
	endif

 	allocate(pptr%low(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id," low."
	    	stop 'Failure to allocate space for pptr%low' 
	endif

 	allocate(pptr%lines(6,wptr%nlines,6),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id," lines."
	    	stop 'Failure to allocate space for pptr%lines' 
	endif


!Allocate grid arrays.
	nsize=(3*wptr%nlines*(wptr%nlines+1)+1)	
	deallocate(nxx,stat=error)
        allocate(nxx(nsize),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for NXX."
	    	stop 'Failure to allocate space for nxx' 
	endif
	deallocate(nyy,stat=error)
        allocate(nyy(nsize),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for NYY."
	    	stop 'Failure to allocate space for nyy'
	endif

!	Set up the grid array for the camera.(its a hex grid)
	CALL wcamera_GRID(wptr%nlines,NXX,NYY)

!	Now determine actual x,y positions of the tubes using the
!	grids.  Note that in the x direction the step size is
!	wptr%spacing_pmt(1) wide.  In the Y direction it is 
!	cos(30)*wptr%spacing_pmt(1) wide due to close packing. 
!	Only index 1 in wptr%spacing_pmt is used in constant pixel size
!	cameras

!	Go through the lines (and the center tube)
!	Using vector arithmitic	here!
!First get pmt half spacing in deg. Convert from wptr%spacing_pmt which is in mm
                                                                  
 pptr%pmt_spacing=wptr%spacing_pmt(1)/(meters_per_deg*1000.) !vector arith

 pptr%pmt_radius=wptr%radius_pmt(1)/(meters_per_deg*1000.)   !vector arith
	pptr%camera_y=pptr%pmt_spacing(1)*nyy*cosd(30.)		  !vector arith
	pptr%camera_x=pptr%pmt_spacing(1)*nxx			  !vector arith
			!Note that this value of the pmt_spacing is the pmt
			!seperation.

!	Fill in pptr%lines:: distance,the pmt_spacing, and the first pixel 
!index.
	pix=2
	do i=1,wptr%nlines
		dist_line=(i*(2*pptr%pmt_spacing(1)))
		dist_fov=cosd(30.)*dist_line
		pix=pix+(i-1)*6
		do j=1,6	!over sectors
			pptr%lines(1,i,j)=dist_fov
			pptr%lines(2,i,j)=dist_line/2.+pptr%pmt_spacing(1)
			pptr%lines(3,i,j)=pptr%pmt_spacing(1)/cosd(30.)
				!Now fill in the pixel index
			pptr%lines(4,i,j)=pix + (i*6) - j*i
			pptr%lines(5,i,j)=pptr%lines(4,i,j)+i-1
			pptr%lines(6,i,j)=i+1
		enddo
			!Last of sector 1 is same as first of sector 6
		pptr%lines(5,i,1)=pptr%lines(4,i,6)
	enddo

!	Set up the adjacency table. Used for image cleanup.
	call wcamera_adjacency_build(wptr,pptr)

!Max angle(in deg) of field of view(plus a little)
	wptr%wcamera_min_dn=(pptr%lines(1,wptr%nlines,1)+  &
	& 2.*pptr%lines(3,wptr%nlines,1)*cosd(30.))/cosd(30.0)
					!Convert to a direction cosign.
	wptr%wcamera_min_dn=cosd(wptr%wcamera_min_dn)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!Efficiency factor
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! mhead%reflectivity:Accounts for dirty mirrors,dusty air and any other linear
!	             reduction(increase) of number of photons that make pe's
! mhead%concentration:Accounts for overall innefficiency of light cones.Its is
!		      assumed that the active area of the pmts
!		      (Ap=pi*pptr%pmt_radius**2) is 100% efficient(after
!		      application of mhead%reflectivity.see above) and that
!		      light on the remaining area of pixel's hexagonal area is 
!		      collected by the light cones which are
!		      mhead%concentration efficient.
!	We need to cal;uclate this speratly for each camera/pmt  combination
!	since the efficiency is dependent on pptr%pmt_spacing and
!	pptr%pmt_radius(pmt_spacing is half spacing!!!!!)
!	Note pmt area/hex area=(pi*pmt_radius**2)/sqrt(3)*2*pmt_spacing/2)
!			      = 0.9068996*(pmt_radius/pmt_spacing)**2 

	pptr%efficiency=mhead%reflectivity*                  &    !Vector arith
  &   ( (0.9069*(pptr%pmt_radius/pptr%pmt_spacing)**2) +     &
  &  mhead%concentration*(1.0- (0.9069*(pptr%pmt_radius/pptr%pmt_spacing)**2)))

!***************************************************************************
! Fill in pe_dc.   GHS 10/03/04
! This is an effort to supply a small source of noise to the 
! max1,2,3 distributions. Model is that gain corrections in the real data 
! analysis had some error. We model this by jittering our gains (pe_dc) with a
! gaussian distribution, centered at pe_dc, specified as command line input 
! parameter with a sigma of pe_dc_sigma*pe_dc (also specified in the command 
! line). That is : pe_dc_sigma is the sigma of pe_dc as a fraction of pe_dc.

 	allocate(pptr%pe_dc(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id," pe_dc."
	    	stop 'Failure to allocate space for pptr%pe_dc' 
	endif

       if(pe_dc_sigma==0)then
                                     !No jitter(default)
           pptr%pe_dc=pe_dc                                       !Vector Arith
        else
           pe_dc_width=pe_dc_sigma*pe_dc
           print*,'Using gaussian distributed pe_dc errors.width:',&
                & pe_dc_width
          do i=1,wptr%npmt
             pptr%pe_dc(i)=pe_dc+gauss(xdummy)*pe_dc_width
          enddo
        endif
!We further also try to account for a pedestal subtration error by jiggleing
! the ADC of each pmt (this has no effect on threholds at this point in our 
!model) Use ped_sigma read in as command line parameter (option -t).
  	allocate(pptr%ped(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id," ped."
	    	stop 'Failure to allocate space for pptr%ped' 
	endif
        if(ped_sigma==0)then
           pptr%ped=0                      !Default
        else
           do i=1,wptr%npmt
              pptr%ped(i)=gauss(xdummy)*ped_sigma
           enddo
        endif
!In order to try to dirty up things a little more add a timing offset to each 
!channel. Pick offset for each PMT randomly from a gaussian dist with width of 
!timing_offset_sigma
 	allocate(pptr%timing_offset(wptr%npmt),stat=error)
	if(error/= 0)then
             print*,"Program could not allocte space for p",wptr%camera_id,&
                  &" timing_offset."
	    	stop 'Failure to allocate space for pptr%timing_offset' 
	endif
        if(timing_offset_sigma==0)then
           pptr%timing_offset=0                      !Default
        else
           print*,'Using gaussian distributed timing offsets/pmt:width:',&
                & timing_offset_sigma,' ns'
           do i=1,wptr%npmt
              pptr%timing_offset(i)=gauss(xdummy)*timing_offset_sigma
           enddo
        endif
 
!More dirtyness:Add an individual single Pe pulse height sigma per channel. 
!Pick single pe sigma for each PMT randomly from a gaussian dist with width of 
!pulse_heigh_width_sigma and a center at pulse_height_width_mean
 	allocate(pptr%single_pe_sigmas(wptr%npmt),stat=error)
	if(error/= 0)then
             print*,"Program could not allocte space for p",wptr%camera_id,&
                  &" single_pe_sigmas."
	    	stop 'Failure to allocate space for pptr%single_pe_sigmas' 
	endif
        if(pulse_height_width_mean==0)then
           print*,'Using default single pe pulse height sigma all pmts:'
           pptr%single_pe_sigmas=0                      !Default
        else
           print*,'                Single Pe pulse heights centered at:',&
                &pulse_height_width_mean,' pes'
           print*,'     Single PePulse heights have distribution width:',&
                & pulse_height_width_sigma,' pes'
           do i=1,wptr%npmt
              do 
                 pptr%single_pe_sigmas(i)= &
                   & gauss(xdummy)*pulse_height_width_sigma + &
                   & pulse_height_width_mean
                 if(pptr%single_pe_sigmas(i)>.1)then !keep reasonable range
                    exit
                 endif
              enddo
           enddo
        endif
 
!debug
!        print*,'Debug:single_pe_sigmas'
!        do i=1,wptr%npmt
!           print*,i,pptr%single_pe_sigmas(i)
!        enddo
!endebug
        
!***************************************************************************



	write(6,1000)wptr%camera_id,wptr%ntrigger_in,pptr%efficiency(1)
1000	format(' ******',a,' PMT Camera Initalized',/, &
     & '                 Number of inner pixels in trigger  =',I10/,&
     & '                 Pixel 1 light collection effciency = ',f10.4)


	deallocate(nxx)
	deallocate(nyy)
	return
   END SUBROUTINE WCAMERA_GEN
!******************************************************************************

!******************************************************************************
   SUBROUTINE WCAMERA_GEN1024(wptr,pptr,mhead)
!******************************************************************************
!	Get PMT locations for Laura's square pixel camera. (for use in
!	testeing wavelet method of image cleanup.
!	1024 1/2" pixels on square spacing 32 x 32 tubes:
!******************************************************************************

!NOTE: 	Set up adjacent pixel table. This table is a lookup table of those
!	pmts considered adjacent to the particular pmt. Adjacency is defined
!	as only having 1 lightcone boundry between adjacent pixels.

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	21/3/00

!	pixels are arranged in a 32 x 32 square array. Define pixel 1 as most
!	negative in x and most negative in y. ie lower left hand corner
!	Count pixels with fastest change being in x within y. That is:
!	pixel 2 is bottom row,second column. 
!	This implies center(0,0) of cemera is the upper right corner of
!	pixel 15*32+16=496. 0,0 is thus bounded by pixels 496,497(below) and
!	16*32+16= 528 and pixel 529.

	use structures
	use wcamera_def
	use whipple_telescope
	IMPLICIT NONE

	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	type(m_head) :: mhead
	

	integer :: pix,i,j,k,istart,NPIX
	real ::	dist_line,dist_fov	
	real :: ring_spacing,outer_ang_step,ang_offset
	integer :: error

!	Allocate all the needful arrays

!Flag this camers is active
	wptr%on=.true.

!Allocate arrays
	allocate(pptr%wadjacent(wptr%neighbor,wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"wadjacent."
	    	stop 'Failure to allocate space for ppte%wadjacent' 
	endif
	allocate(pptr%camera_x(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"camera_x."
	    	stop 'Failure to allocate space for pptr&camera_x'
	endif
	allocate(pptr%camera_y(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"camera_y."
	    	stop 'Failure to allocate space for pptr%camera_y' 
	endif
	allocate(pptr%pmt_spacing(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"pmt_spacing."
	    	stop 'Failure to allocate space for pptr%pmt_spacing' 
	endif
	allocate(pptr%pmt_radius(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"pmt_radius."
	    	stop 'Failure to allocate space for pptr%pmt_radius' 
	endif
	allocate(pptr%efficiency(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"efficiency."
	    	stop 'Failure to allocate space for pptr%efficiency' 
	endif
	allocate(pptr%threshold(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"threshold."
	    	stop 'Failure to allocate space for pptr%threshold' 
	endif
	allocate(pptr%wnoise(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"wnoise."
	    	stop 'Failure to allocate space for pptr%wnoise' 
	endif
	allocate(pptr%sky_mean_disc(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"sky_mean_disc."
	    	stop 'Failure to allocate space for pptr%sky_mean_disc' 
	endif

	allocate(pptr%mean_time_gap(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id, &
					& "mean_time_gap."
	    	stop 'Failure to allocate space for pptr%mean_time_gap' 
	endif

	allocate(pptr%high(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"high."
	    	stop 'Failure to allocate space for pptr%high' 
	endif

 	allocate(pptr%low(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id," low."
	    	stop 'Failure to allocate space for pptr%low' 
	endif

!	fill up arrays.
!First get pmt half spacing in deg. Convert from wptr%spacing_pmt which is in mm
 pptr%pmt_spacing=wptr%spacing_pmt(1)/(meters_per_deg*1000.)  !vector arith
 pptr%pmt_radius=wptr%radius_pmt(1)/(meters_per_deg*1000.)    !vector arith

	k=1			!count pmts.
	do i=1,32       	!iterate over rows
						            !vector arith
		pptr%camera_y(k:k+31)=(i-17+.5)*pptr%pmt_spacing(1)*2
		do j=1,32       !iterate over columns
			pptr%camera_x(k)=(j-17+.5)*pptr%pmt_spacing(1)*2
			!seperation.
                        k=k+1
		enddo
	enddo


!	Set up the adjacency table. Used for image cleanup.
	call wcamera_adjacency_build(wptr,pptr)

!Max angle(in deg) of field of view plus a little
	wptr%wcamera_min_dn= sqrt( &
		& (pptr%camera_x(1024)+2*pptr%pmt_spacing(1024))**2 + &
		& (pptr%camera_y(1024)+2*pptr%pmt_spacing(1024))**2)
					!Convert to a direction cosign.
	wptr%wcamera_min_dn=cosd(wptr%wcamera_min_dn)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!Efficiency factor
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! mhead%reflectivity:Accounts for dirty mirrors,dusty air and any other linear
!	             reduction(increase) of number of photons that make pe's
! mhead%concentration:Accounts for overall innefficiency of light cones.Its is
!		      assumed that the active area of the pmts
!		      (Ap=pi*pptr%pmt_radius**2) is 100% efficient(after
!		      application of mhead%reflectivity.see above) and that
!		      light on the remaining area of pixel's hexagonal area is 
!		      collected by the light cones which are
!		      mhead%concentration efficient.
!	We need to calculate this speratly for each camera/pmt  combination
!	since the efficiency is dependent on pptr%pmt_spacing and
!	pptr%pmt_radius(pmt_spacing is half spacing!!!!!)
!	Note pmt area/pixel area =pi*(pmt_radius**2)/(2*pmt_spacing)**2
!		for square pixels =.7854*(pmt_radius/pmt_spacing)**2

	pptr%efficiency=mhead%reflectivity*                  &    !Vector arith
  &   ( (0.7854*(pptr%pmt_radius/pptr%pmt_spacing)**2) +     &
  &   mhead%concentration*(1.0- (0.7854*(pptr%pmt_radius/pptr%pmt_spacing)**2)))

	write(6,1000)wptr%camera_id,wptr%ntrigger_in,pptr%efficiency(1)
1000	format(' ******',a,' PMT Camera Initalized',/, &
     & '                 Number of inner pixels in trigger =',I6/,&
     & '                 Pixel 1 light collection effciency=',f6.4)


	return
   END SUBROUTINE WCAMERA_GEN1024
!******************************************************************************
   SUBROUTINE WCAMERA_GEN109(mhead)
!******************************************************************************
!	Get PMT locations in the Whipple 11m High Resolution Camera
!	focal plane. 109 tubes
!	1 center pixel + 5 complete rings + a 6 th ring with just 3 pixels in 
!	the center of each side. Constant pixel spacing.
!******************************************************************************

!NOTE:	See WCAMERA_GEN for definition of lines,and sector table.
!	Set up adjacent pixel table. This table is a lookup table of those
!	pmts considered adjacent to the particular pmt. Adjacency is defined
!	as only having 1 lightcone boundry between adjacent pixels.

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	02/9/94

!		The 6th ring of new camera looks like the other rings except
!	that on each side (there are 6 sides of a hexagonical 'ring') only
!	the center 3 positions have pmts in them for a total of 18 pmts in
!	the 6ith ring.

!	We going to be quick and dirty here. We will treat this like a
!	standard 127 pixel camera (6 lines) by modifing the W109%... array..
!	We will then restore W109 and move things around as is appropriate
!	for the 6th ring. It gives us a little extra length in our arrays
!	but we can ignore that.


	use structures
	use wcamera_def
	IMPLICIT NONE

	type(m_head) :: mhead
	integer :: pix,i,j,k,istart,npix
	real ::	dist_line,dist_fov	
	
!	Flag camera is on.
	w109%on=.true.

!	Fake a normal 127 pixel camera
	w109%npmt=127
	call wcamera_gen(w109,p109,mhead)
!	restore pmt number
	w109%npmt=109

!	Outer ring. The 6 th ring looks like the other rings with the exception
!	that only 18 positions of the line are filled with PMT's. These 
!	positions consist of the 3 center positions on each of the 6 sides.
!	Outer ring (#6)
!	Only pixel postions have to be moved. Pmt_spacing are all set and all
!	the same already

	npix=3 	!number of pixels centerd per side.
	i=6
	k=(i-1)*i*3+1		!gets to last pixel thats already ok.
	call wcamera_ring_build(p109,k,i,npix)

!redo adjacency table(since we moved outer ring a bit and changed the pmt #
!back to 109)
	call wcamera_adjacency_build(w109,p109)
	return
   END SUBROUTINE WCAMERA_GEN109
!******************************************************************************

   SUBROUTINE WCAMERA_GEN151(mhead)
!******************************************************************************
!	Get PMT locations in the Whipple 11m High Resolution Camera
!	focal plane. 151 tubes
!	1 center pixel + 6 complete rings + a 7 th ring with just 4 pixels in 
!	the center of each side. Constant pixel spacing.
!******************************************************************************

!NOTE:	See WCAMERA_GEN for definition of lines,and sector table.
!	Set up adjacent pixel table. This table is a lookup table of those
!	pmts considered adjacent to the particular pmt. Adjacency is defined
!	as only having 1 lightcone boundry between adjacent pixels.

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	02/9/94

!		The 7th ring of new camera looks like the other rings except
!	that on each side (there are 6 sides of a hexagonical 'ring') only
!	the center 4 positions have pmts in them for a total of 24 pmts in
!	the 7th ring.

!	We going to be quick and dirty here. We will treat this like a
!	standard 169 pixel camera (7 lines) by modifing the W151%... array..
!	We will then restore W151 and move things around as is appropriate
!	for the 7th ring. It gives us a little extra length in our arrays
!	but we can ignore that.


	use structures
	use wcamera_def
	IMPLICIT NONE

	type(m_head) :: mhead
	integer :: pix,i,j,k,istart,npix
	real ::	dist_line,dist_fov	
	
!	Flag camera is on.
	w151%on=.true.

!	Fake a normal 169 pixel camera
	w151%npmt=169
	call wcamera_gen(w151,p151,mhead)
!	restore pmt number
	w151%npmt=151

!	Outer ring. The 7 th ring looks like the other rings with the exception
!	that only 24 positions of the line are filled with PMT's. These 
!	positions consist of the 4 center positions on each of the 6 sides.
!	Outer ring (#7)
!	Only pixel postions have to be moved. Pmt_spacing are all set and all
!	the same already
	npix=4 	!number of pixels centerd per side.
	i=7
	k=(i-1)*i*3+1		!gets to last pixel thats already ok.
	call wcamera_ring_build(p151,k,i,npix)


!redo adjacency table(since we moved outer ring a bit and changed the pmt #
!back to 151)
	call wcamera_adjacency_build(w151,p151)
	return
   END SUBROUTINE WCAMERA_GEN151
!******************************************************************************

!******************************************************************************
   SUBROUTINE WCAMERA_GEN490(wptr,pptr,mhead)
!******************************************************************************
!	Get PMT locations in the Whipple Upgraded Camera focal plane. 490 
!	!tubes:
!	1 center pixel + 10 complete rings + a 11 th ring with 8 pixels in 
!	the center of each side.  379 1/2" pixels
!	+ 3 true circles of 1" pixels of 37 pixels each: 111 pixels
!	Constant pixel spacing.
!******************************************************************************

!NOTE:	See WCAMERA_GEN for definition of lines,and sector table.
!	Set up adjacent pixel table. This table is a lookup table of those
!	pmts considered adjacent to the particular pmt. Adjacency is defined
!	as only having 1 lightcone boundry between adjacent pixels.

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	15/5/98

!		The 11th ring of this camera looks like the other rings except
!	that on each side (there are 6 sides of a hexagonical 'ring') only
!	the center 8 positions have pmts in them for a total of 48 pmts in
!	the 11th ring.
!	We going to be quick and dirty here. We will treat this like a
!	standard 396 pixel camera (11 lines) by modifing the wptr%... array..
!	We will then restore 379 pixels and move things around as is appropriate
!	for the 11th ring. It gives us a little extra length in our 
!	arrays	but we can ignore that.


	use wcamera_pst_def
	use structures
	use wcamera_def
	use whipple_telescope
	IMPLICIT NONE

	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	type(m_head) :: mhead
	

	integer :: pix,i,j,k,istart,NPIX
	real ::	dist_line,dist_fov	
	real :: ring_spacing,outer_ang_step,ang_offset

!	Flag camera is on.
	wptr%on=.true.

!	Fake a normal hexagonal 547 pixel camera(WCAMERA_GEN allocates all
!		the arrays so we have to be the next biggest complete hexagon
!		after 490.

	wptr%npmt=547
	call wcamera_gen(wptr,pptr,mhead)
!	restore pmt number
	wptr%npmt=490			!Includes outer rings we have to add.

!	Ring#11. The 11 th ring looks like the other rings with the exception
!	that only 48 positions of the line are filled with PMT's. These 
!	positions consist of the 8 center positions on each of the 6 sides.
!	Only pixel postions have to be moved. Pmt_spacing are all set and all
!	the same already
	npix=8		!number of pixels centerd per side.
	i=11		!Ring number
	k=(i-1)*i*3+1		!gets to last pixel thats already ok.
	call wcamera_ring_build(pptr,k,i,npix)

!	Since the next 3 rings of 1" pmts are in true circles(not hexagons) we
!	add them explicitly here.
	outer_ang_step=360.0/37.0	!Angular step size in deg of outer pmts
					!around their circles.
	k=379			!Index to last pmt that was ok.
	do j=1,3
	     ring_spacing=sind((outer_ang_step)/2) * &
			&((w490_outer_ring_dia(j)/2)/meters_per_deg)*2
	  do i=1,37
	     if(j==2)then
                ang_offset=outer_ang_step/2
             else
                ang_offset=0
             endif
             
             k=k+1
				!Camera_x,Camera_y,PMT_sapacing are all in deg.
				!w490_outer_ring_dia is in meters.
	     pptr%camera_x(k)=cosd((i-1)*outer_ang_step+ang_offset)* &
			&((w490_outer_ring_dia(j)/2)/meters_per_deg)
	     pptr%camera_y(k)=-sind((i-1)*outer_ang_step+ang_offset)* &
			&((w490_outer_ring_dia(j)/2)/meters_per_deg)

				!pmt_spacing is half spacing!

             pptr%pmt_spacing(k)=ring_spacing/2

	     pptr%pmt_radius(k)=wptr%radius_pmt(2)/ &
			&(meters_per_deg*1000)
				!Only active are of pmt is sensitive(no light
				!cones for outer pmt's) This will be handled
				!in the wcamera_pix routine
             pptr%efficiency(k)=mhead%reflectivity
	     	
	  enddo  !pmt in ring loop
	enddo    !ring loop

!redo adjacency table(since we moved things around in the outer rings
	call wcamera_adjacency_build(wptr,pptr)

!Max angle(in deg) of field of view(plus a little)
     	wptr%wcamera_min_dn= ((w490_outer_ring_dia(3)/2)/ &
		&meters_per_deg)
        wptr%wcamera_min_dn=wptr%wcamera_min_dn+ pptr%pmt_spacing(wptr%npmt)

					!Convert to a direction cosign.
	wptr%wcamera_min_dn=cosd(wptr%wcamera_min_dn)
!If we have PST trigger  fill PST lookup tables
        if ((index(mhead%options,'PST').ne.0).and. &
		& (.not.pst_filled))then
		call wcamera_fill_pst(mhead,wptr%npmt)
	endif
	return
   END SUBROUTINE WCAMERA_GEN490
!******************************************************************************

!******************************************************************************
   SUBROUTINE WCAMERA_GEN499(wptr,pptr,mhead)
!******************************************************************************
!	Get PMT locations in the Veritas Camera focal plane. 499 tubes:
!	1 center pixel + 11 complete rings + a 12 th ring with 11 pixels in 
!	the center of each side + a 13 th ring with 6 pixels centered on each
!	side.
!	Constant pixel spacing.
!******************************************************************************

!NOTE:	See WCAMERA_GEN for definition of lines,and sector table.
!	Set up adjacent pixel table. This table is a lookup table of those
!	pmts considered adjacent to the particular pmt. Adjacency is defined
!	as only having 1 lightcone boundry between adjacent pixels.

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	15/5/98

!		The 12th ring of this camera looks like the other rings except
!	that on each side (there are 6 sides of a hexagonical 'ring') only
!	the center 11 positions have pmts in them for a total of 66 pmts in
!	the 12th ring.
!		In the 13th ring of this camera only the center 6 positions 
!	have pmts in them for a total of 36 pmts in the 13th ring.

!	We going to be quick and dirty here. We will treat this like a
!	standard 547 pixel camera (13 lines) by modifing the wptr%... array..
!	We will then restore 499 pixels and move things around as is appropriate
!	for the 12th and 13th rings. It gives us a little extra length in our 
!	arrays	but we can ignore that.


	use wcamera_pst_def
	use structures
	use wcamera_def
	IMPLICIT NONE

	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	type(m_head) :: mhead
	

	integer :: pix,i,j,k,istart,NPIX
	real ::	dist_line,dist_fov	
	
!	Flag camera is on.
	wptr%on=.true.

!	Fake a normal hexagonal 547 pixel camera
	wptr%npmt=547
	call wcamera_gen(wptr,pptr,mhead)
!	restore pmt number
	wptr%npmt=499

!	Ring#12. The 12 th ring looks like the other rings with the exception
!	that only 66 positions of the line are filled with PMT's. These 
!	positions consist of the 11 center positions on each of the 6 sides.
!	Only pixel postions have to be moved. Pmt_spacing are all set and all
!	the same already
	npix=11		!number of pixels centerd per side.
	i=12
	k=(i-1)*i*3+1		!gets to last pixel thats already ok.
	call wcamera_ring_build(pptr,k,i,npix)

!Ring13
	npix=6		!pixel/side in ring#13
	i=13		!k already set from last call
	call wcamera_ring_build(pptr,k,i,npix)

!redo adjacency table(since we moved things around in the outer rings
	call wcamera_adjacency_build(wptr,pptr)
!If we have PST trigger  fill PST lookup tables
        if ((index(mhead%options,'PST').ne.0).and. &
		& (.not.pst_filled))then
		call wcamera_fill_pst(mhead,wptr%npmt)
	endif

        return
   END SUBROUTINE WCAMERA_GEN499
!******************************************************************************

   SUBROUTINE WCAMERA_GEN541
!******************************************************************************
!	Get PMT locations in the Whipple 10m Ultra High Resolution Camera
!	focal plane. 541 tubes
!******************************************************************************
!	1 center pixel + 10 rings at a spacing of 1/8 deg=.615".   =331 pmts.
!	Then a gap of 1/16 deg=.3075"
!	Then 4 rings of 1/4deg=1.23" pixel spacing.                =180 pmts
!	Choose spacing of .54 deg=2.66" for last ring to give 0 gap.
!	Last ring is an equivalent ring #5.			   = 30 pmts
!							      Total=541 pmts 
!	Total field of view is 5.5*.54*2=5.94deg

!	Note that unlike the other pixel arraingments these pixels vary in
!	size. This requires gaps between the tranisition rings.


!NOTE:	See WCAMERA_GEN for definition of lines,and sector table.

!	Set up adjacent pixel table. This table is a lookup table of those
!	pmts considered adjacent to the particular pmt. Adjacency is 
!	defined	as only having 1 lightcone boundry between adjacent pixels.
!	

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	02/9/94

!	Modified:

!	27/1/98 GHS
!		Converted to F90.

!	20/5/98 GHS
!		For speed reasons in WCAMERA_PIX redefine lines(3,..) to
!		alread have the /cosd(30) term in it.

	use wcamera_def
	use whipple_telescope
	IMPLICIT NONE

	real :: thresh_mult        
	integer,parameter :: ngrid_lines541=10  !Number of lines to make a grid.
	integer,dimension(:),pointer :: NXX,NYY !pixel grid location arrays
	integer :: nsize,error
	integer :: pix,i,j,ir
	real ::	dist_line,dist_fov	

!	Flag camera is on.
	w541%on=.true.

	print*,' WARNING 541 WCAMERA_GEN CODE IS NOT COMPLETE!!!!!'
	print*,' DONT USE 541 CAMERA UNTIL IT IS'
!	pptr%efficiency not done yet!


!Allocate arrays
	allocate(p541%wadjacent(w541%neighbor,w541%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",w541%camera_id,"wadjacent."
	    	stop 'Failure to allocate space for p541%wadjacent' 
	endif
	allocate(p541%camera_x(w541%npmt),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for p541%camera_x."
	    	stop 'Failure to allocate space for p541%camera_x' 
	endif
	allocate(p541%camera_y(w541%npmt),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for p541%camera_y."
	    	stop 'Failure to allocate space for p541%camera_y' 
	endif
	allocate(p541%pmt_spacing(w541%npmt),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for p541%pmt_spacing."
	    	stop 'Failure to allocate space forp541%pmt_spacing' 
	endif
	allocate(p541%pmt_radius(w541%npmt),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for p541%pmt_radius."
	    	stop 'Failure to allocate space for p541%pmt_radius' 
	endif
	allocate(p541%threshold(w541%npmt),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for p541%threshold."
	    	stop 'Failure to allocate space for p541%threshold' 
	endif
	allocate(p541%wnoise(w541%npmt),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for p541%wnoise."
	    	stop 'Failure to allocate space for p541%wnoise' 
	endif
	allocate(p541%high(w541%npmt),stat=error)
	if(error/= 0)then
                print*,"Program could not allocte space for p541%high."
	    	stop 'Failure to allocate space for p541%high' 
	endif

	allocate(p541%low(w541%npmt),stat=error)
	if(error/= 0)then
                print*,"Program could not allocte space for p541%low."
	    	stop 'Failure to allocate space for p541%low' 
	endif

	allocate(p541%sky_mean_disc(w541%npmt),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for p541%sky_mean_disc."
	    	stop 'Failure to allocate space for p541%sky_mean_disc' 
	endif
	allocate(p541%lines(6,w541%nlines,6),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for p541%lines."
	    	stop 'Failure to allocate space for p541%lines' 
	endif


!Allocate grid arrays.
	nsize=(3*ngrid_lines541*(ngrid_lines541+1)+1)	
	deallocate(nxx,stat=error)
        allocate(nxx(nsize),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for NXX."
	    	stop 'Failure to allocate space for nxx'
	endif
	deallocate(nyy,stat=error)
        allocate(nyy(nsize),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for NYY."
	    	stop 'Failure to allocate space for nyy' 
	endif

!	Set up the grid array for the camera.(its a hex grid)
	CALL wcamera_GRID(ngrid_lines541,NXX,NYY)

!	Now determine actual x,y positions of the tubes using the
!	grids.  Note that in the x direction the step size is
!	pmt_spacing541 wide.  In the Y direction its cos(30)*pmt_spacing541 
!	wide due to close packing.

!	Center + Inner 10 lines	
!	Go through the inner 10 lines.(and the center tube: tubes 1:331)
!	Using vector arithmiti!	here!
	p541%pmt_spacing(1:331)=w541%spacing_pmt(1)/(meters_per_deg*1000.)
	p541%pmt_radius(1:331)=w541%radius_pmt(1)/(meters_per_deg*1000.)


	p541%camera_y(1:331)=p541%pmt_spacing(1)*nyy(1:331)*cosd(30.)
	p541%camera_x(1:331)=p541%pmt_spacing(1)*nxx(1:331)
			!Note that this value of the pmt_spacing is the pmt
			!seperation.

!	Middle lines. These lines look like 4 lines of packed tubes starting at
!	equivalent line #6(position #92 in NXY) there is a total of 180 pixels
!	in these 4 lines.
	p541%pmt_spacing(332:511)= &
                              & w541%spacing_pmt(2)/(meters_per_deg*1000.)
	p541%pmt_radius(332:511)= &
                              & w541%radius_pmt(2)/(meters_per_deg*1000.)

	p541%camera_y(332:511)=p541%pmt_spacing(332)*nyy(92:271)*cosd(30.)
	p541%camera_x(332:511)=p541%pmt_spacing(332)*nxx(92:271)

!	Outer ring. This rings look like ring #5(position #62 in NXY) there is
!	a total	of 30 pixels in this ring.
!	Note:We use an effective radius of .264 deg to place the pmts but
!	their size is still just .25 deg.
	p541%pmt_spacing(512:541)= &
                              & w541%spacing_pmt(3)/(meters_per_deg*1000.)
 	p541%pmt_radius(512:541)= &
                              & w541%radius_pmt(3)/(meters_per_deg*1000.)
 
	p541%camera_y(512:541)=p541%pmt_spacing(512)*nyy(62:91)*cosd(30.)
	p541%camera_x(512:541)=p541%pmt_spacing(512)*nxx(62:91)

!	Fill in the lines distance,the pmt_spacing, and the first pixel index.
!	Inner rings
	pix=2
	do i=1,10
		dist_line=(i*(2*p541%pmt_spacing(1)))
		dist_fov=cosd(30.)*dist_line
		pix=pix+(i-1)*6
		do j=1,6	!over sectors
			p541%lines(1,i,j)=dist_fov
			p541%lines(2,i,j)=dist_line/2.+p541%pmt_spacing(1)
			p541%lines(3,i,j)=p541%pmt_spacing(1)/cosd(30.)
				!Now fill in the pixel index
			p541%lines(4,i,j)=pix + (i*6) - j*i
			p541%lines(5,i,j)=p541%lines(4,i,j)+i-1
			p541%lines(6,i,j)=i+1
		enddo
			!Last of sector 1 is same as first of sector 6
		p541%lines(5,i,1)=p541%lines(4,i,6)
	enddo

!	Middle rings
	ir=6
	pix=pix+10*6-(ir-1)*6
	do i=11,w541%nlines-1
		dist_line=(ir*(2*p541%pmt_spacing(332)))
		dist_fov=cosd(30.)*dist_line
		pix=pix+(ir-1)*6
		do j=1,6	!over sectors
			p541%lines(1,i,j)=dist_fov
			p541%lines(2,i,j)=dist_line/2.+p541%pmt_spacing(332)
			p541%lines(3,i,j)=p541%pmt_spacing(332)/cosd(30.)
				!Now fill in the pixel index
			p541%lines(4,i,j)=pix + (ir*6) - j*ir
			p541%lines(5,i,j)=p541%lines(4,i,j)+ir-1
			p541%lines(6,i,j)=ir+1
		enddo
			!Last of sector 1 is same as first of sector 6
		p541%lines(5,i,1)=p541%lines(4,i,6)
		ir=ir+1
	enddo

!	Outer  ring

	ir=5	!equivalent line number.
	pix=pix+9*6
	i=w541%nlines
	dist_line=(ir*(2*p541%pmt_spacing(512)))
	dist_fov=cosd(30.)*dist_line
	do j=1,6	!over sectors
		p541%lines(1,i,j)=dist_fov
		p541%lines(2,i,j)=dist_line/2.+p541%pmt_spacing(512)
		p541%lines(3,i,j)=p541%pmt_spacing(512)/cosd(30.)
				!Now fill in the pixel index
		p541%lines(4,i,j)=pix + (ir*6) - j*ir
		p541%lines(5,i,j)=p541%lines(4,i,j)+ir-1
		p541%lines(6,i,j)=ir+1
	enddo
			!Last of sector 1 is same as first of sector 6
	p541%lines(5,i,1)=p541%lines(4,i,6)

!	Set up the adjacency table. Used for image cleanup.
	call wcamera_adjacency_build(w541,p541)

!Max angle(in deg) of field of view(plus a little)
	w541%wcamera_min_dn=(p541%lines(1,w541%nlines,1)+ &
	& 2.*p541%lines(3,w541%nlines,1)*cosd(30.))/cosd(30.)
					!Convert to a direction cosign.
	w541%wcamera_min_dn=cosd(w541%wcamera_min_dn)

	deallocate(nxx)
	deallocate(nyy)
	return
   END SUBROUTINE WCAMERA_GEN541
!******************************************************************************

SUBROUTINE WCAMERA_GET_DEAD_PMTS(lun,active_pmts,npmt)
!*****************************************************************************
!          Read dead pmts from file pointed to by lun
!          Flag off channels by setting in array active_pmts to 0.
!*****************************************************************************
!Modified:
!      24/06/03 GHS Eas always reading from unit=21. Fixed to read from LUN

    IMPLICIT NONE
      
      integer:: lun
      real,dimension(:) :: active_pmts
      integer :: npmt
      
      integer :: i,istat,mdate,mchannels,dead_pmt,ios,j

      !At this point we are sitting at first dead pmt
      !in hrc.coff file. It follows run number.
      do
         read(lun,1007,advance="no",iostat=istat) dead_pmt
1007     format(i5)
         !Check to see if were done. Eof(-1) or
         !error(probably input conversion on
         !next run #)but not eor(-2)
         if(istat==0)then
            !succesful read
            if(dead_pmt<0.0.or.dead_pmt>npmt)then
               print*,'Fatal-Dead PMT value as read in, out of range'
               print*,'Dead_pmt:npmt',dead_pmt,npmt
               stop 'Dead PMT value out of range'
            endif
            !Flag pmt as dead.
            active_pmts(dead_pmt)=0
            if(lun.eq.22)then
               write(6,1002)dead_pmt
1002	format(' HV for PMT #',i5,' OFF!')
            else
               write(6,1001)dead_pmt
1001    format(' PMT #',i5,' OFF!')
            endif

         elseif(istat==-2)then   	!end of record,. were done
            exit
         else  		!some kind of error. EOF or
                        !input conversion(we reached next run
                        !number). Either way we are done.
            exit
         endif
      enddo
      return
    END SUBROUTINE WCAMERA_GET_DEAD_PMTS
!*****************************************************************************

    SUBROUTINE WCAMERA_GET_PEDVARS(lun,pedvars,npmt)
!*****************************************************************************
!     Read the pedvars form the hrc.cpeds file(lun is opened to this file)
!    At this point we are sitting at the first line in hrc.cpeds file.
!*****************************************************************************
      
      IMPLICIT NONE

      integer :: lun  !Logical unit number of opened hrc.cpeds file.
      real,dimension(:) :: pedvars   !Ouput pedvars array
      integer :: npmt  !Number of pmts to get pedvars for.

      integer :: i,istat,mdate,mchannels,ios
      real,dimension(10) :: pedestal
      real :: pedvar

      !1 space and one character after the run number.
      !There are 2 more words on this line: the date (i6 but we have
      !already read the first character and the number of 
      !channels. We need the last since the data follows as 2 arrays. First 
      !the pedistals, then the ped vars. WE want the second so must skip the
      ! first.
      read(21,fmt="(i5,i7)",advance="yes",iostat=istat)mdate,mchannels
			!read and drop
      i=0
      do 
				   !mchannells covers all adc's(more then npmt)
         read(21,fmt="(10f7.3)",advance="yes",iostat=istat) pedestal
         i=i+10
         if(istat/=0.or.i>=mchannels)then
            exit
         endif
      enddo
	!now read in the ped vars
      i=0
      do 
         if(i==npmt)then
            exit
         endif
				   !we only want first npmt pedvars.
         read(21,fmt="(f7.3)",advance="no",iostat=istat)pedvar 
         if(istat==0)then
            i=i+1
            pedvars(i)=pedvar
         elseif(istat==-2)then   !Keep going if were at end of line
            cycle
         else
            print*,' Error reading pedvars from hrc.cpeds file'
            stop 'Error reading hrc.cpeds'
         endif
      enddo
      return
    END SUBROUTINE WCAMERA_GET_PEDVARS
!******************************************************************************

   SUBROUTINE WCAMERA_GRID(N_RING,NXX,NYY)
!******************************************************************************
!	Wcamera_GRID fills the integer arrays, NXX,NYY, with photomultiplier
!	X and Y lattice indexs. The  Whipple PMT labeling convention is
!	assumed. The number of rings to be filled is specified by N_RING.
!******************************************************************************
!	Carl W. Akerlof
!	Randall Laboratory of Physics
!	University of Michigan
!	Ann Arbor, Michigan  48541
!	July 28, 1990
!	comments added by GHS Purdue. 02/11/93
!	28/1/98 GHS Converted to F90
!		Seperate NXY(2,:) into NXX,NYY for use with vector algebra in 
!		F90 calling routine.
!	Since each ring is actually a hexagon, we have 6 sides to step through.
      IMPLICIT NONE
      INTEGER :: N_RING,I,L,M,N,NX,NY
	integer,dimension(:),pointer:: NXX,NYY

!	Coords of center, not considered a 'ring'.
      NX=0
      NY=0
!	Set tube #1
      NXX(1)=NX
      NYY(1)=NY
      IF (N_RING .LE. 0)  RETURN

!	Move to start of first ring at tube #2
      N=2

!	Go through the rings.
      DO L=1,N_RING

!	Each ring has 1 more pmt per 'side'. Ring #1 has 1, ring#2 has 2 etc.
!	Step up 1 row from end of last ring.
        NX=NX+2
!	Save pointer to start(sort of, this is a funny case, see last 'side'
!	do loop).
        M=N
        N=N+1					!Bump pmt index.

!Side #1
        DO I=1,L
          NX=NX-1		!Side #1 steps down half a tub dia and to the
          NY=NY-2		!left a complete tube dia.
          NXX(N)=NX
          NYY(N)=NY
          N=N+1
        END DO

!Side #2
        DO I=1,L		!Move to the left along a row.
          NX=NX-2
          NXX(N)=NX
          NYY(N)=NY
          N=N+1
        END DO

!Side #3
        DO I=1,L		!Step up 1 row each time and 1/2 pmt to left.
          NX=NX-1
          NY=NY+2
          NXX(N)=NX
          NYY(N)=NY
          N=N+1
        END DO

!Side #4
        DO I=1,L		!Move up to the top of the 'ring'.
          NX=NX+1		!step up a row each time and to the right
          NY=NY+2		!1/2 pmt each time.
          NXX(N)=NX
          NYY(N)=NY
          N=N+1
        END DO

!Side #5
        DO I=1,L
          NX=NX+2		!Along the top of the 'ring now. Step to
          NXX(N)=NX		!the right 1 pmt each time.
          NYY(N)=NY
          N=N+1
        END DO

!Side #6
        DO I=1,L		!Going back towards the starting point of this
          NX=NX+1		!'ring'. Step down 1 row each time and 1/2 to
          NY=NY-2		!the right each time.

!	This is funny and I haven't quit figured it out but it does work.
!	I think the start of the first ring is weird and all the others
!	start 1 over, so except for the first one, finish up the original
!	PMT (in M) for this ring.
        IF (NY .NE. 0)  THEN	!First ring only?(I may be wrong!)
            NXX(N)=NX
            NYY(N)=NY
            N=N+1
          ELSE			!Do first PMT of this ring which was
            NXX(M)=NX		!skipped(M)
            NYY(M)=NY
          ENDIF
        END DO
      END DO
      RETURN
  END SUBROUTINE WCAMERA_GRID                              
!******************************************************************************

    SUBROUTINE WCAMERA_NOISE_THRESH_GEN(WPTR,PPTR,mhead,options)
!***************************************************************************
! Load up ADC and disc noise and trigger threshold arrays
!***************************************************************************

!	Modified:

!	18/5/98 GHS
!		Convert to skyshine noise level generation using better light
!	cone model as see in wcamera_gen. Delete use of HEXAGON_FACTOR

!	13/5/99 GHS V:1:1:5:1:2:1.7
!		Add 'DEAD TUBES' capability. Look for 'DEADT' option. Read in a
!		file of dead tubes. Use that to deactivate bad PMTS in 
!		W10M_TRIGGER. Flages in new array PPTR%ACTIVE_PMTS.

!	26/5/99 GHS V:1:1:5:1:2:1.7
!		Add 'GAIN CORRECTION' capability. Look for 'GAIN' option. Read
!		in a file of pmt gains. Use that to modify pptr%threshold to
!		reflect differeing thresholds in pmts in real camera.
!		This should work for both W10M_PEPULSE and W10M_TRIGGER.
!		Use W10M_GAINS to do this.                                  

!	01/6/99 GHS V:1:1:5:1:2:1.7
!		Add 'SIGMA' capability. Look for 'SIGMA' option. Read
!		in a file of pmt sigmas. These are yet to be corrected for
!		gains and will be used as relative values for noise generation.
!		Relative values must take account of dead pmts also.
!		This should work for both W10M_PEPULSE and W10M_TRIGGER.

!	16/11/99 GHS V:1:1:5:1:2.5
!		Add special stuff for outer rings of 490 pixel camera which
!		doesn't have lightcones. Modifies WNOISE calculation.

!	16/2/00 GHS V:1:1:6:1:2:3.3
!		For the DEADT dead tube option:
!		If a RUN number is specified in the OPTIONS read in DEADT 
!		information from the standard hrc00.coff file.
!		Convert w10m_time_gap to an array

!       12/02/01 GHS
!               Don't allow negative pedvars to be read in  This is to prevent 
!               infinite loop later with negative mean_time_gaps.
!               Don't allow negative noise to be generated for outer pmts

!	30/10/00 GHS V:1:1:6:1:2:3.8
!               Fix a bunch of bugs, most to do with array lengths and number 
!               of PMTS in the multiplicity trigger.
!               WCAMERA:WCAMERA_NOISE_THRESH_GEN
!                 1:Put in a check on values read in for DEAD pmt's. Must be
!                   positive,non-zero and not greater then number of PMTS in 
!                   camera.
!                 2:For 490 pixel camera change so that all small pixels(379)
!                   not just the 331 PST pixels have the wnoise,etc adjusted 
!                   for the new sigmas.
!                 3:Set array limits for the 490 pixels camera(some of which 
!                   are 547 long) to 490.

!	20/03/01 GHS V:1:1:6:1:2:3.9
!               Add an additional active pmt arrry: pptr%active_HV. This will 
!               gives us 2. The first original arrray: pptr%active_pmts is for 
!               pmts that are not to be included in the event reconstruction 
!               (Hillas parameter calculations). The second will be those tubes
!               that have their HV turnned off and thus are not in the trigger.
!               This second array will be a subset of the first. Up until this
!               point we have been using the first array for both purposes. 
!               This causes some pmts which should have been in the trigger 
!               (and were probably very noisy) to not be included in the 
!               trigger when they should have been.

!      20/06/02 GHS:1:1:6:1:2:3.16
!               Add a 'PAD' option to specify a second run to get pedvars from.
!               Padding uses for a specific channel, the bigger of the pedvars
!               for sigma calculations. Must have SIGMA option specified for 
!               PAD option to be used (obviously!).
!               Modify DEADT code: If SIGMA specified and DEADT is not, use
!               SIGMA run for dead pmts. Also: IF SIGMA specified and PAD 
!               specified use both runs for inactive pmts.
!               Modifications to:WCAMERA_NOISE_THRESH_GEN. Added:
!               WCAMERA_GET_PEDVARS, WCAMERA_GET_DEAD_PMTS.

!      03/02/03 GHS
!               Add code to w10m_noise_thresh_gen to douple check that the 
!               mean_time gap gives required pedvars in un der 'TIM' option.

!      24/02/04 GHS
!               Make sure any pmt turned off in active_hv is also turned off
!               in active_pmts.

	use wcamera_def
	use structures
	use whipple_telescope
	use w10m_pepulse_subs
        use kasaomega_command_line
        IMPLICIT NONE

	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	type(m_head) :: mhead
        character(*) :: options

	real :: thresh_mult,ped_width
	real*8,parameter ::  pi=3.141592654
	real,pointer,dimension(:) :: cfd_pedistel
	integer :: nbins,error
	real :: mean_noise,mean_noise2,pulse_pedvar

	integer :: i,istat,mdate,mchannels,dead_pmt,ios,j,k
	character(len=80) :: run_line
	real,dimension(10) :: pedestal
	real :: pedvar
	real,dimension(:),pointer :: raw_noise
	real,dimension(:),pointer :: pad_ped_vars
        character(LEN=80) :: hrc_coff_name
        character(LEN=80) :: hrc_hvoff_name
        character(LEN=80) :: hrc_cpeds_name
        character(LEN=80) :: hrc_cn2gains_name
        integer :: ngate_bins, cfd_low, cfd_hi,ncfd_pedc_sum
        real :: cfd_pedc_sum
        real :: cfd_ped, cfd_sum_peds, cfd_sum_ped2,cfd_mean_ped,cfd_pedvar
        real, parameter :: area_normalize=26.606

	deallocate(pptr%active_pmts,stat=error)
	allocate(pptr%active_pmts(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for ACTIVE_PMTS."
	     stop 'Failure to allocate space for pptr%active_pmts' 
	endif

	deallocate(pptr%active_hv,stat=error)
	allocate(pptr%active_hv(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for ACTIVE_HV."
	     stop 'Failure to allocate space for pptr%active_hv'
	endif
							!Init to all on.
	pptr%active_pmts=1.0				!Vector arith.
	pptr%active_hv=1.0				!Vector arith.

        !Save input raw noise so we can calculate pedc later.
           deallocate(raw_noise,stat=error)
           allocate(raw_noise(wptr%npmt),stat=error)
           if(error/= 0)then
              print*,"Program could not allocte space for raw_noise."
              stop 'Failure to allocate space for raw_noise' 
           endif

!****************************************************************************
!	DEADT option (from options) Loads in 2 active pmt arrays.
!****************************************************************************
!               Modify DEADT code: If SIGMA specified and DEADT is not, use
!               SIGMA run for dead pmts. Also: IF SIGMA specified and PAD 
!               specified use both runs for inactive pmts.
!See if we are to turn some tubes off!
	if(index(options,'DEADT')/=0.or.index(options,'SIGMA')/=0)then
	   !Run number specified after DEADT(or SIGMA) command
	   !Use standard data analysis produced hrc.coff file.
	   !These are pmts that are not to be included in the image analysis
           !(Hillas parameter caluclation) The pmts tthat should not be 
           !included in the trigger is a subset of this file given by 
           !pptr%active_hv (see below)
           !Specific file Linked to hrc.coff by command/scr file
           hrc_coff_name=trim(hrcyear)//".coff"
           OPEN(21,ACCESS='SEQUENTIAL',STATUS='OLD', &
                & FILE=hrc_coff_name,ACTION='READ',iostat=ios)
           if(ios>0)then
              print*,' file open error for', hrc_coff_name
              stop 'hrc.coff file open error'
           endif

           !Now open file of pmts that had their HV turned off. These pmts 
           !will be a subset of pptr%active pmts.
           !There are the only pmts that should not be included in the trigger.
           !Specific file Linked to hrc.hvoff by command/scr file
           hrc_hvoff_name=trim(hrcyear)//".hvoff"
           OPEN(22,ACCESS='SEQUENTIAL',STATUS='OLD', &
                & FILE=hrc_hvoff_name,ACTION='READ',iostat=ios)
           if(ios>0)then
              print*,' file open error for', hrc_hvoff_name,' ios:',ios
              stop 'hrc.hvoff file open error'
           endif

	!search for the run number
           if(index(options,'DEADT')/=0)then
              call wcamera_find_run_number(21,options,'DEADT')
              call wcamera_find_run_number(22,options,'DEADT')
           else
              call wcamera_find_run_number(21,options,'SIGMA')
              call wcamera_find_run_number(22,options,'SIGMA')
           endif

           call wcamera_get_dead_pmts(21,pptr%active_pmts,wptr%npmt)
           
           call wcamera_get_dead_pmts(22,pptr%active_hv,wptr%npmt)

           if(index(options,'SIGMA')/=0.and.index(options,'PAD')/=0)then
              call wcamera_find_run_number(21,options,'PAD')
              call wcamera_get_dead_pmts(21,pptr%active_pmts,wptr%npmt)

              call wcamera_find_run_number(22,options,'PAD')
              call wcamera_get_dead_pmts(22,pptr%active_hv,wptr%npmt)
           endif
                    !those tubes turned off in HV (0 in active_hv) should be a
                    !subset of those turned off in active_pmts(set to zero). 
                    !Make sure that is so.
           do i=1,wptr%npmt
              if(pptr%active_hv(i)==0)then
                 if(pptr%active_pmts(i)/=0)then
                    print*,'PMT: ',i,&
               &" off in HV but not hrc-coff. Turnning it off in active_pmts."
                    pptr%active_pmts(i)=0
                 endif
              endif
           enddo

           close(21)
           close(22)
           wptr%dead_pmts=.true.
	endif
!****************************************************************************


!****************************************************************************
!	SIGMA option (from options)
!****************************************************************************
	if(index(options,'SIGMA').ne.0)then
			!Option 'SIGMA' is to read in sig's from
			!the cpram created hrc*.cpeds file
	    deallocate(pptr%sigmas,stat=error)
	    allocate(pptr%sigmas(wptr%npmt),stat=error)
	    if(error/= 0)then
	         print*,"Program could not allocte space for SIGMAS."
	         stop 'Failure to allocate space for pptr%sigmas'
	    endif
		  !Run number specified after SIGMA command
		  !Use standard data analysis produced hrc.cpeds file.
		  !Specific file Linked to hrc.cpeds by command/scr file, like:
                  ! "ln -s /home/sembroski/analysis/hrc01.cpeds hrc.cpeds"
            Print*,' Using file on assigned to hrc.cpeds for PMT Sigmas'
            hrc_cpeds_name=trim(hrcyear)//".cpeds"
            OPEN(21,ACCESS='SEQUENTIAL',STATUS='OLD', &
                 & FILE=hrc_cpeds_name,ACTION='READ',iostat=ios)
            if(ios>0)then
               print*,' file open error for', hrc_cpeds_name
               stop 'hrc.cpeds file open error'
            endif
                   !search for the run number
            call wcamera_find_run_number(21,options,'SIGMA')
	!At this point we are sitting at the first line in hrc.cpeds file. 
        !Read the pedvars form the hrc.cpeds file(lun is opened to this file)
            call wcamera_get_pedvars(21,pptr%sigmas,wptr%npmt)

!****************************************************************************
!	PAD option (from options)(for padding)
!****************************************************************************
            if(index(options,'PAD').ne.0)then
               !Get pedvars of padding run.
               deallocate(pad_ped_vars,stat=error)
               allocate(pad_ped_vars(wptr%npmt),stat=error)
               if(error/= 0)then
                  print*,"Program could not allocte space for PAD_PED_VARS."
                  stop 'Failure to allocate space for PAD_PED_VARS'
               endif

               !search for the padding run number
               call wcamera_find_run_number(21,options,'PAD')
               !Read the pedvars from the hrc.cpeds file(lun is opened to this 
               !file)
               call wcamera_get_pedvars(21,pad_ped_vars,wptr%npmt)
               !and pad!
               do i=1,wptr%npmt
                  if(pptr%sigmas(i).lt.pad_ped_vars(i))then
                     pptr%sigmas(i)=pad_ped_vars(i)
                  endif
               enddo
            endif

            !Now check stuff
            
            do i=1,wptr%npmt
               if(pptr%sigmas(i)<0.and.pptr%active_hv(i)==1)then
                  print*,'Fatal-Sigma-Negative pedvar read in.i, &
                       &pedvar:',i,pptr%sigmas
                  stop 'Negative pedvar read in.'
               endif
            enddo
                   !and we are done.
	    close(21)
         endif
!****************************************************************************

!****************************************************************************
!	GAIN option (from options)
!****************************************************************************
	deallocate(pptr%gain,stat=error)
	allocate(pptr%gain(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for GAIN."
	     stop 'Failure to allocate space for pptr%gain'
	endif
			!Init to equal gains.
	pptr%gain=1                     	!Vector arith
        
	if(index(options,'GAIN')/=0)then
 	    				  !Run number specified after GAIN
					  !command.
						!Use standard data analysis
						!produced hrc.coff file.
						!Specific file Linked to
						!hrc.coff by command/scr file
	        Print*,' Using file on assigned to hrc.cn2gains for PMT gains'

		!Gains are caculated to match means of adc distributions for 
		!nitrogen runs. So, for example, to match means of real data
		!we multiply by gains. Thus if we had say a gain of 1.4 then 
		!read data would be low. This implies the actual threshold was
		!higher by 1.4 then the mean threshold.
		!Cleanup level should also be adjusted accordingly if raw
		!sigmas are used

	       	hrc_cn2gains_name=trim(hrcyear)//".cn2gains"
                OPEN(21,ACCESS='SEQUENTIAL',STATUS='OLD', &
	    		& FILE=hrc_cn2gains_name,ACTION='READ',iostat=ios)
                if(ios>0)then
                   print*,' file open error for', hrc_cn2gains_name
                   stop 'hrc.cn2gains file open error'
                endif
						!search for the run number
		call wcamera_find_run_number(21,options,'GAIN')

	!At this point we are sitting at the first line in hrc.cn2 gains
	!for a run. Gains start on next line. Finish reading this line.
	        read(21,fmt="(i5,i7)",advance="yes",iostat=istat)mdate, &
                &mchannels
	!now read in the gains
		i=0
		do 
		    if(i==wptr%npmt)then
			exit
		    endif
				   !we only want first npmt gains.
		    read(21,fmt="(f7.3)",advance="no",iostat=istat)pedvar 
		    if(istat==0)then
			    i=i+1
	   		    if(pedvar<0.and.pptr%active_hv(i)==1)then
                               print*,'Fatal-Gain-Negative pedvar read in.i,&
                                    &pedvar:',i,pedvar
                               stop 'Negative pedvar read in.'
                            endif

                            pptr%gain(i)=pedvar
		    elseif(istat==-2)then   !Keep going if were at end of line
			cycle
		    else
			print*,' Error reading pedvars from hrc.cn2gains file'
			stop 'Error reading hrc.cn2gains'
		    endif
		enddo
	    close(21)
	endif
!****************************************************************************

!Load up adc and disc noise:
	if(mhead%noise)then

!Normal noise generation: NUmber of pe's in gate from sky shine.
!	remeber:
!	pptr%efficiency=mhead%reflectivity*(x+mhead&concentration*(1-x))
!	where x=area(pmt)/aread(hex). Calculated in WCAMERA_GEN

!****************************************************************************
!	1024 square pixel camera
!****************************************************************************
	  if(wptr%camera_id=='1024')then
	      pptr%wnoise=mhead%rate_noise * mhead%adc_gate *   & !Vector arith.
	              &  ((2*pptr%pmt_spacing**2)) *            &
	              &  (pptr%efficiency/mhead%reflectivity)

!****************************************************************************
!****************************************************************************
!	All other cameras
!****************************************************************************

          else
	      pptr%wnoise=mhead%rate_noise * mhead%adc_gate *   & !Vector arith.
	              &  (sqrt(3.0)*2)*(pptr%pmt_spacing**2) *  &
	              &  (pptr%efficiency/mhead%reflectivity)
          endif

!****************************************************************************

		!Save mean pedistal width. I want a generic width here. 
                !ped_width is used to convert the input parameter file 
                !threshold level.

          ped_width=sqrt(pptr%wnoise(1))
 	

!****************************************************************************
!	490 pixel camera
!****************************************************************************
	  if(wptr%camera_id=='490')then
!	16/11/99 GHS V:1:1:5:1:2.5
!	outer 111 pixels of 490 pixel camera have no light cones
	  	do i=380,490
			pptr%wnoise(i)=mhead%rate_noise * mhead%adc_gate *   &
	       		&  (pi)*(pptr%pmt_radius(i)**2) *            &
			&  (pptr%efficiency(i)/mhead%reflectivity)
	   		if((pptr%wnoise(i)<0.0).and.pptr%active_hv(i)==1)then
                               print*,'Fatal-Outer pmt-Negative noise &
                                    &generated.i,wnoise,rate_noise:',&
                                    &i,pptr%wnoise(i),mhead%rate_noise
                               print*,'adc_gate,pmt_radius,efficiency,&
                                    &reflectivity:',&
                                    &mhead%adc_gate,pptr%pmt_radius(i), &
                                    & pptr%efficiency(i),mhead%reflectivity

                               stop 'Negative Noise generated outper PMTs'
                            endif
		enddo
	  endif
!****************************************************************************

!****************************************************************************
!If external sigmas have been read in. Modify pptr%wnoise at this point.
!	This assums an ungraded camera or camera w490!!!!!
!****************************************************************************
	  if(index(options,'SIGMA').ne.0)then
!****************************************************************************
!	490 pixel camera
!****************************************************************************
	     if(wptr%camera_id=='490')then
!12/02/01 GHS Allow all small pixels to be adjusted for sigma.
	       	i=379    		!Only small pixels of interest.
	     else
		i=wptr%npmt
		print*,' Ungraded camera assumed in &
			& WCAMERA_NOISE_THRESH_GEN'
	     endif
				!Zero pmts with HV off.
							   !Vector arith
	     pptr%sigmas(1:i)=pptr%sigmas(1:i)*pptr%active_hv(1:i)
				!Convert to noise rate in pmts in ADC counts.
	     pptr%sigmas(1:i)=pptr%sigmas(1:i)**2   		  !Vector arith

                               !Save raw noise measured pedestal in dc counts 
                               !as given by cpeds
             raw_noise(1:i)=pptr%sigmas(1:i)                   !Vector arith
                                !equalize gains
	     pptr%sigmas(1:i)=pptr%sigmas(1:i)*pptr%gain(1:i)     !Vector arith
				!find mean noise of active pmts   !Vector arith
	     mean_noise=sum(pptr%sigmas(1:i))/sum(pptr%active_hv(1:i))
				!modify relative noise to have unitary mean
	     pptr%sigmas(1:i)=pptr%sigmas(1:i)/mean_noise         !Vector arith
				!Modify orignal nosie to match relative levels.
	     pptr%wnoise(1:i)=pptr%wnoise(1:i)*pptr%sigmas(1:i)   !Vector arith
                                !Determine pedc for OUTHFD4(This method of 
                                !calculation includes gains I think!)
          endif
!****************************************************************************

	  		!Zero noise for hv off pmt's. 
	  i=wptr%npmt
          pptr%wnoise(1:i)=pptr%wnoise(1:i)*pptr%active_hv(1:i)   !Vector arith
							          !Vector arith
       pptr%sky_mean_disc(1:i)=pptr%wnoise(1:i)*mhead%disc_width/mhead%adc_gate
							          !Vector arith

          do i=1,wptr%npmt
             if(pptr%active_hv(i)==1)then
                if(pptr%sky_mean_disc(i)>0.0)then
                   pptr%mean_time_gap(i)= &
                        & 1./(pptr%sky_mean_disc(i)/mhead%disc_width)
                else
                   print*,'Fatal-sky_mean_disc<=0.:i,sky_mean_disc:',i,&
                        & pptr%sky_mean_disc(i)
                   stop 'sky_mean_disc<=0'
                endif
             else
                pptr%mean_time_gap(i)=0.0
             endif
          enddo

!****************************************************************************
!	THRESHOLD GENERATION
!****************************************************************************
!Use given value to find what 
! 'multiplicity' of noise sigima in adc gate.
	  if(index(options,'TWIDTH')/=0)then
!For use of pesum as amount of ped sigma as threshold and not raw pes
		   thresh_mult=mhead%pesum
  		   pptr%threshold=ped_width*thresh_mult      !Vector arith.
		   print*,' PE_THRESHOLD is in units of Pedistal Noise sigmas'
	  elseif(index(options,'TPES')/=0)then
		!! Set the disc threshold use mhead%pesum. 
	   	   thresh_mult=mhead%pesum/ped_width
		   pptr%threshold=mhead%pesum                !Vector arith.
		   print*,' PE_THRESHOLD is in units of PEs'
	  else
		print*,' FATAL--No Option chosen to specify threshold type'
		print*,' Legal values are: TWIDTH and TPES'
		stop 'No Option chosen to specify threshold type'
	  endif


				!Modify for gains.
	  pptr%threshold(1:wptr%npmt)= &                      !Vector arith
               & pptr%threshold(1:wptr%npmt)*pptr%gain(1:wptr%npmt)

!*************************************************************************

!Load up thresholds for 541
	  if(wptr%camera_id=='541')then
			! Set the disc threshold in terms of noise sigma for 
			!the 541 camera middle and outer rings.  Use generic 
			!value for innner use multiplicity' of noise sigima
			! to generate that of the middle and outer. Use 
			!constant sigma for now.
					!Middle rings
	      	pptr%threshold(332:511)=thresh_mult*sqrt(pptr%wnoise(332))
					!outer rings
	  	pptr%threshold(512:541)=thresh_mult*sqrt(pptr%wnoise(512))
	  endif

!Cfd threshold needs to retain the mean skynoise.
!Detemine that from a 10 micro -seconds worth of noise
 !      03/02/03 GHS
!               Add code to w10m_noise_thresh_gen to douple check that the 
!               pedvars are created correctly.
         allocate(pptr%mean_noise(wptr%npmt),stat=error)
          if(error/= 0)then
             print*,"Program could not allocte space for MEAN_NOISE array."
             stop 'Failure to allocate space for pptr%mean_noise' 
          endif

	  if( (index(options,'TIM')/=0) .and. &
	      (wptr%camera_id/='499') )then
               nbins=1.e5
             allocate(cfd_pedistel(nbins),stat=error)
             if(error/= 0)then
              print*,"Program could not allocte space for Cfd_pedistel buffer."
              stop 'Failure to allocate space for cfd_pedistel'
             endif
             cfd_pedc_sum=0
             ncfd_pedc_sum=0
             do i=1,wptr%npmt
!                if(pptr%active_hv(i)/=0)then
                if(pptr%active_pmts(i)/=0)then

                   cfd_pedistel=0			       	!vector arith
                   if(pptr%mean_time_gap(i).le.0.0)then
                  print*,'WCAMERA-Fatal--mean_time_gap<=0.i,mean_time_gap:',&
                           & i,pptr%mean_time_gap(i)
                 stop 'KASAOMEGA-WCAMERA-Mean_time_gap<=0. CFD_pedistal calc.'
                   endif
                   call w10m_noise_pulse(cfd_pedistel,pptr%mean_time_gap(i),&
                   & pptr%single_pe_sigmas(i))
                   !Find mean value. Thats our pedistel
                   pptr%mean_noise(i)=sum(cfd_pedistel)/nbins
                   ngate_bins=(mhead%adc_gate/(.25))
                   k=nbins/ngate_bins
                   cfd_sum_peds=0
                   cfd_sum_ped2=0
                   do j=1,k
                      cfd_low=(j-1)*ngate_bins
                      cfd_hi=cfd_low+ngate_bins-1
                      cfd_ped=sum(cfd_pedistel(cfd_low:cfd_hi))
                      cfd_sum_peds=cfd_sum_peds+cfd_ped
                      cfd_sum_ped2=cfd_sum_ped2+cfd_ped**2
                   enddo
                   cfd_mean_ped=(cfd_sum_peds/k)
                   cfd_pedvar= &
                        & sqrt(cfd_sum_ped2/k-cfd_mean_ped**2)/area_normalize
                   cfd_mean_ped=cfd_mean_ped/area_normalize
 !                  print*,' i,pulse ped,data pedvar,pulse pedvar,ratio:',i,&
 !                       &cfd_mean_ped,sqrt(raw_noise(i)), &
 !                       & cfd_pedvar,sqrt(raw_noise(i))/cfd_pedvar
                   cfd_pedc_sum=cfd_pedc_sum+sqrt(raw_noise(i))/cfd_pedvar
                   ncfd_pedc_sum=ncfd_pedc_sum+1
                else
                   pptr%mean_noise(i)=0
                endif
              enddo
              print*,' Mean PEDC from PEDVAR comparisons:',&
              & cfd_pedc_sum/ncfd_pedc_sum
             deallocate(cfd_pedistel,stat=error)
          else
             pptr%mean_noise(i)=0
          endif

!!find multiplicity of trigger. Just infomative really.
	  thresh_mult=thresh_mult/sqrt(mhead%disc_width/mhead%adc_gate)

	  if(wptr%camera_id/='541')then

	    	write(6,1003)wptr%camera_id,ped_width, &
	&  wptr%camera_id,pptr%threshold(1)/pptr%gain(1), &
	&  wptr%camera_id, thresh_mult, &
	&  wptr%camera_id, pptr%mean_noise(1)
1003	format(     &
	& '       ',a,'             Expected pedistal width  =',f10.4,/, &
	& '       ',a,'          Mean Discriminator Threshold =',f10.4,/, &
	& '   ',a,'Effective Mean Disc Threshold Multiplicity =',f10.4,/, &
	& '   ',a,'         Mean Sky Noise Pedistel for CFD 1 =',f10.4)
				  !Mean time gap between noise pulses

	  else
	       	write(6,1005)pptr%threshold(1),pptr%threshold(332), &
					& thresh_mult
1005	format(     &
	& ' 541 PMT Camera:Discriminator Threshold:Innr:middle =',2f10.4,/, &
	& ' 541 PMT Camera:Effective Threshold Multiplicity    =',f10.4)
					  !Mean time gap between noise pulses
	  endif


	else
       	  pptr%wnoise=0
	  pptr%sky_mean_disc=0
	  write(6,1004)wptr%camera_id,pptr%threshold
1004	format(     &
	& '      ',a,' PMT Camera:Discriminator Threshold: =',f10.4)
	endif
        return
    END SUBROUTINE WCAMERA_NOISE_THRESH_GEN
!******************************************************************************

    SUBROUTINE WCAMERA_PIX(x,y,pptr,wptr,ipix)
!******************************************************************************
!	Returns the pixel that a pe at X,Y would hit.
!******************************************************************************
!	We Divide our hexagonal pixel array into 6 sectors. In each sector
!	we have lines of pmts(or light cones) perpendicular to the bisecter
!	of the sector.	The array pptr%lines(6,line,sector) tells us for each 
!	line in each sector:
!	First index:
!	1:perpendicular distance from the center of the line to the center of
!		the field of view.
!	2:Half length of line from center of line to end.
!	3:1/2 width of the pixel seperation(1/2 width of the line of PMTs etc.)
!	  divided by cos(30deg). This cos(30) term is for use with lightcones.
!	  It reaches into interseces bewtween pixesl when we close pack them.
!	4:PMT index for the first pixel on the line(counting clockwise)
!	5:PMT index for the lastt pixel on the line(counting clockwise)
!		Note that in sector 1 only this number is less then the
!		others on that line(which increase each by one from the first).
!		This is because it is also the first pixel in sector 6.
!		The last pixel in the lines in sector 1 dosen't follow
!		consecutivly with the rest of the pixels in the line.(pixel
!		number otherwise increases in the clockwise direction along
!		the line).
!	6:Number of pixels in the line. 

!	LIGHT CONE VERSION:Too use as a non light cone version, change
!		WCAMERA_GEN,WCAMERA_GEN151,WCAMERA_GEN541 to fill the
!		pmt_spacing array with radii that match the actual sensitive
!		area of the PMTs. Also comment out the lines indicated below
!		with the '!LIGHTCONE' comment.

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	02/9/94

!	Modified:

!	29/1/98 GHS	Convert to F90.

!	20/5/98 GHS
!		For speed reasons redefine (in WCAMERA_GEN) lines(3,..) to
!		alread have the /cosd(30) term in it.
!		Also for speed, check if pe is outside last line, and start
!		looking from the last line in.

!	21/3/00 GHS V:1:1:6:1:2.6
!		Add code for Laura's 1024 square pixel camera

        use wcamera_def
	IMPLICIT NONE

	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	real :: x,y,theta,theta_sec,distance,dist_fov,dist_line
	real :: dist1_sqr,dist2_sqr
	integer :: ipix,isector,i,k,ipix_index,j
	integer,dimension(2) ::  ipixel


!1024 square pixel camera.
	if(wptr%camera_id=='1024')then
		i=(x/(2*pptr%pmt_spacing(1)))+17	!x=0 is lower edge of column 17
		if(i<1.or.i>32)then             !Check if in camera
			ipix=0
			return
		endif
		j=(y/(2*pptr%pmt_spacing(1)))+17    !y=0 is lower edge of row 17
		if(j<1.or.j>32)then             !Check if in camera
			ipix=0
			return
		endif
		ipix=(j-1)*32+i			!Pixel number.
		return
	endif


!	First get the theta angle for X,Y. Use quadrant infromation.
!Find first quadrant angle.
	if(x.eq.0.and.y.ne.0)then
		theta=90.		!On verticle axis
	elseif(y.eq.0)then	
		theta=0.			!On hoizontal axis
	else
		theta=atand(abs(y/x))			!Quadrant ++
	endif
	if(x.ge.0)then
		if(y.lt.0)then
			theta=360.-theta		!Quadrant +-
		endif
	else
		if(y.ge.0)then
			theta=180.-theta			!Quadrant -+
		else
			theta=180.+theta			!Quadrant --
		endif
	endif
		
!Determine the sector
	isector=theta/60. + 1	!Round down to the sector
	if(isector.lt.1)then
	print*,' Info only--isector out of range:isector,theta:',isector,theta
		isector=1
	elseif(isector.gt.6)then
	print*,' Info only--isector out of range:isector,theta:',isector,theta
		isector=6
	endif

!	Determine angle to bisecter of sector
	theta_sec=-(mod(theta,60.)-30.)	!Positive is in clockwise direction.

!	Find distance of X,Y to center of field of view.
	distance=sqrt(x**2+y**2)

!	Find projected perpendicular distance from center of field of view
!	along bisector line.
	dist_fov=cosd(theta_sec)*distance

!	Check if we are beyond last line.
   	if((dist_fov-pptr%lines(1,wptr%nlines,isector)).gt.   &
	       & pptr%lines(3,wptr%nlines,isector))then
		ipix=0
		return
	endif



!	Now search through the lines to see which ones to look at. There
!	may be more then one.(At most 2)
	k=0
				!Note:zeroith line is just the center pixel
				!which has a maximum reach into its hex cell
				! the the same as the pixel spacing/cosd(30deg)
				! of the first line.
	if(dist_fov.lt.pptr%lines(3,1,isector))then
	   k=1
	   ipixel(k)=1
	endif

!	Find perpendicul distance along the line from the bisector of the
!	point X,Y
	dist_line=sind(theta_sec)*distance

!20/5/98 GHS
!	For speed: Start at the outside lines and work back. Pe most lilkely
!	           on outer lines by and area argument(more pixels outer lines)
	do i=wptr%nlines,1,-1
		!Find distance for effective reach of light cones.
!20/5/98 GHS
!The next 2 lines were where the 2 lines that the program spent most of its
!time in (a 13 % each). Eliminate one line by redefineing lines(3,..) to
!alread have the /cosd(30) term in it (in WCAMERA_GEN)
!	   dist_cones=pptr%lines(3,i,isector)/cosd(30.)
!	   if(abs(dist_fov-pptr%lines(1,i,isector)).le.dist_cones)then

	   if(abs(dist_fov-pptr%lines(1,i,isector)).le.   &
	       & pptr%lines(3,i,isector))then
				!Ok weve got one radially
				!Make sure we also have it along the line.
	   	if(abs(dist_line).le.pptr%lines(2,i,isector))then
					!pixel index from start of line.
					!The cosd(30) gets us back to pixel
					! spacing from lightcone reach.
	   	   ipix_index=(pptr%lines(2,i,isector)+dist_line)/ &
			     & (2*pptr%lines(3,i,isector)*cosd(30.))
		    k=k+1	!Bump pixel counter.
		    ipixel(k)=ipix_index+pptr%lines(4,i,isector)
			!See is this is the out of order pixel in
			!sector 1: pptr%lines(6,i,isector) is number of pixels
			!in a complete line. )
			!Do we need to change pixel value?
		    if(isector.eq.1.and.   &
		    	& (ipix_index.eq.pptr%lines(6,i,isector)-1))then
							!last one sector 1
			    ipixel(k)=pptr%lines(5,i,1)
		    endif
     		    if(k==2)then		!quit at 2
			exit
		    endif
					!Test if we are inside pixel(not in hex
					!area)		   
		    dist1_sqr=(pptr%camera_x(ipixel(1))-x)**2+  &
			&  (pptr%camera_y(ipixel(1))-y)**2
		    if(dist1_sqr.le.pptr%pmt_spacing(ipixel(1))**2)then
			exit
		    endif
		endif
	    endif
	enddo

	ipix=0			!Initialize for a miss!
	if(k.eq.1)then
				!If only one. use it
		ipix=ipixel(1)

	elseif(k.eq.2)then
		!We have 2 possible pixels.
		dist2_sqr=(pptr%camera_x(ipixel(2))-x)**2+ &
 			&  (pptr%camera_y(ipixel(2))-y)**2
				! First see if were inside this one.
		if(dist2_sqr.le.pptr%pmt_spacing(ipixel(2))**2)then
			ipix=ipixel(2)
	!Note that for different pixel sizes and gaps between rings this is
	!pretty crude.  This is the LIGHT CONE version. It doesn't work
	!very well on the outside of the outer rings. We get a few we 
	!probably shouldn't.
				!Light cone test.
		elseif(dist1_sqr.le.dist2_sqr)then		      !LIGHTCONE
			ipix=ipixel(1)				      !LIGHTCONE
		else						      !LIGHTCONE
			ipix=ipixel(2)				      !LIGHTCONE
		endif
	endif
	return			
   END SUBROUTINE WCAMERA_PIX
!******************************************************************************

   SUBROUTINE WCAMERA_PST_GCA_MAKE_HEXAGON(RADIUS,NT,N,X,Y) 
!*****************************************************************************
!     Compute x,y coordinates of tubes on a a hexagon  
!*****************************************************************************
      IMPLICIT NONE 

!---- arguments 
      REAL     RADIUS    ! distance of corner tubes from center 
      INTEGER  NT        ! number of tubes per side of hexagon       
      INTEGER  N         ! total number of tubes so far
      REAL     X(:),Y(:) ! tube position 

!---- 
      REAL     EPS           ! 
      REAL     STEP          ! pi/3.0
      REAL     XC(7),YC(7)   ! the seven corners 
      INTEGER  I,J

!---- central tube? 
      IF (NT.LE.0) THEN
        N    = 1 
        X(N) = 0.0 
        Y(N) = 0.0
        RETURN 
      END IF 
!-----
      STEP =  -(4.0/3.0) * ATAN(1.0)
      DO I=1,7
        XC(I) = RADIUS * COS(STEP*(I-1)) 
        YC(I) = RADIUS * SIN(STEP*(I-1)) 
      END DO
!---- 
      DO I=1,6                                 ! all sides 
      DO J=1,NT                                ! all tubes 
       N = N + 1                               ! increment tube count 
       EPS  = FLOAT(J-1) / FLOAT(NT)           ! 
       X(N) = XC(I) + (XC(I+1)-XC(I)) * EPS    ! 
       Y(N) = YC(I) + (YC(I+1)-YC(I)) * EPS   
      END DO
      END DO

      RETURN 
   END SUBROUTINE WCAMERA_PST_GCA_MAKE_HEXAGON 
!*****************************************************************************

   SUBROUTINE WCAMERA_331_PST_MAKE_TABLES
!*****************************************************************************
!		Initalize lookup tables for PST processing.
!*****************************************************************************

!	Most of this came from Joachims GRANITE code: GR_TRIGGER.F90 AND
!	GR_CAMERA.F90.

	use wcamera_pst_def

	IMPLICIT NONE

        integer :: npatch,i,j,k,pix,error
	integer,pointer,dimension(:) :: pst_module	!Only filled 
	integer,pointer,dimension(:,:) :: pst_patches	!here. Not
					!used anywhere yet but maybe someday.
      INTEGER ::IMODULE, IPATCH   ! module, patch
      INTEGER ::IPTCHU(19)        ! u-position within patch
      INTEGER ::IPTCHV(19)        ! v-position within patch
      INTEGER ::IMIDPTCHU( 5)     ! u-position patch center
      INTEGER ::IMIDPTCHV (5)     ! v-position patch center
      INTEGER x,y,u,v,A,B,C,D   
	
!---- description of modules 
      TYPE :: MODULE_T  
        INTEGER U,V                    ! position of center 
        INTEGER A,B,C,D                ! rotation matrix coefficients 
      END TYPE 
      TYPE(MODULE_T) :: MODULE(modules_pst_331)                              !*
  
!---- pixel position for each of 19 bits 
      DATA IPTCHU / 0,-1,-2,-2,-2,+1,0,-1,-1,-1,+2,+1,0,0,0,+2,1,+1,+2/
      DATA IPTCHV /-2,-1,0,+1,+2,-2,-1,0,+1,+2,-2,-1,0,+1,+2,-1,0,+1,0/

!---- patch position within module for each of 5 patches  
      DATA IMIDPTCHU / -4, -2, 0, +2, +4 /
      DATA IMIDPTCHV /  0,  0, 0,  0,  0 /

!---- position of module centre (channel 13 of patch 3) 
      DATA MODULE%U     /+4,+4,+4,+4,+4,-2, 0,+2,+4,-2,-4,-6,-8/             !*
      DATA MODULE%V     / 0,-2, 0,+2,+4,-2,-4,-6,-8,+4,+4,+4,+4/             !*
!     module rotation        <+60 deg >, < 180 deg >,  <-60 deg>             !*
      DATA MODULE%A    / 1, +1,+1,+1,+1, -1,-1,-1,-1,  0, 0, 0, 0 /          !*
      DATA MODULE%B    / 0, +1,+1,+1,+1,  0, 0, 0, 0, -1,-1,-1,-1 /          !*
      DATA MODULE%C    / 0, -1,-1,-1,-1,  0, 0, 0, 0, +1,+1,+1,+1 /          !*
      DATA MODULE%D    / 1,  0, 0, 0, 0, -1,-1,-1,-1, +1,+1,+1,+1 /          !*


!Allocate for PST tables
       allocate(pst_pixels(pixels_patch_pst,patches_pst),stat=error)     !*
	if(error/= 0)then
		print*,"Program could not allocte space for pst_pixels."
	    	stop 'Failure to allocate space for pst_pixels'
	endif

			! 7=max patches that a pixel can be in(may
			! actually be less!)
 	allocate(pst_patches(7,463),stat=error)                              !*
	if(error/= 0)then
		print*,"Program could not allocte space for pst_patches."
	    	stop 'Failure to allocate space for pst_patches'
	endif

 	allocate(pst_module(patches_pst),stat=error)                     !*
	if(error/= 0)then
		print*,"Program could not allocte space for pst_module."
	    	stop 'Failure to allocate space for pst_module'
	endif

!First define the pixel coordinates in U,V,X,Y.  Use GCA code from GRANiTE.
	call wcamera_pst_pixel_define_331                                    !*


!Module 1 has only 1 patch used. Do it first
	imodule=1
	npatch=1				!Number patches
	pst_module(npatch)=imodule	   	!Set module #
			!This is code adopted from gtr_decode_pst from granite.
	ipatch=1               			!Only first 1 used module #1
	do j=1,pixels_patch_pst			!Go through pixels
            X = IPTCHU(J)+IMIDPTCHU(IPATCH)     ! x-position within a module  
            Y = IPTCHV(J)+IMIDPTCHV(IPATCH)        ! y-position 
            A = MODULE(IMODULE)%A                  ! get rotation matrix
            B = MODULE(IMODULE)%B                  !   coefficients for 
            C = MODULE(IMODULE)%C                  !   this module 
            D = MODULE(IMODULE)%D                  !   ready to use 
            U = A*X + B*Y                          ! rotate, module 
            V = C*X + D*Y                          ! rotate, module 
            U = U + MODULE(IMODULE)%U              ! translate module in u  
            V = V + MODULE(IMODULE)%V              ! translate module in v 
            pst_pixels(j,ipatch) = PIXEL(U,V) ! store pixel number 
	enddo

!now do the rest
	do imodule=2,modules_pst_331		   !Iterate of modules       !*

	    do ipatch=1,patches_module_pst         !iterate over patches 
                                                   !within modules.
		npatch=npatch+1			   !Lable patch.
		pst_module(npatch)=imodule	   	!Set module #

		do j=1,pixels_patch_pst	   !Go through pixels
	            X = IPTCHU(J)+IMIDPTCHU(IPATCH) !x-position within a 
                                                    !module 
	            Y = IPTCHV(J)+IMIDPTCHV(IPATCH) ! y-position 
	            A = MODULE(IMODULE)%A           ! get rotation matrix
	            B = MODULE(IMODULE)%B           !   coefficients for 
	            C = MODULE(IMODULE)%C           !   this module 
	            D = MODULE(IMODULE)%D           !   ready to use 
	            U = A*X + B*Y                   ! rotate, module 
	            V = C*X + D*Y                   ! rotate, module 
	            U = U + MODULE(IMODULE)%U       ! translate module in u  
	            V = V + MODULE(IMODULE)%V       ! translate module in v 
            	    pst_pixels(j,npatch) = PIXEL(U,V) ! store pixel number 
		enddo
	    enddo							
	enddo
       print*,' 331 pixel pst trigger in use'

!Now find all patches for a pixel
	pst_patches=0				!vector arith.
	do j=1,patches_pst                                              !*
		do i=1,pixels_patch_pst
                   pix=pst_pixels(i,j)
                   if(pix/=0)then
                      do k=1,7
                         if(pst_patches(k,pix)==j)then
                            exit
                         elseif(pst_patches(k,pix)==0)then
                            pst_patches(k,pix)=j
                            exit
                         elseif(k==7)then
                            print*,' More then 7 patches for a pixel'
                         endif
                      enddo
                   endif
                enddo
	enddo

!done
	return

   END SUBROUTINE WCAMERA_331_PST_MAKE_TABLES
!****************************************************************************

   SUBROUTINE WCAMERA_499_PST_MAKE_TABLES
!*****************************************************************************
!		Initalize lookup tables for PST processing.
!*****************************************************************************

!	Most of this came from Joachims GRANITE code: GR_TRIGGER.F90 AND
!	GR_CAMERA.F90.

	use wcamera_pst_def

	IMPLICIT NONE

        integer :: npatch,i,j,k,pix,error
	integer,pointer,dimension(:) :: pst_module	!Only filled 
	integer,pointer,dimension(:,:) :: pst_patches	!here. Not
					!used anywhere yet but maybe someday.
      INTEGER ::IMODULE, IPATCH   ! module, patch
      INTEGER ::IPTCHU(19)        ! u-position within patch
      INTEGER ::IPTCHV(19)        ! v-position within patch
      INTEGER ::IMIDPTCHU( 5)     ! u-position patch center
      INTEGER ::IMIDPTCHV (5)     ! v-position patch center
      INTEGER x,y,u,v,A,B,C,D   
	
!---- description of modules 
      TYPE :: MODULE_T  
        INTEGER U,V                    ! position of center 
        INTEGER A,B,C,D                ! rotation matrix coefficients 
      END TYPE 
      TYPE(MODULE_T) :: MODULE(modules_pst_499)                              !*
  
!---- pixel position for each of 19 bits 
      DATA IPTCHU / 0,-1,-2,-2,-2,+1,0,-1,-1,-1,+2,+1,0,0,0,+2,1,+1,+2/
      DATA IPTCHV /-2,-1,0,+1,+2,-2,-1,0,+1,+2,-2,-1,0,+1,+2,-1,0,+1,0/

!---- patch position within module for each of 5 patches  
      DATA IMIDPTCHU / -4, -2, 0, +2, +4 /
      DATA IMIDPTCHV /  0,  0, 0,  0,  0 /

!---- position of module centre (channel 13 of patch 3) 
    DATA MODULE%U /+4,+6,+6,+6,+6,+6,+6, 0,-2,-4,-6,-8,-10,-6,-4,-2, 0,+2, +4/
    DATA MODULE%V / 0,-6,-4,-2, 0,+2,+4,+6,+6,+6,+6,+6, +6, 0,-2,-4,-6,-8,-10/

!     module rotation    <+60 deg >,         <-60 deg>        < 180 deg >
    DATA MODULE%A / 1,+1,+1,+1,+1,+1,+1, 0, 0, 0, 0, 0,  0,-1,-1,-1,-1,-1, -1/
    DATA MODULE%B / 0,+1,+1,+1,+1,+1,+1,-1,-1,-1,-1,-1, -1, 0, 0, 0, 0, 0,  0/
    DATA MODULE%C / 0,-1,-1,-1,-1,-1,-1,+1,+1,+1,+1,+1, +1, 0, 0, 0, 0, 0,  0/
    DATA MODULE%D / 1, 0, 0, 0, 0, 0, 0,+1,+1,+1,+1,+1, +1,-1,-1,-1,-1,-1, -1/


!Allocate for PST tables
       allocate(pst_pixels(pixels_patch_pst,patches_pst),stat=error)     !*
	if(error/= 0)then
		print*,"Program could not allocte space for pst_pixels."
	    	stop 'Failure to allocate space for pst_pixels'
	endif

			! 7=max patches that a pixel can be in(may
			! actually be less!)
 	allocate(pst_patches(7,463),stat=error)                              !*
	if(error/= 0)then
		print*,"Program could not allocte space for pst_patches."
	    	stop 'Failure to allocate space for pst_patches'
	endif

 	allocate(pst_module(patches_pst),stat=error)                     !*
	if(error/= 0)then
		print*,"Program could not allocte space for pst_module."
	    	stop 'Failure to allocate space for pst_module'
	endif

!First define the pixel coordinates in U,V,X,Y.  Use GCA code from GRANiTE.
	call wcamera_pst_pixel_define_499                                    !*


!Module 1 has only 1 patch used. Do it first
	imodule=1
	npatch=1				!Number patches
	pst_module(npatch)=imodule	   	!Set module #
			!This is code adopted from gtr_decode_pst from granite.
	ipatch=1               			!Only first 1 used module #1
	do j=1,pixels_patch_pst			!Go through pixels
            X = IPTCHU(J)+IMIDPTCHU(IPATCH)     ! x-position within a module  
            Y = IPTCHV(J)+IMIDPTCHV(IPATCH)        ! y-position 
            A = MODULE(IMODULE)%A                  ! get rotation matrix
            B = MODULE(IMODULE)%B                  !   coefficients for 
            C = MODULE(IMODULE)%C                  !   this module 
            D = MODULE(IMODULE)%D                  !   ready to use 
            U = A*X + B*Y                          ! rotate, module 
            V = C*X + D*Y                          ! rotate, module 
            U = U + MODULE(IMODULE)%U              ! translate module in u  
            V = V + MODULE(IMODULE)%V              ! translate module in v 
            pst_pixels(j,ipatch) = PIXEL(U,V) ! store pixel number 
	enddo

!now do the rest
	do imodule=2,modules_pst_499		   !Iterate of modules       !*

	    do ipatch=1,patches_module_pst         !iterate over patches 
                                                   !within modules.
		npatch=npatch+1			   !Lable patch.
		pst_module(npatch)=imodule	   	!Set module #

		do j=1,pixels_patch_pst	   !Go through pixels
	            X = IPTCHU(J)+IMIDPTCHU(IPATCH) !x-position within a 
                                                    !module 
	            Y = IPTCHV(J)+IMIDPTCHV(IPATCH) ! y-position 
	            A = MODULE(IMODULE)%A           ! get rotation matrix
	            B = MODULE(IMODULE)%B           !   coefficients for 
	            C = MODULE(IMODULE)%C           !   this module 
	            D = MODULE(IMODULE)%D           !   ready to use 
	            U = A*X + B*Y                   ! rotate, module 
	            V = C*X + D*Y                   ! rotate, module 
	            U = U + MODULE(IMODULE)%U       ! translate module in u  
	            V = V + MODULE(IMODULE)%V       ! translate module in v 
            	    pst_pixels(j,npatch) = PIXEL(U,V) ! store pixel number 
		enddo
	    enddo							
	enddo

       print*,' 463 pixel pst trigger in use'

!Now find all patches for a pixel
	pst_patches=0				!vector arith.
	do j=1,patches_pst                                              !*
		do i=1,pixels_patch_pst
                   pix=pst_pixels(i,j)
                   if(pix/=0)then
                      
                      do k=1,7
                         if(pst_patches(k,pix)==j)then
                            exit
                         elseif(pst_patches(k,pix)==0)then
                            pst_patches(k,pix)=j
                            exit
                         elseif(k==7)then
                            print*,' More then 7 patches for a pixel'
                         endif
                      enddo
                   endif
                enddo
	enddo
!done
	return

   END SUBROUTINE WCAMERA_499_PST_MAKE_TABLES
!****************************************************************************

   SUBROUTINE WCAMERA_PST_PIXEL_DEFINE_331
!*****************************************************************************
!     define camera geometry  for Whipple 331 pst trigger
!*****************************************************************************

	use wcamera_pst_def
      IMPLICIT NONE 

      INTEGER  :: I 
      INTEGER  :: N
      INTEGER  :: IC 
      


      REAL :: SCALE                ! distance scale in U,V directions 

!-----------------------------------------------------------------------------
!     331 pixel camera  only
!-----------------------------------------------------------------------------

        ALLOCATE(XDEG(  331))
        ALLOCATE(YDEG(  331)) 
        ALLOCATE(UPOS  (331))
        ALLOCATE(VPOS  (331))
        ALLOCATE(PIXEL (-12:12,-12:12))

	scale=1.0			!It just doesn't matter here!
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 0, 0,N,XDEG,YDEG) !   1 +  6
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 1, 1,N,XDEG,YDEG) !   7 + 12
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 2, 2,N,XDEG,YDEG) !  19 + 18 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 3, 3,N,XDEG,YDEG) !  37 + 24
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 4, 4,N,XDEG,YDEG) !  61 + 30 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 5, 5,N,XDEG,YDEG) !  97 + 36 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 6, 6,N,XDEG,YDEG) ! 127 + 42
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 7, 7,N,XDEG,YDEG) ! 169 + 48 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 8, 8,N,XDEG,YDEG) ! 217 + 54 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 9, 9,N,XDEG,YDEG) ! 271 + 60  
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale*10,10,N,XDEG,YDEG) ! 331 

!-----------------------------------------------------------------------------
!     find (u,v) pixel coordinates  
!-----------------------------------------------------------------------------

      UPOS = NINT( (XDEG-YDEG/TAND(60.0)) / SCALE ) 		!vector arith
      VPOS = NINT( (YDEG/SIND(60.0)) / SCALE )                  !vector arith


      PIXEL = 0                                                 !vector arith

      DO I=LBOUND(UPOS,1),UBOUND(UPOS,1)   
        PIXEL(UPOS(I),VPOS(I)) = I 
      ENDDO  

	return
   END SUBROUTINE WCAMERA_PST_PIXEL_DEFINE_331  
!*****************************************************************************

   SUBROUTINE WCAMERA_PST_PIXEL_DEFINE_499
!*****************************************************************************
!     define camera geometry  for Veritas pst trigger (463 pixels in PST)
!*****************************************************************************

	use wcamera_pst_def
      IMPLICIT NONE 

      INTEGER  :: I,id,isource,k,j 
      INTEGER  :: N
      INTEGER  :: IC 
      


      REAL :: SCALE                ! distance scale in U,V directions 

!-----------------------------------------------------------------------------
!     Veritas 499  pixel camera  only
!-----------------------------------------------------------------------------

        ALLOCATE(XDEG(  469))  !469 pixels in 13 rings(counting the center 
                               !pixel as a ring(ring 0)). 
        ALLOCATE(YDEG(  469))  !6 pixels are not used in trigger. These 6 are 
                               !the corner pixels of 13th ring, where we don't
        ALLOCATE(UPOS  (463))  ! have pmts. See below how we modifiy things 
        ALLOCATE(VPOS  (463))  !for that.

        ALLOCATE(PIXEL (-12:12,-12:12)) !Indexies are for U,V. For U  go up to
                                        ! +/-12. for 13 rings where  center 
                                        !ring(center pixel) is 
                                        !coord 0. Same for V. Obviously all 
                                        !set of U,V values not used. This just
                                        !spans the pixels with some extra room 
                                        !on the sides. (U,V are axis along 
                                        !pixel lines with 60 deg angle between
                                        ! them.)

	scale=1.0			!It just doesn't matter here!
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 0, 0,N,XDEG,YDEG) !   1 +  6
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 1, 1,N,XDEG,YDEG) !   7 + 12
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 2, 2,N,XDEG,YDEG) !  19 + 18 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 3, 3,N,XDEG,YDEG) !  37 + 24
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 4, 4,N,XDEG,YDEG) !  61 + 30 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 5, 5,N,XDEG,YDEG) !  97 + 36 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 6, 6,N,XDEG,YDEG) ! 127 + 42
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 7, 7,N,XDEG,YDEG) ! 169 + 48 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 8, 8,N,XDEG,YDEG) ! 217 + 54 
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale* 9, 9,N,XDEG,YDEG) ! 271 + 60  
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale*10,10,N,XDEG,YDEG) ! 331 + 66
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale*11,11,N,XDEG,YDEG) ! 397 + 72
        CALL WCAMERA_PST_GCA_MAKE_HEXAGON(scale*12,12,N,XDEG,YDEG) ! 469  
!*****************************************************************************
!Now we have to remove 6 of the pixels of ring 13(the corners) which have no 
!PMT's in the veritas 499 pixel camera.( and then compress stuff)
!*****************************************************************************
        id=398    !(destination)First corner
        isource=399
        do k=1,6            !six corners 
           do j=0,10
              xdeg(id)=xdeg(isource+j)
              ydeg(id)=ydeg(isource+j)
              id=id+1
           enddo
           isource=isource+12
        enddo
        id=id-1  !# of pixels(should be 463)
!*****************************************************************************


!-----------------------------------------------------------------------------
!     find (u,v) pixel coordinates  
!-----------------------------------------------------------------------------


      UPOS = NINT( (XDEG(1:id)-YDEG(1:id)/TAND(60.0)) / SCALE )	!vector arith
      VPOS = NINT( (YDEG(1:id)/SIND(60.0)) / SCALE )            !vector arith


      PIXEL = 0                                                 !vector arith

      DO I=LBOUND(UPOS,1),UBOUND(UPOS,1)   
        PIXEL(UPOS(I),VPOS(I)) = I 
      ENDDO  

	return
   END SUBROUTINE WCAMERA_PST_PIXEL_DEFINE_499  
!*****************************************************************************


    SUBROUTINE WCAMERA_CLEANUP(adcin,adcout,wptr,pptr,ibright,iboundry, &
		& neighbors_max,npmts)
	
!******************************************************************************
!	Cleanup the image in the ADC array for use later in genrating
!	Hillas parameters.
!******************************************************************************

!	This is done in preperation for Hillas paramerter 
!	calculations of the image. As we know such cacluations are RMS
!	calculations and need some type of Robustness applied to them. This
!	'filtering' is the robustness. As such, changeing the filtering will
!	change the Hillas parameter distributions.
!	Method:
!	1:Find and keep all pixels with values 4.25 sigma above background
!	  sigma.
!	2:Find and keep all remaining pixes that are adjacent to these 4.25
!	sigma pixels that have counts of 2.25 of the bacground sigma.

!       Also, Determine the pmt with max number of nearest neighbors
!	in the image. Clearly max value is never greater then 6. Return
!	value in NEIGHBORS_MAX.

!	Modified:

!	29/1/98 GHS	Convert to F90.

!	04/3/98 GHS V:1:1:5:1:1:1.2
!		Use pptr%low, pptr%high for cleanup. Generated from widths.

!	01/6/99 GHS
!		Specifically check for pptr%high and pptr%low being 0. If
!		so it indicates a dead tube. Skip such a channel.
	use structures
	use whipple_telescope
	use wcamera_def
	IMPLICIT NONE

	real,pointer,dimension(:) :: adcin,adcout
	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	integer :: ibright,iboundry	!Output: Number of bright and boundry
					!pixels found in the image.
	integer :: neighbors_max        !Maximum number of nearest neighborrs
					!found.

	integer :: npmts,i,j,k		!Input: Number of pmts to search.
	integer :: neighbors_temp,error

	

!	Search for pmts above hiclean sigma.
	ibright=0

!Do BRIGHTS!
	do i=1,npmts
		if(pptr%high(i)>0.and.adcin(i).ge.pptr%high(i))then
			adcout(i)=adcin(i)
			ibright=ibright+1
		else
			adcout(i)=0		!Init all channels.
		endif
	enddo

!	Now look for adjacent channels for low keepers
	iboundry=0
!Do BOUNDRYS
	do i=1,npmts
	    if(pptr%high(i)>0.and.adcout(i).ge.pptr%high(i))then
	    	do j=1,wptr%neighbor
	    	    k=pptr%wadjacent(j,i)	!Get adjacent channel
						!0 means no more.

		    if(k.eq.0)exit  		!Done

		    if(k.le.npmts)then 		!Make sure its a boundry and
						!not selected as a boundry or
						!picture pixel previously
			if(pptr%low(k)>0.and.adcin(k).ge.pptr%low(k))then
				if(adcout(k).eq.0)then
					adcout(k)=adcin(k)
					iboundry=iboundry+1
				endif
			endif
		    endif
		enddo
	    endif
	enddo

!	Count nearest neigbors
	neighbors_max=0
	!Check that This PMT hit. If so look for neighbors.
	!Quit if we get max possible number of neighbors
	do i=1,npmts
	    if(pptr%high(i)>0.and.adcout(i).ge.pptr%high(i))then
	       	neighbors_temp=0
			!Get adjacent channel. 0 means no more.
	    	do j=1,wptr%neighbor
		   	k=pptr%wadjacent(j,i)
			if(k.eq.0)exit
			if(k.le.npmts)then
	  			if(pptr%high(k)>0.and. &
				&  adcout(k).ge.pptr%high(k))then
				     	neighbors_temp=neighbors_temp+1
				endif
			endif
		enddo
		if(neighbors_temp.gt.neighbors_max)then
			neighbors_max=neighbors_temp
			if(neighbors_max.eq.wptr%neighbor)exit
		endif
	    endif
	enddo
	return
   END SUBROUTINE WCAMERA_CLEANUP


!******************************************************************************
    SUBROUTINE WCAMERA_RING_BUILD(pptr,k,i,npix)
!******************************************************************************
!	Build stuff into arrays for paritally filled rings(usually done to
!	circlize a camera.
!******************************************************************************
!	pptr points to all stuff we need for fill up
!	k:index tod of last pixel in previous full ring. Assumtion is
!	camera_x and and camera_y arrays were first filled as if for a fully 
!	populated camera.
!	i=ring number to build. I+1 is also the number of pixels/side for a full
!		ring.
!	npix=number of pixels per side to use for this ring centered on each
!	side
!	npix must always be 1 or 3 or 5.etc less then i(to be centered

!	modified:

!	20/5/98 GHS
!		For speed reasons in WCAMERA_PIX redefine lines(3,..) to
!		alread have the /cosd(30) term in it.

	use wcamera_def

	IMPLICIT NONE

	type(camera_ptrs) :: pptr
	integer :: k,i,npix

	integer :: istart,j,pix,ij
	real ::	dist_line,dist_fov	

	istart=(i-1)*i*3+2		!gets position if first pixel in this
					!ring(which we may not want)
	istart=istart+(i-npix+1)/2	!Position of first pixel we do want
					!(i-npix always odd)

	istart=istart-i			!We start i  higher then this
	pix=k+1 	!First pixel of this ring.

	do j=1,6		!Six sides
		istart=istart+i
		DO Ij=istart,istart+(npix-1)	!npix of them only.
					!on center of side
			k=k+1
			pptr%camera_x(k)=pptr%camera_x(ij)
			pptr%camera_y(k)=pptr%camera_y(ij)
			pptr%pmt_spacing(k)=pptr%pmt_spacing(ij)
			pptr%pmt_radius(k)=pptr%pmt_radius(ij)
			pptr%efficiency(k)=pptr%efficiency(ij)
		enddo
	enddo
!Now do lines of ring#i partially filled ring(only npix center tubes each side)
	dist_line=(i*(2*pptr%pmt_spacing(1)))
	dist_fov=cosd(30.)*dist_line
	do j=1,6	!over sectors
		pptr%lines(1,i,j)=dist_fov
		pptr%lines(2,i,j)=pptr%pmt_spacing(1)*npix
		pptr%lines(3,i,j)=pptr%pmt_spacing(1)/cosd(30.)
			!Now fill in the pixel index
		pptr%lines(4,i,j)=pix + 6*npix - j*npix	!This steps us
							!backwards through
							!pixel numbers.
		pptr%lines(5,i,j)=pptr%lines(4,i,j)+(npix-1)
		pptr%lines(6,i,j)=npix
	enddo
	return
    END SUBROUTINE WCAMERA_RING_BUILD
!******************************************************************************


END MODULE WCAMERA_SUBS

