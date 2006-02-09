!*******************************************************************
!  Version: V:1:3:8:3:5.3"
MODULE KASTRIGGER_SUBS
	IMPLICIT NONE
	real,external ::    gauss		!declare various functions.
	integer,external :: nfluct
	real,external :: pran


	private

	public :: whippleini
	public :: WHIPPLE_CAM_INI
	public :: WHIPPLE_CAM_PROC
	public :: WHIPPLE_IMAGE_PROC
	public :: WHIPPLE_PROC

CONTAINS

    SUBROUTINE GENERATE490(wptr,pptr)
!***************************************************************************
!	Generate 490 camera
!***************************************************************************

!	Modified:

!	19/11/99 GHS V:1:1:5:1:2.5
!		Change to the use of DN_MIN_LOOSE and DN_MIN_TIGHT in
!		WHIPPLE_TILT. DN_MIN_LOOSE is a full 1.0 deg bigger to
!		allow for large scatters and aberations.
	use structures
	use wcamera_def
	use kastrigger_def
	use record_defs
	use wcamera_subs
	IMPLICIT NONE

	type(camera) ::       wptr
	type(camera_ptrs) ::  pptr

	call wcamera_gen490(wptr,pptr,mhead)
	if(dn_min_tight>wptr%wcamera_min_dn)then
		   dn_min_tight=wptr%wcamera_min_dn    
						!Get biggest acceptable angle.
	endif
	dn_min_loose=cos(acos(dn_min_tight)+pi/180.0) !Loose is 1 deg out
						      !further

	call Wcamera_noise_thresh_gen(wptr,pptr,mhead,mhead%options)
     write(6,1499)pptr%pmt_spacing(1)*2,pptr%pmt_radius(1)*2,pptr%efficiency(1)
1499	format(		&
     & '                                      Pixel spacing =',f7.3,' deg.',/,&
     & '                                PMT active diameter =',f7.3,' deg.',/,&
     & '            Overall pmt/lightcone/mirror efficiency =',f7.3)

					!allocate camera arrays and other stuff
     	call whipple_cam_ini(wptr,pptr)
	return
    END SUBROUTINE GENERATE490
!***************************************************************************


    SUBROUTINE GENERATE499(wptr,pptr)
!***************************************************************************
!	Generate 499 camera
!***************************************************************************

!	Modified:

!	19/11/99 GHS V:1:1:5:1:2.5
!		Change to the use of DN_MIN_LOOSE and DN_MIN_TIGHT in
!		WHIPPLE_TILT. DN_MIN_LOOSE is a full 1.o deg bigger to
!		allow forlarge scatters and aberations.

	use structures
	use wcamera_def
	use kastrigger_def
	use record_defs
	use wcamera_subs
	IMPLICIT NONE

	type(camera) ::       wptr
	type(camera_ptrs) ::  pptr

	call wcamera_gen499(wptr,pptr,mhead)
	if(dn_min_tight>wptr%wcamera_min_dn)then
		   dn_min_tight=wptr%wcamera_min_dn    
						!Get biggest acceptable angle.
	endif
	dn_min_loose=cos(acos(dn_min_tight)+pi/180.0) !Loose is 1 deg out
						      !further
	call Wcamera_noise_thresh_gen(wptr,pptr,mhead,mhead%options)
     write(6,1499)pptr%pmt_spacing(1)*2,pptr%pmt_radius(1)*2,pptr%efficiency(1)
1499	format(		&
     & '                                      Pixel spacing =',f7.3,' deg.',/,&
     & '                                PMT active diameter =',f7.3,' deg.',/,&
     & '            Overall pmt/lightcone/mirror efficiency =',f7.3)

					!allocate camera arrays and other stuff
     	call whipple_cam_ini(wptr,pptr)
	return
    END SUBROUTINE GENERATE499
!***************************************************************************

    SUBROUTINE GENERATE1024(wptr,pptr)
!***************************************************************************
!	Generate Square 32 x 32 pixel camera for Laura 
!	Wcamera_gen1024 Fills in arrays:
!	 psq1024%:camera_x,camera_y,pmt_spacing,pmt_radius,efficiency,wadjacent
!	Wcamera_noise_thresh_gen fills in arrays:
!	 psq1024%: active_pmts,gain,noise,sky_mean_disc,mean_time_gap,
!		   threshold,mean_noise
!***************************************************************************

!	written:
!	21/3/00 GHS V:1:1:6:1:2.6
!	Modified:

	use structures
	use wcamera_def
	use kastrigger_def
	use record_defs
	use wcamera_subs
	IMPLICIT NONE

	type(camera) ::       wptr
	type(camera_ptrs) ::  pptr

	call wcamera_gen1024(wptr,pptr,mhead)
	if(dn_min_tight>wptr%wcamera_min_dn)then
		   dn_min_tight=wptr%wcamera_min_dn    
						!Get biggest acceptable angle.
	endif
	dn_min_loose=cos(acos(dn_min_tight)+pi/180.0) !Loose is 1 deg out
						      !further

	call Wcamera_noise_thresh_gen(wptr,pptr,mhead,mhead%options)

     write(6,1499)pptr%pmt_spacing(1)*2,pptr%pmt_radius(1)*2,pptr%efficiency(1)
1499	format(		&
     & '                                      Pixel spacing =',f7.3,' deg.',/,&
     & '                                PMT active diameter =',f7.3,' deg.',/,&
     & '            Overall pmt/lightcone/mirror efficiency =',f7.3)

					!allocate camera arrays and other stuff
     	call whipple_cam_ini(wptr,pptr)
	return
    END SUBROUTINE GENERATE1024
!***************************************************************************



    SUBROUTINE WHIPPLEINI
!******************************************************************************
!	This routine_ initalizes whats needed for Whipple 10m HRC processing
!******************************************************************************

!	Modified:

!	24/5/94 GHS V:1:1:2:1:1.1
!		Add mhead%phi_steps to kasstructures.inc. phi_steps is base of
!		phi divisions for an itheta circle. total steps is 
!		(itheta-1)*mhead%phi_steps. Normally its been 6 but we want 
!		different value for w10m do to its hex structure immaging array.
!		Use mhead%phi_steps=5 for w10m.


!	25/6/96 GHS V:1:0:2:1:2.2
!		541 camera
!		Use pe_threshold from .par file as threshold in pe's for
!		inner pmt's. Use that and noise sigma for inner to get an
!		effective threhols 'multiplicity'. Use that multiplicity
!		and noise sigmas for middle and outer pmt's to get their
!		thresholds.

!	26/1/98 GHS V:1:1:5:1:1.1
!		Convert old KASTRIG f77 version to KASTRIGGER F90 version. 
!		Remove non-whipple stuff.
!		Add stuff for .1875 deg 331 pixel camera.
!		Leave in 37,109,151,271 stuff. Warning: not verified!		

!	24/3/98	GHS V:1:1:5:1:1.2
!		Add variable DN_MIN which has minimum dn used by all camers.
!		This is used as a cut in WHIPPLE_TILT. Use new parameters
!		HTHETA_MAX and HPHI_SIDE to initalize stuff.

!	13/5/98	GHS V:1:1:5:1:2.0
!		Set up for VERITAS.
!		1:Unlike the past we now
!		  do not find the relative values of pe directions and timings
!		  to the mount plane in KASLITE. Instead the PES file has the
!		  directions and timings for when the pe's hit the ground.
!	          It is in this program where we read in the mount direction
!		  (DATAIN) and preform the relative pe directions and adjusted
!		  timing calculations. Put dl,dm,dn into mhead in place of where
!		  it used to come in in KASLITE.
!		  VECINI is now replaced by W10m_MOUNT_VECINI(which uses
!		  VECTOR_REDIRECT) to give unit vectors in original frame of
!		  all required mount directions. It also determines unit vectors
!		  for x,y in the mirror planes of all directions.
!		  Do this to increase flexability in mount direction
!                 choice.
!		2:Move steeing of DISC_WIDTH and ADC_GATE to from WHIPPLEINI to
!		 DATAIN from .PAR file. Just cleaning up really.

!	16/11/99 GHS V:1:1:5:1:2.5
!		Add stuff for Whiplle small pixel camera:W490. This camera uses
!		379 small 1/2" pixels + 3 rings of 37 each 1" pixels. Define
!		camera in a new subroutines: WCAMERA_GEN490 and GENERATE490.
!		Call GENERATE490 from WHIPPLEINI.                     

!	19/11/99 GHS V:1:1:5:1:2.5
!		Change to the use of DN_MIN_LOOSE and DN_MIN_TIGHT in
!		WHIPPLE_TILT. DN_MIN_LOOSE is a full 1.0 deg bigger to
!		allow for large scatters and aberations.

!	21/3/00 GHS V:1:1:6:1:2.6
!		Add stuff for Laura's square pixel camera. This camera uses
!		a square array of pixels. 32 by 32=1024. Use the small 1/2"
!		pmts as used in the w490 camera.Define
!		camera in a new subroutines: WCAMERA_GEN1024 and GENERATE1024.
!		Call GENERATE1024 from WHIPPLEINI.                     

!      07/06/04 GHS V:1:3:8:3:4.4
!                Driffting Gammas: For the whipple strip we need to get GAMMA 
!                efficiencies over all the camera.  Do this by treating gammas
!                as hadrons (tilting the mount in various directions). We want
!                to step in theta only up to gtheta_max (7=2.1 degs nominally)
!                Set the step size to gtheta_step_size (nominally =.3 degrees)
!                We don't really care about phi steps so in intereset of speed
!                and file size use the smallest we can (2 I think)
!                This all happens when the input command line -d option is 
!                given as 'T'. This causes the logical GAMMA_DRIFT to be set to
!                .true. Set this all up in WHIPPLE_INI and STRUCTURES.F90
!      08/06/04 GHS V:1:3:8:3:4.4
!                Dynamically allocate dlm,dmm,dnm,xdlm,xdmm,xdnm,ydlm,ydmm,ydnm
!                in WHIPPLEINI
!      09/06/04 GHS V:1:3:8:3:4.4
!                Rewrite W10M_MOUNT_VEC_INI. Use only iphi=1 and iphi=itheta
!                for gamma_drift. Hadrons same as before. Make x -axis be along
!                -ra for boith gamma_drift and hadrons. Gamma_drift step_size
!                is .4 min along +/- ra (iphi=1,itheta=iphi). Hadrons now also
!                have x axis along ra but that really not important for 
!                hadrons. Added a number of routines to do all this. Most in
!                w10m_subs.f90 but some c++ ones in Veritas.cpp. They use the
!                sla libs.
!       20/09/04 GHS V:1:3:8:3:5.0
!               Use ithphi_max for number of directions(image index)
!               Initalized here!
!               Allocate stheta and sphi arrays here. Hold theta/phi for 
!               different ithphi directions.


	use whipple_telescope
	use wcamera_def
	use kastrigger_def
	use record_defs
	use wcamera_subs
	use structures
	use whipple_tilt_def
	use w10m_subs
        use kastrg_command_line
	IMPLICIT NONE

!	integer*4 :: itheta,iphi
	integer*4 :: i
	real :: thresh_mult,vlength
        integer :: iphi_max,error,error_sum=0
        real :: theta_max

!	Calculate focal plane conversion factors. (meters/deg.)
!	Note that focal plane is not linear . Its got that tan(theta) factor
!	which for small angles is close to theta. Error is small.<1%,
	if(index(mhead%options,'V499')/=0)then    !VERITAS  telescope
           focal_length=veritas_focal_length
           spotsize=veritas_spotsize
           spotsize_ew=veritas_spotsize_ew
           spotsize_ns=veritas_spotsize_ns
           facet_diam=veritas_facet_diam
           mirror_radius_squared=veritas_mirror_radius_squared
        else                                      !Whipple telescope
           focal_length=w10m_focal_length
           spotsize=w10m_spotsize
           spotsize_ew=w10m_spotsize_ew
           spotsize_ns=w10m_spotsize_ns
           facet_diam=w10m_facet_diam
           mirror_radius_squared=w10m_mirror_radius_squared
        endif
        
        meters_per_deg=1./atand(1./focal_length)
	jitter_width=(spotsize/2.)*3.141592654/180.
						!Jitter of normal is 1/2 focal
						!plane jitter.
						!Convert to radians also.
	jitter_width_ns=(spotsize_ns/2.)* 3.141592654/180.
	jitter_width_ew=(spotsize_ew/2.)* 3.141592654/180.


	write(6,1002)focal_length,meters_per_deg,spotsize_ew,&
             & spotsize_ns,2*sqrt(mirror_radius_squared)
1002	format(	  &
	 & '                             Telescope Focal Length = ',f10.4,/, &
	 & '        Focal plane conversion factor in meters/deg = ',f10.4,/, &
         & '  Telescope misalignment and imperfection angle(EW) = ',f10.4,/, &
         & '  Telescope misalignment and imperfection angle(NS) = ',f10.4,/, &
         & '                       Effective Mirror diameter(m) = ',f10.4)



!	Set up max solid angle steps.
	if(hadron)then
             !Has drifting gamma been requested?
           if(gamma_drift)then
!*****************************************************************************
!For the whipple strip we need to get GAMMA efficiencies all over the camera.
!Do this by treating gammas as hadrons (tilting the mount)
!WE want to step in theta only up to gtheta_max (2. degs nominally) and since
!Set the step size to gtheta_step_size (nominally =.4 min)
!We don't really care about phi steps use the smallest we can (2 I think)
!*****************************************************************************
              mhead%itheta_max=gtheta_max
              mhead%step_size=gtheta_step_size !Set itheta step size(radians) to .4 min.
              mhead%phi_steps=gphi_side  !2 is the minimum number I think
              write(6,1005)mhead%step_size,mhead%phi_steps,mhead%itheta_max
1005          format(		&
         & '               Whipple 10m Gamma-ray: multi directions',/,&
	 & '           Whipple 10m:MHEAD%STEP_SIZE for GAMMAS =',f10.6,'(radians)',/, &
	 & '                       MHEAD%PHI_STEPS for GAMMAS =',i10,/, &
	 & '                      MHEAD%ITHETA_MAX for GAMMAS =',i10)
           write(6,1001)mhead%itheta_max
1001	format( &
	 & '          Whipple 10m:MHEAD%ITHETA_MAX for GAMMAS =',i10)
           else
              !Not gamma drift. Treat as a real hadron.
              mhead%itheta_max=htheta_max!Bigger camera (541) needs more.
              mhead%step_size=htheta_step_size !Set itheta step size to 1 deg.
              mhead%phi_steps=hphi_side !Note not 6 here. want it different
					!from image plane structure to avoid
					!bias.
              theta_max=(mhead%itheta_max-1)*mhead%step_size+mhead%step_size/2

              write(6,1000)mhead%step_size,mhead%phi_steps,mhead%itheta_max, &
                   & theta_max

1000          format(		&
	 & '           Whipple 10m:MHEAD%STEP_SIZE for HADRONS =',f10.2,/, &
	 & '                       MHEAD%PHI_STEPS for HADRONS =',i10,/, &
         & '                      MHEAD%ITHETA_MAX for HADRONS =',i10,/, &
	 & '      Radius random direction area(deg)for HADRONS =',f10.2)
           endif
        
        else			!Gamma showers Single direction/
           mhead%itheta_max=1
           mhead%step_size=0
           mhead%phi_steps=0
           write(6,1001)mhead%itheta_max
	endif

!Allocate space for the various dlm,dmm,dnm arrays
        if(mhead%itheta_max.ne.1)then
           iphi_max=(mhead%itheta_max-1)*mhead%phi_steps
        else
           iphi_max=1
        endif

        write(6,1006)iphi_max
1006	format( &
	 & '                                          IPHI_MAX =',i10)

!20/09/04 GHS Init ithphi_max here:
	if(mhead%itheta_max==1)then
	   ithphi_max=1 !one direction only!
	else
	   ithphi_max= &
              &  mhead%phi_steps*((mhead%itheta_max-1)*(mhead%itheta_max)/2)+1
	endif

        write(6,1007)ithphi_max
1007       format(&
	 & '                 Number of directions(ithphi_max)  =',i10)


        error_sum=0
        allocate(dlm(ithphi_max),stat=error)
        error_sum=error_sum+error
        allocate(dmm(ithphi_max),stat=error)
        error_sum=error_sum+error
        allocate(dnm(ithphi_max),stat=error)
        error_sum=error_sum+error
        allocate(xdlm(ithphi_max),stat=error)
        error_sum=error_sum+error
        allocate(xdmm(ithphi_max),stat=error)
        error_sum=error_sum+error
        allocate(xdnm(ithphi_max),stat=error)
        error_sum=error_sum+error
        allocate(ydlm(ithphi_max),stat=error)
        error_sum=error_sum+error
        allocate(ydmm(ithphi_max),stat=error)
        error_sum=error_sum+error
        allocate(ydnm(ithphi_max),stat=error)
        error_sum=error_sum+error
!23/09/04 GHS Add stheta,sphi arrays.
        allocate(stheta(ithphi_max),stat=error)
        error_sum=error_sum+error
        allocate(sphi(ithphi_max),stat=error)
        error_sum=error_sum+error

        if(error_sum/= 0)then
           print*,'Program could not allocte space for dlm,dmm,dnm arrays'
           stop 'Failure to allocate space for dlm,dmm,dnm array'
        endif
!	Set up the mount direction vector arrays.
	call w10m_mount_vecini(mhead,dl,dm,dn,hadron)


	dn_min_tight=1.0		!For use in whipple_tilt.
                                        !Initalzed to 0 deg.Code finds loosest 
					!for multiple camera run.

!***************************************************************************
!	37 Pixel Camera Initialization
!***************************************************************************
	if(index(mhead%options,'W37')/=0)then
		call wcamera_gen(w37,p37,mhead)
		if(dn_min_tight>w37%wcamera_min_dn)then
		   dn_min_tight=w37%wcamera_min_dn    
						!Get biggest acceptable angle.
		endif
						 !Loose is 1 deg out further
		dn_min_loose=cos(acos(dn_min_tight)+pi/180.0)
						      
		call wcamera_noise_thresh_gen(w37,p37,mhead,mhead%options)
        print*, &
 & '                                     Pixel spacing =', &
 & p37%pmt_spacing(1)*2,' deg.'
					!allocate camera arrays and other stuff
		call whipple_cam_ini(w37,p37)
!***************************************************************************
!	109 Pixel Camera Initialization
!***************************************************************************
	elseif(index(mhead%options,'W109')/=0)then
		call wcamera_gen109(mhead)	!Loads up common block arrays.
		if(dn_min_tight>w109%wcamera_min_dn)then
		     dn_min_tight=w109%wcamera_min_dn	!Get biggest acceptable angle.
		endif
						 !Loose is 1 deg out further
		dn_min_loose=cos(acos(dn_min_tight)+pi/180.0)

		call wcamera_noise_thresh_gen(w109,p109,mhead,mhead%options)
        print*, &
 & '                                     Pixel spacing =', &
 & p109%pmt_spacing(1)*2,' deg.'
					!allocate camera arrays and other stuff
		call whipple_cam_ini(w109,p109)

!***************************************************************************
!	151 Pixel Camera Initialization
!***************************************************************************
	elseif(index(mhead%options,'W151')/=0)then
		call wcamera_gen151(mhead)
		if(dn_min_tight>w151%wcamera_min_dn)then
		    dn_min_tight=w151%wcamera_min_dn	!Get biggest acceptable angle.
		endif
						 !Loose is 1 deg out further
		dn_min_loose=cos(acos(dn_min_tight)+pi/180.0)

		call wcamera_noise_thresh_gen(w151,p151,mhead,mhead%options)
        print*, &
 & '                                     Pixel spacing =', &
 & p151%pmt_spacing(1)*2,' deg.'
					!allocate camera arrays and other stuff
		call whipple_cam_ini(w151,p151)

!***************************************************************************
!	271 Pixel Camera Initialization
!***************************************************************************
	elseif(index(mhead%options,'W271')/=0)then
		call wcamera_gen(w271,p271,mhead)
		if(dn_min_tight>w271%wcamera_min_dn)then
		   dn_min_tight=w271%wcamera_min_dn     !Get biggest acceptable angle.
		endif
						 !Loose is 1 deg out further
		dn_min_loose=cos(acos(dn_min_tight)+pi/180.0)

		call wcamera_noise_thresh_gen(w271,p271,mhead,mhead%options)
        print*, &
 & '                                     Pixel spacing =', &
 & p271%pmt_spacing(1)*2,' deg.'
					!allocate camera arrays and other stuff
		call whipple_cam_ini(w271,p271)

!***************************************************************************
!	331 Pixel Camera Initialization
!***************************************************************************
!**331_125**
	elseif(index(mhead%options,'W331_125')/=0)then
	   call wcamera_gen(w331_125,p331_125,mhead)
	   if(dn_min_tight>w331_125%wcamera_min_dn)then
	   	dn_min_tight=w331_125%wcamera_min_dn	!Get biggest acceptable angle.
	   endif
						 !Loose is 1 deg out further
		dn_min_loose=cos(acos(dn_min_tight)+pi/180.0)

	   call wcamera_noise_thresh_gen(w331_125,p331_125,mhead,mhead%options)
        print*, &
		 & '                                     Pixel spacing =', &
 		 & p331_125%pmt_spacing(1)*2,' deg.'
					!allocate camera arrays and other stuff
	   call whipple_cam_ini(w331_125,p331_125)


!**331_1875**
	elseif(index(mhead%options,'W331_1875')/=0)then
	   call wcamera_gen(w331_1875,p331_1875,mhead)
	   if(dn_min_tight>w331_1875%wcamera_min_dn)then
		dn_min_tight=w331_1875%wcamera_min_dn	!Get biggest acceptable angle.
	   endif
						 !Loose is 1 deg out further
		dn_min_loose=cos(acos(dn_min_tight)+pi/180.0)

         call wcamera_noise_thresh_gen(w331_1875,p331_1875,mhead,mhead%options)

        print*, &
		 & '                                     Pixel spacing =', &
		 & p331_1875%pmt_spacing(1)*2,' deg.'
					!allocate camera arrays and other stuff
	   call whipple_cam_ini(w331_1875,p331_1875)


!**331_25**
	elseif(index(mhead%options,'W331_25')/=0)then
		call wcamera_gen(w331_25,p331_25,mhead)
		if(dn_min_tight>w331_25%wcamera_min_dn)then
						!Get biggest acceptable angle.
			dn_min_tight=w331_25%wcamera_min_dn  
		endif
						 !Loose is 1 deg out further
		dn_min_loose=cos(acos(dn_min_tight)+pi/180.0)

             call wcamera_noise_thresh_gen(w331_25,p331_25,mhead,mhead%options)

        print*, &
		 & '                                     Pixel spacing =', &
		 & p331_25%pmt_spacing(1)*2,' deg.'
					!allocate camera arrays and other stuff
		call whipple_cam_ini(w331_25,p331_25)

!***************************************************************************
!	541 Pixel Camera Initialization
!***************************************************************************
	elseif(index(mhead%options,'W541').ne.0)then
	    print*,'    ****541 PMT Camera Initialzed'
 print*,  &
   & '                 Number of inner pixels in trigger =',w541%ntrigger_in
	
 print*,  &
   & '                  541 Trigger Multiplicity(middle) =', mhead%hmult
 print*,  &
   & '            541 Trigger Multiplicity(Middle+inner) =', mhead%hmult+1
	    call wcamera_gen541	!Loads up table lookup arrays.
	    if(dn_min_tight>w541%wcamera_min_dn)then
	    	dn_min_tight=w541%wcamera_min_dn     	!Get biggest acceptable angle.
	    endif
						 !Loose is 1 deg out further
		dn_min_loose=cos(acos(dn_min_tight)+pi/180.0)

        print*, &
 & '                               Inner Pixel spacing =', &
 & p541%pmt_spacing(1)*2,' deg.'
        print*, &
 & '                              Middle Pixel spacing =', &
 & p541%pmt_spacing(332)*2,' deg.'
        print*, &
 & '                               Outer Pixel spacing =', &
 & p541%pmt_spacing(512)*2,' deg.'

!Load up adc and disc noise:
	    if(mhead%noise)then
!	   	p541%wnoise=mhead%rate_noise*mhead%adc_gate* & !Vector arith.
!			  & (pi*p541%pmt_spacing**2)*hexagon_factor
 	Print*,' NOISE GEN FOR 541 DOESNT WORK YET!!!'
!							       !Vector arith.
!	   	p541%sky_mean_disc=p541%wnoise*mhead%disc_width/mhead%adc_gate
!		
! Set the disc threshold in terms of noise sigma for middle and outer rings.
! Use given value for innner and to find what 'multiplicity' of noise sigima
! for middle and outer. Use constant sigma for now.
	   	thresh_mult=Mhead%pesum/sqrt(p541%wnoise(1))
				!Given threshold for inner.
	   	p541%threshold(1:331)=mhead%pesum                !Vector arith.
	   	p541%threshold(332:511)=thresh_mult*sqrt(p541%wnoise(332))
	  	p541%threshold(512:541)=thresh_mult*sqrt(p541%wnoise(512))

	   	thresh_mult=thresh_mult/sqrt(mhead%disc_width/mhead%adc_gate)
	   	write(6,1003)p541%threshold(1),p541%threshold(332),thresh_mult
1003	format(     &
	& ' 541 PMT Camera:Discriminator Threshold:Innr:middle =',2f10.4,/, &
	& ' 541 PMT Camera:Effective Threshold Multiplicity    =',f10.4)
					  !Mean time gap between noise pulses
!Warning: 541 disabled here. Can be re enabled by uncommenting next line
! next line and removing the print and stop staements.
!	    p541%w10m_mean_time_gap=1./(p541%sky_mean_disc/mhead%disc_width)
                print*,' 541 diasbled.'
                stop '541 disabled'
	    else
		p541%threshold=mhead%pesum   		!Vector Arith.
		p541%wnoise=0
		p541%sky_mean_disc=0
	        write(6,1004)p541%threshold(1),p541%threshold(332)
1004	format(     &
	& ' 541 PMT Camera:Discriminator Threshold:Innr:middle =',2f10.4)
	    endif
					!allocate camera arrays and other stuff
	    call whipple_cam_ini(w541,p541)


!************  End of 541 Camera Initialization ****************************

!***************************************************************************
!	Veritas 499 Pixel Camera Initialization
!***************************************************************************
	elseif(index(mhead%options,'V499')/=0)then
		call generate499(v499,pv499)


!***************************************************************************

!***************************************************************************
!	Whipple 490 Small Pixel Camera Initialization
!***************************************************************************
	elseif(index(mhead%options,'W490')/=0)then
		call generate490(w490,p490)

!***************************************************************************
!	Laura's 1024 pixel square camera Initialization
!***************************************************************************
	elseif(index(mhead%options,'SQ1024')/=0)then
		call generate1024(sq1024,psq1024)
	endif

	return
    END SUBROUTINE WHIPPLEINI
!******************************************************************************

    SUBROUTINE WHIPPLE_AREA_INI(pptr)
!*******************************************************************************
!	Init the arays for the area. But only zero what needs zeroing (for
! 	speed reasons)
!*******************************************************************************

!	Modified:

!	12/6/98	GHS V:1:1:5:1:2.1
!		Add option 'TIM' to get timing of each triggered pixel(with
!		noise) and also overall event trigger time.Use the PEPULSE
!		building terchnique(.25 ns steps of pulses built up from
!		single pe pulse shape with varying pulse hieghts. Use CFD
!		timing determination.
!		Dynamically allocate accululation arrays for pe times and
!		pixels in WHIPPLE_AREA_INI.
!		Setup call to W10M_PEPULSE routines.
!		Write out compressed arrays in a slightly different format.
!	12/7/00 GHS V:1:1:6:1:2.7
!		Allocate pptr%wavelength and pptr%spec for muon uv rejection 
!		study.

!       16/04/01 GHS V:1:1:6:1:2.11
!               Reimpliment the quick and dirty way of finding disc triggers 
!               and adc values(summing pe's in each pixel). One would use this
!               instead of the pepulse method when speed is necessary.
!               1:WHIPPLE_AREA_INI: Since we will always save the pixel timing 
!               information no matter which trigger method we use always 
!               allocate the pprt:time,pptr%time_pixel arrays.

!       20/09/04 GHS V:1:3:8:3:5.0
!               Use ithphi_max for number of directions(image index)
	use wcamera_def			!has camera_ptrs
	use kastrigger_def		!has ipe
	use record_defs
        use structures
	IMPLICIT NONE

	type(camera) ::  wptr
	type(camera_ptrs) ::  pptr
	integer :: i,error
					!Speed thingy.
	do i=1,ithphi_max
		if(pptr%pes_in_image(i)/=0)then
			pptr%disc(:,i)=0  		     	!Vector Arith.
			pptr%pes_in_image(i)=0
			pptr%muon_ratio(i)=0
			pptr%emission_alt(i)=0
			pptr%em_alt_sig(i)=0
		endif
	enddo
!***************************************************************************
!              Always allocate the pes timing arrays
!***************************************************************************
        deallocate(pptr%time,stat=error)
        allocate(pptr%time(ipe,ithphi_max),stat=error)
        if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"time."
          stop 'Failure to allocate space for pptr%time'
        endif
        deallocate(pptr%time_pixel,stat=error)
        allocate(pptr%time_pixel(ipe,ithphi_max),stat=error)
        if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"time_pixel."
           stop 'Failure to allocate space for pptr%time_pixel'
	endif
!***************************************************************************

!***************************************************************************
!	For Muon rejection study by uv filter.
!***************************************************************************
        if (index(MHEAD%options,'UV').ne.0)then
		deallocate(pptr%wavelength,stat=error)
		allocate(pptr%wavelength(ipe,ithphi_max),stat=error)
		if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"wavelength."
		     stop 'Failure to allocate space for pptr%wavelength'
		endif
		deallocate(pptr%spec,stat=error)
		allocate(pptr%spec(ipe,ithphi_max),stat=error)
		if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"spec."
		     stop 'Failure to allocate space for pptr%spec'
		endif
	endif
!***************************************************************************

	return
    END SUBROUTINE WHIPPLE_AREA_INI
!******************************************************************************

    SUBROUTINE WHIPPLE_CAM_INI(wptr,pptr)
!*******************************************************************************
!	Allocate camera arrays as needed
!*******************************************************************************
!Modified:

!       16/04/01 GHS V:1:1:6:1:2.11
!               Reimpliment the quick and dirty way of finding disc triggers 
!               and adc values (by summing pe's in each pixel). One would use 
!               this instead of the pepulse method when speed is necessary.
!               4:WHIPPLE_CAM_INI:Since W10M_MULT_TIRG AND W10M_PST_TRIG both 
!               expect pptr%time_trig and pptr&trig_index always allocate them.
!       20/09/04 GHS V:1:3:8:3:5.0
!               Use ithphi_max for number of directions(image index)
!               Initalized back in WHIPPLEINI
        use structures
	use wcamera_def
	use kastrigger_def
	use record_defs
	IMPLICIT NONE

	integer :: error
	type(camera) ::       wptr
	type(camera_ptrs) ::  pptr

!First Allocate the discriminator array. 
	deallocate(pptr%disc,stat=error)
	if(error/= 0)then
 	     print*,"Program could not deallocte space for pptr%disc."
	endif
	allocate(pptr%disc(wptr%npmt,ithphi_max),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"disc."
	     stop 'Failure to allocate space for pptr%disc'
	endif

	deallocate(pptr%pes_in_image,stat=error)
	if(error/= 0)then
 	     print*,"Program could not deallocte space for pptr%pes_in_image."
	endif
	allocate(pptr%pes_in_image(ithphi_max),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"pes_in_image."
	     stop 'Failure to allocate space for pptr%pes_in_image'
	endif

!and the rest.
	deallocate(pptr%muon_ratio,stat=error)
	if(error/= 0)then
 	     print*,"Program could not deallocte space for pptr%muon_ratio."
	endif
	allocate(pptr%muon_ratio(ithphi_max),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"muon_ratio."
	     stop 'Failure to allocate space for pptr%muon_ratio'
	endif
	deallocate(pptr%emission_alt,stat=error)
	if(error/= 0)then
 	     print*,"Program could not deallocte space for pptr%emission_alt."
	endif
	allocate(pptr%emission_alt(ithphi_max),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"emission_alt."
	     stop 'Failure to allocate space for pptr%emission_alt'
	endif

!em_alt_sig
	deallocate(pptr%em_alt_sig,stat=error)
	if(error/= 0)then
 	     print*,"Program could not deallocte space for pptr%em_alt_sig."
	endif
	allocate(pptr%em_alt_sig(ithphi_max),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"em_alt_sig."
	     stop 'Failure to allocate space for pptr%em_alt_sig'
	endif

!Intialize them
	pptr%disc=0	     				!Vector Arith.
	pptr%pes_in_image=0                             !Vector Arith.
	pptr%muon_ratio=0				!Vector Arith.
	pptr%emission_alt=0				!Vector Arith.
	pptr%em_alt_sig=0				!Vector Arith.
	
!ldisc_pes
	deallocate(pptr%ldisc_pes,stat=error)
	if(error/= 0)then
 	     print*,"Program could not deallocte space for pptr%ldisc_pes."
	endif
	allocate(pptr%ldisc_pes(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"ldisc_pes."
	     stop 'Failure to allocate space forpptr%ldisc_pes'
	endif

!ldisc_pulse_height
	deallocate(pptr%ldisc_pulse_height,stat=error)
	if(error/= 0)then
     print*,"Program could not deallocte space for pptr%ldisc_pulse_height."
	endif
	allocate(pptr%ldisc_pulse_height(wptr%npmt),stat=error)
	if(error/= 0)then
	     print*,"Program could not allocte space for p",wptr%camera_id, &
		&  "ldisc_pulse_height."
	     stop 'Failure to allocate space for pptr%ldisc_pulse_height'
	endif

!ldisc_trigger
	deallocate(pptr%ldisc_trigger,stat=error)
	if(error/= 0)then
 	     print*,"Program could not deallocte space for pptr%ldisc_trigger."
	endif
	allocate(pptr%ldisc_trigger(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"ldisc_trigger."
	     stop 'Failure to allocate space for pptr%ldisc_trigger'
	endif
	
!****************************************************************************
!       Timing stuff
!****************************************************************************
        deallocate(pptr%time_trig,stat=error)
        if(error/= 0)then
           print*,"Program could not deallocte space for pptr%time_trig."
        endif
        allocate(pptr%time_trig(wptr%npmt),stat=error)
        if(error/= 0)then
           print*,"Program could not allocte space for p",wptr%camera_id, &
                &  "time_trig."
           stop 'Failure to allocate space for pptr%time_trig'
        endif

        deallocate(pptr%trig_index,stat=error)
        if(error/= 0)then
           print*,"Program could not deallocte space for pptr%trig_index."
        endif
        allocate(pptr%trig_index(wptr%npmt),stat=error)
        if(error/= 0)then
           print*,"Program could not allocte space for p",wptr%camera_id, &
                &  "trig_index."
           stop 'Failure to allocate space for pptr%trig_index'
        endif
!****************************************************************************

	return
    END SUBROUTINE WHIPPLE_CAM_INI
!*******************************************************************************

    SUBROUTINE WHIPPLE_CAM_PROC(pes,wx,wy,ithphi,wptr,pptr,pe_time)
!*******************************************************************************
!	Place this pe into it's pixel in the camera looking in direction
!	specified by index ithphi. If this pixel hits add in other info.
!*******************************************************************************

!Modifed:

!	12/6/98 GHS
!		Add pe_time to input parameters. for use with timing stuff.
!		Setup call to W10M_PEPULSE_ACCUMULATE.

!	16/11/99 GHS V:1:1:5:1:2.5
!		Add code to see for 490 camera is pe hits a pixel in one of 
!		its outer rings.

!	12/7/00 GHS V:1:1:6:1:2.7
!		Accumulate wavelengths and emitting specoies type for muon 
!		rejection study.

!       16/04/01 GHS V:1:1:6:1:2.11
!               Reimpliment the quick and dirty way of finding disc triggers 
!               and adc values by (summing pe's in each pixel). One would use 
!               this instead of the pepulse method when speed is necessary.
!               3:WHIPPLE_CAM_PROC:Always call W10M_PEPULSE_ACCULMULATE
!               to fill pptr:time,pptr%time_pixel.


	use wcamera_def
	use wcamera_subs
	use kastrigger_def
	use record_defs
	use W10M_PEPULSE_SUBS
	use whipple_telescope

	IMPLICIT NONE

	integer :: ipix,ithphi
	real :: wx,wy
	real :: pe_time

	type(pe_s),pointer :: pes	
	type(camera) ::       wptr
	type(camera_ptrs) ::  pptr
	real*8 :: em_alt,em_alt2sum

		!for w490
	real :: wr,ring_rad,radius2,wdist2
	integer :: i,j,k

!Pixels
					!Find which PMT this hits.
	call WCAMERA_PIX(wx,wy,pptr,wptr,ipix)
!debug
!	write(6,4300)wx,wy,ipix
!enddebug						!Make sure its a hit.
	if(ipix.eq.0)then
!	16/11/99 GHS V:1:1:5:1:2.5
		if(w490%on)then
		    wr=sqrt(wx**2+wy**2)
		    do i=1,3
		       ring_rad=((w490_outer_ring_dia(i)/2)/meters_per_deg)
		       if(wr>=ring_rad-pptr%pmt_radius(380).and. &
				 & wr<=ring_rad+pptr%pmt_radius(380))then
					!Dirty, not so quick.
			    k=379+(i-1)*37
			    radius2=pptr%pmt_radius(380)**2
			    do j=1,37
                                wdist2=((pptr%camera_x(k+j)-wx)**2+ &
					 &(pptr%camera_y(k+j)-wy)**2)
			    	if(wdist2<=radius2)then
				    	ipix=k+j	!Found a hit
				     	exit
				endif
			     enddo
			     if(ipix==0)then
!debug
!                               write(102,4300)wx,wy,ipix
!enddebug
				return                   !In a ring but No hit.
			     endif
		       endif	 	
		       if(ipix/=0)then
			   exit			!Leave when pixel found
		       endif
                    enddo
		    if(ipix==0)then
!debug
!                                write(102,4300)wx,wy,ipix
!enddebug
			    return                      !Missed a ring. No hit
		    endif
		else
			return			!pixel missed
		endif
	elseif(ipix.gt.wptr%npmt)then
	        print*,'IPIX out of range:',ipix
		return
	endif
					!OK.  This pe makes it. Record which 
					!tube it hits.
!debug
!	write(101,4300)wx,wy,ipix
!4300    format(' ',2(e14.7,','),i6,',')
!enddebug

        pptr%disc(ipix,ithphi)=pptr%disc(ipix,ithphi)+1

					!and keep a running sum of number in
					!image(also used as index for timing
					!accumulation arrays.)
	pptr%pes_in_image(ithphi)=pptr%pes_in_image(ithphi)+1
					!count muons.
        if(pes%pe%spec==4.or.pes%pe%spec==5)then
    		pptr%muon_ratio(ithphi)=pptr%muon_ratio(ithphi)+1
	endif
					!Mean altitude of emmision.
	em_alt=pes%pe%em_alt		!make it r*8
	pptr%emission_alt(ithphi)=pptr%emission_alt(ithphi)+em_alt

	em_alt=em_alt*em_alt
        pptr%em_alt_sig(ithphi)=pptr%em_alt_sig(ithphi)+em_alt
					!Add pe into the timing pulses
!Timing accumulation
        call w10m_pepulse_accumulate(ipix,pe_time,ithphi,pptr)


!**************************************************************************
!         Wavelength accumulation for muon UV filter rejection study.
!**************************************************************************
        if (index(MHEAD%options,'UV').ne.0)then
           pptr%wavelength(pptr%pes_in_image(ithphi),ithphi)=pes%pe%lambda
                                           !emitting specias type accumulation
                                           !for muon UV filter rejection study.
           pptr%spec(pptr%pes_in_image(ithphi),ithphi)=pes%pe%spec
        endif
!**************************************************************************

	return
    END SUBROUTINE WHIPPLE_CAM_PROC

    SUBROUTINE WHIPPLE_IMAGE_PROC(wptr,pptr)
!**************************************************************************
!	Process the image(s) for this camera. When triggers are found
!	write them out to m file.
!**************************************************************************
!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	03/4/98

!	This is adaption of 331 trigger code from old KASTRIG
!	Modified:

!	25/2/99 GHS	V:1:1:5:1:2.3
!		Add capability for modeling trigger using the full blown
!		timeing. Use pulse shapes and CFD times to get trigger time
!		Requires modifying W10_PEPULSE_ANALIZE and where its called
!		from. Use of 'TIM' option causes use of pepulse construction
!		for trigger.
!	05/3/99 GHS	V:1:1:5:1:2.3
!		Add to the pepulse stuff PST processing. .This needs the 'PST'
!		options. Add the packed (I*2)
!		timing of pes to output file

!	12/7/00 GHS V:1:1:6:1:2.7
!		For mUon rection study by uv filtering:
!		For trigger write out wavelength, spec and muon_ratio.

!       16/04/01 GHS V:1:1:6:1:2.11
!               Reimpliment the quick and dirty way of finding disc triggers 
!               and adc values (by summing pe's in each pixel). One would use 
!               this instead of the pepulse method when speed is necessary.
!               5:WHIPPLE_IMAGE_PROC: Use 'TIM' option to determine if we 
!               analize useing the pepulse method or the old sumed pes in 
!               pixel method. Note change of option to 'TIM'

!      22/04/03 GHS V:1:3:8:3:4.1"
!                490 Pixel camera has only 331 pixels in bot multiplicity and
!                PST trigger, not 379! Fix W10M_PROC
!                to use wptr%ntrigger_in. WCAMERA_DEF already has that.
!      29/04/03 GHS V:1:3:8:3:4.2
!                Add a call to new routine W10M_PEPULSE_INIT in W10M_IMAGE_PROC
!                to create the single pepulse array (pepulse) with appropriate
!                rise time(2 ns this program).
!                Only use for w10m cameras qhe TIM option requested 
!                (Veritas camers do this on their own). This replaces the fixed
!                rise time pulse used previously.
!     03/06/04 GHS V:1:3:8:3:4.4 
!                For drift scan we really only want positions along x axis. 
!                This happens for iphi=1 and if phi_steps is 2 for 
!                iphi=itheta. This second one gets you the negative positions 
!                along the x axis. (Think about it!)
!     17/06/04 GHS V:1:3:8:3:4.6
!                Add a -w Option (sets spec_weight_flag to true) which causes 
!                the ouput MHDF5 file events to be cut on the spectrum weights.
!       20/09/04 GHS V:1:3:8:3:5.0
!               Replace itheta and iphi loops with single ithphi loop
!               For pure hadrons save actual theta,phi (in radians*1000) in
!               mrec: itheta iphi(which is i*2)else save itheta and iphi cell
!               indicies
 

	use kastrigger_def
	use record_defs
	use wcamera_def
	use w10m_subs
	use W10M_PEPULSE_SUBS
	use kastrigger_sub2
        use kastrg_command_line
	IMPLICIT NONE

	type(camera) ::       wptr
	type(camera_ptrs) ::  pptr


	integer :: ithphi
	real,pointer,dimension(:) :: disc
	real,pointer,dimension(:) :: sptr

	integer :: itheta,iphi  !mount inclination directions for
	integer :: nsides       !simulating off axis hadron showers.

        integer :: i,error,j,k,l,ios
	logical :: mult_trigger		!Multiplicity trigger flag
	logical :: pst_trigger		!PST trigger flag

	real :: w_hits_in,w_hits_mid
	integer :: nwords,multiplicity_pixels
	real :: xdummy
	real :: adc_gate
	logical :: first_time=.true.

	real,pointer,dimension(:) :: packed

!	Light cone concentration and mirror reflectivity combined to give
!	efficiency in WCAMERA_GEN
!	**Optical aberrations are handled from within WHIPPLE_TILT

        if(wptr%camera_id=='490')then
           multiplicity_pixels=331
        elseif(wptr%camera_id=='499')then
           multiplicity_pixels=463
        else
           multiplicity_pixels=wptr%npmt
        endif

!Start of ithphi loop
        do ithphi=1,ithphi_max

!07/06/04 GHS:For drift scan we really only want positions along X axis. This 
!             happens for iphi=1 (+x) and iphi=itheta(-x) for ghi_side=2
           !Get itheta and iphi indicies that ithphi stands for.
           nsides=mhead%phi_steps
           call get_itheta_iphi(ithphi,nsides,itheta,iphi)
           if(gamma_drift.and.(iphi.ne.1.and.iphi.ne.itheta))then
              cycle
           endif

		!At least 2 real pes somewhere is a minimum requirement	
           disc =>pptr%disc(1:wptr%npmt,ithphi)

           if(sum(disc(1:multiplicity_pixels))<2)then   !(for speed purposes)
              cycle		!Drop this one and cycle to next IPHI
           endif

           pst_trigger=.false.
           if(index(mhead%options,'PST')==0)then
              pst_trigger=.true.		!Ignore PST if PST
           endif				!option not chosen.
           mult_trigger=.false.

!***************************************************************************
!     PEPULSE event trigger
!Note w10m_pepulse_analyze used for both W10m and VERITAS in this program!!!
!***************************************************************************
           if(index(mhead%options,'TIM')/=0)then
                              !Assume all pixels in trigger.
              adc_gate=0      !Not making ADC's in this program
              call w10m_pepulse_analyze(wptr,pptr,ithphi, &
                   & multiplicity_pixels,mult_trigger,pst_trigger,adc_gate)
!***************************************************************************

!***************************************************************************
!Old non-timing trigger model(Updated 01/05/01 GHS).
!***************************************************************************
           else				!look for a trigger 
              call w10m_trigger(wptr,pptr,ithphi, &
                   & multiplicity_pixels,mult_trigger,pst_trigger)
           endif
!***************************************************************************

!***************************************************************************
!See if we have a trigger
!***************************************************************************
           if(first_time)then
              first_time=.false.
              write(6,1000)' Event Trigger requires both &
                   & Multiplicity and PST trigger'
1000	format(' ',a)
              if(index(mhead%options,'TIM')/=0)then
                 write(6,1000)' Pulse building W10M_PEPULSE_ANALIZE & 
                      & used for trigger determination(slow).'
                 call w10m_pepulse_init
              else
                 write(6,1000)' Non-pulse building W10M_TRIGGER  & 
                      & used for trigger determination(fast).'
              endif
!***********************************************************************

!***************************************************************************
!   Special UV option
!***************************************************************************
              if (index(MHEAD%options,'UV').ne.0)then
                 write(6,1000)' MUON wavelength data being written &
                                & to muon_uv.dat!!!!'
                 OPEN(51,status='new',ACTION='WRITE',&
                      & file='muon_uv.dat',iostat=ios)
                 if(ios>0)then
                    print*,' File open error opening muon_uv.dat'
                    stop 'File open error'
                 endif
              endif
!***********************************************************************
           endif

           if(mult_trigger.and.pst_trigger)then
              !Transfer some data for output M file record.
              mrec%nx=first_pes_ptr%pe%nx
              mrec%ny=first_pes_ptr%pe%ny
              if(gamma_drift.or.(.not.hadron))then
                 mrec%itheta=itheta
                 mrec%iphi=iphi
                 mrec%ithphi=ithphi   !save direction index.Always 1 for gamma

              else   !Save actual theta phi (in radians*1000) for pure hadrons
                 mrec%itheta=stheta(ithphi)*1000  !Rounds down to integers
                 mrec%iphi=sphi(ithphi)*1000
                 mrec%ithphi=ithphi   !save direction index.
!debug
!                 write(6,4300)stheta(ithphi),sphi(ithphi),mrec%itheta,&
!                      & mrec%iphi
!4300       format(2(f10.4,1x),2(i10,1x))
!enddebug
           endif
              mrec%emission_alt=  &
                   &  pptr%emission_alt(ithphi)/pptr%pes_in_image(ithphi)
              mrec%em_alt_sig=    &
                   &  ((pptr%em_alt_sig(ithphi)/pptr%pes_in_image(ithphi))- &
                   &  mrec%emission_alt**2)
              if(mrec%em_alt_sig.gt.0)then
                 mrec%em_alt_sig=sqrt(mrec%em_alt_sig)
              else
                 mrec%em_alt_sig=0.
              endif

              mrec%muon_ratio=pptr%muon_ratio(ithphi)/  &
                   &  pptr%pes_in_image(ithphi)
!*****************************************************************************

!*****************************************************************************
!This is for a test of the muon rejection through uv filter.
!*****************************************************************************
              if (index(MHEAD%options,'UV').ne.0)then
                 do i=1,pptr%pes_in_image(ithphi)
                    write(51,4400)pptr%wavelength(i,ithphi)
4400	format(' ',e14.7,',')
                 enddo
              endif
!*****************************************************************************

           endif	!Trigger found endif
!******End Trigger test********************************************************

!*****************************************************************************
!		Out put record if we have a trigger
!		Need to call this every time for MPI version to be able send 
!		rejection to parent.
!*****************************************************************************
                    !see if were cutting on the energy spectrum weights
           if(spec_weight_flag)then
              if(pran(xdummy)<=event_weight)then
                 call WHIPPLE_OUTPUT_M(disc,mult_trigger, &
                      &pst_trigger,pptr,wptr,ithphi)
              endif
           else
              call WHIPPLE_OUTPUT_M(disc,mult_trigger,pst_trigger, &
                   & pptr,wptr,ithphi)
           endif
!*****************************************************************************

        enddo			!ithphi enddo
        return

    END SUBROUTINE WHIPPLE_IMAGE_PROC
!*****************************************************************************


    SUBROUTINE WHIPPLE_PROC
!*******************************************************************************
!	Determine images from all cameras all directions.
!*******************************************************************************
!	This is code extracted from old w10m_proc.
!	We use as our outside loop the loop over the pe's. We collect in
!	pptr%disc() all images for all cameras in all directions.
!	This is being done to speed things up a little.

!	MOdified:

!	13/5/98	GHS V:1:1:5:1:2.0
!		Set up for VERITAS.
!		1:Mount direction supplied here now. Unlike the past we now
!		  do not find the relative values of pe directions and timings
!		  to the mount plane in KASLITE. Instead the PES file has the
!		  directions and timings for when the pe's hit the ground.
!	          It is here where we used the mount direction(read form the
!		  .par file in DATAIN) and preform the relative pe directions 
!		  and adjusted timing calculations
!		  We do this to increase flexability in mount direction choice.

!	17/11/99 GHS V:1:1:5:1:2.5
!		Add stuff for 490 camera:call to whipple_area_ini;call to
!		whipple_cam_proc. Call to whipple_image_proc
!	21/3/00 GHS V:1:1:6:1:2.6
!		Add stuff for sqaure 1024 camera:call to whipple_area_ini;
!		call to	whipple_cam_proc. Call to whipple_image_proc
!     03/06/04 GHS V:1:3:8:3:4.4 
!                For a gamma drift scan we really only want positions along x 
!                axis. This happens for iphi=1 and if phi_steps is 2 for 
!                iphi=itheta. This second one gets you the negative positions 
!                along the x axis. (Think about it!)
!     15/06/04 GHS V:1:3:8:3:4.5 
!                In W10M_PROC: After call to WHIPPLE_TILT  but before call to 
!                WHIPPLE_CAM_PROC, rotate where pe is in image plane (WX,WY)
!                to reflect the fact that the WHIPPLE 490 pixel camera is 
!                rotated by 180 + 7.306 degrees from having pixel 2 on the + x
!                axis.      
!       20/09/04 GHS V:1:3:8:3:5.0
!               Replace itheta and iphi loops with single ithphi loop


	use structures
	use wcamera_def
	use whipple_tilt_def
	use kastrigger_def
	use record_defs
	use whipple_telescope
        use kastrg_command_line
	use w10m_subs

	IMPLICIT NONE

	integer :: ithphi,nsides,itheta,iphi
	type(pe_s),pointer :: pes	

	integer :: i,iphi_max
        real :: wx,wy,pe_time,pe_time_tilt
        real :: wxtemp,wytemp
        real,parameter :: w10m490rot= -7.306  !490 npixel camera is rotated by
                                            !this much (degrees)
	logical :: dump
        integer :: tcount

!	Process these pe's. For Hadronic showers Whipple requires that all pes
!	be tested for all directions. That is, pes are used multiply. We think
!	this is ok!.	Iterate over itheta and iphi
!	Note:Even for itheta=1 we have to call WHIPPLE_TILT since it determines
!	if a pe hits the mirror(which may not be pointed at Zenith)
!	and it does the optical aberations of the mirror.


!	Initalize things for area
	if(w37%on)then
		call whipple_area_ini(p37)
	
	elseif(w109%on)then
		call whipple_area_ini(p109)
	
	elseif(w151%on)then
		call whipple_area_ini(p151)
	
	elseif(w271%on)then
		call whipple_area_ini(p271)
	
	elseif(w331_125%on)then
		call whipple_area_ini(p331_125)
	
	elseif(w331_1875%on)then
		call whipple_area_ini(p331_1875)
	
	elseif(w331_25%on)then
		call whipple_area_ini(p331_25)
!	17/11/99 GHS V:1:1:5:1:2.5
	elseif(w490%on)then
		call whipple_area_ini(p490)

!	21/3/00 GHS V:1:1:6:1:2.6
	elseif(sq1024%on)then
		call whipple_area_ini(psq1024)
	elseif(w541%on)then
		call whipple_area_ini(p541)
	elseif(v499%on)then
		call whipple_area_ini(pv499)
	endif
 
!**************************************************************************
! Iterate over all pes in this area
!**************************************************************************
        do i=1,ipe
	   if(i==1)then
              pes=>first_pes_ptr	!Start of area
	   else
              pes=>pes%next_ptr	!Go down links.
	   endif
		         !Recreate pe's dn.Going down so should be positive
	   pes%dnpe=sqrt(1.- (pes%pe%dlr**2) - (pes%pe%dmr**2))

!**************************************************************************
!Start ithphi loop
!**************************************************************************
           do ithphi=1, ithphi_max

!07/06/04 GHS:For drift scan we really only want positions along X axis. This 
!             happens for iphi=1 (+x) and iphi=itheta(-x) for ghi_side=2
              nsides=mhead%phi_steps
              call get_itheta_iphi(ithphi,nsides,itheta,iphi)

              if(gamma_drift.and.(iphi.ne.1.and.iphi.ne.itheta))then
                 cycle
              endif
			!Preset for efficiency:Used in WHIPPLE_TILT
              pes%dndnm=pes%dnpe*dnm(ithphi)
              tdlm=dlm(ithphi)
              tdmm=dmm(ithphi)
              tdnm=dnm(ithphi)
              xdl=xdlm(ithphi)
              xdm=xdmm(ithphi)
              xdn=xdnm(ithphi)
              ydl=ydlm(ithphi)
              ydm=ydmm(ithphi)
              ydn=ydnm(ithphi)
!***************************************************************************
!	Collect this  Pe into images
!***************************************************************************
!	WHIPPLE_TILT dtermines: 1:Does pe hit focal plane. 2:Does pe hit mirror
!	at its present inclination.3:Angle of pe to mirror. 
!	4:Position in focal plane of pe after including aberrations.
!	WHIPPLE_TILT also corrects pe_into tilted mirror plane.
              call whipple_tilt(pes,wx,wy,mirror_radius_squared,&
                   dump,pe_time, pe_time_tilt)
              if(dump)then
                 cycle	!drop pe go to next direction.
              endif
 
!*****************************************************************************
! Rotate camera
!*****************************************************************************
!The whipple 490 pixel camera is rotated by 7.306 deg. Actually, cparam has
!pixel #2 at x=-.117 and y=0.15. That is it is actually rotated by 180 +7.306
!deg.
              if(w490%on)then
                 wx=-wx         !rotate by 180 deg
                 wy=-wy
                                      !Now rotate by another 7.306 deg
                 wxtemp=wx*cosd(w10m490rot)+wy*sind(w10m490rot)
                 wytemp=wy*cosd(w10m490rot)-wx*sind(w10m490rot)
                 wx=wxtemp
                 wy=wytemp
              endif
!*****************************************************************************

!*****************************************************************************
!Process this pe. Test for the various cameras
!*****************************************************************************
              if(w37%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,w37,p37,pe_time)

              elseif(w109%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,w109,p109, &
                      & pe_time)

              elseif(w151%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,w151,p151,  &
                      & pe_time)
                 
              elseif(w271%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,w271,p271,  &
                      & pe_time)
		    
              elseif(w331_125%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,w331_125,&
                      & p331_125,pe_time)
		    
              elseif(w331_1875%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,w331_1875,&
                      & p331_1875,pe_time)
		    
              elseif(w331_25%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,w331_25,&
                      & p331_25,pe_time)
		    
!	17/11/99 GHS V:1:1:5:1:2.5
              elseif(w490%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,w490,&
                      & p490,pe_time)
		    
!	21/3/00 GHS V:1:1:6:1:2.6
              elseif(sq1024%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,sq1024,&
                      & psq1024,pe_time)
		    

              elseif(w541%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,w541,p541,  &
                      &  pe_time)

              elseif(v499%on)then
                 call whipple_cam_proc(pes,wx,wy,ithphi,v499,  &
                      &  pv499,pe_time)
              endif
!*****************************************************************************

           enddo                        !End ithphi loop
	enddo				!end pe iteration
!*****************************************************************************

!	We now have all the pe's collected into their appropriate disc arrays
!	for each camera and for each ithphi.

!*****************************************************************************
!Process the images in this direction. Test for the various cameras
!*****************************************************************************
	if(w37%on)then
		call whipple_image_proc(w37,p37)
	
 	elseif(w109%on)then
		call whipple_image_proc(w109,p109)
	
	elseif(w151%on)then
		call whipple_image_proc(w151,p151)
	
	elseif(w271%on)then
		call whipple_image_proc(w271,p271)
	
	elseif(w331_125%on)then
		call whipple_image_proc(w331_125,p331_125)
	
	elseif(w331_1875%on)then
		call whipple_image_proc(w331_1875,p331_1875)
	
	elseif(w331_25%on)then
		call whipple_image_proc(w331_25,p331_25)
	

!  17/11/99 GHS V:1:1:5:1:2.5
	elseif(w490%on)then
		call whipple_image_proc(w490,p490)
	
!  21/3/00 GHS V:1:1:6:1:2.6
	elseif(sq1024%on)then
		call whipple_image_proc(sq1024,psq1024)

	elseif(w541%on)then
		call whipple_image_proc(w541,p541)
	
	elseif(v499%on)then
		call whipple_image_proc(v499,pv499)
	
	endif
!*****************************************************************************

	return
    END SUBROUTINE WHIPPLE_PROC
!*******************************************************************************

    SUBROUTINE WHIPPLE_TILT(pes,wx,wy,wrad2,dump,pe_time,pe_time_tilt)
!    SUBROUTINE WHIPPLE_TILT(pes,wx,wy,wrad2,dump,pe_time,pe_time_tilt, &
!                            & debug_var)
!******************************************************************************
!	This is a modified TILT subroutine_ designed to be optimized
!	for Whipple 10m.
!******************************************************************************
!	Find parameters of this pe relative to mount itheta,iphi vector.

!	Written: 1/16/91 G.H.S. V:1:0:1:0:0.0

!	Modified:

!	01/12/97 GHS  V:1:1:4:1:1.2
!		Include time adjustment calculations for both tilted mirrors
!		and within the mirror aberations. Requires modifying
!		WHIPPLE_TILT to have an aditional PE_TIME argument. Upon return 
!		from the call to WHIPPLE_TILT, PE_TIME will have the pe arrival 
!		time at the focal plane adjusted for mirror tilt and
!		differences in path lengths across the whipple 10m cotton_davis
!		mirror.
!		In WHIPPLE_TILT use the DIST value to accjust for the tilt of
!		the mirror.

!	12/12/97 GHS  V:1:1:4:1:1.3
!		Include in the WHIPPLE_TILT call the argument PE_TIME_TILT.
!		It's the tilt corected time before aberations are applied.
!		used for modeling isynchronus mirrors.

!	03/2/98 GHS	Convert to F90.

!	28/3/98	GHS V:1:1:5:1:1.2
!		Make changes to fit new way of processing pes

!	13/5/98	GHS V:1:1:5:1:2.0
!		Set up for VERITAS.
!		1:Mount direction supplied here now. Unlike the past we now
!		  do not find the relative values of pe directions and timings
!		  to the mount plane in KASLITE. Instead the PES file has the
!		  directions and timings for when the pe's hit the ground.
!	          It is in this program where we read in the mount direction
!		  (DATAIN) and preform the relative pe directions and adjusted
!		  timing calculations. Put dl,dm,dn into mhead in place of where
!		  it used to come in in KASLITE.
!		  VECINI is now replaced by MOUNT_VECINI(which usees
!		  VECTOR_REDIRECT) to give unit vectors in original frame of
!		  all required mount directions. It also determines unitvectors
!		  for x,y in the mirror planes of all directions.
!		  Changes here in mirror plan coord and pe relative direction
!		  calculation
!		  Do this to increase flexability in mount direction
!                 choice.

!	19/11/99 GHS V:1:1:5:1:2.5
!		Change to the use of DN_MIN_LOOSE and DN_MIN_TIGHT in
!		WHIPPLE_TILT. DN_MIN_LOOSE is a full 1.0 deg bigger to
!		allow for large scatters and aberations.
!		Do the tights cut at the very end of whipple_tilt(after scatters
!		and aberations).


 	use structures
	use kastrigger_def
	use record_defs
	use wcamera_def
	use whipple_tilt_def
	IMPLICIT NONE

	real :: wx,wy
	logical,intent(out) :: dump
	type(pe_s),pointer :: pes	

	type(pe_spec) :: peptr
	real :: vn,xg,yg,xn,dist,xmount,ymount,zmount,wdldm2,wmag,wrad2
	real :: pe_time,pe_time_tilt
	real :: vn_actual,vn_diff,vn_now
!        real,dimension(:) :: debug_var
!        The length of the track from hobs to the tilted mount mirror plane is:
!        dist=-((DLm,DMm,DNm) . (xg,yg,zg))/((DLm,DMm,DNm) . (DL1,DM1,DN1))
!        (DLm,DMm,DNm) is normal to mount mirror plane pointed at sky.
!        The term in the denominater is the dot product of the normal to the
!        mirror plane and the direction of the photon. Calculate it first. Its
!	 the relative dn of the photon.
			!Pes%dndnm pre-loaded with dnpe*dnm
	vn=(tdlm*pes%pe%dlr+tdmm*pes%pe%dmr+pes%dndnm)	
							!dnpe always positive
							!Tdnm always negative
!	plus 1.0 deg(for scatters and aberations.)
	dump=.false.
	if(abs(vn).lt.dn_min_loose)then
		dump=.true.
		return
	endif

!        We want to preserve the angle to N(that is dnr=-vn)  and we also
!        want dlr,dmr,-vn to be a unit vector.
	if(abs(vn).ge.1.0)then
		vn=-1.0                 !Always assume negative mount.
	endif

!	9/12/93 GHS
!	Ok. See if this photon hits the telescope mirror.
	xg=pes%pe%xm
	yg=pes%pe%ym	!local ground coords of Photon relative
				!to center of area.

!       Now the numerater. This is perendicular distance from xg,yg,zg=0
!	to mirror plane of mount.
        xn=-(tdlm*xg+tdmm*yg)        !zg*dn=0.

!                Length of vector from hobs to intersection of mount plane.
	dist=xn/vn

!	Now correct timing for tilted mirror plane. Pe must go DIST distance
!	further.
	pe_time=pes%pe%time+(dist/c_light)*1.e9
							!Convert c_light to
							!m/nsec

!	Now the x,y intercepts of the track in the mount plane.
!	Use the x,y unit vectors as defined in MOUNT_VECINI
!	First the vector of this intercept point.X'=X+T(rack)*dist.
	xmount=xg+pes%pe%dlr*dist
	ymount=yg+pes%pe%dmr*dist
	zmount=pes%dnpe*dist	
			!Z direction increases down from the ground plane
			!Define hobs as Z=0
	!Now dot this with the new x' and y' unit vectors to get x,y in
	!mirror plane
	peptr%vpe(1)=xmount*xdl+ymount*xdm+zmount*xdn
	peptr%vpe(2)=xmount*ydl+ymount*ydm+zmount*ydn
	peptr%vpe(3)=0		!init it to 0 for w10m_full_aberration

	!See if this photon is within pmt mirror radius.
!Another note. We could put code here to find which FACIT we hit. This might
!		improve the Aberration calculation slightly also.
!	Do it the simple way:
	if((peptr%vpe(1)**2+peptr%vpe(2)**2).gt.wrad2)then
		dump=.true.		! Gets here if mirror is missed.
		return			!Drop this pe.
	endif

!	Now get relative direction of photon to mirror plane.
	if(vn.eq.-1.0)then	!verticle photon?
		peptr%vdir(1)=0.  !DL
		peptr%vdir(2)=0.  !DM
		peptr%vdir(3)=1.0  !DN	(positive)
	else
!        Form the relative vector components of photon to mount.
!	Agian dot products with x',y' unit vectors.
		peptr%vdir(1)=pes%pe%dlr*xdl+pes%pe%dmr*xdm+pes%dnpe*xdn
		peptr%vdir(2)=pes%pe%dlr*ydl+pes%pe%dmr*ydm+pes%dnpe*ydn
		wdldm2=peptr%vdir(1)**2+peptr%vdir(2)**2	!Normalize it.
		wmag=sqrt(wdldm2/(1.-vn**2))
		peptr%vdir(1)=peptr%vdir(1)/wmag
		peptr%vdir(2)=peptr%vdir(2)/wmag
      					!Positive sign means down going.
		peptr%vdir(3)=sqrt(1-peptr%vdir(1)**2-peptr%vdir(2)**2)
	endif                                                  

!	THIS IS WHERE WE DO OPTICAL ABERRATIONS.
!	W10m_full_aberration finds where in the focal plane this photon lands
!	after applying  both  global and facit aberations for the whipple
!	10m mirror. We will have to do things a little different when we
!	go to the 11m mirror.

!	12/12/97 GHS Save mirror plane time.
	pe_time_tilt=pe_time
	peptr%time=pe_time

! 	call w10m_full_aberration(peptr,debug_var)
 	call w10m_full_aberration(peptr)
	pe_time=peptr%time
	wx=peptr%vw(1)
	wy=peptr%vw(2)
	vn_actual=sqrt(wx**2+wy**2)

	vn_now=cosd(vn_actual)
	if(vn_now<dn_min_tight)then
		dump=.true.
	endif
	return
    END SUBROUTINE WHIPPLE_TILT
!******************************************************************************(

   SUBROUTINE W10M_FULL_ABERRATION(peptr)
!   SUBROUTINE W10M_FULL_ABERRATION(peptr,debug_var)
!******************************************************************************(
!	Aberration of the Whipple 10m aperature Cotten-davis mirror 
!	determined by  exact ray tracing plus a gaussian jitter added for
!	pointing/spotsize errors.
!*****************************************************************************

!	The Whipple 10m aperature mirror is a spherical surface of radius=7.3m.
!	On this surface	are attached small hexagon shaped mirror facets .305 m
!	in diam. These mirror facets are spherical with focal length equal to 
!	the radius of curvature) of 7.3 m (their radii of curvature is thus 
!	14.6 m).
!	However!!! the Cotten-Davis trick is to not have the mirrors oriented
!	tangent to the global surface(ie have them pointing back to the 7.3m
!	origin of the radius of curvature of the global mirror which would give
!	really bad sperical aberrations!) but rather the indidual facets are 
!	pointed back along the optical axis to a point 2 x the global surface 
!	radius, ie. 14.6 m. This reduces the sperical abertions by a whole 
!	bunch.  There are, however, some aberrations left. This is especially
!	true for off axis rays. THis code attemps to calculate the actual 
!	direction of the reflected ray. This is tough since we don't want to
!	find where on a particular facet a photon lands(well maybe later using
!	algorithums we set up for finding pixels). So when we have a photon we
!	generate a facet under it positioned randomly.
!	We then do an exact ray trace to the focal plane. 
!	We jitter the normal of the facet with GEOM.

						 !Z is negative since vector
						 !goes from the forcal plane
						 !to the mirror.
!	This code is an adoption of that orignally written in C by Dave Lewis
!	in 89 Sept.
!	Plus a facet modeling derived from a pascal program from M. Hillas I
!	think.

!	Note that everything is in metric units.

!From peptr:
!	vpe:	Position in meters where photon hits the mirror in the
!		mirror	plane.(optical axis goes through origin).

!	vdir:	 Direction cosign vector of incident ray. In plane of 
!		 mirror. Optical	axis is origin.

!	vw:	Vector in the focal plane where this photon hits(in deg).

!	time:	On input TIME is time pe hits mirror plane if no mirror was
!		there. On output its the time it hits the focal plane after
!		reflection.

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	09/2/95
	
!	Modified:

!	10/2/95 GHS
!		Optimize for speed.

!	01/12/97 GHS  V:1:1:4:1:1.2
!		Include time adjustment calculations  for the mirror aberations
!		Upon return from the call to WHIPPLE_full_aberation, PE_TIME 
!		will have the pe arrival time at the focal plane adjusted for 
!		differences in path lengths across the whipple 10m cotton_davis
!		mirror.
!		1:First, to improve substatialy the accuracy of this
!		calculation we improve on our approximation that the x,y on the 
!		facet is the same as the xy in the mirror plane. Obviously for
!		tilted tracks this is not true but its not too bad an
!		approximation(dn<3 deg). To improve on this(a lot!) back track 
!		the track of the photon to the altitude of the facet origen and
!		use the	xy there as the basis for the xy of the reflection 
!		point. This is easy(except for chosing the correct sign of 
!		things)	and quick to calculate and will remove 80% of our 
!		timinig and position error.
!		Add code to make sure all facets fit within radius of mirror.

!	04/12/97 GHS
!	Convert to F90 for my own amusment.

!NOTE: WE DO LOTS OF VECTOR MATH HERE. ANY VARIABLE THAT STARTS WITH A 'V' IS A
!	VECTOR

!       28/8/00 GHS V:1:1:6:1:2.8
!               Convert to r*8 internally to avoid some small angle roundoff
!               problems with geom. Use geom8  Use cartisian (x gaussian and y
!               gaussian independently) for jitter of mirror normal.

!       08/05/01 GHS V:1:1:6:1:2.12 
!               Use seperate jitter widths for e-w and 
!               ns(top-down) for oval spotsizes.

	use kastrigger_def
	use record_defs
      	use whipple_telescope
!	use kas_ranlux
	IMPLICIT NONE

	type(pe_spec) :: peptr
!        real,dimension(:) :: debug_var

	real*8,dimension(3) :: vfacet,vdelta,vrc,vph,vph1,vn,vperp,vpara, &
             & vdr,vpef
	real*8,dimension(3) :: vdcsn
	real*8,dimension(3) :: v2fl
	real*8 :: mag_vrc,mag_vph,mag_vn,rf,xyf_mag_sqr,path,z_dist


	real*8 :: tix,tiy
	real*8 :: w10m_ct
	real :: xdummy

!	The shape for a SPHERE is rm**2 = (xm**2 + ym**2 + zm**2) where we set
!	rm = FOCAL_LENGTHFL = 7.3 m.(or 12m for Veritas)
!	The center of the sphere as the origin and the z axis chosen to be
!	vertical.
!	NOTE: the origin is at the center of the focal plane.

!	Note on origin: The focal plane is the origin for all these vectors.
!	Positive is along the axis away from the mirror.
!	This is a change from the external coord system which has z positive
!	going the opposite direction and has the origen in the mirror plane.

!	Generate the x,y of a typical facet mirror that this photon would
!	hit. This is an approximation. In the future I will include code to
!	actually figure which facet is hit and where it is. Fake it for now.

!	Convert direction vector to interal geometry.(change sign of Z)!
!	I may have a left hand coord system here but it doesn't matter(I
!	think!)
	vdcsn=peptr%vdir
	vdcsn(3)=-vdcsn(3)

!	First: Position on facet where this photon hits.
	vfacet(3)=0	!Init to 0 we really only need x,y for now.
	do
				!.301 is radius of facet in m. from Hillas.
		vfacet(1)=(pran(xdummy)-.5)*facet_diam
		vfacet(2)=(pran(xdummy)-.5)*facet_diam
		rf=(vfacet(1)**2+vfacet(2)**2)
		if(rf<=(facet_diam/2)**2)then
			exit
		endif
				!repeat till we are on the mirror.
	enddo

!	now use this position to position this fake facet.
!	Make facet vector Vfacet(from origen to center of facet).

	vfacet=vfacet+peptr%vpe		!vector add


!	Using the equation for a sphere determine the z component of Vfacet
!	on the spherical surface of the mirror. It is assumed
!	that the photon is within the mirror aperature.
!	(but the facet may not be and thats an error for the aberration of the
!	edge of the telecope).

	xyf_mag_sqr=vfacet(1)**2+vfacet(2)**2	!This is for speed otimization
    	vfacet(3)=-sqrt(focal_length**2 - xyf_mag_sqr)
			!Z is negative since vector goes from the focal plane
			!to the mirror.

!	01/12/97 GHS Refine our xy approximation:
!	Backtrack the pe track to this zf altitude. This will gives us the
!	x,y,z point where the photon crossed the plane parrallel to the
!	mirror plane but at the mirror facet positon. Use this new x,y in our
!	approximation as to where the photon refelcts from the facet.
	z_dist=focal_length+vfacet(3) !height of facet above mirror plane.
	path=z_dist/vdcsn(3)   !Path length between mirror plane
				    !and facet plane. Should be negative.
	vdelta=path*vdcsn       !X,Y Corrections. Ignore vdelta(3)
	vpef=peptr%vpe+vdelta 	    !X,Y Positon on facet, Ignore vpef(3)
					
!	Now we do the tricky thing. We want here the normal vector from the
!	fake  facet mirror that is situated and centered at Vfacet but is 
!	pointed  a zf of 2 x the focal length from the mirror along the optical
!	axis(ie.at v2fl=0,0,+focal_length).
!	This vector is Vrc=V2fl-Vf
!	First vector from center of facet to (0,0,focal_length.)
        v2fl(1)=0.
        v2fl(2)=0.
        v2fl(3)=+focal_length
	vrc=v2fl-vfacet

!	Now make a unit vector (called Vph1)out of it.
!       dot_product is intrinsic function
	mag_vrc=sqrt(dot_product(vrc,vrc))
	vph1=vrc/mag_vrc

!	We know the center of curvature of the facet mirror will be beyond
!	the 2fl point along the Vph1 vector a distance of 2*focal_length-
!	mag_vrc. So make that little vector and call it Vph

	mag_vph=2*focal_length-mag_vrc
	vph=vph1*mag_vph

!	Find z value of where photon hits the facet mirror. This must
!	satisy the sphere equation from the PH point with radius of curvature
!	of 2*focal_length and use the facet altitude plane xpf,ypf values
!	(which we know shold be adjusted some more, but screw it!)
!	remember sign convention.
	vpef(3)=-sqrt((2*focal_length)**2-(vpef(1)-vph(1))**2- &
		& (vpef(2)-vph(2))**2)+  focal_length+vph(3)

!	01/12/97 GHS
!		Adjust the petime for the fact that it hits at ZPF not at
!		the mirror plane. Note this is always a reduction.
	z_dist=focal_length+vpef(3) !height of reflection point above 
					!mirror plane.
	path=z_dist/vdcsn(3)		!Path length between mirror plane
					!and facet plane. Should be negative.
	peptr%time=peptr%time+(path/c_light)*1.e9
							!Convert c_light to
							!m/nsec


!	So we now have Vpef the vector from the origen to the point where the
!	photon hits the facet. Get the vector from this point to the
!	center of curvature of the facet(the PH point).

	Vn=-Vpef+V2fl+Vph

!	Covert this to a unit vector. This is the normal to the surface we
!	are reflecting from.

	mag_vn=sqrt(dot_product(vn,vn))!dot_product is intrinsic function
	vn=vn/mag_vn

!	At this point we include the effect of mirror imperfections and
!	pointing errors by throwing over a gaussian of width .10 deg(Trevors
!	guess) and using GEOM to re-orient the normal vector randomly.
!	Now since this is a reflection we need only 1/2 of the spotsize
!	jitter.
!       28/8/00 GHS
!       Instead of raidal guassian dist with random in phi, use gaussin in x
!       and gaussian in y. I don't know why this is a better model but it 
!       matches	the measured surface brightness distribution much better.
!       Also, use totaly r*8 version. This gets around roundoff problem for 
!       small angles.
        !
!	 tix=(gauss(xdummy)*jitter_width)
!        tiy=(gauss(xdummy)*jitter_width)
! 08/05/ghs Use seperate jitter widths for e-w and ns(top-down) for oval 
! spotsizes.

	tix=(gauss(xdummy)*jitter_width_ew)
        tiy=(gauss(xdummy)*jitter_width_ns)
        call geom8(vn(1),vn(2),vn(3),tix,tiy)

!        tix=gauss(xdummy)*jitter_width
!	call geom(vn(1),vn(2),vn(3),tix)


!	Now we can do the reflection of the incident ray. The reflected ray
!	will be the sum of the part of the incident ray that is perendicular
!	to the facet mirror with the negative of the component of the incident
!	ray
!	that is parallel.

!	The dot product gives us the projection along Vn: the parallel part

	vpara=vn*dot_product(vdcsn,vn)!dot_product is intrinsic function
  
!	Perpendicular component is just difference.

	vperp=vdcsn-vpara
  
!  Find dl,dm,dn the unit reflected direction vector
!	It is unitized already(except for any roundoff. Ignore that)
!	Don't need third component.
	vdr=vperp-vpara

!	Now find position vector in focal plane of where this photon hits.
!	First get length of vector from hit point on mirror to hit point in\
!	focal plane. z componet is equal to z component of the hit position.
!	Cosin of angle is vdr(3). so:

	w10m_ct=-vpef(3)/vdr(3)		!Always positive

!	01/12/97 GHS
!	This is also the path length of the reflected photon from the facet to
!	the focal plane. Adjust PE_TIME of arrrival of the photon.
!	The correction should always be postitive.

	peptr%time=peptr%time+(w10m_ct/c_light)*1.e9
							!Convert c_light to
							!m/nsec
	
!	Use this to scale unit vector.(we really only need x,y components)
!	Sum of mirror hit position vector and focal plane hit vector is
!	focal plane vector.
!	also convert to deg
	peptr%vw=(vpef+(w10m_ct*vdr))/meters_per_deg
	return
   END SUBROUTINE W10M_FULL_ABERRATION
!******************************************************************************

END MODULE KASTRIGGER_SUBS
!******************************************************************************

!******************************************************************************
    PROGRAM KASTRIGGER
!  Version: V:1:3:8:3:5.3"

!	We want to find triggers for a single mount using various camera
!	configurations on a  Whipple 10m Mirror. This is used to look at the 
!	sensitivity of a telescope or an array of detectors to proton(hadron)
!	and Gamma showers.

!	All parameters for the mount are read from an .par file.

!	Pe's that meet the required parameters can be written to a summary
!	file(unit=7)OUTPES=.true.
!**************ABOVE still need some work for multiple cameras***********

!	It uses as input the sorted PE file that KASLITE and KASSRTMRG have
!	produced.

!	It produces a seperate 'M' file for each camera that has in each record
!	the array of pixel pe counts(excluding noise) for the trigger. Only 
!	events that pass the trigger cut will be written out. The DISC (pixel)
!	array will in the same order as normal 10m events.

!	Files:
!	unit=1	Input parameter file.
!	unit=2  Input sorted pe file.
!	unit=3	Random number seed file.
!	unit=7  Sorted pe summary file.
!	unit=8-15 Triggered Area 'm' files for various cameras

!	This is a conversion of the F77 KASTRIG program  to F90 code.
!	(with lots of clean up done as is appropriate.)
!	Only Whipple telescope code is retained.

!	Written by:
!	Mary P. Kertzman
!	Dept. of Physics and Astronomy
!	DePauw University
!	Greencastle, In. 46135
!	E-Mail: "kertzman@depauw.edu"

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"

!	Modified:

!	23/1/98 GHS  V:1:1:5:1:1.1
!		Conversion from original KASTRIG program. This program is
!		designed to look for triggers from several different cameras on
!		the whipple 10m mirror at the same time. All non whipple stuff
!		has been removed. All EAR stuff has been removed.

!	28/3/98	GHS V:1:1:5:1:1.2
!		Rearrange some stuff in W10_PROC into a new routine:
!		WHIPPLE_PROC. This is set up so that we onloy need to go
!		through the pe's once for all directions and all cameras.
!		Do this to save time.
!		WHIPPLE_PROC is called here.

!	13/5/98	GHS V:1:1:5:1:2.0
!		Set up for VERITAS.
!		1:Mount direction supplied here now. Unlike the past we now
!		  do not find the relative values of pe directions and timings
!		  to the mount plane in KASLITE. Instead the PES file has the
!		  directions and timings for when the pe's hit the ground.
!	          It is in this program where we read in the mount direction
!		  (DATAIN) and preform the relative pe directions and adjusted
!		  timing calculations. Put dl,dm,dn into mhead in place of where
!		  it used to come in in KASLITE.
!		  VECINI is now replaced by MOUNT_VECINI(which usees
!		  VECTOR_REDIRECT) to give unit vectors in original frame of
!		  all required mount directions. It also determines unitvectors
!		  for x,y in the mirror planes of all directions.
!		  Do this to increase flexability in mount direction
!                 choice.
!		2:Move setting of DISC_WIDTH and ADC_GATE to DATAIN from .PAR
!		  file. Just cleaning up really.
!		3:Add MHEAD%CONCENTRATION paramerter to m_head in
!		  STRUCTURES.F90. Used to calculate efficiency along with
!		  MHEAD%REFLECTIVITY. Read in from parameter file in DATAIN.
!		  Add pptr%efficiency array. Fill with efficiency factor in
!		  WCAMERA_GEN
!	12/6/98	GHS V:1:1:5:1:2.1
!		Add option 'TIM' to get timing of each triggered pixel(with
!		noise) and also overall event trigger time.Use the PEPULSE
!		building terchnique(.25 ns steps of pulses built up from
!		single pe pulse shape with varying pulse hieghts. Use CFD
!		timing determination.
!		Dynamically allocate accululation arrays for pe times and
!		pixels in WHIPPLE_AREA_INI.
!		Setup call to W10M_PEPULSE routines.
!		Write out compressed arrays in a slightly different format.

!	09/7/98 GHS V:1:1:5:1:2.2
!		I changed MOUNT_VECINI into W10M_MOUNT_VECINI and placed
!		it in the W10M_subs.f90 package. Do this so KASAOMEGA can use
!		it also.  Change W10M_MOUNT_VECINI to get DL,DM,DN as 
!		arguments. Do like this for compatability reasons
!		between kastrigger and kasaomega.
!		I also placed W10M_VECTOR_REDIRECT into W10M_SUBS.

!	25/2/99 GHS	V:1:1:5:1:2.3
!		Add capability for modeling trigger using the full blown
!		timeing. Use pulse shapes and CFD times to get trigger time
!		Requires modifying W10_PEPULSE_ANALIZE and where its called
!		from. Use of 'TIM' option causes use of pepulse construction
!		for trigger.

!	07/7/99 GHS V:1:1:5:1:2.3
!		For VMS/LINUX compatability add a new module VMS_LINUX. This
!		will have various constants defined as needed. Will have to
!		be edited for VMS or LINUX option.

!	15/7/99 GHS V:1:1:5:1:2.4
!		Add the LINUX MPI capability. (MUlti processor using MPI 
!		commands.)
!		Do this by extracting various subroutines and making 2
!		versions. One is for standard single(no change to what we have)
!		the other is the MPI version.
!		MPI: Break up KASTRIIGER into 3 different procedures. 1:Parent:
!		ONLy 1 of these. Opens input data file, reads header and
!		passess to event processing daughters.reads areas, paseses 
!		areas to requesting daughters, recieves results from daughters
!		and writes to output file. 2:Random number generator daughte:
!		Only 1 of these. Generaates random number sequence and
!		distributes to requesting daughters. 3:Event processing
!		daughters. Any number of these can be used . The more the 
!		better. Receve single area pes from Parent, process to an event
!		Return results to parent. Request ranodom numbers as needed.
!		All processors(except random daughter) will read .par file. 
!		Use of RDIST will insure correct duplication of of this file. 
!		Main program remains untouched. as does  WHIPPLEINI,
!		WCAMERA_GEN,WCAMERA_NOISE_THRESH_GEN,WHIPPLE_CAM_INI (and
!		whatever they call) is also untouched. Again all needed files 
!		are delived to all daughters with RDIST. I know that the parent
!		dosen't need this but it won't hurt. Just takes a little time 
!		at startup. This is a philsopy I will follow for all the MPI 
!		conversion. Only modify	whats absolultly necessary. This helps
!		retain maintianability in source code.
!		First instance of MPI use is in DATAIN. Thus the source of
!		DATAIN will be in a seperate file (KASTRIGGER_SNGL.F90) and
!		the MPI version will be in KASTRIGGER_MPI.F90. MPI is inited in
!		the DATAIN MPI version. The module will be called 
!		KASTRIGGER_SUB2.The choice of single vs MPI version of 
!		KASTRIGGER can thus be made at link time.
!		Also in the MPI version of KASTRIGGER_MPI will be a MODULE
!		called MPI_DEFS for local MPI version specific variables.
!		Other routines with SNGL/MPI versions in KASTRIGGER_SNGL.F90
!		and KASTRIGGER_MPI are:FILEOPN

!	16/11/99 GHS V:1:1:5:1:2.5
!		Add stuff for Whiplle small pixel camera:W490. This camera uses
!		379 small 1/2" pixels + 3 rings of 37 each 1" pixels. 
!		1:Define camera in new subroutines: WCAMERA_GEN490 and 
!		  GENERATE490.
!		2:Call GENERATE490 from WHIPPLEINI.
!		3:Add special stuff in WCAMERA_NOISE_THRESH_GEN(IN WCAMERA.F90)
!		for the outer rings of the 490 pixel camera which
!		doesn't have lightcones. Modifies WNOISE calculation.
!		4: Open output m file and write out mhead to it. FILEOPN
!		5: In WHIPPLE_PROC:Add a call to whipple_area_ini for 490 
!		   camera. Call WHIPPLE_CAM_PROC. Call WHIPPLE_IMAGE_PROC
!		6: In WHIPPLE_CAM_PROC: Add code to see for 490 camera is pe
!			hits a pixel in one of the outer rings.
!		7: Change to the use of DN_MIN_LOOSE and DN_MIN_TIGHT in
!		   WHIPPLE_TILT. DN_MIN_LOOSE is a full 1.o deg bigger to
!		   allow forlarge scatters and aberations. Add the tight cut to
!		   the end of WHIPPLE_TILT.

!	21/3/00 GHS V:1:1:6:1:2.6
!		Add stuff for Laura's square pixel camera. This camera uses
!		a square array of pixels. 32 by 32=1024. Use the small 1/2"
!		pmts as used in the w490 camera.
!		1:Define camera in new subroutines: WCAMERA_GEN1024 and 
!		  GENERATE1024.
!		2:Call GENERATE1024 from WHIPPLEINI.
!		3:Add special stuff in WCAMERA_NOISE_THRESH_GEN(IN WCAMERA.F90)
!		4: Open output m file and write out mhead to it. FILEOPN
!		5: In WHIPPLE_PROC:Add a call to whipple_area_ini for 1024
!		   camera. Call WHIPPLE_CAM_PROC. Call WHIPPLE_IMAGE_PROC
!		6: Modify WCAMERA_PIX for 1024 camera
!		7:
!	
!	12/7/00 GHS V:1:1:6:1:2.7
!		Add stuff for muon rejection idea through UV filtering.
!               Use mhead%option 'UV'
!		1: Add to camers_ptrs wavelength and spec arrays
!			( in WCAMERA_MODULES.F90)
!		2: Allocate pptr%wavelength and pptr%spec in WHIPPLE_AREA_INI
!		3: Accumulate wavelengths into pptr%wavelength in W10M_CAM_PROC
!		4: For trigger write out wavelength, spec, muon_ration. In
!		   WHIPPLE_IMAGE_PROC    Goes to for051.dat.

!       28/8/00 GHS V:1:1:6:1:2.8
!               Convert W10M_FULL_ABERRATION to r*8 internally to avoid some 
!               small angle roundoff problems. Use geom8.  
!               Use cartisian (x gaussian and y
!               gaussian independently) for jitter of mirror normal.

!       21/9/00 GHS V:1:1:6:1:2.9
!               Add HDF5 capability.  All new HDF5 C routines in KASHDF5.c
!               1:PST_MULT.DAT file assumed to be in HDF5 format.
!                 Changes to w10m_pepulse.f90
!               2:Add capability for input PES file to be in HDF5 format.
!                In the options list;  PESFILE=HDF5 enables reading
!                of an HDF5 file of pes. Any other option(or none) assumes
!                unfomatted sequential(non-portable) format. Do this so files 
!                are portable between platformas(pc and alpha in particular).
!                Changes to FILEOPN, READAREA and to this routine.
!       6/10/00 GHS V:1:1:6:1:2.10
!               Add more HDF5 capability.All new HDF5 C routines in KASHDF5.c 
!               1.Add cabability to writer output Mfile as HDF5 file.
!               2. Change options so that HDF5 is assumed for input PESFILE
!                  and for output Mfile. PESFILE=BIN idicates we use 
!                  old format( program pes2hdf5 exits to conver old pes files)
!                  MFILE=BIN indicates we use old format(Program m2hdf5 exists
!                  to conver old type Mfiles to HDF5).New routine( substitues 
!                  for some of the guts of W10_PEPULSE_PACKED

!       16/04/01 GHS V:1:1:6:1:2.11
!               Reimpliment the quick and dirty way of finding disc triggers 
!               and adc values (by summing pe's in each pixel). One would use 
!               this instead of the pepulse method when speed is necessary.
!               1:WHIPPLE_AREA_INI: Since we will always save the pixel timing 
!               information no matter which trigger method we use always 
!               allocate the pptr%time,pptr%time_pixel arrays.
!               2:WHIPPLE_OUTPUT_M: Always write out pixel timing information.
!               3:WHIPPLE_CAM_PROC:Always call W10M_PEPULSE_ACCULMULATE
!               to fill pptr:time,pptr%time_pixel.
!               4:WHIPPLE_CAM_INI:Since W10M_MULT_TIRG AND W10M_PST_TRIG both 
!               expect pptr%time_trig and pptr&trig_index always allocate them.
!               5:WHIPPLE_IMAGE_PROC: Use 'TIM' option to determine if we 
!               analize useing the pepulse method or the old sumed pes in 
!               pixel method. Note change of option to 'TIM'
!               6:Redo W10M_TRIGGER in W10M_SUBS.F90 to use argument list 
!               simular to W10M_PEPULSE_ANALIZE. Use SINGLE_PE_HEIGHT to 
!               genertae pixels pulse height. Check for PST trigger.

!       08/05/01 GHS V:1:1:6:1:2.12 
!               In w10m_full_aperation Use seperate jitter widths for e-w and 
!               ns(top-down) for oval spotsizes.

!	11/07/01 GHS V:1:2:6:1:2.13
!		Add stuff to print out Heavy ion primary name. In FILEOPN.

!       18/04/03 GHS  V:1:3:7:2:3.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               ./kastrigger  -p kastrg.par (sortof like in c).	
!               We do this to run on multi cpu processors.

!       06/05/03 GHS  V:1:3:7:2:3.1
!               Update VERITAS mirror parameter(in WHIPPLE_TELESCOPE.F90)
!               Add mirror_radius_sqaured variable.

!       07/05/02 GHS V:1:3:8:3:4.0
!               Another Major version change. To handle very large pes
!               files (showers ~1 Tev+) use a direct access file for the pes 
!               input file 
!               Requires getting the header record from a seperate 
!               file (pes_input_file//'.head' )
!      22/04/03 GHS V:1:3:8:3:4.1"
!               1:490 Pixel camera has only 331 pixels in bot multiplicity and
!                 PST trigger, not 379! Fix W10M_PROC
!                 to use wptr%ntrigger_in. WCAMERA_DEF already has that.
!               2:Extend largest possible length of input command ine string
!                 to 120 characters (from 80). In wcamera_modules.f90.
!               3:Change and add some options:
!               -C HRC and year for .cpeds,.coff etc :WCAMERA_NOISE_THRESH_GEN 
!               -h (help) New option:lists all command line options.

!      29/04/03 GHS V:1:3:8:3:4.2
!                Add a call to new routine W10M_PEPULSE_INIT in W10M_IMAGE_PROC
!                to create the single pepulse array with appropriate rise time.
!                Only use for w10m cameras qhe TIM option requested 
!                (Veritas camers do this on their own). This replaces the fixed
!                rise time pulse used previously.

!      27/04/07 GHS V:1:3:8:3:4.3
!                Driffting Gammas: For the whipple strip we need to get GAMMA 
!                efficiencies over all the camera.  Do this by treating gammas
!                as hadrons (tilting the mount in various directions). We want
!                to step in theta only up to gtheta_max (7=2.1 degs nominally)
!                Set the step size to gtheta_step_size (nominally =.3 degrees)
!                We don't really care about phi steps so in intereset of speed
!                and file size use the smallest we can (2 I think)
!                This all happens when the input command line -d option is 
!                given as 'T'. This causes the logical GAMMA_DRIFT to be set to
!                .true. Set this all up in WHIPPLE_INI and STRUCTURES.F90
!     03/06/04 GHS V:1:3:8:3:4.4 
!                For drift scan we really only want positions along x axis. 
!                This happens for iphi=1 and if phi_steps is 2 for 
!                iphi=itheta. This second one gets you the negative positions 
!                along the x axis. (Think about it!) So, in WHIPPLE_IMAGE_PROC
!                and WHIPPLE_PROC make these cuts in the itheta,iphi loops.
!                But see 07/06/04
!                Add an elevation command line input option (-e). Value is 
!                elevation in degreees. If used ( ELEVATION_INPUT=.true.) Then
!                in datain dl,dm,dn will be overridden by elevation and az=0.
!      08/06/04 GHS V:1:3:8:3:4.4
!                Dynamically allocate dlm,dmm,dnm,xdlm,xdmm,xdnm,ydlm,ydmm,ydnm
!                in WHIPPLEINI
!      09/06/04 GHS V:1:3:8:3:4.4
!                Rewrite W10M_MOUNT_VEC_INI. Use only iphi=1 and iphi=itheta
!                for gamma_drift. Hadrons same as before. Make x -axis be along
!                -ra for boith gamma_drift and hadrons. Gamma_drift step_size
!                is .4 min along +/- ra (iphi=1,itheta=iphi). Hadrons now also
!                have x axis along ra but that really not important for 
!                hadrons. Added a number of routines to do all this. Most in
!                w10m_subs.f90 but some c++ ones in Veritas.cpp. They use the
!                sla libs.
!     15/06/04 GHS V:1:3:8:3:4.5 
!                In WHIPPLE_PROC: for W490: After call to WHIPPLE_TILT  but 
!                before call to WHIPPLE_CAM_PROC, rotate where pe is in image 
!                plane (WX,WY)to reflect the fact that the WHIPPLE 490 pixel 
!                camera is rotated by 180 + 7.306 degrees from having pixel 2 
!                on the + x axis.      
!     17/06/04 GHS V:1:3:8:3:4.6
!                Add a -w Option (sets spec_weight_flag to true) which causes 
!                the ouput MHDF5 file events to be cut on the spectrum weights.
!     20/09/04 GHS V:1:3:8:3:5.0  NEW VERSION
!                Problems with our old scheme of itheta/iphi. Seen in 2d x,y 
!                plots.  We can see effects of phi steps (5 fold symmetries)
!                Going to replace (for hadrons only, leave gammas and 
!                drift scan as is) with choosing random directions (use same 
!                number of directions as with itheta iphi, (106 for 
!                itheta_max=7 iphi_steps=5) but chose directions randomly 
!                within direction circle of radius: 
!                (itheta_max-1)*step_size+step_size/2= (= 6.5 deg for 
!                                             itheta_max=7and stepsize=1.0 deg)
!                1:Move ithphi_max definition to structures.f90. Init in 
!                  WHIPPLEINI. 
!                2:Convert all dlm dmm xdlm etc arrays to single index 
!                  (ithphi)
!                3:Add new routine: GET_ITHETA_IPHI to convert from ithphi
!                  index to itheta,iphi indices.
!                4:Replace itheta and iphi loops with single ithphi loop in
!                  WHIPPLE_IMAGE_PROC and  WHIPPLE_PROC
!                5:Modify W10M_MOUNT_VECINI to generate random directions for 
!                  hadrons within circle defined by itheta_max and step_size
!                6:For pure hadrons save actual theta,phi (in radians*1000) in
!                  mrec: itheta iphi(which is i*2)else save itheta and iphi 
!                  cell indicies.
!     28/10/04 GHS V:1:3:8:3:5.1
!                Fix call to RANEND to save random seeds. Was being saved 
!                to wrong place. Thus we weren't evolving the seed.
!     01/11/04 GHS V:1:3:8:3:5.2
!                Upgrade the MFILE=MHDF5 option to use the MHDF5SimDataFile
!                classes (instead of the old mhdf5_* routines). This should
!                speed thing up substantially.
!                1:New routines to init and write the ihead record of the
!                  output MHDF5 file. Changes to FILEOPN.
!                2:Convert the write of the MHDF5 to the MHDF5SimDataFile 
!                  in WHIPPLE_OUTPUT_M
!     17/02/05 GHS V:1:3:8:3:5.3
!                Add printseeds flag to ranstart and ranend

!*****************************************************************************
!  Command line input options:
!*****************************************************************************
! -p kastrg.par   :File name for input parameter file:DATAIN 
! -r kastrg.ran   :Random number seed file name.     :DATAIN,RANLUX_SNGL
! -c hrcyear      :HRC and year for .cpeds,.coff etc :WCAMERA_NOISE_THRESH_GEN 
! -i pes_in.dat   :Input pes binary format file.     :FILE_OPEN
! -j pes_in.hdf5  :Input pes hdf5 format file.       :FILE_OPEN
! -o mfile_out.dat:Output mfile binary format file.  :FILE_OPEN
! -r mfile_out.hdf:Output mfile hdf4 format file.    :?
! -s output HDF5 m filename
! -d set drifting gammas flag (T or F)
! -e Input elvation of mount(az assumed to be 0 (north))
! -w Cut events (into the MHDF5 file) according to energy spectrum weights 
!*****************************************************************************

!dir$ name(mhdf5close="MHDF5Close")
 
      use kastrigger_def
      use record_defs
      use wcamera_def
      use kastrigger_subs
      use w10m_pepulse_subs
      use wcamera_subs
      use kastrigger_sub2	!MPI/SNGL versions of DATAIN and FILEOPN
      use kastrg_command_line
        
      IMPLICIT NONE

      integer :: i
      integer :: printseeds
      character(len=80) :: arg_opt
      integer,external :: iargc_
      character(LEN=80) :: update=  '17-feb-2005-GHS'  !Last version update
      mhead%version="V:1:3:8:3:5.3"
      !	Version number.
!	The version number has one digit for each program in the KASCADE system.
!	seperated by ':'.KASTRIGGER (which takes the place of KASTRIG)has 2 
!	digits seperated by ".".
!	New compatibility version require later programs
!	to be modified due to changing output data formats, or require 
!       rerunning
!	of the data base due to corrections to important mistakes.
!	Digit 2:	KASCADE   version
!	      3:	KASLITE   version
!	      4:	KASSRTMRG version
!	      3:	KASGTRIGGER   version.sub version
!	Save in mfile header

      write(6,1205)trim(mhead%version),trim(update)
1205  format(' KASTRIGGER***Version: ',a,' *** Last update of version:',a)

!******************************************************************************
!       Get the various command line arguments.
!       All command line arguments come in pairs: An option (-p) and 
!                                                 a string (kasoam.par)
!       Defaults are in wcamera_modules.f90
!******************************************************************************
      num_cmd_args=iargc_()
      if(num_cmd_args.gt.0)then
         print*,'KASTRIGGER--Number of command line arguments:',num_cmd_args
         do i=1,num_cmd_args,2
            call getarg_(i,arg_opt)
            arg_opt=trim(arg_opt)
            if(arg_opt=="-p")then
               call getarg_(i+1,input_par_file_name)
               input_par_file_name=trim(input_par_file_name)
               print*,' -p option gives input par filename:', &
                    &    trim(input_par_file_name)
            elseif(arg_opt=="-r")then
               call getarg_(i+1,random_seed_file_name)
               random_seed_file_name=trim(random_seed_file_name)
               print*,' -r option gives random seed filename:',&
                    &    trim(random_seed_file_name)
            elseif(arg_opt=="-c")then
               call getarg_(i+1,hrcyear)
               hrcyear=trim(hrcyear)
               print*,' -c option gives hrc year:',trim(hrcyear)
            elseif(arg_opt=="-i")then
               call getarg_(i+1,pes_in_dat_file)
               pes_in_dat_file=trim(pes_in_dat_file)
               print*,' -i option gives input binary pes filename:',&
                    &    trim(pes_in_dat_file)
            elseif(arg_opt=="-j")then
               call getarg_(i+1,pes_in_hdf5_file)
               pes_in_hdf5_file=trim(pes_in_hdf5_file)
               print*,' -j option gives input hdf5 pes filename:',&
                    &    trim(pes_in_hdf5_file)
            elseif(arg_opt=="-o")then
               call getarg_(i+1,mfile_out_dat_file)
               mfile_out_dat_file=trim(mfile_out_dat_file)
               print*,' -o option gives ouput binary m filename:',&
                    &    trim(mfile_out_dat_file)
            elseif(arg_opt=="-s")then
               call getarg_(i+1,mfile_out_hdf_file)
               mfile_out_hdf_file=trim(mfile_out_hdf_file)
               print*,' -s option gives output HDF5 m filename:',&
                    &    trim(mfile_out_hdf_file)
            elseif(arg_opt=="-d")then
               call getarg_(i+1,drifting_gammas_flag)
               If(index(drifting_gammas_flag,'T')/=0)then
                  gamma_drift=.true.
               endif
               print*,' -d option gives Drifting Gammas flag value: ',&
                    &  gamma_drift
            elseif(arg_opt=="-w")then
               call getarg_(i+1,spec_weight_text)
               If(index(spec_weight_text,'T')/=0)then
                  spec_weight_flag=.true.
               endif
        print*,' -w option gives Cut On Energy Spectrum Weight flag value: ',&
                    &  spec_weight_flag
            elseif(arg_opt=="-e")then
               call getarg_(i+1,elev_txt)
               elev_txt=trim(elev_txt)
               read(elev_txt,1000)elevation
1000           format(f8.3)
               print*,' -e option gives mount elevation ',elevation,' deg.'
               print*,' -e overides dl,dm,dn from .par file(datain)'
               elevation_input=.true.
            elseif(arg_opt=="-h")then
        print*,' *************************************************************'
        print*,' * Command line input options:'
        print*,' *************************************************************'
	print*,' * -p kastrg.par   :File name for input parameter file'
	print*,' * -r kastrg.ran   :Random number seed file name.'
	print*,' * -c hrcyear      :HRC and year for .cpeds,.coff etc'
	print*,' * -i pes_in.dat   :Input pes binary format file.'
	print*,' * -j pes_in.hdf5  :Input pes hdf5 format file.'
	print*,' * -o mfile_out.dat:Output mfile binary format file.'
        print*,' * -s output HDF5 m filename'
	print*,' * -d drifting gammas flag (T or F)'
        print*,' * -e Input elevation of mount(az assumed to be 0 (north))'
        print*,'      Overides dl,dm,dn from .par file(datain)'
        print*,' * -w Cut events (into the MHDF5 file) according to energy &
                      & spectrum weights' 
        print*,' * -h Print this options summary'
        print*,' *************************************************************'

            else
               print*,' Illegal command line option #:',i,'Option:', &
                    trim(arg_opt)
               stop 'KASTRIGGER: Illegal command line option'
            endif
         enddo
      else
         print*,' KASTRIGGER--No command line arguments.'
         print*,' KASTRIGGER--Using defaults for all.'
      endif

!******************************************************************************


      call datain		!Get input parameters.

!	Set up for detector
      call whippleini		!Init for whipple 10m detector.
				!set up lookup tables for pmt #'s.

!	Open the PES file which has been assigned to unit 2. Open output 'M'
!	file if specified. Transfer header.

      call fileopn	!Among other things it writes out mhead record.
			!thus mhead%* must be filled in *ini calls.

!	See MODULE STRUCTURES for the format of the PE records.

!	The PES file is sorted by nx then ny then time.

!	The scheme for proceding from here is to:
!	1:gather all pe's for an area.
!	2:For a HADRON search seperate them by theta and phi indicies 
!	collecting mirror hits and times for each theta,phi 'cell'
!	3:Go through the 'cells' as each pe is collected calculating the
!	multiplicity this new pe creates in this cell.  Add the noise as needed
!	as we do this.

!	Read in an area. This is needed by all detector types.
      do
         call readarea
         if(empty)then
            exit	!Leave if this is an empty file.
            !If this is the last area of the shower
         endif        !then the variable SHOWER_END will be set .true.

		!Require at least 2 pe's in the area.
         if(ipe.ge.2)then
				!Process all pe's, all directions, all cameras.
            call whipple_proc
         endif
         if(shower_end)then
            print*,'Shower ends'
            exit
         endif
      enddo
      
!	End of shower.
      If(index(mhead%options,"PESFILE=BIN")/=0)then
         close(unit=2)
!      else
!                                        ! All peshdf5 are C routines.
!         call peshdf5_close          !Close input HDF5 file.
      endif


      If(index(mhead%options,"MFILE=BIN")/=0)then
         if(w37%on)then 
            close(unit=w37%unit)
         elseif(w109%on)then 
            close(unit=w109%unit)
         elseif(w151%on)then
            close(unit=w151%unit)
         elseif(w271%on)then 
            close(unit=w271%unit)
         elseif(w331_125%on)then
            close(unit=w331_125%unit)
         elseif(w331_1875%on)then
            close(unit=w331_1875%unit)
         elseif(w331_25%on)then
            close(unit=w331_25%unit)
         elseif(w490%on)then
            close(unit=w490%unit)
         elseif(sq1024%on)then
            close(unit=sq1024%unit)
         elseif(w541%on)then
            close(unit=w541%unit)
         elseif(v499%on)then
            close(unit=v499%unit)
         else
            print*,' Bad Detector option'
            stop 'Bad detector option'
         endif
      else
         !Output went to a MHDF5 file
         call mhdf5close
      endif


      print*,'KASTRIGGER:Number of Events written to M file:',m_event_count
       
!     28/10/04 GHS V:1:3:8:3:5.1  Fix call to RANEND to save random seeds. 
      if(outpes)close(7)
      printseeds=1
      call ranend(printseeds,random_seed_file_name)  !Save random number seed vector.
      stop 'KASTRIGGER Normal end'
    END PROGRAM KASTRIGGER
!*****************************************************************************
