!Modified:
!	18/5/98 GHS
!	Add pptr%efficiency to camera_ptrs
!	12/6/98 GHS
!	Add pptr%time,pptr%time_pixel,pptr%time_trig,pptr%time_disc 
!		for timing studies.
!	05/3/99 GHS
!	Remove time_disc. Add WCAMERA_PST_DEF module.
!	12/5/99 GHS
!		Add Dead pmt pointer
!	26/5/99 GHS
!		Add pmt gains pointer
!	16/2/00 GHS
!		Convert w10m_time_gap to an array
!	21/3/00 GHS
!               Add SQ1024 camera.
!	12/7/00 GHS
!		Add wavelength to camera_ptrs for use in muon filter study.
!       13/05/03 GHS Add patch_trigger_pattern pointer.
!       03/03/04 GHS V:1:3:8:3:4:4.13
!               Extend largest possible length of input command line strings
!               to 120 characters (from 80).
!        30/03/04 GHS V:1:3:8:3:4:4.16
!                Add PPTR%TIMING_OFFSET
!        31/03/04 GHS V:1:3:8:3:4:4.17
!                Add PPTR%SINGLE_PE_SIGMAS.
!        06/09/05 GHS
!                Convert veritas 499 camera to .15 deg pixel spaceing. 
!                (15.53->15.74)

MODULE WCAMERA_DEF
			!Standard hex camera definition:331,151,270,37
	type camera_ptrs
		integer,pointer,dimension(:,:) :: wadjacent

		real,pointer,dimension(:) :: camera_x
		real,pointer,dimension(:) :: camera_y
		real,pointer,dimension(:) :: pmt_spacing
		real,pointer,dimension(:) :: pmt_radius
		real,pointer,dimension(:) :: efficiency
		real,pointer,dimension(:) :: threshold
		real,pointer,dimension(:) :: wnoise
		real,pointer,dimension(:) :: sky_mean_disc
		real,pointer,dimension(:) :: cfd_threshold
		real,pointer,dimension(:) :: high
		real,pointer,dimension(:) :: low
		real,pointer,dimension(:,:,:) :: lines
		real,pointer,dimension(:,:) :: disc
		integer,pointer,dimension(:) :: pes_in_image
		real,pointer,dimension(:) :: muon_ratio
		real*8,pointer,dimension(:) :: emission_alt
		real*8,pointer,dimension(:) :: em_alt_sig
		real,pointer,dimension(:) :: ldisc		!Local working
		real,pointer,dimension(:) :: ldisc_pes		!arrays
		real,pointer,dimension(:) :: ldisc_pulse_height
		real,pointer,dimension(:) :: ldisc_trigger
		real,pointer,dimension(:,:) :: time
		real,pointer,dimension(:,:) :: time_pixel
		real,pointer,dimension(:) :: time_trig	   !Pixel trigger times
		integer,pointer,dimension(:) :: trig_index !Ordering index array
		real,pointer,dimension(:) :: active_pmts  !Non-dead pmts
		real,pointer,dimension(:) :: active_hv    !Non-turned off HV
		real,pointer,dimension(:) :: gain	  !Pmt gains
		real,pointer,dimension(:) :: sigmas	  !external sigmas
	 	real,pointer,dimension(:) :: mean_time_gap !Mean time gap
							 ! between noise pulses
		real,pointer,dimension(:) :: mean_noise	  !Cfd mean noise level
		real,pointer,dimension(:) :: laura_adc	  !No-noise adc values
		real,pointer,dimension(:,:) :: wavelength !wavelength of pe's
		real,pointer,dimension(:,:) :: spec       !Specias of emmiting
		real,pointer,dimension(:) :: pe_dc    !Jittered pedc per pmt
		real,pointer,dimension(:) :: ped    !Jittered ped per pmt
		real,pointer,dimension(:) :: timing_offset !Timing offset/pmt
		real,pointer,dimension(:) :: single_pe_sigmas !Widths of single
                                                 !pe pulse height distributions
	end type camera_ptrs

	type camera
		logical :: on		!Flag we are to process this camera
		integer :: npmt        !Number of pmts in this camera
		integer :: ntrigger_in  !Number of pixels to include in trigger
		integer :: nlines	!Number of lines in the camera
		integer :: neighbor	!Max number of neighbor pixels
		real,dimension(3) :: spacing_pmt !in,mid,out in mm.
		real,dimension(3) :: radius_pmt !in,mid,out in mm.
		real    :: wcamera_min_dn
		integer :: unit
		character(len=8) ::camera_id	!Ascii string to idintify
						!camera type
		logical :: dead_pmts		!Flag were are to disable pmts.
                logical :: mfile                !HDF5 mfile id
             end type camera
	
!37 Pixel camera
	type(camera),target :: w37 = &
  &camera(.false.,37,37,3,6,(/30.0,.0,.0/),(/25.4,.0,.0/),0,14,"37",.false., &
  &.false.)
	type(camera_ptrs),target :: p37
!109 Pixel camera
	type(camera),target :: w109 = &
     & camera(.false.,109,91,6,6,(/16.28,.0,.0/),(/14.0,.0,.0/),0,14,"109" &
     & ,.false.,.false.)
	type(camera_ptrs),target :: p109
!151 Pixel camera
	type(camera),target :: w151 = &
    & camera(.false.,151,91,3,7,(/16.28,.0,.0/),(/14.0,.0,.0/),0,14,"151" &
    & ,.false.,.false.)
	type(camera_ptrs),target :: p151
!271 Pixel camera
	type(camera),target :: w271 = &
    & camera(.false.,271,91,6,6,(/16.28,.0,.0/),(/14.0,.0,.0/),0,14,"271" &
    & ,.false.,.false.)
	type(camera_ptrs),target :: p271
!331_125 Pixel camera
	type(camera),target ::w331_125= &
      & camera(.false.,331,91,10,6,(/8.71,.0,.0/),(/6.5,.0,.0/),0,14, &
		 & "331_125",.false.,.false.)
	type(camera_ptrs),target :: p331_125
!331_1875 Pixel camera
	type(camera),target ::w331_1875= &
      & camera(.false.,331,91,10,6,(/11.71,.0,.0/),(/9.5,.0,.0/),0,14, &
		 & "331_1875",.false.,.false.)
	type(camera_ptrs),target :: p331_1875
!331_25 Pixel camera
        type(camera),target :: w331_25= &
! Pixel Spacing chnaged for '98-'99 camera GHS 05/3/99
!     & camera(.false.,331,91,10,6,(/16.21,.0,.0/),(/14.0,.0,.0/),0,14, &
      & camera(.false.,331,91,10,6,(/15.5,.0,.0/),(/14.0,.0,.0/),0,14, &
                 & "331_25",.false.,.false.)
	type(camera_ptrs),target :: p331_25
!490 Pixel Camera
     	type(camera),target :: w490 = &
      & camera(.false.,490,331,11,7,(/7.5,0.0,0.0/),(/5.0,12.5,0.0/),  &
                 & 0,14,"490",.false.,.false.)
	type(camera_ptrs),target :: p490

	real,dimension(3) :: w490_outer_ring_dia=(/.3503422,.401752,.450367/)
								 !meters	

!541 Pixel Camera
     	type(camera),target :: w541 = &
      & camera(.false.,541,331,15,7,(/8.71,17.0,36.5/),(/6.5,14.0,25.4/),  &
!      & camera(.false.,541,511,15,7,(/8.71,17.5,37.5/),(/6.5,14.0,25.4/),  &
                 & 0,14,"541",.false.,.false.)
	type(camera_ptrs),target :: p541


!!Veritas 499 Pixel camera .148 deg spacing FOV=3.33 deg. Vlad's design.
!!	(smallest vlad can get for 3/4" pmts
!	type(camera),target :: v499 = &
!      & camera(.false.,499,463,13,6,(/15.53,.0,.0/),(/12.48,.0,.0/),0,14, &
!      & "499",.false.,.false.)
!	type(camera_ptrs),target :: pv499

!Veritas 499 Pixel camera .15 deg spacing FOV=3.33 deg. Vlad's design.
!	(smallest vlad can get for 3/4" pmts
	type(camera),target :: v499 = &
      & camera(.false.,499,463,13,6,(/15.74,.0,.0/),(/12.48,.0,.0/),0,14, &
      & "499",.false.,.false.)
	type(camera_ptrs),target :: pv499

!Laura's 1024 pixel camera(square pixels)
	type(camera),target :: SQ1024 = &
      & camera(.false.,1024,1024,0,8,(/7.5,0.0,0.0/),(/5.0,0.0,0.0/),0,14, &
      & "1024",.false.,.false.)
	type(camera_ptrs),target :: psq1024

!20/09/04 GHS Make following arrays 1d instead of 2d (ithphi instead io 
!itheta,iph as indices)
	real,pointer,dimension(:) :: dlm,dmm,dnm,xdlm,xdmm,xdnm,ydlm,ydmm, &
                                     & ydnm,stheta,sphi
        integer :: m_event_count=0
END MODULE WCAMERA_DEF

MODULE WCAMERA_PST_DEF
	integer,pointer,dimension(:,:) :: pst_pixels !Table that shows which
						!pixels are in which pattern.
						!same ordering of pixels as
						!used in experiment.
	real,pointer,dimension(:,:) :: pst_times
        integer,pointer,dimension(:,:) :: pst_trig_index
	real,pointer,dimension(:) :: patch_trigger_times
	integer,pointer,dimension(:) ::  patch_trigger_index
	integer,pointer,dimension(:) ::  patch_trigger_pattern

      	INTEGER, POINTER,dimension(:,:) :: PIXEL
	integer*2,pointer,dimension(:) :: pst_patterns

	integer,parameter :: modules_pst_331=13     !modules in Whipple camera
	integer,parameter :: modules_pst_499=19     !modules in VERITAS camera
	integer,parameter :: patches_module_pst=5	!Patches in a module
	integer,parameter :: pixels_patch_pst=19        !Pixels in a patch
							!Total number of
							!patches(61)
	integer,parameter :: patches_pst_331= &
             & patches_module_pst*(modules_pst_331-1)+1
	integer,parameter :: patches_pst_499= &
             & patches_module_pst*(modules_pst_499-1)+1
	
        integer :: patches_pst
        logical :: pst_filled=.false.
	real,parameter :: pst_strobe_delay=6.0		!Strobe delay after
							!mult trigger in PST to
							!latch pattern address.
      REAL, POINTER    :: XDEG(:), YDEG(:)	!For wcamera_pst_make_tables
      INTEGER, POINTER :: UPOS(:), VPOS(:) 
END MODULE WCAMERA_PST_DEF

MODULE KASAOMEGA_COMMAND_LINE
!Command line options  
        character(LEN=120) :: input_par_file_name       !-p
        character(LEN=120) :: random_seed_file_name     !-r
        character(LEN=120) :: hrcyear                   !-c
        character(LEN=120) :: cmsum_in_dat_file         !-i
        character(LEN=120) :: cmsum_in_hdf5_file        !-j
        character(LEN=120) :: cmsum_out_dat_file        !-o
        character(LEN=120) :: vhdf5_output_file         !-v
        character(LEN=120) :: display_dat_file          !-q
        character(LEN=120) :: pe_dc_txt                 !-d
        character(LEN=120) :: ped_sigma_txt             !-t
        character(LEN=120) :: pe_dc_sigma_txt           !-s
        integer :: num_cmd_args

END MODULE KASAOMEGA_COMMAND_LINE
MODULE KASTRG_COMMAND_LINE
!Command line options  
        character(LEN=120) :: input_par_file_name  =  'kastrg.par'     !-p
        character(LEN=120) :: random_seed_file_name   !-r
        character(LEN=120) :: hrcyear              =  &
             & '/ida/raid/sembroski/raw10/hrc00'                       !-c
        character(LEN=120) :: pes_in_dat_file    =  'pes_in.dat'       !-i
        character(LEN=120) :: pes_in_hdf5_file   =  'pes_in.hdf5'      !-j
        character(LEN=120) :: mfile_out_dat_file   =  'mfile_out.dat'  !-o
        character(LEN=120) :: mfile_out_hdf_file   =  'mfile_out.hdf'  !-s
        character(LEN=120) :: drifting_gammas_flag   =  'F'            !-d
        character(LEN=120) :: elev_txt                                 !-e
        character(LEN=120) :: spec_weight_text                         !-w

        logical :: gamma_drift=.false.                                 !-d
        real    :: elevation                                           !-e
        logical :: elevation_input=.false.                             !-e
        logical :: spec_weight_flag=.false.                            !-w
        integer :: num_cmd_args
END MODULE KASTRG_COMMAND_LINE
