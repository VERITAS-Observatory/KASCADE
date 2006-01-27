MODULE STRUCTURES
				!Structures copied from f77 version of 
				!KASSTRUCTURES.INC 26/1/98
!Modified:	
!
!	22/3/98 GHS Add wx,wx,dump to pes record. This is so we only have to
!		calculate it once for all cameras. Also now include 
!		htheta_max, and hstep_size as parameters.
!	integer,parameter :: htheta_max=8  	!Max number of steps in theta

!	07/4/98 GHS
!	A look at a 300 gev proton shower indicated that only .46% of triggers
!	occur pas htheta=7. So in the interests of speed lets limit ourselves
!	to htheta_max=7

!	18/5/98 GHS
!	Add m_head%CONCENTRATION parameter for light cones. Used in efdficiency
!	calculation.

!	01/7/98 GHS
!	hntp modifications:
!	Convert h_ntp%maxpe and h_ntp%maxpe_in to arrays.
!	Remove h_ntp%imax for max_p
!	remove h_ntp%intel
!	Add tp h_ntp: clstr_thrsh and in_clstr_thrsh

!	10/7/98 GHS
!	Add HNTP%EMALTMPLN,HNTP%XMPLN,HNTP&YMPLN

!
!       4/10/04 MPK
!       added xk and yk for 2 d analysis to ntuple
!       19/04/04 GHS
!       added ra2d and dec2d for 2d analysis to ntuple
!      27/04/07 GHS V:1:3:8:3:4.3
!                Drifting Gammas: For the whipple strip we need to get GAMMA 
!                efficiencies over all the camera.  Do this by treating gammas
!                as hadrons (tilting the mount in various directions). We want
!                to step in theta only up to gtheta_max (7=2.1 degs nominally)
!                Set the step size to gtheta_step_size (nominally =.3 degrees)
!                We don't really care about phi steps so in intereset of speed
!                and file size use the smallest we can (2 I think)
!                This all happens when the input command line -d option is 
!                given as 'T'. This causes the logical GAMMA_DRIFT to be set to
!                .true. Set this all up in WHIPPLE_INI and STRUCTURES.F90
!                But see 03/06/04.
!  02/06/04 GHS
!                In h_ntp; Change the names of clstr_thrsh(3) (clstr4) and 
!                clstr_thrs(4)(clstr5) to source_ra (src_ra) and source_dec 
!                (src-dec).
!  03/06/04 GHS
!                For drifting gammas were only looking along x axis (iphi=1 or
!                iphi=itheta fro phi_steps=2). We want gtheta_step_size of .1
!                We want to go +/- 2.0 deg so use gtheta_max 0f 21 (includes 0)
!  20/09/04 GHS  
!                Move ithphi_max (number of mount tilt directions for hadrons) !                to here.
!  20/10/04 GHS  Change name of i_threshold in mrec structure to ithphi. Keep
!                it real*4
!  21/10/04 GHS Broaden pointing direction area to 8.5 deg. Increases 
!               # directions to 181
!  29/03/05 GHS I'm going to fixup the h_ntp for the 2d stuff. 
!               1:First, add xo(2) and yo(2) for the 2 possible x,y origen of 
!                 event (Lessard elongation method) xo(1), yo(1) is most 
!                 likly from asymmetry. 
!               2:Change xk,yk to xk(2),yk(2) from 2d source location from 
!                 Mary's method. 
!               3:Change ra2d and dec2d to ra2d(2),dec2d(2) for ra and dec of 
!                 xo,yo.
!               4:Change and add to tags: xo1,xo2,yo1,yo2,ra2d1,ra2d2,dec2d1
!                 dec2d2,xm1,xm2,ym1,ym2
                
!	integer,parameter :: htheta_max=7  	!Max number of steps in theta

	integer,parameter :: htheta_max=9  	!Max number of steps in theta
	real,parameter    :: htheta_step_size=1.0
	integer,parameter :: hphi_side=5  	!Number of phi steps/"side"

!	integer,parameter :: hphi_bins=  &
!				&  hphi_side*((htheta_max*(htheta_max-1))/2)+1

	integer,parameter :: gtheta_max=21  	!Max number of steps in theta
        real,parameter :: gtheta_step_size=(0.4/(60.*24.))*(2*3.141592654)
	                                                 !.4 min in radians.
!	integer,parameter :: gtheta_max=11  	!Max number of steps in theta
!        real,parameter :: gtheta_step_size=(0.8/(60.*24.))*(2*3.141592654)
	                                                 !.8 min in radians.
!	integer,parameter :: gtheta_max=7  	!Max number of steps in theta
!        real,parameter :: gtheta_step_size=((8./6.)/(60.*24.))*(2*3.141592654)
	                                                 !1.33 min in radians.
        integer,parameter :: gphi_side=2  	!Number of phi steps/"side"
                                                !Minimum for iphi points on
                                                !+/- yaxis
        integer :: ithphi_max                   !Number of mount directions to
                                                !tilt for hadrons

!	integer,parameter :: gphi_bins=  &
!				&  hphi_side*((htheta_max*(htheta_max-1))/2)+1



	type segment_head	!** Segment Header Structure **
		INTEGER*4 :: ITYPE	! Primary particle type.
		REAL*4	  :: TEP	! Primary particle energy(TeV.)
		REAL*4	  :: DLI	! Primary particle direction(can 
					! recreate dn)
		REAL*4	  :: DMI 	! Primary particle direction(can 
					! recreate dn)
		character*4 :: MAGNET_field ! Earths mag. field specification
					!F=false =none. !W=whipple, Tucson Ariz
		REAL*4	  ::  ETR 	! Threshold level for gammas,e and mu.
		REAL*4	  ::  DEPTH	! Injection depth in gm/cm**2
		REAL*4	  ::  zOBS	! Observation altitude in meters.
		REAL*4	  ::  Xinitial	! Origen of shower.
		REAL*4	  ::  Yinitial	!
		integer*4 ::  idfile	! File id # for this shower.
		character*8 :: version	!Version of kascade that made this file.
	end type segment_head
	integer*4,parameter :: segment_head_long_word_length=13

	type pe_head		!** PE HEADER STRUCTURE **
		type(segment_head) :: segment_head
		REAL*4	::  DL		! Mount direction(can recreate dnm)
		REAL*4	::  DM		! Mount direction(can recreate dnm)
		REAL*4	::  XSEG	! Grid width in x direction.
		REAL*4	::  YSEG	! Grid width in y direction.
		REAL*4	::  HOBS	! Observation altitude in meters.
		real*4	::  X_OFFSET    !X,Y coord of core in area 0,0
		real*4	::  Y_OFFSET	
		CHARACTER*16 :: PETYPE	! Character: 'VI'=visable photons,
					! 'SB'=solar blind etc.,'VT' Veritas 
					!triangular array.
		character*16 :: version	!Version of kaslite that made this file.
	end type pe_head
	integer*4,parameter :: pe_head_long_word_length= &
			& segment_head_long_word_length + 15

	type m_head		!** M file Header structure **
		type(pe_head) :: pe_head
		logical*4 ::	noise		!Noise used in KASTRIG?
		real*4	  ::	disc_width	!Size of discriminater 
						!coincidence window.
		real*4	  ::	rate_noise	!Noise rate in pe/deg**2/ns.
		real*4	  ::	reflectivity	!Mirror reflectivity as 
						!fraction of best possible
		real*4	  ::	full_aperture	!Full aperture of tubes in deg.
		real*4	  ::	pesum		!Disc threshold for pmt's
		real*4	  ::	step_size	!Step size in theta direction
		integer*4 ::	itheta_max	!Maximum number of steps
		integer*4 ::	phi_steps	!Base number of phi steps for an
						!Itheta ring.total number=
						!(itheta-1)*phi_steps
		character*16 ::	version		!Version of kastrig that
						!made this m file.
		real*4	  ::	adc_gate	!Size of adc gate window in ns.
		real*4	  ::	hmult		!Trigger multiplicity.
		real*4    ::    concentration	!LIght cone concentration param
		character*60 ::	options		!Run options.
	end type m_head
	integer*4,parameter :: m_head_long_word_length = &
				& pe_head_long_word_length + 16 + 60/4

	integer*4,parameter :: pes_rec_size=9
        integer :: ipes_next,ipes_recl       !pes record index and file length
	type pe_rec		!**PES record**
		INTEGER*2  ::	NX	!Grid coords (x,y)
		integer*2  ::	ny
		REAL*4	   ::	time	! Time of hit
		REAL*4	   ::	dlr	! Dl of photon relative to mount
		REAL*4	   ::	dmr	! dm of photon(dn can be recreated)
		INTEGER*4  ::	NNEXT	! Segment identifier.
		REAL*4	   ::	XM	!X position of hit in grid rectangle.
		REAL*4	   ::	ym	!Y position of hit in grid rectangle.
		integer*2  ::	spec	!Type of emitting particle.
		integer*2  ::	lambda	!Wavelength of emmited photon(nm)
		real*4	   ::	em_alt	!emmison altitude
	end type pe_rec

	type pe_s		!**PES +stuff as used by kastrigger
		type(pe_rec) :: pe	!PES record
		real :: dnpe
		real :: dndnm
		type(pe_s),pointer :: next_ptr	!Points to next pe_s record.
	end type pe_s 

	integer,parameter :: mrec_size=13
	type m_rec		!** Header stucture for M file record**
				!NOTE: disc array is allocated seperatly.
				!NOTE: If you add or subtract from this
				!definition be sure to change mrec_size.
		integer*2   ::	nx
		integer*2   ::	ny
		integer*2   ::	itheta
		integer*2   ::	iphi
		real*4 	    ::	emission_alt
		real*4	    ::	em_alt_sig
		real*4	    ::	muon_ratio
							!(below not used)
		real*4	    ::	median_time 	!Meadian time of all pes(iso)
		real*4	    ::	fp_median_time   !Meadian time of all pes(aniso)
		real*4	    ::	time_cfd         !First cfd trigger time
		real*4	    ::	fp_time_cfd      !Focal plane trigger time.
						
		real*4      ::	ithphi           !Hadronic direction index
							!(below not used)
		real*4	    ::	fp_threshold !max pixel threshold for trigger
							!(below not used)
		real*4	    ::	i_mult	     !CFD Multiplicity above threshold
		real*4	    ::	fp_mult      !CFD Multiplicity above threshold

	end type m_rec
	
	integer,parameter :: srec_size=mrec_size+4
	type sum_rec
		type(m_rec) :: mrec
		real 	    :: tep
		integer     :: idfile
		real        :: x_offset
		real        :: y_offset
	end type sum_rec

!Whipple Hillas parameters structure
	type hillas
		real ::	 dist	!Hillas parameters
		real ::	 length
		real ::	 width		
		real ::	 miss
		real ::	 azwidth
		real ::	 frac2
		real ::	 frac3
		real ::  size
		integer	::  maxweight1
		integer	::  maxweight2
		integer	::  maxweight3
		integer	::  iboundry
		integer	::  ibright
		real*8 ::  xmean    !X of Center of Mass
		real*8 ::  ymean    !Y of Center of Mass
		real*8 ::  sdevxy
		real*8 ::  alph
		real*8 ::  asym
		real*8 ::  psi
	end type hillas

!       4/10/04 MPK added xk and yk for 2 d analysis
!       19/04/04 GHS added ra2d and dec2d for 2d analysis
!       02/06/04 GHS change names of clstr_thrsh(3)(clstr4) and 
!                clustr_thrsh(4) (clstr5) to source_ra (src_ra) and source_dec
!                (src_dec)
!	integer,parameter :: h_ntp_size=75
!	integer,parameter :: h_ntp_size=77       !19/04/04 GHS
	integer,parameter :: h_ntp_size=85       !29/03/05 GHS
	type h_ntp
		sequence		!Necessay because we want to put this
					!into a common block for cernlib 
		real :: tep	        !Energy in TeV
		real :: type		!Primary type
		real :: id		!Shower id
		real :: x_offset	!Shower core offset position
		real :: y_offset
		real :: x		!X(e-w) position
		real :: y		!Y(n-s) position
		real :: theta		!Zenith angle of mount
		real :: phi		!Azmutial angle
		real :: azwidth		!Hillas values
		real :: width
		real :: length
		real :: dist
		real :: miss
		real :: alpha	
		real :: size
		real,dimension(16) :: max_p
		real :: bright
		real :: boundry
		real :: w_hits_in
		real :: w_hits_mid
		real :: neighbor
		real :: emission_alt
		real :: em_alt_sig
		real :: muon_ratio
		real :: median_time  	!Median time all pes in area
		real :: fp_median_time 	!Median time of all pes
		real :: time_cfd 	!CFD trigger time of Isyn +Noise
		real :: fp_time_cfd 	!CFD trigger time of Asyn +Noise
		real :: ithphi
		real :: fp_threshold
		real :: i_mult
		real :: fp_mult
		real :: xmean		!X of Center of mass of Image
		real :: ymean		!Y of Center of mass of Image
		real :: alph		!Varible used to get slope.
		real :: sdevxy		!Varible used to get slope.
		real :: asym
		real :: psi
		real,dimension(4) :: max_pe
		real,dimension(4) :: max_pe_in
		real,dimension(2) :: clstr_thrsh
                real :: source_ra       !for 2d analysis(drift scan especially)
                real :: source_dec
		real,dimension(4) :: in_clstr_thrsh
		real :: xmpln		!x and y of point where primary track 
		real :: ympln		!crosses mirror plane	
		real :: emaltmpln	!Distance along pramary track from
					!emission alitiude to mirror plane.
                real :: xo(2)           !Lessard 2d x positions
                real :: yo(2)           !Lessard 2d y positions
                real :: xk(2)           !MPK 2d x positions
                real :: yk(2)           !MPK 2d y positions
                real :: ra2d(2)         !GHS 2d ra positions From xo,yo
                real :: dec2d(2)        !GHS 2d dec positions From xo,yo
	end type h_ntp
				!Set up a label array for the Ntupple variables.
	character(len=8),dimension(h_ntp_size) :: tags_h_ntp =(/ &
&'tep     ','type    ','id      ','x_off   ','y_off   ','x       ','y       ',&
&'theta   ','phi     ','azw     ','w       ','l       ','d       ','m       ',&
&'alpha   ','size    ',                       				      &
&'max1    ','max2    ','max3    ','max4    ','max5    ','max6    ','max7    ',&
&'max8    ','maxm1   ','maxm2   ','maxm3   ','maxm4   ','maxm5   ','maxm6   ',&
&'maxm7   ','maxm8   ',	                                                      &
&'bright  ','boundry ','hits_in ','hits_mid','neighbor','em_alt  ',           &
&'em_sig  ','muratio ','med_time','a_median','i_cfd   ','a_cfd   ','ithphi  ',&
&'a_thresh','i_mult  ','a_mult  ','xmean   ','ymean   ','alph    ','sdevxy  ',&
&'asym    ','psi     ',                                                       &
&'maxpet2 ','maxpet3 ','maxpet4 ','maxpet5 ',                                 &
&'max_int2','max_int3','max_int4','max_int5',                                 &
&'clstr2  ','clstr3  ','src_ra  ','src_dec ',                                 &
&'inclstr2','inclstr3','inclstr4','inclstr5',				      &
&'xm      ','ym      ','emaltm  ','xo1     ','xo2     ','yo1     ','yo2     ',&
&'xk1     ','xk2     ','yk1     ','yk2     ','ra2d1   ','ra2d2   ','dec2d1  ',&
&'dec2d2  '/)

END MODULE STRUCTURES

MODULE RECORD_DEFS

	use structures
	type(m_rec) mrec
	type(m_head) mhead

END MODULE RECORD_DEFS


