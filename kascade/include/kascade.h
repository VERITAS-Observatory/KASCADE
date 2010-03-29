!modified:
	!Extend size of debena to 20(from 18).Using debena(20) for all heavies.
      COMMON /ZENITH/zdni,dni
	real*4 dni		!Z Direction cosign of initial particle
	real*8 zdni		!r*8 inital primary direction for timing
				!used in PROPAGATE.

      common /THR/ Thresh(18)            !thresholds
	real*4 thresh

      common /LIFTIM/ TLife(18)            !Lifetimes
	real*4 tlife

      common /MASS/ XM(18),icharge(18)      !Masses and charges of stable
	real*4 xm
	integer*4 icharge
                                    !particles.

      common /user/temid,hobs,tmax,shower_max
	real*4 temid,hobs,tmax
	integer*4 shower_max(8000)


      common/usimda/b_field,bx,by,bz,e_mass
	real*4 b_field,bx,by,bz,e_mass

      common/usiminp/size,magnet_on,ionization_on,multscattering_on,gndprt
	real*4 size
        logical magnet_on,ionization_on,multscattering_on,gndprt

      common/kontrol/ipr(20),inext,debena,namtyp,funcenable
	integer*4 ipr,inext
        character*8 namtyp(18)
        logical debena(20)
        logical funcenable(3)

      common/stor/den(9,1000),dgn(9,10000),nen,ngn
	real*4 den,dgn
	integer*4 nen,ngn

	real*4 refr0,sea_level,z_air,rad_len,cq,c_light,pi,twopi
      parameter  (refr0=1.00027)          !Refraction index of air at STP
      parameter  (sea_level=1033.)        !Depth of sea level in gm/cm**2
      parameter  (z_air=7.37)             !Atomic number of air.
      parameter  (rad_len=36.6)           !Radiation length in air.Tsai 1974.
      parameter  (cq=0.02629)             !Use in compton scattering calc.
      parameter  (c_light=2.99792e+8)     !Speed of light in vacume.

      parameter  (pi=3.141592654)	
      parameter  (twopi=6.28318531)


	common/debug/debugr,dvissum,dsbsum,doldsum,dcsum,dasum,
	1 shower_dist,shower_trans
	integer*4 shower_dist(300),shower_trans(300)
	real*4 debugr,dvissum,dsbsum,doldsum,dcsum,dasum 

c	This is file of structures for the KASACADE system.
c	No RECORD commands in this file. Put them in parent include
c	file(like in kaslite.inc).

c	Modified:
c	05/3/96 GHS
c		Remove adc array from m541.  Add a sum541 record structure.
c		Extend h_ntp max array to 16(for use with 541 camera)
c	23/5/96 GHS
c		Add H_NTP.INTEL for intelligent trigger flag.
c	10/6/96 GHS
c		Combine production and DEV version for VERITAS run at DePauw
c		Foolowing changes are updates from [.dev] version
c		1:09/10/95 GHS	Bring up to v:1:0:3:1:2
c			Make changes tp PE strucutrue(add .em_alt).
c 			Make changes to M109 Structure(add .emission_alt,
c			.em_alt_sig,.muon_ratio)
c		2:18/10/95 GHS	Add record definition for Argentine SeachLight
c			M file.	Fix typo m109_size(was incorrectly n109_size).
c	11/6/96 GHS 
c		For 541:Make changes to M541 Structure(add .emission_alt,
c			.em_alt_sig,.muon_ratio)
c		
c	24/6/96 GHS
c		Add M541.MEDIAN_TIME
c		Change order of variables in M541 so that the disc array
c		comes last. This is needed for m-file zero supression
c		compression to work.

c	01/7/96 GHS
c		Add to H_NTP: h_ntp.emission_alt,h_ntp.em_alt_sig,
c		h_ntp.muon_ratio,h_ntp.median_time

c	09/8/96 GHS
c		Add to HILLAS and H_NTP: XMEAN and YMEAN. Used by
c		KAS_ARRAY_TRIGGER for parallax calculations.Also add
c		DIST which is used as a flag to indicate if we had enough
c		pixels after cleanup to determine image parameters.

c	16/1/97 GHS
c		Convert KASCADE to structures. Define a SEGMENT_HEAD and
c		a SEGMENT structures.
c		Replace shower definition stuff in PE_HEAD with inclusion
c		of SEGMENT_HEAD.
c		Add PE_HEAD.VERSION to PE_HEAD structure.
c	06/6/97 GHS
c		Fix m_10m_size and M37_size declarations.
c	02/6/97 GHS
c		Add M331,M151,M271,SUM331,SUM271,SUM151,CM109,CMSUM109
c		structures.
c	04/9/97 GHS
c		Add to H_NTP the maximum pe_threshold(effective)that would have
c		triggered this event under the HMULT/HMULT-1 trigger conditions
c		for the 541 camera and under the inner only trigger conditions.

c	17/10/97 GHS
c		Add to H_NTP the hillas image parameters ALPH(not alpha) and
c		SDEVXY for use in 2-D analysis for KAS_ARRAY_TRIGGER

c	11/12/97 GHS
c		Add following variables to m331:
c		m331.time_peak,	m331.time_mean,	m331.time_cfd, m331.time_max 
c		m331.fp_median_time, m331.fp_time_peak, m331.fp_time_mean,
c		m331.fp_time_cfd, m331.fp_time_max
c		This is for use for energy determination.
c	16/12/97 GHS
c		Add following variables to h_ntp:
c		time_peak,time_mean,time_cfd,time_max,fp_median_time,
c		fp_time_peak, fp_time_mean,fp_time_cfd,fp_time_max

c	16/12/97 GHS
c		Add X_OFFSET and Y_OFFSET to PE_HEAD.And to SUM541,and to H_NTP

c	22/12/97 GHS
c		Add to m331:
c		m331.n_time_peak,m331.n_time_mean,m331.n_time_cfd
c		m331.nfp_time_peak,m331.nfp_time_mean,m331.nfp_time_cfd
c		Add to h_ntp:
c		n_time_peak,n_time_mean,n_time_cfd
c		nfp_time_peak,nfp_time_mean,nfp_time_cfd

c	26/12/97 GHS
c		Add to m331:
c		m331.asyn_threshold,m331.isyn_threshold
c		m331.asyn_mult,m331.isyn_mult
c	29/12/97 GHS
c		Add to h_ntp:
c		asyn_threshold,isyn_threshold
c		asyn_mult,isyn_mult

!	26/6/98 GHS
!		Modified to match structures.f90 and veritas stuff.

!	10/7/98 GHS
!		Update hntp to match STRUCTURES.F90

c ** Segment Header Structure **
	integer segment_head_size
	parameter (segment_head_size=13)	!Length in longwords of header.
	type segment_HEAD_dev
	    sequence
                INTEGER*4   ITYPE	! Primary particle type.
		REAL*4	    TEP		! Primary particle energy(TeV.)
		REAL*4	    DLI		! Primary particle direction(can 
					! recreate dn)
		REAL*4	    DMI		! Primary particle direction(can 
					! recreate dn)
		character*4 MAGNET_field! Earths magnetic field specification
					!F=false =none. !W=whipple, Tucson Ariz
		REAL*4	    ETR		! Threshold level for gammas,e and mu.
		REAL*4	    DEPTH	! Injection depth in gm/cm**2
		REAL*4	    zOBS	! Observation altitude in meters.
		REAL*4	    Xinitial	! Origen of shower.
		REAL*4	    Yinitial	!
		integer*4   idfile	! File id # for this shower.
		character*8 version	!Version of kascade that made this file.
       end type segment_HEAD_dev 

c ** Segment Record Structure **
	integer segment_size
	parameter (segment_size=11)	!Length in longwords of record.
	type segment_dev
	    sequence
		real*4 xstart        !Initial xy,z of segment.
		real*4 ystart
		real*4 hstart
		real*4 dlstart       !Initial x,y direction cosigns of segment
		real*4 dmstart
		real*4 tend          !relative time at end of segment.
		real*4 hend          !final altitude of segment
		real*4 dlend         !final direction cosigns of segment.
		real*4 dmend
		real*4 gamma         !gamma at middle of segment.
		integer*4 nspec      !particle type
         end  type segment_dev
                          


!	RECORD/segment_head_dev/segment_head	   	
!	RECORD/segment_dev/segment	   	
        type(segment_HEAD_dev)segment_head
        type(segment_dev)segment
        common/segments/segment_head,segment

!******************************************************************************
!      Heavy Nuclei declarations block:
!******************************************************************************
	character*15 nuclei_names(113)
			 !see below for nuclei_names data statement


	data nuclei_names/'Hydrogen', 'Helium', 'Lithium', 
	1'Beryllium',
	1'Boron', 'Carbon','Nitrogen', 'Oxygen', 'Fluorine', 'Neon', 
	1'Sodium', 'Magnesium', 'Aluminum', 'Silicon','Phosphorous', 
	1'Sulfur','Chlorine', 'Argon', 'Potassium', 'Calcium', 
	1'Scandium', 'Titanium','Vanadium', 'Chromium', 'Manganese', 
	1'Iron', 'Cobalt', 'Nickel', 'Copper', 'Zinc', 'Gallium',
	1'Germanium', 'Arsenic', 'Selenium', 'Bromine', 'Krypton', 
	1'Rubidium', 'Strontium', 'Yttrium', 'Zirconium', 'Niobium', 
	1'Molybdenum', 'Technetium', 'Ruthenium', 'Rhodium', 
	1'Palladium','Silver', 'Cadmium', 'Indium', 'Tin', 
	1'Antimony', 'Tellurium','Iodine', 'Xenon', 'Cesium',
	1'Barium', 'Lanthanum', 'Cerium', 'Praseodymium', 
	1'Neodymium', 'Promethium', 'Samarium','Europium', 
	1'Gadolinium','Terbium', 'Dysprosium', 'Holmium', 'Erbium', 
	1'Thulium', 'Ytterbium','Lutetium', 'Hafnium', 'Tantalum', 
	1'Tungsten', 'Rhenium', 'Osmium', 'Iridium','Platinum', 
	1'Gold', 'Mercury', 'Thallium', 'Lead', 'Bismuth', 
	1'Polonium', 'Astatine','Radon', 'Francium', 'Radium', 
	1'Actinium', 'Thorium', 'Proactinium', 'Uranium', 
	1'Neptunium', 'Plutonium','Americium', 'Curium', 
	1'Berkelium', 'Californium', 'Einsteinium','Fermium', 
	1'Mendelevium', 'Nobelium', 'Lawrencium', 'Rutherfordium', 
	1'Dubnium', 'Seaborgium','Bohrium', 'Hassium', 
	1'Meitnerium', 'Unununium', 'Ununbium', 'Ununtrium', 
	1'Ununquadium'/



