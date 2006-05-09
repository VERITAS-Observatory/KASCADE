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

      common/usiminp/size,magnet_on,gndprt
	real*4 size
	logical magnet_on,gndprt

      common/kontrol/ipr(20),inext,debena,namtyp
	integer*4 ipr,inext
      character*8 namtyp(18)
      logical debena(20)

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
	STRUCTURE /segment_HEAD/
	    UNION
	    MAP
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
	    ENDMAP
	    MAP
		real*4   recbuf(segment_head_size)
	    endmap
	    END UNION
	END STRUCTURE

c ** Segment Record Structure **
	integer segment_size
	parameter (segment_size=11)	!Length in longwords of record.
	STRUCTURE /segment/
	    UNION
	    MAP
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
	    ENDMAP
	    MAP
		real*4   recbuf(segment_size)
	    endmap
	    END UNION
	END STRUCTURE

                          
C ** PE HEADER STRUCTURE
c	Modified 16/1/97 GHS
	integer pehead_size           !Length in longwords of header.
	parameter (pehead_size=segment_head_size+12)
	STRUCTURE /PE_HEAD/
	    UNION
	    MAP
		record /segment_head/segment_head
		REAL*4	    DL		! Mount direction(can recreate dnm)
		REAL*4	    DM		! Mount direction(can recreate dnm)
		REAL*4	    XSEG	! Grid width in x direction.
		REAL*4	    YSEG	! Grid width in y direction.
		REAL*4	    HOBS	! Observation altitude in meters.
		real*4	    X_OFFSET    !X,Y coord of core in area 0,0
		real*4	    Y_OFFSET	
		CHARACTER*4 PETYPE	! Character: 'VI'=visable photons,
					! 'SB'=solar blind etc.,'VT' Veritas 
					!triangular array.
		character*16 version	!Version of kaslite that made this file.
	    ENDMAP
	    MAP
		real*4   recbuf(pehead_size)
	    endmap
	    END UNION
	END STRUCTURE

C	PE structure(new)
	integer pe_size
	parameter (pe_size=9)	!Length of pe_rec in longwords(r*4)
	STRUCTURE /PE_REC/
	    UNION
	    MAP			    ! this is the original nomenclature
		INTEGER*2	NX	!Grid coords (x,y)
		integer*2	ny
		REAL*4		time	! Time of hit
		REAL*4		dlr	! Dl of photon relative to mount
		REAL*4		dmr	! dm of photon(dn can be recreated)
		INTEGER*4	NNEXT	! Segment identifier.
		REAL*4		XM	!X position of hit in grid rectangle.
		REAL*4		ym	!Y position of hit in grid rectangle.
		integer*2	spec	!Type of emitting particle.
		integer*2	lambda	!Wavelength of emmited photon(nm)
		real*4		em_alt	!emmison altitude
	    ENDMAP
	    MAP
		REAL*4		recbuf(pe_size)	!So pe.recbuf replaces RECBUF
	    ENDMAP
	    END UNION
	END STRUCTURE


c	M file Head structure GHS 29/10/93
c	Modified:
c	24/5/94 GHS.  Added phi_steps
	integer mhead_size
	parameter(mhead_size=pehead_size+20)
	STRUCTURE /M_HEAD/
	    UNION
	    MAP
		record /pe_head/pe_head
		logical*4	noise		!Noise used in KASTRIG?
		real*4		disc_width	!Size of discriminater 
						!coincidence window.
		real*4		rate_noise	!Noise rate in pe/deg**2/ns.
		real*4		reflectivity	!Mirror reflectivity as 
						!fraction of best possible
		real*4		full_aperture	!Full aperture of tubes in deg.
		real*4		pesum		!Disc threshold for pmt's
		real*4		step_size	!Step size in theta direction
		integer		itheta_max	!Maximum number of steps
		integer		phi_steps	!Base number of phi steps for an
						!Itheta ring.total number=
						!(itheta-1)*phi_steps
		character*16	version		!Version of kastrig that
						!made this m file.
		real*4		adc_gate	!Size of adc gate window in ns.
		real*4		hmult		!Trigger multiplicity.
		real*4		concentration
		character*16	options		!Run options.
	    ENDMAP
	    MAP
		real*4   recbuf(mhead_size)
	    endmap
	    END UNION
	END STRUCTURE



c	Make structure for Hillas ntuple(must be all real!)
	integer h_ntp_size
	parameter (h_ntp_size=73)
	structure/H_ntp/
		union
			map
				real tep	        !Energy in TeV
				real type		!Primary type
				real id		!Shower id
				real x_offset	!Shower core offset position
				real y_offset
				real x		!X(e-w) position
				real y		!Y(n-s) position
				real theta		!Zenith angle of mount
				real phi		!Azmutial angle
				real azwidth		!Hillas values
				real width
				real length
				real dist
				real miss
				real alpha	
				real size
				real max_p(16)
				real bright
				real boundry
				real w_hits_in
				real w_hits_mid
				real neighbor
				real emission_alt
				real em_alt_sig
				real muon_ratio
				real median_time    !Median time all pes in area
				real fp_median_time !Median time of all pes
				real time_cfd 	!CFD trigger time of Isyn +Noise
				real fp_time_cfd!CFD trigger time of Asyn +Noise
				real i_threshold
				real fp_threshold
				real i_mult
				real fp_mult
				real xmean	!X of Center of mass of Image
				real ymean	!Y of Center of mass of Image
				real alph	!Varible used to get slope.
				real sdevxy	!Varible used to get slope.
				real asym
				real psi
				real max_pe(4)
				real max_pe_in(4)
				real clstr_thrsh(4)
				real in_clstr_thrsh(4)
				real xmpln 	!x and y of point where primary
				real ympln	!track crosses mirror plane	
				real emaltmpln	!Distance along pramary track 
						!from emission alitiude to 
						!mirror plane.
			endmap
			map
				real recbuf(H_ntp_size)
			endmap
		endunion
	endstructure
	character*8 tags_h_ntp(h_ntp_size)	!Names of the pes variables
				!Set up a label array for the Ntupple variables.



!VERITAS
c	VERITAS Trigger ntupple record structure.
c	VERITAS Trigger ntupple record structure.
c	Make structure for VERITAS ntuple(must be all real since its a CWN!)
	integer v_ntp_size,v_size
!	parameter(v_size=7)		!Full VERITAS ARRAY
!	parameter(v_size=4)		!VERITAS SUB-ARRAY
!	parameter(v_size=3)		!VERITAS SUB-ARRAY
	parameter(v_size=2)		!VERITAS SUB-ARRAY


	parameter(v_ntp_size=10+v_size+4+v_size+4+8*v_size)
	structure/v_ntp/
		union
			map
				real*4		x
				real*4		y
				real*4		theta
				real*4		phi
				real*4		tep
				real*4		file_id
				real*4		hits_center
				real*4		hits_outer
				real		x_offset
				real		y_offset

				real*4		hit_pat(v_size)
				real		xcore
				real		ycore
				real		parallaxwidth
				real		iv_core

				real*4		hit_pat_2d(v_size)
				real		xsource
				real		ysource
				real		sourcewidth
				real		iv_source

			        real		hit_trig(v_size)
				real		dist(v_size)
				real		size(v_size)
				real		xmean(v_size)
				real		ymean(v_size)
				real		xmpln(v_size)
				real		ympln(v_size)
				real		emaltmpln(v_size)

			endmap
			map
				real*4 recbuf(v_ntp_size)
			endmap
		endunion
	endstructure
	character*8 tags_v_ntp(v_ntp_size) !Names of the array trigger variables
				!Set up a label array for the Ntupple variables.
c	Make structure for VERITAS ntuple(must be all real since its a CWN!)

c	VERITAS Array  file record structure.
c	Note the need for a pulse height for each mirror is for use by 
c	KAS_ARRAY_TRIGGER. Its used as a flag to indicate when a mirror has
c	been tested in a particular position in the array.
c	Note PHEIGHT must follow IPHI so that structureas M and M_V are
c	equivalent up to that point!
	integer mv_size
	parameter(mv_size=4+v_size+11)
	structure/m_v/
		union
			map
				integer		nx
				integer		ny
				integer		itheta
				integer		iphi
				real*4		pheight(v_size)
				real		xmean
				real		ymean
				real		dist
				real		alph
				real		sdevxy
				real		size
				real		xmpln
				real		ympln
				real		emaltmpln
				real		x_offset
				real		y_offset
			endmap
			map
				real*4 recbuf(mv_size)
			endmap
		endunion
	endstructure

c	Generic M file record structure used by KAS_ARRAY_TRIGGER
	integer m_size,mr_size
	parameter (m_size=mv_size)
	parameter (mr_size=m_size-4)
	structure/m/
		union
			map
				integer		nx
				integer		ny
				integer		itheta
				integer		iphi
				real*4		pheight(mr_size)
			endmap
			map
				real*4 recbuf(m_size)
			endmap
		endunion
	endstructure

	RECORD/segment_head/segment_head	   	
	RECORD/segment/segment	   	
	common/segments/segment_head,segment

!******************************************************************************
!      Heavy Nuclei declarations block:
!******************************************************************************
	character*15 nuclei_names(113)
			 !see below for nuclei_names data statement


!DATA STatements Follow.
!Must follow all declaration statements(in Profortran)

	data tags_h_ntp/
	1'tep','type','id','x_off','y_off','x','y',
	1'theta','phi','azw','w','l','d','m',
	1'alpha','size',				
	1'max1','max2','max3','max4','max5','max6','max7',
	1'max8','maxm1','maxm2','maxm3','maxm4','maxm5','maxm6',
	1'maxm7','maxm8',	
	1'bright','boundry','hits_in','hits_mid','neighbor','em_alt',
	1'em_sig','muratio','med_time','a_median','i_cfd','a_cfd',
	1'i_thresh','a_thresh','i_mult','a_mult','xmean','ymean',
	1'alph','sdevxy','asym','psi',
	1'maxpet2','maxpet3','maxpet4','maxpet5',
	1'max_int2','max_int3','max_int4','max_int5',
	1'clstr2','clstr3','clstr4','clstr5',
	1'inclstr2','inclstr3','inclstr4','inclstr5',
	1'xm','ym','emaltm'/

!v_size=7
!	data tags_v_ntp/'x','y','theta','phi','tep','id','hits_in',
!	1 'hits_out','x_off','y_off',
!	1 'phit1','phit2','phit3','phit4','phit5','phit6','phit7',
!	1 'xcore','ycore','pwidth','iv_core',
!	2 'd2_hit1','d2_hit2','d2_hit3','d2_hit4','d2_hit5','d2_hit6',
!	2 'd2_hit7',
!	2 'xsource','ysource','swidth','iv_source',
!	3 'hit1','hit2','hit3','hit4','hit5','hit6','hit7',
!	3 'd1','d2','d3','d4','d5','d6','d7',
!	3 'size1','size2','size3','size4','size5','size6','size7',
!	3 'xmn1','xmn2','xmn3','xmn4','xmn5','xmn6','xmn7',
!	3 'ymn1','ymn2','ymn3','ymn4','ymn5','ymn6','ymn7',
!	3 'xmp1','xmp2','xmp3','xmp4','xmp5','xmp6','xmp7',
!	3 'ymp1','ymp2','ymp3','ymp4','ymp5','ymp6','ymp7',
!	3 'emalm1','emalm2','emalm3','emalm4','emalm5','emalm6','emalm7'/
!
!v_size=4
!	data tags_v_ntp/'x','y','theta','phi','tep','id','hits_in',
!	1 'hits_out','x_off','y_off',
!	1 'phit1','phit2','phit3','phit4',
!	1 'xcore','ycore','pwidth','iv_core',
!	2 'd2_hit1','d2_hit2','d2_hit3','d2_hit4',
!	2 'xsource','ysource','swidth','iv_source',
!	3 'hit1','hit2','hit3','hit4',
!	3 'd1','d2','d3','d4',
!	3 'size1','size2','size3','size4',
!	3 'xmn1','xmn2','xmn3','xmn4',
!	3 'ymn1','ymn2','ymn3','ymn4',
!	3 'xmp1','xmp2','xmp3','xmp4',
!	3 'ymp1','ymp2','ymp3','ymp4',
!	3 'emalm1','emalm2','emalm3','emalm4'/

!v_size=3
!	data tags_v_ntp/'x','y','theta','phi','tep','id','hits_in',
!	1 'hits_out','x_off','y_off',
!	1 'phit1','phit2','phit3',
!	1 'xcore','ycore','pwidth','iv_core',
!	2 'd2_hit1','d2_hit2','d2_hit3',
!	2 'xsource','ysource','swidth','iv_source',
!	3 'hit1','hit2','hit3',
!	3 'd1','d2','d3',
!	3 'size1','size2','size3',
!	3 'xmn1','xmn2','xmn3',
!	3 'ymn1','ymn2','ymn3',
!	3 'xmp1','xmp2','xmp3',
!	3 'ymp1','ymp2','ymp3',
!	3 'emalm1','emalm2','emalm3'/

!v_size=2
	data tags_v_ntp/'x','y','theta','phi','tep','id','hits_in',
	1 'hits_out','x_off','y_off',
	1 'phit1','phit2',
	1 'xcore','ycore','pwidth','iv_core',
	2 'd2_hit1','d2_hit2',
	2 'xsource','ysource','swidth','iv_source',
	3 'hit1','hit2',
	3 'd1','d2',
	3 'size1','size2',
	3 'xmn1','xmn2',
	3 'ymn1','ymn2',
	3 'xmp1','xmp2',
	3 'ymp1','ymp2',
	3 'emalm1','emalm2'/
 
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



