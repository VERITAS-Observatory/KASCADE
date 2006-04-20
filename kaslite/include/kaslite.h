c        Common block for the input data file.
c	Modified:
C	Common structures.

c	This is file of structures for the KASACADE system.

c	Modified:
c	09/8/96 GHS
c		Add to HILLAS and H_NTP: XMEAN and YMEAN. Used by
c		KAS_ARRAY_TRIGGER for parallax calculations.Also add
c		DIST which is used as a flag to indicate if we had enough
c		pixels after cleanup to determine image parameters.
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


	record /segment/segment
	RECORD /PE_REC/PE
        integer pe_next
	common /perec/pe,segment,pe_next

        COMMON /HALE1/DL,DM,DN,xseg,yseg,reflectivity,x_offset,y_offset,
     1                namtyp, plot, luny_cut, benchmark_flag, VeritasPMTs,
     1                WhipplePMTs, ADPPMTs, petype
        real dl,dm,dn,xseg,yseg,reflectivity,x_offset,y_offset
        character*8 namtyp(18)
        logical plot,luny_cut
        logical*1 benchmark_flag,VeritasPMTs,WhipplePMTs,ADPPMTs

	character*16 petype
c        Inext counts number of segments in input file

        COMMON/HALE2/DNI,hobs,photons,tzcobs,ftzcobs
	real dni,hobs,photons(8,10000),tzcobs,ftzcobs

	common/lost/mountlost,uplost
	real*8 mountlost
	real uplost

c	25/10/93 G.H.S. V:1:0:1.4
c	Moving rate calculation to KASLITE from KASCADE. Add 10m arrays.
	common/rate/qeta(0:104,0:50),qgamma(0:104,0:50),gamvis(0:50),
	1 gammin(0:104,0:50),qeta_hobs(0:104),qgamma_hobs(0:104),
	2 setavis(0:50),smaxsb(0:104,0:50),atmprob(0:104,0:50),i380,ihobs,
	3 qetavis(0:50),qgamvis(0:50),qetavis_hobs,qgamvis_hobs,	!Visable
	4 qetaold(0:50),qgamold(0:50),qetaold_hobs,qgamold_hobs,	!Old 
	5 qetaceas(0:50),qgamceas(0:50),qetaceas_hobs,qgamceas_hobs,	!CEAS
	6 qetaasgat(0:50),qgamasgat(0:50),qetaasgat_hobs,qgamasgat_hobs,!ASGAT
	7 qetasb(0:104,0:50),qgamsb(0:104,0:50),qetasb_hobs(0:104),	!SB
	8 qgamsb_hobs(0:104),
	9 qeta10m(0:104,0:50),qgam10m(0:104,0:50),qeta10m_hobs(0:104),	!10m
	1 qgam10m_hobs(0:104),
	2 qetasl(0:104,0:50),qgamsl(0:104,0:50),qetasl_hobs(0:104),	!SL
	3 qgamsl_hobs(0:104),
	2 rhoratio(0:50),Etasea(0:104),lambda

	real lambda,rhoratio,Etasea
        integer i380,ihobs

	  real qeta,qgamma,gamvis,
	1 gammin,qeta_hobs,qgamma_hobs
	  real setavis,smaxsb,atmprob
	  real qetavis,qgamvis,qetavis_hobs,qgamvis_hobs	     !Visable
	  real qetaold,qgamold,qetaold_hobs,qgamold_hobs             !Old 
	  real qetaceas,qgamceas,qetaceas_hobs,qgamceas_hobs  	     !CEAS
	  real qetaasgat,qgamasgat,qetaasgat_hobs,qgamasgat_hobs     !ASGAT
	  real qetasb,qgamsb,qetasb_hobs,	                     !SB
	8 qgamsb_hobs
	  real  qeta10m,qgam10m,qeta10m_hobs,	                     !10m
	1 qgam10m_hobs
	  real  qetasl,qgamsl,qetasl_hobs,	                     !SL
	3 qgamsl_hobs



      real c_light,pi,twopi,sea_level,eta380,eta210

      parameter  (c_light=2.99792e+8/1.e9)     !Speed of light in vacume.
c                                                M/nsec
      parameter  (pi=3.141592654)
      parameter (twopi=3.141592654*2)
      parameter (sea_level=1033.)       !Sea level in gm/cm**2
      parameter (eta380=.0002829)       !Eta(n-1) at sea level at 380 nanometer
      parameter (eta210=.0003188)       !Eta(n-1) at sea level at 210 nanometer


