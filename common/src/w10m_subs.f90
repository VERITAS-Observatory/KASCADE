MODULE W10M_SUBS
!	real,parameter :: pst_pulse_width=18.0 !ECL Pulses to PST's from CFD's
!					       !are 12.5 ns long with a tail
!					       !out to 28.6 ns. If we look for
!					       !hafway along the tail minus a 
!					       !little we get 18 ns.
	real,parameter :: pst_pulse_width=10.0 !ECL Pulses to PST's from CFD's
                                               !As measured by GHS in 09/01
CONTAINS
  SUBROUTINE SINGLE_PE_HEIGHT(height,after_pulse,single_pe_sigma)
!	Models Chucks measurment of single pe pulse height distribution	for
!	a r1398 pmt.
!	Mean of distribution is set to 1.0 (chucks channel 139)
!	The distribution is modeled in two parts. 
!	Area 1:Below height=.68 (channel 95) down to .158 its constant.0 below
!	       .158 (channel 22)
!	Area 2:Above .68 its a gaussian, with a mean of 1.09(channel 152) and
!		a sigma of .2865 (40 channels)
!	Area 1 is .2865 of the total area under the curve. Throw over that
!		to pick our areas.
!	Written by:	
!		Glenn Sembroski
!		Purdue Univ.

!	Modified:

!	28/9/98 GHS
!		Add a gaussian for after pulsing.
!		Choose some percent of the time (AFTER_PULSE_FRAC) to geneate
!		our pulse height value from the afterpulse spectrum. Assume
!		this spectrum is gaussian with mean AFTER_PULSE_MEAN and width
!		AFTER_PULSE_SIGMA.

!	25/2/99 GHS
!		Add this after pulseing version to w10m_pepulse file. Add a
!		flag, AFTER_PULSE,  to indicate whether to add after pulseing
!		or not. Use this to turn off afterpulses when we have true
!		shower pe's. Afterpulses only show up in noise!

!       22/03/01 GHS
!               Change shape of single pe pulse height distribution as a test.
!               I want longer tails so double the gaussian width. Also, get 
!               rid of threshold stuff at front to distribution. Let gaussian 
!               extend tail down to .0
!     
!       31/03/04 GHS
!               Use shape as defined for 22/03/01 but add input parameter of
!               single_pe_sigma so that each PMT can have a different pe 
!               distribution (set at call time). default to local value
	IMPLICIT NONE

	logical :: after_pulse
	real :: single_pe_sigma
        real :: sigma
        real :: height
	real :: y
	real :: xdummy
	real :: gauss,pran
	real,parameter :: single_pe_edge=.1   !Minimum acceptable pulse height.
	real,parameter :: single_pe_gauss_mean=1.0   	!gaussian mean
        real,parameter :: single_pe_gauss_sigma=.275   !Default gaussian width
                                                       !(if single_pe_sigma=0)
	real,parameter :: after_pulse_frac=3.e-4  !Fraction of afterpulses
	real,parameter :: after_pulse_mean=2      !Mean of afterpulses
	real,parameter :: after_pulse_sigma=6     !Width of afterpulse spectrum
	
!	see if this is an after pulse.
	if(after_pulse) then
	   y=pran(xdummy)
	   if(y<after_pulse_frac)then
		do
			height=gauss(xdummy)*after_pulse_sigma   &
				& + after_pulse_mean
			if(height>single_pe_edge)then
				return
			endif
		enddo
	   endif
	endif

!	If we are not to make afterpulses or if this pulse wasn't chosen as
!	and afterpulse, pick from standard distribution.
!       Default sigma: If incoming sigma given as 0:
        if(single_pe_sigma.eq.0)then
           sigma=single_pe_gauss_sigma
        else
           sigma=single_pe_sigma
        endif

!       Pick from a gaussian with mean=0 and sigma=1
!       modify result to mean=single_pe_gauss_mean 
!       if result is below single_pe_edge try again.
		do

			height=gauss(xdummy)*sigma + single_pe_gauss_mean
			if(height>single_pe_edge)then
				return
			endif
		enddo
!	endif
   END SUBROUTINE SINGLE_PE_HEIGHT
!****************************************************************************

     SUBROUTINE W10M_ARRAY_ORDER(array,index)
!****************************************************************************
!	Orders by size ARRAY values. Used to get max1,max2,max3 etc.
!	Retruns INDEX which indicates size order in ARRAY.
!	Notes this ends up with index having pointers from lowest to hightest
!	INDEX(1) will point to lowest value in ARRAY. INDEX(size(array)) will
!	point to the highest.
!****************************************************************************

	IMPLICIT NONE
	real,dimension(:),pointer :: array
	real,dimension(:),pointer :: arrays
	real :: temp_array
	integer,dimension(:) :: index
	integer :: npmt,temp_index,i,k,m,error

	npmt=size(array)
	deallocate(arrays,stat=error)
        allocate(arrays(npmt),stat=error)
	if(error/= 0)then
	    	print*,"Program could not allocte space for array buffer."
	    	stop 'Failure to allocate space for arrays'
	endif

!Save a copy
	arrays=array
!Init index array
	do i=1,npmt
		index(i)=i
	enddo

!do the sort.
	do i=2,npmt
		do k=1,i-1
			if(arrays(i)<arrays(k))then
					!Swap them
				temp_array=arrays(k)
				arrays(k)=arrays(i)
				arrays(i)=temp_array
				temp_index=index(k)
				index(k)=index(i)
				index(i)=temp_index
			endif
		enddo
	enddo	!End of sort
	deallocate(arrays,stat=error)
	return
    END SUBROUTINE W10M_ARRAY_ORDER
!****************************************************************************

   SUBROUTINE W10M_COMPRESS(disc_in,j)
!*****************************************************************************
!	Compress(0 supress) an array into itself.
!*****************************************************************************
!	Resultant array has no stings of 0's.
!	They are replaced by the negative of a single index that gives
!	next non-zero element. Use W10M_UNCOMPRESS to restore.

!	J points to last word used in output array.
!	Flag the end of the compression with a -(size+1) where size is length
!	of original array(unless we fill the disc array)!.

	real,dimension(:),pointer :: disc_in
	integer :: npmts,i,j
	logical :: skip

	npmts=size(disc_in)
!	Start with no skip in progress
	skip=.false.
	j=0

!	Go through the array elements.
	do i=1,npmts
		if(disc_in(i).eq.0)then
			skip=.true.		!Were skipping zero's
		elseif(skip)then
			skip=.false.		!Done skipping.
			j=j+1			!Need to give index
			disc_in(j)=-i		!Flagged as index by neagtive
			j=j+1			!Save non-zero value.
			disc_in(j)=disc_in(i)
		else
			j=j+1			!Save non-zero value.
			disc_in(j)=disc_in(i)
		endif
	enddo
	j=j+1
	if(j.le.npmts)then
		disc_in(j)=-(npmts+1)			!Flag very end.
	else
		j=j-1
	endif
	return
    END SUBROUTINE W10M_COMPRESS
!******************************************************************************(

 
    SUBROUTINE W10M_MOUNT_VECINI(mhead,dl,dm,dn,hadron)
!******************************************************************************
!******************************************************************************
!	All directions of the mount (M) are given in the upward(-Z) direction 
!	convention (ie M dn is neg.). 
!
!       X unit vector is along -RA. That is  X is perendicular to M and 
!       perpendicular to celestial North pole vector P (for polaris)
!       That is X direction is M x P
!        
!       Y unit vector is along  +DEC. That is Y directin is X x M
!
! written: 08/06/04 G.H.S.

!	Modified:
!       08/06/04 GHS 
!               This is a modified version of W10M_MOUNT_VECINI
!       09/06/04 GHS V:1:3:8:3:4.4
!                Rewrite W10M_MOUNT_VEC_INI. Use only iphi=1 and iphi=itheta
!                for gamma_drift. Hadrons same as before. Make x -axis be along
!                -ra for boith gamma_drift and hadrons. Gamma_drift step_size
!                is .4 min along +/- ra (iphi=1,itheta=iphi). Hadrons now also
!                have +x axis along -ra but that really not important for 
!                hadrons. Added a number of routines to do all this. Most in
!                w10m_subs.f90 but some c++ ones in Veritas.cpp. They use the
!                sla libs.
!     20/09/04 GHS V:1:3:8:3:5.0  NEW VERSION
!                Problems with our old scheme of itheta/iphi. Seen in 2d x,y 
!                plots.  We can see effects of phi steps (5 fold symmetries)
!                Going to replace (for hadrons only, leave gammas and 
!                drift scan as is) with choosing random directions (use same 
!                number of directions as with itheta iphi, (106 for 
!                itheta_max=7 iphi_steps=5) but chose directios randomly 
!                within direction circle of radius: 
!                (itheta_max-1)*step_size+step_size/2= (= 6.5 deg for 
!                                             itheta_max=7and stepsize=1.0 deg)
!Define c++ routine names.
!dir$ name(w10mgetradecfromvec="W10mGetRaDecFromVec") 
!dir$ name(w10mgetvecfromradec="W10mGetVecFromRaDec") 

      use wcamera_def
      use structures
      use whipple_telescope
      use kastrg_command_line
      IMPLICIT NONE

      real :: xdummy
      real :: gauss,pran
      real :: dl,dm,dn
      logical :: hadron
      type(m_head) :: mhead

      real*8,parameter ::  pi=3.141592654
	                                            !Polaris. Remeber z is neg
      real :: theta,phi,theta_max
      integer :: nsteps,i,j,ithphi
      real*8 :: ra,dec,ranew
      real*8,dimension(3) :: P,M,Z,X,Y,X0,Y0,M0,D  !Vectors used intenally
      real :: xproj,yproj
      integer :: nsides
!*****************************************************************************
                                 !center (itheta=1,iphi=1,theta=0,phi=0)
                                 !dl,dm,dn Read from input .par file.
      call set_vector(dl,dm,dn,M0)
      call set_vector(1.0,0.0,0.0,X0) 
      call unit_cross_product(X0,M0,Y0)

                               !define polaris(P)
      call set_vector(0.0,cos(WHIPLAT),-sin(WHIPLAT),P)
!*****************************************************************************

!*****************************************************************************
!Hadronic init only
!*****************************************************************************
      if(.not.gamma_drift.and.hadron)then
                                  ! Get theta_max for random hadrons (radians)
         theta_max=(mhead%itheta_max-1)*mhead%step_size+mhead%step_size/2
         theta_max=theta_max*pi/180.    !convert to radians

!*****************************************************************************
!Gamma drift scan init only
!*****************************************************************************
      elseif(gamma_drift)then
           !For a driftscan make up an array of unit vectors pointing in the 
           !RA, (constant)DEC directions the scan will go through. Steps are in
           !driftscan_step_size(nominally .4 min) per itheta.For compatability 
           !with hadron processing we are using gphi_side=2 but will only fill 
           !iphi=1 and iphi=itheta. (+ and - steps in RA but constant DEC)
           !To do this easily were going find the RA and DEC that the original
           !M vector points to, say on Jan 0 2000. This uses the elev,az of the
           !mount. To get new offset mount vectors we get new a new ra value
           !which steps of drift_step (nominally .4 min) in both + and - ra 
           !directions for gtheta_max steps in each direction.
           !Get Ra and Dec for MJD for jan 0 2000 (arbitrary date. and this 
           !mount direction)
         call W10mGetRaDecFromVec(M0,ra, dec)   !This is a c++ routine
                    !fillup the rest of the dlm,dmm,dnm arrays
         nsides=mhead%phi_steps
      endif
!*****************************************************************************

!***************************************************************************
!Iterate over all directions(only 1 step for pure gammas)
!***************************************************************************
      do ithphi=1,ithphi_max

!***************************************************************************
! Hadrons(and not gamma drift): random directions
!***************************************************************************
         if(.not.gamma_drift.and.hadron)then
            phi=2*pi*pran(xdummy)       !Must be in Radians
            theta=theta_max*sqrt(pran(xdummy)) !Random r**2 distribution
            !repoint mount unit vector by theta,phi
            call w10m_vector_redirect(theta, phi, X0,Y0,M0,M)
            stheta(ithphi)=theta  !Save theta and phi in radians
            sphi(ithphi)=phi
!debug
!            print*,stheta(ithphi),sphi(ithphi)
!enddebug
 
!***************************************************************************
           
!***************************************************************************
!Gamma drift scan(and pure gammas)
!***************************************************************************
         else
            if(ithphi.eq.1)then
                              !Do theta=0 special. only phi=0.(itheta=1,iphi=1)
               M=M0                                     !vector arith!
            else
               call get_itheta_iphi(ithphi,nsides,i,j)
               if((j.ne.1).and.(i.ne.j))then
                  cycle
               endif
               if(j==1)then
                  ranew=ra-(i-1)*mhead%step_size !Along positive X (-ra)
               else
                  ranew=ra+(i-1)*mhead%step_size !Along negative X (+ra)
               endif
               call W10mGetVecFromRaDec(ranew,dec,M)!This is a c++ routine
            endif
              
         endif
         call get_vector(M,dlm(ithphi),dmm(ithphi),dnm(ithphi))

!debug
!         print*,dlm(ithphi),dmm(ithphi),dnm(ithphi),stheta(ithphi), &
!              & sphi(ithphi)
!enddebug

        
!Now x,y unit vectors in mirror plane (X along -ra)
         if(M(1)==P(1).and.M(2)==P(2).and.M(3)==P(3))then
            call set_vector(1.0,0.0,0.0,X)
            call get_vector(X,xdlm(ithphi),xdmm(ithphi),xdnm(ithphi))
         else
                    !X=M x P  X = x unit vector;M=mount unit vector 
                    !P = polaris unit vector. Puts X along -RA direction.
            call unit_cross_product(M,P,X)
            call get_vector(X,xdlm(ithphi),xdmm(ithphi),xdnm(ithphi))
                    !	Y unitvector from X x M
            call unit_cross_product(X,M,Y)
            call get_vector(Y,ydlm(ithphi),ydmm(ithphi),ydnm(ithphi))
         endif
      enddo   !End ithphi loop
      return
    END SUBROUTINE W10M_MOUNT_VECINI
!******************************************************************************

    SUBROUTINE GET_VECTOR(X,x1,x2,x3)
!******************************************************************************
!        Copy a vector into components
!******************************************************************************
      real*8,dimension(3) :: X
      real x1,x2,x3
      x1=X(1)
      x2=X(2)
      x3=X(3)
      return
    END SUBROUTINE GET_VECTOR
!******************************************************************************

    SUBROUTINE SET_VECTOR(x1,x2,x3,X)
!******************************************************************************
!        Copy a vector into components
!******************************************************************************
      real*8,dimension(3) :: X
      real x1,x2,x3
      X(1)=x1
      X(2)=x2
      X(3)=x3
      return
    END SUBROUTINE SET_VECTOR
!******************************************************************************

    SUBROUTINE UNIT_CROSS_PRODUCT(X,Y,Z)
!******************************************************************************
!        Produce cross product of X x Y and put into Z.
!        Give Z unit length.
!******************************************************************************
      real*8,dimension(3) :: X,Y,Z

      !Cross product
      Z(1)=X(2)*Y(3)-X(3)*Y(2)
      Z(2)=X(3)*Y(1)-X(1)*Y(3)
      Z(3)=X(1)*Y(2)-X(2)*Y(1)
      
      !Normalize
       Z=Z/sqrt(sum(Z*Z))                             !Vector arith
      return
    END SUBROUTINE UNIT_CROSS_PRODUCT
!******************************************************************************
    SUBROUTINE W10M_VECTOR_REDIRECT(theta,phi,X,Y,Z,R)
!******************************************************************************
!	Returns a unit vector defined by theta and phi(in radians) in coord 
!       sytem with unit vectors defineing  x axis: X,  y axis: Y and z axis: Z
!       This replaces the old W10M_VECTOR_REDIRECT which was relative to 
!       an arbitray x axis. Assume that x,y,z unit vectors are perpendicular.
!       Works for left or right handed system I think
!       Resutlant unit vector in R
!******************************************************************************

!	Written by:

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"
!	07/06/04

!	Modified:

!	This replaces an old version which was an adaptation of the GEOM 
!       subroutine used in KASCADE and other places.
      IMPLICIT NONE
      real :: theta,phi
      real*8,dimension(3) :: X,Y,Z,R
      
!New vector will be: r=sin(theta)*cos(phi)*x+sin(theta)*sin(phi)*y+cos(theta)z

      R=sin(theta)*cos(phi)*X+sin(theta)*sin(phi)*Y+cos(theta)*Z  !Vector arith

      !Normalize

      R=R/sqrt(sum(R*R))                                         !Vector arith
      return
    END SUBROUTINE W10M_VECTOR_REDIRECT
! ***********************************************************************

    SUBROUTINE GET_ITHETA_IPHI(ithphi,nsides,itheta,iphi)
!***************************************************************************
!	For a specific itheta and nsides get corresponding itheta/iphi.
!***************************************************************************
! See note:     20/09/04 GHS V:1:3:8:3:5.0 in KASTRIGGER.F90

!	Modified

!***************************************************************************
!       defn: ithphi=1+((itheta-2)*(itheta-1)/2)*nsides + iphi
!***************************************************************************
      IMPLICIT NONE
      integer :: ithphi,nsides,itheta,iphi
      integer :: nthphi

!Easiest way to do this is to search.
!First  a special case
     if(ithphi.eq.1)then
        itheta=1
        iphi=1
        return
     endif
!useing triangular series formula (1+2+3+...+n)=(n+1)*n/2
     itheta=1
     do 
        itheta=itheta+1

        !nthphi will be base value (or last ithphi for previous itheta)

        nthphi=1+(((itheta-1)*(itheta-2))/2)*nsides
        if(nthphi.ge.ithphi)then
           itheta=itheta-1
           exit
        endif
     enddo

     iphi=ithphi-(((itheta-1)*(itheta-2))/2)*nsides-1

     return
    END SUBROUTINE GET_ITHETA_IPHI


! ***********************************************************************

    SUBROUTINE W10M_TRIGGER(wptr,pptr,ithphi,npmt_mult_trigger,mult_trigger,&
         & pst_trigger)
!***************************************************************************
!	Find which pixels fire and if we have a simple multiplicity trigger.
!***************************************************************************
!	Modified

!	13/4/98 GHS
!		Require real pe hit in any pixel nopt just inner ones.

!	18/5/98 GHS
!		Replace mhead%reflectivity with pptr%efficiency. Better light
!		cone moddel used in WCAMERA_GEN.

!	20/5/98 GHS 
!		Rearrange disc_pes stuff for speed purposes. Reformat way we
!		apply efficiency so that we dont have fractional pes in
!		disc_pes.

!	02/7/98 GHS
!		When determining width of guassian for disc, its a better
!		model to use a width of sqrt(phwf) when disc_pes=0 istead
!		of 0 (and not allowing <0 values)
!		The high tails work extremely well when tested against full
!		blown model using timing pulses on .25 ns steps etc.

!	13/5/99 GHS
!		Zero pmt channels that are not active if 'DEADT' option chosen.

!       01/05/01 GHS
!               Bring up to speed for new routines. Make call to yhis routine 
!               simular to call to W10M_PEPULSE_ANALIZE
!               Use SINGLE_PE_HEIGHT function for signal + noise pulse heights
!               ASSUME NO AFTER PULSEING!
!               Only look at npmt_mult_trigger pixels.
!        31/03/04 GHS V:1:3:8:3:4:4.17
!                For still more attempts at dirtying up things, change 
!                SINGLE_PE_HEIGHT (W10m_SUBS.F90) to have the width of the
!                single pe pulse height distribution be an input parameter.
!                Defaults to old value (.275). Add PPTR%SINGLE_PE_SIGMAS.
!                Fill it in WCAMERA_GEN. Put in parameters 
!                PULSE_HEIGHT_WIDTH_MEAN and PULSE_HEIGHT_WIDTH_SIGMA in
!                WHIPPLE_TELESCOPE.F90

!

	use whipple_telescope
	use wcamera_def
	use structures
	use record_defs

        IMPLICIT NONE

	type(camera) ::       wptr
	type(camera_ptrs) ::  pptr
	real,pointer,dimension(:) :: disc,disc_pes,disc_pulse_height,disc_trigger
	real :: w_pe_hits	
	logical :: mult_trigger,pst_trigger,ifirst=.true.,after_pulse=.false.
	integer :: i,j,idisc,ithphi,npmt_mult_trigger
	real :: disc_width,pulse_height,trigger_time

	real :: gauss,xdummy,a,pran
	integer :: nfluct

!***************************************************************************
!		Pixel triggers
!***************************************************************************
	disc => pptr%disc(1:wptr%npmt,ithphi)
        disc_pes => pptr%ldisc_pes
        disc_pulse_height => pptr%ldisc_pulse_height
        disc_trigger => pptr%ldisc_trigger

!Find pulse heights in the pixels. Include NOise if requested.
!pptr%disc is filled in WHIPPLE_CAM_PROC in KASTRIGGER.F90.

		!**LOTS OF VECTOR ARITH FOLLOWS**
				!Use single_pe__height function to
				!get pulse height fluctuations. For 0 channels
				!use a  pulse height width factor.  Remove mean	
				!sky, its like a pesdistal
				!Can't do vector arithmatic here because of
				!gauss function which needs to be called once 
				!for each pixel
	disc_pulse_height=0     !INit to 0		          !Vector Arith

                             		!zero pmts that are dead.
	if(wptr%dead_pmts)then
		disc=disc*pptr%active_hv                          !vector Arith
	endif

	do i=1,npmt_mult_trigger
	   if(disc(i)>0)then !Do this for speed to reduce calls
				!to gauss,nfluct and thus pran,RANLUX
				!Get total number(signal +noise) pes in each 
				!pixel.
				!Use some tricky interger round down here to
				!get last fraction of pe.
              disc_pes(i)=disc(i)
              do j=1,disc(i)
                 if(pran(xdummy).gt.pptr%efficiency(i))then
                               !!!!!!!!!REDUCE DISC!!!!!!!!!!
                    disc_pes(i)=disc_pes(i)-1      
                 endif
              enddo

              if(mhead%noise)then                       
                 disc_pes(i)=disc_pes(i)+nfluct(pptr%sky_mean_disc(i))
              endif

              if(disc_pes(i)<=0)then
                 disc_width=sqrt(phwidth_factor)
                 disc_pulse_height(i)=disc_pes(i)+gauss(xdummy)*disc_width
              else
                 do j=1,disc_pes(i)
                    call single_pe_height(pulse_height,after_pulse, &
                         & pptr%single_pe_sigmas(i))
                    disc_pulse_height(i)=disc_pulse_height(i)+pulse_height
                 enddo
              endif
						!remove pedistal
              disc_pulse_height(i)=disc_pulse_height(i)-pptr%sky_mean_disc(i)
              if(disc_pulse_height(i)<0)then
                 disc_pulse_height(i)=0
              endif
	    endif
	enddo
		!Test for which discriminators fire for real shower pes
	call w10m_threshold(pptr,disc_pulse_height,disc_trigger)

        w_pe_hits=sum(disc_trigger(1:npmt_mult_trigger))   !Vector Arith

!Test for a trigger. Make sure at least on of our triggered pixels comes
!from a pixel with real pes in it.So first only check pixels with real pes in
!them.
	if(w_pe_hits>0)then
!add in noise hits(if any)
	   do i=1,npmt_mult_trigger
	      if(disc(i)==0)then !Do this for speed to reduce calls
					!to gauss,nfluct and thus pran,RANLUX
		if(wptr%dead_pmts.and.(pptr%active_hv(i)==0))then
			disc_pes(i)=0
			disc_pulse_height(i)=0
			cycle
		endif

	        if(mhead%noise)then                       
		   disc_pes(i)=nfluct(pptr%sky_mean_disc(i))
		else
		   disc_pes(i)=0
	    	endif
		if(disc_pes(i)<=0)then
			disc_width=sqrt(phwidth_factor)
	    	        disc_pulse_height(i)=disc_pes(i)+gauss(xdummy)*disc_width
		else
			do j=1,disc_pes(i)
                           call single_pe_height(pulse_height,after_pulse, &
                         & pptr%single_pe_sigmas(i))
                           disc_pulse_height(i)=disc_pulse_height(i)+pulse_height
                        enddo
		endif

						!remove pedistal
		disc_pulse_height(i)=disc_pulse_height(i)-pptr%sky_mean_disc(i)
		if(disc_pulse_height(i)<0)then
			disc_pulse_height(i)=0
		endif
	      endif
	   enddo
		!Test for which discriminators fire for all pixels
	   call w10m_threshold(pptr,disc_pulse_height,disc_trigger)
!*******End Pixel Triggers***********************************************

!**************************************************************************
!		Trigger test
!**************************************************************************

!**************************************************************************
!Test Multiplicity Trigger:			
           w_pe_hits=sum(disc_trigger(1:npmt_mult_trigger))
              
           if(ifirst)then
              print*,'Only first',npmt_mult_trigger, &
                   & ' pixels used in Multiplicity trigger!'
              ifirst=.false.
           endif

           if(w_pe_hits<mhead%hmult)then
              mult_trigger=.false.
           else
              mult_trigger=.true.
           endif

!****************************************************************************
!PST analysis
           if(index(mhead%options,'PST')/=0.and.mult_trigger)then 
              !If we have limited ourselves to less then 331 pixels in 
              !trigger they remaining pixels will have the TDC overflow
              !value(100000.+) and thus be ignored in w10m_pst_trigger.
	    	                                   !set up for pst trigger test.
              pptr%time_trig=100001.          !Vector Arith
              do i=1,wptr%ntrigger_in
                 if(disc_trigger(i)==1)then
                    pptr%time_trig(i)=100.     !Put all trigger pixel times
                                               !at 100. ns.
                 endif
              enddo

              call w10m_pst_trigger(pptr,trigger_time)
              if(trigger_time<100000.) then
                 pst_trigger=.true.
              else
                 pst_trigger=.false.
              endif
           endif
!****************************************************************************
	else
           mult_trigger=.false.
	   pst_trigger=.false.
        endif  
!*********End Trigger Test*****************************************************
	return
    END SUBROUTINE W10M_TRIGGER
!******************************************************************************

    SUBROUTINE W10M_THRESHOLD(pptr,disc_pulse_height,disc_trigger)
!******************************************************************************
!	Determine which pixel discrimaters fire.
!******************************************************************************(

!	Modified:
	
!	29/1/98 GHS Converted to F90.
	
	use wcamera_def
	type(camera_ptrs) ::  pptr
	real,pointer,dimension(:) ::  disc_pulse_height,disc_trigger

	integer :: i,isize

	isize=size(disc_pulse_height)
	do i=1,isize
		if(disc_pulse_height(i)>pptr%threshold(i))then
			disc_trigger(i)=1
		else
			disc_trigger(i)=0
		endif
	enddo
	return
    END SUBROUTINE W10M_THRESHOLD
!*****************************************************************************

    SUBROUTINE W10M_UNCOMPRESS(disc_in,disc_out,npmts)
!******************************************************************************
!	Uncompress (restore 0 strings) an array.
!******************************************************************************
	real,dimension(:) :: disc_in,disc_out
	integer :: npmts,i,j

	disc_out=0		!Init it to 0			!Vector Arith
	j=1
	i=1
	do
	  	if(j.gt.npmts)then
			exit
		endif
	   	if(disc_in(j).lt.0)then		!Start on non zero data
			i=-disc_in(j)
			if(i.ne.(npmts+1))then  !look for end of process  
				j=j+1
				cycle
			else
				exit
			endif
		else
			disc_out(i)=disc_in(j)
			i=i+1				
			j=j+1
			cycle
		endif
	enddo
	return
    END SUBROUTINE W10M_UNCOMPRESS
!******************************************************************************





!    SUBROUTINE W10M_VECTOR_REDIRECT(theta,phi,dl,dm,dn,rdl,rdm,rdn)
!! ****************************************************************************
!!	Take a unit vector dl,dm,dn and redirect it by angle theta in
!!	azmituthal direction phi(theta,phi in radians) from direction of
!!	dl,dm,dn and put resultant unit vector into rdl,rdm,rdn.
!!*****************************************************************************
!!
!!	Written by:
!
!!	G.H.Sembroski
!!	Physics Dept
!!	Purdue University
!!	W.Lafayette, In 47907
!!	E-Mail: "sembroski@physics.purdue.edu"
!!	14/5/98
!
!!	Modified:
!
!!	This is an adaptation of the GEOM subroutine used in KASCADE and other
!!	places.
!	IMPLICIT NONE
!	real :: theta,phi,dl,dm,dn,rdl,rdm,rdn
!       real*8,dimension(9) :: a
!	real*8,dimension(3) ::b,r9
!	real :: f,vlength
!
!       f=sqrt(dl*dl+dm*dm)       !f is sin of verticle angle.
!       a(1)=dl*dn/f
!       a(2)=-dm/f
!       a(3)=dl
!       a(4)=dm*dn/f
!       a(5)=dl/f
!       a(6)=dm
!       a(7)=-f
!       a(8)=0.
!       a(9)=dn
!!             | a(1) a(2) a(3) |  | b(1) |  |r9(1) |
!!             | a(4) a(5) a(6) | x| b(2) |= |r9(2) |
!!             | a(7) a(8) a(9) |  | b(3) |  |r9(3) |
!!             vector b is new direction in frame of
!!             track
!!             vector a(3);a(6);a(9)  is track in lab
!!             frame  -v
!!             vector a(2);a(5);.. is vector product
!!             vxz normalized (mod=1)
!
!       b(1)=cos(phi)*sin(theta)
!       b(2)=sin(phi)*sin(theta)
!       b(3)=cos(theta)
!
!! standard routine for multiplying of matrices
!       call mtxmlt(a,b,r9)
!
!! new direction cosines
!!      Force unit length to the vector.
!!     10/23/86
!       vlength=sqrt(r9(1)**2+r9(2)**2+r9(3)**2)
!       rdl=r9(1)/vlength
!       rdm=r9(2)/vlength
!       rdn=r9(3)/vlength
!       return
!    END SUBROUTINE W10M_VECTOR_REDIRECT
!******************************************************************************

   SUBROUTINE W10M_PST_TRIGGER(pptr,trigger_time)
!****************************************************************************
!	Determine if we have a trigger in the pst. If we do put the trigger
!	time into TRIGGER_TIME
!****************************************************************************

!	The PST model is as follows:
!	1:Find patches that have multiplicity trigger(iclude timing.
!	2:Using only those channels in a multiplicity trigger patch that
!	  are on PST_STROBE_DELAY after the trigger(models internal delays
!	  for address strobe inside PST) to form address to check PST_PATTERNS
!	  Use first patch to have a good pattern to get trigger time.

! Modifed:

!       13/05/03 GHS
!          Change logic to find all patches that trigger (not just the first 
!          one.). Save trigger pattern in patch_trigger_pattern array.  Retunr
!          time of first trigger.


	use record_defs
	use wcamera_pst_def
	use wcamera_def
	implicit none
        type(camera_ptrs) :: pptr

	real :: trigger_time,time_trigger
	real,pointer,dimension(:) :: trig_times
	integer,pointer,dimension(:) :: trig_index
	integer :: i,j,k,pix
	integer :: trigger_index
	real :: time_strobe
	real,pointer,dimension(:) :: ptr_trigger_times	 
							!Used to sort patch
							!triggers.
	integer,pointer,dimension(:) :: ptr_trigger_index
							!Used to sort patch
							!trigger times. 
	integer :: pst_address				!Used to form pattern
							!address							



 !*!*!*!*!Remember in all of this a time of >100000 is an overflow!

	trigger_time=100001.    !Default no trigger
        patch_trigger_times=0				!Vector Arith. Init.
        patch_trigger_pattern=0                         !Vector Arith.

!Find patches that have multiplicity trigger.
	do i=1,patches_pst
	    do j=1,pixels_patch_pst		
		pix=pst_pixels(j,i)
		if(pix/=0)then
                   pst_times(j,i)=pptr%time_trig(pix)
                endif
             enddo
					!Order patch pixel times.
            trig_times=>pst_times(:,i)
	    trig_index=>pst_trig_index(:,i)
	    call W10M_ARRAY_ORDER(trig_times,trig_index)

					!look for multiplicity trigger
	    call W10M_SLIDE(trig_times,trig_index,pst_pulse_width, &
			& trigger_index,mhead%hmult)

					!get the time this patch triggers
	    if(trigger_index/=0)then
	    	patch_trigger_times(i)=trig_times(trigger_index)
	    else
	    	patch_trigger_times(i)=100001.	!flag  not triggered.
			
	    endif
	enddo

				!now order the patch trigger times, and start 
				!checking form the earliest to see if we have 
				!an acceptable cluster
	ptr_trigger_times=>patch_trigger_times
	ptr_trigger_index=>patch_trigger_index
	call W10M_ARRAY_ORDER(ptr_trigger_times,ptr_trigger_index)
	

	do i=1,patches_pst		!go though patches in the order they
					!triggered looking for a good pattern.
	   k=patch_trigger_index(i)	!patch that triggered next.
					
	   if(patch_trigger_times(k)>100000.)then
	   	return  	!Last trigger hasd been found.
	   else
					!Get time this patch triggered.
	   	time_trigger=patch_trigger_times(k)
					!Get time patch bits are strobed into
					!address
	   	time_strobe=time_trigger+pst_strobe_delay
				
				!Go through patch pixel trigger times and
				!form address of pattern of patch with those
				!pixels that are high at TIME_STROBE.
		pst_address=0
	   	do j=1,pixels_patch_pst
					!test pixel is on at strobe time.
	   	   if(time_strobe>=pst_times(j,k).and. &
		   	 & time_strobe<=pst_times(j,k)+pst_pulse_width)then
					!its a hit. set bit in addrerss
			pst_address=ibset(pst_address,j-1)
		   endif
		enddo
				!Check to see if pattern good.
		if(pst_patterns(pst_address)/=0)then
			if(trigger_time==100001.)then
                           trigger_time=time_trigger   !Save time of first hit
                        endif
                        patch_trigger_pattern(k)=pst_address
		endif
	
	    endif
	enddo
			!will seldom if ever get here, but be safe.
   	trigger_time=100001. 	!Default flag of no trigger found.
   	return          
   END SUBROUTINE W10M_PST_TRIGGER
!****************************************************************************


!******************************************************************************	
   SUBROUTINE W10M_SLIDE(times,index,window,trigger_index,multiplicity)
!****************************************************************************
!	Search with a sliding WINDOW over TIMES(element ording specified by
!	array INDEX) until we get Multiplicity in window. Trigger_index points 
!	to element that causes the multiplicity requirement to be fulfilled.
!****************************************************************************

	implicit none

	real,pointer,dimension(:) :: times
	integer,pointer,dimension(:) :: index
	real :: window
	integer :: trigger_index
	real :: multiplicity
	
	integer :: npmts,i
	real :: start_window,end_window


		!get size of trigger area.
	npmts=size(times)

	do i=1,npmts-multiplicity+1
		start_window=times(index(i))
		if(start_window>100000)then	
			trigger_index=0		!no trigger		
                        return
		endif
		end_window=start_window+window
		trigger_index=i+multiplicity-1	!If this time is within window
						!we have a trigger
		if(times(index(trigger_index))<=end_window)then
	       		trigger_index=index(trigger_index)
			return
		endif
		if(times(index(trigger_index))>100000)then
			trigger_index=0		!no trigger		
			return
		endif
	enddo
	trigger_index=0		!no trigger		
	return
   END SUBROUTINE W10M_SLIDE
!****************************************************************************


END MODULE W10M_SUBS
