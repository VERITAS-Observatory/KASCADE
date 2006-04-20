
    subroutine whippletilt(dlr,dmr,dnr, dndnm, tdlm, tdmm, tdnm, xdl, xdm,  &
                           & xdn, ydl, ydm, ydn, xg, yg, fTime, wrad2,      &
                           &dn_min_tight,dn_min_loose, facet_diam,          &
                           focal_length,   &
                           & jitter_width_ew,jitter_width_ns,meters_per_deg,& 
                           & wx, wy,dump, pe_time, pe_time_tilt) 
!******************************************************************************
!	This is a gets x,y in focal plan of tilted telescope
!******************************************************************************
!	19/11/99 GHS V:1:1:5:1:2.5
!		Change to the use of DN_MIN_LOOSE and DN_MIN_TIGHT in
!		WHIPPLE_TILT. DN_MIN_LOOSE is a full 1.0 deg bigger to
!		allow for large scatters and aberations.
!	Do the tights cut at the very end of whipple_tilt(after scatters
!		and aberations).

 	!use structures
	use kastrigger_def
	!use whipple_tilt_def
	use record_defs
	!use wcamera_def

	IMPLICIT NONE

        real*8 :: dlr,dmr,dnr,dndnm,tdlm,tdmm,tdnm,xdl,xdm,xdn,ydl,ydm,ydn
        real*8 :: xg,yg,fTime,wx,wy,wrad2,dn_min_tight,dn_min_loose,facet_diam
        real*8 :: focal_length,jitter_width_ew,jitter_width_ns,meters_per_deg

	real,external ::    gauss		!declare various functions.
	integer,external :: nfluct
        real,external :: pran

	logical*1,intent(out) :: dump
	real*8 :: pe_time,pe_time_tilt

	type(pe_spec) :: peptr
	real*8 :: vn,xn,dist,xmount,ymount,zmount,wdldm2,wmag
	real*8 :: vn_actual,vn_diff,vn_now,fPhotonRad2
        logical debugprint

         !print*,'dlr,dmr,dnr:',dlr,dmr,dnr
        !print*,'tdlm, tdmm, tdnm:',tdlm, tdmm, tdnm
        !if(wx.eq.0.and.wy.eq.0)then
        !   debugPrint=.true.
        !else
           debugprint=.false.
        !endif

!        The length of the track from hobs to the tilted mount mirror plane is:
!        dist=-((DLm,DMm,DNm) . (xg,yg,zg))/((DLm,DMm,DNm) . (DL1,DM1,DN1))
!        (DLm,DMm,DNm) is normal to mount mirror plane pointed at sky.
!        The term in the denominater is the dot product of the normal to the
!        mirror plane and the direction of the photon. Calculate it first. Its
!	 the relative dn of the photon.
			!Dndnm pre-loaded with dnr*dnm
	vn=(tdlm*dlr+tdmm*dmr+dndnm)	
							!dnr always positive
							!Tdnm always negative
!	plus 1.0 deg(for scatters and aberations.)
	dump=.false.
	if(abs(vn).lt.dn_min_loose)then
		dump=.true.
		wx=1
                if(debugprint)then
                 !print*,'abs(vn), dn_min_loose: ',acos(abs(vn))*180/pi, &
                 !     & acos(dn_min_loose)*180/pi
                endif
                !print*,'dump at atT1'
                return
	endif

!        We want to preserve the angle to N(that is dnr=-vn)  and we also
!        want dlr,dmr,-vn to be a unit vector.
	if(abs(vn).ge.1.0)then
		vn=-1.0                 !Always assume negative mount.
	endif

!	9/12/93 GHS
!	Ok. See if this photon hits the telescope mirror.

!       Now the numerater. This is perendicular distance from xg,yg,zg=0
!	to mirror plane of mount.
        xn=-(tdlm*xg+tdmm*yg)        !zg*dn=0.

!                Length of vector from hobs to intersection of mount plane.
	dist=xn/vn

!	Now correct timing for tilted mirror plane. Pe must go DIST distance
!	further.
	pe_time=ftime+(dist/c_light)*1.e9
							!Convert c_light to
							!m/nsec

!	Now the x,y intercepts of the track in the mount plane.
!	Use the x,y unit vectors as defined in MOUNT_VECINI
!	First the vector of this intercept point.X'=X+T(rack)*dist.
	xmount=xg+dlr*dist
	ymount=yg+dmr*dist
	zmount=dnr*dist	
			!Z direction increases down from the ground plane
			!Define hobs as Z=0
	!Now dot this with the new x' and y' unit vectors to get x,y in
	!mirror plane
	peptr%vpe(1)=xmount*xdl+ymount*xdm+zmount*xdn
	peptr%vpe(2)=xmount*ydl+ymount*ydm+zmount*ydn
	peptr%vpe(3)=0		!init it to 0 for w10m_full_aberration

	!See if this photon is within pmt mirror radius.
!Another note. We could put code here to find which FACET we hit. This might
!		improve the Aberration calculation slightly also.
!	Do it the simple way:
        fPhotonRad2=(peptr%vpe(1)**2+peptr%vpe(2)**2)
        !if(debugprint)then
        !   print*,1,peptr%vpe(1),peptr%vpe(2)
        !endif
        !debugprint=.false.

	if(fPhotonRad2.gt.wrad2)then
            dump=.true.		! Gets here if mirror is missed.
            wx=2
            !if(debugprint)then
             !  print*,2,peptr%vpe(1),peptr%vpe(2)
               !print*,'fPhotonRad2,wrad2:',fPhotonRad2,wrad2
            !endif
            !print*,'Dumt atT2'
            return			!Drop this pe.
          endif

       !debugprint=.false.
!	Now get relative direction of photon to mirror plane.
	if(vn.eq.-1.0)then	!verticle photon?
		peptr%vdir(1)=0.  !DL
		peptr%vdir(2)=0.  !DM
		peptr%vdir(3)=1.0  !DN	(positive)
	else
!        Form the relative vector components of photon to mount.
!	Again dot products with x',y' unit vectors.
		peptr%vdir(1)=dlr*xdl+dmr*xdm+dnr*xdn
		peptr%vdir(2)=dlr*ydl+dmr*ydm+dnr*ydn
		wdldm2=peptr%vdir(1)**2+peptr%vdir(2)**2	!Normalize it.
		wmag=sqrt(wdldm2/(1.-vn**2))
		peptr%vdir(1)=peptr%vdir(1)/wmag
		peptr%vdir(2)=peptr%vdir(2)/wmag
      					!Positive sign means down going.
		peptr%vdir(3)=sqrt(1-peptr%vdir(1)**2-peptr%vdir(2)**2)
	endif                                                  

!	THIS IS WHERE WE DO OPTICAL ABERRATIONS.
!	W10m_full_aberration finds where in the focal plane this photon lands
!	after applying  both  global and facet aberations for the whipple
!	10m mirror. We will have to do things a little different when we
!	go to the 11m mirror.

!	12/12/97 GHS Save mirror plane time.
	pe_time_tilt=pe_time
	peptr%time=pe_time

! 	call w10m_full_aberration(peptr,debug_var)
 	call fullAberration(peptr,facet_diam,focal_length,jitter_width_ew, &
                            & jitter_width_ns, meters_per_deg)
	pe_time=peptr%time
	wx=peptr%vw(1)
	wy=peptr%vw(2)
	vn_actual=sqrt(wx**2+wy**2)

	vn_now = cos(vn_actual*pi/180.0)
        if(vn_now<dn_min_tight)then
           dump=.true.
           wx=3
           if(debugprint)then
              print*,'vn_actual,dn_Min_Tight:deg ',vn_actual, &
                   & acos(dn_min_tight)*180/pi
           endif
           !print*,'Dump at T3'
        endif
	return
    END SUBROUTINE whippletilt
!***************************************************************************

   SUBROUTINE FULLABERRATION(peptr,facet_diam,focal_length,jitter_width_ew, &
                             &  jitter_width_ns,meters_per_deg)
!   SUBROUTINE W10M_FULL_ABERRATION(peptr,debug_var)
!**************************************************************************
!	Aberration of a Cotten-davis mirror 
!	determined by  exact ray tracing plus a gaussian jitter added for
!	pointing/spotsize errors.
!**************************************************************************

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
!	use record_defs
!      	use whipple_telescope
!	use kas_ranlux
	IMPLICIT NONE

	type(pe_spec) :: peptr
        real*8 :: facet_diam,focal_length,jitter_width_ew,jitter_width_ns
        real*8 :: meters_per_deg

	real*8,dimension(3) :: vfacet,vdelta,vrc,vph,vph1,vn,vperp,vpara, &
             & vdr,vpef
	real*8,dimension(3) :: vdcsn
	real*8,dimension(3) :: v2fl
	real*8 :: mag_vrc,mag_vph,mag_vn,rf,xyf_mag_sqr,path,z_dist


	real*8 :: tix,tiy
	real*8 :: w10m_ct
	real :: xdummy

	real,external ::    gauss		!declare various functions.
	integer,external :: nfluct
        real,external :: pran

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
   END SUBROUTINE FULLABERRATION
!******************************************************************************
! Notes:
!******************************************************************************
!  Version: V:1:4:9:4:6.0"

!	We want to find triggers for a single mount using various camera
!	configurations on a  Chernkov telescope. This is used to look at the 
!	sensitivity of a telescope or an array of detectors to cosmic ray 
!	and Gamma showers.

!	Events that meet the required parameters can be written to a ksTeFile

!	It uses as input the sorted PES file that ksKascade,ksLight and 
!       kePeSort have produced.

!	This is a conversion of the F77 KASTRIG program  to F90 code.
!	(with lots of clean up done as is appropriate.)
! ****************************************************************************

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
!	E-Mail: "sembrosk@physics.purdue.edu"

!	Modified:

!	23/1/98 GHS  V:1:1:5:1:1.1
!		Conversion from original KASTRIG program. This program is
!		designed to look for triggers from several different cameras on
!		the whipple 10m mirror at the same time. All non whipple stuff
!		has been removed.

!	13/5/98	GHS V:1:1:5:1:2.0
!		Set up for VERITAS.
!		1:Mount direction supplied here now. Unlike the past we now
!		  do not find the relative values of pe directions and timings
!		  to the mount plane in KASLITE. Instead the PES file has the
!		  directions and timings for when the pe's hit the ground.
!	          It is in this program where we read in the mount direction
!		  and preform the relative pe directions and adjusted
!		  timing calculations. 
!	12/6/98	GHS V:1:1:5:1:2.1
!		Add option 'TIM' to get timing of each triggered pixel(with
!		noise) and also overall event trigger time.Use the PEPULSE
!		building terchnique(.25 ns steps of pulses built up from
!		single pe pulse shape with varying pulse hieghts. Use CFD
!		timing determination.
!	25/2/99 GHS	V:1:1:5:1:2.3
!		Add capability for modeling trigger using the full blown
!		timeing. Use pulse shapes and CFD times to get trigger time
!		Requires modifying W10_PEPULSE_ANALIZE and where its called
!		from. Use of 'TIM' option causes use of pepulse construction
!		for trigger.
!	16/11/99 GHS V:1:1:5:1:2.5
!		Add stuff for Whiplle small pixel camera:W490. This camera uses
!		379 small 1/2" pixels + 3 rings of 37 each 1" pixels. 
!       28/8/00 GHS V:1:1:6:1:2.8
!               Convert W10M_FULL_ABERRATION to r*8 internally to avoid some 
!               small angle roundoff problems. Use geom8.  
!               Use cartisian (x gaussian and y
!               gaussian independently) for jitter of mirror normal.
!       16/04/01 GHS V:1:1:6:1:2.11
!               Reimpliment the quick and dirty way of finding disc triggers 
!               and adc values (by summing pe's in each pixel). One would use 
!               this instead of the pepulse method when speed is necessary.
!               5:WHIPPLE_IMAGE_PROC: Use 'TIM' option to determine if we 
!               analize useing the pepulse method or the old sumed pes in 
!               pixel method. Note change of option to 'TIM'
!               6:Redo W10M_TRIGGER in W10M_SUBS.F90 to use argument list 
!               simular to W10M_PEPULSE_ANALIZE. Use SINGLE_PE_HEIGHT to 
!               genertae pixels pulse height. Check for PST trigger.
!       08/05/01 GHS V:1:1:6:1:2.12 
!               In w10m_full_aperation Use seperate jitter widths for e-w and 
!               ns(top-down) for oval spotsizes.
!       06/05/03 GHS  V:1:3:7:2:3.1
!               Update VERITAS mirror parameter(in WHIPPLE_TELESCOPE.F90)
!               Add mirror_radius_sqaured variable.
!      22/04/03 GHS V:1:3:8:3:4.1"
!               1:490 Pixel camera has only 331 pixels in bot multiplicity and
!                 PST trigger, not 379! Fix W10M_PROC
!                 to use wptr%ntrigger_in. WCAMERA_DEF already has that.
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
!                to step in theta only up to MaximumThetaDeg (7=2.1 degs nominally)
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
!      28/2/06GHS V:1:4:9:4:6.0
!                Major revamping of code to use VEGAS type config files. This
!                now becomes a subroutien of ksTrigger.cpp. Lots and lots of 
!                cleanup, (but its still pretty messy).

!*****************************************************************************


