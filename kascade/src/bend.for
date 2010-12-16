	subroutine bendit(tenergy,zpath,ispec,dl1,dm1,dn1,qz,xmass)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      calculate change in direction in magnetic field
c      input:       dl1 dm1 dn1  dir cos of trk
c             tenergy         Kinetic energy of particle Tev
c             zpath         length of segment m (r*8)
c             ispec     particle type
c	      qz        particle charge
c	      xmass     particle mass (TeV)
c      output:       dl1 dm1 dn1  new dir cos of trk
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c           This assumes bx=0,by<<bz  (sin(15)<<cos(15))

!!!!!!!!!!!!Well now bx component is added, assume 10 degrees to the east

c      G.H.S. 10/23/86
c                Fix major error in direction cosign calc. at end.
c                I tried to get cute and got caught.
c                Force unit length to direction vector.

c       G.H.S.+T.R.P 4/1/89
c             Pete found a major problem in the BEND subroutine. The old
c             one is correct in principle but has a big propensity for
c             round off error.  Pete reformulated the algorithum to avoid
c             this.

!	ghs 25/06/01 V:1:2.0
!		Modify for effect of charge of heavies. Radius is inversly 
!               proportional to charge
        character*256 coutstring
!!!!!   qf ghs 12/11/10
!              Add east component of magnetic field


       real*8 zpath
	real xmass	!Mass of particle (in TeV/c)

	integer qz	!Charge of particle
       include 'kascade.h'
c       Determine charge.
       if(qz.eq.0)return       !Neutrals dont bend

c       Unit vector(bx,by,bz) of magnetic field is determined in initalization
c       routine_ BENDINIT

c      CALC MOMENTUM OF particle
       etev=tenergy+xmass       !Energy of particle in Tev.
       p_particle=sqrt(etev**2-xmass**2)*1.e6       !Momentum in Mev.

c      Calcultae radius of curvature.
!	GHS 25/06/01
!	For heavies with charge .ne.1 radius goes like inverse of charge.
       radius=p_particle/(0.03*b_field*abs(qz))
c      angle of revolution
       al=zpath/radius         !angle in radians.


       if(qz.lt.0.) al=-al     !Use sign of charge to see which way
c                                           we bend.
       cal=cos(al)            !Set up to calculate the terms.
       sal=sin(al)
c       This ccc was the main point of problem previously. This method of
c       calculation assures accracy(I hope).
       if(abs(al).gt.0.01) then      !Be warry of accuracy here.
            ccc=1.-cal
       else
            ccc=(al**2)/2.             !expansion
       end if


c       Now calculate new vector.

       dlt= dm1*bz*sal - dn1*by*sal + dl1*(1.-(bz*bz+by*by)*ccc) +
     1        bx*ccc*(dm1*by+dn1*bz)
       dmt= dn1*bx*sal - dl1*bz*sal + dm1*(1.-(bx*bx+bz*bz)*ccc) + 
     1        by*ccc*(dn1*bz+dl1*bx)
       dnt= dl1*by*sal - dm1*bx*sal + dn1*(1.-(bx*bx+by*by)*ccc) + 
     1        bz*ccc*(dm1*by+ dl1*bx)

c       Normalize.
       xmag=sqrt(dlt**2+dmt**2+dnt**2)
       dl1=dlt/xmag
       dm1=dmt/xmag                  !Preserves signs.
       dn1=dnt/xmag

       return
       end



       subroutine bendinitit

c       Init calcultions for BEND routine.
c	The BEND program really does assume bx=0. It doesn't even use bx.
!!!!!   bx is no longer 0

c       Written by : Glenn Sembroski 4/1/89

c       Modified:

c	11/21/90 G.H.S. V:1:0.3
c		Since we have chosen dn positive to be down going and we have
c		chosen the +x axis to be east this makes the +Y axis South.
c		Only thing to fix was the direction of the earths magnetic
c		field which should have a component in the North or
c		-Y direction.

c	2/4/92 G.H.S. V:1:0.6
c		Add ASGAT values for magnetic field to 
c		BENDINIT. New places will new to re-edit BENDINIT to get
c		correct mag field. Some day maybe they should be
c		readin as parameters from .inp file but not today.

c	23/4/92 G.H.S. V:1:0.6
c		Let the letter for the Magnetic filed indicate values to use.
c		T(for compatability) or L=Haleakala. A for ASGAT. F for none.
c		M for Macro.

c	22/10/93 G.H.S. v:1:0.6
c		Add to Magnetic field values using 'W' for WHIPPLE

c	23/10/95 G.H.S. v:1:0.6
c		Add to Magnetic field values using 'S' for Argentina
c		Searchlight array.

!      08/02/05 GHS  V:1:3.6 
!               Update earths magnetic field for Whipple (and for kitt peak).
!               Values determined by Gernot Maier (Leeds) using program Geomag.
!               He gets:Bx=-25.2 By=40.0 =>dipangle=58.3 deg(we were using 60)
!               And a filed strength 0f .4795 gauss (we were using .5)
!
!!!    11/12/2010 QF,GHS
!      Add a east component by 10 degrees, so dipangle=58.3, bxangle=10
!
        character*256 coutstring

	include 'kascade.h'

!DEBUG TEST
	segment_head%magnet_field='W'
!end

c      DETERMINE unit vector for MAG FIELD b

c	And north only!

!!!!!This is for Haleakala ONLY!!!!!!
	if(index(segment_head%magnet_field,'L').ne.0.or.
     1     index(segment_head%magnet_field,'t').ne.0)then
		b_field=0.365      !Gauss
		dip_angle=37.8    !Degree

		write(coutstring,1000)'HALEAKALA',b_field,dip_angle
1000	format(a,' Magnetic field values being used!!!'
     1           ' B_FIELD(gauss):',f7.3,' DIP_ANGLE(deg):',f7.3)
	call kscharstring2coutit(trim(coutstring)//char(0))

	elseif(index(segment_head%magnet_field,'A').ne.0)then
		!!!!!This is for ASGAT ONLY!!!!!!
		b_field=0.45     !Gauss
		dip_angle=57.0    !Degrees.
		write(coutstring,1000)'ASGAT',b_field,dip_angle
	call kscharstring2coutit(trim(coutstring)//char(0))

	else if(index(segment_head%magnet_field,'M').ne.0)then
		!!!!!This is for MACRO ONLY!!!!!!
		!NOTE THESE ARE JUST HALEAKALA VALUES!!!!
		b_field=0.365     !Gauss
		dip_angle=37.8    !Degrees.
                write(coutstring,1000)'MACRO',b_field,dip_angle
	call kscharstring2coutit(trim(coutstring)//char(0))
	else if(index(segment_head%magnet_field,'W').ne.0)then
		!!!!!This is for WHIPPLE ONLY!!!!!!
!		b_field=0.5      !Gauss
!		dip_angle=60.0    !Degrees.

! 08/02/05 GHS Replaced by new values: see comment above.
! 11/15/10 GHS, QF Add east component of B field.
!                  The magnetic field is calculated from the website:
!             http://www.ngdc.noaa.gov/geomagmodels/struts/calcIGRFWMM
 
                !b_field=0.4714      !Gauss
		!dip_angle=58.23    !Degrees.
                !angle_east=10.4   !Degrees.
                b_field=1.0      !Gauss
		dip_angle=0.0    !Degrees.
                angle_east=90.0   !Degrees.

		write(coutstring,1001)'WHIPPLE',b_field,dip_angle,angle_east
 1001		format(a,' Magnetic field values being used!!!'
     1           ' B_FIELD(gauss):',f7.3,' DIP_ANGLE(deg):',f7.3,
     1           ' ANGLE_EAST(deg):', f7.3)
	        call kscharstring2coutit(trim(coutstring)//char(0))
                bz = sin(dip_angle/deg2rad)
                bx = cos(dip_angle/deg2rad)*sin(angle_east/deg2rad)
                by = -cos(dip_angle/deg2rad)*cos(angle_east/deg2rad)
		write(coutstring,1002)'B: ',bx,by,bz
 1002		format(a,' Bx: ',f7.3,' By: ',f7.3,' Bz: ', f7.3)
		call kscharstring2coutit(trim(coutstring)//char(0))
                return
                
	else if(index(segment_head%magnet_field,'S').ne.0)then
		!!!!!This is for Argentine Search light array!!!!!!
		b_field=0.35      !Gauss
		dip_angle=-25.0    !Degrees.
		write(coutstring,1000)'Argentine Search light array',
     1    b_field,dip_angle
	call kscharstring2coutit(trim(coutstring)//char(0))

	else
            write(coutString,3000)
     1    ' KASCADE--FATAL--Illegal magnetic field type:',
     1    segment_head%magnet_field
3000	format(a,a)
	call kscharstring2coutit(trim(coutstring)//char(0))
    
		stop
	endif

!	This next line is unnecessary. Nobody(including BEND) ever uses BX.
!	BEND assumes that there is no field in x direction!!!!!
       
       bx=0.
       by=-cos(deg2rad*dip_angle)!Mag field points north.
                                 !+Y direction is South!!!
       bz=sin(deg2rad*dip_angle)
       return
       end

