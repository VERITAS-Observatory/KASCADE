!**********************************************************************
!	F77 code for KASTRIGGER
!	Includes CERN source code for RANLUX. (it used to any way, commented
!       out here and moved to ranlux.for
!**********************************************************************

!	subroutine load_pst_patterns(pst_patterns,n)
!
!**********************************************************************
!	Read in from a binary file the pst pattern array. Done as a
!	f77 routine so we could use in Absoft fortran LInux the variuous
!	options (-N3 and -N51) that let us read binary files made on a 
!	VMS machine
!**********************************************************************
!	integer*2 pst_patterns(n)
!	integer*4 n,i
!	open(41,file='pst_mul2.dat',status='OLD',form='UNFORMATTED')
!	do i=1,n
!        	read(41)pst_patterns(i)
!		if(i.lt.100)then
!		  write(6,4300)' i:pst_patterns:',i,pst_patterns(i)
!4300	format(a,i5,o5)
!		endif
!	enddo
!	close(unit=41)		
!	return
!	end
!**********************************************************************

!        function gauss(x)
!	Modified:
!	2/4/92 G.H.S. V:1:0:1:0:0.4
!		Fix width of GAUSS. It was .6932. Make it 1.0 so nfluct has
!		correct width.
!
!	NOTE: This funciton only gives values out to 3/.6932=4.33 sigma and 
!	above 3 sigma its not perfect(but its not bad!).
!
c     This is petes for use by nfluct.
!	real xdummy!
!
!	Sum 6 random numbers(whose mean will be 3)
!
!        sum=pran(xdummy)
!        do i=1,5
!                sum=sum+pran(xdummy)
!	enddo
!        gauss=(sum-3.)/.6932	!Put mean at 0. Correct width.
!        return
!        end
!**********************************************************************

       subroutine geom(dl,dm,dn,tix)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      geom is a subroutine_ which determines
c      the new direction cosines of the particle
cu              dl dm dn dir cos of segm
cu       input:       tix       m.c.s. angle
cu       output:       dl dm dn new dir cos
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc.

c     10/23/86 G.H.S.       Always force final direction vector to be unit
c                    length. Adjust dn to make it so. Does this cause
c                    any bias??? May become non unit length by sucessive
c                    round off error.

c       Warning: This program has round off problems when tix < .001. If
c       this is an important region use geom8.

c	Modified:

c	15/12/93 GHS
c		Convert to real*8 internally to prevent round off problems
c		for small tix.

	external pran
	real*8 pi
	parameter  (pi=3.141592654)
	real*8 a(9),b(3),r9(3),f,rk,c,s,vlength
	real*8 dn_local,dl_local,dm_local,qq,p,u,tix_local
	real dl,dm,dn,tix
       qq(p,u)=sqrt(p*p+u*u)


	tix_local=tix		!Convert input variables to r*8 internaly
	dl_local=dl
	dm_local=dm
				!Calculate our own dn. Make it r*8 to prevent
				!round of problems for small tix
	dn_local=sqrt(1.d0-dl_local**2-dm_local**2)

       f=qq(dl_local,dm_local)       !f is sin of verticle angle.
       a(1)=dl_local*dn_local/f
       a(2)=-dm_local/f
       a(3)=dl_local
       a(4)=dm_local*dn_local/f
       a(5)=dl_local/f
       a(6)=dm_local
       a(7)=-f
       a(8)=0.
       a(9)=dn_local
c             | a(1) a(2) a(3) |  | b(1) |  |r9(1) |
c             | a(4) a(5) a(6) | x| b(2) |= |r9(2) |
c             | a(7) a(8) a(9) |  | b(3) |  |r9(3) |
c             vector b is new direction in frame of
c             track
c
c             vector a(3);a(6);a(9)  is track in lab
c             frame  -v
c
c             vector a(2);a(5);.. is vector product
c             vxz normalized (mod=1)

       rk=2.d0*pi*pran(xdummy)
       c=cos(rk)
       s=sin(rk)
       b(1)=c*sin(tix_local)
       b(2)=s*sin(tix_local)
       b(3)=cos(tix_local)
c
c standard routine for multiplying of matrices
c	Converted to r*8 15/12/93 GHS
       call mtxmlt(a,b,r9)
c
c new direction cosines
c
c      Force unit length to the vector.
c     10/23/86
       vlength=sqrt(r9(1)**2+r9(2)**2+r9(3)**2)
       dl=r9(1)/vlength
       dm=r9(2)/vlength
       dn=r9(3)/vlength
       if(abs(vlength-1.).gt.1.e-5)then
		write(6,1000)dl,dm,dn,r9(3),vlength
1000  format(' ***Geom:Found Unit vector correction .gt.1.e-5.
     1 dl,dm,dn,r9(3),vlength:',/,' ***',5e14.7)
	endif

       if(abs(dn).lt.1.e-8)then
               dn=1.e-8       !Cheap way to handle horizontal tracks.
               dl=(1.-1.e-8)*dl
               dm=(1.-1.e-8)*dm                       
       endif
       return
       end

!**********************************************************************

       subroutine geom8(dl,dm,dn,tix,tiy)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      geom is a subroutine_ which determines
c      the new direction cosines of the particle
cu              dl dm dn dir cos of segm
cu       input:       tix    x scat angle
cu       input:       tiy    y scat angle
cu       output:       dl dm dn new dir cos
c        This version assumes input varaibles are r*8. Do this to avoid
c        roundoff problems for small tix (tix<.001)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc.

c     10/23/86 G.H.S.       Always force final direction vector to be unit
c                    length. Adjust dn to make it so. Does this cause
c                    any bias??? May become non unit length by sucessive
c                    round off error.


c	Modified:

c	15/12/93 GHS
c		Convert to real*8 internally to prevent round off problems
c		for small tix.
c       28/8/00 GHS
c               Convert to use of and x and y scattereing angles.

	external pran
	real*8 pi
	parameter  (pi=3.141592654)
	real*8 a(9),b(3),r9(3),f,c,s,vlength
!	real*8 rk
	real*8 dn_local,dl_local,dm_local,qq,p,u,tix_local
	real*8 dl,dm,dn,tix,tiy
       qq(p,u)=sqrt(p*p+u*u)


	dl_local=dl
	dm_local=dm
				!Calculate our own dn. Make it r*8 to prevent
				!round of problems for small tix
	dn_local=sqrt(1.d0-dl_local**2-dm_local**2)

       f=qq(dl_local,dm_local)       !f is sin of verticle angle.
       a(1)=dl_local*dn_local/f
       a(2)=-dm_local/f
       a(3)=dl_local
       a(4)=dm_local*dn_local/f
       a(5)=dl_local/f
       a(6)=dm_local
       a(7)=-f
       a(8)=0.
       a(9)=dn_local
c             | a(1) a(2) a(3) |  | b(1) |  |r9(1) |
c             | a(4) a(5) a(6) | x| b(2) |= |r9(2) |
c             | a(7) a(8) a(9) |  | b(3) |  |r9(3) |
c             vector b is new direction in frame of
c             track
c
c             vector a(3);a(6);a(9)  is track in lab
c             frame  -v
c
c             vector a(2);a(5);.. is vector product
c             vxz normalized (mod=1)

       tix_local=sqrt(tix**2+tiy**2)	!Get length of scatter.
	if(tix_local==0)then
	   return     !no change to vector.
	endif

!Use specific tix and tiy
!       rk=2.d0*pi*pran(xdummy)
!       c=cos(rk)
!       s=sin(rk)

       c=tix/tix_local
       s=tiy/tix_local
       b(1)=c*sin(tix_local)
       b(2)=s*sin(tix_local)
       b(3)=cos(tix_local)
c
c standard routine for multiplying of matrices
c	Converted to r*8 15/12/93 GHS
       call mtxmlt(a,b,r9)
c
c new direction cosines
c
c      Force unit length to the vector.
c     10/23/86
       vlength=sqrt(r9(1)**2+r9(2)**2+r9(3)**2)
       dl=r9(1)/vlength
       dm=r9(2)/vlength
       dn=r9(3)/vlength
       if(abs(vlength-1.).gt.1.e-5)then
		write(6,1000)dl,dm,dn,r9(3),vlength
1000  format(' ***Geom:Found Unit vector correction .gt.1.e-5.
     1 dl,dm,dn,r9(3),vlength:',/,' ***',5e14.7)
	endif

       if(abs(dn).lt.1.e-8)then
               dn=1.e-8       !Cheap way to handle horizontal tracks.
               dl=(1.-1.e-8)*dl
               dm=(1.-1.e-8)*dm                       
       endif
       return
       end

!**********************************************************************

!	subroutine mass_number2charge_mass(ia,qz,xmass)
!******************************************************************************
!   Determine charge Z of most stable nuclei of nuclear number A
!   Determine mass from Z and A using semi-empirical mass formula
!******************************************************************************
!   Uses fromulae from "Physics of Nuclei and Particles, Vol 1", Marmier and 
!   Sheldon,1969, Acedemic Press, pg 36-38, formula: 2-35,2-38,2-39,2-40
!******************************************************************************
!   This is from the liquid drop model of stable nuclei.
!
!  Written by:
!  Glenn Sembroski
!  Physics Dept.
!  Purdue Univ.
!  W. Lafayette, IN USA 47907
!
! Modified:
!!
!
!
!	IMPLICIT NONE
!	real a
!	integer qz,ia
!	real  xmass,pmass
!	logical  first_argon
!
!	real mp,mn,ap,av,as,ac,aa
!	parameter (mp=938.256e-6)   !Mass of proton (TeV)
!	parameter (mn=939.550e-6)   !Mass of neutron(TeV)
!	parameter (ap=33.5e-6)     !Pairing coef.(TeV)
!	parameter (av=14.1e-6)     !Volume coef (TeV)
!	parameter (as=13.0e-6)     !Surface coef(TeV)
!	parameter (ac=0.595e-6)    !Coulmb coef.(TeV)
!	parameter (aa=19e-6)       !Assymetry coef(TeV)
!	data first_argon /.true./
!
!
!         a=ia
!                         !Correct our formula for elements up to a=56(Fe)
!                         !which is as high as NUC_LIB goes.
!         if(ia==18)then	  !Force Oxygen isotope
!	    qz=8
!	 elseif(ia==24)then
!            qz=12         !Force Magnesium
!         elseif(ia==28)then
!            qz=14         !Force silicon
!         elseif(ia==32)then
!            qz=16         !Force Sulpher
!         elseif(ia==33)then
!            qz=16         !Force Sulpher
!         elseif(ia==35)then
!            qz=17         !Force Chlorine
!         elseif(ia==39)then
!            qz=19         !Force Potassium
!         elseif(ia==40)then
!            qz=18         !Force Argon !Could have been calcium 40.
!            if(first_argon)then
!               print*,'Warning--Forcing Argon for all atomic masses of 40'
!               first_argon=.false.
!            endif
!         elseif(a==56)then
!            qz=26         !Force Iron.
!         else
!            qz=anint(a/(1.98+0.0155*(a**(2./3.)))) 	!Use nearest integer 
!							!function
!         endif
!
!First determine pairing mass term
!         if(mod(qz,2)==0)then
!            if(mod(ia,2)==0)then
!               pmass=-ap*a**(-.75)   !even-even nuclei
!            else
!               pmass=0.              !even-odd nuclei
!            endif
!         else
!            if(mod(ia,2)==0)then
!               pmass=0.              !Odd-even nuclei
!            else
!               pmass=ap*a**(-.75)   !Odd-odd  nuclei
!            endif
!         endif
!
!         xmass = qz*mp + (a-qz)*mn - av*a + as*(a**(2./3.)) +
!	1 ac*(qz**2)/(a**(1./3.)) + aa*((a-2*qz)**2)/a + pmass
! 	return
!	end
!******************************************************************************


              subroutine mtxmlt(a,b,r)
c      Calculates r=Ab, where r,b are vectors and A is matrix, all 3-d.

c	Modified:

c	15/12/93 GHS
c		Make everything real*8

       real*8 a(9),b(3),r(3)
       do 100 i=1,3
              r(i)=0.
              ii=3*(i-1)
              do 101 j=1,3
                     ij=ii+j
                     r(i)=a(ij)*b(j)+r(i)
101           continue
100    continue
       return
       end
!****************************************************************************


!	function nfluct(x)
!****************************************************************************
c       Puts a statistical fluctuation on number of pe's(photons)
!****************************************************************************
c       written by:  Pete Palfrey
c                    Purdue
c                    4/1/89
!
!	Modified:
!	11/5/98 GHS V:1:1:5.3
!		Fix bug in NFLUCT. For small values of x (x<12) check for
!		values of nfluct out to 18. before we were limited to 4*x.
!		This was especially bad for x<.25. This error became very 
!		noticeable when we went to segment step sizes of .02 radiation
!		lengths.
!
!	external pran
!
c       See if fluctuation will be poisson.
!        if(x.lt.12.) then        !Poisson
!                sum=exp(-x)
!                old=sum
!                test=pran(dummy)
!	11/5/98 GHS V:1:1:5.3 Let imax always be 18.
!                imax=min(4.*x,18.)
!                imax=18
!                do i=1,imax
!                        if(sum.gt.test) then
!                                nfluct=i-1
!                                return
!                        end if
!                        xx=i
!                        old=old*x/xx
!                        sum=sum+old
!                end do
!                nfluct=imax
c        Fluctuation is gaussian distributed.
!        else
!                sca=sqrt(x)*gauss(x) + x
!                if(sca.gt.0.) then
!                        nfluct=nint(sca)
!                else
!                        nfluct=0
!                end if
!        end if
!        return
!        end
!****************************************************************************
!
!
!        FUNCTION REXP(X)
c      This function randomly picks from an exponatial distribution using
c      X as the scale factor.
c      Its used for interaction depth(Xis is gm/cm**2) and for decay length
c      (where x is in meters)
c      Rexp is distributed as exp(-s/X)
!	external pran
!
!        Q = pran(xdummy)
!
!c       This produces the required random distribution.
!	REXP = -X*ALOG(Q)
!	RETURN
!        END
!****************************************************************************
