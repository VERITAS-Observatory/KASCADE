!kasatm.for
!This is the fortran code for the Palfrey Us Standard atmosphere 1968
!It has the gms,yds and rho functions and the setatm routine which
! initalizes them. The flag INITATM=.true. causes setatm to be called on 
! first use.

!See kasatm.cpp,kasatm.h for the C++ verison of the Sembroski US Standard Atm 
!1976

!*****************************************************************************

        subroutine printAtmVersion
! ***************************************************************************
! Print the version of the stmosphere model we are using
! ****************************************************************************
        IMPLICIT none
        print*,'Using Palfrys implementation of US Standard Atmosphere 68'
        return
        end



        subroutine setatm
!*****************************************************************************
!       Palfrey formulation.
!       Calculate the parameters need by the atmosheric functions: YDS,
!       GMS and RHO.
!*****************************************************************************

c       Assume that the density as a funciton of altitude is strickly an 
c       exponantial.
c       Break Atm up into 3 regions.  Use data from CRC Handbook of Chemistry
c       and physics (70'-71') pg F-148.

c       Written by: Pete Palfrey
c                   Purdue
c                   April 1 1989
        
c       Modified.

        IMPLICIT NONE

        common/densit/asea,bsea,a2atm,b2atm,a3atm,b3atm,csea,c2atm,
     1 c3atm,c11atm,c22atm,gms1,gms2,initatm
	real asea,bsea,a2atm,b2atm,a3atm,b3atm,csea,c2atm,
     1 c3atm,c11atm,c22atm,gms1,gms2 
        logical initatm

c      Atmoshere parameters for SETATM,YDS,GMS,and RHO
	real zsea,zatm1,zatm2,zatm3,rsea,ratm1,ratm2,ratm3
      parameter  (zsea=0.)           !Km at sea level.
      parameter  (zatm1=11.01907)    !km of first slope change
      parameter  (zatm2=32.16191)    !km of second slope change
      parameter  (zatm3=53.44562)    !Km of top at Atm of intrest.
      parameter  (rsea=1.2250e-3)    !Density at sea level gm/cm**3.
      parameter  (ratm1=0.36391e-3)  !Density at altitude zatm1.
      parameter  (ratm2=0.012721e-3) !Density at altitude zatm1.
      parameter  (ratm3=0.00071881e-3)!Density at altitude zatm1.

      real gms
      data initatm/.true./


        initatm=.false.

        bsea=alog(rsea/ratm1)/(zatm1-zsea)        !inv km.l Slope First region.
        b2atm=alog(ratm1/ratm2)/(zatm2-zatm1)     !inverse km. Second region
        b3atm=alog(ratm2/ratm3)/(zatm3-zatm2)     !Top region.

c        !Constants for RHO
        asea=rsea                               !gm/cm3 at sea level
        a2atm=asea*exp(-(bsea-b2atm)*zatm1)     !gm/cm3 at zatm1, Second reg
        a3atm=a2atm*exp(-(b2atm-b3atm)*zatm2)   !gm/cm3 at zatm2, Third reg.

c       Constants for YDS.
        csea=100000.*asea/bsea
        c2atm=100000.*a2atm/b2atm
        c3atm=100000.*a3atm/b3atm

        c11atm=c3atm*exp(-zatm2*b3atm) + c2atm*(exp(-zatm1*b2atm)
     1 -exp(-b2atm*zatm2)) - csea*exp(-zatm1*bsea)
        c22atm=c3atm*exp(-zatm2*b3atm) - c2atm*exp(-b2atm*zatm2)

c       Set up constants for GMS
        gms1=gms(zatm1*1000.)
        gms2=gms(zatm2*1000.)
 
        return
        end
!******************************************************************************


        real function rho(zrho)
!******************************************************************************
!       Determines density in gm/cm3 at altitude zrho(meters)
!******************************************************************************

c       Assume that the density as a funciton of altitude is strickly an 
c       exponantial.
c       Break Atm up into 3 regions.  Use data from CRC Handbook of Chemistry
c       and physics (70'-71') pg F-148.

c       Written by: Pete Palfrey
c                   Purdue
c                   April 1 1989
        
c       Modified.

        IMPLICIT NONE

        real zrho,zzrho

        common/densit/asea,bsea,a2atm,b2atm,a3atm,b3atm,csea,c2atm,
     1 c3atm,c11atm,c22atm,gms1,gms2,initatm
	real asea,bsea,a2atm,b2atm,a3atm,b3atm,csea,c2atm,
     1 c3atm,c11atm,c22atm,gms1,gms2 
        logical initatm


c      Atmoshere parameters for SETATM,YDS,GMS,and RHO
	real zsea,zatm1,zatm2,zatm3,rsea,ratm1,ratm2,ratm3
      parameter  (zsea=0.)           !Km at sea level.
      parameter  (zatm1=11.01907)    !km of first slope change
      parameter  (zatm2=32.16191)    !km of second slope change
      parameter  (zatm3=53.44562)    !Km of top at Atm of intrest.
      parameter  (rsea=1.2250e-3)    !Density at sea level gm/cm**3.
      parameter  (ratm1=0.36391e-3)  !Density at altitude zatm1.
      parameter  (ratm2=0.012721e-3) !Density at altitude zatm1.
      parameter  (ratm3=0.00071881e-3)!Density at altitude zatm1.


      if(initatm)then
         call setatm
      endif
 
        zzrho=zrho/1000.        !convert to km from meters
        if(zzrho.lt.zatm1) then               !Sea level to zatm1
                rho=asea*exp(-zzrho*bsea)

        else if(zzrho.lt.zatm2) then            !Middle region.
                rho=a2atm*exp(-zzrho*b2atm)

        else                              !top region
                rho=a3atm*exp(-zzrho*b3atm)
        end if
        return
        end
!******************************************************************************


       real function gms(zgms)
!******************************************************************************
!        Calulate gm/cm2 as funtion of altitude zgms(meters)
!******************************************************************************

c        Palfrey's fit to the U.S. standard atmosphere.  Data is from
c        Handbook of Chemistry and Physics (70'-71') pg f148.  He uses
c        the altitude vs density data to calculate the parameters for
c        his fits.  He see 2 breaks in the density vs altitude curve
c        and breaks his algorithum up accordingly.  This is much faster
c        then the Gaisser fit and I know where it comes from.

c       Assume that the density as a funciton of altitude is strickly an 
c       exponantial.
c       Break Atm up into 3 regions.  Use data from CRC Handbook of Chemistry
c       and physics (70'-71') pg F-148.

c       Written by: Pete Palfrey
c                   Purdue
c                   April 1 1989
        
c       Modified.

        IMPLICIT NONE

        real zgms,zzgms

        common/densit/asea,bsea,a2atm,b2atm,a3atm,b3atm,csea,c2atm,
     1 c3atm,c11atm,c22atm,gms1,gms2,initatm
	real asea,bsea,a2atm,b2atm,a3atm,b3atm,csea,c2atm,
     1 c3atm,c11atm,c22atm,gms1,gms2 
        logical initatm


c      Atmoshere parameters for SETATM,YDS,GMS,and RHO
	real zsea,zatm1,zatm2,zatm3,rsea,ratm1,ratm2,ratm3
      parameter  (zsea=0.)           !Km at sea level.
      parameter  (zatm1=11.01907)    !km of first slope change
      parameter  (zatm2=32.16191)    !km of second slope change
      parameter  (zatm3=53.44562)    !Km of top at Atm of intrest.
      parameter  (rsea=1.2250e-3)    !Density at sea level gm/cm**3.
      parameter  (ratm1=0.36391e-3)  !Density at altitude zatm1.
      parameter  (ratm2=0.012721e-3) !Density at altitude zatm1.
      parameter  (ratm3=0.00071881e-3)!Density at altitude zatm1.


      if(initatm)then
         call setatm
      endif
 
      zzgms=zgms/1000.          !km from meters
        if(zzgms.lt.zatm1) then !First region. sea level to zatm1
	    if(bsea*zzgms.lt.0)then
		print*,' ***GMS--Warning bsea*zzgms<0:bsea,zzgms:'
	1 ,bsea,zzgms
	    endif
                gms=csea*exp(-bsea*zzgms) + c11atm

        else if(zzgms.lt.zatm2) then            !Middle region.
                gms=c2atm*exp(-b2atm*zzgms) + c22atm

        else                                 !Top region
                gms=c3atm*exp(-b3atm*zzgms)
        end if
        return
        end
!******************************************************************************

        real function yds(ggms)
!******************************************************************************
!        Convert from depth ggms (gm/cm2) to altitude in meters
!******************************************************************************

c        Palfrey's fit to the U.S. standard atmosphere.  Data is from
c        Handbook of Chemistry and Physics (70'-71') pg f148.  He uses
c        the altitude vs density data to calculate the parameters for
c        his fits.  He see 2 breaks in the density vs altitude curve
c        and breaks his algorithum up accordingly.  This is much faster
c        then the Gaisser fit and I know where it comes from.

c       Assume that the density as a funciton of altitude is strickly an 
c       exponantial.
c       Break Atm up into 3 regions.  Use data from CRC Handbook of Chemistry
c       and physics (70'-71') pg F-148.

c       Written by: Pete Palfrey
c                   Purdue
c                   April 1 1989
        
c       Modified.

        IMPLICIT NONE

        real ggms,zzgms

        common/densit/asea,bsea,a2atm,b2atm,a3atm,b3atm,csea,c2atm,
     1 c3atm,c11atm,c22atm,gms1,gms2,initatm
	real asea,bsea,a2atm,b2atm,a3atm,b3atm,csea,c2atm,
     1 c3atm,c11atm,c22atm,gms1,gms2 
        logical initatm


c      Atmoshere parameters for SETATM,YDS,GMS,and RHO
	real zsea,zatm1,zatm2,zatm3,rsea,ratm1,ratm2,ratm3
      parameter  (zsea=0.)           !Km at sea level.
      parameter  (zatm1=11.01907)    !km of first slope change
      parameter  (zatm2=32.16191)    !km of second slope change
      parameter  (zatm3=53.44562)    !Km of top at Atm of intrest.
      parameter  (rsea=1.2250e-3)    !Density at sea level gm/cm**3.
      parameter  (ratm1=0.36391e-3)  !Density at altitude zatm1.
      parameter  (ratm2=0.012721e-3) !Density at altitude zatm1.
      parameter  (ratm3=0.00071881e-3)!Density at altitude zatm1.

      if(initatm)then
         call setatm
      endif
 
      if(ggms.gt.gms1) then     !Bottom of atm.
                zzgms=-alog((ggms-c11atm)/csea)/bsea

        elseif(ggms.gt.gms2) then   !Middle atm
                zzgms=-alog((ggms-c22atm)/c2atm)/b2atm

c                      !Bottom.
        else
                zzgms=-alog(ggms/c3atm)/b3atm
        endif
       yds=zzgms*1000.        !in meters
        return
        end
!******************************************************************************
