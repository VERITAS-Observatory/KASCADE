        SUBROUTINE KSKASLITEMAIN(WhippleMount, VeritasMount, TriangularGrid, 
     1                           SquareGrid, NorthSouthGrid,RandomCoreOffset, 
     1                           ksWhipplePMTs, ksVeritasPMTs, ksADPPMTs,
     1                           benchmark, ksdli, ksdmi, ksdni, kshobs,
     1                           ksitype, tep, ksxseg, ksyseg, ksxoff, ksyoff,
     1                           showerid,efficiency)

c     Version: V:2.0.0

c       This is the chernkov light main subroutine which uses the showers 
c       produced by
c       the ksKascade shower program and produces the cherenkov photo-electrons
c       that we should get in our detector.

c       It uses as input a file made by ksKascade which describes all particle 
c       track segments in the shower(Only Charged tracks saved). These 
c       segments include
c       information on the number of PE's produced(Solar blind tube, Visable,
c       Old Luny,CEASAR, and ASGAT), and their Cherenkov angles.(but see 
c	25/10/93)

c       The output consistes of a file of PEs on the ground.  

c       Input/Output
c	 unit=1:		Used to run through ASGAT.dat. ASGATacceptance
c				Also used for EXTINT.DAT for photon rates.
c        unit=2:                Shower segment file
c        unit=3:		Random number seed file.  kaslite*.ran
c        unit=7:                Output PE file


c        Written:
c	Mary P. Kertzman
c	Dept. of Physics and Astronomy
c	DePauw University
c	Greencastle, In. 46135 
c	E-Mail: "kertzman@depauw.edu"

c	G.H.Sembroski
c	Physics Dept
c	Purdue University
c	W.Lafayette, In 47907
c	E-Mail: "sembroski@physics.purdue.edu"
c       The input shower segment file which was made by ksKascade
!     **********************************************************************
c	The first record of the file is the ksSegmentHeader record and has 
c       the following information:
c       ksitype: The particle type of the primary particle
c		where:                                         
c	        Particle species codes.
c              	 1:Gamma
c                2:positron
c		 3:electron
c                4:muon (+)
c                5:muon (-)
c                6:pion (0)
c                7:pion (+)
c                8:pion (-)
c                9:kaon (+)
c               10:kaon (-)
c               11:kaon (0long)
c               12:kaon (0short)
c               13:proton
c               14:neutron
c               15:neutrino(electron)
c               16:anti-neutrino(electron)
c               17:neutrino(muon)
c               18:anti-neutrino(muon)
c		>20: Heavy ion of A=ITYPE-20
c        dli,dmi,dni: Direction cosigns
c			of the primary particle.dni reconstructed from these.
c			dni assummed to be positive(going down).
c        hobs:          Altitude of observation. Particles in the
c			shower are followed to this altitude,then stopped
c			(Meters above sea level).
!     **********************************************************************
c     The shower consists of a number of straight segments of tracks. Charged
c     tracks are divided up into segments of length .01(susually) radiation 
!     lengths or less. Each record describes one of these segments. 
c     These records follow the header record and are read into the following:
c	xstart,ystart,hstart: Initial xy,z of segment.
c       dlstart,dmstart:Initial x,y direction cosigns of segment. dnstart can 
!                       be recreated from dnstart=sqrt(1-dl1**2-dm1**2)
c                       dn assummed to be positive(going down).
c       tend: relative time at end of segment.
c	hend: final altitude of segment
c	dlend,segment.dmend: final direction cosigns of segment.
c	gamma: gamma at middle of segment.
c	nspec: particle type
! ***************************************************************************
c       Modified.
c	2/4/92 G.H.S. V:1:0:1.2
c		Fix width of GAUSS. It was .6932. Make it 1.0 so nfluct has
c		correct width.
c	22/7/96 GHS V:1:0:3.3
c		The sparse area cut is made for Whipple only. NOT! Veritas!
c	21/5/97 GHS V:1:1:4.2
c		Add the Vetritas 'VT' option. This specifies a veritas array
c		with a triangular grid spacing.TRiangle array achieved by 
c               having 1:NY as before. 2:NY even then NX as before. 3:NY odd 
c               then NX center shifted 1/2 XSEG to right. This will only make 
c               a difference to this routine where NX,NY are set and to 
c               KAS_ARRAY_TRIGGER. If we let:
c               yseg=10m=sqrt(xseg**2-(xseg/2)**2). => 
c		xseg=sqrt(4/3)*yseg=11.547m.    Do it this way so 
c		both xseg and yseg are .ge. 10m but see 02/05/03
c	26/11/97 GHS V:1:1:4.4
c		Add offset option to PETYPE. This option indicates that we are
c		to randomly reposition the core of the shower within the grid 
c		area. We do this by once ranomly generating an X_OFFSET
c		and a Y_OFFSET within the area. We then modify the 
c		segment postion as it is read in. This allows us to have the 
c		cores of our showers be at other places besides strickly at 
c		x=0,y=0. This is important for any array study.
!	20/3/98 GHS V:1:1:5.1
!		Again in RATE_UV_GEN Limit use of ATEXP as exponent to prevent
!		some overflows.	       
!	11/5/98 GHS V:1:1:5.3
!		Fix bug in NFLUCT. For small values of x (x<12) check for
!		values of nfluct out to 18. before we were limited to 4*x.
!		This was especially bad for x<.25. GThis became very noticable
!		when we went to segment step sizes of .02 radiation lengths.
!	11/07/01 GHS V:1:2:6.2
!		Extend to handle Heavy ion tracks. ITYPE is A of ion +20.
!		Thus HE(A=4) has ITYPE=24 and FE(56) has ITYPE=76. 
!		1:Add routine MASS_NUMBER2CHARGE_MASS that given A gives back
!		  the charge and mass in GeV of the ion that made the track.
!		  Use this along with the NUCLI_NAMES array to specify shower
!		  primary.
!		2:Determine segment generater charge before we deterime C^ rate
!		3:Add QZ (charge) argument to RATE_GEN and RATE_UV_GEN. 
!       30/04/02 GHS V:1:3:7.1
!               For Kitt Peak Veritas site change mirror diameter to 12 m.
!               1:Change grid spacing from 10m to 12 m (xseg,yxseg) for 'VT' 
!                 option.
!               2:Set mirror area to 113.1 (12m diameter). Not used anywhere.
!                 Just printed out as information.
!       18/03/04 GHS V:1:3:8.2
!               RATEINIT: Print out which PMT,MIrror/filter used
!       06/12/04 GHS V:1:3:8.3
!               Add BENCHMARK_FLAG to determine whether to use 1.0 for QE and
!               mirror reflectvivity. Defaults to False (use telescope type 
!               specified QE and mirror reflectivity tables.
!               (Whipple and Veritas only)If benchmark is selected:
!               1:use 1.0 for quantum eff.: RATEINIT
!               2:use 1.0 for mirror reflectivity:RATEINIT
!               3:Don't write out pe file.Use MAKE_PE_FILE to flag this: MAIN
!               4:Collect rho vs r histogram and write to text file:MAIN
!      07/12/04 GHS  V:1:3:8.4
!               Move setatm,rho,yds,gms into seperate file: kasatm.for.
!               Remove atm stuff from kaslite.h. include it explictly in
!               kasatm.for.
!               Do this in preperation for conversion to atm76 model 
!               (kasatm.cpp,atmosphere76.h, atmosphere76.cpp)
!               Convert to atmosphere76.cpp,kasatm.cpp
!      08/12/04 GHS  V:1:3:8.5
!               Altitude bug: rate_uv_gen,rate_gen:extint table only goes 
!               up to 50 km.  Make it that when we are above 50 km its ok (we 
!               don't crash) but we don't produce any light(rate=0)!
!      17/02/05 GHS  V:1:3.8.7
!               Add new veritas PMT Q.E. table for Photonis xp2970eff.  
!               Remove obsolete code for some the extranious detectors. Keep 
!               only Whipple 10m and VERITAS 12m
!      20/02/06 GHS: V:1:3.9.0
!               Convert this to a subprogram called by ksLight.cpp. Use config
!               files and command line options to replace .par files. Use c
!               ctime stuff for dat and time. General cleanup.

      IMPLICIT NONE

! *************************************************************************
! Input subroutine argument specifications
! *************************************************************************
      logical*1 WhippleMount,VeritasMount,TriangularGrid, SquareGrid
      logical*1 NorthSouthGrid, RandomCoreOffset, ksWhipplePMTs
      logical*1 ksVeritasPMTs,ksADPPMTs,Benchmark
      real*4  ksdli, ksdmi, ksdni, kshobs,ksxseg, ksyseg, ksxoff
      real*4  ksyoff, tep, efficiency
      integer*4 ksitype,showerid

      logical*1 goodread
      logical sparse_area
      integer*4 phtcnt,photonkept,totalmade,sparse_lost,heavy_made,rangelost
      real*8 totalpath
      integer*2 numw
      character*80 version
      character*80 update

      real*8 shower_max,shower_max2
      integer inext,itwcount
      real xmass

      integer*4 i,nmade,ilambda,nfluct,nmadeold,ij,n,ny,nx
      real areamir,projarea,arearatio,gms,hmid,dns,path,rho
      real w10m,yg,xg,ttime,dlr,dmr,rate,sangle,wavelength,emissionalt

!******************************************************************************
!      Heavy Nuclei declarations block:
!******************************************************************************
      integer qz,ia
      integer ios

!******************************************************************************
!Benchmark variables
!******************************************************************************
      real bench_rho(300),radius,radiushigh,radiuslow,area
      integer irho
      character*80 pe_benchmark_output_file


      !include 'kaslite_command_line.h'
      include 'kaslite.h'
      
      data version/'V:2.0.0'/
      data update/'16-May-2006 GHS'/

      write(6,1205)trim(version),trim(update)
 1205 format(' ksLight: Version: ',a,' ksLight: Last Update:',a)

! *************************************************************************
! load up Command line and input parmeters. Replaces command line input code
! and call to datin
! *************************************************************************
      xseg=ksxseg
      yseg=ksyseg
      x_offset=ksxoff
      y_offset=ksyoff
      hobs=kshobs
      pe_next=1
      dn=-ksdni  
      dni=ksdni

      benchmark_flag=benchmark
      VeritasPMTs=ksVeritasPMTs
      WhipplePMTs=ksWhipplePMTs
      ADPPMTs=ksADPPMTs

! **************************************************************************
!Initalize arrays for Cherenkov photon production rates.     
! **************************************************************************
      call rateinit 

c     We are going to create a file of PE's due to photons.
c     Each photon can only hit one area on the ground. In each area a 'mount'
c     in the form of a disk of area=areamir tilted at
c     dl,dm,dn will be formed. The coordinates of where a photon intesects
c     this plane of this mount will be calculated and its timing
c     corrected.  Since we dont actually care where on the plane it hits
c     we can adjust the # pes to the number that actually hit the mount
c     projected area of mirror/(xseg*yseg)) before we generate all the pes.
c     We then assume all pes hit the 'mount'.This is good reason for keeping
c     xseg*yseg comparable to detector size.

      if(WhippleMount)then
         areamir=78.5           !10m mirror active area
         print*,' **Never area cut for Whipple 10m. Useing Efficiency only.'
         if(tep.gt.5.0)then     !29/6/94 GHS:sparse area adjustment flag
            sparse_area=.true.
            print*,' For Whipple 10m>>>>>>>>>>>>>>>>>>>'
            print*,'>>>>>>>> For energies above 5.0 TeV',
     $           ' only areas with even values of nx, ny kept.'
            print*,'>>> REduction by factor of 4>>>>>>>'
         else
            sparse_area=.false.
         endif
      elseif(VeritasMount)then  !Veritas array(Kitt Peak)  
         areamir=113.1          !12m mirror active area
         print*,' **No area or sparse cut for VERITAS 12m. Efficincy cut only.'
      else
         stop 'FATAL--No Mount specified. Should never happen!'
      endif
                                !Projected area of mirror on ground.
      projarea=areamir/(-dn)	!12/5/90 We want mount direction(dn) not
                                !Shower direction here(dni) but dn ~-dni is
                                ! ok. projarea not used anywhere in this prog.
      arearatio=efficiency
      
      write(6,1003)xseg,yseg,xseg*yseg,areamir,projarea,arearatio
 1003 format(' TELESCOPE PARAMETRS:',/,
     $     '                Grid dimensions on ground: ',f6.2,
     $     ' m(X) x',f6.2,' m(Y)',/,
     $     '                                Grid area: ',f6.2,' m**2',/,
     $     '                              Mirror area: ',f6.2,' m**2',/,
     $     '                 Projected area on ground: ',f6.2,' m**2',/,
     $     '                     Pe Correction factor: ',f6.4)
      
! ***************************************************************************
!     DEtermine some constants for chenkov light propagation
! ***************************************************************************
      ftzcobs=1./(c_light*100.*rho(0.0)) !Convert c_light to cm/sec.
      tzcobs=gms(hobs)*ftzcobs  !Constants for time of flight(in sec)
      
      
! ***************************************************************************
! Init some shower summary statisctics variables
! ***************************************************************************
      phtcnt=0.               !Total number of photons processed.
      uplost=0.               !Number lost in Cernek going up.
      mountlost=0.            !Number that missed the mount or didn't reflect.
      photonkept=0.           !Number actually written out.
      heavy_made=0.           !Number from ions.

      INEXT=0                   !Counts segments

! ---------------------------------------------------------------------------
! Start main loop
! ---------------------------------------------------------------------------
      do while(1)
c     READ a SEGMENT!
         call kssegmentread(goodread, segment.XStart, segment.YStart, 
     1       segment.HStart, segment.DlStart, segment.DmStart, segment.tend, 
     1       segment.HEnd, segment.DlEnd, segment.DmEnd, segment.Gamma, 
     1       segment.nspec)

         if(.not.goodread)then  ! end of shower.
            if(inext.eq.0)then
               write(6,3000)"ksLight:Fatal--Segment file had no segments"
 3000          format(a)
               stop 'Failure:Zero segments in segment file'
            endif
            exit
         endif

         inext=inext+1
         nmade=0

! If 'OF' option is selected translate segment postion by the randomly
! generated X_OFFSET,Y_OFFSET postion within the area.
         if(RandomCoreOffset)then
            segment.xstart=segment.xstart+x_offset
            segment.ystart=segment.ystart+y_offset
         endif

c Process Segment:
c Whipple 10m + VERITAS
c	The number of photons produced by a track segment for the 10m
c	telescope is determined here. 
         if(WhippleMount.or.VeritasMount)then
                            !Whipple 10m for both with and without ears.
                            !Also for Veritas array which used 12m mirrors/pmts
            hmid=(segment.hstart+segment.hend)/2.          !Mid altitude
            dns=sqrt(1.-segment.dlstart**2-segment.dmstart**2) !Form the dns
                                !Segment is assumed straight for this.
            path=(segment.hstart-segment.hend)/dns
            if(segment.nspec.gt.20)then !Determine charge to modify rate.
               ia=segment.nspec-20
               call mass_number2charge_mass(ia,qz,xmass)
            else
               qz=1    !Everthing else has charge of +/- 1, Since we use Z**2 
            endif      !to modify rate use qz=1.

c     Iterate over all wavelength intervels.
            do lambda=180.,700.,5.
               ilambda=lambda	!lambda is real*4
               i=(ilambda-180)/5
                                !Determine photon production rate for this 
                                !5 nm wavelength intervel. LAMBDA passed 
                                !through common block.
               call rate_uv_gen(hmid,dns,qeta10m,qgam10m,qeta10m_hobs,
     $           qgam10m_hobs,rate,sangle,qz)

               w10m=path*rate	!Determine number of pe's produced at lambda 
                                !for this segment
               if(w10m.gt.0)then
                  numw=nfluct(w10m) !Fluctuate it.
                  nmadeold=nmade    !save pointer.
                  if(numw.ne.0)then
                     call cerenk(numw,sangle,etasea(i),nmade,arearatio)
                  endif
               elseif(sangle.eq.0)then
                  exit          !Quit when threshold reached(flag:sangle=0.0)
               endif
               phtcnt=phtcnt+numw
            enddo
         endif


c Photons from this segment get stored in array PHOTONS by CERENK.

c     elements of PHOTONS
c     photons(1,i),photons(2,i): X,Y (meters)at hobs of photon i.
c     photons(3,i),photons(4,i),photons(5,i):direction cosigns of photon i.
c     photons(6,i):Time of arrival of photon i at hobs.
c     photons(7,i):Wavelength of photon in nm.Added 25/10/93 G.H.S. V:1:0:2.0

         totalpath=totalpath+path !collect for debugging purposes
         if(nmade.eq.0)then
            cycle                !go to next segment if none to do.
         endif
         totalmade=totalmade+nmade !collect for debugging purposes
         do ij=1,nmade
            if(VeritasMount)then
               if(.not.NorthSouthGrid)then !Array areas are long in east-west
                                           !direction Alternate rows offset
                                !Special triangular array specification.
                                !ex: x,y=(0,0) center of        NX,NY=(0,0)
                                !    x,y=(xseg/2,yseg)center of NX,NY=(0,1)
                                !    x,y=(0,2*yseg)center of    NX,NY=(0,2)
                  n=(photons(2,ij)/yseg)+.5+100000.
                  ny=n-100000   !The '100000' here is to get the round down to
                                !work right. Negative numbers round up!
                  yg=photons(2,ij)-ny*yseg !Form 'Local' coordinates.
                  if(mod(ny,2).eq.0)then !ny even. NX determined as in old way.
                     n=(photons(1,ij)/xseg)+.5+100000.
                     nx=n-100000
                     xg=photons(1,ij)-nx*xseg
                  else          !NY odd. Shift NX center to right 1/2 of xseg.
                     n=(photons(1,ij)/xseg)+100000.
                     nx=n-100000
                     xg=photons(1,ij)-(nx+.5)*xseg
                  endif
               else             !Array areas are long in North-South
                                !direction.Alternate coulums offset
                  n=(photons(1,ij)/xseg)+.5+100000.
                  nx=n-100000 
                  xg=photons(1,ij)-nx*xseg
                                !    x,y=(0,0) center of        NX,NY=(0,0)
                                !    x,y=(xseg,yseg/2)center of NX,NY=(1,0)
                                !    x,y=(2*xseg,0)center of    NX,NY=(2,0)
                  if(mod(nx,2).eq.0)then
                                !nx even. NY determined as in old way.
                     n=(photons(2,ij)/yseg)+.5+100000.
                     ny=n-100000
                     yg=photons(2,ij)-ny*yseg
                  else
                                !NX odd. Shift NY center to right 1/2
                                !of yseg.
                     n=(photons(2,ij)/yseg)+100000.
                     ny=n-100000
                     yg=photons(2,ij)-(ny+.5)*yseg
                  endif
               endif
            else
               n=(photons(2,ij)/yseg)+.5+100000.
               ny=n-100000 
               yg=photons(2,ij)-ny*yseg
               n=(photons(1,ij)/xseg)+.5+100000.
               nx=n-100000
                                !x,y=(0,0) is in area 0,0 as is x,y=(-.5,-.5)
               xg=photons(1,ij)-nx*xseg
            endif

!***************************************************************************
!	Limit nx ny for sort
!***************************************************************************
            if(abs(nx).gt.16000.or.abs(ny).gt.16000)then
               rangelost=rangelost+1
               cycle         !Drop it.
            endif
!***************************************************************************


!***************************************************************************
!			Sparse_area check.
!***************************************************************************
                            !(for tep > 5.0 TeV reduce pe 
                            !file size by only keeping
			    !areas with nx and ny even. Factor of 4 reduction.
            if(sparse_area)then
               if(mod(nx,2).ne.0.or.mod(ny,2).ne.0)then
                  sparse_lost=sparse_lost+1
                  cycle      !Drop it.
               endif
            endif
!***************************************************************************
!Load up some variable (just for claity of the code)
            ttime=photons(6,ij)
            dlr=photons(3,ij)
            dmr=photons(4,ij)
            wavelength=photons(7,ij)
            emissionalt=photons(8,ij)
!****************************************************************************
c        Write out the record. (disabled for Benchmark mode)
!****************************************************************************
            if(.not.benchmark_flag)then
            
               call kspewrite(nx, ny, ttime, dlr, dmr, inext, xg, yg, 
     1              segment.nspec, wavelength, emissionalt)
               photonkept=photonkept+1
               if(pe.spec.gt.20)then
                  heavy_made=heavy_made+1
               endif
           endif

!****************************************************************************
!Benchmark modification:Collect benchmark histogram rho vs r
!****************************************************************************
            if(benchmark_flag)then
               radius=sqrt(photons(1,ij)**2+photons(2,ij)**2)
               irho=(radius+.5)+1 !make integer on 1 m steps
               if(irho.le.300)then
                  bench_rho(irho)=bench_rho(irho)+1
               endif
            endif
!****************************************************************************


!****************************************************************************
!TW modification. 01/06/02 GHS V:1:2:6.3 Keep it.
!****************************************************************************
            if(nx.eq.0.and.ny.eq.0)then
               itwcount=itwcount+1
            endif
!****************************************************************************


             shower_max=shower_max+hmid
            shower_max2=shower_max2+hmid**2
         
         enddo                  !Endo of cherenkov photon do loop.
      enddo                     !End of segment do while

        
!END of SEGment file found.
!******************************************************************************
!Open the output Benchmark text file
!******************************************************************************
      if(benchmark_flag)then
         open (12,access='sequential',status='new',
     1        iostat=ios,file="benchmark.txt")
         if (ios>0)then
            print*,'KASLITE--benchmark text output file open error:',
     1           trim(pe_benchmark_output_file),ios
            stop 'Benchmark Output text file open failure'
         endif
!******************************************************************************

!******************************************************************************
! For Benchmark:convert from counts to a density(divided by area of ring)
!      write out what we want to the ouput file
!******************************************************************************
         radius=.5
         bench_rho(1)=bench_rho(1)/(pi*radius**2)
         write(12,1005)bench_rho(1)
 1005    format((e14.7,','))
         do i=2,300
            radiuslow=.5+(i-2)  !starts at .5 and steps by 1
            radiushigh=.5+(i-1) !starts at 1.5 and steps by 1
            area=pi*(radiushigh**2-radiuslow**2)
            bench_rho(i)=bench_rho(i)/area
            write(12,1005)bench_rho(i)
         enddo
         
         close(12)
!******************************************************************************
         
      endif


! ****************************************************************************
! Write summary statistics
! ****************************************************************************
      write(6,2400)phtcnt,photonkept
 2400 format(
     $     '              Number of photons processed=',i10,/,
     $     '            Number of photons written out=',i10)
                                !Uplost determined directly in cerenk

      write(6,2403)totalmade,heavy_made,rangelost,uplost
 2403 format(
     $     '             Total Number of photons made=',i10,/,
     $     '     Number of photons made by Heavy ions=',i10,/,
     $     '   Number of photons outside nx-ny limits=',i10,/,
     $     '     Number of photons headed up to space=',f10.0)
                                !TW mod.01/06/02 GHS V:1:2:6.3 Keep it.
      write(6,2404)itwcount
 2404 format(
     $     '    Number of Photons in area (nx=0,ny=0)=',i10)
                                !end TW mod

      if(sparse_area)then
         write(6,2402)sparse_lost
 2402    format(
     $        ' Number of photons dropped in sparse mode=',i10)
      endif
      
      write(6,1000)inext
 1000 format( '             Number of segments in Shower=',i10)
                                !SHOWER MAX
      if(photonkept.ne.0)then
         shower_max=shower_max/photonkept !Mean shower max.
         shower_max2=(shower_max2/photonkept-(shower_max**2))
         if(shower_max2.gt.0)then
            shower_max2=sqrt(shower_max2)
         else
            shower_max2=0
         endif
!         write(6,2401)'E:ID:SHWMax,Sig:Path:#',
!     1        tep,showerid,shower_max,shower_max2,totalpath,totalmade
! 2401    format(a,f14.7,i6,1x,f7.1,1x,f7.1,e20.10,i12)
      endif
      return
      END
! ***************************************************************************


      subroutine cerenk(nphotons,sangle,etazero,nmade,arearatio)
! ***************************************************************************
!        This routine generates all the photons for a segment.
! ***************************************************************************
c        Modified:

c        2/2/87 G.H.S. KASCADE assumes segments are straight with intial 
c               direction going from hstart to hend through xstart,
c               ystart.
c               Assume the track is straight(that is the chrenenkov
c               emission points lie on this same line) but the cherenkov
c               cone direction changes smoothly along the segment from
c               the initial direction to the final direction.(see also
c               3/13/87)

c        5/16/89 G.H.S
c                Redo the time of flight of the photons using the gms functions.
c                Pete's code does this also but he uses the rho functions. why?

c	25/10/93 G.H.S. V:1:0:2.0
c		For UV photons that are being created for each wavelength 
c		intervel, use NMADE as pointer in PHOTONS. Remove clearing of
c		NMADE in this routine. Store LAMBDA into PHOTONS as we make them
c		LAMBDA comes in through common block.

c        input:

c        Nphotons:number of photons to generate from this segment.(i*2)
c        sangle: Sin of Cherenkov angle.
c        etazero: Eta(N-1) to use for these photons for time of flight.
c                 will be different for visable and solar blind.
c        Nmade:Pointer in photon array for last photon created.
c	       Usually upon entry this is 0 but for Whipple(or any UV detector)
c	       it may not be.
c	 Arearatio:Probability of each individual photon hitting the detector.
c		or for LUNY its the reflectance of the mirrors. Actual position
c		on ground is needed by KASTRIG.

c        The shower consists of a number of straight segments of tracks.Charged
c        tracks are divided up into segments of length .2 radiation lengths or 
c        less. Each record describes one of these segments. 
c        These records follow the header record and are read into following:
c            xstart,ystart,hstart    Initial xy,z of segment.
c            dlstart,dmstart         Initial x,y direction cosigns of segment
c                                    dnstart can be recreated from 
c                                    dnstart=sqrt(1-dl1**2-dm1**2)
c                  all dn assummed to be positive(going down).
c            tend                    relative time at end of segment.
c            hend                    final altitude of segment
c            dlend,dmend             final direction cosigns of segment.
c            gamma                   gamma at middle of segment.
c            nspec                   particle type

	

c        Particle species codes.
c                1:Gamma
c                2:positron
c                3:electron
c                4:muon (+)
c                5:muon (-)
c                6:pion (0)
c                7:pion (+)
c                8:pion (-)
c                9:kaon (+)
c               10:kaon (-)
c               11:kaon (0long)
c               12:kaon (0short)
c               13:proton
c               14:neutron
c               15:neutrino(electron)
c               16:anti-neutrino(electron)
c               17:neutrino(muon)
c               18:anti-neutrino(muon)
c		>20: Heavy ion of A=ITYPE-20

c        The resulting cherenkov photons are stored in the array photons
c        elements of photons
c        photons(1,i),photons(2,i): X,Y (meters)at hobs of photon i.
c        photons(3,i),photons(4,i),photons(5,i):direction cosigns of photon i.
c        photons(6,i):Time of arrival of photon i at hobs.
c	 photons(7,i):Wavelength of photon in nm.Added 25/10/93 G.H.S. V:1:0:2.0


c                Stuff for finding direction cosigns of emitted photons.
      integer*2 nphotons
      dimension a(9),b(3),r9(3)
      equivalence (dl1,r9(1)),(dm1,r9(2)),(dn1,r9(3))
      include 'kaslite.h'
      qq(p,u)=sqrt(p*p+u*u)

      dns=sqrt(1.-segment.dlstart**2-segment.dmstart**2) !Form the dn's
      dnf=sqrt(1.-segment.dlend**2-segment.dmend**2)

c        Length of segment.
      slength=(segment.hstart-segment.hend)/dns

c        Get the cos of the cherenkov angle.  SANGlE is the sin.
      b(3)=sqrt(1.-sangle**2)

      tprimary=(segment.hend-hobs)/(dni*c_light) 
		   !Time for a primary to hit hobs from end of segment.

c Velocity in m/nsec of emitting particle.
      velocity=(c_light*(1.-1./(2.*segment.gamma**2))) 

      do 100 i=1,nphotons       !loop over photons.
        
c	See if this photon would hit a mirror
         if(arearatio.lt.1.0)then
            if(pran(dummy).gt.arearatio)then
               mountlost=mountlost+1
               goto 100         !Missed:Drop it.
            endif
         endif
                                !Hits!
         nmade=nmade+1
c        Generate the track of this photon.
c        Get fractional position down the segment from the beginnig of
c        the segment.
         sn=pran(dummy)

c        get unit vector in segment direction interpolated between the begging
c        and endign directions.


			          !doesn't change much.
         dlp=sn*(segment.dlend-segment.dlstart)+segment.dlstart
         dmp=sn*(segment.dmend-segment.dmstart)+segment.dmstart        
         dnp=sn*(dnf-dns)+dns
c        Kascade assumes segments are straight with intial direction
c        going from hstart to hend through starting at xstart,ystart. 
c        CERENK will also.
c        assume the track is straight(that is the chrenenkov
c        emission points lie on this same line) but the cherenkov
c        cone direction changes smoothly along the segment from
c        the initial direction to the final direction.

         vlength=sqrt(dlp**2+dmp**2+dnp**2) !Get length of starting vector
         
         dlp=dlp/vlength        !normalize unit vector
                                !(Just being careful here.)
         dmp=dmp/vlength
         dnp=dnp/vlength        !d*p is segment unit vector at its start.
c                                Good enough for path length.
        
c                Set up arrays for finding photon directions.
         f=qq(dlp,dmp)
         a(1)=dlp*dnp/f
         a(2)=-dmp/f
         a(3)=dlp
         a(4)=dmp*dnp/f
         a(5)=dlp/f
         a(6)=dmp
         a(7)=-f
         a(8)=0.
         a(9)=dnp
c     | a(1) a(2) a(3) |  | b(1) |  |r9(1) |
c     | a(4) a(5) a(6) | x| b(2) |= |r9(2) |
c     | a(7) a(8) a(9) |  | b(3) |  |r9(3) |
c     vector b is new direction in frame of
c     track

c     vector a(3);a(6);a(9)  is track in lab
c     frame  -v

c     vector a(2);a(5);. is vector product
c     vxz normalized (mod=1)

         rk=twopi*pran(dummy)   !sample cos(phi) and sin(phi)
         b(1)=cos(rk)*sangle
         b(2)=sin(rk)*sangle

        
         call mtxmlt(a,b,r9)    !multiply matrix.

c        Normalize result.
         vlength=sqrt(dl1**2+dm1**2+dn1**2)

c        10/24/86 G.H.S. Only earth seeking photons wanted!
         if(dn1.lt.0.)then
            nmade=nmade-1
            uplost=uplost+1
            return
         endif


         dl1=dl1/vlength        !save direction cosigns of this photon.
         dm1=dm1/vlength
         dn1=dn1/vlength        !d*1 is photon unit vector
         photons(3,nmade)=dl1   !Save in photon vector.
         photons(4,nmade)=dm1
         photons(5,nmade)=dn1

c        get x,y,z of emission point(note that the z axis has been negated)
        
c        3/13/87 G.H.S. Change sign below so that snl increases DOWN the track.
c        and starts at the start of the segment. This fixes bad x,y of photon
c        emission problem.

         snl=slength*(sn)       !length of vector from segment start
                                !to point on segment of photon emission.
                                !zz,xx,yy are coord of photon emission.
         zz=segment.hstart-snl*dns !- sign due to z axis negation
         xx=segment.xstart+snl*segment.dlstart
         yy=segment.ystart+snl*segment.dmstart 


c        get cord where photon hits ground.

         dheight=(zz-hobs)
c     Altitude of photon emission point above ground
         path=dheight/dn1       !distance photon takes to get to ground.

         photons(1,nmade)=xx+path*dl1
         photons(2,nmade)=yy+path*dm1
c                                photons(1,made),photons(2,nmade),hobs
c                                are coordinates of where photon hits!

c        G.H.S. 5/18/89
c                 New time of flight for photon.  Pets stuff is different and
c                uses the rho functions. I don't understand why.
         tz=dheight/(c_light)        
c                                verticle flight time(as if photon was coming 
c                                from zenith at speed c.
         tzcorrect=etazero*(tzcobs-gms(zz)*ftzcobs)
c                                             !Correction due to varience of
c                                             !index of refration with altitude

         tphoton=(tz+tzcorrect)/dn1 !time for photon to move from
c                                             emmision point to hobs (in nsec)
         ttrack=(slength-snl)/velocity
c                                  !Time for particle to get from emmision
c                                  !point to end of segment(where tend takes
c                                  !effect).
         photons(6,nmade)=(segment.tend-ttrack)+tphoton-tprimary
         
         photons(7,nmade)=lambda !25/10/93 G.H.S. V:1:0:2.0
						!Save wavelength of this photon
						!Mostly for UV detectors, 
						!otherwise its 0.
         photons(8,nmade)=zz	!Save emission alitiude

 100  continue
      return
      end
        


      function gauss(x)
c	modified:
c	2/4/92 G.H.S. V:1:0:1.2
c		Fix width of GAUSS. It was .6932. Make it 1.0 so nfluct has
c		correct width.

c	NOTE: This funciton only gives values out to 3/.6932=4.33 sigma and 
c	above 3 sigma its not perfect(but its not bad!).

c     This is petes for use by nfluct.
      sum=pran(dummy)
      do 100 i=1,5
         sum=sum+pran(dummy)
 100  continue
      gauss=(sum-3.)/.6932
      return
      end
!******************************************************************************


      subroutine mass_number2charge_mass(ia,qz,xmass)
!******************************************************************************
!   Determine charge Z of most stable nuclei of nuclear number A
!   Determine mass from Z and A using semi-empirical mass formula
!******************************************************************************
!   Uses fromulae from "Physics of Nuclei and Particles, Vol 1", Marmier and 
!   Sheldon,1969, Acedemic Press, pg 36-38, formula: 2-35,2-38,2-39,2-40
!******************************************************************************
!   This is from the liquid drop model of stable nuclei.

!  Written by:
!  Glenn Sembroski
!  Physics Dept.
!  Purdue Univ.
!  W. Lafayette, IN USA 47907

! Modified:

      IMPLICIT NONE
      real a
      integer qz,ia
      real  xmass,pmass
      logical  first_argon
      
      real mp,mn,ap,av,as,ac,aa
      parameter (mp=938.256e-6) !Mass of proton (TeV)
      parameter (mn=939.550e-6) !Mass of neutron(TeV)
      parameter (ap=33.5e-6)    !Pairing coef.(TeV)
      parameter (av=14.1e-6)    !Volume coef (TeV)
      parameter (as=13.0e-6)    !Surface coef(TeV)
      parameter (ac=0.595e-6)   !Coulmb coef.(TeV)
      parameter (aa=19e-6)      !Assymetry coef(TeV)
      data first_argon /.true./


      a=ia
                                !Correct our formula for elements up to 
                                !a=56(Fe)
                                !which is as high as NUC_LIB goes.
      if(ia==18)then            !Force Oxygen isotope
         qz=8
      elseif(ia==24)then
         qz=12                  !Force Magnesium
      elseif(ia==28)then
         qz=14                  !Force silicon
      elseif(ia==32)then
         qz=16                  !Force Sulpher
      elseif(ia==33)then
         qz=16                  !Force Sulpher
      elseif(ia==35)then
         qz=17                  !Force Chlorine
      elseif(ia==39)then
         qz=19                  !Force Potassium
      elseif(ia==40)then
         qz=18                  !Force Argon !Could have been calcium 40.
         if(first_argon)then
            print*,'Warning--Forcing Argon for all atomic masses of 40'
            first_argon=.false.
         endif
      elseif(a==56)then
         qz=26                  !Force Iron.
      else
         qz=anint(a/(1.98+0.0155*(a**(2./3.)))) !Use nearest integer 
                                !function
      endif

!First determine pairing mass term
      if(mod(qz,2)==0)then
         if(mod(ia,2)==0)then
            pmass=-ap*a**(-.75) !even-even nuclei
         else
            pmass=0.            !even-odd nuclei
         endif
      else
         if(mod(ia,2)==0)then
            pmass=0.            !Odd-even nuclei
         else
            pmass=ap*a**(-.75)  !Odd-odd  nuclei
         endif
      endif
      
      xmass = qz*mp + (a-qz)*mn - av*a + as*(a**(2./3.)) +
     $     ac*(qz**2)/(a**(1./3.)) + aa*((a-2*qz)**2)/a + pmass
      return
      end
!******************************************************************************

      subroutine mtxmlt(a,b,r)
!******************************************************************************
!        Calculates r=Ab, where r,b are vectors and A is matrix, all 3-d.
!******************************************************************************
      dimension a(9),b(3),r(3)
      do i=1,3
         r(i)=0.
         ii=3*(i-1)
         do j=1,3
            ij=ii+j
            r(i)=a(ij)*b(j)+r(i)
         end do
      end do
      return
      end


      function nfluct(x)
!****************************************************************************
c       Puts a statistical fluctuation on number of pe's(photons)
!****************************************************************************
c       written by:  Pete Palfrey
c                    Purdue
c                    4/1/89

!	Modified:
!	11/5/98 GHS V:1:1:5.3
!		Fix bug in NFLUCT. For small values of x (x<12) check for
!		values of nfluct out to 18. before we were limited to 4*x.
!		This was especially bad for x<.25. This error became very 
!		noticeable when we went to segment step sizes of .02 radiation
!		lengths.

c       See if fluctuation will be poisson.
      if(x.lt.12.) then         !Poisson
         sum=exp(-x)
         old=sum
         test=pran(dummy)
!	11/5/98 GHS V:1:1:5.3 Let imax always be 18.
!                imax=min(4.*x,18.)
         imax=18
         do i=1,imax
            if(sum.gt.test) then
               nfluct=i-1
               return
            end if
            xx=i
            old=old*x/xx
            sum=sum+old
         end do
         nfluct=imax
c     Fluctuation is gaussian distributed.
      else
         sca=sqrt(x)*gauss(x) + x
         if(sca.gt.0.) then
            nfluct=nint(sca)
         else
            nfluct=0
         end if
      end if
      return
      end
!***************************************************************************



      
      subroutine rateinit
!****************************************************************************
!       Calculates all the constants need to calculate the
!      different production rate of Cherenkov photons and resultant PE's.
!****************************************************************************

!       The formalism for all this comes from: Physics of Nuclei and Particles,
!       Marmier and Sheldon, Academic Press, 1969 pgs(190-197)

c     Written: Glenn Sembroski
c              Purdue
c              4/20/89

c     Modified:

c	25/10/93 G.H.S. V:1:0:2.0
c		This new version of KASLITE moves the Number of photons/segment
c		calculation here from KASCADE. This allows among other things
c		us to tag each photon with a wavelength. This means the
c		structure of the PE file will now change(yes Virginia we will 
c		now user structures).  For now we will include all the code 
c		from KASCADE but this is mainly for calculating Whipple 10m
c		showers.
c		Include 10M PMT response and mirror reflectivities(from
c		Schubnell).

c	23/8/94 GHS V:1:0:3.0
c		Use Adrian's weathered PMT quantum eff for the  R1398HA pmt for
c		the whipple 10m.
c		Add use of new 10m reflectivity measurements(clean mirrors).
c		Add use of Vicanti provided atm extintion. Changes to far uv
c		mostly.

c	10/10/95 GHS V:1:0:3.0
c		Add stuff for the 'SL' (Trevers Argentinia Search light array)

c	20/10/95 GHS V:1:0:3.0
c		Add reflect_sl. Argentine search light reflectivity table. 
c		derived form graph of reflectivity of pure rhodium, degraded in
c		a wavelength dependent manner to accoutn for mirros being dirty.
c		Dirtyness dependency determined from measured values from Adrian
c		sent to Trever in an email on this date.

c	24/10/95 GHS V:1:0:3.0
c		Add quatum efficnecy table for RCA4518(from graph of rca4522,
c		Trever says they are the same.) for Argentine Search lights
c		array.

c	15/5/97 GHS V:1:1:4.1
c		Put a lower limit of 1.e-30 on ATMPROB.  Set to 0.0 when
c		less then this value. This prevents UNIX underflows on SUN.

!       18/03/04 GHS V:1:3:8.2
!               RATEINIT: Print out which PMT,MIrror/filter used

!       06/12/04 GHS V:1:3:8.3
!               Add BENCHMARK_FLAG to determine whether to use 1.0 for QE and
!               mirror reflectvivity. Defaults to False (use telescope type 
!               specified QE and mirror reflectivity tables.
!               (Whipple and Veritas only)
!      17/02/05 GHS  V:1:3.8.7
!               Add new veritas PMT Q.E. table for Photonis xp2970eff.  
!               Use this for VERITAS.
!               Remove obsolete code for some theextranious detectors. Keep 
!               only Whipple 10m and VERITAS 12m

	real extint(0:50,0:104)
        real R821eff(0:104)
        real xp2970eff(0:104)
        real r1450eff(0:104)
	real emi9870beff(0:104)
	real  r1398eff(0:104),R1398HA_UV(5,0:104)
	real  adp(0:104)
        real rca4518(0:104)
	real hoyau340(0:104)
        real hoyab370(0:104)
	real hoyab390(0:104)
	real asgateff(0:104)
	real trans_100(0:104)
	REAL*4	REFLect_10M(0:104)
	REAL*4	REFLect_10M_1993(0:104)
        real reflect(0:104)
	real reflect_sl(0:104)
        !include 'kaslite_command_line.h'
	include 'kaslite.h'

c     Spectral response array for the Hamamatsu R821 pmt.
c     Taken from Hamamatsu plot labled 'R821'.
c     Wave lengths are from 180 nanometers to 700 nanometers in
c     5 nanometer steps.
        data r821eff/.132,.139,.146,.153,.16,.157,.154,.151,.148,.145,
     1 .137,.129,.121,.113,.105,.096,.087,.078,.069,.06,.049,.038,.027,
     2 .016,.005,.004,.003,.002,.001,.0001,75*0./

c     Spectral response array for the Hamamatsu R1450 pmt.
c     Taken from Hamamatsu plot labled 'E678-12D(supplied)'.
c     This number(E678-12d) is that of a base but the plot looks
c     correct.  Wave lengths are from 180 nanometers to 700 nanometers in
c     5 nanometer steps.
        data r1450eff/18*0.,.0006,.001,.0015,.004,.009,.015,.02,.05,.09,
     1 .11,.15,.17,.20,.21,.22,.23,.24,.25,.26,.27,.275,.285,.29,.29,
     2 .29,.29,.285,.280,.272,.264,.256,.25,.246,.242,.238,.234,.23,
     3 .224,.218,.212,.206,.2,.19,.18,.17,.16,.15,.14,.13,.12,.115,.11,
     4 .106,.102,.098,.094,.09,.082,.075,.068,.061,.054,.049,.044,.040,
     5 .036,.032,.029,.025,.021,.017,.013,.011,.009,.007,.006,.005,
     6 .0043,.0036,.0029,.0022,.0015,.0013,.0011,.0008,.0006,.0004/


!Quantum efficiency from XP2970 from John Finley and Philips (180-700nm in 5nm)
!This is the VERITAS PMT: From Stephen Fegan.
        data xp2970eff/0.00013,0.00031,0.00076,0.00187,0.00457,0.00988,
     1       0.01862,0.02878,0.04000,0.05028,0.06007,0.06766,0.07747,0.08500,
     2       0.09181,0.09758,0.10355,0.11044,0.11792,0.12552,0.13207,0.13625,
     3       0.13990,0.14435,0.15012,0.15384,0.15757,0.16130,0.16503,0.16828,
     4       0.17153,0.17478,0.17803,0.17932,0.18062,0.18191,0.18321,0.18342,
     5       0.18363,0.18384,0.18405,0.18548,0.18690,0.18832,0.18974,0.18977,
     6       0.18981,0.18984,0.18988,0.18823,0.18658,0.18492,0.18327,0.17999,
     7       0.17670,0.17342,0.17014,0.16527,0.16040,0.15554,0.15067,0.14627,
     8       0.14187,0.13747,0.13307,0.12791,0.12276,0.11760,0.11245,0.10293,
     9       0.09342,0.08391,0.07439,0.06583,0.05726,0.04869,0.04013,0.02957,
     1       0.02560,0.02187,0.01865,0.01576,0.01324,0.01076,0.00881,0.00693,
     2       0.00567,0.00425,0.00336,0.00266,0.00194,0.00143,0.00107,0.00081,
     3       0.00060,0.00045,0.00034,0.00025,0.00019,0.00014,0.00011,0.00008,
     4       0.00006,0.00005,0.00003/ 


c     Spectral response array for the EMI 9870b pmt.
c     Taken from old table Pete had for HPW. This is a bialkali tube
c	and so should behave like the R1450. It does have thicker glass.
c	It seems to scale by very roughly a factor of 2 to the R1450.
c 	Wave lengths are from 180 nanometers to 700 nanometers in
c     5 nanometer steps.
	data emi9870beff/ 21*0.0,0.0134,0.0268,0.0402,
	2 0.0536,0.0641,0.0746,0.0851,0.0956,0.1009,
	3 0.1062,0.1114,0.1167,0.1184,0.1201,0.1218,
	4 0.1235,0.1249,0.1264,0.1278,0.1292,0.1289,
	5 0.1286,0.1284,0.1281,0.1265,0.1249,0.1232,
	6 0.1216,0.1192,0.1168,0.1144,0.1120,0.1089,
	7 0.1058,0.1026,0.0995,0.0962,0.0928,0.0895,
	8 0.0861,0.0823,0.0785,0.0746,0.0708,0.0668,
	9 0.0627,0.0587,0.0546,0.0505,0.0465,0.0424,
	1 0.0383,0.0342,0.0302,0.0261,0.0221,0.0197,
	2 0.0173,0.0149,0.0125,0.0111,0.0097,0.0082,
	3 0.0068,0.0061,0.0053,0.0046,0.0038,0.0036,
	4 0.0034,0.0032,0.0030,0.0025,0.0020,0.0014,
	5 0.0009,0.0007,0.0005,0.0004,0.0002,0.0001,
	6 0.0001,0.0000,0.0000/

c	Spectral response array for the Hamamatsu R1398 pmt. This is the visible
c	1.125" tube for the Whipple HRC. 
c	Quantum eff. for the R1398HA (with UV window) weathered whipple tubes.
	data  R1398HA_UV/
	1 180.,  0.034,  0.006,  0.007,  0.006,				!GHS
	1 185.,  0.045,  0.012,  0.013,  0.012,				!GHS
	1 190.,  0.062,  0.018,  0.019,  0.018,
	1 195.,  0.076,  0.024,  0.025,  0.024, !Quantum efficiency for 
	1 200.,  0.090,  0.029,  0.031,  0.030,	!Hamamatsu
	1 205.,  0.108,  0.035,  0.038,  0.037,	!R1398 HA (UV window)
	1 210.,  0.125,  0.041,  0.044,  0.043,	!col 1 = lambda in nm
	1 215.,  0.143,  0.057,  0.058,  0.058,	!col 2 = typical values (given
	1 220.,  0.160,  0.073,  0.072,  0.073,	!by Hamamatsu)
	1 225.,  0.178,  0.095,  0.091,  0.093,	!col 3 = measured values for 
	1 230.,  0.187,  0.118,  0.109,  0.113,	!tube serial # LA5163
	1 235.,  0.197,  0.132,  0.122,  0.127,	!col 4 = measured values for 
	1 240.,  0.206,  0.146,  0.134,  0.140,	!tube serial # LA5420
	1 245.,  0.216,  0.154,  0.142,  0.148,	!col 5 = average between col 3
	1 250.,  0.225,  0.162,  0.151,  0.156,	!and 4
	1 255.,  0.230,  0.167,  0.157,  0.162,	!Measurements have been done by
	1 260.,  0.235,  0.172,  0.164,  0.168,	!Hamamatsu in July, 1994.
	1 265.,  0.239,  0.173,  0.166,  0.170,	!Values given by Hamamatsu were
	1 270.,  0.244,  0.175,  0.169,  0.172,	!from 200 to 700 nm, in 10 nm 
	1 275.,  0.249,  0.183,  0.176,  0.179,	!steps.  I have extrapolated 
	1 280.,  0.253,  0.190,  0.183,  0.186,	!and also from 200 to 190 nm.
	1 285.,  0.256,  0.196,  0.189,  0.192,	!between values.
	1 290.,  0.260,  0.202,  0.195,  0.198,	! Adrian Rovero.
	1 295.,  0.263,  0.206,  0.199,  0.203,
	1 300.,  0.267,  0.210,  0.203,  0.207,	!I added extrapolated values
	1 305.,  0.267,  0.212,  0.206,  0.209,	!for 180,185 nm.
	1 310.,  0.267,  0.215,  0.208,  0.211,	! Glenn Sembroski
	1 315.,  0.267,  0.219,  0.212,  0.216,	
	1 320.,  0.267,  0.223,  0.217,  0.220,
	1 325.,  0.267,  0.226,  0.221,  0.224,
	1 330.,  0.269,  0.229,  0.226,  0.228,
	1 335.,  0.271,  0.235,  0.232,  0.233,
	1 340.,  0.272,  0.240,  0.238,  0.239,
	1 345.,  0.274,  0.240,  0.237,  0.239,
	1 350.,  0.276,  0.240,  0.236,  0.238,
	1 355.,  0.278,  0.239,  0.235,  0.237,
	1 360.,  0.280,  0.237,  0.233,  0.235,
	1 365.,  0.282,  0.238,  0.234,  0.236,
	1 370.,  0.284,  0.239,  0.235,  0.237,
	1 375.,  0.286,  0.241,  0.237,  0.239,
	1 380.,  0.286,  0.243,  0.238,  0.241,
	1 385.,  0.286,  0.241,  0.237,  0.239,
	1 390.,  0.286,  0.239,  0.235,  0.237,
	1 395.,  0.286,  0.239,  0.236,  0.237,
	1 400.,  0.286,  0.239,  0.236,  0.238,
	1 405.,  0.280,  0.238,  0.236,  0.237,
	1 410.,  0.275,  0.238,  0.235,  0.236,
	1 415.,  0.269,  0.237,  0.234,  0.235,
	1 420.,  0.264,  0.236,  0.233,  0.234,
	1 425.,  0.258,  0.234,  0.231,  0.233,
	1 430.,  0.253,  0.232,  0.230,  0.231,
	1 435.,  0.248,  0.230,  0.228,  0.229,
	1 440.,  0.243,  0.228,  0.226,  0.227,
	1 445.,  0.238,  0.224,  0.221,  0.223,
	1 450.,  0.233,  0.220,  0.216,  0.218,
	1 455.,  0.226,  0.217,  0.213,  0.215,
	1 460.,  0.220,  0.214,  0.209,  0.212,
	1 465.,  0.213,  0.209,  0.204,  0.206,
	1 470.,  0.207,  0.204,  0.198,  0.201,
	1 475.,  0.200,  0.198,  0.193,  0.195,
	1 480.,  0.196,  0.193,  0.187,  0.190,
	1 485.,  0.191,  0.186,  0.180,  0.183,
	1 490.,  0.187,  0.180,  0.174,  0.177,
	1 495.,  0.182,  0.173,  0.167,  0.170,
	1 500.,  0.178,  0.167,  0.160,  0.163,
	1 505.,  0.170,  0.159,  0.153,  0.156,
	1 510.,  0.163,  0.152,  0.146,  0.149,
	1 515.,  0.155,  0.146,  0.140,  0.143,
	1 520.,  0.148,  0.140,  0.134,  0.137,
	1 525.,  0.140,  0.135,  0.129,  0.132,
	1 530.,  0.132,  0.130,  0.125,  0.128,
	1 535.,  0.124,  0.126,  0.120,  0.123,
	1 540.,  0.116,  0.122,  0.116,  0.119,
	1 545.,  0.108,  0.117,  0.110,  0.114,
	1 550.,  0.100,  0.112,  0.104,  0.108,
	1 555.,  0.094,  0.104,  0.094,  0.099,
	1 560.,  0.088,  0.095,  0.084,  0.090,
	1 565.,  0.082,  0.084,  0.074,  0.079,
	1 570.,  0.076,  0.073,  0.063,  0.067,
	1 575.,  0.070,  0.065,  0.056,  0.060,
	1 580.,  0.065,  0.056,  0.050,  0.053,
	1 585.,  0.060,  0.051,  0.046,  0.048,
	1 590.,  0.055,  0.046,  0.041,  0.044,
	1 595.,  0.050,  0.042,  0.038,  0.040,
	1 600.,  0.045,  0.039,  0.034,  0.036,
	1 605.,  0.041,  0.035,  0.031,  0.033,
	1 610.,  0.037,  0.032,  0.028,  0.030,
	1 615.,  0.032,  0.029,  0.025,  0.027,
	1 620.,  0.028,  0.025,  0.022,  0.024,
	1 625.,  0.024,  0.023,  0.020,  0.021,
	1 630.,  0.021,  0.020,  0.017,  0.019,
	1 635.,  0.018,  0.017,  0.015,  0.016,
	1 640.,  0.016,  0.015,  0.013,  0.014,
	1 645.,  0.013,  0.013,  0.011,  0.012,
	1 650.,  0.010,  0.011,  0.009,  0.010,
	1 655.,  0.008,  0.009,  0.008,  0.009,
	1 660.,  0.007,  0.008,  0.006,  0.007,
	1 665.,  0.005,  0.006,  0.005,  0.006,
	1 670.,  0.004,  0.005,  0.004,  0.005,
	1 675.,  0.002,  0.004,  0.004,  0.004,
	1 680.,  0.002,  0.003,  0.003,  0.003,
	1 685.,  0.001,  0.003,  0.002,  0.002,
	1 690.,  0.001,  0.002,  0.002,  0.002,
	1 695.,  0.000,  0.002,  0.001,  0.001,
	1 700.,  0.000,  0.001,  0.001,  0.001/

c	Spectral response array for the RCA4518. This is for the Argentine
c	Searchlight mirror array. Dervied from reading values from a graph
c	of quantum efficency of RCA4522 which Trever says is the same as the
c	RCA4518.
	data rca4518/5*0.,.015,.030,.050,.080,.110,.140,.165,.190, !180-240 nm
	1  .212,.225,.232,.240,.250,.260,.263,.265,.270,.275,.278, !245-295 nm
	2  .280,.283,.285,.288,.290,.293,.295,.296,.297,.297,.298, !300-350 nm
	3  .298,.298,.298,.297,.294,.290,.288,.285,.282,.280,.276, !355-405 nm
	4  .272,.269,.265,.260,.255,.248,.242,.236,.230,.223,.215, !410-460 nm
	5  .199,.192,.184,.175,.166,.157,.148,.142,.134,.125,.120, !465-515 nm
	6  .115,.100,.083,.076,.068,.0585,.051,.0445,.038,.0322,   !520-565 nm
	7  .0255,.0228,.018,.0145,.011,.0093,.0076,.0062,.0048,    !570-610 nm
	8  .0041,.0033,.0026,.0018,.0016,.0014,.0012,.0010,10*0/   !615-700 nm

c     Spectal Transmission for Hoya U340 filter.
c     Taken from Hoya's book using the tables for this filter.
	data hoyau340/14*0.,.00002,.001,.0055,.06,.117,.25,.389,.480,
	1 .613,.670,.727,.75,.772,.78,.79,.794,.798,.799,.799,.785,.755,
	2 .72,.658,.58,.466,.32,.131,.010,.022,55*0.,.00002,.0001,.0004,
	3 .0022,.004,.009,.014/

c     Spectal Transmission for Hoya B370 filter.
c     Taken from Hoya's book using the tables for this filter.
	  data hoyab370/24*0.,.0035,.04,.08,.19,.293,.41,.52,.605,.678,
     1 .7205,.763,.783,.804,.811,.815,.8135,.812,.795,.778,.747,.716,
     2 .673,.63,.574,.518,.447,.377,.309,.24,.18,.12,.084,.048,.032,
     3 .015,.0097,.0037,.0022,.0008,.0005,.0002,.00014,.00008,.00007,
     4 .00006,.000075,.00009,.00015,.0002,.00025,.0003,.0002,.0001,
     5 .00006,.00002,15*0.,.00001,.000015,.00002,.00003,.00004,.00007,
     6 .0001,.0013,.0025,.0175,.032/

c     Spectal Transmission for Hoya B390 filter.
c     Taken from Hoya's book using the tables for this filter.
	data hoyab390/25*0.,.00045,.0090,.0160,.023,.0815,.140,.237,
	1 .334,.422,.510,.57,.63,.665,.7,.721,.742,.7565,.771,.771,
	2 .771,.763,.755,.7425,.73,.7075,.685,.657,.629,.5895,.550,
	3 .502,.454,.381,.308,.231,.154,.099,.044,.026,.008,.0065,
	4 .005,.0025,.00007,.000065,.00006,.00008,.0001,.0002,.0003,
	5 .000175,.00005,.00003,.00001,.000005,17*0.,.000005,.00001,
	6 .00005,.0001,.00075,.0014,.0039,.0064/

c	PMT,Filter, and mirror transmission for ASGAT. Comes from Pete.
	data asgateff/29*0.,.0275,.055,.0775,.1,.125,.15,.165,.18,.1835,
	1 .187,.1885,.19,.1885,.187,.1835,.18,.172,.164,.152,.14,.125,
	2 .11,.09,.07,.0575,.045,.0325,.02,.0135,.007,.0035,45*0./

c	Dummy perfect (100%) transmission array for use with ASGAT to replace
c	filter and reflectance arrays in RATE_QGEN calls.
c	All values =1.0.
	data trans_100/105*1.0/


c	Wavelength dependent Reflectivity of the WHIPPLE 10 meter mirrors.
c	M. Schubnel took this from the ARTEMIS report 'UV light in the 
c	atmosphere', Fig 2 by P.Fleury et al. (Palaiseau, June 1990).  
c	(M. Schubnell 27-Sep-1993) (but see below)
	DATA	REFLect_10M/
	1   0.539,0.554,0.568,0.582,0.596,0.609,0.622,0.634,0.646,0.657,
	1   0.668,0.679,0.690,0.700,0.709,0.719,0.727,0.736,0.744,0.752,
	1   0.760,0.767,0.774,0.781,0.788,0.794,0.800,0.806,0.811,0.816,
	1   0.821,0.826,0.831,0.835,0.839,0.843,0.846,0.850,0.853,0.856,
	1   0.859,0.862,0.865,0.867,0.869,0.872,0.874,0.875,0.877,0.879,
	1   0.880,0.882,0.883,0.884,0.885,0.886,0.887,0.888,0.888,0.889,
	1   0.889,0.890,0.890,0.890,0.891,0.891,0.891,0.891,0.891,0.891,
	1   0.891,0.891,0.890,0.890,0.890,0.890,0.889,0.889,0.889,0.888,
	1   0.888,0.887,0.887,0.887,0.886,0.886,0.885,0.885,0.884,0.884,
	1   0.883,0.883,0.882,0.882,0.881,0.881,0.880,0.880,0.879,0.879,
	1   0.878,0.877,0.877,0.876,0.876/

c	Wavelength dependent Reflectivity of the WHIPPLE 10 meter mirrors.
c	Mirror reflectivity for new recoated mirror measurements taken on 
c	September, 1993. These reflectivities are for clean mirrors.
c	The best estimation for dirty mirror is 4%less than these values 
c	(substract 0.04 from these reflectivities).
c	180,185 nm interpolated values added by GHS.
c	From Adrian, August 1994.
	DATA	REFLect_10M_1993/
	1 .6230,.6370,.6510,.6655,.6800,.6965,.7130,.7370,.7610,.7845,!180-225nm
	2 .8080,.8250,.8420,.8500,.8580,.8655,.8730,.8750,.8770,.8820,!230-275nm
	3 .8870,.8870,.8870,.8875,.8880,.8905,.8930,.8936,.8942,.8935,!280-325nm
	4 .8927,.8906,.8884,.8845,.8822,.8822,.8799,.8774,.8750,.8720,!330-375nm
	5 .8690,.8658,.8626,.8592,.8558,.8519,.8481,.8443,.8405,.8365,!380-425nm
	6 .8326,.8286,.8247,.8209,.8171,.8133,.8095,.8056,.8018,.7983,!430-475nm
	7 .7949,.7917,.7885,.7855,.7825,.7800,.7776,.7758,.7740,.7725,!480-525nm
	8 .7711,.7699,.7687,.7676,.7666,.7659,.7653,.7648,.7644,.7639,!530-575nm
	9 .7635,.7634,.7633,.7632,.7631,.7632,.7634,.7635,.7636,.7637,!580-625nm
	1 .7638,.7640,.7642,.7642,.7643,.7644,.7646,.7647,.7648,.7647,!630-675nm
	2 .7646,.7647,.7647,.7646,.7645/			      !680-700nm

c      Reflectance of al in air for a while.  180-240 nanometers from G.Hass,
c      W.R. Hunter, Physics of thin films, pg.75. From the graph. 1963. 
c      245-700 nanometers from  G.Hass,J.B.heaney,W.R.Hunter ? pg 17. From the 
c      table, slightly degraded  to match that of the smaller wavelenths from 
c      the previous paper.
           data reflect/.82,.83,.84,.85,.86,.865,.87,.875,.88,.8825,
     1 .885,.8875,.89,.891,.8915,.892,3*.893,7*.894,4*.895,11*.896,
     2 6*.895,5*.894,4*.893,3*.892,3*.891,.89,.89,3*.889,3*.888,3*.887,
     3 3*.886,3*.885,3*.884,.883,3*.882,.881,.881,.88,.88,.879,.879,
     4 .878,.877,.876,.875,.874,.873,.872,.871,.870,.869,.868,.867,.866/

cSL	Wavelength dependent reflectivity of Argentine searchlight mirrors.
c	Derived from a graph of rodium(Drummeter and somebody 1964). THis
c	curve is degraded in a wavelength dependent fashion to account for the
c	mirrors being dirty and weathered. The degradation values are derived
c	from ratios of measured searchlight mirror reflectivities and rhodium 
c	by Adrian listed in an e-mail note sent to Trever 20/10/95.
	data Reflect_sl/0.305,0.313,0.320,0.328,0.336,0.344,
	1 0.352,0.361,0.370,0.379,0.388,0.397,
	2 0.406,0.416,0.425,0.435,0.445,0.455,
	3 0.465,0.475,0.485,0.495,0.505,0.516,
	4 0.526,0.537,0.548,0.556,0.565,0.574,
	5 0.583,0.592,0.601,0.607,0.613,0.620,
	6 0.626,0.632,0.639,0.645,0.652,0.655,
	7 0.658,0.661,0.664,0.667,0.670,0.673,
	8 0.675,0.678,0.681,0.684,0.687,0.690,
	9 0.693,0.694,0.696,0.697,0.699,0.700,
	1 0.702,0.703,0.705,0.706,0.708,0.709,
	2 0.710,0.712,0.713,0.715,0.716,0.718,
	3 0.719,0.721,0.722,0.724,0.725,0.726,
	4 0.728,0.729,0.730,0.732,0.733,0.734,
	5 0.736,0.737,0.738,0.740,0.741,0.742,
	6 0.744,0.745,0.746,0.748,0.749,0.750,
	7 0.752,0.753,0.754,0.756,0.757,0.759,
	8 0.760,0.761,0.763/
 

c     ADP    Wavelength dependent quantum efficiency for ADP detectors. 
c     Linier interpolation of some points given by Wei Cui. 12/10/00. He gave me 
c     points at 180 nm=.18,300 nm=.56, 366=.77, 450-700 nm=.84. 
         data adp/0.180, 0.196, 0.212, 0.228, 0.243, 0.259, 0.275, 0.291,
	1 0.307, 0.322, 0.338, 0.354, 0.370, 0.386, 0.402, 0.418,
	1 0.433, 0.449, 0.465, 0.481, 0.497, 0.512, 0.528, 0.544,
	1 0.560, 0.576, 0.592, 0.608, 0.625, 0.641, 0.657, 0.673,
	1 0.689, 0.705, 0.722, 0.738, 0.754, 0.770, 0.774, 0.778,
	1 0.782, 0.786, 0.791, 0.795, 0.799, 0.803, 0.807, 0.811,
	1 0.815, 0.819, 0.824, 0.828, 0.832, 0.836, 0.840, 0.840,
	1 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840,
	1 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840,
	1 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840,
	1 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840,
	1 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840,
	1 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840, 0.840,
	1 0.840/



c     
c       Form the ratio of density of atm as function of altitude.
      do i=0,50
              h=1000.*i        !Altitude in meters.
              rhoratio(i)=rho(h)/rho(0.)       
	enddo

c       And calculate it at exactly hobs.
       rhorat_hobs=rho(hobs)/rho(0.)       

c      Form ETASEA=(Index of refraction-1) at sea level. Start at
c      180 nanometers and go up to 700 nanometers.
c      Parameters from CRC handbook of Chem and Phy. 70'-71'
c      pg E-231.
      Do lambda=180.,700.,5.
            ilambda=lambda
            i=(ilambda-180)/5
            etasea(i)=1.e-7*(2726.43+12.288/(lambda**2*1.e-6)+
     1 0.3555/(lambda**4*1.e-12))
	enddo

c      Read in the extinction data.
!	call readex(extint)		!OLd palfrey and Sembroski derived.

	call readex_uv(extint)		!newer and better Vacanti derived.

c        Construct the atmosphere transmisson probabilities using
c        the direction of the primary as the approxamated direction of the
c        Cherenkov photons.
c        Assume here that that 1:the photons travel in direction of segment,
c        2:That the segments are close enough to direction of primary that we
c        can use the primaryies direction for the ATMPROB calculation.
c        This last assumption is a bad one and will be approximatly corrected 
c        in the RATE_GEN and RATE_UV_GEN routines.
c        This correction factor as a function of lambda is:
c        (atmbrob(j,i))**(dni/dn-1).
c        We will approximate its effect by just using the correction at the
c        central lambda of the tube/filter combination.

        ihobs=hobs/1000.        !Altitude index just below Obs altitude.
        do lambda=180,700,5         !In nanometers
               ilambda=lambda
               i=(ilambda-180)/5
c                                 !get linearly interpolated tau for obs height.
               tlow=(extint(ihobs+1,i)-extint(ihobs,i))*
     1 (hobs/1000.-ihobs)+extint(ihobs,i)

               do j=ihobs+1,50     !Start above the obs altitude.
c                  Transmission prob.
			atmprob(i,j)=exp(-(extint(j,i)-tlow)/dni)
						!15/5/97 GHS V:1:1:4.1
                       if(atmprob(i,j).lt.1.e-30)then
				atmprob(i,j)=0.0	!Prevents underflows
							!in qeta/qgamma
							!calculations.
			endif
		enddo
	enddo


c        Now construct the 'Q' integrels for calculating number of Photons
c        that hit the ground for 5 nanometer steps. This is in some sense
c        a differential spectrum.
c        Note that there is no tube eff or filter xmission terms in this.
c        We want the 'Q' as functions of both Lambda and altitude.

c        Number of photons on ground is thus N(lambda)=
c        ((h1-h2)/dn))*(qeta-qgamma/gamma**2)


c        Constant
         constant=twopi/137

         do j=ihobs+1,50            !Start just above obs alt.
               do lambda=180,700,5         !In nanometers
                      ilambda=lambda
                      i=(ilambda-180)/5
c                   Calculate the Q integrels.  Units are photons/meter.
                      Qeta(i,j)=(1.d+9)*(constant*2*rhoratio(j)*
     1 etasea(i)*(1./(lambda-2.5)-1./(lambda+2.5)))*atmprob(i,j)
                      Qgamma(i,j)=(1.d+9)*atmprob(i,j)*constant*
     1 (1./(lambda-2.5)-1./(lambda+2.5))
		enddo
	enddo

c	Calculate these at hobs:
       do lambda=180,700,5         !In nanometers
             ilambda=lambda
             i=(ilambda-180)/5
c                   Calculate the Q integrels.  Units are photons/meter.
             Qeta_hobs(i)=(1.e+9)*(constant*2*rhorat_hobs*
     1 etasea(i)*(1./(lambda-2.5)-1./(lambda+2.5)))
                      Qgamma_hobs(i)=(1.e+9)*constant*
     1 (1./(lambda-2.5)-1./(lambda+2.5))
	enddo


c	Calculate visable gamma threshold table, and chernkov angle table.
c	Use etasea at lambda=380.
c       Chrenkov angle will be sin(theta)=sqrt(setavis-1/gamma**2)
         i380=(380.-180.)/5.          !Index to reference lambda
         do j=0,50
                 gamvis(j)=sqrt(1./(2*rhoratio(j)*etasea(i380)))
                 setavis(j)=2*rhoratio(j)*etasea(i380)
	enddo


c	Construct lookup tables ofr rate calculations for different detectors.
c	Used in calls to RATE_GEN

cVisable.
c	Make up the lookup tabels for Visable(26 mirror Haleakala)
	call rate_qgen(r1450eff,hoyau340,reflect,qetavis,qgamvis,
	1 qetavis_hobs,qgamvis_hobs)

c	Construct lookup tables for  rate calculations for different detectors
c	sensitive to far UV.  Used in calls to RATE_UV_GEN

c	The integrated q's for detectors that are sensitive to lambdas below
c	270 nm will need to be lambda(max) dependent since for every gamma we 
c	will have some lambda max where q1-q2 goes negative and we should stop
c	the integration. For most gamma's this is beyond 700 nanometers.

c	Another way of saying this is that this is needed since for far uv 
c	light, eta(index of refraction) is wavelength dependent
c	which means the Cherenkov threhold is also wavelength dependents as
c	is Cherenkov angle. So just precalculate some things here.

c        Gamma threshold and sin**2 of maximum chrenkov angle.
         do lambda=180,700,5
                ilambda=lambda
                i=(ilambda-180)/5
                do j=0,50
                      gammin(i,j)=sqrt(1./(2*rhoratio(j)*etasea(i)))
                      smaxsb(i,j)=2*rhoratio(j)*etasea(i)
		enddo
	enddo


!***************************************************************************
c       ADP
!***************************************************************************
c	No filter, use dummy array(100% transmission) TRANS_100
c	Build efficiency array. use average weathered results.
	if(ADPPMTs)then
           Print*,' Using ADP quantum efficincy'
           call rate_qgen_uv(ADP,trans_100,REFLect_10M_1993,qeta10m,
	1 qgam10m,qeta10m_hobs,qgam10m_hobs)

!***************************************************************************
cWhipple 10m
	elseif(WhipplePMTs)then
        print*,'KASLITE-UV PMTS(Whipple) using for cherenkov pe gen:'
        print*,'     QE:A.Rovero 1994 R1398HA (with UV window) weathered PMTS'
        print*,'     No filter used'
        print*,'     Mirror Reflec:Recoated Clean WHIPPLE 10 meter Sept-93'
            do i=0,104
               r1398eff(i)=R1398HA_UV(5,i)
            enddo
            call rate_qgen_uv(R1398EFF,trans_100,REFLect_10M_1993,qeta10m,
     1           qgam10m,qeta10m_hobs,qgam10m_hobs)


c***************************************************************************
cVERITAS 12m
        elseif(VeritasPMTs)then

c     ************************************************************************
c     Benchmark mode
c     ***********************************************************************
          if(benchmark_flag) then
        print*,'KASLITE-UV PMTS(Veritas) using for cherenkov pe gen:'
        print*,'Benchmark Mode:'
        print*,'      QE:1.0 all lambda'
        print*,'      No filter used'
        print*,'      Mirror Reflec:1.0 all lambda'
                                             ! Trans_100 is 100% (1.0) array
              call rate_qgen_uv(trans_100,trans_100,trans_100,qeta10m,
     1      qgam10m,qeta10m_hobs,qgam10m_hobs)

c      ***********************************************************************
c      normal VERITAS mode
c      ********************************************************************
          else
        print*,'KASLITE-UV PMTS(Veritas) using for cherenkov pe gen:'
        print*,'     QE:xp2970 JFinley/S.Fegan 2005 Measured VERITAS pmts'
        print*,'     No filter used'
        print*,'     Mirror Reflec:Recoated Clean WHIPPLE 10 meter Sept-93'
            call rate_qgen_uv(xp2970eff,trans_100,REFLect_10M_1993,qeta10m,
     1           qgam10m,qeta10m_hobs,qgam10m_hobs)
c     ************************************************************************
         endif
      else
         print*,' Fatal-KASLIT--RateINIT-Invalid petype:',petype
         stop 'Fatal Error-KASLITE-Invalid PETYPE'
      endif

      return
      end
c     ***********************************************************************


      subroutine rate_gen(height,dns,qeta_gen,qgam_gen,qetagen_hobs,
	1 qgamgen_hobs,rate,qz)
!*****************************************************************************
!     This is a generic routine_ that calculates the #/meter of path length
!	of Chernkov photons for a segment at altitude Height(meters),
!	with velocity gamma, using the specified q_eta and q_gamma.  
!*****************************************************************************
c	25/10/93 G.H.S. V:1:0:2.0
!       The guts of the calculation has already been done in routine_ 
!       RATE_INIT.RATE is the number of Cherenkov photo-electrons a segment 
!       would produce at hobs. This code is for detectors sensitive in the 
!       'VISABLE' range, 270nm to 700nm.

c	Emission angle.
c        For rate calculation purposes it was approximated in RATEINIT that 
c	1:the photons travel in direction of the segment
c	2:That the segments are close enough to direction of 
c        the primary that we can use the primaries direction for the ATMPROB 
c        calculation. 
c	This last assumption is a bad one (especiall at 45 deg)
c        and can be approximatly corrected by appling the correction factor:
c        (atmbrob(j,i))**(dni/dn-1).
c        Since this correction factor is fairly constant for visable photons
c	 as a function of lambda we can approximate it by just using the value
c        at the central lambda: aribitrarily set at 380 nanometers.
!*****************************************************************************


c	Modified:

c	20/1/97 GHS V:1:1:4.0
c		Convert to use of structures for input segment file.
c		Remove direct accsee format for input segment file. Use
c		unformatted sequential instead.Note that this program will be
c		incompatable with segment files made under KASCADE V:1:0
c		Replace routine VACUSERINI with FILEOPEN.
c		Remove all referneces to old rate variables. see '*' comments
!      08/12/04 GHS  V:1:3:8.5
!               Altitude bug: rate_uv_gen,rate_gen:extint table only goes 
!               up to 50 km.  Make it that when we are above 50 km its ok (we 
!               don't crash) but we don't produce any light(rate=0)!
!*****************************************************************************


	real qeta_gen(0:50),qgam_gen(0:50),qetagen_hobs,qgamgen_hobs
	integer qz
        include 'kaslite.h'


	if(iheight.gt.49)then
                                !Flag that were too high for this routine
           rate=0.              !(extinct table only goes up to 50 km)
           return               !ie we dont produce light above 50 km
        endif

        iheight=height/1000.
        hint=(height/1000.-iheight)      !Interpolation factor.

c      Interpolate the correction factor for the segment not being in the
c      direction of the primary.
	atexp=(dni-dns)/dns
	ahigh=atmprob(i380,iheight+1)**atexp


c       Interpolate the Q's
	if(iheight.gt.ihobs)then
                qetaint=qeta_gen(iheight)+(qeta_gen(iheight+1)-
     1 qeta_gen(iheight))* hint
                qgamint=qgam_gen(iheight)+(qgam_gen(iheight+1)-
     1 qgam_gen(iheight))* hint
        alow=atmprob(i380,iheight)**atexp
	aint=alow+(ahigh-alow)*hint	!This should be within 10% of 1.

        else
c                     Just above hobs. Use special qeta,qgamma.
                 hi=(height-hobs)/((iheight+1)*1000.-hobs)
                qetaint=qetagen_hobs+(qeta_gen(iheight+1)-
     1 qetagen_hobs)* hi
                qgamint=qgamgen_hobs+(qgam_gen(iheight+1)-
     1 qgamgen_hobs)* hi
        alow=1.                    !Always 1. at hobs.
	aint=alow+(ahigh-alow)*hi	!This should be within 10% of 1.
        endif

        rate=qetaint-(qgamint/(segment.gamma**2))
c      correct the rate.
       rate=rate*aint

!	11/07/01 GHS: Correct rate for charge
	rate=rate*(qz**2)
        return
        end

	subroutine rate_qgen(pmteff,filter,reflection,qeta_gen,qgam_gen,
	1 qetagen_hobs,qgamgen_hobs)

c	25/10/93 G.H.S. V:1:0:2.0
c	Generate integrated Q's for pmt efficiency PMTEFF, filter transmission 
c	FILTER and mirror reflectivity REFLECTION. This is for 
c	visable light (270-700 nanometers) where etasea is constant. Gamma 
c	threshold is then just altitude dependent not lambda.

c     Written: Glenn Sembroski
c              Purdue
c		25/10/93

	real reflection(0:104),filter(0:104),pmteff(0:104),
	1 qeta_gen(0:50),qgam_gen(0:50),
	2 qetagen_hobs,qgamgen_hobs
	include 'kaslite.h'

c        These will be the true integral 'Q's. They are integrated from
c        270 to 700 nanometer. Use the 'Q's calculated in RATEINIT to do this.
c        Start at just above hobs.
         do j=ihobs+1,50
		qeta_gen(j)=0	!Init some things.
		qgam_gen(j)=0
                do lambda=270,700,5
                      ilambda=lambda
                      i=(ilambda-180)/5
                      qe=qeta(i,j)*pmteff(i)*filter(i)*reflection(i)
                      qeta_gen(j)=qeta_gen(j)+qe
                      qg=qgamma(i,j)*pmteff(i)*filter(i)*reflection(i)
                      qgam_gen(j)=qgam_gen(j)+qg
		enddo
	enddo

c       Make a special qetagen and qgamgen for exactly at hobs. ATMPROB is 1.
c       here. Requires a special qeta_hobs(i),qgamma_hobs(i)
	qetagen_hobs=0	!init some things
	qgamgen_hobs=0
         do lambda=270,700,5
                      ilambda=lambda
                      i=(ilambda-180)/5
                      qetagen_hobs=qetagen_hobs+qeta_hobs(i)*
     1 pmteff(i)*filter(i)*reflection(i)
                      qgamgen_hobs=qgamgen_hobs+qgamma_hobs(i)*
     1 pmteff(i)*filter(i)*reflection(i)
	enddo
	return
	end


	subroutine rate_qgen_uv(pmteff,filter,reflection,qeta_gen,qgam_gen,
	1 qetagen_hobs,qgamgen_hobs)

c	25/10/93 G.H.S. V:1:0:2.0
c	Form the q's, as a function of lambda.
c	Generate non-integrated Q's as a function of lambda for pmt efficiency
c	PMTEFF, filter transmission FILTER and mirror reflectivity REFLECTION. 
c	This is for all wavelengths of cherenkov light (180nm - 700 nm).
c        The Gamma threshold is now a funtion of both altitude and
c        lambda.
c	This is needed since for far uv light eta is wavelength dependent
c	which means sherenkov threhold is also wavelength dependents as
c	is chenkov angle. So just precalculate some things here.

c       Photon rate at lambda will be qeta_gen(lambda,height)-
c       qgam_gen(lambda_max,height)/gamma**2
c       Cherenkov angle will be sqrt(smaxsb-1/gamma**2)


c     Written: Glenn Sembroski
c              Purdue
c		25/10/93

	real reflection(0:104),filter(0:104),pmteff(0:104),
	1 qeta_gen(0:104,0:50),qgam_gen(0:104,0:50),
	2 qetagen_hobs(0:104),qgamgen_hobs(0:104)
	include 'kaslite.h'

c        Now form the q's, as a function of lambda.
c        This just saves a little work.
c        Start at just above hobs.
         do j=ihobs+1,50
		 do lambda=180,700,5
                      ilambda=lambda
                      i=(ilambda-180)/5
c       Terms for the Cherenkov angle and rate.
                      qeta_gen(i,j)=qeta(i,j)*pmteff(i)*filter(i)*
	1 reflection(i)
                      qgam_gen(i,j)=qgamma(i,j)*pmteff(i)*filter(i)*
	1 reflection(i)
                  enddo
         enddo

c        Make up sepcial set at hobs.
         do lambda=180,700,5
               ilambda=lambda
               i=(ilambda-180)/5
c       Terms for the Cherenkov angle and rate.
               qetagen_hobs(i)=qeta_hobs(i)*pmteff(i)*filter(i)*
	1 reflection(i)
               qgamgen_hobs(i)=qgamma_hobs(i)*pmteff(i)*filter(i)*
	1 reflection(i)
	enddo
	return
	end
!*****************************************************************************

      subroutine rate_uv_gen(height,dns,qeta_gen,qgam_gen,
	1 qetagen_hobs,qgamgen_hobs,rate,sangle,qz)
!*****************************************************************************
!     This function calculates the rate/meter of path length of Chernkov
!     photo-electrons for a segment at altitude Height(meters), with velocity
!     gamma,zenith direction cosign dns, in the wavelength intervel specified
!	by LAMBDA. Its for use with wavelengths that go below 270 nm= (ie into
!	the UV).
!*****************************************************************************

c	25/10/93 G.H.S. V:1:0:2.0
c     The guts of the calculation has already been done in routine RATEINIT.
c     It also calculates the cherenkov angle of emission at this wavelength.
c	This calculation must by wavelength(instead of of as an integerl, see
c	RATE_GEN)due to the rates convoluted dependence on gamma.
c     This is the number of PE's that are created by the photons emitted by a
c	segment in a 5 nm wavelength intervel centered at LAMBDA. Specific
c	detector details are specified by the q_eta and q_gamma arguments
c	which have been pre-cacluated in routine RATE_INIT.
c     Because the gamma threshold(minimum gamma) is altitude and lambda 
c	dependent we have to search for last good lambda(usually it will be 
c	310 nanometers, where eta maximizes(minimizes?))
c     Interpolate for height 
c	The calling routine  can use a rate value of exactly 0 as a flag to 
c	indicate that the maximum lambda has been reached.

c     Written: Glenn Sembroski
c              Purdue
c              25/10/93

!*****************************************************************************
c     Modified:

c	20/1/97 GHS V:1:1:4.0
c		Convert to use of structures for input segment file.
c		Remove direct accsee format for input segment file. Use
c		unformatted sequential instead.Note that this program will be
c		incompatable with segment files made under KASCADE V:1:0
c		Replace routine VACUSERINI with FILEOPEN.
c		Remove all referneces to old rate variables. see '*' comments

!	05/2/98 GHS V:1:1:5.1
!		Limit atexp to prevent some overflows.Also limit direction
!	cosign between initial particle direction and this track to <= 1.0

!	20/3/98 GHS V:1:1:5.1
!		Again limit use of ATEXP as exponent to prevent
!		some overflows.	       

!	11/07/01 GHS V1:2:6.2
!		For Heavy ions add QZ=charge to argument list. Modify rate by
!		QZ**2.
!      08/12/04 GHS  V:1:3:8.5
!               Altitude bug: rate_uv_gen,rate_gen:extint table only goes 
!               up to 50 km.  Make it that when we are above 50 km its ok (we 
!               don't crash) but we don't produce any light(rate=0)!
!*****************************************************************************


	real qeta_gen(0:104,0:50),qgam_gen(0:104,0:50),
	2 qetagen_hobs(0:104),qgamgen_hobs(0:104)
	real pangle
	integer qz
        include 'kaslite.h'

c     see if weve exceeded max wavelength that this gamma is above threshold.

	pangle=(dns*dni+segment.dlstart*dli+
	1 segment.dmstart*dmi)
	if(abs(pangle).gt.1.0)then
		pangle=1.0
	endif
	pangle=abs(acosd(pangle))
       iheight=height/1000.    !Lower altitude index.
       hint=(height/1000.-iheight)     !Interpolation factor.
       rate=0.
       sangle=0.
       atexp=(dni-dns)/dns		!Correction exponent.

	if(atexp.gt.40)then
		atexp=40.  	!Limit atexp to prevent some overflows.	       
	endif
	ilambda=lambda
	i=(ilambda-180)/5

c     see if weve exceeded max wavelength that this gamma is above threshold.
	if(i.lt.0.or.i.gt.104.or.iheight.lt.-1.or.iheight.lt.-1)then
           print*,' KASLITE:RATE_UV_GEN array bounds error. i,iheight:',
     1   i,iheight
        endif

	if(iheight.gt.49)then
           sangle=0.            !Flag that were too high for this routine
           rate=0.              !(extinct table only goes up to 50 km)
           return               !ie we dont produce light above 50 km
        endif


        if(segment.gamma.lt.gammin(i,iheight+1))then
		sangle=0.!Flag that weve reached max lambda for this gamma.
		rate=0.
		return
	endif

c	Calculate rate in this intervel.
c       Interpolate the Q's and smaxsb and the atm correction factor.
	if(iheight.gt.ihobs)then
		qetaint=qeta_gen(i,iheight)+(qeta_gen(i,iheight+1)-
     1 qeta_gen(i,iheight))* hint
		qgamint=qgam_gen(i,iheight)+(qgam_gen(i,iheight+1)-
     1 qgam_gen(i,iheight))* hint
c    6/23/89 G.H.S.  Add atm direction correction factor.
! 08/5/98 GHS  Limit use of ATEXP as exponent to prevent some overflows.
!              This was testting atmprob=0
		if(atmprob(i,iheight+1).lt.1.e-6)then
			ahigh=0.
		else
			ahigh=atmprob(i,iheight+1)**atexp
		endif
	
! 20/3/98 GHS  Limit use of ATEXP as exponent to prevent some overflows.
!              This was testting atmprob=0
		if(atmprob(i,iheight).lt.1.e-6)then
			alow=0.
		else
			alow=atmprob(i,iheight)**atexp
		endif
	
		aint=alow+(ahigh-alow)*hint 
c                                             !this number should be very close
c                                             !to 1.(within 10%) if atmprob is
c                                              !1% or bigger.
	else
		hi=(height-hobs)/((iheight+1)*1000.-hobs)
                qetaint=qetagen_hobs(i)+(qeta_gen(i,iheight+1)-
     1 qetagen_hobs(i))* hi
                qgamint=qgamgen_hobs(i)+(qgam_gen(i,iheight+1)-
     1 qgamgen_hobs(i))* hi
		if(atmprob(i,iheight+1).eq.0)then
			ahigh=0.
		else
                     ahigh=atmprob(i,iheight+1)**atexp
		endif
                alow=1.
                aint=alow+(ahigh-alow)*hi !this number should be very close
c                                              !to 1.(within 10%).
	endif

c	Interpolate cherenkov angle.
	smaxint=smaxsb(i,iheight)+(smaxsb(i,iheight+1)-
     1 smaxsb(i,iheight))* hint

c      Calculate rate.
	rate=(qetaint-(qgamint/(segment.gamma**2)))*aint

!	11/07/01 GHS: Adjust rate for charge.
	rate=rate*(qz**2)
c      Calculate angle.
	sangle=sqrt(smaxint-1./segment.gamma**2)
        return
        end
!*****************************************************************************


	subroutine readex(extint)
!*****************************************************************************
!      Read in the extinction data and reformat to 5 nanometer steps.
!*****************************************************************************
c      Open extinction data file
c      Used to get an array of atmospheric photon extinction
c      values.

       real extint(0:50,0:104)
	OPEN(1,ACCESS='SEQUENTIAL',STATUS='OLD',READONLY,
     1 file='extint.dat')

c       Get Probabilty for absorbing or scattering Cherenkov photons.
c       !For 270 to 700 nanometers
c       Use stuff from 'Handbook of Geophysics and space environments.',Shea
c       L.Vally ed. Air Force Cambridge Research Laboratories.,McGraw-Hill.
c       Chaptor 7. table 7.4.

c       !For 180 to 265 nanometers use results pete generates where he uses
c       !as far as I can tell some constants and graphs from the above
c       !reference to generate his numbers. From program 
c       !purdd::disk1:[halzen.kascade]uvtrans.for

c       NOTE: Below 180 Nanometers is an extremly stong oxygen absorption.

c       !Array EXTINT has all the extintion optical thicknesses 
c       First index of extint is km of
c       altitude starting a sea level(=0) to 50 km.  The second index is 
c       wavelengths in 5 nanometer steps from 180 nanometers to 700
c       !nanometers. The transmision 
c       fraction(probability) is T=exp(-(tau(high)-tau(low))sec(zenith angle))
c       where tau(a) is the extintion optical thickness at altitude a(km) 
c       from array extint. sec(zenith angle) is just 1/dn.

c        First read in the palfrey numbers from 180 to 270 nanometers.
        Do lambda=180,270,5
               ilambda=lambda
               i=(ilambda-180)/5
               read(1,1002)jlambda             !Read lambda from the file
1002  format(i4)
               if(jlambda.ne.ilambda)then
                         write(6,1003)ilambda,jlambda
1003  format(' ***RATEex--FATAL-Error reading lambda from EXTINT.DAT.',
	1 /,' ***Wanted:',i4,' Got:',i10)
                         stop   'Error readin extint.dat'
               endif

               read(1,1001)(extint(j,i),j=0,50)           !at 51 altitudes
1001    format(10f10.3)
	enddo

c        Next from file are at 280 to 400 in steps of 20
        Do lambda=280,400,20
               ilambda=lambda
               i=(ilambda-180)/5
               read(1,1002)jlambda             !Read lambda from the file
               if(jlambda.ne.ilambda)then
                         write(6,1003)ilambda,jlambda
                         stop   'Error readin extint.dat'
               endif

               read(1,1001)(extint(j,i),j=0,50)           !at 51 altitudes
	enddo

c        Next from file are at 450 to 700 in steps of 50
        Do lambda=450,700,50
               ilambda=lambda
               i=(ilambda-180)/5
               read(1,1002)jlambda             !Read lambda from the file
               if(jlambda.ne.ilambda)then
                         write(6,1003)ilambda,jlambda
                         stop   'Error readin extint.dat'
               endif

               read(1,1001)(extint(j,i),j=0,50)           !at 51 altitudes
	enddo
        close(unit=1)            !Close it 

c       Now fill in the blanks.
c         First at 275.
          ilow=(270-180)/5
          i275=ilow+1
          ihigh=(280-180)/5
          do j=0,50            !at 51 altitudes
                extint(j,i275)=(extint(j,ilow)+extint(j,ihigh))/2.
	enddo

c      Now from 285 to 395
          do lambda=280,380,20
               ilambda=lambda
               ilow=(ilambda-180)/5
               ihigh=ilow+4
               do im=1,3
                       do j=0,50            !at 51 altitudes
                              extint(j,ilow+im)=extint(j,ilow)+
     1 ((im)/4.)*(extint(j,ihigh)-extint(j,ilow))
			enddo
		enddo
	enddo

c      Now from 405 to 695
          do lambda=400,650,50
               ilambda=lambda
               ilow=(ilambda-180)/5
               ihigh=ilow+10
               do im=1,9
                       do j=0,50            !at 51 altitudes
                              extint(j,ilow+im)=extint(j,ilow)+
     1 ((im)/10.)*(extint(j,ihigh)-extint(j,ilow))
			enddo
		enddo
	enddo
	return
	end
!*****************************************************************************


       SUBROUTINE READEX_UV(extinct)
c	Vacanti routine_ for reading in his extinction file.
c	File constructed form ARTEMUS data. Ref when I can get it.

      integer lambda,jlambda,i,j
      integer ios

      real extinct(0:50,0:104)

      print*,'KASLITE-Using Vacanti atm. extinction  data from ARTEMUS'
      
      open(1,file='extinction_uv.dat',status='old',iostat=ios)
      if (ios>0)then
         print*,'KASLITE--extinction_uv.dat input file open error'
         stop 'extinction_uv.dat input file open failure'
      endif

	do lambda=180,700,5
		i=(lambda-180.)/5.
		read(1,1000)jlambda
1000	format(i4)
		if(jlambda.ne.lambda)then
			stop 'READEX_UV: error reading lambda'
		endif
		read(1,1001)(extinct(j,i),j=0,50)
1001	format(10f10.3)
	enddo

      close(1)
      return
      end
! **************************************************************************


