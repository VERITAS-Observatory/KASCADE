c      Program KASCADE
c	Version: V:1:3.7


c     3 dim hadron- electromagnetic  shower cascade program 

c      Written by:

c	Mary P. Kertzman
c	Dept. of Physics and Astronomy
c	DePauw University
c	Greencastle, In. 46135
c	E-Mail: "kertzman@depauw.edu"

c	G.H Sembroski
c	Physics Dept
c	Purdue University
c	W.Lafayette, In 47907
c	E-Mail: "sembroski@physics.purdue.edu"

c      Based on original algorithums supplied by T. Stanev.

c      Input/Output assignments:
c           Unit 1=Extint.dat   !Atmospheric extintion file.
c                2=Segment output file.
c                3=kascade.ran      !Random number seed save file.
c                4=Input parameter file.
c                6=Log file.
c		 7=Particle on ground file.

c      Modifications:


c      G.H.S. 1/8/86
c             Make all energy variables which are for K.E. start with a 't'
c             all total energy variables start with 'e'. Make sure this 
c             distinction is followed in the code. Important for low energy
c             situations.

c      G.H.S. 5/14/86
c             Add in low energy nucleon-delta production.  Treat as a
c             3 body decay. Changes in:UNICAS, DECAY.

c      G.H.S. 7/7/86
c             There is a problem with charge ratio's in LTYPES. We are
c             falsely biased towards positive charges. Fix when I can.


c      G.H.S. 2/2/87
c             To make a better approximation for the segment and the
c             effects of mag bending and mult scatt: 1.When 
c             propagating a segment , assume the segment is straight
c             with its initial direction to caculate final x,y values
c             times, mid x,y values and tmid. Apply ther direction
c             changes due to multiple scattering and mag field 
c             bending only at the end of the segment.Changes to
c             PROPAGATE.

c      G.H.S.+T.R.P 4/1/89
c             Pete found a major problem in the BEND subroutine. The old
c             one is correct in principle but has a big propensity for
c             round off error.  Pete reformulated the algorithum to avoid
c             this.  Also add a BENDINIT subroutine.

c        G.H.S. 4/9/89
c             Following T.R.P.'s suggestions the time will now be calculated
c             by accumulation differences of travel time of a segment versus
c             the travel time the primary particle moving at speed c would
c             have taken.  Also do things in real*8.  On the cray everything
c             is real*8.  Changes to PROPAGATE.

c        G.H.S.  5/23/89
c             We dont handle horizontal(dn=0)traks very well. For now assume
c             that they are not very important and dont let dn get less then
c             1.e-8.

c        G.H.S.  6/23/89
c             Minimal cuts in SEGMENT_OUT.  Only ignore neutrals.
c             Make correction to cherenkov rate production for segments not
c             parallel to primary. (QETA,QGAMMA assume for atm extinction that
c             all segments in direction of primary.)
c             Move ATMPROB array to common block.
c             Change RATEVIS,RATESB.  Approximate correction to that needed
c             at central lambda of tube/filter response.  Fix interpolation
c             for points just above hobs in rateinit.

c	7/16/90 G.H.S. V:1:0.1
c		Don't let particle go above 1 gm anywhere(PROPINT,UNICAS).

c	10/11/90 G.H.S. V:1:0.2
c		Add POG file(Particle-On-Ground).
c		Use a new flag GNDPRT to determine if we make the particle-on-
c		ground file.  Same format as PE file so kassrtmrg can
c		work on it. Use 5m x 5m grid spacing always (same as in KASLITE)
c		Assume detector area is 1m x 1m.Change format of POG file a
c		a little from old PE format to match new PE format. Move idfile
c		determination to .MAIN.

c	11/21/90 G.H.S. V:1:0.3
c		Since we have chosen dn positive to be down going and we have
c		chosen the +x axis to be east this makes the +Y axis South.
c		Only thing to fix was the direction of the earths magnetic
c		field in BENDINIT which should have a component in the North or
c		-Y direction. In PROPDEC:For decaying type particles that hit 
c		hobs, make sure they do by pretending that they go through hobs
c		then through air with density same as at hobs until they reach 
c		their decay distance. Use the resultant gm/cm**2 traveled as 
c		the value to feed to propint.

c	9/17/91 G.H.S.V:1:0.5
c		Make some miner changes to make this SGI Unix compatable.
c		1:Convert KASCADE to use RNDM to make us(at least
c			at the source code level) machine independent.
c		2:For the POG file equvlence nx,ny to nxy(i*4) and always
c			retreve nx,ny the same way. This should insure
c			machine independence.

c	22/1/92 M.P.K. V:1:0.6
c		New and improved RIONIZE. Old was high by 20-25% at 1 Tev
c		and had no low(below min ioniz) rize.  New one is of course
c		perfect.
		
c	7/3/92 G.H.S. V:1:0.6
c		Add KNOCK-ON electrons from charged particle segments. Added
c		to subroutine PROPAGATE. Since more then 2 electrons can now
c		be generated by a sigle track, NEN limit in STOR_E is increased
c		to 1000.  Fix a miss type declaration of ZBETA in propagate.
c		This fix makes no difference to previous results.

c	25/3/92 M.P.K. V:1:0.6
c		In DECAY, Compton scattering was scattering positrons, not 
c		electrons. Fixed it.

c	2/4/92 G.H.S. V:1:0.6
c		Fix width of GAUSS. It was .6932. Make it 1.0 so nfluct has
c		correct width. Add ASGAT values for magnetic field to 
c		BENDINIT. New places will require re-edit of BENDINIT to get
c		correct mag field. Some day maybe they should be
c		readin as parameters from .inp file but not today.

c	23/4/92 G.H.S. V:1:0.6
c		Let the letter for the Magnetic filed indicate values to use.
c		T(for compatability) or L=Haleakala. A for ASGAT. F for none.

c	22/10/93 G.H.S. v:1:0.6
c		Add to Magnetic field values using 'W' for WHIPPLE

c	11/9/95 GHS V:1:0.6
c		Correct sign of dn normalization caculation in PROPAGATE.
c		Fix hobs limitation in PROPINT. Fix a buch of stuff including
c		handeling of up going particles in PROPAGATE,PROPINT,PROPDEC.

c	23/10/95 G.H.S. v:1:0.6
c		Add to Magnetic field values using 'S' for Argentina
c		Searchlight array.

c	09/12/96 G.H.S. v:1:0.7
c		Replace Stanev bremsstrahlung code with new and improved(and
c		correct :) ) code written by GHS. Replace routines RBSME and 
c		RBSMS with routines BREMSSTRAHLUNG and BREM_INTENSITY.

c	17/12/96 G.H.S. v:1:0.7
c		Replace Stanev pair production code with new and improved(and
c		correct :) ) code written by GHS. Replace routines RPAIMS and
c		RPAIME	with routines PAIR_PRODUCTION and PAIR_INTENSITY and
c		PAIR_CROSSECTION.  This fixes a major problem in the
c		pair_poduction crossections at low energies and fixes minor 
c		inaccuaracies in the energy calculation of the pairs above 
c		primary photon energies of 500 MeV.
c		Also fix a minor error in the compton crosssection calculation.
c		Remove DIST subroutine. Its not used anymore.

c	16/1/97 G.H.S. v:1:1.0
c		Convert from a direct access output file for the segments to an
c		unformated binaray file. Use structures(in KASSTRUCTURES.H) 
c		for header and data structures. Remove calls and code that 
c		determines number of photons created per segment (RATEINIT,
c		RATEVIS,RATEOLD and RATESB  etc). Save in file KASCADE_RATE.FOR
c		This is now done in KASLITE as needed. Do this to considerably
c		speed up this program. However this removes chernkov emmsion 
c		threshold check. All segments are written out now.
c		Replace all VAX* files into/with things like: FILEOPEN,DATIN
c		Remove NSHOW parameter from data file. Remove NSHOW and
c		NSEGMENTS from segment file header.
c		Also remove THRESHOLDCHECK and add SEGMENT_OUT(old VAXUSER).
c		Remove PLOT flag variable from input data file.
c		Clean up KASCADE.H of unused variables.
c	NOTE FUNCTION COEL needs to be rewritten for style reasons but since
c		I don't know what its doing I'm going to leave it alone for
c		now!!!

c	06/5/97 GHS V:1:1.1
c		Cleaning up for use on UNIX:
c		1). Get rid of 'shared' and 'recordtype' spcifications in open
c			statements.
c		2). Fix the style problem in COEL
c		3). Convert to the RANLUX random number generator. Requires us
c			to use the CERN MATHLIB. We also no longer read in IX
c			(a seed used in the old RNDM) in the input parameter 
c			file. So once again we have a new input parameter file
c			format. But see 13/01/05
c		4). Don't use  STATUS='new' in UNIX OPEN statements when we are
c			using logical links in UNIX. It thinks the LINK file
c			is the new file and thus won't open a new one. Use
c			STATUS='unknown' instead. Use 'new' when opening VMS
c			files

!	15/9/99 GHS V:1:1.2
!		Convert to generic form. Linux/VMS
!		(do this with some attention to the fact that we may want to
!		parrallelize this someday,but probably not).
!		1:Read File_id from a temporary file with a logical name
!		assigned to KASCADE.TMP. Works for both LINUX and VMS.
!		Replaces GET_FILE_ID.H
!		2.Remove RECL argument form open of POG file. Replaces
!		OPEN_POG_FILE.H
!		3:Replace OPEN_NEW_SEG_FILE.h with a normal open statement.
!		4: Update NFLUCT to match w10mf77.for version.
!		5: Update GEOM to use r*8 internally. Requires a r*8 version
!		of MTXMLT called DMTXMLT.From w10m_f77.for.
! 		6:Use CERN lib source code for RANLUX(from w10m_f77.for).  Use
!		RANLUX_SNGL.FOR for PRAN,RANSTART and RANEND. Basically I
!		removed these routines from this source file and will add
!		ranlux_sngl.for to Link. Using source code of RANLUX means we 
!		don't need to use CERN LIBS here anymore(I hope).
!               But see 13/01/05
!		7: Reorder KACADE.H. Explicitly copy whats in
!		KASSTRUCTURES.H into KASCADE.H and pur all DATA statements
!		last. Move all "include KACSADE.H" stements to end of
!		declarations in this file.

!     28/03/01 GHS V:1:1.3
!      Use a better coef value for the multiple scatering width calculation.
!      Use a value of 13.6*sqrt(2)=19.233. This is from Particle Physicis
!      Booklet of 1998 (and all post 1990 we think) pg 184.
!      Thanks to Julien Guy for pointing out this correction.

!	08/06/01 GHS V:1:2.0
!               Insert stuff to do heavy nuclei. This is an almost wholesale
!               adaptation of the orginal KASHEAVY version by Mary Kertzman
!               dated 08/08/95. Below are her original comments:
!		"Added code for heacy primaries. THe new code includes
!		the nuclib subroutines from Gaisser and Stanev, as well
!		as subroutine do_heavy. The heavy code plugs in to
!		the beginning of UNICAS. Everything else remains the
!		same. Changes to UNICAS"
!               Below are my adaptations and explanations of her method.
!               1: Define SEGMENT_HEAD.ITYPE(and thus ISPEC) as 20+A where A 
!                  is atomic mass number(must be <= 56). Thus HE(4) has 
!                  ITYPE=24 and Fe(56) (the heaviest nucleus we can handle!) 
!                  (BUU SEE BELOW!!!!) has ITYPE=76
!                  In DATAIN determine Z for given A. Use a formula derived 
!                  from the nuclear liquid drop model. See DATAIN for 
!                  refernece.
!                  Use a lookup table for names as a function of Z. Add 
!		   subroutine:MASS_NUMBER2CHARGE_MASS.
!               2: Scheme is: If primary particle is a nucleus (ispec>20) then
!                  in the start of UNICAS call DO_HEAVY which propagates the
!                  heavies and their fragments, determines all the nuclear 
!                  interactions , saves all the tracks of the heavies and 
!                  produces all the single nucleons that come out of such 
!                  interactions. These are saved (by store_g) to be processed 
!                  in the normal manner in the main loop.
!		3: Expand the debug print facility (DEBENA) to cover heavies.
!                  Use debena(20) to flag heavies printout.
!		4: Modify PROPINT to handle heavies. Small changes.
!               5: Modify PROPAGATE to handle heavies.
!               6: Modify RIONIZ,Add qx and xmass to argument list. Convert for
!                  use with heavy nuclei.
!		7: KNOCK_ON: 
!		  a:Treat as spin 0 (A=even) or spin 1/2(A odd) This choice is
!                   my best guess. I don't see it verified anywhere. GEANT 
!                   (from CERN) treats all heavies as spin 1/2.
!		  b:Inlcude Z**2 in rate calculation.
!		  c:Poission fluctuate (NFLUCT) number created along track. 
!                   Replaces use of KNOCK_MADE for all knock-on generation. I 
!                   tested this method and it appears to work just as well as 
!                   any other method I or Pete Palfrey could come up with for 
!                   all ranges of rate
!                 d:For spin 0,1/2 Function 'FUN' now has max of 1.0 (removed 
!		    a 1/beta**2 factor, which in almost all cases had no 
!                   effect)
!		8: Modify MULTSCATT for use with Heavies. Particle data book 
!		   says only modification needed is to as a factor of QZ
!		   (heavy charge) to the THETARMS calculation. Also, leave in 
!                  the single large scatter code(I think this just adjusts 
!                  tails of multiple scattering beyound 5 deg. to something
!                  non-gausian. I don't know if this is the best thing to do 
!                  for heavies.
!		9: BEND:Modify for effect of charge of heavies. Bend radius is 
!		   inversly proportional to charge.
!               10: I found an error in N_FPNI. It was being defaulted to as an
!                   integer function, rounding down the result. I made it real.
!                   Also in N_FPNI use FPNI for energies below 150 GeV instead
!                   of FPNI_HE which we don't seem to have. FPNI_NUC gives an
!                   error if you try to use it below 150 GeV some where(at 
!                   least at 17 GeV it fails). Also there is a disontinuity in
!                   between FPNI and FPNI_NUC at 150 GeV of .6611 gm/cm**2 
!                   (out of ~85 gm /cm**2, <1%) but for prettyness add it back
!                   in in N_FPNI.
!		11: Nuc_lib had a lower limit of 53 GeV/nucleon. I had to 
!		    extend this down to about 2 GeV/nucleon. Mostly I need to 
!		    exted SIGNUC_INI. To do that I needed to run SIGMA_AIR at 
!		    lower energies. This required extending SIGMA_PP to lower 
!		    energies. 
!		    A quick rundown of the NUC_LIB routines(see the routines 
!		    themselves for more details) that have been modfified are:
!		    	SIGMA_PIP-Crude extention down to AL=.3
!		  	SIGMA_PP- Careful, accurate extension down to AL=.3
!			N_FPNI-See above.
!			FPNI_NUC-See above.
!			BLOCK,BLOCK_INI,SSLOPE,FMINUS,FPLUS- Added comments 
!							     and references.
!			SIGNUC_INI-Extend down to AL=.3(2 GeV). 
!	10/07/01 GHS V:1:2.0
!		At very low energies its possible for electron/positron
!		to end up with less energy at the interaction point from
!		de/dx then we give to the gamma. This is due to an 
!		apporximation we make of ignoring the de/dx energy loss before
!		we determine in E_GAMMA in BREMSSTRAHLUNG. Limit things here 
!		in a quick and dirty way that probably won't make any 
!		difference. Do this in UNICAS.

!      11/12/02 GHS  V:1:2.1
!               Use kascade.seg as output file link.

!      30/04/03 GHS  V:1:3.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               ./kascade -p g100gev20w.par (sortof like in c).
!               Setup for command line input parameter use.(Changes in main,
!               DATIN, FILEOPEN

!      02/05/03 GHS  V:1:3.1
!               Read in the primary energy (as integer gev) as a command line 
!               parameter.

!      07/12/04 GHS  V:1:3.2
!               Move setatm,rho,yds,gms into seperate file: kasatm.for.
!               Remove atm stuff from kascade.h. include it explictly in
!               kasatm.for.
!               Do this in preperation for conversion to atm76 model 
!               (kasatm.cpp,atmosphere76.h, atmosphere76.cpp)


!      08/12/04 GHS  V:1:3.3
!               A Bug, sort of: When the injection depth is less than 1gm we 
!               have problems with thinking that we have upgoing particles and 
!               dropping them. Change places where yds(1.0) is used as the 
!               test limit to .5*segment_head.depth

!      13/01/05 GHS  V:1:3.4
!               Move the RANLUX code to file ranlux.for, add the stuff from
!               ranlux_sngl.for. Now all ranlux stuff in one place.


!      13/01/05 GHS  V:1:3.5
!               Replaced old MULTSCATT routine with a C++ version of Vladimir
!               Vasilov's compton_scattering code from his CHESS.FOR. This new
!               code should do a much better job (especially for large angle 
!               scatters).  It does however have several limits.
!               1:particles  with KE > 1 TeV are not scattered.
!                 (again for time efficency reasons)
!               2:Segment lengths < .00245 gm/cm**2 have too few single angle 
!                 scatters (less than 30) for our gaussian approximation to be
!                 accurate. Allow user to specify tMax< .00245 but put out a 
!                 warning that his results in no multiple scattering. 
!               This required a new file: kasmultscatt.cpp (has a wraper for 
!               use of the new MultipleCoulombScattering class, ie its makes 
!               the API the same as the original code) to be linked into
!               this program. For testing purposes the MULTSCATT routine was 
!               moved to a file called kasmultscatt.for. the API are the same 
!               for the two files (this is simular to what we did for the 
!               atmosphere76 and atmosphere68 versions of the code going into 
!               kasatm.cpp and kasatm.for respectivly.)You choose which version
!               of the multiple scattering code you use by choosing which 
!               file you link in(in Makefile).

!      08/02/05 GHS  V:1:3.6 
!               Update earths magnetic field for Whipple (and for kitt peak).
!               Values determined by Gernot Maier (Leeds) using program Geomag.
!               He gets:Bx=-25.2 By=40.0 =>dipangle=58.3 deg(we were using 60)
!               And a filed strength 0f .4795 gauss (we were using .5)
!      17/02/05 GHS  V:1:3.7 
!               Add a printseeds flag to ranstart and ranend.

!!*****************************************************************************
!	THINGS to DO!!
!*****************************************************************************
!	Fix FPNI to match N_FPNI and get rid of discontinuity at 100 GeV.
!	Make a Muon Bremstrahlung routine.
!       Can we extend NUC_LIB to beyound A=56,Z=26 (Fe) nuclei?
!       Photo-Production.
!	Add fit functions to sigma_pip for sigt,sigel for E0<2.1 TeV
!       Multscatt:Add sum of single angle scatters for tmax<.00245 gm/cm**2
!*****************************************************************************

c		UNIT=2	Ouput Segment file.
c		UNIT=4  Input parameter file
c		UNIT=7	Particle on the ground (POG) output file.

c      Particle species codes.
c              1:Gamma
c              2:positron
c              3:electron
c              4:muon (+)
c              5:muon (-)
c              6:pion (0)
c              7:pion (+)
c              8:pion (-)
c              9:kaon (+)
c             10:kaon (-)
c             11:kaon (0long)
c             12:kaon (0short)
c             13:proton
c             14:neutron
c             15:neutrino(electron)
c             16:anti-neutrino(electron)
c             17:neutrino(muon)
c             18:anti-neutrino(muon)


	character*80 version
	character*80 update
        character*80 arg_opt
        external iargc_
        integer num_cmd_args
        character*80 file_id_txt
        logical got_energy
        integer integer_energy_gev
        character*80 energy_gev_txt
	integer printseeds

        integer ios,i
        include 'kascade_command_line.h'
	include 'kascade.h'

        data got_energy/.false./

	data version/'V:1:3.7'/
        data update/'17-Feb-2005 GHS'/    !Last version update        

        data namtyp/'Gamma   ','Positron','Electron','Mu +    ',
     1 'Mu -    ','PI 0    ','PI +    ','PI -    ','K +     ',
     2 'K -     ','K long  ','K short ','Proton  ','Neutron ',
     3 'eNeutrin','anti-eNu','muNeutri','ant-muNu'/
       data icharge/0,1,-1,1,-1,0,1,-1,1,-1,0,0,1,0,0,0,0,0/
c                !Thresholds(Tev.)Lab KE energies.       
c                                       !5/14/86 Threshold for hadrons is KE 
c                                       !equal a delta mass 1.232 GeV)
c                                       !6/3/86  KE Thresholds for charged 
c                                       !pions set to 300 mev.,kaons threshold
c                                       ! set to their mass.
c                                       !Thresholds for gammas and electrons
c                                       !and mu's loaded in DATIN.
       DATA Thresh/3*23.E-6,2*100.e-6,134.963E-6,2*300.0e-6,4*0.0005,
     1 2*0.00123,4*1./

c            !Life times(c_light*Tau/Mass in meters/Tev.)
       DATA TLife/3*-2.,2*6.234e+6,0.0,2*5.591E+4,2*7.513E+3,3.122E+4,
     1  5.375E+1,6*-2./

c            !Masses of stable particles(Tev.).  Uses particle code as index.
       DATA XM/.0, 2*.5110034E-6, 2*105.6594E-6,  134.9626E-6,
     2 2*139.5669E-6, 2*493.668E-6, 2*497.67E-6,
     1 938.2796E-6, 939.5731E-6, 4*0.0/


       data input_par_file_name/'kascade.par'/     !-p
       data random_seed_file_name/'kascade.ran'/     !-r
       data mfile_out_dat_file/'kascade.seg'/     !-o




c	Save version for header file.
	segment_head.version=version
	write(6,1623)version,update
1623	format('KASCADE VERSION:',a,' Last updated:',a)
c	First Number of version number is for KASCADE system.
c	Second Number is for KASCADE program.
c	Version number indicates compatability:first digit(before period)
c	indicates compatability. Number after period indicates update
c	number.

!******************************************************************************
!       Get the various command line arguments.
!       All command line arguments come in pairs: An option (-p) and 
!                                                 a string (g100gev20w.par)
!*****************************************************************************
!  Command line input options:
!*****************************************************************************
! -p kascade.par   :File name for input parameter file
! -r kascade.ran   :Random number seed file name.     
! -o seg.dat       :Output segment file name:
! -d file ID       :Shower ID number
! -e gev           :Shower primary's energy in gev.
! -h Prints this options summary
!******************************************************************************

        num_cmd_args=iargc_()
        if(num_cmd_args.gt.0)then
           print*,'KASCADE--Number of command line arguments:',
     1       num_cmd_args
           do i=1,num_cmd_args,2
              call getarg_(i,arg_opt)
              arg_opt=trim(arg_opt)
              if(arg_opt=="-p")then
                 call getarg_(i+1,input_par_file_name)
                 input_par_file_name=trim(input_par_file_name)
                print*,'-p option gives parameter file name:', 
     1                 trim(input_par_file_name)
c      incident energy in Tev
              elseif(arg_opt=="-r")then
                 call getarg_(i+1,random_seed_file_name)
                 random_seed_file_name=trim(random_seed_file_name)
                 print*,'-r option gives Randome seed file name:',
     1                trim(random_seed_file_name)
              elseif(arg_opt=="-o")then
                 call getarg_(i+1,segment_output_file)
                 segment_output_file=trim(segment_output_file)
                 print*,'-o option gives output segment file name:',
     1                trim(segment_output_file)
              elseif(arg_opt=="-d")then
                 call getarg_(i+1,file_id_txt)
                 file_id_txt=trim(file_id_txt)
                 read(file_id_txt,1002)segment_head.idfile
 1002            format(i5)
                 print*,'-d option gives file id:',segment_head.idfile
              elseif(arg_opt=="-e")then
                 call getarg_(i+1,energy_gev_txt)
                 energy_gev_txt=trim(energy_gev_txt)
                 read(energy_gev_txt,8002)integer_energy_gev
 8002            format(i5)
                 segment_head.tep=integer_energy_gev/1000.
                 print*,'-e option gives primary energy(gev):',
     1   integer_energy_gev
                 got_energy=.true.
              elseif(arg_opt=="-h")then
        print*,' *************************************************************'
        print*,' * Command line input options:'
        print*,' *************************************************************'
	print*,' * -p kascade.par   :File name for input parameter file'
	print*,' * -r kascade.ran   :Random number seed file name.'     
	print*,' * -o seg.dat       :Output segment file name'
	print*,' * -d file ID       :Shower ID number'
	print*,' * -e gev           :Shower primary energy in gev.'
	print*,' * -h Prints this options summary'
        print*,' *************************************************************'
        stop 'Kascade stoped'
           else
                 print*,' Illegal command line option #:',i,'Option:',
     1            trim(arg_opt)
                 stop 'KASCADE: Illegal command line option'
              endif
           enddo
        else
           print*,' KASCADE--No command line arguments.'
           print*,' KASCADE--Using defaults for all.'

        endif
        if(.not.got_energy)then
           print*,' KASCADE--FATAL--Need input primary energy!'
           stop 'need input primary energy'
        endif

!******************************************************************************


	e_mass=xm(2)*1.e6
		       !Electron mass in Mev.

		!Read in the run parameters.
	call datin              !DATIN calls YDS.

	write(6,1003)segment_head.idfile
1003  format('                          File Id for shower  =',i6)

            !All altitude variables start with 'h' and are in meters.
       height=yds(segment_head.depth) 
				      !inital altitude in meters above sea_level
c     10/17/86       G.H.S.              Define xinitial and y initial
       path=(height-hobs)/dni              !Length of trajectory
       segment_head.xinitial=-path*segment_head. dli  
				        !X offset to get center of shower
					!at origen at hobs.
       segment_head.yinitial=-path*segment_head.dmi

	call fileopen		!Open output segment file and write out 
				!SEGMENT_HEAD record.

c     Init things.
       if(magnet_on)call bendinit  !Init bend subroutine things including field



	if(gndprt)then
c        open and create POG file.
	        OPEN(7,ACCESS='sequential',STATUS='new',
	1 FORM='UNFORMATTED', iostat=ios)
       if(ios>0)then
          print*,'KASCADE-FATAL-Failure to open output POG file.'
          stop 'Failure to open output POG file'
       endif

c	write the POG file header.
	write(7)segment_head.itype,segment_head.tep,
	1 segment_head.dli,segment_head.dmi
		!Header:
	!1:	itype		Primary particle type.
	!2:	tep		Primary particle energy(TeV.)
	!3,4:	dl,dm		Primary pareticle direction(can recreate dn)
c	Second record is continuation of header
	write(7)0.0,0.0,segment_head.magnet_field,segment_head.etr,
	1 segment_head.depth
	!6,7:	dl,dm		Mount direction(always zenith for particle
				!counters.
	!8:	magnet_field	Earths magnetic field type?
	!9:	etr		Threshold level for gammas,e and mu.
	!10:	depth		Injection depth in gm/cm**2
c	Third record is continuation of header.
	write(7)hobs,5.0,5.0,'P',segment_head.idfile
	!11:	hobs		Observation altitude in meters.
        !12:    xseg            Grid width in x direction.Always 5m x 5m for
				!particle counters.
        !13:    yseg            Grid width in y direction.
	!14:    petype		Character: 'P'=particles
	!15:	idfile		File index number.

	endif

c
c!       All K.e. variables start with 'e' or 't'.(Can be either Mev or Tev)

       WRITE(6,1000) hobs,height
1000  format('  Observatory at(m.):',f7.1,' Injection at(m.):',f10.1)





       
	do i=1,20
		if(debena(i))then
			write(6,1722)
1722  format('0',30x,'Tenergy(Tev)  Alt(meters)    Direction cosigns ',
     1 '       X,Y(meters)    Time(nsec)')
			goto 103
		endif
	enddo

!Do the shower.
c                            Init shower parameters
103	hei=height
	tenergy=segment_head.tep
        dl=segment_head.dli
        dm=segment_head.dmi
        dn=dni
        if(abs(dn).lt.1.e-8)then
		dn=1.e-8       !Cheap way to handle horizontal tracks.
                dl=(1.-1.e-8)*dl
                dm=(1.-1.e-8)*dm                       
        endif
        zdni=dn       !Direction of primary particle for timeing use
c                             in  PROPAGATE.  r*8.


        ispec=segment_head.itype       !Initial particle type
        x=segment_head.xinitial              !Initial x,y coord.
        y=segment_head.yinitial
        size=0.              !size of shower
        CALL UNICAS (tenergy,ispec,hei,dl,dm,dn,x,y)
c                                   Run the cascade. This the guts
c                                   of everything.
        
         write(6,1001)segment_head.idfile,size,segment_head.tep,
	1 segment_head.xinitial,segment_head.yinitial,inext
1001  format(' Shower File id:',i4,
	1 /,' Shower size:',f7.0,
	1 /,' Shower energy=',e14.7,' TeV',
   	2 /,' Shower core at:',f10.1,',',f10.1,
	3 /,' Number of segments in shower:',i10)

        printseeds=1   !True
        call ranend(printseeds,random_seed_file_name)     
                        !Save random number seed vector and Print it out.

	close(2) 		!Close the segment file.
	if(gndprt)then
		close(7)	!close the POG file
	endif


!***************************************************************************
!debug TW: But this is nice. Leave it. 18/04/02 ghs
!***************************************************************************
        inmax=0
        iamax=0
        do i=1,2000
           if(shower_max(i).gt.inmax)then
              inmax=shower_max(i)
              iamax=10*i
           endif
        enddo
        write(6,1004)iamax,inmax
 1004   format(' Shower Max at:',i10,' meters. #:',i10,'.')
!enddebug
!***************************************************************************

        STOP 'Kascade normal end.'

       END



       subroutine bend(tenergy,zpath,ispec,dl1,dm1,dn1,qz,xmass)

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

       real*8 zpath
	real xmass	!Mass of particle (in GeV/c)
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
       dlt=dl1*cal + dm1*bz*sal - dn1*by*sal
       dmt=-dl1*bz*sal + dm1*(1.-bz*bz*ccc) + dn1*by*bz*ccc
       dnt=dl1*by*sal + dm1*by*bz*ccc + dn1*(1.-by*by*ccc)

c       Normalize.
       xmag=sqrt(dlt**2+dmt**2+dnt**2)
       dl1=dlt/xmag
       dm1=dmt/xmag                  !Preserves signs.
       dn1=dnt/xmag

       return
       end



       subroutine bendinit

c       Init calcultions for BEND routine.
c	The BEND program really does assume bx=0. It doesn't even use bx.

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

	include 'kascade.h'

c      DETERMINE unit vector for MAG FIELD b
c	And north only!

!!!!!This is for Haleakala ONLY!!!!!!
	if(index(segment_head.magnet_field,'L').ne.0.or.
	1  index(segment_head.magnet_field,'t').ne.0)then
		b_field=0.365      !Gauss
		dip_angle=37.8    !Degrees.
		write(6,1000)'HALEAKALA',b_field,dip_angle
1000	format(' ',a,' Magnetic field values being used!!!',
	1/,' B_FIELD(gauss):',f7.3,' DIP_ANGLE(deg):',f7.3)

	elseif(index(segment_head.magnet_field,'A').ne.0)then
		!!!!!This is for ASGAT ONLY!!!!!!
		b_field=0.45     !Gauss
		dip_angle=57.0    !Degrees.
		write(6,1000)'ASGAT',b_field,dip_angle

	else if(index(segment_head.magnet_field,'M').ne.0)then
		!!!!!This is for MACRO ONLY!!!!!!
		!NOTE THESE ARE JUST HALEAKALA VALUES!!!!
		b_field=0.365     !Gauss
		dip_angle=37.8    !Degrees.
                write(6,1000)'MACRO',b_field,dip_angle
	else if(index(segment_head.magnet_field,'W').ne.0)then
		!!!!!This is for WHIPPLE ONLY!!!!!!
!		b_field=0.5      !Gauss
!		dip_angle=60.0    !Degrees.

! 08/02/05 GHS Replaced by new values: see comment above.
		b_field=0.4795      !Gauss
		dip_angle=58.3    !Degrees.
		write(6,1000)'WHIPPLE',b_field,dip_angle

	else if(index(segment_head.magnet_field,'S').ne.0)then
		!!!!!This is for Argentine Search light array!!!!!!
		b_field=0.35      !Gauss
		dip_angle=-25.0    !Degrees.
		write(6,1000)'Argentine Search light array',
	1 b_field,dip_angle

	else
		print*,' KASCADE--FATAL--Illegal magnetic field type:',
	1 segment_head.magnet_field

		stop
	endif

!	This next line is unnecessary. Nobody(including BEND) ever uses BX.
!	BEND assumes that there is no field in x direction!!!!!
       bx=0.
       by=-cosd(dip_angle)       !Mag field points north.
				 !+Y direction is South!!!
       bz=sind(dip_angle)
       return
       end




	subroutine bremsstrahlung(e_tev,ispec,e_gamma,t,bremdedx)
c	Electron/positron bremsstrahlung calculation.

c	Determine energy E_GAMMA of bremsstrahlung photon emitted by 
c	electron/position  of total energy U where U=E_TEV+mass_elec*c**2 =
c	total energy of electron/positron. E_TEV is KE of electron/positron in
c	TeV.

C	Determine distance (T: in  units of radiation lengths) to photon 
c	emission above a minimum energy E_MIN.

c	Determine average energy loss(BREMDEDX: in TeV) of electron due to 
c	emission of photons of energies below E_MIN while traversing distance T.
	
c	Let V be the fraction of energy the emitted photon has of the orginal
c	energy U of the electron/positron: V=E_GAMMA/U.
c	The fraction(V) is distributed as 1/V multiplied by an energy
c	dependent 'intensity' function that is semi-constant.

c	E_TEV is kinetic energy of electron/position in TeV.
c	ISPEC is particle type (Only acceptable values are: 2=positron, 
c	3=electron)

c	Physics note:
c	If Theta(U,V)dV is probability/radiation_length of emmision of
c	photons of with energies between V*U  and U*(V+dV) then 
c	U * (V*Theta(U,V))dV is the energy intensity of radiation of photons 
c	with energies beteween U*V and U*(V+dV).  While Theta(U,V)
c	may diverge as 1/V, at V=0 (infared divergence)  V*Theta(U,V) does
c	not. ie. no infared catastropy!

c	Generation of V:
c	Procedure:
c	Randomly pick a V from the 1/V distribution limited
c	to a range of V_MIN to V_MAX. 
c	Note: V_MIN defined as E_MIN/E_TEV.
c	V_MIN must be >0 to avoid the infared divergence. Pick a value of
c	a photon somewhat less then our chernkov threshold.
c	V_MAX set by V_MAX=(1-mass_elecron*c**2/U) .

c		Then:

c	Using the 3 equations from Rossi and Greisen, Rev. Mod.Phy.,13,Oct 1941
c	pg.253.(RG) (Don't use complete screening equation. gamma<2 equation 
c	with Butcher-messel f1,f2 parameters is adequate.)
c	Determine  V*Theta(V,U) for the appropriate screening. Use
c	parameterization of the f1,f2 functions from Butcher and Messel,
c	Nuc.Phy 20,1960,pg 15-128. (BM) Use a power law fit to the C(gamma) 
c	function derived from the table given in RG pg.255.
c	Fit was done by G.H.Sembroski, Physics Dept.,Purdue University,
c	W.lafayette,In.  11/5/96
c	Use rejection method against these functions to test if we
c	should keep this value of V.


c	The mean free path(MFP) to emission at energy U is determined by 
c	integrating Theta(V,U)dV from  V_MIN to V_MAX. This results in the 
c	probability/rad_length for emmsion by an electron of energy U of a 
c	photon with some energy greater then E_MIN=V_MIN*U.
c	E_MIN  is chosen to be well below the lowest energy Gamma we are
c	interested in. In this case: 1 MeV (we drop gammas below 10 MeV since
c	they don't chrenkov radiate.). An aditional reason for chosing E_MIN
c	much less then the threshold energy is that it reduces the error
c	associated with assuming the electron retains energy U until its point
c	of interaction. Acturally it is constantly lossing energy to
c	inionization and to bremsstrahlung emission below E_MIN. Use of a small
c	E_MIN results in a small Mean-free-path length to the interaction
c	point which minimizes this error. The counter reason for a small E_MIN
c	is computation time. The smaller E_MIN the more interactions above E_MIN
c	(most of which are dropped for haveing photon energies below thershold).

c	Glenn Sembroski has determined the MFP over the range of interested
c	energies(using routine_  brem_intensity). Two functions of the form

c		MFP=A1*ln(U)**(-B1)  for E_TEV<=15 MeV
c		MFP=A2*ln(U)**(-B2)  for E_TEV>15 MeV

c	were found to be a good fit to the data .
c	The actual distance T (in radiation lengths) this electron goes before
c	emission is distributed as 

c	  probability(emission between T and T+dT) = (1/MFP)exp(-T/MFP)

c	This approach to modeling bremsstrahlung has so far neglected those 
c	photons	emitted with energies below E_MIN. The average amount of energy
c	lost to these photons per radiation length by the electron was 
c	determined by Glenn Sembroski by numerically integrating 
c	U*V*Theta(V,U) from 0 to V_MIN.	For V_MIN*U= 1MeV this de/dx(brem) was
c	found to tend to a constant for large U and had a value of 
c	1.39799 MeV/Rad_length(Defined as = DEDX_MAX). For all U the following 
c	function was found to fit well:

c	     bremdedx=dedx_max(1.-A(U**(-B)))

c	Written by:
c		Glenn Sembroski
c		Physics Dept
c		Purdue Univ
c		W.Lafayette Indiana
c		05/12/96

c	Modified:


	real e_tev,v,t,bremdedx
	integer ispec

	real u,v_min,v_max,e_mev,mfp,brem,test_brem,brem_max

	real e_min
	parameter(e_min=1.)		!Lower cutoff energy: 1 MeV
	parameter(dedx_max=1.3979998)	!Max DE/dx due to brem below 1MeV
					!(Units: MeV/radiation length)

	include 'kascade.h'


	if(ispec.ne.2.and.ispec.ne.3)then
		print*,' BREMSTRAHLUNG--FATAL--Invalid ISPEC:',ispec
		stop
	endif
	e_mev=e_tev*1.e6		!For internal use convert to MeV.
   	if(e_mev.lt.e_min)then
		print*,' BREMSTRAHLUNG--FATAL--E_TEV<E_MIN'
	endif

	u=e_tev+xm(ispec)		!Total energy in TeV.
       	v_min=e_min/(u*1.e6)		!Lower energy threshold fraction
	v_max=1.-(xm(ispec)/u) 		!Can't get more then U-mc**2 into
					!photon. xm in TeV
	if(v_min.gt.v_max)then
		print*,' BREMSTRAHLUNG--FATAL--V_MIN>V_MAX'
		stop
	endif

c T:
c	Determine Mean Free Path (for emission of a photon with energy greater
c	then E_MIN=1MeV in units of radiation lengths) from fitted function.
c	Fitted by Glenn Sembroski 05/12/96, Purdue University
	if(e_mev.le.15.)then
 		mfp=3.865*alog(e_mev)**(-2.341)	!for E_MEV<=15 MeV,E_MIN=1MeV
	else
		mfp=1.189*alog(e_mev)**(-1.191)	!for E_MEV>15 MeV,E_MIN=1MeV
	endif

c	Generate actual distance T(in radiation lengths):
c  Adoption of DIST from KASCADE. Just what to pick from a normalized
c  prob=(1/MFP)*exp(-T/MFP)  where MFP is the mean free path. MFP is
c	the inverse of the probability/(unit length) which  is a cross
c	section( sort of). 
	r=pran(xdummy)
	t=-alog(r)*mfp       !t distributed exponentially


c E_GAMMA:
c	Determine emitted photon energy E_GAMMA.
c	NOTE:We drop all constant coeficeints.
c 	They are not needed when we use this as part of a rejection scheme.
c	See Rossi and Greisen for the correct coeficients if you want them.

c	Get max value of V*theta(V,U) function by looking at 
c	V_MIN*(Theta(V_MIN,U).  Always bigest value there.

	call brem_intensity(u,ispec,v_min,brem_max)
	
c	Now pick a fraction of the energy from a 1/V distribution with
c	a minimum fractional energy of V_MIN and a max of V_MAX.

100	r=pran(xdummy)
	v=v_min*exp(-alog(v_min**r))	!This function gives a 1/v
					!distribution with a v_min lower limit.
	if(v.gt.v_max)goto 100	!Reject if above v_max
					!(I know I could have included V_max
					! in the distribution function but this
					! probably requires less computation
					! overall and may be clearer).
c	Use rejection method for the 'intensity' function (V*Theta(U,V))
c	Pick our test value.
	test_brem=pran(xdummy)*brem_max
c	Get function that 'modulates' the 1/v probability distribution.
	call brem_intensity(u,ispec,v,brem)

c	Do we keep this value?(MC rejection method used here.)
	if(test_brem.gt.brem)then                             
		goto 100	!try again.
	endif

c	determin photon energy(in TeV).
	e_gamma=v*U

c BREMDEDX:
c	Determine average energy loss(in TeV) due to bremsstrahlung photon 
c	emission below E_MIN (=1MeV) while electron/positron travels distance T.
c	This was found by integrating U*V*Theta(V,U) from 0 to V_MIN.
c	(V_MIN=E_MIN/U).  
c       BREMDEDX was found to tend to a constant for large U and had a value
c	of 1.397998 (=DEDX_MAX)MeV/Rad_length. For all U a curve of the form:
c		bremdedx= dedx_max*(1-A*U**(-B))
c	was fit.

c	NOTE: this fit is only good for E_MIN= 1.0 MeV

c	For safty sakes put as lower limit on bremdedx at 3 MeV
c	We probably will never call this routine with e_mev at this
c	value since all electrons below 10 MeV are dropped.

	if(e_mev.lt.3.0)then				!This is just a fudge
							!so we don't get a low
							!energy problem
		bremdedx=dedx_max*(1.-1.621*(3.**(-1.266)))
	elseif(e_mev.lt.35.)then
		bremdedx=dedx_max*(1.-1.621*(e_mev**(-1.266)))
	else
		bremdedx=dedx_max
	endif

	bremdedx=(bremdedx*1.e-6)*t			!Convert to TeV

	return
	end

	subroutine brem_intensity(u,ispec,v,brem)
c	This routine calculates the 'modulation' of the 1/V distribution 
c	of the photon production probability with  energy V of U(total 
c	electron/position energy in TeV).
c	Note: We call this the intensity since V*Theta(V,U) is also the
c	intensity of emmision at fractional energy V.

c	NOTE:We drop all constant coeficeints in this routine.
c 	They are not needed when we use this as part of a rejection scheme.
c	See Rossi and Greisen for the correct coeficients if you want them.
c	The correct coeficient to get V*Theta(V,U)= probability of emmision at
c	fraction v / radiation length from this routine would
c	be v*theta(v,U)= rad_length*coef*brem
c	Where
c	coef=4.*alpha*n_av*(z_air**2.)*(r_elect**2.)/a_atm
c		where:
c			alpha=1./137.04
c			n_av=6.024e23
c			z_air=7.37
c			r_elect=2.817e-13
c			a_atm=z_air/.5118
c			rad_length=36.6		!Latest value from Tsai, 1974
						!for air. 		
c				or
c			rad_length=43.0 	!Old value used by Bethe-
c						!Heitler and Rossi-Greisen
c						!Use only to compare with their
c						!plots.

c	Using the 3 equationss from Rossi and Greisen, Rev. Mod.Phy.,13,Oct 1941
c	pg.253.(RG) (Don't use complete screening equation. gamma<2 equation 
c	with Butcher-messel f1,f2 parameters is adequate.)
c	Determine the fuction for the appropriate screening. Use
c	parameterization of the f1,f2 functions from Butcher and Messel,
c	Nuc.Phy 20,1960,pg 15-128. (BM) Use a power law fit to the C_FUN(gamma) 
c	function derived from the table given in RG pg.255.
c	Fit was done by G.H.Sembroski, Physics Dept.,Purdue University,
c	W.lafayette,In.  11/5/96
c	We will use the rejection method against these functions to test if we
c	should keep this value of v.

c	U is total energy of electron/positiron in TeV.
c	ISPEC is particle type (Only acceptable values are: 2=positron, 
c	3=electron)
c	V is fraction that goes to photon.
c	BREM is relative intensity value of V*theta(U,V). Actual intensity
c	is BREM*COEF*RAD_LENGTH (see above).
 
c	Written by:
c		Glenn Sembroski
c		Physics Dept
c		Purdue Univ
c		W.Lafayette Indiana
c		05/12/96

c	Modified:


	real u,v,eta,brem
	integer ispec

	real common_term,common_term1,common_term2,common_term3
	real gamma,delta,f1,f2,fn_term1,fn_term2
	real fncs,c_fun,fng15
	include 'kascade.h'
	

c	Determine function value according to screening parameter value..
c	(1-v) is used alot.
	eta=1.-v

c	Determine RG screening parameter
	gamma=100.*(xm(ispec)/U)*(v/eta)*(z_air**(-1./3.))


c	Complete to Intermediate screening: Gamma<2
	if(gamma.lt.2.0)then
			!Determine Screening parameter as used by BM in their
			!f1,f2 parameterizations..
		delta=1.36*gamma
			!Common terms.
		common_term1=1.+ (eta**2)
		common_term2=(2./3.)*eta
		common_term3=(1./3.)*alog(z_air)
			!BM has 2 different f1 f2 parameterizations of Gamma<2
			!f1 f2 functions.
		!First.
		if(gamma.lt.0.8)then
			f1=20.867 - (3.242*delta) + (0.625*(delta**2))
			f2=20.209 - (1.930*delta) + (0.086*(delta**2))
		else
			f1=21.12 - (4.184*alog(delta+.952))
			f2=f1
		endif
		fn_term1= (f1/4.)-common_term3
		fn_term2= (f2/4.)-common_term3
		brem = (common_term1*fn_term1) - (common_term2*fn_term2)

	!Now look at gamma >2
	else
		!Special term for gamma>2 equations
		fncs= alog( ( (2.*u)/xm(ispec)) * (eta/v) ) -.5
		common_term=(1.+ (eta**2))-((2./3.)*eta)
		if(gamma.lt.15.0)then
			c_fun=0.616*gamma**(-1.46)
			fng15=fncs-c_fun
			brem=common_term*fng15
		else
				!Unscreened
			brem=common_term*fncs
		endif
	endif
	return
	end





        FUNCTION CENTER(L,COEF)
C  SAMPLES RAPIDITY FOR CENTRAL PRODUCTION ACCORDING TO THE
C  STENLUND & OTTERLUND FIT. 'COEFF'IS MIXING (<NU>).
c      Not energy dependent.
c      G.H.S. 1/10/86
c             remove E from argumrnt list

        DIMENSION T(25,2)
        DATA T/.018,.053,.116,.209,.325,.451,.572,.681,.771,.844,
     *         .900,.941,.970,.987,.996,.999,9*1.,
     *  4*0.,  .003,.011,.029,.062,.111,.174,.250,.334,.424,.518,
     *         .612,.702,.785,.856,.914,.955,.980,.993,.998,     2*1./
        IC=2
        IF(pran(xdummy).LT.COEF) IC=1
        F1=pran(xdummy)
        DO 10 I=1,25
               IF(F1.LT.T(I,IC)) GO TO 12
   10   CONTINUE
   12   F2=.04*(I-1)
        CENTER=F2 + .04*pran(xdummy)
        RETURN
        END
 

        FUNCTION COEL(L,E,il)
C  CHOOSES ELASTICITY OF LEADING PARTICLES. NEW VALUES FOR
C  LEADING PROTONS INSTALLED.  TSS, DEC '83

c		L is leading particle type
c		il=1  its normal.
c              	il=2  its charge exchange.
c		il=3  its diffractive.
c             Reformatted in the modern style.  Can't tell if
c             E is supposed to be total or Kinectic energy(Tev).
c             Low energy cut is at 100 GEV. 
c             Assume its K.E. in calling programs G.H.S. 1/10/86

c	Modified:

c	06/5/97 GHS
c		Fix the jump to 1. It was done in a bad way before.
c		Also prevent IL from being modified upon return(bad form).
	integer i,il
        DIMENSION P(20,3)
        DATA P/.0675,.1314,.1971,.2628,.3278,.3906,.4505,.5083,.5646,
     *  .6181,.6700,.7206,.7682,.8144,.8592,.9011,.9394,.9697,.9899,1.,
     *  .183,.321,.432,.518,.592,.652,.705,.752,.793,.829,
     *  .862,.890,.915,.936,.954,.970,.982,.991,.997,1.,
     *  .138,.254,.351,.430,.497,.556,.611,.658,.701,.741,
     *  .777,.810,.840,.869,.896,.920,.943,.964,.982,1./
 
c	Save type
	i=il
cDiffractive.
       if(i.eq.3)then
              IF(E.le..1)then
                     COEL=1.-pran(xdummy)/10.
                     RETURN

              else
                     COEL=1.-.005/E
                      RETURN
              endif
	endif

cNot a hadron(proton=13,neutron=14) Must be a meson.
        IF(L.lt.13) then
			!Normal
              if(i.eq.1)then
                     COEL=DIFFPI(1)       
                     RETURN
			!Charge exchange
              else
                     i=3
                     goto 1
              endif
	endif

C	Comes here when a hadron(proton or neutron) and not diffractive
c	or its Charge exchange in which case we've reset i to normal(3)

1	T=pran(xdummy)
	DO J=1,20
		IF(T.LT.P(J,I))then
			COEL=.05*(J-pran(xdummy))
                        RETURN
                endif
	enddo
	print*,' KASLITE--Warning--In COEL: coel not set before return'
	COEL=.05*(20-pran(xdummy))
        END
 


              subroutine comptu(e,u,y)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c compton effect; modified 1/14/85, TRPalfrey, to give exact sgmatotal.
c e  is the photon energy in Mev.
c u is fractional energy of secondary photon
c y is secondary photon production direction cosign.
c alf is the sum of coefficiens alfai
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       include 'kascade.h'

       gama(a,b)=1.-(a*b)/(1.+a**2)       !define function

       umin=e_mass/(2.*e+e_mass)
       alf=0.5-alog(umin)
       fract=0.5/alf
1       g1=pran(xdummy)

c choose the term to model
       if(g1.lt.fract) then              !first term, likely at low e
              g1=pran(xdummy)
              g2=pran(xdummy)
              utemp=1.-abs(g2-g1)       !trick: f(x)=x  0.le.x.le.1
       else                            !second term
              g1=pran(xdummy)
              utemp=umin**g1              !trick: f(x)=1./x xmin.lt.x.le.1
       end if
       a=(1.-utemp)/utemp/e
       ssqth=1.022*a-.2611*a*a              !sin(th-scat)**2

c rejection never follows,except for arithmetic problems
       gg=gama(utemp,ssqth)
       g1=pran(xdummy)
       if(g1.le.gg) then
              y=1.-e_mass*(1.-utemp)/(utemp*e)       !kinematics for 
c                                                 cos(thscat)
              if(abs(y).ge.1.) go to 1       !for round/truncate errors.
              u=utemp
              return
       else
              go to 1
       end if
       end
 

       subroutine datin
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      READ IN THE RUN PARAMETERS
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Modified:
c      G.H.S. 4/4/89
c               Move Open statement into this VAXUSRDAT routine.
c               Read debena directly from input parameter file.

c        G.H.S. 4/27/89
c             Read in the observation height in meters in DATIN.

c        G.H.S. 6/27/89
c             Use external assign for name of input parameter file.

c	G.H.S. 10/12/90 V:1:0.2
c		Add GNDPRT flag. Indicates if particle-on-ground)(pog) file
c		is to be made.

c	23/4/92 G.H.S. V:1:0.6
c		Let the letter for the Magnetic filed indicate values to use.
c		T(for compatability) or L=Haleakala. A for ASGAT. F for none.

c	16/1/97 GHS v:1.1.0
c		Move open of parameter input file to here.
c		Remove NSHOW parameter from data file.

c     08/06/01 GHS V:1:1:3
c               Update for capability for nuclei as primary type. ITYPE is 
c               given as atomic mass number (A) of primary + 20. ie: He(4) has 
c               ITYPE=24; U(238) has ITYPE=258. Use a formula derived for 
c               the liquid drop model to determine Z(charge) for this A.
c               Use nuclei_names lookup table for nuclei name.

!      30/04/03 GHS  V:1:3.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               ./kascade -p g100gev20w.par (sortof like in c).
!               Setup for command line input parameter use.(Changes in main,
!               DATIN, FILEOPEN

!      02/05/03 GHS  V:1:3.1
!               Read in the primary energy (as integer gev) as a command line 
!               parameter. (-e option)

!      13/01/05 GHS  V:1:3.5
!               Segment lengths < .00245 gm/cm**2 have too few single angle 
!               scatters (less than 30) for our gaussian approximation to be
!               accurate. Allow user to specifiy tMax< .00245 but put out a 
!               warning that this results in no multiple scattering..


        include 'kascade_command_line.h'
	character*8 adate
	character*10 time          	
        integer ios
	integer z_nuclei
	real xmass
	integer printseeds
        include 'kascade.h'

c      OPEN INPUT DATA FILE AND READ IN RUN PARAMETERS

       open(4,access='sequential',status='old',readonly, iostat=ios,
     1    file=input_par_file_name)
       if(ios>0)then
          print*,'KASCADE-FATAL-Failure to open input paramter file:',
     1    input_par_file_name       
          stop 'Failure to open input .par file'
       endif


c      GET TIME AND DATE OF THIS RUN(call f90 interface routien)
	call datetime(adate,time)

	WRITE(6,1000)adate(1:4),adate(5:6),adate(7:8),time(1:2),
	1 time(3:4),time(5:)
1000	format(' ',15x,a4,' ',a2,'/',a2,5x,a2,':',a2,':',a,
	1   '--------KASCADE------')

c      GET TIME AND DATE OF THIS RUN(F77 version, vms libraries)
                                   !Use file to init random.
	printseeds=1   !True
        call ranstart(printseeds,random_seed_file_name)

c      Initial particle type
       read(4,8001)segment_head.itype
8001  format(i5)

c      Inital direction cosigns
       read(4,8007)segment_head.dli,segment_head.dmi
8007  format(3f14.6)
c	Reconstruct zenith component.
	       !Forces unit length.Forces downward going
       dni=sqrt(1.-segment_head.dli**2-segment_head.dmi**2)
					!also!

c      THRESHOLD ENERGY for gammas and electron and mu's in Mev.
       read(4,8002)segment_head.etr
8002            format(f20.10)
       do 100 i=1,5
			!Convert to TeV
              thresh(i)=segment_head.etr*1.0e-6
100    continue

c      injection depth in gm/cm2
       read(4,8002)segment_head.depth

c        G.H.S. 4/27/89
c             Read in the observation height in meters in DATIN.
c      lab altitude in meters above sea level.
       read(4,8002) hobs
       segment_head.zobs=gms(hobs)              !Observation level in gms/cm2

c      tmax is maximum length of segment for multiple Coulomb scatterin
       read(4,8006)tmax
8006  format(2f10.3)


c              Debena indicates if particle is to be printed or processed.
       read(4,8004)(debena(i),i=1,20)
8004  format(20l)

c	23/4/92 G.H.S. V:1:0.6
c		Let the letter for the Magnetic filed indicate values to use.
c		T(for compatability) or L=Haleakala. A for ASGAT. F for none.
c      magnet_on  true  WHEN EARTHS MAGNETIC FIELD IS ON
       read(4,8005)segment_head.magnet_field
8005	format(a2)
	if(index(segment_head.magnet_field,'F').eq.0)then
		magnet_on=.true.
	else
		magnet_on=.false.
	endif
	
c      GNDPRT= true  When we want to make a POG file.
       read(4,8004)gndprt

       close(unit=4)

c      WRITE TO DISK FILE
       WRITE(6,7003)segment_head.tep
!*****************************************************************************
!   Add Heavy Nuclei capability
!*****************************************************************************
       if(segment_head.itype.gt.20)then
          call mass_number2charge_mass(segment_head.itype-20,z_nuclei,
	1 xmass)
          write(6,7001)segment_head.itype, trim(nuclei_names(z_nuclei)),
	1 segment_head.itype-20
7001  format('       Itype: Type code for primary particle  =',i5,5x,a,
	1 '(',i3,')')
       else	          
       		write(6,7002)segment_head.itype,namtyp(segment_head.itype)
       endif
       
       write(6,7013)segment_head.dli,segment_head.dmi,dni
       WRITE(6,7004)segment_head.etr
       WRITE(6,7005)segment_head.zobs
       WRITE(6,7006)tmax 

      if(tmax<.00245)then
         print*,'WARNING--****************************************************'
         print*,'WARNING--For Tmax < .00245 gm/cm**2, multiple scattering'
         print*,'WARNING--becomes inaccurate.'
         print*,'WARNING--For Tmax < .001   gm/cm**2 Program will run but' 
         print*,'WARNING--multiple scattering is disabled'
         print*,'WARNING--****************************************************'
      endif


       WRITE(6,7007)segment_head.depth
       WRITE(6,7008)(debena(kk),kk=1,20)
       WRITE(6,7012)magnet_on,segment_head.magnet_field
       write(6,7010)gndprt

c
7003  format('                        Primary energy  TEV   =',f14.4)
7002  format('       Itype: Type code for primary particle  =',i5,5x,a8)
7013  format('      Dli,Dmi,Dni: Direction cosigns primary  =',3f14.5)
7004  format(' Thresh energy(MEV) mus gammas and electrons  =',f14.2)
7005  format('  Zobs:observation height (gm/cm2 from top)   =',f14.4)
7006  format('            Tmax:max seg for mult coul scat   =',f14.4)
7007  format('            Injection depth gm/cm2 from top   =',f14.4)
7008  format('                  Particle enable      1-20   =',20l3)
7012  format('                     Magnet_on:field enable   =',l3,/,
	1    '                    Magnet field values for   =',a)
7010  format('     GNDPRT:Enable POG file creation(=True)   =',l3)
       return

       end

       subroutine daughter(energy,height,x,y,tim,ispec,rk,pplab,ipar,
     1 itrace)
c      calculate the direction cosigns of this daughter particle and
c      save in the appropiate storage array.
c      energy=Total energy of the particle.(Tev)
c      ipar=parent type.
c      itrace=tells which call to daughter this was for debugging trace
c             purposes.

c      Modified:

c      4/7/89 G.H.S.
c            Fix a consistency check on dn.

c	9/26/91 G.H.S.V:1:0.5
c		There were 2 lines for dl and dm. dl dm are not used in
c		this routione. Deleted them.
c		

       common/daught/ a(9),b(3),r9(3)       !For decay direction cosins.
       equivalence (dl1,r9(1)),(dm1,r9(2)),(dn1,r9(3))
       include 'kascade.h'
       
c       Don't check for threshold here. do it when its called back out.
       plab=sqrt(energy**2-xm(ispec)**2)       !Magnitude of lab momentum
       dn=(pplab/plab)              !direction cosin rel to track of particle
       if(abs(dn).lt.1.e-8)then
              dn=1.e-8       !Cheap way to handle horizontal tracks.
				!V:1:0.5 Deleted 2 lines here to set dl dm
				!dl=(1.-1.e-8)*dl	dl,dm not used and
				!dm=(1.-1.e-8)*dm	they are local.
       endif

c        CONSISTENCY CHECK!
       if(dn**2.gt.1.0)then              !Take care of unlikly round off error
              write(6,7899)pplab,plab,dn,itrace
7899  format(' ***DAUGHTER--CONSISTENCY CHECK:pplab,plab,dn,itrace:',
     1 /,' ***',3e14.7,i4)
              write(6,1234)energy,height,x,y,tim,ispec,rk,
     1 xm(ispec)
1234  format(' ***energy,height,x,y,tim,ispec,rk,xm(ispec):',
     1 /,' ***',5e14.7,i5,/,' ***',2e14.7)
               dn=1.       !Round it
        endif



       sinlab=sqrt(1.-dn**2)       !Sin in lab of daugter to primary
       b(1)=cos(rk)*sinlab       !This is  unit vector for the daughter
c                            in the lab frame relative to the 
       b(2)=sin(rk)*sinlab       !parent initial direction
       b(3)=dn                     !gaurenteed unit length this way.
       call mtxmlt(a,b,r9)       !multiply matrix. Get unit vector in lab

       vlength=sqrt(dl1**2+dm1**2+dn1**2)       !Normalize it
       dl1=dl1/vlength              !save direction cosins of this daughter
       dm1=dm1/vlength              !Preserves signs.
       dn1=dn1/vlength              !d*1 equivalenced to r9 and is daughter
c                                   unit vector

       if(abs(vlength-1.).gt.1.e-5)write(6,1760)dl1,dm1,dn1,vlength
1760  format(' ***Daughter:Found Unit vector correction .gt.1.e-5.
     1 dl,dm,dn,,vlength:',4e14.7)

       tedaughter=energy-xm(ispec)
       if(ispec.eq.2.or.ispec.eq.3)then       !Electrons-positrons
              call stor_e(tedaughter,height,dl1,dm1,dn1,x,y,tim,ispec)
              if(debena(ipar))then
                     write(6,1000)namtyp(ispec),nen,
     1 tedaughter,height,dl1,dm1,dn1,x,y,tim
1000  format(' ',16x,a8,':',i3,3x,f10.6,f13.0,2(f8.5,','),f8.5,
     1 2f10.1,f12.1)
                     endif
              else
              call stor_g(tedaughter,height,dl1,dm1,dn1,x,y,tim,ispec)
              if(debena(ipar))then
                     write(6,1000)namtyp(ispec),ngn,
     1 tedaughter,height,dl1,dm1,dn1,x,y,tim
                     endif
              endif
       return
       end
       


      SUBROUTINE DECAY(tenergy,height,dl,dm,dn,x,y,tim,ispec)
C**************************************************************
C   THREE DIMENSIONAL VERSION OF DECAY WITH 18 PARTICLE TYPES *
C THREE BODY DECAYS ARE NOT CORRECT - SEE THE CORRECT VERSION *
C IN 'DECAY1.FOR'[NTRINO]        TSS, OCT '85, MADISON        *
C**************************************************************  

c      G.H.S.1/2/85
c             True 3 diem. Save daughters in storage arrays den dgn.

c      G.H.S 1/8/86
c             Do things exactly for energy calcultions using Tenergy as
c             Kinetic energy.

c      G.H.S. 5/14/86       Treat low energy (<2.5 Gev) nucleon-delta production
c                    as a three body decay.

c      G.H.S.     5/19/86       
c                Treat gamma compton interaction as a two particle decay.

c      G.H.S.     4/7/89
c                Fix calls to daughter in electron scatter so that ispec is not
c                changed.  Doesn't fix anything but its cleaner.

c	25/3/92 M.P.K. V:1:0.6
c		Compton scattering was scattering positrons, not 
c		electrons. Fixed it.


c      Tenergy=Kinetic energy of primary in lab(Tev)
c      height=altitude in meters above sea_level of decay
c      dl,dm,dn=direction cosigns of the primary
c      x,y=cord of decay in meters
c      tim= tim of decay in nano sec.
c      ispec=particle type of primary
c      Particle species codes.
c             1:Gamma
c              2:positron
c              3:electron
c              4:muon (+)
c              5:muon (-)
c                    6:pion (0)
c                    7:pion (+)
c                    8:pion (-)
c                    9:kaon (+)
c            10:kaon (-)
c            11:kaon (0long)
c            12:kaon (0short)
c            13:proton
c            14:neutron
c            15:neutrino(electron)
c            16:anti-neutrino(electron)
c            17:neutrino(muon)
c            18:anti-neutrino(muon)

       DIMENSION LD1(11),LD2(11),AW(11),AU(11),LT1(17),LT2(17),LT3(17)
       dimension ltd1(14),ltd2(14),ltd3(14)

       common/daught/ a(9),b(3),r9(3)       !For decay direction cosigns.
       equivalence (dl1,r9(1)),(dm1,r9(2)),(dn1,r9(3))
       include 'kascade.h'
       qq(p,u)=sqrt(p*p+u*u)


cTwo particle decay types:
       DATA LD1/ 1, 2, 3, 4, 5, 4, 5, 7, 8, 6, 7/       !First daughter type
       data LD2/ 1,15,16,17,18,17,18, 6, 6, 6, 8/       !Second daughtewr type.

cThree particle decay types:
       data LT1/ 1, 2, 3, 8, 6, 6, 7, 7, 6, 6, 8, 7, 8, 7, 8, 6, 7/  !First type
       data LT2/ 2,15,16, 7, 2, 4, 6, 8, 3, 5, 6, 3, 2, 5, 4, 6, 8/ !Second 
       data LT3/ 3,18,17, 7,15,17, 6, 8,16,18, 6,16,15,18,17, 6, 6/ !Third

cDelta production decay types.
       data ltd1/13,14,14,13,14,13,14,14,13,13,14,13,14,13/       !Nucleon type.
       data ltd2/14,13,14,13,14,14,13,13,14,13,14,13,13,14/       !Delta nucleon.
       data ltd3/ 8, 8, 6, 8, 7, 6, 6, 7, 7, 6, 7, 8, 6, 6/       !Delta pion.
       data xmd/0.001230/       !Delta mass in Tev.

cTwo particle decay parameters:
c       p1=M*AW=sqrt((M**2-(m1+m2)**2)*(M**2-(m1-m2)**2)/(4*M**2))
       DATA AW/ 0.5, 2*0.4999933, 2*0.2134358,  2*0.4770957,
     1 2*0.4155391,   0.4200673,    0.4139481  /

c       e1(daughter,total)=AU*M(parent)=(M**2+m1**2-m2**2)/(2*M)
       DATA AU/ 0.5, 2*0.5000067, 2*0.7865642,  2*0.5229043,
     1 2*0.5025933,   0.5,          0.5        /


       if(debena(ispec))then
              nn=ngn+1
              if(ispec.le.3.and.ispec.ne.1)nn=nen+1    !Get id # for this part.
              write(6,1000)namtyp(ispec),nn,tenergy,height,
     1 dl,dm,dn,x,y,tim
1000  format('0',30x,'Tenergy(Tev)  Alt(meters)    Direction cosigns ',
     1 '       X,Y(meters)    Time(nsec)',/,' ',a8,':',i5,' >>>Dec',
     2 10x,f10.6,f13.0,2(f8.5,','),f8.5,2f10.1,f12.1)
              if(ispec.gt.12)then
                     write(6,1002)
1002  format(' Delta production.')
                     endif
              endif

       XM0=XM(ispec)              !Get mass of parent

       IF(tenergy.LT.XM0)then       !Drop the decay if parent particle
              if(debena(ispec))then       !Non-relativistic.
                     write(6,1001)
1001  format(' ',31x,' droped')
                     endif
              RETURN
	endif



cSet up arrays for transformation matrix for subroutine_ daughter.

       f=qq(dl,dm)
       a(1)=dl*dn/f
       a(2)=-dm/f
       a(3)=dl
       a(4)=dm*dn/f
       a(5)=dl/f
       a(6)=dm
       a(7)=-f
       a(8)=0.
       a(9)=dn
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
c

       tEParent=tenergy
       if(tenergy.eq.xm(ispec)) teparent=tenergy+1.E-9       !Special case. 
							     !Stopped kaons.

c       find the parent.
       GO TO(397,398,398,400,401,402,403,404,405,406,407,408,409,409),
     1 ispec
c              5/14/86 Below 2.5 Gev proton and neutron delta production is
c              treated as 3 body decay.
c              Gammas can 'decay' for compton scattering.
c              But not electron yet.
398       WRITE(6,1900) ispec
1900  format(' ***Decay:WRONG PARTICLE type=',I9)
       RETURN

cGamma: ispec=1       Compton only. Pair production done in UNICAS.
c                     Since compton production needs to call daughter
c                     to set up decay angles its done here.
397       emev=tenergy*(1.0e+6)              !Mev energy of parent gamma.
                                   !through common block.
       call comptu(emev,u,tix)              !Do the Compton scatter, U is 
c                            fraction of intial energy scattered gamma has.

       egamma=u*tenergy       !gamma energy after scatter in Tev.
       pp=tix*egamma              !tix=gamma scattering direction cosign
       rk=twopi*pran(xdummy)              !Pick a random phi.

       ll1=1             !scattered gamma
       ll2=3             !'decayed' product electron.

c                                          Save the scattered gamma.
       call daughter(egamma,height,x,y,tim,ll1,rk,pp,ispec,1)

       ee=(1.-u)*tenergy+xm(ll2)       !Total energy of 
c                                          Compton-struck electron in tev.

       if(ee.lt.thresh(ll2))return       !Check electron threshold.
       plab=sqrt(ee**2-xm(ll2)**2)       !Total momentum in tev 
c                                          of produced electron.
       pperp=egamma*sqrt(1.-tix**2)       !perp momentum of electron and 
c                                   gamma.
       if(plab.lt.pperp)then
              write(6,3400)u,tix,tenergy,egamma,ee,plab,pperp,pp
3400  format(' ***Decay-FATAL-:u,tix,tenergy,egamma,ee,plab,
	1 pperp,pp:'/,' ***',8e14.7)
              stop 
              endif

       pp=sqrt(plab**2-pperp**2)       !P parrellel for electron.

       rk=rk-pi                     !Other phi.
       call daughter(ee,height,x,y,tim,ll2,rk,pp,ispec,2) 

       return


c       Muon +  ispec=4  
400       K=2                            !decay: e+ neutrino anti-neutrino
      GO TO 3000

c       Muon-        ispec=5
401       K=3                            !decay: e- neutrino anti-neutrino
      GO TO 3000

c       Pi 0  ispec=6  
402       IF(pran(xdummy) .le. 0.9885)then
              K=1                     !decay: gamma gamma
              GO TO 2000
              else
              K=1                     !decay: gamma e+ e-
              GO TO 3000
              endif

cPi +  ispec=7
403       K2=0              !Offset
       GO TO 805

cPi -  ispec=8
 404  K2=1              !Offset

 805  IF(pran(xdummy) .le. 1.2468E-04)then
              K= 2+ K2              !decay: e neutrino
              GO TO 2000
              
              else
              K= 4+ K2              !decay: muon neutrino
              GO TO 2000
              endif

cKaon +  ispec=9
 405  K2=0       !Offsets
      K3=0
      GO TO 425

cKaon - ispec=10
 406       K2=1
      K3=4

425       R=pran(xdummy)
      IF ( R .le. 0.635 )then
              K = 6 + K2              !decay: muon neutrino
              GOTO 2000
              endif
       IF (R .le. 0.8466)then
              K= 8 + K2              !decay: pi+ pi0
              GOTO 2000
              endif
       IF (R .le. 0.9025)then
              K= 4 + K3              !decay: pi+ pi+ pi-
              GOTO 3000
              endif
       IF (R .le. 0.9507)then
              K = 5 + K3              !decay: pi0 e+ neutrino
              GO TO 3000
              endif
       IF (R .le. 0.9827)then
              K = 6 + K3              !decay: pi0 mu+ neutrino
              GOTO 3000

              else
              K = 7 + K3              !decay: pi+ pi0 pi0
              GO TO 3000
              endif

cK-long  ispec=11
407       R=pran(xdummy)
       IF (R .le. 0.194)then
              K = 12                     !decay: pi+ e- neutrino
              GOTO 3000
              endif
       IF (R .le. 0.388)then
              K = 13                     !decay: pi- e+ neutrino
              GOTO 3000
              endif
       IF (R .le. 0.523)then
              K = 14                     !decay: pi+ mu- neutrino
              GOTO 3000
              endif
       IF (R .le. 0.658)then
              K = 15                     !decay: pi- mu+ neutrino
              GOTO 3000
              endif
       IF (R .le. 0.873)then
              K = 16                     !decay: pi0 pi0 pi0
              GOTO 3000

              else
              K = 17                     !decay: pi- pi+ pi0
              GOTO 3000
              endif


cK-short  ispec=12
408       IF (pran(xdummy) .le. 0.3139)then
              K=10                     !decay: pi0 pi0
              GO TO 2000

              else
              K=11                     !decay: pi+ pi-
              goto 2000
              endif


cnucleon-delta production ispec=13,14 (tenergy<2.5 Gev)
c                            G.H.S.5/14/86 Treat nucleon-delta production
c                            as a three body decay.
409       if(tenergy.gt.0.0025)then
              WRITE(6,1900) ispec      !Shouldn't get here if more 2.5 Gev.
              return
              endif                     
       r=1.                             !forward or back delta important. 
       if(pran(xdummy).lt.0.5)r=-1.       !Direction cosign in cm only allowed 2
c                                   values in delta producton (1,-1)
       rd=pran(xdummy)                     !Pick the decay mode.
       if(rd.lt.0.375)then
              kd=1                            !n goes to p + (n,pi-)
       else if(rd.lt.0.4375)then
              kd=2                            !n goes to n + (p,pi-)
       else if(rd.lt.0.5)then
              kd=3                            !n goes to n + (n,pi0)
       else if(rd.lt.0.75.and.r.eq.1.0)then
              kd=4                            !n goes to p + (p,pi-) (forward
c                                          delta).
       else if(rd.lt.0.75.and.r.eq.-1.0)then
              kd=5                            !n goes to n + (n,pi+) 
c                                          (backwards delta).
       else if(r.eq.1.0)then
              kd=6                            !n goes to p + (n,pi0) (forward
c                                          delta).
       else
              kd=7                            !n goes to n + (p,pi0)
       endif

       if(ispec.eq.14)kd=kd+7              !For proton primary: charge exchange.
c                                   pi+ goes to pi-, p goes to n, and vice-
c                                   versa.
       ll1=ltd1(kd)       !Nucleon.
       ll2=ltd2(kd)       !Delta nucleon.
       ll3=ltd3(kd)       !Delta pion.

c                     Set up to use 3 body decay code.
c                     Treat as nucleon-nucleon goes to nucleon-delta and
c                     then delta goes to nucleon-pi
       etot1=tenergy+xm(ispec)
        Stev =2*xm(ispec)**2+2*xm(ispec)*etot1
c                                   Center-of-mass energy**2  for nucleon-
c                                   nucleon collision.
c                                   Approximation here that the target and
c                                   incident particles are of the same 
                                   !mass.

        IF (sqrt(Stev).lt.(xm(ll1)+xmd))then       !Need at least sum of rest 
              if(debena(ispec))then
                     write(6,1003)etot1,sqrt(stev)       !masses in c.m to 
1003  format(' Delta   :drop',21x,' total incident energy:',f8.6,
     1 'available C.M. energy:',f8.6)        !create the
c                                           nucleon-delta pair. If not 
                     endif
c                                          enough, quit.
              return
              else              !Scatter nucleon-nucleon to nucleon-
c                            delta.
c                     Try to create a nucleon-delta pair.
c                     Use as the 'particle' that will decay the c.m. of the
c                     incident nucleus and the target nucleus.
              enuc=(stev+xm(ll1)**2-xmd**2)/(2*sqrt(stev))       !Energy of the
c                                   daughter nucleon in the rest frame(C.M)
c                                   of the decaying particle.
              xm0=sqrt(stev)       !If we have a single fictious particle at rest 
c                            in the c.m. system with energy sqrt(stev) then
c                            that will also be its mass.
              U=enuc/xm0
               pnuc=SQRT(enuc**2-xm(ll1)**2)
              W=pnuc/xm0

              xm2s=(xmd/xm0)**2       !Normalized Delta mass squared.

              eparent=etot1+xm(ispec)       !energy of reaction in lab frame. 
c                                   combined incident nucleon and target 
c                                   nucleon.
c                     Following variables needed for jump to 3200:
c                     eparent(lab),xm0(lab),U(daugh nuc),W(daugh nuc.),
c                     r(daugh .Nuc.),and xm2s(delta),ll1,ll2,ll3
              goto 3200
              endif 
       
cTwo particle decay
       
2000       LL1 = LD1(K)       !Daughter #1 type
       LL2 = LD2(K)       !Daughter #2 type

      W = AW(K)              !p1/M In c.m.
      U = AU(K)              !e1/M in c.m.

       eparent=teparent+xm0       !Total energy of parent(Tev)
       beta=sqrt(1.-(xm0/eparent)**2)       !Beta of c.m.

       r=2.*pran(xdummy)-1.       !pick direction cosign in cm (its evenly
c                            distributed between -1 and 1)(cos(theta))
       edaugh= eparent*(U+beta*W*r)       !Total Lab energy of first daughter(Tev)

       pplab=eparent*(W*r+beta*U)       !p parrallel in lab

       rk=twopi*pran(xdummy)       !sample phi which also remains
c                            unchanged by the lorentz transformation

       call daughter(edaugh,height,x,y,tim,ll1,rk,pplab,ispec,3)

c Second daughter
       edaugh=eparent-edaugh              !Total energy conservation(Tev)
       pplab=eparent*(W*(-r)+beta*(1-U))       !P parrallel in lab
c                                   Oposite sign for other daughter
       rk=rk-pi              !Reverse azimutial direction also
       call daughter(edaugh,height,x,y,tim,ll2,rk,pplab,ispec,4)

       return

cThree particle decay. Use Dalitz triangle method to pick point in phase
cspace.  Approximation implied is that the decays have a constant distribution
cin phase space.
cOne explination of this method can be found in the book "Relativistic 
c Kinematics" by R.Hagedorn (Cern) pub by: W.A.Benjamin,Inc. New York 1963.
c pages100-106.

 3000 XM0 = XM(ispec)       !Parent mass.
      LL1 = LT1(K)       !daughter types.
      LL2 = LT2(K)
      LL3 = LT3(K)
c Normalize things by dividing by parent mass.
      XM1 = XM(LL1)/XM0              !normalized daughter masses.
      XM2 = XM(LL2)/XM0
      XM3 = XM(LL3)/XM0

      Q = 1.-XM1-XM2-XM3       !Normalized K.E. of the decay

cThe Dalitz triangle method uses the fact that in an equilateral triangle
cthe sum of perpendicular distances of any point inside the triangle to the
cthree sides is a constant. Let this constant be the Total Kinetic energy (Q)
cand the three distances will be the individual K.Energies of the daughter
cparticles.
cChoose randomly with an even distribution a point within the triangle.
cActually choose a point within the biggest circle that can fit within the
ctriangle since this is an upper limit due to momentum conservation(Non-
crelativistic).

cChooserandomly the polar cord  of the point within a triangle of height Q.
cR is distributed as f(x)=x 
3100       R = SQRT(pran(xdummy))

      CALL SINCO(S,C)       !Pick a random angle.

      tE1 =(Q/3.)*(1.+R*C)       !K.E. of daughter 1.
      tE2 = (Q/3.)*(1.-R*(.5*C+.866025404*S))
      tE3 = Q-tE1-tE2

cNow demand relativistic momentum conservation.
      P1S = tE1*(tE1+2.*XM1)       !Momentum**2 for daughter 1
      P2S = tE2*(tE2+2.*XM2)
      P3S = tE3*(tE3+2.*XM3)

cTest the momentum
      IF ((P1S-P2S-P3S)**2.GE.4.*P2S*P3S) GOTO 3100       !Loop until we find
c                                                 a legal point
cWe now have the ke and p of the three particles.Convert to lab
cFirst consider particles 2+3 as one particle and find things as if
cthis were a two body decay.
      XM2S = (1.-XM1)**2-2.*tE1       !Mass**2 of combined particle
      XM1S = XM1*XM1              !mass**2 of daughter 1

cCaluculate parameters for this '2 body decay'
       DEL = XM1S-XM2S
      U = .5+.5*DEL       !e1/M
      W = .5*SQRT(1.+DEL*DEL-2.*(XM1S+XM2S))      !p1/m
      
       R = 2.*pran(xdummy)-1.       !Pick direction cosign (its evenly distributed
c                            between -1 and 1)
       eparent=teparent+xm0       !Total energy of parent(Tev)
c                                   Below is entry point for 'delta' 3 body
c                                   decay.
3200       beta=sqrt(1.-(xm0/eparent)**2)       !Beta of c.m.
       Edaugh = eparent*(U+beta*W*R)              !Lab energy of first daughter
       pplab=eparent*(W*r+beta*U)       !P parrallel in lab
       rk=twopi*pran(xdummy)       !sample phi which also remains
c                            Constant
cDaughter #1
       call daughter(edaugh,height,x,y,tim,ll1,rk,pplab,ispec,5)

cComposit
       energy=eparent-edaugh              !Total Energy of composite(Tev,lab)

       pplab=eparent*(W*(-r)+beta*(1.-U))       !P parrallel in lab
       pperp=xm0*w*sqrt(1.-r**2)       !Pperp in cm and lab
       plab=sqrt(pperp**2+pplab**2)       !Doing it this way insures that 
c                                   pplab is less then plab

       dn3=pplab/plab              !Direction cosign of composit

       sinlab=sqrt(1.-dn3**2)       !Sin in lab
       rk=rk-pi              !Revers azimutial direction also
       b(1)=cos(rk)*sinlab       !This is  unit vector for the composite
c                            in the lab frame relative to the 
       b(2)=sin(rk)*sinlab       !parent initial direction

       b(3)=sqrt(1.-sinlab**2)       !gaurenteed unit length this way.
       call mtxmlt(a,b,r9)       !multiply matrix. Get unit vector in lab

       vlength=sqrt(dl1**2+dm1**2+dn1**2)       !Normalize it
       dl1=dl1/vlength              !save direction cosigns of this daughter
       dm1=dm1/vlength              !Preserves signs
       dn1=dn1/vlength              !d*1 equivalenced to r9 and is daughter
c                            unit vector
       if(abs(vlength-1.).gt.1.e-5)write(6,1000)dl1,dm1,dn1,vlength
1560  format(' ***Decay1:Found Unit vector correction .gt.1.e-5.
     1 dl,dm,dn,vlength:',4e14.7)

c       Dl1,dm1,dn1 is unit vector in lab of the composite direction
c       enter it into the transformation matrix.

       f=qq(dl1,dm1)
       a(1)=dl1*dn1/f
       a(2)=-dm1/f
       a(3)=dl1
       a(4)=dm1*dn1/f
       a(5)=dl1/f
       a(6)=dm1
       a(7)=-f
       a(8)=0.
       a(9)=dn1

       XM0 = XM0*SQRT(XM2S)       !Total mass of the composite
      XM1S = (XM(LL2)/XM0)**2       !Normalized mass**2 of daughters 2 ,3
      XM2S = (XM(LL3)/XM0)**2

cParameters for daughter 2
       DEL = XM1S-XM2S
      U = .5+.5*DEL       !e1/M
      W = .5*SQRT(1.+DEL*DEL-2.*(XM1S+XM2S))         !p1/m
      
       R = 2.*pran(xdummy)-1.      ! Pick direction cosign (its evenly distributed
c                            between -1 and 1)

        beta=sqrt(1.-(xm0/energy)**2)       !Beta of c.m.
       Edaugh = energy*(U+beta*W*R)              !Lab energy of first daughter
       pplab=energy*(W*r+beta*U)       !P parrallel in lab
       rk=twopi*pran(xdummy)       !Get azmuthial phi for direction cos.

cdaughter #2
       if(abs(pplab).gt.sqrt(edaugh**2-xm(ll2)**2))then
              write(6,1273)edaugh,energy,beta,u,w,xm0,rk,pperp,
     1 xm(ll2),ll2
1273  format(' ***Decay:edaugh,energy,beta,u,w,xm0,rk,pperp,
     1 xm(ll2),ll2:',/,' ***',5e14.7,/,' ***',4e14.7,i5)
              endif

       call daughter(edaugh,height,x,y,tim,ll2,rk,pplab,ispec,6)

cdaughter #3
       edaugh=energy-edaugh              !Energy particle 3.(Tev.)
       pplab=energy*(W*(-r)+beta*(1-U))       !P parrallel in lab of daughter
       rx=rk-pi
       if(abs(pplab).gt.sqrt(edaugh**2-xm(ll3)**2))then
              write(6,1273)edaugh,energy,beta,u,w,xm0,rk,pperp,
     1 xm(ll3),ll3
              endif
       call daughter(edaugh,height,x,y,tim,ll3,rk,pplab,ispec,7)
       return
       END
 
        FUNCTION DIFFPI(N)
C
C Sample from (1-x)**N distribution for the diffractive pions
c      Note no energy dependence.
c      G.H.S/ 1/10/86
c             Remove E from argument list.
        D=pran(xdummy)
        DO 1 I=1,N
        F1=pran(xdummy)
    1   IF(F1.LT.D) D=F1
        DIFFPI=D
        RETURN
        END
 
 

       FUNCTION FPNI(Tenergy,L)

c      This function_ returns with the interaction length for a particle
c      of type L with kinectic energy Tenergy(Tev).

       DIMENSION XS(21,3)
       DIMENSION AXX(3),BXX(3),CXX(3)
       DATA XS/53.,106.,163.,166.,148.,136.,124.,111.,113.,115.,
     1 115.,114.,115.,3*117.,5*119.,              !Pion,Muon,electron,gamma?
     2 8*1.e3,171.,107.,100.,96.,94.1,92.3,91.3,90.9,90.7,4*90.6,       !Kaon
     3 6*1.e3,15*130./                            !Hadorns

       DATA AXX/196.,280.,178./       !Parameter arrays for high energy things
     1 BXX/1.70,2.50,1.60/
     2 CXX/0.067,0.1,0.067/

       IFF=1                            !Use index 1 for gammas,electrons,and
c                                   pions.
       IF(L.GT.8) IFF=3              !Index 3 for kaons.
       IF(L.GT.12)IFF=2              !Index 2 for hadrons

c       check the energy
c       For high energy(>100 gev) Use parametric equation.
       IF (tenergy .GT. 0.1)then
              FPNI=2.4E4/(AXX(iff)+BXX(iff)*
     1  ALOG((tenergy/CXX(iff))**(1.8)))
              return
              endif

c       Medium energy: 2.3 to 100 Gev
       IF(tenergy.GT..0023)then
              FPNI=XS(21,IFF)

              else
c       Low energy: 300 to 2300 Mev.
              NE=(tenergy-3.E-4)/1.E-4 + 1
              FPNI=XS(NE,IFF)
              endif
       return
       END
 
 
 
        FUNCTION FPTE(E0)
C ENERGY DEPENDENCE OF THE AVERAGE PT
c      G.H.S1/8/86 Since this is constant for energys below 100gev it
c      doesn't matter if e0 is total or Kinetic energy.
c      Im not sure of the units of this result(FPte)
c      It May be some kind of ratio.
c      E0=energy(tev)
        IF(E0.le.0.1)then
c       For low energis:below 100 Gev
              FPte=1.              !Pperp average is always constant
              return

              else
c       High energies:>100Gev
              FPte=1.+ 0.08*ALOG10(E0/.1)
              RETURN
              endif
        END

! *************************************************************************

	subroutine fileopen
!*************************************************************************
!	Open output segment file
!*************************************************************************

c	Written by:

c	G.H.Sembroski
c	Physics Dept
c	Purdue University
c	W.Lafayette, In 47907
c	E-Mail: "sembroski@physics.purdue.edu"
c	16/1/97

c        Modified:
!      11/12/02 GHS  V:1:2.1
!               Use kascade.seg as output file link.
!      30/04/03 GHS  V:1:3.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               ./kascade -p g100gev20w.par (sortof like in c).

       integer ios
       include 'kascade_command_line.h'
       include 'kascade.h'

c     Open output file that saves shower data.
c    File name specified by assignment statement to kascade.seg
	open (2,access='sequential',status='new',form='unformatted',
     1 file=segment_output_file, iostat=ios)
         if(ios>0)then
            print*,'KASCADE--FATAL-Failure to open output seg file:',
     1 segment_output_file           
            stop 'Failure to open output .seg file'
         endif

c       !The output file will consist of all the segemnts of tracks in
c       !the showers.  The first record of a shower will have the inital
c       !parameters of the shower.

       write(2)(segment_head.recbuf(i),i=1,segment_head_size)

       INEXT=0 !Counts segments

       return
       end

!*****************************************************************************

       subroutine geom(dl,dm,dn,tix)
!*****************************************************************************
!      geom is a subroutine_ which determines
!      the new direction cosines of the particle
!              dl dm dn dir cos of segm
!       input:       tix       m.c.s. angle
!       output:       dl dm dn new dir cos
!*****************************************************************************

!	Mofified:

c     10/23/86 G.H.S.       Always force final direction vector to be unit
c                    length. Adjust dn to make it so. Does this cause
c                    any bias??? May become non unit length by sucessive
c                    round off error.

c	15/12/93 GHS
c		Convert to real*8 internally to prevent round off problems
c		for small tix.

!	15/9/99 GHS V:1:1.2
!		Move r*8 version to KASCADE source code. Rename the r*8 
!		MTXMLT as DMTXMLT
!		Also, fix nomalization of r*8 dl,dm,dn

        real*8 pi
	parameter  (pi=3.141592654)
	real*8 a(9),b(3),r9(3),f,rk,c,s,vlength
	real*8 dn_local,dl_local,dm_local,qq,p,u,tix_local
	real dl,dm,dn,tix
       qq(p,u)=sqrt(p*p+u*u)


	tix_local=tix		!Convert input variables to r*8 internaly
	dl_local=dl
	dm_local=dm
	dn_local=dn
				!Normalize. Make all r*8 to prevent
				!round of problems for small tix
	vlength=sqrt(dn_local**2+dl_local**2+dm_local**2)
       if(abs(vlength-1.).gt.1.e-5)then
		write(6,1001)dl_local,dm_local,dn_local,vlength
1001  format(' ***Geom:Found Input Unit vector correction .gt.1.e-5.
     1 dl,dm,dn,vlength:',/,' ***',4e14.7)
	endif
	dl_local=dl_local/vlength
	dm_local=dm_local/vlength
	dn_local=dn_local/vlength

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
       call dmtxmlt(a,b,r9)
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
!****************************************************************************

              subroutine dmtxmlt(a,b,r)
!****************************************************************************
!      Calculates r=Ab, where r,b are vectors and A is matrix, all 3-d.
!****************************************************************************

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


        function gauss(x)
c	Modified:
c	2/4/92 G.H.S. V:1:0.6
c		Fix width of GAUSS. It was .6932. Make it 1.0 so nfluct has
c		correct width.

c	NOTE: This funciton only gives values out to 3/.6932=4.33 sigma and 
c	above 3 sigma its not perfect(but its not bad!).

c     This is petes for use by nfluct.
        sum=pran(xdummy)
        do 100 i=1,5
                sum=sum+pran(xdummy)
100     continue
        gauss=(sum-3.)/.6932
        return
        end



        subroutine knock_on(tenergy,tsegment,ispec,dl,dm,dn,tim,
	1 zdni,xm,zbeta,zgamma,xstart,ystart,hstart,hend,t_min,num,qz)
!*****************************************************************************
c       Implimentation of Knock-on electron generation from a projectile
c	track segment.
!*****************************************************************************
c	Written by:

c	G.H.Sembroski
c	Physics Dept
c	Purdue University
c	W.Lafayette, In 47907
c	E-Mail: 47474::SEMBROSKI
c	Feburary 26 1992.  
c       See Glenns Notebook:Gamma-Ray Monte-Carlo vol.4 pg 23

c       The fromulas are taken from the Particle Properties Data Booklet, 1984
c	pg 110-111.
c	These are essentialy the formulas from Rossi: High Energy Particles
c	1952.
     
c	Input parameters:
c	tenergy:	Kinectic energy(TeV) of projectile.
c	tsegment:	Gm/cm**2 that projectile travels in this track segment.
c	ispec:		Species type:2=e+,3=e-,4=mu+,5=mu-,7=pi+,8=pi-,9=k+,
c			10=k-,13=Proton.
c	dl,dm,dn:	Direction cosigns of projectile.
c	tim:		Time (nanasec) differerence of when a non-interacting
c			primary would have reached altitude HSTART vs when 
c			this projectile reached it.
c	zdni:		R*8 Verticle direction cosign of inital primary particle
c	xm:		Mass in TeV/c**2 of projectile.
c	zbeta,zgamma:	R*8 kinectic values for projectile.
c	xstart,ystart:	x,y coords(meters) of start of projectile track segment.
c	hstart:		Altitude (above sea level in meters) of start of
c			projectile track segment.
c	hend:		Altitude (above sea level in meters) of end of
c			projectile track segment.
c	t_min:		Minimum energy of a knock-on electron were interested
c			in.(TeV). Typically 20e-6 TeV: thresh(3)
c	num:		i*4 Number of knock_on electrons made.
c	qz:		Charge of projectile.

c	Method:
c	1:Determine expected # of  knock-on electrons created.
c	       Differential rate is of form:
c		rate=const*(qz/beta)**2*F/Tenergy**2
c		There is a different F for each spin(1/2,0,1)and for electrons
c		and positrons.  Total expected rate  is integrel of this 
!		funtion from 
c		t_min to T_max	times gm./cm**2 it went through(tsegment).
!		This number is then poisson fluctuated to give number created
!		for this segment.
c	2:Pick T of created electron from F/(beta*t)**2 distribution. 
c		Easiest way is to pick from 1/t**2 dist by direct method and 
c		then using that t use rejection method to pick from F/beta. If
c		F is fairly flat this is fairly efficient. I'd guess for 
c		all particle types this scheme is about 50+% efficient.

c	3:With T known, determine angle of electron to dl,dm,dn of original
c		track. Use GEOM to determine direction cosigns of electron.
c		DetermIne emmission point along projectile track segment
c		(random) and enter the knock-on electron into the electon 
c               stack.
    
!Modified:

!       21/06/01 GHS V:1:2.0
!		Heavy ion knock-on production(ispec>20):
!		1:Treat as spin 0 (A=even) or spin 1/2(A odd) This choice is
!                 my best guess. I don't see it verified anywhere. GEANT (from
!		  CERN) treats all heavies as spin 1/2.
!		2:Inlcude Z**2 in rate calculation.
!		3:Poission fluctuate number created along track. Replaces use
!	          of KNOCK_MADE for all knock-on generation. I tested this 
!                 method and it appears to work just as well as any other 
!                 method I or Pete Palfrey could come up with for all ranges 
!                 of rate
!               4:For spin 0,1/2 Function fun now has max of 1.0 (removed a 
!		  1/beta**2 factor, which in almost all cases had no effect)

	real*4 tenergy,tsegment,dl,dm,dn,tim,xm,xstart,ystart,hstart,
	1 hend,t_min,em,z_over_a,d,const,t_max,total_rate,t,fun,
	2 fun_max,emratio
	integer ispec,qz,num,num1
        real*8 zgamma,zbeta,betasqr,zdni


        parameter (EM=.5110034e-6)      !electrom mass in Tev
        parameter (Z_OVER_A = 0.5118)   !Z/A for air
        parameter (D=0.3070e-6) !this is the combination of constants:
                                !4*pi*N*r**2*m*c**2 (in Tev/gm/cm**2)
	parameter (CONST=.5*D*Z_OVER_A)
     

!c	Dispatch according to projectile species.
!        goto (1,2,3,4,4,1,4,4,4,4,1,1,4,1,1,1,1,1),ispec

!       18/06/01 GHS. I can't belive I used a computed goto. Replace with 
!                     normal if-then-else structure.
!***************************************************************************
                  !Neutral particle.
!***************************************************************************
	if(qz.eq.0)then
	      return	!No knock on's for neutral particles.
                        !Gammas, PI(0)'s, kshort,klong,neutrino's
!***************************************************************************
c	POSITRONS:
!***************************************************************************
	elseif(ispec.eq.2)then
c	First calculate T_max:Maximum possiple energy transfer from projectile
c	to target.
		t_max=tenergy		!Positrons can stop dead!
					!Using z**2=1(charge**2)
		total_rate=const*(1/t_min-1/t_max-
	1 (2/t_max)*alog(t_max/t_min)+
	1 3*(t_max-t_min)/(t_max**2)-(t_max**2-t_min**2)/(t_max**3)+
	2 (t_max**3-t_min**3)/(3*t_max**4))

!		num=knock_made(total_rate,tsegment)
		num=nfluct(total_rate*tsegment)


		if(num.eq.0)return
		num1=num

c	For positrons max value of F/beta**2 is 1.
c	Now pick from 1/t**2
101		t=(t_max*t_min)/(t_max-pran(xdummy)*(t_max-t_min))

c	Now see if this t makes it for the F  function for positrons.
		fun=(1.-t/tenergy+(t/tenergy)**2)**2 !Fun max = 1(t=0)
		if(pran(xdummy).gt.fun)goto 101	!loop until we find one.

c	Put it into the stack
		call knock_enter(dl,dm,dn,xstart,ystart,hstart,hend,tim,
	1 zdni,zgamma,zbeta,xm,t)

		num1=num1-1
		if(num1.ne.0)goto 101
		return
!***************************************************************************


!***************************************************************************
!ELECTRONS:
!***************************************************************************
	elseif(ispec.eq.3)then
		t_max=tenergy/2.	!Electrons due to indistinguishability
					!Can only get 1/2 of max possible.
		if(t_max.lt.t_min)return
					!Using z**2=1(charge**2)
		total_rate=const*((t_max-t_min)/tenergy**2 +
	1 (2*t_min-tenergy)/(t_min*(t_min-tenergy))-
	2 (2*t_max-tenergy)/(t_max*(t_max-tenergy)))
	
!		num=knock_made(total_rate,tsegment)
		num=nfluct(total_rate*tsegment)


		if(num.eq.0)return
		num1=num

c	Maximum F at t_max.
 		fun_max=((tenergy/(t_max*(tenergy-t_max)))-
	1(1./tenergy))**2
		fun_max=fun_max*t_max*t_max

c	Now pick from 1/t**2
100		t=(t_max*t_min)/(t_max-pran(xdummy)*(t_max-t_min))

c	Now see if this t makes it for the F function for electrons.
		fun=((tenergy/(t*(tenergy-t)))-(1./tenergy))**2
		fun=fun*t*t
					!loop until we find one.
		if(pran(xdummy)*fun_max.gt.fun)goto 100	

c	Put it into the stack
		call knock_enter(dl,dm,dn,xstart,ystart,hstart,hend,tim,
	1 zdni,zgamma,zbeta,xm,t)

		num1=num1-1
		if(num1.ne.0)goto 100
		return
!***************************************************************************


!***************************************************************************
!SPIN 0 and SPIN 1/2
!***************************************************************************
	elseif((ispec.ge.4.and.ispec.le.10.and.ispec.ne.6).or.
	1 ispec.eq.13.or.ispec>20)then
                                     !muons+/-(1/2),pi+/-(0),kaon+/-(0)
                                     !protons(1/2)
                                     !Heavy ions(A=even:spin=0, A=odd:spin 1/2)
		betasqr=zbeta*zbeta
		emratio=em/xm	!Ratio of electron to projectile mass
				!Maximum energy that can be given to knock-on
		t_max=(2*em*betasqr*zgamma*zgamma)/(1+2*zgamma*emratio+
	1 emratio**2)
		if(t_max.le.t_min)return	!Leave if we can't make an
						!interesting knock-on electorn.
!Both spin 0 and spin 1/2 use the same first term.
		total_rate=qz*qz*(const/betasqr)*((1./t_min)-(1/t_max))-
		1 (const/t_max)*alog(t_max/t_min)

!***************************************************************************
!SPIN 0: pi+/-,kaon+/-, heavy nuclei with A even
!***************************************************************************
		if((ispec.ge.7.and.ispec.le.10).or.
	1 (ispec.gt.20.and.mod(ispec-20,2).eq.0))then
!			num=knock_made(total_rate,tsegment)
			num=nfluct(total_rate*tsegment)


			if(num.ne.0)then
			   do num1=1,num
				!Now pick from 1/t**2
102				t=(t_max*t_min)/(t_max-pran(xdummy)*
	1 (t_max-t_min))
				fun=(1-betasqr*t/t_max)
				!Fun max<1.for t_min>0)
					!loop until we find one.
				if(pran(xdummy).gt.fun)goto 102
c	Put it into the stack
				call knock_enter(dl,dm,dn,xstart,
	1 ystart,hstart,hend,tim,zdni,zgamma,zbeta,xm,t)

			   enddo

			endif
			return
		else

!***************************************************************************
!SPIN 1/2:  muons, protons, Heavy nuclei with A odd,
!***************************************************************************
			total_rate=total_rate+qz*qz*(const/betasqr)*
	1 .5*(1./(tenergy+xm)**2)*(t_max-t_min)
!			num=knock_made(total_rate,tsegment)
			num=nfluct(total_rate*tsegment)

			if(num.ne.0)then
			   do num1=1,num
				!Now pick from 1/t**2
103				t=(t_max*t_min)/(t_max-pran(xdummy)*
	1 (t_max-t_min))
				fun=(1-betasqr*t/t_max+
	1 .5*(t/(tenergy+xm))**2)
				!Fun max<1.for t_min>0)
					!loop until we find one.
				if(pran(xdummy).gt.fun)goto 103	
c	Put it into the stack
				call knock_enter(dl,dm,dn,xstart,
	1 ystart,hstart,hend,tim,zdni,zgamma,zbeta,xm,t)

			   enddo

			endif
			return
		endif
!***************************************************************************

	endif
	end

	
	subroutine knock_enter(dl,dm,dn,xstart,ystart,hstart,hend,tim,
	1 zdni,zgamma,zbeta,xm,t)
c	This routine determines where a knock on electron of energy T is
c	produced. It uses as input the parameters of the segemnt of the
c	projectile that produced it.  Once determined the new electron is 
c	entered into the stack.

c	Written by:

c	G.H.Sembroski
c	Physics Dept
c	Purdue University
c	W.Lafayette, In 47907
c	E-Mail: 47474::SEMBROSKI

	real*8 zheight,zpath,zprime,zptim,zstim,zgamma,zbeta,zdni,
	1 gammaprime
	real*4 t,em,c_light,tix,dlk,dmk,dnk,hstart,hend,dl,dm,dn,hk,
	1 xstart,ystart,xk,yk,tim,timk
	integer ispec		!Local variable only!!!!!

	parameter  (c_light=2.99792e+8)     !Speed of light in vacume.
        parameter (EM=.5110034e-6)      !electrom mass in Tev

c	First:determine angle of knock on electron to projectile
c	path.	From Lezniak: Average Added Component of Chernkov Radiation
c	Due to Knock-On Electrons. NIM.136 (1976)pg 299-306 who referneces:
c	Rossi: High Energy Particles 1952.

	gammaprime=(t+em)/em
	tix=(sqrt((gammaprime-1)/(gammaprime+1))/zbeta)*
	1 (1+em/(xm*zgamma))
	tix=acos(tix)	!angle of knock_on electron.
	dlk=dl		!Save direction cosigns.
	dmk=dm
	dnk=dn
	call geom(dlk,dmk,dnk,tix) !Produces new dlk,dmk,dnk @ angle tix to old.

c	Now x,y,height of collision.

       zheight=pran(xdummy)*(hstart-hend)  !Verticle distance to travel
       zpath=zheight/dn       !Segment is assumed straignt with direction
					!Collision is randomomly placed along segment.
c              Use original path length since its best aprox to tsegment
	xk=xstart+zpath*dl       !Segment is assumed straignt with direction
	yk=ystart+zpath*dm       !dl,dm,dn and length zpath.
	hk=hstart-zheight

c       Calculate time for emission of this electron
c       ZDNI is the z direction cosign for the inital primary particle.
c       calculate its travel time and that of this present particle between
c       these altitudes.

       zprime=zheight/zdni	!Distance primary travels between the start of
				! segemnt and knockon prodcution site.
       zptim=(zprime/c_light)*1.e+9	!Time in nanosec for a speed c particle
					!to move between these two altitudes.
       zstim=(zpath/(c_light*zbeta))*1.e+9	!Time in nanaosec for secondary
						!particle.
       timk=tim+(zstim-zptim)	!Only accumulate differences for this track.

	ispec=3	!electron: Save it on stack.
	call stor_e(t,hk,dlk,dmk,dnk,xk,yk,timk,ispec)

4300	format(' ',7(e14.7,','))
	return
	end

!
!	function knock_made(total_rate,tsegment)
!	real*4 total_rate,tsegment,prob
!	integer num,knock_made
!	total_rate=total_rate*tsegment
!	num=total_rate		!Get integer number to make
!	prob=total_rate-num	!Get fraction.
!	if(pran(xdummy).le.prob)num=num+1
!	knock_made=num
!	return
!	end


        FUNCTION LMESTY(L,IL)
        L1=L
        IF(L.GT.8) GO TO 1
        IF(pran(xdummy).GT..75) L1=6
        GO TO 2
 
    1   RR=pran(xdummy)
        IF(RR.GT..75) L1=11
        IF(RR.GT..875) L1=12
 
    2   LMESTY=L1
        RETURN
        END

        FUNCTION LTYPES(tE0,L0,indk)
C  DETERMINES TYPE OF CENTRAL PARTICLES.
c      Looks like te0 is in Gev.
c      Also looks like te0 is Kinetic energy.

c      G.H.S.5/20/86       Initalize ind to zero in calling routine.
c                    Set indk=3 for pions. This lets us test on
c                    indk not.eq 0 for second prat of pair.

c	9/26/91 G.H.S. V:1:0.5
c		Funny behavior in this routine. I'm not sure its correct.
c		For a particular central production set of calls, wk 
c		(probability that we make pions or gammas) is only calculated
c		once. Routione assumes its saved for susequent calls. For
c		security between different fortran compilers we will explicitly
c		save it.

c		
c      INDK=0:       First time for this pair this routine called.
c      indk=1: Second pair of K+ K- needed.
c      indk=2: Second pair of Ks Kl needed.
c      indk=3:       Another pion needed.

        DIMENSION WWK(23)
	save wk			! V:1:0.5
        DATA WWK/
     *  0.,0.,.003,.015,.024,.031,.036,.042,.047,.051,
     *  .055,.059,.063,.066,.069,.071,.074,.077,.079,.081,
     *  .082,.083,.084/
 
c     5/20/86 G.H.S.       Replace this test with test on indk.
c        IF(tE0.EQ.tEOLD)   GO TO 10
       if(indk.eq.0)then

               IF(L0.GE.13)then       !NUCLEON INTERACTION; ENERGY 
c                                   DEPENDENT RK
                     NK=10.*ALOG10(tE0/10.) + 1
                     IF (NK.LE.0)   NK=1
                     IF(NK.GT.23)  NK=23
                     WK=WWK(NK)
                     
              else IF(L0.GE. 9)then              !KAON INTERACTION;   RK = .2
                     WK=0.2
              else                            !PION INTERACTION;  RK = .1
                      WK=0.1
                     endif
				!Note that for a particular central production
				!wk only calculated once.
              
       else if(indk.ne.3)then              !SECOND PAIR MEMBER(kaon)
              IF(INDK.eq.1)then
                      LTYPES = 10
                      INDK=0
                     return
       
                     else
                     LTYPES = 12
                      INDK=0
                     return
                     endif
              endif

       IF(pran(xdummy).ge.WK)then       !PION PRODUCTION.
              R = pran(xdummy)
              LTYPES = 6
              IF (R .GT. 0.33333)       LTYPES = 7
              IF (R .GT. 0.66666)       LTYPES = 8
              indk=3
              return

              else              !K+K- (OR 2Ko) PRODUCTION. FIRST K.
              IF(pran(xdummy).GT..5)then
                      LTYPES = 9
                      INDK=1
                     return
       
                     else       !2Ko PRODUCTION
                     LTYPES=11
                      INDK=2
                     return
                     endif
              endif
       END
 

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
	parameter (mp=938.256e-6)   !Mass of proton (TeV)
	parameter (mn=939.550e-6)   !Mass of neutron(TeV)
	parameter (ap=33.5e-6)     !Pairing coef.(TeV)
	parameter (av=14.1e-6)     !Volume coef (TeV)
	parameter (as=13.0e-6)     !Surface coef(TeV)
	parameter (ac=0.595e-6)    !Coulmb coef.(TeV)
	parameter (aa=19e-6)       !Assymetry coef(TeV)
	data first_argon /.true./


         a=ia
                         !Correct our formula for elements up to a=56(Fe)
                         !which is as high as NUC_LIB goes.
         if(ia==18)then	  !Force Oxygen isotope
	    qz=8
	 elseif(ia==24)then
            qz=12         !Force Magnesium
         elseif(ia==28)then
            qz=14         !Force silicon
         elseif(ia==32)then
            qz=16         !Force Sulpher
         elseif(ia==33)then
            qz=16         !Force Sulpher
         elseif(ia==35)then
            qz=17         !Force Chlorine
         elseif(ia==39)then
            qz=19         !Force Potassium
         elseif(ia==40)then
            qz=18         !Force Argon !Could have been calcium 40.
            if(first_argon)then
               print*,'Warning--Forcing Argon for all atomic masses of 40'
               first_argon=.false.
            endif
         elseif(a==56)then
            qz=26         !Force Iron.
         else
            qz=anint(a/(1.98+0.0155*(a**(2./3.)))) 	!Use nearest integer 
							!function
         endif

!First determine pairing mass term
         if(mod(qz,2)==0)then
            if(mod(ia,2)==0)then
               pmass=-ap*a**(-.75)   !even-even nuclei
            else
               pmass=0.              !even-odd nuclei
            endif
         else
            if(mod(ia,2)==0)then
               pmass=0.              !Odd-even nuclei
            else
               pmass=ap*a**(-.75)   !Odd-odd  nuclei
            endif
         endif

         xmass = qz*mp + (a-qz)*mn - av*a + as*(a**(2./3.)) +
	1 ac*(qz**2)/(a**(1./3.)) + aa*((a-2*qz)**2)/a + pmass
 	return
	end





	subroutine mtxmlt(a,b,r)
c      Calculates r=Ab, where r,b are vectors and A is matrix, all 3-d.
       dimension a(9),b(3),r(3)
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
        if(x.lt.12.) then        !Poisson
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
c        Fluctuation is gaussian distributed.
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
!****************************************************************************

	subroutine pair_production(e_gamma,e_electron,e_positron)

!****************************************************************************
!	Gamma to Electron/positron pair production calculation.
!****************************************************************************

c	Determine energies E_ELECTRON, E_POSITRON of pair production from
c	a gamma ray of inital energy E_GAMMA. If one of the pair of particles
c	(call it PARTICLE1) gets fraction U*E_GAMMA of the primary gamma and 
c	the other (call it PARTICLE2)	gets (1-U)*E_GAMMA. Which particle 
c	is an electron and which is a positron is chosen randomly.


c	The fraction (U) is distribution is described by an energy
c	dependent probability intensity function.

c	E_GAMMA is total energy of the incoming Gamma in TeV.
c	E_ELECTRON is the KE of the outgoing electron in TeV
c	E_POSITRON is the KE of the outgoing positron in TeV
c	T is the distance in radiation lengths to the pair production
c	interaction point.

c	Physics note:
c	Generation of U:
c	Procedure:
c	We will use the Monte-Carlo rejection method to pick our U.
c	Pick with even probability a U between U_MIN and U_MAX where
c	U_MIN is  set by U_MIN=(mass_elecron*c**2/E_GAMMA) and
c	U_MAX is set by U_MAX=(1-mass_elecron*c**2/E_GAMMA).

c		Then:

c	Using the 3 equations from Rossi and Greisen, Rev. Mod.Phy.,13,Oct 1941
c	pg.258-259.(RG) (Don't use complete screening equation. gamma<2 
c	equation with Butcher-messel f1,f2 parameters is adequate.)
c	Determine  PSI(E_GAMMA,U) for the appropriate screening. Use
c	parameterization of the f1,f2 functions from Butcher and Messel,
c	Nuc.Phy 20,1960,pg 15-128. (BM) Use a power law fit to the C(gamma) 
c	function derived from the table given in RG pg.255.
c	Fit was done by G.H.Sembroski, Physics Dept.,Purdue University,
c	W.lafayette,In.  11/5/96

c	Use rejection method against these functions to test if we
c	should keep this value of U.


c	Written by:
c		Glenn Sembroski
c		Physics Dept
c		Purdue Univ
c		W.Lafayette Indiana
c		11/12/96

c	Modified:


	real e_gamma,e_electron,e_positron

	real u,u_min,u_max,e_mev,pair,test_pair,pair_max

	include 'kascade.h'
	e_mev=e_gamma*1.e6		!For internal use convert to MeV.


c Pair production:
c	Determine energies of pair produced electron/positron pair.

c	NOTE:We drop all constant coeficeints in this rejection scheme.
c 	They are not needed when we use this as part of a rejection scheme.
c	See Rossi and Greisen for the correct coeficients if you want them.

c	Get a max value of PSI(U,E_GAMMA) for all U by multiplying
c	PSI(.5,E_GAMMA) by 1.5. The maximum value of PSI(U,E_GAMMA) is
c	always less then 1.5*PSI(.5_E_GAMMA) for all gamma. At high energies
c	this is fairly efficient. At low energies its 50% inefficient.

	call pair_intensity(e_gamma,.5,pair)
	pair_max=pair*1.5

       	u_min=xm(3)/e_gamma		!Lower energy threshold fraction
	u_max=1.-(xm(3)/e_gamma)	!Can't get more then U-mc**2 into
					!electron/positron. xm in TeV
	

c	Use rejection method for the 'probability intensity' function
c	 PSI(U,E_GAMMA)
c	Pick a U.
100	u=pran(xdummy)		!evenly distributed between 0 and 1.
c	only keep it if its above u_min and u_max
	if(u.lt.u_min.or.u.gt.u_max)then
		goto 100		!Try again
	endif

c	Pick our test value.
	test_pair=pran(xdummy)*pair_max
c	See if its below actual value.
	call pair_intensity(e_gamma,u,pair)

c	Do we keep this value?(MC rejection method used here.)
	if(test_pair.gt.pair)then                             
		goto 100	!try again.
	endif

c	Pick if u goes with electron
	if(pran(xdummy).lt.0.5)then
					!electron gets U
		e_electron=u*e_gamma
		e_positron=(1.-u)*e_gamma
	else
		e_electron=(1.-u)*e_gamma
		e_positron=u*e_gamma
	endif
	return
	end

	subroutine pair_crossection(e_gamma,sigma)

c	determine crosssection SIGMA(in probability/radiation length) for a 
c	photon	of energy E_GAMMA in air.

c	SIGMA is total probability/radiation length for pair production with
c	a photon of energy E_GAMMA. Its a crosssection. Inverse is
c	mean-free-path to the interaction point.

c	The mean free path(MFP) to emission at energy E_GAMMA is determined by 
c	taking the inverse of the crosssection (SIGMA)(or probability/radiation
c	length)

c	Glenn Sembroski has determined the pair production crossection over the
c	 range of interested energies(by numerically integrating PSI(U,E_GAMMA)
c	using routine_  PAIR_INTENSITY, See notes in that routine for
c	references for the formulas used.). Functions of the form:

c		SIGMA=A1+B1*ln(E_GAMMA)                 for E_GAMMA<=100 MeV
c		SIGMA=PAIR_CROSS_MAX-A2*(E_GAMMA**(-B2))  for 100MeV<E_GAMMA<1TeV
c		SIGMA=PAIR_CROSS_MAX                    for 1Tev<E_GAMMA
c	were found to be a reasonable fit to the data .

c	The actual distance T (in radiation lengths) this electron goes before
c	emission is distributed as:

c	  probability(emission between T and T+dT) = (SIGMA)exp(-T*SIGMA)

c	Written by:
c		Glenn Sembroski
c		Physics Dept
c		Purdue Univ
c		W.Lafayette Indiana
c		12/12/96

c	Modified:


c	E_GAMMA: Photon energy in TeV
c	SIGMA:	Crossection in probability/radiation length.

	real e_gamma,sigma
	real pair_cross_max
	parameter(pair_cross_max=0.7895)	!Max crossection(in
						!probability/radiation length)
						!for a photon to pair produce. 

	e_mev=e_gamma*1.e6
	if(e_gamma.gt.1.0)then			!Constant above 1 TeV
		sigma=pair_cross_max
	elseif(e_mev.gt.100.)then
						!for 100MeV<E_GAMMA<10GeV
		SIGMA=PAIR_CROSS_MAX-7.682*(E_Mev**(-.7258))
	else
						!for E_GAMMA<=100 MeV
		SIGMA=-0.1293+0.1484*alog(e_mev)
	endif
	return
	end


	subroutine pair_intensity(w,u,pair)
c	This routine calculates the probability density(PAIR) of pair 
c	production where energy U of W(total photon energy in TeV) goes
c	to one of the pair of the produced electron and positron.
c	(Which one is decided elsewhere). The other particle gets the
c	rest of the energy.

c	NOTE:We drop all constant coeficeints in this routine.
c 	They are not needed when we use this as part of a rejection scheme.
c	See Rossi and Greisen for the correct coeficients if you want them.
c	The correct coeficient to get Psi(W,u) probability of emmision at
c	fraction u / radiation length from this routine would
c	be psi(W,u)= rad_length*coef*PAIR
c	Where
c	coef=4.*alpha*n_av*(z_air**2.)*(r_elect**2.)/a_atm
c		where:
c			alpha=1./137.04
c			n_av=6.024e23
c			z_air=7.37
c			r_elect=2.817e-13
c			a_atm=z_air/.5118
c			rad_length=36.6		!Latest value from Tsai, 1974
						!for air. 		
c				or
c			rad_length=43.0 	!Old value used by Bethe-
c						!Heitler and Rossi-Greisen
c						!Use only to compare with their
c						!plots.

c	Using the 3 equations from Rossi and Greisen, Rev. Mod.Phy.,13,Oct 1941
c	pg.258-259.(RG) (Don't use complete screening equation. gamma<2 
c	equation with Butcher-messel f1,f2 parameters is adequate.)
c	Determine the fuction for the appropriate screening. Use
c	parameterization of the f1,f2 functions from Butcher and Messel,
c	Nuc.Phy 20,1960,pg 15-128. (BM) Use a power law fit to the C_FUN(gamma) 
c	function derived from the table given in RG pg.255.
c	Fit was done by G.H.Sembroski, Physics Dept.,Purdue University,
c	W.lafayette,In.  11/5/96
c	We will use the rejection method against these functions to test if we
c	should keep this value of u.

c	W is total energy of photon in TeV.
c	U is fraction that goes into one of the pair(the other gets (1-U).
c	PAIR is relative probability value of PSI(W,U). Actual probability
c	per radiation length is PAIR*COEF*RAD_LENGTH (see above).
 
c	Written by:
c		Glenn Sembroski
c		Physics Dept
c		Purdue Univ
c		W.Lafayette Indiana
c		11/12/96

c	Modified:

	real w,u,eta,pair

	real common_term,common_term1,common_term2,common_term3
	real gamma,delta,f1,f2,fn_term1,fn_term2
	real fncs,c_fun,fng15
	include 'kascade.h'
	

	isp=3			!Use electron mass for calculation.

c	Determine function value according to screening parameter value..
c	(1-u) is used alot.
	eta=1.-u


c	Determine RG screening parameter
	gamma=100.*(xm(isp)/W)*(1./(u*eta))*(z_air**(-1./3.))


c	Complete to Intermediate screening: Gamma<2
	if(gamma.lt.2.0)then
			!Determine Screening parameter as used by BM in their
			!f1,f2 parameterizations..
		delta=1.36*gamma
			!Common terms.
		common_term1=(u**2) + (eta**2)
		common_term2=(2./3.)*u*eta
		common_term3=(1./3.)*alog(z_air)
			!BM has 2 different f1 f2 parameterizations of Gamma<2
			!f1 f2 functions.
		!First.
		if(gamma.lt.0.8)then
			f1=20.867 - (3.242*delta) + (0.625*(delta**2))
			f2=20.209 - (1.930*delta) + (0.086*(delta**2))
		else
			f1=21.12 - (4.184*alog(delta+.952))
			f2=f1
		endif
		fn_term1= (f1/4.)-common_term3
		fn_term2= (f2/4.)-common_term3
		pair = (common_term1*fn_term1) + (common_term2*fn_term2)

	!Now look at gamma >2
	else
		!Special term for gamma>2 equations
		fncs= alog( ( (2.*W)/xm(isp)) * (u*eta) ) -.5
		common_term=((u**2) + (eta**2))+((2./3.)*u*eta)
		if(gamma.lt.15.0)then
			c_fun=0.616*gamma**(-1.46)
			fng15=fncs-c_fun
			pair=common_term*fng15
		else
				!Unscreened
			pair=common_term*fncs
		endif
	endif
	return
	end


       subroutine propagate(tenergy,height,dl,dm,dn,x,y,tim,
     1 ispec,hd,tsegment)
c      Propagate particle from altitude height to altitude hd.
c      Draw track if enabled.
c      If hd is below hobs return with hd=hobs after propgation.

c      G.H.S. 11/14/86
c                    In order to make Cherenkov light more realistic, add to
c                    output segment records the direction cosigns at the
c                    end of a segment.  Requires moving calls to RIONZ,
c                    MULTSCAT,and BEND into subrotine_ PROPAGATE. Changes to
c                    PROPAGTE,PROPDEC, and PROPINT.

c      G.H.S. 2/2/87
c                    To make a better approximation for the segment and the
c                    effects of mag bending and mult scatt: 1.When 
c                    propagating a segment , assume the segment is straight
c                    with its initial direction to caculate final x,y values
c                    times, mid x,y values and tmid. Apply ther direction
c                    changes due to multiple scattering and mag field 
c                    bending only at the end of the segment.Changes to
c                    PROPAGATE.


c      G.H.S.     3/13/87
c                    To reflect that we no longer use the middle of the 
c                    track segment as a special point convert subroutine
c                    PROPAGATE to only write out beggining and ending 
c                    parameters( exception:gamma) of track to the segment 
c                    file.

c        G.H.S. 4/9/89
c             Following T.R.P.'s suggestions the time will now be calculated
c             by accumulation of differences of travel time of a segment versus
c             the travel time the primary particle moving at speed c would
c             have taken.  Also do things in real*8.  On the cray everything
c             is real*8.  Changes to PROPAGATE.

c        G.H.S. 4/20/89
c             Fix propagate setting of below-threshold flag(tenergy=0).  This 
c             has been wrong for a long time.  The only problem it causes is 
c             excess running time.

c	7/16/90 G.H.S. V:1:0.1
c		Fix a possible upward going particle in PROPINT.
c		Don't let particle go above 1 gm.

C	5/3/91 G.H.S.V:1:0.4
C		Make the CONSISTENCY checks on dl*,dm* tougher. After fixing 
c		them ,double check.

c	6/3/92 G.H.S.V:1:0.6
c		Adjust tsegment when particle hits the ground. Last segment 
c		was getting too much de/dx.


c	11/9/95 GHS V:1:0.6
c		Correct sign of dn normalization caculation in PROPAGATE.
c	        Fix a buch of stuff includinghandeling of up going particles.

!	15/06/01 GHS
!		Add stuff to accomodate heavy nucleons(ispec>20).
!		tenergy is totsl KE of nucleon.
!               ispec-20 is mass number of nucleon.
!	

c      Retrun flags:1.       height=hobs               !Hits the ground.
c                   2.       tenergy.le.thresh(ispec)(or 
c                            thresh(proton)*Mass number for nuclei)   
c                            Drop it, it is to weak.


       real*8 zdlstart,zdmstart,zdnstart,zheight,zbeta,zgamma,ztemid,
     1 zxm,zpath,zprime,zptim,zstim
	integer*2 nx,ny,mxy(2)
	integer*4 nxy
	equivalence(mxy(1),nxy)
	equivalence(nx,mxy(1))
	equivalence(ny,mxy(2))
	integer jcharge,ja
	real xmass
	include 'kascade.h'


	if(ispec.lt.20)then
           jcharge=icharge(ispec) !Not a nuclei
           xmass=xm(ispec)
	else
                                !Get Z of nucleus
           ja=ispec-20
                                !Get charge and Nucleon mass.
           call mass_number2charge_mass(ja,jcharge,xmass)
	endif

!****************************************************************************
! Neutrals that hit
!****************************************************************************
	IF (Hd.LE.hobs)then
           if(jcharge.ne.0)then
              tsegment=((height-hobs)/(height-hd))*tsegment
              if(tsegment.lt.0)then
                 print*,
	1 '**PROPAGATE-Warn-Negative seg:tsegment,height,hobs,hd:',
	1 tsegment,height,hobs,hd
              endif
           endif
           hd=hobs		!Hits the ground!
				!Note that neutrals don't care about tsegment
	endif
!****************************************************************************
 

!****************************************************************************
c    Horoizontal track Consistency check.
!****************************************************************************
	icheck=0
100     dldm=(1.-dl**2-dm**2)	!Make absolutly sure we have
				!a unit vector.
	if(dldm.le.0.0)then
           if (abs(dldm).lt.1.e-7)then
c 					!Only report it if its sizable.
              write(6,1000)dl,dm,dn,dldm
1000  format(' ***PROPAGATE--Consistency failure-dl,dm,dn,dldm:',4e14.7)
           endif
           dm=dm-sign(1.e-6,dm)	!Reduce by 1.e-6
           dl=dl-sign(1.e-6,dl)	!Reduce by 1.e-6
           icheck=icheck+1
           if(icheck.gt.5)then
              print*,' ***PROPAGATE--FATAL--Cant fix consistency failure'
              stop
           endif
           goto 100
	endif
	dn=sign(sqrt(dldm),dn)	!Make absolutly sure we have
	   			!a unit vector(of correct sign.11/9/95).

	
        zdlstart=dl             !Save intiial direction cosigns.
        zdmstart=dm
        zdnstart=dn
!****************************************************************************

        de=0.                   !For neutrals.

        zheight=(height-hd)     !Verticle distance to travel
        zpath=zheight/zdnstart  !Segment is assumed straignt with direction
				    ! zd*startt and this length.

        segment.xstart=x	!Save initial position for writing to segment 
                                !file.
        segment.ystart=y          
        segment.hstart=height
        segment.dlstart=dl
        segment.dmstart=dm

c              Use original path length since its best aprox to tsegment
        x=x+zpath*dl            !Segment is assumed straignt with direction
        y=y+zpath*dm            !dl,dm,dn and length zpath.

c       Calculate beta and gamma very carefully, r*8.       

	zbeta=0

!****************************************************************************
!       neutrals: no energy change
!****************************************************************************
        if(jcharge.eq.0)then
           temid=tenergy        !K.E.at mid point of segment.
        else

!****************************************************************************
!	Charge particle proagation
!****************************************************************************

!*****************************************************************************
!	DE/DX:
!*****************************************************************************
			!Do de/dx
           call rioniz(tenergy,de,tsegment,ispec,jcharge,xmass)
           temid=tenergy-de/2.  !K.E.at mid point of segment.
           if(ispec.lt.20)then
              if(temid.lt.thresh(ispec))then
                 tenergy=temid  !Flag that we are 
                                !below threshold.
                 return         !Quit if we're below threshold.
              endif
           else
				!KE Threshold for nuclei is mass number *
				!threshold for proton.
              if(temid.lt.ja*thresh(13))then
                 tenergy=temid  !Flag that we are 
							!below threshold.
                 return         !Quit if we're below threshold.
              endif
           endif

!*****************************************************************************
!	Determine beta and gamma for this temid
!*****************************************************************************

           zxm=xmass            !set mass(convert to real*8)
           
           ztemid=temid
           zgamma=(ztemid+zxm)/zxm
           zbeta=sqrt(1.-1./zgamma**2)


!*****************************************************************************
!	Generate any knock on electrons.
!*****************************************************************************
           call knock_on(temid,tsegment,ispec,dl,dm,dn,
	1 tim,zdni,xmass,zbeta,zgamma,segment.xstart,
	1 segment.ystart,segment.hstart,hd,thresh(3),num,jcharge)
           if((debena(3).or.debena(19)).and.num.ne.0)then
              nstart=nen
              do n=1,num
					!Use ispeck here(this is a debug if
					!clause) so we dont mess up ispec.
                 call reca_e(tek,hek,dlk,dmk,dnk,xk,yk,
	1 timk,ispeck)
                 if(ispec.lt.20)then
                    write(6,1001)nen+1,namtyp(ispec),
     1 tek,hek,dlk,dmk,dnk,xk,yk,timk
1001  format(' Knock-on e:',i3':from ',a8,':',f10.6,f13.0,
	1 2(f8.5,','),f8.5,2f10.1,f12.1)
                 else
                    write(6,1002)nen+1,
	1 trim(nuclei_names(jcharge)),ja,tek,hek,dlk,dmk,dnk,xk,yk,timk
1002  format(' Knock-on e:',i3':from ',a,'(',i3,'):',3x,f10.6,f10.0,
	1 2(f6.3,','),f6.3,2f10.1,f12.1)
                 endif			
              enddo
              nen=nstart             !Restore them back.
              num=0
           endif


!*****************************************************************************
!	Multiple scattering changes direction at end of segment.
!*****************************************************************************

           call multscatt(temid,tsegment,ispec,dl,dm,dn,
	1 jcharge,xmass)
					!MULTSCATT comes back with dl,dm,dn 
					!pointing in the new direction.
c       There might be some correction to 'zpath' distance here due to particle
c       wiggleing. Probably only important at low energies.


!*****************************************************************************
!       Bend in magnetic field. 
!*****************************************************************************
!       dl,dm,dn reflect new direction of segment at
c       its end do to field bending.  This is all fairly crude but propably
c       produces only small errors which don't affect general properties of the
c       shower or its cherenkov light.

           if(magnet_on) then
		call bend(temid,zpath,ispec,dl,dm,dn,jcharge,xmass)
	   endif

c       Maybe we should have some corecction to path (and tsegment) here due
c       to the particle following an arc but prob only important at low
c       energies

	endif                   !End of propagation modifications 

c       Propagate the particle to hd.

!****************************************************************************
!       Calculate time for this particle to end of segment.
!****************************************************************************

c       Slight error here since a staright path of length zpath assumed.
c       Probably a good enough approx.  Can be improved by takeing smaller
c       steps in radiation lengths.

c       ZDNI is the z direction cosign for the inital primary particle.
c       calculate its travel time and that of this present particle between
c       these altitudes.
       
        if(zbeta.eq.0)then
           if(xmass.eq.0.)then
              zbeta=1.
           else
              zxm=xmass         !r*8 convert
              ztemid=temid
              zgamma=(ztemid+zxm)/zxm
              zbeta=sqrt(1.-1./zgamma**2)
           endif
	endif
	zprime=zheight/zdni     !Distance primary travels between the
c                                       altitudes of this segment.

        zptim=(zprime/c_light)*1.e+9 !Time in nanosec for a speed c particle
c                                      to move between these two altitudes.

        zstim=(zpath/(c_light*zbeta))*1.e+9 !Time in nanaosec for secondary
c                                             particle.
        tim=tim+(zstim-zptim)   !Only accumulate differences for this
c                                      track.
	
        height=hd               !final height                     
        tenergy=tenergy-de      !Final energy.
        if(tenergy.lt.0)then
           tenergy=1.e-15
        endif

c       Write this segment out to the segment file.
        segment.tend=tim
        segment.hend=height
        segment.dlend=dl
        segment.dmend=dm
        segment.nspec=ispec	!ispec is local, nspec is in a common block.
        call segment_out        !Leave all cuts(no neutral etc.) to the USER
c                              routine.

        if(height.eq.hobs)then  !Particle hits!
           if(ispec.lt.20)then
              if(debena(ispec))then !Do we print it?
                 nn=ngn+1
                 if(ispec.eq.2.or.ispec.eq.3)then
                    nn=nen+1
                 endif
                 write(6,1003)namtyp(ispec),nn,tenergy,hd,x,y,tim
1003	format('0',a8,i4,' :hits! ',11x,f10.6,f13.0,23x,2f10.1,f12.1)
		 endif
                 
           else
           	if(debena(ispec))then
                	 write(6,1004)trim(nuclei_names(jcharge)),ja,
     1 tenergy,hd,x,y,tim
1004	format('0',a,'(',i3,') :hits',11x,f10.6,f13.0,23x,2f10.1,f12.1)

              	endif
           endif

           if(jcharge.ne.0)then
              size=size+1.
           endif
c                                         Get 'SIZE' of shower
c                                          which is defined as number of
c                                          charged particles in shower at
c                                          observation altitude.

c	See if were making a particle on ground file.
           if(gndprt)then
              n=(x/5.0)+.5+100000. !Form index on grid.
              nx=n-100000       !The '100000' here is to 
					!get the round down to work right.
						!Negative number round up!
		!Grid is always 5m x 5m on flat ground for POG detectors.
              n=(y/5.0)+.5+100000.
              ny=n-100000
c	Limit nx,ny for Pete's sort to be less then 16000.
              if(abs(nx).gt.16000.or.abs(ny).gt.16000)then
                 goto 106	!Drop it.
              endif

c	Form local ground coords of particle relative to center of area.
c        x,y=(0,0) is in area 0,0 as is x,y=(-.5,-.5)
              xg=x-nx*5.0
              if(abs(xg).gt.0.5)then
                 goto 106
              endif
			!Drop if not in 1m x 1m counter.
              yg=y-ny*5.0
              if(abs(yg).gt.0.5)then
                 goto 106
              endif
			!Drop if not in 1m x 1m counter.
c	Put the data into the POG record. Only keep what we need. 
c	nx,ny always i*2, but equivalenced to i*4 nxy.
c						
c        Write out the POG record. First 6 values in record mimic pe record
c	 structure exactly. Dn,ipsec added for convience only.
              write(7)nxy,tim,dl,dm,inext,dn,tenergy,ispec
 106       endif
        endif
        return
        end


       subroutine propdec(tenergy,height,dl,dm,dn,x,y,tim,ispec,t)
c      propagate the particle from altitude=height a for a distance t (meters)
c      This routine is for propagation to decay points which can be
c      specified by a distance in meters.  Assumption is made that we can
c      approximate this distance by an depth in gm/cm2 and use PROPINT to
c      do the actual propagation.  This is not quite correct because of
c      multiple scattering and bending, but its good enough.

c      Those particles which INTERACT and
c      have there distance specified by an interaction length need to use the
c      PROPINT.

c      Tenergy=Kinetic energy of particle(Tev.)
c      If particle goes below hobs height will be set to hobs.

c      G.H.S. 5/23/86       Propagate a distance defined in meters.  This is
c                    in preperation to slicing up the trajectory for
c                    multi.scat de/dx etc.

c      G.H.S. 11/14/86       In order to make Cherenkov light more realistic,
c                     add to
c                    to output segment records the direction cosigns at the
c                    end of a segment.  Requires moving calls to RIONZ,
c                    MULTSCAT,and BEND into subroutine_ PROPAGATE. Changes to
c                    PROPAGTE,PROPDEC, and PROPINT.

c      G.H.S.  4/20/89
c            Following Pete's advice convert this routine to use PROPINT to
c            do the actual propagation.


c      G.H.S.  5/2/89
c            In order to prevent problems limit the altitudes to above hobs.

c	11/29/90 G.H.S. v:1:0.3
c		For decaying type particles that hit hobs, make sure they do
c		by pretending that they go through hobs, through air with
c		density same as at hobs until they reach their decay distance.
c		Use the resultant gm/cm**2 traveled as the values to feed
c		to propint.

c	11/9/95 GHS
c		Limit up going neutrals to 1.0 gm/cm**2 altitude.
c		Correct handeling of Up going charged at 1.0 gm/cm**2
c		Fix handeling of sign for up going.
!      08/12/04 GHS  V:1:3.3
!               A Bug: When the injection depth is less than 1gm we have 
!               problems with thinking that we have upgoing particles and 
!               dropping them. Change places where 1.0 gm is used as the 
!               test limit to .5*segment_head.depth

       include 'kascade.h'

              
       r=t*dn                     !Verticle length in meters of 
c                                   distance to interaction.
       Hd=height-r              !veriticle height of the interaction.
cNeutral particles.(gammas,K long,K short, Neutron, Neutrinos)
       if(icharge(ispec).eq.0)then
		if(hd.lt.hobs)hd=hobs
		if(hd.ge.yds(.5*segment_head.depth))then    !Control upgoing.
			hd=yds(.5*segment_head.depth)
		  	write(6,1000)tenergy,height,dl,dm,
	1 dn,x,y,tim,ispec,t
1000	format(' ***Prodec-Neutral Track goes too high:tenergy,
	1 height,dl,dm,dn,x,y,tim,ispec,t:',/,(' ***'5e14.7,/),
	1 (' ***'2e14.7,/),i5,e14.7,/,
	1  ' ***HD set to yds(.5*segment_head.depth)')
		endif
				!Note that PROPAGATE for neutral particles
				!doesn't care about TSEGMENT, only HD.
		call propagate(tenergy,height,dl,dm,dn,x,y,tim,
     1 ispec,hd,tsegment)
    
cCharged propagation
       else       
		tgms=0
		if(hd.lt.hobs)then	!Below hobs pretend decaying particle
					!continues through air with density of
	    				!that at hobs. Need for propint in case
					!we are slowed down enough or bent
					!enough to not reach hobs.
          				
			tgms=(hobs-hd)*100.*rho(hobs)/dn
			hd=hobs
		endif
		tgms=tgms+(gms(hd)-gms(height))/dn
		if(tgms.lt.0)then
			print*,
	1 '****** PROPDEC--Warning-Negative tgms:tgms,hd,height,dn:',
	2 tgms,hd,height,dn
		endif
             				!Note that PROPINT doesn't care about
					!HD, it only cares about TGMS.
              call propint(tenergy,height,dl,dm,dn,x,y,tim,ispec,tgms)
       endif

       return
       end


       subroutine propint(tenergy,height,dl,dm,dn,x,y,tim,ispec,t)
c      propagate the particle from altitude=height a distance t (gm/cm**2)
c      This routine is for propagation to interaction points which can be
c      specified by a distance in gm/cm**2.  Those particles which DECAY and
c      have there distance specified by a length in meters need to use the
c      routine PROPDEC.(which calls PROPINT 4/20/89).

c      Tenergy=Kinetic energy of particle(Tev.)
c      If particle goes below hobs height will be set to hobs.

c      Modified:

c      G.H.S. 11/14/86       
c                    In order to make Cherenkov light more realistic, add to
c                    to output segment records the direction cosigns at the
c                    end of a segment.  Requires moving calls to RIONZ,
c                    MULTSCAT,and BEND into subroutine_ PROPAGATE. Changes to
c                    PROPAGTE,PROPDEC, and PROPINT.

c      G.H.S. 4/20/89
c             The neutrals were not being propagated corectly. They were
c             only being sent 'tsegment' instead of 't'.

c	G.H.S. 5/2/89
c             Don't allow us to go below hobs. See 11/9/95

c	G.H.S. 10/23/89
c		Don't let us go above 1 gm.

c	11/9/95 GHS V:1:0.6
c		Fix hobs limitation. Make test at hobs not sea_level. Fix
c		t(distance in gm/cm**2) after new distance.
c		Fix handeling of up going particles.

!	14/06/01 GHS V:1:2.0
!		Modify for use with heavies. Heavies have ispec>20.
!		Mass number of heavy is ispec-20.

!      08/12/04 GHS  V:1:3.3
!               A Bug: When the injection depth is less than 1gm we have 
!               problems with thinking that we have upgoing particles and 
!               dropping them. Change places where 1.0 gm is used as the 
!               test limit to .5*segment_head.depth

	real tnucleon,xmass
	include 'kascade.h'

!****************************************************************************
c              Neutral particles.(gammas,K long,K short, Neutron, Neutrinos)
!****************************************************************************
c              No slices. go straight to destination.
	if(ispec.lt.20)then		!See that this isn't a heavy.
	  if(icharge(ispec).eq.0)then
		r=t*dn                  !Verticle length in gm/cm**2 of 
					!distance to interaction.
		g=GMS(height)           !Now convert to altitude in m of 
					!interaction point
		gr=g+r
		if((gr).ge.segment_head.zobs)then
			hd=hobs		!Neutrals don't care about tsegment(t)
		elseif(gr.lt.(.5*segment_head.depth))then
		  	write(6,1000)g,r,tenergy,height,dl,dm,
	1 dn,x,y,tim,ispec,t
1000	format(' ***Propint-Neutral Track goes too high:g,r,tenergy,
	1 height,dl,dm,dn,x,y,tim,ispec,t:',/,2(' ***'5e14.7,/),
	2 ' ***',i5,e14.7,/,' ***G+R set to 1 gm.')
                                         !veriticle height of the interaction.
			Hd=YDS(.5*SEGMENT_HEAD.DEPTH) 
		else
			Hd=YDS(gr)       !veriticle height of the interaction.
		endif
				!Note that PROPAGATE for neutral particles
				!doesn't care about TSEGMENT, only HD.
		call propagate(tenergy,height,dl,dm,dn,x,y,tim,
     1 ispec,hd,t)
        	return

	  endif
	endif

!****************************************************************************
!Charged propagation. Single or a Heavy
!****************************************************************************
c                    t is total distance to go in gm/cm**2. Should be positive.
	tsofar=0.       !Distance traveled sofar.
	ttend=gms(height)       !Starting depth. in gm/cm**2
             
				!Determine length of segment.
100	if(t-tsofar.le.tmax*rad_len)then		!End of segment?
		tsegment=t-tsofar
		tsofar=t	!Do it this way so end of segment
					!test works!
	else
		tsegment=tmax*rad_len
		tsofar=tsofar+tsegment
	endif

	if(tsegment.lt.0)then
	     		print*,
	1 '****** PROPINT--Warn-Negative seg:tsegment,t,tsofar,height:',
	1 tsegment,t,tsofar,height
	endif

	tstart=ttend                     !Save start of segment.
	ttend=tstart+tsegment*dn       !End altitude in gm/cm**2

	if(ttend.lt.(.5*segment_head.depth))then
		write(6,1002)tstart,tsegment,tenergy,height,dl,dm,
	1 dn,x,y,tim,ispec,t
1002	format(' ***Propint-Track goes too high:tstart,tsegment,tenergy,
	1 height,dl,dm,dn,x,y,tim,ispec,t:',/,2(' ***'5e14.7,/),
	2 ' ***',i5,e14.7,/,' ***G+R set to .5*segment_head.depth gm.')
		ttend=.5*segment_head.depth
	        tsofar=tsofar-tsegment
	    	tsegment=abs((tstart-ttend)/dn)
	        tsofar=tsofar+tsegment
		t=tsofar
	endif
	Hd=YDS(ttend)           !veriticle height of end of the
					!segment in meters.

	if(height-hd.ne.0.0)then
              	call propagate(tenergy,height,dl,dm,dn,x,y,tim,
     1 ispec,hd,tsegment)
	endif

c!!Make corection to tsegment after propagate????

	if(ispec.lt.20)then
!*************************************************************************
c       Single particle Too weak?
!*************************************************************************
		if(tenergy.lt.thresh(ispec))then
			if(debena(ispec))then
                       		if(ispec.eq.2.or.ispec.eq.3)then
                        	       nnnn=nen+1
                       		else
                        	       nnnn=ngn+1
                       		endif
          	       		write(6,1001)namtyp(ispec),
     1 nnnn,tenergy,height,x,y,tim
1001  format(a8,':de/dx drop ',i4,3x,f10.6,f13.0,23x,2f10.1,f12.1)
!                       		write(6,1012)thresh(ispec)
!1012  format(' ',25x,' DE/DX dropp. Abs. threshold for this
!     1 particle=',f10.6,' Tev.')
			endif
			return
		endif
!****************************************************************************
!       Heavy nucleus too weak?
!****************************************************************************
	else
	 	tnucleon=tenergy/(ispec-20) !Determine energy /nucleon. We
					!may have lost some during propagation.
			!Test to see if we go under hadron
			! KE threshold per/nucleon(much less then cherenkov 
			!threshold)(Mass of delta for now)
		IF (tnucleon.lt.thresh(14))then
			if(debena(20))then
	          	  call mass_number2charge_mass(ispec-20,
	1 z_nuclei,xmass)
          	       	  write(6,2001)trim(nuclei_names(z_nuclei)),
	1 ispec-20,tenergy,height,x,y,tim
2001  format('0',a,'(',i3,'):de/dx drop ',3x,f10.6,f13.0,23x,2f10.1,f12.1)

			endif
  			return
		endif
	endif

       !Hit ground?
	if(height.le.hobs)then       !quit if we hit.
		return
	endif
	!More to do?
	if(tsofar.ge.t)then
		return
	endif
	goto 100		!Do some more.
       end



        FUNCTION PTran(PTE,IL)
C  SAMPLES TranSVERSE MOMENTUM DISTRIBUTION AS
C  PT*EXP(-PT/<PT>);  <PT> = 2.*COEFF
c      G.H.S 1/10/86
c             Removed X for argumrnt list since ptran doesn't 
c             depend on it.

       DIMENSION COEFF(18)
        DATA COEFF/5*0.,3*.17E-3,4*.20E-3,2*.25E-3,4*0./
        PTran=-COEFF(IL)*ALOG(pran(xdummy)*pran(xdummy))
        PTran=PTran*PTE
        RETURN
        END
 
        FUNCTION REXP(X)
c      This function randomly picks from an exponatial distribution using
c      X as the scale factor.
c      Its used for interaction depth(Xis is gm/cm**2) and for decay length
c      (where x is in meters)
c      Rexp is distributed as exp(-s/X)
        Q = pran(xdummy)
c      This produces the required random distribution.
        REXP = -X*ALOG(Q)
        RETURN
        END

        subroutine rioniz(tenergy,de,t,ispec,qz,xmass)
c       12/91 M.P.Kertzman
c       Replacement for Stanev's rioniz
c
c       ***---->>> ADD the following line to kascade.h:
c       common/rz/irz_first,alpha,cf2,cf3
c
c       The code is taken from "Techniques for Nuclear and Particle Physics
c       Experimetns" by W.R. Leo, 1987, chapter two
c       For heavy particles the expression from Leo is essentially the same
c       as found in the particle data book.
     
!	Modified:

!	18/06/01 GHS v:1:2.0
!		Check that equation for heavy particles(nucleos and nuclei) has
!		been done corretcly. It has! Mary's good! 
!		Note: t=dx*rho(density).
!		Note: We dropped the last term of equation 2.27(the density 
!                     correction)
!		Add in a charge argument, QZ, and insrt into that equation
!		Add in the XMASS argument for beta,gamma, and eta calculations.
!		Replace the computed goto with if statements. I couldn't help
!		it. :)

        common/rz/irz_first,alpha,cf2,cf3
    
	real*4 tenergy,xmass
	integer qz
        real*8 gamma,beta,XO,Xl,X,m,a
        real*8 z,em,delta,eta
     
        parameter (EM=.5110034e-6)      !electrom mass in Tev
        parameter (Z=7.37)

c       Density effect parameters (all taken from Leo, pg.26) for air.
        parameter (a = 0.1091)
        parameter (XO = 1.742)
        parameter (Xl = 4.28)
        parameter (C = -10.6)
        parameter (m = 3.40)
c       other parameters
        parameter (FIZ=85.7e-12)        !ionization potential for air(Tev)
        parameter (Z_OVER_A = 0.5118)   !Z/A for air!GHS 18/06/01:I'm not sure
					!where Mary got this but assume its ok!
        parameter (CF=0.1535e-6) !this is the combination of constants:
                                !2*pi*N*(r**2)*m*(c**2) (in Tev/gm/cm**2)
     
        include 'kascade.h'
        if(irz_first.eq.0) then         !These only need to be claculated
                irz_first=1             !once. This is set up this way to
                alpha=2*CF*Z_OVER_A     !facilitate using the code for a
                cf2=dlog(2*EM/FIZ)      !medium other thatn air. You could
                cf3=dlog(EM/FIZ)-0.34657                ! 0.34657=0.5*ln2
        endif                           !also just set these values for air.
     
        gamma = (xmass + Tenergy)/(xmass)
        beta = sqrt((gamma**2 - 1.)/(gamma**2))
        betasqr=beta*beta
        eta = beta*gamma
     
        x = dlog10(eta)
        If (x.lt.XO) then
                delta = 0
        Else if (x.lt.Xl) then
                delta = 4.6052*X + C + a*((Xl - X)**m)
        Else
                delta = 4.6052*X + C
        Endif
     
!18/06/01 GHS Replaced. I couldn't stand it.
!!!        goto (3,1,1,2,2,3,2,2,2,2,3,3,2,3,3,3,3,3),ispec
!!!		        !branch by species
     
!*************************************************************************
c       dE for leptons,  From Leo, p. 35
!*************************************************************************
	if(ispec.eq.2.or.ispec.eq.3)then
		tau = tenergy/EM
        	taut = tau + 2.0
           	if(ispec.eq.3) then
                		!electron
                	F=((tau**2)/8-(2*tau+1)*.6931)/((tau+1)**2)+1-
	1 (beta**2)
        	elseif(ispec.eq.2) then
                		!positron
                	F=-(23+14/taut+10/(taut**2)+4/(taut**3))*
	1 (beta**2)/12+1.3862944
        	endif
        	de=alpha*(1/betasqr)*(cf3+0.5*(alog(tau**2*taut)+
	1 F-delta))*t
	        return
!*************************************************************************
	
!*************************************************************************
c       dE for HEAVY PARTICLES, Leo eq. 2.27
!*************************************************************************
	elseif((ispec.ge.4.and.ispec.ne.6.and.ispec.le.10).or.
	1 (ispec.eq.13).or.(ispec.gt.20))then

!	18/06/01 GHS. Add qz**2 for nuclei.Everything else stays the same.
		de=alpha*(1/betasqr)*(qz**2)*(cf2+dlog(gamma*gamma-1)
	1 -betasqr-delta*0.5)*t
	        return
!*************************************************************************

!*************************************************************************
c       Neutrals (no dE/dx)
!*************************************************************************
	else     
	       	de=0.
        	return
     	endif
        end
     


       subroutine reca_g(tenergy,height,dl,dm,dn,x,y,tim,ispec)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      Recall last entered particle(lifo).                            c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       include 'kascade.h'

       tenergy=dgn(1,ngn)
       height=dgn(2,ngn)
       dl=dgn(3,ngn)
       dm=dgn(4,ngn)
       dn=dgn(5,ngn)
       x=dgn(6,ngn)
       y=dgn(7,ngn)
       tim=dgn(8,ngn)
       ispec=dgn(9,ngn)

       ngn=ngn-1
       return
       end


              subroutine reca_e(tenergy,height,dl,dm,dn,x,y,tim,ispec)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        Recall last stored electron(lifo).                            c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       include 'kascade.h'
23       tenergy=den(1,nen)
       height=den(2,nen)
       dl=den(3,nen)
       dm=den(4,nen)
       dn=den(5,nen)
       x=den(6,nen)
       y=den(7,nen)
       tim=den(8,nen)
       ispec=den(9,nen)
       nen=nen-1
       return
       end
!****************************************************************************




       Subroutine segment_out
c        Write out the segment.
c        This is where we decide which segments to write out.
c        Written: G.H.S. 4/4/89

c        Modified:

c        G.H.S.  4/13/89
c           Cut on neutrals. 

c	G.H.S.	9/25/89
c	    Add consistency check for d*end.

C	5/3/91 G.H.S.V:1:0.4
C		Make the CONSISTENCY checks on dl*,dm* tougher. After fixing 
c		them double check.

c	16/1/97 G.H.S. v:1:1.0
c		Convert from a direct access output file for the segments to an
c		unformated binaray file. Use structures(in KASSTRUCTURES.H) 
c		for header and data structures. Remove calls and code that 
c		determines number of photons created per segment (RATEINIT,
c		RATEVIS,RATEOLD and RATESB  etc). This is now done in KASLITE 
c		as needed. Do this to considerably speed up this program.
c		Replace all VAX* files into/with things like: FILEOPEN,DATIN
c		Remove NSHOW parameter from data file. Remove NSHOW and
c		NSEGMENTS from segment file header.
c		Also remove THRESHOLDCHECK and add SEGMENT_OUT.

c        Below follows all cuts on writting the segment to disk.
c        Ignore neutrals.  4/13/89
c        Ignore upward going particles.  4/13/89
c        Cut on lack of photons       4/20/89 REMOVE THIS CUT 17/1/97 v1:1.0 GHS
c        Cut on angle:Only keep =/- 5 deg of primary since this is all
c        atmprob is good for.    4/28/89   REMOVE THIS CUT! 6/23/89

	real xmass
	integer jcharge,ja

        include 'kascade.h'
	if(segment.nspec.lt.20)then
           jcharge=icharge(segment.nspec) !Not a nuclei
           xmass=xm(segment.nspec)
	else
                                !Get Z of nucleus
           ja=segment.nspec-20
                                !Get charge and Nucleon mass.
           call mass_number2charge_mass(ja,jcharge,xmass)
	endif

C Ignore neutrals.
        if(jcharge.eq.0)then
           return
        endif

C Ignore upward going tracks.
	if(segment.hstart.lt.segment.hend)then
		return
	endif

c Consistency check!!! Horizontal tracks.
c	Check	Horizontal Consistency for d*end 9/25/89 G.H.S.
	icheck=0
100       dlsdms=1.-segment.dlend**2-segment.dmend**2
       if(dlsdms.le.0.0)then
               write(6,1301)segment.dlend,segment.dmend,dlsdms,
	1 icheck+1
1301  format(' ***SEGMENT_OUT--Consistency failure. dlend,dmend,
	1 dlsdms:',3e20.12,' attempt #',i3)
							!Fix it
		segment.dmend=segment.dmend-
	1 sign(1.e-6,segment.dmend)
		segment.dlend=segment.dlend-
	1 sign(1.e-6,segment.dlend)
		icheck=icheck+1
		if(icheck.gt.5)then
	print*,' ***SEGMENT_OUT--FATAL--Cant fix consistency failure'
			stop
		endif
		goto 100
	endif

c Consistency check for d*start!!!
	icheck=0
101       dlsdms=1.-segment.dlstart**2-segment.dmstart**2
       if(dlsdms.le.0.0)then
               write(6,1300)segment.dlstart,segment.dmstart,
	1 dlsdms,icheck
1300  format(' ***SEGMENT_OUT--Consistency failure. dlstart,dmstart,
	1 dlsdms:',3e14.7,' attempt #',i3)
						!Fix it
		segment.dlstart=segment.dlstart-
	1 sign(1.e-6,segment.dlstart)
		segment.dmstart=segment.dmstart-
	1 sign(1.e-6,segment.dmstart)
		icheck=icheck+1
		if(icheck.gt.5)then
	print*,' ***SEGMENT_OUT--FATAL--Cant fix consistency failure'
			stop
		endif
		goto 101
	endif

c Gamma at middle of segment.
       segment.gamma=(temid+xmass)/xmass

c Write out this segment.
c	SEGMENT record consists of:
		!segment.xstart        !Initial xy,z of segment.
		!segment.ystart
		!segment.hstart
		!segment.dlstart       !Initial x,y direction cosigns of segment
		!segment.dmstart
		!segment.tend          !relative time at end of segment.
		!segment.hend          !final altitude of segment
		!segment.dlend         !final direction cosigns of segment.
		!segment.dmend
		!segment.gamma         !gamma at middle of segment.
		!segment.nspec      !particle type
			
		!All mapped to segment.recbuf in KASSTRUCTURES.H
        write(2)(segment.recbuf(i),i=1,segment_size)
	inext=inext+1			!count segments.

!Debug:For pretty pictures of shower.
!c     Use original path length since its best aprox to tsegment
!        path=(segment.hstart-segment.hend)/
!     1       sqrt(1.-segment.dlstart**2-segment.dmstart**2)
!                                !Segment is assumed straignt with direction
!        xend=segment.xstart+path*segment.dlstart 
!        yend=segment.ystart+path*segment.dmstart 
!        write(43,4300)segment.xstart,segment.ystart,segment.hstart,xend,
!     1       yend,segment.hend,segment.nspec
! 4300   format(' ',6(f10.3,','),i4,',')
!enddebug


!*************************************************************************
!debug tw: But its nice. Leave it.  18/04/02 ghs
!*************************************************************************
        do i=(segment.hend/10),(segment.hstart/10)
           if(i.le.8000.and.i.ge.1)then
              shower_max(i)=shower_max(i)+1
           endif
        enddo
!enddebug
!*************************************************************************

         return
         end


! ****************************************************************************


	subroutine stor_e(tenergy,height,dl,dm,dn,
     1 x,y,tim,ispec)

c      save an electron or positron
c      Modified:

c      G.H.S. 4/20/89
c            nen can only get up to 2

c	7/3/92 G.H.S. V:1:0.6
c		Since knock-ons can make more then 2 electrons per particle
c		track, increase nen to 1000. Make exceeding this value a fatal
c		error

       include 'kascade.h'
       nen=nen+1
       if(nen.gt.1000) then
              WRITE(6,7079)
7079  format(' ***STOR_E--FATAL--NEN limit exeeded ')
              stop
	endif
       den(1,nen)=tenergy
       den(2,nen)=height
       den(3,nen)=dl
       den(4,nen)=dm
       den(5,nen)=dn
       den(6,nen)=x
       den(7,nen)=y
       den(8,nen)=tim
       den(9,nen)=ispec
       return
       end
c***********************************************
	subroutine stor_g(tenergy,height,dl,dm,dn,
     1 x,y,tim,ispec)

c      save a particle
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       include 'kascade.h'
       ngn=ngn+1
       if(ngn.gt.10000) then
              WRITE(6,7079)
7079  format(' ***Stor_g:ngn limit exeeded ')
              return
              end if
       dgn(1,ngn)=tenergy
       dgn(2,ngn)=height
       dgn(3,ngn)=dl
       dgn(4,ngn)=dm
       dgn(5,ngn)=dn
       dgn(6,ngn)=x
       dgn(7,ngn)=y
       dgn(8,ngn)=tim
       dgn(9,ngn)=ispec
       return
       end
c***********************************************



        SUBROUTINE TARGET(tetev,height,dl,dm,dn,xx,yy,tim,ispec)
C *******************************************************
C  THREE DIMENSIONAL CASCADE SIMULATION VERSION OF      *
C  'INTERF' TYPE HADRON-AIR INTERACTION_ ROUTINE.        *
C  ASSIGNS CHARGES TO ALL PARTICLES.                    *
c      but charge not conserved G.H.S. 12/18/85
C  USE WITH 'INTPH1.FOR' IN PHOTOPRODUCTION CALCULATION.*
C  LAMBDA PRODUCTION INCLUDED.  TSS, OCT '85, MADISON   *
C *******************************************************

c      G.H.S. 1/3/85 
c             Modified for true 3 dimensions.

c     4/9/86 G.H.S.       Fix last particle code.  The priorities for the
c                    distributions to be preserved for the last particles
c                    are Energy, Transvers Mommentum  in that order.

c      G.H.S. 1/14/87       Error in Lambda production. In the piece of code which
c                    generates accompamning K's with Lambda production, if 
c                    the energy requested for the K is more then is 
c                    available then the enrgy of the K is reduce to what is
c                    avaialable and the Parallel Momentum of the K is
c                    adjusted accordingly. The error was in the readjustment
c                    Of the parrel momentum. Fixed it.

c      tetev=Kinectic energy of primary in Tev.
c      height=altitude in meters above sea_level of interaction.
c      dl,dm,dn=direction cosigns of primary
c      xx,yy in meters of interaction
c      tim time in nano seconds of interaction
c      ispec=primary type

c      Particle species codes.
c             1:Gamma
c              2:positron
c              3:electron
c              4:muon (+)
c              5:muon (-)
c                    6:pion (0)
c                    7:pion (+)
c                    8:pion (-)
c                    9:kaon (+)
c            10:kaon (-)
c            11:kaon (0long)
c            12:kaon (0short)
c            13:proton
c            14:neutron
c            15:neutrino(electron)
c            16:anti-neutrino(electron)
c            17:neutrino(muon)
c            18:anti-neutrino(muon)


        DIMENSION CHARGE(3),CHARG(3)
        DIMENSION Avnu(18)
       common/daught/ a(9),b(3),r9(3)       !For decay direction cosigns.
       equivalence (dl1,r9(1)),(dm1,r9(2)),(dn1,r9(3))
       include 'kascade.h'
       qq(p,u)=sqrt(p*p+u*u)

C   WARNING! THESE VALUES ARE FOR BERILIUM
C       DATA Avnu/6*0.,2*1.18,4*1.14,2*1.29,4*0./
C   VALUES FOR AIR
        DATA Avnu/6*0.,2*1.42,4*1.28,2*1.56,4*0./
 
        DATA CHARGE/.6925,.2175,.09/       !CHARGE is the probabilities for the 
c                                   three possible types of meson 
c                                   interactions. the first is normal, 
c                                   second is charge exchange, and last
c                                   is diffractive.

        DATA CHARG/.46,.45,.09/              !CHARG has the probabilities for the 
c                                   nuclen interactions. The last is again
c                                   diffractive, and the first two just
c                                   make up the sum to 1.0.

       if(debena(ispec))then
              nn=ngn+1
              write(6,1000)namtyp(ispec),nn,tetev,height,
     1 dl,dm,dn,xx,yy,tim
1000  format('0',30x,'tetev(Tev)  Alt(meters)    Direction cosigns ',
     1 '       X,Y(meters)    Time(nsec)',/,' ',a8,':',i5,' >>>Tar',
     2 10x,f10.6,f13.0,2(f8.5,','),f8.5,2f10.1,f12.1)
              endif

       etev=tetev+xm(ispec)              !Total energy of primary(Tev)
              egev=1000.*etev                     !Total Energy in Gev.
       tegev=tetev*1000.              !Kinetic energy in Gev.

       etEXC=4.E-4*(1. + 0.6213*Alog(etev/2.E-3))
c                            Amount of energy in this interaction that will
c                            disapear from the cascade as evaporation 
c                            nucleons.
c...............Don't know if etev above should be total or Kinetic energy........
        IF (etexc.GT.8.E-4)etexc=8.E-4
       totexc=totexc+etexc      !Debug. see how much is lost in shower to evap.

        EUSED =etexc                    !Total energy used so far.

        P0=SQRT(etev*etev-xm(ispec)**2)       !Momentum of primary(Tev)

       ymax=alog((etev+p0)/xm(ispec))       !Ymax=alog((E+P)/M. Maximum allowed 
c                                   value for the rapidity variable.

        U=AVNU(ispec)                     !Average number of interactions the
c                                   incident particle undergoes in an
c                                   interaction. Also called the effective
c                                   target size.

        COEF01=.25*U/(1.+.25*U)              !This is a mixing parameter that takes 
c                                   account of the above fact to give
c                                   different shapes of the multiplicity
c                                   distribution for different species of
c                                   incident particles.

        PTE=FPTE(etev)                     !Average Transverse momentum.(in some 
c                                   funny units)

       f=qq(dl,dm)              !Set up arrays for transformation matrix for 
       a(1)=dl*dn/f              !subroutine daughter.
       a(2)=-dm/f              !  | a(1) a(2) a(3) |  | b(1) |  |r9(1) |
       a(3)=dl                     !  | a(4) a(5) a(6) | x| b(2) |= |r9(2) |
       a(4)=dm*dn/f              !  | a(7) a(8) a(9) |  | b(3) |  |r9(3) |
       a(5)=dl/f
       a(6)=dm                     !Vector b is new direction in frame of track.
       a(7)=-f
       a(8)=0.                     !Vector a(3);a(6);a(9)  is track in lab frame -v
       a(9)=dn
c                            Vector a(2);a(5);.. is vector product
c                            vxz normalized (mod=1)

C  DEAL SEPARATELY WITH NUCLEONS AND MESONS

c       Mesons:

        IF(ispec.lt.13)then
               SELECT=pran(xdummy)
              il=2                                   !Default to charge 
c                                                 exchange.
              if(select.ge.charg(1)+charg(2))il=3       !Is it diffractive.
              if(select.lt.charg(1))il=1              !Or is it normal.

               ispc=LMESTY(ispec,IL)       !Pick leading meson.
               X=COEL(ispec,tetev,IL)       !Like fynman x.
               PP=X*P0                     !get parallel momentum.

              pperp= PTran(PTE,ispc)       !Get transvers momentum.
c                                   Supposedly in Tev.

               Ee=SQRT(PP*PP  + xm(ispc)**2 + Pperp**2)!Total energy of 
c                                                 daughter meson (Tev)

               EUSED = EUSED +ee       !Adjust amount of total energy used.

              rk=twopi*pran(xdummy)       !Get azmuthial phi for direction cos.
                            
              call daughter(ee,height,xx,yy,tim,ispc,rk,pp,ispec,8)
c                Save this leading meson.
               GO TO 30              !Go do central particle stuff.
              endif


C   CHOOSE LEADING BARIONS.

c       Lambdas:

       XSLAMB = XLAMBD(tegev)                     !# lambdas /interaction(<.2)

       IF(pran(xdummy).le.XSLAMB)then              !See if we make one.
c                                          LAMBDA PRODUCTION
              if(debena(ispec))then
                     write(6,7000)
7000  format(' ',8x,'Lambda production:')
              endif

              IF (pran(xdummy) .LT. .5)then              ! BACKWARD LAMBDA
                       XLAM = DIFFPI(1)              !Sample (1-x**n) dist.
                       PP = XLAM*P0                     !P parrallel(Tev)

c                                          LAMBDA DECAY
c              !!This 'decay' looks questionable!!!
                     IF(pran(xdummy).lt.0.64)then       !P + pi minus
c                          the proton.
                            ispc=13
                            Pperp=PTran(PTE,ispc)       !Tranvsvers momentum
                            pplab=.8712*pp
                            Ee=SQRT(pplab**2+xm(ispc)**2+Pperp**2)
                            rk=twopi*pran(xdummy)       !Get azmuthial phi for 
c                                                 direction cos.
                            call daughter(ee,height,xx,yy,tim,ispc,
     1 rk,pplab,ispec,9)
                             EUSED=EUSED + ee       !Drop remaining energy


c                            the pi
                            ispc=8
                            pperp= PTran(PTE,ispc)
                            pplab=.1296*pp
                            ee=SQRT(pplab**2+xm(ispc)**2+Pperp**2)
                            rk=twopi*pran(xdummy)       !Get azmuthial phi for 
c                                                 direction cos
                            call daughter(ee,height,xx,yy,tim,ispc,
     1 rk,pplab,ispec,10)
                              EUSED=EUSED + ee
                     
                            else
c                                                 LAMBDA  -  N + pi zero
c                            Neutron
                            ispc=14
                            Pperp= PTran(PTE,ispc)
                            pplab=.8746*pp
                            ee=SQRT(pplab**2+xm(ispc)**2+PPerp**2)
                            rk=twopi*pran(xdummy)       !Get azmuthial phi for 
c                                                 direction cos.
                            call daughter(ee,height,xx,yy,tim,ispc,
     1 rk,pplab,ispec,11)
                             EUSED=EUSED + ee
       
c                             Pi 0
                              ispc=6
                            PPerp= PTran(PTE,ispc)
                            pplab=.1257*pp
                            Ee=SQRT(pplab**2+xm(ispc)**2+PPerp**2)
                            rk=twopi*pran(xdummy)
c                                   Get azmuthial phi for direction cos.
                            call daughter(ee,height,xx,yy,tim,ispc,
     1 rk,pplab,ispec,12)
                              EUSED=EUSED + ee
                            endif
 
c                     GENERATE ACCOMPANING K+
                     ispc = 9              !If proton primary K +
                     If (ispec.Ne.13)then       !If neutron  primary.
                            ispc= 11              !K short or K long.
                             IF (pran(xdummy) .GT. 0.5)ispc= 12
                            endif
                      X=DIFFPI(2)       !(1-x**n)dist for diff pi's.
                      PP=X*P0
                     Pperp = PTran(PTE,ispc)
                      eE= SQRT(PP*PP + xm(ispc)**2 +pperp**2)
                     rk=twopi*pran(xdummy)
c                                   Get azmuthial phi for direction cos.
                      EUSED = EUSED + ee
       
                      EAVAIL=eTev-EUSED       !See if this last particle.
                      IF (EAVAIL.gt.0)then
                            call daughter(ee,height,xx,yy,tim,
     1 ispc,rk,pp,ispec,13)
                            goto 61              !Go do central production
                            endif
c                                          LAST PARTICLE TAKES All 
c                                          AVAILABLE ENERGY.
                     ee=ee+EAVAIL                     !Recalculate energy.

c                                   5/9/86 G.H.S. preserve P transvers at
c                                   expense of diffp distribution.
                     pp2=(ee**2-xm(ispc)**2-pperp**2)
                     if(pp2.lt.0.0)then              !Make sure enough
c                                                 energy to make it
                            totexc=totexc+ee       !Keep track of evap.
                            if(debena(ispc))then
                                   write(6,1003)namtyp(ispc),
     1 ee,xm(ispc),totexc
                                   endif
                            return                     !Drop it.
       
                            else
c      G.H.S. 1/14/87       Error was found here. we had:
c                           pp=sqrt(p2)   Should be:
                            pp=sqrt(pp2)

            call daughter(ee,height,xx,yy,tim,ispc,rk,pp,ispec,14)
                             RETURN
                            endif
                     endif

C   GENERATE BACKWARD K+. DO NOT WORRY ABOUT THE LAMBDA
c                     Treat as a central particle.
56              YNORM=CENTER(ispec,COEF01)       !Sample rapidity dist of central
c                                          particles
               IF(YNORM.GT..5) GO TO 56

               YLAB=YNORM*YMAX + .05*YMAX       !Adjust a little I forget why.

c       Particle type.
               ispc = 9                     !Assum central particle is a k+
               FR = pran(xdummy)
               IF (FR .GT. 0.5) ispc = 11       !No its realy a k long
               IF (FR .GT. 0.75)ispc = 12       !Actualy its a k short

               pperp=PTran(PTE,ispc)              !Whatever it is sample the 
c                                          transverse Momentum dist.

               AMT=SQRT(xm(ispc)**2 + pperp*pperp)       !Mperp
                  Ee= AMT*COSH(YLAB)
              pplab=amt*sinh(ylab)              !P parallel in lab
              rk=twopi*pran(xdummy)
c                                   Get azmuthial phi for direction cos.
              call daughter(ee,height,xx,yy,tim,ispc,rk,pplab,ispec,15)
               EUSED=EUSED + ee
              goto 57              !get a baryon also
              endif
 
C   PICK THE TYPE OF THE LEADING NUCLEON ACCORDING TO 'CHARGE'
C  COULD BE PROTON, CHARGE EXCHANGE NEUTRON OR DIFFRACTIVE PROTON
C  FOR INCIDENT PROTON. REVERSE FOR INCIDENT NEUTRON.
 
c                                          Pick type of interaction.
57       if(debena(ispec).ne.0)then
              write(6,7001)
7001  format(' ',8x,'Leading particle production:')
              endif
       SELECT = pran(xdummy)
       if((select.le.(charge(1)+charge(2))).and.
     1 (select.gt.charge(1)))then
              il=2                            !its charge exchange.
              nuc=14-jmod(ispec,13)       !proton goes to n and vice versa.

              else
              il=3
              nuc=ispec
              if(select.le.charge(1))il=1
              endif

       ispc=NUC
        FG=1.5-.25*tegev              !Low energy correction to
c                                   multiplicity behavior.
        IF(tegev.LT.6..AND.pran(xdummy).LT.FG)then
              X=SQRT(pran(xdummy))

              else
               X=COEL(ispec,tetev,IL)
              endif
                     
       PP=X*P0                            !X always<= 1.0
       pperp = PTran(PTE,13)
           ee=SQRT(PP*PP  + xm(NUC)**2 + pperp*pperp)

       rk=twopi*pran(xdummy)
                            !Get azmuthial phi for direction cos.
        EUSED = EUSED + ee
        EAVAIL=etev-EUSED
        IF(EAVAIL.gt.0)then              !Was this last particle
              call daughter(ee,height,xx,yy,tim,ispc,rk,pp,ispec,16)
              goto 61                     !Go do central production.
              endif
c                                          LAST PARTICLE TAKES All 
c                                          AVAILABLE ENERGY.
       ee=ee+EAVAIL                            !Recalculate energy.

c                                   5/9/86 G.H.S. preserve P transvers at
c                                   expense of diffp distribution.
       pp2=(ee**2-xm(ispc)**2-pperp**2)
       if(pp2.lt.0.0)then                     !Make sure enough
              totexc=totexc+ee
c                                          energy to make it
              if(debena(ispc))then
                     write(6,1003)namtyp(ispc),ee,xm(ispc),totexc
              endif

              return              !drop it.

       else
              pp=sqrt(pp2)
       call daughter(ee,height,xx,yy,tim,ispc,rk,pp,ispec,17)
c                                                 put it back.
               RETURN
       endif


C                       SAMPLE .50 DIFFRACTIVE PION

   61   RR=pran(xdummy)
        IF(RR.le..50)then
 
               IF(tegev.lt.10.)then
                      DIFFI=.5+.25*tegev
                      N1=DIFFI
                      N2=N1+1
                      DDIF=DIFFI-N1
                      ND=N1
                      IF(pran(xdummy).LT.DDIF) ND=N2
       
                     else
                     ND=3
              
                     endif
              
              X=DIFFPI(ND)
              IF (pran(xdummy) .le. 0.25)then
                     ispc = 6
                     GO TO 38
                     endif

              IF (pran(xdummy) .le. 0.50)then
                     ispc = 8
                     IF (ispec .EQ. 14)       ispc=7
                     GO TO 38

                     else
                     ispc = ispec - 6
                     endif

38               PP=X*P0
              pperp = PTran(PTE,ispc)
               ee=SQRT(PP*PP + xm(ispc)**2 + pperp*pperp)
              rk=twopi*pran(xdummy)
c                                   Get azmuthial phi for direction cos.
              call daughter(ee,height,xx,yy,tim,ispc,rk,pp,ispec,18)
               EUSED = EUSED + ee
              goto 30
              endif 

 
 
C  CENTRAL PRODUCTION ACCORDING TO A MODIFIED
C  STENLUND & OTTERLUND FIT.
c                            CHECK AVAILABLE ENERGY, STOP IF EXCEEDED

30       if(debena(ispec))then
              write(6,7002)
7002  format(' ',8x,'Central production:')
         endif

       EAVAIL=etev-EUSED
       indk=0                     !Init ltypes.
42       if(eavail.gt.0)then
              YNORM=CENTER(ispec,COEF01)       !Sample rapidity dist.
               YADD=.05*YMAX
               YLAB=YNORM*YMAX + YADD
              ispc=LTYPES(tegev,ispec,indk)       !Get a species type.
               IF (ispc.GE.9. AND.ispc.LE.12)YLAB = 0.9*YLAB       !Adjust for 
c                                                        mesons.

               pperp=PTran(PTE,ispc)       !Get pperp for this particle
               AMT=SQRT(xm(ispc)**2 + pperp*pperp)
              Ee=AMT*COSH(YLAB)
              pplab=amt*sinh(ylab)
              EAVAIL=EAVAIL-Ee
              rk=twopi*pran(xdummy)       !Get azmuthial phi for direction cos.
              call daughter(ee,height,xx,yy,tim,ispc,rk,pplab,ispec,19)
               GO TO 42              
              else 
C  LAST PARTICLE TAKES All AVAILABLE ENERGY.
c      Only non electron-positrons produced centrally so last particle
c      was in dgn
              call reca_g(eee,height,dl2,dm2,dn2,xx,yy,tim,ispc)       
c                                            !pull it back
              ee=eee+EAVAIL       !Recalculate energy.
c                                   5/9/86 G.H.S. preserve P transvers at
c                                   expense of diffp distribution.
              pp2=(ee**2-xm(ispc)**2-pperp**2)

              if(pp2.lt.0.0)then       !Make sure enough energy to make it.
                     totexc=totexc+ee
                     if(debena(ispc))then
                            write(6,1003)namtyp(ispc),ngn+1,
     1 ee,xm(ispc),totexc
1003  format(' ',16x,a8,':',i3,3x,f10.6,3x,
     1 'Not enough energy. drop it! mass=',f10.6,
     2 ' Total evap. energy=',e14.7)
                     endif

                     return                     !Drop it. Energy goes into 
c                                          evaporation.
              else
                     pplab=sqrt(pp2)
           call daughter(ee,height,xx,yy,tim,ispc,rk,pplab,ispec,20)
c                                   put it back.
                      RETURN
              endif
        endif
         END





	SUBROUTINE UNICAS(TEPRIM,ispec,Height,dl,dm,dn,x,y)
c      G.H.S. 12/30/85
c             Added stuff to generalize in anticipation of including ems code.
c             Add timing. Seperate electrons,positrons from all other 
c             particles.

c      G.H.S. 5/14/86       Add in low energy nucleon-delta production.  Treat as a
c                  3 body decay.  Changes also in :DECAY.

c      G.H.S. 5/19/86       Add Gamma interactions:Compton and pair production, 
c                    from EMS program. Eventuall all of EMS will be absorbed
c                    into this program. Treat gamma interaction as a 'decay'.

c      G.H.S. 5/19/86        Found that the stable particle propagation code in 
c                    UNICAS didn't use the verticle direction cosign to
c                    modify the verticle distance traveled to the
c                    interaction point. Fixed it.

c      G.H.S. 5/22/86       Make an exception and drop low energy brem created
c                    gammas before they get entered.

c	7/16/90 G.H.S. V:1:0.1
c		Fix a possible upward going particl.
c		Don't let particle go above 1 gm.

c	09/12/96 G.H.S. v:1:0.7
c		Replace Stanev bremsstrahlung code with new and improved(and
c		correct) code written by GHS. Replace routines RBSME and RBSMS
c		with routines BREMSSTRAHLUNG and BREM_INTENSITY.

c	17/12/96 G.H.S. v:1:0.7
c		Replace Stanev pair production code with new and improved(and
c		correct) code written by GHS. Replace routines RPAIMS and
c		RPAIME	with routines PAIR_PRODUCTION and PAIR_INTENSITY and
c		PAIR_CROSSECTION.  This fixes a major problem in the
c		pair_poduction crossections at low energies and fixes minor 
c		inaccuaracies in the energy calculation of the pairs above 
c		primary photon energies of 500 MeV.
c		Also fix a minor error in the compton crosssection calculation.

c	11/06/01 GHS, M.P.K.
c		Added calls to the heavy primary code (subroutine do_heavy
c		which in turns call routines from nuclib). Changed the 
c		treeament of the initial particle to allow for heavy
c		primaries. For ispec, use 20+mass of the primary (i.e.
c		Helium 4 would have ispec=24).
!               Scheme is: if primary particle is a nucleus (ispec>20) then
!               in the start of UNICAS call DO_HEAVY whcih propagates the
!               heavies and their fragments, determines all the nuclear 
!               interactions , saves all the tracks of the heavies and 
!               produces all the single nucleons that come out of such 
!               interactions. These are saved (by store_g) to be processed 
!               in the normal manner in the main loop.
!               

!	10/07/01 GHS V1:2.0
!		 At very low energies its possible for electron/positron
!		to end up with less energy at the BREMSTRAHLUNG interaction 
!		point from de/dx then we give to the gamma. This is due to an 
!		apporximatio we make of ignoring the de/dx energy loss before 
!		we determine in BREMSSTRAHLUNG  e_gamma. Limit things in a 
!		quick and dirty manner to give all available energy to the 
!		gamma
!      08/12/04 GHS  V:1:3.3
!               A Bug: When the injection depth is less than 1gm we have 
!               problems with thinking that we have upgoing particles and 
!               dropping them. Change places where 1.0 gm is used as the 
!               test limit to .5*segment_head.depth


c      This is the main shower generation routine.
c      tEprim=Kinetic Energy of primary particle.
c      ispec=Code number of species of primary particle.
c
c      Particle species codes.
c             1:Gamma
c              2:positron
c              3:electron
c              4:muon (+)
c              5:muon (-)
c                    6:pion (0)
c                    7:pion (+)
c                    8:pion (-)
c                    9:kaon (+)
c            10:kaon (-)
c            11:kaon (0long)
c            12:kaon (0short)
c            13:proton
c            14:neutron
c            15:neutrino(electron)
c            16:anti-neutrino(electron)
c            17:neutrino(muon)
c            18:anti-neutrino(muon)

c      height=Altitude in meters above sea_level of injection
c      Dl,dm,dn= direction cosigns of this particle.

c      Electrons and positons generated are stored in a sperate array from
c      that of other particles and are preferentially handled first.

	real tetev,e_gamma,t,bremdedx,e_positron,e_electron
       include 'kascade.h'
	
c               INITALIZE THINGS
       nen=0       !Nr electrons-positrons to propagate
       ngn=0       !Nr of all other types of particles stored away

c      G.H.S. 4/13/89
c         Time is now the differece from the time the primary would have hit
c         the ground(hobs) and the time the secondary does(in nanosec).
       tim=0.

c      Save particle in array holding those particles not yet followed.
c      DEN array for electrons-positrons.
c      dgn arrAY FOR ALL OTHERS.
       	if(ispec.eq.2.or.ispec.eq.3) then        	!e+ or e-
              	call stor_e(teprim,height,dl,dm,dn,x,y,tim,ispec)

       	elseif(ispec.le.18)then 		!Anything else not a heavy
              	call stor_g(teprim,height,dl,dm,dn,x,y,tim,ispec)

!	11/06/01 GHS. If primary is a heavy nucleus process it here.
       	else
		call do_heavy(teprim,height,dl,dm,dn,x,y,tim,ispec)

	endif
c
c TOP OF LOOP TOP OF LOOP TOP OF LOOP

c    Unstack any waiting electron-positron
100    IF (nen.ne.0)then              !Do we have any more particles?
              call reca_e(tetev,height,dl,dm,dn,x,y,tim,ispec)



c Electon-positron:ispec=2,3
c                            Electron/positrons will either brem or
c                            hit the ground.  Make the assumption here
c                            That de/dx and multiple scattering changes to
c                            energy and path length are second order as far
c                            as the calculation of the interaction distance
c                            is concerned.
c          Check that it is above threshold.
              soft=0.            !Init soft gamma energy sum for printout.


c                     To allow us to follow electrons(and positrons) to hobs
c                     only drop a e+ or e-  when its below
c                     threshold.

101           if(thresh(ispec).gt.tetev)then
                     if(debena(ispec))then
                            write(6,1001)namtyp(ispec),
     1 nen+1,tetev,height,x,y,tim
                     endif
                     goto 100       !Drop it.
              endif

c BREMSSTRAHLUNG(above 1 MeV, see notes in routine_ BREMSSTRAHLUNG)..
   	call bremsstrahlung(tetev,ispec,e_gamma,t,bremdedx)

c                                   e_gamma=energy of gamma
c                                   t is distance in radiation lengths to 
c                                   interaction point.
c				    BREMDEDX=average energy lost from electron
c				    due to BREM of photons with energies below
c				    1 MeV. See notes in routine BREMSSTRAHLUNG.

              t=t*rad_len              !Convert to gm/cm**2



              call propint(tetev,height,dl,dm,dn,x,y,tim,ispec,t)
c                     Propagate sets new height and tim.

!****************************************************************************
c	GHS 10/07/01: At very low energies its possible for electron/positron
!		to end up with less energy at the interaction point from
!		de/dx then we give to the gamma. This is due to an apporximatio
!		we make of ignoring the de/dx energy loss before we determine
!		in BREMSSTRAHLUNG above e_gamma. Limit things here in a quick
!		and dirty manner. Give all availabe energy to the gamma.

	      if(e_gamma.gt.tetev-bremdedx)then
			e_gamma=tetev-bremdedx
	      endif

!****************************************************************************

              if(height.eq.hobs.or.tetev.lt.thresh(ispec).or.
	1 height.ge.yds(.5*segment_head.depth))then
			goto 100
	      endif
                          !Quit if hit ground, or below Cherenkov threshold.

c       5/22/86 G.H.S.       
c              Drop the interaction(but do reduce the energy) if the gamma is
c              soft.
              tetev=tetev-e_gamma-bremdedx     !(Tev)  electron energy.
              if(e_gamma.lt.thresh(1))then     !Continue on with this photon?
                     soft=soft+e_gamma         !if the gamma's too soft.
                     goto 101
               endif
c                                   Assume negligable pperp.

              if(debena(ispec))then
                     nn=nen+1
                     write(6,1006)namtyp(ispec),nn,
	1 tetev+e_gamma+bremdedx,height,dl,dm,dn,x,y,tim
1006  format('0',30x,'tetev(Tev)  Alt(meters)    Direction cosigns ',
     1 '       X,Y(meters)    Time(nsec)',/,' ',a8,':',i5,' >>>Brem',
     2 9x,f10.6,f13.0,2(f8.5,','),f8.5,2f10.1,f12.1)
                     write(6,1007)soft
1007  format(' ',15x,'Energy(tev) lost to soft gammas:',f10.6)
                     write(6,1000)namtyp(ispec),nn,
     1 tetev,height,dl,dm,dn,x,y,tim
              endif

c              SET UP for produced gamma.
              ispc=1              !Type =gamma.
              call stor_g(e_gamma,height,dl,dm,dn,x,y,tim,ispc) 
              if(debena(ispc))then
                     write(6,1000)namtyp(ispc),ngn,
     1 e_gamma,height,dl,dm,dn,x,y,tim
              endif

              go to 101       !Continue on with the electron.
       endif



C OTHER PARTICLES
       if(ngn.ne.0)then       !Get out any other particle.
              call reca_g(tetev,height,dl,dm,dn,x,y,tim,ispec)

c       Check that it is above threshold.
              if(thresh(ispec).gt.tetev)then
                     if(debena(ispec))then
                            write(6,1001)namtyp(ispec),
     1 ngn+1,tetev,height,x,y,tim
1001  format('0',a8,':drop ',i4,13x,f10.6,f13.0,23x,2f10.1,f12.1)
                            write(6,1012)thresh(ispec)
1012  format(' ',25x,' Abs. threshold for this particle=',f10.6,
     1 ' Tev.')
                     endif
                     goto 100       !Drop it.
               endif
                                   
cGamma:
              if(ispec.eq.1)then
                     theight=gms(height)


c        Gamma interaction.
                     emev=tetev*(1.0e+6)       !This cross section code uses
c                                          energy in Mev. units.

c                     CALCULATE TOTAL COMPTON CROSSECTION(FUNCTION OF emev)
c			Fix error in crosssection 17/12/96 GHS
c                     term1=alog((2.*emev+e_mass)/e_mass)       !Heitler

                     term1=alog((2.*emev)/e_mass)      !Heitler equation III(55)
                     sigc=54.983*cq*(term1+.5)/emev

c			Pair Production crosssection. 17/12/96 GHS
		     call pair_crossection(tetev,sigp)

c              Form total crossection and find interaction point.
                     stot= sigc+sigp    !This crossection is in units of
c					!proabability/radiation length.
c                                        use lambda*sigma=1. where lambda is
c                                        interaction length(or mean free path) 
c                                        in radiation lengths.
C Propagate the gamma.
c	Generate actual distance T(in radiation lengths):
c  Adoption of DIST from Old KASCADE. Just want to pick from a normalized
c  prob=(SIGMA)*exp(-T*SIGMA)  where SIGMA is probability/radiation length
		     r=pran(xdummy)
		     t=-alog(r)/stot		!t distributed exponentially
					 	!t is distance in radiation 
						!lengths to interaction point.
                     t=t*rad_len       		!Convert to gm/cm**2
        	     call propint(tetev,height,dl,dm,dn,x,y,tim,
	1 ispec,t)
				       		!Propagate sets new height and
						!tim.
		     if(height.eq.hobs.or.tetev.lt.thresh(ispec).
	1or.height.ge.yds(.5*segment_head.depth))then
	 		goto 100		!Quit if hit ground,or below 
		     endif
						!Chrenkow threshold
                     if(debena(ispec))then
                            nn=ngn+1
              write(6,1005)namtyp(ispec),nn,tetev,height,
     1 dl,dm,dn,x,y,tim
1005  format('0',30x,'tetev(Tev)  Alt(meters)    Direction cosigns ',
     1 '       X,Y(meters)    Time(nsec)',/,' ',a8,':',i5,' >>>Pair',
     2 9x,f10.6,f13.0,2(f8.5,','),f8.5,2f10.1,f12.1)
	             endif

c COMPTON or PAIR PRODUCTION?
                      if(pran(xdummy).le.(sigc/stot))then
						!Compton scattereing.
						!Treat compton production as a
						!2 particle decay.
cCOMPTON
		CALL decay(tetev,height,dl,dm,dn,x,y,tim,ispec)
                        goto 100
	              else
cPAIR PRODUCTION
		call pair_production(tetev,e_electron,e_positron)
                        ispec=2              !Positron.
                call stor_e(e_positron,height,dl,dm,dn,x,y,tim,
     1 ispec) 
                        if(debena(1))then
                                  write(6,1000)namtyp(ispec),nen,
     1 e_positron,height,dl,dm,dn,x,y,tim
1000  format(' ',16x,a8,':',i3,3x,f10.6,f13.0,2(f8.5,','),f8.5,
     1 2f10.1,f12.1)
                        endif
c     SET UP OTHER PARTICLE OF THE PAIR
			ispec=3			!Electron
                call stor_e(e_electron,height,dl,dm,dn,x,y,tim,
     1 ispec) 
                        if(debena(1))then
                                  write(6,1000)namtyp(ispec),nen,
     1 e_electron,height,dl,dm,dn,x,y,tim
                        endif
                        go to 100
                     endif
              endif


cNeutrino type particle: Drop and go on.
              IF(ispec.GE.15)then
                     if(debena(ispec))then
                            write(6,1001)namtyp(ispec),
     1 ngn+1,tetev,height,x,y,tim
                     endif
                     GO TO 100       !drop it
              endif

cA good particle.
              TT = TLife(ispec)       !Get Lifetime*C_light/mass in meter/mev

c Pi zero(Or any very short lifetime particle)
c Decay it immediatly, enter its products, and go on.
              IF (TT.EQ..0)then       
                     CALL DECAY(tetev,height,dl,dm,dn,x,y,tim,ispec)
                     GOTO 100
              endif

c Muon: Popagate it until it decays (or goes below obs altitude),
c enter its decay products, and go on.
              IF (ispec .EQ. 4 .OR.ispec.EQ. 5)then

c Add mass to get total energy. (Gamma=Etot/Mass)
                     t=REXP(TT*(tetev+xm(ispec)))       !Decay distance in 
c                                                 meters.
                     call propdec(tetev,height,dl,dm,dn,x,y,tim,
     1 ispec,t)
c              Propagate sets new height and tim.
			    if(height.eq.hobs.or.
	1 tetev.lt.thresh(ispec).or.height.eq.yds(.5*segment_head.depth))then
				goto 100
	    		    endif
c                            Quit it hits or below chrenkov threshold.

c                            Were above obs depth. Decay the muon.
                     CALL DECAY(tetev,height,dl,dm,dn,x,y,tim,ispec)
                     GOTO 100       !Do some more
                     endif

cRemaining particles may decay or interact. Get interaction depth and record it.
              f=FPNI(tetev,ispec)
c                     G.H.S. 5/19/86 Found that the stable particle 
c                     propagation code here didn't use the verticle 
c                     direction cosign to modify the verticle distance 
c                     traveled to the interaction point. Fixed it.
              r=REXP(f)
              g=GMS(height)
		gr=g+r*dn
		if(gr.le.(.5*segment_head.depth))then
			write(6,2308)g,r,tetev,height,dl,dm,
	1 dn,x,y,tim,ispec,t
2308	format(' ***UNICAS-Track goes too high:g,r,tetev,
	1 height,dl,dm,dn,x,y,tim,ispec,t:',/,2(' ***'5e14.7,/),
	2 ' ***',i5,e14.7,/,' ***G+R set to .5*segment_head.depth gm.')
	                gr=.5*segment_head.depth
		endif
              Hd=YDS(gr)
c                            See were it interacts

cStable hadrons:
c                            Propaget to interaction alt or drop if below 
c                            obs alt. If interacts get reaction products.
c                            5/14/86 Treat delta production as 3 body decay.
              IF (TT.lt.0.)then       !Hadrons have a negative lifetime to 
c                                   flag them.
c                                   propagte to the interaction using r as
c                                   distance in gm/cm**2.
                     call propint(tetev,height,dl,dm,dn,x,y,tim,ispec,r)
			    if(height.eq.hobs.or.
	1 tetev.lt.thresh(ispec).or.height.eq.yds(.5*segment_head.depth))then
				goto 100
	    		    endif

c                     Hits the ground or below threshold.
c              Propagate sets new height and tim and tetev.

           if(tetev.lt.0.0025)then              !See if low enough K.E.
c                                                 for delta production.
                 call decay(tetev,height,dl,dm,dn,x,y,tim,ispec)
c                                   G.H.S. 5/14/86 Treat delta production 
c                                   as 3 body decay. tetev is assured to
c                                   be between 2.5 and 1.23 Gev.(from 
c                                   threshold cut).
           else
                 CALL TARGET(tetev,height,dl,dm,dn,x,y,tim,ispec)
           endif
                     goto 100
              endif

cCharged pions,Kaons:See if they decay or interact
cG.H.S.12/30/85
c Multi by zenith direction cosign to correct for slant.
cG.H.S.1/8/86
c Add mass to get total energy. (Gamma=Etot/Mass)
              t=REXP(TT*(tetev+xm(ispec)))       !distance to decay in meters.
              HDd = height-t*dn
              if(hdd.gt.hd)then              !See if it decays before 
c                                          interacting.
                     call propdec(tetev,height,dl,dm,dn,x,y,       !It decays.
     1 tim,ispec,t)
c              Propagate sets new height and tim.and tetev.
			    if(height.eq.hobs.or.
	1 tetev.lt.thresh(ispec).or.height.eq.yds(.5*segment_head.depth))then
				goto 100
	    		    endif
c                            Hits or below threshold.
                     CALL DECAY(tetev,height,dl,dm,dn,x,y,tim,ispec)
                     goto 100

                     else
                     call propint(tetev,height,dl,dm,dn,x,y,
     1 tim,ispec,r)                                             !It interacts.

c              Propagate sets new height and tim.
			    if(height.eq.hobs.or.
	1 tetev.lt.thresh(ispec).or.height.eq.yds(.5*segment_head.depth))then
				goto 100
	    		    endif
c                            Hits or below threshold.
                     CALL TARGET(tetev,height,dl,dm,dn,x,y,tim,ispec)
c                                          It interacts. Get reaction 
c                                          products, enter them and go on
                     goto 100
                     endif
              endif
       return
       END
 
 
 

 
       FUNCTION XLAMBD(tE)
C RETURNS THE AVERAGE NUMBER OF LAMBDA PER INTERACTION
C AS FUNCTION OF THE PROJECTILE NUCLEON ENERGY (GeV)
C        TSS,  DEC '83

c      G.H.S. 1/7/85
c             Put in test for nl lt zero. Can happen whenever
c             e is less then 4 Gev. Pete palfrey says that in this
c             case the cross section vanises. Ill set xlambd to 0
c      Te=Kinetic energy of nucleon.(Gev)
       
        DIMENSION AVNL(25)
        DATA AVNL/
     *  .001,.01,.022,.033,
     *  .043,.053,.063,.072,.079,.086,.093,.098,.105,.111,
     *  .115,.119,.123,.126,.129,.131,.133,.134,.135,.136,.136/
 
        NL=10.*ALOG10(tE/4.) + 1
        IF(NL.GT.25)  NL=25

c       G.H.S.1/9/86
       if(nl.lt.1)then              !No lambdas below 4 gev.
              xlambd=0.
              return
              endif

        XLAMBD=AVNL(NL)
        RETURN
        END
!*****************************************************************************



!*****************************************************************************
!   All heavy nuclei code follows here
!*****************************************************************************

	subroutine do_heavy(teprim,height,dl,dm,dn,x,y,tim,ispec)
!*****************************************************************************
!	Main routine for processing heavy nuclei. Results in tracks of heavies,
!       and places nucleons(p ands n) into array DGN for further processing.
!*****************************************************************************

!	Input:  teprim=primary nucleus total K.E.energy in TeV
!               height=inital altitude(in meters) of primary.
!	        dl,dm,dn=inital direction cosigns of primary.
!               x,y=inital x,y cords(meters) ofg primary.
!               tim=Diff in time(ns) of fragment from v=c primary at this alt.
!               ispec=sprimary species. For heavies of mass number IA, ispec =
!               IA+20, ie. helium (4) has ispec=24.

! Modified:

!	13/06/01 GHS
!               Impliment and integrate into KASCADE. Use calls to PROPINT,
!               TARGET and STOR_G where appropriate. Add comments as we can
!               figure things out(lower case text). Clean up as we can.


	IMPLICIT NONE
	real teprim,height,dl,dm,dn,x,y,tim
	integer ispec

	include 'heavy.h'
	include 'kascade.h'

 

!	intitialize the nuclear interaction stuff. 
	call nuc_nuc_ini   !Inits: nucleus profiles, PP cross sections and
			   !interaction lengths is air.

			!3 types(as far as I can see now) of nucleon 
			!fragmentations are  possible.
			!For KODFRAG=-1: Superposition model.
			!For KODFRAG=1: Nuclear fragmentation. Total 
			!               fragmentation to all nucleons at first
                        !               interaction
			!For KODFRAG=2: Group all spectator nucleson(non 
			!elastic or wounded nucleons) as 1 fragment.
			!For KODFRAG=3: Abrasion-Ablation model.

	kodfrag=3	!Use the abrasion-ablation code

	call heavy_nuc(teprim,height,dl,dm,dn,x,y,tim,ispec)
                              !This models interaction of nucleus and air
			      !using method flagged by KODFRAG. As single
			      !nucleons are produced they are placed into
		              !DGN for further processing.
	return
	end

!*****************************************************************************



	SUBROUTINE HEAVY_NUC(teprim,height,dl,dm,dn,x,y,tim,ispec)
!****************************************************************************
C...Routine that generates the interaction of a nucleus of
C.  mass number A and KE energy  per nucleon teprim/a (TeV)
!****************************************************************************
!  Modified:

!	11/06/01 GHS
!	    As single nucleons are produced place them into DGN for further
!               processing.
!	    As single n8ucleons interact process the interaction with TARGET
!	    Propagate and record tracks of fragments to their specifi 
!               interaction points.

	IMPLICIT NONE
	real teprim,height,dl,dm,dn,x,y,tim
	integer ispec,i,j,k,nfrag,nbt,nf
	integer ia,ja,isp,z_nuclei

	real tnucleon,enucleon,d_len,tetev,etev,r,sig0,sigel,angx,angy
	real sigt,slope,rho

	real pran, xdummy
	real rexp
	integer iatarget

	include 'heavy.h'

	integer JAFRAG (60), IAF(60)
	real XFRAG(60),XXX(60),YYY(60), AXX(60), AYY(60),tim_fragment(60),
	1 e0(60)
	real amp,amp2,rpox
	real xx1,yy1,axx1,ayy1
	real xmass

	include 'kascade.h'
	DATA AMP/0.938/ AMP2/0.879844/
	DATA RPOX /0.3624/


!Initalize primary parameters. 
	ia=ispec-20     !ia is mass number of beam nucleon
	if(ia.gt.iamax)	then
		stop "Heavy primary mass number must be less than 57"
	endif

!***************************************************************************
!	Superposition Model: Treat as IA seperate nucleons, that shower 
!	and interact indpendently.
!***************************************************************************
	IF (KODFRAG .EQ. -1)  THEN
			!Get charge of stable nuclei of mass number ia.
		call mass_number2charge_mass(ia,z_nuclei,xmass)
		isp=13                !Protons
		tnucleon=teprim/ia	!tnucleon is KE energy per nucleon of 
                			!the beam nucleus
		DO K=1,z_nuclei
			call stor_g(tnucleon,height,dl,dm,dn,x,y,tim,isp) 
              	enddo
		isp=14		      !Neutrons
		DO K=1,ia-z_nuclei
			call stor_g(tnucleon,height,dl,dm,dn,x,y,tim,isp) 
            		if(debena(isp))then
                	     write(6,1000)namtyp(isp),ngn,
     1 tnucleon,height,dl,dm,dn,x,y,tim
1000  format(' Superposition ',a8,':',i3,3x,f10.6,f13.0,2(f8.5,','),f8.5,
     1 2f7.1,f12.1)
              	        endif
              	enddo
		RETURN
	ENDIF
!***************************************************************************



!***************************************************************************
!Framentation models: ia=mass number of beam nucleus,tnucleon=KE energy per 
! nucleon(TeV)
!***************************************************************************

	if(kodfrag.ne.3)then
		do i=1,60
			do j=1,3
				ppp(j,i)=0	!INit ppp to zero for non 
						!Abrasion-Ablation models
			enddo
		enddo
	endif

	NFRAG = 1 		!We start with one nucleous(the primary)
	JAFRAG(1) = IA          !Mass number of original nucleus
	XFRAG(1)  = height      !Starting altitude of primary nucleus (meters)
	AXX(1)    = dl	        !Diretion cosigns of primary nucleus
	AYY(1)    = dm		!Can Get dn from dl,dm
	XXX(1)    = x		!coords of entry point of primary nucleus
	YYY(1)    = y
	tim_fragment(1) = tim   !Time diff from v=c primary at this alt.
	e0(1)     = teprim/ia + xm(13)  !total Eneregy/nucleon for this 
					!nucleus.

!***************************************************************************
!	Main Fragment processing loop
!***************************************************************************
	DO WHILE (NFRAG .GT. 0)
				   !Get out the next nucleus to fragment.
				   !We are using last in/first out here.
	   height = XFRAG(NFRAG)   !Starting altitude of fragment in meters
	   XX1 = XXX(NFRAG)        !x,y cords of start of fragment
	   YY1 = YYY(NFRAG)  
	   AXX1 = AXX(NFRAG)       !direction cosigns of fragment
	   AYY1 = AYY(NFRAG)
	   JA = JAFRAG(NFRAG)      !mass number of nucleus
	   tim=tim_fragment(nfrag) !time of fragment
	   enucleon=e0(nfrag)      !Total Energy(TeV) per nucleon this fragment
				   !(before proagation)
	   NFRAG = NFRAG-1	   !Reduce fragment count 


           znth=sqrt(1-axx1**2-ayy1**2)  !Determine zenith direction cosign.
	   		      !Make sure we are normalized
	   d_len=sqrt(znth**2+axx1**2+ayy1**2)
	   axx1=axx1/d_len
	   ayy1=ayy1/d_len
	   znth=znth/d_len


	   IF (JA .eq. 1) THEN   !is the nucleus a nucleon?
		if(pran(xdummy)<.5)then   !Randomly choose charge
			isp=14
		else 
			isp=13
		endif
		tnucleon=enucleon-xm(isp)
		call stor_g(tnucleon,height,axx1,ayy1,znth,xx1,yy1,tim,isp) 
	   else
		CALL SIGNUC_INI (jA,enucleon) !A-air (nucleus-nucleus) cross 
					!sectio. Also determins ALNUC 
		r = rexp(ALNUC(JA))	!Distance to interaction point in 
					!gm/cm**2(I checked this. Code says
					!ALNUC is gm/cm**2
				!Propagate nucleus to interaction point.
				!This saves the track in the seg file
				!It also bends the track in the earths magnetic
				!field,mutiple scatters and reduces energy due 					!to ionization. 
		isp=ja+20

!***********************************************************************
! Note that the propagation causes the nucleus to lose energy. 
!  ie. enucleon changes!!!!!
!***********************************************************************
		tetev=ja*(enucleon-xm(13)) !KE energy in TeV of fragment 
					   !before propagation.

		call propint(tetev,height,axx1,ayy1,znth,xx1,yy1,tim,isp,r)
           		!See if the nucleus is to be dropped
			!Worry about threshold test.
 	   	tnucleon=tetev/ja	!Re-determine energy /nucleon. We may
					!have lost some during propagation.
		enucleon=tnucleon+xm(13)	!Total energy /nucleon(TeV)

			!Test to see if we hit the ground or go under hadron
			!KE threshold per/nucleon(much less then cherenkov 
			!threshold)
		IF (height.gt.hobs.and.tnucleon.ge.thresh(14))then  
					 !This is a keeper
!Debug/Shower generation listing.
	   	  if(debena(20))then
          	  	call mass_number2charge_mass(ja,z_nuclei,xmass)
              	  	write(6,4000)trim(nuclei_names(z_nuclei)),
	1 ja,tetev,height,axx1,ayy1,znth,xx1,yy1,tim
4000  format('0',30x,'tetev(Tev)  Alt(meters)    Direction cosigns ',
	1 '       X,Y(meters)    Time(nsec)',/,' ',a,
	1 '(',i3,') >>>Heavy int.   ',
	1 f10.6,f13.0,2(f8.5,','),f8.5,2f10.1,f12.1)
           	  endif

	            !Generate mass number of air nucleus which is the target
		  IATARGET = 14 + 2*INT((1.+RPOX)*PRAN(XDUMMY))
	            !Get the inelastic and elastic cross sections for this 
		    !intereaction
				!Get pp cross sections for this energy.
		  CALL SIGMA_PP (enucleon, SIGT, SIGEL, SIG0, SLOPE, RHO)
 
		  CALL INT_NUC (IATARGET, JA, SIG0, SIGEL) 
		    !INT_NUC produces (in  common block /CNUCNS/) NB:Number of 
                    !wounded nucleons in the beam nucleus.     NBEL:Number of 
                    !elastically scattered nucleons in the beam nucleus.


		  NBT = NB + NBEL    !Total number of nucleons produced.

!***************************************************************************
!	Decide how to fragment things 
!***************************************************************************
		  IF (KODFRAG .EQ. 1)  THEN
			     !Complete disolution of the nucleus.Disolve all 
			     !fragments into elastic nucleons of mass 1, 
			     !energy tnucleon.
	         	CALL FRAGM1(JA,NBT, NF, IAF)
			nbel=nbel+nf	!Increase elastic nuclei
			nf=0		!No fragments to process.

	      	  ELSE IF(KODFRAG .EQ. 2)  THEN
			!Combine all spectators into 1 fragment.
	         	CALL FRAGM2(JA,NBT, NF, IAF)
	     	  ELSE if(kodfrag.eq.3)then
			!Abrasion-ablation model.
	         	CALL FRAGM (IATARGET, JA, NBT,B, NF, IAF)
	   	  	if(debena(20))then
          	  	  call mass_number2charge_mass(iatarget,
	1 z_nuclei,xmass)
	      	  	  write(6,4001)trim(nuclei_names(z_nuclei)),
	1 iatarget,nb,nbel,nf
4001	format(' Heavy on ',a,'(',i3,') produces:',
	1 '# Wounded nucleons=',i5,' # Elastic nucleons= ',i5,
	2 '# Fragments=',i5)
		          write(6,4002)(iaf(j),j=1,nf)
4002	format(' Fragment masses:',8(i4),7(/,17x,8i4))
			endif

	     	  else
			stop 'Illegal KODFLAG specified'
		  ENDIF


!*****************************************************************************
!          New Fragments
!*****************************************************************************
		  if(nf.ne.0)then
                      DO K=1,NF  !Load in the new fragments onto end of
					!fragment arrays.
				!array.
	      		NFRAG = NFRAG+1   
	      		JAFRAG(NFRAG) = IAF(K)  !Mass number
	      		XFRAG(NFRAG) =  height  !altitude of production 
						!gm/cm**2
	      		XXX(NFRAG) = XX1 	!coords of production
	      		YYY(NFRAG) = YY1
	      			      !Change diretions of fragments
			              !PPP only non-zero for 
				      !kodfrag=3:Abrasion-ablation model.
				      !Note: the use of geom might be more 
				      !accurate here but it probably doesn't 
				      !matter.
	      		etev=enucleon*iaf(k)    !Total energy of fragment
				           !PPP(1,k) is x momentum(MeV/c) of 
					   !fragment k.
			ANGX = 1.e-6*PPP(1,K)/etev !change of direction 
						    !cosigns in radians (I 
						    !think, I hope)
	      		ANGY = 1.E-6*PPP(2,K)/etev
	      		AXX(NFRAG) = AXX1 + ANGX
	      		AYY(NFRAG) = AYY1 + ANGY
	      		tim_fragment(nfrag)=tim  !time of fragment(ns)
			e0(nfrag)=enucleon
	   	    ENDDO
                  endif

!*****************************************************************************
!	  Wounded nucleons
!*****************************************************************************
	   	  if(nb.ne.0)then
                     DO K=1,NB	!Wounded nulcei interact as single 
				!particles right here.
				!treat as a nucleon-air interaction.
				!They will have same directions as primary
				!fragment. By definition they will have mass
				! 1 and energy tnucleon.
			if(pran(xdummy)<.5)then   !Randomly choose charge
				isp=14
			else 
				isp=13
			endif
			call target(tnucleon,height,axx1,ayy1,znth,xx1,
	1 yy1,tim,isp)
                     ENDDO
                  endif
!*****************************************************************************
!	Elasticaslly scattered nucleons
!*****************************************************************************
	   	  if(nbel.ne.0)then
                     DO K=1,NBEL !Treat elastically scattered nucleons as
				      !free nucleons emitted to propagate to 
				      !their next interaction.They retain 
				      !direction of original nucleus.
			if(pran(xdummy)<.5)then   !Randomly choose charge
				isp=14
			else 
				isp=13
			endif
			call stor_g(tnucleon,height,axx1,ayy1,znth,
	1 xx1,yy1,tim,isp) 
                     ENDDO
                  endif
               else
					!Fragment/nucleus dropped
	   	  if(debena(20))then
          	  	call mass_number2charge_mass(ja,z_nuclei,xmass)
			if(height.le.hobs)then
	              	  	write(6,4004)trim(nuclei_names(z_nuclei)),
	1 ja,tetev,height,axx1,ayy1,znth,xx1,yy1,tim
4004  format('0',30x,'tetev(Tev)  Alt(meters)    Direction cosigns ',
     1 '      X,Y(meters)    Time(nsec)',/,' ',a,'(',i3,') >>>Heavy hits!',
     1 f10.6,f13.0,2(f8.5,','),f8.5,2f10.1,f12.1)
		        endif
           	  endif
		endif        !End of 'interaction above hobs?' if statement.
	   ENDIF             !End of 'is this a nucleon?' if statement



	ENDDO
	RETURN
	END


