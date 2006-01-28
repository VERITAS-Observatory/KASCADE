MODULE KASTRIGGER_SUB2
!****************************************************************************
!	These are the routines extracted from KASTRIGGER_SUBS (KASTRIGGER.F90)
!	to allow the single/MPI versions of KASTRIGGER. This file is for
!	the single processor version of KASTRIGGER.
!	The mPI version are in file KASTRIGGER_MPI.F90
!****************************************************************************

	IMPLICIT NONE
	private
	public :: datain
	public :: fileopn
	public :: readarea
	public :: whipple_output_m

CONTAINS
    SUBROUTINE DATAIN
!******************************************************************************
!        Read in the input parameters from the parameter file.
!******************************************************************************

!     Modified:

!	13/5/98	GHS V:1:1:5:1:2.0
!		Set up for VERITAS.
!		1:Mount direction supplied here now. Unlike the past we now
!		  do not find the relative values of pe directions and timings
!		  to the mount plane in KASLITE. Instead the PES file has the
!		  directions cnad timings for when the pe's hit the ground.
!	          It is in this program where we read in the mount direction
!		  (DATAIN) and preform the relative pe directions and adjusted
!		  timing corrections.
!		  Do this to increase flexability in mount direction choice.
!		2:Move setting of DISC_WIDTH and ADC_GATE to DATAIN from .PAR
!		  file. Just cleaning up really.
!		3:Add MHEAD%CONCENTRATION paramerter to m_head in
!		  STRUCTURES.F90. Used to calculate efficiency along with
!		  MHEAD%REFLECTIVITY.in from parameter file in DATAIN.

!	15/7/99 GHS V:1:1:5:1:2.4
!		Move initalization of RANLUX(call to RANSTART) to FILEOPN

!      18/04/03 GHS V:1:1:6:1:3.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               We use random_seed_file_name and input_par_file_name here.
!     03/06/04 GHS V:1:3:8:3:4.4 
!                Add an elevation command line input option (-e). Value is 
!                elevation in degreees. If used ( ELEVATION_INPUT=.true.) Then
!                in datain dl,dm,dn will be overridden by elevation and az=0.
!     17/02/05 GHS V:1:3:8:3:5.3
!                Add printseeds flag to ranstart and ranend

! 	use structures
	use kastrigger_def
	use record_defs
        use kastrg_command_line
!	use kas_ranlux
	IMPLICIT NONE

	real :: vlength
	character(len=8) :: adate
	character(len=10) :: time          	
        integer :: ios
	integer :: printseeds

        call date_and_time(adate,time)

     WRITE(6,1000)adate(1:4),adate(5:6),adate(7:8),time(1:2),time(3:4),time(5:)
1000  format(' ',15x,a4,' ',a2,'/',a2,5x,a2,':',a2,':',a, &
			&   '--------KASTRIGGER------')

        !Init random numnber gen.
        printseeds=1
        call ranstart(printseeds,random_seed_file_name)


!	Open input parameter (.par) file. Assigned to unit 1 in command file.
        OPEN(1,ACCESS='SEQUENTIAL',STATUS='OLD',ACTION='READ',iostat=ios,&
             & file=input_par_file_name)
        if(ios>0)then
           print*,'Failure opening input parameter file:',input_par_file_name
           stop 'Input parameter file open failure'
        endif

        READ(1,1001)DL,DM,DN  		!1:Get direction cosigns of the mount 
					!relative to zenith.
					!dl,dm defined in structures.f90
					!which is 'USE'd by kastrigger_def
					!Dn defined in kastrigger_def
1001  FORMAT(3F20.10)  
!debug
!       print*,'1dl,dm,dn: ',dl,dm,dn
!enddebug
       if(elevation_input)then      !Use elevation from input command line to
                                     !determine dl,dm,dn
           dl=-1.0e-15               !Assume azimuth is 0 deg (north)
           dn=-cosd(90.-elevation)
           dm=sqrt(1.-dl**2-dn**2)
        endif
!debug
!       print*,'2dl,dm,dn: ',dl,dm,dn
!enddebug



        vlength=sqrt(dl**2+dm**2+dn**2)!Get length of starting vector

        dl=dl/vlength !normalize unit vector(Just being careful here.)
        dm=dm/vlength
        dn=dn/vlength 

!Trigger parameters.
	read(1,1002)mhead%disc_width	!2:effective 10m trigger coincidence 
					!pulse width. Used for noise 
					!determination within trigger.
	read(1,1002)mhead%adc_gate	!3:10m adc gate width.Set here for 
					!campatability of shared code with
					!KASAOMEGA
        read(1,1002)mhead%pesum   	!4:Minimum number of photon pe's  for a
1002	format(f10.4)			!pixel to fire a disc.In either pes or
					!units of pedistal widths(sigmas)
	read(1,1002)mhead%hmult		!5:Minimum Trigger multiplicity
	read(1,1003)mhead%noise		!6:Noise Generation enable.
	read(1,1002)mhead%rate_noise	!7:Noise rate:pe's/ns/deg**2
	read(1,1002)mhead%reflectivity	!8:Mirror mhead%reflectivity(Fraction 
					!of 1.)
	read(1,1002)mhead%concentration	!9:Light cone concentration factor.
	read(1,1003)outpes		!10:PES summary file to be created flag.
1003	format(l1)
	read(1,1003)hadron		!11:HADRON search enable.
	read(1,1004)mhead%options  	!12:Aperture/camera/telescope type:
1004	format(a)			!W541=Whipple 541 pixel camera
					!W331_125=whipple 331 .125 deg pixel
					!W331_25=whipple 331 .25 deg pixel
					!W331_1875=whipple 331 .1875 deg
					!W37,W109,W151,W271
       WRITE(6,1011)dl,dm,dn,mhead%disc_width,mhead%adc_gate,mhead%pesum, &
	 &  mhead%hmult,mhead%noise,mhead%rate_noise,mhead%reflectivity,  &
	&   mhead%concentration,outpes, hadron,mhead%options

1011	format(    &
	 & '                                   Mount dl,dm,dn   = ',e14.7,/, &
	 & '                                                    = ',e14.7,/, &
	 & '                                                    = ',e14.7,/, &
	 & '   MHEAD%DISC_WIDTH:Effective width for disc noise  = ',f10.4,/, &
	 & '                     MHEAD%ADC_GATE:ADC Gate width  = ',f10.4,/, &
	 & 'Minimum pe threshold in a pixel(pes or ped_widths)  = ',f10.4,/, &
	 & '   Minimum Trigger Multiplicity (541 inner,mid:-1)  = ',f10.4,/, &
	 & '   MHEAD%NOISE:Enable generation of noise in pixels = ',l5,/, &
	 & '        MHEAD%RATE_NOISE:Noise rate: pes/ns/deg**2  = ',f10.4,/, &
	 & 'MHEAD%REFLECTIVITY:Fraction of good PEs to reflect  = ',f10.4,/, &
	 & '         MHEAD%CONCENTRATION:Light cone efficiency  = ',f10.4,/, &
	 & '  OUTPES:Enable creation of summary file(Yes=true)  = ',l5,/, &
	 & '   HADRON:Enable angle search algorithum(Yes=true)  = ',l5,/, &
	 & '   OPTIONS: = ',a)
	close(unit=1)
	return
    END SUBROUTINE DATAIN
!******************************************************************************

    SUBROUTINE FILEOPN
!******************************************************************************
!	Open the input file. Open the Ouput M files, transfer header. Open
!	the output Summary file.
! PES Files may be in Only BIN format.
!******************************************************************************
!	Modified:

!	29/1/98 GHS	Converted to F90.

!	13/5/98	GHS V:1:1:5:1:2.0
!		Set up for VERITAS.
!		1:Mount direction supplied here now. Unlike the past we now
!		  do not find the relative values of pe directions and timings
!		  to the mount plane in KASLITE. Instead the PES file has the
!		  directions cnad timings for when the pe's hit the ground.
!	          It is in this program where we read in the mount direction
!		  (DATAIN) and preform the relative pe directions and adjusted
!		  timing corrections. Put dl,dm,dn in top mhead inplace of 
!                 it used to come in in KASLITE. Do all this to increase 
!                 flexability in mount direction choice.
!	17/11/99 GHS V:1:1:5:1:2.5
!		Include call for whipple 490 pixel camera
!	21/3/00 GHS V:1:1:6:1:2.6
!		Include call for Laura's 1024 pixel square camera.
!       21/9/00 GHS V:1:1:6:1:2.9
!               Add HDF5 capability.  All new HDF5 C routines in KASHDF5.c
!               Add capability for input PES file to be in HDF5 format.
!                In the options list;  'HDF5' enables reading
!                of an HDF5 file of pes. Any other option(or none) assumes
!                unfomatted sequential(non-portable) format. Do this so files 
!                are portable between platformas(pc and alpha in particular).
!	11/07/01 GHS V:1:2:6:1:2.13
!		Add stuff to print out Heavy ion primary name.

!       20/06/02 GHS:1:1:6:1:2:4.0
!               Major version change. This version gets various input 
!               and output file names from the command line:

!       07/05/02 GHS V:1:3:8:3:4.0
!               Another Major version change. To handle very large pes
!               files (showers ~1 Tev+) use a direct access file for the pes 
!               input file 
!               Requires getting the header record from a seperate 
!               file (pes_input_file//'.head' )
!     17/06/04 GHS V:1:3:8:3:4.6
!                Add a -w Option (sets spec_weight_flag to true) which causes 
!                the ouput MHDF5 file events to be cut on the spectrum weights.
!     01/11/04 GHS V:1:3:8:3:5.2
!                Upgrade the MFILE=MHDF5 option to use the MHDF5SimDataFile
!                classes (instead of the old mhdf5_* routines). This should
!                speed thing up substantially.
!                1:New routines to init and write the ihead record of the
!                  output MHDF5 file.
 
!dir$ name(mhdf5init="MHDF5Init") 
!dir$ name(veritaseventweight="VeritasEventWeight") 
!dir$ name(whippleeventweight="WhippleEventWeight") 


	use kastrigger_def
	use record_defs
	use wcamera_def
        use kastrg_command_line
	IMPLICIT NONE
        character*80 pes_head_input_file

	real :: dni,area
	integer :: pt,ios,ierr
	logical :: mfile
        logical :: quiet=.false.

        character(len=12),dimension(9) ::  ptype=(/ &           !PMT/Grid type
	&' Visable    ',' Old Luny   ',' Solar Blind',' CEASAR     ', &
	&' ASGAT      ',' Part-on-gnd','Whipple     ',' VERITAS    ', &
	&'VERITAS-TRI '/)

 	character(len=8),dimension(18) :: namtyp=(/ &		!Particle names.
	&'Gamma   ','Positron','Electron','Mu +    ','Mu -    ','PI 0    ', &
	&'PI +    ','PI -    ','K +     ','K -     ','K long  ','K short ', &
	&'Proton  ','Neutron ','eNeutrin','anti-eNu','muNeutri','ant-muNu'/)

!******************************************************************************
!      Heavy Nuclei declarations:
!******************************************************************************
	real :: xmass
	integer :: qz,ia
	character(len=13),dimension(113) ::  nuclei_names=(/ &
  & 'Hydrogen     ','Helium       ','Lithium      ','Beryllium    ',&
  & 'Boron        ','Carbon       ','Nitrogen     ','Oxygen       ',&
  & 'Fluorine     ','Neon         ','Sodium       ','Magnesium    ',&
  & 'Aluminum     ','Silicon      ','Phosphorous  ','Sulfur       ',&
  & 'Chlorine     ','Argon        ','Potassium    ','Calcium      ',&
  & 'Scandium     ','Titanium     ','Vanadium     ','Chromium     ',&
  & 'Manganese    ','Iron         ','Cobalt       ','Nickel       ',&
  & 'Copper       ','Zinc         ','Gallium      ','Germanium    ',&
  & 'Arsenic      ','Selenium     ','Bromine      ','Krypton      ',& 
  & 'Rubidium     ','Strontium    ','Yttrium      ','Zirconium    ',&
  & 'Niobium      ','Molybdenum   ','Technetium   ','Ruthenium    ',&
  & 'Rhodium      ','Palladium    ','Silver       ','Cadmium      ',&
  & 'Indium       ','Tin          ','Antimony     ','Tellurium    ',&
  & 'Iodine       ','Xenon        ','Cesium       ','Barium       ',&
  & 'Lanthanum    ','Cerium       ','Praseodymium ','Neodymium    ',&
  & 'Promethium   ','Samarium     ','Europium     ','Gadolinium   ',&
  & 'Terbium      ','Dysprosium   ','Holmium      ','Erbium       ',&
  & 'Thulium      ','Ytterbium    ','Lutetium     ','Hafnium      ',&
  & 'Tantalum     ','Tungsten     ','Rhenium      ','Osmium       ',&
  & 'Iridium      ','Platinum     ','Gold         ','Mercury      ',&
  & 'Thallium     ','Lead         ','Bismuth      ','Polonium     ',&
  & 'Astatine     ','Radon        ','Francium     ','Radium       ',&
  & 'Actinium     ','Thorium      ','Proactinium  ','Uranium      ',&
  & 'Neptunium    ','Plutonium    ','Americium    ','Curium       ',&
  & 'Berkelium    ','Californium  ','Einsteinium  ','Fermium      ',&
  & 'Mendelevium  ','Nobelium     ','Lawrencium   ','Rutherfordium',&
  & 'Dubnium      ','Seaborgium   ','Bohrium      ','Hassium      ',&
  & 'Meitnerium   ','Unununium    ','Ununbium     ','Ununtrium    ',&
  & 'Ununquadium  '/)


!	Input file. Structure from STRUCTURES
!        See if PES file is an  HDF5 file(its the default).
!       File name is pesfile.hdf.
       If(index(mhead%options,"PESFILE=BIN")/=0)then
!Input file is unformatted non-portable.
!	Read in the pe header lines and write the info out.
          pes_head_input_file=trim(pes_in_dat_file)//".head"
          OPEN(2,ACCESS='sequential',STATUS='old',FORM='unformatted',&
               & ACTION='READ',iostat=ios,file=trim(pes_head_input_file))
          if(ios>0)then
             print*,'File open failure pes head input file:', &
                  & trim(pes_head_input_file)
             stop ' PES input head file open fialure'
          endif
          read(2)mhead%pe_head
          close(2)

          !Note in f90 direct access recl is in bytes!
          open (2,access='direct',status='old',form='unformatted', &
               &  recl=pes_rec_size*4,iostat=ios,file=trim(pes_in_dat_file))
          if (ios>0)then
             print*,'KASTRIGGER-Fileopn-Pes data input file open error:', &
                  &       trim(pes_in_dat_file),ios
             stop 'Pes Input data file open failure'
          endif
          ipes_next=1
          
     !   else                                   !Default is HDF5 pes file.
      !                                         ! All peshdf5 are C routines.
     !   print*,' Using HDF5 pes input file type.'
     !     call peshdf5_open(trim(pes_in_hdf5_file)//char(0),ierr) 
     !                                                 !Open input HDF5 file.
     !     if(ierr<0)then
     !            print*,' HDF5 open failure for file pesfile.dat'
     !            print*,' peshdf5_open ierr:',ierr
     !            stop  'HDF5 Pes input file open failure'
     !     endif
     !   
     !     call peshdf5_pehead_read(mhead%pe_head)  !Read in pe_head
     !
     !
       endif

!Load up some stuff into mhead%pe_head before we write it out.
!	This is also done in whippleini but we need to reload here
!	since the above read cleared them out.

	mhead%pe_head%dl=dl		!These used to be set in KASLITE. Its 
	mhead%pe_head%dm=dm		!done here now for flexability.



!	Open the output 'M'file.  Structure from STRUCTURES
!	Write out headers.
        !Defualt is HDF5 format.
    If(index(mhead%options,"MFILE=BIN")/=0)then

!	21/3/00 GHS V:1:1:6:1:2.6
	if(sq1024%on)then
          OPEN(sq1024%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	  Write(sq1024%unit)mhead
	

	elseif(w37%on)then
	   OPEN(w37%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	   Write(w37%unit)mhead
	
	elseif(w109%on)then
	   OPEN(w109%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	   Write(w109%unit)mhead
	
	elseif(w151%on)then
	   OPEN(w151%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	   Write(w151%unit)mhead
	
	elseif(w271%on)then
	   OPEN(w271%unit,ACCESS='sequential',STATUS='new',& 
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	   Write(w271%unit)mhead
	

	elseif(w331_125%on)then
        OPEN(w331_125%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	   Write(w331_125%unit)mhead
	

	elseif(w331_1875%on)then
        OPEN(w331_1875%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	   Write(w331_1875%unit)mhead
	
	elseif(w331_25%on)then
          OPEN(w331_25%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	  Write(w331_25%unit)mhead
	

!	16/11/99 GHS V:1:1:5:1:2.5
	elseif(w490%on)then
          OPEN(w490%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	  Write(w490%unit)mhead
	

	elseif(w541%on)then
	   OPEN(w541%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	   Write(w541%unit)mhead
	

	elseif(V499%on)then
         OPEN(V499%unit,ACCESS='sequential',STATUS='new', &
               & FORM='UNFORMATTED',iostat=ios,file=mfile_out_dat_file)
	 mhead%pesum=pv499%threshold(1)
	 Write(v499%unit)mhead
	endif


        if(ios>0)then
           print*,'File open failure for output M file.'
           stop 'OUtput M file open failure'
        endif
!Mfile is HDF5 format!
     else
                          !Create HDF5 output Mfile. All HDF5 routines are in
                          ! C in kashdf5.c.
                          !1:Instantiate MHDF5SimDataFile object 
                          !2:Create new file
                          !3:Transfer mhead data to ihead structure and write
                          !  out header record to MHDF5 file.
       call mhdf5init(trim(mfile_out_hdf_file)//char(0),        &
       & mhead%pe_head%segment_head%itype,mhead%pe_head%segment_head%tep,     &
       & mhead%pe_head%segment_head%dli,  mhead%pe_head%segment_head%dmi,     &
       & mhead%pe_head%segment_head%magnet_field,                             &
       & mhead%pe_head%segment_head%etr,  mhead%pe_head%segment_head%depth,   &
       & mhead%pe_head%segment_head%zobs, mhead%pe_head%segment_head%xinitial,&
       & mhead%pe_head%segment_head%yinitial,                                 &
       & mhead%pe_head%segment_head%idfile,mhead%pe_head%segment_head%version,&
       & mhead%pe_head%dl,       mhead%pe_head%dm,    mhead%pe_head%xseg,     &
       & mhead%pe_head%yseg,     mhead%pe_head%hobs,  mhead%pe_head%x_offset, &
       & mhead%pe_head%y_offset, mhead%pe_head%petype,mhead%pe_head%version,  &
       & mhead%noise,            mhead%disc_width,    mhead%rate_noise,       &
       & mhead%reflectivity,     mhead%full_aperture, mhead%pesum,            &
       & mhead%step_size,        mhead%itheta_max,    mhead%phi_steps,        &
       & mhead%version,          mhead%adc_gate,      mhead%hmult,            &
       & mhead%concentration,    mhead%options)

       print*,' MHDF5 Output Filename: ',trim(mfile_out_hdf_file)
     endif


!	pes output file will have only pe file header subset.
	if(outpes)write(7)mhead

!	Calculate some stuff
 	dni=sqrt(1.-mhead%pe_head%segment_head%dli**2-  &
		&   mhead%pe_head%segment_head%dmi**2)
	area=mhead%pe_head%xseg*mhead%pe_head%yseg


	if(index(mhead%pe_head%petype,'VI')/=0)then
		pt=1
	elseif(index(mhead%pe_head%petype,'OL')/=0)then
		pt=2
	elseif(index(mhead%pe_head%petype,'SB')/=0)then
		pt=3
	elseif(index(mhead%pe_head%petype,'C')/=0)then
		pt=4
	elseif(index(mhead%pe_head%petype,'AS')/=0)then
		pt=5
	elseif(index(mhead%pe_head%petype,'POG')/=0)then
		pt=6
	elseif(index(mhead%pe_head%petype,'W')/=0)then
		pt=7
	elseif(index(mhead%pe_head%petype,'VA')/=0)then
		pt=8
	elseif(index(mhead%pe_head%petype,'VT')/=0)then
		pt=9
	else
		write(6,1001)mhead%pe_head%petype
1001	format(' KASTRIGGER_FATAL--Illegal photon type:petype=',a)
		stop
	endif

	if(mhead%pe_head%segment_head%itype>20)then
		ia=mhead%pe_head%segment_head%itype-20
		call mass_number2charge_mass(ia,qz,xmass)
 	        write(6,2009)trim(nuclei_names(qz)),ia
2009  format(' SHOWER SPECIFICATION:',/,&
	& '  Primary for this shower was:  ',a,'(',i3,')')
	else
	        write(6,2008)namtyp(mhead%pe_head%segment_head%itype)
2008  format(' SHOWER SPECIFICATION:',/,&
	& '  Primary for this shower was:  ',a)
	endif

	write(6,1003) &
	& mhead%pe_head%segment_head%tep,                              &
	& mhead%pe_head%segment_head%dli,                              &
	& mhead%pe_head%segment_head%dmi,dni,                          &
	& mhead%pe_head%segment_head%depth,                            &
	& mhead%pe_head%hobs,                                          &
	& mhead%pe_head%segment_head%magnet_field,                     &
	& mhead%pe_head%segment_head%etr,                              &
	& mhead%pe_head%xseg,mhead%pe_head%yseg,                      &
	& mhead%pe_head%segment_head%idfile,                           &
	& mhead%pe_head%x_offset,mhead%pe_head%y_offset,              &
	& mhead%pe_head%segment_head%version,                          &
	& mhead%pe_head%version,mhead%version
1003	format(      							&
	& '               Primary Energy: ',f10.5,' TeV',/              &
	& '        Primary  DLI DMI DNI : ', 3e14.7,/,                  &
	& '              Injection depth: ',f10.2,/,                    &
	& '            Detector altitude: ',f10.1,/,                    &
	& ' Magnetic Field specification: ',a,/,                        &
	& '                Thershold MeV: ' f8.2,/,                     &
	& '                    xseg,yseg: ',f8.2,' x',f8.2,/,           &
	& '                       IDFILE: ',i5,/,                       &
	& '   Shower Offset Impact Point: ','x=',f6.2,' y=',f6.2,/,     &
	& '         From KASCADE Version: ',a,/,                        &
	& '         From KASLITE Version: ',a,/,                        &
	& '      This KASTRIGGER Version: ',a)

!*************************************************************************
!See if we are to cut on the energy spectrum weight.
!*************************************************************************
        print*,' spec_weight_flag: ',spec_weight_flag
        if(spec_weight_flag)then
           event_type=mhead%pe_head%segment_head%itype !Change to real
           event_tep=mhead%pe_head%segment_head%tep
           print*,'event_type,event_tep: ',event_type,event_tep
           if(index(mhead%options,'V499')/=0)then    !VERITAS  telescope
              call veritaseventweight(event_tep,event_type,event_weight,quiet)
           else
              call whippleeventweight(event_tep,event_type,event_weight,quiet)
           endif
           print*,' Cutting ouput events with a spectrum weight of: ', &
                & event_weight
        endif

!*************************************************************************
	return
    END SUBROUTINE FILEOPN

!*****************************************************************************

    SUBROUTINE READAREA
!*****************************************************************************
!	This routine reads in an areas worth of pe's.
!*****************************************************************************
!	written: 10/16/90 G./H.S.
!	Modified:

!	29/1/98 GHS	Convert to F90.

!       21/9/00 GHS V:1:1:6:1:2.9
!               Add HDF5 capability.  All new HDF5 C routines in KASHDF5.c
!               Add capability for input PES file to be in HDF5 format.
!                In the options list;  'HDF5' enables reading
!                of an HDF5 file of pes. Any other option(or none) assumes
!                unfomatted sequential(non-portable) format. Do this so files 
!                are portable between platformas(pc and alpha in particular).
!       07/05/02 GHS V:1:3:8:3:4.0
!               Another Major version change. To handle very large pes
!               files (showers ~1 Tev+) use a direct access file for the pes 
!               input file 
!               Requires getting the header record from a seperate 
!               file (pes_input_file//'.head' )



!	I'm using a dynamically allocated pes records for each area. The
!	pointers to these records are held in a linked list format. Each
!	record has a pointer to the next record. Pointer FIRST_PES_PTR points
!	to the first record. FIRST_PES_PTR%NEXT_PTR points to the second; etc.

	use kastrigger_def
        use kastrg_command_line
	use record_defs
	IMPLICIT NONE

	logical :: first=.true.
	integer :: i,error,nx_save=-1000000,ny_save=-1000000
        !integer :: pes_hdf5_index=0
        integer :: ierr,ios
        integer :: icount

!	Read in the very first one to init things.
!	After this there is always one sitting in pes_ptr
!	Get space for it
	if(first)then
		allocate(pes_ptr,stat=error)
		if(error/= 0)then
                   print*,"Program could not allocte space for FIRST PES_PTR."
                   stop
		endif
 
!See if input file is HDF5 format.
                If(index(mhead%options,"PESFILE=BIN")/=0)then
                                !Old Binary(unformated direct access
                                !non-portable) PES file is input.
                   read(2,rec=ipes_next,iostat=ios)pes_ptr%pe

                   if (ios==10013)then
                      empty=.true.			!Empty file
                      return
                   endif

                   if (ios>0)then
               print*,'KASTRIGGER-Error reading first Pes data input record', &
                  & trim(pes_in_dat_file),ios,' ipes_next:',ipes_next
                      stop 'Read of first Pes Input data record failure'
                   endif

                   ipes_next=ipes_next+1

                !else
                !   call peshdf5_in(pes_hdf5_index,pes_ptr%pe,ierr)  !Get pes
                !   if(ierr<0)then
                !      empty=.true.			!Empty file
                !      return
                !   endif
                !                  !Bump pes pointer in HDF5 file for next time.
                !   pes_hdf5_index=pes_hdf5_index+1 
                endif

                first_pes_ptr=>pes_ptr
		first=.false.
	else				!New area, deallocate all old stuff
				!Worry here that we are not nullifying the
				!pointer within the thing we are
				!deallocating!!!!!!!!!!!!!!!
		do i=1,ipe
			if(i/=ipe)then
				last_pes_ptr=>first_pes_ptr%next_ptr
				deallocate(first_pes_ptr,stat=error)
				if(error/= 0)then
 	     print*,"Program could not deallocte space for FIRST_PES_PTR."
					stop
				endif
				first_pes_ptr=>last_pes_ptr
			else
				nullify(first_pes_ptr)
				deallocate(LAST_pes_ptr,stat=error)
				if(error/= 0)then
 	     print*,"Program could not deallocte space for LAST_PES_PTR."
				stop
		endif
			endif
		enddo
		first_pes_ptr=>pes_ptr
	endif
	!Init for next area.
	ipe=1
	last_pes_ptr => first_pes_ptr		!Save the pe.


!	Gather the pes of an area.
!	read a pe from the input file.
	do
		nullify(pes_ptr)
		allocate(pes_ptr,stat=error)	!Get space for next record
		if(error/= 0)then
	 	     print*,"Program could not allocte space for PES_PTR."
		     stop
		endif

!See if input file is HDF5 format.
                If(index(mhead%options,"PESFILE=BIN")/=0)then
                                !Old Binary(unformated sequential 
                                !non-portable) PES file is input.
                   read(2,rec=ipes_next,iostat=ios)pes_ptr%pe

                   if (ios==-1)then
                      shower_end=.true.			!End of shower found
                      return
                   endif

                   if (ios>0)then
                      print*,'KASTRIGGER-Error reading Pes data input record',&
                           & trim(pes_in_dat_file),ios,' ipes_next:',ipes_next
                      stop 'Read of Pes Input data record failure'
                   endif
                   ipes_next=ipes_next+1

                 !else
                 !   call peshdf5_in(pes_hdf5_index,pes_ptr%pe,ierr)  !Get pes
                 !  if(ierr<0)then
                 !     shower_end=.true.			!End of shower found
                 !     return
                 !  endif
                                  !Bump pes pointer in HDF5 file for next pes.
                 !  pes_hdf5_index=pes_hdf5_index+1 

                 endif


		if(pes_ptr%pe%nx==first_pes_ptr%pe%nx.and. &
        		 & pes_ptr%pe%ny==first_pes_ptr%pe%ny)then
			ipe=ipe+1
			last_pes_ptr%next_ptr=>pes_ptr	!Extend linked list
			last_pes_ptr=>pes_ptr		!repoint last pointer.
		else
			if(first_pes_ptr%pe%nx<nx_save)then
				print*,' X PES Out of order. nx_save,nx:', &
					&nx_save,first_pes_ptr%pe%nx
				print*,' PES Out of order. ny_save,ny:', &
					&ny_save,first_pes_ptr%pe%ny

			elseif(first_pes_ptr%pe%nx==nx_save.and.&
				&first_pes_ptr%pe%ny<ny_save)then
				print*,' Y PES Out of order. nx_save,nx:', &
					&nx_save,first_pes_ptr%pe%nx
				print*,' PES Out of order. ny_save,ny:', &
					&ny_save,first_pes_ptr%pe%ny

			elseif(first_pes_ptr%pe%nx/=nx_save)then
				nx_save=first_pes_ptr%pe%nx
				ny_save=first_pes_ptr%pe%ny
			elseif(first_pes_ptr%pe%ny/=ny_save)then
				ny_save=first_pes_ptr%pe%ny
			endif
			icount=icount+1!count areas.
                        if(mod(icount,10000)==0)then
                           print*,'Processing area #',icount
                        endif

                        return  !next area
		endif
	enddo

!436	shower_end=.true.			!End of shower found
!	return

!435	empty=.true.			!Empty file
!	return
    END SUBROUTINE READAREA
!******************************************************************************

    SUBROUTINE WHIPPLE_OUTPUT_M(disc,mult_trigger,pst_trigger,pptr,wptr, &
		& ithphi)

!*****************************************************************************
!	Compress output M-file and write to disk.
!*****************************************************************************
!
!       14/10/99 ghs
!                 Fix error in trigger test.
!       06/10/00 GHS
!                 Add capability to write Mfile out in HDF5 format.
!                 Remove binary uncompressed format capability.
!       Modified:
!       9/10/00 GHS V:1:1:6:1:2.10
!               Add more HDF5 capability.All new HDF5 C routines in KASHDF5.c 
!               1.Add cabability to writer output Mfile as HDF5 file.
!                 This required a routine (W10M_PEPULSE_PACKED_TIME) that 
!                 orders the time data by pixel and converts to integer*2. 
!                 Also IS saves the offset time for reconstruction later. The 
!                 guts of this routine was extracted and is now also called 
!                 (for optine PEPULSE=BIN) from the guts of W10M_PEPULSE_PACK.

!       16/04/01 GHS V:1:1:6:1:2.11
!               Reimpliment the quick and dirty way of finding disc triggers 
!               and adc values(summing pe's in each pixel). One would use this
!               instead of the pepulse method when speed is necessary.
!               2:WHIPPLE_OUTPUT_M: Always write out pixxel timing information.
!     01/11/04 GHS V:1:3:8:3:5.2
!                Upgrade the MFILE=MHDF5 option to use the MHDF5SimDataFile
!                classes (instead of the old mhdf5_* routines). This should
!                speed thing up substantially.
!                2:Convert the write of the MHDF5 to the MHDF5SimDataFile 
!                  in WHIPPLE_OUTPUT_M


!dir$ name(mhdf5out="MHDF5Out") 




	use record_defs 	!Has mhead and mrec
	use structures
	use wcamera_def
	use w10m_subs
	use w10m_pepulse_subs
	IMPLICIT NONE

	integer :: ithphi
	real*4,pointer,dimension(:) :: disc,times
	logical :: mult_trigger		!Multiplicity trigger flag
	logical :: pst_trigger		!PST trigger flag
	type(camera) ::       wptr
	type(camera_ptrs) ::  pptr

	integer*4 nwords,j,k,i,error
	real,pointer,dimension(:) :: packed
        integer :: mfile_index=0

        if((mult_trigger.and.pst_trigger))then
           If(index(mhead%options,"MFILE=BIN")/=0)then
                                 !Compress the disc array into itself using 
                                 !our crude compression alogrithum
!Write it out using Pete's trick.
              call w10m_compress(disc,j)
              nwords=mrec_size+1+j !length of output record
              
	!For timeing processing add the packed timing array and change format
	!slightly. This new format is handled by KAS_SUM_COMPRESS with no
	!changes needed to that code.
! Reference timew for packed pes time array  is kept in:
!	mrec%time_cfd
!To restore all times: time=pixesl_time_packed/4+mrec%time_cfd
              nwords=nwords+1	!Add 1 for number of 
                                                   !packed values.2 I*2 /
                                                   !word.
              call W10M_PEPULSE_PACK(wptr,pptr,ithphi,packed)
              k=size(packed)
              nwords=nwords+k !length of output record
              write(wptr%unit)nwords,MREC,        &
                   &  real(j),(disc(i),i=1,j), &
                   &  real(k),(packed(i),i=1,k)
              mfile_index=mfile_index+1
              if(mod(mfile_index,1000)==0)then
                 print*,' Trigger at:',mfile_index
              endif



           else
                               !Mfile in HDF5 format.
              !First, order times apporpriatly and fill in mrec%time_cdf
              call w10m_pepulse_time_pack(wptr,pptr,ithphi,times)
                  
                       !Write out the hdf5 record.
              call mhdf5out(mfile_index,mhead%pe_head%segment_head%tep,     &
                   & mhead%pe_head%segment_head%idfile,                     &
                   & mhead%pe_head%x_offset,mhead%pe_head%y_offset,         &
                   & mrec%nx, mrec%ny, mrec%itheta, mrec%iphi,              &
                   & mrec%emission_alt, mrec%em_alt_sig, mrec%muon_ratio,   &
                   & mrec%median_time, mrec%fp_median_time, mrec%time_cfd,  &
                   & mrec%fp_time_cfd, mrec%ithphi, mrec%fp_threshold,      & 
                   & mrec%i_mult, mrec%fp_mult,disc,times,wptr%npmt)
              mfile_index=mfile_index+1
              if(mod(mfile_index,1000)==0)then
                 print*,'mfile_index:',mfile_index
              endif
           endif
           m_event_count=m_event_count+1

!*******  END OF Output M-file.**************************************
	endif
	return
    END SUBROUTINE WHIPPLE_OUTPUT_M
!******************************************************************************



END MODULE KASTRIGGER_SUB2
