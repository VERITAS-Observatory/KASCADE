MODULE KASAOMEGA_SUB2
!****************************************************************************
!	These are the routines extracted from KASAOMEGA_SUBS (KASAOMEGA.F90)
!	to allow the single/MPI versions of KASAOMEGA. This file is for
!	the single processor version of KASAOMEGA.
!	The mPI version are in file KASAOMEGA_MPI.F90
!****************************************************************************

	IMPLICIT NONE
	private
	public :: datain
	public :: fileopen
	public :: cmsum_hdf5_read_area
	public :: cmsum_read_area
	public :: newtrig
	public :: hillas_write
        public :: laura
        public :: vhdf5_open
        public :: vhdf5_peds_out
        public :: veritas_vhdf5_out
        public :: whipple_vhdf5_out
        public :: veritas_vhdf5_mc_out
        public :: veritas_vhdf5_out_cleanup
        public :: w10m_display
      CONTAINS



    SUBROUTINE DATAIN
!******************************************************************************
!        Read in the input parameters from the parameter file.
!******************************************************************************

!     Modified:

!	09/2/98 GHS V:1:1:5:1:1:1.1
!		Convert to F90.

!	21/5/98 GHS V:1:1:5:1:2:1.0
!		Add NEW_CONCENTRATION to input parameter file. Used in new
!	versions of WCAMERA.F90 code for VERITAS. Better lightcone modeling.

!	07/10/99 GHS V:1:1:5:1:2:3.0
!		1:Use file name: 'kasaom.par' as generic input parameter file
!		  name. Logicalliy assign actual file to this name in calling
!		  command/script file.

!      20/06/02 GHS:1:1:6:1:2:4.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               We use random_seed_file_name and input_par_file_name here.
!      17/02/05 GHS  V:1:3:8:3:4:4.22 
!               Add a printseeds flag to ranstart and ranend.

	use kasaomega_def
	use kasaomega_command_line
        IMPLICIT NONE

        character(len=8) :: adate
	character(len=10) :: time
        integer :: ios
        integer :: printseeds

	call date_and_time(adate,time)

	
     WRITE(6,1000)adate(1:4),adate(5:6),adate(7:8),time(1:2),time(3:4),time(5:)
1000  format(' ',15x,a4,' ',a2,'/',a2,5x,a2,':',a2,':',a, &
			&    '--------KASAOMEGA-START------')

		!Initalize randome number seed vector.
 	printseeds=1
        call ranstart(printseeds,random_seed_file_name)

!	Open input parameter file
        OPEN(1,ACCESS='SEQUENTIAL',STATUS='OLD',ACTION='READ', &
             & file=trim(input_par_file_name),iostat=ios)
        if(ios>0)then
           print*,' Failure opening input par file: ', &
                & trim(input_par_file_name)
           stop 'Input paramter file open failure'
        endif

	read(1,1001)hadron		!Enable proton proccessing flag.
1001	format(l3)

	read(1,1003)options	!OPTIONS:(any combination of 
					!following options)
					!W10m:'SC'=Supercuts.
					!W10m:'IMAGE'=Images:Produce files to 
					!make images of triggers.
					!W10m:'OUT'=Output, make a new Mfile 
					!after  cuts.
					!W10m:'SM'=INput and Ouput are Summary
					!	  M files.
					!w10m:'AN'=Rgenerate adc vaules using
					!new_noise as noise rate.
1003	format(a)
	read(1,1004)hmult		!Required Hard trigger multiplicity. 
1004	format(i3)			
	read(1,1002)swindow		!Discriminator window.
1002	format(f10.2) 
	read(1,1002)new_adc_gate	!New adc gate

	read(1,1003)detector		!DETECTOR: 'W37','W109','W151',W271,
					!'w331_125','W331_25','W331_1875,'W541'
	read(1,1002)pe_threshold 	!New pmt threshold. works 
					!exactly like PESUM in KASTRIG.
	read(1,1002)new_noise 		!New noise rate 
	read(1,1002)new_reflectivity 	!New Reflectivity value
	read(1,1002)new_concentration 	!New Lightcone concentration value

!        print*,' Options:',options
	return
    END SUBROUTINE DATAIN
!******************************************************************************
    SUBROUTINE FILEOPEN
!******************************************************************************
!	Open the input M(or SM or CM) file and read in its header.
!******************************************************************************

!	Modified:

!	09/2/98 GHS V:1:1:5:1:1:1.1
!		Convert to F90.
!	28/4/99 GHs
!		Make provision to read in m file header withough concentration
!		variable. This is for compatability with old format.
!		Look for 'OLDM' in options.
!	07/10/99 GHS V:1:1:5:1:2:3.0
!		Convert the hdf4 version to one that reads and writes binary
!		files only. If we want ntuples of HDF4 or HDF5 files we can
!		write conversion programs.  Do this so we don't have to
!		worry about mixing libraries(CERN,MPI,HDF5 etc)
!		This is still a single processor version.(Not MPI!)
!		2: change call to open cmsum file to the binary version.
!	30/10/00 GHS
!               Add stuff for HDF5 format input of the cm file.. To retrieve 
!               this data we use the standardized
!               C routines mhdf5_open, mhdf5_in, etc. from kashdf5.c  The same 
!               routines as used in kassum_hdf5 to make the cmsum file. 
!               1: New OPTION: INFILE=BIN. Chosing this option gives us the old
!                  unformatted file input format. Default is now for HDF5 
!                  input format.
!               2: Change FILEOPEN(KASAOMEGA_SNGL.F90) to test for INFILE=BIN
!                  and to add calls to mhdf5_open, mhdf5_mhead_read,
!                  mhdf5_make_types.
!       20/06/02 GHS:1:1:6:1:2:4.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:


      use kasaomega_def               !in file kasaomega_defs.f90
      use record_defs                 !in file structures.f90
      use kasaomega_cmn_sub
      use w10m_subs
      use kasaomega_command_line

      IMPLICIT NONE

      real :: dn,dni,area
      integer :: pt
      character(len=12),dimension(9) ::  ptype=(/ &           !PMT/Grid type
           &' Visable    ',' Old Luny   ',' Solar Blind',' CEASAR     ', &
           &' ASGAT      ',' Part-on-gnd','Whipple     ',' VERITAS    ', &
           &'VERITAS-TRI '/)

      character(len=8),dimension(18) :: namtyp=(/ &	      !Particle names.
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
 
      integer :: ierr,ios

!
!30/10/00 GHS  V:1:1:6:1:2:3.7
!            Tests options to see if we use unformatted binary format for input
!            file. Defauls is to assume input cmsum file is in HDF5 mfile 
!            format.

      if(index(options,'INFILE=BIN')/=0)then

!07/10/99 GHS Use binary version of input file.
!           print*,' Before cmsum_file_open'
         call cmsum_file_open

      else

!          Open HDF5 input cmsum file.

         call cmsum_hdf5_file_open

      endif

!*************************************************************************
!            Open VHDF5RunData formatted HDF5 file for output. 
!*************************************************************************
      if(index(options,'OUTVHDF5').ne.0)then
         call vhdf5_open  !Works for both Whipple and VERITAS simulated data
      endif
!*************************************************************************

!*************************************************************************
!                         Open ouput hillas parameter file(binary format).
!*************************************************************************
!     OPEN(4,ACCESS='sequential',STATUS='new',FORM='UNFORMATTED', &
      if(index(options,'HILLAS').ne.0)then
         OPEN(4,ACCESS='sequential',STATUS='new',FORM='BINARY', &
           & file=cmsum_out_dat_file,iostat=ios)
         if(ios>0)then
            print*,' error opening',cmsum_out_dat_file,' ios:',ios
            stop 'CMSUM_OUT.DAT file open error'
         endif
      endif

!*************************************************************************
!Load up some other values.
!*************************************************************************
      dni=sqrt(1.-mhead%pe_head%segment_head%dli**2-   &
           &  mhead%pe_head%segment_head%dmi**2)
		!Mount is always looking up.
      dn=-sqrt(1-mhead%pe_head%dl**2-mhead%pe_head%dm**2)



!*****************************************************************************
! Following not used for now. Needs some work (alocating dlm,dmm,dnm,xdlm,
! xdlmm,xdnn,ydlm,ydmm,ydnm arrays I think!)
!*****************************************************************************
             !Init some vecotr arrys(used to get MIRROR PLANE core loactions in
	     ! W10M_PROC).
!        print*,' Before w10m_mount_vecini'
!     call w10m_mount_vecini(mhead,mhead%pe_head%dl,mhead%pe_head%dm,dn,hadron)
!        print*,' AFTER w10m_mount_vecini'
!*****************************************************************************


      area=mhead%pe_head%xseg*mhead%pe_head%yseg
!Sparse area correction: for tep gt 5.0 TeV KASLITE used areas that were
			!4 times xseg*yseg but only kept pes from central
			!xseg by yseg. This means we have to adjust for that.

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
		write(6,1000)mhead%pe_head%petype
1000	format(' KASAOMEGA_FATAL--Illegal photon type:petype=',a)
		stop
	endif
	if(mhead%pe_head%segment_head%itype>20)then
		ia=mhead%pe_head%segment_head%itype-20
		call mass_number2charge_mass(ia,qz,xmass)
 	        write(6,2009)trim(nuclei_names(qz)),ia
2009  format(' Shower Parameters:',/,&
	& '  Primary for this shower was:  ',a,'(',i3,')')
	else
	        write(6,2008)namtyp(mhead%pe_head%segment_head%itype)
2008  format(' Shower Parameters:',/,&
	& '  Primary for this shower was:  ',a)
	endif

	write(6,1001)ptype(pt),					       &
	& mhead%pe_head%segment_head%tep,                              &
	& mhead%pe_head%segment_head%dli,                              &
	& mhead%pe_head%segment_head%dmi,dni,                          &
	& mhead%pe_head%segment_head%depth,                            &
	& mhead%pe_head%dl,mhead%pe_head%dm,dn,                        &
	& mhead%pe_head%hobs,                                          &
	& mhead%pe_head%segment_head%magnet_field,                     &
	& mhead%pe_head%segment_head%etr,                              &
	& mhead%pe_head%xseg,mhead%pe_head%yseg,                       &
	& mhead%pe_head%segment_head%idfile,                           &
	& mhead%pe_head%x_offset,mhead%pe_head%y_offset,               &
	& mhead%pe_head%segment_head%version,                          &
	& mhead%pe_head%version,mhead%version,trim(version)
1001	format( &
	& '                Detector type: ',a,/,                        &
	& '               Primary Energy: ',f10.5,' TeV',/              &
	& '        Primary  DLI DMI DNI : ', 3e14.7,/,                  &
	& '              Injection depth: ',f10.2,/,                    &
	& '               Mount dl,dm,dn: '3e14.7,/,                    &
	& '            Detector altitude: ',f10.1,/,                    &
	& ' Magnetic Field specification: ',a,/,                        &
	& '                Thershold MeV: ' f8.2,/,                     &
	& '                    xseg,yseg: ',f8.2,' x',f8.2,/,           &
	& '                       IDFILE: ',i5,/,                       &
	& '   Shower Offset Impact Point: ','x=',f6.2,' y=',f6.2,/,     &
	& '         From KASCADE Version: ',a,/,                        &
	& '         From KASLITE Version: ',a,/,                        &
	& '      From KASTRIGGER Version: ',a,/,                        &
        & '      This KASAOMEGA  Version: ',a)

       WRITE(6,1002)mhead%pesum,mhead%hmult,mhead%noise,mhead%rate_noise, &
		   & mhead%reflectivity,mhead%concentration,             &
		   & trim(mhead%options), &
		   & mhead%step_size,mhead%phi_steps,mhead%itheta_max,    &
		   & mhead%disc_width,mhead%adc_gate
1002	format( ' Original Trigger parameters:',/,    &
	 & ' Minimum pe threshold in a pixel(includes noise)  = ',f10.4,/, &
	 & ' Minimum Trigger Multiplicity (541 inner,mid:+1)  = ',f10.4,/, &
	 & ' MHEAD%NOISE:Enable generation of noise in pixels = ',l5,/,    &
	 & '      MHEAD%RATE_NOISE:Noise rate: pes/ns/deg**2  = ',f10.4,/, &
	 & '            MHEAD%REFLECTIVITY:Global efficiency  = ',f10.4,/, &
	 & '   MHEAD%CONCENTRATION:Efficiency of light cones  = ',f10.4,/, &
	 & ' KASTRIGGER OPTIONS  = ',a,/,            &
	 & '            Whipple 10m:MHEAD%STEP_SIZE for HADRONS = ',f10.2,/, &
	 & '                        MHEAD%PHI_STEPS for HADRONS = ',i10,/,   &
	 & '                       MHEAD%ITHETA_MAX for HADRONS = ',i10,/,   &
	 & '          Effective Discriminater window width (ns) = ',f10.2,/, &
	 & '                       Original ADC gate width (ns) = ',f10.4)
	write(6,1003)area,options,detector
1003	format(' New trigger parameters:',/,           &
	&  '            Grid area (m**2) = ',f10.4,/,&
	&  ' Reqested processing OPTIONS = ',a,/,   &
	&  '      Detector specification = ',a)
	return
    END SUBROUTINE FILEOPEN
!*****************************************************************************

    SUBROUTINE CMSUM_READ_AREA(srec,disc,wptr,pptr,idone)
!*****************************************************************************
!	Read in an event from the binary summary file.
!*****************************************************************************
!    Modified:
!	30/10/00 GHS V:1:1:6:1:2:3.7
!               Add stuff for HDF5 format input of the cm file.
!
!               1: New OPTION: INFILE=BIN.Chosing this option gives us the old
!                  unformatted file input format. Default is now for HDF5 
!                  input format.
!               2. Move the unpacking stuff for the binary records into 
!                  CMSUM_READ_AREA. Create a new routine CMSUM_HDF5_READ_AREA.
!                  In w10m_proc put check for INFILE=BIN to determine which 
!                  READ_AREA to call.

        use kasaomega_def
	use wcamera_def		!Used in _mpi version only(for Michelle)
	use record_defs
	use w10m_subs
        use w10m_pepulse_subs
        
        IMPLICIT NONE

	type(sum_rec) :: srec
	type(camera)  :: wptr
	type(camera_ptrs) :: pptr

 	integer :: nwords,i,j,k,m,n,error,isize
	real,dimension(60000),target :: disc_cm
					    !Lots of extra space here(I hope!)
	logical :: idone
	real,pointer,dimension(:) ::disc
        integer :: icount=0

!Now  reconstitue summary  record.
	idone=.false.
				!Read an area from the input file using Pete's
				!trick,until eof reached.
        disc_cm=0
        if(index(options,'CM')==0)then
           read(8,end=533)nwords,mrec, (disc_cm(i),i=1,nwords-mrec_size)
        else
           read(8,end=533)nwords,srec,(disc_cm(i),i=1,nwords-srec_size)
           mrec=srec%mrec			 		  !vector arith
           mhead%pe_head%segment_head%tep=srec%tep		!?
           mhead%pe_head%segment_head%idfile=srec%idfile
           mhead%pe_head%x_offset=srec%x_offset
           mhead%pe_head%y_offset=srec%y_offset
        endif
        icount=icount+1
        !UnCompress the disc array.
			!Check to see if we have timing data.
	if(index(options,'TIM')/=0)then
		call w10m_unpack(disc_cm,disc,wptr,pptr)
	else
				!Skip word count.
		call w10m_uncompress(disc_cm(2:),disc,wptr%npmt)
	endif
        return
533	idone=.true.
        print*,'Input file ends.',icount,' events read in.'
        print*,ihillas_out,' events written to output file'
 	return
    END SUBROUTINE CMSUM_READ_AREA
!******************************************************************************

     SUBROUTINE CMSUM_HDF5_READ_AREA(srec,disc,wptr,pptr,idone)
!*****************************************************************************
!	Read in an event from the HDF5  summary file.
!*****************************************************************************
!
!    Modified:
!	30/10/00 GHS V:1:1:6:1:2:3.7
!               Add stuff for HDF5 format input of the cm file. Note that
!               the HDF5 format for the cmsum file is the same as that for
!               the HDF5 format of the M files. The header is in a seperate
!               dataset. The disc and time arrays are varaiable length arrays
!               within compound data structures and tep,idfile,x_offset and 
!               y_offset are in the
!               mrec record. To retrieve this data we use the standardized
!               C routines mhdf5_open, mhdf5_in, etc. from kashdf5.c  The same 
!               routines as used in kassum_hdf5 to make the cmsum file. 
!
!               *******For single proc only at first.(not in mpi code).******
!
!               1: New OPTION: INFILE=BIN. Chosing this option gives us the old
!                  unformatted file input format. Default is now for HDF5 
!                  input format.
!               2. Move the unpacking stuff for the binary records into 
!                  CMSUM_READ_AREA. Create a new routine CMSUM_HDF5_READ_AREA.
!                  In w10m_proc put check for INFILE=BIN to determine which 
!                  READ_AREA to call.
!Modified:
!      01/03/04 GHS V:1:3:8:3:4:4.12
!               in kasaomega_sngle:CMSUM_HDF5_READ_AREA put in code to see 
!               catch when the # pes exceeds itime_size which is size of time 
!               array. Extend the itime_size to 150000 pes.



	use structures
	use kasaomega_def
!        use vms_linux
	use wcamera_def	
	use record_defs
	IMPLICIT NONE

	type(sum_rec) :: srec
	type(camera)  :: wptr
	type(camera_ptrs) :: pptr

 	integer :: i,j,k,ierr,jpe,error
	logical :: idone,ifirst=.true.
	real,pointer,dimension(:) ::disc

	real,pointer,dimension(:) ::time
        integer :: icount=0             !This will be used as a 'C' array index
                                        !in mhdf5_in.Tus it starts at 0.
        integer :: ibad=0
        integer :: isize,jtime_size
        integer,parameter :: itime_size=150000
        if(ifirst)then
           deallocate(time,stat=error)
           allocate(time(itime_size),stat=error)
           if(error/= 0)then
              print*,"Program could not allocate space for time."
              stop
           endif
           ifirst=.false.
        endif

!Now  reconstitue summary  record.
	idone=.false.
				!Read an event from the HDF5 input file
        do
           call mhdf5_in(icount,srec%tep,srec%idfile,srec%x_offset, &
             & srec%y_offset,srec%mrec,disc,time,cmsum_in,ierr,wptr%npmt)
           if(ierr.ge.0)then
                        !test that we haven't exceeeded the size of time array.
              jtime_size=0
              do i=1,wptr%npmt
                 jtime_size=jtime_size+disc(i)
              enddo
              if(jtime_size>itime_size)then
                 print*,'Fatal--kasaomega_sngl:CMSUM_HDF5_READ_AREA', &
                      & '--# pes in event:',jtime_size, &
                      & ', exceedes size of TIME array:',itime_size
                 stop 'Number of pes exceeds size of TIME array'
              else
                 exit    !Good event, process it
              endif
           elseif(ierr.eq.-4)then
              ibad=ibad+1
              if(ibad>10)then
                 call mhdf5_mrec_size(cmsum_in, isize)
                 print*,'FATAL-CMSUM_HDF5_READ_AREA-Read of Input HDF5 file &
                   &Failed at index:',icount
                 print*,'Total number of events in input HDF5 file:',isize
                 stop 'Too many reads failed on input HDF5 file'
              else
                 print*,'WARNING-CMSUM_HDF5_READ_AREA-Read of Input HDF5 file &
                   &Failed at index:',icount
                 print*,'        Retry read of event.'
              endif
              cycle            !Try to reread event
           elseif(ierr.eq.-3)then
              print*,'FATAL-CMSUM_HDF5_READ_AREA-Illegal Value for recnum &
                   &reqested(<0):icount:',icount
              stop 'Illegal icount reqested from input HDF5 file(<0)'
           elseif(ierr.eq.-2)then
              print*,'FATAL-CMSUM_HDF5_READ_AREA--MREC dataset does not exist &
                   &in input HDF5 file.'
              stop 'Bad input HDF5 file'
           elseif(ierr.eq.-1)then
              idone=.true.
              print*,'Input file ends.',icount,' events read in.'
              print*,ihillas_out,' events written to output file'
              return                   !end of file found
           endif
        enddo

        icount=icount+1  !Bump count for next record.

  	mrec=srec%mrec			 		   !vector arith

        mhead%pe_head%segment_head%tep=srec%tep		!?
	mhead%pe_head%segment_head%idfile=srec%idfile
	mhead%pe_head%x_offset=srec%x_offset
	mhead%pe_head%y_offset=srec%y_offset

				!Allocate the pes timing arrays
				!Only 1 ithphi at a time now.
           jpe=sum(disc) 		!total number of pes.
           pptr%pes_in_image(1)=jpe
        
           deallocate(pptr%time,stat=error)
           allocate(pptr%time(jpe,1),stat=error)
           if(error/= 0)then
         print*,"Program could not allocte space for p",wptr%camera_id,"time."
              stop
           endif

           deallocate(pptr%time_pixel,stat=error)
           allocate(pptr%time_pixel(jpe,1),stat=error)
           if(error/= 0)then
    print*,"Program could not allocte space for p",wptr%camera_id,"time_pixel."
              stop
           endif
				!load up time_pixel array.
				!times of Pes are packed in order of pixels.
           j=0
           do i=1,wptr%npmt
		if(disc(i)/=0)then
			do k=1,disc(i)
				j=j+1
				pptr%time_pixel(j,1)=i
                                pptr%time(j,1)=time(j)
                             enddo
		endif
	enddo
	if(j/=jpe)then
		print*,' CMSUM_HDF5_READ_AREA--fatal-Miscount of pes.'
		stop 'PE count verify failed in CMSUM_HDF5_READ_AREA'
	endif
        return
    END SUBROUTINE CMSUM_HDF5_READ_AREA
!******************************************************************************
 
   SUBROUTINE HILLAS_WRITE(hntp)
!*****************************************************************************
!  Write the HNTP record to the output binary file(single processor version)
!*****************************************************************************
	use kasaomega_def

	IMPLICIT NONE
	type(h_ntp) :: hntp
	write(4)hntp
        ihillas_out=ihillas_out+1
	return
   END SUBROUTINE HILLAS_WRITE
!*****************************************************************************

    SUBROUTINE LAURA(wptr,pptr)
!******************************************************************************
!	This is dummy. Never call laura from non mpi
!******************************************************************************
	use wcamera_def

	IMPLICIT NONE

        integer :: i,event_count=0
        type(camera) :: wptr
        type(camera_ptrs) :: pptr

        print*,' Laura called from non- mpi program. Thats fatal!'
        stop
    END SUBROUTINE LAURA
!**************************************************************************

    SUBROUTINE NEWTRIG(icount)
!**************************************************************************
!	Single processor version just counts dropped event
!**************************************************************************
	IMPLICIT NONE
	integer*4 icount
	icount=icount+1
	return
    END SUBROUTINE NEWTRIG
!*****************************************************************************

    SUBROUTINE VHDF5_OPEN
!*****************************************************************************
!    This is a wrapper for a call to the C++ routine VHDF5Open(in file
!    Veritas.cpp). Its task is to open and prepare for writting to the 
!    VHDF5RunData format ouput file.
!*****************************************************************************
!    This works for both Whipple and VERITAS. GHS 08/03/04
!*****************************************************************************
!Set up case for name for call to C++ routine.
!dir$ name(vhdf5open="VHDF5Open") 
      use kasaomega_def              !Has 'options'
      use kasaomega_command_line
      use wcamera_pst_def

      IMPLICIT NONE
      integer tele_flag

      if(index(detector,'W490')/=0)then
         tele_flag=1
      elseif(index(detector,'V499')/=0)then
         tele_flag=2
      else
         tele_flag=0
      endif

      call VHDF5Open(trim(vhdf5_output_file)//char(0),tele_flag)
      return
    END SUBROUTINE  VHDF5_OPEN 
!******************************************************************************

   SUBROUTINE VHDF5_PEDS_OUT(wptr,pptr,prototypev499,detectorV499)
!*****************************************************************************
!    This is a wrapper for a call to the C++ routine VHDF5PEDSOut(in file
!    Veritas.cpp). Its task is to Make the PEDS dataset in the VHDF5 ouput 
!    file saveing pedvars,peds,pirpedvars,gains,tubesoff.
!*****************************************************************************
!    This call will work for both Whipple and Veritas data. GHS 08/03/04
!    Only thing of wquestion was the pedestals and we can use what 
!    VHDF5PEDSOUT provides without change.
!*****************************************************************************

!Modified:
! 04/02/05 GHS V:1:3:8:3:4:4.21
!                Replace 4.25 with 'hiclean' when calculating pedvars.

!Set up case for name for call to C++ routine.
!dir$ name(vhdf5pedsout="VHDF5PEDSOut") 
      use wcamera_def
      use whipple_telescope


      IMPLICIT NONE
      type(camera) :: wptr
      type(camera_ptrs) :: pptr
      real*4,pointer,dimension(:) :: pedvars,pairpedvars,peds,gains,tubesoff
      integer :: error
      logical :: prototypev499,detectorv499
      integer :: flag
      integer :: maxNumberVHDF5Channels=499

      deallocate(pedvars,stat=error)
      allocate(pedvars(maxNumberVHDF5Channels),stat=error)
      if(error/= 0)then
         print*,"Program  could not allocte space for pedvars"
         stop
      endif
      deallocate(pairpedvars,stat=error)
      allocate(pairpedvars(maxNumberVHDF5Channels),stat=error)
      if(error/= 0)then
         print*,"Program  could not allocte space for pairpedvars"
         stop
      endif
      deallocate(peds,stat=error)
      allocate(peds(maxNumberVHDF5Channels),stat=error)
      if(error/= 0)then
         print*,"Program  could not allocte space for peds"
         stop
      endif
      deallocate(gains,stat=error)
      allocate(gains(maxNumberVHDF5Channels),stat=error)
      if(error/= 0)then
         print*,"Program could not allocte space for gains"
         stop
      endif
      deallocate(tubesoff,stat=error)
      allocate(tubesoff(maxNumberVHDF5Channels),stat=error)
      if(error/= 0)then
         print*,"Program  could not allocte space for &
              &tubesoff"
         stop
      endif
           
      pedvars=pptr%high/hiclean  !Use for pairpedvars also.  !Vector arith
      peds=0. !dummy here. filled in in VHDF5PEDSOut.     !Vector arith
      pairpedvars=pedvars                                 !Vector arith
      gains=1.0     !gains already corrected for          !Vector arith
      tubesoff=0.0  !Dead PMT adc already zeroed.         !Vector arith

      if(prototypev499)then !Basically I'm not sure how f90 logicals and C++
                            !bool are related.
         flag=1
      elseif(detectorv499)then
         flag=2
      else
         flag=0
      endif
 
      call vhdf5pedsout(pedvars(1),peds(1),pairpedvars(1),gains(1), &
           & tubesoff(1),flag)
      return

    END SUBROUTINE  VHDF5_PEDS_OUT

!******************************************************************************
 
   SUBROUTINE VERITAS_VHDF5_OUT(event_number,wptr,pptr,prototypev499)
!******************************************************************************
!    This is a wrapper for a call to the C++ routine VHDF5Out (whose 
!    task is to write out a VHDF5RunData VArrayEvent 
!    record out)
!******************************************************************************
!This is specific to VERITAS simulations
!Set up case for name for call to C++ routine.
!dir$ name(vhdf5out="VHDF5Out") 
      use wcamera_def
      use kasaomega_def
      use wcamera_pst_def
      IMPLICIT NONE


      type(camera) :: wptr
      type(camera_ptrs) :: pptr
      integer event_number
      logical :: prototypev499
      integer :: prototypeflag

      if(prototypev499)then !Basically I'm not sure how f90 logicals and C++
                            !bool are related.
         prototypeflag=1
      else
         prototypeflag=0
      endif

                        !Call the C++ routine that does this.
      call VHDF5Out(event_number,patch_trigger_pattern(1),patches_pst,&
             & wptr%npmt, pptr%time_trig(1), prototypeflag)
!VHDF5Out returns the event_number of this event(Its the index in the VHDF5 
!file)
      return
    END SUBROUTINE VERITAS_VHDF5_OUT

    SUBROUTINE VERITAS_VHDF5_MC_OUT(event_number,h,weight)
!******************************************************************************
!    This is a wrapper for a call to the C++ routine VHDF5KascadeMCOut (whose 
!    task is to write out a VHDF5RunData KASCADE Monte-carlo event 
!    record out at index event_number)
!******************************************************************************
!Modified:
! 20/10/04 GHS Add ithphi.

!Set up case for name for call to C++ routine.
!dir$ name(vhdf5kascademcout="VHDF5KascadeMCOut") 
      use structures
      IMPLICIT NONE
      type(h_ntp) :: h
      integer event_number
      real*4 :: weight

                              !Call the C++ routine that does this.
      call VHDF5KascadeMCOut(event_number, h%tep, h%type, h%id, h%x_offset, &
           & h%y_offset, h%x, h%y, h%theta, h%phi, h%ithphi, h%azwidth, &
           & h%width, &
           & h%length,h%dist, h%miss, h%alpha, h%size, h%asym, h%max_p(9), &
           & h%bright, h%boundry, h%xmean, h%ymean, h%alph, h%sdevxy, h%psi, &
           & h%emission_alt, h%em_alt_sig, h%muon_ratio, h%time_cfd, &
           & h%xmpln, h%ympln, h%emaltmpln, weight, h%xo(1), h%yo(1), h%xo(2),&
           & h%yo(2), h%xk(1), h%yk(1), h%xk(2), h%yk(2),h%ra2d(1), &
           & h%dec2d(1), h%ra2d(2), h%dec2d(2))
      return
    END SUBROUTINE VERITAS_VHDF5_MC_OUT


    SUBROUTINE VERITAS_VHDF5_OUT_CLEANUP
!******************************************************************************
!    This is a wrapper for a call to the C++ rouitne VHDF5KascadeCleanup. Its
!    task is to cleanup(close file, delete things)
!    after a VHDF5 with tags file is finished being written out,
!******************************************************************************
!Set up case for name for call to C++ routine.
!dir$ name(vhdf5kascadeoutcleanup="VHDF5KascadeOutCleanup") 
      IMPLICIT NONE
                          !Call the C++ routine that does this.
      call VHDF5KascadeOutCleanup()
      return
    END SUBROUTINE VERITAS_VHDF5_OUT_CLEANUP
!***********************************************************************

    SUBROUTINE W10M_DISPLAY(wptr,pptr,hntp,adc,mrec)
!*************************************************************************
!  This is a wrapper to a call to the C++ routine DisplayW10m
!*************************************************************************
!Modified:

!      20/06/02 GHS:1:1:6:1:2:4.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               We use display_dat_file here.



!dir$ name(displayw10m="DisplayW10m") 
      use wcamera_def
      use kasaomega_def
      use kasaomega_command_line
      
      IMPLICIT NONE

      type(camera) :: wptr
      type(camera_ptrs) :: pptr
      real*4,pointer,dimension(:) :: adc      
      type(h_ntp) :: hntp
      type(m_rec) :: mrec
      integer id,type,nx,ny,theta,phi
      logical :: first_call=.true.
      real*4,pointer,dimension(:) :: pedvars,peds,gains,tubesoff,dispadc
      integer :: error
      integer :: iadc_ped=20


!All hntp values are real. convert the necessary ones to integer.
  
      id=hntp%id
      type=hntp%type
      nx=mrec%nx
      ny=mrec%ny
      theta=mrec%itheta
      phi=mrec%iphi

      if(first_call)then
         deallocate(dispadc,stat=error)
         allocate(dispadc(wptr%npmt),stat=error)
         if(error/= 0)then
            print*,"Program (W10m_Dispaly) could not allocte space for dispadc"
            stop
         endif
         deallocate(pedvars,stat=error)
         allocate(pedvars(wptr%npmt),stat=error)
         if(error/= 0)then
            print*,"Program (W10m_Dispaly) could not allocte space for pedvars"
            stop
         endif
           deallocate(peds,stat=error)
         allocate(peds(wptr%npmt),stat=error)
         if(error/= 0)then
            print*,"Program (W10m_Dispaly) could not allocte space for peds"
            stop
         endif
           deallocate(gains,stat=error)
         allocate(gains(wptr%npmt),stat=error)
         if(error/= 0)then
            print*,"Program (W10m_Dispaly) could not allocte space for gains"
            stop
         endif
           deallocate(tubesoff,stat=error)
         allocate(tubesoff(wptr%npmt),stat=error)
         if(error/= 0)then
            print*,"Program (W10m_Dispaly) could not allocte space for &
                 &tubesoff"
            stop
         endif
           
         pedvars=pptr%high/4.25                              !Vector arith
         peds=iadc_ped                                      !Vector arith
         gains=1       !gains already corrected for          !Vector arith
         tubesoff=0    !Dead PMT adc already zeroed.         !Vector arith

         call DisplayW10m(wptr%npmt, pedvars(1), hntp%tep, id, type, nx, ny,&
              & theta, phi, hntp%x, hntp%y, hntp%emission_alt, hntp%azwidth,&
              & hntp%width, hntp%length, hntp%dist, hntp%miss, hntp%alpha,  &
              & hntp%size, hntp%xmean, hntp%ymean, hntp%alph, hntp%sdevxy,  &
              & hntp%asym, trim(display_dat_file)//char(0), mrec%muon_ratio)
         call DisplayW10m(wptr%npmt, peds(1), hntp%tep, id, type, nx, ny,&
              & theta, phi, hntp%x, hntp%y, hntp%emission_alt, hntp%azwidth,&
              & hntp%width, hntp%length, hntp%dist, hntp%miss, hntp%alpha,  &
              & hntp%size, hntp%xmean, hntp%ymean, hntp%alph, hntp%sdevxy,  &
              & hntp%asym, trim(display_dat_file)//char(0), mrec%muon_ratio)
         !No padding yet. Use on-source pedvars for pairpedvars
         call DisplayW10m(wptr%npmt, pedvars(1), hntp%tep, id, type, nx, ny,&
              & theta, phi, hntp%x, hntp%y, hntp%emission_alt, hntp%azwidth,&
              & hntp%width, hntp%length, hntp%dist, hntp%miss, hntp%alpha,  &
              & hntp%size, hntp%xmean, hntp%ymean, hntp%alph, hntp%sdevxy,  &
              & hntp%asym, trim(display_dat_file)//char(0), mrec%muon_ratio)
         call DisplayW10m(wptr%npmt, gains(1), hntp%tep, id, type, nx, ny,&
              & theta, phi, hntp%x, hntp%y, hntp%emission_alt, hntp%azwidth,&
              & hntp%width, hntp%length, hntp%dist, hntp%miss, hntp%alpha,  &
              & hntp%size, hntp%xmean, hntp%ymean, hntp%alph, hntp%sdevxy,  &
              & hntp%asym, trim(display_dat_file)//char(0), mrec%muon_ratio)
         call DisplayW10m(wptr%npmt, tubesoff(1),hntp%tep, id, type, nx, ny,&
              & theta, phi, hntp%x, hntp%y, hntp%emission_alt, hntp%azwidth,&
              & hntp%width, hntp%length, hntp%dist, hntp%miss, hntp%alpha,  &
              & hntp%size, hntp%xmean, hntp%ymean, hntp%alph, hntp%sdevxy,  &
              & hntp%asym, trim(display_dat_file)//char(0), mrec%muon_ratio)
         first_call=.false.
         deallocate(pedvars,stat=error)
      endif
!Add in pedestal(we took it out before cleanup)

      dispadc=adc+iadc_ped                              !vector arith

      if(index(detector,'V499')==0)then
!   12/05/03 GHS VERITAS Camera has no limits on ADC.(actually it does but
!                it is imposed in VPulse::Fadc)and applies its own pedc.
!Clip to range of Lecroy adc's !vector arith
         where (dispadc>=1024)
            dispadc=1023
         endwhere
      endif

      where (dispadc<0)
         dispadc=1 
      endwhere

      call DisplayW10m(wptr%npmt, dispadc(1), hntp%tep, id, type, nx, ny,&
           & theta, phi, hntp%x, hntp%y, hntp%emission_alt, hntp%azwidth,&
           & hntp%width, hntp%length, hntp%dist, hntp%miss, hntp%alpha,  &
           & hntp%size, hntp%xmean, hntp%ymean, hntp%alph, hntp%sdevxy,  &
           & hntp%asym, trim(display_dat_file)//char(0), mrec%muon_ratio)
      return
    END SUBROUTINE W10M_DISPLAY
!*************************************************************************

   SUBROUTINE WHIPPLE_VHDF5_OUT(event_number,wptr,pptr,adc,ped_number)
!******************************************************************************
!    This is a wrapper for a call to the C++ routine WhippleVHDF5Out (whose 
!    task is to write out a VHDF5RunData VArrayEvent 
!    record out)
!******************************************************************************
!Modified:
! 04/02/05 GHS V:1:3:8:3:4:4.21
!                Set up to generate pedestal events when we write out the
!                VHDF5 file for whipple events (do same for veritas event 
!                latter)
!                In this routine determine (once only) the pedvars used in the
!                run and pass a pointer to them in the call to  the C++ 
!                routine: WhippleVHDF5Out in file 'Veritas.cpp'. Also pass a
!                pointer to a logical variable enable_ped_events that enables 
!                the generation of the pedestal events in WhippleVHDF5Out.

!This is specific to WHIPPLE simulations
!Set up case for name for call to C++ routine.

!dir$ name(whipplevhdf5out="WhippleVHDF5Out") 

      use wcamera_def
      use kasaomega_def
      use wcamera_pst_def
      use whipple_telescope
      IMPLICIT NONE


      type(camera) :: wptr
      type(camera_ptrs) :: pptr
      real,dimension(:) :: adc
      integer :: event_number,ped_number,error
      logical :: enable_ped_events=.true.
      logical :: first_time=.true.
      real*4 ,pointer,dimension(:) :: pedvars 

!If first time through, generate the pedvars. Use the pptr%high and hiclean.
!(thus this includes padding!!!!!)
      if(first_time)then
         allocate(pedvars(size(pptr%high)),stat=error)
         if(error/= 0)then
            print*,"WHIPPLE_VHDF%_OUT could not allocte space for pedvars"
            stop
         endif
         pedvars=pptr%high/hiclean                           !vector arith
         first_time=.false.
      endif

!Call the C++ routine that does this.
!Forget about patches, forget about FADC

      call WhippleVHDF5Out(event_number, wptr%npmt, adc(1),enable_ped_events, &
           & pedvars(1), ped_number)

!WhippleVHDF5Out returns the event_number of this event(Its the index in the 
!VHDF5 file) Index my change by 2 if we have added a pedestal event before this
! event.

      return
    END SUBROUTINE WHIPPLE_VHDF5_OUT

END MODULE KASAOMEGA_SUB2
