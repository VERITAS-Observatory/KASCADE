
MODULE M2HDF5_SUBS
  
  logical :: mfile
  logical :: convert=.true.
  logical :: verify=.true.
  integer :: npixels
  character(LEN=80) :: mfile_in_file 
  character(LEN=80) :: mfile_out_hdf5_file 

CONTAINS

    SUBROUTINE FILEOPEN(tep,idfile,x_offset,y_offset)
!******************************************************************************
!	Open the binary M file and read in its header.
!       Create the output HDF file, Read in the header record, Define a 
!       datatype for the header,define a dataset for the header, write out the
!       header to the HDF file, define the compound datatype for the pes 
!       records, define the pes dataset
!******************************************************************************

!	Modified:

	use structures
	IMPLICIT NONE

	integer :: pt,ios,error,idfile
 	real :: dni,area,dn,tep,x_offset,y_offset
        type(m_head),pointer :: mhead

	character(len=12),dimension(9) ::  ptype=(/ &           !PMT/Grid type
	&' Visable    ',' Old Luny   ',' Solar Blind',' CEASAR     ', &
	&' ASGAT      ',' Part-on-gnd','Whipple     ',' VERITAS    ', &
	&'VERITAS-TRI '/)
	character(len=8),dimension(18) :: namtyp=(/ &	!Particle names.
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


      allocate(mhead,stat=error)
	if(error/= 0)then
		print*,"Program could not allocte space for mhead."
	    	stop
	endif

           OPEN(8,ACCESS='sequential',STATUS='old',FORM='UNFORMATTED', &
		& file=trim(mfile_in_file),iostat=ios)
           if(ios>0)then
              print*,' Failure opening input mfile in Fileopen', &
                   & trim(mfile_in_file)
              stop 'M2HDF5 Input mfile open failure'
           endif

           !	Read in the mfile header.
           read(8)mhead

		!Load up some other values.
           dni=sqrt(1.-mhead%pe_head%segment_head%dli**2-   &
                &  mhead%pe_head%segment_head%dmi**2)
		!Mount is always looking up.
           dn=-sqrt(1-mhead%pe_head%dl**2-mhead%pe_head%dm**2)

!Get shower id stuff to send back.
           tep= mhead%pe_head%segment_head%tep
           idfile=mhead%pe_head%segment_head%idfile
           x_offset=mhead%pe_head%x_offset
           y_offset=mhead%pe_head%y_offset

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
              write(6,1000)mhead%pe_head%petype
1000	format(' M2HDF5_FATAL--Illegal photon type:petype=',a)
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
	& mhead%pe_head%version,mhead%version


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
	& '      From KASTRIGGER Version: ',a)
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


!	open the output HDF file and write out the header summary file.

!These are C routines.
       call mhdf5_make_types   !Inialize the all the different 'types'
       print*,' Creating HDF5 file'
       call mhdf5_create(trim(mfile_out_hdf5_file)//char(0),mfile)

!       Write out the pehead record into its own dataset.
       print*,' Writing out pe_head'
       call mhdf5_mhead_write(mhead,mfile)
       if(index(mhead%options,'W490')/=0)then
          npixels=490
       elseif(index(mhead%options,'V499')/=0)then
          npixels=499
       else
          print*,' Unable to decode number of pixels in camera'
          stop 'Failure to decode number of pixels in camera'
       endif
       print*,' Number of pixels in camera:',npixels

	return
    END SUBROUTINE FILEOPEN
!******************************************************************************

    SUBROUTINE PROCESS(tep,idfile,x_offset,y_offset)
!******************************************************************************
!	This routine_ procesess the m files. 
!	All events are added to the output dataset file.
!******************************************************************************

!	Modified:

      use structures

	IMPLICIT NONE

      real,dimension(:),pointer :: disc_cm,disc,time,disc_old,time_old

      integer :: icount,error,i,j,k,ierr,pt,nwords,ios,idfile
      real :: dni,area,dn,tep,x_offset,y_offset
      type(m_rec),pointer ::  mrec,mrec_old
      type(m_head),pointer :: mhead,mhead_old
      logical :: ldisc,ltime


	character(len=12),dimension(9) ::  ptype=(/ &           !PMT/Grid type
	&' Visable    ',' Old Luny   ',' Solar Blind',' CEASAR     ', &
	&' ASGAT      ',' Part-on-gnd','Whipple     ',' VERITAS    ', &
	&'VERITAS-TRI '/)
	character(len=8),dimension(18) :: namtyp=(/ &	!Particle names.
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


      allocate(mrec,stat=error)
	if(error/= 0)then
		print*,"Program could not allocte space for pe."
	    	stop
	endif
      allocate(disc_cm(60000),stat=error)
	if(error/= 0)then
		print*,"Program could not allocte space for disc."
	    	stop
	endif



	if(convert)then

   	deallocate(disc,stat=error)
   	allocate(disc(npixels),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for DISC."
	     stop
	endif

           do
!	   print*,' Read in Mrec record'
              read(8,end=533)nwords,mrec, (disc_cm(i),i=1,nwords-mrec_size)
              icount=icount+1
!debug
!           if(icount<10)then
!              write(6,1003)icount,tep,idfile,mrec
1003 format(' i:',i6,f6.3,i5,4i5,2f9.2,f5.2,4e14.7,4f5.1)
!
!           endif
!enddebug

                         !unpack packed array
              call m_unpack(disc_cm,disc,time,mrec%time_cfd)


!debug
!           if(icount<10)then
!              write(6,1004)disc
1004          format( 10f7.0)
!              write(6,1005)(time(i),i=1,sum(disc))
1005          format(8f9.3)
!           endif
!enddebug

!And write it out. Mrec event # start at 0.(i.e. index=icount-1)
!           print*,' Write out mrec record:',icount-1

              call mhdf5_out(icount-1,tep,idfile,x_offset,y_offset,mrec, &
                   & disc,time,mfile,npixels)
           enddo

!	End of shower.
!	write out the number of m records found and close it up
533        print*,' End of shower found.',icount,' events sent to HDF file.'

!debug
!        Print*,' Last m record:'
!              write(6,1003)icount,tep,idfile,mrec
!              write(6,1004)disc
!              write(6,1005)(time(i),i=1,sum(disc))
!enddebug


           close(unit=8)
           call mhdf5_close(mfile)
           print*,' New HDF file closed'
           call mhdf5_close_types
        endif


        if(verify)then

!DEBUG and verify code ****************************************************
!
!**************************************************************************
!Now reopen the HDF5 file. Also reopen thew M file. Verify all.
!**************************************************************************
           print*,' Verifying files'
           allocate(mhead,stat=error)
           if(error/= 0)then
              print*,"Program could not allocte space for mhead."
              stop
           endif
      
           allocate(mhead_old,stat=error)
           if(error/= 0)then
              print*,"Program could not allocte space for mhead_old."
              stop
           endif

           OPEN(8,ACCESS='sequential',STATUS='old',FORM='UNFORMATTED', &
                & file=trim(mfile_in_file),iostat=ios)
           if(ios>0)then
              print*,' Failure opening input file mfile in Process:',&
                   & trim(mfile_in_file)
              stop 'M2HDF5 input mfile open failure in Process'
           endif

!	Read in the mfile header.
           read(8)mhead_old

!Open HDF% Mfile.
          print*,' Reopen mfile.hdf'
          call mhdf5_open(trim(mfile_out_hdf5_file)//char(0),mfile,ierr)
          if(ierr<0)then
             print*,' MHDF5_open failure for output mfile:', &
                  &  trim(mfile_out_hdf5_file)
             stop  'File open failure'
          endif
          call mhdf5_make_types

          print*,' Read in the header'
          call mhdf5_mhead_read(mhead,mfile)
          print*,' Header read in.'
 

!Load up some other values.
          dni=sqrt(1.-mhead%pe_head%segment_head%dli**2-   &
               &  mhead%pe_head%segment_head%dmi**2)
	!Mount is always looking up.
          dn=-sqrt(1-mhead%pe_head%dl**2-mhead%pe_head%dm**2)
!
!
!
          area=mhead%pe_head%xseg*mhead%pe_head%yseg
!
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
1000         format(' M2HDF5_FATAL--Illegal photon type:petype=',a)
             stop
          endif
!
!
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

          write(6,1001)ptype(pt),				              &
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
               & mhead%pe_head%version,mhead%version
!
!
1001   format( &
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
            & '      From KASTRIGGER Version: ',a)
          WRITE(6,1002)mhead%pesum,mhead%hmult,mhead%noise,mhead%rate_noise, &
               & mhead%reflectivity,mhead%concentration,             &
               & trim(mhead%options), &
               & mhead%step_size,mhead%phi_steps,mhead%itheta_max,    &
               & mhead%disc_width,mhead%adc_gate
1002      format( ' Original Trigger parameters:',/,    &
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
	 & '                       Original ADC gate width (ns) = ',f10.4)!

          if(index(mhead%options,'W490')/=0)then
             npixels=490
          elseif(index(mhead%options,'V499')/=0)then
             npixels=499
          else
            print*,' Unable to decode number of pixels in camera'
             stop 'Failure to decode number of pixels in camera'
          endif
          print*,' Number of pixels in camera:',npixels

   	deallocate(disc,stat=error)
   	allocate(disc(npixels),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for DISC."
	     stop
	endif


          allocate(mrec_old,stat=error)
          if(error/= 0)then
             print*,"Program could not allocte space for mrec_old."
             stop
          endif
        
          deallocate(disc_old,stat=error)
          allocate(disc_old(npixels),stat=error)
          if(error/= 0)then
 	     print*,"Program could not allocte space for DISC_old."
	     stop
          endif

          deallocate(time,stat=error)
          allocate(time(60000),stat=error)
          if(error/= 0)then
             print*,"Program could not allocte space for time."
             stop
          endif
!	read a mrec from the HDF file until eof reached.!

          print*,' Read in mrec records'
          i=0
          do
           
             call mhdf5_in(i,tep,idfile,x_offset,y_offset,mrec,disc,time, &
                  & mfile,ierr,npixels)
!           print*,' ierr:',ierr
             if(ierr<0)then
                print*,'No more events at i:',i
                exit
             endif

!Verify stuff form header.
             if(tep/=mhead%pe_head%segment_head%tep)then
                print*,' Verify error for tep:',tep, &
                     mhead%pe_head%segment_head%tep
                stop 'Verify error for TEP'
             endif
             if(idfile/=mhead%pe_head%segment_head%idfile)then
                print*,' Verify error for idfile:',idfile, &
                     mhead%pe_head%segment_head%idfile
                stop 'Verify error for IDFILE'
             endif
             if(x_offset/=mhead%pe_head%x_offset)then
                print*,' Verify error for x_offset:',x_offset, &
                     mhead%pe_head%x_offset
                stop 'Verify error for X_OFFSET'
             endif
             if(y_offset/=mhead%pe_head%y_offset)then
                print*,' Verify error for y_offset:',y_offset, &
                     mhead%pe_head%y_offset
                stop 'Verify error for Y_OFFSET'
             endif




             read(8,end=534)nwords,mrec_old, (disc_cm(j),j=1,nwords-mrec_size)

!  Unpack packed array
             call m_unpack(disc_cm,disc_old,time_old,mrec_old%time_cfd)
             do j=1,npixels
                if(disc(j)/=disc_old(j))then
                   print*,'Verify error for disc.'
                   write(6,2000)'old disc at i:',i
                   write(6,1004)(disc_old(k),k=1,npixels)
                   write(6,2000)'new disc at i:',i
                   write(6,1004)(disc(k),k=1,npixels)
                    stop 'Disc verify error'
                endif
             enddo

             do j=1,sum(disc)
!************************************************************************
!Fake packing and unpacking roundoff.
!This is for  when the input file was made made directly by kastrigger.
!If made by this routine the pack/unpack sequence is already done.
!                k=int((time(j)-mrec%time_cfd)/.25)
!                time(j)=k*.25+mrec%time_cfd
!************************************************************************
                if(time(j)/=time_old(j))then
                   print*,'Verify error for time.'
                   write(6,2000)'old times at i:',i
2000               format(a,i6)
                   write(6,1005)(time_old(k),k=1,sum(disc))
                   write(6,2000)'new times at i:',i
                   write(6,1005)(time(k),k=1,sum(disc))
                   stop 'Time verify error'
                endif
             enddo

!             if(i+1<10)then
!                write(6,1003)i+1,tep,idfile,mrec
!                write(6,1004)disc
!                write(6,1005)(time(j),j=1,sum(disc))
!             endif

             i=i+1
         enddo
!
!	End of shower.
!	write out the last mrec record found and close it up
534     print*,' End of shower found.',i,' events sent to HDF file.'
         print*,' Verify successful file id:', &
              & mhead%pe_head%segment_head%idfile
!        Print*,' Last mrec record:'
!        write(6,1003)i,tep,idfile,mrec
!        write(6,1004)disc
!        write(6,1005)(time(i),i=1,sum(disc))
        
        call mhdf5_close(mfile)
        call mhdf5_close_types
        close(unit=8)
   endif
   return
!!END DEBUG***************************************************************

    END SUBROUTINE PROCESS
!******************************************************************************

    SUBROUTINE M_UNCOMPRESS(disc_in,disc_out,npmts)
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
    END SUBROUTINE M_UNCOMPRESS
!******************************************************************************

!*******************************************************************************
    SUBROUTINE M_UNPACK(disc_cm,disc,time,time_cfd)
!*******************************************************************************
!	Unpack compressed disc array and pe time arrays. Fill up time
!	and disc.
!*******************************************************************************

	IMPLICIT NONE

	real,pointer,dimension(:) :: time
	real,target,dimension(:) :: disc_cm
	real,pointer,dimension(:) :: disc
        real*4 :: time_cfd
	integer :: i,j,k,m,n,error,jpe
	integer*2,pointer,dimension(:) :: packedi2,i2ptr
	real,pointer,dimension(:) :: sptr

			!DISC
	m=1  		!Points to number of words in compressed array
	n=disc_cm(m)	!Number of words in compressed array
	j=m+1		!Points to first word of compressed array
	n=m+n		!Points to last word of compressed array
	call m_uncompress(disc_cm(j:n),disc,npixels)
				!TIME_TRIG		

				!Allocate the pes timing arrays
				!Only 1 ithphi at a time now.
	jpe=sum(disc) 		!total number of pes.

	deallocate(time,stat=error)
	allocate(time(jpe),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for time."
	     stop
	endif


	m=n+1		!points to number of words in packed
			!pes timeing array (which is packed 2 i*2 to
			!each r*4 word.)
	k=disc_cm(m)	!Number of words in packed array
	deallocate(packedi2,stat=error)
			!Last work may be 0(ie jpe  may be odd)
	allocate(packedi2(2*k),stat=error)
	if(error/= 0)then
	     print*,"Program could not allocte space for packedi2."
	     stop
	endif

	j=m+1		!Points to first word of packed array
	k=m+k		!Points to last word of packed array
	sptr=>disc_cm(j:k)
		
	packedi2=transfer(sptr,packedi2)
				!convert to real, add in timing.
				!drop filler 0 if its there.
	i2ptr=>packedi2(1:jpe)
	time(1:)=i2ptr*.25+time_cfd		!Vector Arith
	return
    END SUBROUTINE M_UNPACK
END MODULE M2HDF5_SUBS

PROGRAM M2HDF5
!******************************************************************************
!	This program reads in a binary PES file for the whipple
!	Camera and converts it into an hdf5 file.
!******************************************************************************

!	Files:
!	unit=8  'M' input file.


!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: 47474::SEMBROSKI

!	Modified:

!	25/9/00 GHS
!		Converted from kassum_compress.f90

!		In FILEOPEN
!		1:Use 'mfile.dat' as assigned logical name of input binary
!                 M file.
!               2.Use 'mfile.hdf' as assigned logical name of output M
!                 hdf5 file.
!       06/05/03 GHS
!               Move input parameters to command line.
	use m2hdf5_subs
	IMPLICIT NONE
        integer :: i,num_cmd_args
        character(len=80) :: arg_opt
        integer,external :: iargc_
        character(len=8) :: adate
	character(len=10) :: time

	real :: tep,x_offset,y_offset
        integer :: idfile
!******************************************************************************
!       Get the various command line arguments.
!       All command line arguments come in pairs: An option (-p) and 
!                                                 a string (kasoam.par)
!       Defaults are in wcamera_modules.f90
!******************************************************************************
        num_cmd_args=iargc_()
        if(num_cmd_args.gt.0)then
           print*,'M2HDF5--Number of command line arguments:',num_cmd_args
           do i=1,num_cmd_args,2
              call getarg_(i,arg_opt)
              arg_opt=trim(arg_opt)
              if(arg_opt=="-i")then
                 call getarg_(i+1,mfile_in_file)
                 mfile_in_file=trim(mfile_in_file)
                 print*,' -i option gives:',trim(mfile_in_file)
              elseif(arg_opt=="-o")then
                 call getarg_(i+1,mfile_out_hdf5_file)
                 mfile_out_hdf5_file=trim(mfile_out_hdf5_file)
                 print*,' -o option gives:',trim(mfile_out_hdf5_file)
              else
                 print*,' Illegal command line option #:',i,'Option:', &
                      trim(arg_opt)
                 stop 'M2HDF5: Illegal command line option'
              endif
           enddo
        else
           print*,' M2HDF5--No command line arguments.'
           print*,' M2HDF5--Using defaults for all.'
        endif

!******************************************************************************


        if(convert)then                            !Open the input 'M' file.
           call fileopen(tep,idfile,x_offset,y_offset)	
        endif
                                !Create the output HDF file, Read in the header
                                !record, Define a datatype for the header, 
                                !define a dataset for the header, write out the
                                !header to the HDF file.
                                !define the compound datatype for the pes 
                                !records, define the pes dataset
	call process(tep,idfile,x_offset,y_offset)
                                !Read in PES file and write out to HDF5 pes
                                !dataset. Clean up when done.
        stop 'M2HDF5 normal stop'
END PROGRAM M2HDF5
