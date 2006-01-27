MODULE W10M_PEPLS_DEF
!Modified:
!      29/04/03 GHS
!                Add a call to new routine W10M_PEPULSE_INIT which used the 
!                veritas single pepulse clAss to fill the PEPULSE array with a
!                pulse of the requested rise time. This change is for the w10m
!                cameras. This replaces the fixed rise time pulse used 
!                previously.
!        31/03/04 GHS V:1:3:8:3:4:4.17
!                For still more attempts at dirtying up things, change 
!                SINGLE_PE_HEIGHT (W10m_SUBS.F90) to have the width of the
!                single pe pulse height distribution be an input parameter.
!                Defaults to old vaslue (.275). Add PPTR%SINGLE_PE_SIGMAS.
!                Fill it in WCAMERA_GEN. Put in parameters 
!                PULSE_HEIGHT_WIDTH_MEAN and PULSE_HEIGHT_WIDTH_SIGMA in
!                WHIPPLE_TELESCOPE.F90. Also add sigmas to W10M_NOISE_PULSE 
!                argument list


				!pe pulse profile. Max=1. Risetime=2.6 ns.
				!25 ns long
                                !Steps are .25 ns
!	real,dimension(100),target :: pepulse = (/ &
! &0.000, 0.004, 0.011, 0.019, 0.026, 0.033, 0.041, 0.053, 0.068,&
! &0.083, 0.112, 0.153, 0.201, 0.267, 0.347, 0.428, 0.509, 0.589,&
! &0.663, 0.737, 0.811, 0.885, 0.934, 0.967, 0.994, 1.000, 0.993,&
! &0.987, 0.980, 0.973, 0.966, 0.955, 0.941, 0.920, 0.905, 0.890,&
! &0.870, 0.841, 0.809, 0.789, 0.774, 0.759, 0.739, 0.714, 0.688,&
! &0.668, 0.648, 0.628, 0.608, 0.582, 0.554, 0.520, 0.492, 0.465,&
! &0.438, 0.411, 0.388, 0.374, 0.366, 0.355, 0.335, 0.321, 0.309,&
! &0.302, 0.290, 0.279, 0.272, 0.266, 0.256, 0.243, 0.230, 0.219,&
! &0.212, 0.205, 0.196, 0.182, 0.175, 0.165, 0.152, 0.139, 0.128,&
! &0.121, 0.121, 0.118, 0.111, 0.097, 0.086, 0.079, 0.067, 0.055,&
! &0.045, 0.040, 0.037, 0.033, 0.026, 0.019, 0.011, 0.007, 0.003,&
! &0.000/)
	
				!pe pulse profile. Max=1. Risetime=2 ns.
				!16.5 ns long
                                !Steps are .25 ns
        real,pointer,dimension(:) :: pepulse 

!Use the veritas code to fill this array with pulse of requested rise time.
!VSinglePe use this pulse as the base pulse.
!        real,dimension(67),target :: pepulse = (/ &
! &0.0,    .011,  .022,  .033,  .044,  .067,  .089,  .15,  .22,  &
! & .34,   .46,   .58,   .69,   .80,   .91,   .96,  1.0,   .99,  &
! & .98,   .97,   .96,   .94,   .91,   .89,   .86,   .81,  .78,  &
! & .76,   .73,   .69,   .66,   .63,   .60,   .56,   .51,  .47,  &
! & .43,   .39,   .37,   .36,   .33,   .31,   .30,   .28,  .27,  &
! & .26,   .24,   .22,   .21,   .20,   .18,   .17,   .15,  .13,  &
! & .12,   .12,   .11,   .089,  .078,  .060,  .044,  .038, .033, &
! & .022,  .011,  .005, 0.0/)

				!cfd pulses are 10 ns long with 2 ns rise and
				!fall times. max=1.0
	real,dimension(50),target :: cfdpulse =   &
	& (/0.0, .022,  .044, .089, .22, .46, .69, .91, 1.0, 1.0, &
	&   1.0,  1.0,   1.0,  1.0, 1.0, 1.0, 1.0, 1.0,	1.0, 1.0, & 
	&   1.0,  1.0,   1.0,  1.0, 1.0, 1.0, 1.0, 1.0,	1.0, 1.0, & 
	&   1.0,  1.0,   1.0,  1.0, 1.0, 1.0, 1.0, 1.0,	1.0, 1.0, & 
	&   1.0,  1.0,   .91,  .69, .46, .22,.089,.044,.022, 0.0/)


	real,parameter :: cfd_pulse_width=size(cfdpulse)*.25
	real :: threshold
	real,pointer,dimension(:,:) :: pulse,npulse
	real :: time_min,rtime_min

END MODULE W10M_PEPLS_DEF



MODULE W10M_PEPULSE_SUBS

!Modified:
!      29/04/03 GHS V:1:3:8:3:4.2
!                Add a new routine W10M_PEPULSE_INIT which is a wrapper to the
!                C++ routines W10MPepulseSize and W10mPepulseBuild. It creates
!                the single pepulse array (pepulse) with appropriate rise time.
!                Only called for w10m cameras using the TIM option.
!                This replaces the fixed rise time pulse used previously.

	implicit none
	real,external :: gauss,pran		!declare various functions.
	real,external :: rexp
					!CFD parameters.
!	real,parameter :: cfd_delay=(2.0*4)	!Steps in .25 ns for 2 ns delay
!	real,parameter :: cfd_gain=1.05
			!Values of delay and gain(invers fraction) from Andrew
			!Burdett, 17/4/99
	integer,parameter :: cfd_delay=(4.0*4)	!Steps in .25 ns for 2 ns delay
	real,parameter :: cfd_gain=1.51	!(1/.66)This could be wrong. Could be
					!3.33(1/.3)(factory default)
!	real,parameter :: mult_cfd_delay=(4.0*4)  !Steps in .25 ns for 4 ns 
!						  !delay
!	real,parameter :: mult_cfd_gain=1.51	!(1/.66)This could be wrong. 

!	23/4/99 Folowing is from Kevin for the Phipllips CFD used to do the
!	multiplicity discrimination.
	integer,parameter :: mult_cfd_delay=(7.5*4)  !Steps in .25 ns for 4 ns 
						  !delay
	real,parameter :: mult_cfd_gain=3.0	!(1/3 fraction)
!	real,parameter :: mult_cfd_gain=1.5	!(2/3 fraction)


!	Mult fraction .33 delay
!	real,parameter :: mult_adc_delay=-9-7.5	!7.5 ns extra for space.This 
						!is a tunned number.
!	Mult fraction .66 delay
	real,parameter :: mult_adc_delay=-9.5-7.5
						!7.5 ns extra for space.This 
						!is a tunned number.

	real,parameter :: pst_adc_delay=-8	!13ns extra for space.This is
						! a tunned number.

	real,parameter :: adc_delay=mult_adc_delay
	integer,parameter :: adc_delay_steps=adc_delay*4
	logical  :: mult_trig_type=.true.

!	real,parameter :: adc_delay=pst_adc_delay
! 	logical :: mult_trig_type=.false.

	real :: mult_trigger_time,pst_trigger_time
	private

        public :: veritas_fadcpedwidths
        public :: veritas_pepulse_adc
        public :: veritas_pepulse_analyze
	public :: w10m_pepulse_init
	public :: w10m_pepulse_accumulate
	public :: w10m_pepulse_analyze
	public :: w10m_pepulse_pack
	public :: w10m_pepulse_time_pack
	public :: w10m_noise_pulse
	public :: w10m_pepulse_adc
	public :: w10m_unpack
	public :: mult_trig_type 
CONTAINS

   SUBROUTINE VERITAS_FADCPEDWIDTHS(wptr,pptr,ped_widths)
!*****************************************************************************
!       This is a wrapper for the C++ function  VeritasPedWidths:
!	It models the sky noise into the FADC and determins the Pedistal width
!       in the FADC data after converting to charge (see PePulseAdc). For use
!       in deremining high /low sigma values for cleanup.
!       This is VERITAS version using C++ classes and functions.
!*****************************************************************************
!  Written: 12/07/03 GHS

!	Modified:

!       The main task in this routine is setting up calls to the C++ routine
!       to do the work we want. There are a number of arguments that
!       have to be passed (carefully)
!dir$ name(fadcpedwidths="FADCPedWidths") 
	use wcamera_def
	use record_defs
	use structures
	use w10m_pepls_def
        use kasaomega_def
        use w10m_subs
	IMPLICIT NONE

        type(camera) :: wptr
        type(camera_ptrs) :: pptr
	real :: adc_gate
	real,pointer,dimension(:) :: ped_widths

        call FADCPedWidths(wptr%npmt, pptr%mean_time_gap(1),pptr%active_hv(1), &
             & ped_widths(1))
	return
      END SUBROUTINE VERITAS_FADCPEDWIDTHS
!*****************************************************************************

   SUBROUTINE VERITAS_PEPULSE_ADC(wptr,pptr,adc,npmt_trigger)
!*****************************************************************************
!       This is a wrapper for the C++ function  PePulseAdc:
!	It applies the Veritas fadc sampling to the pulse arrays using the 
!       timing to determine the adc values.
!       This is VERITAS version using C++ classes and methods.
!*****************************************************************************
!       modified:
!       08/01/02 GHS V:1:1:6:1:2:3.13
!               This is derived from the W10M_PEPULSE_ADC routine.
!               Cleaned up a bit.

!       The main task in this routine is setting up calls to the C++ routine
!       PePulseAdc to do the work we want.

!Set up case for name for call to C++ routine.
!dir$ name(pepulseadc="PePulseAdc") 
!dir$ name(pepulseprintparameters="PePulsePrintParameters")
!dir$ name(pefadcprint="PeFadcPrint") 
	use wcamera_def
	use structures
	use w10m_pepls_def
	implicit none

        type(camera) :: wptr
        type(camera_ptrs) :: pptr
	real,pointer,dimension(:) :: adc
	integer :: npmt_trigger
	
	real :: trigger_time
        logical :: initialize_adc=.true.

	if(initialize_adc)then
		initialize_adc=.false.
		if(mult_trig_type)then
			write(6,1000)' ADC gate timing set by Multiplicity &
					& trigger'
1000	format(' ',a)
			write(6,1001) ' Multiplcity Trigger fraction:', &
				& 1/mult_cfd_gain
1001	format(' ',a,f10.3)
		else
			write(6,1000)' ADC gate timing set by PST trigger'
		endif
                        !Print out constants used in FADC calculations
               ! print*,'Calling pepulseprintparameters'
                call PePulsePrintParameters()
	endif

        call PePulseAdc(wptr%npmt, npmt_trigger, pptr%active_pmts(1), adc(1), &
             & pptr%mean_time_gap(1), pst_trigger_time)

	return
   END SUBROUTINE VERITAS_PEPULSE_ADC
!*****************************************************************************
   SUBROUTINE VERITAS_PEPULSE_ANALYZE(wptr,pptr,ithphi,npmt_mult_trigger, &
	            &  mult_event_trigger,pst_event_trigger,adc_gate)
!*****************************************************************************
!       This is a wrapper for the C++ function  PePulseVeritas:
!	It accumulates the pulse shape into the pulse arrays.
!	Determines pixel and event trigger times.
!       Determines if we have triggers.
!       This is VERITAS version using C++ classes and functions.
!*****************************************************************************

!	Modified:

!       30/11/01 GHS V:1:1:6:1:2:3.13
!               This is derived from the W10M_PEPULSE_ANALYZE routine.
!               Cleaned up a bit(Remover Laura and Michelle options)

!       The main task in this routine is setting up calls to the C++ routine
!       PePulseVeritas to do the work we want.

!Set up case for name for call to C++ routine.
!dir$ name(pepulseveritas="PePulseVeritas") 
!dir$ name(pepulseprint="PePulsePrint") 

	use wcamera_def
	use record_defs
	use structures
	use w10m_pepls_def
        use kasaomega_def
        use w10m_subs
	IMPLICIT NONE

        type(camera) :: wptr
        type(camera_ptrs) :: pptr
	integer :: ithphi,npmt_mult_trigger
	logical :: mult_event_trigger,pst_event_trigger
	real :: adc_gate

	integer :: ntrigger
	real :: rtime_max,trigger_time
	real,pointer,dimension(:) :: trig_times

!Process the pe into waveforms, cfd trigger times , included Noise as needed.
!PePulseVeritas is a C++ routine.
        !print*,'Before PePulseVeritas'
        call PePulseVeritas(pptr%pes_in_image(ithphi),pptr%time(1,ithphi), &
             & wptr%npmt, pptr%time_pixel(1,ithphi), pptr%active_hv(1), &
             & pptr%disc(1,ithphi),pptr%efficiency(1),pptr%mean_time_gap(1), &
             & pptr%time_trig(1),npmt_mult_trigger,pptr%threshold(1),ntrigger)
        !print*,'After PePulseVeritas'
		

!We have to have at least hmult triggers before we even start!
        pst_event_trigger=.false.
        if(ntrigger>=mhead%hmult)then
           mrec%i_mult=ntrigger	      	!Save for ntupple file.
                
!****************************************************************************
!PST analysis
           if(index(mhead%options,'PST')/=0)then 
                     !If we have limnited ourselves to less then 331 pixels in 
                     !trigger they remaining pixels will have the TDC overflow
                     !value(100000.+) and thus be ignored in w10m_pst_trigger.
              call w10m_pst_trigger(pptr,trigger_time)
              if(trigger_time<100000.) then
                 pst_trigger_time=trigger_time
                 pst_event_trigger=.true.
              else
                 pst_trigger_time=100001.
                 pst_event_trigger=.false.
              endif
						!Save for ntupple file.
              mrec%fp_median_time=pst_trigger_time 
              mult_event_trigger=pst_event_trigger 
                                       !No multiplicity check for Veiritas
           endif

!****************************************************************************

!****************************************************************************
!!Multiplicity trigger: Always do it.
!			!The multiplicity trigger is generated by a cfd
!			!discriminator. So model it as we did the pmt
!			!pulses.
!			!Note if no trigger will get a trigger at the overflow
!			!value of 100000+.
!           trig_times=>pptr%time_trig(1:npmt_mult_trigger)
!           call w10m_mult_trigger(trig_times,mhead%hmult)
!				!Save for ntupple file.
!           mrec%median_time=mult_trigger_time
!           if(mult_trigger_time<100000.)then
!              mult_event_trigger=.true.
!           endif
!****************************************************************************
        endif
        if(index(mhead%options,'PST')==0)then 
           pst_event_trigger=.true.	!If were not using PST
                                        !default it true.
        endif
	return
   END SUBROUTINE VERITAS_PEPULSE_ANALYZE
!*****************************************************************************

   SUBROUTINE W10M_PEPULSE_INIT
!*****************************************************************************
!      Create a pepulse with a specified rise time. Use the VSinglePe class to
!       do it. We use 2 routines (necessary to get around C==/Fortran 
!       interface problems, not real efficient but only need to do once)
!*****************************************************************************
!dir$ name(w10mpepulsesize="W10mPepulseSize") 
!dir$ name(w10mpepulsebuild="W10mPepulseBuild") 

     use w10m_pepls_def

     IMPLICIT NONE

     integer :: error,nbins
     real :: risetime=3.5        !Set risetime of out pulses
                                   !First we need the length of our new array
     call w10mpepulsesize(risetime,nbins)
                               !allocate the new array
     deallocate(pepulse,stat=error)
     allocate(pepulse(nbins),stat=error)
     if(error/= 0)then
        print*,"Program could not allocte space for PEPULSE array."
        stop 'Failure to allocate space for PEPULSE'
     endif
                          !Now fill it.
     call w10mpepulsebuild(pepulse,risetime)
     print*,' Using single pe pulse with a risetime of : ',risetime,' ns'
     return
   END SUBROUTINE W10M_PEPULSE_INIT
!*****************************************************************************

   SUBROUTINE W10M_CFD_TRIGGER(pulsec,cfd_time,threshold,trigger,gain,delay)
!****************************************************************************
!	Find CFD trigger time for pulse in PULSEC
!	CFD_TIME is index within pulse of cfd trigger.
!****************************************************************************
	implicit none


	real,pointer,dimension(:) :: pulsec
	real :: cfd_time,threshold,gain
	integer:: delay
	logical :: trigger

	real,dimension(:),pointer :: main_pulse,dpulse
	real,pointer,dimension(:) :: dptr,mptr
	integer :: i,j,error


	integer :: psize,lsize

		!scheme is to model cfd trigger electronics
!Allocate temp buffer
	psize=size(pulsec)

!Look for a trigger
	trigger=.false.
	do i=1,psize
		if(pulsec(i)>= threshold)then
			trigger=.true.
			exit
		endif
	enddo
	if(.not.trigger)then
		return
	endif

	lsize=2*psize
	deallocate(dpulse,stat=error)
	if(error/= 0)then
           print*,"Program could not deallocate space for DPULSE buffer."
           !stop 'Failure to deallocate space for dpulse'
	endif
        allocate(dpulse(lsize),stat=error)
	if(error/= 0)then
           print*,"Program could not allocte space for DPULSE buffer."
           stop 'Failure to allocate space for dpulse'
	endif
	deallocate(main_pulse,stat=error)
	if(error/= 0)then
	    print*,"Program could not deallocte space for MAIN_PULSE buffer."
            !stop 'Failure to deallocate space for main_pulse'
	endif
        allocate(main_pulse(lsize),stat=error)
	if(error/= 0)then
           print*,"Program could not allocte space for MAIN_PULSE buffer."
           stop 'Failure to allocate space for main_pulse'
	endif
	dpulse=0		!Init to zero
	main_pulse=0

		!construct delayed pulse
	dptr=>dpulse(delay:delay+psize-1)	
	dptr=pulsec*gain		!Load in delayed pulse

	mptr=>main_pulse(1:psize)
	mptr=pulsec		!load in main pulse

	main_pulse=main_pulse-dpulse !form difference and look for zero crossing
				!after first above mainpulse threshold		
				!now look for zero crossing.
	do j=i,psize
		if(mptr(j)<=0)then
			cfd_time=j
			exit
		elseif(j==psize)then
			trigger=.false.
			exit			!Exit so we can do the
						!deallocattions
		endif
	enddo
	return
   END SUBROUTINE W10M_CFD_TRIGGER
!****************************************************************************

   SUBROUTINE W10M_NOISE_PULSE(pulsen,mean_time_gap,single_pe_sigma)
!****************************************************************************
!	Generate random skyshine pulses and fill into PULSEN
!****************************************************************************

!	Modified:

!	25/2/99 GHS
!		Add AFTER_PULSE flag to turn on afterpulses in single pe
!		pulse height distribution. Afterpulses only show up in noise.
!	16/2/00 GHS
!		Explictily use mean_time_gap as an argument. That way we
!		can have varying noise for different pixels.
!       12/02/01 GHS
!               Stop if mean_time_gap is LE 0. Negative mean_time_gap can put 
!               us in infinite loop.
!        31/03/04 GHS V:1:3:8:3:4:4.17
!                For still more attempts at dirtying up things, change 
!                SINGLE_PE_HEIGHT (W10m_SUBS.F90) to have the width of the
!                single pe pulse height distribution be an input parameter.
!                Defaults to old vaslue (.275). Add PPTR%SINGLE_PE_SIGMAS.
!                Fill it in WCAMERA_GEN. Put in parameters 
!                PULSE_HEIGHT_WIDTH_MEAN and PULSE_HEIGHT_WIDTH_SIGMA in
!                WHIPPLE_TELESCOPE.F90
!        31/03/04 GHS V:1:3:8:3:4:4.17
!                Also add single pe pulse height sigma to argument list
!                Past on to single_pe_height call.

	use wcamera_def
	use w10m_pepls_def
	use w10m_subs
        implicit none

	real :: mean_time_gap
	real,pointer,dimension(:) :: pulsen	
        real single_pe_sigma
	real,pointer,dimension(:) :: ppulse	
	real :: time,time_step,pulse_height
	integer :: nbins
	real,external :: rexp
	logical :: after_pulse
!        integer :: icount
!        real :: avgap

!12/02/01 GHS
        if(mean_time_gap<=0.0)then
           print*,'Fatal-Mean_time_gap is <=0. mean_time_gap:',mean_time_gap
           stop 'Mean_time_gap le 0'
        endif

	nbins =size(pulsen)
	ppulse=>pepulse
	time=-(size(ppulse)+1)*.25		!Starting time(in ns)
			     !Use exponetially distributed time gaps between
			     !skyshine pulses
	after_pulse=.true.

	do
		time_step=rexp(mean_time_gap)
                time=time+time_step
		if(time*4>nbins)then 
                        exit
		endif
		call single_pe_height(pulse_height,after_pulse, &
                         & single_pe_sigma)
		call w10m_pulse_build(time,pulse_height,pulsen,ppulse)
					!Make pulses to end
	enddo
	return
   END SUBROUTINE W10M_NOISE_PULSE
!*****************************************************************************
	
   SUBROUTINE W10M_PEPULSE_ACCUMULATE(IPIX,PE_TIME,ithphi,pptr)
!*****************************************************************************
!	Accumulate the pe pulse times  into the pulse arrays for this camera.
!*****************************************************************************
!Modified:
!        30/03/04 GHS V:1:3:8:3:4:4.17
!                In order to try to dirty up things a little more add a timing
!                offset to each channel(pptr%timing_offset). Pick offset for 
!                each PMT randomly from a gaussian dist with width of 
!                timing_offset_sigma. Fill it in WCAMERA_GEN. Apply offset in
!                W10M_PEPULSE_ACCUMULATE. 

	use w10m_pepls_def
	use structures
	use wcamera_def
	IMPLICIT NONE

	real :: pe_time
	integer :: ipix,ithphi

	type(camera_ptrs) ::  pptr


!Save focal plane times.
!	pptr%time(pptr%pes_in_image(ithphi),ithphi)=pe_time
!30/3/04 add timing offset
	pptr%time(pptr%pes_in_image(ithphi),ithphi)=pe_time + &
             & pptr%timing_offset(ipix)
!Save pixel
	pptr%time_pixel(pptr%pes_in_image(ithphi),ithphi)=ipix
	return
   END SUBROUTINE W10M_PEPULSE_ACCUMULATE
!*****************************************************************************

   SUBROUTINE W10M_PEPULSE_ADC(wptr,pptr,adc,npmt_mult_trigger,adc_gate)
!*****************************************************************************
!	Applies the adc gate to the pulse arrays using the timing to
!	determine the adc values.
!*****************************************************************************
!       modified:
!	30/10/00 GHS V:1:1:6:1:2:3.8
!               Fix a bunch of bugs, most to do with array lengths and number 
!               of PMTS in the multiplicity trigger.
!                W10M_PEPULSE:W10M_PEPULSE_ADC
!                 1. Pre-subtract pedistal from new noise channels after 379.

	use wcamera_def
	use structures
	use w10m_pepls_def
	implicit none

        type(camera) :: wptr
        type(camera_ptrs) :: pptr
	real,pointer,dimension(:) :: adc
	integer :: npmt_mult_trigger,i
	real :: adc_gate
	
	real,pointer,dimension(:) :: sptr
        real :: ptime,ntime
	integer :: ithphi,nstart,pstart,pend,nend
	real :: area_normalize
	logical :: initialize_adc=.true.

!debug
	integer :: icount=0,ideb
	real :: inside,lo_outside,hi_outside
!enddebug

			!Determine conversion constand from a pulse height
			!to a nomalized(area=1)	for adc
	if(initialize_adc)then
		area_normalize=sum(pepulse)
		print*,"pepulse normalized area:",area_normalize
                initialize_adc=.false.
		if(mult_trig_type)then
			write(6,1000)' ADC gate timing set by Multiplicity &
					& trigger'
1000	format(' ',a)
			write(6,1001) ' Multiplcity Trigger fraction:', &
				& 1/mult_cfd_gain
1001	format(' ',a,f10.3)
		else
			write(6,1000)' ADC gate timing set by PST trigger'
		endif
	endif

	ithphi=1    !This routine is only called from in KASAOMEGA  where
		    !ithphi is always 1.

			!Our first problem will be to add the noise in to 
			!those non trigger channels that were not in the
			!trigger.
	if(npmt_mult_trigger<wptr%npmt)then
	    do i=npmt_mult_trigger+1,wptr%npmt
               if(pptr%disc(i,ithphi)/=0)then
                  sptr=>pulse(1:,i)
               else
                  sptr=>npulse(1:,i)
                                  !Init to negative pedistal
                  sptr=-pptr%mean_noise(i)             !Vector arith
               endif
              if(pptr%active_pmts(i)/=0)then
                 if(pptr%mean_time_gap(i).le.0.0)then
            print*,'W10mPePulseADC-Fatal--mean_time_gap<=0.i,mean_time_gap:',&
                           & i,pptr%mean_time_gap(i)
           stop 'KASAOMEGA-W10PePulseADC-Mean_time_gap<=0.'
                  endif
                  call w10m_noise_pulse(sptr,pptr%mean_time_gap(i), &
                       &pptr%single_pe_sigmas(i))	
               endif
             enddo
	endif
			!Now determine where the gate is for both pes pixels
			!and pure noise pixels
		
			!We have 2 choices, the pst timing or the mult timing.
	if(mult_trig_type)then
				!MUlt timing.
		ptime=(mult_trigger_time-time_min)+ adc_delay	!Real time here.
		ntime=(mult_trigger_time-rtime_min)+adc_delay	!Real time here.
	else
				!PST timing
		ptime=(pst_trigger_time-time_min)+adc_delay	!Real time here.
		ntime=(pst_trigger_time-rtime_min)+adc_delay	!Real time here.
	endif

	pstart=ptime*4				!convert to index in array
	pend=(ptime+adc_gate)*4-1
	nstart=ntime*4
	nend=(ntime+adc_gate)*4-1
				!Now go through and collect adc. Pedistal
				!already subtracted.
	do i=1,wptr%npmt
           if(pptr%active_pmts(i)/=0)then
              if(pptr%disc(i,ithphi)/=0)then   
                 sptr=>pulse(pstart:pend,i)
              else
                 sptr=>npulse(nstart:nend,i)
              endif
				!Form adc and nomalize to area(area pf 1 pe=1)
              adc(i)=sum(sptr)/area_normalize
           else
              adc(i)=0
           endif

	enddo
	return
   END SUBROUTINE W10M_PEPULSE_ADC
!*****************************************************************************

   SUBROUTINE W10M_PEPULSE_ANALYZE(wptr,pptr,ithphi,npmt_mult_trigger, &
	            &  mult_event_trigger,pst_event_trigger,adc_gate)
!*****************************************************************************
!	Accumulate the pulse shape into the pulse arrays.
!	and determine pixel and event trigger times.
!*****************************************************************************

!   Determine various properties of the pulses in each pixel

!	Modified:

!	25/2/99 GHS
!		Modify for use in determining the trigger using the pulse
!		reconstruction and CFD timing.
!		Add AFTER_PULSE flag to turn off afterpulses in single pe
!		pulse height distribution. Afterpulses only show up in noise.

!	05/3/99 GHS
!		Add stuff for PST analysis and processing.

!	18/4/99 GHS
!		Still working to improve PST stuff. Do some stuff to reduce
!		running time. First restrict how many pixels need to be
!		initalzes in PULSE.
!	16/2/00 GHS
!		Incorporate stuff to ignore dead tubes. Convert meant_time_gap
!		to an array.

!	24/3/00 GHS
!		Add call to w10m_pepulse_laura_adc to get no-noise adc values
!		of this event for Laura's file.
!       31/03/04 GHS V:1:3:8:3:4:4.17
!                For still more attempts at dirtying up things, change 
!                SINGLE_PE_HEIGHT (W10m_SUBS.F90) to have the width of the
!                single pe pulse height distribution be an input parameter.
!                Defaults to old vaslue (.275). Add PPTR%SINGLE_PE_SIGMAS.
!                Fill it in WCAMERA_GEN. Put in parameters 
!                PULSE_HEIGHT_WIDTH_MEAN and PULSE_HEIGHT_WIDTH_SIGMA in
!                WHIPPLE_TELESCOPE.F90

	use wcamera_def
!	use kastrigger_def
	use record_defs
	use structures
	use w10m_pepls_def
        use kasaomega_def
        use w10m_subs
!	use kas_ranlux
	implicit none

        type(camera) :: wptr
        type(camera_ptrs) :: pptr
	integer :: ithphi
	integer :: npmt_mult_trigger
	logical :: mult_event_trigger,pst_event_trigger
	real :: adc_gate
	integer :: i,nbins,error,time_index,j,k,ntrigger
	real :: time_max,time,pulse_height
	real :: rtime_max,trigger_time

	real,pointer,dimension(:) :: sptr
	real,pointer,dimension(:) :: ppulse
	real,pointer,dimension(:) :: trig_times
	real :: apulse
	logical,pointer,dimension(:) :: pixel_init


	logical :: trigger,after_pulse,real_pulse
	integer :: trigger_index,adc_gate_steps

	real :: pran,xdummy

	ppulse=>pepulse
     	time_index=pptr%pes_in_image(ithphi)	!Total number of pes in image.
			!the limits of the times.

			!Absolute minimin coincidence time is pst_pulse_width
			!earlier the earliest shower pe time.
			!this sort of models the coincidence gate.
			!It enables noise generated triggers to contribute
			!even in those pixels that have real pes in them.
			!(note pst_pulse_width is bigger then cfd_pulse_width)
  	time_min=minval(pptr%time(1:time_index,ithphi))-pst_pulse_width
			!pulse take out the adc gate delay
	if(adc_delay<0)then
		time_min=time_min+adc_delay
	endif

			!to completly fit a signal pe pulse at the end add 
			!the length in ns of pepulse to the maxtime.
   	time_max=maxval(pptr%time(1:time_index,ithphi))+(size(pepulse)+1)*.25

!find the number of bins needed for .25 ns steps.
	nbins=(time_max-time_min)*4+1
						!round up
			!Further add the width of the ADC gate (for noise
			!reasons),(which may be 0 for KASTRIGGER)
	adc_gate_steps=adc_gate*4
	nbins=nbins+adc_gate_steps

! Allocate real pulse accumulation array

        deallocate(pulse,stat=error)
	if(error/= 0)then
           print*,"Program could not dallocte space for Pe Pulse buffers."
           !stop 'Failure to allocate space for pulse'
	endif

  	allocate(pulse(nbins,wptr%npmt),stat=error)
	if(error/= 0)then
	print*,"Program could not allocte space for Pe Pulse buffers."
	    	stop 'Failure to allocate space for pulse'
	endif                   	
        pulse=0                                                 !Vector Arith

!	Allocate pixel initalize flag array.

   	deallocate(pixel_init,stat=error)
	if(error/= 0)then
           print*,"Program could not deallocte Pe Pixel_INit buffers."
           !stop 'Failure to allocate space for pixel_init'
	endif                   	

   	allocate(pixel_init(wptr%npmt),stat=error)
	if(error/= 0)then
           print*,"Program could not allocte Pe Pixel_INit buffers."
           stop 'Failure to allocate space for pixel_init'
	endif                   	


			!Flags whether we have initalized a pixel's pulse
			!array.
	pixel_init=.false.				!vector arith.


!go through the pes(that hit pixels). Collect pulses for all pixels at the 
!same time. Do this for pulse height need.
	after_pulse=.false.		!Turn off afterpulses in single pe
					!pulse height distribution.
	do i=1,time_index
		j=pptr%time_pixel(i,ithphi)
!		if(pptr%active_hv(j)==0)then
		if(pptr%active_pmts(j)==0)then  !is thic correct?
					!PMT is dead. Ignore it.
		        pptr%disc(j,ithphi)=0
			cycle
		endif
		if(pran(xdummy).gt.pptr%efficiency(j))then
				!!!!!!!!!REDUCE DISC!!!!!!!!!!
			pptr%disc(j,ithphi)=pptr%disc(j,ithphi)-1      
			cycle		!skip it.
		endif
				!Do the following to reduce amount of time
				!spent initalizing the pulse array.
		sptr=>pulse(1:nbins,j)
		if(.not.pixel_init(j))then
				!Load into this array a negative pedistal 
				!value(we are capcitively coupled)
			sptr=-pptr%mean_noise(j)		!vector arith.
			pixel_init(j)=.true.
		endif

					!See if we reject this one.
	!Pick pulse height from single pe distribution
		call single_pe_height(pulse_height,after_pulse, &
                     & pptr%single_pe_sigmas(j))
 			!add in the next pulse with pulse height
		time=(pptr%time(i,ithphi)-time_min)	!Real time here.
		call w10m_pulse_build(time,pulse_height,sptr,ppulse)
             enddo

	pptr%time_trig=100001.   		    !Vector arith
						    !TDC overflow
						    !Use this to help searching 
						    !for triggers		
	ntrigger=0
	real_pulse=.false.


	do i=1,npmt_mult_trigger	!Only do whats need now to determine if we
				!have a trigger.
	    if(pptr%disc(i,ithphi)/=0)then   	!This also ignores dead pmts
						!which had disc set to 0 above
		   sptr=>pulse(1:nbins,i)
!Add in noise to jiggle the timing(includes extra for ADC calc).
              if(pptr%mean_time_gap(i).le.0.0)then
        print*,'W10mPePulseAnalyze1-Fatal--mean_time_gap<=0.i,mean_time_gap:',&
                           & i,pptr%mean_time_gap(i)
           stop 'KASAOMEGA-W10PePulseAnalyze1-Mean_time_gap<=0.'
                   endif
  		   call w10m_noise_pulse(sptr,pptr%mean_time_gap(i), &
                       &pptr%single_pe_sigmas(i))	
				!Remember not to include the extra noise only
				!stuff at the end due to ADC_GATE
				!considerations.
				!Also jump over the adc_delay at start
		   if(adc_delay<0)then
			   sptr=>pulse(-adc_delay_steps:nbins-adc_gate_steps,i)
		   else
			   sptr=>pulse(1:nbins-adc_gate_steps,i)
		   endif

		   call w10m_cfd_trigger(sptr,pptr%time_trig(i),  &
			   &  pptr%threshold(i),trigger,cfd_gain,cfd_delay)

		   if(trigger)then
			real_pulse=.true.

				!Correct for adc_delay if we took out above
		   	if(adc_delay<0)then
		        	pptr%time_trig(i)=  &
   				&  pptr%time_trig(i)/4-adc_delay+time_min
		   	else
		        	pptr%time_trig(i)=  pptr%time_trig(i)/4+time_min
			endif

		        ntrigger=ntrigger+1		!count the triggers.
	           endif
           endif
	enddo

!Now look for a trigger.
!Do we have a least one trigger where the pixel also had at least one real pe
!in it?
	pst_event_trigger=.false.
	mult_event_trigger=.false.
	if(real_pulse)then
						!Work a bit to narrow the noise
				!window. We only want triggers with 'real' hits
				!so we only have to generate noise
				!pst_pulse_width before and after.
				!real easy when only 1 real hit
	    rtime_min=minval(pptr%time_trig(1:npmt_mult_trigger))-pst_pulse_width
	    if(ntrigger==1)then
   		rtime_max=rtime_min+2*max(pst_pulse_width,adc_gate)
	    else
				!Order the times to find the last one.
	        trig_times=>pptr%time_trig(1:npmt_mult_trigger)
	        call W10M_ARRAY_ORDER(trig_times,pptr%trig_index)
		rtime_max=trig_times(pptr%trig_index(ntrigger))+ &
				& max(pst_pulse_width,adc_gate)
	    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				!Also make some extra room to take care of
				!the adc gate widow precedding the trigger.
				!For multiplicity trigger timeing only
	    if(adc_delay<0)then
		    rtime_min=rtime_min+adc_delay
	    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

				!find the number of bins needed for.25 ns steps.
	    nbins=(rtime_max-rtime_min)*4+1
! Allocate noise pulse accumulation array

            deallocate(npulse,stat=error)
	    if(error/= 0)then
               print*,"Program failed to  deallocte Pe nPulse buffers."
               !stop 'Failure to deallocate space for npulse'
	    endif
   	    allocate(npulse(nbins,wptr%npmt),stat=error)
	    if(error/= 0)then
               print*,"Program could not allocte space for Pe nPulse buffers."
               stop 'Failure to allocate space for npulse'
            endif
 
		   				!Look for noise triggers now.
	    do i=1,npmt_mult_trigger !Only do whats need now to determine if we
				     !have a trigger. 
			!Pre-Subtract pedistal
	   	npulse(1:,i)=-pptr%mean_noise(i)		!Vector Arith

		if(pptr%active_pmts(i)==0)then
					!PMT is dead. Ignore it.
			pptr%disc(i,ithphi)=0
			cycle
		endif

		if(pptr%disc(i,ithphi)==0)then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				!Be sure to skip over the extra time added for
				!the adc gate
	    	   if(adc_delay<0)then
		   	sptr=>npulse((-adc_delay_steps):nbins,i)
	    	   else
		   	sptr=>npulse(1:nbins,i)
	    	   endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Do noise.
              if(pptr%mean_time_gap(i).le.0.0)then
        print*,'W10mPePulseAnalyze2-Fatal--mean_time_gap<=0.i,mean_time_gap:',&
                           & i,pptr%mean_time_gap(i)
           stop 'KASAOMEGA-W10PePulseAnalyze2-Mean_time_gap<=0.'
                   endif
  		   call w10m_noise_pulse(sptr,pptr%mean_time_gap(i), &
                       &pptr%single_pe_sigmas(i))	

		   call w10m_cfd_trigger(sptr,pptr%time_trig(i),  &
			   &  pptr%threshold(i),trigger,cfd_gain,cfd_delay)

		   if(trigger)then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				!Be sure to correct for the gate delay
				!the adc gate
		    	if(adc_delay<0)then
			   pptr%time_trig(i)=  pptr%time_trig(i)/4+ &
				& rtime_min-adc_delay		

			else
			   pptr%time_trig(i)=  pptr%time_trig(i)/4+rtime_min
			endif

			ntrigger=ntrigger+1		!count the triggers.
!	print*,'NOISE:i,time_trig(i),ntrigger:',i,pptr%time_trig(i),ntrigger
	           endif
		endif
	    enddo
 		

!We have to have at least hmult triggers before we even start!
	    if(ntrigger>=mhead%hmult)then
		mrec%i_mult=ntrigger	      	!Save for ntupple file.

!****************************************************************************
!PST analysis
		if(index(mhead%options,'PST')/=0)then 
		      !If we have limnited ourselves to less then 331 pixels in 
		      !trigger they remaining pixels will have the TDC overflow
		      !value(100000.+) and thus be ignored in w10m_pst_trigger.
	    	    call w10m_pst_trigger(pptr,trigger_time)
	    	    if(trigger_time<100000.) then
			pst_trigger_time=trigger_time
			pst_event_trigger=.true.
		    else
			pst_trigger_time=100001.
			pst_event_trigger=.false.
	    	    endif                       
						!Save for ntupple file.
			mrec%fp_median_time=pst_trigger_time 
		endif
!****************************************************************************

!****************************************************************************
!Multiplicity trigger: Always do it.
			!The multiplicity trigger is generated by a cfd
			!discriminator. So model it as we did the pmt
			!pulses.
			!Note if no trigger will get a trigger at the overflow
			!value of 100000.+
	        trig_times=>pptr%time_trig(1:npmt_mult_trigger)
		call w10m_mult_trigger(trig_times,mhead%hmult)
				!Save for ntupple file.
		mrec%median_time=mult_trigger_time
 		if(mult_trigger_time<100000.)then
		    mult_event_trigger=.true.
		endif
!****************************************************************************
           endif
        endif
	if(index(mhead%options,'PST')==0)then 
		pst_event_trigger=.true.	!If were not using PST
							!default it true.
        endif

	return
   END SUBROUTINE W10M_PEPULSE_ANALYZE
!*****************************************************************************

   SUBROUTINE W10M_PEPULSE_LAURA_ADC(wptr,pptr)
!*****************************************************************************
!	Applies the adc gate to the pulse arrays using the timing to
!	determine the adc values for the no-noise laura file.
!*****************************************************************************
	use wcamera_def
	use structures
	use w10m_pepls_def

	implicit none

        type(camera) :: wptr
        type(camera_ptrs) :: pptr
	integer :: i,ithphi
	
	real,pointer,dimension(:) :: sptr
	real :: area_normalize
	logical :: initialize_adc=.true.


			!Determine conversion constand from a pulse height
			!to a nomalized(area=1)	for adc
	if(initialize_adc)then
		area_normalize=sum(pepulse)
		initialize_adc=.false.
	endif
				!Now go through and collect adc. 
	ithphi=1	!This routine only called in KASAOMEGA where ithphi is
			!always 1.
	do i=1,wptr%npmt
		if(pptr%disc(i,ithphi)/=0)then
			sptr=>pulse(1:,i)
		   	!Form adc,restore negative pedistal(remember that pulse
			!has the chanel by channel pedistal already
			!subtracted. we need to add that much back in)
			! and nomalize to area(area pf 1 pe=1)
			pptr%laura_adc(i)=(sum(sptr)+ &
				& size(sptr)*pptr%mean_noise(i))/area_normalize
		else
			pptr%laura_adc(i)=0
		endif
	enddo
	return
   END SUBROUTINE W10M_PEPULSE_LAURA_ADC
!*****************************************************************************

    SUBROUTINE W10M_MULT_TRIGGER(trig_times,hmult)
!*****************************************************************************
!	Find whether we have a straight multiplicity trigger in npmts. Find
!	trigger time by constructing the summed multiplicity pulse and doing 
!	the standard CFD proccess to it.
!*****************************************************************************
	use w10m_pepls_def
	implicit none

	real,pointer,dimension(:) :: trig_times
	real :: hmult

	integer :: npmts,nbins,i,error
	real :: time_minn,time_max,time,mult_threshold,trigger_time
	real,pointer,dimension(:) :: pulse_cfd,ppulse
	logical :: trigger,ifirst=.true.

	npmts=size(trig_times)		!Get number of channels in trigger.
			!Absolute minimin coincidence time 
        if(ifirst)then
           print*,'Only first',npmts, &
                   & ' pixels used in Multiplicity trigger!'
           ifirst=.false.
        endif


  	time_minn=minval(trig_times)
			!to completly fit a signal pe pulse at the end add 
			!10 ns to the maxtime.
   	time_max=time_minn
	do i=1,npmts
		if (trig_times(i)>time_max.and.trig_times(i)<100000.)then
			time_max=trig_times(i)
		endif
	enddo

	time_max=time_max+cfd_pulse_width

!find the number of bins needed for .25 ns steps.
	nbins=(time_max-time_minn)*4+1
						!round up
! Allocate summed cfd pulse accumulation array
   	deallocate(pulse_cfd,stat=error)
	if(error/= 0)then
           print*,"Program could not deallocte space for cfd Pulse buffer."
           !stop 'Failure to deallocate space for pulse_cfd'
	endif                   	
   	allocate(pulse_cfd(nbins),stat=error)
	if(error/= 0)then
	print*,"Program could not allocte space for cfd Pulse buffer."
	    	stop 'Failure to allocate space for pulse_cfd'
	endif                   	

	pulse_cfd=0					!Vector arith

!go through the pmts(that have pulses). 
	ppulse=>cfdpulse
	do i=1,npmts
		if(trig_times(i)<100000.)then
			time=(trig_times(i)-time_minn)	!Real time here.
			call w10m_pulse_build(time,1.0,pulse_cfd,ppulse)
		endif
	enddo
			!Now we have the multiplicity pulse built.
			!look for cfd trigger.
	mult_threshold=hmult-.5		
	call w10m_cfd_trigger(pulse_cfd,trigger_time,  &
			   &  mult_threshold,trigger,mult_cfd_gain, &
			   &  mult_cfd_delay)
	if(trigger)then
		mult_trigger_time=trigger_time/4+time_minn

4301 format(' ',f14.7)
	else
		mult_trigger_time=100001.
	endif
	return
    END SUBROUTINE W10M_MULT_TRIGGER
!*****************************************************************************


   SUBROUTINE W10M_PULSE_BUILD(time,pulse_height,pulseb,ppulse)
!****************************************************************************
!Add a single pe into the pulse array
!****************************************************************************

	use w10m_pepls_def
	implicit none

	real :: time,pulse_height
	real,pointer,dimension(:) :: pulseb,ppulse

	real,dimension(:),pointer :: pulse_ptr,sptr
	integer :: pulse_index,k,nbin

	nbin=size(pulseb)

	pulse_index=(time)*4+1		!convert time to .24 ns steps
	if(pulse_index>nbin)then
		if(pulse_index>nbin+1)then
			print*,' Pulse out of range:',time,' ns.'
		endif
		return
	endif
		!If this is negative restrict the pulse
		!first just get the part of the ppulse we want
	if(pulse_index<1)then
		k=-pulse_index+1
		sptr=>ppulse(k:)
		pulse_index=1
	elseif(pulse_index+size(ppulse)> nbin-1)then
		k=nbin-pulse_index-1
		sptr=>ppulse(1:k)
	else
		sptr=>ppulse
	endif

        pulse_ptr=>pulseb(pulse_index:(pulse_index+size(sptr)-1))
				!add in the next pulse with pulse height
				!variation
		!THIS IS A VECTOR OPERATION into PULSE.
	pulse_ptr=pulse_ptr+(sptr*pulse_height)
	return
   END SUBROUTINE W10M_PULSE_BUILD
!****************************************************************************


   SUBROUTINE W10M_PEPULSE_PACK(wptr,pptr,ithphi,packed)
!*****************************************************************************
!	Pack the pes times into an i*2 array. convert to .25 ns steps
!	Subtrack time_min
!*****************************************************************************
!       Modified:
!       9/10/00 GHS V:1:1:6:1:2.10
!               Add more HDF5 capability.All new HDF5 C routines in KASHDF5.c 
!               1.Add cabability to writer output Mfile as HDF5 file.
!                 This required that some of the guts of this routine be 
!                 extracted to from a new routine: W10M_PEPULSE_TIME_PACK.

	use wcamera_def
	use record_defs
	use structures
	implicit none

        type(camera) :: wptr
        type(camera_ptrs) :: pptr
	integer :: ithphi,time_index
	real*4,pointer,dimension(:) :: times,packed

        integer*2,pointer,dimension(:) :: pixels_time_packed

	integer :: error,i,j,k

        call W10M_PEPULSE_TIME_PACK(wptr,pptr,ithphi,times)

			!PIXELS_TIME_PACKED is a integer*2 array. we are going
			!to first make the pes times relative to time_minn and
			!then to convert the times to integers where each step
			!is .25 seconds and put them into I*2

	deallocate(pixels_time_packed,stat=error)
	if(error/= 0)then
           print*,"Program could not deallocte space for pixels_time_packed"
           !stop 'Failure to deallocate space for pixels_time_packed'
	endif
	allocate (pixels_time_packed(size(times)),stat=error)
	if(error/= 0)then
           print*,"Program could not allocate space for pixels_time_packed"
           !stop 'Failure to allocate space for pixels_time_packed'
	endif
			!PIXELS_TIME_PACKED is a intger*2 array. we are going
			!to first make the pes times relative to time_minn and
			!then to convert the times to integers where each step
			!is .25 seconds and put them into I*2

	do i=1,size(times)
		k=int((times(i)-mrec%time_cfd)/.25)
		pixels_time_packed(i)=k
	enddo
        k=size(pixels_time_packed)/2
					!Allocate output array and pack out
					!times(as bytes) into them. This is
					!messy I know.
	if(k*2.lt.size(pixels_time_packed))then
		k=k+1
	endif

	deallocate(packed,stat=error)
	if(error/= 0)then
           print*,"Program could not deallocte space for packed"
           !stop 'Failure to deallocate space for packed'
	endif
	allocate(packed(k),stat=error)
	if(error/= 0)then
           print*,"Program could not allocte space for packed"
           stop 'Failure to allocate space for packed'
	endif

                                		!I think this packs things as
						!we want.
	packed=transfer(pixels_time_packed,packed)
	return
   END SUBROUTINE W10M_PEPULSE_PACK
!****************************************************************************

  SUBROUTINE W10M_PEPULSE_TIME_PACK(wptr,pptr,ithphi,times)
!*****************************************************************************
!	Order the pes times into an i*2 array. convert to .25 ns steps
!	Subtrack time_min
!*****************************************************************************
!
!       Modified:
!       9/10/00 GHS V:1:1:6:1:2.10
!               Add more HDF5 capability.All new HDF5 C routines in KASHDF5.c 
!               1.Add cabability to writer output Mfile as HDF5 file.
!                 This required this routine that orders the time data by 
!                 pixel and converts to integer*2. Also save the offset time 
!                 for reconstruction later. The guts of this routine was 
!                 extracted and is now call from the guts of W10M_PEPULSE_PACK.


	use wcamera_def
	use record_defs
	use structures
        use w10m_subs
	implicit none

        type(camera) :: wptr
        type(camera_ptrs) :: pptr
	integer :: ithphi

	real*4,pointer,dimension(:) :: times
	real,pointer,dimension(:) :: pixels_time
	integer,pointer,dimension(:) :: pixels_time_index
	integer :: time_index,error,i,j,k
	real :: time_minn
					!I have to order all the times by
					!pixel.
     	time_index=pptr%pes_in_image(ithphi)	!Total number of pes in image.

						!reference time.
  	time_minn=minval(pptr%time(1:time_index,ithphi))-pst_pulse_width

	pixels_time=>pptr%time_pixel(1:time_index,ithphi)
	deallocate(pixels_time_index,stat=error)
	if(error/= 0)then
           print*,"Program could not deallocate space for pixels_time_index"
           !stop 'Failure to deallocate space for pixels_time_index'
	endif
	allocate (pixels_time_index(size(pixels_time)),stat=error)
	if(error/= 0)then
           print*,"Program could not allocate space for pixels_time_index"
           stop 'Failure to allocate space for pixels_time_index'
	endif
		
	call W10M_ARRAY_ORDER(pixels_time,pixels_time_index)
                        		!Now we can pick up the pes by order
					!of pixel(time ordered also I think)
	deallocate(times,stat=error)
	if(error/= 0)then
           print*,"Program could not deallocte space for times"
           !stop 'Failure to deallocate space for times'
	endif
	allocate (times(size(pixels_time)),stat=error)
	if(error/= 0)then
           print*,"Program could not allocate space for times"
           !stop 'Failure to allocate space for times'
	endif

	do i=1,time_index
           j=pixels_time_index(i)
           times(i)=pptr%time(j,ithphi)
        enddo

!	Fill  mrec stuff here.
	mrec%time_cfd=time_minn            !Reference time for packed times
        
	return
   END SUBROUTINE W10M_PEPULSE_TIME_PACK
!****************************************************************************


    SUBROUTINE W10M_UNPACK(disc_cm,disc,wptr,pptr)
!*******************************************************************************
!	Unpack compressed disc array and pe timien arrays. Fill up pptr%time
!	and pptr$time_pixel, and pptr%disc for use by W10M_PEPULSE_ANALIZE.
!*******************************************************************************
	use wcamera_def
!	use kastrigger_def
	use record_defs
	use w10m_subs
	implicit none

        type(camera) :: wptr
        type(camera_ptrs) :: pptr
	real,target,dimension(:) :: disc_cm
	real,pointer,dimension(:) :: disc

	integer :: i,j,k,m,n,error,jpe
	integer*2,pointer,dimension(:) :: packedi2,i2ptr
	real,pointer,dimension(:) :: sptr

			!DISC
	m=1  		!Points to number of words in compressed array
	n=disc_cm(m)	!Number of words in compressed array
	j=m+1		!Points to first word of compressed array
	n=m+n		!Points to last word of compressed array
	call w10m_uncompress(disc_cm(j:n),disc,wptr%npmt)
				!TIME_TRIG		

				!Allocate the pes timing arrays
				!Only 1 ithphi at a time now.
	jpe=sum(disc) 		!total number of pes.
	pptr%pes_in_image(1)=jpe

	deallocate(pptr%time,stat=error)
	if(error/= 0)then
           print*,"Program could not deallocte space for p", &
                & wptr%camera_id,"time."
	     !stop 'Failure to allocate space for pptr%time'
	endif
	allocate(pptr%time(jpe,1),stat=error)
	if(error/= 0)then
           print*,"Program could not allocte space for p",wptr%camera_id, &
                & "time."
           stop 'Failure to allocate space for pptr%time'
	endif

	deallocate(pptr%time_pixel,stat=error)
	if(error/= 0)then
           print*,"Program could not deallocte space for p",wptr%camera_id, &
                & "time_pixel."
           !stop 'Failure to deallocate space for pptr%time_pixel'
	endif
	allocate(pptr%time_pixel(jpe,1),stat=error)
	if(error/= 0)then
           print*,"Program could not allocte space for p",wptr%camera_id, &
                & "time_pixel."
           stop 'Failure to allocate space for pptr%time_pixel'
	endif
				!load up time_pixel array.
				!Pes are packed in order of pixels.
	j=0
	do i=1,wptr%npmt
		if(disc(i)/=0)then
			do k=1,disc(i)
				j=j+1
				pptr%time_pixel(j,1)=i
			enddo
		endif
	enddo
	if(j/=jpe)then
		print*,' Miscount of pes'
		stop 'Miscount of pes'
	endif

	m=n+1		!points to number of words in packed
			!pes timeing array (which is packed 2 i*2 to
			!each r*4 word.)
	k=disc_cm(m)	!Number of words in packed array
	deallocate(packedi2,stat=error)
	if(error/= 0)then
	     print*,"Program could not deallocate space for packedi2."
	     !stop 'Failure to deallocate space for packedi2'
	endif
			!Last work may be 0(ie jpe  may be odd)
	allocate(packedi2(2*k),stat=error)
	if(error/= 0)then
	     print*,"Program could not allocte space for packedi2."
	     stop 'Failure to allocate space for packedi2'
	endif

	j=m+1		!Points to first word of packed array
	k=m+k		!Points to last word of packed array
	sptr=>disc_cm(j:k)
		
	packedi2=transfer(sptr,packedi2)
				!convert to real, add in timing.
				!drop filler 0 if its there.
	i2ptr=>packedi2(1:jpe)
	pptr%time(1:,1)=i2ptr*.25+mrec%time_cfd		!Vector Arith
	return
    END SUBROUTINE W10M_UNPACK
!****************************************************************************

END MODULE W10M_PEPULSE_SUBS
