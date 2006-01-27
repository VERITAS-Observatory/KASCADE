MODULE KASAOMEGA_DEF
!Modified:
!21/07/03 GHS Add v499 prototype flag
	use structures
	character(LEN=80) :: version    !Present version.
	logical :: hadron
	character(len=120) :: options
	character(len=20) :: detector
	integer :: hmult
	real :: swindow,pe_threshold,new_noise,new_reflectivity,new_adc_gate
	real :: new_concentration,old_reflectivity,old_concentration
	real*8,parameter ::  pi=3.141592654
	real,parameter :: c_light=2.99792e+8    !Speed of light in vacume.m/sec

        integer :: cmsum_in        !hid_t for input HDF5 file spec.
        integer :: ihillas_out=0   !Counts ouput events.
        logical :: v499prototype=.false.
        logical :: v499detector=.false.
END MODULE KASAOMEGA_DEF



MODULE SUM_INIT
!******************************************************************************
!Updated for Mt Hopkins Heavies 17/07/01 GHS
!Update to lastest proton spectra from: Weibel-Smith et.al. Astronomy and 
!Astrophysics, 300(1), pg.389-398,Feb. 1, 1998
!******************************************************************************

!******************************************************************************
        character(len=80) :: sum_init_title= "23/07/01 20 deg south, .17 deg&
             &spot size."

!******************************************************************************
!as of 23/07/01:Gamma-Ray Database parameters
!******************************************************************************
	integer,parameter :: g_energies=18
	integer :: g_num=g_energies  		!Number of different energies
						!Shower energies in GeV
						!As of 17/3/00
	real,dimension(g_energies):: g_gev =  &
             &(/84., 100., 120., 143.,  172.,  205.,  246.,  294., 353., 422.,&
             & 505., 605., 724., 867., 1038., 1243., 1781., 2553./)
	real,dimension(g_energies) :: g_nshowers = &
             &(/30.,  30.,  30.,  30.,   30.,   30.,   30.,   50.,  50.,  25.,&
             &  25.,  25.,  15.,  15.,   10.,   10.,   10.,   10./)
				!New gamma flux from Dave Lewis 15/5/96
	real :: g_alpha=-2.45 			!Spectral index for gammas.
	real :: g_phi0=7.16e-3			!Spectral Amplitude for gammas
	                                        !Units=/m**2/sec/GeV
	real ::	g_area_solid=115.47		!Area for grid point for
                                                !VERITAS
	real :: g_sparse=100.0	  		!No sparse grid for VERITAS

	real :: g_weight(g_energies)		!Weights to use at each energy
!******************************************************************************

!******************************************************************************
!as of 23/07/01:Proton  Database parameters
!******************************************************************************
	integer,parameter :: p_energies=21
	integer ::  p_num=p_energies		!Number of different energies
						!Shower energies in GeV
	real,dimension(p_energies) ::  p_gev= &
             & (/20.,  24.,  28.,  34.,  41.,  49.,  58.,  70.,  84., 100.,&
             & 120., 143., 205., 294., 422., 605., 867., 1243., 1781., 2553.,&
             & 3660./) 
	real,dimension(p_energies) ::  p_nshowers=&
             &(/500., 400., 300., 300., 300., 300., 200., 200., 100.,  60.,&
             &  60.,  60.,  60.,  50.,  40.,  30.,  15.,   10.,   10.,   10.,&
             &   10./)

!Flux parameters As of Feb. 1 1998:See Wiebel-Sooth ref above	
        real ::  p_alpha=-2.77	    		!Spectral index for protons.
	real ::  p_phi0=2.35e4			!Spectral Amplitude for protons
	                                        !Units=/m**2/sec/sr/GeV
	real ::  p_area_solid=0.063		!Area-solid angle for 'VT'
	real ::  p_sparse=100.0			!No sparse grid for VERITAS

	real ::  p_weight(p_energies)		!Weights to use at each energy
!******************************************************************************

!******************************************************************************
!as of 23/07/01:IRON (Fe56) Database parameters
!******************************************************************************
	integer,parameter :: fe_energies=9
	integer ::  fe_num=fe_energies		!Number of different energies
						!Shower energies in GeV
	real,dimension(fe_energies) :: fe_gev = &
             & (/422., 605., 867., 1243., 1781., 2553., 3660.,5246.,7519./)
	real,dimension(fe_energies) ::fe_nshowers = &
             & (/ 50.,  30.,  15.,   10.,   10.,   10.,   10.,  10.,   5./)
!Flux parameters As of Feb. 1 1998:See Wiebel-Sooth ref above
        real ::  fe_alpha=-2.60	    		!Spectral index for protons.
	real ::  fe_phi0=1.04e3			!Spectral Amplitude for protons
	                                        !Units=/m**2/sec/sr/GeV
	real ::  fe_area_solid=0.044		!Area-solid angle for 'VT'
	real ::  fe_sparse=100.0		!No sparse grid for VERITAS

	real ::  fe_weight(p_energies)		!Weights to use at each energy
!
!******************************************************************************
END MODULE SUM_INIT
!******************************************************************************
