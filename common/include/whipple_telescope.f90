MODULE WHIPPLE_TELESCOPE

!	Modifed:

!	15/5/98 GHS
!		Values for Veritas strawman entered.
!       06/05/03 GHS
!               VERITAS spotsize updated. Mirror_radius_squared added.
!       04/05/04 GHS


!Whipple 10m
	real,parameter :: w10m_focal_length=7.3 
                                               !Whipple 10m focal length. f .73
	real,parameter :: w10m_spotsize=0.035	!Better W10m spotsize(gaussian
                                                !sigma of mirror aligmment and
                                                !individual spotsize)
	real,parameter :: w10m_spotsize_ew=0.06 !East-West(x) spotsize sigma
	real,parameter :: w10m_spotsize_ns=0.06 !North-South(y) spotsize sigma


	real,parameter :: w10m_facet_diam=.602  !Facet diameter.(m)
	real,parameter :: w10m_mirror_radius_squared=24.98733	
                                                !Sqared effective radius of 
 						!Whipple 10m(m**2)
        real, parameter :: WHIPLAT=0.552978     !Whipple coords (radians)
        real, parameter :: WHIPLONG=1.935190

!VERITAS
	real,parameter :: veritas_focal_length=12.0 !VERITAS
	real,parameter :: veritas_spotsize=0.035    !VERITAS STRAWMAN
	real,parameter :: veritas_spotsize_ew=0.025  !VERITAS GHS 06/05/03
	real,parameter :: veritas_spotsize_ns=0.025  !VERITAS 
	real,parameter :: veritas_facet_diam=.61    !VERITAS
	real,parameter :: veritas_mirror_radius_squared=36.0    !VERITAS

	real :: focal_length
        real :: meters_per_deg
        real :: spotsize
        real :: spotsize_ew
        real :: spotsize_ns
        real :: jitter_width
        real :: jitter_width_ns
        real :: jitter_width_ew
        real :: facet_diam
        real :: mirror_radius_squared

	real,parameter :: phwidth_factor=0.135	!For use in better pulse height
						!jitter of w10m telescope.
						!02/7/98 GHS

	real :: pe_dc=1.0		!Default Pe to adc digital counts(GAIN)
                                        !can be reset by command line parameter
                                        !Option= -d
	real :: ped_sigma=0.0		!Default sigma for ped
                                        !can be reset by command line parameter
                                        !Option= -t
	real :: pe_dc_sigma=0.0		!Default sigma for pedc
                                        !can be reset by command line parameter
                                        !Option= -s
!	real :: timing_offset_sigma=3.0 !test value (ns)
	real :: timing_offset_sigma=0.0 !Default timing offset width
                                        !no command line parameter yet
!	real :: pulse_height_width_mean=0!Use default width (set in 
                                        !SINGLE_PE_HEIGHT)
	real :: pulse_height_width_mean=.64 !Generate single pe pulse height 
	real :: pulse_height_width_sigma=.11!widths from gausian distribution
                                        !These values for whipple 10m
                                        !No command line parameter yet
!generic
        real,parameter :: hiclean=4.25, loclean=2.25
        real,parameter :: efactor=1.5   !Lessard elongation factore for DIST 
                                        !estimation for 2-d analysis in 
                                        !w_hillas
END MODULE WHIPPLE_TELESCOPE
