MODULE KASTRIGGER_DEF
!	Modified:

!	24/3/98	GHS V:1:1:5:1:1.2
!		 Add DN_MIN for use in W10M_POINTING,WHIPPLE_TILT,WHIPPLEINI.

!	18/5/98 GHS
!		Remove hexagon_factor. Replaced by a better model of lightcones
!		in WCAMERA_GEN

!	19/11/99 GHS V:1:1:5:1:2.5
!		Replace dn_min with dn_min_loose and dn_min_tight

!     17/06/04 GHS V:1:3:8:3:4.6
!                Add a -w Option (sets spec_weight_flag to true) which causes 
!                the ouput MHDF5 file events to be cut on the spectrum weights.
 
	use structures

	type(pe_s),pointer :: first_pes_ptr,last_pes_ptr,pes_ptr
!	type(m_head) :: mhead
!	type(m_rec) :: mrec
	logical :: outpes,hadron
	real*8,parameter ::  pi=3.141592654
	real,parameter :: c_light=2.99792e+8     !Speed of light in vacume.m/sec
	real :: dl,dm,dn
	integer :: ipe
	logical :: empty,shower_end
	real :: dn_min_loose,dn_min_tight

	type pe_spec	
		real,dimension(3) :: vpe  !x,y,z of pe in mirror plane.
		real,dimension(3) :: vdir !direction cosigns.dl=vdir(1),
					  !dm=vdir(2),dn=vdir(3). Z is down.
		real,dimension(3) :: vw	  !Position in focal plane of pe,only
					  !x,y components used
		real :: time		!arrival time at focal plane.
	end type pe_spec	

        real :: event_type
        real :: event_tep
        real :: event_weight
!***********************************************************************

END MODULE KASTRIGGER_DEF

MODULE WHIPPLE_TILT_DEF
	real*8 :: tdlm,tdmm,tdnm
!        real :: tdmdl,tdmdn,tdldn2,txmag,tymag
	real*8 :: xdl,xdm,xdn,ydl,ydm,ydn
END MODULE WHIPPLE_TILT_DEF









