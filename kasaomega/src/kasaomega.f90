!	Version: V:1:3:8:3:4:4.26
MODULE KASAOMEGA_SUBS

CONTAINS
    SUBROUTINE SOLIDANGLE
!******************************************************************************
!	Print the mean solid angle contriubions for the sky pixels
!		For Hadrons print out mean solid angle per pixel.
!		For Gammas print out detection area per grid point.
!******************************************************************************
!	Solid angle is just intergrel form ithet-deltheta to itheta+deltheta
!	of sin(theta)dtheta*2*pi=2*pi*(cos(theta-deltheta)-cos(theta+deltheta))

!	Modified:

!	20/4/98 GHS V:1:1:5:1:1:1.4
!		Convert to F90.	

!       20/10/04 GHS
!               Fix for use with random directions within theta_max.

	use kasaomega_def
	use record_defs
	IMPLICIT NONE

!	real*8,parameter ::  pi=3.141592654
!	real :: solid_angle_mean,detheta,thetamin,thetamax,area
	real :: solid_angle_area,thetamax,area
        real :: annulus_solid_angle
	integer :: is,i,nsteps

	area=mhead%pe_head%xseg*mhead%pe_head%yseg
	if(hadron)then
           !Get radius in deg of random circle
           thetamax=(mhead%itheta_max-1)*mhead%step_size+mhead%step_size/2
	   solid_angle_area=2*pi*(1.-cosd(thetamax))*area
!debug
                print*,'Total solid angle for shower: ',solid_angle_area, &
                     & ' m**2 sr';
!enddebug
	   ithphi_max= &
              &  mhead%phi_steps*((mhead%itheta_max-1)*(mhead%itheta_max)/2)+1
                
        print*,'itheta=',i,'Solid angle area per trigger= ', &
             & solid_angle_area/ithphi_max,' m**2 sr'

!*************************************************************************
!		detheta=mhead%step_size*2*pi/360.	!Dtheta in radians.
!			!do zenith first.
!		thetamax=detheta/2.		!Center annulus goes out to
!						!mhead%step_size/2.
!		solid_angle_mean=2*pi*(1.-cos(thetamax))
!!debug
!                print*,'itheta=1:cap solid angle: ',solid_angle_mean*area
!!enddebug
!                is=1
!		do i=2,mhead%itheta_max
!			nsteps=(i-1)*mhead%phi_steps	!Number of phi steps
!						!around anulus.
!                        is=is+nsteps
!				!Add in area of annulus
!			thetamin=thetamax		!Limits of integration.
!			thetamax=thetamin+detheta
!			annulus_solid_angle=2*pi*(cos(thetamin)-cos(thetamax)) 
!!debug
!        print*,'itheta=',i,' Anulus segment solid angle area= ', &
!             & annulus_solid_angle*area/nsteps
!!enddebug
!			solid_angle_mean=annulus_solid_angle + solid_angle_mean
!		enddo
!		solid_angle_mean=(solid_angle_mean/is)*area
!		write(6,1000)solid_angle_mean
!1000	format(  &
!	&  '   Area Solid Angle(m**2 st) = ',f10.4)!
!*************************************************************************

	else				!Gamma showers
		write(6,1001)area
1001	format(  &
	&  '        Detection Area(m**2) = ',f10.2)
	endif
	return
    END SUBROUTINE SOLIDANGLE
!******************************************************************************

    SUBROUTINE V10M_GENERATE499(wptr,pptr)
!******************************************************************************
!	Initlaize for the chosen 499 pixel veritas camera.
!******************************************************************************
	use wcamera_def
	use wcamera_subs
	use kasaomega_def
	use record_defs
	IMPLICIT NONE

	type(camera),pointer :: wptr
	type(camera_ptrs),pointer :: pptr

	call wcamera_gen499(wptr,pptr,mhead)

!	call t3a(wptr,pptr)
!	call t4a(wptr,pptr)
!	if(1.ne.2)stop 'debug stop'

	call wcamera_noise_thresh_gen(wptr,pptr,mhead,options)
	call w10m_image_sigma(wptr,pptr)
	call w10m_efficiency_ratio_gen(wptr,pptr)
	return
    END SUBROUTINE V10M_GENERATE499
!******************************************************************************


    SUBROUTINE W10M_ADC_ORDER(adc,index,npmts)
!******************************************************************************
!	Orders by size adc values. Used to get max1,max2,max3 etc.
!******************************************************************************

!	Returns INDEX which indicates size order in ADC.
!	Modified:

!	09/2/98 GHS V:1:1:5:1:1:1.1
!		Convert to F90.

	use kasaomega_def
	IMPLICIT NONE

	real,dimension(:) :: adc
	integer,dimension(:) :: index

	real,pointer,dimension(:) :: adcs
	real :: temp_adc
	integer :: npmts,temp_index
	integer :: i,k,m,error
   	deallocate(adcs,stat=error)
   	allocate(adcs(npmts),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for ADCS."
	     stop 'Failed to allocate space for ADCS'
	endif
	adcs=adc						   !Vector Arit

	do i=1,npmts
		index(i)=i
	enddo

	do i=2,npmts
		do k=1,i-1
			if(adcs(i).lt.adcs(k))then
					!Swap them
				temp_adc=adcs(k)
				adcs(k)=adcs(i)
				adcs(i)=temp_adc
				temp_index=index(k)
				index(k)=index(i)
				index(i)=temp_index
			endif
		enddo
	enddo	!End of sort
	deallocate(adcs,stat=error)
	return
    END SUBROUTINE W10M_ADC_ORDER
!******************************************************************************

    SUBROUTINE W10M_ADC_ORDER_top(adc,npmts,top,norder,index)
!******************************************************************************
!	Orders by size adc values. Used to get max1,max2,max3 etc.
!	For speed only find top norder of them. Order from largest to smallest
!******************************************************************************

!	Modified:

!	24/2/98 GHS V:1:1:5:1:1:1.2
!		Convert from w10m_adc_order and make it much faster.

!	01/7/98 GHS  V:1:1:5:1:1:1.4
!		Revesre the order and put actual vlues in top. largest to
!		smallesr Drop use of index.

!        12/03/04 GHS V:1:3:8:3:4:4.15
!               Add the index array back in.

	use kasaomega_def
	IMPLICIT NONE

	real,dimension(:) :: adc,top

	real,pointer,dimension(:) :: adcs
	real :: temp_adc
	integer :: npmts,temp_index
	integer,dimension(:) :: index
	integer,dimension(1) :: tindex
	integer :: norder
	integer :: i,k,m,error
	logical :: no_exchange
	real :: a,anext
	
   	deallocate(adcs,stat=error)
   	allocate(adcs(npmts),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for ADCS."
	     stop 'Failed on allocating space for ADCS'
	endif
	adcs=adc	!temp copy                              !Vector Arit

	do k=1,norder
		tindex=maxloc(adcs)  !maxloc wants an array as a destination
		index(k)=tindex(1)
		top(k)=adcs(index(k))
		adcs(index(k))=-1000000
	enddo	!End of sort
	deallocate(adcs,stat=error)
	return
    END SUBROUTINE W10M_ADC_ORDER_TOP
!******************************************************************************

    SUBROUTINE W10M_CLUSTER(pptr,disc,trigger,npmts,norder,clstr_threshold)
!****************************************************************************
!	Find the clusters of pixels in a camera using those that are set in
!	TRIGGER. Find the highest trheshold for each size cluster up to norder
!	andp  put into clstr_threshold.
!****************************************************************************

!Modified:

!	09/7/98 GHS V:1:1:5:1:2:1.4
!		Correct W10M_CLUSTER to not call a cluster which has a large
!		extent 2 clusters.

	use wcamera_def
	IMPLICIT NONE

	type(camera_ptrs) :: pptr
	real,pointer,dimension(:) :: disc,trigger
	real,dimension(:) :: clstr_threshold
	integer :: npmts,norder

	real,pointer,dimension(:,:) :: clstr_arrays
	real,pointer,dimension(:) :: clstr_disc
	integer,pointer,dimension(:) :: cluster,clstr_size,cindex
	integer :: i,j,k,ncluster,m,n,mcluster,ki
	integer :: error
!	Create another pixel array where we will tag all the clusters.
   	deallocate(cluster,stat=error)
   	allocate(cluster(npmts),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for cluster."
	     stop 'Failed to allocated space for cluster.'
	endif
!	Create another array that counts size of clusters.
   	deallocate(clstr_size,stat=error)
   	allocate(clstr_size(npmts),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for clstr_size."
	     stop 'Failed to allocate space for clstr_size'
	endif

!	Find the clusters. Go throught each tube one at a time and find any
!	hit neighbors. if pixel is already in the cluster add any hit neighbors
!	to that cluster otherwise start a new cluster.
	ncluster=0
	cluster=0						!vector arith
	do i=1,npmts
	    if(trigger(i)/=0)then		!This pixel on?
	    	if(cluster(i)==0) then		!In a cluster yet?
			ncluster=ncluster+1
			m=ncluster
			cluster(i)=m
			clstr_size(m)=1         !New cluster. Start count
		else
			m=cluster(i)
		endif
						!Check neighbors
		do j=1,6
			k=pptr%wadjacent(j,i)
			if(k==0)then		!no more neighbors
				exit
			elseif(k>npmts)then	!out of bounds
				cycle
			elseif(trigger(k)/=0)then	!dont double count
				ki=cluster(k)
				if(ki==0) then
					cluster(k)=m
				        clstr_size(m)=clstr_size(m)+1
!09/7/98 GHS Fix for clusters that run into each other and become 1 cluster.
				elseif(ki/=m)then
						!we have got a single cluster
						!which up to this point lokked
						!like 2 clusters. Convert one
						!to the other.
					do n=1,npmts
					    if(cluster(n)==ki)then
					     	cluster(n)=m
					    	clstr_size(m)=clstr_size(m)+1
					    endif
					enddo
					clstr_size(ki)=0
				endif
			endif
		enddo
	    endif
	enddo
		
!Now we know the sizes of the clusters. Some may be only 1 pixel. Some may be
! 0, ie were 2 that got made into 1.Forget those. Order all the other clusters.
!	Create another array that will hold top norder disc values for each
!	clusters(clstr_size may be less then norder and thats ok.
!	(mcluster my be less then ncluster and thats ok too)

   	deallocate(clstr_arrays,stat=error)
   	allocate(clstr_arrays(norder,ncluster),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for clstr_arrays."
	     stop 'Failed to allocate space for clstr_arrays'
	endif
	clstr_arrays=0					!Vector arith	
	mcluster=0
	k=0
	do i=1,ncluster				!Go through clusters
	   if(clstr_size(i)>2)k=k+1
	   if(clstr_size(i)>1)then		!Ignore single pixel clusters
		mcluster=mcluster+1		!index to non single pixel
						!clusters
					!array for collected and then ordering
					!disc vlues in this cluster
	   	deallocate(clstr_disc,stat=error)
	   	allocate(clstr_disc(clstr_size(i)),stat=error)
		if(error/= 0)then
	 	     print*,"Program could not allocte space for clstr_disc."
		     stop 'Failed to allocate space for clstr_disc'
		endif

                deallocate(cindex,stat=error)
                allocate(cindex(clstr_size(i)),stat=error)
                if(error/= 0)then
                   print*,"Program could not allocte space for cindex."
                   stop 'Failed to allocate space for cindex'
                endif
		m=0

						!Find these guys
		do j=1,npmts
		    if(cluster(j)==i)then
		    	m=m+1
		    	clstr_disc(m)=disc(j)				
		    endif
		enddo
					!Order top norder(or clstr_size if its
				 	!smaller)
	        n=min(m,norder)
		call w10m_adc_order_top(clstr_disc,m,   &
			&   clstr_arrays(1:,mcluster),n,cindex)
	    endif
	enddo

				!Ok now we can find biggest thrshold for each
				!cluster size (we start at 2)

	clstr_threshold=0				!vector arith
	if(mcluster>0)then
	    do i=2,norder
		do j=1,mcluster
			if(clstr_arrays(i,j)>clstr_threshold(i-1))then
				clstr_threshold(i-1)=clstr_arrays(i,j)
			endif
		enddo
		if(clstr_threshold(i-1)==0) exit
	     enddo
	endif
        return
    END SUBROUTINE W10M_CLUSTER
!***************************************************************************

    SUBROUTINE W10M_EFFICIENCY_RATIO_GEN(wptr,pptr)
!*************************************************************************
!	Replace new pptr%efficicny(which was appropriate for KASTRIGGER with
!	the ration of the new efficiency to the old (which is whats appropriate
!	for KASAOMEGA
!***************************************************************************

!	Modified:
!	01/12/99 GHS V:1:1:6:1:2:3.2
!		Outer rings of 490 pixel camera have no lightcones.
!	22/3/00 GHS V:1:1:6:1:2:3.4
!		Square pixels of SQ1024 camera use different ratio for

!	30/10/00 GHS V:1:1:6:1:2:3.8
!               Fix a bunch of bugs, most to do with array lengths and number 
!               of PMTS in the multiplicity trigger.
!                KASAOMEGA_SUBS:W10M_EFFICIENCY_RATE_GEN
!                 1:Limit size of pptr%efficiency we are modifying to 
!                   wptr%npmts.Add wptr to subroputines arguments

	use wcamera_def
	use kasaomega_def
	IMPLICIT NONE

	type(camera),pointer :: wptr
	type(camera_ptrs),pointer :: pptr
	integer :: i
	real :: eff_old
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!Efficiency factor
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! _reflectivity:Accounts for dirty mirrors,dusty air and any other linear
!	             reduction(increase) of number of photons that make pe's
! _concentration:Accounts for overall innefficiency of light cones.Its is
!		      assumed that the active area of the pmts
!		      (Ap=pi*pptr%pmt_radius**2) is 100% efficient(after
!		      application of mhead%reflectivity.see above) and that
!		      light on the remaining area of pixel's hexagonal area is 
!		      collected by the light cones which are
!		      mhead%concentration efficient.
!	We need to calcuclate this speratly for each camera/pmt  combination
!	since the efficiency is dependent on pptr%pmt_spacing and
!	pptr%pmt_radius(pmt_spacing is half spacing!!!!!)
!	Note pmt area/hex area=(pi*pmt_radius**2)/sqrt(3)*2*pmt_spacing/2)
!			      = 0.9068996*(pmt_radius/pmt_spacing)**2 
!	Note pmt area/square pixel area =pi*(pmt_radius**2)/(2*pmt_spacing)**2
!		for square pixels =.7854*(pmt_radius/pmt_spacing)**2


	do i=1,wptr%npmt
				!No light cones on outer 111 pmts of 490
				!camera
	    if(index(detector,'W490')/=0.and.i>379)then
		eff_old=old_reflectivity
				!Use different area weighting factor for square
				!pixels in SQ1024 camera.
	    elseif(index(detector,'SQ1024')/=0)then
		eff_old=old_reflectivity*                  & 
  &   ( (0.7854*(pptr%pmt_radius(i)/pptr%pmt_spacing(i))**2) +     &
  &   old_concentration*(1.0- (0.7854*(pptr%pmt_radius(i)/         &
  &   pptr%pmt_spacing(i))**2)))
	    else
		eff_old=old_reflectivity*                  & 
  &   ( (0.9069*(pptr%pmt_radius(i)/pptr%pmt_spacing(i))**2) +     &
  &   old_concentration*(1.0- (0.9069*(pptr%pmt_radius(i)/         &
  &   pptr%pmt_spacing(i))**2)))
		pptr%efficiency(i)=pptr%efficiency(i)/eff_old
	    endif
	enddo
	return
    END SUBROUTINE W10M_EFFICIENCY_RATIO_GEN
!***************************************************************************

    SUBROUTINE W10M_GENERATE(wptr,pptr)
!******************************************************************************
!	Common init stuff.
!******************************************************************************
	use wcamera_def
	use wcamera_subs
	use kasaomega_def
	use record_defs
        IMPLICIT NONE

	type(camera),pointer :: wptr
	type(camera_ptrs),pointer :: pptr
        
	call wcamera_noise_thresh_gen(wptr,pptr,mhead,options)
	call w10m_image_sigma(wptr,pptr)
	call w10m_efficiency_ratio_gen(wptr,pptr)
	return
    END SUBROUTINE W10M_GENERATE
!******************************************************************************


    SUBROUTINE W10M_IMAGE_SIGMA(wptr,pptr)
!******************************************************************************
!	If the 'SIGMA' option is chosen the pedistal widths are read from a 
!	file. If this option is not chosen then the we model pedistal widths
!	that are due to the noise rate and the pulse height jitter.
!******************************************************************************
!
!	When we do the modeling it is done this way since I couldn't figure out
!	how to do it analytically. Its main purpose is to find the width in
!!!!!VVV says use gauss with width=sqrt(n*(1+phwf))
!	pe's of the pedestal under this model for use in the image cleanup
!	procedure. Model is: collect a distribution of pedestal values by:
!	1:Determine mean skyshine in the adc gate(done in calling routine
!	  Entered as variable array pptr%wnoise.
!	2: Loop over some large number(N)of 'events' determining at each event:
!	3: Fluctuate skyshine mean(poission for low values, gaussian for higher
!	   with sqrt(N) width).
!	4: Convert to a pulse height using a gaussian jitter with w width of
!	   sqrt(phwidth_factor*N).(Mean value theorem is too narrow for single
!	   pe dist.)
!	5: collect sum and sum of squares so that in the end we can determine
!	   a width of the pedestal from the combined effects of 3 and 4.
!	6: Determined width
!	7: Fill pptr%high, and pptr%low with the appropriate cleanup values.

!	Written:

!	Glenn Sembroski
!	16/6/95
!	Purdue.

!	Modified:

!	09/2/98 GHS V:1:1:5:1:1:1.1
!		Convert to F90.  Also try to make a little more efficient.

!	04/3/98 GHS V:1:1:5:1:1:1.2
!		Generate pptr%high and pptr%low here.
!      19/12/02 GHS V:1:1:6:1:2:4.1 
!                Fix use of Pe_dc. Its the number of digital counts/pe.
!      28/05/03 GHS V:1:3:8:3:4:4.5
!                Since the scale and gains of things change when the ADC values
!                are determined from the FADC (V499 and TIM) we should 
!                determine the pedvars by generating noise streams with VPulse.
!                Do this in W10M_IMAGE_SIGMA since where it is needed is for 
!                the pprtr%high and pptr%low calculation for cleanup.
!      10/03/04 GHS V:1:3:8:3:4:4.14
!                Replace global pe_dc with jittered pe_dc/pmt
!
	use wcamera_def
	use whipple_telescope
	use kasaomega_def
	use W10M_PEPULSE_SUBS

	IMPLICIT NONE

	type(camera),pointer :: wptr
	type(camera_ptrs),pointer :: pptr

	integer :: i,j,n,error
	real :: ped,ped_ph,sum_ped,sum_ped2,ped_ph_mean,ped_ph_width2
	real :: pwidth
	integer :: nfluct
	real :: gauss,xdummy
	real,pointer,dimension(:) :: ped_widths

!Fill up array thats used in image cleanup as the image sigmas.
	if(index(options,'SIGMA').ne.0)then
				!Option 'SIGMA' is to read in sig's from
				!external file generated by GRANITE.
				!We need ADC cleanup levels from this
			!Should only be zero for dead pmts.
           do i=1,wptr%npmt
              if (pptr%active_pmts(i)/=0)then
! 19/12/02 GHS  Fix use of Pe_dc. Its the number of digital counts/pe.
                 pptr%high(i)=sqrt(pptr%wnoise(i))*hiclean*pptr%pe_dc(i)
                 pptr%low(i)=sqrt(pptr%wnoise(i))*loclean*pptr%pe_dc(i)
              else
                 pptr%high(i)=0
                 pptr%low(i)=0
              endif
           enddo
                                  !Veritas FADC generated pedvars
        elseif( (index(options,'TIM')/=0) .and. &
                (index(detector,"V499")/=0)       )then
           deallocate(ped_widths,stat=error)
           allocate(ped_widths(wptr%npmt),stat=error)
           if(error/= 0)then
              print*,'Program could not allocte space for ped_widths.'
              stop 'Failled to allocate space for ped_widths'
           endif
           call veritas_fadcpedwidths(wptr,pptr,ped_widths)
           pptr%high=ped_widths*hiclean*pptr%pe_dc              !vector arith
           pptr%low =ped_widths*loclean*pptr%pe_dc              !vector Arith
           print*,' Calculating FADC pedvars for hi/low cleanup using VPulse'
           print*,' PedVar for CHANNEL 1:  ',ped_widths(1)
           print*,' HIgh/Low CHANNEL 1:  ',pptr%high(1),'/',pptr%low(1)
        else
						!This is very slow!!!!!!!!
      print*,' Generating fluctuating noise sigmas for Cleanup. This is slow!'
!12/07/03 GHS
	   n=5000
           do i=1,wptr%npmt
				!Check for dead pmts
              if (pptr%active_pmts(i)/=0)then
                 sum_ped=0
                 sum_ped2=0
                 do j=1,n
                    ped=nfluct(pptr%wnoise(i))
!02/7/98 GHS Fix ped=0 calculation (needed for very low noise rates)
                    if(ped==0)then
                       pwidth=sqrt(phwidth_factor)
                    else
                       pwidth=sqrt(phwidth_factor*ped)
                    endif
                    ped_ph=ped+gauss(xdummy)*pwidth
                    if(ped_ph<0) then
                       ped_ph=0
                    endif
                    sum_ped=sum_ped+ped_ph
                    sum_ped2=sum_ped2+ped_ph**2
                 enddo
                 ped_ph_mean=sum_ped/n
                 ped_ph_width2=((sum_ped2/n)-ped_ph_mean**2)

		!Squared width.
! 19/12/02 GHS  Fix use of Pe_dc. Its the number of digital counts/pe.

                 pptr%high(i)=sqrt(ped_ph_width2)*hiclean*pptr%pe_dc(i)
                 pptr%low(i)=sqrt(ped_ph_width2)*loclean*pptr%pe_dc(i)
	      else
				!Zero dead PMT's
                 pptr%high(i)=0
                 pptr%low(i)=0
	      endif
           enddo
	endif
	return
      END SUBROUTINE W10M_IMAGE_SIGMA
!******************************************************************************

    SUBROUTINE W10M_INI
!******************************************************************************
!	This routine_ initalizes whats needed for Whipple 10m processing
!******************************************************************************

!	Written:03/11/93 G.H.S.

!	Modified:

!	09/2/98 GHS V:1:1:5:1:1.1.1
!		Convert to F90.

!	21/5/98 GHS V:1:1:5:1:2:1.0
!		Added NEW_CONCENTRATION to input parameter file. Used in new
!	versions of WCAMERA.F90 code for VERITAS. Better lightcone modeling.
!	Change how we determine efficiency. We now use the ratio of the new
!	(determined by parmeteres NEW_REFLEECTIVITY and NEW_CONCENTRATION) to
!	the orginally appled efficiency determed by MHEAD%REFLECTIVITY and
!	MHEAD%CONCENTRATION. Introduce a new routine W10M_EFFICIENCY_RATIO_GEN
!	to do this. In order to be compatable with shared routines(bewteen
!	KASTRIGGER and KASAOMEGA) replace the old MHEAD%REFLECTIVITY and
!	MHEAD%CONCENTRATION by the new one but save the old ones in 
!	OLD_REFLECTIVITY AND OLD_CONCENTRATION.

!	01/12/99 GHS V:1:1:6:1:2:3.2
!		Add 490 pixel camera initalization.
!	22/3/00 GHS V:1:1:6:1:2:3.4
!		Add 1024 square pixel camera initalization.
!       21/07/03 GHS V:1:3:8:3:4:4.6
!               Add stuff to detewrmine if the detector is the v499 prototype.
!               Sets flag to that effect, that flag used by VHDF5Out (in 
!               Veritas.cpp) to reorder channells. Thats the only place we use
!               this flag, otherwise the prototype is simulated as a 499 
!               camera with half the pmts turned off in the hrcXX.coff and
!               hrcXX.hvoff files.
!        29/03/04 GHS V:1:3:8:3:4:4.16
!                Add a call to new routine W10M_PEPULSE_INIT in W10M_INI
!                to create the single pepulse array with appropriate rise time.
!                Only use for w10m cameras with the TIM option requested 
!                (Veritas cameras do this on their own). This replaces the 
!                fixed rise time pulse used previously.

!	Files:
!	unit=11	Ped width file for use in image cleanup(4.25/2.25 stuff)opt='W'
!	unit=10 Sigma width file(option 'SIG')

	use whipple_telescope
	use wcamera_def
	use wcamera_subs
	use kasaomega_def
	use record_defs
	use W10M_PEPULSE_SUBS
	IMPLICIT NONE

	type(camera),pointer :: wptr
	type(camera_ptrs),pointer :: pptr
	integer :: j

        write(6,1004)pe_dc
1004	format('            Mean Pe to Digital Counts(pe_dc) = ',f8.3, &
             & ' digitalcounts/pe')
        write(6,1008)pe_dc_sigma*pe_dc
1008	format('         Sigma of  Pe_dc (pe_dc_sigma*pe_dc) = ',f8.3, &
             & ' digitalcounts/pe')
        write(6,1009)ped_sigma
1009	format('                   Sigma of  Ped (ped_sigma) = ',f8.3, &
             & ' digitalcounts')

!	Calculate focal plane conversion factors. (meters/deg.)
!	Note that focal plane is not linear . Its got that tan(theta) factor
!	which for small angles is close to theta. Error is small.<1%,
        if(index(detector,"V499")/=0)then
           focal_length=veritas_focal_length
        else
           focal_length=w10m_focal_length
        endif

	meters_per_deg=1./atand(1./focal_length)

	write(6,1010)focal_length,meters_per_deg
1010	format(	  &
	 & '                      Telescope Focal Length = ',f10.4,/, &
	 & ' Focal plane conversion factor in meters/deg = ',f10.4)



	if(hmult.ne.mhead%hmult)then
		write(6,1000)hmult,mhead%hmult
1000	format( &
	&  '      New Trigger Multiplicity Imposed =',i10,'  over old =',i10)
		mhead%hmult=hmult
	endif


	if(index(options,'TPES').ne.0)then
           write(6,1001)'photo-electrons',pe_threshold,mhead%pesum
        else
           write(6,1001)'pedestal Sigmas',pe_threshold,mhead%pesum
        endif
1001    format( &
        & 'Pixel Threshold New(in ',a,') =',f10.4,'  Old (pes) ='f10.4)

	mhead%pesum=pe_threshold

	if(index(options,'AN').ne.0.and.new_noise.ne.mhead%rate_noise)then
		write(6,1002)new_noise,mhead%rate_noise
1002	format(   &
	& '                 New noise rate imposed =',f10.4,' over old ='f10.4)
		mhead%rate_noise=new_noise
	endif

	if(swindow.ne.mhead%disc_width)then
		write(6,1003)swindow,mhead%disc_width
1003	format(   &
	& ' New Discriminator window width imposed =',f10.4,' over old ='f10.4)
		mhead%disc_width=swindow
	endif

!	21/5/98 GHS
!	If we are changeing the reflectivity and/or the concentration it
!	affects several things (in the folowing order):
!	pptr%efficiency will change from what was used in KASTRIGGER.
!	  This changes:
!		A: In WCAMERA_GEN Pixel skyshine noise rate(WNOISE). Which 
!		   affects threshold, ped_width etc.
!		B: In W10m_trigger pptr%efficiency is alpplied to disc.
!		   DISC was already modified by pptr%efficency in KASTRIGGER
!		   So we need to modify our pptr%efficiency at this point
!		   to just reflect the ratio of the new pptr%efficiency to the
!		   old.
!	To accomplish all this and stay compatable with the shared code
!	WCAMERA_GEN and W10M_TRIGGER we will now(before the call to
!	WCAMERA_GEN) insert the new values of reflectivity/concentration into
!	MHEAD and save the old values temporarily.
!	After the call to WCAMERA_GEN we will then modify pptr%efficiency to be
!	the ratio of the old to the a new routine W10M_EFFICIENCY_RATIO_GEN
!	The call is made from within this routine.


	if(new_reflectivity.ne.mhead%reflectivity)then
		write(6,1005)new_reflectivity,mhead%reflectivity
1005	format(   &
	& '        New PE Efficiency Factor in use =',f10.4,' over old =', &
        &f10.4)
		old_reflectivity=mhead%reflectivity
		mhead%reflectivity=new_reflectivity
	else
		old_reflectivity=mhead%reflectivity
	endif
	if(new_concentration.ne.mhead%concentration)then
		write(6,1006)new_concentration,mhead%concentration
1006	format(   &
	& '       New PE Concentration Factor in use =',f10.4,' over old =', &
        & f10.4)
		old_concentration=mhead%concentration
		mhead%concentration=new_concentration
	else
		old_concentration=mhead%concentration
	endif




	if(new_adc_gate.ne.mhead%adc_gate)then
		write(6,1007)new_adc_gate,mhead%adc_gate
1007	format(   &
	& '       New ADC gate (ns) in use =',f10.4,' over old =',f10.4)
		mhead%adc_gate=new_adc_gate
	endif

!	Set up the PMT positions; ONly allow one camera at a time.
!	This also sets up the LINES(Not really needed) and ADJACENT arrays.


!***************************************************************************
!	37 Pixel Camera Initialization
!***************************************************************************
 	if(index(detector,'W37').ne.0)then
		print*,'   ****37 PMT Camera Initalized'
 print*,'                 Number of inner pixels in trigger =',w37%ntrigger_in
		wptr =>w37
		pptr =>p37
		call wcamera_gen(wptr,pptr,mhead)
		call w10m_generate(wptr,pptr)
!109 Camera
!***************************************************************************
!	109 Pixel Camera Initialization
!***************************************************************************
 	elseif(index(detector,'W109').ne.0)then
		print*,'   ****109 PMT Camera(10m hrc style) Initalized'
 print*,'                 Number of inner pixels in trigger =',w109%ntrigger_in
		wptr =>w109
		pptr =>p109
		call wcamera_gen109(mhead)	!Loads up common block arrays.
		call w10m_generate(wptr,pptr)

!***************************************************************************
!	151 Pixel Camera Initialization
!***************************************************************************
 	elseif(index(detector,'W151').ne.0)then
		print*,'   ****151 PMT Camera Initalized'
 print*,'                 Number of inner pixels in trigger =',w151%ntrigger_in
		wptr =>w151
		pptr =>p151
		call wcamera_gen151(mhead)
		call w10m_generate(wptr,pptr)

!***************************************************************************
!	271 Pixel Camera Initialization
!***************************************************************************
 	elseif(index(detector,'W271').ne.0)then
		print*,'   ****271 PMT Camera Initalized'
 print*,'                 Number of inner pixels in trigger =',w271%ntrigger_in
		wptr =>w271
		pptr =>p271
		call wcamera_gen(wptr,pptr,mhead)
		call w10m_generate(wptr,pptr)

!***************************************************************************
!	331 Pixel Camera Initialization
!***************************************************************************
 	elseif(index(detector,'W331_125').ne.0)then
	print*,'   ***331 PMT Camera Initialized:.125 deg. pixel spacing'
 print*,  &
 & '                 Number of inner pixels in trigger =',w331_125%ntrigger_in
		wptr =>w331_125
		pptr =>p331_125
		call wcamera_gen(wptr,pptr,mhead)
		call w10m_generate(wptr,pptr)

 	elseif(index(detector,'W331_1875').ne.0)then
	print*,'   ***331 PMT Camera Initialized:.1875 deg. pixel spacing'
 print*,  &
 & '                 Number of inner pixels in trigger =',w331_1875%ntrigger_in
		wptr =>w331_1875
		pptr =>p331_1875
		call wcamera_gen(wptr,pptr,mhead)
		call w10m_generate(wptr,pptr)

 	elseif(index(detector,'W331_25').ne.0)then
		print*,'  ****331 PMT Camera Initalized:.25 deg. pixel spacing'
 print*,  &
   & '                 Number of inner pixels in trigger =',w331_25%ntrigger_in
		wptr =>w331_25
		pptr =>p331_25
		call wcamera_gen(wptr,pptr,mhead)
		call w10m_generate(wptr,pptr)

!***************************************************************************
!	490 Pixel Camera Initialization
!***************************************************************************
 	elseif(index(detector,'W490').ne.0)then
		print*,'   ****490 PMT Camera Initalized'
 print*,'                 Number of inner pixels in trigger =',w490%ntrigger_in
		wptr =>w490
		pptr =>p490
                if(index(options,'TIM'))then
                   call w10m_pepulse_init
                endif
                call wcamera_gen490(wptr,pptr,mhead)
		call w10m_generate(wptr,pptr)

!***************************************************************************
!	1024 square Pixel Camera Initialization
!***************************************************************************
 	elseif(index(detector,'SQ1024').ne.0)then
		print*,'   ****1024 Square Pixel Camera Initalized'
 print*,'                 Number of inner pixels in trigger =', &
	&    sq1024%ntrigger_in
		wptr =>sq1024
		pptr =>psq1024
		call wcamera_gen1024(wptr,pptr,mhead)
		call w10m_generate(wptr,pptr)

!***************************************************************************
!	541 Pixel Camera Initialization
!***************************************************************************
 	elseif(index(detector,'W541').ne.0)then
	    print*,'    ****541 PMT Camera Initialzed'
 print*,  &
   & '                 Number of inner pixels in trigger =',w541%ntrigger_in
	
 print*,  &
   & '                    541 Trigger Multiplicity(inner) =',mhead%hmult
		wptr =>w541
		pptr =>p541
		call wcamera_gen(wptr,pptr,mhead)
	    	call wcamera_gen541	!Loads up table lookup arrays.
		call w10m_generate(wptr,pptr)

!************  End of 541 Camera Initialization ****************************
!499 Camera
!***************************************************************************
!	499 Pixel Camera Initialization
!***************************************************************************
 	elseif(index(detector,'V499').ne.0)then
		wptr =>v499
		pptr =>pv499
		v499detector=.true.
                call v10m_generate499(wptr,pptr)
                !See if this is the VERITAS prototype. (Only affects VHDF5Out
                !in Veritas.cpp)

                if(index(detector,'V499=prototype').ne.0)then
                   v499prototype=.true.
                endif
	endif

	return
    END SUBROUTINE W10M_INI
!******************************************************************************

    SUBROUTINE W10M_PROC(WPTR,PPTR)
!******************************************************************************
!	This IS A GENERIC routine_ for procesessing the area triggers for the 
!	Whipple  10m detector.
!******************************************************************************
!	It has provisions for applying the super-cut filter and then
!	calculating the hillas parameters.
!	Cuts can then be made these parameters.
!	See W541_proc for notes on camera model.
!	See W541_proc for notes on options allowed.
!	Modified:

!	6/11/97 GHS  V:1:1:4:1:1:1.16
!		Convert the IM option to making just a single ntupple file
!		with multiple ntupples. This is all for use with the CERN
!		paw/hbook/cernlib etc stuff. I have a pixel.kumac file that
!		runs under PAW to read this file and display events.
!		This is only implimented in W331_PROC for now.

!	09/2/98 GHS V:1:1:5:1:1.1.1
!		Convert to F90.

!	07/5/98 GHS V:1:1:5:1:1.1.5
!		In order to have the flexability to have the long axis of
!		our areas for the VERITAS triangular grid we have added new 
!		options to petype(in KASLITE):
!		'EW' and 'NS'. EW gives us the long axis in the east=west
!		direction(ie along the rows) with alternating rows offset in
!		the East-west direction by xseg/2.
!		               NS gives us the long axis in the North-South
!		direction(ie along the columns) with alternating columns offset
!		in the North-South direction by yseg/2.  This is used to allow
!		us to put the long axis of the area in the direction of 
!		inclination of the showers. Sometimes it is desirable to have 
!		them inclined from the South, and sometimes form the west for 
!		example.
!			If neither option is specified. 'EW' is assumed. 
!	NOTE:as of 29/6/98 mhead%pe_head%petype was limited to 4 char and so
!		the NS option got left behind. Force NS option for now.

!	01/7/98 GHS V:1:1:5:1:2:1.4
!		Make changes for multiplcity and cluster trigger levels.
!		Extend max_pe,maxpe_in to 2 through 5 multiplicity. e have to 
!		convert h_ntp%maxpe and maxpe_in to an arrays of 4 elements.
!		Remove imax arrays from code and from h_ntp. Remove intel from
!		h_ntp.
!		Add a W10M_CLUSTER routine.

!	10/7/98 GHS V:1:1:5:1:2:1.5
!		Add mirror plane variable:HNTP%EMALTMPL,HNTP%XMPLN,HNTP%YMPLN
!		Calculated in W10M_PROC. Also, if we dont have an image
!		determine HNTP%XMEAN and HNTP%YMEAN  from adc values of
!		triggered pixels.

!	18/4/99 GHS
!		In order to study the detailed timing of the multiplicity
!		trigger and the PST KASTRIGGER now saves the times of each
!		pe in an event in an I*2 array as part of the 'TI' option.
!		The data format for the 'TI' option has thus changed. 
!		Change readin of data file for 'TI option. Unpack and save
!		using W10M_PEPULSE_ACCUMULATE.

!	13/5/99 GHS V:1:1:5:1:2:1.7
!		Add 'DEAD TUBES' capability. Look for 'DEADT' option. Read in a
!		file of dead tubes. Use that to deactivate bad PMTS in 
!		W10M_TRIGGER. Flages in new array PPTR%ACTIVE_PMTS.

!	26/5/99 GHS V:1:1:5:1:2:1.7
!		Add 'GAIN CORRECTION' capability. Look for 'GAIN' option. Read
!		in a file of pmt gains. Use that to modify pptr%threshold to
!		reflect differeing thresholds in pmts in real camera.
!		This should work for both W10M_PEPULSE and W10M_TRIGGER.
!		Use W10M_GAINS to do this.                                  

!	18/6/99 GHS V:1:1:5:1:2:1.7
!		Dead pmt's added to pepulse. ASlready worling for GAIN and
!		SIGMAS(I think).

!	29/6/99 GHS V:1:1:5:1:2:2.0
!		Convert to the use of an HDF file for input and Output. This is
!		done for portability of the data files between Linux and VMS
!		and because of problems linking in the Cern libraries with
!		the ABSOFT Fortran on LINUX. Later we will add MPI stuff.

!	07/10/99 GHS V:1:1:5:1:2:3.0
!		Convert the hdf4 version to one that reads and writes binary
!		files only. If we want ntuples of HDF4 or HDF5 files we can
!		write conversion programs.  Do this so we don't have to
!		worry about mixing libraries(CERN,MPI,HDF5 etc)
!		This is still a single processor version.(Not MPI!)

!	04/5/00 GHS V:1:1:6:1:2:3.6
!		Add code to generate an event file in HDF4 format readable
!		by Rod's data analysis. Later we can write a program that will
!		convert this file to a .fz file for distribution of MC events
!		to the collaboration. This HDF4 file will have lots of tag
!		variables(like shower energy, position,direction, particle
!		type , etc). MPI version only for now. Use parrallel IO as
!		for LAURA(shared write).
!		1:IN W10m_PROC we call OUTHDF4. For MPI version: if this is the
!		  OUTHDF4 daughter and we never come back. For all event
!		  processing daughters we defined the MPI data type event 
!		  structure and return.
!		2: Add a call to OUTHDF4_EVENT to W10M_PROC just after the
!		   ADC's are determined for a trigger event. OUTHDF4_EVENT will
!		   send event to OUTHDF4 daughter. Pnly event processing
!		   daughters reach this call.
!		3: Add a non=mpi (sngl) version of OUTHDF4 stuff
!		Don't forget to have OUTHDF4 daughter screw up the gains 
!		appropriatly.
!               But see 12/05/03
!	30/10/00 GHS V:1:1:6:1:2:3.8
!               Fix a bunch of bugs, most to do with array lengths and number 
!               of PMTS in the multiplicity trigger.
!                KASAOMEGA_SUBS:W10M_PROC
!                 1:Restrict multiplicity trigger to 379 small pixels for 490
!                   pixel camera in call to W10M_PEPULSE_ANALYZE and 
!                   W10M_PEPULSE_ADC..
!                 2:Set array limits for the 490 pixels camera(some of which 
!                   are 547 long) to 490.
!       06/05/01 GHS V:1:1:6:1:2:3.11
!               Use 'multiplicity_pixels' for calculation of maxm1,maxm2 etc.
!               This is 379 for 490 pixel camera. We were using 490 pixels.
!               This matches how CPARAM does things.
!               Make this correction in a few other places also.!
!       04/12/01 GHS V:1:1:6:1:2:3.13
!               Set Multiplicity _pixels to 463 for Veritas 449 camera.
!               For 449 use VERITAS_PEPULSE_ANALIZE instead of 
!               W10M_PEPULSE_ANALIZE.
!       15/02/02 GHSV:1:1:6:1:2:3.14
!               Add option "OUTVEvent" which causes a tagged simulated Vevent
!               to be written out. In W10m_proc call veritas_vevent_out which 
!               is a wrapper to call the C++ routine VeventOut.But see 12/05/03

!      10/05/02 GHS:1:1:6:1:2:3.15
!               Add a 'DISPLAY' option which causes ADC and hillas data for 
!               each event to be written to a binary file. For use with 
!               non-VERITAS event displays. VERITAS event displays will use 
!               standard VEvent/fpio files. Subroutine W10M_DISPLAY added. 
!               W10M_PROC.

!      19/12/02 GHS V:1:1:6:1:2:4.1 
!                Use intergized(dc) adc for cleanup and hillas calcs. Use real
!                for max1,max2,max3(so that when we multiply by pe_dc in paw
!                the histograms look ok).
!                Fix use of Pe_dc. Its the number of digital counts/pe.

!      22/04/03 GHS V:1:1:6:1:2:4.2
!                490 Pixel camera has only 331 pixels in both multiplicity and
!                PST trigger, not 379! Fix W10M_PROC
!                to use wptr%ntrigger_in. WCAMERA_DEF already has that.
!      28/05/03 GHS V:1:3:8:3:4:4.4
!                Add option 'TIM=FAST' for V499 camera. This option first runs
!                W10_TRIGGER on an event with threshold=pesum-1 and if it
!                passes that it then runs veritas_pepulse_analyze with
!                threshols =pesum and looks for the trigger there.
!      21/07/03 GHS V:1:3:8:3:4:4.7
!                Set up name for call to C++ routine to get weight to use for
!                an event.
!       20/08/03 GHS V:1:3:8:3:4:4.8
!               Add a call to VHDF5KascadeAttributes to add xseg and yseg 
!               as an attribute to the KASCADE dataset in the VHDF5 ouput file.
!       25/08/03 GHS V:1:3:8:3:4:4.9
!               For TIM=FAST make reduction of pptr%threshold for quick and 
!               dirty trigger check (W10M_TRIGGER) to 2 (used to be 1). At 1 
!               we miss ~25% of triggers.
!       18/02/04 GHS V:1:3:8:3:4:4.10
!               Add a WhippleEventWeight routine to Veritas.cpp. Put in a call
!               to it in W10m_proc.
!      10/03/04 GHS V:1:3:8:3:4:4.14
!                Replace global pe_dc with jittered pe_dc/pmt.
!                Add a pedestal error. pptr%peds.
!
!      03/05/04 GHS V:1:3:8:3:4:4.19
!                Convert W_HILLAS to look (and act) more like the 
!                get_hillasparameters in cparam.c
!                1:Add xo,yo offset source location arguments to W_HILLAS call.
!                6:Add xt,yt arrays to W_HILLAS argument list. This returns 
!                  estimated source location of event. 2 pairs of locations 
!                  for each estimation. First pair is for Lessard estimation 
!                  (xt(1),yt(1) and xt(2),yt(2)) Add code to calculate this.
!                7:Add code to determine which pair of coords to use for 
!                  2-d estimation of source location. Uses asymmetry from 
!                  W_HILLAS call in W10M_PROC
!
!      20/10/04 GHS V:1:3:8:3:4:4.20
!                For use in random directions fro hadrons save ithphi index
!                from mrec%ithphi to hntp%ithphi. For use later in 
!                kasarraytrigger
!
!      15/06/04 GHS  V:1:3:8:3:4:4.26 
!                In W10M_PROC: for W490: After second call to W_HILLAS to 
!                determine correct assymetry decision, rotate where xo1,yo1,
!                xo2,yo2 to reflect the fact that the WHIPPLE 490 pixel 
!                camera is rotated by 180 + 7.306 degrees from having pixel 2 
!                on the + x axis.      
!

!dir$ name(veritaseventweight="VeritasEventWeight") 
!dir$ name(whippleeventweight="WhippleEventWeight") 
!dir$ name(vhdf5kascadeattributes="VHDF5KascadeAttributes") 

!!	use structures
	use wcamera_def
	use wcamera_subs
	use whipple_telescope
!IMAGE Not used in MPI.	
!	use image
	use kasaomega_def
	use record_defs
	use w10m_subs
	use W10M_PEPULSE_SUBS
	use kasaomega_sub2
        use kasaomega_cmn_sub

	IMPLICIT NONE

	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	type(hillas) :: hrec,temp_hrec
        type(sum_rec) :: srec
	type(h_ntp) :: hntp
	common/ntp/hntp			!Needed for use by ntupple stuff

        real,dimension(2000000) :: paw
        common /pawc/paw        	!for ntuple creation for IMAGE option 
					!only.

	logical :: idone
	real,pointer,dimension(:) ::disc,disc_pes,disc_pulse_height, &
             & disc_trigger
	real :: w_hits_in,w_hits_mid
	real,pointer,dimension(:) :: sky_mean_adc,adc,adc_clean,adc_int
 	integer :: error,nwords,i,j,m,k,n,norder
	real,pointer,dimension(:) :: sptr
	real,dimension(5) :: maxpe_top

	integer ::  nfluct			!Integer fluctuation.
	logical ::  w10m_image,mult_trigger,pst_trigger

!Variables for Special test of w10m_trigger effectiveness
	logical ::  mult_trigger_test,pst_trigger_test
        integer ::vtrig_count_test, w10m_count_test, vtrig_and_w10m_count_test


	integer :: neighbors_max,newtrig_drop,is

	real :: gauss,xdummy
	real*8 :: sumsig,sumxsig,sumysig
	real :: x_g,y_g,distm,xn,vn

        integer :: ith,iphi,ithphi,nxx,nyy
	real,pointer,dimension(:) :: adc_gain_correction
	real :: gain_error
        integer :: multiplicity_pixels,image_pixels
	logical :: first_time=.true.
        integer :: vhdf5varrayevent_index=-1 !Starts at -1 so if we DON'T ever
                                             !call VHDF5Out (where first call
                                             !will set to 0) and then add 1 to
                                             !convert from a C++ index to a 
                                             !count we get 0 events out as we 
                                             !should.
        integer :: ped_number                !Number of pedestal events 
                                             !written out
        integer :: display_event_count=0, &
             & jevent_counter=0
        real*4 ::  event_weight
        integer,pointer,dimension(:) :: aindex
        real :: xo,yo
        real,dimension(2) :: xt,yt,xtemp,ytemp
        integer :: ideb
        real,parameter :: w10m490rot= 7.306  !490 npixel camera is rotated by
                                            !this much (degrees)
        logical :: quiet=.false.
!****************************************************************************
!		Array allocations
!****************************************************************************
!	read an area from the input file. This version assumes input file
!	is a compressed summary file with timing.

   	deallocate(pptr%disc,stat=error)
   	allocate(pptr%disc(wptr%npmt,1),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for pptr%DISC."
	     stop 'Failled to allocate space for pptr%disc'
	endif

	disc=>pptr%disc(1:,1)

				!This is done for compatability with
				!w10m_pepuls subroutines.
	deallocate(pptr%pes_in_image,stat=error)
!	if(error/= 0)then
! 	     print*,"Program could not deallocte space for pptr%pes_in_image."
!	endif
	allocate(pptr%pes_in_image(1),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"pes_in_image."
	     stop 'Failed to allocate space for pptr%pes_in_image'
	endif

!****************************************************************************
!	Create arrays for PE timing stuff
!****************************************************************************
        deallocate(pptr%time_trig,stat=error)
        allocate(pptr%time_trig(wptr%npmt),stat=error)
        if(error/= 0)then
           print*,"Program could not allocte space for p",wptr%camera_id, &
                &  "time_trig."
           stop 'Failed to allocate space for pptr%time_trig'
        endif

        deallocate(pptr%trig_index,stat=error)
        allocate(pptr%trig_index(wptr%npmt),stat=error)
        if(error/= 0)then
           print*,"Program could not allocte space for p",wptr%camera_id, &
                &  "trig_index."
           stop 'Failed to allocate space for pptr%trig_index'
        endif
!****************************************************************************


	deallocate(disc_pes,stat=error)
	allocate(disc_pes(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for DISC_PES."
	     stop 'Failed to allocate space for disc_pes'
	endif
	deallocate(disc_pulse_height,stat=error)
	allocate(disc_pulse_height(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for DISC_PULSE_HEIGHT."
	     stop 'Failed to allocate space for disc_pulse_height'
	endif
	deallocate(disc_trigger,stat=error)
	allocate(disc_trigger(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for DISC_TRIGGER."
	     stop 'Failed to allocate space for disc_trigger' 
	endif
	deallocate(sky_mean_adc,stat=error)
	allocate(sky_mean_adc(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for SKY_MEAN_ADC."
	     stop 'Failed to allocate space for sky_mean_adc' 
	endif
	deallocate(adc,stat=error)
	allocate(adc(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for ADC."
	     stop 'Failed to allocate space for adc'
	endif
!   19/12/02 GHS Form intergized(dc) adc for cleanup and hillas calcs.
	deallocate(adc_int,stat=error)
	allocate(adc_int(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for ADC_INT."
	     stop 'Failed to allocate space for adc_int'
	endif
	deallocate(adc_clean,stat=error)
	allocate(adc_clean(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for ADC."
	     stop 'Failed to allocate space for adc_clean'
	endif


!Stuff for W10m_trigger
!ldisc_pes
	deallocate(pptr%ldisc_pes,stat=error)
	if(error/= 0)then
 	     print*,"Program could not deallocte space for pptr%ldisc_pes."
	endif
	allocate(pptr%ldisc_pes(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"ldisc_pes."
	     stop 'Failure to allocate space forpptr%ldisc_pes'
	endif

!ldisc_pulse_height
	deallocate(pptr%ldisc_pulse_height,stat=error)
	if(error/= 0)then
     print*,"Program could not deallocte space for pptr%ldisc_pulse_height."
	endif
	allocate(pptr%ldisc_pulse_height(wptr%npmt),stat=error)
	if(error/= 0)then
	     print*,"Program could not allocte space for p",wptr%camera_id, &
		&  "ldisc_pulse_height."
	     stop 'Failure to allocate space for pptr%ldisc_pulse_height'
	endif

!ldisc_trigger
	deallocate(pptr%ldisc_trigger,stat=error)
	if(error/= 0)then
 	     print*,"Program could not deallocte space for pptr%ldisc_trigger."
	endif
	allocate(pptr%ldisc_trigger(wptr%npmt),stat=error)
	if(error/= 0)then
  print*,"Program could not allocte space for p",wptr%camera_id,"ldisc_trigger."
	     stop 'Failure to allocate space for pptr%ldisc_trigger'
	endif
	
	deallocate(aindex,stat=error)
	allocate(aindex(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for AINDEX."
	     stop 'Failed to allocate space for aindex'
	endif




!****************************************************************************
!	LAURA OPTION:
!****************************************************************************
        If(index(options,'LAURA')/=0)then
		deallocate(pptr%laura_adc,stat=error)
		allocate(pptr%laura_adc(wptr%npmt),stat=error)
		if(error/= 0)then
 		     print*,"Program could not allocte space for LAURA_ADC."
		     stop  'Failed to allocate space for pptr%laura_adc'
		endif
	endif
!****************************************************************************

					!get extra amount of noise in ADC gate
					!over that already found in disc window
	sky_mean_adc(1:wptr%npmt)=pptr%wnoise(1:wptr%npmt)* & !Vector arith
                     & (mhead%adc_gate-mhead%disc_width)/mhead%adc_gate


!debug
!****************************************************************************
!	ADC gain error correction 
!****************************************************************************
!	deallocate(adc_gain_correction,stat=error)
!	allocate(adc_gain_correction(wptr%npmt),stat=error)
!	if(error/= 0)then
! 	     print*,"Program could not allocte space for ADC_GAIN_CORRECTION"
!	     stop  'Failed to allocate space for adc_gain_correction'
!	endif
!	gain_error=.3			!30% gain determination error width
!	do i=1,wptr%npmt
!	   do
!		adc_gain_correction(i)=gauss(xdummy)*gain_error 
!		if(abs(adc_gain_correction(i))>2*gain_error)then
!			cycle
!		else
!			adc_gain_correction(i)=	adc_gain_correction(i)+1
!			exit
!		endif
!	   enddo
!	enddo
!******************************************************************************

!enddebug
!****************************************************************************
!	Make VHDF5 PEDS Dataset:peds,pedvars,pairpedvars,gains,tubesoff
!****************************************************************************
        if(index(options,'OUTVHDF5').ne.0)then
           call vhdf5_peds_out(wptr,pptr,v499prototype,v499detector)
                                      !Works for both Whipple
                                      !and VERITAS simulated data. GHS 08/03/04
        endif


        forever_loop:do					!Main event loop.
 
!******************************************************************************
!		Event Input
!******************************************************************************
!		Note: IN MPI version parent never returns from this until
!			input file is done.
           if(index(options,'INFILE=BIN')/=0)then

!	07/10/99 GHS Convert to binary cmSum_in.dat input.
              call cmsum_read_area(srec,disc,wptr,pptr,idone)
           else
             !31/10/00 GHS CMSUM input file assumed to be in HDF5 format.
              call cmsum_hdf5_read_area(srec,disc,wptr,pptr,idone)
!debug
!         print*,' mhead%pe_head%segment_head%tep:', &
!              & mhead%pe_head%segment_head%tep
!enddebug
           endif
           if(idone)then
              exit
           endif
           jevent_counter=jevent_counter+1
           if(mod(jevent_counter,100000).eq.0)then
              print*,'KASAOMEGA at event,tep,idfile ',jevent_counter, &
                   & mhead%pe_head%segment_head%tep, &
                   & mhead%pe_head%segment_head%idfile 
           endif
!******************End Event Input*******************************************



!*****************************************************************************
!	Search for triggers
!*****************************************************************************
!12/02/01 GHS For 490 camera limit trigger check to small pixels:
!04/12/01 GHS For Veritas 499 pixel camera just use the 463 PST pixels.
           if(wptr%camera_id=='490')then
              multiplicity_pixels=wptr%ntrigger_in  !331 pixels in mult trig.
              image_pixels=379
           elseif(wptr%camera_id=='499')then
              multiplicity_pixels=wptr%ntrigger_in
              image_pixels=wptr%npmt
           else
              multiplicity_pixels=wptr%ntrigger_in
              image_pixels=wptr%npmt
           endif
				!See if we want pulse derived triggers and that
				!we have the timing information to do it.      
           if(first_time)then
              first_time=.false.
              write(6,1000)' Event Trigger requires both &
                   & Multiplicity and PST trigger'
1000	format(' ',a)
              if(index(options,'TIM')/=0)then
                 if(wptr%camera_id=='499')then
                    write(6,1000)' Pulse building Veritas_PePulse_Analize &
                         & used for trigger determination(slow, C++ Version).'
                 else
                    write(6,1000)' Pulse building W10M_PEPULSE_ANALIZE & 
                         & used for trigger determination(slow).'
                 endif
              else
                 write(6,1000)' Non-pulse building W10M_TRIGGER  & 
                      & used for trigger determination(fast).'
              endif

           endif
!See if we have a trigger
           ithphi=1
           if(index(options,'TIM')/=0)then
              !Assume all pixels in trigger.
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Note that this call to W10M_PEPULSE_ANALYZE (and the one to 
! VERITAS_PEPULSE_ANALYZE) modifies pptr%disc by the
!efficiency factor!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              if(wptr%camera_id=='499')then
!FAST Test:  As far as I can tell w10m_trigger does not change pptr%disc 
! (beyond dead tube stuff). So it should be ok to run this first (before 
!veritas_pepulse_analyze)
                 if(index(options,'TIM=FAST')/=0)then

                    pptr%threshold=pptr%threshold-2               !Vector arith
                    call w10m_trigger(wptr,pptr,ithphi, &
                      & multiplicity_pixels,mult_trigger_test,pst_trigger_test)
                    pptr%threshold=pptr%threshold+2               !Vector arith
              
                    if(mult_trigger_test.and.pst_trigger_test)then
                       w10m_count_test=w10m_count_test+1
                      !veritas_pepulse_analyze does change pptr%disc 
                      !(efficiency correction)
                       call veritas_pepulse_analyze(wptr,pptr,ithphi, &
                         &multiplicity_pixels,mult_trigger,pst_trigger, &
                         & mhead%adc_gate)
                       if(mult_trigger.and.pst_trigger)then
                          vtrig_count_test=vtrig_count_test+1
                       endif
                    else
                       mult_trigger=.false.
                       pst_trigger=.false.
                    endif
  !End FAST Test
 
                 else
                    call veritas_pepulse_analyze(wptr,pptr,ithphi, &
                         &multiplicity_pixels,mult_trigger,pst_trigger, &
                         & mhead%adc_gate)
                 endif
!End V499 camera

              else
                 call w10m_pepulse_analyze(wptr,pptr,ithphi, &
                      & multiplicity_pixels,mult_trigger,pst_trigger, &
                      & mhead%adc_gate)
              endif

           else
              !Updated  non-timing trigger model.
              call w10m_trigger(wptr,pptr,ithphi, &
                   & multiplicity_pixels,mult_trigger,pst_trigger)
           endif

!	print*,'Daughter testing for trigger'
           if(mult_trig_type)then
              if((.not.mult_trigger).or.(.not.pst_trigger))then
                 call newtrig(newtrig_drop)
!                 print*,' Before cycle forever_loop'
                 cycle forever_loop		!Next event!!!!!!!!!!!
              endif
           elseif(.not.pst_trigger)then
              call newtrig(newtrig_drop)
              cycle forever_loop			!Next event!!!!!!!!!!!
           endif
!***************End of TRIGGER DETERNINATION***********************************

!*************************************************************************
!	ADC Generation
!*************************************************************************
!	Ok we have a trigger. Generate the ADC's
           if(index(options,'TIM')/=0)then
		!Generate the adc's from the pulse arrays using the pulse
		!arrays. Use the timing from the trigger to determine adc gate
		!position.
              if(wptr%camera_id=='499')then
                 !cycle
                 call veritas_pepulse_adc(wptr,pptr,adc,multiplicity_pixels)
              else
                 call w10m_pepulse_adc(wptr,pptr,adc,multiplicity_pixels, &
                   & mhead%adc_gate)
              endif
           else
		!Old way.
			
              do i=1,wptr%npmt

!***************************************************************************
!	zero dead PMT's channels
!***************************************************************************
	       	 if(pptr%active_pmts(i)==0)then
			adc(i)=0
			cycle
		 endif
!***************************************************************************

	         adc(i)=pptr%ldisc_pes(i)+nfluct(sky_mean_adc(i))
					!and jitter it, and remove sky 
					!generated pedistal
	        adc(i)=adc(i)+gauss(xdummy)*sqrt(phwidth_factor*abs(adc(i)))- &
	 &  pptr%sky_mean_disc(i)-sky_mean_adc(i)
             enddo
          endif

          if(index(detector,'V499')==0)then
!   12/05/03 GHS VERITAS Camera has no limits on ADC.(actually it does but
!                it is imposed in VPulse::Fadc)and applies its own pedc.
!   19/12/02 GHS Form intergized(dc) adc for cleanup and hillas calcs.
!                Fix use of Pe_dc. Its the number of digital counts/pe.
             adc=adc*pptr%pe_dc     !Convert to dc               !Vector Arith
                 !high/low already in dc for cleanup
             !add in pedestal jitter to match real data 

                 do i=1,wptr%npmt
                if(adc(i).gt.0)then   !make sure this is an active channel
                   adc(i)=adc(i)+pptr%ped(i)
                endif
             enddo             

             where (adc>=1024)  
                adc=1023
             endwhere

!the V499 already has applied the pe_dc values in veritas_pepulse

          endif

                                !Don't worry obout negative adc, cleanup 
                                !and hillas paramater calc ignore.
                                !Negative ADC (before pedestal added)
                                !needed for display output
          adc_int=int(adc)  !Convert to integers               !Vector Arith
                          ! for use with hillas param calc.


!**************End ADC Generation********************************************
!*************************************************************************
!            Write out simulated single telescope VArrayEvent into a 
!            VHDF5RunData formatted HDF5 file. 
!*************************************************************************
! Note:We write out an event if it causes a trigger. It doesn't matter if we 
!can get HIllas parama or not, triggered events are always written out.
! Note: The event number is the index in the VHDF5 data file and is returned 
!here. Used by the veritas_vhdf5_mc_out to specifiy the MC record location
!VERITAS:Indicate to VERITASVHDF5Out if this is a prototype file and need to 
!have the adc hitpattern,cfd pattern,and adc channels reordered. Ignore 
!reordering the pst's for now(since I haven't any idea how to do that!).
!WHIPPLE: Call th WHIPPLE VHDF5OUT routine instead.**********************
!**************************************************************************
            if(index(options,'OUTVHDF5').ne.0)then
               if(index(detector,"V499").ne.0)then
                  call veritas_vhdf5_out(vhdf5varrayevent_index,wptr,pptr, &
                       & v499prototype)
               else

                 call whipple_vhdf5_out(vhdf5varrayevent_index,wptr,pptr, &
                       adc, ped_number)
              endif
           endif
!*************************************************************************



!****************************************************************************
!	Generate Hillas parameters
!****************************************************************************
				!See if we are to make hillas parameters
          if(index(options,'HPARAM').ne.0)then

!	Determine the hillas parameters.
!	Before determining the HIllas parameters we need to clean up the image
!	useing the old 2.25, 4.25 sigma boundry,bright scheme.
!       ADC's and sigmas are in pes not dc.
             adc_clean=0					!vector arith.
!SPECIAL for 541
             if(wptr%camera_id=='541')then
!   19/12/02 GHS Use the intergized(dc) adc for cleanup and hillas calcs.
                call WCAMERA_CLEANUP(adc_int,adc_clean,wptr,pptr, &
                     & hrec%ibright,hrec%iboundry,neighbors_max,511)
             else
               call WCAMERA_CLEANUP(adc_int,adc_clean,wptr,pptr,hrec%ibright, &
                    & hrec%iboundry,neighbors_max,image_pixels)

            endif

!	Now determine hillas parameters for the cleaned image.
!	Make sure we have enough pixels after cleanup to do the job. 

            if(hrec%ibright+hrec%iboundry>=2)then
               w10m_image=.true.
	!Limit to adc range.But adc_clean is in dc
        !(or at least made into integers)
               xo=0.0
               yo=0.0
               call w_hillas(pptr%camera_x(1:image_pixels),  &
                   & pptr%camera_y(1:image_pixels),&
                   & adc_clean(1:image_pixels),hrec,xo,yo,xt,yt)

!***************************2-d analysis *******************************
!           determine source location
!***********************************************************************
!To determine which of the xt,yt pair to use as the source location we are
!going to call w_hillas again (using a temp_hrec and xtemp,ytemp) and using 
!xt(1) and yt(1) as the xo,yo offset. The sign of the returned asymmetery (the
! only thing we really want from this second call to w_hillas) will
! tell us if we should use the second pair of xt,yt as the source location. 
!(cparam.c does this as a recusive call within get_hillas_parameters but I 
!think that makes things more difficulat to understand).               
               xo=xt(1)
               yo=yt(1)
               call w_hillas(pptr%camera_x(1:image_pixels),  &
                   & pptr%camera_y(1:image_pixels),&
                   & adc_clean(1:image_pixels),temp_hrec,xo,yo,xtemp,ytemp)
  !hrec%asym=temp_hrec%asym !Save asym relative to xt(1),yt(1)
!*****************************************************************************
! Rotate camera
!*****************************************************************************
!The whipple 490 pixel camera is rotated by 7.306 deg. Actually, cparam has
!pixel #2 at x=-.117 and y=0.15. That is it is actually rotated by 180 +7.306
!deg. Ignore the 180 for now.
               if(wptr%camera_id=='490')then
                 !xt(1)=-xt(1)         !rotate by 180 deg. Ignore for now.
                 !yt(1)=-yt(1)
                 !xt(2)=-xt(2)
                 !yt(2)=-yt(2)

                                      !Now rotate by another 7.306 deg
                 do i=1,2
                    xtemp(i)=xt(i)*cosd(w10m490rot)+yt(i)*sind(w10m490rot)
                    ytemp(i)=yt(i)*cosd(w10m490rot)-xt(i)*sind(w10m490rot)
                    xt(i)=xtemp(i)
                    yt(i)=ytemp(i)
                 enddo
              endif
!*****************************************************************************

               if(temp_hrec%asym>0)then
                                   !asymmetry relative to xt(1),yt(1) as origen
                   hntp%xo(1)=xt(1)  !Most likely
                   hntp%yo(1)=yt(1)
                   hntp%xo(2)=xt(2)
                   hntp%yo(2)=yt(2)
                else
                   hntp%xo(1)=xt(2)  !Most likely
                   hntp%yo(1)=yt(2)
                   hntp%xo(2)=xt(1)
                   hntp%yo(2)=yt(1)
                 endif

                   
            else
               w10m_image=.false.
            endif
	
!****************************************************************************
!		Preperation for Hillas ntuple production
!****************************************************************************
            hntp%tep=mhead%pe_head%segment_head%tep     !Energy in TeV
            hntp%type=mhead%pe_head%segment_head%itype  !Primary type
            hntp%id=mhead%pe_head%segment_head%idfile   !Shower id
            hntp%x_offset=mhead%pe_head%x_offset        !Core position
            hntp%y_offset=mhead%pe_head%y_offset
            
				!Determine mirror position relative to
					!central area(not core).Make provisions
					!for VERITAS triangular array.
            if(index(mhead%pe_head%petype,'VT').ne.0)then
!****************************************************************
!	NOTE:as of 29/6/98 mhead%pe_head%petype was limited to 4 char and so
!		the NS option got left behind. So look in the 'options' for
!		this program instead. Ie we have to repeat the NS or EW option
!		in the .par file for this program.
! 	       	    if(index(mhead%pe_head%petype,'NS').eq.0)then
!****************************************************************
               if(index(options,'NS').eq.0)then
                    !EW long axis
                  hntp%y=mrec%ny*mhead%pe_head%yseg   !Y(s-n) position
                  nyy=mrec%ny    	!Convert to integer from real
                  if(mod(nyy,2).eq.0)then
					!NY even :use standard conversion 
					!for x
                     hntp%x=mrec%nx*mhead%pe_head%xseg 
                       !X(e-w) position
                  else
					!NY odd. Shift NX center to right 1/2
					!of xseg.
                     hntp%x=(mrec%nx+.5)*mhead%pe_head%xseg !X(e-w)
                  endif
               else				!NS axis is long
                  hntp%x=mrec%nx*mhead%pe_head%xseg  !X(e-w) position
                  nxx=mrec%nx    	!Convert to integer from real
                  if(mod(nxx,2).eq.0)then
					!NX even :use standard conversion for y
                     hntp%y=mrec%ny*mhead%pe_head%yseg 
                       !Y(n-s) position
                  else
					!NX odd. Shift NY center up 1/2
					!of yseg.
                     hntp%y=(mrec%ny+.5)*mhead%pe_head%yseg !Y(n-s)
                  endif

!*************************************
               endif
            else
               hntp%y=mrec%ny*mhead%pe_head%yseg      !Y(s-n) position
               hntp%x=mrec%nx*mhead%pe_head%xseg       !X(e-w) position
            endif


            hntp%theta=mrec%itheta
            hntp%phi=mrec%iphi
            hntp%ithphi=mrec%ithphi

            hntp%neighbor=neighbors_max


				!Find biggest hits in pmts in that were
				!included in the trigger).
            norder=8
!get max1,max2,max3 etc for all pixels in pst trigger
            call w10m_adc_order_top(adc,wptr%ntrigger_in,hntp%max_p, &
                   &  norder,aindex)
			
!Get max1,max2 etc for all pixels used in image.
!This is what cparam uses for max1,max2,max3.
            call w10m_adc_order_top(adc,image_pixels,&
                   &hntp%max_p(norder+1:),norder,aindex)
            hntp%max_pe_in(1)=aindex(1)
            hntp%max_pe_in(2)=aindex(2)
            hntp%max_pe_in(3)=aindex(3)

            if(w10m_image)then
					!SIZE:only use cleaned up pixels to 
					!determine image size.
               if(hrec%size>0)then
                  hntp%size=hrec%size		
               else
                  print*,' Found event with size =0'
                  hntp%size=.0001    !Fake to prevent divide by 0
						   !in plotting programs.
               endif
               hntp%azwidth=hrec%azwidth
               hntp%width=hrec%width
               hntp%length=hrec%length
               hntp%dist=hrec%dist
               hntp%miss=hrec%miss
               if(hrec%dist>0) then
                  hntp%alpha=asind(abs(hrec%miss/hrec%dist))
               else
                  hntp%alpha=-1.
               endif
               hntp%bright=hrec%ibright
               hntp%boundry=hrec%iboundry
               hntp%w_hits_in=w_hits_in
               hntp%w_hits_mid=w_hits_mid
               hntp%xmean=hrec%xmean
               hntp%ymean=hrec%ymean
               hntp%alph=hrec%alph
               hntp%sdevxy=hrec%sdevxy
               hntp%asym=hrec%asym
               hntp%psi=hrec%psi
            else
!When we don't have enough pixels after cleanup use tirggered pixels instead to
!find center of mass of event.
               hrec%dist=-1		!Flag we got no cleaned up image.
               hntp%size=-1		!Another flag and allows use  in ntp
					!calculation of l/s just puts it off
					!the graph.
		
               hntp%dist=hrec%dist
						 !Sum of weights.!Vector Arith
               sumsig= sum(adc(1:image_pixels)*disc_trigger(1:image_pixels))
			           !Sum of weighted X positions  !Vector Arith
               sumxsig=sum(pptr%camera_x(1:image_pixels)*&
                      & adc(1:image_pixels)* &
                      & disc_trigger(1:image_pixels))
			           !Sum of weighted y positions  !Vector Arith
               sumysig=sum(pptr%camera_y(1:image_pixels)*&
                      & adc(1:image_pixels)* &
                      & disc_trigger(1:image_pixels))
               if(sumsig/=0)then
                  hrec%xmean=sumxsig/sumsig
                  hrec%ymean=sumysig/sumsig
               else
                  hrec%xmean=0
                  hrec%ymean=0
               endif
               hntp%xmean=hrec%xmean
               hntp%ymean=hrec%ymean

            endif

            hntp%emission_alt=mrec%emission_alt
            hntp%em_alt_sig=mrec%em_alt_sig
            hntp%muon_ratio=mrec%muon_ratio
							!Timing stuff
!When we use the 'TIM' option only get mult_trigger_time->median_time
! pst_trigger_time->fp_median_time and the event reference time ->time_cfd
! ntrigger->i_mult
!We get no threshold ,max cluster size, etc.

            hntp%median_time=mrec%median_time	!mult_trigger_time
            hntp%fp_median_time=mrec%fp_median_time !pst_trigger_time
            hntp%time_cfd=mrec%time_cfd		!Reference time.
            hntp%i_mult=mrec%i_mult			!ntrigger
            
            if(index(options,'TIM')/=0)then
               hntp%fp_time_cfd=0
               hntp%fp_threshold=0
               hntp%fp_mult=0
            else
               hntp%fp_time_cfd=mrec%fp_time_cfd
               hntp%fp_threshold=mrec%fp_threshold
               hntp%fp_mult=mrec%fp_mult
            endif

!************keep this for later*********
!				!get actual ground distance from mount to core.
!            x_g=hntp%x-hntp%x_offset
!            y_g=hntp%y-hntp%y_offset
!				!Project the these ground x,y coords to the 
!				! mount mirror plane. This is just dot porduct
!				! of x and y unit mirror plane unit vectors 
!				!with the ground plane position vector.
!				!(remember z of ground plane is 0)
!            ith=mrec%itheta
!            iphi=mrec%iphi
!            hntp%xmpln=x_g*xdlm(ith,iphi) + y_g*xdmm(ith,iphi)
!            hntp%ympln=x_g*ydlm(ith,iphi) + y_g*ydmm(ith,iphi)!!
!
!                		!now get the distance along the primary track
!				!from the 
!				!mirror plane to the emission altitude.
!			!NOTE this is not exactly the perpendicular distance
!			!from the shower-max to the mirror plane except for
!			!when ith=1,iphi=1(on axis)
!				!First distance along track from ground to
!				!emission altitude
!            hntp%emaltmpln=abs(hntp%emission_alt/dnm(1,1))
!				!subtract from that the  distance along the
!				!track from where the track crosses the mirror
!				!plane to where it hits the ground.
!				!The following is taken from WHIPPLE_TILT
!see note in main program on axis directions.
!
!        The length of the track from hobs to the tilted mount mirror plane is:
!        distm=-((DLm,DMm,DNm) . (xg,yg,zg))/((DLm,DMm,DNm) . (DL1,DM1,DN1))
!        (DLm,DMm,DNm) is normal to mount mirror plane pointed at sky.
!        The term in the denominater is the dot product of the normal to the
!        mirror plane and the direction of the photon(-mirror normal at
!	ith=0,iphi=0). Calculate it first. Its the relative dn of the photon.
!            vn=(dlm(ith,iphi)*dlm(1,1)+dmm(ith,iphi)*dmm(1,1)+ &
!                & dnm(ith,iphi)*dnm(1,1))	
!            if(abs(vn).ge.1.0)then
!               vn=1.0
!            endif
!       Now the numerater. This is perendicular distance from x_g,y_g,z_g=0
!!	to mirror plane of mount. Sign is important here.
!            xn=(dlm(ith,iphi)*x_g+dmm(ith,iphi)*y_g)        !z_g*dn=0.
!                Length of vector from hobs to intersection of mount plane.
!            distm=(xn/vn)  
!				!distace from emalt to mirror plane.
!            hntp%emaltmpln=hntp%emaltmpln-distm-mhead%pe_head%hobs/dnm(1,1)!
!
!****************************************************************************
           if(index(options,'OUTVHDF5').ne.0)then
               if(index(detector,'V499')/=0)then
                  call veritaseventweight(hntp%tep,hntp%type,event_weight,&
                       & quiet)
               else
                  call whippleeventweight(hntp%tep,hntp%type,event_weight,&
                       & quiet)
               endif
               call veritas_vhdf5_mc_out(vhdf5varrayevent_index, hntp, &
                    & event_weight)
            endif
!*************************************************************************

!*************************************************************************
!            Write out HILLAS paramter to ntupple file.
!*************************************************************************
            if(index(options,'HILLAS').ne.0)then
!Write out hillas parameters to binary file.
               call hillas_write(hntp)
            endif
!*************************************************************************

!!!!!!!END hillas ntuple stuff.**************************************
         endif	!end HPARAM stuff

        enddo forever_loop     !End of forever loop 

!	End of shower.

!*************************************************************************
!FAST Test Results
!*************************************************************************
     if(index(options,'TIM=FAST')/=0)then
         print*,'# FAST test w10m_trigger  triggers found:',w10m_count_test
         print*,'# FAST test veritas_pepulse_analyze triggers found:', &
              & vtrig_count_test
      endif
!*************************************************************************



!*************************************************************************
!        Write out xseg and yseg as an attribute of the KASCADE dataset.
!        Clean up the VHDF5 stuff.
!*************************************************************************
      if(index(options,'OUTVHDF5').ne.0)then
         call vhdf5kascadeattributes(mhead%pe_head%xseg,mhead%pe_head%yseg)
         call veritas_vhdf5_out_cleanup
         print*,' Pedestal VHDF5 Events written out:', &
            & ped_number
         print*,' Tagged VHDF5VarrayEvents written out:', &
            & vhdf5varrayevent_index-ped_number+1
      endif
!*************************************************************************

!*************************************************************************
!        Clean up the Display stuff.
!*************************************************************************
      if(index(options,'DISPLAY').ne.0)then
         print*,' Tagged events written out for DISPLAY:',&
            & display_event_count
      endif
!*************************************************************************
      return
    END SUBROUTINE W10M_PROC
!******************************************************************************

    SUBROUTINE W_HILLAS(wx,wy,weight,hrec,xo,yo,xt,yt)
!******************************************************************************
!	Calculate the hillas/akerlof parameters for an image.
!******************************************************************************

!	This can be used for both PMT pixel arrays and raw images(weights=1)


!	Modified:

!	09/2/98 GHS V:1:1:5:1:1.1.1
!		Convert to F90.

!	06/3/98 GHS V:1:1:5:1:1:1.3
!		Add calculation of assymetery parameter which include
!		calculation of psi angle between xaxis and major axis of 
!		ellipse.  This formulation for psi and asymmetry is from Rod 
!		Lessard thesis(nov 1996) which uses results from Micale 
!		Punche's thesis which. The results are also in Punche's Hillas
!		parameter derivation notes(you know, that smugged baddly 
!		coppied set of notes from his logbook that we all use)...
!        03/05/04 GHS V:1:3:8:3:4:4.19
!                Convert W_HILLAS to look (and act) more like the 
!                get_hillasparameters in cparam.c
!                1:Add and implement xo,yo offset source location arguments 
!                  to W_HILLAS call.
!                2:Fix error in psi calculation (been there for 6 years!!!)
!                3:Add slope and intercept calculation for use in 2-d analysis.
!                4:Add Lessard elongation factor parameter to 
!                  WHIPPLE_TELESCOPE. Used in W_HILLAS.
!                5:Add use of WHIPPLE_TELESCOPE to W_HILLAS
!                6:Add xt,yt arrays to argument list. This returns estimated
!                  source location of event. 2 piars of locations for each 
!                  estimation. First pair is for Lessard estimation 
!                  (xt(1),yt(1) and xt(2),yt(2)) Add code to calculate this.
!      29/03/05 GHS  V:1:3:8:3:4:4.24
!               Fix yet another bug in the asymmetry calculation in W_HILLAS
!               And fix a bug in the slope calculation.
!      29/03/05 GHS  V:1:3:8:3:4:4.25
!               The slope calculation in W_HILLAS has round off problems. Use
!               instead the psi calculation which is specially formulated to 
!               avoid those problems. This makes the xo1,yo1,xo2,yo2 
!               calculation much simpler. Code come from S.Fegans 
!               HillasParameterization.cxx and 
!               TwoDimensionalParameterization.cxx
!

      use structures
      use wcamera_def
      use whipple_telescope    !Has efactor(2-d analysis)

      IMPLICIT NONE

      real,dimension(:) :: wx,wy,weight
      type(hillas) :: hrec
      real :: xo,yo
      real,dimension(:) :: xt,yt

      real,pointer,dimension(:) :: xrel,yrel

      real*8,parameter ::  pi=3.141592654
      real*8 :: sumsig,sumxsig,sumysig,sumx2sig,sumy2sig,sumxysig
      logical :: weighted
      real*8 :: xmean,x2mean,xmean2,ymean,y2mean,ymean2,xymean,meanxy
      real*8 :: sdevx2,sdevy2,sdevxy,d,z,u,v,squared_miss,az2
      real*8 :: squared_width

!Asymmetry stuff	06/3/98 GHS V:1:1:5:1:1:1.3
      real*8 :: ac,bc,cc
      real*8 :: sumx3sig,sumx2ysig,sumxy2sig,sumy3sig
      real*8 :: x3mean,y3mean,x2ymean,xy2mean
      real*8 :: sdevx3,sdevy3,sdevx2y,sdevxy2
      real*8 :: cpsi,spsi,psi,asym
!     real*8 :: psio,psia,psi,asym
!2d stuff
!      real*8 :: slope,yint,,qa,qb,qc
      real*8 :: destimated
      integer :: i,k,error

      weighted=.false.	!Init flag that tells us if we are dealing with
 !pixels or pe's.
      deallocate(xrel,stat=error)
	
      allocate(xrel(size(weight)),stat=error)
      if(error/= 0)then
 	 print*,"Fatal-W_HILLAS-Program could not allocate space for xrel."
         stop  'Failed to allocate space for xrel in w_hillas'
      endif
      deallocate(yrel,stat=error)
      allocate(yrel(size(weight)),stat=error)
      if(error/= 0)then
 	 print*,"Fatal-W_HILLAS-Program could not allocate space for yrel."
         stop  'Failed to allocate space for yrel in w_hillas'
      endif
!debug
!        print*,'xo,yo: ',xo,yo
!        do i=1,379
!           if(weight(i).ne.0)then
!              print*,'i,adc: ',i,weight(i)
!           endif
!        enddo
!enddebug

!       Reset the source location from 0,0
      xrel=wx-xo                                                !Vector Arith
      yrel=wy-yo                                                !Vector Arith

!	Go through the points/pixels collecting the means etc.
      k=0
      sumsig=sum(weight)	     !Sum of weights.
      sumxsig = sum(xrel*weight)   !Sum of weighted X positions !Vector Arith
      sumysig = sum(yrel*weight)   !Sum of weighted Y positions !Vector Arith
      sumx2sig = sum(xrel*xrel*weight) !Sum of weighted X**2    !Vector Arith
      sumY2sig = sum(yrel*yrel*weight) !Sum of weighted Y**2    !Vector Arith
      sumxysig = sum(xrel*yrel*weight) !Sum of weighted X*Y     !Vector Arith

!Asymmetry stuff	06/3/98 GHS V:1:1:5:1:1:1.3
      sumx3sig = sum(xrel*xrel*xrel*weight) !sum weighted X**3  !Vector Arith
      sumy3sig = sum(yrel*yrel*yrel*weight) !sum weighted y**3  !Vector Arith
      sumx2ysig = sum(xrel*xrel*yrel*weight)!sum weighted x**2*y!Vector Arith
      sumxy2sig = sum(xrel*yrel*yrel*weight)!sum weighted x*y**2!Vector Arith

      if (maxval(weight)>1.0)weighted=.true.
 
!	Now calculate all the means and sigmas and things.
		!X first.
      xmean = sumxsig / sumsig	!Weighted mean of X
      x2mean = sumx2sig / sumsig	!Weighted mean of X**2
      xmean2 = xmean * xmean		!X mean squared
      !Then Y.
      ymean = sumysig / sumsig	!Weighted mean of Y
      y2mean = sumy2sig / sumsig	!Weighted mean of Y**2
      ymean2 = ymean * ymean		!Y mean squared
      !Now XY
      xymean = sumxysig / sumsig	!Mean of X*Y
      meanxy = xmean * ymean		!X*Y mean squared
      !Determine the sigmas. 
      
      sdevx2 = x2mean - xmean2	!Sigma**2 of X
      sdevy2 = y2mean - ymean2	!Sigma**2 of Y
      sdevxy = xymean - meanxy	!sigma of XY

!Asymmetry stuff	06/3/98 GHS V:1:1:5:1:1:1.3
      x3mean=sumx3sig/sumsig		!weighted mean of x**3
      y3mean=sumy3sig/sumsig		!weighted mean of y**3
      x2ymean=sumx2ysig/sumsig	!weighted mean of x**2*y
      xy2mean=sumxy2sig/sumsig	!weighted mean of x*y**2
      sdevx3=x3mean-3*xmean*x2mean+2*xmean**3 !Sigma of x**3
      sdevy3=y3mean-3*ymean*y2mean+2*ymean**3 !Sigma of y**3
      sdevx2y=x2ymean-2*xymean*xmean+2*xmean**2*ymean-x2mean*ymean
      !Sigma of x**2y
      sdevxy2=xy2mean-2*xymean*ymean+2*ymean**2*xmean-xmean*y2mean
      


		!Now make some terms used in dertermining parameters.
      d = sdevy2 - sdevx2		


!	Akerlof's LENGTH and WIDTH-suggested by CA 90/10/19
!	Thes tweo parameters tell us something about the shape of the image.
		!First a useful term.
      z = SQRT(d*d+4*sdevxy*sdevxy)
		!Length is RMS extent of image about the major axis of the
		!image. Major axis is defined as line through image that 
		!maximaizes LENGTH.

!****************************distance**************************************
!	DIST is distance to center of mass of image. Its one of the parameters.

      hrec%dist = SQRT(xmean2 + ymean2)

!**************************************************************************

!****************************length**************************************

      hrec%length = SQRT((sdevx2+sdevy2+z)/2.0)
		!Width is RMS extent of image about minor axis which is 
		!perpendicular to the major axis.
!**************************************************************************

!****************************width*****************************************

      squared_width=((sdevy2+sdevx2-z)/2.0)
      if(squared_width.ge.0)then
         hrec%width = SQRT((sdevy2+sdevx2-z)/2.0)
      else
         hrec%width = 0
      endif
!**************************************************************************



!	Now calculate some parameters that tell us about the orientation of
!	the image.

!****************************miss******************************************
		!MISS is the perpendicular distance form the Major axis(see 
		!above to the origen(or we could modify later to have it be to
		!the source position if its not at the origen.)
	!Formula for miss introduced CA, MP 90/11/01,revised MP 91/01/12
      if(z.eq.0.0)then	!major axis perendicular to origen(not likely).
         hrec%miss = hrec%dist
      else
         u=1+d/z
         v=2-u
         squared_miss=((u*xmean2+v*ymean2)/2.0-meanxy*(2.0*sdevxy/z))
         if(squared_miss.ge.0)then
            hrec%miss=SQRT(squared_miss)
         else
            hrec%miss=0.
         endif
      endif
!**************************************************************************

!******************sdevxy and alph for MPK 2-d analysis********************
      hrec%sdevxy=sdevxy
      hrec%alph = (d+z)/2.0	!For IMAGE production.
!**************************************************************************

!************************slope and intecept********************************
!  Used later in 2-d analysis
!Replaced by use of psi calculation below 10/05/05 GHS
!**************************************************************************
!        if(sdevxy==0)then
!           slope=999.     !Slope is verticle
!        else
! 31/03/05 GHS  Fix a bug in the slope calculation. 
!           slope=d+sqrt(d**2+4*sdevxy**2)/(2*sdevxy)  !wrong!
!           slope=(d+sqrt(d**2+4*sdevxy**2))/(2*sdevxy)
!       endif
!        yint=ymean-slope*xmean
!**************************************************************************


!***********************Asymmetry******************************************
!Asymmetry stuff	06/3/98 GHS V:1:1:5:1:1:1.3
!03/05/04  GHS fix error in psi ( its only been wrong for 6 years!)
!	psio= ((d+z)*ymean+2*sdevxy)
!	psia=(2*sdevxy-(d-z)*xmean)
!10/05/05 GHS Improve psi calculation (actually its the same, just code from 
!             S.Fegans HillasParameterization.cxx
      if( abs(sdevxy) > .00000001 )then  
                                      ! length  != width, semi-major axis 
                                     ! not along x or y
         ac=(d+z)*ymean + 2.0*sdevxy*xmean
         bc=2.0*sdevxy*ymean - (d-z)*xmean
         cc=sqrt(ac*ac+bc*bc);
         cpsi=bc/cc;
         spsi=ac/cc;
         psi=atan2(ac,bc)
    
      else if ( z > .00000001 )then ! semi-major axis along x or y 
                               ! and length != width
         if(sdevx2 > sdevy2)then
            cpsi=1.
            spsi=0.
            psi=pi/2.
         else
            cpsi=0.
            spsi=1.
            psi=0.
         endif
      else
         
         cpsi=1.
         spsi=0.
         psi=pi/2.
      endif


	!psio= ((d+z)*ymean+2*sdevxy*xmean)
	!psia=(2*sdevxy*ymean-(d-z)*xmean)
	!if(psio==0.and.psia==0)then
	!	psi=pi/2
	!else
	!	psi=atan2( psio,psia)
	!endif

      hrec%psi=psi    !save it.

      asym=sdevx3*cpsi**3+3*sdevx2y*spsi*cpsi**2 + &
              & 3*sdevxy2*cpsi*spsi**2+sdevy3*spsi**3

!      29/03/05 GHS:Fix yet another bug in the asymmetry calculation.
!I don't have any idea why this was here:
!comment it out:"asym=asym/hrec%length**3"  !Wrong!

!at this point asym is the cube of the asymmetry
! cparam.c uses a fancy exp(log(abs(asym))/3) to take the cube root of asym
! Thats because c doesn't have the ** ability. Fortran does. We use it.
! This form is exactly as shown in Lessards thesis.
	
         hrec%asym=abs(asym)**(1./3.)
 
!retain the sign.
         if(asym<0)then
            hrec%asym=-hrec%asym
         endif
!**************************************************************************

!********************Centroid**********************************************
!	09/8/96 GHS  V:1:0:2:1:2:1.13
		!Save center of mass of the image.
         hrec%xmean=xmean
         hrec%ymean=ymean
!**************************************************************************

!************************azwidth*******************************************
!	Now th infamous AZWIDTH. Use Akerlofs formulation. MP 91/01/12
!	AZWIDTH is a combined shape and orientation parameter(thats why its so
!	effective).
         d = y2mean - x2mean		!A different d then we had from hillas
         z = SQRT(d*d+4*xymean*xymean)	!And a differenet z.
         az2=((x2mean+y2mean-z)/2.0)	!Ta Da!
         if(az2.ge.0.0)then
            hrec%azwidth = SQRT(az2)	!Ta Da!
         else
            hrec%azwidth=0.
         endif
!**************************************************************************

!********************max1,max2,max3****************************************
!	The following is for pixels where the weights are different then 1,
!	That is for PMT's not individual pe's
	if(weighted)then
		!Find the pixels with the top three weights 
		hrec%maxweight1=0
		hrec%maxweight2=0	!Init to 0.
		hrec%maxweight3=0
		do i=1,size(weight)
			if(weight(i).gt.hrec%maxweight1)then
				hrec%maxweight3=hrec%maxweight2
				hrec%maxweight2=hrec%maxweight1
				hrec%maxweight1=weight(i)
			elseif(weight(i).gt.hrec%maxweight2)then
				hrec%maxweight3=hrec%maxweight2
				hrec%maxweight2=weight(i)
			elseif(weight(i).gt.hrec%maxweight3)then
				hrec%maxweight3=weight(i)
			endif
		enddo
!**************************************************************************
 
!********************frac2,frac3****************************************
			!These are intensity or concentration parameters.
			!FRAC2 is also called CONC in the literture.
		hrec%frac2 = (hrec%maxweight1+hrec%maxweight2)/sumsig
		hrec%frac3 = (hrec%maxweight1+hrec%maxweight2+   &
				& hrec%maxweight3) / sumsig
	endif
!**************************************************************************
!*************************size*********************************************

	hrec%size=sumsig	!Total number of pe's
!**************************************************************************

!*************************lessard 2-d analysis*****************************
!    Lessard:: dist(estimated)=elongation factor*(1-width/length)
!    efactor is a parameter set in WHOIPPLE_TELESCOPE
         destimated=efactor*(1.-(hrec%width/hrec%length))

!10/05/05 GHS Due to roundoff problems with the slope and yint calculation
!use the psi,cpsi,spsi values instead. Make life alot easier.
!!qa,qb,qc factors in new origen calculation along image long axis from 
!! center of mass a distance destimated.
!         qa=slope**2+1
!         qb=2*(slope*yint - ymean*slope - xmean)
!         qc=xmean**2 + ymean**2 - 2*ymean*yint + yint**2 - destimated**2
!!Note that only the qc term must be recalculated if we have a second way of 
!!estimating dist (say for Mary)
         
!useing qa,qb,qc determine source point of image.
!         xt(1)=(-qb-sqrt(qb**2-4*qa*qc))/(2*qa) !quadratic  has 2 soultions 
!         yt(1)=slope*xt(1)+yint                     
!         xt(2)=(-qb+sqrt(qb**2-4*qa*qc))/(2*qa) 
!         yt(2)=slope*xt(2)+yint                    
        
         xt(1)=xmean-destimated*cpsi
         yt(1)=ymean-destimated*spsi
         xt(2)=xmean+destimated*cpsi
         yt(2)=ymean+destimated*spsi
         

!Calling program will decide which pair to use(using asymmetry)
	return
    END SUBROUTINE W_HILLAS
!******************************************************************************

    SUBROUTINE T3A(WPTR,PPTR)
!******************************************************************************
!	Determins number of 3adjacent independent patterns in camera.
!	Use the wadjcent array to do this.

	use structures
	use wcamera_def
	IMPLICIT NONE

	type(camera) :: wptr
	type(camera_ptrs) :: pptr
		
	integer :: icount,i,j,k,l,m
	integer neighbori,neighborj,neighbork

	icount=0			!Conunts t3a patterns
!	Iterate through all possible groups of 3
	do i=1,wptr%npmt-2		!This can't be one of last 2
	    do j=i+1,wptr%npmt-1   	!This can't be last 1
	    	do k=j+1,wptr%npmt
					!So pixels i,j,k define pattern
					!check if they are adjcent.
		    neighbori=0
		    neighborj=0
		    neighbork=0
		

		    do l=1,6		!6 possible neighbors.
					!For 3 all paterns have at least one
					!pixel with 2 newighbors.	
				!I as center
			m=pptr%wadjacent(l,i)
			if(m==j.or.m==k)then
				neighbori=neighbori+1
			endif
				!J as center
			m=pptr%wadjacent(l,j)
			if(m==i.or.m==k)then
				neighborj=neighborj+1
			endif
				!K as center
			m=pptr%wadjacent(l,k)
			if(m==i.or.m==j)then
				neighbork=neighbork+1
			endif
		    	if(neighbori==2.or.neighborj==2.or.neighbork==2)then
		    		icount=icount+1
				exit		!We got one. don't look any
						!longer
			endif
		    enddo
		enddo
	    enddo
	enddo
	write(6,1000)wptr%npmt,icount
1000	format(' T3a count for',i5,'pixels is:',i10)
	return
    END SUBROUTINE T3A
!******************************************************************************

    SUBROUTINE T4A(WPTR,PPTR)
!******************************************************************************
!	Determins number of 4 adjacent independent patterns(clusters)in camera.
!******************************************************************************
!	Use the subroutine w10m_cluster to do this.

	use structures
	use wcamera_def
	IMPLICIT NONE

	type(camera) :: wptr
	type(camera_ptrs) :: pptr
	real,pointer,dimension(:) :: disc,trigger
	real,dimension(500) :: threshold
	integer :: norder,error,ii
	logical :: iok,jok,kok,mok
	integer :: icount,i,j,k,l,m,n,icount_old
	real :: w_max_sep2,radius_center,dist2
	norder=4		!Were looking for t4a
	deallocate(trigger,stat=error)
	allocate(trigger(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for TRIGGER."
	     stop  'Failed to allocate space for trigger'
	endif
	deallocate(disc,stat=error)
	allocate(disc(wptr%npmt),stat=error)
	if(error/= 0)then
 	     print*,"Program could not allocte space for DISC."
	     stop 'Failed to allocate space for disc'
	endif
	icount=0			!Counts t4a patterns
	icount_old=0
	radius_center=pptr%pmt_spacing(1)
        w_max_sep2=(8.25*radius_center)**2

!	Iterate through all possible groups of 4
	do i=1,wptr%npmt-3		!This can't be one of last 3
	    do j=i+1,wptr%npmt-2	!This can't be one of last 2
		dist2=(pptr%camera_x(i)-pptr%camera_x(j))**2+  &
			 &(pptr%camera_y(i)-pptr%camera_y(j))**2
		if(w_max_sep2<dist2)then
			cycle
		endif
	    	do k=j+1,wptr%npmt-1   	!This can't be last 1
		    dist2=(pptr%camera_x(i)-pptr%camera_x(k))**2+  &
			 &(pptr%camera_y(i)-pptr%camera_y(k))**2
		    if(w_max_sep2<dist2)then
			cycle
		    endif
		    dist2=(pptr%camera_x(j)-pptr%camera_x(k))**2+  &
			 &(pptr%camera_y(j)-pptr%camera_y(k))**2
		    if(w_max_sep2<dist2)then
			cycle
		    endif
	    	    do m=k+1,wptr%npmt
					!So pixels i,j,k,m define pattern
					!check if they are adjcent.
			trigger=0			!Vector init.
			trigger(i)=1
			disc(i)=i		!Just for fun
			trigger(j)=1
			disc(j)=j		!Just for fun
			trigger(k)=1
			disc(k)=k		!Just for fun
			trigger(m)=1
			disc(m)=m		!Just for fun

!quick check. every body better be connected to somebody else
					!First I
			iok=.false.
			jok=.false.
			kok=.false.
			mok=.false.
			do n=1,6
			   ii=pptr%wadjacent(n,i)
			   if(ii==j.or.ii==k.or.ii==m)then
			   	iok=.true.
			   	exit
			   endif
			enddo
					!Then j
			if(iok)then
			    do n=1,6
				ii=pptr%wadjacent(n,j)
				if(ii==i.or.ii==k.or.ii==m)then
				   jok=.true.
				   exit
				endif
			    enddo
					!then k
 			    if(jok)then
			 	do n=1,6
				    ii=pptr%wadjacent(n,k)
				    if(ii==j.or.ii==i.or.ii==m)then
					kok=.true.
					exit
				    endif
			        enddo
					!then m
				if(kok)then
				     do n=1,6
					ii=pptr%wadjacent(n,m)
					if(ii==j.or.ii==k.or.ii==i)then
					    mok=.true.
					    exit
					endif
		 		     enddo
				     if(mok)then
				call w10m_cluster(pptr,disc,trigger,  &
				& wptr%npmt,norder,threshold)
					if(threshold(3)/=0)then
						icount=icount+1
					endif
				      endif !mok endif
				endif   !kok endif
			    endif       !jok endif
			endif		!iok endif
		    enddo
		enddo
	    	print*,' i,j,icount:',i,j,icount
	    enddo
!	    print*,' i,ic,icount:',i,icount-icount_old,icount
	    icount_old=icount
	enddo
	write(6,1000)wptr%npmt,icount
1000	format(' T4a count for',i5,'pixels is:',i10)
	return
    END SUBROUTINE T4A

!******************************************************************************
END MODULE KASAOMEGA_SUBS

 PROGRAM KASAOMEGA
!	Version: V:1:3:8:3:4:4.26
!******************************************************************************
!	This routine_ processes the m-file 'events' for a
!	particular detector arangement. It is used to build a file of
!	various parameters, particulary Hillas parameters for image processing.
!******************************************************************************

!	It uses as input a single compressed summary 'M' file 

!	Note: Since this routine is run many times, try to be minimal on the
!	amount of output it produces to unit=6. 

!	Files:
!	unit=1	Input parameter file. 'kasaom.par'
!	UNIT=2	IMAGE output ntupple logical unit number(hropen)
!	unit=3  Random number vector seed file.
!	unit=4	Hillas Binary output file 'cmsum_out.dat'
!	unit=8  Compressed Sum input file. 'cmcum_in.dat'
!	unit=21 HRC.Coff,hrc.cpeds,hrc.cn2gains For DEAT.PEDVARS AND GAINS
!	unit=33 Michell's output data file.
!	Unit=41 PST pattern data file(wcamera.f90--wcamera_fill_pst)
!	Created: 17/12/93
!	Written by:

!	Mary P. Kertzman
!	Dept. of Physics and Astronomy
!	DePauw University
!	Greencastle, In. 
!	E-Mail: "kertzman@depauw.edu"

!	G.H.Sembroski
!	Physics Dept
!	Purdue University
!	W.Lafayette, In 47907
!	E-Mail: "sembroski@physics.purdue.edu"


!	Options:
!	  'HPARAM    Hillas param calculation option.
!	  'IMAGE'    Option 'IMAGE' for producing files for W10m Image
!		     displays.
!	  'LAURA'    Write out file for laura's wavelet study.
!	  'MICHELLE' Write out file for Michelle's template study.
!	  'TIM'	     Input cm file has pe timeings.
!	  'AN'	     Add noise(sky shine) to images.
!	  'DEADT'    Read dead tubes form hrc.coff file
!	  'SIGMA'    Read ped vars from hec.cpeds file.
!	  'GAIN'     Read gains from hrc.cn2gains file.
!	  'TWIDTH'   Requested threshold is in sigma widths.
!	  'TPES'     Requested threshold is in pe's.
!	  'PST2' 'PST3' 'PST4' Use pst for trigger.
!	  'HILLAS'   Write out hillas parameter ntupple file
!	  'NS'	     Use north-south version of triangular ground grid.
!         'DISPLAY'  Writes ADC and hillas stuff to binary file for non
!                    VERITAS displays.
!         'OUTVHDF5' Write out VHDF5RunData format single telescope 
!                    VHDF5VArrayEvent. Include /MONTECARLO dataset
!NOTE on Coordiante system.: Due to kascades use of z going down we have the
!	following coordinate system. X axis is E-W with positive being EAST!
!	Y axis is N-S with positive Y being SOUTH!!!(not NORTH!!!!)


!	Modified:

!	09/2/98 GHS	Convert to F90. Clean up lots of stuff.

!	06/3/98 GHS V:1:1:5:1:1:1.3
!		Add calculation of assymetery parameter which include
!		calculation of psi angle between xaxis and major axis of 
!		ellipse.  This formulation for psi and asymmetry is from Rod 
!		Lessard thesis(nov 1996) which uses results from Micale 
!		Punche's thesis which. The results are also in Punche's Hillas
!		parameter derivation notes(you know, that smugged baddly 
!		coppied set of notes from his logbook that we all use)...

!	07/5/98 GHS V:1:1:5:1:1:1.5
!		In order to have the flexability to have the long axis of
!		our areas for the VERITAS triangular grid weve added  new 
!		options to petype(in KASLITE):
!		'EW' and 'NS'. EW gives us the long axis in the east=west
!		direction(ie along the rows) with alternating rows offset in
!		the East-west direction by xseg/2.
!		               NS gives us the long axis in the North-South
!		direction(ie along the columns) with alternating columns offset
!		in the North-South direction by yseg/2.  This is used to allow
!		us to put the long axis of the area in the direction of 
!		inclination of the showers. Sometimes it is desirable to have 
!		them inclined from the South, and sometimes form the west for 
!		example.
!			If neither option is specified. 'EW' is assumed. 

!	16/6/98 GHS V:1:1:5:1:2:1.1
!		Add code to read in timing arrays from compressed files.
!		We know that there is timing info since the 'TI' option is
!		set in mhead%options

!	24/6/98 GHS V:1:1:5:1:2:1.2
!		Replace W10M_ADC_ORDER with W10M_ADC_ORDER_TOP. This new
!		reoutine only order top highest values in array.Reduces run
!		time by 2/3. May be possible to reduce even further with some
!		thought(which I'm short on today).
!		Also modify NTUPLE_CWN_HILLAS to limit size of ntuple files
!		to 2000000 events(1000000 blocks). Each new file' name ends
!		with *.*_v1,*.*_v2 etc.  Do this to avoid some PAW problems
!		with really big ntupples. Also makes it easier to put on tape.

!	29/6/98 GHS V:1:1:5:1:2:1.3
!		mhead%pe_head%petype was limited to 4 char and so
!		the NS option got left behind. Force NS option for now.
!		Previous to this date the EW option was assumed.(affects
!		hntp%x and hntp%y only as far as I can tell.)

!	01/7/98 GHS V:1:1:5:1:2:1.4
!		Make changes for multiplcity and cluster trigger levels.
!		Extend max_pe,maxpe_in to 2 through 5 multiplicity. e have to 
!		convert h_ntp%maxpe and maxpe_in to an arrays of 4 elements.
!		Remove imax arrays from code and from h_ntp. Remove intel from
!		h_ntp.
!		Add a W10M_CLUSTER routine. Rewrite adc_order_top so order
!		from highest to lowest and put values only in an array. Drop
!		index determination. h_ntp%index dropped form hillas ntupple.
!		Fix disc and pedistal widht calc(but not adc, not needed there)
!		for very low(<2) noise rates. id #pes=0 use a width of
!		sqrt(phwidth_factor) not 0. Also limit pulse height to >=0
!		(before pedistal subtraction)  This is done(especially for disc
!		trigger in W10M_TRIGGER	) To better model disc pulse height
!		Out method is extremely good even for high tails.
!		Also reduce phwidth_factor to .135 to make model better. This
!		is all from modeling timing pulses in detail(.25 ns steps).

!	10/7/98 GHS V:1:1:5:1:2:1.5
!		Add mirror plane variable:HNTP%EMALTMPL,HNTP%XMPLN,HNTP%YMPLN
!		Calculated in W10M_PROC. Also, if we dont have an image
!		determine HNTP%XMEAN and HNTP%YMEAN  from adc values of
!		triggered pixels.

!	10/7/98 GHS V:1:1:5:1:2:1.6
!		Use of the 331_25 database(pre june 1998) didn't have
!	        the options correctly. so ignore mhead%options for it.
!		This is basically a kluge. Also for use of this data base,
!		'W10mtr' MHEAD didn't have concentration. Data base assumes
!		concentration=1, reflectivity=1 ie efficiency=1. Force that
!		where mhead is read in.  THIS USES THE 'OLDM' OPTION (I think).


!	15/9/98 GHS
!######################ERROR################################################
!		hntp%emaltmpln is miscaluclated. I forgot to subtract the
!		track distance from the ground(hops) to sea level. 
!	THIS IS NOT CORRECTED for initial VERITAS study. Weve gone to far to
!	reddo this now. Correct in paw by subtracting 1399m/cos(20)
!       But see 12/05/03
!######################ERROR################################################

!	18/4/99 GHS
!		In order to study the detailed timing of the multiplicity
!		trigger and the PST KASTRIGGER now saves the times of each
!		pe in an event in an I*2 array as part of the 'TI' option.
!		The data format for the 'TI' option has thus changed. 
!		Change readin of data file for 'TI option. Unpack and save
!		using W10M_PEPULSE_ACCUMULATE.


!	13/5/99 GHS V:1:1:5:1:2:1.7
!		Add 'DEAD TUBES' capability. Look for 'DEADT' option. Read in a
!		file of dead tubes.  Use that to deactivate bad PMTS in 
!		W10M_TRIGGER. Flages in new array PPTR%ACTIVE_PMTS.
!		Weve also made lots of changes which I have been bad at
!		documenting to prepare for doing PST and detailed pulse 
!		modeling. Lots of changes to w10m_pepulse.
	
!	26/5/99 GHS V:1:1:5:1:2:1.7
!		Add in W10M_PROC 'GAIN CORRECTION' capability. Look for 'GAIN'
!		option. Read
!		in a file of pmt gains. Use that to modify pptr%threshold to
!		reflect differeing thresholds in pmts in real camera.
!		This should work for both W10M_PEPULSE and W10M_TRIGGER.

!	18/6/99 GHS V:1:1:5:1:2:1.7
!		Add 'DEADT' option capability to W10M_PEPULSE.
!		Change "IM' option to 'IMAGE'(conflict with TIM option)

!	29/6/99 GHS V:1:1:5:1:2:2.0
!		Convert to the use of an HDF file for input and Output. This is
!		done for portability of the data files between Linux and VMS
!		and because of problems linking in the Cern libraries with
!		the ABSOFT Fortran on LINUX. Later we will add MPI stuff.
!		Put opening of input summary HDF file(assume always compressed
!		summary with timing format from now on, this may change later),
!		into FILEOPEN. Put opening and definition of HDF output file 
!		into FILEOPEN. 
!		Convert W10M_PROC to use of HDF input and output files.
!		BUT SEE BELOW

!	07/10/99 GHS V:1:1:5:1:2:3.0
!		Convert the hdf4 version to one that reads and writes binary
!		files only. If we want ntuples of HDF4 or HDF5 files we can
!		write conversion programs.  Do this so we don't have to
!		worry about mixing libraries(CERN,MPI,HDF5 etc)
!		This is still a single processor version.(Not MPI!)
!		1:Use file name: kasaom.par as generic input parameter file
!		  name. Logicalliy assign actual file to this name in calling
!		  command/script file.
!		2:FILEOPEN:change call to open cmsum file to the binary 
!		  version to CMSUM_FILE_IN. Do simular in W10M_PROC for
!		  reading in areas.
!		3:Use file name: 'kasaom.par' as generic input parameter file
!		  name. Logicalliy assign actual file to this name in calling
!		  command/script file.

!	07/10/99 GHS V:1:1:6:1:2:3.1
!		Convert to MPI/SNGL capability. The routines that differ for
!		the MPI version will be put into kassaomega_mpi.f90 and their
!		single processor versions will be in kasaomega_sngl.f90.
!		These include: DATAIN.


!	07/10/99 GHS V:1:1:6:1:2:3.2
!		Add 490 pixel camera. Add 3 and 4 fold PST capability.
!		WCAMERA mods listed in KASTRIGGER.F90
!		1:Modify W10M_EFFICIENCY_RATIO_GEN for on lightcones on outer
!		rings of 490 pixel camera
!		2:Add 490 camera initlization to W10M_INI.

!	16/2/00 GHS V:1:1:6:1:2:3.3
!		For the DEADT dead tube option:
!		If a RUN number is specified in the OPTIONS read in DEADT 
!		information from the standard hrc00.coff file.
!		Convert w10m_time_gap to an array
!		Upgrade W10M_PEPULSE_ANALYZE to handle dead tubes.

!	17/3/00 GHS V:1:1:6:1:2:3.4
!		Write out Michell's Mfile. This is a binary version of the 
!		cmsum_in.dat file for use by MIchelle D'Vale to use for 
!		template image analysis. Uses option 'MICHELLE'. Writes to
!		new file michelle.dat. Use subroutine MICHELLE.

!	22/3/00 GHS V:1:1:6:1:2:3.4
!		Add square 1024 pixel camera.
!		WCAMERA mods listed in KASTRIGGER.F90
!		1:In W10M_EFFICIENCY_RATIO_GEN:Square pixels of SQ1024 camera 
!			use different ratio for	efficency calculation.
!		2:In W10M_INI: Add initilazation for SQ1024

!	24/3/00 GHS V:1:1:6:1:2:3.5
!		Open Laura's Mfile. This is a no-noise adc values
!		binary version of the cmsum_in.dat file for use by Laura 
!		to use for her wavelet cleanup study. Uses option 'LAURA'
!		Opens to New file laura.datIin DATAIN). Use subroutine 
!		W10M_PEPULSE_LAURA_ADC	(Called from W10M_PEPULSE_ANALIZE).
!		to collect the data into pptr%laura_adc
!		From W10M_PROC call LAURA to write out the event
!		Since varius mpi processes will be writting to laura.dat use
!		MPI parallel i/o. Do 'collective ' open in DATAIN where MPI
!		is initalized on all nodes. Do 'shared' write from subroutine
!		LAURA.

!	04/5/00 GHS V:1:1:6:1:2:3.6
!		Add code to generate an event file in HDF4 format readable
!		by Rod's data analysis. Later we can write a program that will
!		convert this file to a .fz file for distribution of MC events
!		to the collaboration. This HDF4 file will have lots of tag
!		variables(like shower energy, position,direction, particle
!		type , etc). MPI version only for now. 

!*****************************************************************************
!		I am worried about maximum size limit for HDF4. Its 2.15 Gbytes
!		(32 bit address?) in a file. I think this is ok. our events
!		(for 490 pixel camera) are about 490*2+17*4=1048 bytes/event. 
!		or:2.15e9/1048=2.05e6 triggers.
!*****************************************************************************

!		1: Add a call in DATAIN to PROC_SET_MYID to determine which
!		myid's go with which procs.
!		2: In FILEOPEN have parent also send mhead to OUTHDF4 daughter.
!		   and if we are the OUTHDF4 daughter , recieve mhead.
!		3:IN W10m_PROC we call OUTHDF4. For MPI version: if this is the
!		  OUTHDF4 daughter and we never come back. For all event
!		  processing daughters we defined the MPI data type event 
!		  structure and return.
!		4: Add a call to OUTHDF4_EVENT to W10M_PROC just after the
!		   ADC's are determined for a trigger event. OUTHDF4_EVENT will
!		   send event to OUTHDF4 daughter. Only event processing
!		   daughters reach this call.
!		5: Add a non=mpi (sngl) version of OUTHDF4 stuff
!		Don't forget to screw up the gains appropriatly
!               But see 12/05/03 
!	30/10/00 GHS V:1:1:6:1:2:3.7
!               Add stuff for HDF5 format input of the cm file. Note that
!               the HDF5 format for the cmsum file is the same as that for
!               the HDF5 format of the M files. The header is in a seperate
!               dataset. The disc and time arrays are varaiable length arrays
!               within compound data structures and tep, idfile, x_offset and 
!               y_offset are in the mrec record. To retrieve this data we use 
!               the standardized C routines(in KASHDF5.C): mhdf5_open, 
!               mhdf5_in, etc. from kashdf5.c  The same 
!               routines as used in kassum_hdf5 to make the cmsum file. 
!
!               *******For single proccess version.******
!
!               1: New OPTION: INFILE=BIN. Chosing this option gives us the old
!                  unformatted file input format. Default is now for HDF5 
!                  input format.
!               2: Change FILEOPEN(KASAOMEGA_SNGL.F(90) to test for INFILE=BIN
!                  and to add calls to mhdf5_open, mhdf5_mhead_in, 
!                  mhdf5_make_types.
!               3. Move the unpacking stuff for the binary records into 
!                  CMSUM_READ_AREA. Create a new routine CMSUM_HDF5_READ_AREA.
!                  In w10m_proc put check for INFILE=BIN to determine which 
!                  READ_AREA to call.
!               4: In MAIN put test to INFILE=BIN to decide how to close input
!                  file.
!
!               *******For MPI version.******
!
!               1: New OPTION: INFILE=BIN. Chosing this option gives us the old
!                  unformatted file input format. Default is now for HDF5 
!                  input format.
!               2: Change FILEOPEN(KASAOMEGA_MPI.f90) to test for INFILE=BIN
!                  and to add calls to mhdf5_open, mhdf5_mhead_in, 
!                  mhdf5_make_types.

!               3. Move the unpacking stuff for the binary records into 
!                  CMSUM_READ_AREA.
!                  Create a new routine CMSUM_HDF5_READ_AREA.
!                  In w10m_proc put check for INFILE=BIN to determine which 
!                  READ_AREA to call.
!               4: In MAIN put test to INFILE=BIN to decide how to close input
!                  file.

!	30/10/00 GHS V:1:1:6:1:2:3.8
!               Fix a bunch of bugs, most to do with array lengths and number 
!               of PMTS in the multiplicity trigger.
!               WCAMERA:WCAMERA_NOISE_THRESH_GEN
!                 1:Put in a check on values read in for DEAD pmt's. Must be
!                   positive,non-zero and not greater then number of PMTS in 
!                   camera.
!                 2:For 490 pixel camera change so that all small pixels(379)
!                   not just the 331 PST pixels have the wnoise,etc adjusted 
!                   for the new sigmas.
!                 3:Set array limits for the 490 pixels camera(some of which 
!                   are 547 long) to 490.
!                KASAOMEGA_SUBS:W10M_EFFICIENCY_RATE_GEN
!                 1:Limit size of pptr%efficiency we are modifying to 
!                   wptr%npmts.Add wptr to subroputines arguments
!                KASAOMEGA_SUBS:W10M_PROC
!                 1:Restrict multiplicity trigger to 379 small pixels for 490
!                   pixel camera in call to W10M_PEPULSE_ANALYZE and 
!                   W10M_PEPULSE_ADC.
!                 2:Set array limits for the 490 pixels camera(some of which 
!                   are 547 long) to 490.
!                W10M_PEPULSE:W10M_PEPULSE_ADC
!                 1. Pre-subtract pedistal from new noise channels after 379

!	20/03/01 GHS V:1:1:6:1:2:3.9
!               Add an additional active pmt arrry: pptr%active_HV. This will 
!               gives us 2. The first original arrray: pptr%active_pmts is for 
!               pmts that are not to be included in the event reconstruction 
!               (Hillas parameter calculations). The second will be those tubes
!               that have their HV turnned off and thus are not in the trigger.
!               This second array will be a subset of the first. Up until this
!               point we have been using the first array for both purposes. 
!               This causes some pmts which should have been in the trigger 
!               (and were probably very noisy) to not be included in the 
!               trigger when they should have been.

!       22/05/01 GHS V:1:1:6:1:2:3.10
!               Use new scheme for termination of OUTHDF4.Evnt processing 
!               daughters send termination notification to OUTHDF4 daughter. 
!               OUTHDF4 daughter counts these notifications and exits when all
!               daughters are done.(MPI version only)
!               But see 12/05/03

!       06/05/01 GHS V:1:1:6:1:2:3.11
!               Use 'multiplicity_pixels' for calculation of maxm1,maxm2 etc.
!               This is 379 for 490 pixel camera. We were using 490 pixels.
!               This matches how CPARAM does things.
!               In W10M_PROC
!	23/07/01 GHS V:1:2:6:1:2:3.12
!		Add stuff to FILEOPEN to print out Heavy ion primary name.
!       04/12/01 GHS V:1:1:6:1:2:3.13
!               In w10m_proc: Set Multiplicity _pixels to 463 for Veritas 
!               449 camera.
!               For 449 use VERITAS_PEPULSE_ANALIZE  instead of 
!               W10M_PEPULSE_ANALIZE.
!       08/01/02 GHS V:1:1:6:1:2:3.13
!               For 449 use VERITAS_PEPULSE_ADC  instead of 
!               W10M_PEPULSE_ADC.

!      15/02/02 GHSV:1:1:6:1:2:3.14
!               Add option "OUTVEvent" which causes a tagged simulated Vevent
!               to be written out. In W10m_proc call veritas_vevent_out which 
!               is a wrapper to call the C++ routine VeventOut.But see 12/05/03

!      10/05/02 GHS:1:1:6:1:2:3.15
!               Add a 'DISPLAY' option which causes ADC and hillas data for 
!               each event to be written to a binary file. For use with 
!               non-VERITAS event displays. VERITAS event displays will use 
!               standard VEvent/fpio files. Subroutine W10M_DISPLAY added. 
!               W10M_PROC.

!      20/06/02 GHS:1:1:6:1:2:3.16
!               Add a 'PAD' option to specify a second run to get pedvars from.
!               Padding uses for a specific channel the bigger of the pedvars
!               for sigma calculations. Must have SIGMA option specified for 
!               PAD option to be used (obviously!).
!               Modify DEADT code: If SIGMA specified and DEADT is not, use
!               SIGMA run for dead pmts. Also: IF SIGMA specified and PAD 
!               specified use both runs for inactive pmts. 
!               Modifications to:WCAMERA_NOISE_THRESH_GEN. Added:
!               WCAMERA_GET_PEDVARS, WCAMERA_GET_DEAD_PMTS.

!      20/06/02 GHS:1:1:6:1:2:4.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               ./kasaomga -p kasaom.par (sortof like in c).
!      19/12/02 GHS V:1:1:6:1:2:4.1 
!                Use intergized(dc) adc for cleanup and hillas calcs. Use real
!                for max1,max2,max3(so that when we multiply by pe_dc in paw
!                the histograms look ok). 
!                Fix use of Pe_dc. Its the number of digital counts/pe.
!                Add -d option to command line to input pe_dc
!      22/04/03 GHS V:1:1:6:1:2:4.2
!                490 Pixel camera has only 331 pixels in bot multiplicity and
!                PST trigger, not 379! Fix W10M_PROC
!                to use wptr%ntrigger_in. WCAMERA_DEF already has that.
!      12/05/03 GHS V:1:3:8:3:4:4.3
!                For V499 camera don't limit ADC value's to 1024 (11 bit). No 
!                limit should be imposed.
!		 h_ntp%emaltmpln calculation is corrected  See 15/9/98 
!                Change SC option to HPARAM. HILLAS option only used to
!                indicate Hillas binary file to be made(needs HPARAM also).
!                Replace OUTVEvent option with OUTVHDF5. Write event in 
!                VERITAS VHDF5RunData format with extra dataset /MONTECARLO
!                that contains hillas+ paramter (h_ntp) as whats in the HILLAS
!                ntupple file. This is a 'Single Telescope' VHDF5VArrayEvent 
!                file. 
!                Remove all OUTHDF4 stuff. No longer needed!
!                Add patch_trigger_patterns(1:pst_patches). Need for output for
!                VHDF5VArrayEvent. WCAMERA_MODULES.F90,               
!      12/05/03 GHS V:1:3:8:3:4:4.3
!                Add a call to PePulseADCPrint to veritas_pepulse_adc to print
!                out constants used in FADC.
!                Add PePulseAdcPrint subroutine to Veritas.cpp.
!      28/05/03 GHS V:1:3:8:3:4:4.4
!                Add option 'TIM=FAST' for V499 camera. This option first runs
!                W10_TRIGGER on an event with threshold=pesum-1 and if it
!                passes that it then runs veritas_pepulse_analyze with
!                threshols =pesum and looks for the trigger there.
!      28/05/03 GHS V:1:3:8:3:4:4.5
!                Since the scale and gains of things change when the ADC values
!                are determined from the FADC (V499 and TIM) we should 
!                determine the pedvars by generating noise streams with VPulse.
!                Do this in W10M_IMAGE_SIGMA since where it is needed is for 
!                the pprtr%high and pptr%low calculation for cleanup.
!
!       21/07/03 GHS V:1:3:8:3:4:4.6
!               Add stuff to determine if the detector is the v499 prototype.
!               Sets flag to that effect, that flag used by VHDF5Out (in 
!               Veritas.cpp) to reorder channells. Thats the only place we use
!               this flag, otherwise the prototype is simulated as a 499 
!               camera with half the pmts turned off in the hrcXX.coff and
!               hrcXX.hvoff files.

!       29/07/03 GHS V:1:3:8:3:4:4.7
!               Add stuff to write a PEDS dataset to the VHDF5 file with 5
!               records (499 arrays each):peds,pedvars,pairpedvars,gains,
!               tubesoff

!       20/08/03 GHS V:1:3:8:3:4:4.8
!               Add a call to VHDF5KascadeAttributes to add xseg and yseg 
!               as an attribute to the KASCADE dataset in the VHDF5 ouput file.
!       25/08/03 GHS V:1:3:8:3:4:4.9
!               For TIM=FAST make reduction of pptr%threshold for quick and 
!               dirty trigger check (W10M_TRIGGER) to 2 (used to be 1). At 1 
!               we miss ~25% of triggers.
!       18/02/04 GHS V:1:3:8:3:4:4.10
!               Upgrade Veritas.cpp for updated VHDF5 (sperastes Paramdataset
!               from Kascade Dataset). Removes use of VHDF5VArrayEvent and uses
!               VHDF5Event class.
!               Add a WhippleEventWeight routine to Veritas.cpp. Put in a call
!               to it in W10m_proc.
!      24/02/04 GHS V:1:3:8:3:4:4.11
!               Make sure any pmt turned off in active_hv is also turned off
!               in active_pmts.
!      01/03/04 GHS V:1:3:8:3:4:4.12
!               in kasaomega_sngle:CMSUM_HDF5_READ_AREA put in code to see 
!               catch when the # pes exceeds itime_size which is size of time 
!               array. Extend the itime_size to 150000 pes.
!      03/03/04 GHS V:1:3:8:3:4:4.13
!               Extend largest possible length of input command ine string
!               to 120 characters (from 80). In wcamera_modules.f90.
!      10/03/04 GHS V:1:3:8:3:4:4.14
!               Change and add some options:
!               -c (cpeds cn2gains etc) Now gives HRC path/filename spec
!               -v (VHDF5) Now specifies VHDF5 output file
!               -h (help) New option:lists all command line options.
!               -s (sigma) New Option: specifies sigma for pe_dc as fraction 
!                          of pe_dc:
!      10/03/04 GHS V:1:3:8:3:4:4.14
!               Jitter pe_dc on tube by tube basis. Supply a small source of 
!               noise to the max1,2,3 distributions. Model is that gain 
!               corrections in the real data analysis had some error. We model
!               this by jittering our gains (pe_dc) per pmt with a gaussian 
!               distribution, centered at pe_dc, specified as command line 
!               input parameter,with a sigma of pe_dc_sigma*pe_dc (also 
!               specified in the command line as fraction of pe_dc).
!               Add in wcamera_modules.f90  a new array pptr%pe_dc. Thus we 
!               can have a different pe_dc per PMT
!               Allocate and fill in the array in WCAMERA_GEN.
!               Replace pe_dc with pptr%pe_dc(i):
!                  In W10M_IMAGE_SIGMA:High/low clean up values.
!                  In W10m_PROC: Adc values (this is the important one for 
!                                our model(see above))
!              Do simular for PEDs. Assumne we need to introduce an error in
!              the pedestal subtraction. pptr%peds
!        12/03/04 GHS V:1:3:8:3:4:4.15
!               Add the index array back in to adc_order_top. Put the 
!               max1,max2,max3 indiexes into hntp%max_pe_int.
!        29/03/04 GHS V:1:3:8:3:4:4.16
!                Add a call to new routine W10M_PEPULSE_INIT in W10M_INI
!                to create the single pepulse array with appropriate rise time.
!                Only use for w10m cameras with the TIM option requested 
!                (Veritas camers do this on their own). This replaces the fixed
!                rise time pulse used previously.
!        30/03/04 GHS V:1:3:8:3:4:4.17
!                In order to try to dirty up things a little more add a timing
!                offset to each channel(pptr%timing_offset). Pick offset for 
!                each PMT randomly from a gaussian dist with width of 
!                timing_offset_sigma. Fill it in WCAMERA_GEN. Apply offset in
!                W10M_PEPULSE_ACCUMULATE. 
!        31/03/04 GHS V:1:3:8:3:4:4.18
!                For still more attempts at dirtying up things, change 
!                SINGLE_PE_HEIGHT (W10m_SUBS.F90) to have the width of the
!                single pe pulse height distribution be an input parameter.
!                Defaults to old vaslue (.275). Add PPTR%SINGLE_PE_SIGMAS.
!                Fill it in WCAMERA_GEN. Put in parameters 
!                PULSE_HEIGHT_WIDTH_MEAN and PULSE_HEIGHT_WIDTH_SIGMA in
!                WHIPPLE_TELESCOPE.F90. Also add sigmas to W10M_NOISE_PULSE 
!                argument list
!        03/05/04 GHS V:1:3:8:3:4:4.19
!                Convert W_HILLAS to look (and act) more like the 
!                get_hillasparametersin cparam.c
!                1:Add xo,yo offset source location arguments to W_HILLAS call.
!                2:Fix error in psi part of asym calculation (been there for 
!                  6 years!!!)
!                3:Add slope and intercept calculation for use in 2-d analysis.
!                4:Add Lessard elongation factor parameter to 
!                   WHIPPLE_TELESCOPE. Used in W_HILLAS.
!                5:Add use of WHIPPLE_TELESCOPE to W_HILLAS
!                6:Add xt,yt arrays to argument list. This returns estimated
!                  source location of event. 2 piars of locations for each 
!                  estimation. First pair is for Lessard estimation 
!                  (xt(1),yt(1) and xt(2),yt(2)) Add code to calculate this.
!                7:Add code to determine which pair of coords to use for 
!                  2-d estimation of source location. Uses asymmetry from 
!                  W_HILLAS call in W10M_PROC

! 20/10/04 GHS V:1:3:8:3:4:4.20
!                For use in random directions from hadrons save ithphi index
!                from mrec%ithphi to hntp%ithphi. In W10m_PROC. For use later 
!                in kasarraytrigger.

! 04/02/05 GHS V:1:3:8:3:4:4.21
!                Set up to generate pedestal events when we writer out the
!                VHDF5 file for whipple events (do same for veritas event 
!                latter)
!                1.Replace in VHDF5_PEDS_OUT 4.25 with hiclean.
!                2.In file 'kasaomega_sngl.f90':in routine WHIPPLE_VHDF5_OUT:
!                  determine (once only) the pedvars used in the run and pass
!                  a pointer to them in the call to  the C++ routine: 
!                  WhippleVHDF5Out in file 'Veritas.cpp'. Also pass pointer to
!                  a logical variable that enables the generation of the 
!                  pedestal events in WhippleVHDF5Out.
!                3.In file Veritas.cpp, routine WhippleVHDF5Out add above 
!                  arguments. Add code to gererate and write pedestal events.
!                  This uses a new class: WhippleVHDF5Event.
!      17/02/05 GHS  V:1:3:8:3:4:4.22 
!               Add a printseeds flag to ranstart and ranend.
!      29/03/05 GHS  V:1:3:8:3:4:4.23
!               I'm going to fixup the h_ntp for the 2d stuff. 
!               1:First, add xo(2) and yo(2) for the 2 possible x,y origen of 
!                 event (Lessard elongation method) xo(1), yo(1) is most 
!                 likly from asymmetry. 
!               2:Change xk,yk to xk(2),yk(2) from 2d source location from 
!                 Mary's method. 
!               3:Change ra2d and dec2d to ra2d(2),dec2d(2) for ra and dec of 
!                 xo,yo.
!               4:Change and add to tags: xo1,xo2,yo1,yo2,ra2d1,ra2d2,dec2d1
!                 dec2d2,xm1,xm2,ym1,ym2
!      29/03/05 GHS  V:1:3:8:3:4:4.24
!               Fix yet another bug in the asymmetry calculation in W_HILLAS
!               And fix a bug in the slope calculation.
!      11/05/05 GHS  V:1:3:8:3:4:4.25
!               The slope calculation in W_HILLAS has round off problems. Use
!               instead the psi calculation which is specially formulated to 
!               avoid those problems. This makes the xo1,yo1,xo2,yo2 
!               calculation much simpler. Code come from S.Fegans 
!               HillasParameterization.cxx and 
!               TwoDimensionalParameterization.cxx
!      15/06/04 GHS  V:1:3:8:3:4:4.26 
!                In W10M_PROC: for W490: After second call to W_HILLAS to 
!                determine correct assymetry decision, rotate where xo1,yo1,
!                xo2,yo2 to reflect the fact that the WHIPPLE 490 pixel 
!                camera is rotated by 180 + 7.306 degrees from having pixel 2 
!                on the + x axis.      
!*****************************************************************************

!*****************************************************************************
!  Command line input options:
!*****************************************************************************
! -p kasaom.par   :File name for input parameter file:DATAIN 
! -r kasaom.ran   :Randomnumber seed file name.      :DATAIN,RANLUX_SNGL
! -c hrc02        :HRC and year for .cpeds,.coff etc :WCAMERA_NOISE_THRESH_GEN 
! -i cmsum_in.dat :Input cmsum binary format file.   :CMSUM_FILE_OPEN
! -j cmsum_in.hdf5:Input cmsum hdf5 format file.     :CMSUM_HDF5_FILE_OPEN
! -o cmsum_out.dat:Output cmsum binary format file.  :FILEOPEN
! -v vhdf5_output_file:Output VHDF5RunData format file.    :OUTHDF5
! -q display.dat  :Display.dat file for qteventview. :W10M_DISPLAY
! -d pe_dc_txt    :Value for pe_dc digitalcounts/pe  :
! -s pe_dc_sigma_txt :Value for sigma of pe_dc as a fraction of pe_dc for pmts.
! -t ped_sigma_txt :Value for sigma of ped ind digital counts.
! -h help         :List all these options.
!*****************************************************************************

	use kasaomega_subs
	use kasaomega_sub2
	use wcamera_def
	use kasaomega_def
	use kasaomega_command_line
        use whipple_telescope
IMPLICIT NONE

	character(LEN=80) :: update= '11-May-2005 GHS' !Last version update
        integer :: i
        character(len=80) :: arg_opt
        integer,external :: iargc_
        character(len=8) :: adate
	character(len=10) :: time
        integer printseeds

!	Version number.
!	The version number has one digit for each program in the KASCADE 
!       system.
!	seperated by ':'. KASAOMEGA has 2 digits seperated by ".".
!	Digits for previous programs are for compatibility version to KASMULT.
!	New compatibility version require later programs
!	to be modified due to changing output data formats, or require 
!       rerunning
!	of the data base due to corrections to important mistakes.
!	Digit 2:	KASCADE    version
!	      3:	KASLITE    version
!	      4:	KASSRTMRG  version
!	      5:	KASTRIGGER version
!	      6:	KASAOMEGA  version.sub version
	version= ' V:1:3:8:3:4:4.26'   !Present version.
	write(6,1205)trim(version),trim(update)
1205	format(' KASAOMEGA***Version: ',a,' ***Last update:',a)

!******************************************************************************
!       Get the various command line arguments.
!       All command line arguments come in pairs: An option (-p) and 
!                                                 a string (kasoam.par)
!       Defaults are in wcamera_modules.f90
!******************************************************************************
        num_cmd_args=iargc_()
        if(num_cmd_args.gt.0)then
           print*,'KASAOMEGA--Number of command line arguments:',num_cmd_args
           do i=1,num_cmd_args,2
              call getarg_(i,arg_opt)
              arg_opt=trim(arg_opt)
              if(arg_opt=="-p")then
                 call getarg_(i+1,input_par_file_name)
                 input_par_file_name=trim(input_par_file_name)
                print*,' -p option gives parameter filename:',&
                     & trim(input_par_file_name)
              elseif(arg_opt=="-r")then
                 call getarg_(i+1,random_seed_file_name)
                 random_seed_file_name=trim(random_seed_file_name)
                 print*,' -r option gives random seed filename:',&
                      &trim(random_seed_file_name)
              elseif(arg_opt=="-c")then
                 call getarg_(i+1,hrcyear)
                 hrcyear=trim(hrcyear)
                 print*,' -c option gives hrc path/filename spec:',&
                      &trim(hrcyear)
              elseif(arg_opt=="-i")then
                 call getarg_(i+1,cmsum_in_dat_file)
                 cmsum_in_dat_file=trim(cmsum_in_dat_file)
                 print*,' -i option gives binary input mfile name:',&
                      &trim(cmsum_in_dat_file)
              elseif(arg_opt=="-j")then
                 call getarg_(i+1,cmsum_in_hdf5_file)
                 cmsum_in_hdf5_file=trim(cmsum_in_hdf5_file)
                 print*,' -j option gives hdf5 input mfile name:',&
                      &trim(cmsum_in_hdf5_file)
              elseif(arg_opt=="-o")then
                 call getarg_(i+1,cmsum_out_dat_file)
                cmsum_out_dat_file=trim(cmsum_out_dat_file)
                 print*,' -o option gives:',trim(cmsum_out_dat_file)
              elseif(arg_opt=="-v")then
                 call getarg_(i+1,vhdf5_output_file)
                 vhdf5_output_file=trim(vhdf5_output_file)
                 print*,' -v option gives:',trim(vhdf5_output_file)
              elseif(arg_opt=="-q")then
                 call getarg_(i+1,display_dat_file)
                 display_dat_file=trim(display_dat_file)
                 print*,' -q option gives:',trim(display_dat_file)
              elseif(arg_opt=="-d")then
                 call getarg_(i+1,pe_dc_txt)
                 pe_dc_txt=trim(pe_dc_txt)
                 read(pe_dc_txt,1000)pe_dc
1000	format(f8.3)
                 print*,' -d option gives pe_dc:',pe_dc
              elseif(arg_opt=="-s")then
                 call getarg_(i+1,pe_dc_sigma_txt)
                 pe_dc_sigma_txt=trim(pe_dc_sigma_txt)
                 read(pe_dc_sigma_txt,1000)pe_dc_sigma
                 print*,' -s option gives pe_dc_sigma:',pe_dc_sigma
              elseif(arg_opt=="-t")then
                 call getarg_(i+1,ped_sigma_txt)
                 ped_sigma_txt=trim(ped_sigma_txt)
                 read(ped_sigma_txt,1000)ped_sigma
                 print*,' -t option gives ped_sigma:',ped_sigma
              elseif(arg_opt=="-h")then
        print*,' *************************************************************'
        print*,' * Command line input options:'
        print*,' *************************************************************'
        print*,' * -p kasaom.par   :File name for input parameter file'
        print*,' * -r kasaom.ran   :Randomnumber seed file name'      
        print*,' * -c hrc02        :HRC and year for .cpeds,.coff etc' 
        print*,' * -i cmsum_in.dat :Input cmsum binary format file.'   
        print*,' * -j cmsum_in.hdf5:Input cmsum hdf5 format file.'     
        print*,' * -o cmsum_out.dat:Output cmsum binary format file.'  
        print*,' * -v vhdf5_output_file:Output VHDF5RunData format file.'
        print*,' * -q display.dat  :Display.dat file for qteventview.' 
        print*,' * -d pe_dc_txt    :Value for pe_dc digitalcounts/pe'
        print*,' * -s pe_dc_sigma_txt :Value of pe_dc_sigma as frac of pe_dc'
        print*,' * -t ped_sigma_txt :Value of ped_sigma digital counts.'
        print*,' * -h Print this options summary'
        print*,' *************************************************************'
                 stop
              else
                 print*,' Illegal command line option #:',i,'Option:', &
                      trim(arg_opt)
                 stop 'KASAOMEGA: Illegal command line option'
              endif
           enddo
        else
           print*,' KASAOMEGA--No command line arguments.'
           print*,' KASAOMEGA--Using defaults for all.'
        endif

!******************************************************************************
		!Get input parameters.
	call datain
	call fileopen		!Open the input compressed summary 'M' file 
				!and read in mhead%
				!This sets up  mhead%step_size,
				!mhead%Itheta_max,mhead%rate_noise
!	Initalize various things according to detector type.
	if(index(detector,'W').ne.0 &
             & .or.index(detector,'V').ne.0 &
             & .or.index(detector,'SQ').ne.0)then
		print*,' Whipple 10m TELESCOPE CAMERA.'
		call w10m_ini	!Calculate WHRC_X,WHRC_Y ETC.
	else
		print*,' KASAOMEGA_FATAL--Bad detector type:',detector
		stop 'Bad detector type from input .par file'
	endif

        call solidangle			!Print our solidangle contributions

!	The file is sorted by nx then ny then itheta then iphi.
	if(index(detector,'W37').ne.0)then
		call W10m_proc(w37,p37)
	elseif(index(detector,'SQ1024').ne.0)then   !1024 square pixel camera 
		call W10m_proc(sq1024,psq1024)
	elseif(index(detector,'W109').ne.0)then	!109 pmt hrc camera 
		call W10m_proc(w109,p109)
	elseif(index(detector,'W151').ne.0)then	!151 pmt hrc camera 
		call W10m_proc(w151,p151)
	elseif(index(detector,'W271').ne.0)then	!271 pmt hrc camera 
		call W10m_proc(w271,p271)
	elseif(index(detector,'W331_125').ne.0)then !331 pmt hrc camera 
		call W10m_proc(w331_125,p331_125)
	elseif(index(detector,'W331_1875').ne.0)then !331 pmt hrc camera 
		call W10m_proc(w331_1875,p331_1875)
	elseif(index(detector,'W331_25').ne.0)then   !331 pmt hrc camera 
		call W10m_proc(w331_25,p331_25)
	elseif(index(detector,'W490').ne.0)then   !490 pmt hrc camera 
		call W10m_proc(w490,p490)
	elseif(index(detector,'W541').ne.0)then !541 pmt uhrc camera
		call W10m_proc(w541,p541)
						!Now the 499 veritas cameras
 	elseif(index(detector,'V499').ne.0)then
		call W10m_proc(v499,pv499)
	endif

	if(index(options,'AN').ne.0)then
                printseeds=1
		call ranend(printseeds,random_seed_file_name)!Save random seed vector if
	endif					  !regenerate noise

	close(unit=4)
	call date_and_time(adate,time)
	
     WRITE(6,1001)adate(1:4),adate(5:6),adate(7:8),time(1:2),time(3:4),time(5:)
1001  format(' ',15x,a4,' ',a2,'/',a2,5x,a2,':',a2,':',a, &
			&    '--------KASAOMEGA-END------')
        print*,'KASAOMEGA normal end'
        call exit

END PROGRAM KASAOMEGA
