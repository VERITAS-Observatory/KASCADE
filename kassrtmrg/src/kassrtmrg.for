c	Program KASSRTMRG
c	Version:V:1:3:8:3.1
c	This program sorts a kascade pe file by area and time

c	Written by:
c		Pete Palfrey
c		Physics Dept
c		Purdue Univ.
c		5/9/90

c	Modified:
c	10/15/90 G.H.S. V:1:0:1:0.0
c		Add version number stuff.  Add an input parameter 
c		file:KASSRTMRG.INP. New subroutine DATAIN
c		Define a single parameter:FILE=char*1
c		FILE='V' (Variable format input file 5 words long)
c		FILE='F' (FIXED format 5 words long),
c		FILE='L' (8 words long. VMS:Fixed format, SGI:Stream?).
c				Only viable format for SGI.
c		Use FILE to determine some things.
c		THIS VERSION WILL THUS BE COMPATABLE WITH PREVIOUS VERSIONS!!
c		But see 29/10/93

c	10/18/91 G.H.S. V:1:0:1:0.1
c		Make changes so that this will run under SGI unix.
c		1: replace for$date,for$time    with  call date..,call time..
c		2: replace read(2'inext)   with   read(2,rec=inext)
c		3: remove "blocksize=20480" from open statements.
c		4: Use machine_type.h to determine which OPEN statement to
c			use for sequential files(VMS: fixed, 
c			SGI:default=stream?)
c		5: Since SGI and VMS use different ordering in i*4, use an
c			algorithum that is machine independent for nx,ny
c			sorting word.

c	03/6/92 G.H.S. V:1:0:1:0.1
c		For SGI compatability, remove explicit file name reference in
c		OPEN statements for SCRATCH files. Scratch disk determined by
c		setenv TMPDIR --- statement in UNIX script file and with
c		DEFINE SYS$SCRATCH --- in VMS DCL command file.

c	02/11/92 GHS  V:1:0:1:0.2
c		Again for SGI compatability use an include file(DELETE_PE.H)
c		to delete the pe file from the disk to make space after it is
c		read in. We have to use a SYSTEM call for SGI since opening a
c		file with disposition=delete and closing it(the way we do it
c		on VMS) doesn't delete the file until we exit the program.

c	18/1/93 GHS  V:1:0:1:0.2
c		Make provisions for one of the scratch files to be on a
c		seperate disk. This allows for really big showers. In the case
c		where the final sort would put the resultant pes file on the
c		same disk as the input file, pur the pes file on the other
c		disk, do the final sort, deletete the input file and then
c		copy the output pes file to the correct disk. This is only
c		implimented for SGI. The VAX include files(set_final_out.h,
c		copy_file_out.h) are dummy's.

c	27/10/93 GHS V:1:0:2:1.0
c		Note new version of KASLITE and thus this is a major new
c		version. Needed since were changing the input file format.
c		This is done in a structure which allows for future format
c		changes without changing this code. Old compatabily can be
c		retained by changing KASSTRUCTURES.H which is the pe file
c		record structure. Remove different file format option.
c		For vax again specify Scrath files as status=SCRATCH.

c	16/11/93 GHS V:1:0:2:1.0
c		Change the number of merg blocks in BIGMRG to a parameter
c		(N_MERGE) from the value 8. This is to improve performance.
c		Try a value of N_MERGE = 16 to start with.
c	07/5/97 GHS V:1:1:4:1.1
c		Convert to UNix again.
c		1:Remove 'shared' type specifier in OPEN statements.
c		2:Remove 'recordtype' specifier from OPEN statements.
c		3:Remove associatevariable from OPEN statements. Add
c		inext=inext+1, jnext=jnext+1 where appropriate(and maybe
c		someplaces it wasn't necessary but its safer.
c		4:For VMS/SGI-UNIX BLOCK_SIZE is in i*4. For SUN-UNIX its in
c		bytes. Use recl=recl_size in all direct access OPEN
c		statements where RECl_SIZE is set in KASSRTMRG.H.
c		5:Make the flag for end of nxny an integer result so we
c		don't get NaN errors.NXNY_FLAG.
 
c	16/12/97 GHS V:1:1:5:1.0
c		Add printout of pe_head.x_offset, pe_head.y_offset
c		The addition of X_OFFSET AND Y_OFFSET to pe_head causes
c		a major version number change for KASLITE.

!	23/9/99 GHS V:1:1:5:1.1
!		Upgrade to a generic version for use on LINUX.
!		1:Inlcude kastructures.h explicitly in KASSRTMRG.H. Move 
!               all
!		data statements to the end of the file. Put KASSRTMRG.H as
!		last in all declaration segments. Needed for absoft f77 which
!		doesn't allow mixing of data statements and declaration
!		statements.
!		2:Fix dat and time calls. Use DATETIME F90 interface
!		routine.
!		3:Remove nameing of scratch files. Not needed.
!		4:extend pe_head.petype to 16 characters.
 
!       19/10/00 GHS V:1:1:5:1.2
!               Add OPTIONS to input common block and DATAIN
!               Test OPTIONS to see if output file is PESFILE=BIN
!               Otherwise assume output file is HDF5 format.

!	11/07/01 GHSV:1:2:6:1.3
!		Add stuff to find heavy ion primary names.
!	11/07/01 GHSV:1:2:6:1.4
!		Add iostat tests to file openings,reads and writes.

!       30/04/02 GHS V:1:3:7:2.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               ./kassrtmrg  -p kassrtmrg.par  (sortof like in c).
!               Setup for command line input parameter use.(Changes in main,
!               DATIN, FILEOPEN
!       05/05/02 GHS V:1:3:7:2.1
!               Make internal scratch pe files normal direct access files with 
!               dispose=delete option so they  delete when closed. Give them 
!               specific names so that they go in same directory as the 
!               pe_input_file(add extensions 'scr11' and 'scr12' to 
!               pe_input_file name). This puts them in a specific place where 
!               we should know if there is room. Otherwise 'scratch' files are
!               placed wherever the fortran compiler thinks they should go 
!               (/tmp for absoft). Harder to make surte there is room for them.

!       07/05/02 GHS V:1:3:8:3.0
!               Another Major version change. To handle very large pe and pes
!               files (showers ~1 Tev+) use a direct access file for the pe 
!               input and pes output files (we already use direct access for 
!               the internal pe scratch sorting files). 
!               Requires getting and putting the header record in a seperate 
!               file (pe_input_file//'.head',pes_output_file//'.head' )
!       24/03/04 GHS V:1:3:8:3.1 
!               Change input command line string lengths to 120 chars (in
!               KASSRTMRG_COMMAND_LINE.H)

c	files
c	Unit  1:  Input parameter file.
c	Unit 11:  Input pe file
c	Unit 12:  Sratch file.
c	Unit 14:  Output sorted pe file.


        external iargc_
        integer num_cmd_args
        character*80 arg_opt
	include 'kassrtmrg_command_line.h'
        include 'kassrtmrg.h'
        character*80 pe_head_input_file
	character*80 version
	character*80 update

	common/tans/mrg(block_size,32)
	real*4 datam(block_size),mrg
	integer*4 idatam(block_size),imrg(block_size,32),jtemp
	equivalence (idatam(1),datam(1)),(mrg(1,1),imrg(1,1))
	integer ios
        logical endflag
	data version/'V:1:3:8:3.1'/
        data update/'24-March-2004 GHS'/

c	Version number.
c	The version number has one digit for each program in the KASCADE system.
c	seperated by ':'. KASSRTMRG has 2 digits seperated by ".".
c	Digits for previous programs are for compatibility version to KASMULT.
c	New compatibility version require later programs
c	to be modified due to changing output data formats, or require rerunning
c	of the data base due to corrections to important mistakes.
c	Digit 2:	KASCADE version
c	      3:	KASLITE version
c	      4:	KASSRTMRG version

	write(6,1205)trim(version),trim(update)
1205	format(' KASSRTMRG***Version: ',a,' ***Last Updated:',a)

!******************************************************************************
!       Get the various command line arguments.
!       All command line arguments come in pairs: An option (-p) and 
!                                                 a string (kaslite.par)
!******************************************************************************
        num_cmd_args=iargc_()
        if(num_cmd_args.gt.0)then
           print*,'KASSRTMRG--Number of command line arguments:',
     1       num_cmd_args
           do i=1,num_cmd_args,2
              call getarg_(i,arg_opt)
              arg_opt=trim(arg_opt)
              if(arg_opt=="-p")then
                 call getarg_(i+1,input_par_file_name)
                 input_par_file_name=trim(input_par_file_name)
                print*,' -p option gives:',trim(input_par_file_name)
              elseif(arg_opt=="-i")then
                 call getarg_(i+1,pe_input_file)
                 pe_input_file=trim(pe_input_file)
                 print*,' -i option gives:',trim(pe_input_file)
              elseif(arg_opt=="-o")then
                 call getarg_(i+1,pes_output_file)
                 pes_output_file=trim(pes_output_file)
                 print*,' -o option gives:',trim(pes_output_file)
              elseif(arg_opt=="-h")then
                 call getarg_(i+1,pes_hdf5_output_file)
                 pes_hdf5_output_file=trim(pes_hdf5_output_file)
                 print*,' -h option gives:',trim(pes_hdf5_output_file)
              else
                 print*,' Illegal command line option #:',i,'Option:',
     1            trim(arg_opt)
                 stop 'KASSRTMRG: Illegal command line option'
              endif
           enddo
        else
           print*,' KASSRTMRG--No command line arguments.'
           print*,' KASSRTMRG--Using defaults for all.'

        endif

!******************************************************************************


	call datain

c	Open the inoput,scratch and ouput files. Transfer the headers.
	call fileopn
c	The rest of the PE file structure is found in KASSTRUCTURES.H.

	nlastwd=block_size-pe_size
	nwrites=0
	inext=1
	endflag=.false.
	jtot=0
	jptrlast=32
	nxny_flag=0		!Init nxny_flag
10	imerge=0
c		Readin and sort a block of nmaxwds elements.
1	jin=0

c		Read them in.
	do i=1,nmaxwds
		j=jin*pe_size
c	01/11/93 GHS use pe structure here(only place). Get a pe.
!       07/05/03 GHS This file is now direct access(for large file purposes)
		read(11,rec=ipe_next,end=99,iostat=ios) 
     1                                       (pe.recbuf(ii),ii=1,pe_size)
                if (ios>0)then
                   print*,'KASSRTMRG-PE input file  read error.ios:',ios
                   print*,'Retrying failed pe input file read'
                   read(11,rec=ipe_next,end=99,iostat=ios) 
     1                  (pe.recbuf(ii),ii=1,pe_size)
                   if (ios>0)then
                      print*,'KASSRTMRG-PE input file read error on retry'
                      print*,'I quit!'
                      stop 'PE input file read failure'
                   endif
                endif
                ipe_next=ipe_next+1

			!read this way so we can pick up correct variables.
	!Transfer into the block
		do ii=1,pe_size
			datam(j+ii)=pe.recbuf(ii)
		enddo
c	3/22/90 G.H.S.
c		Reverse order here of nx and ny(for VMS), make them positive 
c		definate so that we can sort on nx-ny and get the sort in the
c		correct	order.
c	10/21/91 G.H.S. V:1:0:1:0.1
c		For machine independence set things up so we don't 
c		have to worry about word order.

			!Modify block nx,ny for sort.
		jtemp=(pe.nx+16000)	!Put into i*4 to prevent integer
					!overflows in VMS.
		idatam(j+1)=(jtemp*32000)+(pe.ny+16000) !Jtemp is i*4 which
							!forces the calculation
							!to be i*4 and prevents
							!overflows.
				!Keep track of biggest value to that we can 
				!Make an end flag that is bigger.
		if(idatam(j+1).ge.nxny_flag)then
			nxny_flag=idatam(j+1)+1
		endif
		jin=jin+1
	end do
	jtot=jtot+jin
c	
c	Sort this block
2	do i=1,nlastwd,pe_size
		ixyminm=idatam(i)
		tminim=datam(i+1)
		jmin=i
		do j=i+pe_size,block_size,pe_size
		   id=idatam(j)
		   if(id.lt.ixyminm) then
			ixyminm=id
			tminim=datam(j+1)
			jmin=j
			go to 3
		   end if
		   if((id.eq.ixyminm).and.(datam(j+1).lt.tminim)) then
			tminim=datam(j+1)
			jmin=j
		   end if
3		continue
		end do
		if(jmin.ne.i) then	!need to swap
		   do kk=0,pe_size-1
			it=idatam(i+kk)
			idatam(i+kk)=idatam(jmin+kk)
			idatam(jmin+kk)=it
		   end do
		end if
	end do
c
c	Collect this block into the merge array
c	We can have up to 32 blocks.
	imerge=imerge+1
	do j=1,block_size
		imrg(j,imerge)=idatam(j)
	end do
4	if(imerge.eq.jptrlast) then
c		If we are full merge these blocks into a super block which is
c		then written out into a temporary file.
		call pmerge(jptrlast)
		if(endflag) go to 100	!If were done, leave.
		go to 10		!Go get another 32 blocks(if we can)
	else
		go to 1			!Go get another block.
	endif
c
c	Comes here when we run out of events from the input file.
99	endflag=.true.		!Indicate that were about done.
	if(jin.gt.0) then	!Check for a partially full block.
		jtot=jtot+jin
		i1=jin*pe_size
		do i=i1+1,block_size,pe_size
			idatam(i)=nxny_flag	!Flag the end of the block.
			datam(i+1)=0.		!Time to 0
		end do
		jptrlast=imerge+1
		go to 2			!Sort this partial block.
	else
		jptrlast=imerge		!No partial block to sort. Merge what
					!we have.
c	1/2/91 G.H.S. V:1:0:1:0.0
c		This takes care of exactly filled buffers.
c		Pete's fix.
		if(imerge.eq.0)goto 100		!See if any left.
c	12/28/90 G.H.S. V:1:0:1:0.0
c		Handle zero length pe files.
		if(jtot.eq.0)go to 100	!jtot indicates total number of pe's
					!found in input file.
		goto 4
	end if
c
100	write(6,1003) nwrites,jtot
1003	format(' Records written='i6,' for nphots='i10)
	close(11)
c	At this point we have a set of sorted and merged super blocks.
c	out on the scratch disk file. We can now delete the input pe
c	file to reduce the disk space required by this program. Do
c	this by reopening the file with the dispose='delete' option
c	and then closing it.
	if(pe_delete)then
		print*,' Deleteing .pe  and .pe.head files'

                pe_head_input_file=trim(pe_input_file)//".head"
                OPEN(11,ACCESS='sequential',STATUS='old',
     1               file=trim(pe_head_input_file),FORM='UNFORMATTED',
     1               dispose='DELETE',iostat=ios)
                if (ios>0)then
                   print*,'KASSRTMRG-Delete of input PE headfile open error:',
     1          trim(pe_head_input_file)  
                   stop 'Input PE head file open failure'
                endif
		close(unit=11)

		open (11,access='sequential',form='unformatted',status='old',
	1 dispose='DELETE',recl=pe_size,iostat=ios,file=trim(pe_input_file))
                if (ios>0)then
                   print*,'KASSRTMRG-PE file open for delete error:',
     1    trim(pe_input_file)
                   stop 'PE file delete open failure'
                endif
		close(unit=11)

	endif

c	Each super block is 32 records
c	in this file.  Merge them into the final output file.
c	12/28/90 G.H.S. V:1:0:1:0.0
c		Handle zero length pe files.
	if(jtot.ne.0)call bigmrg

	if(index(options,'PESFILE=BIN').ne.0)then
		close(unit=14)
	else
		print*,' Closeing sorted HDF5 pes file.'
		print*,' Number of PES in HDF5 pes file:',icount
		call peshdf5_close
	endif
        print*," KASSRTMRG Normal End"
        print*,"!****************************************************"
	end
cccccccccc


	subroutine bigmrg
c	Merge the 'superblocks' which are stored as 32 records each on the
c	output scratch file.
c	Partition the file into groups of 8 superblocks and then merge the
c	individual groups into new super blocks that are 8 times bigger then
c	the previous superblocks. Keep repeating this until were left with 1
c	last meerge.

c	Modifications:

c	03/6/92 G.H.S. V:1:0:1:0.1
c		For SGI compatability, remove explicit file name reference in
c		OPEN statements for SCRATCH files. Scratch disk determined by
c		setenv TMPDIR --- statement in UNIX script file and with
c		DEFINE SYS$SCRATCH --- in VMS DCL command file.

c	16/11/93 GHS V:1:0:2:1.0
c		Change the number of merg blocks in BIGMERG to a parameter
c		(N_MERGE) from the value 8. This is to improve performance.
c		Try a value of N_MERGE = 16 to start with.

c	29/11/93 GHS V:1:0:2:1.0
c		Try M_MERGE=32. I thinks thats the biggest it can be made.

!       05/05/02 GHS V:1:3:7:2.1
!               Make internal scratch pe files normal direct access files with 
!               dispose=delete option so they  delete when closed. Give them 
!               specific names so that they go in same directory as the 
!               pe_input_file(add extensions 'scr11' and 'scr12' to 
!               pe_input_file name). This puts them in a specific place where 
!               we should know if there is room. Otherwise 'scratch' files are
!               placed wherever the fortran compiler thinks they should go 
!               (/tmp for absoft). Harder to make surte there is room for them.



	include 'kassrtmrg_command_line.h'
        character*80 scratch_file_name
        character*2 unit_file
	include 'kassrtmrg.h'
c	Number of super blocks to merge at a time. 
	parameter (n_merge=32)
	real*4 mrg(block_size,n_merge)
	integer*4 imrg(block_size,n_merge),idone(block_size),itemp
!	equivalence (imrg(1,1),mrg(1,1))
	equivalence (imrg,mrg)
	integer*4 iptr(n_merge),jptr(n_merge),jptrmax(n_merge),jtemp
	integer*2 itemp2(2)
	equivalence (itemp,itemp2(1))
	integer ios
	logical endflag,lodd,start/.true./
	
c
	mr=nwrites	!nwrites is total number of records in scratch file.
	endflag=.false.
c
	DO IMRGERS=1,10
		nw=0		!new number of writes
		mm=mod(imrgers,2)
		if(mm.eq.0) then
			lodd=.false.
		else
			lodd=.true.
		end if
c	flip-flop unit numbers.
		nfile=11+mm	!old file
		mfile=12-mm	!new file
c
		if(start)then
			ns=32
			start=.false.
		else
			ns=ns*n_merge	!initial value of length of sorted 
					!string
		endif
		nf=ns*n_merge	!final value. Only work on N_MEREGE 
					!superblocks at a time.
	
		if(nf.ge.mr) then
			endflag=.true.	!ONLY place to set endflag
			Print*,' setting endflag to true'
c		final file.
			mfile=14

		else
c		Open new merge scratch
                   write(unit_file,1024)mfile
 1024           format(i2)
                   scratch_file_name=trim(pe_input_file)//'scr'//unit_file
                   if(lodd) then !This is unit 11.
                      open (mfile,access='direct',dispose='delete',
	1	  status='new',form='unformatted',recl=recl_size,
	1         iostat=ios,file=trim(scratch_file_name))
                 	  if (ios>0)then
                   print*,'KASSRTMRG-Scratch file 11 open error:',
     1         trim(scratch_file_name)
                   stop 'Scratch file open failure'
                          endif

			  jnext=1
			  print*,' 11:opening direct access mfile:',
     1         trim(scratch_file_name)
			else
				!This is unit 12.
			  open (mfile,access='direct',dispose='delete',
	1	  status='new',form='unformatted',recl=recl_size,
	1         iostat=ios,file=trim(scratch_file_name))
                 	  if (ios>0)then
                   print*,'KASSRTMRG-Scratch file 12 open error:',
     1         trim(scratch_file_name)

                              stop 'Scratch file open failure'
                          endif
			  print*,' 12:opening direct access mfile:',
     1         trim(scratch_file_name)
			  inext=1
			end if
		end if
c
		jcrnt=1
		jmax=n_merge
		do j=1,n_merge
			iptr(j)=-1		!precaution
			jptr(j)=1+(j-1)*ns
			jptrmax(j)=j*ns
			if(jptrmax(j).ge.mr) then
				jptrmax(j)=mr
				js=j+1
				jmax=j
				if(js.le.n_merge) then
					do k=js,n_merge
						jptr(k)=0
						jptrmax(k)=-1
						iptr(k)=-1
					end do
					go to 20
				end if
			end if
		end do
c
20		do i=1,jmax
		   irec=jptr(i)
		   if(irec.gt.0) then
			if (lodd) then
				inext=irec
				read(nfile,rec=inext,iostat=ios)(imrg(ii,i),
	1 ii=1,block_size)
                 	  if (ios>0)then
            print*,'KASSRTMRG-Scratch file read error.nfile:ios:inext',
	1 nfile,ios,inext
                              stop 'Scratch file read failure'
                          endif

				inext=inext+1	!May not really be needed but
						!it makes it safe.
			else
				jnext=irec
				read(nfile,rec=jnext,iostat=ios)(imrg(ii,i),
	1 ii=1,block_size)
                 	  if (ios>0)then
            print*,'KASSRTMRG-Scratch file read error.nfile:ios:jnext:',
	1 nfile,ios,jnext
                              stop 'Scratch file read failure'
                          endif
				jnext=jnext+1	!May not be needed
			end if
			iptr(i)=1
		   end if
		end do
c
c	write(6,1300) imrgers,jptrmax
c1300	format(' imerger, jptrmax='i3,8i6)
c
		kout=1
1		i=iptr(jcrnt)
		ixymin=imrg(i,jcrnt)
		tmin=mrg(i+1,jcrnt)
		jmin=jcrnt
		if(jcrnt.lt.jmax) then
		   do j=jcrnt+1,jmax
			i=iptr(j)
			if(i.gt.0) then		!flag
				id=imrg(i,j)
				if(id.lt.ixymin) then
					ixymin=id
					tmin=mrg(i+1,j)
					jmin=j
					go to 3
				end if
				td=mrg(i+1,j)
				if((id.eq.ixymin).and.(td.lt.tmin)) then
					tmin=td
					jmin=j
				end if
			end if
3		   continue
		   end do
		end if
c
		imin=iptr(jmin)
		do i=0,pe_size-1
			idone(kout+i)=imrg(imin+i,jmin)
		end do
		kout=kout+pe_size
		if(kout.gt.block_size) then
			if(endflag) then
				ilast=block_size
				if((nwrites-nw).eq.1) then
					do ij=1,block_size,pe_size
					   if(idone(ij).eq.nxny_flag) then
						ilast=ij-1
						go to 5
					   end if
					end do
				end if
5				do ij=1,ilast,pe_size
					!itemp and itemp2 equivalenced!
c	3/22/90 G.H.S.
c		restore nx and ny here.
c	10/21/91 G.H.S. V:1:0:1:0.1
c		 Use machine independent algorithum.
c		Worry about i*2 int. overflows during calculation.
					jtemp=(idone(ij)/32000)
					itemp2(1)=(jtemp-16000)
					itemp2(2)=idone(ij)-
	1 (jtemp*32000)-16000
					idone(ij)=itemp
!***************************************************************************
!       19/10/00 GHS V:1:1:5:1.2
!               Test OPTIONS to see if output file is PESFILE=BIN
!               Otherwise assume output file is HDF5 format.
!  07/05/03 GHS Ouput file is now direct access for large file ability.
		if(index(options,'PESFILE=BIN').ne.0)then
			write(mfile,rec=ipes_next,iostat=ios)
     1                                        (idone(k),k=ij,ij+pe_size-1)
              		if (ios>0)then
                 print*,'KASSRTMRG-Output PES data file write error.ios:',ios
                           print*,'Retrying failed PES file write'
                           write(mfile,rec=ipes_next,iostat=ios)
     1                          (idone(k),k=ij,ij+pe_size-1)
                           if (ios>0)then
                 print*,'KASSRTMRG-Output PES data file write error on retry'
                              print*,'I quit!'
                              stop 'Output PEs file write failure'
                           endif
                        endif
                        ipes_next=ipes_next+1
		else
!HDF5 Write it out. Pe event # start at 0.(i.e. index=icount-1)
			icount=icount+1
			do ip=1,pe_size
				k=ij+ip-1
				rec_pe.irecbuf(ip)=idone(k)
			enddo
			call peshdf5_out(icount-1,rec_pe)
		endif
!***************************************************************************

				call check(idone(ij),idone(ij+1))
				end do

			elseif(lodd) then
				write(mfile,rec=jnext,iostat=ios) idone

		              	if (ios>0)then
                print*,'KASSRTMRG-Scratch file write error.mfile,ios,jnext:',
	1 mfile,ios,jnext
                   		   stop 'Output Scratch file write failure'
             		 	endif

				jnext=jnext+1	!May not be needed
			else
				write(mfile,rec=inext,iostat=ios) idone
		              	if (ios>0)then
                print*,'KASSRTMRG-Scratch file write error.mfile,ios,inext:',
	1 mfile,ios,inext
                   		  stop 'Output Scratch file write failure'
             		 	endif
				inext=inext+1	!May not be needed.
			end if
	
			nw=1+nw
			if(nw.eq.nwrites) go to 500
			kout=1
		end if
		iptr(jmin)=pe_size+iptr(jmin)
		if(iptr(jmin).lt.block_size) go to 1
c
c	now see if we can fill jmin buffer
		if(jptr(jmin).lt.jptrmax(jmin)) then
			jptr(jmin)=1+jptr(jmin)
			if(lodd) then
				inext=jptr(jmin)
				read(nfile,rec=inext,iostat=ios)
	1(imrg(ii,jmin),ii=1,block_size)
                 	  if (ios>0)then
            print*,'KASSRTMRG-Scratch file read error.nfile:ios,inext:',
	1 nfile,ios,inext
                              stop 'Scratch file read failure'
                          endif
				inext=inext+1	!May not be needed.
			else
				jnext=jptr(jmin)
				read(nfile,rec=jnext,iostat=ios) 
	1 (imrg(ii,jmin),ii=1,block_size)
                 	  if (ios>0)then
            print*,'KASSRTMRG-Scratch file read error.nfile:ios:jnext:',
	1 nfile,ios,jnext
                              stop 'Scratch file read failure'
                          endif
				jnext=jnext+1	!May not be needed
			end if
			iptr(jmin)=1
		else
			iptr(jmin)=-1
		end if
		do j=1,n_merge
			jcrnt=j
			if(iptr(j).gt.0) go to 1
		end do
c	We can't get here if endflag set or if a pass is done, so here we
c	try to refill all of our buffers.
c
		nss=jptrmax(n_merge)	!last place we looked
		jcrnt=1
		do j=1,n_merge
			iptr(j)=1
			jptr(j)=nss+1+(j-1)*ns	!can't exceed mr
			jptrmax(j)=jptr(j)+ns-1
			if(jptrmax(j).ge.mr) then
				jptrmax(j)=mr
				jmax=j
				js=j+1
				if(js.le.n_merge) then
					do k=js,n_merge
						jptr(k)=0
						jptrmax(k)=-1
						iptr(k)=-1
					end do
					go to 20
				end if
			end if
		end do
		go to 20
c
500		close(unit=nfile)  !file should delete on closing.

		if(endflag) then
			write(6,1005) imrgers
1005	format(' Done after 'i2,' big merges.')
			if(mfile.ne.14)then
                           close(unit=mfile)  !file should delete on closing

                        endif
			return	!HAPPY DAYS RETURN
		else
			write(6,1006) imrgers
1006	format(' Just finished merge number 'i2,'.')
		end if
	END DO
	STOP 'KASSRTMRG_FATAL--TOO MANY MERGES: More then 10!'
	end
cccccccccc

	subroutine check(itemp,time)
	common /check1/nsort,nx,ny,ttime,ifirst
	integer*4 itemp,jtemp
	integer*2 nxy(2)
	equivalence (jtemp,nxy(1))
	real*4 time
	integer ifirst

	if (ifirst.ne.2)then
		print*,' Checking sort results.'
		ifirst=2
	endif
	jtemp=itemp
	nsort=nsort+1
	if(nsort.ne.1)then
		if(nxy(1).eq.nx.and.nxy(2).eq.ny.and.
	1 time.lt.ttime)then
                   print*,' KASSRTMRG_CHECK_error--TIME out of seq:Rec #:',
	1 nsort
                   print*,' nx,nxy(1),ny,nxy(2),ttime,time:',
	1 nx,nxy(1),ny,nxy(2),ttime,time
		elseif(nxy(1).eq.nx.and.nxy(2).lt.ny)then
                   print*,' KASSRTMRG_Check-error--NY out of seq:Rec #:',
	1 nsort
                   print*,' nx,nxy(1),ny,nxy(2),ttime,time:',
	1 nx,nxy(1),ny,nxy(2),ttime,time
		elseif(nxy(1).lt.nx)then
                   print*,' KASSRTMRG_Check-error--NX out of seq:Rec #:',
	1 nsort
                   print*,' nx,nxy(1),ny,nxy(2),ttime,time:',
	1 nx,nxy(1),ny,nxy(2),ttime,time

		else
                   nx=nxy(1)
                   ny=nxy(2)
                   ttime=time
		endif
	else
           nx=nxy(1)
           ny=nxy(2)
           ttime=time
	endif
	return
	end

        subroutine datain

c        Read in the input parameters from the parameter file.

c	Written 10/15/90 
c		G.H.S.

c     Modified:

c	1/25/91 G.H.S. V:1:0:1:0.0
c		In order to reduce the total amount of disk space needed to do
c		a sort we are adding a flag PE_DELETE to the .inp file that 
c               indicates
c		that we can delete the input pe file before we start writing
c		out the sorted PES file. This could be a little dangerous to 
c               do if the
c		program bombs in the middle the pes file creation.

c	10/21/91 G.H.S.V:1:0:1:0.1
c		For machine independence replace VMS for$date, for$time calls
c		with DATE and TIME.

c	27/10/93 GHS V:1:0:2:1.0
c		Remove different file format option.

!       19/10/00 GHS V:1:1:5:1.2
!               Add OPTIONS to input common block and DATAIN
!               Test OPTIONS to see if output file is PESFILE=BIN
!               Otherwise assume output file is HDF5 format.
!       30/04/02 GHS V:1:3:7:2.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               ./kassrtmrg  -p kassrtmrg.par  (sortof like in c).
!               Setup for command line input parameter use.(Changes in main,
!               DATIN, FILEOPEN

	include 'kassrtmrg_command_line.h'
c       19/10/00 GHS 
        InCLUDE 'kassrtmrg.h'
	integer ios
	character*8 adate
	character*10 time          	

c      GET TIME AND DATE OF THIS RUN(call f90 interface routine)
	call datetime(adate,time)

	WRITE(6,1000)adate(1:4),adate(5:6),adate(7:8),time(1:2),
	1 time(3:4),time(5:)
1000	format(' ',15x,a4,' ',a2,'/',a2,5x,a2,':',a2,':',a,
	1   '--------KASSRTMRG------')

c	Open input data file.
        OPEN(1,ACCESS='SEQUENTIAL',STATUS='OLD',READONLY,
     1      file=input_par_file_name,iostat=ios)
            if (ios>0)then
                print*,'KASSRTMRG-Input parmeter file open error:',
     1             input_par_file_name
                stop 'Input parameter file open failure'
            endif
				!Logical flag that indicates if this program
	read(1,1002,iostat=ios)pe_delete 
             if (ios>0)then
	            print*,'KASSRTMRG-Input parmaeter error reading pe_delete'
                    stop 'Input parameter file read failure'
             endif
				!should delete the PE file before the PES file
				!is created.  This reducers the amount or 
				!required didsk space by 33%.
1002	format(l)
	write(6,1012)pe_delete
1012	format(' PE_DELETE:.true.=Delete pe file before pes built =',l)
!19/10/00 GHS
	read(1,1003,iostat=ios)options
1003	format(a)
	if(ios.ne.0)then
		print*,' Read failure on input file:fort.1 ios:',ios
		print*,' If IOS=-1 then eof was found' 
		print*,' If IOS>0 then some error occured'
		stop 'Read Failure on input file'
	endif

	write(6,1013)trim(options)
1013	format(' OPTIONS:',a)
	return
        end


	subroutine fileopn
c	Opens int input,scratch and output files and transfers the headers
c	from input to output.
c	written: 10/15/90 G.H.S.

c	Modified:

c	10/21/91 G.H.S.V:1:0:1:0.1
c		Use machine_type.h to determine if we can use fixed record
c		format for sequential files.

c	03/6/92 G.H.S. V:1:0:1:0.1
c		For SGI compatability, remove explicit file name reference in
c		OPEN statements for SCRATCH files. Scratch disk determined by
c		setenv TMPDIR --- statement in UNIX script file and with
c		DEFINE SYS$SCRATCH --- in VMS DCL command file.

c	27/10/93 GHS V:1:0:2:1.0
c               Note new version of KASLITE and thus this is a major new
c		version. Needed since were changing the input file format.
c		This is done in a structure which allows for future format
c		changes without changing this code. Old compatabily can be
c		retained by changing KASSTRUCTURES.H which is the pe file
c		record structure. Remove different file format option.

c	16/12/97 GHS V:1:1:5:1.0
c		Add printout of pe_head.x_offset, pe_head.y_offset
c		The addition of X_OFFSET AND Y_OFFSET to pe_head causes
c		a major version number change for KASLITE.

!	23/9/99 GHS  V:1:1:5:1.1
!		Remove name of scratch file when opening unit 12.

!       19/10/00 GHS V:1:1:5:1.2
!               Add OPTIONS to input common block and DATAIN
!               Test OPTIONS to see if output file is PESFILE=BIN
!               Otherwise assume output file is HDF5 format.

!       30/04/02 GHS V:1:3:7:2.0
!               Major version change. This version gets various input 
!               parameters and file names from the command line:
!               ./kassrtmrg  -p kassrtmrg.par  (sortof like in c).
!               Setup for command line input parameter use.(Changes in main,
!               DATIN, FILEOPEN

!       07/05/02 GHS V:1:3:8:3.0
!               Another Major version change. To handle very large pe and pes
!               files (showers ~1 Tev+) use a direct access file for the pe 
!               input and pes output files (we already use direct access for 
!               the internal pe scratch sorting files). 
!               Requires getting and putting the header record in a seperate 
!               file (pe_input_file//'.head',pes_output_file//'.head' )

	include 'kassrtmrg_command_line.h'
        character*80 pe_head_input_file
        character*80 pes_head_output_file
        character*80 scratch_file_name
	include 'kassrtmrg.h'
	character*12 ptype(9)
	character*8 namtyp(18)
	integer ios
!******************************************************************************
!      Heavy Nuclei declarations block:
!******************************************************************************
	real xmass
	integer qz,ia
	character*15 nuclei_names(113)
			 !see below for nuclei_names data statement

	data ptype/' Visable    ',' Old Luny   ',' Solar Blind',
	1 ' CEASAR     ',' ASGAT      ',' Part-on-gnd','Whipple',
	2 ' VERITAS','VERITAS-TRI'/

c        Particle names.
        data namtyp/'Gamma   ','Positron','Electron','Mu +    ',
     1 'Mu -    ','PI 0    ','PI +    ','PI -    ','K +     ',
     2 'K -     ','K long  ','K short ','Proton  ','Neutron ',
     3 'eNeutrin','anti-eNu','muNeutri','ant-muNu'/

	data nuclei_names/'Hydrogen', 'Helium', 'Lithium', 
	1'Beryllium',
	1'Boron', 'Carbon','Nitrogen', 'Oxygen', 'Fluorine', 'Neon', 
	1'Sodium', 'Magnesium', 'Aluminum', 'Silicon','Phosphorous', 
	1'Sulfur','Chlorine', 'Argon', 'Potassium', 'Calcium', 
	1'Scandium', 'Titanium','Vanadium', 'Chromium', 'Manganese', 
	1'Iron', 'Cobalt', 'Nickel', 'Copper', 'Zinc', 'Gallium',
	1'Germanium', 'Arsenic', 'Selenium', 'Bromine', 'Krypton', 
	1'Rubidium', 'Strontium', 'Yttrium', 'Zirconium', 'Niobium', 
	1'Molybdenum', 'Technetium', 'Ruthenium', 'Rhodium', 
	1'Palladium','Silver', 'Cadmium', 'Indium', 'Tin', 
	1'Antimony', 'Tellurium','Iodine', 'Xenon', 'Cesium',
	1'Barium', 'Lanthanum', 'Cerium', 'Praseodymium', 
	1'Neodymium', 'Promethium', 'Samarium','Europium', 
	1'Gadolinium','Terbium', 'Dysprosium', 'Holmium', 'Erbium', 
	1'Thulium', 'Ytterbium','Lutetium', 'Hafnium', 'Tantalum', 
	1'Tungsten', 'Rhenium', 'Osmium', 'Iridium','Platinum', 
	1'Gold', 'Mercury', 'Thallium', 'Lead', 'Bismuth', 
	1'Polonium', 'Astatine','Radon', 'Francium', 'Radium', 
	1'Actinium', 'Thorium', 'Proactinium', 'Uranium', 
	1'Neptunium', 'Plutonium','Americium', 'Curium', 
	1'Berkelium', 'Californium', 'Einsteinium','Fermium', 
	1'Mendelevium', 'Nobelium', 'Lawrencium', 'Rutherfordium', 
	1'Dubnium', 'Seaborgium','Bohrium', 'Hassium', 
	1'Meitnerium', 'Unununium', 'Ununbium', 'Ununtrium', 
	1'Ununquadium'/


c	Input pe(or POG) header file.
	pe_head_input_file=trim(pe_input_file)//".head"
        OPEN(11,ACCESS='sequential',STATUS='old',
	1 file=trim(pe_head_input_file),FORM='UNFORMATTED',iostat=ios)
        if (ios>0)then
           print*,'KASSRTMRG-Input PE file open error:',
     1          trim(pe_head_input_file)  
           stop 'Input PE head file open failure'
        endif
	read(11,iostat=ios)(pe_head.recbuf(i),i=1,pehead_size)
        if (ios>0)then
           print*,'KASSRTMRG-Input PE head file read error',
     1          trim(pe_head_input_file)  
           stop 'Input PE head file read failure'
        endif
        close(11)            !Done with it.

!Now open the directaccess file for the input pe's.
	  open (11,access='direct',status='old',form='unformatted',
     1           recl=pe_size,iostat=ios,file=trim(pe_input_file))
          if (ios>0)then
             print*,'KASSRTMRG-Fileopn-Pe data input file open error:',
     1            trim(pe_input_file),ios
             stop 'Pe Input data file open failure'
          endif
          ipe_next=1   !init the index counter for this file.

c	Scratch file.
                   scratch_file_name=trim(pe_input_file)//'scr12'
                   print*,'Scratch_file_name:',trim(scratch_file_name)

	  open (12,access='direct',status='new',form='unformatted',
	1 recl=recl_size,iostat=ios,dispose='delete',
     1    file=trim(scratch_file_name))
              if (ios>0)then
                   print*,'KASSRTMRG-Fileopn-Scratch file open error:',
     1       trim(scratch_file_name),ios
                   stop 'Scratch file 12 open failure'
              endif


	if(index(options,'PESFILE=BIN').ne.0)then
c     Binary Output file.
!  07/05/03 GHS Convert ouput pes file to 2 files. First is header 
!  (pes_output_file//'.head') Second is direct access file. Do so we can make 
!  large files.

	print*,' Output PES file is in unformatted direct access format.'
	pes_head_output_file=trim(pes_output_file)//".head"
        open (14,access='sequential',status='new',
	1 form='unformatted',file=trim(pes_head_output_file),iostat=ios)
        if (ios>0)then
           print*,'KASSRTMRG output PES head file open error',
     1          trim(pes_head_output_file)
           stop 'Output PES head file open failure'
        endif

c	Transfer header informatrion to ouput file..
        write(14,iostat=ios)(pe_head.recbuf(i),i=1,pehead_size)
        if (ios>0)then
           print*,'KASSRTMRG-PES head file write error.ios:',ios
           stop 'PES head file write failure'
        endif
        close(14)

        open (14,access='direct',status='new',form='unformatted',
     1           recl=pe_size,iostat=ios,file=trim(pes_output_file))
        if (ios>0)then
           print*,'KASSRTMRG-Fileopn-Pes data output file open error:',
     1       trim(pes_output_file),ios
           stop 'Pes Output data file open failure'
        endif
        ipes_next=1

	else

!	open the output HDF file and write out the header summary file.
						!These are C routines.
	        print*,' Output PES file is in HDF5 format.'
		call peshdf5_create(pes_hdf5_ouput_file)
			     !Write out the pehead record into its own dataset.
        	call peshdf5_pehead_write(pe_head)
		icount=0
	endif


	dni=sqrt(1-pe_head.segment_head.dli**2-
	1 pe_head.segment_head.dmi**2)
	dn=-sqrt(1-pe_head.dl**2-pe_head.dm**2)	   !Mount is always looking up.


	if(index(pe_head.petype,'VI').ne.0)then
		pt=1
	elseif(index(pe_head.petype,'OL').ne.0)then
		pt=2
	elseif(index(pe_head.petype,'SB').ne.0)then
		pt=3
	elseif(index(pe_head.petype,'C').ne.0)then
		pt=4
	elseif(index(pe_head.petype,'AS').ne.0)then
		pt=5
	elseif(index(pe_head.petype,'POG').ne.0)then
		pt=6
	elseif(index(pe_head.petype,'W').ne.0)then
		pt=7
	elseif(index(pe_head.petype,'VA').ne.0)then
		pt=8
	elseif(index(pe_head.petype,'VT').ne.0)then
		pt=9
	else
		write(6,1001)pe_head.petype
1001	format(' KASSRTMRG_FATAL--Illegal photon type: pe_head.petype=',a)
		stop

	endif
	if(pe_head.segment_head.itype.gt.20)then
		ia=pe_head.segment_head.itype-20
		call mass_number2charge_mass(ia,qz,xmass)
 	        write(6,2009)trim(nuclei_names(qz)),ia
2009  format(' SHOWER FILE PARAMETERS:',/,
	1 '  Primary for this shower was:  ',a,'(',i3,')')
	else
	        write(6,2008)namtyp(pe_head.segment_head.itype)
2008  format(' SHOWER FILE PARAMETERS:',/,
	1 '  Primary for this shower was:  ',a)
	endif


	write(6,1002)ptype(pt),
	1 pe_head.segment_head.tep,pe_head.segment_head.dli,
	1 pe_head.segment_head.dmi,dni,pe_head.segment_head.depth,
	1 pe_head.dl,pe_head.dm,dn,pe_head.hobs,
	1 pe_head.segment_head.magnet_field,pe_head.segment_head.etr,
	1 pe_head.xseg,pe_head.yseg,pe_head.segment_head.idfile,
	1 pe_head.x_offset,pe_head.y_offset,
	1 pe_head.segment_head.version,pe_head.version
1002	format(' SHOWER SPECIFICATION:',/,
	1 '                Detector type: ',a,/,
	1 '               Primary Energy: ',f10.5,' TeV',/
	1 '        Primary  DLI DMI DNI : ', 3e14.7,/,
	1 '              Injection depth: ',f10.2,/,
	1 '               Mount dl,dm,dn: '3e14.7,/,
	1 '            Detector altitude: ',f10.1,/,
	1 ' Magnetic Field specification: ',a,/,
	2 '                Thershold MeV: ' f8.2,/,
	1 '                    xseg,yseg: ',f8.2,' x',f8.2,/,
	1 '                       IDFILE: ',i5,/,
	1 '   Shower Offset Impact Point: ','x=',f6.2,' y=',f6.2,/,
	1 '         From KASCADE Version: ',a,/,
	1 '         From KASLITE Version: ',a)
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
!******************************************************************************

	subroutine pmerge(jptrlast)
c	Merge the jptrlast blocks in the mrg array an write out this
c	resultant 'super block' to the scratch disk file as 32 records.

	include 'kassrtmrg.h'
	common/tans/mrg(block_size,32)
	real*4 mrg
	integer*4 imrg(block_size,32),idone(block_size)
	equivalence (imrg(1,1),mrg(1,1))
	integer*4 iptr(32)
	logical endflag
	integer ios
c
	endflag=.false.
	do j=1,jptrlast
		iptr(j)=1
	end do
	kout=1
	jcrnt=1
c
1	i=iptr(jcrnt)
	ixymin=imrg(i,jcrnt)
	tmin=mrg(i+1,jcrnt)
	jmin=jcrnt
	if(jptrlast.gt.jcrnt) then
	   do j=jcrnt+1,jptrlast
		i=iptr(j)
		if(i.gt.0) then		!flag
			id=imrg(i,j)
			if(id.lt.ixymin) then
				ixymin=id
				tmin=mrg(i+1,j)
				jmin=j
				go to 3
			end if
			td=mrg(i+1,j)
			if((id.eq.ixymin).and.(td.lt.tmin)) then
				tmin=td
				jmin=j
			end if
		end if
3	   continue
	   end do
	end if
c
	imin=iptr(jmin)
	do i=0,pe_size-1
		idone(kout+i)=imrg(imin+i,jmin)
	end do
	kout=kout+pe_size
	if(kout.gt.block_size) then
							!Write this out
		write(12,rec=inext,iostat=ios) (idone(i),i=1,block_size)
	       		if (ios>0)then
           print*,'KASSRTMRG Scratch file(fort.12) write error.ios,inext:',
	1 ios,inext
			stop 'Scratch file write failure.'
		endif
		
		inext=inext+1	!May not be needed.
		nwrites=1+nwrites
		if(endflag) return
		kout=1
	end if
	iptr(jmin)=pe_size+iptr(jmin)
	if(iptr(jmin).gt.block_size) then
		iptr(jmin)=-1
		do j=1,jptrlast
		   jcrnt=j
		   if(iptr(j).gt.0) go to 1
		end do
		return		!we are done if no positive iptr's
	end if
	go to 1
	end
cccccccccc

