/* kashdf5.c */

/* This set of functions is written to provide interface between
 * KASTRIGGER/PES2HDF5/KASAOMEGA/M2HDF5 and HDF5. 
 *
 * It supports the HDF5 pes data file. (Routines peshdf5*,mhdf5*)
 *
 * It supports the reading of the PST patterns from a HDF5 file.
 * (wcamera_psthdf5_get).
 *
 * 19/9/00 GHS
 *
 */ 
/* Modified:
 * 18/04/03 GHS
 * Specify file name in peshdf5_create and peshdf5_open
 *
 * 20/10/04 GHS
 *Change i_threshold variable in mrec and hdf5 file to ithphi. Also make it i*4
 */

#include "kashdf5.h"


/* DEFINE STATEMENTS */
#define CHUNK_SIZE_PES         50          /* size of the chunk            */ 
#define CHUNK_SIZE_MREC        5000       /* size of the chunk            */ 
#define COMPRESSION_PES        6           /* compression effort           */

/* FUNCTION DECLARATIONS:  Private */
/* STATIC VARIABLES */
static  hid_t pes_file;                     /* peshdf5 file identifier  */
static  hid_t times_type,pix_type,ipix_type,mrec_type;
static  hid_t mdata_type;                 /* mhdf5 mrec data record type 
					  * identifier  */
static hid_t mhead_type;                 /* mhdf5 mhead type identifier  */

/*-------------------------------------------------------------------------
 * Function:    peshdf5_create
 *
 * Purpose:     Creates pes HDF5 file.
 *              This program also isolates the HDF5 file
 *              identifier from the user by making it static 
 *              variable and limiting its scope to only this file. 
 *
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *              19/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void peshdf5_create(char *pesfilename)
{ 
    /*
     * Create a file.
     */
    pes_file = H5Fcreate(pesfilename, H5F_ACC_TRUNC, 
                         H5P_DEFAULT, H5P_DEFAULT);


}
/*-------------------------------------------------------------------------
 * Function:    peshdf5_open
 *
 * Purpose:     Opens an existing pes HDF5 file.
 *              This program also isolates the HDF5 file
 *              identifier from the user by making it static 
 *              variable and limiting its scope to only this file. 
 *
 *
 * Return:      VOID
 *
 * Programmer:  GHS
 *              20/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void peshdf5_open(char *pesfilename,int *ierr)
{ 
    /*
     * open a file.
     */
    pes_file = H5Fopen(pesfilename, H5F_ACC_RDONLY,H5P_DEFAULT);

    if(pes_file<0)  /* Does file exist? */
      {
	*ierr=-1;
	return;  /* no! */
      }
    else
      {
        *ierr=0;
	return;   /* yes! */
      }
}
/*-------------------------------------------------------------------------
 * Function:    peshdf5_pehead_write
 *
 * Purpose:     Writes pes header record  into existing peshdf5 file
 *		as a dataset attached to the root.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		19/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void peshdf5_pehead_write(head *pehead)
 {
    hid_t    dataspace, dataset;             /* handles */
    hid_t    cparms;
//    hid_t    filespace;
    hid_t    memtype_id,disktype_id;
    hid_t    s4,s8,s16;
//    hsize_t  vrbl;
    hsize_t  dims=1;
//    hsize_t  size=1;
//    hssize_t offset=0;

    herr_t   status;


	/* first create pehead dataspace ,1 element of a single compound 
	 * datatype*/

      dataspace  = H5Screate_simple(1, &dims, &dims); 



	/* Next create the compound data type that describes the pe_header
	 * record. Once the memory version is made we will make the disk
         * based version which is packed. Remember all HDF5 transferes 
	 * are from
         * a source to a destination(both defined).
	 * We have a struct for pe_head called head.*/

    memtype_id = H5Tcreate (H5T_COMPOUND, sizeof(head));

		/* PE_HEAD*/
    H5Tinsert (memtype_id, "itype", HOFFSET(head,itype),H5T_NATIVE_INT);
    H5Tinsert (memtype_id, "tep", HOFFSET(head,tep),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "dli", HOFFSET(head,dli),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "dmi", HOFFSET(head,dmi),H5T_NATIVE_FLOAT);
		/* Character arrays are funny
		 * Make up a specific char type*/
		s4=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s4, 4);
    H5Tinsert (memtype_id, "magnet_field",HOFFSET(head,magnet_field),s4);

    H5Tinsert (memtype_id, "etr", HOFFSET(head,etr),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "depth", HOFFSET(head,depth),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "zobs", HOFFSET(head,zobs),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "xinitial", HOFFSET(head,xinitial),
		H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "yinitial", HOFFSET(head,yinitial),
		H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "idfile", HOFFSET(head,idfile),H5T_NATIVE_INT);
		s8=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s8, 8);
    H5Tinsert (memtype_id, "version_seg",HOFFSET(head,version_seg),s8);

		/* PE_HEAD*/
    H5Tinsert (memtype_id, "dl", HOFFSET(head,dl),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "dm", HOFFSET(head,dm),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "xseg", HOFFSET(head,xseg),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "yseg", HOFFSET(head,yseg),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "hobs", HOFFSET(head,hobs),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "x_offset", HOFFSET(head,x_offset),
		H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "y_offset", HOFFSET(head,y_offset),
		H5T_NATIVE_FLOAT);
		s16=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s16, 16);
    H5Tinsert (memtype_id, "petype", HOFFSET(head,petype),s16);
    H5Tinsert (memtype_id, "version_pe", HOFFSET(head,version_pe),s16);
		/* Duplicate this type for the disk but then pack it*/
    disktype_id = H5Tcopy (memtype_id);
    /*		 H5Tpack (disktype_id);*/
		 H5Tclose(memtype_id); 	/*release memtype_id*/


   /* 
     * GEt dataset creation properties*/
      cparms     = H5Pcreate (H5P_DATASET_CREATE);
 
    /*
     * Create a new dataset within the file using cparms
     * creation properties.
     */
      dataset    = H5Dcreate(pes_file, "PEHEAD", disktype_id, 
                            dataspace, cparms);

     /* Write the data to the disk dataset.*/
      status     = H5Dwrite(dataset, disktype_id, H5S_ALL, 
                   H5S_ALL, H5P_DEFAULT, pehead);

    /*
     * Release resources.
     */
                   H5Pclose(cparms);
                   H5Sclose(dataspace);
                   H5Dclose(dataset);
		   H5Tclose(disktype_id);

}
/*-------------------------------------------------------------------------
 * Function:    peshdf5_pehead_read
 *
 * Purpose:     Reads pes header record  from existing peshdf5 file
 *		from  a dataset attached to the root.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		20/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void peshdf5_pehead_read(head *pehead)
 {
    hid_t    dataset;             /* handles */
    hid_t    memtype_id;
    hid_t    s4,s8,s16;

    herr_t   status;


	/* Create the compound data type that describes the pe_header
	 * record. 
	 * We have a struct for pehead called head.*/


    /* printf(" Define memtype_id\n"); */
    memtype_id = H5Tcreate (H5T_COMPOUND, sizeof(head));

		/* PE_HEAD*/
    H5Tinsert (memtype_id, "itype", HOFFSET(head,itype),H5T_NATIVE_INT);
    H5Tinsert (memtype_id, "tep", HOFFSET(head,tep),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "dli", HOFFSET(head,dli),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "dmi", HOFFSET(head,dmi),H5T_NATIVE_FLOAT);
		/* Character arrays are funny
		 * Make up a specific char type*/
		s4=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s4, 4);
    H5Tinsert (memtype_id, "magnet_field",HOFFSET(head,magnet_field),s4);

    H5Tinsert (memtype_id, "etr", HOFFSET(head,etr),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "depth", HOFFSET(head,depth),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "zobs", HOFFSET(head,zobs),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "xinitial", HOFFSET(head,xinitial),
		H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "yinitial", HOFFSET(head,yinitial),
		H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "idfile", HOFFSET(head,idfile),H5T_NATIVE_INT);
		s8=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s8, 8);
    H5Tinsert (memtype_id, "version_seg",HOFFSET(head,version_seg),s8);

		/* PE_HEAD*/
    H5Tinsert (memtype_id, "dl", HOFFSET(head,dl),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "dm", HOFFSET(head,dm),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "xseg", HOFFSET(head,xseg),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "yseg", HOFFSET(head,yseg),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "hobs", HOFFSET(head,hobs),H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "x_offset", HOFFSET(head,x_offset),
		H5T_NATIVE_FLOAT);
    H5Tinsert (memtype_id, "y_offset", HOFFSET(head,y_offset),
		H5T_NATIVE_FLOAT);
		s16=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s16, 16);
    H5Tinsert (memtype_id, "petype", HOFFSET(head,petype),s16);
    H5Tinsert (memtype_id, "version_pe", HOFFSET(head,version_pe),s16);


    /*
     * Open the existeing dataset within the file
     */
    /*printf(" Define dataset\n");*/
      dataset    = H5Dopen(pes_file, "PEHEAD");


     /* read the data from the disk dataset.*/
      /*printf(" Read dataset\n");*/
      status     = H5Dread(dataset, memtype_id, H5S_ALL, 
                   H5S_ALL, H5P_DEFAULT, pehead);

    /*
     * Release resources.
     */
      /*printf(" Release Resources\n");*/
                   H5Tclose(memtype_id);
                   H5Dclose(dataset);
		   /*printf(" Returning to calling routine\n");*/

}

/*-------------------------------------------------------------------------
 * Function:    peshdf5_out
 *
 * Purpose:     Adds a pes record  into existing peshdf5 file
 *		into the PES dataset at record RECNUM.  RECNUM starts at 0.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		19/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
/*The name of the dataset is PES */


void peshdf5_out(int *recnum, pe_rec *pe)
{

    hid_t    filespace,dataspace,dataset;
    hid_t    memtype_id,cparms;

    hsize_t  dims=1;
    hsize_t  vrbl;
    hsize_t  size=1;
    hssize_t  offset=0;

    herr_t   status;


	/* Create the pe compound datatype.*/

    	memtype_id = H5Tcreate (H5T_COMPOUND, sizeof(pe_rec));

    	H5Tinsert (memtype_id, "nx", HOFFSET(pe_rec,nx),H5T_NATIVE_SHORT);
        H5Tinsert (memtype_id, "ny", HOFFSET(pe_rec,ny),H5T_NATIVE_SHORT);
        H5Tinsert (memtype_id, "time", HOFFSET(pe_rec,time),H5T_NATIVE_FLOAT);
    	H5Tinsert (memtype_id, "dlr", HOFFSET(pe_rec,dlr),H5T_NATIVE_FLOAT);
    	H5Tinsert (memtype_id, "dmr", HOFFSET(pe_rec,dmr),H5T_NATIVE_FLOAT);
    	H5Tinsert (memtype_id, "nnext", HOFFSET(pe_rec,nnext),H5T_NATIVE_INT);
    	H5Tinsert (memtype_id, "xm", HOFFSET(pe_rec,xm),H5T_NATIVE_FLOAT);
	H5Tinsert (memtype_id, "ym", HOFFSET(pe_rec,ym),H5T_NATIVE_FLOAT);
	H5Tinsert (memtype_id, "spec", HOFFSET(pe_rec,spec),H5T_NATIVE_SHORT);
   	H5Tinsert (memtype_id, "lambda", HOFFSET(pe_rec,lambda),
			H5T_NATIVE_SHORT);
	H5Tinsert (memtype_id, "em_alt", HOFFSET(pe_rec,em_alt),
			H5T_NATIVE_FLOAT);


    /* see if the PES  dataset exists yet */

    H5E_BEGIN_TRY {
    dataset      = H5Dopen(pes_file, "PES");
    } H5E_END_TRY;
    if(dataset < 0)                          /* data set is not found */
      {                                      /* create data set */

 	/* create pe dataspace ,1 element of a single compound 
	 * datatype, unlimited dimensions.
	 */
	 vrbl=H5S_UNLIMITED;
	 dataspace  = H5Screate_simple(1, &dims, &vrbl); 

    /* 
     * Modify dataset creation properties, i.e. enable chunking and 
     * use GZIP compression with the compression effort set to "comression". 
     * Note that compression can be used only when dataset is chunked. 
     */
      vrbl=(hsize_t)CHUNK_SIZE_PES;
      cparms     = H5Pcreate (H5P_DATASET_CREATE);
      status     = H5Pset_chunk(cparms, 1, &vrbl);
                   H5Pset_deflate(cparms, COMPRESSION_PES);
    /*
     * Create a new dataset within the file using cparam
     * creation properties.
     */
      dataset    = H5Dcreate(pes_file, "PES", memtype_id, 
                            dataspace, cparms);
    /*
     * Extend the dataset.
     */
      size=*recnum+1;   /* Make big enough to have a place for this pes. */ 
      status     = H5Dextend (dataset, &size);

      offset=*recnum;
    /*
     * Select a hyperslab.
     */
      filespace  = H5Dget_space (dataset); 
      status     = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, 
                          &offset, NULL, &dims, NULL);  
    /*
     * Write the data to the hyperslab.
     */
      status     = H5Dwrite(dataset, memtype_id, dataspace, 
                   filespace, H5P_DEFAULT, pe);
    /*
     * Release resources.
     */
                   H5Pclose(cparms);
                   H5Sclose(filespace); 
                   H5Sclose(dataspace);
         	   H5Tclose(memtype_id); 	/*release memtype_id*/ 
		   H5Dclose(dataset); 
     }
    else   /* !!!!!!!!!Dataset exists!!!!!!!!!!!!!!!
	    *         Add pe to dataset 
	    */
     {

    /*
     * Get dataset dimension. This is number of pes currently in the file.
     */
      filespace  = H5Dget_space(dataset);
      status     = H5Sget_simple_extent_dims(filespace, &size, NULL);
                   H5Sclose(filespace);
    /*
     * Extend the dataset if needed.
     */
      if(*recnum>(int)size-1)
        {
	  size=*recnum+1;
	  status     = H5Dextend (dataset, &size);
	}
	  
    /*
     * Select a hyperslab. This is place where to put the pes.
     */
      offset=*recnum;
      filespace  = H5Dget_space(dataset); /* Do again since we may have 
					   * extened it. 
					   */
      status     = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset,
                   NULL, &dims, NULL);

    /*
     * Define memory space
     */
      dataspace  = H5Screate_simple(1, &dims, NULL); 

    /*
     * Write the data to the hyperslab.
     */
      status     = H5Dwrite(dataset, memtype_id, dataspace, 
                        filespace, H5P_DEFAULT, pe);

    /*
     * Release resources.
     */
                H5Sclose(filespace);
                H5Sclose(dataspace);
		H5Tclose(memtype_id); 	/*release memtype_id*/             
		H5Dclose(dataset); 
     }
}

/*-------------------------------------------------------------------------
 * Function:    peshdf5_in
 *
 * Purpose:     Reads specified pes record (RECNUM)  from existing 
 *              peshdf5 file
 *		into the PE record.
 *              RECNUM starts at 0.
 *
 * Return:      Void.   ierr=-1 Reqested recnum is larger then # pES in file.
 *                      ierr=-2 No PES dataset => No pes in shower.
 *                      ierr=0  All ok
 *
 * Programmer:  GHS
 *		20/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
/*The name of the dataset is PES */


void peshdf5_in (int *recnum, pe_rec *pe, int *ierr)
{

    hid_t    filespace,dataspace;
    hid_t    memtype_id,dataset;

    hsize_t  dims=1;
    hsize_t  size=0;
    hssize_t  offset=0,ssize;

    herr_t   status;

    *ierr=0;           /* Flag that all is OK */
    /*
     * Open the existeing dataset within the file
     */

    /* see if the PES  dataset exists yet */

    H5E_BEGIN_TRY {
       dataset    = H5Dopen(pes_file, "PES");
    } H5E_END_TRY;
    if(dataset < 0)                          /* data set is not found */
      {                                      /* No pes in this shower */
	*ierr=-2;
	return;
      }
    /*
     * Get Datset size
     */
      filespace=H5Dget_space(dataset);
      status = H5Sget_simple_extent_dims(filespace, &size, NULL);
     /*
      * Check that we are not asking for more events then we have
      */
      offset=*recnum;
      ssize=size;
      if(offset>ssize-1)
	  {
	    H5Sclose(filespace);
	    H5Dclose(dataset);
	    *ierr=-1
;               /* recnum is out of bounds of the single
				     * diminsioned array of the PES dataset.
				     * The bounds are 0 to size-1.
				     */
	    return;
	  }
      else
	{
	  *ierr=0;
	}
      /* Now create  the pe compound datatype.*/
    	memtype_id = H5Tcreate (H5T_COMPOUND, sizeof(pe_rec));
			/*nx,ny are not part of record. They are inherent in
			 * the dataset name. So skip em here */

    	H5Tinsert (memtype_id, "nx", HOFFSET(pe_rec,nx),H5T_NATIVE_SHORT);
        H5Tinsert (memtype_id, "ny", HOFFSET(pe_rec,ny),H5T_NATIVE_SHORT);
        H5Tinsert (memtype_id, "time", HOFFSET(pe_rec,time),H5T_NATIVE_FLOAT);
    	H5Tinsert (memtype_id, "dlr", HOFFSET(pe_rec,dlr),H5T_NATIVE_FLOAT);
    	H5Tinsert (memtype_id, "dmr", HOFFSET(pe_rec,dmr),H5T_NATIVE_FLOAT);
    	H5Tinsert (memtype_id, "nnext", HOFFSET(pe_rec,nnext),H5T_NATIVE_INT);
    	H5Tinsert (memtype_id, "xm", HOFFSET(pe_rec,xm),H5T_NATIVE_FLOAT);
	H5Tinsert (memtype_id, "ym", HOFFSET(pe_rec,ym),H5T_NATIVE_FLOAT);
	H5Tinsert (memtype_id, "spec", HOFFSET(pe_rec,spec),H5T_NATIVE_SHORT);
   	H5Tinsert (memtype_id, "lambda", HOFFSET(pe_rec,lambda),
			H5T_NATIVE_SHORT);
	H5Tinsert (memtype_id, "em_alt", HOFFSET(pe_rec,em_alt),
			H5T_NATIVE_FLOAT);

    /*
     * Define memory space
     */
      dataspace  = H5Screate_simple(1, &dims, NULL); 

    /*
     * Select a hyperslab.
     */
      status     = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset,
                   NULL, &dims, NULL);

     /* read the data from the disk dataset.*/
      status     = H5Dread(dataset, memtype_id, dataspace, 
                   filespace, H5P_DEFAULT, pe);

    /*
     * Release resources.
     */
                   H5Tclose(memtype_id);
                   H5Sclose(filespace);
                   H5Sclose(dataspace);
                   H5Dclose(dataset);

}

/*-------------------------------------------------------------------------
 * Function:    peshdf5_close
 *
 * Purpose:     Then it closes existing HDF5 pes file. 
 *
 * Return:      Void
 *
 * Programmer:  ghs
 *              20/9/99
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void peshdf5_close(void)
{ 
               H5Fclose(pes_file);
}

/*-------------------------------------------------------------------------
 * Function:    wcamera_psthdf5_get
 *
 * Purpose:     Open existing pst_mult.hdf file.
 *              Read in the specified pst array dataset.
 *              Close up and cleanup.
 *              Dataset names are: PST_MULT2, PST_MULT3, PST_MULT4
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		19/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
 void wcamera_psthdf5_get(char *dataset_name, short *pst_patterns , int *ierr)
{

    hid_t    dataset,pst_file;      /* handles */
    herr_t   status;

    /* Open existing file */

   pst_file = H5Fopen("pst_mult.hdf", H5F_ACC_RDONLY, H5P_DEFAULT);

    if(pst_file<0)  /* Does file exist? */
      {
	*ierr=-1;
	return;  /* no! */
      }
    else
      {
        *ierr=0;
      }

   /* Open the dataset */

   dataset = H5Dopen(pst_file,dataset_name);

   /* Read the data     */
   
   status = H5Dread(dataset, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, 
                    H5P_DEFAULT, pst_patterns);
    /*
     * Release resources.
     */
                   H5Dclose(dataset);
                   H5Fclose(pst_file);
}



/*-------------------------------------------------------------------------
 * Function:    mhdf5_make_types
 *
q * Purpose:     Make common types for m2hdf5. 
 *
 * Return:      Void
 *
 * Programmer:  ghs
 *              20/9/99
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void mhdf5_make_types(void)
{ 
    hid_t    s4,s8,s16,s60;
    herr_t   status;

/* all types defined are static and global. see top of this file */



              /* mhdf5 mhead type identifier  */
	/* Create the compound data type that describes the m_header
	 * record. Once the memory version is made we will make the disk
         * based version which is packed. Remember all HDF5 transferes are 
	 *from
         * a source to a destination(both defined).
	 * We have a struct for m_head called mhead.
	 */

    /* printf(" Define mhead_type\n"); */

       mhead_type = H5Tcreate (H5T_COMPOUND, sizeof(m_head)); 
       /* printf(" mhead_type:%d\n",mhead_type);*/ 

       /* seg_M_HEAD*/
    status=H5Tinsert (mhead_type, "itype", HOFFSET(m_head,itype),
           H5T_NATIVE_INT);
     
    /*printf(" H5Tinsert status:%d\n",status); */
    status=H5Tinsert (mhead_type, "tep", HOFFSET(m_head,tep),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "dli", HOFFSET(m_head,dli),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "dmi", HOFFSET(m_head,dmi),
		      H5T_NATIVE_FLOAT);
    		/* Character arrays are funny
		 * Make up a specific char type*/
		s4=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s4, 4);
    status=H5Tinsert (mhead_type, "magnet_field",HOFFSET(m_head,magnet_field),
		      s4);

    status=H5Tinsert (mhead_type, "etr", HOFFSET(m_head,etr),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "depth", HOFFSET(m_head,depth),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "zobs", HOFFSET(m_head,zobs),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "xinitial", HOFFSET(m_head,xinitial),
		H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "yinitial", HOFFSET(m_head,yinitial),
		H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "idfile", HOFFSET(m_head,idfile),
		      H5T_NATIVE_INT);
		s8=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s8, 8);
    status=H5Tinsert (mhead_type, "version_seg",
		      HOFFSET(m_head,version_seg),s8);

		/* PE_M_HEAD*/
    status=H5Tinsert (mhead_type, "dl", HOFFSET(m_head,dl),H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "dm", HOFFSET(m_head,dm),H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "xseg", HOFFSET(m_head,xseg),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "yseg", HOFFSET(m_head,yseg),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "hobs", HOFFSET(m_head,hobs),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "x_offset", HOFFSET(m_head,x_offset),
		H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "y_offset", HOFFSET(m_head,y_offset),
		H5T_NATIVE_FLOAT);
		s16=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s16, 16);
    status=H5Tinsert (mhead_type, "petype", HOFFSET(m_head,petype),s16);
    status=H5Tinsert (mhead_type, "version_pe", 
		      HOFFSET(m_head,version_pe),s16);

                 /* M_HEAD */
    status=H5Tinsert (mhead_type, "noise", HOFFSET(m_head,noise),
		      H5T_NATIVE_INT);
    status=H5Tinsert (mhead_type, "disc_width", HOFFSET(m_head,disc_width),
	       H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "rate_noise", HOFFSET(m_head,rate_noise),
	       H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "reflectivity", 
		      HOFFSET(m_head,reflectivity),H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "full_apeture", 
		      HOFFSET(m_head,full_aperture), H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "pesum", HOFFSET(m_head,pesum),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "step_size", HOFFSET(m_head,step_size),
	       H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "itheta_max", HOFFSET(m_head,itheta_max),
               H5T_NATIVE_INT);
    status=H5Tinsert (mhead_type, "phi_steps", HOFFSET(m_head,phi_steps),
               H5T_NATIVE_INT);
    status=H5Tinsert (mhead_type, "version_m", HOFFSET(m_head,version_m),s16);
    status=H5Tinsert (mhead_type, "adc_gate", HOFFSET(m_head,adc_gate),
	       H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "hmult", HOFFSET(m_head,hmult),
		      H5T_NATIVE_FLOAT);
    status=H5Tinsert (mhead_type, "concentration", 
		      HOFFSET(m_head,concentration),H5T_NATIVE_FLOAT);
		s60=  H5Tcopy(H5T_C_S1);
		H5Tset_size(s60, 60);
    status=H5Tinsert (mhead_type, "options", HOFFSET(m_head,options),s60);



/*
  * At this point we are going to define the mrec datatype
  * The we will create the compound data type that defines
  * the elements of the wdata part of mdata.  
  * Then we wll define another datatype that includes mrec and wdata as a
  * single element. i.e. for mdata
  */

	/* Create the mrec compound datatype.*/
    	mrec_type = H5Tcreate (H5T_COMPOUND, sizeof(m_rec));

    	status=H5Tinsert (mrec_type, "nx", HOFFSET(m_rec,nx),
			  H5T_NATIVE_SHORT);
	/*printf(" H5Tinsert status:%d\n",status);*/ 
        status=H5Tinsert (mrec_type, "ny", HOFFSET(m_rec,ny),
			  H5T_NATIVE_SHORT);
	/*printf(" H5Tinsert status:%d\n",status);*/ 
        status=H5Tinsert (mrec_type, "itheta", HOFFSET(m_rec,itheta),
                   H5T_NATIVE_SHORT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 
        status=H5Tinsert (mrec_type, "iphi", HOFFSET(m_rec,iphi),
			  H5T_NATIVE_SHORT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 
        status=H5Tinsert (mrec_type, "emission_alt", 
			  HOFFSET(m_rec,emission_alt),H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status); */
    	status=H5Tinsert (mrec_type, "em_alt_sig", HOFFSET(m_rec,em_alt_sig),
                   H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status); */
    	status=H5Tinsert (mrec_type, "muon_ratio", HOFFSET(m_rec,muon_ratio),
                   H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 
    	status=H5Tinsert (mrec_type, "median_time", 
			  HOFFSET(m_rec,median_time),H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 
    	status=H5Tinsert (mrec_type, "fp_median_time", 
			  HOFFSET(m_rec,fp_median_time),H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 
	status=H5Tinsert (mrec_type, "time_cfd", HOFFSET(m_rec,time_cfd),
                   H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 
	status=H5Tinsert (mrec_type, "fp_time_cfd", 
			  HOFFSET(m_rec,fp_time_cfd),H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 
	//   	status=H5Tinsert (mrec_type, "i_threshold", 
	//		  HOFFSET(m_rec,i_threshold),H5T_NATIVE_FLOAT);
   	status=H5Tinsert (mrec_type, "ithphi", 
			  HOFFSET(m_rec,ithphi),H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 
	status=H5Tinsert (mrec_type, "fp_threshold", 
			  HOFFSET(m_rec,fp_threshold),H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 
	status=H5Tinsert (mrec_type, "i_mult", HOFFSET(m_rec,i_mult),
			H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status); */
	status=H5Tinsert (mrec_type, "fp_mult", HOFFSET(m_rec,fp_mult),
			H5T_NATIVE_FLOAT);
	/* printf(" H5Tinsert status:%d\n",status);*/ 


/* Create a VL datatype for times element */
	       times_type = H5Tvlen_create(H5T_NATIVE_FLOAT);
/* Create a VL datatype for pix element */
	       pix_type = H5Tvlen_create(times_type);
/* Create a VL datatype for ipix element */
	       ipix_type = H5Tvlen_create(H5T_NATIVE_INT);

/* Define m_data type */
/* Create the base compund data type for mdata */
     mdata_type = H5Tcreate(H5T_COMPOUND,sizeof(m_data));

     status=H5Tinsert (mdata_type, "tep", HOFFSET(m_data,tep),
		       H5T_NATIVE_FLOAT);
     /* printf(" H5Tinsert status:%d\n",status);*/ 
     status=H5Tinsert (mdata_type, "idfile", HOFFSET(m_data,idfile),
		       H5T_NATIVE_INT);
     /* printf(" H5Tinsert status:%d\n",status);*/ 
     status=H5Tinsert (mdata_type, "x_offset", HOFFSET(m_data,x_offset),
		       H5T_NATIVE_FLOAT);
     /* printf(" H5Tinsert status:%d\n",status);*/ 
     status=H5Tinsert (mdata_type, "y_offset", HOFFSET(m_data,y_offset),
		       H5T_NATIVE_FLOAT);
     /* printf(" H5Tinsert status:%d\n",status);*/ 
     status=H5Tinsert (mdata_type, "mrec", HOFFSET(m_data,mrec),mrec_type);
     /* printf(" H5Tinsert status19:%d\n",status);*/ 
     status=H5Tinsert (mdata_type, "ipix", HOFFSET(m_data,ipix),ipix_type);
     /* printf(" H5Tinsert status20:%d\n",status);*/ 
     status=H5Tinsert (mdata_type, "pix", HOFFSET(m_data,pix),pix_type);
     /* printf(" H5Tinsert status20:%d\n",status);*/ 
     

}


/*-------------------------------------------------------------------------
 * Function:    mhdf5_create
 *
 * Purpose:     Creates m HDF5 file.
 *              This program also isolates the HDF5 file
 *              identifier from the user by making it static 
 *              variable and limiting its scope to only this file. 
 *
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *              19/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void mhdf5_create(char *mfilename, hid_t *m_file)
{ 
    /*
     * Create a file.
     */
    
  *m_file = H5Fcreate(mfilename, H5F_ACC_TRUNC, 
                         H5P_DEFAULT, H5P_DEFAULT);
}
/*-------------------------------------------------------------------------
 * Function:    mhdf5_open
 *
 * Purpose:     Opens an existing mrec HDF5 file.
 *              This program also isolates the HDF5 file
 *              identifier from the user by making it static 
 *              variable and limiting its scope to only this file. 
 *
 *
 * Return:      VOID
 *
 * Programmer:  GHS
 *              20/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void mhdf5_open(char *mfilename, hid_t *m_file, int *ierr)
{ 
  /*
   * open a file.
   */
  *m_file= H5Fopen(mfilename, H5F_ACC_RDWR,H5P_DEFAULT);

  if(*m_file<0){     /* Does file exist? */

    *ierr=-1;
    printf(" mhdf5\n");
    printf(mfilename);
    return;  /* no! */
  }
  else{
    *ierr=0;
    return;   /* yes! */
  }
}
/*-------------------------------------------------------------------------
 * Function:    mhdf5_mhead_write
 *
 * Purpose:     Writes m header record  into existing mhdf5 file
 *		as a dataset attached to the root.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		19/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void mhdf5_mhead_write(m_head *mhead, hid_t *m_file)
 {
   hid_t    dataspace, dataset;             /* handles */
   hid_t    cparms;
   hsize_t  dims=1;
   herr_t   status;

    /* Various data types define in mhdf5_make_types called in mhd5f_create */
      
    /* first create mhead dataspace ,1 element of a single compound datatype*/
   dataspace  = H5Screate_simple(1, &dims, NULL); 

   /* GEt dataset creation properties*/
   cparms     = H5Pcreate (H5P_DATASET_CREATE);
 
   /*Create a new dataset within the file using cparms creation properties.*/
   dataset    = H5Dcreate(*m_file, "MHEAD", mhead_type, dataspace, cparms);

     /* Write the data to the disk dataset.*/
   status     = H5Dwrite(dataset, mhead_type, H5S_ALL, 
                   H5S_ALL, H5P_DEFAULT, mhead);

    /*Release resources.*/

   H5Pclose(cparms);
   H5Sclose(dataspace);
   H5Dclose(dataset);
}



/*-------------------------------------------------------------------------
 * Function:    mhdf5_mhead_read
 *
 * Purpose:     Reads m header record  from existing mhdf5 file
 *		from  a dataset attached to the root.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		27/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void mhdf5_mhead_read(m_head *mhead, hid_t *m_file)
 {
    hid_t    dataset;             /* handles */
//   hid_t cparms,dataspace;
    herr_t   status;

    /* Various data types define in mhdf5_make_types call in mhd5f_open */

   /*
     * Open the existeing dataset within the file
     */
    /*printf(" Define dataset\n");*/

      dataset    = H5Dopen(*m_file, "MHEAD");


     /* read the data from the disk dataset.*/
      /* printf(" Read dataset\n");*/

      status     = H5Dread(dataset, mhead_type, H5S_ALL, 
                   H5S_ALL, H5P_DEFAULT, mhead);

    /*
     * Release resources.
     */
      /* printf(" Release Resources\n");*/
       H5Dclose(dataset);

    /*printf(" Returning to calling routine\n");*/

}
/*-------------------------------------------------------------------------
 * Function:    mhdf5_mrec_size
 *
 * Purpose:     Gets number of records (extent) of a mrec dataset
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		25/10/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
/*The name of the dataset is MREC */
void mhdf5_mrec_size(hid_t *m_file, int *isize)
{
  hid_t    filespace,dataset;
  hsize_t  size;
  herr_t   status;

  /*
   * Open the existeing dataset within the file
   */

  /* see if the MREC  dataset exists yet */
  H5E_BEGIN_TRY {
    dataset    = H5Dopen(*m_file, "MREC");
  } H5E_END_TRY;
  if(dataset < 0) {                          /* data set is not found */
    *isize=0;                              /* No pes in this shower */
    /*      H5Dclose(dataset); */
    return;
  }
  else {

    /*
     * Get Datset size
     */
    filespace=H5Dget_space(dataset);
    status = H5Sget_simple_extent_dims(filespace, &size, NULL);
    *isize=size;
 
      H5Sclose(filespace);
      H5Dclose(dataset);
     return;
    }
}
/*-------------------------------------------------------------------------
 * Function:    mhdf5_out
 *
 * Purpose:     Adds a m record  into existing m hdf5 file
 *		into the MREC dataset at record RECNUM  RECNUM starts at 0.
 *              The m record consits of two part:
 *              1: The  header (called mrec)
 *              2: The pes times arainged by pixel.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		19/9/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
/*The name of the dataset is MREC */
void mhdf5_out(int *recnum, float *tep, int *idfile, float *x_offset, 
	       float *y_offset, m_rec *mrec_head, float *disc, float *times,
	       hid_t *m_file, int *npixels)
{

  /*
   *ARGUMENTS:
   *     recnum: place in array to be rwritten out of this record.
   *     mrec:   header struff for this record.
   *     disc:   Array of pixels with number of pes that hit each pixel.
   *     time:   pes hit times for each pe. Times are in order of pes in disc.
   */

  int nhit_pixels,i,itimes,j,idisc;
  static    hid_t    filespace,dataspace,dataset,memspace;
  static    hid_t    cparms;
  hsize_t  dims=1;
  hsize_t  vrbl;
  hsize_t  size=1;
  hssize_t  offset=0;
  herr_t   status;
    
  m_data    mdata;       /*m_data is structure of output  mrec record*/

    /* mdata.pix  will be our VL pixel array that holds the pe times in vl
     * variables.
     */
    /*Count number of non-zero pixels */
  nhit_pixels=0;
  for (i=0; i<*npixels; i++) {
    idisc=(int) disc[i];
    if(idisc>0) {
      nhit_pixels++;
    }
  }
  mdata.pix.p=(void *)malloc(nhit_pixels*sizeof(hvl_t));
  mdata.ipix.p=(void *)malloc(nhit_pixels*sizeof(int));
		
  /*disc has number of pes in each pixels.times has pes */ 
  /*times inorder of disc */
  /*Count number of non-zero pixels */
  nhit_pixels=0;
  itimes=0;
  for (i=0; i<*npixels; i++) {
    idisc=(int) disc[i];
    if(idisc>0)
      {
	/* pixels are numbered starting at 1 */
	((int *)mdata.ipix.p)[nhit_pixels]=i+1;
	((hvl_t *)mdata.pix.p)[nhit_pixels].len=idisc;
	((hvl_t *)mdata.pix.p)[nhit_pixels].p=
	  (void *)malloc(idisc*sizeof(float));
	for( j=0; j<idisc; j++){
	  ((float *)((hvl_t *)mdata.pix.p)[nhit_pixels].p)[j]=
	    times[itimes];
	  itimes++;
	}
	nhit_pixels++;
      }
  }                                                      

  /* define pix and ipix lengths */
  mdata.pix.len=nhit_pixels;
  mdata.ipix.len=nhit_pixels;
    
  /* Set energy and file id */
  mdata.tep=*tep;
  mdata.idfile=*idfile;
  mdata.x_offset=*x_offset;
  mdata.y_offset=*y_offset;
     
  /* Fill in the mrec head part */
  memcpy(&mdata.mrec,mrec_head,sizeof(m_rec));


 /* see if the MREC  dataset exists yet */
  H5E_BEGIN_TRY {
    dataset      = H5Dopen(*m_file, "MREC");
  } H5E_END_TRY;
  if(dataset < 0) {
   
    /* data set is not found create data set */
    /* Create the dataspace */
    vrbl=H5S_UNLIMITED;
    dataspace  = H5Screate_simple(1, &dims, &vrbl); 
     
    /* Modify dataset creation properties, i.e. enable chunking and 
     * use GZIP compression with the compression effort set to "comression". 
     * Note that compression can be used only when dataset is chunked.
     */
    
    cparms     = H5Pcreate (H5P_DATASET_CREATE);
    vrbl=(hsize_t)CHUNK_SIZE_MREC;
    status     = H5Pset_chunk(cparms, 1, &vrbl);
    status     = H5Pset_deflate(cparms, COMPRESSION_PES);
    
    /*Create a new dataset within the file using cparam creation propertie */
    dataset    = H5Dcreate(*m_file, "MREC", mdata_type, dataspace, cparms);
      
    /* Extend the dataset. We may start writien at some offset .ne.0 */
    /* Make big enough to have a place for this mrecord.*/ 
    size=*recnum+1;
    status     = H5Dextend (dataset, &size);

    H5Pclose(cparms);
    H5Sclose(dataspace);

  }else {
    /* !!!!!!!!!Dataset exists!!!!!!!!!!!!!!Add mdata to data*/
    
    /* Get dataset dimension. This is number of mdatas currently in the file.*/
    filespace  = H5Dget_space(dataset);
    status     = H5Sget_simple_extent_dims(filespace, &size, NULL);
    H5Sclose(filespace);
    
    /*Extend the dataset if needed.*/
    if(*recnum>(int)size-1) {
      size=*recnum+1;
      status     = H5Dextend (dataset, &size);
    }
  }

  /*Select a hyperslab. This is place where to put the pes.*/
  offset=*recnum;
  filespace  = H5Dget_space(dataset);
  status     = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset,
				     NULL, &dims, NULL);
  /*Define memory space*/
  memspace  = H5Screate_simple(1, &dims, NULL); 
    
  /*Write the data to the hyperslab.*/
  status     = H5Dwrite(dataset, mdata_type, memspace, 
			    filespace, H5P_DEFAULT, &mdata);
    
  H5Sclose(filespace);
  H5Sclose(memspace);
  H5Dclose(dataset);

  /* free up the allocated memory */
  for (i=0; i<nhit_pixels; i++) {
    free( ((hvl_t *)mdata.pix.p)[i].p );
  }
  free(mdata.pix.p);
  free(mdata.ipix.p);
}

/*-------------------------------------------------------------------------
 * Function:    mhdf5_in
 *
 * Purpose:     Read a m record from anexisting m hdf5 file
 *		into the MREC dataset from  record RECNUM.  
 *              RECNUM starts at 0.
 *              The m record consits of two part:
 *              1: The  header (called mrec)
 *              2: The pes times arainged by pixel.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		19/9/00
 *
 * Modifications:
 *  
 * 30/07/03 GHS:Put in a check to make sure reqested event (recnum) is valid.
 *
 *-------------------------------------------------------------------------
 */
/*The name of the dataset is MREC */
void mhdf5_in(int *recnum, float *tep, int *idfile, float *x_offset, 
	      float *y_offset, m_rec *mrec_head, float *disc, float *times,
	      hid_t *m_file, int *ierr, int *npixels)
{

  /*
   *ARGUMENTS:
   *     recnum: place in dataset record is to be read from
   *     mrec:   header struff for this record.
   *     disc:   Array of pixels with number of pes that hit each pixel.
   *     time:   pes hit times for each pe. Times are in order of pes in disc.
   */
  

  hid_t    filespace,dataset,memspace;
  hsize_t  dims=1;
//  hsize_t  vrbl;
  hsize_t  size,ssize;
  hssize_t offset=0;
  herr_t   status;
  
  int      nhit_pixels,i,itimes,j,ipixel,idisc;
  m_data   mdata;       /*m_data is structure of input  mrec record*/
  
  /*recnum must be positive or 0, its a 'C' array index*/
  if(*recnum<0)
    {
      *ierr=-3;
      return;
    }

/*Open the existeing dataset within the file */
  H5E_BEGIN_TRY {
    dataset    = H5Dopen(*m_file, "MREC");
  } H5E_END_TRY;
  if(dataset < 0) {           /* data set is not found.No pes in this shower */
    *ierr=-2;
    return;
  }
  /*Get Datset size*/
  filespace=H5Dget_space(dataset);
  status = H5Sget_simple_extent_dims(filespace, &size, NULL);
  
  /*Check that we are not asking for more events then we have*/
  offset=*recnum;
  ssize=size;
  if(offset>(int)ssize-1) {
    H5Sclose(filespace);
    H5Dclose(dataset);
    *ierr=-1;
    /* recnum is out of bounds of the single
     * diminsioned array of the PES dataset.
     * The bounds are 0 to size-1.
     */
    return;
  }else{
    *ierr=0;
  }
  
  /*Define memory space*/
  memspace  = H5Screate_simple(1, &dims, NULL); 
  
  /* Select a hyperslab.*/
  filespace=H5Dget_space(dataset);
  status     = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset,
				   NULL, &dims, NULL);
  /* read the data from the disk dataset.*/
  status     = H5Dread(dataset, mdata_type, memspace, filespace, H5P_DEFAULT,
		       &mdata);

  /*Check to see if the read worked*/
  if(status<0)
    {
      *ierr=-4;
      return;
    }

/* Unpack the data into the original arrays */
  *tep=mdata.tep;
  *idfile=mdata.idfile;
  *x_offset=mdata.x_offset;
  *y_offset=mdata.y_offset;

  /* Get the mrec part */
  memcpy(mrec_head, &mdata.mrec, sizeof(m_rec));
  
  /* Init Disc Array to 0 */
  for (i=0; i<*npixels; i++) {
    disc[i]=0;
    /*Do it this way 'cause C don't seem to have a better way */
  }            
  
  /* Number of hit pixels in this record */
  nhit_pixels=mdata.ipix.len;
  itimes=0;
  for (i=0; i<nhit_pixels; i++) {
    ipixel=((int *)mdata.ipix.p)[i]-1;
    idisc=((hvl_t *)mdata.pix.p)[i].len;
    disc[ipixel]=idisc;
   
    for (j=0; j<idisc; j++) {
      times[itimes]=((float *)((hvl_t *)mdata.pix.p)[i].p)[j];
      itimes++;
    }
  }	    
 
  /* Free up the memory allocated by HDF5 for the variable length arrays */
  status=H5Dvlen_reclaim(mdata_type,memspace,H5P_DEFAULT,&mdata);
  
  /*Release resources. */
  H5Sclose(filespace);
  H5Sclose(memspace);
  H5Dclose(dataset);
  
  return;
}
/*-------------------------------------------------------------------------
 * Function:    mhdf5_close
 *
 * Purpose:     Then it closes existing HDF5 MREC file. 
 *
 * Return:      Void
 *
 * Programmer:  ghs
 *              20/9/99
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void mhdf5_close(hid_t *m_file)
{ 
    H5Fclose(*m_file);
}
/*-------------------------------------------------------------------------
 * Function:    mhdf5_close_types
 *
 * Purpose:     Closes types made by mhdf5_make_types. 
 *
 * Return:      Void
 *
 * Programmer:  ghs
 *              20/9/99
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void mhdf5_close_types(void)
{ 
    H5Tclose(mhead_type);
    H5Tclose(mdata_type);
    H5Tclose(times_type);
    H5Tclose(pix_type);
    H5Tclose(ipix_type);
    H5Tclose(mrec_type); 
}
/*-------------------------------------------------------------------------
 * Function:    mhdf5_data_print
 *
 * Purpose:     Print out a fake event for use by mhdf5 routines.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		21/11/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */ 
void mhdf5_data_print(m_rec *mrec_head,float *disc,float *time,
		      int *nhit_pixels)
{
  int    i,j,k;
  printf(" out:nx::%d\n",(*mrec_head).nx); 
  printf(" out:ny::%d\n",(*mrec_head).ny);
  printf(" out:emission_alt::%f\n",(*mrec_head).emission_alt); 
  k=0;
  for (i=0; i< *nhit_pixels; i++) {
    printf(" out:i:,disc[i]::%d,%f\n",i,disc[i]);
    for( j=0; j<disc[i]; j++) {
      printf(" out:i,j,pix::%d,%d,%f\n",i,j,time[k]);
      k++;
    }
  }
}
/*-------------------------------------------------------------------------
 * Function:    mhdf5_data_make
 *
 * Purpose:     Create a fake event for use by mhdf5 routines.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		21/11/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */ 
void mhdf5_data_make(float *tep,int *idfile,float *x_offset,float *y_offset,
		    m_rec *mrec_head,float *disc,float *time,int *size,
		    int *nhit_pixels)
{
  int   k,i,j;
  float   a;

 /* Set up fake data */
  *tep=1.234;
  *idfile=321;
  *x_offset=-4.5;
  *y_offset=3.5;
  
  (*mrec_head).nx=*size;
  (*mrec_head).ny=*size/2;
  (*mrec_head).emission_alt=*size*1000.+3.1415*100.;
  for (i=0; i<490; i++) {
    disc[i]=0;
  }   

  k=0;
  for (i=0; i<*nhit_pixels; i++) {
    disc[i]=5;
    /*
     *disc[i]=i+1;
     *    for( j=0; j<i+1; j++) {
     */
    for( j=0; j<5; j++) {
      a=i;
      time[k]=(a*j*5+1)/(a+j+1);
      k++;
    }
  }
}


/*-------------------------------------------------------------------------
 * Function:    hdf_data_make
 *
 * Purpose:     Create a fake event for use by hdf_test routines.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		21/11/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */ 
void hdf5_data_make(m_data *mdata_same, int *size, int *nhit_pixels)
{
  int   i,j;
  float   a;
 /* Set up fake data */
  (*mdata_same).tep=1.234;
  (*mdata_same).idfile=321;
  (*mdata_same).x_offset=-4.5;
  (*mdata_same).y_offset=3.5;
  
  (*mdata_same).mrec.nx=*size;
  (*mdata_same).mrec.ny=*size/2;
  (*mdata_same).mrec.emission_alt=*size*1000.+3.1415*100.;

  (*mdata_same).pix.len=*nhit_pixels;
  (*mdata_same).pix.p=(void *)malloc(*nhit_pixels*sizeof(hvl_t));
  (*mdata_same).ipix.len=*nhit_pixels;
  (*mdata_same).ipix.p=(void *)malloc(*nhit_pixels*sizeof(int));
   

  for (i=0; i<*nhit_pixels; i++) {
    ((int *)(*mdata_same).ipix.p)[i]=i+1;
    ((hvl_t *)(*mdata_same).pix.p)[i].len=i+1;
    ((hvl_t *)(*mdata_same).pix.p)[i].p=
      (void *)malloc((i+1)*sizeof(float));
	   
    for( j=0; j<i+1; j++) {
      a=i;
      ((float *)((hvl_t *)(*mdata_same).pix.p)[i].p)[j]=(a*j*5+1)/(a+j+1);
    }
  }
}


/*-------------------------------------------------------------------------
 * Function:    hdf5_data_print
 *
 * Purpose:     Print out a fake event for use by mhdf5 routines.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		21/11/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */ 
void hdf5_data_print(m_data *mdata_same, int *nhit_pixels)
{
  int    i,j,k;
  printf(" out:nx::%d\n",(*mdata_same).mrec.nx); 
  printf(" out:ny::%d\n",(*mdata_same).mrec.ny);
  printf(" out:emission_alt::%f\n",(*mdata_same).mrec.emission_alt); 
  k=0;
  for (i=0; i< *nhit_pixels; i++) {
    printf(" out:i:,disc[i].len::%d,%d\n",
		 ((int *)(*mdata_same).ipix.p)[i],
		 ((hvl_t *)(*mdata_same).pix.p)[i].len);
    for( j=0; j<(int)((hvl_t *)(*mdata_same).pix.p)[i].len; j++) {
      printf(" out:i,j,pix::%d,%d,%f\n",i,j,
		   ((float *)((hvl_t *)(*mdata_same).pix.p)[i].p)[j]);
    }
  }
}

/*-------------------------------------------------------------------------
 * Function:    hdf5_out
 *
 * Purpose:     Print out a fake event for use by mhdf5 routines.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		21/11/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void hdf5_out(int *ssize, m_data *mdata_same, hid_t *m_file,int *nhit_pixels)
{
  hsize_t  dims=1;
  hsize_t  vrbl;
  hid_t    filespace,dataspace,dataset,memspace;
  hid_t    cparms;
  herr_t   status;
  hsize_t  size;
  hssize_t offset=0;
  int    i;
  

  size=*ssize;
 
  /* Create mdata dataset ,1 element of a single compound 
   * datatype, unlimited dimensions.
   */
  vrbl=H5S_UNLIMITED;
  dataspace  = H5Screate_simple(1, &dims, &vrbl); 
  printf(" H5Dcreate_simple dataspace::%d\n",dataspace); 
   
  memspace  = H5Screate_simple(1, &dims, NULL);

  vrbl=(hsize_t)CHUNK_SIZE_MREC;    
  cparms     = H5Pcreate (H5P_DATASET_CREATE);
  printf(" H5Pcreate cparms::%d\n",cparms);
  status     = H5Pset_chunk(cparms, 1, &vrbl);
  printf(" H5Pset_chunk status::%d\n",status);
  status     = H5Pset_deflate(cparms, COMPRESSION_PES);
  printf(" H5Pset_deflate status::%d\n",status);
   
  dataset    = H5Dcreate(*m_file, "MREC", mdata_type, dataspace, cparms);
  printf(" MREC dataset:%d\n",dataset);
  
   /* Extend the dataset.*/
  status     = H5Dextend (dataset,&size);
  printf(" H5Dextend status::%d\n",status);
  offset=size-1;
   
   /* Select a hyperslab. */
  filespace  = H5Dget_space (dataset); 
  printf(" H5Dget_space filespace::%d\n",filespace);
  status     = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, 
				   &offset, NULL, &dims, NULL);  
  printf(" H5Sselect_hyperslab status::%d\n",status);

  /* Write the data to the hyperslab.*/
  status     = H5Dwrite(dataset, mdata_type, memspace, filespace, 
			H5P_DEFAULT, mdata_same);
  printf(" H5Dwrite status::%d\n",status);

  /* free up the times allocated memory */
  for (i=0; i<*nhit_pixels; i++) {
    free(((hvl_t *)(*mdata_same).pix.p)[i].p);
    
  }
  free((*mdata_same).pix.p);
  free((*mdata_same).ipix.p);
}


/*-------------------------------------------------------------------------
 * Function:    mhdf5_build_mdata
 *
 * Purpose:     Convert input data into mdata structure.
 *
 * Return:      Void
 *
 * Programmer:  GHS
 *		22/11/00
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void mhdf5_create_mdata(m_data *mdata, float *tep, int *idfile, 
			float *x_offset, float *y_offset, m_rec *mrec_head,
			float *disc, float *times, int *npixels) 
{
  int nhit_pixels,i,j,idisc,itimes;


    /*Count number of non-zero pixels */
  nhit_pixels=0;
  for (i=0; i<*npixels; i++) 
    {
      idisc=(int) disc[i];
      if(idisc>0) {
	nhit_pixels++;
      }
    }

  (*mdata).pix.p=(void *)malloc(nhit_pixels*sizeof(hvl_t));
  (*mdata).ipix.p=(void *)malloc(nhit_pixels*sizeof(int));

		/*disc has number of pes in each pixels.
		 *   times has pes times inorder of disc */

  /*Count number of non-zero pixels */
  nhit_pixels=0;
  itimes=0;
  for (i=0; i<*npixels; i++) 
    {
      idisc=(int) disc[i];
      if(idisc>0)
	{
                              /* pixels are numbered starting at 1 */
	  ((int *)(*mdata).ipix.p)[nhit_pixels]=i+1;

	  ((hvl_t *)(*mdata).pix.p)[nhit_pixels].len=idisc;

	  ((hvl_t *)(*mdata).pix.p)[nhit_pixels].p=
		                   (void *)malloc(idisc*sizeof(float));
	  for( j=0; j<idisc; j++)
	    {
	      ((float *)((hvl_t *)(*mdata).pix.p)[nhit_pixels].p)[j]=
		                  times[itimes];
	      itimes++;
	    }
	  nhit_pixels++;
	}
    }
                                                             
  /* define pix and ipix lengths */
  (*mdata).pix.len=*npixels;
  (*mdata).ipix.len=*npixels;

  /* Set energy and file id */
  mdata->tep=*tep;
  (*mdata).idfile=*idfile;
  (*mdata).x_offset=*x_offset;
  (*mdata).y_offset=*y_offset;

  /* Fill in the mrec head part */
  memcpy(&((*mdata).mrec), mrec_head,sizeof(m_rec));
}
