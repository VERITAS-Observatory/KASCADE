//MHDF5SimDataFile.cpp
// ************************************************************************
//Member functions of a class to Create, Open,Read Write Simulate and//Print a MHDF5 file for KASCADE trigger output.
// The HDF5 code here follows closely the methods used in kashdf5.c
// used in KASCADE and the VHDF5RunDataFile class used in VERITAS.
// See comments in the MHDF5SimDataFile.h file.

//   G.H.Sembroski
//   Physics Dept.
//   Purdue Univ.
//   W.Lafayette, In 47907
//   sembroski@physics.purdue.edu
//   31/07/03

//Modified:
// 20/10/04 GHS
//          Change definition of mrec file for variable i_threshold which now
//          become ithphi.
// *************************************************************************

#include "MHDF5SimDataFile.h"

//DEFINE STATEMENTS 
//#define COMPRESSION_PES        6  /* compression effort           */

MHDF5SimDataFile::MHDF5SimDataFile()
// ************************************************************************
//Constructor for MHDF5 Sim data  file.
// ************************************************************************
{
               //Initalize file struture types.
  makeTypes();

  testInput=false;
  firstTime=true;

  writeDataset=-1;    //Init for writeEvent member function.
  eventBufIndex=0;
  nextWriteIndex=0;
  chunkSizeEvent=kDefaultChunkSize;
  memSpaceSize=0;

  readDataset=-1;    //Init for readEvent member function.
  return;
}

// ************************************************************************

MHDF5SimDataFile::~MHDF5SimDataFile()
// ************************************************************************
//Destructor for MHDF5 Sim data  file.
// ************************************************************************
{
               //Close file struture types.
  closeTypes();
}

// ************************************************************************

void MHDF5SimDataFile::makeTypes()
// ***********************************************************************
//Initalize file struture types for MHDF5 MREC  file. Use c level calls to 
//HDF5 functions 
// ***********************************************************************
{ 
//The types generated by this method are HDF5 types which it uses to define
//the recods in HDF5 files. Not to be confussed with C++ or c types.
//All types defined are Public. See this class definition
// **********************************************************************

  herr_t status;

  // all types defined are static and global. see top of this file 
  // MHDF55 mhead type identifier  
  // Create the compound data type that describes the m_header
  // record. Once the memory version is made we will make the disk
  // based version which is packed. Remember all HDF5 transferes are 
  //froma source to a destination(both defined).
  // We have a struct for m_head called MHDF5MHead.
	 
  MHeadType = H5Tcreate (H5T_COMPOUND, sizeof(MHDF5MHead)); 

  // ************************seg_M_HEAD****************************
  status=H5Tinsert (MHeadType, "itype", HOFFSET(MHDF5MHead,itype),
                                                             H5T_NATIVE_INT);
  status=H5Tinsert (MHeadType, "tep", HOFFSET(MHDF5MHead,tep),
		                                           H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "dli", HOFFSET(MHDF5MHead,dli),
		                                           H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "dmi", HOFFSET(MHDF5MHead,dmi),
		                                           H5T_NATIVE_FLOAT);
  // Character arrays are funny
  // Make up a specific char type
  hid_t s4=  H5Tcopy(H5T_C_S1);
  H5Tset_size(s4, 4);
  status=H5Tinsert (MHeadType, "magnet_field",HOFFSET(MHDF5MHead,magnet_field),
		                                                          s4);
  status=H5Tinsert (MHeadType, "etr", HOFFSET(MHDF5MHead,etr),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "depth", HOFFSET(MHDF5MHead,depth),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "zobs", HOFFSET(MHDF5MHead,zobs),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "xinitial", HOFFSET(MHDF5MHead,xinitial),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "yinitial", HOFFSET(MHDF5MHead,yinitial),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "idfile", HOFFSET(MHDF5MHead,idfile),
		                                              H5T_NATIVE_INT);
  hid_t s8=  H5Tcopy(H5T_C_S1);
  H5Tset_size(s8, 8);
  status=H5Tinsert (MHeadType, "version_seg", HOFFSET(MHDF5MHead,version_seg),
                                                                          s8);
  // *********************PE_M_HEAD******************************
  status=H5Tinsert (MHeadType, "dl", HOFFSET(MHDF5MHead,dl),H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "dm", HOFFSET(MHDF5MHead,dm),H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "xseg", HOFFSET(MHDF5MHead,xseg),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "yseg", HOFFSET(MHDF5MHead,yseg),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "hobs", HOFFSET(MHDF5MHead,hobs),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "x_offset", HOFFSET(MHDF5MHead,x_offset),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "y_offset", HOFFSET(MHDF5MHead,y_offset),
		                                            H5T_NATIVE_FLOAT);
  hid_t s16=  H5Tcopy(H5T_C_S1);
  H5Tset_size(s16, 16);
  status=H5Tinsert (MHeadType, "petype", HOFFSET(MHDF5MHead,petype),s16);
  
  status=H5Tinsert (MHeadType, "version_pe", HOFFSET(MHDF5MHead,version_pe),
                                                                         s16);

  // *****************************M_HEAD************************************* 
  status=H5Tinsert (MHeadType, "noise", HOFFSET(MHDF5MHead,noise),
		                                              H5T_NATIVE_INT);
  status=H5Tinsert (MHeadType, "disc_width", HOFFSET(MHDF5MHead,disc_width),
	                                                    H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "rate_noise", HOFFSET(MHDF5MHead,rate_noise),
	                                                    H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "reflectivity",
                           HOFFSET(MHDF5MHead,reflectivity),H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "full_apeture", 
		         HOFFSET(MHDF5MHead,full_aperture), H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "pesum", HOFFSET(MHDF5MHead,pesum),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "step_size", HOFFSET(MHDF5MHead,step_size),
	                                                    H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "itheta_max", HOFFSET(MHDF5MHead,itheta_max),
                                                              H5T_NATIVE_INT);
  status=H5Tinsert (MHeadType, "phi_steps", HOFFSET(MHDF5MHead,phi_steps),
                                                              H5T_NATIVE_INT);
  status=H5Tinsert (MHeadType, "version_m", HOFFSET(MHDF5MHead,version_m),s16);
  status=H5Tinsert (MHeadType, "adc_gate", HOFFSET(MHDF5MHead,adc_gate),
	                                                    H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "hmult", HOFFSET(MHDF5MHead,hmult),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MHeadType, "concentration", 
		          HOFFSET(MHDF5MHead,concentration),H5T_NATIVE_FLOAT);
  hid_t s60=  H5Tcopy(H5T_C_S1);
  H5Tset_size(s60, 60);
  status=H5Tinsert (MHeadType, "options", HOFFSET(MHDF5MHead,options),s60);

  // ***************** End of MHeadTpe definition************************

  // At this point we are going to define the mrec datatype
  // The we will create the compound data type that defines
  // the elements of the wdata part of mdata.  
  // Then we will define another datatype that includes mrec and wdata as a
  // single element. i.e. for mdata
  
	// Create the mrec compound datatype.
  MRecType = H5Tcreate (H5T_COMPOUND, sizeof(MHDF5Rec));

  status=H5Tinsert (MRecType, "nx", HOFFSET(MHDF5Rec,nx), H5T_NATIVE_SHORT);
  status=H5Tinsert (MRecType, "ny", HOFFSET(MHDF5Rec,ny), H5T_NATIVE_SHORT);
  status=H5Tinsert (MRecType, "itheta", HOFFSET(MHDF5Rec,itheta),
                                                          H5T_NATIVE_SHORT);
  status=H5Tinsert (MRecType, "iphi", HOFFSET(MHDF5Rec,iphi),
			                                  H5T_NATIVE_SHORT);
  status=H5Tinsert (MRecType, "emission_alt", HOFFSET(MHDF5Rec,emission_alt),
                                                          H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "em_alt_sig", HOFFSET(MHDF5Rec,em_alt_sig),
                                                          H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "muon_ratio", HOFFSET(MHDF5Rec,muon_ratio),
                                                          H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "median_time", HOFFSET(MHDF5Rec,median_time),
                                                          H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "fp_median_time",
                         HOFFSET(MHDF5Rec,fp_median_time),H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "time_cfd", HOFFSET(MHDF5Rec,time_cfd),
                                                          H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "fp_time_cfd", HOFFSET(MHDF5Rec,fp_time_cfd),
                                                          H5T_NATIVE_FLOAT);
  //status=H5Tinsert (MRecType, "i_threshold", HOFFSET(MHDF5Rec,i_threshold),
  //                                                        H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "ithphi", HOFFSET(MHDF5Rec,ithphi),
                                                          H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "fp_threshold", HOFFSET(MHDF5Rec,fp_threshold),
                                                          H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "i_mult", HOFFSET(MHDF5Rec,i_mult),
			                                  H5T_NATIVE_FLOAT);
  status=H5Tinsert (MRecType, "fp_mult", HOFFSET(MHDF5Rec,fp_mult),
			                                  H5T_NATIVE_FLOAT);

  //Now the overall type.
  MDataType = H5Tcreate(H5T_COMPOUND,sizeof(MHDF5Data));

  status=H5Tinsert (MDataType, "tep", HOFFSET(MHDF5Data,tep),
		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MDataType, "idfile", HOFFSET(MHDF5Data,idfile),
  	                                                      H5T_NATIVE_INT);
  status=H5Tinsert (MDataType, "x_offset", HOFFSET(MHDF5Data,x_offset),
  		                                            H5T_NATIVE_FLOAT);
  status=H5Tinsert (MDataType, "y_offset", HOFFSET(MHDF5Data,y_offset),
  		                                            H5T_NATIVE_FLOAT);

  status=H5Tinsert (MDataType, "mrec", HOFFSET(MHDF5Data,mrec),MRecType);


// Create a VL datatype for ipix element 
  iPixType = H5Tvlen_create(H5T_NATIVE_INT);

  status=H5Tinsert (MDataType, "ipix", HOFFSET(MHDF5Data,ipix),iPixType);
// Create a VL datatype for times element 
  timesType = H5Tvlen_create(H5T_NATIVE_FLOAT);
// Create a VL datatype for pix element 
  pixType = H5Tvlen_create(timesType);

  status=H5Tinsert (MDataType, "pix", HOFFSET(MHDF5Data,pix),pixType);

   //Types are all now defined     
  return;
}


// **********************************************************************

void MHDF5SimDataFile::createFile(char* filename)
{ 
  //Might want to put some type of flag here so that we can't read and
  // write to the same file. This will fail due to all the handles 
  // (memspace etc) that we are not set up for dual use for.
  
  hdf5File = H5Fcreate(filename, H5F_ACC_TRUNC, 
                       H5P_DEFAULT, H5P_DEFAULT);
  return;
}

// **********************************************************************

void MHDF5SimDataFile::openFile(char* filename)
//-------------------------------------------------------------------------
//Opens an existing MHDF5SimDataFile file.
//-------------------------------------------------------------------------
{ 
   //Might want to put some type of flag here so that we can't read and
  // write to the same file. This will fial due to all the handles 
  // (memspace etc) that we are not set up for dual use for.

 //open a file.
  //cout<<"Trying to open:***"<<filename<<"*****"<<endl;
  hdf5File = H5Fopen(filename, H5F_ACC_RDWR,H5P_DEFAULT);
  if(hdf5File<0)
    // Did file open ok? 
    {
      const char* string=
	"Fatal--Failure Trying to open existing MHDF5 file";
      V_THROW(MHDF5SimDataFileOpenError,string);
    }
  return;
}

// **********************************************************************

void MHDF5SimDataFile::writeMHeadRec(MHDF5MHead* iHead) 
   //iHead is a struct
{
  herr_t status;
  
  //We are now ready to write this out.
  // create info dataspace ,1 element of a single compound datatype
  hsize_t dims;
  dims=1;
  hid_t dataspace  = H5Screate_simple(1, &dims, NULL); 

   // GEt dataset creation properties
  hid_t cparms     = H5Pcreate (H5P_DATASET_CREATE);
 
   //Create a new dataset within the file using cparms creation properties.
  hid_t dataset    = H5Dcreate(hdf5File, "MHEAD", MHeadType, dataspace, 
                                                                   cparms);

     // Write the data to the disk dataset.
  status  = H5Dwrite(dataset, MHeadType, H5S_ALL, H5S_ALL,  H5P_DEFAULT,iHead);

    //Release resources.
  H5Pclose(cparms);
  H5Sclose(dataspace);
  H5Dclose(dataset);
}


// **********************************************************************

void  MHDF5SimDataFile::readMHeadRec(MHDF5MHead* iHead)
{
  //Read in the MHead record from the hdf5 file.
  //Since there are no VL arrays in this record we don't have to screw around 
  //with it. We can read straight into iHead.
  hid_t   dataset;
  herr_t   status;
  hsize_t  dims;
  dims=1;
  hssize_t offset;
  offset=0;
  
//Open the existeing dataset within the file 
  H5E_BEGIN_TRY 
    {
      dataset    = H5Dopen(hdf5File, "MHEAD");
    } H5E_END_TRY;
  if(dataset < 0) 
    {           // data set is not found.No pes in this shower 
      const char* string=
	"Attempt to read MHead rec from MHDF5 file that has no MHEAD dataset";
      V_THROW(MHDF5MHEADDatasetNotFound,string);
    }
  //Define memory space
  hid_t memspace  = H5Screate_simple(1, &dims, NULL); 

  // read the data from the disk dataset.
  status  = H5Dread(dataset, MHeadType, memspace, H5S_ALL,H5P_DEFAULT, iHead);

  H5Sclose(memspace);
  H5Dclose(dataset);
  return;
}

// **********************************************************************

void MHDF5SimDataFile::writeEvent(MHDF5EventData* MHDF5ev)
{

  //WE no longer use the recordnNumber here since data is going into a buffer
  //thats flushed later.
  //However the index of the next event to be written out will be 
  //eventBufIndex+nextWriteIndex
  
  eventWrtB[eventBufIndex].tep=MHDF5ev->dataHead.tep;
  eventWrtB[eventBufIndex].idfile=MHDF5ev->dataHead.idfile;
  eventWrtB[eventBufIndex].x_offset=MHDF5ev->dataHead.x_offset;
  eventWrtB[eventBufIndex].y_offset=MHDF5ev->dataHead.y_offset;
  eventWrtB[eventBufIndex].mrec=MHDF5ev->rec;


  int numberOfPixels=MHDF5ev->iPix.size();
  eventWrtB[eventBufIndex].ipix.len=numberOfPixels;
  eventWrtB[eventBufIndex].ipix.p = new int[numberOfPixels];

  eventWrtB[eventBufIndex].pix.len = numberOfPixels;
  eventWrtB[eventBufIndex].pix.p = new MHDF5PixData[numberOfPixels];


  for(int i=0;i<numberOfPixels;i++)
    {
      ((int*)eventWrtB[eventBufIndex].ipix.p)[i]=MHDF5ev->iPix[i];
      int numberOfTimes=MHDF5ev->pixels[i].size();
      MHDF5PixData* cur=
	&(((MHDF5PixData*)eventWrtB[eventBufIndex].pix.p)[i]);
      cur->times.len = numberOfTimes; 
      cur->times.p = new float[numberOfTimes];
      for(int j=0;j<numberOfTimes;j++)
	{
	  ((float*)cur->times.p)[j] = MHDF5ev->pixels[i][j];
	}
    }



  //Read to write, See if time to flush the write buffer,
   //Bump the event index;
  eventBufIndex++;
  if(eventBufIndex<kMHDF5BufSize)
    {
      return;
    }
  else
    {
      flushEventWriteBuffer();
      nextWriteIndex=nextWriteIndex+kMHDF5BufSize;

// free up the allocated memory 
      for(int m=0;m<eventBufIndex;m++)
	{ 
	  int numberOfPixels= eventWrtB[m].ipix.len;
	  delete [](int*)eventWrtB[m].ipix.p;
	  for(int i=0;i<numberOfPixels;i++)
	    { 
	      MHDF5PixData* cur=&(((MHDF5PixData*)eventWrtB[m].pix.p)[i]);
	      delete [](float*)cur->times.p;
	    } 
	  delete [](MHDF5PixData*)eventWrtB[m].pix.p;
	}
      eventBufIndex=0;
    }
  return;
}

// **********************************************************************


void MHDF5SimDataFile::readEvent(MHDF5EventData* MHDF5ev, int recordNumber)

  //Read in an MHDF5 event record. Read in the recordNumber event in the file.
  //recordNumber starts at 0.
  //Note recordNumber may not (and probably won't) be the same as 
  //eventNumber!
  //Note this may have to be broken up into multiple parts so we can
  //handle the variable length array allocations.
{
  hsize_t  dims;
  hssize_t offset;
  herr_t   status;
  MHDF5Data event;                //instantiates an event input object.
                                  //This is what the HFD5 will read into
                                  //We then repackage it into MHDF5ev
  
  if(readDataset<0)
    {
      //Open the existing dataset within the file 
      H5E_BEGIN_TRY 
	{
	  readDataset    = H5Dopen(hdf5File, "MREC");
	} 
      H5E_END_TRY;
      if(readDataset < 0) 
	{           // data set is not found.No events in this shower 
	  const char* string=
	"Attempt to read rec from MHDF5 file that has no MREC dataset";
	  V_THROW(MHDF5MRECDatasetNotFound,string);
	}
      else
	{
	  //First time through. define things.
	  readFilespace=H5Dget_space(readDataset);
	  //Get Datset size

	  status = H5Sget_simple_extent_dims(readFilespace, &readSize,NULL);
	  //Define memory space
	  dims=1;
	  readMemspace  = H5Screate_simple(1, &dims, NULL); 
	}
    }

  
  //Check that we are not asking for more events then we have
  offset=recordNumber;

  if(offset>(int)readSize-1) 
    {
      H5Sclose(readFilespace);
      H5Dclose(readDataset);
      readDataset=-1;
      const char* string="Attempt to read  record not in MREC dataset";
      V_THROW(MHDF5RequestedMRECRecordNumberTooBig,string);
    }
  
  
  // Select a hyperslab.
  dims=1;
  status     = H5Sselect_hyperslab(readFilespace, H5S_SELECT_SET, &offset,
				   NULL, &dims, NULL);
  // read the data from the disk readDataset.
  status     = H5Dread(readDataset, MDataType, readMemspace, readFilespace, 
		       H5P_DEFAULT,&event);



  // Unpack the data into the original arrays 
  MHDF5ev->dataHead.tep=event.tep;
  MHDF5ev->dataHead.idfile=event.idfile;
  MHDF5ev->dataHead.x_offset=event.x_offset;
  MHDF5ev->dataHead.y_offset=event.y_offset;
  MHDF5ev->rec=event.mrec;

  int numberOfPixels= event.ipix.len;
  MHDF5ev->iPix.resize(numberOfPixels);
  MHDF5ev->pixels.resize(numberOfPixels);

  for(int i=0;i<numberOfPixels;i++)
    {
      MHDF5ev->iPix[i]=((int*)event.ipix.p)[i];
      MHDF5PixData* cur=&(((MHDF5PixData*)event.pix.p)[i]);
      int numberOfTimes=cur->times.len;
      MHDF5ev->pixels[i].resize(numberOfTimes);
      for(int j=0;j<numberOfTimes;j++)
	{
	  MHDF5ev->pixels[i][j]=((float*)cur->times.p)[j];
	}
    }   

  // Free up the memory allocated by HDF5 for the variable length arrays 
  status=H5Dvlen_reclaim(MDataType,readMemspace,H5P_DEFAULT,&event);
  return;
}

//  ********************************************************************

void MHDF5SimDataFile::closeFile()
{
  flushEventWriteBuffer();

  H5Fclose(hdf5File);
  return;
}

//  ********************************************************************


void MHDF5SimDataFile::closeTypes()
{ 
  H5Tclose(MHeadType);               //HDF5 info/header type reference
  H5Tclose(MRecType);
  H5Tclose(MDataType);
  H5Tclose(timesType);
  H5Tclose(pixType);
  H5Tclose(iPixType);

  return;
}

//  ********************************************************************

int MHDF5SimDataFile::getNumberEventsInFile()
  //Gets number of records (extent) of a EVENT dataset
{
  if(testInput)
    {
      return numberTestEvents;
    }

  hid_t    filespace,dataset;
  hsize_t  size;
  herr_t   status;

  //Open the existing dataset within the file
  //see if the EVENT dataset exists yet
  H5E_BEGIN_TRY 
    {
      dataset    = H5Dopen(hdf5File, "MREC");
    } 
  H5E_END_TRY;
  if(dataset < 0) 
    {                          // data set is not found 
      return 0;
    }
  else 
    {
     //Get Datset size
      filespace=H5Dget_space(dataset);
      status = H5Sget_simple_extent_dims(filespace, &size, NULL);
      H5Sclose(filespace);
      H5Dclose(dataset);
      return (int)size;
    }
}

// ************************************************************************


// *********************************************************************

void MHDF5SimDataFile:: flushEventWriteBuffer()
{
  //The data is set up in its HDF5 format.. Now we have to write it out. 
  //Most of the folowing follows from the kashdf5.c  mhdf5_out routine.

  if(eventBufIndex==0)  //See if anytrhing to write out.
   {
     return;
   }
// see if the EVENT  dataset exists yet 
  hid_t  cparms;
  herr_t status;
  hsize_t  dims;
  hsize_t  vrbl;
  hsize_t size;
  if(writeDataset<0)
    {
      H5E_BEGIN_TRY 
	{                           //HDF5 macro. hope it compiles.
	  writeDataset      = H5Dopen(hdf5File, "MREC");
	} H5E_END_TRY;
      if(writeDataset < 0) 
	{
	// data set is not found create data set 
	// Create the dataspace 
	  dims=1;
	  vrbl=H5S_UNLIMITED;
	  writeDataspace  = H5Screate_simple(1, &dims, &vrbl); 
     
    // Modify dataset creation properties, i.e. enable chunking and 
    // use GZIP compression with the compression effort set to 
    // "comression". 
    // Note that compression can be used only when dataset is chunked.
    // (Disabled for now!)
     
    
	  cparms     = H5Pcreate (H5P_DATASET_CREATE);
	  vrbl    = (hsize_t)chunkSizeEvent;
	  status     = H5Pset_chunk(cparms, 1, &vrbl);
    //status     = H5Pset_deflate(cparms, COMPRESSION_PES);
    
   //Create a new dataset within the file using cparam creation properties
	  //	  writeDataset    = H5Dcreate(hdf5File, "MREC", MDataType, 
	  //		      writeDataspace, cparms);
	  writeDataset    = H5Dcreate(hdf5File, "MREC", MDataType, 
				      writeDataspace, cparms);
	  writeFilespace  = H5Dget_space(writeDataset);      
    
    // Extend the dataset. We may start writing at some offset .ne.0 
    // Make big enough to have a place for this mrecord. 
	  size=eventBufIndex+nextWriteIndex;
	  status     = H5Dextend (writeDataset, &size);
	      
	  H5Pclose(cparms);
	}
      else 
	{
    // !!!!!!!!!Dataset exists!!!!!!!!!!!!!!Add event to EVENT

 	  writeFilespace  = H5Dget_space(writeDataset);      
	}
    }

  //Get dataset dimension. This is number of events currently in the file.
  status = H5Sget_simple_extent_dims(writeFilespace, &size, NULL);
    //Extend the dataset if needed.
  //screwy stuff here due to all index's starting at 0.
  //eventBufIndex is +1 of the last buf index to write. (buf index starts at 0)
  //thus eventBufIndex is number of events in buffer to write out.
  //nextWriteIndex is +1 of the index of the last event written out. Therefore
  //it is the present size of the file.

  hssize_t offset=nextWriteIndex;
  if(offset+eventBufIndex>(int)size) 
    {
	size=offset+eventBufIndex;
      status     = H5Dextend (writeDataset, &size);
      writeFilespace  = H5Dget_space(writeDataset); //Update the FileSpace 
    }
  //Select a hyperslab. This is place where to put the event data.
   dims=eventBufIndex;
  status     = H5Sselect_hyperslab(writeFilespace, H5S_SELECT_SET, &offset,
				     NULL, &dims, NULL);
  //Define memory space
  dims=eventBufIndex;
  if(dims!=memSpaceSize)
    {
     writeMemspace  = H5Screate_simple(1, &dims, NULL); 
      memSpaceSize=dims;
    }
 //Write the data to the hyperslab.
  status     = H5Dwrite(writeDataset, MDataType, writeMemspace, 
  			    writeFilespace, H5P_DEFAULT, &eventWrtB);
  return;
}

//  ********************************************************************

bool MHDF5SimDataFile::existsDataset(char* datasetName)
  //Determine if a dataset exists in this file.
{
  hid_t    dataset;
  //Try to open the dataset within the file
  //to see if the dataset exists.
  H5E_BEGIN_TRY 
    {
      dataset    = H5Dopen(hdf5File, datasetName);
    } 
  H5E_END_TRY;
  if(dataset < 0) 
    {                          // data set is not found 
      return false;
    }
  else 
    {
      H5Dclose(dataset);
      return true;
    }
}
// **********************************************************************

