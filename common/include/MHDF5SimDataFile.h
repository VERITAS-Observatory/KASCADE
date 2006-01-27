#ifndef MHDF5_SIM_DATA_FILE_H
#define MHDF5_SIM_DATA_FILE_H
//This is the include file for the MHDF5 Structures and MHDF5SimDataFile 
//classe. This class is used to create and write and read the KASCADE MHDF5 
//Run data files.  This replaces c code that had mhdf5_X calls (kashdf5.c)
//This code is mostly an adaptation of the VHDF5RunDataFile class code.
//***********************************************************************
//   G.H.Sembroski
//   Physics Dept.
//   Purdue Univ.
//   W.Lafayette, In 47907
//   sembroski@physics.purdue.edu
//   31/07/03

//Modified:
//****************************************************************************
//Variable length HDF5 data types are used where appropriate.
//In  the MHDF5SimDataFile class member functions we make c calls to the HDF5 
//library functions.
//This code is derived form the kashdf5.c developed to write the KASCADE 
//simulation summary event files.
//20/10/04 GHS
//  in mrec change i_threshold to ithphi

#include <iostream>
#include <string>
#include <vector>
#include <hdf5.h>
#include <cmath>
using namespace std;

#include "exception.h"
//***********************************************************************

//The following structures are used to 'define' the structure of the data in 
//the MHDF5 file.

//Some constants
const int kMHDF5BufSize=10;
const int kDefaultChunkSize=10;//Best for  MHDF5 events.
const int kMaxNumberOfChannels=499;

//Define the Exceptions
class MHDF5SimDataFileOpenError:public VException
{
public:
  MHDF5SimDataFileOpenError(const char* msg=NULL)
    {
      setStrings("MHDF5SimDataFileOpenError",msg);
    }
};

class MHDF5MHEADDatasetNotFound:public VException 
{
public:
  MHDF5MHEADDatasetNotFound(const char* msg=NULL)
    {
      setStrings("MHDF5MHEADDatasetNotFound", msg);
    }
};

class MHDF5MRECDatasetNotFound:public VException
{
public:
  MHDF5MRECDatasetNotFound(const char* msg=NULL)
    {
      setStrings("MHDF5MRECDatasetNotFound", msg);
    }
};

class MHDF5RequestedMRECRecordNumberTooBig:public VException
{
public:
  MHDF5RequestedMRECRecordNumberTooBig(const char* msg=NULL)
    {
      setStrings("MHDF5RequestedMRECRecordNumberTooBig", msg);
    }
};

//Define the MHDF5 run data structures
// m_head data structure         
struct MHDF5MHead {
  int	itype;            // Start of seg head stuff 
  float	tep;
  float	dli;
  float	dmi;
  char 	magnet_field[4];
  float	etr;
  float	depth;
  float	zobs;
  float	xinitial;
  float	yinitial;
  int	idfile;
  char	version_seg[8];
  float	dl;               // Start of pe head stuff 
  float	dm;
  float	xseg;
  float	yseg;
  float	hobs;
  float	x_offset;
  float	y_offset;
  char	petype[16];
  char	version_pe[16];
  int     noise;             // Start of m head stuff 
  float   disc_width;
  float   rate_noise;
  float   reflectivity;
  float   full_aperture;
  float   pesum;
  float   step_size;
  int     itheta_max;
  int     phi_steps;
  char    version_m[16];
  float   adc_gate;
  float   hmult;
  float   concentration;
  char    options[60];
};

//20/10/04 GHS
//  in mrec change i_threshold to ithphi
// m_rec data structure 
struct MHDF5Rec 
{
  short     nx;
  short     ny;
  short     itheta;
  short     iphi;
  float     emission_alt;
  float     em_alt_sig;
  float     muon_ratio;
  float     median_time;
  float     fp_median_time;
  float     time_cfd;
  float     fp_time_cfd;
  float     ithphi;
  float     fp_threshold;
  float     i_mult;
  float     fp_mult;
};
struct MHDF5DataHead
{
  float     tep;
  int       idfile;
  float     x_offset;
  float     y_offset;
};

struct MHDF5PixData
{
  hvl_t times;
};

struct MHDF5Data
{
  float     tep;
  int       idfile;
  float     x_offset;
  float     y_offset;
  MHDF5Rec mrec;
  hvl_t     ipix;            //pixel id array
  hvl_t     pix;             // Pointer to MHDF5PixData array we can 
                             //dynamically allocate 
			     //according to the
			     // number of hit pes in each pixel.
                             //(VL arrauy of VL array.
};


//***************************************************************************
//The next class is used to transfer data. It defines the way we hold the
//run data internally in this program.

class MHDF5EventData
{
 public:
  MHDF5DataHead dataHead;
  MHDF5Rec rec;
  vector<int > iPix;          //pixel id array
  vector< vector<float > > pixels; //times arrays for all pixels
  MHDF5EventData(){};
};


//***************************************************************************
//Class for the MHDF5 Sim Data File


class MHDF5SimDataFile
{
 private: 
                             //Used by the writeEvent member function
  hid_t     writeFilespace;   
  hid_t     writeDataspace;
  hid_t     writeDataset;
  hid_t     writeMemspace;
  unsigned int memSpaceSize; 

  hid_t     readFilespace;   //Used by the readEvent member function
  hid_t     readDataspace;
  hid_t     readDataset;
  hid_t     readMemspace;
  hsize_t   readSize;              
                     //These are HDF5 types, not to be confused
                                    //with C++ or C types.
  hid_t MHeadType;               //HDF5 info/header type reference
  hid_t MRecType;
  hid_t MDataHeadType;
  hid_t MDataType;

  hid_t MDataTempType;
  
  hid_t timesType;
  hid_t pixType;
  hid_t iPixType;


                          //Now some local data
  bool testInput;
  unsigned int evNum;
  int numberTestEvents;
  bool firstTime;
  int numberCompareErrors;

 protected:
  MHDF5Data eventWrtB[kMHDF5BufSize];  //Ouput write buff.
   unsigned int maxNumberOfChannels;
  int eventBufIndex;
  int nextWriteIndex;
  hid_t hdf5File;
  int chunkSizeEvent;

 public:
  MHDF5SimDataFile();
  ~MHDF5SimDataFile();
  void makeTypes();
  void createFile(char* filename);
  void openFile(char* filename);  //Opens file for read/write.
  bool existsDataset(char* datasetName);
  bool existsMHEADDataset(){return existsDataset("MHEAD");};
  bool existsMRECDataset(){return existsDataset("MREC");};
  void writeMHeadRec(MHDF5MHead* iHead); 
  void readMHeadRec(MHDF5MHead* iHead); 

  void writeEvent(MHDF5EventData* MHDF5ev);

  void readEvent(MHDF5EventData* MHDF5ev,int recordNumber);
  void closeFile();
  void closeTypes();
  void setNextWriteIndex(int nextIndex){nextWriteIndex=nextIndex;return;};
  int  getNumberEventsInFile(); 
  int getNextIndexToWriteTo(){return nextWriteIndex+eventBufIndex;};
  unsigned int getMaxNumberOfChannels(){return maxNumberOfChannels;};
  void flushEventWriteBuffer();
};
#endif



