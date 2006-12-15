//-*-mode:c++; mode:font-lock;-*-
//Program to Sort a KSPeFile by nx,ny using Palfry's and Pizlo's
// sort merge algorithum. Makes things faster because all filke access is 
//sequential
//Ny varies fastest
//--------------------------------------------------


#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cmath>
#include <time.h>
#include <algorithm>
#include <vector>


#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"
#include "KSPeFile.h"

bool ksCheckSort(int nxold, int nyold, KSPeData* pfPe);
const int kSortMax=2000000;

class FileAndPe
{
 public:
  std::ifstream* pfFile;
  KSPeData fPe;
  inline bool operator < (const FileAndPe& o) const;//This for use with sort
};

inline bool FileAndPe::operator < (const FileAndPe& o) const
{
  return fPe<o.fPe;
}

int main(int argc, char** argv)
{ 
  time_t thetime=time(NULL);
  std::cout<<"ksPeSortMerge:  START------"<<ctime(&thetime)<<std::endl;

  argv++;
  argc--;
  if(argc<2)
    {
      std::cout
	<<"ksPeSortMerge:Usage: ./ksPeSortMerge <peInputFileName> "
	"<peSortedOuputFileName>"<<std::endl;
      exit(EXIT_FAILURE);
    }
  
  std::string PeFileName=*argv;
  argv++;
  std::string PeSortedFileName=*argv;
  
  std::cout<<"ksPeSortMerge:Input UnSorted Pe File: "<<PeFileName<<std::endl;
  std::cout<<"ksPeSortMerge:Ouput Sorted Pe File: "<<PeSortedFileName
	   <<std::endl;
  
  
  // -----------------------------------------------------------------
  // Open input file, read and print out headers.
  // -----------------------------------------------------------------
  KSSegmentHeadData* pfSegmentHead=new KSSegmentHeadData;
  KSPeHeadData* pfPeHead=new KSPeHeadData();
  
  KSPeFile* pfPeFile=new KSPeFile();
  pfPeFile->Open(PeFileName.c_str());
  if(pfPeFile==NULL)    {
      std::cout<<"ksPeSortMerge:Failed to open Input Pe File: "<<PeFileName
	       <<std::endl;
      return 1;
    }
  bool goodread=pfPeFile->ReadSegmentHead(pfSegmentHead);
  if(!goodread)
    {
      std::cout<<"ksPeSortMerge: Failed to read Segment Header from Pe File"
	       <<std::endl;
      return 1;
    }
  
  pfSegmentHead->PrintSegmentHead();
  
  goodread=pfPeFile->ReadPeHead(pfPeHead);
  if(!goodread)
    {
      std::cout<<"ksPeSortMerge: Failed to read Pe Header from Pe File"
	       <<std::endl;
      return 1;
    }
  
  pfPeHead->PrintPeHead();
  
  // **********************************************************************
  //Create output file	and write our headers
  // **********************************************************************
  KSPeFile* pfSortedPeFile=new KSPeFile();
  
  pfSortedPeFile->Create(PeSortedFileName.c_str());
  if(pfSortedPeFile==NULL)
    {
      std::cout<<"ksPeSortMerge:Failed to Create Output Pe File: "
	       <<PeSortedFileName
	       <<std::endl;
      return 1;
    }

  pfSortedPeFile->WriteSegmentHead(pfSegmentHead);
  pfSortedPeFile->WritePeHead(pfPeHead);
  // ***********************************************************************

  // -----------------------------------------------------------------
  // Read in blocks of kSortMax pes, sort them, and write them out to temp 
  // files.
  // -----------------------------------------------------------------
  std::vector<KSPeData> pfPeSortVector;
  std::vector<std::string> pfTempFileNames;
  KSPeData* pfBinaryPe = new KSPeData();
  int fInputNumPe=0;
  int fNumFiles=0;
  int fNxMinimum=0;
  int fNyMinimum=0;
  while(1)
    {
      pfPeSortVector.clear();
      // ****************************************************************
      // Read in a block of Pe's into a vector.
      for(int i=0;i<kSortMax;i++)
	{
	  bool goodread=pfPeFile->ReadPe(pfBinaryPe);
	  if(goodread)
	    {
	      if(pfBinaryPe->fNx<fNxMinimum)
		{
		  fNxMinimum=pfBinaryPe->fNx;
		}
	      if(pfBinaryPe->fNy<fNyMinimum)
		{
		  fNyMinimum=pfBinaryPe->fNy;
		}
	      pfPeSortVector.push_back(*pfBinaryPe);
	      fInputNumPe++;
	    }
	  else  //Check for eof here.
	    {
	      if(!pfPeFile->foundEOF())
		{
		  std::cout<<"ksPeSortMerge: Pe file read failure"<<std::endl;
		  exit(1);
		}
	      else
		{
		  std::cout<<"ksPeSortMerge: Found input file EOF"<<std::endl;
		  break;
		}
	    }
	}

      // ****************************************************************
      // Sort this vector using < overloaded operator defined in KSPeData
      // This operator sorts by NX,NY with NY varying fastest.
      // ****************************************************************
      if(pfPeSortVector.size()>0)
	{
	  std::sort(pfPeSortVector.begin(),pfPeSortVector.end());
	  // *************************************************************
	  // Now create an temp output file top hold this block and write 
	  // them out
	  // *************************************************************
	  char fName[80];
	  sprintf(fName,"TempSortFile%0ipe",fNumFiles);
	  std::string fPeFileName(fName);
	  // **************************************************************
	  // Create a new file, Since these files must quite a bit less than 
	  // 2.2 gbyte so that the above sort can be done in memory then we 
	  // can use std::ifstream
	  // ************************************************************** 
	  std::ofstream* pfTempFile=new std::ofstream(fPeFileName.c_str(), 
			  std::ios::out | std::ios::binary);
	  for(int i=0;i<(int)pfPeSortVector.size();i++)
	    {
	      pfTempFile->write((char*)&pfPeSortVector.at(i),sizeof(KSPeData));
	    }
	  pfTempFile->close();
	  fNumFiles++;
	  pfTempFileNames.push_back(fPeFileName);
	}

      if(pfPeFile->foundEOF())
	{
	  break;
	}
    }

  // *********************************************************************
  // Do the special case of an empty file, which is possible and ok.
  // *********************************************************************
  if(fInputNumPe==0)
    {
      std::cout<<"ksPeSortMerge: No pes in input Pe file. Possible but not "
	"likely. Skipping sort (obviously)"<<std::endl;
      std::cout<<"ksPeSortMerge: Number of Sorted PEs written out: "
	   << fInputNumPe<<std::endl;
      thetime=time(NULL);
      std::cout<<"ksPeSortMerge:  Not quite but close to a: NORMAL END------"
	       <<ctime(&thetime)<<std::endl;
      return 0;
    }


  thetime=time(NULL);
  std::cout<<"ksPeSortMerge: Input Read Finished."<<ctime(&thetime)<<std::endl;
  std::cout<<"ksPeSortMerge: Merging, Checking, and Writing out sorted file."
	   <<std::endl;

  std::cout<<"ksPesortMerge: Number of PEs read in: "
	   <<(long)pfPeFile->getNumPes()<<std::endl;
  std::cout<<"ksPesortMerge: Number of Temporary Sort files of maximum size "
	   <<kSortMax<<" created: "<<pfTempFileNames.size()<<std::endl;

  std::cout<<"ksPesortMerge: NX Minimum= "<<fNxMinimum<<std::endl;
  std::cout<<"ksPesortMerge: NY Minimum= "<<fNyMinimum<<std::endl;

  // ***********************************************************************
  // That was the sort part. Now the merge part.
  // Open for reading all the temp files and place in a vector.Using 
  // as vector of structs and iterators here so erase() will be available 
  // later.
  // ************************************************************************
  // Init setup for merge loop. Note this will also work for only 1 file
  // ************************************************************************
  FileAndPe fTempFileAndPe;
  std::vector<FileAndPe> pfFilePeList;
  KSPeData fPe;
  std::vector<FileAndPe>::iterator fPos;
  for(int i=0;i<(int)pfTempFileNames.size();i++)
    {
      fTempFileAndPe.pfFile=new std::ifstream(pfTempFileNames.at(i).c_str(), 
		            std::ios::in | std::ios::binary);
      fTempFileAndPe.pfFile->read((char*)&fTempFileAndPe.fPe, 
				                            sizeof(KSPeData));
      pfFilePeList.push_back(fTempFileAndPe);
   }

  // ************************************************************************
  // Merge and write loop
  // ************************************************************************
  int fNxOld=0;
  int fNyOld=0;
  int fNumOut=0;
  for(int i=0;i<fInputNumPe;i++)
    {
      // **************************************************************
      // Find next pe to write out. Note we have overloaded the < in the stuct 
      // **************************************************************
      std::vector<FileAndPe>::iterator fNextFileAndPePos =
	            std::min_element(pfFilePeList.begin(),pfFilePeList.end());

      // ***************************************************************
      // Check that it is indeed in order then write it out.
      // ***************************************************************
      
      fTempFileAndPe=*fNextFileAndPePos;
      if(i!=0)  //First element correct by definition.
	{
	  bool fOrderOK=ksCheckSort(fNxOld,fNyOld,&fTempFileAndPe.fPe);
	  if(!fOrderOK)
	    {
	      std::cout<<"ksPeSortMerge: Sort failed at sort index: "<<i
		       <<std::endl;
	      return 1;
	    }
	}
      pfSortedPeFile->WritePe(&fTempFileAndPe.fPe);
      fNumOut++;
      fNxOld=fTempFileAndPe.fPe.fNx;   //save new "olds"
      fNyOld=fTempFileAndPe.fPe.fNy;
      // ******************************************************************
      // Replenish the 'used' pe. If thats the end of file we get tricky: 
      // Close the file. Remove the entry for that file from pfTempSrtFiles.
      // ******************************************************************
 
      fTempFileAndPe.pfFile->read((char*)&fTempFileAndPe.fPe,sizeof(KSPeData));
      if(!fTempFileAndPe.pfFile->good())
	{
	  // EOF found for that file Remove it from FileAndPe vector 
	  pfFilePeList.erase(fNextFileAndPePos);
	}
      else
	{
	  *fNextFileAndPePos=fTempFileAndPe;
	}
    }
  std::cout<<"ksPeSortMerge: Successful Sort(I checked!)"<<std::endl;
  pfSortedPeFile->Close();
  std::cout<<"ksPeSortMerge: Number of Sorted PEs written out: "
	   << (long)pfSortedPeFile->getNumPes()
	   <<std::endl;
      
  thetime=time(NULL);
  std::cout<<"ksPeSortMerge:  NORMAL END------"<<ctime(&thetime)
	   <<std::endl;

  return 0;
}
// **************************************************************************

bool ksCheckSort(int nxold, int nyold, KSPeData* pfPe)
// ************************************************************************
// Checks sort order. Nx,ny, ny varies fastest and increasing
{
  int nxnew=pfPe->fNx;
  int nynew=pfPe->fNy;
  if(nxnew==nxold && nynew<nyold)
    {
      std::cout<<"ksPeSortMerge: Check-error--NY out of seq."<<std::endl;
      return false;
    }
  else if(nxnew<nxold)
    {
      std::cout<<"ksPeSortMerge:ksCheckSort: nxnew,nxold: "<<nxnew<<" "<<nxold
	       <<std::endl;
      std::cout<<"ksPeSortMerge: Check-error--NX out of seq."<<std::endl;
      return false;
    }
  return true;
}
