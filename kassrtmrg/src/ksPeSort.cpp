//-*-mode:c++; mode:font-lock;-*-
//Program to Sort a KSPeFile by nx,ny using root BuildIndex
//Ny varies fastest
//--------------------------------------------------


#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cmath>
#include <time.h>

#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"
#include "KSRootPeDataClasses.h"

#include "KSPeFile.h"

#include "TROOT.h"
#include "TObject.h"
#include "TTree.h"
#include "TBranch.h"
#include "TTreeIndex.h"


bool ksCheckSort(int nxold, int nyold, KSRootPeData* pfRootPe);

int main(int argc, char** argv)
{ 
  time_t thetime=time(NULL);
  std::cout<<"ksPeSort:  START------"<<ctime(&thetime)<<std::endl;

  TROOT root("ksPe2TFile","Sort a Pe binary file");
  argv++;
  argc--;
  if(argc<2)
    {
      std::cout
	<<"ksPeSort:Usage: ./ksPe2TFile <peInputFileName> <peSortedOuputFileName>"
	<<std::endl;
      exit(EXIT_FAILURE);
    }
  
  std::string PeFileName=*argv;
  argv++;
  std::string PeSortedFileName=*argv;
  
  std::cout<<"ksPeSort:Input UnSorted Pe File: "<<PeFileName<<std::endl;
  std::cout<<"ksPeSort:Ouput Sorted Pe File: "<<PeSortedFileName<<std::endl;
  
  
  // -----------------------------------------------------------------
  // Open input file, read and print out headers.
  // -----------------------------------------------------------------
  KSSegmentHeadData* pfSegmentHead=new KSSegmentHeadData;
  KSPeHeadData* pfPeHead=new KSPeHeadData();
  
  KSPeFile* pfPeFile=new KSPeFile();
  pfPeFile->Open(PeFileName.c_str());
  if(pfPeFile==NULL)
    {
      std::cout<<"ksPeSort:Failed to open Input Pe File: "<<PeFileName
	       <<std::endl;
      return 1;
    }
  bool goodread=pfPeFile->ReadSegmentHead(pfSegmentHead);
  if(!goodread)
    {
      std::cout<<"ksPeSort: Failed to read Segment Header from Pe File"
	       <<std::endl;
      return 1;
    }
  
  pfSegmentHead->PrintSegmentHead();
  
  goodread=pfPeFile->ReadPeHead(pfPeHead);
  if(!goodread)
    {
      std::cout<<"ksPeSort: Failed to read Pe Header from Pe File"<<std::endl;
      return 1;
    }
  
  pfPeHead->PrintPeHead();
 
  // -----------------------------------------------------------------
  // Create and Open output sorted pe file, write out segment and pe headers
  // -----------------------------------------------------------------
  KSPeFile* pfSortedPeFile=new KSPeFile();
  
  pfSortedPeFile->Create(PeSortedFileName.c_str());
  if(pfSortedPeFile==NULL)
    {
      std::cout<<"ksPeSort:Failed to Create Output Pe File: "<<PeSortedFileName
	       <<std::endl;
      return 1;
    }

  pfSortedPeFile->WriteSegmentHead(pfSegmentHead);
  pfSortedPeFile->WritePeHead(pfPeHead);

  // ----------------------------------------------------------------------
  // Set up for the sort.
  // ----------------------------------------------------------------------
  // 1:We will do the sort by first reading all the pe's into a TTree.
  // 2:Then we will use Build Index on nx,ny to sort the pes by nx,ny
  // 3:Then we will write out the pes from the TTree in sorted order.
  // 4:As a precaution we check as we write them out the the pe's are indeed
  //   being written out in sorted order.
  // ------------------------------------------------------------------

  // ------------------------------------------------------------------
  // create Pe TTree
  // ------------------------------------------------------------------
  //Note that the pe saved to the root tree has to be the root version of
  //the pe class. ie. it has Classdef's and ClassImps and is a TObject.

  KSPeData* pfBinaryPe = new KSPeData();
  KSRootPeData* pfRootPe = new KSRootPeData();

  TFile* pfTempTreeFile = (TFile*) new TFile("SortTempTreeFile.root", 
				   "RECREATE","Temoprary Sort ROOT file");
  pfTempTreeFile->cd();
  TTree* pfPeTree=new TTree("PeTree","Pe Tree");
  //pfPeTree->Branch("Pe","KSRootPeData",&pfRootPe,16000,0);
  pfPeTree->Branch("Pe","KSRootPeData",&pfRootPe,1000,0);

  // ------------------------------------------------------------------
  // Read pe's , transfer over to a RootPe record and fill the ttree. 
  // Keep track of minimums
  // ------------------------------------------------------------------
  int nxmin=0;                       //If minimum is > 0 we don't care
  int nymin=0;
  int icount=0;
  while(1)
    {
      bool goodread=pfPeFile->ReadPe(pfBinaryPe);
      if(goodread)
	{
	  if(pfBinaryPe->fNx<nxmin)
	    {
	      nxmin=pfBinaryPe->fNx;
	    }
	  if(pfBinaryPe->fNy<nymin)
	    {
	      nymin=pfBinaryPe->fNy;
	    }
	  pfRootPe->CopyInPe(pfBinaryPe);
	  pfTempTreeFile->cd();
	  pfPeTree->Fill();
	  icount++;
	  //if(icount%1000==0)
	  //  {
	  //    std::cout<<icount<<std::endl;
	  //  }
	}
      else  //Check for eof here.
	{
	  if(!pfPeFile->foundEOF())
	    {
	      std::cout<<"ksPeSort: Pe file read failure"<<std::endl;
	      exit(1);
	    }
	  else
	    {
	      break;
	    }
	}
    }
  thetime=time(NULL);
  std::cout<<"ksPeSort:  Input file read successfully------"
	   <<ctime(&thetime)<<std::endl;
  pfPeFile->Close();
  std::cout<<"ksPesort: Number of PEs read in: "<<(long)pfPeFile->getNumPes()
	   <<std::endl;
  
  if(pfPeFile->getNumPes()<=0)
    {
      std::cout<<"ksPeSort: No pes in input Pe file. Possible but not likely."
	"Skipping sort (obviously)"<<std::endl;
      std::cout<<"ksPeSort: Number of Sorted PEs written out: "
	   << (long)pfSortedPeFile->getNumPes()
	   <<std::endl;
      thetime=time(NULL);
      std::cout<<"ksPeSort:  Not quite but close to a: NORMAL END------"
	       <<ctime(&thetime)<<std::endl;
      return 0;
    }
      int fNumEntries=pfPeTree->GetEntries();
      std::cout<<"ksPeSort: Number of PEs in tree: "<<fNumEntries<<std::endl;
      std::cout<<"ksPeSort: NX minimum= "<<nxmin<<std::endl;
      std::cout<<"ksPeSort: NY minimum= "<<nymin<<std::endl;
      std::cout<<"ksPeSort: Sorting by NY fastest and increasing"<<std::endl;

  // -----------------------------------------------------------------------
  //Build the index of the ttree.
  // -----------------------------------------------------------------------
  // Note that we have to make the sort values of  nx and ny positive 
  //definate. Find offsets that will do that
  int nxoffset=1-nxmin;
  int nyoffset=1-nymin;

  //Build the index sorting strings.
  char nxStr[80];
  sprintf(nxStr,"Pe->fNx + %i",nxoffset);
  char nyStr[80];
  sprintf(nyStr,"Pe->fNy + %i",nyoffset);

  // BuildIndex is probably where all the time is spent
  //  std::cout<<"Major,minor: "<<nxStr<<" -- "<<nyStr<<std::endl;

  int fNumIndexes=pfPeTree->BuildIndex(nxStr,nyStr);   //Ny varies fastest
  if(fNumIndexes!=fNumEntries)
    {
      std::cout<<"ksPrSort: Got wrong number of sorted indexes: "<<fNumIndexes
	       <<std::endl;
      return 1;
    }

  thetime=time(NULL);
  std::cout<<"ksPeSort: Sort Finished. "<<ctime(&thetime)<<std::endl;
  std::cout<<"ksPeSort: Checking & writing out sorted file."<<std::endl;

  // Now get a pointer to the index array list
  TTreeIndex* pfPeTreeIndex=(TTreeIndex*)pfPeTree->GetTreeIndex();
  Long64_t* fIndexList=pfPeTreeIndex->GetIndex();

  // ---------------------------------------------------------------------
  // Save the TTree to the file, checking as we go that all is in sorted order
  // ---------------------------------------------------------------------
  int nxold=0;
  int nyold=0;
  Long64_t fPeIndex=0;
  for(int i=0;i<fNumIndexes;i++)
    {
      fPeIndex=fIndexList[i];
      pfPeTree->GetEntry(fPeIndex);

      if(i!=0)  //First element correct by definition.
	{
	  bool fOrderOK=ksCheckSort(nxold,nyold,pfRootPe);
	  if(!fOrderOK)
	    {
	      std::cout<<"ksPeSort: Sort failed at sort index: "<<i<<std::endl;
	      return 1;
	    }
	}
      pfRootPe->CopyOutPe(pfBinaryPe); //Transfer over to non-root version
      pfSortedPeFile->WritePe(pfBinaryPe);
      nxold=pfRootPe->fNx;   //save new "olds"
      nyold=pfRootPe->fNy;
    }
  std::cout<<"ksPeSort: Successful Sort(I checked!)"<<std::endl;
  pfSortedPeFile->Close();
  std::cout<<"ksPeSort: Number of Sorted PEs written out: "
	   << (long)pfSortedPeFile->getNumPes()
	   <<std::endl;

      
  thetime=time(NULL);
  std::cout<<"ksPeSort:  NORMAL END------"<<ctime(&thetime)
	   <<std::endl;

  return 0;
}
// **************************************************************************

bool ksCheckSort(int nxold, int nyold, KSRootPeData* pfRootPe)
// ************************************************************************
// Checks sort order. Nx,ny, ny varies fastest and increasing
{
  int nxnew=pfRootPe->fNx;
  int nynew=pfRootPe->fNy;
  if(nxnew==nxold && nynew<nyold)
    {
      std::cout<<"ksPeSort: Check-error--NY out of seq."<<std::endl;
      return false;
    }
  else if(nxnew<nxold)
    {
      std::cout<<"ksPeSort:ksCheckSort: nxnew,nxold: "<<nxnew<<" "<<nxold
	       <<std::endl;
      std::cout<<"ksPeSort: Check-error--NX out of seq."<<std::endl;
      return false;
    }
  return true;
}
