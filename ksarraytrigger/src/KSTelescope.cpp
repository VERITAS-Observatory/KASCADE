/inc//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSTelescope
 * \brief Class to hold and process an Telescope.
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#include "KSTelescope.h"

extern "C" float   pran(float* dummy);
extern "C" double  Rexp(double fMeanIntervel);
extern "C" int    KascadeType2CorsikaType(int fKType);
extern "C" void   GetAzElevFromVec(double* X, double& fAzimuth, 
				   double& fElevation);

KSTelescope::KSTelescope(VATelID TelID, KSAomegaDataIn* pDataIn);
{
  pfDataIn=pDataIn;
  // ****************************************************************
  // Open the input file
  // ****************************************************************
  fTelID=TelID;
  pfEventFile=NULL;
  if(pfDataIn->fRootFileName[fTelID]!=" ")
    {
      pfEventFile= new VAVDF();
      pfEventFile->OpenForStage3(pfDataIn->fRootFileName[fTelID].c_str());
      if(pfEventFile==NULL)
	{
	  std::cout<<"ksArrayTrigger: Failed to Open input file "
		   <<pfDataIn->fRootFileName[fTelID]<<std::endl;
	  exit(1);
	}
      // ****************************************************************
      // Make up a bitset array to keep track of which events have been
      // checked for being in an event
      // ****************************************************************
      fNumEvents=pfEventFile->getNumTelescopes();
      pfArrayEventsUsed= new std::bitset<fNumEvents>;

      //Get start of run
      pfInRunHeader=fEventFile->getRunHeaderPtr();
      if(pfInRunHeader==NULL)
	{
	  std::cout<<"File: "<<fInputFileName<<" has no RunHeader"
		   <<std::endl;
	  exit(1);
	}
      fFirstValidEventTime = pfInRunHeader->pfRunDetails->fFirstValidEventTime;
      pfSimTree = (TTree*)pfEventFile->getSimulationEventTreePtr();
      pfSimData = pfEventFile->getSimulationDataPtr();
    }
}
// ************************************************************************


KSTelescope::~KSTelescope
{
  //Nothing here
}
// ************************************************************************

void KSTelescope::makeGridDirMap()
// *****************************************************************
// Now the tricky and probably inefficient (but clear) way to find the event
// with a particular fNx,fNy,fDirectionIndex. Scheme is to create a std::map 
// where the key is a number built out of fNx,fNy and fDirectionIndex (saved
// in the KascadeSimulationData TTree) and the value is the event index that 
// has those values.
// This is a sperate method since we don't need to create it for the first
// telescope in the array.
// *****************************************************************
// Go through simulation TTree filling map
{
  fGridDirMap.reset();
  for(int i=0;i<fNumEvents)
    {
      pfSimTree->GetEntry(i);
      int fNx=pfSimData->fNx;
      int fNy=pfSimData->fNy;
      int fDir=pfSimData->fDirectionIndex;
      int64_t fKey=makeGrinDirKey(fNx,fNy,fDir);
      fGridDirMap[fKey]=i;
    }
  return;
}
// *************************************************************************


int64_t KSTelescope::makeGridDirKey( int fNx, int fNy, int fDir)
// *************************************************************************
// Combine fNx,fNy,fDir into a unique 64 bit number for use as a key in the
// GridDirMap. Use the fact that the range of fNX and fNY are given by 
// gAbsNXAbsNYMax (in KSCommon.h)
// **************************************************************************
{
  int fNumNxNy=(2*gAbsNXAbsNYMax)+1;  //the +1 is for 0
  int64_t fKey=fDir*fNumNxNy + fNx;
  fKey=fKey*fNumNxNy+fNy;
  return fKey;
}
// ***************************************************************************

void KSTelescope::unmakeGridDirKey(int64_t fKey,  int fNx, int fNy, int fDir)
// ***************************************************************************
// Decode fKey back into fNx fNy and fDir. This is the reverse process 
// (compliment) of what is done in makeGridDirKey. (see above).
// ***************************************************************************
{
   int fNumNxNy=2*gAbsNXAbsNYMax+1;
   int64_t fTempKey=fKey/fNumNxNy;
   fNy=fKey-(fTempKey*fNumNxNy);
   fDir=fTempKey/fNumNxNy;
   fNx=fTempKey-fDir*fNumNxNy;
   return;
}




  
