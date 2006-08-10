//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSArrayEvent
 * \brief Class to hold and process an ArrayEvent.
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#include "KSArrayEvent.h"

extern "C" float   pran(float* dummy);
extern "C" double  Rexp(double fMeanIntervel);
extern "C" int    KascadeType2CorsikaType(int fKType);
extern "C" void   GetAzElevFromVec(double* X, double& fAzimuth, 
				   double& fElevation);

KSArrayEvent::KSArrayEvent(std::string fOutputRootFileName,
			   KSArrayTriggerDataIn* pDataIn)
{
  pfDataIn=pDataIn;
  // ****************************************************************
  // Open the input files, check we have enough telescopes for multiplicity
  // ****************************************************************
  // Note that while these files all share the same mount direction vectors
  // they may not have the same triggers. We thus make no assumptions about
  // correlations of eventIndex between input files.
  // ****************************************************************
  fNumTelsInArray=0;
  // Try each telescope

  pfT1 = new KSTelescope(E_T1,pfDataIn->fT1RootFileName);
  if(pfT1->fEventFile!=NULL)
    {
      fArray.push_back(pfT1);
    }
  pfT2 = new KSTelescope(E_T2,pfDataIn->fT2RootFileName);
  if(pfT2->fEventFile!=NULL)
    {
      fArray.push_back(pfT2);
    }
  pfT3 = new KSTelescope(E_T3,pfDataIn->fT3RootFileName);
  if(pfT3->fEventFile!=NULL)
    {
      fArray.push_back(pfT3);
    }
  pfT4 = new KSTelescope(E_T4,pfDataIn->fT4RootFileName);
  if(pfT4->fEventFile!=NULL)
    {
      fArray.push_back(pfT4);
    }

  //Check we have enough for multiplicity
  fNumTels=fArray.size();
  if(fNumTels<pfDataIn->fArrayTriggerMultiplicty)
    {
      std::cout<<"ksArrayTrigger: Too few telescopes specified to make "
	"multiplicty requirements"<<std::endl;
      exit(1);
    }
  if(fNumTels<2)
    {
      std::cout<<"ksArrayTrigger: Need at least 2 telescopes to make an array"
	std::endl;
      exit(1);
    }
  VATime fFirstValidEventTime=fArray[0]->getfFirstValidEventTime;


  // **************************************************************************
  // Open the ouput VDF file. Create and write the Run Header,ArrayInfo,
  // QStatsData. PixelsStatusData RelGainData and SimulationHeader
  // **************************************************************************

  pfVDFOut= new KSArrayVFDHelper(fArray,fFirstValidEventTime);
  pfVDFOut->WriteRunHeader();
  pfVDFOut->WriteArrayInfo();
  pfVDFOut->WriteQStatsData();
  pfVDFOut->WritePixelStatusData();
  pfVDFOut->WriteRelGainData();
  pfVDFOut->WriteSimulationHeader();

  // ***********************************************************************
  // Set up for the trigger search 
  // **********************************************************************
  // We need to make fGridDirMap's for all the telescopes except the first one
  // **********************************************************************
  for(int i=1;i<fNumTels;i++)
    {
      fArray[i]->makeGridDirMap();
    }

  fBaseTel=0; //Index to base telescope in fArray for search.
  fBaseIndex=0;  //Index in Base TTree of next event to look at for a trigger
}
// ***************************************************************************

KSArrayEvent::~KSArrayEvent()
{
  // Nothing to do yet
}

bool KSArrayEvent FindTrigger()
// ***************************************************************************
// Find the next trigger in the array. Return false if shower has ended
// ***************************************************************************
{
  while(1)   //Searh for trigger
    {
      // **********************************************************************
      // First test that  this event in this telescope has not been used yet
      // *********************************************************************
      while(1)
	{
	  if(!fArray[fBaseTel]->pfArrayEventsUsed[fBaseIndex])
	    {
	      fBaseIndex++;
	      if(fBaseIndex==fArray[fBaseTel]->fNumEvents)
		{
		  // *********************************************************
		  // We have finished looking for array triggers were this
		  // telscope (fBaseTel) is in the trigger. If out required
		  // multiplicity allows us switch to the next telescope as 
		  // the next telescope to use as our Base Telescope. Be 
		  // careful to not use events that have already been tested. 
		  // *********************************************************
		  fBaseTel++;
		  fBaseIndex=0;
		  if(fNumTels-fBaseTel<pfDataIn->fArrayTriggerMultiplicty)
		    {
		      // *********************************************
		      // Were done. Shower is finished
		      // *********************************************
		      return false;
		    }
		  continue;
		}
	    }
	}
      // ******************************************************************
      // Try using this event this telescope as a base event
      // ******************************************************************
      // Flag that we are using it
      fArray[fBaseTel]->pfArrayEventsUsed[fBaseIndex]=true;
      // Now test the other telescopes to see if they triggered.
      // Find nx,ny,fdir this telescope this event
      int fNx;
      int fNy;
      int fDir;
      int64_t fBaseGridDirKey=fArray[fBaseTel]->getGridDirIndex(fBaseIndex,
								fNx,fNy,fDir);
      int fMult=1;
      
      fTriggerEvents.clear();
      TrigEvent fTEvent;
      fTevent.fIndex=fBaseIndex;
      fTEvent.fTel=fBaseTel;
      fTriggerEvents.push_back(fTEvent);
      
      for(int i=fBaseTel+1;i<fNumTels)
	{
	  // ***********************************************************
	  // Find the GridDir Key for the next telescope relative to this 
	  // telescope this event
	  // ***********************************************************
	  int64_t fKey=GetTelescopeGridDirKey(fBaseTel,fNx,fNy,fDir,i);
	  
	  // **********************************************************
	  // See if this telescope has an event with this GridKey
	  // **********************************************************
	  int fIndex=fArray[i]->getIndexThisGridDirKey(fKey);
	  // Returns -1 if event doesn't exist
	  if(fIndex>-1)
	    {
	      //put t event into Trigger events vector.
	      fTevent.fIndex=fIndex;
	      fTEvent.fTel=i;
	      fTriggerEvents.push_back(fTEvent);
	      // Flag that we are using it
	      fArray[fTel]->pfArrayEventsUsed[fIndex]=false;
	    }
	}
      // ************************************************************
      // Now see if we have enought telescopes in out trigger vector to
      // meet the multiplicity rewquirment
      // ************************************************************
      if(fTriggeredEvents.size()<pfDataIn->fArrayTriggerMultiplicty)
	{
	  // *********************************************************
	  // Try again
	  continue;
	}
      else
	{
	  return true;   
	}
    }
}
