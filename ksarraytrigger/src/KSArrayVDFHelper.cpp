//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSArrayVDFHelper.cpp
 * \ingroup common 
 * \brief File of methods for KSArrayVDFHelper.
 * This file has various methods to build the some of the various records 
 * needed for a VEGAS stage 2 multi-telescope output root file for veritas
 * data. This includes the VAQStatsData, VAPixelStatusData, VARelGainData,
 * VAKascadeSimulationHeader and VAArrayInfo records. 
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.
// ***********************************************************************
#include "KSArrayVDFHelper.h"

KSArrayVDFHelper::KSArrayVDFHelper(std::vector< KSTelescope* >* pArray,
			 VATime& startTime)
{
  pfArray=pArray;
  fNumTels=pfArray->size();
  fStartTime=startTime;
  // *************************************************************
  // Get end time for time slices as a day later
  // *************************************************************
  double fEndMJD=fStartTime.getMJDDbl()+1.0; //end time is a day later
  fEndTime.setFromMJDDbl(fEndMJD);
}
// ***********************************************************************

KSArrayVDFHelper::~KSArrayVDFHelper()
{
  //Nothing to do
}
// ***********************************************************************

void KSArrayVDFHelper::CreateArrayVDFFile(string fFileName)
, double& fEastLongitude, 
// ***********************************************************************
// Create the ArrayVDF file with the correct VAArrayInfo
// ***********************************************************************
{
  pfOut= new VAVDF();
  pfOut->createFile(fFileName,4,fStartTime);
    }
  if(pfOut==NULL)
    {
      std::cout<<"ksArrayTrigger: Failed to create ArrayVDF output file "
	       <<fFileName<<std::endl;
      exit(1);
    }
 return;
}
// ************************************************************************

void KSArrayVDFHelper::FillRunHeader(int runNumber)
// ****************************************************************
// Create the VARunHeader 
// ****************************************************************
{
  uint fRunNumber=pfArray[0]->getRunNumber();

  VARunHeader* pfRunHeader=pfOut->getRunHeaderPtr();
  pfRunHeader->pfRunDetails->fFirstEventTime=fStartTime;
  pfRunHeader->pfRunDetails->fRunNum=fRunNumber;
  pfRunHeader->pfRunDetails->fTels=fNumTels;
  pfRunHeader->pfRunDetails->fExpectedTels.resize(4,false);
  for (int i=0;i<fNumTels;i++)
    {
      VATelID fTelID=pfArray->at(i).fTelID;
      pfRunHeader->pfRunDetails->fExpectedTels[fTelID]=true;
    }
  pfRunHeader->pfRunDetails->fFirstValidEventTime=fStartTime;
  // ********************************************************************
  // May need more stuff here(like direction, source etc)
  // We don't write it because we will want to add the end of run time
  // at the end of the run.
  return;
}


void KSArrayVDFHelper::FillAndWriteQStats()
// ****************************************************************
// Create the VAQStatsData 
// ****************************************************************
{
 //Set up the time slice. We only have one for simulations
  VATimeSliceQStats fTimeSliceQStatsForArray;
  fTimeSliceQStatsForArray.fTelColl.clear();
  //Copy in the telescopes VATelQStats . Set the tel id.
  for(int i=0;i<pfArray->size())
    {
      VAQStatsData* pfQStatsForTel = 
	pfArray->at(i).pfEventFile->getQStatsDataPtr();

      //We have only one time slice.
      VATimeSliceQStats fTimeSliceQStatsForTel=
	                                    pfQStatsForTel->fTimeSliceColl[0];
      VATelQStats fTelQStatsForTel=fTimeSliceQStatsForTel.fTelColl[0];
      fTelQStatsForTel.fTelId=pfArray->at(i).fTelID;

      // ***************************************************************
      // At this point we have obtained VATelQStats for a telescope, chnaged
      // the tel ID to what is appropriate. Now at this into the output
      // VATimeSliceQStats
      // ***************************************************************
      fTimeSliceQStatsForArray.fTelColl.push_back(fTelQStatsForTel);
    }

  // *********************************************************************
  // Set up the slice times
  // *********************************************************************
  fTimeSliceQStatsForArray.fStartTime=fStartTime;
  fTimeSliceQStatsForArray.fEndTime=fEndTime;

  //Put this time slice (only one we've got) into QStats Data.
  VAQStatsData* pfQStatsForArray=pfOut->getQStatsDataPtr();
  pfQStatsForArray->fTimeSliceColl.clear();
  pfQStatsForArray->fTimeSliceColl.push_back(fTimeSliceQStatsForArray);
  pfOut->writeQStatsData();
  return;
}
// ***************************************************************************

void KSArrayVDFHelper::FillAndWriteRelGains();
// ****************************************************************
// Create the VARelGainsData 
// ****************************************************************
{
  
  VARelGainData* pfRelGainDataForArray=pfOut->getRelGainDataPtr();
  pfRelGainDataForAray->fTelColl.clear();
  for(int i=0;i<fNumTels;i++)
    {
      VARelGainData* pfRelGainDataForTel=
	pfArray->at(i).pfEventFile->getRelGainDataPtr();
      VATelRelGains  fTelRelGains=pfRelGainDataForTel->fTelColl[0];
      fTelRelGains.fTelId=pfArray->at(i).fTelID;
      pfRelGainDataForAray->fTelColl.push_back(fTelRelGains);
    }
  pfOut->writeRelGain();
  return;
}
// ***************************************************************************

void KSArrayVDFHelper::FillAndWritePixelStatus()
// ****************************************************************
// Copy the PixelStatus
// ****************************************************************
{
  VAPixelStatus* pfPixelStatusForArray=pfOut->getPixelStatusPtr();
  // *************************************************************
  // Easiest is to just copy over all the data in detail
  // *************************************************************
  for(int i=0;i<fNumTels;i++)
    {
      TelOnOffLogType tempTelOnOffLog;
      tempTelOnOffLog.fTelId=pfArray->at(i).fTelID;

      TelSuppressedLogType tempTelSuppressedLog;
      tempTelSuppressedLog.fTelId=pfArray->at(i).fTelID;
      
      VAPixelStatus* pfPixelStatusForTel=
	pfArray->at(i).pfEventFile->getPixelStatusPtr();




      for(int i=0;i<fNumPixels;i++)
	{
	  OnOffStatus tempOnOffStatus;
	  if(i<fNumPMT)
	    {
	      tempOnOffStatus.isOn=!off[i];
	    }
	  else
	    {
	      tempOnOffStatus.isOn=false;
	    }
	  tempOnOffStatus.startTime=fStartTime;
	  tempOnOffStatus.stopTime=fEndTime;  //A day later
	  ChOnOffLogType tempChanOnOffLog;
	  tempChanOnOffLog.push_back(tempOnOffStatus);
	  tempTelOnOffLog.fChColl.push_back(tempChanOnOffLog); 
	  
	  SuppressedStatus tempSuppressedStatus;
	  if(i<fNumPMT)
	    {
	      tempSuppressedStatus.isSuppressed=off[i];
	    }
	  else
	    {
	      tempSuppressedStatus.isSuppressed=true;
	    }
	  tempSuppressedStatus.startTime=fStartTime;
	  tempSuppressedStatus.stopTime=fEndTime;

	  ChSuppressedLogType tempChanSuppressedLog;
	  tempChanSuppressedLog.push_back(tempSuppressedStatus);
	  tempTelSuppressedLog.fChColl.push_back(tempChanSuppressedLog);
	}

  pfPixelStatus->fOnOffLogs.push_back(tempTelOnOffLog);
  pfPixelStatus->fSuppressedLogs.push_back(tempTelSuppressedLog);
  return;
}
// ***************************************************************************

