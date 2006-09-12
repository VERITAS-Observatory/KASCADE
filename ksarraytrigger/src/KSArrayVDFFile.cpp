//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSArrayVDFFile.cpp
 * \ingroup common 
 * \brief File of methods for KSArrayVDFFile.
 * This file has various methods to build the some of the various records 
 * needed for a VEGAS stage2 multi-telescope output root file for veritas
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
#include "KSArrayVDFFile.h"

KSArrayVDFFile::KSArrayVDFFile(std::vector< KSTelescope* >& pArray,
			 VATime& startTime)
{
  pfArray=pArray;
  fNumTels=pfArray.size();
  fStartTime=startTime;
  // *************************************************************
  // Get end time for time slices as a day later
  // *************************************************************
  double fEndMJD=fStartTime.getMJDDbl()+1.0; //end time is a day later
  fEndTime.setFromMJDDbl(fEndMJD);
}
// ***********************************************************************

KSArrayVDFFile::~KSArrayVDFFile()
{
  //Nothing to do
}
// ***********************************************************************

void KSArrayVDFFile::CreateVDFFile(string fFileName)
// ***********************************************************************
// Create the ArrayVDF file with the correct VAArrayInfo
// ***********************************************************************
{
  pfOut= new VAVDF();
  pfOut->createFile(fFileName,4,fStartTime);
  if(pfOut==NULL)
    {
      std::cout<<"ksArrayTrigger: Failed to create ArrayVDF output file "
	       <<fFileName<<std::endl;
      exit(1);
    }
 return;
}
// ************************************************************************

void KSArrayVDFFile::FillRunHeader(int& runNumber)
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
      VATelID fTelID=pfArray[i]->fTelID;
      pfRunHeader->pfRunDetails->fExpectedTels[fTelID]=true;
    }
  pfRunHeader->pfRunDetails->fFirstValidEventTime=fStartTime;
  // ********************************************************************
  // May need more stuff here(like direction, source etc)
  // We don't write it because we will want to add the end of run time
  // at the end of the run.
  return;
}
// *******************************************************************

void KSArrayVDFFile::FillAndWriteQStatsData()
// ****************************************************************
// Create the VAQStatsData 
// ****************************************************************
{
  //Set up the time slice. We only have one for simulations
  VATimeSliceQStats fTimeSliceQStatsForArray;
  fTimeSliceQStatsForArray.fTelColl.clear();
  //Copy in the telescopes VATelQStats . Set the tel id.
  for(int i=0;i<fNumTels;i++)
    {
      VAQStatsData* pfQStatsForTel = 
	pfArray[i]->pfVDFEventFile->getQStatsDataPtr();
      
      //We have only one time slice.
      VATimeSliceQStats fTimeSliceQStatsForTel=
	pfQStatsForTel->fTimeSliceColl[0];
      VATelQStats fTelQStatsForTel=fTimeSliceQStatsForTel.fTelColl[0];
      fTelQStatsForTel.fTelId=pfArray[i]->fTelID;
      
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

void KSArrayVDFFile::FillAndWriteRelGainsData()
// ****************************************************************
// Create the VARelGainsData 
// ****************************************************************
{
  
  VARelGainData* pfRelGainDataForArray=pfOut->getRelGainDataPtr();
  pfRelGainDataForArray->fTelColl.clear();
  for(int i=0;i<fNumTels;i++)
    {
      VARelGainData* pfRelGainDataForTel=
	pfArray[i]->pfVDFEventFile->getRelGainDataPtr();
      VATelRelGains  fTelRelGains=pfRelGainDataForTel->fTelColl[0];
      fTelRelGains.fTelId=pfArray[i]->fTelID;
      pfRelGainDataForArray->fTelColl.push_back(fTelRelGains);
    }
  pfOut->writeRelGainData();
  return;
}
// ***************************************************************************

void KSArrayVDFFile::FillAndWritePixelStatusData()
// ****************************************************************
// Copy the PixelStatus
// ****************************************************************
{

  // *************************************************************
  // Easiest is to just copy over all the data in detail
  // *************************************************************
  for(int i=0;i<fNumTels;i++)
    {
      TelOnOffLogType tempTelOnOffLog;
      tempTelOnOffLog.fTelId=pfArray[i]->fTelID;
      
      TelSuppressedLogType tempTelSuppressedLog;
      tempTelSuppressedLog.fTelId=pfArray[i]->fTelID;
      
      for(int i=0;i<fNumPixels;i++)
	{
	  OnOffStatus tempOnOffStatus;
	  tempOnOffStatus.isOn=true;
	  tempOnOffStatus.startTime=fStartTime;
	  tempOnOffStatus.stopTime=fEndTime;  //A day later
	  ChOnOffLogType tempChanOnOffLog;
	  tempChanOnOffLog.push_back(tempOnOffStatus);
	  tempTelOnOffLog.fChColl.push_back(tempChanOnOffLog); 
	  
	  SuppressedStatus tempSuppressedStatus;

	  tempSuppressedStatus.isSuppressed=false;
	  tempSuppressedStatus.startTime=fStartTime;
	  tempSuppressedStatus.stopTime=fEndTime;
	  
	  ChSuppressedLogType tempChanSuppressedLog;
	  tempChanSuppressedLog.push_back(tempSuppressedStatus);
	  tempTelSuppressedLog.fChColl.push_back(tempChanSuppressedLog);
	}
      
      VAPixelStatusData* pfPixelStatusForArray=pfOut->getPixelStatusPtr();
      pfPixelStatusForArray->fOnOffLogs.push_back(tempTelOnOffLog);
      pfPixelStatusForArray->fSuppressedLogs.push_back(tempTelSuppressedLog);
    }
  return;
}
// ***************************************************************************

void KSArrayVDFFile::FillAndWriteSimulationHeader()
// ********************************************************************
// As it says
// ********************************************************************
{
  VASimulationHeader* pfRootSimHead = 
                        pfArray[0]->pfVDFEventFile->getSimulationHeaderPtr();
  VAKascadeSimulationHead* pfKRootSimHead =
                       dynamic_cast< VAKascadeSimulationHead* >(pfRootSimHead);
  // ******************************************************
  //Copy over Simulation header
  // ******************************************************
  if(pfKRootSimHead==NULL)
    {
      std::cout<<"ksArrayTrigger: Input File has no "
	"VAKascadeSimulationHeader record"<<std::endl;
      exit(1);
    }
  if(pfRootSimHead->fSimulationPackage!=
     VASimulationHeader::E_KASCADE)
    {
      std::cout<<"ksArrayTrigger: Wrong simulation package "
	"found. fSimulationPackage: "<< pfRootSimHead->fSimulationPackage
	       <<std::endl;
      exit(1);
    }
  pfOut->setSimulationHeaderPtr(pfKRootSimHead); 
  pfOut->writeSimulationHeader();
  return;
}
// ************************************************************************

void KSArrayVDFFile::CreateKascadeSimulationDataEventTree()
// ************************************************************************
// Create VAKJascadeSimulationData pointer, TTree etc;
// ************************************************************************
{
  pfVDFKSimEvent= new VAKascadeSimulationData();
  pfVDFSimulationEventTree=new TTree(gSimulatedEventsTreeName.c_str(),
					    "Simulation Parameters");
  if(pfVDFSimulationEventTree==NULL)
    {
      std::cout<<"KSArrayTrigger: Problem creating pfSimulationEventTree"
	       <<std::endl;
      exit(1);
    }
  pfVDFSimulationEventTree->Branch(gSimulatedEventsBranchName.c_str(),
			"VAKascadeSimulationData", &pfVDFKSimEvent, 16000, 0);
  pfOut->setSimulationPtr(pfVDFKSimEvent);
  pfOut->setSimulationEventTree(pfVDFSimulationEventTree);
  return;
}
