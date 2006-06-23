//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSVDFHelper.cpp
 * \ingroup common 
 * \brief File of methods for KSVDFHelper.
 * This file has various methods to build the some of the various records 
 * needed for a minimal VEGAS stage 2 output root file for whipple and veritas
 * data. This includes the VAQStatsData, VAPixelStatusData, VARelGainData and 
 * VAArrayInfo records. This code was originally in cparamVDF.cpp
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
#include "KSVDFHelper.h"

KSVDFHelper::KSVDFHelper(int numChannels, VATime& startTime,
			 int TelID, int NumWindowSamples, 
			 KSCameraTypes CameraType)
{
  fNumChannels=numChannels;
  fStartTime=startTime;
  fEndTime.setFromMJDDbl(fStartTime.getMJDDbl() + 1.0);//A day later
  fTelID=TelID;
  fCameraType=CameraType;
  fNumWindowSamples=NumWindowSamples;
 }
// ***********************************************************************

KSVDFHelper::~KSVDFHelper()
{
  //Nothing to do
}
// ***********************************************************************

void KSVDFHelper::CreateVDFFile(string fFileName, double& fEastLongitude, 
				  double& fLatitude)
// ***********************************************************************
// Create the VDF file with the correct VAArrayInfo
// ***********************************************************************
{
  pfOut= new VAVDF();

  if(fCameraType==WHIPPLE490)
    {
      std::string lCam("WC490");
      pfOut->createFileWithCamera(fFileName,1,lCam);
                     //Creates and Opens the output file
                     //(But does not write them).
                     //Also creates all the objects to be written to the file;
                     //Creates a VAArrrayInfo with a whipple 490 pixel camara
      pfArrayInfo=pfOut->getArrayInfoPtr();
      //Pick up Longitude and Latitude
      fEastLongitude=pfArrayInfo->longitude();
      fLatitude=pfArrayInfo->latitude();
    }
  else if(fCameraType==VERITAS499)
    {
      pfOut->createFile(fFileName,1,fStartTime);
    }
  if(pfOut==NULL)
    {
      std::cout<<"ksAomega: Failed ot create VDF output file "<<fFileName
	       <<std::endl;
      exit(1);
    }
 return;
}
// ************************************************************************

void KSVDFHelper::FillRunHeader(int runNumber)
// ****************************************************************
// Create the VARunHeader 
// ****************************************************************
{
  //std::cout<<"KSVDFHelper:RunNumber: "<<runNumber<<std::endl;
  VARunHeader* pfRunHeader=pfOut->getRunHeaderPtr();
  pfRunHeader->pfRunDetails->fFirstEventTime=fStartTime;
  pfRunHeader->pfRunDetails->fRunNum=runNumber;
  pfRunHeader->pfRunDetails->fTels=1;
  pfRunHeader->pfRunDetails->fExpectedTels.resize(1);
  pfRunHeader->pfRunDetails->fExpectedTels.at(0)=true;
  //pfRunHeader->pfRunDetails->fNumOfChans.resize(1);
  //  pfRunHeader->pfRunDetails->fNumOfChans.at(0)=fNumChannels;
  pfRunHeader->pfRunDetails->fFirstValidEventTime=
                                   pfRunHeader->pfRunDetails->fFirstEventTime;
  return;
}


void KSVDFHelper::FillQStats(const float* ped, const float* pedvar)
// ****************************************************************
// Create the VAQStatsData 
// ****************************************************************
{
  VATelQStats tempTelQStats;
  tempTelQStats.fTelId=fTelID;//Only one telescope

  for(int i=0;i<fNumChannels;i++)
    {
      VASumWinQStats tempSumWinQStats;//We have only 1 window size per channel
      tempSumWinQStats.fSumWinSize=fNumWindowSamples;   //arbitrary
      tempSumWinQStats.fNumEvtsAcc=1000;                //arbitrary
      tempSumWinQStats.fChargeMean=ped[i];
      tempSumWinQStats.fChargeVar=pedvar[i];

      VAChanQStats tempChanQStats;   //Only this entry in our "sum"
      tempChanQStats.fChanNum=i;
      for(int j=0;j<fNumWindowSamples;j++)
	{
	  tempChanQStats.fSumWinColl.push_back(tempSumWinQStats);
	}
      //Add this Channel to this telescope data
      tempTelQStats.fChanColl.push_back(tempChanQStats); 
    }
  //At this point we have a VATelQStats for 1 telescope for one time slice
  //in tempTelQStats

  //Set up the time slice.
  VATimeSliceQStats tempTimeSliceQStats;
  tempTimeSliceQStats.fTelColl.push_back(tempTelQStats);
  tempTimeSliceQStats.fStartTime=fStartTime;
  tempTimeSliceQStats.fEndTime=fEndTime;
      
  //Put this time slice (onl;y one we've got) into QStats Data.
  pfQStats=pfOut->getQStatsDataPtr();
  pfQStats->fTimeSliceColl.push_back(tempTimeSliceQStats);
  return;
}
// ***************************************************************************

void KSVDFHelper::FillRelGains(const float* gain)
// ****************************************************************
// Create the VARelGainsData 
// ****************************************************************
{
  VATelRelGains tempTelRelGains;
  tempTelRelGains.fTelId=fTelID;  
  tempTelRelGains.fLowGainRefRelGain=1.0;
  tempTelRelGains.fHighGainRefRelGain=1.0;
  tempTelRelGains.fLowGainRefVarRelGain=1.0;//arbitrary
  tempTelRelGains.fHighGainRefVarRelGain=1.0;//arbitrary

  //Gains are not time variable.
  for(int i=0;i<fNumChannels;i++)
    {   //Use same gain for hi and low. Only one channel here.
      VAChanRelGains tempChanRelGains;
      tempChanRelGains.fChanNum=i;
      tempChanRelGains.fNumEvtsAcc=1000;   //arbitrary
      tempChanRelGains.fMean=gain[i];
      tempChanRelGains.fVar=sqrt(gain[i]);//arbitrary

      tempTelRelGains.fLowGainChanColl.push_back(tempChanRelGains);
      tempTelRelGains.fHighGainChanColl.push_back(tempChanRelGains);
    }

  pfRelGainData=pfOut->getRelGainDataPtr();
  pfRelGainData->fTelColl.push_back(tempTelRelGains);
  return;
}
// ***************************************************************************

void KSVDFHelper::FillPixelStatus(int fNumPMT, bool* off)
// ****************************************************************
// Create the PixelStatus
// ****************************************************************
{
  //std::cout<<"KSVDFHelper: fNumPMT: "<<fNumPMT<<std::endl;
  TelOnOffLogType tempTelOnOffLog;
  tempTelOnOffLog.fTelId=fTelID;

  TelSuppressedLogType tempTelSuppressedLog;
  tempTelSuppressedLog.fTelId=fTelID;

  for(int i=0;i<fNumChannels;i++)
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
  pfPixelStatus=pfOut->getPixelStatusPtr();
  pfPixelStatus->fOnOffLogs.push_back(tempTelOnOffLog);
  pfPixelStatus->fSuppressedLogs.push_back(tempTelSuppressedLog);
  return;
}
// ***************************************************************************

