//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSW10mVDF.cpp
 * \ingroup common 
 * \brief File of methods for KSW10mVDF.
 * This file has various methods to build the some of the various recods 
 * needed for a minimal VEGAS stage 2 output root file for whipple data. This 
 * includes the VAQStatsData, VAPixelStatusData, VARelGainData and VAArrayInfo
 * records. This code was originally in cparamVDF.cpp
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
#include "KSW10mVDF.h"

KSW10mVDF::KSW10mVDF(VAVDF* pOut, int numChannels, VATime& startTime,
		     int Whipple10MId, int NumWindowSamples)
{
  pfOut=pOut;
  fNumChannels=numChannels;
  fStartTime=startTime;
  fEndTime.setFromMJDDbl(fStartTime.getMJDDbl() + 1.0);//A day later
  fWhipple10MId=Whipple10MId;
  fNumWindowSamples=NumWindowSamples;
  //std::cout<<"KSW10mVDF: *****Initalizing"<<std::endl;
  //std::cout<<"KSW10mVDF: numChannels,TelID,numWindowSamples: "<<fNumChannels
  //	   <<" "<<fWhipple10MId<<" "<<fNumWindowSamples<<endl;
  //std::cout<<"KSW10mVDF: MJD Start,MJD End: "<<fStartTime.getMJDDbl()<<" "
  //	   <<fEndTime.getMJDDbl()<<std::endl;
}
// ***********************************************************************

KSW10mVDF::~KSW10mVDF()
{
  //Nothing to do
}
// ***********************************************************************

void KSW10mVDF::CreateW10mVDFFile(string fFileName, double& fEastLongitude, 
				  double& fLatitude)
// ***********************************************************************
// Create the VDF file with the correct VAArrayInfo
// ***********************************************************************
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
  
  //std::cout<<"KSW10mVDF: FileName: "<<fFileName<<std::endl;
  //std::cout<<"KSW10mVDF:Telescope at East Longitude: "<<fEastLongitude	//   <<", Latitude: "<<fLatitude<<std::endl;
  return;
}
// ************************************************************************

void KSW10mVDF::FillRunHeader(int runNumber)
// ****************************************************************
// Create the Whipple VARunHeader 
// ****************************************************************
{
  //std::cout<<"KSW10mVDF:RunNumber: "<<runNumber<<std::endl;
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


void KSW10mVDF::FillW10mQStats(const float* ped, const float* pedvar)
// ****************************************************************
// Create the Whipple VAQStatsData 
// ****************************************************************
{
  VATelQStats tempTelQStats;
  tempTelQStats.fTelId=fWhipple10MId;//Only one telescope

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

void KSW10mVDF::FillW10mRelGains(const float* gain)
// ****************************************************************
// Create the Whipple VARelGainsData 
// ****************************************************************
{
  VATelRelGains tempTelRelGains;
  tempTelRelGains.fTelId=fWhipple10MId;  
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
      tempChanRelGains.fMean=1./gain[i];
      tempChanRelGains.fVar=sqrt(gain[i]);//arbitrary

      tempTelRelGains.fLowGainChanColl.push_back(tempChanRelGains);
      tempTelRelGains.fHighGainChanColl.push_back(tempChanRelGains);
    }

  pfRelGainData=pfOut->getRelGainDataPtr();
  pfRelGainData->fTelColl.push_back(tempTelRelGains);
  return;
}
// ***************************************************************************

void KSW10mVDF::FillPixelStatus(int fNumPMT, bool* off)
// ****************************************************************
// Create the Whipple PixelStatus
// ****************************************************************
{
  //std::cout<<"KSW10mVDF: fNumPMT: "<<fNumPMT<<std::endl;
  TelOnOffLogType tempTelOnOffLog;
  tempTelOnOffLog.fTelId=fWhipple10MId;

  TelSuppressedLogType tempTelSuppressedLog;
  tempTelSuppressedLog.fTelId=fWhipple10MId;

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

