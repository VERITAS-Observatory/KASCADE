/**
 * \class KSCFD
 * \ingroup common
 * \brief File of methods for KSCFD. * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/


#include "KSCFD.h"

KSCFD::KSCFD(KSCameraTypes CameraType)
{
  fCameraType=CameraType;
  fNumCFDDelayBins     = (int)(gCFDDelayNS[fCameraType]/gWaveFormBinSizeNS);
  fCFDFraction             = gCFDFraction[fCameraType];
  fCFDOffsetPE         = gCFDOffsetPE[fCameraType];
  fCFDTriggerDelayBins = (int)(gCFDTriggerDelayNS[fCameraType]/
			                                   gWaveFormBinSizeNS);
}
// ************************************************************************

KSCFD::~KSCFD()
{
  // Nothing here
}
// ************************************************************************


bool KSCFD::isFired(KSPixel& fPixel, double fStartTimeOffsetNS,
		    double fLastTimeOffsetNS, double fLastCFDCrossingNS,
		    int nx, int ny)
// ***************************************************************************
// Test to see if this pixel's waveform fires its CFD
// ***************************************************************************
// Note: Units of fPixel->pfWaveform are PE's
// ***************************************************************************
// We are going to try and do both WHIPPLE490 and VERITAS499 CFD's here in the
// same code
// ***************************************************************************
// Model CFD on waveform. Sets in fPixel.fCFDTriggerTimeNS (in NS) time of 
// trigger(s) (see below) (Whipple:or to fOverflowTime time if no trigger 
// found.
// ************************************************************************
// ---------------------------------------------------------------------------
// The purpose of the CFD (Constant Fraction Discriminator) is to find the time
// an acceptably high enough pulse (one that goes above fThreshold) reaches a 
// certain fraction of its maximum height. That fraction is set by the 
// fCFDGain. (Trigger fraction is 1/fCFDGain).
// ---------------------------------------------------------------------------
// How CFD's work: Internal to the CFD the input waveform is mainpulated by 
// 1:Negating , and attenuating  (by fCFDGain) the input pulse.
// 2:This is then added back to the the delayed original pulse to form  the 
//   internal CFD pulse (called CFDPulse below).
// 3:This internal CFD pulse is searched for offset level crossings. 
// 4:Requirement then is that original pulse is above threshold at the same 
//   time as the internal pulse is above the offset.

// 5:The time that this happens is the CFD triggerTime.
// 6:VERITAS CFD's have the  added requirment that the original pulse must
//   still be above threshold gCFDTriggerDelayNS after 0 crossing time.

// ---------------------------------------------------------------------------
// fCFDDelay is determined normaly by the expected rise time of the pulse.
// ---------------------------------------------------------------------------
//  Check the complete wave form for triggers. ie we can have multiple 
//  triggers. This is especially important when the threshold is near the 
//  night sky pedvar.
// **************************************************************************
{
  bool printWaveForms=false; 
  bool triggerFound=false;
  int fNumWaveFormBins=fPixel.fNumWaveFormBins;
  
  fPixel.fCFDTriggerTimeNS.clear();           //Default is no triggers
 

  int fStartThresholdCheckBin=(int)(fStartTimeOffsetNS/gWaveFormBinSizeNS);
  if(fStartThresholdCheckBin>fNumWaveFormBins-1)
    {                
      return triggerFound;    // no trigger
    }

  fLastThresholdCheckBin=(int)(fLastTimeOffsetNS/gWaveFormBinSizeNS);
  fLastCFDCrossingCheckBin= (int)(fLastCFDCrossingNS/gWaveFormBinSizeNS);

  // *********************************************************************
  // Note: Since we have to look for a transision of the summed original
  //       waveform and the delayed negated amplified waveform we don't need 
  //       to look for threshold before the delay value
  // *********************************************************************
  if(fStartThresholdCheckBin<fNumCFDDelayBins)            //Force minimum.
    {
      fStartThresholdCheckBin=fNumCFDDelayBins;  
    }                                
  
  // *************************************************************************
  // To improve performance, first check that we have at least one bin in the
  // waveform above threshold.
  // *************************************************************************
  int thresholdCrossingBin=fStartThresholdCheckBin; 
  bool aboveThreshold=findAboveThreshold(fPixel,thresholdCrossingBin);
  if(!aboveThreshold)
    {
      return triggerFound;
    }

  // ******************************************************
  // We search for all  offset crossings
  // Note we use thresholdCrossing to init where we start looking
  // ************************************************************************
  int offsetCrossingBin=thresholdCrossingBin;

  makeInternalCFDWaveForm(fPixel,printWaveForms);

  // *********************************************************
  // Now that the CFDPulse is constructed we can make a loop that
  // checks for the positive threshold crossing followed by the positive to 
  // negative transition through the offset level. If at that time the normal
  // wave form is above threshold and if CFDTriggerD4elay it still is above
  // threshold we have a CFD trigger.
  while(1)
    {
      // ******************************************************************
      //Check for negative to pos crossing of positive offset level in 
      //CFDWaveform
      // *******************************************************************
      bool offsetCrossingFound=findNextOffsetCrossing(offsetCrossingBin);
      if(!offsetCrossingFound)
	{
	  return triggerFound;
	}
      // *******************************************************************
      // We have an offset crossing. 
      // *******************************************************************
      // WHIPPLE490
      // **************************
      if(fCameraType==WHIPPLE490) 
	{
	  // *************************************************************
	  // We may need to get better at when we check for the threshold 
	  // for Whipple
	  // *************************************************************
	  if(fPixel.pfWaveForm->GetWaveFormElement(offsetCrossingBin+fCFDTriggerDelayBins)
	     >=fPixel.fThreshold) 
	    {
	      triggerFound=true;
	      double triggerTimeNS = offsetCrossingBin*gWaveFormBinSizeNS +
		fPixel.fWaveFormStartNS;
	      fPixel.fCFDTriggerTimeNS.push_back(triggerTimeNS);
	      offsetCrossingBin=offsetCrossingBin-fCFDTriggerDelayBins+
		int(gPSTPulseWidthNS[fCameraType]/gWaveFormBinSizeNS);
	      
	    }
	}
      // *************************
      // VERITAS499
      // *************************
      else if(fCameraType==VERITAS499)
	{         
	  // ***********************************************************
	  //Do we trigger? The internal pulse must be above offset and the
	  //original must be above threshold gCFDTriggerDelay (1.5 ns) later
	  // ************************************************************

	  bool aboveAbove=aboveThresholdAboveOffset(fPixel,offsetCrossingBin);
	  
	  if(!aboveAbove)
	    {
	      if(offsetCrossingBin>fLastThresholdCheckBin)
		{
		  return triggerFound;
		}
	      else
		{
		  offsetCrossingBin=offsetCrossingBin+fNumCFDDelayBins+1;
		}
	    }
	  else
	    {
	      // *********************************************************
	      //WE TRIGGER! Determine when and save the time
	      //j+fCFDTriggerDelayBins is trigger bin in fWaveForm. 
	      triggerFound=true;
	      double triggerTimeNS =(offsetCrossingBin+
				     fCFDTriggerDelayBins)*gWaveFormBinSizeNS +
		                     fPixel.fWaveFormStartNS;
	      fPixel.fCFDTriggerTimeNS.push_back( triggerTimeNS);
	      offsetCrossingBin=offsetCrossingBin-fCFDTriggerDelayBins+
		int(gPSTPulseWidthNS[fCameraType]/gWaveFormBinSizeNS);
	    }
	}
    }
 return triggerFound;
}

// *************************************************************************

void KSCFD::PrintWaveForm(int pixelID, int nx, int ny, int seqNum, 
			  double time, std::vector<double>& waveForm,
			  double waveFormStartNS)
// **************************************************************************
// Dump wave form to ouput in form easy to plot with root. Used for debugging.
// **************************************************************************
{

  int fNumBins=waveForm.size();
  for(int i=0;i<fNumBins;i++)
    {
      double fBinTime=waveFormStartNS+i*gWaveFormBinSizeNS;
      fMyFile<<nx<<" "<<ny<<" "<<seqNum<<" "<<pixelID<<" "<<fBinTime<<" "
	       <<waveForm.at(i)<<" "<<time<<std::endl;
    }
  return;
}
// ***************************************************************************
void KSCFD::PrintWaveForm(int pixelID, int nx, int ny, int seqNum, 
			  double time, KSWaveForm* pWaveForm,
			  double waveFormStartNS)
// **************************************************************************
// Dump wave form to ouput in form easy to plot with root. Used for debugging.
// **************************************************************************
{

  int fNumBins=pWaveForm->GetNumWaveFormBins();
  for(int i=0;i<fNumBins;i++)
    {
      double fBinTime=waveFormStartNS+i*gWaveFormBinSizeNS;
      fMyFile<<nx<<" "<<ny<<" "<<seqNum<<" "<<pixelID<<" "<<fBinTime<<" "
	       << pWaveForm->GetWaveFormElement(i)<<" "<<time<<std::endl;
    }
  return;
}
// ***************************************************************************

void KSCFD::makeInternalCFDWaveForm(KSPixel& fPixel,bool printWaveForms)
// *********************************************************************
// How CFD's work: WE are going to followe"Techniques for Nuclear and Particle
// physics Experiments", W.R Leo, PG 319. This is the way veritas and lecroy 
// CFDs work with an extra check needed for the veritas CFD's.
// WE ASSUME PULSES ARE POSITIVE GOING. THIS IS THE OPPOSITE OF REALITY.
// BUT WE ARE SMART GUYS AND CAN HANDLE A SIGN CHANGE NOW AND THEN!!!!!!!
// Internal to the CFD the input waveform is mainpulated by 
// 1: input signal is split into  waveforms.
// 2: First one is delayed by fCFDDelay
// 3: Second one is negated, and attenuated by fCFDFraction 
// 4: Then pulses are summed (called fCFDPulse below). 
// ***********************************************************************
{

  int fNumWaveFormBins=fPixel.fNumWaveFormBins;
  fNegativePulse.clear();
  fNegativePulse.resize(fNumWaveFormBins,0.0);

    // Attenuate and negate waveform(this is what CFD's do)
  for(int i=0;i<fNumWaveFormBins;i++)
    {
      fNegativePulse.at(i)=(-fPixel.pfWaveForm->GetWaveFormElement(i)*fCFDFraction);
    }

  // **************************************************************
  //Add delayed pulse to  Negated and attenuated Pulse 
  //Note; CFDPulse needs only to be as long as fWaveForm since we
  //      even though the delayed pulse extends after end of fWaveForm
  //      we can't use it there. 
  // ***************************************************************
  fCFDPulse.clear();
  //std::cout<<"fNumWaveFormBins: "<<fNumWaveFormBins<<std::endl;

  fCFDPulse.resize(fNumWaveFormBins,0.0);

  //Note that first fNumCFDDelayBins-1 bins are 0 in fCFDPulse.

  for(int i=fNumCFDDelayBins;i<fNumWaveFormBins;i++)
    {
      int j=i-fNumCFDDelayBins; //Pick up earlier value of oirginal pulse
                                //Thus its delayed. This is correct!!!!!
      fCFDPulse.at(i)=fNegativePulse.at(i)+fPixel.pfWaveForm->GetWaveFormElement(j);

    }
  if(printWaveForms)
    {
      fMyFile.open("CFDwaveforms.dat", std::ios::out);
      fMyFile<<"/I:y/I:seq/I:pix/I:t/F:p/F:time/F:"<<std::endl;
      double fThresholdTime=fPixel.fWaveFormStartNS;
      PrintWaveForm(fPixel.fID,0,0,1,fThresholdTime, 
		    fPixel.pfWaveForm,fPixel.fWaveFormStartNS);
      PrintWaveForm(fPixel.fID,0,0,2,fThresholdTime, 
		    fNegativePulse,fPixel.fWaveFormStartNS);
      PrintWaveForm(fPixel.fID,0,0,3,fThresholdTime, 
		    fCFDPulse,fPixel.fWaveFormStartNS);
      fMyFile.close();
    }
  return;
}
// *************************************************************************

bool KSCFD::findAboveThreshold(KSPixel& fPixel, int& thresholdCrossingBin)
// ****************************************************************
// Scan through the pixel wave form starting at threshold Crossing bin 
// until we find a point above threshold.
// *******************************************************************
{
  bool aboveThresholdFound=false;
  // ********************************************************************
  // find when we go above threshold
  // ***********************************************************************
  for(int i=thresholdCrossingBin;i<=fLastThresholdCheckBin;i++)
    {
      if (fPixel.pfWaveForm->GetWaveFormElement(i)>=fPixel.fThreshold)
	{
	  aboveThresholdFound=true;
	  thresholdCrossingBin=i;
	  return aboveThresholdFound;
	}
    }
  return false;
}
// **************************************************************************

bool KSCFD::findNextOffsetCrossing(int& thresholdCrossingBin)
// **************************************************************************
// Find when we make a positive corssing of the offset threshold in the CFD 
// pulse
// **************************************************************************
// Make sure we start below the offset.
// **************************************************************************
// We need to start looking -fNumCFDDelayBins before the threshold trigger 
// time. This is because we delayed the main pulse by that much so the
// threshold time is not where we should start but ealier.
// *************************************************************************
{
  bool belowOffset=false;
  int offsetCrossingBin=0;
  int firstCFDCrossingCheckBin=thresholdCrossingBin-fNumCFDDelayBins;
  if(firstCFDCrossingCheckBin<0)
    {
      firstCFDCrossingCheckBin=0;
    }

  for(int i=firstCFDCrossingCheckBin;i<fLastCFDCrossingCheckBin;i++)
    {
      if(fCFDPulse.at(i)<fCFDOffsetPE)
	{
	  belowOffset=true;
	  offsetCrossingBin=i;
	  break;
	}
    }
  if(!belowOffset)
    {
      return false;
    }
  // ************************************************************************
  // Now find when we go above the offset
  // ************************************************************************
  for(int i=offsetCrossingBin;i<fLastCFDCrossingCheckBin;i++)
    {
      if(fCFDPulse.at(i)>fCFDOffsetPE)
	{
	  thresholdCrossingBin=i;
	  return true;
	}
    }
  return false;
}
// ***************************************************************************

bool KSCFD::aboveThresholdAboveOffset(KSPixel& fPixel,
					   int& offsetCrossingBin)
// ****************************************************************
// Scan through the pixel wave and the CFDwave to find next time that the
// CFDWave is above offset and gCFDTriggerDelay later the original pulse is
// above threshold. Quit if we go below offset or reach the end of 
// the wave.
// *******************************************************************
{
  // *****************************************************************
  // Find first bin where we are above offset and gfCFDTriggerDelay later we 
  // are above threshol
  // *****************************************************************
  for(int i=offsetCrossingBin;i<=fLastThresholdCheckBin;i++)
    {
      if (fCFDPulse.at(i)<fCFDOffsetPE) //Quit if we go below offset
	{
	  offsetCrossingBin=i;
	  return false;
	}
      else if (fPixel.pfWaveForm->GetWaveFormElement(i+fCFDTriggerDelayBins)>=fPixel.fThreshold)
	{
	  offsetCrossingBin=i;
	  return true;
	}
    }
  offsetCrossingBin=fLastThresholdCheckBin+1;
  return false;
}
// *************************************************************************
