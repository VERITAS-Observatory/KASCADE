/**
 * \class KSCFD
 * \ingroup common
 * \brief File of methods for KSCFD.
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#include <iostream>
#include "KSCFD.h"


KSCFD::KSCFD(KSCameraTypes CameraType)
{
  fCameraType=CameraType;
  fNumCFDDelayBins              = (int)(gCFDDelayNS[fCameraType]/
					gWaveFormBinSizeNS);
  fCFDGain                      = kCFDGain[fCameraType];
  fCFDOffsetPE                  = kCFDOffsetPE[fCameraType];
  fCFDTriggerDelayBins =
                    (int)(gCFDTriggerDelayNS[fCameraType]/gWaveFormBinSizeNS);
}
// ************************************************************************

KSCFD::~KSCFD()
{
  // Nothing here
}
// ************************************************************************


bool KSCFD::isFired(KSPixel& fPixel, double fStartTimeOffsetNS,
		    double fWaveFormLastThresholdNS, int nx, int ny)
// ***************************************************************************
// Test to see if this pixel's waveform fires its CFD
// ***************************************************************************
// Note: Units of fPixel.fWaveform are PE's
// ***************************************************************************
// We are going to try and do both WHIPPLE490 and VERITAS499 CFD's here in the
// same code
// ***************************************************************************
// Model CFD on waveform. Sets in fPixel.fCFDTriggerTimeNS (in NS) time of 
// trigger (see below) or to fOverflowTime time if no trigger found.
// ************************************************************************
// ---------------------------------------------------------------------------
// The purpose of the CFD (Constant Fraction Discriminator) is to find the time
// an acceptably high enough pulse (one that goes above fThreshold) reaches a 
// certain fraction of its maximum height. That fraction is set by the 
// fCFDGain. (Trigger fraction is 1/fCFDGain).
// The VERITAS CFD discriminator has the added requirment that the pulse must 
// stay above fThreshold for a minimum time of fMinTimeAboveThresholdNS. 
// ---------------------------------------------------------------------------
// How CFD's work: Internal to the CFD the input waveform is mainpulated by 
// 1: Delaying(by fCFDDelay), negating , and amplifying (by fCFDGain) the 
//    input pulse.
// 2:This is then added back to the original pulse to  from  the internal
//   CFD pulse (called MainPulse below).
// 3:The original input pulse is then searched for going above threshold
// 4:Once it is found to be above threshold the positive to negative
//   tranisition across the fCFDOffsetPe level is looked for in the internal 
//   CFD pulse (MainPulse, see above) (WHIPPLE CFD's use fCFDOffsetPe=0)
// 5:The time of that transition is the CFD triggerTime.
// 6:VERITAS CFD's have the  added requirment that the original pulse must
//   still be above threshold gCFDTriggerDelayNS after 0 crossing time.

// ---------------------------------------------------------------------------
// fCFDDelay is determined normaly by the expected rise time of the pulse.
// ---------------------------------------------------------------------------

{
  bool fPrintWaveForms=false;
  if(nx==0 && ny==0)
    {
      fPrintWaveForms=true;
    }
 
  int fNumWaveFormBins=fPixel.fNumWaveFormBins;


  fPixel.fCFDTriggerTimeNS=gOverflowTime;             //Default is no trigger
  int fThresholdIndex;                       //Find if we go over threshold.
                                             //Get Bin to start searching at.
  int fStartIndex=(int)(fStartTimeOffsetNS/gWaveFormBinSizeNS);
  if(fStartIndex>fNumWaveFormBins-1)
    {                
      return false;    // no trigger
    }
  int fLastThresholdCheckBin=(int)((fWaveFormLastThresholdNS-
                                fPixel.fWaveFormStartNS)/ gWaveFormBinSizeNS);
  // *****************************************************************
  // Stop looking so that we have enough room to make a FADC trace.
  // Its probably possible to shorten this a bit more but this will 
  // work
  // *****************************************************************
  int fLastCFDCrossingCheckBin=fNumWaveFormBins - 1 - fCFDTriggerDelayBins
     - (int)(gFADCBinSizeNS*gFADCNumSamples[fCameraType]/gWaveFormBinSizeNS)
     + (int)(gFADCTOffsetNS[fCameraType]/gWaveFormBinSizeNS);

  // *********************************************************************
  // Note: Since we have to look for a transision of the summed original
  //       waveform and the delayed negated amplified waveform we don't need 
  //       to look for threshold before the delay value
  // *********************************************************************
  if(fStartIndex<fNumCFDDelayBins)            //Force minimum.
    {
      fStartIndex=fNumCFDDelayBins;  
    }                                
  
  //Now look for when we cross the threshold
  // ************************************************************************
  // So where do we stop looking? At the very end of any possible real pulse+ 
  // the PST Gate  width.
  //for(fThresholdIndex=fStartIndex;
  //    fThresholdIndex<fNumWaveFormBins-fCFDTriggerDelayBins-1;
  //    fThresholdIndex++)
  for(fThresholdIndex=fStartIndex; fThresholdIndex<=fLastThresholdCheckBin;
      fThresholdIndex++)
    {
      if(fPixel.fWaveForm.at(fThresholdIndex)>=fPixel.fThreshold)
	{
	  // *********************************************************
	  // Ok we are over threshold. Form CFD internal pulse and look
	  // for 0 (or CFDoffset crossing. any time to end of window.
	  // We should probably be more careful here for how long we look
	  // *********************************************************
          fNegativePulse.clear();
	  fNegativePulse.resize(fNumWaveFormBins,0);
	  
               // Amplify and negate waveform(this is what CFD's do)
	  for(int i=0;i<fNumWaveFormBins;i++)
	    {
	      fNegativePulse.at(i)=(-fPixel.fWaveForm.at(i)*fCFDGain);
	    }
	  //if(fPrintWaveForms)
	  // {
	  //   double fThresholdTime=gWaveFormBinSizeNS*fThresholdIndex+
	  //	fPixel.fWaveFormStartNS;
	  //   double fNegStartTime=fPixel.fWaveFormStartNS+
	  //	                         fNumCFDDelayBins*gWaveFormBinSizeNS;
	  //   PrintWaveForm(fPixel.fID,nx,ny,3,fThresholdTime, 
	  //		      fNegativePulse,fNegStartTime);
	  // }
	  // **************************************************************
	  //Add delayed, Negated and amplified fPulse to original
	  //Note; Main pulse needs only to be as long as fWaveForm since we
	  //      even though the delayed pulse extends after end of fWaveForm
	  //      we can't use it there. 
	  // ***************************************************************
          fMainPulse.clear();
	  fMainPulse.resize(fNumWaveFormBins,0);
	  for(int i=fNumCFDDelayBins;i<fNumWaveFormBins;i++)
	    {
	      int j=i-fNumCFDDelayBins;
	      fMainPulse.at(i)=fPixel.fWaveForm.at(i)+fNegativePulse.at(j);
	    }

	  // *****************************************************************
	  // Now that the MainPulse is constructed we can check to find the 
	  // positive to neg transition through the offset level (0 for 
	  // WHIPPLE490) level of fMainPulse. We have to make sure first that 
	  // if we start in a fired state, we need to look for the neg to pos 
	  // transition through the offset level
	  // *****************************************************************
	  //Start looking for offset crossing:AFTER THRESHOLD REACHED!.
	  // *****************************************************************
	  // Stop looking so that we have enough room to make a FADC trace.
	  // Its probably possible to shorten this a bit more but this will 
	  // work
	  // *****************************************************************

	  bool fCFDFired=false;  //Flag that cfd has fired and has not yet
                                 //reset(gone positive)
	  if(fMainPulse.at(fThresholdIndex)<fCFDOffsetPE)
	                                               //See if we start fired.
	    {
	      fCFDFired=true; //set flag.
            }

	  // **************************************************************
	  // Now we look for the crossing which is a trigger.
	  // **************************************************************
	  // *****************************************************************
	  // Stop looking so that we have enough room to make a FADC trace.
	  // Its probably possible to shorten this a bit more but this will 
	  // work
	  // *****************************************************************
	  for(int j=fThresholdIndex+1;j<fLastCFDCrossingCheckBin;j++)
	    {
	      if(fCFDFired)
		{
		  if(fMainPulse.at(j)>fCFDOffsetPE)
		                                 //neg->positive crossing.reset
		    fCFDFired=false;
		}
	      else if(fMainPulse.at(j)<=fCFDOffsetPE)//pos->neg crossing. 
		{ 
		  fCFDFired=true;
		  // **************************
		  // WHIPPLE490
		  // **************************
		  if(fCameraType==WHIPPLE490)  //No other requirements for 
		    {                          //whipple
		      fPixel.fCFDTriggerTimeNS =
			j*gWaveFormBinSizeNS +fPixel.fWaveFormStartNS;
		      //if(fPrintWaveForms)
		      //	{
		      //	  PrintWaveForm(fPixel.fID,nx,ny,4,
		      //	fPixel.fCFDTriggerTimeNS,fMainPulse,
		      //			fPixel.fWaveFormStartNS);
		      //	}
		      return true;
		    } 
		  // *************************
		  // VERITAS499
		  // *************************
		  else if(fCameraType==VERITAS499)
		    {                      //Do we trigger. Check disc
		      if(fPixel.fWaveForm.at(j+fCFDTriggerDelayBins)
			 >=fPixel.fThreshold) 
			{           //WE TRIGGER! Determine when and return
			  //j+fCFDTriggerDelayBins is trigger 
			  //bin in fWaveForm. 
			  fPixel.fCFDTriggerTimeNS =
			    (j+fCFDTriggerDelayBins)*gWaveFormBinSizeNS +
			    fPixel.fWaveFormStartNS;
			  //if(fPrintWaveForms)
			  // {
			  //   PrintWaveForm(fPixel.fID,nx,ny,4,
			  //		    fPixel.fCFDTriggerTimeNS,
			  //		    fMainPulse,
			  //		    fPixel.fWaveFormStartNS);
			  // }
			  return true;
			}
		    }
		}
	    }
	  //if(fPrintWaveForms)
	  // {
	  //   PrintWaveForm(fPixel.fID,nx,ny,4,0.0,fMainPulse,
	  //		    fPixel.fWaveFormStartNS);
	  // }
	  // ***********************************************************
	  // Note that we don't find a trigger we don't keep trying to see if 
	  // reach threshold again. Someday add that.
	  // ***********************************************************
	  return false;
	}
    }
  return false;
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
      std::cout<<nx<<" "<<ny<<" "<<seqNum<<" "<<pixelID<<" "<<fBinTime<<" "
	       <<waveForm.at(i)<<" "<<time<<std::endl;
    }
  return;
}
// ***************************************************************************

