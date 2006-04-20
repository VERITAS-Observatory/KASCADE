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
  fCFDMinTimeAboveThresholdBins =
           (int)(kCFDMinTimeAboveThresholdNS[fCameraType]/gWaveFormBinSizeNS);
}
// ************************************************************************

KSCFD::~KSCFD()
{
  // Nothing here
}
// ************************************************************************


bool KSCFD::isFired(KSPixel& fPixel, double fStartTimeOffsetNS)
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
//   stay above threshold for a minimum time of fMinTimeAboveThresholdNS. 
// ---------------------------------------------------------------------------
// fCFDDelay is determined normaly by the expected rise time of the pulse.
// ---------------------------------------------------------------------------

{
  int fNumWaveFormBins=fPixel.fNumWaveFormBins;
  fPixel.fCFDTriggerTimeNS=gOverflowTime;             //Default is no trigger
  int fThresholdIndex;                       //Find if we go over threshold.
                                             //Get Bin to start searching at.
  int fStartIndex=(int)(fStartTimeOffsetNS/gWaveFormBinSizeNS);
  if(fStartIndex>fNumWaveFormBins-1)
    {                
      return false;    // no trigger
    }
  
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
  for(fThresholdIndex=fStartIndex;fThresholdIndex<fNumWaveFormBins;
      fThresholdIndex++)
    {
      if(fPixel.fWaveForm[fThresholdIndex]>=fPixel.fThreshold)
	{
	  

          fNegativePulse.clear();
	  fNegativePulse.resize(fNumWaveFormBins,0);
	  
               // Amplify and negate waveform(this is what CFD's do)
	  for(int i=0;i<fNumWaveFormBins;i++)
	    {
	      fNegativePulse[i]=(-fPixel.fWaveForm[i]*fCFDGain);
	    }

	  // **************************************************************
	  //Add delayed, Negated and amplified fPulse to original
	  //Note; Main pulse needs only to be as long as fWaveForm since we
	  //      even though the delayed pulse extends after end of fWaveForm
	  //      we can't use it there. 
	  // Some time we can come back and do this all within one loop for
	  // efficiency(we can probalby get rid of use of the fNegativePulse 
	  // vector)
	  // ***************************************************************
          fMainPulse.clear();
	  fMainPulse.resize(fNumWaveFormBins,0);
	  for(int i=0;i<fNumWaveFormBins;i++)
	    {
	      int j=i+fNumCFDDelayBins;
	      fMainPulse[i]=fPixel.fWaveForm[i]+fNegativePulse[j];
	    }

	  // *****************************************************************
	  // Now that the MainPulse is constructed we can check to find the 
	  // positive to neg transition through the offset level (0 for 
	  // WHIPPLE490) level of fMainPulse. We have to make sure first that 
	  // if we start in a fired state, we need to look for the neg to pos 
	  // transition through the offset level
	  // *****************************************************************
	  bool fCFDFired=false;  //Flag that cfd has fired and has not yet
                                 //reset(gone positive)
	  if(fMainPulse[fStartIndex]<fCFDOffsetPE)  //See if we start fired.
	    {
	      fCFDFired=true; //set flag.
            }
	                  //look for offset crossing.
	  for(int j=fStartIndex+1;
	      j<fNumWaveFormBins-1-fCFDMinTimeAboveThresholdBins;j++)
	    {
	      if(fCFDFired)
		{
		  if(fMainPulse[j]>fCFDOffsetPE)//neg->positive crossing.reset
		    fCFDFired=false;
		}
	      else if(fMainPulse[j]<=fCFDOffsetPE)//pos->neg crossing. 
		{                              //Do we trigger. Check disc
		  fCFDFired=true;
		  if(fPixel.fWaveForm[j+fCFDMinTimeAboveThresholdBins]
		     >=fPixel.fThreshold) 
		    {           //WE TRIGGER! Determine when and return
		                //j+fCFDMinTimeAboveThresholdBins is trigger 
			        //bin in fWaveForm. 
		      fPixel.fCFDTriggerTimeNS =
			(j+fCFDMinTimeAboveThresholdBins)*gWaveFormBinSizeNS +
			                           fPixel.fWaveFormStartNS;
		      return true;
		    }
		}
	      
	    }
	}
    }
  return false;
}
// *************************************************************************


