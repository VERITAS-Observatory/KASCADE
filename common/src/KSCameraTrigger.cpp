/**
 * \class KSCameraTrigger
 * \ingroup common * \brief File of methods for KSCameraTrigger.
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include <iostream>
#include <cmath>

#include "KSCameraTrigger.h"
  
extern "C" float pran(float* fXDummy);
extern "C" int NFluct(double mean);
extern "C" double Gauss();

// In the constructor is where most of the work gets done.
// ********************************************************************

KSCameraTrigger::KSCameraTrigger(KSTeHeadData* pTeHead, bool UsePatternTrigger,
				 std::vector<KSPixel>* pPixel)
{
  pfTeHead=pTeHead;
  fUsePatternTrigger=UsePatternTrigger;
  pfPixel=pPixel;

  fCameraType=pfTeHead->fCameraType;
  fTriggerMultiplicity=pfTeHead->fTriggerMultiplicity;
  fNumPixelsTrigger=gNumTriggerPixels[fCameraType];

  pfPixelTriggerTime.resize(fNumPixelsTrigger);//For use with sort,Slide
  if(fUsePatternTrigger)
    {
      pfTimeTrigger= new double[fNumPixelsTrigger];
      fPatternTriggerLevel=pfTeHead->fPatternTriggerLevel;
      pfPST= new KSPST(fCameraType,fPatternTriggerLevel);
    }
}
// ********************************************************************

KSCameraTrigger::~KSCameraTrigger()
{
  //nothing to do
}
// ********************************************************************

void KSCameraTrigger::Print()
{
  std::cout<<"Number of inner pixels in trigger  ="<<fNumPixelsTrigger
	   <<std::endl;
  return;
}
// ************************************************************************


bool KSCameraTrigger::isFastTriggered()
//  ***********************************************************************
// Determins if the image held in the pixel vector for the present direction
// causes a trigger
// ************************************************************************
//	02/7/98 GHS
//		When determining width of guassian for disc, its a better
//		model to use a width of sqrt(phwf) when disc_pes=0 istead
//		of 0 (and not allowing <0 values)
//		The high tails work extremely well when tested against full
//		blown model using timing pulses on .25 ns steps etc.
//              Best found value for phwf is ~.135. This derived from use of
//              the pulse reconstruction code to model things and then 
//              determine phwf from the modeling results.
//This is the fast trigger! Not the pulse building one (waveform).
// ************************************************************************
{
// ***************************************************************************
// Find pulse heights in the pixels. Include noise if requested.
// ***************************************************************************
  double fDiscPulseHeight=0;
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      pfPixel->at(i).fDiscPulseHeight=0;
      int fDisc=pfPixel->at(i).fDisc;  //numper of pes this pixel, 
                                               //this direction
      // *********************************************************************
      // Use single_pe__height function to get pulse height fluctuations. For 
      // 0 channels use a  pulse height width factor.  Remove mean sky, its 
      // like a pesdistal
      if(fDisc>0)
	{                       // Do this for speed to reduce calls
				// to Gauss,NFluct and thus pran,RANLUX
				// Get total number(signal +noise) pes in each 
				// pixel. Use some tricky interger round down 
	                        // here to get last fraction of pe.
	  int fDiscPes=fDisc;
	  double fEfficiency  = pfPixel->at(i).fEfficiency;
	  double fDiscNoise  = (double)pfPixel->at(i).fDiscNoise;
	  if(fEfficiency<1.0)
	    {
	      for(int j=0;j<fDisc;j++)
		{
		  if(pran(&fXDummy)>fEfficiency)
		    {                         // REDUCE DISC
		      fDiscPes--; 
		    }
		} 
	    }
	  
	  double fDNoise=(float)fDiscNoise;
	  int fDN=NFluct(fDNoise);
	  //std::cout<<fDiscNoise<<" "<<fDN<<std::endl;
	  fDiscPes=fDiscPes+fDN;  //Add in sky noise
	    
	  // ***************************************************************
	  // Convert to a pulse height
	  // ***************************************************************
	  if(fDiscPes<=0)
	    {
	      double fDiscWidth =sqrt(gPulseHeightWidthFactor);
	      fDiscPulseHeight=Gauss()*fDiscWidth;
	      //below we set fDiscPulseHieght to 0 if it's <0
	    }
	  else
	    {
	      double fPulseHeight;
	      bool fAfterPulse=false;
	      fDiscPulseHeight=0;
	      for(int j=0;j<fDiscPes;j++)
		{
		  fPulseHeight=
		    pfPixel->at(i).pfSinglePe->getPulseHeight(fAfterPulse);
		  fDiscPulseHeight += fPulseHeight;
		}
	    }
						// remove Disc pedistal
	  fDiscPulseHeight=fDiscPulseHeight-fDiscNoise;
	  if(fDiscPulseHeight<0)
	    {
	      fDiscPulseHeight=0;
	    }
	  pfPixel->at(i).fDiscPulseHeight=fDiscPulseHeight;
	  //std::cout<<fDiscPes<<" "<<fDiscPulseHeight<<std::endl;

	}
    }
  // *********************************************************************
  // For speed: Test if enough discriminators fire for real shower pes to
  // Make event worth generating discPulse form noise only;
  // *********************************************************************

  int fPeHits=checkThreshold();
  bool fMultiplicityTrigger=false;//Multiplicity Trigger
  bool fPSTTrigger=true;          //PST Trigger

  if(fPeHits>0)
    {
      // ******************************************************************
      // add in pure noise hits(if any)
      // ******************************************************************
      for(int i=0;i<fNumPixelsTrigger;i++)
	{
	  int fDisc=pfPixel->at(i).fDisc;  //numper of pes this pixel, 
	  if(fDisc==0)                 // Do this for speed to reduce calls
	    {		               // to Gauss,NFluct and thus pran,RANLUX
	      double fDiscNoise  = (double)pfPixel->at(i).fDiscNoise;
	      double fDiscPes   = NFluct(fDiscNoise);  //Add in sky noise
	      // Note that we do not correct for efficiency here. Already 
	      // done when we specified base noise rate.
	      if(fDiscPes<=0)
		{
		  double fDiscWidth =sqrt(gPulseHeightWidthFactor);
		  fDiscPulseHeight=Gauss()*fDiscWidth;
		}
	      else
		{
		  fDiscPulseHeight=0;
		  double fPulseHeight;
		  bool fAfterPulse=true;
		  for(int j=0;j<fDiscPes;j++)
		    {
		      fPulseHeight=
			pfPixel->at(i).pfSinglePe->getPulseHeight(fAfterPulse);
		      fDiscPulseHeight += fPulseHeight;
		    }
		}
	  // remove Disc pedistal
	      fDiscPulseHeight=fDiscPulseHeight-fDiscNoise;
	      if(fDiscPulseHeight<0)
		{
		  fDiscPulseHeight=0;
		}
	      pfPixel->at(i).fDiscPulseHeight=fDiscPulseHeight;
	    }
	}
      // *******End Pixel Triggers********************************************


      // **********************************************************************
      // 		Multiplicity Trigger test
      // *********************************************************************
      fPeHits=checkThreshold();  //Recheck now that we have possible noise 
                                 //triggers

      if(fPeHits>=fTriggerMultiplicity)
	{
	  fMultiplicityTrigger=true;
	}

      // *********************************************************************
      // PST analysis
      // *********************************************************************
      if(fUsePatternTrigger && fMultiplicityTrigger) 
	{ // If we have limited ourselves to less then 331 pixels in 
	  // trigger they remaining pixels will have the TDC overflow
	  // value gOverflowTime and thus be ignored in w10m_pst_trigger.
	  // set up for pst trigger test.
	  for(int i=0;i<fNumPixelsTrigger;i++)
	    {
	      if(pfPixel->at(i).fDiscTrigger)
		{
		  pfTimeTrigger[i]=100.;// Put all trigger times at 100. ns.
		}
	      else
		{
		  pfTimeTrigger[i]=gOverflowTime; //Time overflow value
		}
	    }

	  fPSTTrigger=pfPST->isTriggered(pfTimeTrigger,fPSTTriggerTimeNS);
	}
    }

      // *******************************************************************
      // Determine if we have an event trigger 
      // *******************************************************************
  bool fImageTrigger=fPSTTrigger && fMultiplicityTrigger;
  return fImageTrigger;
}
// **************************************************************************

bool KSCameraTrigger::isWaveFormTriggered()
// *********************************************************************
// At this point the fPixel array has all the trigger times set up
// Look for triggers
// *********************************************************************
{
  // *********************************************************************
  // Note; WHIPPLE490 Does a overall Multiplicity coincidence for timing
  // reasons
  // *******************************************************************
 // **********************************************************************
  // Load up a trigger time array structure and test for Triggers
  //
  if(fCameraType==WHIPPLE490)
    {
      for(int i=0;i<fNumPixelsTrigger;i++)
	{
	  pfPixelTriggerTime[i].fTime=pfPixel->at(i).fCFDTriggerTimeNS; 
	  pfPixelTriggerTime[i].fIndex=i; 
	}
      // ****************************************************************
      // Order pixel times. Since we defined < operator for KSPixelTimes
      // this will for by fTime and bring the fIndex along. Tricky!
      // ****************************************************************
      std::sort(pfPixelTriggerTime.begin(),pfPixelTriggerTime.end());
					
      // ****************************************************************
      // Look for multiplicity trigger. Slide window returns with 100001. 
      // if no trigger
      // ****************************************************************
      fMultiplicityTriggerTime =pfPST->SlideWindow(pfPixelTriggerTime,
                              gTrigMultiplicityWidthNS,fTriggerMultiplicity);
      if(fMultiplicityTriggerTime>=gOverflowTime)
	{
	  return false;
	}
    }
  // (summed cfd pulses over multiplicity) for timing reasons.
  // we may want to add that at some time   
  // **********************************************************************
  // Load up a trigger time array and test for Triggers
  //
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      pfTimeTrigger[i]=pfPixel->at(i).fCFDTriggerTimeNS; 
    }
   bool fPSTTrigger=pfPST->isTriggered(pfTimeTrigger,fPSTTriggerTimeNS);

  return fPSTTrigger;
} 

// ***************************************************************************


int KSCameraTrigger::checkThreshold()
// **************************************************************************
//	Determine which pixel discrimaters fire.
// **************************************************************************{
{
  int fCount=0;
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      if(pfPixel->at(i).fDiscPulseHeight>=pfPixel->at(i).fThreshold)
	{
	  pfPixel->at(i).fDiscTrigger=true;
	  fCount++;
	}
      else
	{
	  pfPixel->at(i).fDiscTrigger=false;
	}
    }
  return fCount;
}
