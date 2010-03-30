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
extern "C" double gaussfast();

// In the constructor is where most of the work gets done.
// ********************************************************************

KSCameraTrigger::KSCameraTrigger(KSTeHeadData* pTeHead, bool UsePatternTrigger,
				 std::vector<KSPixel>* pPixel, 
				 int numPixelsInTrigger)
{
  pfTeHead=pTeHead;
  fUsePatternTrigger=UsePatternTrigger;
  pfPixel=pPixel;

  fCameraType=pfTeHead->fCameraType;
  fTriggerMultiplicity=pfTeHead->fTriggerMultiplicity;
 
  fNumPixelsTrigger=numPixelsInTrigger;


  pfPixelTriggerTime.resize(fNumPixelsTrigger);//For use with sort,Slide
  if(fUsePatternTrigger)
    {
      std::vector<double> nullDoubleVector;
      fCFDTriggerTimes.clear();
      fCFDTriggerTimes.resize(fNumPixelsTrigger,nullDoubleVector);
      fPatternTriggerLevel=pfTeHead->fPatternTriggerLevel;
      pfPST= new KSPST(fCameraType,fPatternTriggerLevel,fNumPixelsTrigger);
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
				// to gaussfast,NFluct and thus pran,RANLUX
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
	  
	  double fDNoise=fDiscNoise;
	  int fDN=NFluct(fDNoise);
	  //std::cout<<fDiscNoise<<" "<<fDN<<std::endl;
	  fDiscPes=fDiscPes+fDN;  //Add in sky noise
	    
	  // ***************************************************************
	  // Convert to a pulse height
	  // ***************************************************************
	  if(fDiscPes<=0)
	    {
	      double fDiscWidth =sqrt(gPulseHeightWidthFactor);
	      fDiscPulseHeight=gaussfast()*fDiscWidth;
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
	    {		               // to gaussfast,NFluct and thus pran,RANLUX
	      double fDiscNoise  = (double)pfPixel->at(i).fDiscNoise;
	      double fDiscPes   = NFluct(fDiscNoise);  //Add in sky noise
	      // Note that we do not correct for efficiency here. Already 
	      // done when we specified base noise rate.
	      if(fDiscPes<=0)
		{
		  double fDiscWidth =sqrt(gPulseHeightWidthFactor);
		  fDiscPulseHeight=gaussfast()*fDiscWidth;
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
	      fCFDTriggerTimes.at(i).clear();
	      if(pfPixel->at(i).fDiscTrigger)   //fDiscTrigger is a bool
		{
		  fCFDTriggerTimes.at(i).push_back(100.); 
		}
	      //	      else
	      //	{
	      //	  fCFDTriggerTimes.at(i).push_back(gOverflowTime); 
	      //	}
	    }

	  fPSTTrigger=pfPST->isTriggered(fCFDTriggerTimes,fPSTTriggerTimeNS);
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
  // Allow for mutiple hits on each channel. Use methods availabe in the KSPST
  // class
  // ***********************************************************************
  // For now only one hit is being looked for in each channel.
  // this will change almost imeadiatly
  // **************************************************
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      fCFDTriggerTimes.at(i).clear();
      if(pfPixel->at(i).fCFDTriggerTimeNS.size()>0)
	{
	  //std::cout<<" "<<i;
	  fCFDTriggerTimes.at(i)=pfPixel->at(i).fCFDTriggerTimeNS; 
	}
    }
  if(fCameraType==WHIPPLE490)
    {
      // ************************************************************
      // Build the CFD summed pulse wave (in a funny way)
      // ************************************************************
      pfPST->buildSummedCFD(fCFDTriggerTimes,
				    gTrigMultiplicityWidthNS);
      
      // ************************************************************
      // Find the first time that has required multiplcity. May be 
      // overflow time
      // ************************************************************
      pfPST->pfCFDTriggerChannels=NULL;  //Flag to not check for ignorable 
                                         //patterns.
      fMultiplicityTriggerTime = pfPST->findSummedCFDTriggerTime(
					     pfPST->fSummedCFDWaveFormStartNS,
					     fTriggerMultiplicity);
      if(fMultiplicityTriggerTime>=gOverflowTime)
	{
	  return false;
	}
    }
  // ***********************************************************************
  // For both Veritas and whipple , check the pst.
  // **********************************************************************
   bool fPSTTrigger=pfPST->isTriggered(fCFDTriggerTimes,fPSTTriggerTimeNS);

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
