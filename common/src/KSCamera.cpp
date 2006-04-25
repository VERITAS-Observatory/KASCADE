/**
 * \class KSCamera
 * \ingroup common * \brief File of methods for KSCamera.
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include <iostream>

#include "KSCamera.h"
// In the constructor is where most of the work gets done.
KSCamera::KSCamera(KSCameraTypes CameraType, KSTeHeadData* pTeHead, 
		                                      bool fUsePatternTrigger)
{
  pfTeHead=pTeHead;

  fCameraType=CameraType;
  fLatitude=gLatitude[fCameraType];
  fEastLongitude=gEastLongitude[fCameraType];

  // ********************************************************************
  //	Calculate focal plane conversion factors. (meters/deg.)
  //	Note that focal plane is not linear . Its got that tan(theta) factor
  //	which for small angles is close to theta. Error is small.<1%,
  // ********************************************************************

  fFocalLengthM             = gFocalLengthM[fCameraType];
  fFacetDiameterM           = gFacetDiameterM[fCameraType];
  fMirrorRadiusSquared      = gMirrorRadiusSquared[fCameraType];
  fMetersPerDeg             = 1./(atan(1./fFocalLengthM)*gRad2Deg);

  //Jitter of normal is 1/2 focal plane jitter. Convert to radians also.
  fJitterWidthNorthSouthRad =(gSpotSizeNorthSouthDeg[fCameraType]/2.)* 
    gDeg2Rad;
  fJitterWidthEastWestRad   = (gSpotSizeEastWestDeg[fCameraType]/2.)* 
    gDeg2Rad;


  generateCameraPixels();

  loadPixelCharacteristics();    // Loads Efficiency,Threshold,SkyDiscNoise

  pfCameraTrigger = new KSCameraTrigger(pfTeHead,fUsePatternTrigger,&fPixel); 
                                                           //sets up pst.
  pfCFD = new KSCFD(fCameraType);

}
// ********************************************************************

KSCamera::~KSCamera()
{
  //nothing to do
}

void KSCamera::Print()
{
  std::cout<<"                        Telescope Focal Length(m) = "
	   <<fFocalLengthM<<std::endl;
  std::cout<<"      Focal plane conversion factor in meters/deg = "
	   <<fMetersPerDeg<<std::endl;
  std::cout<<"    Facet EastWest misalignment jitter angle(deg) = "
	   <<fJitterWidthEastWestRad*gRad2Deg<<std::endl;
  std::cout<<"  Facet NorthSouth misalignment jitter angle(deg) = "
           <<fJitterWidthNorthSouthRad*gRad2Deg<<std::endl;
  std::cout<<"                     Effective Mirror diameter(m) = "
	   <<2*sqrt(fMirrorRadiusSquared)<<std::endl;


  std::cout<<"Camera Initalized:"<<std::endl;
  std::cout<<"Pixel 1 light collection effciency = "<<fPixel[1].fEfficiency
	   <<std::endl;

  pfCameraTrigger->Print();
  return;
}
// ************************************************************************

// ***************************************************************************
void KSCamera::generateCameraPixels()
{
  //	Load up camera
  //	set  pmt number
  fNumPixels=gNumPixelsCamera[fCameraType];
  fNumPixelsTrigger =gNumTriggerPixels[fCameraType];
  // *************************************************************************
  // 	Get PMT locations and radii from WhippleCams.h
  // *************************************************************************
  KSPixel fPixelElem(fCameraType); //create a standard pixel to start with
  
  fPixel.resize(fNumPixels,fPixelElem);   //  Allocate pixels

  // *******************************************************************
  // Now load actual x,y positions of the tubes using the the arrays in 
  // WhippleCams.h In future we may have seperate arrays for different cameras
  // *******************************************************************
  if(fCameraType==WHIPPLE490)
    {
      pfTelescopePixelX=WC490Xcoord;
      pfTelescopePixelY=WC490Ycoord;
      pfTelescopePixelRadius=WC490Radius;
    }
  else if(fCameraType==VERITAS499)
    {
      pfTelescopePixelX=VC499Xcoord;
      pfTelescopePixelY=VC499Ycoord;
      pfTelescopePixelRadius=VC499Radius;
    }
  double fHalfSpacingDeg =
                        gPixelHalfSpacingMM[fCameraType]/(fMetersPerDeg*1000.);
  for(int i=0;i<fNumPixels;i++)
    {
      fPixel[i].fHalfSpacingDeg = fHalfSpacingDeg;
      fPixel[i].fRadiusDeg      = (double)pfTelescopePixelRadius[i];
      fPixel[i].fYDeg           = (double)pfTelescopePixelX[i];
      fPixel[i].fXDeg           = (double)pfTelescopePixelY[i];
    }


  // **********************************************************************
  // Now generate the Cell Grid for the pixel look-up tables. This is
  // used to find which pixels are hit by photons. 
  // **********************************************************************
  double fMaxFOVDeg;
  if(fCameraType==VERITAS499)
    {
      // **********************************************************************
      // For VERITAS cameras this is straight forward since all pixels (and 
      // light cones) are the same size;
      // *********************************************************************
      pfPixelGrid=new KSCellGrid(pfTelescopePixelX,pfTelescopePixelY,
			     pfTelescopePixelRadius,fNumPixels);
      fMaxFOVDeg=sqrt(pfPixelGrid->fMaxFOV2);
    }
  else if(fCameraType==WHIPPLE490)
    {
      // **********************************************************************
      // Not so simple since on the outside of the pmt array we have the 
      // larger -no light cone ring pixels
      // **********************************************************************
      // So plan for whipple is to test the inner camera as normal:
      int fNumInnerPixels=379;
      pfPixelGrid=new KSCellGrid(pfTelescopePixelX,pfTelescopePixelY,
			     pfTelescopePixelRadius,fNumInnerPixels);

      // Then test the outer rings specially
      // Max angle(in deg) of field of view(plus a little)
      double fRingRadiusDeg=(g490OuterRingDiameterM[2]/2.0)/fMetersPerDeg;
      double fRingSpacingDeg=sin(gDeg2Rad*g490OuterRingsAngularStepDeg/2.) * 
	                      (fRingRadiusDeg)*2;
      fMaxFOVDeg = (g490OuterRingDiameterM[2]/2)/fMetersPerDeg+ 
	                fRingSpacingDeg;
    }
  if(fMaxFOVDeg<1.0)
    {
      fMaxFOVDeg=1.0;
    } 
  fMaxFOVDeg2=fMaxFOVDeg*fMaxFOVDeg;   //Used For speed reasons later.
  fMinimumDnTight=cos(fMaxFOVDeg*gDeg2Rad);    //Used by tilt I think
  fMinimumDnLoose=cos((fMaxFOVDeg+1.0)*gDeg2Rad);
  std::cout<<"Tight Camera FOV radius: "<<fMaxFOVDeg<<" deg."<<std::endl;
  return;
}
// ***************************************************************************

 void KSCamera::loadPixelCharacteristics()
// ***********************************************************************
// Efficiency factor
// ***********************************************************************
// pfTeHead->fEfficiency: Accounts for dirty mirrors,dusty air and 
// any other linear reduction of number of photons that make pe's
// ***********************************************************************
// pfTeHead->fLightConeConcentration:Accounts for overall 
// innefficiency of light cones.It is assumed that the active area of the pmts
// (Ap=pi*pmt_radius**2) is 100% efficient(after application of 
// ffEfficiencyn.see above) and that the light on the remaining area
// of a pixel's hexagonal area is collected by the light cones which are
// fLightConeConcentration efficient. 
// Efficiency=BaseEfficiency*(x+lightconeconcentration*(1-x))
// where x=area ratio=area(pmt)/area(hex), pmt area=pi*r**2, 
// hex area=3*pmt_half_spacing**2. 
// =>area ratio=(pi*pmt_radius**2)/sqrt(3)*2*pmt_spacing/2)
//             = 0.9068996*(pmt_radius/pmt_half_spacing)**2 
// Thus total efficiency is fraction of area of pmt at 100% + fraction of
// left over hex area at fLightConeEff.
{
   double fBaseEfficiency  = pfTeHead->fEfficiency;
   double fLightConeEff    = pfTeHead->fLightConeConcentration;
   double fNoiseRate       = pfTeHead->fNoiseRate;
   double fDiscGateNS      = pfTeHead->fDiscCoincidenceWidthNS;;
   double fThreshold       = pfTeHead->fDiscriminatorThresholdPes;
   for(int i=0;i<fNumPixels;i++)
     {
       double fHalfSpc       = fPixel[i].fHalfSpacingDeg;
       double fRadSpace      = (fPixel[i].fRadiusDeg/fHalfSpc);
       double fFracPMTArea   = 0.9069*fRadSpace*fRadSpace;
       double fPixelEff      = (fFracPMTArea+fLightConeEff*(1.0-fFracPMTArea));
       double fEfficiency    = fBaseEfficiency*fPixelEff;
       fPixel[i].fEfficiency = fEfficiency;

       // *******************************************************************
       // Disc noise generation: Num of pe's in disc window from sky shine.
       // *******************************************************************
       double fPixNoiseRate = fNoiseRate*3*fHalfSpc*fHalfSpc*fEfficiency;
       fPixel[i].fNoiseRatePerNS=fPixNoiseRate;
       fPixel[i].fDiscNoise  = fPixNoiseRate*fDiscGateNS;
       fPixel[i].fThreshold= fThreshold;
     }

   // ********************************************************************
   // outer 111 pixels of WHIPPLE490 pixel camera have no light cones
   // ********************************************************************
   if(fCameraType==WHIPPLE490)
     {
       for(int i=379;i<490;i++)
	 {
	   fPixel[i].fEfficiency = fBaseEfficiency;
	   fPixel[i].fDiscNoise = fBaseEfficiency*fDiscGateNS*fNoiseRate*M_PI*
	                            fPixel[i].fRadiusDeg*fPixel[i].fRadiusDeg;
	 }
     }

  
   return;
}
// ***********************************************************************


void KSCamera::loadNoiseRatesAndPeds()
// *************************************************************************
// Using each pixels fNoiseRatePerNS and the relative pedvars, determine
// each pixel's adjusted night sky pedestal variences. Do this to model a 
// particular run. This adjustes the fNoiseRatePerNS. From this is the pedvars 
// for the pixel night sky Wave form is calculated.
// Also determine the pedvar for the charge window(we only have one as yet)
// in the FADC traces using simular methods. 
// *************************************************************************
// Note: The fPedVarRel have already been gain adjusted
// *************************************************************************
{
  // ***********************************************************************
  //1:Convert the input variences into noise
  //2:Find noise ratios to mean
  //3:Modifiy each pixels noise by these ratios
  int fNumGoodPixels=0;
  int fNum=fNumPixels;
  double fNoiseSum=0;
  if(fCameraType==WHIPPLE490)
    {
      fNum=379;  // do inner same size pixels seperatly for wwWHIPPLE490 camera
    }
  for(int i=0;i<fNum;i++)
    {
      if(!fPixel[i].fBadPixel)
	{
	  fNumGoodPixels++;
	  fPixel[i].fPedVarRel=fPixel[i].fPedVarRel*fPixel[i].fPedVarRel;
	  fNoiseSum+=fPixel[i].fPedVarRel;
	}
    }
  double fMeanNoise=fNoiseSum/fNumGoodPixels;
  for(int i=0;i<fNum;i++)
    {
      if(!fPixel[i].fBadPixel)
	{
	  fPixel[i].fNoiseRatePerNS=
	       fPixel[i].fNoiseRatePerNS*fPixel[i].fPedVarRel/fMeanNoise;
	}
    }
  // ***********************************************************************
  // Now do outer pmts of WHIPPLE490 camera
  // ***********************************************************************
  if(fCameraType==WHIPPLE490)
    {
      fNumGoodPixels=0;
      fNoiseSum=0;
      for(int i=379;i<fNumPixels;i++)
	{
	  if(!fPixel[i].fBadPixel)
	    {
	      fNumGoodPixels++;
	      fPixel[i].fPedVarRel=fPixel[i].fPedVarRel*fPixel[i].fPedVarRel;
	      fNoiseSum+=fPixel[i].fPedVarRel;
	    }
	}
      fMeanNoise=fNoiseSum/fNumGoodPixels;
      for(int i=379;i<fNumPixels;i++)
	{
	  if(!fPixel[i].fBadPixel)
	    {
	      fPixel[i].fNoiseRatePerNS=
		   fPixel[i].fNoiseRatePerNS*fPixel[i].fPedVarRel/fMeanNoise;
	    }
	}
    }
  // *************************************************************************
  // Now determine from fNoiseRatePerNS each pixels fNightSkyWaveFormPedestal
  // Add the Charge variance for the FADC/ADC Window
// **************************************************************************
  for(int i=0;i<fNumPixels;i++)
    {
      fPixel[i].DetermineNoisePedestals();
    }
  return;
}
// *************************************************************************


bool KSCamera::getPixelIndex(double fXDeg, double fYDeg, int& fIPix)
// ****************************************************************************
// 	Returns the pixel that a pe at fXDeg,fYDeg would hit.
// ****************************************************************************
{
   // *******************************************************************
  // 	Find distance of X,Y to center of field of view.
  // *******************************************************************
  double fDistance2=(fXDeg*fXDeg+fYDeg*fYDeg);
  if(fDistance2>fMaxFOVDeg2)
    {
      return true;  //Out of file-of-view , dump it
    }

  //Check if within field of view for CellGrid
  else  if(fDistance2<=pfPixelGrid->fMaxFOV2)
    {
      bool fKeep=pfPixelGrid->GetCellIndex(fXDeg,fYDeg,fIPix);
      if(fKeep)
	{
	  return false; //Good one Don't dump it
	}
    }
  
  // **************************************************************
  // Special test for outer rings
  // **************************************************************
  if(fCameraType==WHIPPLE490)         //Check Outer rings
    {                             
      bool fKeep=trywhipple490OuterPixels(fXDeg,fYDeg,fIPix);
      return !fKeep;  //Whatever, but dump flag
    }
  else
    {
      return true;   //Dump it;
    }
} 




bool KSCamera::trywhipple490OuterPixels(double fXDeg, double fYDeg, int& fIPix)
// *************************************************************************
// While this pe didn't hit the inner pixels of the whipple490 camera, see if
// it hits any of the pmts in the outer rings.
// *************************************************************************
// Note: No light cones outer rings!!  Makes life a little simpler.
{
  double fDistance = sqrt(fXDeg*fXDeg+fYDeg*fYDeg);
  double fPMTRadiusDeg=g490OuterPixelRadiusMM/(fMetersPerDeg*1000);
  // *************************************************************************
  // loop over rings.
  // *************************************************************************
  bool fKeep=false;
  for(int i=0;i<3;i++)
    {
      double fRingRadiusDeg=(g490OuterRingDiameterM[i]/2.)/fMetersPerDeg;
      if(fDistance>=fRingRadiusDeg-fPMTRadiusDeg &&
	 fDistance<=fRingRadiusDeg+fPMTRadiusDeg)
	{     //The pe hits within the ring. See if it hits a pmt.
	      // Dirty, not so quick.
	  int fPixelID=379+i*37;
	  double fPMTRadiusDegSquared=fPMTRadiusDeg*fPMTRadiusDeg;
	  for(int j=0;j<37;j++)
	    {
	      double fWDistanceSquared=pow((fPixel[fPixelID+j].fXDeg-fXDeg),2)+
		pow((fPixel[fPixelID+j].fYDeg-fYDeg),2);
	      if(fWDistanceSquared<=fPMTRadiusDegSquared)
		{
		  fIPix=fPixelID+j;	// Found a hit
		  fKeep=true;
		  return fKeep;
		}
	    }
	  if(!fKeep)
	    {
	      return fKeep;             // In a ring but No hit.
	    }
	}
      
    }
  return fKeep;
}
// **************************************************************************

void KSCamera::InitPixelImageData()
// **************************************************************************
// INitalize (set to zero) those parts of all pixels that vary image to image
// **************************************************************************
{
  for(int i=0;i<fNumPixels;i++)
    {
      fPixel[i].fDisc=0;               //Counts hits.
      fPixel[i].fDiscPulseHeight=0;
      fPixel[i].fDiscTrigger=false;        //This pixels fires
      fPixel[i].fTimePe.clear();
      fPixel[i].fCFDTriggerTimeNS=gOverflowTime;
    }
  return;
}
// ****************************************************************************

int KSCamera::buildTriggerWaveForms()
// ***************************************************************************
// This is for the complete waveform/cfd trigger. It follows the old 
// Veritas.cpp/PePulseVeritas. Its is adequate for both WHIPPLE490 and 
// VERITAS499
// ***************************************************************************
//Note: For speed reasons we only process those pixels necessary to make
//decion on whether to go on to next step or not.
{
  // ***********************************************************************
  // Build the pe pulse waveform arrays for only trigger pixels with real Pe's
  // in them: Add noise also. Then see if they trigger CFD's (and when)
  // ***********************************************************************
  findWaveFormLimits(fWaveFormStart,fWaveFormLength);

  int fCFDTriggers=0;
  double fStartTimeOffset=0;   // Start searching for CFD trigger at beginning
                               //  of fWaveForm;
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      if(fPixel[i].fTimePe.size()>0 && !fPixel[i].fBadPixel)
	{
	  fPixel[i].InitWaveForm(fWaveFormStart,fWaveFormLength);
	  fPixel[i].BuildPeWaveForm();
	  fPixel[i].AddNoiseToWaveForm(false);
	  fPixel[i].RemoveNightSkyPedestalFromWaveForm();
	  if(fPixel[i].fDisc>0)
	    {
	      bool fCFDTrig=pfCFD->isFired(fPixel[i],fStartTimeOffset);
	      if(fCFDTrig)
		{
		  fCFDTriggers++;
		}
	    }
	}
    }
  if(fCFDTriggers==0)
    {
      return fCFDTriggers;
    }
  
  // ********************************************************************
  // We have al least one pixel containing at least one real pe that fired
  // Now generate wave forms from sky shine for Trigger Pixels that don't
  // have any real pes in them and see if they trigger.
  // ******************************************************************** 
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      if(fPixel[i].fTimePe.size()==0)
	{
	  fPixel[i].InitWaveForm(fWaveFormStart,fWaveFormLength);
	  fPixel[i].AddNoiseToWaveForm(false);
	  fPixel[i].RemoveNightSkyPedestalFromWaveForm();
	  bool fCFDTrig=pfCFD->isFired(fPixel[i],fStartTimeOffset);
	  if(fCFDTrig)
	    {
	      fCFDTriggers++;
	    }
	}
    }
  return fCFDTriggers;
}
// ************************************************************************
       
void KSCamera::buildNonTriggerWaveForms()
// ************************************************************************
// At this point we build the rest of the waveforms. Kindof debatable from a
// stylistic argument that this code should go here but its ok.
// ************************************************************************
{
  for(int i=fNumPixelsTrigger;i<fNumPixels;i++)
    {
      fPixel[i].InitWaveForm(fWaveFormStart,fWaveFormLength);
      fPixel[i].AddNoiseToWaveForm(false);
    } 
}
// ************************************************************************

void KSCamera::findWaveFormLimits(double& fWaveFormStartNS,
				                   double& fWaveFormLengthNS)
// **********************************************************************
// Find when we should start the waveform and how long it should be.
// **********************************************************************
{

  double fPixelMinTimeNS=gOverflowTime;
  double fPixelMaxTimeNS=0;
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      int fNumPes=fPixel[i].fTimePe.size();
      if(fNumPes>0)
	{
	  for(int j=0;j<fNumPes;j++)
	    {
	      double fPeTime=fPixel[i].fTimePe[i];
	      if(fPeTime<fPixelMinTimeNS)
		{
		  fPixelMinTimeNS=fPeTime;
		}
	      if(fPeTime>fPixelMaxTimeNS)
		{
		  fPixelMaxTimeNS=fPeTime;
		}
	    }
	}
    }
  fWaveFormStartNS=fPixelMinTimeNS-kPSTPulseWidth;

  fPixelMaxTimeNS = fPixelMaxTimeNS 
                                 + gCFDDelayNS[fCameraType]
                                 + kCFDMinTimeAboveThresholdNS[fCameraType]  
                                 + fPixel[0].fSinglePeSizeNS
                                 + gFADCBinSizeNS*gFADCNumSamples[fCameraType] 
                                 - gFADCDelayNS;
  fWaveFormLength = fPixelMaxTimeNS-fWaveFormStartNS;
  return;
}
// ************************************************************************