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

extern "C" double Gauss();

// In the constructor is where most of the work gets done.
// ************************************************************************

KSCamera::KSCamera(KSCameraTypes CameraType, KSTeHeadData* pTeHead, 
		   bool fUsePatternTrigger, double DigCntsPePE,
		   double NoiseRateSigma, double PSFNSDeg, double PSFEWDeg)
// ************************************************************************
// Used by KSEvent and KSArea
// ************************************************************************{
{
  fCameraType=CameraType;
  fDigCntsPerPEHiGain=DigCntsPePE;
  fNoiseRateSigma=NoiseRateSigma;
  InitCamera(pTeHead,fUsePatternTrigger);


  //Jitter of normal is 1/2 focal plane jitter. Convert to radians also.
  if(PSFNSDeg<0.0)
    {
      fJitterWidthNorthSouthRad =(gPSFNorthSouthDeg[fCameraType]/2.)*gDeg2Rad;
    }
  else
    {
      fJitterWidthNorthSouthRad =(PSFNSDeg/2.)*gDeg2Rad;
    }

  if(PSFEWDeg<0.0)
    {
      fJitterWidthEastWestRad   = (gPSFEastWestDeg[fCameraType]/2.)*gDeg2Rad;
    }
  else
    {
      fJitterWidthEastWestRad   = (PSFEWDeg/2.)* gDeg2Rad;
    }

}
// ************************************************************************

//KSCamera::KSCamera(KSCameraTypes CameraType, KSTeHeadData* pTeHead, 
//		   bool fUsePatternTrigger, double DigCntsPePE)
//{
//  fCameraType=CameraType;
//  fNoiseRateSigma=0.0;
//  fDigCntsPerPEHiGain=DigCntsPePE;
//  InitCamera(pTeHead,fUsePatternTrigger);
//}
// ************************************************************************

//KSCamera::KSCamera(KSCameraTypes CameraType, KSTeHeadData* pTeHead, 
//		                                      bool fUsePatternTrigger)
// *************************************************************************
// Used by KSArea
// *************************************************************************
//{
//  fCameraType=CameraType;
//  fNoiseRateSigma=0.0;
//  fDigCntsPerPEHiGain=gFADCDigCntsPerPEHiGain[fCameraType];
//  InitCamera(pTeHead,fUsePatternTrigger);
//}
// ************************************************************************

void KSCamera::InitCamera(KSTeHeadData* pTeHead, bool fUsePatternTrigger)
{
  pfTeHead=pTeHead;

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



  generateCameraPixels();

  loadPixelCharacteristics();    // Loads Efficiency,Threshold,SkyDiscNoise

  pfCameraTrigger = new KSCameraTrigger(pfTeHead,fUsePatternTrigger,&fPixel); 
                                                           //sets up pst.
  pfCFD = new KSCFD(fCameraType);

  // ************************************************************************
  //	Make up another  vector of  pixels, fPedPixels, for doing pedestal 
  //    event calculations.  These are seperate so we don't mess up waveforms
  //    when we generate pedestal events.  All we want is a place to keep the 
  //    waveforms, anything else is already set up in fPixels
  // ************************************************************************
  KSPixel fPixelElem(fCameraType,fDigCntsPerPEHiGain); //create a standard 
                                                     //pixel to start with
  int fPedPixelsSize=fPixel.size();
  fPedPixels.clear();
  fPedPixels.resize(fPedPixelsSize,fPixelElem);//Allocate pixels
  // Some stuff gets filled in in loadNoiseRatesAndPeds
  return;
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
  std::cout<<"         Pixel 1 light Total collection effciency = "
	   <<fPixel.at(0).fEfficiency<<std::endl;
  std::cout<<"    Pixel 1 Total Light Cone collection effciency = "
	   <<fPixel.at(0).fEfficiency/fPixel.at(0).fBaseEfficiency<<std::endl;
  std::cout<<"            Pixel 1 Single Pe Mean FADC Area (DC) = "
	   <<fPixel.at(0).fSinglePeMeanFADCArea<<std::endl;

  pfCameraTrigger->Print();

  std::cout<<"                  Pixel 1 Single Pe Rise Time(ns) = "
	   <<fPixel.at(0).pfSinglePe->getRiseTimeNS()<<std::endl;
  std::cout<<"                  Pixel 1 Single Pe Fall Time(ns) = "
	   <<fPixel.at(0).pfSinglePe->getFallTimeNS()<<std::endl;


  //fPixel.at(0).pfSinglePe->PrintSinglePe();

//fPixel.at(0).PrintPulseHeightsOfLightPulse();

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
  KSPixel fPixelElem(fCameraType,fDigCntsPerPEHiGain); //create a standard 
                                                     //pixel to start with
  fPixel.clear();
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
      fPixel.at(i).fID=i;  //Set pixel index ID's
      fPixel.at(i).fHalfSpacingDeg = fHalfSpacingDeg;
      fPixel.at(i).fRadiusDeg      = (double)pfTelescopePixelRadius[i];
      fPixel.at(i).fXDeg           = (double)pfTelescopePixelX[i];
      fPixel.at(i).fYDeg           = (double)pfTelescopePixelY[i];
    }


  // **********************************************************************
  // Now generate the Cell Grid for the pixel look-up tables. This is
  // used to find which pixels are hit by photons. 
  // **********************************************************************
  double fMaxFOVDeg=10;

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
// hex area=sqrt(3)*2*pmt_half_spacing**2. 
// =>area ratio=(pi*pmt_radius**2)/sqrt(3)*2*pmt_spacing**2)
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
       double fHalfSpc       = fPixel.at(i).fHalfSpacingDeg;
       double fActiveCathodeRadiusDeg =
                  gPixelActiveCathRadiusMM[fCameraType]/(fMetersPerDeg*1000.);
       double fRadSpace =fActiveCathodeRadiusDeg/fHalfSpc;
       //double fRadSpace      = (fPixel.at(i).fRadiusDeg/fHalfSpc);
       double fFracPMTArea   = 0.9069*fRadSpace*fRadSpace;
       double fPixelEff      = (fFracPMTArea+fLightConeEff*(1.0-fFracPMTArea));
       double fEfficiency    = fBaseEfficiency*fPixelEff;
       fPixel.at(i).fBaseEfficiency = fBaseEfficiency;
       fPixel.at(i).fEfficiency = fEfficiency;

       // *******************************************************************
       // Noise generation: Num of pe's in disc window from sky shine.
       // *******************************************************************
       double fNoise=fNoiseRate;
       if(fNoiseRateSigma>0.0)   //Jitter noise rate if requested.
	 {
	   fNoise=fNoiseRate+Gauss()*fNoiseRateSigma;
	 }

       double fPixNoiseRate=fNoise*                   //Base Noise rate
	                    (sqrt(3)*2*fHalfSpc*fHalfSpc) //Hexagon pixel area
	                    *fPixelEff;                   //Lightcone eff.

       fPixel.at(i).fNoiseRatePerNS=fPixNoiseRate;
       fPixel.at(i).fDiscNoise  = fPixNoiseRate*fDiscGateNS;
       fPixel.at(i).fThreshold= fThreshold;
     }

   // ********************************************************************
   // outer 111 pixels of WHIPPLE490 pixel camera have no light cones
   // ********************************************************************
   if(fCameraType==WHIPPLE490)
     {//Note no variance for noise rate
       for(int i=379;i<490;i++)
	 {
	   fPixel.at(i).fBaseEfficiency = fBaseEfficiency;
	   fPixel.at(i).fEfficiency = fBaseEfficiency;
	   fPixel.at(i).fDiscNoise = fDiscGateNS*fNoiseRate*M_PI*
	                            fPixel.at(i).fRadiusDeg*fPixel.at(i).fRadiusDeg;
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
      if(!fPixel.at(i).fBadPixel)
	{
	  fNumGoodPixels++;
	  fPixel.at(i).fPedVarRel=fPixel.at(i).fPedVarRel*fPixel.at(i).fPedVarRel;
	  fNoiseSum+=fPixel.at(i).fPedVarRel;
	}
    }
  double fMeanNoise=fNoiseSum/fNumGoodPixels;
  for(int i=0;i<fNum;i++)
    {
      if(!fPixel.at(i).fBadPixel)
	{
	  fPixel.at(i).fNoiseRatePerNS=
	       fPixel.at(i).fNoiseRatePerNS*fPixel.at(i).fPedVarRel/fMeanNoise;
	}
      else
	{
	  fPixel.at(i).fNoiseRatePerNS=0.0;
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
	  if(!fPixel.at(i).fBadPixel)
	    {
	      fNumGoodPixels++;
	      fPixel.at(i).fPedVarRel=fPixel.at(i).fPedVarRel*fPixel.at(i).fPedVarRel;
	      fNoiseSum+=fPixel.at(i).fPedVarRel;
	    }
	}
      fMeanNoise=fNoiseSum/fNumGoodPixels;
      for(int i=379;i<fNumPixels;i++)
	{
	  if(!fPixel.at(i).fBadPixel)
	    {
	      fPixel.at(i).fNoiseRatePerNS=
		   fPixel.at(i).fNoiseRatePerNS*fPixel.at(i).fPedVarRel/fMeanNoise;
	    }
	  else
	    {
	      fPixel.at(i).fNoiseRatePerNS=0.0;
	    }
	}
    }
  // *************************************************************************
  // Now determine from fNoiseRatePerNS each pixels fNightSkyWaveFormPedestal
  // Add the Charge variance for the FADC/ADC Window
// **************************************************************************
  for(int i=0;i<fNumPixels;i++)
    {
      if(!fPixel.at(i).fBadPixel)
	{
	  fPixel.at(i).DetermineNoisePedestals();
	}
      else
	{
	  fPixel.at(i).fWaveFormNightSkyPedestal=0.0;
	  fPixel.at(i).fChargeVarPE=0.0;
	  fPixel.at(i).fChargeVarDC=0.0;
	  fPixel.at(i).fPedDC=0.0;
	}
      // Fill in stuff used to make ped traces
      fPedPixels.at(i).fNoiseRatePerNS = fPixel.at(i).fNoiseRatePerNS;
      fPedPixels.at(i).fWaveFormNightSkyPedestal = 
	                                   fPixel.at(i).fWaveFormNightSkyPedestal;
      fPedPixels.at(i).fChargeVarPE    = fPixel.at(i).fChargeVarPE;
      fPedPixels.at(i).fChargeVarDC    = fPixel.at(i).fChargeVarDC;
      fPedPixels.at(i).fPedDC          = fPixel.at(i).fPedDC;
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
	      double fWDistanceSquared=
		pow((fPixel.at(fPixelID+j).fXDeg- fXDeg),2)+
		pow((fPixel.at(fPixelID+j).fYDeg-fYDeg),2);
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
// Called in KSEvent::BuildImage
// **************************************************************************
// Note: this DOES NOT reset fBadPixel!!!
// **************************************************************************
{
  for(int i=0;i<fNumPixels;i++)
    {
      fPixel.at(i).fDisc=0;               //Counts hits.
      fPixel.at(i).fDiscPulseHeight=0;
      fPixel.at(i).fDiscTrigger=false;        //This pixels fires
      fPixel.at(i).fTimePe.clear();
      fPixel.at(i).fCFDTriggerTimeNS.clear();
    }
  return;
}
// ****************************************************************************

int KSCamera::buildTriggerWaveForms(int nx, int ny)
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
 
  int numCFDTriggers=0;
  int numCFDNoiseTriggers=0;
  int numCFDRealTriggers=0;
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      // **************************************************************
      // This check of fBadPixel leaves the fCFDTriggerTimeNS=gOverflowTimeNS
      // for bad pixels (set in BuildImage by call to InitPixelImageData)
      // ***********************************************************
      if(fPixel.at(i).fTimePe.size()>0 && !fPixel.at(i).fBadPixel)
	{
	  fPixel.at(i).InitWaveForm(fWaveFormStart,fWaveFormLength);
	  fPixel.at(i).BuildPeWaveForm();
	  //if(fPrintWaveForm)fPixel.at(i).PrintWaveForm(nx,ny,1,0.0);

	  fPixel.at(i).AddNoiseToWaveForm(false);//Note that this noise has not 
	                                      //been modified by overall 
	                                      //efficiency but has been 
	                                      //modified by light cone 
	                                      //efficiency

	  // Remove the night sky pedestal. PMTs Capacitivly coupled
	  fPixel.at(i).RemovePedestalFromWaveForm(
				      fPixel.at(i).fWaveFormNightSkyPedestal);
	  // ********************************************************
	  // Always check to see if this waveform triggered even if,
	  // due to the efficiency cut in BuildPeWaveForm, it didn't have any 
	  // reall pes in it.We might trigger on noise alone.
	  // ***************************************************************
	  bool CFDTrig=pfCFD->isFired(fPixel.at(i),fStartTimeOffsetNS,
					   fLastTimeOffsetNS,
					   fLastCFDCrossingNS,nx,ny);

	  if(CFDTrig)
	    {
	      if(fPixel.at(i).fDisc>0)  //Check that we had a real pe in the 
		                        //mix
		{
		  numCFDTriggers++;
		}
	      else
		{
		  numCFDNoiseTriggers++; //Nope!Pure noise trigger(after 
		}                       //efficiency cut)
	    }
	}
    }
  if(numCFDTriggers==0)
    {
      return numCFDTriggers;
    }
  // ********************************************************************
  // We have al least one pixel containing at least one real pe that fired
  // Now generate wave forms from sky shine for Trigger Pixels that don't
  // have any real pes in them(before efficiency cut) and see if they trigger.
  // ******************************************************************** 
  numCFDRealTriggers=numCFDTriggers;
  numCFDTriggers+=numCFDNoiseTriggers;
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      // **************************************************************
      // This check of fBadPixel leaves the fCFDTriggerTimeNS.size()=0
      // For bad pixels (set in BuildImage by call to InitPixelImageData)
      // ***********************************************************
      if(fPixel.at(i).fTimePe.size()==0 && !fPixel.at(i).fBadPixel)
	{
	  fPixel.at(i).InitWaveForm(fWaveFormStart,fWaveFormLength);
	  fPixel.at(i).AddNoiseToWaveForm(false);
                                              //Note that this noise has not 
	                                      //been modified by overall 
	                                      //efficiency but has been 
	                                      //modified by light cone 
	                                      //efficiency

	  // Remove the night sky pedestal. PMTs Capacitivly coupled
	  fPixel.at(i).RemovePedestalFromWaveForm(
				      fPixel.at(i).fWaveFormNightSkyPedestal);


	  bool CFDTrig=pfCFD->isFired(fPixel.at(i),fStartTimeOffsetNS,
				       fLastTimeOffsetNS,
				       fLastCFDCrossingNS,nx,ny);
	  if(CFDTrig)
	    {
	      numCFDNoiseTriggers++;
	      numCFDTriggers++;
	    }
	}
    }
  //debug
  //if(numCFDRealTriggers>0)
  //	{
  //	  std::cout<<numCFDTriggers<<" "<<numCFDRealTriggers<<" "
  //		   <<numCFDNoiseTriggers<<std::endl;
  //	}
  return numCFDTriggers;
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
      // **************************************************************
      // This check of fBadPixel just saves some time
      // ***********************************************************
      if(!fPixel.at(i).fBadPixel)
	{
	  fPixel.at(i).InitWaveForm(fWaveFormStart,fWaveFormLength);
	  if(fPixel.at(i).fTimePe.size()>0)
	    {
	      fPixel.at(i).BuildPeWaveForm();
	    }
	  fPixel.at(i).AddNoiseToWaveForm(false);//Note that this noise has not 
	                                      //been modified by overall 
	                                      //efficiency but has been 
	                                      //modified by light cone 
	                                      //efficiency


	  // Remove the night sky pedestal. PMTs Capacitivly coupled
	  fPixel.at(i).RemovePedestalFromWaveForm(
				     fPixel.at(i).fWaveFormNightSkyPedestal);
	}
    } 
  return;
}
// ************************************************************************

void KSCamera::findWaveFormLimits(double& fWaveFormStartNS,
				                   double& fWaveFormLengthNS)
// **********************************************************************
// Find when we should start the waveform and how long it should be.
// **********************************************************************
// This look like it works for pe times <0 which happens for non-zenith 
// showers.
{
  bool minMaxInitalized=false;
  double fPixelMinTimeNS=0;  //This init just avoids a warning.
  double fPixelMaxTimeNS=0;
  for(int i=0;i<fNumPixelsTrigger;i++)
    {
      int fNumPes=fPixel.at(i).fTimePe.size();
      if(fNumPes>0)
	{
	  for(int j=0;j<fNumPes;j++)
	    {
	      double fPeTime=fPixel.at(i).fTimePe.at(j);
	      if(!minMaxInitalized)
		{
		  fPixelMinTimeNS=fPeTime;
		  fPixelMaxTimeNS=fPeTime;
		  minMaxInitalized=true;
		}  
	      else
		{
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
    }

  fWaveFormStartNS=fPixelMinTimeNS -gFADCWindowOffsetNS[fCameraType]
                                   -gFADCTOffsetNS[fCameraType]
                                   -gCFDDelayNS[fCameraType]
                                   -gCFDTriggerDelayNS[fCameraType]-1.0;

  double fWaveFormEndNS = fPixelMaxTimeNS 
    + fPixel.at(0).fSinglePeSizeNS
    + gCFDDelayNS[fCameraType]
    + gCFDTriggerDelayNS[fCameraType]  
    - gFADCWindowOffsetNS[fCameraType]
    - gFADCTOffsetNS[fCameraType]
    + gPSTStrobeDelayNS
    + gFADCBinSizeNS*gFADCNumSamples[fCameraType]+1;

  fWaveFormLength = fWaveFormEndNS-fWaveFormStartNS;


  // *********************************************************************
  // Now find the first and last time we should look at for the signal 
  // reaching threshold
  // ************************************************************************ 
  fStartTimeOffsetNS = fPixelMinTimeNS-fWaveFormStartNS
                                   -gCFDDelayNS[fCameraType]
                                   -gCFDTriggerDelayNS[fCameraType]+1.0;
                              // Start searching for CFD trigger a little early
                              // at beginning
                              // of fWaveForm, offset a bit to make room for
                              // us being offset by the FADC trace;
  fLastTimeOffsetNS  = fPixelMaxTimeNS + fPixel.at(0).fSinglePeSizeNS
                                       - fWaveFormStartNS; 

  //                                     + gPSTPulseWidthNS[fCameraType]
  // *************************************************************************
  // Now last time to check for a CFD zero crossing
  // *****************************************************************
  fLastCFDCrossingNS=(fLastTimeOffsetNS+ gCFDDelayNS[fCameraType]);

  return;
}
// ************************************************************************
  
void KSCamera::loadAPedestalEventIntoPedPixels()
// *********************************************************************
// Create a Pedestal event in the fPedPixels waveforms
// *********************************************************************
{
  // ********************************************************************
  // First task is to fill up the waveforms.
  // ********************************************************************
  double fTraceLengthNS=gFADCNumSamples[fCameraType]*gFADCBinSizeNS+1;
  for(int i=0;i<gNumPixelsCamera[fCameraType];i++)
    {
      fPedPixels.at(i).InitWaveForm(0.0,fTraceLengthNS);
      bool fAfterPulse=false;
      fPedPixels.at(i).AddNoiseToWaveForm(fAfterPulse);  
      //Note that this noise has not been modified by overall efficiency 
      //but has been modified by light cone efficiency
      // ****************************************************************
      // Note: For both WHIPPLE and VERITAS the wave form fed to the 
      // ADC/FADC is ac coupled(0 mean) which in our case means the night 
      // sky pedestal has to be removed. 
      // ****************************************************************
      fPedPixels.at(i).RemovePedestalFromWaveForm(
			     fPedPixels.at(i).fWaveFormNightSkyPedestal);
    }
  return;
}
// ***********************************************************************

