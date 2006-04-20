/**
 * \class KSCamera
 * \ingroup common
 * \brief File of methods for KSCamera.
 
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
  generateCamera();



  // ************************************************************************
  // VERITAS499 CAMERA
  // ************************************************************************
  int fNumPix;
  int fRing;
  int fLastGoodPixel;
  double fMaxFOVDeg=0;
  if(fCameraType==VERITAS499)
    {
      //  Ring#12. (center pixel is ring 0) Which looks like the other rings 
      //  with the exception that only 66 positions of the line are filled 
      //  with PMT's. These 
      //  positions consist of the 11 center positions on each of the 6 sides.
      //  Only pixel postions have to be moved. PMT_spacing are all set and all
      //  the same already
      fNumPix=11;		// number of pixels centerd per side.
      fRing=12;
      fLastGoodPixel=(fRing-1)*fRing*3; // gets to last pixel thats ok.
      ringBuild(fLastGoodPixel,fRing,fNumPix);

      // Ring#13
      fNumPix=6;		        // pixel/side in ring#13
      fRing=13;		        //fLastGoodPixel already set from last call
      ringBuild(fLastGoodPixel,fRing,fNumPix);
      fMaxFOVDeg=( pfLinePerpDistanceDeg[fRing] +
	                2*cos(30.*gDeg2Rad)*pfLineHalfPixelSepDeg[fRing]  )/
	               cos(30.*gDeg2Rad);
    }

  // ************************************************************************
  // WHIPPLE490 CAMERA
  // ************************************************************************
  if(fCameraType==WHIPPLE490)
    {
      // Ring#11. The 11 th ring looks like the other rings with the exception
      // that only 48 positions of the line are filled with PMT's. These 
      // positions consist of the 8 center positions on each of the 6 sides.
      // Only pixel postions have to be moved. PMT_spacing are all set and all
      // the same already
      fNumPix=8;		// number of pixels centerd per side.
      fRing=11;		// Ring number
      fLastGoodPixel=(fRing-1)*fRing*3;// gets to last pixel thats ok.
      ringBuild(fLastGoodPixel,fRing,fNumPix);
      // -------------------------------------------------------------------
      // for whipple490: outer  3 rings of 1" pmts are in true circles(not 
      //hexagons)
      // -------------------------------------------------------------------

      double fAngOffset;

      double fRingSpacingDeg;
      for(int j=0;j<3;j++)
	{
	  double fRingRadiusDeg=(g490OuterRingDiameterM[j]/2.0)/fMetersPerDeg;
	  fRingSpacingDeg=sin(gDeg2Rad*g490OuterRingsAngularStepDeg/2.) * 
	                      (fRingRadiusDeg)*2;
	  double fPMTRadiusDeg=g490OuterPixelRadiusMM/(fMetersPerDeg*1000);
	  for(int i=0;i<37;i++)
	    {
	      if(j==1)   //Middle Ring
		{
		  fAngOffset=g490OuterRingsAngularStepDeg/2;
		}
	      else
		{
		  fAngOffset=0;
		}
	      fLastGoodPixel++;     //Camera_x,Camera_y,PMT_sapacing are all 
	                            //in deg.fOouterRingDiameterM is in meters.
	      fPixel[fLastGoodPixel].fXDeg = 
		cos(gDeg2Rad*(i*g490OuterRingsAngularStepDeg+fAngOffset))*
		fRingRadiusDeg;
	      fPixel[fLastGoodPixel].fYDeg  =
		-sin(gDeg2Rad*(i*g490OuterRingsAngularStepDeg+fAngOffset))*
		fRingRadiusDeg;
				// pmt_spacing is half spacing// 
	      fPixel[fLastGoodPixel].fHalfSpacingDeg=fRingSpacingDeg/2;
	      fPixel[fLastGoodPixel].fRadiusDeg=fPMTRadiusDeg;
	    }  // pmt in ring loop
	}    // ring loop
      

      // Max angle(in deg) of field of view(plus a little)
    // std::cout<<"g490OuterRingDiameterM[2]/2: "<<g490OuterRingDiameterM[2]/2
      //	       <<std::endl;
      fMaxFOVDeg = (g490OuterRingDiameterM[2]/2)/fMetersPerDeg; 
      //std::cout<<"fMaxFOVDeg:1: "<<fMaxFOVDeg<<std::endl;
 
      fMaxFOVDeg = (g490OuterRingDiameterM[2]/2)/fMetersPerDeg+ 
	                fRingSpacingDeg;
     }

  if(fMaxFOVDeg<1.0)
    {
      fMaxFOVDeg=1.0;
    } 
  fMinimumDnTight=cos(fMaxFOVDeg*gDeg2Rad);
  fMinimumDnLoose=cos((fMaxFOVDeg+1.0)*gDeg2Rad);
  std::cout<<"Tight Camera FOV radius: "<<fMaxFOVDeg<<" deg."<<std::endl;
  return;
}
// ***************************************************************************


// ****************************************************************************

void KSCamera::generateCamera()
// ****************************************************************************
// 	Get PMT locations and radii from WhippleCams.h
// ****************************************************************************
{
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
      fPixel[i].fRadiusDeg = pfTelescopePixelRadius[i];
      fPixel[i].fYDeg = fPixel[i].fHalfSpacingDeg*fGrid.pfYIndex[i]*
	                                                   cos(gDeg2Rad*30.);
      fPixel[i].fXDeg = fPixel[i].fHalfSpacingDeg*fGrid.pfXIndex[i];
      // Note that this value of the pmt_spacing is the half pmt seperation. 
      // fGrid.pfXIndex has the factor of 2 (cause sometimes we take half 
      // steps in X. always full*cos(30deg) in Y 
    }
  // **********************************************************************
  // Fill in the line arrays:: distance,the spacing, and first and last pixel 
  // index. Line i=0 is first ring(Ring#1)Where we call center pixel the 0th 
  // ring . So a line=ring-1
  // **********************************************************************
  // Note: We use pixel index which starts at 0. Pixel number starts at 1.
  // **********************************************************************

  int fPixelIndex=1;                  //First Pixel Index in ring=1,line=0
  for(int i=0;i<fNumLines;i++)
    {
      double fDistLine = (i+1)*(2*fHalfSpacingDeg); //X dist to a line
      pfLinePerpDistanceDeg[i] = cos(gDeg2Rad*30.0)*fDistLine; //Perp dist 
      pfLineHalfLengthDeg[i]   = fDistLine/2.+fHalfSpacingDeg;//Line half 
                        //length since half length is (i+1) fHalfSpacings
                        //Add 1/2 pixels spacing to set up int round down 
                        
      pfLineHalfPixelSepDeg[i]=  fHalfSpacingDeg/cos(gDeg2Rad*30.0);

      fPixelIndex=fPixelIndex+(i)*6;      //First pixel index in ring
      pfLineNumPixels[i] = i+2;           //Number of pixels/side in this ring 
                                          //line i=0, next to center, has 2
      // Now fill in the pixel index
      for(int j=0;j<6;j++)		 // over sectors
	{			//counting backwards
	  pfLineFirstPixelID[i][j]=  fPixelIndex + (i+1)*6-(j+1)*(i+1);
	  pfLineLastPixelID[i][j] =  pfLineFirstPixelID[i][j] + i+1;
	}
      // Last of sector 0 is same as first of sector 5
      pfLineLastPixelID[i][0]=pfLineFirstPixelID[i][5];
    }
  return;
}
// ***********************************************************************

void KSCamera::ringBuild(int& fLastGoodPixel, int fRing, int npix)
// ****************************************************************************
// 	Build stuff into arrays for paritally filled rings(usually done to
// 	circlize a camera.
// ****************************************************************************
// fLasGoodPixel:index to last pixel ok pixel . we add on from here.
// Assumtion is fXDeg and fYDeg arrays were first filled as if for a fully 
// populated camera.  We kindof repositon things down in the array.
// fRing=ring number to build. fRing+2 is also the number of pixels/side for a
// full ring.	npix=number of pixels per side to use for this new ring 
// centered on each side. npix must always be 1 or 3 or 5.etc less then i(to 
// be centered
{
  int fLine=fRing-1;
  int fStart=(fRing-1)*fRing*3+1;   // gets array index of first pixel in this
				    // ring(which we may not want)
  fStart=fStart+(fRing-npix+1)/2;   // Array index  of first pixel we do want
				    // (fRing-npix always odd)
  int fPix=fLastGoodPixel+1; 	    // Save Array Index of first pixel of 
                                    // repostioned ring
  int fNextPixel=fPix;
  for(int j=5;j>=0;j--)            // Six sides but sectors count backwards
    {		
      pfLineFirstPixelID[fLine][j]= fNextPixel;	
      for(int ij=fStart;ij<fStart+(npix);ij++)//npix of them only on side
	{ 
                                               // Move ij pixel down.
	  fPixel[fNextPixel].fXDeg           = fPixel[ij].fXDeg;
	  fPixel[fNextPixel].fYDeg           = fPixel[ij].fYDeg; 
	  fPixel[fNextPixel].fHalfSpacingDeg = 
	                                      fPixel[ij].fHalfSpacingDeg;
	  fPixel[fNextPixel].fRadiusDeg = fPixel[ij].fRadiusDeg;
	  fNextPixel++;
	}
      pfLineLastPixelID[fLine][j] = fNextPixel-1;
      fStart=fStart+fRing; //Move up to start of next sector which is number
                           //on a side
    }
  fLastGoodPixel=fNextPixel-1;

  // ***********************************************************************
  // Now do lines of ring#i partially filled ring(only npix center tubes each 
  // side)
  // ***********************************************************************
  double fHalfSpacingDeg=fPixel[fPix].fHalfSpacingDeg;
  double fDistLine = (fLine+1)*(2*fHalfSpacingDeg);    //X dist to a line
  pfLinePerpDistanceDeg[fLine] = cos(gDeg2Rad*30.0)*fDistLine;//Perp dist
  pfLineHalfLengthDeg[fLine]   = (npix)*fHalfSpacingDeg;//Line Half 
                        //length since half length is (npix+1) +1  
                        //half spaceings 
  pfLineHalfPixelSepDeg[fLine]=  fHalfSpacingDeg/cos(gDeg2Rad*30.0);

  pfLineNumPixels[fLine] = npix;      //Number of pixels/side in this ring 
  // Now fill in the pixel index
  //for(int j=0;j<6;j++)		 // over sectors
  // {			
  //   pfLineFirstPixelID[fLine][j]= fPix + 6*npix - (j+1)*npix;	
  //   pfLineLastPixelID[fLine][j] = pfLineFirstPixelID[fLine][j] + (npix-1);
  //  }
  return;
}
// **********************************************************************

void KSCamera::buildAdjacencyArrays()
// *************************************************************************
// 	Set up the adjacency table . Used for image cleanup.
// **************************************************************************
// Use a stupid brute force dumb distance algorithum that has the advantage of
// being easy to understand(and write). Use vector<int> for neighbors. Allows
// for varying numners of neighbors. With our funny gaps in some of our 
// cameras we can have 7 adjacent pmts 
{
  int fNumPixels=gNumPixelsCamera[fCameraType];
  for(int i=0;i<fNumPixels;i++)
    {
      double fRadiusCenterDeg=fPixel[i].fHalfSpacingDeg;
      for(int j=0;j<fNumPixels;j++)//see if tube j is close enough.Define close
	{	                   //enough as less then 2.25 times pmt radius.
	  double fRadiusNeighborDeg=fPixel[j].fHalfSpacingDeg;
	  double fMaxSeperationDeg;
	  if(fRadiusCenterDeg==fRadiusNeighborDeg)
	    {
	      fMaxSeperationDeg=(2.25*fRadiusCenterDeg);
	    }
	  else if(fRadiusCenterDeg<fRadiusNeighborDeg)
	    {
	      fMaxSeperationDeg=(2.25*fRadiusCenterDeg+fRadiusNeighborDeg);
	    }
	  else
	    {
	      fMaxSeperationDeg=(fRadiusCenterDeg+2.5*fRadiusNeighborDeg);
	    }
	  double fMaxSeperationSquaredDeg= fMaxSeperationDeg*fMaxSeperationDeg;
	  // Square max seperation of centers of 
	  //pmts to be considered adjacent.
	  if(j!=i)	// Skip on self test.
	    {
	      double fXSepDeg=(fPixel[i].fXDeg-fPixel[j].fXDeg);
	      double fYSepDeg=(fPixel[i].fYDeg-fPixel[j].fYDeg);
	      double fSeperationDistanceSquaredDeg=
		fXSepDeg*fXSepDeg+fYSepDeg*fYSepDeg;
	      if(fSeperationDistanceSquaredDeg<fMaxSeperationSquaredDeg)
		{
		  fPixel[i].fAdjacentPixels.push_back(j);
		}
	    }
	}
    }
  return;
}
//  ***********************************************************************

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


bool KSCamera::getPixelIndex(double fXDeg, double fYDeg, int& fIPix)
// ****************************************************************************
// 	Returns the pixel that a pe at fXDeg,fYDeg would hit.
// ****************************************************************************
// We have previously divided our hexagonal pixel array into 6 sectors. In 
// each sector we have lines of pmts(or light cones) perpendicular to the 
// bisecter of the sector.
//  1:perpendicular distance from the center of the line to the center of the 
//    field of view.
//  2:Half length of line from center of line to end.
//  3:1/2 width of the pixel seperation(1/2 width of the line of PMTs etc.)
//    divided by cos(30deg). This cos(30) term is for use with lightcones.
//    It reaches into interseces bewtween pixesl when we close pack them.
//  4:PMT index for the first pixel on the line(counting clockwise)
//  5:PMT index for the lastt pixel on the line(counting clockwise)
//  Note that in sector 1 only this number is less then the others on that 
//  line(which increase each by one from the first). This is because it is also
//  the first pixel in sector 6. The last pixel in the lines in sector 1 
//  dosen't follow consecutivly with the rest of the pixels in the line.(pixel
//  number otherwise increases in the clockwise direction along the line).
//  6:Number of pixels in the line. 
// Too use as a non light cone version, change the pmt_spacing array with 
// radii that match the actual sensitive area of the PMTs. Also comment out 
// the lines indicated below with the '// LIGHTCONE' comment.
 // *************************************************************************
{
  bool fDump=true;;
  // ***********************************************************************
  //  Get the theta angle for X,Y. Use quadrant infromation.
  // ***********************************************************************
  // Find first quadrant angle.
  double fTheta;
  if(fXDeg==0 && fYDeg!=0)
    {
      fTheta=90.;		                        // On verticle axis
    }
  else if(fYDeg==0)	
    {
      fTheta=0.;                     			// On hoizontal axis
    }
  else
    {
      fTheta=atan(fabs(fYDeg/fXDeg))*gRad2Deg;                // Quadrant ++
    }
  if(fXDeg>=0)
    {
      if(fYDeg<0)
	{
	  fTheta=360.-fTheta;		                 // Quadrant +-
	}
    }
  else
    {
      if(fYDeg>=0)
	{
	  fTheta=180.-fTheta;			  // Quadrant -+
	}
      else
	{
	  fTheta=180.+fTheta;			// Quadrant --
	}
    }
  

  // *******************************************************************
  // Determine the sector
  // *******************************************************************
  int fSector=(int)(fTheta/60. + 1);             // Round down to the sector


  // *******************************************************************
  // Sanity check
  // *******************************************************************
  if(fSector<1)
    {
      std::cout<<" ksTrigger:Info only--fSector out of range:fSector,fTheta:"
	       <<fSector<<" "<<fTheta<<std::endl;
      fSector=1;
    }
  else if(fSector>6)
    {
      if(fTheta!=360.0)
	{
	  std::cout
	    <<"ksTrigger: Info only--fSector out of range:fSector,fTheta:"
	    <<fSector<<" "<<fTheta<<std::endl;
	}
      fSector=6;
    }

  // *******************************************************************
  // 	Determine angle to bisecter of sector
  // *******************************************************************
  double fThetaSector=-(fmod(fTheta,60.)-30.); // Positive is in clockwise direction.

// *******************************************************************
// 	Find distance of X,Y to center of field of view.
// *******************************************************************
  double fDistance=sqrt(fXDeg*fXDeg+fYDeg*fYDeg);


// *******************************************************************
// 	Find projected perpendicular fDistance from center of field of view
// 	along bfSector line.
// *******************************************************************
  double fDistanceFOV=cos(gDeg2Rad*fThetaSector)*fDistance;



// *******************************************************************
// 	Check if we are beyond next to last line since for whipple only
//      the first ring dips into region of last line.
// *******************************************************************
      // ********************************************************************
      // if WHIPPLE490 camera and not in inner pixels, check if outer rings
      // ********************************************************************
  if(fCameraType==WHIPPLE490)
    {
      if((fDistanceFOV-pfLinePerpDistanceDeg[fNumLines-2])>
	 pfLineHalfPixelSepDeg[fNumLines-2])
	{
	  fDump=trywhipple490OuterPixels(fXDeg,fYDeg,fIPix);
	  // **************************************************************
	  // Sanity check
	  // ************************************************************
	  if(!fDump && fIPix>fNumPixels)
	    {
	      std::cout<<"ksTrigger: fIPix out of range:"<<fIPix
		       <<std::endl;
	      return fDump;
	    }
	  if(!fDump)
	    {
	      //std::cout<<fXDeg<<" "<<fYDeg<<" "<<fIPix<<std::endl;
	      return fDump;   //got it
	    }
	  //Might still be in last line
	  else if((fDistanceFOV-pfLinePerpDistanceDeg[fNumLines-1])>
		  pfLineHalfPixelSepDeg[fNumLines-1])
	    {  
	      fDump=true;  //Totaly missed the camera
	      return fDump;
	    }
	}
    }
  else              //Veritas499 see if beyond last line.
    {
      if((fDistanceFOV-pfLinePerpDistanceDeg[fNumLines-1])>
	 pfLineHalfPixelSepDeg[fNumLines-1])
	{  
	  fDump=true;  //
	  return fDump;
	}
    }

// *******************************************************************
// Search through the lines to see which ones to look at. 
// *******************************************************************
  int fIPixel;
// Note: line -1 (counting as c++) is just the center pixel which has a 
// maximum reach into its hex cell the the same as the pixel 
// HalfSpacing/cosd(30deg) of the zeroith line.

  fDump=true;			// Initialize for a miss// 
  fIPix=-1;
  double fLightConeWidth=(2*pfLineHalfPixelSepDeg[0]*
					 cos(gDeg2Rad*30.0)) ;
  if(isInCell(fXDeg,fYDeg,fLightConeWidth))
    {
      fIPix=0;
      fDump=false;
    }
  else
    { // ***************************************************************
      // Find perpendicul distance along the line from the bfSector of the 
      // point X,Y
      // *******************************************************************
      double fDistanceLine=sin(gDeg2Rad*fThetaSector)*fDistance;

      // *******************************************************************
      // For speed: Start at the outside lines and work back. Pe most lilkely
      // on outer lines by an area argument(more pixels outer lines)
      // *******************************************************************
      for(int i=fNumLines-1;i>=0;i--)
	{
	  // *****************************************************************
	  // Find distance for effective reach of light cones. Remeber that the
	  // lines are LineHalfPixelSepDeg*cos(30) apart but are close packed
	  // Thus we may need to check lightcone cells along 2 lines
	  // ****************************************************************
	  if(fabs(fDistanceFOV-pfLinePerpDistanceDeg[i])<=
	     pfLineHalfPixelSepDeg[i])
	    {
	      //Ok weve possible got one radially. Make sure
	      //we also have it along the line. (Some lines are short)
	      if(fabs(fDistanceLine)<=pfLineHalfLengthDeg[i])
		{                        //Pixel index from start of line.The 
	                                 //cosd(30) gets us back to pixel
	                                 //spacing from lightcone reach.
		  double fLightConeWidth=(2*pfLineHalfPixelSepDeg[i]*
					 cos(gDeg2Rad*30.0)) ;
		  int fIpixIndex=(int)((pfLineHalfLengthDeg[i]+fDistanceLine)/ 
				   fLightConeWidth);
		     
		  fIPixel=fIpixIndex+pfLineFirstPixelID[i][fSector-1];
	                                 // See is this is the out of order 
	                                 //pixel in sector 1:pfLineNumPixels 
	                                 //is number of pixels in a complete 
	                                 //line. Do we need to change pixel 
	                                 //value?
		  if(fSector==1 && (fIpixIndex==pfLineNumPixels[i]-1))
		    {                        // last one sector 1
		      fIPixel=pfLineLastPixelID[i][0];
		    }
	      // Test if we are inside lightcone
		  double fCellXDeg=fPixel[fIPixel].fXDeg-fXDeg;
		  double fCellYDeg=fPixel[fIPixel].fYDeg-fYDeg;
		  bool fInCell=isInCell(fCellXDeg,fCellYDeg,fLightConeWidth);
		  if(fInCell)
		    {
		      fIPix=fIPixel;
		      fDump=false;
		      break;         //Found it.
		    }
		}            //If this wasn't it, maybe a cell in in next inner
	    }                //line.
	  
	}
    }

// **************************************************************************
// Sanity check
// **************************************************************************
  if(!fDump && fIPix>fNumPixels)
    {
      std::cout<<"ksTrigger: fIPix out of range:"<<fIPix<<std::endl;
      fDump=true;
    }
  // if(!fDump)
  // {
  //   std::cout<<fXDeg<<" "<<fYDeg<<" "<<fIPix<<std::endl;
  // }
  return fDump;
}			

// *************************************************************************

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
  bool fDump=true;
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
		  fDump=false;
		  return fDump;
		}
	    }
	  if(fDump)
	    {
	      return fDump;             // In a ring but No hit.
	    }
	}
      
    }
  return fDump;
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
		 
bool KSCamera::isInCell(double fCellXDeg,double fCellYDeg, double fCellWidth)
// ******************************************************************
// See if fCellXdeg,fCellYDeg is inside a light cone cell of 'width' 
// fCellWidth (this is full seperation width between two parallel sides of 
// hexagon, NOT seperation between opposite 'points' of the hexagon))
// **********************************************************************
//  0,0 of fCellXDeg,fCellYDeg is center of cell
// Cell volume can be divided into 6 quadrants but we only need angle to 
// quadrant bisector. This is simular to getPixelIndex but the cell is rotated 
// by 30 deg to camera rings.
{
  double fThetaDeg=0;
  if(fCellXDeg==0)
    {
      fThetaDeg=0;
    }
  else if(fCellYDeg==0)
    {
      fThetaDeg=30.;
    }
  else
    {
      fThetaDeg=atan(fabs(fCellYDeg/fCellXDeg))*gRad2Deg;
    }
  if(fThetaDeg>30)
    {
      fThetaDeg=fabs(60.-fThetaDeg);
    }
  double fPerpDistanceDeg=sqrt(fCellXDeg*fCellXDeg+fCellYDeg*fCellYDeg)*
                                                     cos(fThetaDeg*gDeg2Rad);
  if(fPerpDistanceDeg<=fCellWidth/2.)
    {
      return true;
    }
  else
    {
      return false;
    }
}
// ************************************************************************

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

void KSCamera::findWaveFormLimits(double& fWaveFormStartNS,
				                   double& fWaveFormLengthNS)
{
  // **********************************************************************
  // Find when we should start the waveform and how long it should be.
  // **********************************************************************

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
