//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSArea
 * \brief Class to hold and process an areas worth of pes. *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#include "KSArea.h"

extern "C" float pran(float* dummy);

KSArea::KSArea(KSPeFile* pPesFile, KSTeFile* pTeFile, 
	       KSSegmentHeadData* pSegmentHead, KSPeHeadData* pPeHead, 
	       KSTriggerDataIn* pDataIn)
{
  pfPesFile=pPesFile;
  pfTeFile=pTeFile;



  //Set up constant tags for output events from this shower
  fTe.fPrimaryType      = pSegmentHead->fType;
  fTe.fShowerEnergyTeV  = pSegmentHead->fGeVEnergyPrimary/100.;
  fTe.fShowerID         = pSegmentHead->fShowerID;
  fTe.fXCoreOffsetM     = pPeHead->fXCoreOffsetM;
  fTe.fYCoreOffsetM     = pPeHead->fYCoreOffsetM;

  fType                 = fTe.fPrimaryType;

  pfDataIn                 = pDataIn;
  fGammas2D                = pfDataIn->pfTeHead->fGammas2D;
  fMultipleMountDirections = pfDataIn->pfTeHead->fMultipleMountDirections;
  fCameraType              = pfDataIn->pfTeHead->fCameraType; 


  fFirstRead=true;

  // ------------------------------------------------------------------------
  //  Initalize the camera (Lots and Lots of work done in this constructor)
  // ----------------------------------------------------------------------
  
  int numPixelsInTrigger=gNumTriggerPixels[fCameraType];
  pfCamera=new KSCamera(fCameraType, pfDataIn->pfTeHead, 
			pfDataIn->fUsePatternTrigger, 0.0,
			gFADCDigCntsPerPEHiGain[fCameraType],
			numPixelsInTrigger,
			pfDataIn->fPSFNorthSouthDeg, 
			pfDataIn->fPSFEastWestDeg);
  pfCamera->Print();

  // ************************************************************************
  // Set up for the multiple mount directions stuff
  // ************************************************************************
  fStepSizeRad  = pfDataIn->fGammaStepSizeRad;
  pfMountDir = new  KSMountDirection(pfDataIn->pfTeHead, fStepSizeRad);
 
  if(pfDataIn->fLoadMountDirectionsFromFile)
    {
      std::ifstream* pfMountDirFile=new std::ifstream(
				  pfDataIn->fMountDirectionFileName.c_str(), 
				  std::ios::in | std::ios::binary);
      if(pfMountDirFile->fail())
	{
	  std::cout<<"KSASrea--Failed to Open an pre-existing input Mount "
	    "Direction file: "<<pfDataIn->fMountDirectionFileName.c_str()
		   <<std::endl;
	  exit(1);
	}
      pfMountDir->readMountDirections(pfMountDirFile);
      pfMountDirFile->close();
    }
  else
    {
      pfMountDir->createMountDirections(pPeHead->fXAreaWidthM,
					pPeHead->fYAreaWidthM);
    }
 
  // ********************************************************************
  // Get the number of mount directions now: May override config file.
  // ********************************************************************
  fNumDirections= pfMountDir->fNumDirections;
  pfDataIn->pfTeHead->fNumDirections=fNumDirections;
     
  if(pfDataIn->fSaveMountDirectionsToFile)
    {
      std::ofstream* pfMountDirFile=new std::ofstream(
				   pfDataIn->fMountDirectionFileName.c_str(),
				   std::ios::out | std::ios::binary);
      if(pfMountDirFile->fail())
	{
	  std::cout<<"KSArea--Failed to Open a new output Mount "
	    "Direction file "<<std::endl;
	  exit(1);
	}
      pfMountDir->writeMountDirections(pfMountDirFile); 
      pfMountDirFile->close();
      std::cout<<"KSArea--Mount Directions written to file: "
	       << pfDataIn->fMountDirectionFileName.c_str()<<std::endl;

    }

  // *******************************************************************
  // Construct the Ray tracing object:KSTiltTrace
  // (repklaces whippletilt)
  // *******************************************************************
  string alignmentMethod="WHIPPLE";
  if(pfDataIn->pfTeHead->fAlignmentMethod==MCGILL){
    alignmentMethod="MCGILL";
  }

  pfTiltAndTrace=new KSTiltAndTrace(
				  pfCamera->fMinimumDnTight, 
				  pfCamera->fMinimumDnLoose, 
				  pfCamera->fMirrorRadiusSquared,
				  pfCamera->fFacetDiameterM,
				  pfCamera->fFocalLengthM,
				  pfCamera->fJitterWidthEastWestRad,
				  pfCamera->fJitterWidthNorthSouthRad,
				  pfCamera->fMetersPerDeg,
				  pfDataIn->pfTeHead->fFocalPlaneLocationM,
				  pfDataIn->pfTeHead->fAlignmentPlaneLocationM,
				  alignmentMethod,
				  pfDataIn->pfTeHead->fFacetLocationFileName);
  
  fNumAreasProcessed=0;
  fGoodTriggerCount=0;
}
// **************************************************************************

KSArea::~KSArea()
{
  // nothing to see here
}

// ***************************************************************************

bool KSArea::ReadPes()
// ***************************************************************************
// Read an areas worth of pes from the input Pes file. Return true if we
// reach the end of file
// ***************************************************************************
{
  fPes.clear();

  // Init if this is first event read in
  if(fFirstRead)
    {
      bool fGoodRead= pfPesFile->ReadPe(&fPe);
      if(!fGoodRead)
	{
	  if(pfPesFile->foundEOF())
	    {//Shower ended, Empty
	      return true;
	    }
	  else
	    {
	      std::cout<<"Read Failure from PesFile. Aborting"<<std::endl;
	      exit(1);
	    }
	}
      fFirstRead=false;
    }

  fPes.push_back(fPe);   //This is either the first one in file or one left 
                         //over from last scan through area
  fAreaNx=fPe.fNx;
  fAreaNy=fPe.fNy;
  //std::cout<<"At:Nx,Ny: "<<fAreaNx<<" "<<fAreaNy<<std::endl;

  fNumAreasProcessed++;

  // ******************************************************************
  // Loop on reading in rest of pes
  // ******************************************************************
  bool fShowerEnd=false;
  while(1)
    {
      bool fGoodRead= pfPesFile->ReadPe(&fPe);
      if(!fGoodRead)
	{
	  if(pfPesFile->foundEOF())
	    {//Shower ended
	      fPes.push_back(fPe);
	      fShowerEnd=true;
	      break;
	    }
	  else
	    {
	      std::cout<<"Read Failure from PesFile. Aborting"<<std::endl;
	      exit(1);
	    }
	}  
      if(fPe.fNx!=fAreaNx || fPe.fNy!=fAreaNy)
	{
	  break;

	}
      fPes.push_back(fPe);  //Save the pe into this area
    }

  return fShowerEnd;   //Note we leave first pe of next area in fPe
}
// *************************************************************************

void KSArea::ProcessImages()
// ***********************************************************************
// This is where all the work gets done. Look in all directions. Build the 
// image. Look for triggers. When we find a trigger write it out.
// ***********************************************************************
{
  //if(fAreaNx==0 && fAreaNy==0)
  // {
  //  std::cout<<"Now at Nx,Ny: "<<fAreaNx<<" "<<fAreaNy<<std::endl;
  //}

  // First check that hwe have enough pe's to even think about this area
  int fNumPes=fPes.size();
  if(fNumPes<kNumPesAreaMinimum)
    {
      return;
    }

// **************************************************************************
// Main processing loop
// Start directions loop
// **************************************************************************

  for(int ithphi=0;ithphi<fNumDirections;ithphi++)
    { 
      // ***************************************************************
      // For Gamma2D we really only want positions along X. This was set 
      // up in MOuntDirections
      // ***************************************************************
			// Preset for efficiency:Used in KSTiltAndTrace
     
      pfTiltAndTrace->fTDlm=pfMountDir->pfDlm[ithphi];
      pfTiltAndTrace->fTDmm=pfMountDir->pfDmm[ithphi];
      pfTiltAndTrace->fTDnm=pfMountDir->pfDnm[ithphi];
      
      pfTiltAndTrace->fXDl=pfMountDir->pfXDlm[ithphi];
      pfTiltAndTrace->fXDm=pfMountDir->pfXDmm[ithphi];
      pfTiltAndTrace->fXDn=pfMountDir->pfXDnm[ithphi];
      
      pfTiltAndTrace->fYDl=pfMountDir->pfYDlm[ithphi];
      pfTiltAndTrace->fYDm=pfMountDir->pfYDmm[ithphi];
      pfTiltAndTrace->fYDn=pfMountDir->pfYDnm[ithphi];
      
      double fEmissionAltitudeSum=0;
      double fEmissionAltitudeSquaredSum=0;
      double fMuonSum=0;
      double fPeHits=0;

      pfCamera->InitPixelImageData();

// **********************************************************************
//iterate over pes in area
// **********************************************************************
      int fDumping=0;
      int dump1=0;
      int dump2=0;
      int dump3=0;
      int dump4=0;
      int dump5=0;
      bool fPesHitPixels=false;
      for(int ipe=0;ipe<fNumPes;ipe++)
	{
	  double fDlp=fPes.at(ipe).fPhotonDl; // Direction cosigns of photon
	  double fDmp=fPes.at(ipe).fPhotonDm;
	  double fDnp = sqrt(1.-fDlp*fDlp-fDmp*fDmp); //Recreate pe's dn. 
	                                              //Going down so 
                                                      //should be positive
	  pfTiltAndTrace->fDlr=fDlp;
	  pfTiltAndTrace->fDmr=fDmp;
	  pfTiltAndTrace->fDnr=fDnp;

	  pfTiltAndTrace->fXg=fPes.at(ipe).fX;
	  pfTiltAndTrace->fYg=fPes.at(ipe).fY;
 
	  pfTiltAndTrace->fTime=fPes.at(ipe).fTime;

// ***************************************************************************
// 	Collect this  Pe into image this direction
// ***************************************************************************
// 	pfTiltAndTrace detrmines: 
//	      1:Does pe hit focal plane. 
//	      2:Does pe hit mirror at its present inclination.
//            3:Angle of pe to mirror. 
// 	      4:Position in focal plane of pe after including aberrations.
//            pfTiltAndTrace corrects pe_into tilted mirror plane.
// ***************************************************************************
// KSTiltAndTrace is a C++ modified from whipple_tilt to interface with this 
// code. Additions have been made for alignment methods and dfocalPlane 
// locations
// ***************************************************************************
	  int dump=pfTiltAndTrace->Tilt(); //dump is Flag that we are to drop 
	                                  // this pe (drop==1)
	  double fWX=pfTiltAndTrace->fW[0];
	  double fWY=pfTiltAndTrace->fW[1];

	  debugCount++;

	  double fPeTime=pfTiltAndTrace->fPeTime;

	  if(dump==1)            //Note here that we now use type int for 
	    {	                  //fDump to avoid differences between Absoft 
	      if(fWX==1)dump1++;  //and Intel Fortrans definions of true and 
	      if(fWX==2)dump2++;  //false
	      if(fWX==3)dump3++;
	      if(fWX==4)dump4++;
	      fDumping++;
	      continue;
	    }                 // drop pe go to next direction.
	  
	  // *****************************************************************
	  //  Correct various camera pecularities.
	  //  for WHIPPLE490: Flip both axis (I think,but am not sure)
	  // ****************************************************************
	  // The whipple 490 pixel camera is rotated by 7.306 deg. Actually, 
	  //cparam has pixel #2 at x=-.117 and y=0.15. That is it is actually 
	  //rotated by 180 +7.306 deg. This rotation is handled in
	  //WhippleCams.h
	  if(pfCamera->fCameraType==WHIPPLE490)
	    {
	      fWX=-fWX;         // rotate by 180 deg both axis
	      fWY=-fWY;
	    }
	  // ****************************************************************
	  // The Veritas 499 pixel camera also needs to flip.
	  // It already has X + east and Y + south for the pixel locations.
	  // ****************************************************************
	  if(pfCamera->fCameraType==VERITAS499)
	    {
	      fWX=-fWX;         // rotate by 180 deg both axis
	      fWY=-fWY;
	    }

	  cout<<"cnt,x,y: "<<debugCount<<" "<<fWX<<" "<<fWY<<endl;

	  // ******************************************************************
	  // Find which PMT this hits.
	  // *****************************************************************
	  int fPixelIndex;

	  bool fDumpFlag=pfCamera->getPixelIndex(fWX,fWY,fPixelIndex);
	  if(fDumpFlag)
	    {
	      dump5++;
	      fDumping++;
	      continue;
	    }                 // drop pe go to next one.


// **************************************************************************
// Save this hit
// **************************************************************************
// 1:Record which tube it hits.
// 2:Keep a running sum of number in image
// 3:Count muons.
// 4:Save time of hit and pixel index
// 5:Collect sum for Mean altitude of emmision.
// 6:Collets sum*sum for rms calc of Mean altitude of emission
	  fPesHitPixels=true;
	  pfCamera->fPixel[fPixelIndex].fDisc++;
	                                        // Save focal plane times.
	  pfCamera->fPixel[fPixelIndex].fTimePe.push_back(fPeTime);

	  fPeHits++;
	  if(fPes.at(ipe).fTrackType==4 || fPes.at(ipe).fTrackType==5)
	    {
	      fMuonSum++;
	    }
	  

	  double fEmAlt=fPes.at(ipe).fEmissionAltitude;
	  fEmissionAltitudeSum        += fEmAlt;
	  fEmissionAltitudeSquaredSum += fEmAlt*fEmAlt;


	}                                //End of pes loop


// ***************************************************************************
//  We now have all the pe's collected into the discriminators
// ***************************************************************************
// Test to see if we have al least on pixel with a pe even look for a trigger
      if(!fPesHitPixels)
	{
	  continue;    // For performance, don't test for trigger
	}
 
      bool fTriggered=pfCamera->isFastTriggered();

      // *********************************************************************
      // Write out Triggered Event if it did!
      // *********************************************************************
      
      if(fTriggered)
	{
	  fTe.fMountDl= pfTiltAndTrace->fTDlm;  //direction of the mount
	  fTe.fMountDm= pfTiltAndTrace->fTDmm;
	  fTe.fNx=fAreaNx;
	  fTe.fNy=fAreaNy;
	  fTe.fAomega=pfMountDir->fAomega;
	  
	  if(fGammas2D || fMultipleMountDirections)
	    {	//Save actual theta phi (in radians*1000) for multiple 
                //directions
	      fTe.fThetaIndex     = (int)pfMountDir->pfSTheta[ithphi]*1000;
	      fTe.fPhiIndex       = (int)pfMountDir->pfSPhi[ithphi]*1000;
	    }
	  else
	    {           //Single direction:Gammas
	      fTe.fThetaIndex=1;
	      fTe.fPhiIndex=1;
	    }
	  fTe.fDirectionIndex = ithphi;    //Hadronic direction index
	  fTe.fEmissionAltitude      = fEmissionAltitudeSum/fPeHits;
	  fTe.fEmissionAltitudeSigma = fEmissionAltitudeSquaredSum/fPeHits-
	    (fTe.fEmissionAltitude*fTe.fEmissionAltitude);
	  if(fTe.fEmissionAltitudeSigma>0)
	    {
	      fTe.fEmissionAltitudeSigma=sqrt(fTe.fEmissionAltitudeSigma);
	    }
	  else
	    {
	      fTe.fEmissionAltitudeSigma=0;
	    }
	  fTe.fMuonRatio=fMuonSum/fPeHits;

	  // ***************************************************************
	  // Write out event.  In 2 parts 
	  // ***************************************************************
          // Write out tags and such
	  pfTeFile->WriteTe(&fTe);


	  // Write out Pes pixel data in compressed format
	  pfTeFile->WriteTePixelData(pfCamera->fPixel);

	  fGoodTriggerCount++;
	} // End good trigger block
    }     // End directions loop
  return;
}
 
void KSArea::PrintStats()
// ***********************************************************************
// This is where all the work gets done. Look in all directions. Build the 
// image. Look for triggers. When we find a trigger write it out.
// ***********************************************************************
{
  if(fMultipleMountDirections)
    {
      if(fGammas2D  )
	{
	  
	  std::cout<<"Gamma-ray: Gammas2D"<<std::endl;
	  std::cout<<"                         Step Size for Gammas2D (deg) = "
		   <<fStepSizeRad*180/M_PI<<std::endl;
	  std::cout<<"                      Maximum Theta for Gammas2D(deg) = "
		   <<pfDataIn->pfTeHead->fMaximumThetaRad*180/M_PI<<std::endl;
	  std::cout<<"             Number Steps for Gammas2D (+/- MaxTheta) = "
		   <<fNumDirections<<std::endl;
	}
      else
	{
	  //Not Gammas2D. Treat as hadrons.
	  std::cout<<"Multiple Random Directions: "<<std::endl;
	  std::cout<<"             Number of Random Directions = "
		   <<fNumDirections<<std::endl;
	  std::cout<<"            Radius random direction area = "
		   <<pfDataIn->pfTeHead->fMaximumThetaRad*180/M_PI<<std::endl;
	}
      
    }
  else
    {
      std::cout<<"Gammas:"<<std::endl;
      std::cout<<"          Number of Directions for Gammas:  "
	       <<fNumDirections<<std::endl;
    }
  std::cout<<"                    Number of Areas processed: "
	   <<fNumAreasProcessed<<std::endl;
  std::cout<<" Number of Triggers written to Output Te File: "
	   <<fGoodTriggerCount<<std::endl;
  return;
}
// ***********************************************************************


