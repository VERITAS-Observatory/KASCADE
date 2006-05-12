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
extern "C" void whippletilt(double* fDlp, double* fDmp, double* fDnp, 
			    double* fDnDnm, double* fTDlm, double* fTDmm, 
			    double* fTDnm,double* fXDl, double* fXDm, 
			    double* fXDn, double* fYDl, double* fYDm, 
			    double* fYDn, double* fX, double* fY, 
			    double* fTime, double* fMirrorRadiusSquared, 
			    double* fMinimumDnTight, double* fMinimumDnLoose, 
			    double* fFacetDiameterM,double* fFocalLength,
			    double* fJitterWidthEastWestRad,
			    double* fJitterWidthNorthSouthRad,
			    double* fMetersPerDeg,
			    double* fWX, double* fWY,int* fDump, 
			    double* fPeTime, double* fPeTimeTilt); 

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
  fDriftingGammas          = pfDataIn->pfTeHead->fDriftingGammas;
  fMultipleMountDirections = pfDataIn->pfTeHead->fMultipleMountDirections;
  fCameraType              = pfDataIn->pfTeHead->fCameraType; 


  fFirstRead=true;

  // ------------------------------------------------------------------------
  //  Initalize the camera (Lots and Lots of work done in this constructor)
  // ----------------------------------------------------------------------
  pfCamera=new KSCamera(fCameraType, pfDataIn->pfTeHead, 
			                        pfDataIn->fUsePatternTrigger);
  pfCamera->Print();

  // ************************************************************************
  // Set up for the multiple mount directions stuff
  // ************************************************************************
  fStepSizeRad  = pfDataIn->fDriftedGammaStepSizeRad;
  pfMountDir = new  KSMountDirection(pfDataIn->pfTeHead, fStepSizeRad);
 
  if(pfDataIn->fLoadMountDirectionsFromFile)
    {
      std::ifstream* pfMountDirFile=new std::ifstream(
				  pfDataIn->fMountDirectionFileName.c_str(), 
				  std::ios::in | std::ios::binary);
      if(pfMountDirFile->fail())
	{
	  std::cout<<"KSASrea--Failed to Open an pre-existing input Mount "
	    "Direction file "<<std::endl;
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
    }



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
      // For drift scan we really only want positions along X axis. This 
      // happens for iphi=1 (+x) and iphi=itheta(-x) for ghi_side=2
 
      int fTheta;
      int fPhi;
      if(fDriftingGammas)
	{
	  pfMountDir->getIThetaIPhi(ithphi,fPhiSteps,fTheta,fPhi);
	  if(fPhi!=1 && fPhi!=fTheta)
	    {
	      continue;
	    }
	}

			// Preset for efficiency:Used in WHIPPLE_TILT
      double fTDlm=pfMountDir->pfDlm[ithphi];   //Mount direction
      double fTDmm=pfMountDir->pfDmm[ithphi];
      double fTDnm=pfMountDir->pfDnm[ithphi];
      
      double fXDl=pfMountDir->pfXDlm[ithphi];   //X vector in focal plane for 
      double fXDm=pfMountDir->pfXDmm[ithphi];   //this mount direction
      double fXDn=pfMountDir->pfXDnm[ithphi];
      
      double fYDl=pfMountDir->pfYDlm[ithphi];   //Y vector in focal plane for 
      double fYDm=pfMountDir->pfYDmm[ithphi];   //this mount direction
      double fYDn=pfMountDir->pfYDnm[ithphi];
      
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
      bool fPesHitPixels=false;
      for(int ipe=0;ipe<fNumPes;ipe++)
	{
	  double fDlp=fPes[ipe].fPhotonDl;     // Direction cosigns of photon
	  double fDmp=fPes[ipe].fPhotonDm;
	  double fDnp = sqrt(1.-fDlp*fDlp-fDmp*fDmp); //Recreate pe's dn. 
	                                              //Going down so 
                                                      //should be positive
	  double fDnDnm=fDnp*fTDnm;

// ***************************************************************************
// 	Collect this  Pe into image this direction
// ***************************************************************************
// 	WHIPPLE_TILT dtermines: 1:Does pe hit focal plane. 2:Does pe hit mirror
// 	at its present inclination.3:Angle of pe to mirror. 
// 	4:Position in focal plane of pe after including aberrations.
// 	WHIPPLE_TILT also corrects pe_into tilted mirror plane.
// ***************************************************************************
// whippletilt is f90 modified from whipple_tilt to interface with this code
// ***************************************************************************
	  double fWX=0;
	  double fWY=0;
	  int    fDump=0;   //Flag that we are to drop this pe (drop==1)
	  double fPeTime;
	  double fPeTimeTilt;

	  //if(fAreaNx==0 && fAreaNy==0)fWX=-1;
	  whippletilt(&fDlp, &fDmp, &fDnp, &fDnDnm, &fTDlm, &fTDmm, &fTDnm,
		      &fXDl, &fXDm, &fXDn, &fYDl, &fYDm, &fYDn, &fPes[ipe].fX,
		      &fPes[ipe].fY, &fPes[ipe].fTime, 
		      &pfCamera->fMirrorRadiusSquared, 
		      &pfCamera->fMinimumDnTight, 
		      &pfCamera->fMinimumDnLoose, 
		      &pfCamera->fFacetDiameterM,
		      &pfCamera->fFocalLengthM,
		      &pfCamera->fJitterWidthEastWestRad,
		      &pfCamera->fJitterWidthNorthSouthRad,
		      &pfCamera->fMetersPerDeg,
		      &fWX, &fWY,&fDump, &fPeTime, &fPeTimeTilt); 
	  if(fDump==1)            //Note here that we now use type int for 
	    {	                  //fDump to avoid differences between Absoft 
	      if(fWX==1)dump1++;  //and Intel Fortrans definions of true and 
	      if(fWX==2)dump2++;  //false
	      if(fWX==3)dump3++;
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

	  // ******************************************************************
	  // Find which PMT this hits.
	  // *****************************************************************
	  int fPixelIndex;

	  bool fDumpFlag=pfCamera->getPixelIndex(fWX,fWY,fPixelIndex);
	  if(fDumpFlag)
	    {
	      dump4++;
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
	  if(fType==4 || fType==5)
	    {
	      fMuonSum++;
	    }
	  

	  double fEmAlt=fPes[ipe].fEmissionAltitude;
	  fEmissionAltitudeSum        += fEmAlt;
	  fEmissionAltitudeSquaredSum += fEmAlt*fEmAlt;


	}                                //End of pes loop


// ***************************************************************************
//  We now have all the pe's collected into the  disc
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
	  fTe.fMountDl=fTDlm;  //direction of the mount
	  fTe.fMountDm=fTDmm;
	  fTe.fNx=fAreaNx;
	  fTe.fNy=fAreaNy;
	  fTe.fAomega=pfMountDir->fAomega;
	  
	  if(fDriftingGammas || !fMultipleMountDirections)
	    {
	      int fITheta;
	      int fIPhi;
	      pfMountDir->getIThetaIPhi(ithphi,fPhiSteps,fITheta,fIPhi); 
	      fTe.fThetaIndex     = fITheta;
	      fTe.fPhiIndex       = fIPhi;
	    }
	  else if(fMultipleMountDirections)
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
      if(fDriftingGammas  )
	{
	  
	  std::cout<<"Gamma-ray: Drifting Gammas"<<std::endl;
	  std::cout<<"     Step Size for Drifting Gammas (deg) = "
		   <<fStepSizeRad*180/M_PI<<std::endl;
	  std::cout<<"  Maximum Theta for Drifting GAMMAS(deg) = "
		   <<pfDataIn->pfTeHead->fMaximumThetaRad*180/M_PI<<std::endl;
	  std::cout<<"    Number Phi Steps for Drifting Gammas = "
		   <<fPhiSteps<<std::endl;
	  std::cout<<"  Number Theta Steps for Drifting Gammas = "
		   <<fThetaSteps<<std::endl;
	  std::cout<<"Number of Directions for Drifting Gammas = "
		   <<fNumDirections<<std::endl;
	}
      else
	{
	  //Not drifting Gammas. Treat as hadrons.
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


