/**
 * \class KSTeDataClasses
 * \ingroup common
 * \brief File of methods for KSTeDataClasses.
 *  Declaration of the data class for the pes
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include <iostream>

#include "KSTeDataClasses.h"

#ifdef __MAKECINT__
#pragma link C++ class KSTeHeadData;
#pragma link C++ class KSTeData;
#endif

KSTeHeadData::KSTeHeadData()
{
  setFacetLocationFileName(" ");
}

KSTeHeadData::~KSTeHeadData()
{
  //nothing to do
}
void KSTeHeadData::PrintTeHead()
// ***************************************************************************
//	Print parameters
// ***************************************************************************
{
  std::cout<<"Te Head:"<<std::endl;
  std::cout<<" Camera Type:(Whipple490="<<WHIPPLE490<<",VERITAS499="
	   <<VERITAS499<<"): "<<fCameraType<<std::endl; 
  std::cout<<"            Trace Creation: "<<fTraceCreation<<std::endl;
  std::cout<<"                  Gammas2D: "<<fGammas2D<<std::endl;
  std::cout<<" Multiple Mount Directions: "<<fMultipleMountDirections
	   <<std::endl; 
  std::cout<<"                Sky Noise Rate pe/ns/deg: "<<fNoiseRate
	   <<std::endl;
  std::cout<<"   Discriminator Coincidence Width in ns: "
	   <<fDiscCoincidenceWidthNS<<std::endl;
  std::cout<<"          Discriminator Threshold in Pes: "
	   <<fDiscriminatorThresholdPes<<std::endl;
  std::cout<<"     Global Photon Generation Efficiency: "<<fEfficiency
	   <<std::endl;
  std::cout<<"  Maximum Mount Redirection Theta in Deg: "
	   <<fMaximumThetaRad*180./M_PI<<std::endl;
  std::cout<<" Number of Mount Re-Directions per event: "<<fNumDirections
	   <<std::endl;
  std::cout<<"        Trigger Multiplicity Requirement: "<<fTriggerMultiplicity
	   <<std::endl;
  std::cout<<"                   Pattern Trigger Level: "<<fPatternTriggerLevel
	   <<std::endl;
  if(fPatternTriggerLevel<2 || fPatternTriggerLevel>4)
    {
      std::cout<<" Pattern Trigger Disabled! Acceptable Pattern Trigger "
	"Levels 2,3 or 4"<<std::endl;
    } 
  std::cout<<"     Light Cone Concentration Efficiency: "
	   <<fLightConeConcentration<<std::endl;
  std::cout<<"           Mount Dl (x direction cosign): "<<fMountDl<<std::endl;
  std::cout<<"           Mount Dm (y direction cosign): "<<fMountDm<<std::endl;
  std::cout<<"           Mount Dn (z direction cosign): "<<fMountDn<<std::endl;

  std::cout<<"  Facet Normal Jitter sigma(North-South): "<<fPSFNorthSouthDeg
	   <<std::endl;
  std::cout<<"    Facet Normal Jitter sigma(East-West): "<<fPSFEastWestDeg
	   <<std::endl;
  std::cout<<"           Focal plane location (meters): "
	   <<fFocalPlaneLocationM<<std::endl;
  if(fAlignmentMethod==MCGILL){
    std::cout<<"       Facet Alignment plane loc(meters): "
	     <<fAlignmentPlaneLocationM<<std::endl;
  }

  std::cout<<"                  Facet Alignment Method: ";
  if(fAlignmentMethod==WHIPPLE){
    std::cout<<"WHIPPLE"<<std::endl;
  }
  else{
    std::cout<<"MCGILL"<<std::endl;
  }
  if(getFacetLocationFileName()==" "){
    std::cout<<"        Using Hillas random facet method:"<<std::endl;
      }
  else{
    std::cout<<"                     Facet Location File: "
	     <<getFacetLocationFileName()<<std::endl;
  }
  
//std::cout<<"                       ksTrigger Version: "<<fVersion<<std::endl;
  return;
}
// **************************************************************************

bool KSTeHeadData::setFacetLocationFileName(std::string FacetLocationFileSpec)
// *********************************************************
// Save file specification in te header
// *********************************************************
{
  int numChar=FacetLocationFileSpec.size();
  if(numChar+1> kNumCharFacetLocationName){
    std::cout<<"KSTeHeadData: Facet Location File Name: "
	     <<FacetLocationFileSpec<<" too long ("
	     <<numChar<<"bytes) to fit in KSTeHeadData char array."
 	     <<std::endl;
    return false;
  }

  FacetLocationFileSpec.copy(fFacetLocationFileName, numChar);
  fFacetLocationFileName[numChar]='\0';
  return true;  
}
// **************************************************************************

std::string KSTeHeadData::getFacetLocationFileName()
// *************************************************************
// Convert the fFacetLocationFileName char array back to a std::string
// *************************************************************
{
  std::string FacetLocationSpec=fFacetLocationFileName;
  return FacetLocationSpec;
}
// ***************************************************************************

KSTeData::KSTeData()
{
  //nothing to do
}
// ******************************************************************


KSTeData::~KSTeData()
{
  //nothing to do
}
void KSTeData::PrintTe()
{
  //nothing to do
}

// ***************************************************************************

