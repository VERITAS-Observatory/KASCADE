/**
 * \class KSRootPeDataClasses
 * \ingroup common
 * \brief File of methods for KSRootPeDataClasses.
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

#include "KSRootPeDataClasses.h"

#ifdef __MAKECINT__
#pragma link C++ class KSRootPeHeadData;
#pragma link C++ class KSRootPeData;
#endif

#ifndef _NOROOT
   ClassImp(KSRootPeHeadData);
   ClassImp(KSRootPeData);
#endif



KSRootPeHeadData::KSRootPeHeadData()
{
  //nothing to do
}
// ***************************************************************************

KSRootPeHeadData::KSRootPeHeadData(KSPeHeadData* pfPeHead)
{
  fXAreaWidthM      = pfPeHead->fXAreaWidthM;
  fYAreaWidthM      = pfPeHead->fYAreaWidthM;
  fXCoreOffsetM     = pfPeHead->fXCoreOffsetM;
  fYCoreOffsetM     = pfPeHead->fYCoreOffsetM;
  fEfficiency       = pfPeHead->fEfficiency;
  fWhippleMount     = pfPeHead->fWhippleMount;
  fVeritasMount     = pfPeHead->fVeritasMount;
  fTriangularGrid   = pfPeHead->fTriangularGrid;
  fSquareGrid       = pfPeHead->fSquareGrid;
  fNorthSouthGrid   = pfPeHead->fNorthSouthGrid;
  fRandomCoreOffset = pfPeHead->fRandomCoreOffset; 
  fWhipplePMTs      = pfPeHead->fWhipplePMTs;
  fVeritasPMTs      = pfPeHead->fVeritasPMTs;
  fUpgradePMTs          = pfPeHead->fUpgradePMTs;
  //for(int i=0;i<10;i++)
  //  {
  //    fVersion[i]   = pfPeHead->fVersion[i];
  //  }
}
// ***************************************************************************

KSRootPeHeadData::~KSRootPeHeadData()
{
  //nothing to do
}
// ***************************************************************************


void KSRootPeHeadData::PrintPeHead()
// ***************************************************************************
//	Print parameters
// ***************************************************************************
{
  std::cout<<"Pe Head:"<<std::endl;
  std::cout<<"  Grid Dimensions(meters) X: "<<fXAreaWidthM<<", Y: "
	   <<fYAreaWidthM<<std::endl;
  std::cout<<"              Whipple Mount: "<<fWhippleMount<<std::endl;
  std::cout<<"              Veritas Mount: "<<fVeritasMount<<std::endl;
  std::cout<<"            Triangular Grid: "<<fTriangularGrid<<std::endl;
  std::cout<<"                Square Grid: "<<fSquareGrid<<std::endl;
  std::cout<<"           North-South Grid: "<<fNorthSouthGrid<<std::endl;
  std::cout<<"               Whipple PMTs: "<<fWhipplePMTs<<std::endl;
  std::cout<<"               Veritas PMTs: "<<fVeritasPMTs<<std::endl;
  std::cout<<"               Upgrade PMTs: "<<fUpgradePMTs<<std::endl;
  std::cout<<"         Random Core Offset: "<<fRandomCoreOffset<<std::endl;
  std::cout<<"   Shower impact point at X: "<<fXCoreOffsetM<<",  Y: "
	   <<fYCoreOffsetM<<std::endl;
  std::cout<<" Efficiency (% photns kept): "<<fEfficiency<<std::endl;
  return;
}
// **************************************************************************

KSRootPeData::KSRootPeData()
{
  //nothing to do
}
// ***************************************************************************

KSRootPeData::~KSRootPeData()
{
  //nothing to do
}
// ***************************************************************************

void KSRootPeData::PrintPe()
{
  //nothing to do
}
// ***************************************************************************


void KSRootPeData::CopyInPe(KSPeData* pfPe)
{
  fNx               = pfPe->fNx;           
  fNy               = pfPe->fNy;
  fTime             = pfPe->fTime;            
  fPhotonDl         = pfPe->fPhotonDl;      
  fPhotonDm         = pfPe->fPhotonDm;      
  fSegmentID        = pfPe->fSegmentID;    
  fX                = pfPe->fX;               
  fY                = pfPe->fY;               
  fTrackType        = pfPe->fTrackType;    
  fLambda           = pfPe->fLambda;       
  fEmissionAltitude = pfPe->fEmissionAltitude;
  return;
}
// ***************************************************************************


void KSRootPeData::CopyOutPe(KSPeData* pfPe)
{
  pfPe->fNx               =fNx;           
  pfPe->fNy               =fNy;
  pfPe->fTime             =fTime;            
  pfPe->fPhotonDl         =fPhotonDl;      
  pfPe->fPhotonDm         =fPhotonDm;      
  pfPe->fSegmentID        =fSegmentID;    
  pfPe->fX                =fX;               
  pfPe->fY                =fY;               
  pfPe->fTrackType        =fTrackType;    
  pfPe->fLambda           =fLambda;       
  pfPe->fEmissionAltitude =fEmissionAltitude;
  return;
}
// ***************************************************************************
