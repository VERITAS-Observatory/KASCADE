/**
 * \class KSPeDataClasses
 * \ingroup common
 * \brief File of methods for KSPeDataClasses.
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

#include "KSPeDataClasses.h"

#ifdef __MAKECINT__
#pragma link C++ class KSPeHeadData;
#pragma link C++ class KSPeData;
#endif

KSPeHeadData::KSPeHeadData()
{
  //nothing to do
}
KSPeHeadData::~KSPeHeadData()
{
  //nothing to do
}
void KSPeHeadData::PrintPeHead()
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
  std::cout<<"                   ADP PMTs: "<<fADPPMTs<<std::endl;
  std::cout<<"         Random Core Offset: "<<fRandomCoreOffset<<std::endl;
  std::cout<<"   Shower impact point at X: "<<fXCoreOffsetM<<",  Y: "
	   <<fYCoreOffsetM<<std::endl;
  std::cout<<" Efficiency (% photons kept): "<<fEfficiency<<std::endl;
  std::cout<<"        Extinction File Name: "<<fExtinctionFileName<<std::endl;


  //std::cout<<"             ksLight version: "<<fVersion<<std::endl;
  return;
}
// **************************************************************************
bool KSPeHeadData::setExtinctionFileName(std::string extSpec)
// *********************************************************
// Save file specification  in pe header
// *********************************************************
{
  int numChar=extSpec.size();
  if(numChar+1> kNumCharExtName){
    std::cout<<"ExtinctionFile too long ("<<numChar<<"bytes) to fit "
      "in PeHead char array. Extinction File Name: "<<extSpec
	     <<std::endl;
    return false;
  }

  extSpec.copy(fExtinctionFileName, numChar);
  fExtinctionFileName[numChar]='\0';
  return true;  
}
// **************************************************************************

std::string KSPeHeadData::getExtinctionFileName()
// *************************************************************
// Convert the fExtinctionFileName char array back to a std::string
// *************************************************************
{
  std::string extSpec=fExtinctionFileName;
  return extSpec;
}
// *********************************************************************




// ***************************************************************************


KSPeData::KSPeData()
{
  //nothing to do
}
KSPeData::~KSPeData()
{
  //nothing to do
}
void KSPeData::PrintPe()
{
  //nothing to do
}

// ***************************************************************************

