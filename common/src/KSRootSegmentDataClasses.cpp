/**
 * \class KSRootSegmentDataClasses
 * \ingroup common
 * \brief File of methods for KSRootSegmentDataClasses.
 *  Declaration of the data class for the pes
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.


#include "KSRootSegmentDataClasses.h"
#include "KSKascadeNames.h"
#include <iostream>





#ifdef _NOROOT
extern "C" void MassNumber2ChargeAndMass(int ia, int& qz,double& xmass);
#endif

#ifdef __MAKECINT__
#pragma link C++ class KSRootSegmentHeadData;
#pragma link C++ class KSRootSegmentData;
#endif

#ifndef _NOROOT
   ClassImp(KSRootSegmentHeadData);
   ClassImp(KSRootSegmentData);
#endif

KSRootSegmentHeadData::KSRootSegmentHeadData()
{
  //nothing to do
}
// ***************************************************************************

KSRootSegmentHeadData::KSRootSegmentHeadData(KSSegmentHeadData* pfSegHead)
{
  //Copy over header.
  fType                        = pfSegHead->fType;
  fGeVEnergyPrimary            = pfSegHead->fGeVEnergyPrimary;
  fDlInitial                   = pfSegHead->fDlInitial;		 
  fDmInitial                   = pfSegHead->fDmInitial;		 
  fDnInitial                   = pfSegHead->fDnInitial;             
  fEnergyThresholdMeV          = pfSegHead->fEnergyThresholdMeV;
  fMaxCoulombScatSegmentLength = pfSegHead->fMaxCoulombScatSegmentLength;
  fInjectionDepth              = pfSegHead->fInjectionDepth; 
  fObservationAltitudeM        = pfSegHead->fObservationAltitudeM; 
  fShowerID                    = pfSegHead->fShowerID;	       
  for(int i=0;i<10;i++)
    {
      fEarthsMagneticFieldSpec[i]=pfSegHead->fEarthsMagneticFieldSpec[i];
    }
  for(int i=0;i<3;i++)
    {
      fFunctionEnableFlags[i]=1;
    }
  // for(int i=0;i<80;i++)
  //  {
  //    fVersion[i]=pfSegHead->fVersion[i];	 
  //  }
}
// ***************************************************************************

KSRootSegmentHeadData::~KSRootSegmentHeadData()
{
  //nothing to do
}
// ***************************************************************************

void KSRootSegmentHeadData::PrintSegmentHead()
// ***************************************************************************
//	Print parameters
// ***************************************************************************
//   Add Heavy Nuclei capability
// ***************************************************************************
{
  std::cout<<"Segment Head:"<<std::endl;
  if(fType > 20)
    {
      int fKascadeHeavyType=fType-20;
#ifdef _NOROOT
      int fZNuclei;
      double fXMass;
      MassNumber2ChargeAndMass(fKascadeHeavyType,fZNuclei,fXMass);
      std::cout<<"       Itype: Type code for primary particle  ="
	       <<fType<<"     "<<fNucleiNames[fZNuclei-1]
	       <<"("<<fKascadeHeavyType<<")"<<std::endl;
#else
      std::cout<<" Itype: Type code for Heavy primary particle  ="
	       <<fType<<" Atomic Number:" <<fKascadeHeavyType<<std::endl;
#endif
    }
  else
    {
      std::cout<<"       Itype: Type code for primary particle  ="
	       <<fType<<"     "<<fNameType[fType-1]
	       <<std::endl;
    }
  
  std::cout<<"                     ID number of the Shower  ="
	   <<fShowerID<<std::endl;
  std::cout<<"                         Primary energy  TEV  ="
	   <<fGeVEnergyPrimary/1000.<<std::endl;
  std::cout<<"      Dli,Dmi,Dni: Direction cosigns primary  ="
	   <<fDlInitial<<","<<fDmInitial<<","
	   <<fDnInitial<<std::endl;
  
  std::cout<<" Thresh energy(MEV) mus gammas and electrons  ="
	   <<fEnergyThresholdMeV<<std::endl;
  std::cout<<"               Observatory altitude (meters)  ="
	   <<fObservationAltitudeM<<std::endl;
  std::cout<<"   Maximum segment length (gm/cm**2) for MCS  ="
	   <<fMaxCoulombScatSegmentLength<<std::endl;
  if(fMaxCoulombScatSegmentLength<.00245)
    {
      std::cout<<"WARNING--***********************************************"
	       <<std::endl;
      std::cout<<"WARNING--For Tmax < .00245 gm/cm**2, multiple scattering"
	       <<std::endl;
      std::cout<<"WARNING--becomes inaccurate."<<std::endl;
      std::cout<<"WARNING--For Tmax < .001   gm/cm**2 Program will run but" 
	       <<std::endl;
      std::cout<<"WARNING--multiple scattering is disabled"
	       <<std::endl;
      std::cout<<"WARNING--***********************************************"
	       <<std::endl;
      
    }
  std::cout<<"            Injection depth gm/cm2 from top   ="
	   <<fInjectionDepth<<std::endl;
  std::cout<<"                    Magnet field values for   ="
	   <<fEarthsMagneticFieldSpec[0]<<std::endl;
  std::cout<<"      FunctionEnableFlags:"<<std::endl;
  std::cout<<"                              MagneticField   ="
	   <<fFunctionEnableFlags[0]<<std::endl;
  std::cout<<"                                 Ionization   ="
	   <<fFunctionEnableFlags[1]<<std::endl;
  std::cout<<"                 Multiple Coulmb Scattering   ="
	   <<fFunctionEnableFlags[2]<<std::endl;
  return;
}
// **************************************************************************

KSRootSegmentData::KSRootSegmentData()
{
  //nothing to do
}
// ***************************************************************************

KSRootSegmentData::~KSRootSegmentData()
{
  //nothing to do
}
// ***************************************************************************

void KSRootSegmentData::PrintSegment()
{
  //Nothing yet
}
// ***************************************************************************

