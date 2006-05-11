/**
 * \class KSSegmentDataClasses
 * \ingroup common
 * \brief File of methods for KSSegmentDataClasses.
 *  Declaration of the data class for the pes
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.


#include "KSSegmentDataClasses.h"
#include "KSKascadeNames.h"
#include <iostream>





#ifdef _NOROOT
extern "C" void MassNumber2ChargeAndMass(int ia, int& qz,double& xmass);
#endif

#ifdef __MAKECINT__
#pragma link C++ class KSSegmentHeadData;
#pragma link C++ class KSSegmentData;
#endif

KSSegmentHeadData::KSSegmentHeadData()
{
  //nothing to do
}
// ***************************************************************************

KSSegmentHeadData::~KSSegmentHeadData()
{
  //nothing to do
}
// ***************************************************************************

void KSSegmentHeadData::PrintSegmentHead()
{
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





}
KSSegmentData::KSSegmentData()
{
  //nothing to do
}
KSSegmentData::~KSSegmentData()
{
  //nothing to do
}
void KSSegmentData::PrintSegment()
{
  //Nothing yet
}
// ***************************************************************************

