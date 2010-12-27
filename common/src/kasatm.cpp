//Wrapper for atmosphere class  as needed by kascade (fortran callable).
//Mainly matches types.

#include <string>
#include "atmosphere.h"
using namespace std;

static atmosphere atm;

extern "C" float rho(float* pAltitude);//Use of pointers so that these are
extern "C" float gms(float* pAltitude);// fortran callable
extern "C" float yds(float* pDepth);
extern "C" float eta(float* pAltitude, int* pLambdaNM);


extern "C" void initAtmosphere(string atmProfileFile);

void initAtmosphere(string atmProfileFile)
// ***************************************************
// Initalize for the selected atmosphere.
// ***************************************************
{
  atm.init(atmProfileFile);
  return;
}
// ***************************************************

// *************************************************************
// The following are wrappers for  calls from fortran, thus the arguments
// are pointers.
// **************************************************************

float rho(float* pAltitude)
  //altitude in meters,  density gm/cm**3
{
  float r = (float) atm.getDensity(pAltitude);
  return r;
}

float gms(float* pAltitude)
  //altitude in meters,  depth gm/cm**2
{
  float g =(float) atm.getDepth(pAltitude);
  return g;
}

 //altitude in meters,  depth gm/cm**2
float yds(float* pDepth)
{
  float y=(float) atm.getAltitudeM(pDepth);
  return y;
}

 //altitude in meters,  wavelength in nm
float eta(float* pAltitude, int* pLambdaNM)
{
  float e=(float) atm.getEta(pAltitude, pLambdaNM);
  return e;
}
