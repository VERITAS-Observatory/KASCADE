//Wrapper for atmosphere76 as needed by kascade.
//Mainly matches types.

#include "atmosphere76.h"
using namespace std;

static atmosphere76 atm76;

extern "C" float rho(float* altitude);
extern "C" float gms(float* altitude);
extern "C" float yds(float* depth);
extern "C" void printatmversion();

void  printatmversion()
{
  std::cout<<"Using atmosphere76 implementation of US Standard Atmosphere 76"
	   <<std::endl;
  return;
}



float rho(float* altitude)
  //altitude in meters,  density gm/cm**3
{
  double alt= (double) *altitude;
  float r = (float) atm76.getDensity(alt);
  return r;
}

float gms(float* altitude)
  //altitude in meters,  depth gm/cm**2
{
  //cout<<" In GMS wrapper"<<endl;
  double alt=(double) *altitude;
  float g =(float) atm76.getDepth(alt);
  //cout<<"alt: "<<alt<<" gms:"<<g<<endl;
  return g;
}

 //altitude in meters,  depth gm/cm**2
float yds(float* depth)
{
  double dpth=(double) *depth;
  float y=(float) atm76.getAltitude(dpth);
  return y;
}

