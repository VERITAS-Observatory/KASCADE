//Wrapper for GHS MultipleCoulombScattering class (derived from VVV's 
//Coulomb_Scattering code  from his chesss.for) as needed by kascade.
//This wrapper allows us to call it from fortran code.
//Mainly matches types. But also redirects particle using geom.
//Written by GHS 31/12/04

#include "Random.h"
#include "MultipleCoulombScattering.h"
#include <iostream>
#include <iomanip>

using namespace std;
                                          //Constructor sets up lookup tables
static MultipleCoulombScattering multScatt;

extern "C" void multscatt(float* tenergy, float* tsegment, int* ispec, 
			  float* dl, float* dm, float* dn, int* qz,
			  float* xmass);

//Debug
extern "C" float rndm2(float* xdummy);
extern  RandomNumbers RanP;
//enddebug

//***********************************************************************
// geom is a fortran routine that we call. Its in kascade.for
// **********************************************************************
extern "C" void geom(float* dl,float* dm, float* dn,float* tix);



float rndm2(float* xdummy)
{
  return RanP.Uniform();
}


void multscatt(float* tenergy, float* tsegment, int* ispec, float* dl, 
	       float* dm, float* dn, int* qz,float* xmass)
//************************************************************************
//Wrapper for C++ version of multscatt routine. Uses VVV's 
//Coulomb_Scatter code from his chess.for code. 
//Rewritten by GHS as the C++ class: MultipleCoulombScattering
//This is designed to replace the old fortran kascade.for subroutine multscatt
//which has various problems.

{
  //get the tix angle. Change types as needed
  double Energy= (double) (*tenergy + *xmass);  //Total energy (TeV)
  double path= (double) *tsegment;              //Path length in gm/cm**2
  double mass = (double) *xmass;                //Particle mass in TeV
  int charge = *qz;                             //Particle charge.

  //test tsegment to make sure its positive definite.
  if(path<0)
    {
      cout<<"***multscatt--Tsegment out of range: "<<endl;
      cout<<"***(tenergy,tsegment,ispec,dl,dm,dn): "<<endl;
      cout<<tenergy<<","<<tsegment<<","<<ispec<<","<<dl<<","<<dm<<","<<
	dn<<endl;
      return;
    }
  if(charge==0)
    {               //No multiple scattering: dl,dm,dn remain unchanged
      return;
    }
  
  //Finally: we ready to get the scattering angle(in radians)
  
  double tix=multScatt.getScatteringAngle(Energy, path, charge, mass);

  //Now redirect the direction. This copies original fortran multiscatt code
  
  float Tix= (float)tix;

  //debug
  //  cout<<tix<<endl;
  //  *dl=tix;
  //

  geom(dl,dm,dn,&Tix);    //geom is fortran routine in kascade.for


  return;
}

