//-*-mode:c++; mode:font-lock;-*-
/**
 * \class BendTest
 * \ingroup
 * \brief This code is used to test the modified bend and bend_init routines
 * they have been modifed by Qi Feng for Bx !=0
 *
 * Original Author: Glenn H. Sembroski * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

//Written by:
// G.H.Sembroski
//Physics Dept.
//Purdue Univ.
//West Lafayette, In. 479096
//sembrosk@physics.purdue.edu
//765-494-5172

// 07/12/10



#include "stdint.h"
#include <time.h>
#include <string>
#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;

extern "C" void bendit(float* tenergy,double* zpath,int* ispec,float* dl1,
		     float* dm1,float* dn1,int* qz,float* xmass);
extern "C" void bendinitit();

extern "C" void kscharstring2coutit(char* coutString);

int main(int argc, char** argv)
{
  // *********************************************************************
  // Since this is just a test program no need for fancy inputs
  // Outputs can be root TTree::ReadFile()compatable.
  // *********************************************************************
  //Idea is to start a particle and bend it in a circle in a mgnetic field
  // and see if the radius of curvature matches expected.
  // *********************************************************************
  // First set up the inital field
  // *********************************************************************
  bendinitit();

  // ********************************************************************
  //Set up constants for our loop
  // ********************************************************************
  //Energy/type

  float tenergy = 1.e-6; //1 MeV
  int   ispec   = 1;
  float xmass   = 1.e-6; //1 MeV
  int   qz      = 1;

  // ********************************************************************
  //Inital direction: striaght down. 
  // *********************************************************************
  float dl=0.0;
  float dm=0.0;
  float dn=1.0;
  
  // *********************************************************************
  //Initial position
  // *********************************************************************
  double x=0.0;
  double y=0.0;
  double z=-1000.0; //meters


  // **********************************************************************
  //Radius of curvature
  // **********************************************************************
  double BField     = 1.0;        //Gauss
  double etev       = tenergy+xmass;  //Energy of particle in Tev.
  double p_particle = sqrt( (etev*etev)-(xmass*xmass) )*1.e6;      //Momentum in Mev.

  double radius     = p_particle/(0.03*BField*qz);//radius of curvature.
  cout<<"radius(m): "<<radius<<endl;

  double diameter   =  2*3.14159265*radius;  //meters

  // ********************************************************************
  // Propagation loop
  // ********************************************************************
  int numSteps=1000;
  double zpath=2*diameter/numSteps;
  cout<<"Segment length(m): "<<zpath<<endl;

  for(int i=0;i<numSteps;i++){
    x=x+dl*zpath;
    y=y+dm*zpath;
    z=z+dn*zpath;
    cout<<i<<" "<<x<<" "<<y<<" "<<z<<endl;

    bendit(&tenergy,&zpath,&ispec,&dl,&dm,&dn,&qz,&xmass); //bend it a bit
    

  }
  x=x+dl*zpath;
  y=y+dm*zpath;
  z=z+dn*zpath;
  cout<<numSteps+1<<" "<<x<<" "<<y<<" "<<z<<endl;
  return 0;
}
// **************************************************************************


void kscharstring2coutit(char* coutString)
// ***********************************************************************
// Routine to allow fortran routines to print to cout especiall if it has been
// redirected to a file.
// **********************************************************************
{
  std::string ksStr(coutString);
  if(!ksStr.empty())
    {
      std::cout<<ksStr<<std::endl;
    }
  return;
}
// **************************************************************************
