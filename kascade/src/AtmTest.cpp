//-*-mode:c++; mode:font-lock;-*-
/**
 * \class AtmTest
 * \ingroup
 * \brief This code is used to test the modified kasatm/atmosphere classes
 * they have been modifed by GHS to use the CORSIKA AtmProf tables (or the 
 * original US76)
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

// 27/12/10



#include "stdint.h"
#include <time.h>
#include <string>
#include <iostream>
#include <fstream>
#include <cmath>
#include <iomanip>

using namespace std;

extern "C" float rho(float* pAltitude);//Use of pointers so that these are
extern "C" float gms(float* pAltitude);// fortran callable
extern "C" float yds(float* pDepth);
extern "C" float eta(float* pAltitude, int* pLambdaNM);
extern "C" void initAtmosphere(string atmProfileFile);

int main(int argc, char** argv)
{
  // *********************************************************************
  // Since this is just a test program no need for fancy inputs
  // Outputs can be root TTree::ReadFile()compatable.
  // *********************************************************************
  // Mainly we just want to make sure the curves we produce match whats on
  // http://veritas.sao.arizona.edu/wiki/index.php/Atmosphere#Atmospheric_Profiles_for_VERITAS
  // *********************************************************************
  // First initalize atmosphere
  // *********************************************************************
   // Original atmosphere. Does it still work?
  //initAtmosphere("US76");
  
  //V.Crab.US76.50km
  initAtmosphere("atmprof20.dat");
  
  //V.Winter.US76.50km
  //initAtmosphere("/project/veritas/sembrosk/KASCADE/inputs/atmprof21.dat");

  //V.Summer.US76.50km  
  //initAtmosphere("/project/veritas/sembrosk/KASCADE/inputs/atmprof22.dat");


  // ********************************************************************
  // We are going to make a txt file suaitable for ploting. This file will have
  // the appropriate first line for reading into a root TTree using 
  // TTree::ReadFile()
  // ********************************************************************

  ofstream TestTxt("AtmTest.txt"); 
  TestTxt<<"alt/F:density:thickness:thickAlt:eta"<<endl;
  
  double atmStepsKM=.1;
  double atmTopKM=130.0;
  int numSteps= (atmTopKM/ atmStepsKM) + 1;
    
  
  int wavelength=400; //400 nm.

  for(int i=0;i<numSteps;i++){
    float altKM=i*atmStepsKM;
    float altM=altKM*1000;

    float density=rho(&altM);
    float thickness=gms(&altM);
    float thickAltKM=yds(&thickness)/1000;
    float Eta=eta(&altM,&wavelength);
   
    TestTxt<<std::setw(12)<<altKM
	   <<std::setw(12)<<density
	   <<std::setw(12)<<thickness
	   <<std::setw(12)<<thickAltKM
	   <<std::setw(12)<<Eta
	   <<endl;
  }
  return 0;
}
// **************************************************************************


