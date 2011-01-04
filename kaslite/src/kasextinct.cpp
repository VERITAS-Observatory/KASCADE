//Wrapper for Extiontion file  class  as needed by kaslite (fortran callable).
//Mainly matches types.

#include <string>
#include "KSExtinctionFile.h"
using namespace std;

static KSExtinctionFile extinctFile;

extern "C" void initExtinction(string extinctionFileName);
extern "C" string getExtinctionFileName();
extern "C" float getextinction(int* pAltitude, int* pLambdaNM);

void initExtinction(string extinctionFileName)
// ***************************************************
// Initalize for the selected atmosphere.
// ***************************************************
{
  extinctFile.readFile(extinctionFileName);
  return;
}
// ***************************************************

string getExtinctionFileName()
// *************************************************************
//  Return the title of the atmosphere: 
// *************************************************************
{
  string extinctionFileName=extinctFile.fExtinctionFileName;
  return extinctionFileName;
}

// *************************************************************
// The following is a wrappers for calls from fortran, thus the arguments
// are pointers.
// **************************************************************


//altitude in meters,  wavelength in nm
float getextinction(int* pAltitude, int* pLambdaNM)
{
  float e=(float) extinctFile.getExtinction(pAltitude, pLambdaNM);
  return e;
}
