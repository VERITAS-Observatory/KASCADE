//-*-mode:c++; mode:font-lock;-*-
// **************************************************************************
// This is a class that reads, stores ind interpolates an extinction file.
// **************************************************************************

#ifndef ATMOSPHERE_H
#define ATMOSPHERE_H

#include <iostream>
#include <fstream>
#include <cmath>
#include <string>
#include <vector>
#include <map>
#include <sstream>
using namespace std;


// *********************************************************************
//Define the class
class KSExtinctionFile
{
 private:
  ifstream fIn;
  map< int,vector<float>*> fExtinctionMap;
  bool fKASCADEExtinctFile;

 public:
  KSExtinctionFile();
  ~KSExtinctionFile();

  void readFile(string extinctionFileName);
  // ******************************************************************
  // Fortran callable methods (thats why we use pointers here)
  // ******************************************************************
  float  getExtinction(int* pAltitude, int* pLambdaNM);
  string getExtinctionFileName(){return fExtinctionFileName;};

  string fExtinctionFileName; 
};
#endif



