//-*-mode:c++; mode:font-lock;-*-
//KSExtinctionFile.cpp
//	This file has has the C++ routines for the ExtinctionFile class
//   Purdue Univ.
//   W.Lafayette, In 47907
//sembroski@physics.purdue.edu
//   01/12/04
//Modified:

#include "KSExtinctionFile.h"
#include <cstdlib>

KSExtinctionFile::KSExtinctionFile()
//Constructor of KSExtinctionFile. 
{
  //Nothing yet.
}
// *******************************************************************
KSExtinctionFile::~KSExtinctionFile()
{
  //Nothing yet
}
// *************************************************************************

void KSExtinctionFile::readFile(string extinctionFileName)
// *******************************************************************
// Read the Extinction text file into a map (for use in interpolation later
// Files are of 2 types. KASCADE original and Corsika. I f clever we may be
// able to read both with same code.
// *******************************************************************
{
  // **********************************************************************
  // Open the file
  // **********************************************************************
  std::cout<<"Reading Extintion file: "<<extinctionFileName<<std::endl;
  fExtinctionFileName=extinctionFileName;

  fIn.open(fExtinctionFileName.c_str());
  if(!fIn){
    cout<<"Fatal-Extinction file "<<fExtinctionFileName
        <<" failed to open!"<<endl;
    exit(EXIT_FAILURE);
  }
  int wavelength=0;
  // *********************************************************************
  // Figure out file format
  // If this line has a ! in it then this is a Corsika file
  // *********************************************************************
  int icount=256;
  string strBuffer;
  char buffer[256];
  getline(fIn,strBuffer);
  
  if(strBuffer.find(',')==string::npos){
    fKASCADEExtinctFile=true;
  }
  else{
    fKASCADEExtinctFile=false;
  }
  fIn.close();
  fIn.open(fExtinctionFileName.c_str());
  
  if(fKASCADEExtinctFile){
    while(1){
      fIn>>wavelength;
      if(fIn.eof()){
	break;
      }
      vector< float >* pExtinctVec = new vector< float >;
      pExtinctVec->clear();
      pExtinctVec->resize(51,0);

      for(int i=0;i<51;i++){
	fIn>>pExtinctVec->at(i);
      }
      fExtinctionMap[wavelength]=pExtinctVec;
    }
    
  }
  else{
    while(1){
       fIn.getline(buffer,icount,',');
       if(fIn.eof()){
	 break;
       }
      strBuffer=buffer;

      istringstream issLambda(strBuffer);
      issLambda>>wavelength;
      fIn.ignore(120,'\n');  //Skip rest of this line (if any)

      vector< float >* pExtinctVec = new vector< float >;
      pExtinctVec->clear();
      pExtinctVec->resize(51,0);

      for(int i=0;i<5;i++){
	for(int j=0;j<10;j++){
	  fIn.getline(buffer,icount,',');
	  strBuffer=buffer;
	  istringstream issExt(strBuffer);
	  float extinct=0;
	  issExt>>extinct;
	  pExtinctVec->at(i*10+j)=extinct;
	}
	fIn.ignore(120,'\n');  //Skip rest of this line (if any)
      }
      fIn.getline(buffer,icount,',');
      strBuffer=buffer;
      istringstream issLast(strBuffer);
      float extinct=0;
      issLast>>extinct;
      pExtinctVec->at(50)=extinct;
      fIn.ignore(120,'\n');  //Skip rest of this line (if any)

      fExtinctionMap[wavelength]=pExtinctVec;
    }
  }
  return;
}
// **********************************************************************

float KSExtinctionFile::getExtinction(int* pAltitudeKM, int* pLambdaNM)
// **********************************************************************
// Using the map read in return the interpolated extinction value. Used to
// fill extinction table in kskaslitemian.for
// **********************************************************************
{
  // *******************************************************************
  // KASCADE extinction file is easy
  // **********************************************************************
  float e=0;
  if(fKASCADEExtinctFile){
    vector< float >* pExtintVec=fExtinctionMap[*pLambdaNM];
    e= pExtintVec->at(*pAltitudeKM);
  }
  else{
    // *********************************************************************
    // Not so easy for CORSIKA tables.
    //1: tables have 200 nm as lowest wavelength
    //2:200-270 nm 5 nm steps (thus thats easy)
    //3:270-400 nm 20 nm steps (interpolate if needed)
    //4:400-700 nm 50 nm steps (interpolate if needed)
    // *********************************************************************
    // Below map: use lowest
    // *********************************************************************
    // Get lowest key
    // *********************************************************************

    // **********************
    // Below map?
    // **********************
    if(fExtinctionMap.begin()->first > *pLambdaNM){
      vector< float >* pExtintVec=fExtinctionMap.begin()->second;
      e=pExtintVec->at(*pAltitudeKM);
    }

    // **********************************************************************
    // Search to see if we have this lambda in table.
    // **********************************************************************
    else if(fExtinctionMap.find(*pLambdaNM)!= fExtinctionMap.end() ){
      vector< float >* pExtintVec=fExtinctionMap.find(*pLambdaNM)->second;
      e=pExtintVec->at(*pAltitudeKM);
    }
 
    // *********************************************************************
    // Got to search and interpolate.  Do it the brute force way. its fast 
    // enough for this application.
    // *********************************************************************
    else{
      map<int,vector<float>*>::iterator pos;
      for(pos = fExtinctionMap.begin(); pos != fExtinctionMap.end();++pos){
	if(pos->first>*pLambdaNM){
	  vector< float >* pExtintVec=pos->second;
	  float highE=pExtintVec->at(*pAltitudeKM);
	  float highLambda=(float)pos->first;
	  pos=--pos;
	  pExtintVec=pos->second;
	  float lowE=pExtintVec->at(*pAltitudeKM);
	  float lowLambda=(float)pos->first;
	  float fraction=(*pLambdaNM-lowLambda)/(highLambda-lowLambda);
	  e=lowE+fraction*(highE-lowE);
	  break;
	}
      }
    }
  }	
  return e; 
}
// *************************************************************************  
