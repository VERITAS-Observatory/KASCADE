/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class for the PST trigger.


#ifndef KSPST_H
#define KSPST_H

#include <iostream>
#include <iomanip>
#include <vector>
#include <bitset>
#include <algorithm>   //for std::sort.
#include <string>
#include <fstream>      //for input binary pst pattern file
#include <set>

#include "KSCommon.h"
#include "KSCameraGrid.h"


const int kNumPatchesPerModule=  5;
const int kNumPixelsPerPatch  = 19;
                              //{VERITAS499,WHIPPLE490}
const int kNumModules[2]      = {        19,        13};
const int kNumPatches[2]      = {5*(19-1)+1,5*(13-1)+1};  //91,61

// ***********************************************************************
// Now stuff for the PST/pixel/patch processing:
// ***********************************************************************
//---- pixel position for each of 19 bits in a patch 
const int kPatchU[19]={ 0,-1,-2,-2,-2,+1, 0,-1,-1,-1,+2,+1,0, 0, 0,+2,1,+1,+2};
const int kPatchV[19]={-2,-1, 0,+1,+2,-2,-1, 0,+1,+2,-2,-1,0,+1,+2,-1,0,+1, 0};
//---- patch position within module for each of 5 patches  
const int kMiddlePatchU[5] = {-4, -2, 0, +2, +4};
const int kMiddlePatchV[5] = {0,  0, 0,  0,  0 };

//---- description of modules 

// 499 camera
//---- 499position of module centre (U and V) (channel 13 of patch 3) 
const int k499U[19]={4,+6,+6,+6,+6,+6,+6,0,-2,-4,-6,-8,-10,-6,-4,-2, 0,+2, +4};
const int k499V[19]={0,-6,-4,-2, 0,+2,+4,6,+6,+6,+6,+6, +6, 0,-2,-4,-6,-8,-10};
//499module rotation     <+60 deg >,         <-60 deg>        < 180 deg >
const int k499A[19] ={1,+1,+1,+1,+1,+1,+1, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1};
const int k499B[19] ={0,+1,+1,+1,+1,+1,+1,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0};
const int k499C[19] ={0,-1,-1,-1,-1,-1,-1,+1,+1,+1,+1,+1,+1, 0, 0, 0, 0, 0, 0};
const int k499D[19] ={1, 0, 0, 0, 0, 0, 0,+1,+1,+1,+1,+1,+1,-1,-1,-1,-1,-1,-1};

// 331 camera
//---- 331position of module centre (channel 13 of patch 3) 
const int k331U[13] = {+4,+4,+4,+4,+4,-2, 0,+2,+4,-2,-4,-6,-8};
const int k331V[13] = { 0,-2, 0,+2,+4,-2,-4,-6,-8,+4,+4,+4,+4};
//331module rotation        <+60 deg >, < 180 deg >,  <-60 deg>
const int k331A[13] = { 1,+1,+1,+1,+1,-1,-1,-1,-1, 0, 0, 0, 0};
const int k331B[13] = { 0,+1,+1,+1,+1, 0, 0, 0, 0,-1,-1,-1,-1};
const int k331C[13] = { 0,-1,-1,-1,-1, 0, 0, 0, 0,+1,+1,+1,+1};
const int k331D[13] = { 1, 0, 0, 0, 0,-1,-1,-1,-1,+1,+1,+1,+1};


class KSPixelTimes
{
 public:
  virtual ~KSPixelTimes();
  double fTime;
  int fIndex;
  inline bool operator < (const KSPixelTimes& o) const;//This for use with sort
};

inline bool KSPixelTimes::operator < (const KSPixelTimes& o) const
{
  return fTime<o.fTime;
}

class KSPixelUV
{
  // ***********************************************************************
  // KSPixelUV fakes an 2 dim int array, NumUVIndex by NumUVIndex with the
  // lower bound of each dimentions being StrartUVIndex;
  // Normaly StartUVIndex is -12 and NumUVIndex is 25.
  // The array is held in a vector of vectors as usual for multi dimen arrays
  // ***********************************************************************
 public:
  KSPixelUV(int StartIndex, int fNumUVIndex);
  int  get(int U,int V){return fPixelUV.at(U-fStartIndex).at(V-fStartIndex);};
  void set(int U, int V, int Pixel)
  {fPixelUV.at(U-fStartIndex).at(V-fStartIndex)=Pixel; return;};
private:
  std::vector< std::vector<int> > fPixelUV;
  int fStartIndex;
};
// *************************************************************************



class KSPST
// *******************************************************
// ** PST class for KASACADE
// *******************************************************
{
 public:
  KSPST(KSCameraTypes CameraType, int TriggerMultiplicity, 
	int NumPixelsInTrigger);
  virtual ~KSPST();
 
  bool isTriggered(std::vector< std::vector < double > >& CFDTrggerTimes, 
		   double& fImageTriggerTime); 
  void buildSummedCFD(std::vector< std::vector < double > >& CFDTrggerTimes, 
		      double CFDpulseWidthNS);
  double findSummedCFDTriggerTime(double& startTimeNS, int Multiplicity); 

  std::vector<bool> fL2TriggerPixels;
  std::vector<bool> fL2TriggerPixelsForPatch;

  double fSummedCFDWaveFormStartNS;

  std::vector< int >* pfCFDTriggerChannels;


 private:
  KSCameraTypes fCameraType;
  int fNumPixelsCamera;
  int fNumPixelsTrigger;
  int fTriggerMultiplicity;

  int fNumPatches;
  std::vector< KSPixelTimes> pfPatchTriggerTimes;

  int* pfPatchTriggerPattern;
  std::vector< std::vector<int>   > pfPixelsInPatch;
  //std::vector< std::vector<KSPixelTimes> > pfPixelsInPatchTimes;
  std::bitset<19> fMemoryAddressBits;
  std::set<int> fCFDList;
  short* pfPSTPatterns;
  int* pfU;
  int* pfV;
  int* pfA;
  int* pfB;
  int* pfC;
  int* pfD;

  std::vector< std::vector< int > > fSummedCFDWaveForm;
  double fSummedCFDWaveFormEndNS;
  std::vector< std::vector <double> > fPatchCFDTriggerTimes;
  std::vector<double> fNullDoubleVector;

  std::set<int>* getSummedCFDSet(double time);
  std::set<int> fCFDSet;
  unsigned long getPatchPattern(int fPatchIndex,
				             std::set<int>* pStrobeTimeCFDs);
  void FillPSTPatterns();
  void FillPixelsInPatch();
  int nearestInt(double fX);
};
// ***************************************************************************


#endif
