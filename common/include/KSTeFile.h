/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 * 
 **/
// This is a class that mainpulate the Kascade binary segment file.

#ifndef KSTEFILE_H
#define KSTEFILE_H


#include <string>
#include <iostream>
#include <fstream>
#include <vector>


#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"
#include "KSTeDataClasses.h"
#include "KSPixel.h"
#include "KSMountDirection.h"

// *******************************************************
// ** PE File class **
// *******************************************************
class KSTeFile
{
 public:
  KSTeFile();
  virtual ~KSTeFile();
  bool Create(std::string TeFileName);
  bool Open(std::string TeFileName);
  void Close();

  void WriteSegmentHead(KSSegmentHeadData* segHead);
  void WritePeHead(KSPeHeadData* peHead);
  void WriteTeHead(KSTeHeadData* teHead);
  void WriteMountDirections(KSMountDirection* pMountDir);
  void WriteTe(KSTeData* te);
  void WriteTePixelData(std::vector<KSPixel>& fPixel);
  double getNumTes(){return fNumTe;};

  bool ReadSegmentHead(KSSegmentHeadData* segHead);
  bool ReadPeHead(KSPeHeadData* peHead);
  bool ReadTeHead(KSTeHeadData* teHead);
  bool ReadMountDirections(KSMountDirection* pMountDir);
  bool ReadTe(KSTeData* te);
  bool ReadTePixelData(std::vector<KSPixel>& fPixel);

  bool foundEOF(){return fFoundEOF;};
  bool foundReadError(){return fFoundError;};

 private:
  float getMinPeTime(std::vector<KSPixel>& fPixel);

  std::ifstream* pfInFile;
  std::ofstream* pfOutFile;
  bool fSegmentHeadWritten;
  bool fSegmentHeadRead;
  bool fPeHeadWritten;
  bool fPeHeadRead;
  bool fTeHeadWritten;
  bool fTeHeadRead;

  bool fFirstTeRead;
  bool fFirstTeWritten;
  bool fTeRead;
  bool fTeWritten;

  bool fFoundEOF;
  bool fFoundError;
  double fNumTe;
};
// ***************************************************************************


#endif
