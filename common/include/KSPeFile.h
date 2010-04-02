/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 * 
 **/
// This is a class that mainpulate the Kascade binary segment file.

#ifndef KSPEFILE_H
#define KSPEFILE_H


#include <cstdlib>
#include <string>
#include <iostream>
#include <fstream>
#include <fcntl.h>

#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"

// *******************************************************
// ** PE File class **
// *******************************************************
class KSPeFile
{
 public:
  KSPeFile();
  virtual ~KSPeFile();
  bool Create(std::string PeFileName);
  bool Open(std::string PeFileName);
  void Close();
  void Delete();

  void WriteSegmentHead(KSSegmentHeadData* segHead);
  void WritePeHead(KSPeHeadData* peHead);
  void WritePe(KSPeData* pe);
  double getNumPes(){return fNumPe;};

  bool ReadSegmentHead(KSSegmentHeadData* segHead);
  bool ReadPeHead(KSPeHeadData* peHead);
  bool ReadPe(KSPeData* pe);
  bool foundEOF(){return fFoundEOF;};
  bool foundReadError(){return fFoundError;};

 private:
  //std::ifstream* pfInFile;
  //std::ofstream* pfOutFile;
  std::string fInFileName;
  int pfInFile;
  int pfOutFile;
  bool fSegmentHeadWritten;
  bool fSegmentHeadRead;
  bool fPeHeadWritten;
  bool fPeHeadRead;
  bool fFoundEOF;
  bool fFoundError;
  double  fNumPe;
};
// ***************************************************************************


#endif
