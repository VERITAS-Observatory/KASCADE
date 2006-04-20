/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that mainpulate the Kascade binary segment file.

#ifndef KSSEGMENTFILE_H
#define KSSEGMENTFILE_H


#include <string>
#include <iostream>
#include <fstream>


#include "KSSegmentDataClasses.h"

// *******************************************************
// ** SEGMENT File class **
// *******************************************************
class KSSegmentFile
{
 public:
  KSSegmentFile();
  virtual ~KSSegmentFile();
  bool Create(std::string SegFileName);
  bool Open(std::string SegFileName);
  void Close();

  void WriteSegmentHead(KSSegmentHeadData* segHead);
  void WriteSegment(KSSegmentData* segment);

  bool ReadSegmentHead(KSSegmentHeadData* segHead);
  bool ReadSegment(KSSegmentData* segment);
 
 private:
  std::ifstream* pfInFile;
  std::ofstream* pfOutFile;
  bool fSegmentHeadWritten;
  bool fSegmentHeadRead;
};
// ***************************************************************************


#endif
