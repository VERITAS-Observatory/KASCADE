/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSArrayVBFFile_H
#define KSArrayVBFFile_H


#include <stdlib.h>
#include <iostream>
#include <vector>
#include <string>
#include <cmath>

#include <VBF/VBankFileReader.h>
#include <VBF/VBankFileWriter.h>
#include <VBF/VPacket.h>
#include <VBF/VArrayEvent.h>
#include <VBF/VDatum.h>
#include <VBF/VKascadeSimulationData.h>
#include <VBF/VKascadeSimulationHeader.h>
#include <VBF/VEventType.h>

#include "KSTelescope.h"
#include "VACommon.h"

// declare that we're using the VConfigMaskUtil namespace, which gives us
// easy access to parseConfigMask().
// include the configuration mask utilities, which give us parseConfigMask()
#include <VBF/VConfigMaskUtil.h>
using namespace VConfigMaskUtil;


class KSArrayVBFFile
// *******************************************************
// ** File class for creating and filling records for VERITAS
// ** Array VBF files for multi telescope runs
// *******************************************************
{
 public:
  KSArrayVBFFile(std::vector< KSTelescope* >& pArray);
  virtual ~KSArrayVBFFile();

  void CreateVBFFile(std::string fFileName, uint fRunNumber);
  void CopyOutHeaderPacket();

  VBankFileWriter* getVBFFilePtr(){return pfWriter;};

 private:
  std::vector< KSTelescope* > pfArray;
  VBankFileWriter* pfWriter;
  int fNumTels;
};
// ***************************************************************************




#endif
