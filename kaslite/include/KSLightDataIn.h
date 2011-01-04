//-*-mode:c++; mode:font-lock;-*-
/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSLIGHTDATAIN_H
#define KSLIGHTDATAIN_H

#include "VAAlgorithm.h"
#include "VAConfigurationHelper.h"
#include "VAConfigurationData.h"

#include "KSPeDataClasses.h"

#include "stdint.h"
#include <cmath>
#include <string>
#include <algorithm>



class KSLightDataIn : public VAAlgorithm  //VAAlgoritm specifys a virtual 
		                       //getConfig()
{
private:
  static std::string sDefaultMountType;
  static std::string sDefaultGridType;
  static std::string sDefaultGridOrientation;
  static std::string sDefaultCoreOffsetMode;
  static std::string sDefaultPMTType;
  static std::string sDefaultRandomSeedFileName;
  static double      sDefaultEfficiency;
  static bool        sDefaultBenchmarkFlag;
  static std::string sDefaultExtinctionFileName;
public:
  KSLightDataIn(KSPeHeadData* pfPeHead);
  virtual ~KSLightDataIn();

  virtual VAConfigurationData getConfig() const;
  static void configure(VAConfigInfo& file, VAOptions& command_line);  

  KSPeHeadData*     pfPeHead;
  std::string       fRandomSeedFileName;
  bool fBenchmarkFlag;
  std::string        fExtinctionFileName;
};
#endif

