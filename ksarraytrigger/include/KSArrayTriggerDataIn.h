//-*-mode:c++; mode:font-lock;-*-
/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSARRAYTRIGGERDATAIN_H
#define KSARRAYTRIGGERDATAIN_H

#include "VAAlgorithm.h"
#include "VAConfigurationHelper.h"
#include "VAConfigurationData.h"
#include "VACommon.h"

#include "stdint.h"
#include <cmath>
#include <string>
#include <algorithm>
#include "KSCommon.h"

class KSArrayTriggerDataIn : public VAAlgorithm  //VAAlgoritm specifys a virtual 
		                       //getConfig()
{
private:
  static std::string sDefaultT1RootFileName;
  static std::string sDefaultT2RootFileName;
  static std::string sDefaultT3RootFileName;
  static std::string sDefaultT4RootFileName;
  static std::string sDefaultRandomSeedFileName;

  static int         sDefaultArrayTriggerMultiplicity; 
  static double      sDefaultTelescopeEventGateWidthNS;
  static double      sDefaultArrayCoincidenceGateWidthNS;
  static int         sDefaultRunNumber;
public:
  KSArrayTriggerDataIn();
  virtual ~KSArrayTriggerDataIn();

  virtual VAConfigurationData getConfig() const;
  static void configure(VAConfigInfo& file, VAOptions& command_line);  
  void Print();

  int         fArrayTriggerMultiplicity; 
  double      fTelescopeEventGateWidthNS;
  double      fArrayCoincidenceGateWidthNS;
  int         fRunNumber;
    
  std::vector<std::string> fRootFileName;
  std::string fRandomSeedFileName;
};
#endif

