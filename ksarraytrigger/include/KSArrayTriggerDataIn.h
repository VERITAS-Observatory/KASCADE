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

#include "stdint.h"
#include <cmath>
#include <string>
#include <algorithm>

#include "VAAlgorithm.h"
#include "VAConfigurationHelper.h"
#include "VAConfigurationData.h"
#include "VACommon.h"


#include "KSCommon.h"

enum KSArrayTriggerDataType  {VBFFILE=0, ROOTFILE=1};

class KSArrayTriggerDataIn : public VAAlgorithm  
			     //VAAlgoritm specifys a virtual 
		                       //getConfig()
{
private:
  static std::string sDefaultDataType;

  static std::string sDefaultT1FileName;
  static std::string sDefaultT2FileName;
  static std::string sDefaultT3FileName;
  static std::string sDefaultT4FileName;

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
    
  KSArrayTriggerDataType fDataType;

  std::vector<std::string> fFileName;
  std::string fRandomSeedFileName;
};
#endif

