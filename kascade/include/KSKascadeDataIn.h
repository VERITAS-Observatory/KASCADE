//-*-mode:c++; mode:font-lock;-*-
/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSKASCADEDATAIN_H
#define KSKASCADEDATAIN_H

#include "VAAlgorithm.h"
#include "VAConfigurationHelper.h"
#include "VAConfigurationData.h"

#include "KSSegmentDataClasses.h"

//JOHN WRIGHT
#include "KSCommon.h"

#include "stdint.h"
#include <cmath>
#include <string>


class KSKascadeDataIn : public VAAlgorithm  //VAAlgoritm specifys a virtual 
		                            //getConfig()
{
private:
  static int               sDefaultType;
  static double            sDefaultGeVEnergyPrimary;
  static double            sDefaultDlInitial;
  static double            sDefaultDmInitial;

  //JOHN WRIGHT
  static double            sDefaultAzInitialDeg;
  static double            sDefaultZenithInitialDeg;

  static double            sDefaultEnergyThresholdMeV;
  static double            sDefaultMaxCoulombScatSegmentLength;
  static double            sDefaultInjectionDepth;
  static double            sDefaultObservationAltitudeM;
  static int               sDefaultShowerID;
  static std::string       sDefaultEarthsMagneticFieldSpec;
  static std::string       sDefaultRandomSeedFileName;
  static std::vector<bool> sDefaultParticleTraceEnableFlags;
  static std::vector<bool> sDefaultFunctionEnableFlags;
  //  static int         sDefaultParticleTraceEnableFlags[20];
  
public:
  KSKascadeDataIn(KSSegmentHeadData* SegmentHead);
  virtual ~KSKascadeDataIn();
  virtual VAConfigurationData getConfig() const;
  static void configure(VAConfigInfo& file, VAOptions& command_line);  

  KSSegmentHeadData* pfSegmentHead;
  std::string        fRandomSeedFileName;
  int       fParticleTraceEnableFlags[20];

};
#endif

