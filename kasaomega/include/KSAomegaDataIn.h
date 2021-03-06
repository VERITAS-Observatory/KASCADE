//-*-mode:c++; mode:font-lock;-*-
/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSAOMEGADATAIN_H
#define KSAOMEGADATAIN_H

#include "stdint.h"
#include <cmath>
#include <string>
#include <algorithm>

#include "VAAlgorithm.h"
#include "VAConfigurationHelper.h"
#include "VAConfigurationData.h"
#include "VACommon.h"

#include "KSCommon.h"

class KSAomegaDataIn : public VAAlgorithm  //VAAlgoritm specifys a virtual 
		                       //getConfig()
{
private:
  static std::string sDefaultVBFFileName;
  static std::string sDefaultRootFileName;
  static std::string sDefaultPixelStatsRootFileName;
  static std::string sDefaultRandomSeedFileName;
  static std::string sDefaultSimulationConfigFileName;
  //  static std::string sDefaultMountDirectionsFileName;

  static std::string sDefaultRelativeGains;   //These have "ON" or "OFF" values
  static std::string sDefaultRelativePedVars;
  static std::string sDefaultBadPixelSupression;
  static std::string sDefaultTelescope;

  static int         sDefaultNewPatternTriggerLevel;
  static int         sDefaultNewTriggerMultiplicity;   
  static int         sDefaultNewNumPixelsInTrigger; 
  static double      sDefaultNewADCGateWidthNS;
  static double      sDefaultNewDiscriminatorThresholdPes;
  static double      sDefaultNewNoiseRate;	
  static double      sDefaultNoiseRateSigma;	
  static double      sDefaultNewEfficiency;
  static double      sDefaultNewLightConeConcentration; 
  static double      sDefaultDigitalCountsPerPE; 
  static int         sDefaultRunNumber;
  static double      sDefaultSinglePeRiseTimeNS;
  static double      sDefaultSinglePeFallTimeNS;
public:
  KSAomegaDataIn();
  virtual ~KSAomegaDataIn();

  virtual VAConfigurationData getConfig() const;
  static void configure(VAConfigInfo& file, VAOptions& command_line);  
  void Print();


  bool fUseRelativeGains;   
  bool fUseRelativePedVars;
  bool fUseBadPixelSupression;

  int         fNewPatternTriggerLevel;
  int         fNewTriggerMultiplicity; 
  int         fNewNumPixelsInTrigger; 
  double      fNewADCGateWidthNS;
  double      fNewDiscriminatorThresholdPes;
  double      fNewNoiseRate;	
  double      fNoiseRateSigma;	
  double      fNewLightConeConcentration;
  double      fNewEfficiency;
  double      fDigitalCountsPerPE; 
  int         fRunNumber;
  VATelID     fTelescope;
  double      fSinglePeRiseTimeNS;
  double      fSinglePeFallTimeNS;
  
  std::string fVBFFileName;
  std::string fRootFileName;
  std::string fPixelStatsRootFileName;
  std::string fRandomSeedFileName;
  std::string fSimulationConfigFileName;
  //  std::string fMountDirectionFileName;  
};
#endif

