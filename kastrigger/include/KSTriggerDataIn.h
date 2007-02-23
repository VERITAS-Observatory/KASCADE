//-*-mode:c++; mode:font-lock;-*-
/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSTRIGGERDATAIN_H
#define KSTRIGGERDATAIN_H

#include "VAAlgorithm.h"
#include "VAConfigurationHelper.h"
#include "VAConfigurationData.h"

#include "stdint.h"
#include <cmath>
#include <string>
#include <algorithm>
#include "KSTeDataClasses.h"
#include "KSCommon.h"

class KSTriggerDataIn : public VAAlgorithm  //VAAlgoritm specifys a virtual 
		                       //getConfig()
{
private:
  static std::string sDefaultCameraType;
  static std::string sDefaultTraceEnable;
  static std::string sDefaultDriftingGammas;
  static std::string sDefaultGammas2D;
  static std::string sDefaultUseElevationForDlDmDn;
  static std::string sDefaultMultipleMountDirections;
  static std::string sDefaultLoadMountDirectionsFromFile;
  static std::string sDefaultSaveMountDirectionsToFile;
  static std::string sDefaultMountDirectionsFileName;
  static std::string sDefaultRandomSeedFileName;
  static double      sDefaultNoiseRate;	
  static double      sDefaultDiscCoincidenceWidthNS;
  static double      sDefaultEfficiency;
  static double      sDefaultDiscriminatorThresholdPes;
  static double      sDefaultMaximumThetaDeg;	  
  static int         sDefaultNumDirections;	
  static int         sDefaultPatternTriggerLevel;
  static int         sDefaultTriggerMultiplicity; 
  static double      sDefaultLightConeConcentration; 
  static double      sDefaultMountDl;                
  static double      sDefaultMountDm;                
  static double      sDefaultMountDn;                
  static double      sDefaultMountElevationDeg;      
  static double      sDefaultGammaStepSizeDeg;
public:
  KSTriggerDataIn(KSTeHeadData* pfTeHead);
  virtual ~KSTriggerDataIn();

  virtual VAConfigurationData getConfig() const;
  static void configure(VAConfigInfo& file, VAOptions& command_line);  

  KSTeHeadData*     pfTeHead;
  bool   fUseElevationForDlDmDn;    //Use the elevation value to determine 
                                    //mount dl.dm,dn  
  bool   fUsePatternTrigger;        //Use patttern trigger for trigger 
                                    //decision
  double fMountElevationDeg;        //Mount elevation in deg -> 
                                    // fUseElevationForDlDmDn
  double fGammaStepSizeRad;         // Drifted and 2d Gamma step size
  bool fLoadMountDirectionsFromFile;
  bool fSaveMountDirectionsToFile;
  std::string       fMountDirectionFileName;
  std::string       fRandomSeedFileName;
};
#endif

