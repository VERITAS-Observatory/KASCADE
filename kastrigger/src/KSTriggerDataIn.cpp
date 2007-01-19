//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSTriggerDataIn
 * \brief Class to manipulate input parameter for kascade wthough use of
 * config files.
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#include <algorithm>

#include "KSTriggerDataIn.h"
#include "KSKascadeNames.h"

// **************************************************************************
std::string KSTriggerDataIn::sDefaultCameraType="WHIPPLE490";
std::string KSTriggerDataIn::sDefaultTraceEnable="OFF";
std::string KSTriggerDataIn::sDefaultDriftingGammas="OFF";
std::string KSTriggerDataIn::sDefaultUseElevationForDlDmDn="OFF";
std::string KSTriggerDataIn::sDefaultMultipleMountDirections="OFF";
std::string KSTriggerDataIn::sDefaultLoadMountDirectionsFromFile="OFF";
std::string KSTriggerDataIn::sDefaultSaveMountDirectionsToFile="OFF";

std::string KSTriggerDataIn::sDefaultMountDirectionsFileName=" ";
std::string KSTriggerDataIn::sDefaultRandomSeedFileName=" ";

double      KSTriggerDataIn::sDefaultNoiseRate=12.5;	
double      KSTriggerDataIn::sDefaultDiscCoincidenceWidthNS=6.5;
double      KSTriggerDataIn::sDefaultEfficiency=1.0;
double      KSTriggerDataIn::sDefaultDiscriminatorThresholdPes=4.0;
double      KSTriggerDataIn::sDefaultMaximumThetaDeg=9.0;
int         KSTriggerDataIn::sDefaultNumDirections=150;	
double      KSTriggerDataIn::sDefaultDriftedGammaStepSizeDeg=.1;
int         KSTriggerDataIn::sDefaultPatternTriggerLevel=3;
int         KSTriggerDataIn::sDefaultTriggerMultiplicity=3; 
double      KSTriggerDataIn::sDefaultLightConeConcentration=1.0; 
double      KSTriggerDataIn::sDefaultMountDl=-1.e-15;                
double      KSTriggerDataIn::sDefaultMountDm=-0.017452212;                
double      KSTriggerDataIn::sDefaultMountDn=-.9998477;                
double      KSTriggerDataIn::sDefaultMountElevationDeg=0.0;      
// **************************************************************************


KSTriggerDataIn::KSTriggerDataIn(KSTeHeadData* thead)
{
  pfTeHead=thead;
  std::transform(sDefaultCameraType.begin(),sDefaultCameraType.end(),
		 sDefaultCameraType.begin(),
		 toupper);
  std::transform(sDefaultTraceEnable.begin(),sDefaultTraceEnable.end(),
		 sDefaultTraceEnable.begin(),
		 toupper);
  std::transform(sDefaultDriftingGammas.begin(),sDefaultDriftingGammas.end(),
		 sDefaultDriftingGammas.begin(),
		 toupper);
  std::transform(sDefaultUseElevationForDlDmDn.begin(),
		 sDefaultUseElevationForDlDmDn.end(),
		 sDefaultUseElevationForDlDmDn.begin(),
		 toupper);
  std::transform(sDefaultMultipleMountDirections.begin(),
		 sDefaultMultipleMountDirections.end(),
		 sDefaultMultipleMountDirections.begin(),
		 toupper);
  std::transform(sDefaultLoadMountDirectionsFromFile.begin(),
		 sDefaultLoadMountDirectionsFromFile.end(),
		 sDefaultLoadMountDirectionsFromFile.begin(),
		 toupper);
  std::transform(sDefaultSaveMountDirectionsToFile.begin(),
		 sDefaultSaveMountDirectionsToFile.end(),
		 sDefaultSaveMountDirectionsToFile.begin(),
		 toupper);
  // -------------------------------------------------------------------
  // Decode selections
  // -------------------------------------------------------------------
  if( sDefaultCameraType=="WHIPPLE490")
    {
      pfTeHead->fCameraType=WHIPPLE490;
    }
  else if(sDefaultCameraType=="VERITAS499")
    {
      pfTeHead->fCameraType=VERITAS499;
    }
  else
    {
      std::cout<<"Illegal option for CameraType: "<<sDefaultCameraType
	       <<" Assuming CameraType=WHIPPLE490"<<std::endl;
      pfTeHead->fCameraType=WHIPPLE490;
    }
  
  pfTeHead->fTraceCreation=false;
  if(sDefaultTraceEnable=="ON")
    {
      pfTeHead->fTraceCreation=true;
    }
  else if(sDefaultTraceEnable!="OFF")
    {
      std::cout<<"Illegal option for TraceEnable: "<<sDefaultTraceEnable
	       <<" Assuming TraceEnable=OFF"<<std::endl;
    }
  
  pfTeHead->fDriftingGammas=false;
  if(sDefaultDriftingGammas=="ON")
    {
      pfTeHead->fDriftingGammas=true;
    }
  else if(sDefaultDriftingGammas!="OFF")
    {
      std::cout<<"Illegal option for DriftingGammas: "<<sDefaultDriftingGammas
	       <<" Assuming DriftingGammas=OFF"<<std::endl;
    }

  fUseElevationForDlDmDn=false;
  if(sDefaultUseElevationForDlDmDn=="ON")
    {
      fUseElevationForDlDmDn=true;
    }
  else if(sDefaultUseElevationForDlDmDn!="OFF")
    {
      std::cout<<"Illegal option for UseElevationForDlDmDn: "
	       <<sDefaultUseElevationForDlDmDn
	       <<" Assuming UseElevationForDlDmDn=OFF"<<std::endl;
    }


  pfTeHead->fMultipleMountDirections=false;
  if(sDefaultMultipleMountDirections=="ON")
    {
      pfTeHead->fMultipleMountDirections=true;
    }
  else if(sDefaultMultipleMountDirections!="OFF")
    {
      std::cout<<"Illegal option for MultipleMountDirections: "
	       <<sDefaultMultipleMountDirections
	       <<" Assuming MultipleMountDirections=OFF"<<std::endl;
    }
  fLoadMountDirectionsFromFile=false;
  if(sDefaultLoadMountDirectionsFromFile=="ON")
    {
      fLoadMountDirectionsFromFile=true;
    }
  else if(sDefaultLoadMountDirectionsFromFile!="OFF")
    {
      std::cout<<"Illegal option for LoadMountDirectionsFromFile: "
	       <<sDefaultLoadMountDirectionsFromFile
	       <<" Assuming LoadMountDirectionsFromFile=OFF"<<std::endl;
    }

  fSaveMountDirectionsToFile=false;
  if(sDefaultSaveMountDirectionsToFile=="ON")
    {
      fSaveMountDirectionsToFile=true;
    }
  else if(sDefaultSaveMountDirectionsToFile!="OFF")
    {
      std::cout<<"Illegal option for SaveMountDirectionsToFile: "
	       <<sDefaultSaveMountDirectionsToFile
	       <<" Assuming SaveMountDirectionsToFile=OFF"<<std::endl;
    }

  if(sDefaultEfficiency>1.0 || sDefaultEfficiency<0.0)
    {
      std::cout<<"ksTrigger: Efficiency out of range(0.0->1.0). Was: "
	       <<sDefaultEfficiency<<" Defaulting to 1.0"<<std::endl;
      sDefaultEfficiency=1.0;
    }
  if(sDefaultLightConeConcentration>1.0 || sDefaultLightConeConcentration<0.0)
    {
      std::cout<<"ksTrigger: LightConeConcentration out of range(0.0->1.0). "
	       <<"Was: "<<sDefaultLightConeConcentration<<" Defaulting to 1.0"
	       <<std::endl;
      sDefaultLightConeConcentration=1.0;
    }

  pfTeHead->fEfficiency                = sDefaultEfficiency;
  pfTeHead->fNoiseRate                 = sDefaultNoiseRate;
  pfTeHead->fDiscCoincidenceWidthNS    = sDefaultDiscCoincidenceWidthNS;
  pfTeHead->fDiscriminatorThresholdPes = sDefaultDiscriminatorThresholdPes;
  pfTeHead->fMaximumThetaRad           = sDefaultMaximumThetaDeg*gDeg2Rad;
  pfTeHead->fNumDirections             = sDefaultNumDirections;

  fDriftedGammaStepSizeRad        = sDefaultDriftedGammaStepSizeDeg*gDeg2Rad;

  pfTeHead->fPatternTriggerLevel       = sDefaultPatternTriggerLevel; 
  if(pfTeHead->fPatternTriggerLevel>=2 ||pfTeHead->fPatternTriggerLevel<=4)
    {
      fUsePatternTrigger=true;
    }
  else
    {
      fUsePatternTrigger=false;
    }
  pfTeHead->fTriggerMultiplicity       = sDefaultTriggerMultiplicity; 
  pfTeHead->fLightConeConcentration    = sDefaultLightConeConcentration;
  pfTeHead->fMountDl                   = sDefaultMountDl;               
  pfTeHead->fMountDm                   = sDefaultMountDm;               
  pfTeHead->fMountDn                   = sDefaultMountDn;               
  fMountElevationDeg         = sDefaultMountElevationDeg;     
  fMountDirectionFileName=sDefaultMountDirectionsFileName;  
  fRandomSeedFileName=sDefaultRandomSeedFileName;  //A ksTriggerDataIn variable
}
// **************************************************************************

KSTriggerDataIn::~KSTriggerDataIn()
{
  // nothing to see here
}


VAConfigurationData KSTriggerDataIn::getConfig() const
{
  VAConfigurationData config;
  config.fName = std::string("KSTriggerData");
  config.setValue("CameraType",sDefaultCameraType);
  config.setValue("TraceEnable",sDefaultTraceEnable);
  config.setValue("DriftingGammas",sDefaultDriftingGammas);
  config.setValue("UseElevationForDlDmDn",sDefaultUseElevationForDlDmDn);
  config.setValue("LoadMountDirectionsFromFile",
		  sDefaultLoadMountDirectionsFromFile);
  config.setValue("SaveMountDirectionsToFile",
		  sDefaultSaveMountDirectionsToFile);
  config.setValue("MultipleMountDirections",sDefaultMultipleMountDirections);
  config.setValue("Efficiency",pfTeHead->fEfficiency);
  config.setValue("NoiseRate",pfTeHead->fNoiseRate);
  config.setValue("DiscCoincidenceWidthNS",pfTeHead->fDiscCoincidenceWidthNS);
  config.setValue("DiscriminatorThresholdPes",
		  pfTeHead->fDiscriminatorThresholdPes);
  config.setValue("MaximumThetaDeg",sDefaultMaximumThetaDeg);
  config.setValue("NumDirections",pfTeHead->fNumDirections);
  config.setValue("TriggerMultiplicity",pfTeHead->fTriggerMultiplicity);
  config.setValue("PatternTriggerLevel",pfTeHead->fPatternTriggerLevel);
  config.setValue("DriftedGammaStepSizeDeg",sDefaultDriftedGammaStepSizeDeg);
  config.setValue("LightConeConcentration",pfTeHead->fLightConeConcentration);
  config.setValue("MountDl",pfTeHead->fMountDl);
  config.setValue("MountDm",pfTeHead->fMountDm);
  config.setValue("MountDn",pfTeHead->fMountDn);
  config.setValue("MountElevationDeg",fMountElevationDeg);
  config.setValue("MountDirectionsFileName",fMountDirectionFileName);
  config.setValue("RandomSeedFileName",fRandomSeedFileName);
  return config;
}

void KSTriggerDataIn::configure(VAConfigInfo& file, VAOptions& command_line)
{
  doVAConfiguration(file, command_line, 
		    "CameraType", sDefaultCameraType,
		    "KSTriggerDataIn",
		    "Camera Type: WHIPPLE490 or VERITAS499. Specifies camera "
		    "structure and telescope parameters such as mirror size "
		    "and focal length. ");
  doVAConfiguration(file, command_line, 
		    "TraceEnable", sDefaultTraceEnable,
		    "KSTriggerDataIn",
		    "ON enables the production of pmt traces for the pixels. "
		    "This is slightly more accurate but much slower.(Not yet "
                    "implemented!) OFF disables (default)");
  doVAConfiguration(file, command_line, 
		    "DriftingGammas",sDefaultDriftingGammas,
		    "KSTriggerDataIn",
		    "ON enables the use of a special Theta/Phi mount "
                    "redirection mode useful for Drift-scan modeling of Gamma "
		    "(signal) events. OFF (default) disables. If this option "
		    "is chosen then you should also set -MaximumThetaDeg=1.0 "
		    "(for example).");
  doVAConfiguration(file, command_line, 
		    "MaximumThetaDeg",sDefaultMaximumThetaDeg,
		    "KSTriggerDataIn",
		    "Random direction of hadrons is modeled by looking for "
		    "triggers when the mount is pointed in a random direction "
		    "less than this value form the nominal direction. Used "
		    "when MultipleMountDirections=ON. Also used when "
		    "DriftingGammas=ON chosen for range of directions.");
  doVAConfiguration(file, command_line, 
		    "DriftedGammaStepSizeDeg",sDefaultDriftedGammaStepSizeDeg,
		    "KSTriggerDataIn",
		    "For DriftedGammas=ON events: Specifies the size of steps "
		    "to take in RA away from the original direction.");
  doVAConfiguration(file, command_line, 
		    "UseElevationForDlDmDn",sDefaultUseElevationForDlDmDn,
		    "KSTriggerDataIn",
		    "ON enables the use of the MountElevationDeg to "
		    "determine MountDirectionXDl, MountDirectionYDm and "
		    "MountDirectionZDn. This is part of DriftingGammas mode.");
  doVAConfiguration(file, command_line, 
		    "MultipleMountDirections",sDefaultMultipleMountDirections,
		    "KSTriggerDataIn",
		    "ON enables the use of random directions within a circle "
		    "of radius MaximumThetaDeg of original Mount direction "
                    "for each event. This models the random arrival "
		    "directions of Cosmic ray showers.");
  doVAConfiguration(file, command_line, 
		    "NumDirections",sDefaultNumDirections,
		    "KSTriggerDataIn",
		    "For non-gamma events: Specifies the number of random "
		    "directions to test for triggers for each event. Used "
		    "only when MultipleMountDirections=ON");
  doVAConfiguration(file, command_line, 
		    "LoadMountDirectionsFromFile",
		    sDefaultLoadMountDirectionsFromFile,
		    "KSTriggerDataIn",
		    "ON enables the reading from a file specfified in "
		    "MountDirectionsFileName the various mount directions. "
		    "This is needed when array events are to be created from "
		    "events from the various telescopes.");
  doVAConfiguration(file, command_line, 
		    "SaveMountDirectionsToFile",
		    sDefaultSaveMountDirectionsToFile,
		    "KSTriggerDataIn",
		    "ON enables the saveing to a file specfified in "
		    "MountDirectionsFileName the various mount directions. "
		    "This is needed when array events are to be created from "
		    "events from the various telescopes(they all need to use "
		    "the same set of directions).");
  doVAConfiguration(file, command_line, 
		    "MountDirectionsFileName",sDefaultMountDirectionsFileName,
		    "KSTriggerDataIn",
		    "File Name for Mount Directions Binary File. See "
		    "LoadMountDirectionsFromFile and "
		    "SaveMountDirectionsToFile options.");
  doVAConfiguration(file, command_line, 
		    "NoiseRate",sDefaultNoiseRate,
		    "KSTriggerDataIn",
		    "Night sky shine rate in pes/deg/ns (after application of "
                    "all efficiency factors)");
  doVAConfiguration(file, command_line, 
		    "DiscCoincidenceWidthNS",sDefaultDiscCoincidenceWidthNS,
		    "KSTriggerDataIn",
		    "Specifies an effective timing width for the addition of "
		    "sky-shine noise to the discriminator signal. For the "
		    "TraceEnable=OFF method.");
  doVAConfiguration(file, command_line, 
		    "DiscriminatorThreshold",sDefaultDiscriminatorThresholdPes,
		    "KSTriggerDataIn",
		    "Specifies a Threshold level for a pixel to fire. Value "
		    "is in units of mean pes level");
  doVAConfiguration(file, command_line, 
		    "Efficiency",sDefaultEfficiency,
		    "KSTriggerDataIn",
		    "Specifies an overall efficiency value. Only this "
		    "fraction of generated Cherenkov photons will be used in "
		    "testing for a trigger. Models dirty mirrors, dirty air, "
		    "dirty pmts/lightcones etc.  Allowed range is 0.0 -> 1.0 "
		    "inclusive.");
  doVAConfiguration(file, command_line, 
		    "TriggerMultiplicity",sDefaultTriggerMultiplicity,
		    "KSTriggerDataIn",
		    "Number of pixels needed for a trigger. ");
  doVAConfiguration(file, command_line, 
		    "PatternTriggerLevel", sDefaultPatternTriggerLevel,
		    "KSTriggerDataIn",
		    "PST triggered pixel adjacency requirement for an event "
		    "trigger. Acceptable levels are only:  2 ,3, or 4. Any "
		    "other value disables use of Pattern Trigger.");
  doVAConfiguration(file, command_line, 
		    "LightConeConcentration",sDefaultLightConeConcentration,
		    "KSTriggerDataIn",
		    "Fraction of light that hits light cone that the light "
		    "cone will then reflect onto the active PMT photo-cathode "
		    "to create photo-electrons.");
  doVAConfiguration(file, command_line, 
		    "MountDirectionXDl",sDefaultMountDl,
		    "KSTriggerDataIn",
		    "X direction cosign of the mount direction. X goes "
		    "East.");
  doVAConfiguration(file, command_line, 
		    "MountDirectionYDm",sDefaultMountDm,
		    "KSTriggerDataIn",
		    "Y direction cosign of the mount direction, Y goes "
		    "South");
  doVAConfiguration(file, command_line, 
		    "MountDirectionZDn",sDefaultMountDn,
		    "KSTriggerDataIn",
		    "Z direction cosign of the mount direction, Z goes down, "
		    "for sure. So this is always a negative value.");
  doVAConfiguration(file, command_line, 
		    "MountElevationDeg",sDefaultMountElevationDeg,
		    "KSTriggerDataIn",
		    "Elevation in degrees of mount. Used when "
		    "UseElevationForDlDmDn=ON. In that case it superceds "
		    "MountDirectionXDl,MountDirectionYDm and "
		    "MountDirectionZDn values.  This is part of the "
		    "DriftingGammas drift-scan modeling");
  doVAConfiguration(file, command_line, 
		    "RandomSeedFileName",sDefaultRandomSeedFileName,
		    "KSTriggerDataIn",
		    "File Name for Random Seed File.");
}
// **************************************************************************
 
