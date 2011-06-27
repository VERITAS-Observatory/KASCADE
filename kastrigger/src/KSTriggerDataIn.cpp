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
std::string KSTriggerDataIn::sDefaultCameraType="VERITAS499";
std::string KSTriggerDataIn::sDefaultTraceEnable="OFF";
std::string KSTriggerDataIn::sDefaultGammas2D="OFF";
std::string KSTriggerDataIn::sDefaultUseElevationForDlDmDn="OFF";
std::string KSTriggerDataIn::sDefaultMultipleMountDirections="OFF";
std::string KSTriggerDataIn::sDefaultLoadMountDirectionsFromFile="OFF";
std::string KSTriggerDataIn::sDefaultSaveMountDirectionsToFile="OFF";

std::string KSTriggerDataIn::sDefaultMountDirectionsFileName=" ";
std::string KSTriggerDataIn::sDefaultRandomSeedFileName=" ";

double      KSTriggerDataIn::sDefaultNoiseRate=6.0 ;	
double      KSTriggerDataIn::sDefaultDiscCoincidenceWidthNS=6.5;
double      KSTriggerDataIn::sDefaultEfficiency=1.0;
double      KSTriggerDataIn::sDefaultDiscriminatorThresholdPes=3.0;
double      KSTriggerDataIn::sDefaultMaximumThetaDeg=9.0;
int         KSTriggerDataIn::sDefaultNumDirections=150;	
double      KSTriggerDataIn::sDefaultGammaStepSizeDeg=.1;
int         KSTriggerDataIn::sDefaultPatternTriggerLevel=3;
int         KSTriggerDataIn::sDefaultTriggerMultiplicity=3; 
double      KSTriggerDataIn::sDefaultLightConeConcentration=1.0; 
double      KSTriggerDataIn::sDefaultMountAzDeg=-1.0;    
double      KSTriggerDataIn::sDefaultMountZenithDeg=-1.0;
double      KSTriggerDataIn::sDefaultMountElevationDeg=0.0;  
double      KSTriggerDataIn::sDefaultFocalPlaneLocationM=11.985;
double      KSTriggerDataIn::sDefaultAlignmentPlaneLocationM=-1;
std::string KSTriggerDataIn::sDefaultAlignmentMethod="MCGILL";
std::string KSTriggerDataIn::sDefaultFacetLocationFileName= " ";
double      KSTriggerDataIn::sDefaultPSFNorthSouthDeg=-1.0;
double      KSTriggerDataIn::sDefaultPSFEastWestDeg=-1.0;
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
  std::transform(sDefaultGammas2D.begin(),sDefaultGammas2D.end(),
		 sDefaultGammas2D.begin(),
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
  std::transform(sDefaultAlignmentMethod.begin(),
		 sDefaultAlignmentMethod.end(),
		 sDefaultAlignmentMethod.begin(),
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
	       <<" Assuming CameraType=VERITAS499"<<std::endl;
      pfTeHead->fCameraType=VERITAS499;
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
  
  pfTeHead->fGammas2D=false;
  if(sDefaultGammas2D=="ON")
    {
      pfTeHead->fGammas2D=true;
    }
  else if(sDefaultGammas2D!="OFF")
    {
      std::cout<<"Illegal option for Gammas2D: "<<sDefaultGammas2D
	       <<" Assuming Gammas2D=OFF"<<std::endl;
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

  fGammaStepSizeRad        = sDefaultGammaStepSizeDeg*gDeg2Rad;

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

  // ***********************************************************************
  //set mount dl, dm, dn from az and zenith if these have been set
  // Signs determined by diirection shower is coming FROM (up into the sky)
  // Remember:KASCADE convention:  Z + down, x + east y + south.
  // **********************************************************************
  if( sDefaultMountAzDeg >= 0.0 && sDefaultMountZenithDeg >= 0.0 )
  {
    fMountDl = sin(sDefaultMountZenithDeg * gDeg2Rad) * 
                                sin(sDefaultMountAzDeg * gDeg2Rad);
    fMountDm = -sin(sDefaultMountZenithDeg * gDeg2Rad) * 
                                cos(sDefaultMountAzDeg * gDeg2Rad);
    fMountDn = -cos(sDefaultMountZenithDeg * gDeg2Rad);

    if( fabs(fMountDl) < 1.e-15)
    {
      fMountDl = 1.e-15;
    }
    if( fabs(fMountDm) < 1.e-15)
    {
      fMountDm = 1.e-15;
    }
    if( fabs(fMountDn) < 1.e-15)
    {
      fMountDn = 1.e-15;
    }
  }

  pfTeHead->fMountDl                   = fMountDl;               
  pfTeHead->fMountDm                   = fMountDm;               
  pfTeHead->fMountDn                   = fMountDn;               
  fMountElevationDeg         = sDefaultMountElevationDeg;     
  fMountDirectionFileName=sDefaultMountDirectionsFileName;  
  fRandomSeedFileName=sDefaultRandomSeedFileName;  //A ksTriggerDataIn variable
  
  pfTeHead->fFocalPlaneLocationM=sDefaultFocalPlaneLocationM;

  if(sDefaultAlignmentPlaneLocationM<0){
    pfTeHead->fAlignmentPlaneLocationM=pfTeHead->fFocalPlaneLocationM;
  }
  else{
    pfTeHead->fAlignmentPlaneLocationM=sDefaultAlignmentPlaneLocationM;
  }

  pfTeHead->fFacetLocationFileName=sDefaultFacetLocationFileName; 

    
    if( sDefaultAlignmentMethod=="WHIPPLE")
    {
      pfTeHead->fAlignmentMethod=WHIPPLE;
    }
  else if(sDefaultAlignmentMethod=="MCGILL")
    {
      pfTeHead->fAlignmentMethod=MCGILL;
    }
  else
    {
      std::cout<<"Illegal option for FacetAlignmentMethod: "
	       <<sDefaultAlignmentMethod
	       <<" Assuming Alignment Method: MCGILL"<<std::endl;
      pfTeHead->fAlignmentMethod=MCGILL;
    }

  if(sDefaultPSFNorthSouthDeg<0)
    {
      pfTeHead->fPSFNorthSouthDeg = gPSFNorthSouthDeg[pfTeHead->fCameraType];
    }
  else
    {
      pfTeHead->fPSFNorthSouthDeg = sDefaultPSFNorthSouthDeg;
    }

  if(sDefaultPSFEastWestDeg<0)
    {
      pfTeHead->fPSFEastWestDeg   = gPSFEastWestDeg[pfTeHead->fCameraType];
    }
  else
    {
      pfTeHead->fPSFEastWestDeg   = sDefaultPSFEastWestDeg;
    }
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
  config.setValue("Gammas2D",sDefaultGammas2D);
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
  config.setValue("GammaStepSizeDeg",sDefaultGammaStepSizeDeg);
  config.setValue("LightConeConcentration",pfTeHead->fLightConeConcentration);
  config.setValue("MountAzDeg",sDefaultMountAzDeg);       
  config.setValue("MountZenithDeg",sDefaultMountZenithDeg);
  config.setValue("MountDl",pfTeHead->fMountDl);
  config.setValue("MountDm",pfTeHead->fMountDm);
  config.setValue("MountDn",pfTeHead->fMountDn);
  config.setValue("MountElevationDeg",fMountElevationDeg);
  config.setValue("MountDirectionsFileName",fMountDirectionFileName);
  config.setValue("RandomSeedFileName",fRandomSeedFileName);
  config.setValue("FacetAlignmentMethod",sDefaultAlignmentMethod);
  config.setValue("FacetLocationFileName",sDefaultFacetLocationFileName);
  config.setValue("FocalPlaneLocationM",sDefaultFocalPlaneLocationM);
  config.setValue("McGillFacetAlignmentPlaneLocationM",
		                          pfTeHead->fAlignmentPlaneLocationM);
  config.setValue("PSFNorthSouthDeg",fPSFNorthSouthDeg);
  config.setValue("PSFEastWestDeg",fPSFNorthSouthDeg);

  return config;
}

void KSTriggerDataIn::configure(VAConfigInfo& file, VAOptions& command_line)
{
  doVAConfiguration(file, command_line, 
		    "CameraType", sDefaultCameraType,
		    "KSTriggerDataIn",
		    "Camera Type: WHIPPLE490 or VERITAS499. Specifies camera "
		    "structure and telescope parameters such as mirror size "
		    "and focal length. Default is VERITAS499");
  doVAConfiguration(file, command_line, 
		    "TraceEnable", sDefaultTraceEnable,
		    "KSTriggerDataIn",
		    "ON enables the production of pmt traces for the pixels. "
		    "This is slightly more accurate but much slower.(Not yet "
                    "implemented!) OFF disables (default)");
  doVAConfiguration(file, command_line, 
		    "Gammas2D",sDefaultGammas2D,
		    "KSTriggerDataIn",
		    "ON enables the use of a special X,Y mount "
                    "redirection mode useful for 2D modeling of Gamma "
		    "(signal) events. OFF (default) disables. This option "
		    "when chosen uses -MaximumThetaDeg and -GammaStepSizeDeg");
  doVAConfiguration(file, command_line, 
		    "MaximumThetaDeg",sDefaultMaximumThetaDeg,
		    "KSTriggerDataIn",
		    "Random direction of hadrons is modeled by looking for "
		    "triggers when the mount is pointed in a random direction "
		    "less than this value form the nominal direction. Used "
		    "when MultipleMountDirections=ON. Also used when "
		    "Gamma2D=ON chosen for range of "
		    "directions.");
  doVAConfiguration(file, command_line, 
		    "GammaStepSizeDeg",sDefaultGammaStepSizeDeg,
		    "KSTriggerDataIn",
		    "For Gammas2D=ON events: Specifies "
		    "the "
		    "size of steps to take, in "
		    "X,Y for Gammas2D");
  doVAConfiguration(file, command_line, 
		    "UseElevationForDlDmDn",sDefaultUseElevationForDlDmDn,
		    "KSTriggerDataIn",
		    "ON enables the use of the MountElevationDeg to "
		    "determine MountDirection Dl, Dm and Dn. This is part of "
		    "DriftingGammas mode.");
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
		    "Night sky shine rate in pes/deg**2 (after application of "
                    "all efficiency factors). Default is 6.0");
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
		    "is in units of mean pes level. Default is 3.0.");
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
                    "MountAzDeg",sDefaultMountAzDeg,
		    "KSTriggerDataIn",
		    "Azimuth angle of the mount (0 degrees North). ");

  doVAConfiguration(file, command_line,
                    "MountZenithDeg", sDefaultMountZenithDeg,
		    "KSTriggerDataIn",
		    "Zenith angle of the mount (0 degrees is Zenith). ");

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

  doVAConfiguration(file, command_line,"FocalPlaneLocationM",
		    sDefaultFocalPlaneLocationM,"KSTriggerDataIn",
		    "Distance from center of Veritas mirror to focal "
		    "plane. This allows us to move the focal plane when "
		    "we model focusing when looking at shower images. "
		    "Default is Veritas mirror focus length of 11.985 "
		    "meters");

  doVAConfiguration(file, command_line,"McGillFacetAlignmentPlaneLocationM",
		     sDefaultAlignmentPlaneLocationM,"KSTriggerDataIn",
		    "Distance from center of Veritas mirror to the "
		    "plane to be used for facet alignment (used with MCGILL "
		    "alignment method only). Typically this distance is the "
		    "same as FocalPlaneLocationM (and "
		    "defaults to it) but it can be set to something "
		    "different. This is used when modeling "
		    "\"defocusing\" the telescope to bring shower max "
		    "into better focus.");
  
  doVAConfiguration(file, command_line,"FacetLocationFileName",
		    sDefaultFacetLocationFileName,"KSTriggerDataIn",
		    "A \".txt\" file with the locations (in meters) of "
		    "all facets (including those locations that are not "
		    "used, like the ones at the center of the Mirror). "
		    "This file is TTree::ReadFile() compatable with a "
		    "header line that includes the variables XM,YM and "
		    "EXIST. For example, the line might look like: "
		    "\"FACETID/I:XM/F:YM:EXIST\". The variable EXIST has "
		    "a value of 1.0 if the facet exists or 0.0 if not. "
		    "If file name is not specified then the default of "
		    "using the Hillas method of randomly generating "
		    "facets locations for each photon is used.");
  
  doVAConfiguration(file, command_line,"FacetAlignmentMethod",
		    sDefaultAlignmentMethod,"KSTriggerDataIn",
		    "Facet mirror alignment method to simulate. Options are "
		    "the original WHIPPLE method or the new (as of spring "
		    "2009) MCGILL method. The default is MCGILL");

  doVAConfiguration(file, command_line, 
		    "PSFNorthSouthDeg",sDefaultPSFNorthSouthDeg,
		    "KSTriggerDataIn",
		    "Size of PSF for the telescope in the North/South "
		    "direction in Degrees. This is not the actual value but "
		    "is scaled to it. Default is value given in KSCommon.h "
		    "for gPSFEastWestDeg[fCamera].");

  doVAConfiguration(file, command_line, 
		    "PSFEastWestDeg",sDefaultPSFEastWestDeg,
		    "KSTriggerDataIn",
		    "Size of PSF for the telescope in the East/West "
		    "direction in Degrees. This is not the actual value but "
		    "is scaled to it. Default is value given in KSCommon.h "
		    "for gPSFEastWestDeg[fCamera].");
}
// **************************************************************************
 
