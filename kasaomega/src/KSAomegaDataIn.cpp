//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSAomegaDataIn
 * \brief Class to manipulate input parameters for ksAomega though use of
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

#include "KSAomegaDataIn.h"


// **************************************************************************
std::string KSAomegaDataIn::sDefaultVBFFileName=" ";//empty;
std::string KSAomegaDataIn::sDefaultRootFileName=" ";//empty;;
std::string KSAomegaDataIn::sDefaultPixelStatsRootFileName=" ";//empty;;
std::string KSAomegaDataIn::sDefaultRandomSeedFileName=" ";//empty;;

std::string KSAomegaDataIn::sDefaultRelativeGains="OFF"; 
std::string KSAomegaDataIn::sDefaultRelativePedVars="OFF";
std::string KSAomegaDataIn::sDefaultBadPixelSupression="OFF";

int         KSAomegaDataIn::sDefaultNewPatternTriggerLevel=3;
int         KSAomegaDataIn::sDefaultNewTriggerMultiplicity=3; 
double      KSAomegaDataIn::sDefaultNewADCGateWidthNS=20.0;
double      KSAomegaDataIn::sDefaultNewDiscriminatorThresholdPes=10.0;
double      KSAomegaDataIn::sDefaultNewNoiseRate=12.5;	
double      KSAomegaDataIn::sDefaultNewEfficiency=1.0;
double      KSAomegaDataIn::sDefaultNewLightConeConcentration=0.35; 
double      KSAomegaDataIn::sDefaultDigitalCountsPerPE=4.2; 
int         KSAomegaDataIn::sDefaultRunNumber=90000;
// **************************************************************************

KSAomegaDataIn::KSAomegaDataIn()
{
  fVBFFileName            = sDefaultVBFFileName;
  fRootFileName           = sDefaultRootFileName;
  fPixelStatsRootFileName = sDefaultPixelStatsRootFileName;
  fRandomSeedFileName     = sDefaultRandomSeedFileName;
  if(fRandomSeedFileName==string())
    {
      std::cout<<"Fatal--Random Seed file name must be specified"<<std::endl;
      exit(1);
    }
  std::transform(sDefaultRelativeGains.begin(),
		 sDefaultRelativeGains.end(),
		 sDefaultRelativeGains.begin(),
		 toupper);
  std::transform(sDefaultRelativePedVars.begin(),
		 sDefaultRelativePedVars.end(),
		 sDefaultRelativePedVars.begin(),
		 toupper);
  std::transform(sDefaultBadPixelSupression.begin(),
		 sDefaultBadPixelSupression.end(),
		 sDefaultBadPixelSupression.begin(),
		 toupper);
  // -------------------------------------------------------------------
  // Decode selections
  // -------------------------------------------------------------------
 
  fUseRelativeGains=false;
  if(sDefaultRelativeGains=="ON")
    {
      fUseRelativeGains=true;
    }
  else if(sDefaultRelativeGains!="OFF")
    {
      std::cout<<"Illegal option for RelativeGains: "<<sDefaultRelativeGains
	       <<" Assuming RelativeGains=OFF"<<std::endl;
    }

  fUseRelativePedVars=false;
  if(sDefaultRelativePedVars=="ON")
    {
      fUseRelativePedVars=true;
    }
  else if(sDefaultRelativePedVars!="OFF")
    {
      std::cout<<"Illegal option for RelativePedVars: "
	       <<sDefaultRelativePedVars
	       <<" Assuming RelativePedVars=OFF"<<std::endl;
    }

  fUseBadPixelSupression=false;
  if(sDefaultBadPixelSupression=="ON")
    {
      fUseBadPixelSupression=true;
    }
  else if(sDefaultBadPixelSupression!="OFF")
    {
      std::cout<<"Illegal option for BadPixelSupression: "
	       <<sDefaultBadPixelSupression
	       <<" Assuming BadPixelSupression=OFF"<<std::endl;
    }

  fNewPatternTriggerLevel  = sDefaultNewPatternTriggerLevel; 
  if(fNewPatternTriggerLevel<2 ||fNewPatternTriggerLevel>4)
    {
      std::cout<<"Illegal value for PatternTriggerLevel: "
	       <<fNewPatternTriggerLevel
	       <<"Defaulting to PatternTriggerLevel = 3"<<std::endl;
      fNewPatternTriggerLevel=3; 
    }

  fNewTriggerMultiplicity=sDefaultNewTriggerMultiplicity;
  if(fNewTriggerMultiplicity<1)
    {
      std::cout<<"Illegal value for MultiplicityTriggerLevel: " 
	       <<fNewTriggerMultiplicity<<"Defaulting to "
	       <<fNewPatternTriggerLevel<<std::endl;
      fNewTriggerMultiplicity=fNewPatternTriggerLevel;
    }

  if(fNewTriggerMultiplicity!=fNewPatternTriggerLevel)
    {
      std::cout<<"Warning:MultiplicityTriggerLevel (" 
	       <<fNewTriggerMultiplicity
	       <<") not the same as PatternTriggerLevel"
	       <<fNewPatternTriggerLevel<<std::endl;
    } 

  fNewADCGateWidthNS            = sDefaultNewADCGateWidthNS;
  fNewDiscriminatorThresholdPes = sDefaultNewDiscriminatorThresholdPes;
  fNewNoiseRate                 = sDefaultNewNoiseRate;
 
  fNewEfficiency = sDefaultNewEfficiency;
  if(sDefaultNewEfficiency>1.0 || sDefaultNewEfficiency<0.0)
    {
      std::cout<<"ksAomega: NewEfficiency out of range(0.0->1.0). Was: "
	       <<sDefaultNewEfficiency<<" Defaulting to 1.0"<<std::endl;
      fNewEfficiency=1.0;
    }

  fNewLightConeConcentration = sDefaultNewLightConeConcentration;
  if(sDefaultNewLightConeConcentration>1.0 || 
     sDefaultNewLightConeConcentration<0.0)
    {
      std::cout<<"ksAomega: NewLightConeConcentration out of range(0.0->1.0). "
	       <<"Was: "<<sDefaultNewLightConeConcentration
	       <<" Defaulting to 1.0"
	       <<std::endl;
      fNewLightConeConcentration=1.0;
    }

  fDigitalCountsPerPE = sDefaultDigitalCountsPerPE;
  fRunNumber=sDefaultRunNumber;
}
// **************************************************************************

KSAomegaDataIn::~KSAomegaDataIn()
{
  // nothing to see here
}


VAConfigurationData KSAomegaDataIn::getConfig() const
{
  VAConfigurationData config;
  config.fName = std::string("KSAomegaData");
  config.setValue("VBFOutputFileName",fVBFFileName);
  config.setValue("RootOutputFileName",fRootFileName);
  config.setValue("PixelStatusRootFileName",fPixelStatsRootFileName);
  config.setValue("RandomSeedFileName",fRandomSeedFileName);
  config.setValue("RelativeGains",sDefaultRelativeGains);
  config.setValue("RelativePedVars",sDefaultRelativePedVars);
  config.setValue("BadPixelSupression",sDefaultBadPixelSupression);
  config.setValue("PatternTriggerLevel",fNewPatternTriggerLevel);
  config.setValue("TriggerMultiplicity",fNewTriggerMultiplicity);
  config.setValue("ADCGateWidthNS",fNewADCGateWidthNS);
  config.setValue("DiscriminatorThresholdPes",fNewDiscriminatorThresholdPes);
  config.setValue("NoiseRate",fNewNoiseRate);
  config.setValue("Efficiency",fNewEfficiency);
  config.setValue("LightConeConcentration",fNewLightConeConcentration);
  config.setValue("DigitalCountsPerPE",fDigitalCountsPerPE);
  config.setValue("OutputRunNumber",fRunNumber);
  return config;
}
// *************************************************************************

void KSAomegaDataIn::configure(VAConfigInfo& file, VAOptions& command_line)
{
  doVAConfiguration(file, command_line, 
		    "VBFOutputFileName",sDefaultVBFFileName,
		    "KSAomegaDataIn",
		    "File Name for Output Veritas VBF Event file. This file "
		    "will mimic a real raw data file complete with trace "
		    "data. Default is to not create such a file.");
  doVAConfiguration(file, command_line, 
		    "RootOutputFileName",sDefaultRootFileName,
		    "KSAomegaDataIn",
		    "File Name for Output Veritas Root Event file. This file "
		    "will mimic a Veritas VEGAS stage2 calibrated event root "
		    "file (VDF file).  It will have no trace data but will "
		    "have standard VARunHeader, VAArrayInfo, VAQStatsData, "
		    "VARelGainData records and single telescope "
		    "VACalibratedArrayEvent and "
		    "VASimulationData TTrees. This file will "
		    "be suitable for processing through stages 3-6 of VEGAS. "
		    "Default is to not create such a file.");
  doVAConfiguration(file, command_line, 
		    "PixelStatsFileName",sDefaultPixelStatsRootFileName,
		    "KSAomegaDataIn",
		    "File Name for a VEGAS Stage 2 type Input Root file. This "
		    "file will contain: VAPixelStatusData record, used when "
		    "BadPixelSupression is set ON. VAQStatsData record, used "
		    "when RelativePedVars is set ON. and a VARelGainData "
		    "record, used when RelativeGains is set ON. Use this to "
		    "simulate a particular run.  Default(No file name given) "
		    "is to use: No bad pixels,Relative Gains All=1, All "
		    "Relative PedVars all =1, All pedestals = "
		    "kDefaultPedestal (nominally 20)");
  doVAConfiguration(file, command_line, 
		    "RandomSeedFileName",sDefaultRandomSeedFileName,
		    "KSAomegaDataIn",
		    "File Name for Random Seed File.");
  doVAConfiguration(file, command_line, 
		    "RelativeGains",sDefaultRelativeGains,
		    "KSAomegaDataIn",
		    "ON enables the use of a special VARelGainsData record in "
		    "the file specified by  PixelStatsFileName to model the "
		    "relative gains of the pixels after a particular run. "
		    "OFF (default) disables.");
  doVAConfiguration(file, command_line, 
		    "BadPixelSupression",sDefaultBadPixelSupression,
		    "KSAomegaDataIn",
		    "ON enables the use of a special VAPixelsStatusData "
		    "record in the file specified by  PixelStatsFileName to "
		    "model the dead pixels of a particular run. OFF (default) "
		    "disables.");
  doVAConfiguration(file, command_line, 
		    "RelativePedVars",sDefaultRelativePedVars,
		    "KSAomegaDataIn",
		    "ON enables the use of a special VARelGainsData record in "
		    "the file specified by  PixelStatsFileName to model the "
		    "relative pedvars (standard deviation) pixels after a "
		    "particular run. OFF (default) disables.");
   doVAConfiguration(file, command_line, 
		    "PatternTriggerLevel", sDefaultNewPatternTriggerLevel,
		    "KSAomegaDataIn",
		    "PST triggered pixel adjacency requirement for an event "
		    "trigger. Acceptable levels are only:  2 ,3, or 4. Any "
		    "other value defaults to 3.");
  doVAConfiguration(file, command_line, 
		    "TriggerMultiplicity",sDefaultNewTriggerMultiplicity,
		    "KSAomegaDataIn",
		    "Require this multiplicity value for a trigger. "); 
  doVAConfiguration(file, command_line,
		    "NoiseRate",sDefaultNewNoiseRate,
		    "KSAomegaDataIn",
		    "Night sky shine rate in pes/deg/ns (after application of "
                    "all efficiency factors)");
  doVAConfiguration(file, command_line, 
		    "ADCGateWidthNS",sDefaultNewADCGateWidthNS,
		    "KSAomegaDataIn",
		    "Specifies an length of the ADC gate for charge "
		    "integration.");
  doVAConfiguration(file, command_line, 
		    "DiscriminatorThreshold",
		    sDefaultNewDiscriminatorThresholdPes,
		    "KSAomegaDataIn",
		    "Specifies a Threshold level for a pixel to fire. Value "
		    "is in units of mean pes level.");
  doVAConfiguration(file, command_line, 
		    "Efficiency",sDefaultNewEfficiency,
		    "KSAomegaDataIn",
		    "Specifies an overall efficiency value. Only this "
		    "fraction of generated Cherenkov photons will be used in "
		    "testing for a trigger. Models dirty mirrors, dirty air, "
		    "dirty pmts/lightcones etc.  Allowed range is 0.0 -> 1.0 "
		    "inclusive.");
  doVAConfiguration(file, command_line, 
		    "LightConeConcentration",sDefaultNewLightConeConcentration,
		    "KSAomegaDataIn",
		    "Fraction of light that hits light cone that the light "
		    "cone will then reflect onto the active PMT photo-cathode "
		    "to create photo-electrons.");
  doVAConfiguration(file, command_line, 
		    "DigtalCountsPerPE",sDefaultDigitalCountsPerPE,
		    "KSAomegaDataIn",
		    "Conversion factor from pe's to digital counts in FADC" 
                    "traces.");
  doVAConfiguration(file, command_line, 
		    "OutputRunNumber",sDefaultRunNumber,
		    "KSAomegaDataIn",
		    "Run Number to use for Output file (VDF or VBF) if one is "
		    "specified. Default value is 90000");
}

// ***********************************************************************
 
void KSAomegaDataIn::Print()

// ************************************************************************
// Print out all the parameters for this run
{
  std::cout<<"ksAomega run parameters:"<<std::endl;
  std::cout<<"              UseRelativeGains: "<<fUseRelativeGains
	   <<std::endl;   
  std::cout<<"            UseRelativePedVars: "<<fUseRelativePedVars
	   <<std::endl;
  std::cout<<"         UseBadPixelSupression: "<<fUseBadPixelSupression
	   <<std::endl;
  std::cout<<"       New PatternTriggerLevel: "<<fNewPatternTriggerLevel
	   <<std::endl;
  std::cout<<"       New TriggerMultiplicity: "<<fNewTriggerMultiplicity
	   <<std::endl; 
  std::cout<<"            New ADCGateWidthNS: "<<fNewADCGateWidthNS
	   <<std::endl;
  std::cout<<" New DiscriminatorThresholdPes: "
	   <<fNewDiscriminatorThresholdPes<<std::endl;
  std::cout<<"                 New NoiseRate: "<<fNewNoiseRate<<std::endl;
  std::cout<<"                New Efficiency: "<<fNewEfficiency<<std::endl;
  std::cout<<"         Digital Counts per PE: "<<fDigitalCountsPerPE
	   <<std::endl; 
  std::cout<<"             Output Run Number: "<<fRunNumber<<std::endl;
  return;
}
