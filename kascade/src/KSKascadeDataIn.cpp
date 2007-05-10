//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSKascadeDataIn
 * \brief Class to manipulate input parameters for kascade though use of
 * config files.
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
 
#include "KSKascadeDataIn.h"
#include "KSKascadeNames.h"


// **************************************************************************
int               KSKascadeDataIn::sDefaultType = 1;  //Gamma
                             //1 deg from South(+ is south, - is north I think)
double            KSKascadeDataIn::sDefaultGeVEnergyPrimary=1000.0; //1 Tev
double            KSKascadeDataIn::sDefaultDlInitial=1.e-15; 
double            KSKascadeDataIn::sDefaultDmInitial=0.017452212;

double            KSKascadeDataIn::sDefaultAzInitialDeg=-1.0;
double            KSKascadeDataIn::sDefaultZenithInitialDeg=-1.0;

double            KSKascadeDataIn::sDefaultEnergyThresholdMeV=10.; //MeV
double            KSKascadeDataIn::sDefaultMaxCoulombScatSegmentLength=0.02;
                                                                     //gm/cm**2
double            KSKascadeDataIn::sDefaultInjectionDepth=.1; //gm/cm**2
double            KSKascadeDataIn::sDefaultObservationAltitudeM=2320.0;
                                                        //meters (whipple 10 m)
int               KSKascadeDataIn::sDefaultShowerID=1;
std::string       KSKascadeDataIn::sDefaultEarthsMagneticFieldSpec="W";
std::vector<bool> KSKascadeDataIn::sDefaultParticleTraceEnableFlags(20,false);
std::vector<bool> KSKascadeDataIn::sDefaultFunctionEnableFlags(3,true);

//int              KSKascadeDataIn::sDefaultParticleTraceEnableFlags;[20]=
//                                  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
std::string       KSKascadeDataIn::sDefaultRandomSeedFileName="";
// **************************************************************************


KSKascadeDataIn::KSKascadeDataIn(KSSegmentHeadData* SegmentHead)
{
  pfSegmentHead=SegmentHead;
  pfSegmentHead->fType=sDefaultType;
  pfSegmentHead->fGeVEnergyPrimary=sDefaultGeVEnergyPrimary;

  //If Azimuth and Zenith angle have been set from default override 0dl and dm
  if( sDefaultAzInitialDeg >= 0.0 && sDefaultZenithInitialDeg >= 0.0 )
  {
    sDefaultDlInitial = -sin(sDefaultZenithInitialDeg * gDeg2Rad) * 
                                       sin(sDefaultAzInitialDeg * gDeg2Rad);
    sDefaultDmInitial = sin(sDefaultZenithInitialDeg * gDeg2Rad) * 
                                       cos(sDefaultAzInitialDeg * gDeg2Rad);
    if(fabs(sDefaultDlInitial)<1.e-15)
      {
	sDefaultDlInitial=1.e-15;
      }
    if(fabs(sDefaultDmInitial)<1.e-15)
      {
	sDefaultDmInitial=1.e-15;
      }
  }

  pfSegmentHead->fDlInitial=sDefaultDlInitial;
  pfSegmentHead->fDmInitial=sDefaultDmInitial;

  pfSegmentHead->fEnergyThresholdMeV=sDefaultEnergyThresholdMeV;
  pfSegmentHead->fMaxCoulombScatSegmentLength = 
                                           sDefaultMaxCoulombScatSegmentLength;
  pfSegmentHead->fInjectionDepth=sDefaultInjectionDepth;
  pfSegmentHead->fObservationAltitudeM=sDefaultObservationAltitudeM;
  pfSegmentHead->fShowerID=sDefaultShowerID;

  for(int i=0;i<(int)sDefaultEarthsMagneticFieldSpec.size();i++)
    {
      pfSegmentHead->fEarthsMagneticFieldSpec[i] = 
	                             sDefaultEarthsMagneticFieldSpec.at(i);
    }

  pfSegmentHead->fDnInitial=sqrt(1.-
		     pfSegmentHead->fDlInitial*pfSegmentHead->fDlInitial-
		     pfSegmentHead->fDmInitial*pfSegmentHead->fDmInitial);

  //  std::string version("V1.0.0");

  // for(int i=0;i<(int)version.size();i++)
  //  {
  //    pfSegmentHead->fVersion[i]=version.at(i);
  //  }

  for(int i=0;i<20;i++)
    {
      if(sDefaultParticleTraceEnableFlags[i])
	{
	  fParticleTraceEnableFlags[i]=1;
	}
      else
	{
	  fParticleTraceEnableFlags[i]=0;
	}
    }
  for(int i=0;i<3;i++)
    {
      if(sDefaultFunctionEnableFlags[i])
	{
	  pfSegmentHead->fFunctionEnableFlags[i]=1;
	}
      else
	{
	  pfSegmentHead->fFunctionEnableFlags[i]=0;
	}
    }


  fRandomSeedFileName=sDefaultRandomSeedFileName;

}
// **************************************************************************

KSKascadeDataIn::~KSKascadeDataIn()
{
  // nothing to see here
}


VAConfigurationData KSKascadeDataIn::getConfig() const
{
  VAConfigurationData config;
  config.fName = std::string("KSKascadeData");
  config.setValue("PrimaryType",pfSegmentHead->fType);
  config.setValue("PrimaryEnergyGeV",pfSegmentHead->fGeVEnergyPrimary);

  config.setValue("AzInitialDeg",sDefaultAzInitialDeg);
  config.setValue("ZenithInitialDeg",sDefaultZenithInitialDeg);

  config.setValue("dlInitial",pfSegmentHead->fDlInitial);
  config.setValue("dmInitial",pfSegmentHead->fDmInitial);
  config.setValue("ThresholdEnergyMeV",
		  pfSegmentHead->fEnergyThresholdMeV);
  config.setValue("MaxCoulombScatSegmentLength",
		  pfSegmentHead->fMaxCoulombScatSegmentLength);
  config.setValue("InjectionDepth",pfSegmentHead->fInjectionDepth);
  config.setValue("ObservationAltitudeM",
		  pfSegmentHead->fObservationAltitudeM);
  config.setValue("ShowerID",pfSegmentHead->fShowerID);
  config.setValue("EarthsMagneticFieldSpec",
		  pfSegmentHead->fEarthsMagneticFieldSpec);
  config.setValue("ParticleTraceEnableFlags",sDefaultParticleTraceEnableFlags);
  config.setValue("FunctionEnableFlags",sDefaultFunctionEnableFlags);
  config.setValue("RandomSeedFileName",fRandomSeedFileName);
  return config;
}

void KSKascadeDataIn::configure(VAConfigInfo& file, VAOptions& command_line)
{
  doVAConfiguration(file, command_line, 
		    "PrimaryType", sDefaultType,
		    "KSKascadeDataIn",
		    "Primary Type: KASCADE codes:Ex:1=gamma,13=proton  or  "
                    " Kascade Heavy Type codes:20+Atomic N:  Ex 24=>He4, "
                    "76=>Fe56");
  doVAConfiguration(file, command_line, 
		    "PrimaryEnergyGeV", sDefaultGeVEnergyPrimary,
		    "KSKascadeDataIn",
		    "Primary Energy in GeV");
  doVAConfiguration(file, command_line, 
		    "dlInitial", sDefaultDlInitial,
		    "KSKascadeDataIn",
		    "Primary Inital x direction cosign (x+ east)"
		    "This is the direction the Primary is GOING TO. ");
  doVAConfiguration(file, command_line, 
		    "dmInitial", sDefaultDmInitial,
		    "KSKascadeDataIn",
		    "Primary Inital y direction cosign (y + south, z +down"
                    "This is the direction the Primary is GOING TO. ");

  doVAConfiguration(file, command_line,
                  "AzInitialDeg", sDefaultAzInitialDeg,
                  "KSKascadeDataIn",
                  "Primary Initial azimuth angle (0 degrees North). "
                  "This is the direction the Primary is COMING FROM. "
		  "Overrides dlInitial and dmInitial");
  doVAConfiguration(file, command_line,
                  "ZenithInitialDeg", sDefaultZenithInitialDeg,
                  "KSKascadeDataIn",
                  "Primary Initial zenith angle (0 degrees up). "
                  "This is the direction the Primary is COMING FROM. "
                  "Overrides dlInitial and dmInitial");

  doVAConfiguration(file, command_line, 
		    "EnergyThresholdMeV", 
		    sDefaultEnergyThresholdMeV,
		    "KSKascadeDataIn",
		    "Particle Track Energy Threshold in MeV");
  doVAConfiguration(file, command_line, 
		    "MaxCoulombScatSegmentLength", 
		    sDefaultMaxCoulombScatSegmentLength,
		    "KSKascadeDataIn",
		    "Maximum Coulomb Scattering segment Length in gm/cm**2");
  doVAConfiguration(file, command_line, 
		    "InjectionDepth", sDefaultInjectionDepth,
		    "KSKascadeDataIn",
		    "Primary Injection Depth in gm/cm**2");
  doVAConfiguration(file, command_line, 
		    "ObservationAltitudeM", 
		    sDefaultObservationAltitudeM,
		    "KSKascadeDataIn",
		    "Observatory Altitude in Meters above sea level.");
  doVAConfiguration(file, command_line, 
		    "ShowerID", 
		    sDefaultShowerID,
		    "KSKascadeDataIn",
		    "Shower ID number.");
  doVAConfiguration(file, command_line, 
		    "EarthsMagneticFieldSpec", 
		    sDefaultEarthsMagneticFieldSpec,
		    "KSKascadeDataIn",
		    "Earths Magnetic File specification. Possible values "
		    "are W (for Whipple/BaseCamp)or ?");
  //for(int k=0;k<18;k++)
  //  {
  //    std::string FlagOption("TraceEnableFlag");
  //    char flag[3];
  //    int j=k+1;
  //    sprintf(flag,"%02i",j);
  //    std::string Flag(flag);
  //    std::string Option=FlagOption+Flag;
  //    std::string Comment("A 1 here enables trace for ");
  //    Comment+=fNameType[k];
  //    doVAConfiguration(file, command_line, 
  //  			Option.c_str(), sDefaultParticleTraceEnableFlags[k],
  //    			"KSKascadeDataIn",
  //    			Comment.c_str());
  //  }
  doVAConfiguration(file, command_line, 
		    "ParticleTraceEnableFlags",
		    sDefaultParticleTraceEnableFlags,
		    "KSKascadeDataIn",
		    "A comma seperated list of 20 Boolean values (true/false "
		    "or true/false value (for gcc true/false=1/0) ). A true  "
		    "in any of the first 18 positions enables a debug trace "
		    "of that KASCADE particle type to be printed. A true in "
		    "the last position (20th) enables trace for Heavies "
		    "(Kascade Type= 20 + Atomic N). Default is all false: "
		    "No trace printing");
  
  //  std::string HeavyOption("HeavyParticleTraceEnableFlag");
  //std::string Comment("A 1 here enables trace for Heavies "
  //		      "(Kascade Type= 20 + Atomic N)");
  //doVAConfiguration(file, command_line, 
  //		    HeavyOption.c_str(), sDefaultParticleTraceEnableFlags[19],
  //			"KSKascadeDataIn",
  //			Comment.c_str());
  doVAConfiguration(file, command_line, 
		    "FunctionEnableFlags",
		    sDefaultFunctionEnableFlags,
		    "KSKascadeDataIn",
		    "A comma seperated list of 3 Boolean values (true/false  "
		    "or or a true/false value ( for gcc true/false=1/0) ). "
		    "These flags enable(disable) various functions "
		    "in the shower generation code. "
		    "Flag 1 Enables: Bending in the Earths Magnetic Field."
		    "Flag 2 Enables: Ionization losses."
		    "Flag 3 Enables: Multiple Coulomb Scattering."
		    "Default is all true: All functions enabled!");
  doVAConfiguration(file, command_line, 
		    "RandomSeedFileName",sDefaultRandomSeedFileName,
		    "KSKascadeDataIn",
		    "File Name for Random Seed File.");
}
// **************************************************************************

