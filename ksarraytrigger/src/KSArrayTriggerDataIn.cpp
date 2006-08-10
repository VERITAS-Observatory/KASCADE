//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSArrayTriggerDataIn
 * \brief Class to manipulate input parameters for ksArrayTrigger though use of
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

#include "KSArrayTriggerDataIn.h"


// **************************************************************************
std::string KSArrayTriggerDataIn::sDefaultT1RootFileName=" ";//empty;;
std::string KSArrayTriggerDataIn::sDefaultT2RootFileName=" ";//empty;;
std::string KSArrayTriggerDataIn::sDefaultT3RootFileName=" ";//empty;;
std::string KSArrayTriggerDataIn::sDefaultT4RootFileName=" ";//empty;;
std::string KSArrayTriggerDataIn::sDefaultRandomSeedFileName=" ";//empty;;

int         KSArrayTriggerDataIn::sDefaultArrayTriggerMultiplicity=2; 
double      KSArrayTriggerDataIn::sDefaultTelescopeEventGateWidthNS=20;
double      KSArrayTriggerDataIn::sDefaultArrayCoincidenceGateWidthNS=40;
int         KSArrayTriggerDataIn::sDefaultRunNumber=95000;
// **************************************************************************

KSArrayTriggerDataIn::KSArrayTriggerDataIn()
{
  fRootFileName.resize(4);
  fRootFileName[E_T1]=sDefaultT1RootFileName;
  fRootFileName[E_T2]=sDefaultT2RootFileName;
  fRootFileName[E_T3]=sDefaultT3RootFileName;
  fRootFileName[E_T4]=sDefaultT4RootFileName;

  fRandomSeedFileName       = sDefaultRandomSeedFileName;
  if(fRandomSeedFileName==" ")
    {
      std::cout<<"ksArrayTrigger:Fatal--Random Seed file name must be "
	"specified"<<std::endl;
      exit(1);
    }

  fArrayTriggerMultiplicity=sDefaultArrayTriggerMultiplicity;
  if(fArrayTriggerMultiplicity<1 || fArrayTriggerMultiplicity>4)
    {
      std::cout<<"Illegal value for ArrayMultiplicityTrigger: " 
	       <<fNewTriggerMultiplicity<<"Defaulting to 2"
	       <<std::endl;
      fArrayTriggerMultiplicity=2;
    }

  fTelescopeEventGateWidthNS   = sDefaultTelescopeEventGateWidthNS;
  fArrayCoincidenceGateWidthNS = sDefaultArrayCoincidenceGateWidthNS;
  fRunNumber                   = sDefaultRunNumber;
}
// **************************************************************************

KSArrayTriggerDataIn::~KSArrayTriggerDataIn()
{
  // nothing to see here
}


VAConfigurationData KSArrayTriggerDataIn::getConfig() const
{
  VAConfigurationData config;
  config.fName = std::string("KSArrayTriggerData");
  config.setValue("ArrayTriggerMultiplicity",fArrayTriggerMultiplicity);
  config.setValue("TelescopeEventGateWidthNS",fTelescopeEventGateWidthNS);
  config.setValue("ArrayCoincidenceGateWidthNS",fArrayCoincidenceGateWidthNS);
  config.setValue("T1ShowerFileName",fRootFileName[E_T1]);
  config.setValue("T2ShowerFileName",fRootFileName[E_T2]);
  config.setValue("T3ShowerFileName",fRootFileName[E_T3]);
  config.setValue("T4ShowerFileName",fRootFileName[E_T4]);
  config.setValue("RandomSeedFileName",fRandomSeedFileName);
  config.setValue("OutputRunNumber",fRunNumber);
  return config;
}
// *************************************************************************

void KSArrayTriggerDataIn::configure(VAConfigInfo& file, VAOptions& command_line)
{
  doVAConfiguration(file, command_line, 
		    "ArrayTriggerMultiplicity",
		    sDefaultArrayTriggerMultiplicity,
		    "KSArrayTriggerDataIn",
		    "Specifies a Multiplicity level for an Array Event. Range "
		    "is 1 to 4");
  doVAConfiguration(file, command_line, 
		    "T1ShowerFileName",sDefaultT1RootFileName,
		    "KSArrayTriggerDataIn",
		    "Shower File Name for Input Veritas Root Event file. "
		    "This file is for events found in T1. All input Telescope "
		    "files are created by ksAomega from the same base shower "
                    "file. Default is to not use this the T1 telescope in "
		    "the array.");
  doVAConfiguration(file, command_line, 
		    "T2ShowerFileName",sDefaultT2RootFileName,
		    "KSArrayTriggerDataIn",
		    "Shower File Name for Input Veritas Root Event file. "
		    "This file is for events found in T2. All input Telescope "
		    "files are created by ksAomega from the same base shower "
                    "file. Default is to not use this the T2 telescope in "
		    "the array.");
  doVAConfiguration(file, command_line, 
		    "T3ShowerFileName",sDefaultT3RootFileName,
		    "KSArrayTriggerDataIn",
		    "Shower File Name for Input Veritas Root Event file. "
		    "This file is for events found in T3. All input Telescope "
		    "files are created by ksAomega from the same base shower "
                    "file. Default is to not use this the T3 telescope in "
		    "the array.");
  doVAConfiguration(file, command_line, 
		    "T4ShowerFileName",sDefaultT4RootFileName,
		    "KSArrayTriggerDataIn",
		    "Shower File Name for Input Veritas Root Event file. "
		    "This file is for events found in T4. All input Telescope "
		    "files are created by ksAomega from the same base shower "
                    "file. Default is to not use this the T4 telescope in "
		    "the array.");
  doVAConfiguration(file, command_line,
		    "TelescopeEventGateWidthNS",
		    sDefaultTelescopeEventGateWidthNS,
		    "KSArrayTriggerDataIn",
		    "Individual Telescope event Pulse width in NS as fed "
		    "into the L3 trigger coincidence.");
  doVAConfiguration(file, command_line, 
		    "ArrayCoincidenceGateWidthNS",
		    sDefaultArrayCoincidenceGateWidthNS,
		    "KSArrayTriggerDataIn",
		    "Coincidence window width in NS for the array trigggers.");
  doVAConfiguration(file, command_line, 
		    "RandomSeedFileName",sDefaultRandomSeedFileName,
		    "KSArrayTriggerDataIn",
		    "File Name for Random Seed File.");
  doVAConfiguration(file, command_line, 
		    "OutputRunNumber",sDefaultRunNumber,
		    "KSArrayTriggerDataIn",
		    "Run Number to use for Output file (VDF or VBF) if one is "
		    "specified. Default value is 95000");
}

// ***********************************************************************
 
void KSArrayTriggerDataIn::Print()

// ************************************************************************
// Print out all the parameters for this run
{
  std::cout<<"ksArrayTrigger run parameters:"<<std::endl;
  std::cout<<"             T1ShowerFileName: "<<fRootFileName[E_T1]<<std::endl;
  std::cout<<"             T2ShowerFileName: "<<fRootFileName[E_T2]<<std::endl;
  std::cout<<"             T3ShowerFileName: "<<fRootFileName[E_T3]<<std::endl;
  std::cout<<"             T4ShowerFileName: "<<fRootFileName[E_T4]<<std::endl;
  std::cout<<"     ArrayTriggerMultiplicity: "<<fArrayTriggerMultiplicity
	   <<std::endl; 
  std::cout<<"    TelescopeEventGateWidthNS: "<<fTelescopeEventGateWidthNS
	   <<std::endl;
  std::cout<<"            Output Run Number: "<<fRunNumber<<std::endl;
  return;
}
