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

#include "KSArrayTriggerDataIn.h"


// **************************************************************************
std::string KSArrayTriggerDataIn::sDefaultDataType="VBFFILE";

std::string KSArrayTriggerDataIn::sDefaultT1FileName=" ";//empty;;
std::string KSArrayTriggerDataIn::sDefaultT2FileName=" ";//empty;;
std::string KSArrayTriggerDataIn::sDefaultT3FileName=" ";//empty;;
std::string KSArrayTriggerDataIn::sDefaultT4FileName=" ";//empty;;
std::string KSArrayTriggerDataIn::sDefaultRandomSeedFileName=" ";//empty;;
std::string KSArrayTriggerDataIn::sDefaultTelescopePositionListFile=" ";

int         KSArrayTriggerDataIn::sDefaultArrayTriggerMultiplicity=2; 
double      KSArrayTriggerDataIn::sDefaultTelescopeEventGateWidthNS=20;
double      KSArrayTriggerDataIn::sDefaultArrayCoincidenceGateWidthNS=40;
int         KSArrayTriggerDataIn::sDefaultRunNumber=95000;
// **************************************************************************

KSArrayTriggerDataIn::KSArrayTriggerDataIn()
{
  std::transform(sDefaultDataType.begin(),
		 sDefaultDataType.end(),
		 sDefaultDataType.begin(),
		 toupper);
  if(sDefaultDataType=="VBFFILE")
    {
      fDataType=VBFFILE;
    }
  else if(sDefaultDataType=="ROOTFILE")
    {
      fDataType=ROOTFILE;
    }
  else
    {
      std::cout<<"ksArrayTrigger: Illegal Data type specified for option: "
	"-DataType="<<sDefaultDataType<<std::endl;
      std::cout<<"ksArrayTrigger: Acceptable values for DataType are: VBFFILE "
	"or ROOTFILE"<<std::endl;
      exit(1);
    }
  
  fFileName.resize(4);
  fFileName[E_T1]=sDefaultT1FileName;
  fFileName[E_T2]=sDefaultT2FileName;
  fFileName[E_T3]=sDefaultT3FileName;
  fFileName[E_T4]=sDefaultT4FileName;

  fTelescopePositionListFile = sDefaultTelescopePositionListFile;

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
	       <<fArrayTriggerMultiplicity<<"Defaulting to 2"
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
  config.setValue("DataType",fDataType);
  config.setValue("ArrayTriggerMultiplicity",fArrayTriggerMultiplicity);
  config.setValue("TelescopeEventGateWidthNS",fTelescopeEventGateWidthNS);
  config.setValue("ArrayCoincidenceGateWidthNS",fArrayCoincidenceGateWidthNS);
  config.setValue("TelescopeArrayPositions",fTelescopePositionListFile);
  config.setValue("T1ShowerFileName",fFileName[E_T1]);
  config.setValue("T2ShowerFileName",fFileName[E_T2]);
  config.setValue("T3ShowerFileName",fFileName[E_T3]);
  config.setValue("T4ShowerFileName",fFileName[E_T4]);
  config.setValue("RandomSeedFileName",fRandomSeedFileName);
  config.setValue("OutputRunNumber",fRunNumber);
  return config;
}
// *************************************************************************

void KSArrayTriggerDataIn::configure(VAConfigInfo& file, 
				     VAOptions& command_line)
{
  doVAConfiguration(file, command_line, 
		    "DataType",sDefaultDataType,
		    "KSArrayTriggerDataIn",
		    "Specifies Input and Output file type. Acceptable values "
		    "are VBFFILE of ROOTFILE. Default is VBFFILE");
  doVAConfiguration(file, command_line, 
		    "ArrayTriggerMultiplicity",
		    sDefaultArrayTriggerMultiplicity,
		    "KSArrayTriggerDataIn",
		    "Specifies a Multiplicity level for an Array Event. Range "
		    "is 1 to 4");
  doVAConfiguration(file, command_line, 
		    "TelescopeArrayPositions",
		    sDefaultTelescopePositionListFile,
		    "KSArrayTriggerDataIn",
		    "File Name for relative positions of the telescopes in "
		    "the array in meters. Each line in this file gives the X,"
		    "Y and Z coordinates in meters of a telescope (in the "
		    "order: first line:T1, second line:T2, third line:T3 and "
		    "forth line: T4) in the array. +Y is North, +X is "
		    "and +Z is up (Presently the Z value is ignored and set "
		    "to 0 in this program)_. The X,Y,Z values on each line "
		    "are separated by spaces (not commas). Once these values "
		    "are given this program will find the best \"fit\" of "
		    "these relative locations to the array grid (see the "
		    "-GridType option in ksLight). The \"fitted\" telescope "
		    "locations are usually within 5 meters or less of the "
		    "locations specified.  If this options is not specified "
		    "then the telescope positions are the ones set in "
		    "ksAomega which it retrieves from VAArrayInfoLite using "
		    "a date of 2007-09-11 00:02:00 UTC.");  
  doVAConfiguration(file, command_line, 
		    "T1ShowerFileName",sDefaultT1FileName,
		    "KSArrayTriggerDataIn",
		    "Single Shower File Name for T1 Input Veritas Event file. "
		    "This file is for events found in T1. All input Telescope "
		    "files are created by ksAomega from the same base shower "
                    "file. File type is specifed by DataType option. "
		    "Default is to not use the T1 telescope in the array.");
  doVAConfiguration(file, command_line, 
		    "T2ShowerFileName",sDefaultT2FileName,
		    "KSArrayTriggerDataIn",
		    "Shower File Name for T2 Input Veritas Event file.  File "
		    "type is specifed by DataType option. "
		    "Default is to not use the T2 telescope in the array.");
  doVAConfiguration(file, command_line, 
		    "T3ShowerFileName",sDefaultT3FileName,
		    "KSArrayTriggerDataIn",
		    "Shower File Name for T3 Input Veritas Event file.  File "
		    "type is specifed by DataType option. "
		    "Default is to not use the T3 telescope in the array.");
  doVAConfiguration(file, command_line, 
		    "T4ShowerFileName",sDefaultT4FileName,
		    "KSArrayTriggerDataIn",
		    "Shower File Name for T4 Input Veritas Event file.  File "
		    "type is specifed by DataType option. "
		    "Default is to not use the T4 telescope in the array.");

  doVAConfiguration(file, command_line,
		    "TelescopeEventGateWidthNS",
		    sDefaultTelescopeEventGateWidthNS,
		    "KSArrayTriggerDataIn",
		    "Individual Telescope event Pulse width in NS as fed "
		    "into the L3 trigger coincidence.(Ignored at Present!)");
  doVAConfiguration(file, command_line, 
		    "ArrayCoincidenceGateWidthNS",
		    sDefaultArrayCoincidenceGateWidthNS,
		    "KSArrayTriggerDataIn",
		    "Coincidence window width in NS for the array trigggers. "
		    "(Ignored at Present!)");
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
  std::cout<<" DataType(0=VBFFILE,1=ROOTFILE): "<<fDataType<<std::endl;
  std::cout<<"               T1ShowerFileName: "<<fFileName[E_T1]<<std::endl;
  std::cout<<"               T2ShowerFileName: "<<fFileName[E_T2]<<std::endl;
  std::cout<<"               T3ShowerFileName: "<<fFileName[E_T3]<<std::endl;
  std::cout<<"               T4ShowerFileName: "<<fFileName[E_T4]<<std::endl;
  std::cout<<"       ArrayTriggerMultiplicity: "<<fArrayTriggerMultiplicity
	   <<std::endl; 
  std::cout<<"    TelescopeEventGateWidthNS: "<<fTelescopeEventGateWidthNS
	   <<std::endl;
  std::cout<<"            Output Run Number: "<<fRunNumber<<std::endl;
  return;
}
