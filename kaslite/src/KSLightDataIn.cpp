//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSLightDataIn
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
 
#include "KSLightDataIn.h"
#include "KSKascadeNames.h"

// **************************************************************************
std::string KSLightDataIn::sDefaultMountType ="WHIPPLE";
std::string KSLightDataIn::sDefaultGridType ="TRIANGULAR";  
std::string KSLightDataIn::sDefaultGridOrientation ="NORTHSOUTH";
std::string KSLightDataIn::sDefaultCoreOffsetMode="RANDOM";
std::string KSLightDataIn::sDefaultPMTType="WHIPPLE";
std::string KSLightDataIn::sDefaultRandomSeedFileName="";
double      KSLightDataIn::sDefaultEfficiency=1.0;
bool        KSLightDataIn::sDefaultBenchmarkFlag=false;
// **************************************************************************


KSLightDataIn::KSLightDataIn(KSPeHeadData* phead)
{
  pfPeHead=phead;
  std::transform(sDefaultMountType.begin(),sDefaultMountType.end(),
	    sDefaultMountType.begin(),
	    toupper);
  std::transform(sDefaultGridType.begin(),sDefaultGridType.end(),
	    sDefaultGridType.begin(),
	    toupper);
  std::transform(sDefaultGridOrientation.begin(),sDefaultGridOrientation.end(),
	    sDefaultGridOrientation.begin(),
	    toupper);
  std::transform(sDefaultCoreOffsetMode.begin(),sDefaultCoreOffsetMode.end(),
	    sDefaultCoreOffsetMode.begin(),
	    toupper);
  std::transform(sDefaultPMTType.begin(),sDefaultPMTType.end(),
	    sDefaultPMTType.begin(),
	    toupper);
  // -------------------------------------------------------------------
  // Decode selections
  // -------------------------------------------------------------------
  pfPeHead->fWhippleMount=false;
  pfPeHead->fVeritasMount=false;
  if( sDefaultMountType=="WHIPPLE")
    {
      pfPeHead->fWhippleMount=true;
    }
  else if(sDefaultMountType=="VERITAS")
    {
      pfPeHead->fVeritasMount=true;
    }
  else
    {
      std::cout<<"Illegal option for MountType: "<<sDefaultMountType
	       <<" Assuming MountType=WHIPPLE"<<std::endl;
      pfPeHead->fWhippleMount=true;
    }
    
  pfPeHead->fTriangularGrid=false;
  pfPeHead->fSquareGrid=false;
  if(sDefaultGridType=="TRIANGULAR")
    {
      pfPeHead->fTriangularGrid=true;
      if(sDefaultGridOrientation=="NORTHSOUTH")
	{
	  pfPeHead->fNorthSouthGrid=true;
	}
      else if(sDefaultGridOrientation=="EASTWEST")
	{
	  pfPeHead->fNorthSouthGrid=false;
	}
      else
	{
	  std::cout<<"Illegal option for GridOrientation: "
		   <<sDefaultGridOrientation
		   <<" Assuming GridType=NORTHSOUTH"<<std::endl;
	  pfPeHead->fNorthSouthGrid=true;
	}
      
    }
  else if(sDefaultGridType=="SQUARE")
    {
      pfPeHead->fSquareGrid=true;
      pfPeHead->fNorthSouthGrid=false;
    }
  else
    {
      std::cout<<"Illegal option for GridType: "<<sDefaultGridType
	       <<" Assuming GridType=TRIANGULAR"<<std::endl;
      pfPeHead->fTriangularGrid=true;
    }


  if(sDefaultCoreOffsetMode=="RANDOM")
    {
      pfPeHead->fRandomCoreOffset=true;
    }
  else
    {
      pfPeHead->fRandomCoreOffset=false;
    }

  pfPeHead->fWhipplePMTs=false;
  pfPeHead->fVeritasPMTs=false;
  pfPeHead->fADPPMTs=false;

  if(sDefaultPMTType=="WHIPPLE")
    {
      pfPeHead->fWhipplePMTs=true;
    }
  else if(sDefaultPMTType=="VERITAS")
    {
      pfPeHead->fVeritasPMTs=true;
    }
  else if(sDefaultPMTType=="ADP")
     {
      pfPeHead->fADPPMTs=true;
    }
  else
    {
      std::cout<<"Illegal option for PMTType: "<<sDefaultPMTType
	       <<" Assuming PMTType=WHIPPLE"<<std::endl;
      pfPeHead->fWhipplePMTs=true;
    }

  if(sDefaultCoreOffsetMode=="RANDOM")
    {
      pfPeHead->fRandomCoreOffset=true;
    }
  else
    {
      pfPeHead->fRandomCoreOffset=false;
    }

  pfPeHead->fWhipplePMTs=false;
  pfPeHead->fVeritasPMTs=false;
  pfPeHead->fADPPMTs=false;

  if(sDefaultPMTType=="WHIPPLE")
    {
      pfPeHead->fWhipplePMTs=true;
    }
  else if(sDefaultPMTType=="VERITAS")
    {
      pfPeHead->fVeritasPMTs=true;
    }
  else if(sDefaultPMTType=="ADP")
     {
      pfPeHead->fADPPMTs=true;
    }
  else
    {
      std::cout<<"Illegal option for PMTType: "<<sDefaultPMTType
	       <<" Assuming PMTType=WHIPPLE"<<std::endl;
      pfPeHead->fWhipplePMTs=true;
    }


  if(sDefaultEfficiency>1.0 || sDefaultEfficiency<0.0)
    {
      std::cout<<"ksLight: Efficiency out of range(0.0->1.0). Was: "
	       <<sDefaultEfficiency<<" Defaulting to 1.0"<<std::endl;
      sDefaultEfficiency=1.0;
    }
  pfPeHead->fEfficiency=sDefaultEfficiency;

  fRandomSeedFileName=sDefaultRandomSeedFileName;  //A ksLightDataIn variable
  fBenchmarkFlag=sDefaultBenchmarkFlag;      //Another ksLightDataIn variable
}
// **************************************************************************

KSLightDataIn::~KSLightDataIn()
{
  // nothing to see here
}


VAConfigurationData KSLightDataIn::getConfig() const
{
  VAConfigurationData config;
  config.fName = std::string("KSLightData");
  config.setValue("MountType",sDefaultMountType);
  config.setValue("GridType",sDefaultGridType);
  config.setValue("GridOrientation",sDefaultGridOrientation);
  config.setValue("CoreOffsetMode",sDefaultCoreOffsetMode);
  config.setValue("PMTType",sDefaultPMTType);
  config.setValue("Efficiency",pfPeHead->fEfficiency);
  config.setValue("Benchmark",fBenchmarkFlag);
  config.setValue("RandomSeedFileName",fRandomSeedFileName);
  return config;
}

void KSLightDataIn::configure(VAConfigInfo& file, VAOptions& command_line)
{
  doVAConfiguration(file, command_line, 
		    "MountType", sDefaultMountType,
		    "KSLightDataIn",
		    "Mount Type: WHIPPLE or VERITAS. Specifies mirror area "
                    "and grid area size.");
  doVAConfiguration(file, command_line, 
		    "GridType", sDefaultGridType,
		    "KSLightDataIn",
		    "Structure of grid on the ground. TRIANGULAR allows for "
                    "hexagonal arrays. SQUARE is other option");
  doVAConfiguration(file, command_line, 
		    "GridOrientation", sDefaultGridOrientation,
		    "KSLightDataIn",
		    "Specifies what Orientation on the ground a TRIANGULAR "
                    "grid (if chosen) will have. Options are NORTHSOUTH or "
		    "EASTWEST ");
  doVAConfiguration(file, command_line, 
		    "CoreOffsetMode", sDefaultCoreOffsetMode,
		    "KSLightDataIn",
		    "RANDOM mode offsets the core location of a shower "
                    "randomly within the central (0,0) grid area. This "
		    "prevents some subtle corelations (especially for array "
		    "events) and more accuaratly models real data.");
  doVAConfiguration(file, command_line, 
		    "PMTType",sDefaultPMTType,
		    "KSLightDataIn",
		    "Defines which Quantum Efficiency table to use for the "
		    "PMTs. Options are WHIPPLE or  VERITAS or ADP ");
  doVAConfiguration(file, command_line, 
		    "Efficiency",sDefaultEfficiency,
		    "KSLightDataIn",
		    "Specifies an overall efficiency value. Only this "
		    "fraction of generated Cherenkov photons will be saved "
		    "to the output file. Allowed range 0.0 -> 1.0 inclusive");
  doVAConfiguration(file, command_line, 
		    "Benchmark",sDefaultBenchmarkFlag,
		    "KSLightDataIn",
		    "Specifies a special Output mode for producing a VERITAS "
                    "Benchmark results text file");
  doVAConfiguration(file, command_line, 
		    "RandomSeedFileName",sDefaultRandomSeedFileName,
		    "KSLightDataIn",
		    "File Name for Random Seed File.");
}
// **************************************************************************
