/**
 * \class KSGrISUPilotFile
 * \ingroup common
 * \brief Methods forKascade GrISUPilot file.
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#include "KSGrISUPilotFile.h"
extern "C" float yds(float* ggms);

extern "C" float pran(float* fXDummy);


KSGrISUPilotFile::KSGrISUPilotFile(std::string GrISUPilotFileName)
// **********************************************************************
// Read in and 'parse' a GrISU Pilot file, filling in values in preperation
// to creating a ksKascade .config file
// **********************************************************************
{
  fConfigFileName=GrISUPilotFileName + ".config";
  // Set up defaluts: values that flag that they are not set in GrISUPilotFile
  fTITLE=" "; 
  fNUMBR=1;
  fFILEO=" ";
  fFILEL=" ";
  fKASRN=" ";
  for(int i=0;i<2;i++)fENERG[i] = 0;
  fPTYPE=0;
  fINDXG=0;
  for(int i=0;i<2;i++)fDIRCS[i] = 0;
  fTHRES=0;
  fDEPTH=0; 
  fDPOBS=0;
  fTHICK=0;
  for(int i=0;i<20;i++)fPROCS[i]=false;
  for(int i=0;i<3;i++)fFLAGS[i]=false;
  fMAGON="DEFAULT";     //Earths magnetic field specifcation: 



  //open the GrISU Pilot file as an istream.
  std::ifstream fGPIn(GrISUPilotFileName.c_str());
  if(!fGPIn)
    {
      std::cout<<"KSGrISUPilotFail: Failed to open input GrISU Pilot file: "
	       <<GrISUPilotFileName<<std::endl;
      exit(1);
    }
  
  // **************************************************************************
  // Main input file paring loop
  // **************************************************************************
  std::string fAsterisk;
  std::string fGrISUParameter;
  std::string fFlg;
  fPROCSSpecified=false;
  fFLAGSSpecified=false;
  while (1)
    {
      //Read in first text string from FileList. Then check to see if it 
      //contains a '*', if not this line is a comment and we skip it.
      fGPIn >> fAsterisk;
      if (!fGPIn.good()) break;   //are we done yet?

      if(fAsterisk!="*")
	{
	  fGPIn.ignore(std::numeric_limits<std::streamsize>::max(),'\n');
	  continue;
	}
      //Ok this line has a parameter in it. The parameter should be the next 
      //string. Get it and then find which one it is
      // *******************************************************************
      // Note: Following GrISU parameters are ignored since ksKascade doesn't
      //       use them:
      // RSEED KLOGO KSPOG
      // ******************************************************************** 


     fGPIn >> fGrISUParameter;
      
      if(fGrISUParameter == "TITLE")
	{
	  std::getline(fGPIn,fTITLE);
	}
      else if(fGrISUParameter == "NUMBR")
	{
	  fGPIn >> fNUMBR;
	}
      else if(fGrISUParameter == "FILEO")
	{
	  fGPIn >> fFILEO;
	}
      else if(fGrISUParameter == "FILEL")
	{
	  fGPIn >> fFILEL;
	}
      else if(fGrISUParameter == "KASRN")
	{
	  fGPIn >> fKASRN;
	}
      else if(fGrISUParameter == "ENERG")
	{
	  fGPIn >> fENERG[0];
	  fGPIn >> fENERG[1];
	}
      else if(fGrISUParameter == "PTYPE")
	{
	  fGPIn >> fPTYPE;
	}
      else if(fGrISUParameter == "INDXG")
	{
	  fGPIn >> fINDXG;
	}
      else if(fGrISUParameter == "DIRCS")
	{
	  fGPIn >> fDIRCS[0];
	  fGPIn >> fDIRCS[1];
	  fGPIn >> fDIRCS[2];
	}
      else if(fGrISUParameter == "THRES")
	{
	  fGPIn >> fTHRES;
	}
      else if(fGrISUParameter == "DEPTH")
	{
	  fGPIn >> fDEPTH;
	}
      else if(fGrISUParameter == "DPOBS")
	{
	  fGPIn >> fDPOBS;
	}
      else if(fGrISUParameter == "THICK")
	{
	  fGPIn >> fTHICK;
	}
      else if(fGrISUParameter == "PROCS")
	{
	  fPROCSSpecified=true;
	  for(int i=0;i<20;i++)
	    {
	      fGPIn >> fFlg;
	      if(fFlg=="t")
		{
		  fPROCS[i]=true;
		}
	      else
		{
		  fPROCS[i]=false;
		}
	    }
	}
      else if(fGrISUParameter == "FLAGS")
	{
	  fFLAGSSpecified=true;
	  for(int i=0;i<3;i++)
	    {
	      fGPIn >> fFlg;
	      if(fFlg=="t")
		{
		  fFLAGS[i]=true;
		}
	      else
		{
		  fFLAGS[i]=false;
		}
	    }
	} 
      else if(fGrISUParameter == "MAGON")
	{
	  fGPIn >> fMAGON;
	} 
      else if(fGrISUParameter == "THICK")
	{
	  fGPIn >> fTHICK;
	}
	  

      //Skip the rest of this line and go on to the next one
      fGPIn.ignore(std::numeric_limits<std::streamsize>::max(),'\n');
    }
  fGPIn.close();
  // All done
}

KSGrISUPilotFile::~KSGrISUPilotFile()
{
  // nothing to do here
}

// ***************************************************************************

void KSGrISUPilotFile::Convert2Config()
// **************************************************************************
// Convert and write parameters to the ksKascade config file
// **************************************************************************
{
  std::ofstream fOut(fConfigFileName.c_str(), std::ios::out | std::ios::trunc);
  // ****************************************************************
  // Write things out
  // ****************************************************************
  fOut<<"# "<<fTITLE<<std::endl;  //note the "#" makes this a comment
  fOut<<"# This ksKascade config file is derived from GrISU pilot file: "
      <<fConfigFileName<<std::endl;
  if(fENERG[0]!=0 || fENERG[1]!=0)
    {
      fOut<<"PrimaryEnergyGeV "<<1000*fENERG[1]<<std::endl;
    }
  if(fKASRN!=" ")fOut<<"RandomSeedFileName \""<<fKASRN<<"\""<<std::endl;
  if(fPTYPE!=0)fOut<<"PrimaryType "<<fPTYPE<<std::endl;
  if(fDIRCS[0]!=0 ||fDIRCS[1]!=0 || fDIRCS[2]!=0)
    {
      fOut<<"dlInitial "<<fDIRCS[0]<<std::endl;
      fOut<<"dmInitial "<<fDIRCS[1]<<std::endl;
    }
  if(fTHRES!=0)fOut<<"EnergyThresholdMeV "<<fTHRES<<std::endl;
  if(fDEPTH!=0)fOut<<"InjectionDepth "<<fDEPTH<<std::endl;
  if(fDPOBS!=0)
    {
      //must convert to an altitude
      float fObsGms=(float)fDPOBS;
      float fObsAltitudeM=yds(&fObsGms);
      fOut<<"ObservationAltitudeM "<<fObsAltitudeM<<std::endl;
    }
  if(fTHICK!=0)fOut<<"MaxCoulombScatSegmentLength "<<fTHICK<<std::endl;
  if(fPROCSSpecified)
    {
      fOut<<"ParticleTraceEnableFlags ";
      for(int i=0;i<20;i++)
	{
	  if(fPROCS[i])
	    {
	      fOut<<"true,";
	    }
	  else
	    {
	      fOut<<"false,";
	    }
	}
      fOut<<std::endl;
    }
  if(fFLAGSSpecified)
    {
      fOut<<"FunctionEnableFlags ";
      for(int i=0;i<3;i++)
	{
	  if(fFLAGS[i])
	    {
	      fOut<<"true,";
	    }
	  else
	    {
	      fOut<<"false,";
	    }
	}
      fOut<<std::endl;
    }
  if(fMAGON!="DEFAULT")
    {
      fOut<<"EarthsMagneticFieldSpec \""<<fMAGON<<"\""<<std::endl;
    }
  fOut.close();
}


double KSGrISUPilotFile::genrateGrISUShowerEnergy()
// **************************************************************************
// Using the parmaters in fENERG  and  fINDXG generate a shower energy
// from a spectrum of index fINDXG between fENERG[0] and fENERG[1]
// **************************************************************************
{
  // First check that fENERG has been set is single energy
  if(fENERG[0]==0 && fENERG[1]==0)
    {
      return -1;   //negative is flag that these parameters were not set
    }
  else if(fENERG[0]>=fENERG[1])
    {
      return (double)fENERG[0]*1000;    //Single energy. convert to GeV
    }
  else if(fINDXG==0)
    {
      double fEnergy=pran(&fXDummy)*(fENERG[1]-fENERG[0])+ fENERG[0];
      return fEnergy*1000; //convert to GeV
    }
  else
    {
      double r=pran(&fXDummy);
      double fEnergy= r*pow(fENERG[1],fINDXG) + (1-r)*pow(fENERG[0],fINDXG);
      fEnergy= pow(fEnergy,(1./fINDXG));
      return fEnergy*1000; //convert to GeV
    }
}
