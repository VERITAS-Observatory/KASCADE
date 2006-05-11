/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that mainpulate the Kascade binary segment file.

#ifndef KSGRISUPILOTFILE_H
#define KSGRISUPILOTFILE_H


#include <string>
#include <iostream>
#include <fstream>
#include <cmath>


// *******************************************************
// ** GrISU Pilot File class **
// *******************************************************
class KSGrISUPilotFile
{
 public:
  KSGrISUPilotFile(std::string PilotFileName);
  virtual ~KSGrISUPilotFile();
  void Convert2Config();
  double genrateGrISUShowerEnergy();

  std::string getConfigFileName(){return fConfigFileName;};
  
  // ***************************
  //Pilot file variables
  std::string fTITLE;     //Run title; Not used as of yet
  int         fNUMBR;     //Number of showers to generate:GrISU mode only
  std::string fFILEO;     //Output file name: GrISUOuputFileName
  std::string fFILEL;     //Log File Name: LogFileName
  std::string fKASRN;     //Random Seed File Name: RandomSeedFileName 
  double      fENERG[2];  //Shower energy range, min max: GrISUmode only
  int         fPTYPE;     //Primary type(kascade types): PrimaryType
  double      fINDXG;     //Spectral index: GrISU mode only
  double      fDIRCS[3];  //Primary directions cosigns: dlInitial, dmInitial,..
  double      fTHRES;     //Secondary minimun Energy: EnergyThresholdMeV
  double      fDEPTH;     //Injection Depth(gm/cm**2): InjectionDepth
  double      fDPOBS;     //Observation depth (gm/cm**2): ObservationAltitudeM 
  double      fTHICK;     //TRack slice thinckness(gm/cm**2): 
                          //                       MaxCoulombScatSegmentLength
  bool        fPROCS[20]; //Flags for trace prints: TraceEnableFlagXX
  bool        fFLAGS[3];  //Magnet on,Ionization On, MUltipleScattering On.
                          //GrISU mode: Only
  std::string fMAGON;     //Earths magnetic field specifcation: 
                          //                           EarthsMagneticFieldSpec
  // ******************************
 private:
  std::string fConfigFileName;
  bool fPROCSSpecified;
  bool fFLAGSSpecified;
  float fXDummy;
};
// ***************************************************************************

		   
     



#endif
