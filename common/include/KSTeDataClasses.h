/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is derived from structures.f90  
// This is preliminary class which I plan to upgrade all of KASCADE to 
// eventually. Not used by general KASCADE as of yet.


#ifndef KSTEDATACLASSES_H
#define KSTEDATACLASSES_H


#include <string>
#include "KSCommon.h"

class KSTeHeadData
// *******************************************************
// ** PE HEADER STRUCTURE **
// *******************************************************
{
 public:
  KSTeHeadData();
  virtual ~KSTeHeadData();
  void PrintTeHead();

  KSCameraTypes fCameraType;
  bool   fTraceCreation;            //Create pmt Pulse trace for all pixels
  bool   fDriftingGammas;           //Use special theta/phi -> gamma drift scan
  bool   fMultipleMountDirections;  //Generate mutiple mount directions for 
                                    //each event. Normal use for Hadrons
  //bool fMountDirectionsSaved;       //Mount directions are to be saved in the
                                    // Te file (Right after this record)
                                    
  double fNoiseRate;	            //Noise rate in pe/deg**2/ns.
  double fDiscCoincidenceWidthNS;   //Discriminater coincidence window(ns)  
  double fDiscriminatorThresholdPes;//Disc threshold for pmt's (pes)
  double fEfficiency;	            //Overall pe detection efficiency. 1.0=full
  double fMaximumThetaRad;	    //Maximum theta to randomly point mount 
                                    //from inital mount direction
  int    fNumDirections;	    //Number of directions of mount to try for
                                    //each event
  int    fPatternTriggerLevel;      //!=0 Use pattern trig for trig decision
  int    fTriggerMultiplicity;      //Trigger multiplicity.
  double fLightConeConcentration;   //Light cone concentration parameter
  double fMountDl;                  //Original mount x direction cosign
  double fMountDm;                  //Original mount y direction cosign
  double fMountDn;                  //Original mount z direction cosign
  bool   fGammas2D;                 //Use special theta/phi -> gamma drift scan
//  char   fVersion[16];	    //Version of kastrig that made this m file.
};
// ***************************************************************************




class KSTeData
// *******************************************************
// class to hold single triggered event (except for pixel timing data)
// *******************************************************
{
 public:
  KSTeData();
  virtual ~KSTeData();
  void PrintTe();

  // Folowing are mc 'tags'
  short fPrimaryType;
  short fShowerID;
  float fShowerEnergyTeV;
  float fXCoreOffsetM; //X,Y coord of core in area 0,0
  float fYCoreOffsetM;
  float fMountDl;  //Direction of the mount this event
  float fMountDm;
  int   fNx;           //ground indices of event(can use these and xWidth, 
  int   fNy;           //ywidth(from PeHead) and fXCoreOffsetM and 
                       //fYCoreOffsetM to find relative shower impact ground 
                       // coordinates)
  int   fThetaIndex;   //Used by driftscan modeling
  int   fPhiIndex;
  int   fDirectionIndex;        //Hadronic direction index(old ithphi)
  float fEmissionAltitude;
  float fEmissionAltitudeSigma;
  float fMuonRatio;
  float fAomega;              
};
#endif
