#ifndef ATMOSPHERE_H
#define ATMOSPHERE_H
//This is the include file for the atmosphere class.
// ***********************************************************************
// This is an expanded version of the orginal atmosphere76.cpp code.
// This version allows for selction of either the US76 atmosphere or the
// use of any of the atmprof().dat files as used by Corsika. In particular
// the use of the VERITAS specific atmprof21.dat (Veritas Winter atm) and
// atmprof22.dat (Veritas Summer), though any file in the same format will
// work.
// *************************************************************************
// When this class is used to generate the US standard atmosphere 76 values,
// it can also generate Pressure,Density,and Temperature of the atmosphere as a
// function of altitudeM. 
// For US76 it is good up to 86.0 km and pretty good after that.
// It steals unashamably from a subroutine: Atmosphere, witten by Ralph 
// Carmichael, Public Domain Aeronautical Software which was found at 
// www.pdas.com/atmos.htm
// *************************************************************************
// When used with a atmprof().dat file as input the file is read into lookup
// tables for density and depth verses altitude in 1 km or greater steps. The
// various values are interpolated as needed. The index of refraction -1 (eta)
// is determined from a base value given in the atmprof file( at 400 nm) and 
// scaled by density to the standard formula that is wavelength dependent(see
// routines)
// *************************************************************************
// GHS converted the atmosphere76  from f90, put it into a class and added 
// gms and yds functions.
// ***********************************************************************
//   G.H.Sembroski
//   Physics Dept.
//   Purdue Univ.
//   W.Lafayette, In 47907
//   sembroski@physics.purdue.edu
//   01/12/04
// ****************************************************************************

#include <iostream>
#include <fstream>
#include <cmath>
#include <string>
#include <vector>
using namespace std;

// ***********************************************************************
//Some local constants (for US76)
// ***********************************************************************
const double  RadiusEarth = 6369.0;             //radius of the Earth (km)
const double  GMR = 34.163195;                      //hydrostatic constant
const int     NTab=8;           //number of entries in the defining tables
const double  SeaLevelDensity = 1.225e-3;                        //gm/cm**3
const int     NSteps=10000;     //Number of steps to numerically integrate 
                                            //region for depth calculation
const double  DepthStepSize=1.0;                   //depthTable step size
const double  altDepthStepSize=1.0;//altitudeDepthTable step size(gm/cm**2)
                                   //This needs to stay a minimum of 1.0

// ***********************************************************************
//     L O C A L   A R R A Y S   ( 1 9 7 6   S T D.  A T M O S P H E R E )
// ***********************************************************************
const double htab[NTab]=                     //geopotential altitudes(km)
  {0.0, 11.0, 20.0, 32.0, 47.0, 51.0, 71.0, 84.852};

const double ttab[NTab]=           //Base temperature at each altitude(c)
  {288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65, 186.946};

const double ptab[NTab]=                            //Pressure coeficient
  { 1.0, 2.233611E-1, 5.403295E-2, 8.5666784E-3, 1.0945601E-3, 
    6.6063531E-4, 3.9046834E-5, 3.68501E-6 };

const double gtab[NTab]= //parameters for temp fit(0 if region is isothermal)
  {-6.5, 0.0, 1.0, 2.8, 0.0, -2.8, -2.0, 0.0};

// ***************************************************
// A struct useful for yds in atm76
// *************************************************
struct altDepth
{
  double altitude;
  double depth;
};
// ***************************************************************************
// ***********************************************************************
//Some local constants (for atmprof)
// ***********************************************************************
const double kAtmProfAltScaleHeightKM=7.739;
const double kAtmProfReferenceWavelength=400.0; 

// *********************************************************************
//Define the class
class atmosphere
{
 private:
  
  bool   fUseUS76atm;
  double fTemperature;
  double fPressureRatio;
  double fDensity;
  double fDepth;
  double fDepthSum;
  double fAltitudeM;
  double fEta;

 
  string fAtmTitle;

// *********************
  //Mostly US76 stuff here
  // *********************
  int NDepth;
  int NAltDepth;
  double topAlt;
  double topAltDepth;
  double seaLevelDepth;
  double* depthTable;
  altDepth* altDepthTable;
  double altitudeNow;
  int regionIndex;
  double h;
  double deltah;
  double tlocal;
  double regionDepth[NTab];
  //For above top atm use exponential approx (no temperature adjustment)
  double b7atm;
  double a7atm;
  
  int getRegionIndex(double h);
  double getGeopotentialAltitude(double alt)
    {return (alt/1000)*RadiusEarth/((alt/1000)+RadiusEarth);};
  double getGeometricAltitude(double h)
    {return 1000*RadiusEarth*h/(RadiusEarth-h);};
  double getRegionDepth(double altitudeM);

  bool fUS76Initalized;

  void initUS76();
  double getTemperatureUS76(float altitudeM);
  double getPressureRatioUS76(float altitudeM);
  double getDensityUS76(float altitudeM);
  double getDepthUS76(float altitudeM);
  double getDepthSumUS76(float altitudeM);
  double getAltitudeMUS76(float depth);
  double getEtaUS76(float altitudeM, float lambdaNM);
  // *********************************************

  // *****************************************************
  // AtmProfile stuff 
  // *****************************************************
  bool fAtmProfileInitalize;
  void initAtmProf(string atmProfileFileName);
  double getDensityAtmProf(float* pAltitudeM);
  double getDepthAtmProf(float* pAltitudeM);
  double getAltitudeMAtmProf(float* pDepth);
  double getEtaAtmProf(float*  pAltitudeM, float* pLambdaNM);

  vector< double > fAltKM;        //lookup tables
  vector< double > fScaledRhoGMC3;
  vector< double > fLog10GmsGMC2;
  vector< double > fEta400;

  vector< double >::iterator fBegin; //Use to interpolate in density LT
  vector< double >::iterator fEnd;
  vector< double >::iterator fMiddle;

  vector< double > fEtaSeaLevel;

  void   initEtaSeaLevelBasic();
  double getAtmosphereInterpFractionAtmProf(float altitudeM);
  double getDepthInterpFractionAtmProf(float depth);
 // *****************************************************
  

 public:
  atmosphere();
  ~atmosphere();
  void init (string atmProfileFile);
  
  // ******************************************************************
  // Fortran callable methods (thats why we use pointers here)
  // ******************************************************************
  double getTemperature(float* pAltitudeM);  
  double getPressureRatio(float* pAltitudeM);
  double getDensity(float* pAltitudeM);
  double getDepth(float* pAltitudeM);
  double getDepthSum(float* pAltitudeM);
  double getAltitudeM(float* pDepth);
  double getEta(float*  pAltitudeM, float* pLambdaNM);
  
  string getTitle(){return fAtmTitle;};

};
#endif



