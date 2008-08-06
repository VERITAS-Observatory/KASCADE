#ifndef ATMOSPHERE76_H
#define ATMOSPHERE76_H
//This is the include file for the atmosphere76 class.
//This class is used to generate the US standard Atmosphere 1976 vlaues.
//This includes Pressure,Density,and Temperature of the atmosphere as a
//function of altitude. Its good up to 86.0 km and pretty good after that.
// It steals unashamably from a subroutine: Atmosphere, witten by Ralph 
// Carmichael, Public Domain Aeronautical Software which was found at 
// www.pdas.com/atmos.htm
// GHS converted it from f90, put it into a class and added gms and yds
// functions.
// ***********************************************************************
//   G.H.Sembroski
//   Physics Dept.
//   Purdue Univ.
//   W.Lafayette, In 47907
//   sembroski@physics.purdue.edu
//   01/12/04

//Modified:
// ****************************************************************************
#include <iostream>
#include <cmath>
using namespace std;
#include "exception.h"

// ***********************************************************************
//Some local constants
// ***********************************************************************
const double  RadiusEarth = 6369.0;             //radius of the Earth (km)
const double  GMR = 34.163195;                      //hydrostatic constant
const int     NTab=8;           //number of entries in the defining tables
const double SeaLevelDensity = 1.225e-3;                        //gm/cm**3
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

struct altDepth
{
  double altitude;
  double depth;
};

// ***************************************************************************
//Define the class
class atmosphere76
{
 private:
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
  double getRegionDepth(double altitude);

    public:
  atmosphere76();
  ~atmosphere76();
  double getTemperature(double altitude);
  double getPressureRatio(double altitude);
  double getDensity(double altitude);
  double getDepth(double altitude);
  double getDepthSum(double altitude);
  double getAltitude(double depth);
};
#endif



