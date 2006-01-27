//atmosphere76.cpp
//	This file has has the C++ routines for the atmosphere76 class
//      See atmosphere76.h for more comments
//   G.H.Sembroski
//   Physics Dept.
//   Purdue Univ.
//   W.Lafayette, In 47907
//sembroski@physics.purdue.edu
//   01/12/04
//Modified:

#include "atmosphere76.h"

// *************************************************************************


atmosphere76::atmosphere76()
//Constructor of atmospher76. Sets up lookup tables of region depths.
{
  altitudeNow=-1;
  for(int i=0;i<NTab-1;i++)
    { 
      double alt=getGeometricAltitude(htab[i]);
      regionDepth[i]=getRegionDepth(alt);
    }


  //Too add the remainder we have to match with the last region
  //assume exponetial behavior with no temp dependance(that assumption allows
  //us to make an analog calculation)
  double alt6=getGeometricAltitude(htab[NTab-2]);
  double alt7=getGeometricAltitude(htab[NTab-1]);
  double ratm6=getDensity(alt6);
  double ratm7=getDensity(alt7);
  b7atm=log(ratm6/ratm7)/(alt7-alt6);   
  a7atm=ratm6*exp(b7atm*alt6);          //gm/cm3
 
  regionDepth[NTab-1]=a7atm/b7atm*exp(-b7atm*alt7);//Beyond Top region.

  //setup lookup tables for fast depth determination. Will use interpolation
  topAlt=getGeometricAltitude(htab[NTab-1]);  //integer meters
  NDepth= (int)(topAlt/DepthStepSize) +2; //1 for 0 and 1 to get past top
  depthTable = new double[NDepth];
  topAlt=(NDepth-1)*DepthStepSize;//This altitude should be just beyond our
                                     //top region
  topAltDepth=getDepthSum(topAlt); //Get the top
  depthTable[NDepth-1]=topAltDepth;

  seaLevelDepth=getDepthSum(0.);       //Get sealevel depth
  //First element this table at depth i.e topAlt
  NAltDepth=(int)((seaLevelDepth-topAltDepth)/altDepthStepSize) +1;
  altDepthTable=new altDepth[NAltDepth];
  int j=0;
  altDepthTable[j].altitude=topAlt;
  altDepthTable[j].depth=topAltDepth;
    
  int d=(int)(altDepthTable[0].depth/altDepthStepSize);
  double dpth=altDepthStepSize*d+altDepthStepSize;  //First depth to search for
  //fill up the table

  double bstep;
  double top=altDepthTable[0].altitude;
  double depth=altDepthTable[0].depth;
  for(int i=NDepth-2;i>=0;i--)
    {
      double alt=i*DepthStepSize;
      for(double a=alt;a<top;a++)          //1 meter steps
	{
	  if(top-a<1.0)
	    {
	      bstep=100*(top-a);     //cm
	    }
	  else
	    {
	      bstep=100.;
	    }                             //cm
	  depth=depth+getDensity(a)*bstep;      // in gm/cm**2
	  if(depth>=dpth)          //See if time to make entry in altDepthTable
	    {
	      j++;
	      altDepthTable[j].depth=depth;
	      altDepthTable[j].altitude=a;
	      if(j != (int)depth)
		{
		  cout<<"altDepthSize is too small"<<endl;
		  exit(0);
		}
	      dpth=dpth+altDepthStepSize;
	    }
	  depthTable[i]=depth;
	  top=alt;
	}
    } 
  altDepthTable[NAltDepth-1].depth=depthTable[0];
  altDepthTable[NAltDepth-1].altitude=0;
  seaLevelDepth=depthTable[0];
}
// *************************************************************************

atmosphere76::~atmosphere76()
{
  delete [] depthTable;
  delete [] altDepthTable;
}
// *************************************************************************

int atmosphere76::getRegionIndex(double h)
  // determine what region we are in. Use this binary search algorithum
  //Note that if h is above htab[NTab-1](top altitude)this returns NTab-1
{
  int j=NTab-1;                               //setting up for binary search
  int i=0;
  for (;;)
    {
      int k=(i+j)/2;                        //integer division
      if(h < htab[k])
	{
	  j=k;
	}
      else
	{
	  i=k;
	}
      if(j <= i+1)
	{
	  return i;
	}
    }
}
// *************************************************************************

double atmosphere76::getTemperature(double altitude)
  // Determins temperature at altitude. 
  // altitude is geometric altitude in meters
  // This is straight from the f90 subroutine Atmosphere (see .h file)
{
  if(altitude<0)
    {
      altitude=0;
    }
  if(altitude != altitudeNow)//See if last call already calculated some things
    {                        //This is just for speed
      h = getGeopotentialAltitude(altitude); //h is in KM
      regionIndex = getRegionIndex(h);
      deltah=h-htab[regionIndex];          //distance into region(km)
      altitudeNow=altitude;
      tlocal = ttab[regionIndex]+gtab[regionIndex]*deltah;      //Temp at h
    }
                                    //(note for isothermal this is just ttab)
  return tlocal;
}
// *************************************************************************

double atmosphere76::getPressureRatio(double altitude)
  // Determines pressure ratio (relative to sealevel pressure) at 
  // altitude(meters)
{
  if(altitude<0)
    {
      altitude=0;
    }
  double temp=getTemperature(altitude);  //gets lots of stuff includeing:
                                //regionIndex:region index
                                //deltah: geopotential distance into region(km)
  double delta;                 //presure ratio
  if(gtab[regionIndex]== 0.0)
    {
      delta=ptab[regionIndex]*exp(-GMR*deltah/ttab[regionIndex]);
                //Pressure varies exponantionally in non-isothermal region
    }
  else
    {                                          //Power law for isothermal
      delta=ptab[regionIndex]*
	pow((ttab[regionIndex]/temp),(GMR/gtab[regionIndex])); 
    }
  return delta;
}
// *************************************************************************

double atmosphere76::getDensity(double altitude)
  // Determines density in gm/cm**3 at altitude(meters)
{
  if(altitude<0)
    {
      altitude=0;
    }
  double delta=getPressureRatio(altitude); //return pressure relative to that 
                                           //at sea level.
                                           // also causes local temperature
                                           //(tlocal) to be calculated
  double theta=tlocal/ttab[0];             //temperature ratio
  double sigma=delta/theta;                //density ratio
  double density=sigma*SeaLevelDensity;    //density in gm/cm**3
  return density;
}
// *************************************************************************

double atmosphere76::getDepthSum(double altitude)  
  // Get gm/cm**2 at this altitude (also called interaction depth,overburden,
  //  etc.)
  //This is the tough one.
  //We precalculate the total 'thickness' of each region before hand doing a 
  //numerical integration using getDensity.
  //Here we again do a numerical integration from altitude to the top of the
  //local region. Then we add in the thicknesses of each reagion above to 
  //get the total thickness.
{
  if(altitude<0)
    {
      altitude=0;
    }
  double altTop=getGeometricAltitude(htab[NTab-1]);
  if(altitude>altTop)
    {
      double dph=a7atm/b7atm*exp(-b7atm*altitude);
      return dph;
    }
  // Find the region we start in.
  int iRegion=getRegionIndex(getGeopotentialAltitude(altitude));
  //get thickness this region

  double depth=getRegionDepth(altitude); //Numericall integrate this 
                                        //region from altitude to top 
    if(iRegion < NTab-1)
    {
      for(int i=iRegion+1;i<NTab;i++)
	{
	  depth=depth+regionDepth[i];
	}
    }
  return depth;
}
// *************************************************************************

double atmosphere76::getRegionDepth(double altitude)
  //Do numerical integration in this region from altitude to top of region
{
  //Are we above our regions?
  if(altitude>getGeometricAltitude(htab[NTab-1]))
    {
      return 0;   // Assume zero depth above 86 km
    }
  

  //Top altitude of region in meters
  int iRegion=getRegionIndex(getGeopotentialAltitude(altitude));
  double altitudeTop=getGeometricAltitude(htab[iRegion+1]);
  double altitudeStepSize=(altitudeTop-altitude)/NSteps; //steps in m
  double bstep=altitudeStepSize*100;       //step size in cm
  double depth=0;
  for(int i=0;i<NSteps;i++)
    {
      double alt=(i*altitudeStepSize+altitude);  //alt in meters
      double den=getDensity(alt);
      depth=depth+den*bstep;               // in b=gm/cm**2
    }
  return depth;
}
// *************************************************************************

double atmosphere76::getDepth(double altitude)  
  // Get gm/cm**2 at this altitude (also called interaction depth,overburden,
  //  etc.)This is the fast lookup table version. depthTable has been 
  //preloaded by the constructor.  Table is on DepthStepSize spacing.
  //assum linear interpolation.
{
  //First some checks
  if(altitude<0)
    {
      altitude=0;
    }
  if(altitude>=topAlt)
    {
      double dph=a7atm/b7atm*exp(-b7atm*altitude);
      return dph;
    }
  //Now find index for this altitude
  int iLow=(int) (altitude/DepthStepSize);  //Rounds down
  double altLow=iLow*DepthStepSize;
  if(altLow == altitude)
    {
      return depthTable[iLow];
    }
  int iHigh=iLow+1;
  //Interpolate: Assume linear, pretty good assumption with these step sizes
  double altHigh=iHigh*DepthStepSize;
  double fraction=(altitude-altLow)/(altHigh-altLow);
  double depth=fraction*(depthTable[iHigh]-depthTable[iLow])+depthTable[iLow];
  return depth;
}
// *************************************************************************


double atmosphere76::getAltitude(double depth)  
  // Get the altitude for this depth (This is invers to getDepth)
  //  This uses 2 llokup tables. First we use altDepthTable to get a rough
  //  ideal for the latitude. We then search though the depthTable table 
  //  strating at this altitude. Finally we interpolate to get a final 
  //  altitude.
  //  altDepthTable has been preloaded by the constructor.  Table is on 
  //  AltDepthStepSize spacing.
  //  assum linear interpolation.
{
  //First some checks
  double alt;
  if(depth>seaLevelDepth)
    {
      return 0.0;                        //Minimum altitude is sea_level
    }
  if(depth<=altDepthTable[0].depth)
    {
      alt=-log(depth*b7atm/a7atm)/b7atm;
      return alt;
    }
  //Now find altitude index for this depth
  double tdepth=(depth-altDepthTable[0].depth);     
  int i=(int)(tdepth/altDepthStepSize); 
  if(i==NAltDepth-1)
    {
      i--;
    }
  double altHigh=altDepthTable[i].altitude;
  
  //and the next one
  double altLow=altDepthTable[i+1].altitude;

  //interpolate
  double fraction;
  if(i==NAltDepth-2)
    {
      fraction=(depthTable[0]-depth)/(depthTable[0]-i*altDepthStepSize);
    }
  else
    {
      fraction=((i+1)*altDepthStepSize-depth)/altDepthStepSize;
    }
  alt=fraction*(altHigh-altLow)+altLow;   //Best first guess as to altitude
                                          //do this for speed

  i=(int)(alt/DepthStepSize)+1;  //Get us above

  //search for i,i-1 index range that brackets depth in depthTable.
  //DepthTable decrease with increasing index.
  for(;;)
    {
      if(depthTable[i]== depth)
	{                         //Right on it!(This test must come first!)
	  alt=i*DepthStepSize;
	  return alt;
	}
      else if(depthTable[i]>depth)
	{                          //Opps, we are past , step back.(This test
	  i++;                     //must come second so i-- doesn't put us
	}                          // below 0)
      
      else if(depthTable[i-1]<=depth)   //Take another step down?
	{
	  i--;
	}
      else
	{                         //We got it. Interpolate
	  altLow=(i-1)*DepthStepSize;
	  altHigh=(i)*DepthStepSize;
	  fraction=
	    (depth-depthTable[i-1])/(depthTable[i]-depthTable[i-1]);
	  alt=fraction*(altHigh-altLow)+altLow;
	  return alt;
	}
    }
}
