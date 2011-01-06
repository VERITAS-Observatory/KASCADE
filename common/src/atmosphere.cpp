//atmosphere7.cpp
//	This file has has the C++ routines for the atmosphere class
//      See atmosphere.h for more comments //   G.H.Sembroski//   Physics Dept.
//   Purdue Univ.
//   W.Lafayette, In 47907
//sembroski@physics.purdue.edu
//   01/12/04
//Modified:

#include "atmosphere.h"
#include <cstdlib>

// *************************************************************************
// This is an expanded version of the orginal atmosphere76.cpp code.
// This version allows for selction of either the US76 atmosphere or the
// use of any of the atmprof().dat files as used by Corsika. In particular
// the use of the VERITAS specific atmprof21.dat (Veritas Winter atm) and
// atmprof22.dat (Veritas Summer), though any file in the same format will
// work.
// *************************************************************************
// Specification of what atmosphere model to use is made in the init method.
// *************************************************************************


atmosphere::atmosphere()
//Constructor of atmosphere. Sets up lookup tables of region depths.
{
  // *****************************************************************
  fUS76Initalized=false;
}
// *******************************************************************
atmosphere::~atmosphere()
{
  if(fUS76Initalized){
    delete [] depthTable;
    delete [] altDepthTable;
  }
}
// *************************************************************************

void atmosphere::init(string atmProfileFileName)
// *******************************************************************
// Init stuff according to atm type
// program that init this class passes atmProfileFileName which can be either 
// "US76" or the name of the atmprof file (with path if needed) top use.
// *******************************************************************
{
  // ****************************************************************
  // Test if file or US76 specified
  // We do it this funny way so that when the string gotten from the 
  // segment_head is checked this will work.
  // ****************************************************************
  string atmUS76=atmProfileFileName.substr(0,4);
  if(atmUS76=="US76"){
    fUseUS76atm=true;
    if(!fUS76Initalized){
      initUS76();
      fUS76Initalized=true;
      fAtmTitle="US76";
    }
  }
  else{
    fUseUS76atm=false;
    initAtmProf(atmProfileFileName);
  }
  return;
}
// **********************************************************************


double  atmosphere::getTemperature(float* pAltitudeM)
// ***********************************************************************
// Use method specified in init.
// ***********************************************************************
{
  if(fUseUS76atm){
    fTemperature=getTemperatureUS76(*pAltitudeM);
    return fTemperature;
  }
  else{
    cout<<"atmosphere::getTemperature for atmprof files not implemented. Should "
      "never be called!"<<endl;
    exit(EXIT_FAILURE);
  }
}
// ************************************************************************

double atmosphere::getPressureRatio(float* pAltitudeM)
// ***********************************************************************
// Use method specified in init.
// ***********************************************************************
{
  if(fUseUS76atm){
    fPressureRatio=getPressureRatioUS76(*pAltitudeM);
    return fPressureRatio;
  }
  else{
     cout<<"atmosphere::getPressureRatio for atmprof files not implemented. Should "
      "never be called!"<<endl;
    exit(EXIT_FAILURE);
  }
}
// ************************************************************************

double atmosphere::getDensity(float* pAltitudeM)
// ***********************************************************************
// Use method specified in init.
// ***********************************************************************
{
  if(fUseUS76atm){
    fDensity=getDensityUS76(*pAltitudeM);
  }
  else{
    fDensity=getDensityAtmProf(pAltitudeM);
  }
  return fDensity;
}
// ************************************************************************

double atmosphere::getDepth(float* pAltitudeM)
// ***********************************************************************
// Use method specified in init.
// ***********************************************************************
{
  if(fUseUS76atm){
    fDepth=getDepthUS76(*pAltitudeM);
  }
  else{
    fDepth=getDepthAtmProf(pAltitudeM);
  }
  return fDepth;
}
// ************************************************************************

double atmosphere::getDepthSum(float* pAltitudeM)
// ***********************************************************************
// Use method specified in init. This for US76 is high precision vertion
// of getDepth. For AtmProf its the same. 
// ***********************************************************************
{
  if(fUseUS76atm){
    fDepthSum=getDepthSumUS76(*pAltitudeM);
    return fDepthSum;
  }
  else{
     cout<<"atmosphere::getDepthSum for atmprof files not implemented. Should "
      "never be called!"<<endl;
    exit(EXIT_FAILURE);
  }
}
// ************************************************************************

double atmosphere::getAltitudeM(float* pDepth)
// ***********************************************************************
// Use method specified in init.
// ***********************************************************************
{
  if(fUseUS76atm){
    fAltitudeM=getAltitudeMUS76(*pDepth);
  }
  else{
    fAltitudeM=getAltitudeMAtmProf(pDepth);
  }
  return fAltitudeM;
}
// ************************************************************************
double atmosphere::getEta(float* pAltitudeM, float* pLambdaNM)
// ***********************************************************************
// Use method specified in init.
// ***********************************************************************
{
  if(fUseUS76atm){
    fEta=getEtaUS76(*pAltitudeM, *pLambdaNM);
  }
  else{
    fEta=getEtaAtmProf(pAltitudeM,pLambdaNM);
  }
  return fEta;
}
// ************************************************************************

// ************************************************************************
//  U. S. 76  A T M O S P H E R E
// ************************************************************************

void atmosphere::initUS76()
// *******************************************************************
// Initalized use of US76 atmosphere
// ********************************************************************
{
  altitudeNow=-1;
  for(int i=0;i<NTab-1;i++)
    { 
      double alt=getGeometricAltitude(htab[i]);
      regionDepth[i]=getRegionDepth(alt);
    }

  // ****************************************************************
  //Too add the remainder we have to match with the last region
  //assume exponetial behavior with no temp dependance(that assumption allows
  //us to make an analog calculation)
  // ****************************************************************
  double alt6=getGeometricAltitude(htab[NTab-2]);
  double alt7=getGeometricAltitude(htab[NTab-1]);
  double ratm6=getDensityUS76(alt6);
  double ratm7=getDensityUS76(alt7);
  b7atm=log(ratm6/ratm7)/(alt7-alt6);   
  a7atm=ratm6*exp(b7atm*alt6);          //gm/cm3
 
  regionDepth[NTab-1]=a7atm/b7atm*exp(-b7atm*alt7);//Beyond Top region.

  // ***********************************************************************
  // The folloing is for getAltitudeUS76 I think and may be extracted to a
  //seperate method later for use by the atmProf approach.
  // ***********************************************************************
  //setup lookup tables for fast depth determination. Will use interpolation
  topAlt=getGeometricAltitude(htab[NTab-1]);  //integer meters
  NDepth= (int)(topAlt/DepthStepSize) +2; //1 for 0 and 1 to get past top

  depthTable = new double[NDepth];
  topAlt=(NDepth-1)*DepthStepSize;//This altitude should be just beyond our
                                     //top region
  topAltDepth=getDepthSumUS76(topAlt); //Get the top
  depthTable[NDepth-1]=topAltDepth;
  seaLevelDepth=getDepthSumUS76(0.);       //Get sealevel depth

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
	  depth=depth+getDensityUS76(a)*bstep;      // in gm/cm**2
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

  initEtaSeaLevelBasic();

  return;
}
// *************************************************************************


int atmosphere::getRegionIndex(double h)
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

double atmosphere::getTemperatureUS76(float alt)
  // Determins temperature at altitude. 
  // altitude is geometric altitude in meters
  // This is straight from the f90 subroutine Atmosphere (see .h file)
{
  double altitudeM=(double)alt;
  if(altitudeM<0)
    {
      altitudeM=0;
    }
  if(altitudeM != altitudeNow)//See if last call already calculated some things
    {                        //This is just for speed
      h = getGeopotentialAltitude(altitudeM); //h is in KM
      regionIndex = getRegionIndex(h);
      deltah=h-htab[regionIndex];          //distance into region(km)
      altitudeNow=altitudeM;
      tlocal = ttab[regionIndex]+gtab[regionIndex]*deltah;      //Temp at h
    }
                                    //(note for isothermal this is just ttab)
  return tlocal;
}
// *************************************************************************

double atmosphere::getPressureRatioUS76(float alt)
  // Determines pressure ratio (relative to sealevel pressure) at 
  // altitudeM(meters)
{
  double altitudeM=(double)alt;
  if(altitudeM<0)
    {
      altitudeM=0;
    }
  double temp=getTemperatureUS76(altitudeM);  //gets lots of stuff includeing:
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

double atmosphere::getDensityUS76(float alt)
  // Determines density in gm/cm**3 at altitudeM(meters)
{
  double altitudeM=(double)alt;
  if(altitudeM<0)
    {
      altitudeM=0;
    }
  // *********************************************************************
  // return pressure relative to that  at sea level. also causes local
  // temperature(tlocal) to be calculated
  // ********************************************************************
  double delta=getPressureRatioUS76(altitudeM); 
  
  double theta=tlocal/ttab[0];             //temperature ratio
  double sigma=delta/theta;                //density ratio
  double density=sigma*SeaLevelDensity;    //density in gm/cm**3
  return density;
}
// *************************************************************************

double atmosphere::getDepthSumUS76(float alt)  
  // Get gm/cm**2 at this altitude (also called interaction depth,overburden,
  //  etc.)
  //This is the tough one.
  //We precalculate the total 'thickness' of each region before hand doing a 
  //numerical integration using getDensityUS76.
  //Here we again do a numerical integration from altitude to the top of the
  //local region. Then we add in the thicknesses of each reagion above to 
  //get the total thickness.
{
  double altitudeM=(double)alt;
  if(altitudeM<0)
    {
      altitudeM=0;
    }
  double altTop=getGeometricAltitude(htab[NTab-1]);
  if(altitudeM>altTop)
    {
      double dph=a7atm/b7atm*exp(-b7atm*altitudeM);
      return dph;
    }
  // Find the region we start in.
  int iRegion=getRegionIndex(getGeopotentialAltitude(altitudeM));
  //get thickness this region

  double depth=getRegionDepth(altitudeM); //Numericall integrate this 
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

double atmosphere::getRegionDepth(double altitudeM)
  //Do numerical integration in this region from altitudeM to top of region
{
  //Are we above our regions?
  if(altitudeM>getGeometricAltitude(htab[NTab-1]))
    {
      return 0;   // Assume zero depth above 86 km
    }
  

  //Top altitudeM of region in meters
  int iRegion=getRegionIndex(getGeopotentialAltitude(altitudeM));
  double altitudeTop=getGeometricAltitude(htab[iRegion+1]);
  double altitudeStepSize=(altitudeTop-altitudeM)/NSteps; //steps in m
  double bstep=altitudeStepSize*100;       //step size in cm
  double depth=0;
  for(int i=0;i<NSteps;i++)
    {
      double alt=(i*altitudeStepSize+altitudeM);  //alt in meters
      double den=getDensityUS76(alt);
      depth=depth+den*bstep;               // in b=gm/cm**2
    }
  return depth;
}
// *************************************************************************

double atmosphere::getDepthUS76(float alt)  
  // Get gm/cm**2 at this altitudeM (also called interaction depth,overburden,
  //  etc.)This is the fast lookup table version. depthTable has been 
  //preloaded by the constructor.  Table is on DepthStepSize spacing.
  //assum linear interpolation.
{
  //First some checks
  double altitudeM=(double)alt;
  if(altitudeM<0)
    {
      altitudeM=0;
    }
  if(altitudeM>=topAlt)
    {
      double dph=a7atm/b7atm*exp(-b7atm*altitudeM);
      return dph;
    }
  //Now find index for this altitudeM
  int iLow=(int) (altitudeM/DepthStepSize);  //Rounds down
  double altLow=iLow*DepthStepSize;
  if(altLow == altitudeM)
    {
      return depthTable[iLow];
    }
  int iHigh=iLow+1;
  //Interpolate: Assume linear, pretty good assumption with these step sizes
  double altHigh=iHigh*DepthStepSize;
  double fraction=(altitudeM-altLow)/(altHigh-altLow);
  double depth=fraction*(depthTable[iHigh]-depthTable[iLow])+depthTable[iLow];
  return depth;
}
// *************************************************************************


double atmosphere::getAltitudeMUS76(float dpth)  
  // Get the altitudeM for this depth (This is invers to getDepthUS76)
  //  This uses 2 llokup tables. First we use altDepthTable to get a rough
  //  ideal for the latitude. We then search though the depthTable table 
  //  strating at this altitudeM. Finally we interpolate to get a final 
  //  altitudeM.
  //  altDepthTable has been preloaded by the constructor.  Table is on 
  //  AltDepthStepSize spacing.
  //  assum linear interpolation.
{
  //First some checks
  double depth=(double)dpth;

  double alt=0;
  if(depth>seaLevelDepth)
    {
      return 0.0;                        //Minimum altitudeM is sea_level
    }
  if(depth<=altDepthTable[0].depth)
    {
      alt=-log(depth*b7atm/a7atm)/b7atm;
      return alt;
    }
  //Now find altitudeM index for this depth
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
  alt=fraction*(altHigh-altLow)+altLow;   //Best first guess as to altitudeM
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

double atmosphere::getEtaUS76(float altitudeM, float lambdaNM)
// *********************************************************************
// Eta at altitudeM and lambda is eta at sealevel time density ratio
// *********************************************************************
// Etasea is precalculated on 5 nm steps from 180 to 700 nm using the Caucy 
// formula
// *********************************************************************
{
  int i=((lambdaNM-180)/5);  // since index starts at 0 for C++;
  float seaLevelM=0;
  double eta=
    fEtaSeaLevel.at(i)*getDensityUS76(altitudeM)/getDensityUS76(seaLevelM);
  return eta;
}
// ********************************************************************

void atmosphere::initEtaSeaLevelBasic()
// **********************************************************************
// Fill fEtaSeaLevel vector: (Eta=Index of refraction-1) at sea level. Start at
// 180 nanometers and go up to 700 nanometers in 5 nm steps.
// Cauchy formula parameters from CRC handbook of Chem and Phy. 70'-71'
// pg E-231.
// **********************************************************************
{
  int numLambda=(700-180)/5+1;
  fEtaSeaLevel.clear();
  fEtaSeaLevel.resize(numLambda);

  for(int i=0;i<numLambda;i++){
    double lambda=(double)(i*5+180);
    fEtaSeaLevel.at(i)=1.e-7*( 2726.43+12.288/(lambda*lambda*1.e-6)+
			      0.3555/(lambda*lambda*lambda*lambda*1.e-12) );
  }
  return;
}
// **********************************************************************



// ***************************************************************************
// End U. S . 76   A T M O S P H E R E
// ***************************************************************************


// **************************************************************************
// Atmosphere Profile from Lookup tables (atmprof)
// **************************************************************************

void atmosphere::initAtmProf(string atmProfileFileName)
// **************************************************************************
// Read in the lookup tables form the file specified in atmProfileFileName
// *************************************************************************
{
  // **********************************************************************
  // Open the file, make sure it exists. Format of a standard atmProf file is
  // for the first line to be descriptive. Read it and print it out.
  // Skip next 2 lines; The format is:
  //  Alt[km]    density[g/cm^3]  depthsum[g/cm^2]    n-1(eta at 380? nm)
  // ***********************************************************************
  ifstream atmProf(atmProfileFileName.c_str());
  if(!atmProf){
    cout<<"Fatal-Atmospheric profile file "<<atmProfileFileName
	<<" failed to open!"<<endl;
    exit(EXIT_FAILURE);
  }

  // **************************
  // First line is the table lable
  // **************************

  char*  fTitle= new char[256];
  int icount=256;
  atmProf.getline(fTitle,icount);
  fAtmTitle=fTitle;
  
  cout<<"Atmospheric Profile table lable: "<<fAtmTitle<<endl;
  
  // ***************************************
  // Skip second line and thrid lines.
  // ***************************************
  icount=256;
  atmProf.getline(fTitle,icount);
  icount=256;
  atmProf.getline(fTitle,icount);

  // ******************************************
  // Now loop reading in stuff
  // ******************************************
  double alt=0;
  double rho=0;
  double thick=0;
  double eta=0;

  fAltKM.clear();
  fScaledRhoGMC3.clear();
  fLog10GmsGMC2.clear();
  fEta400.clear();

  while(1){
    atmProf>>alt>>rho>>thick>>eta;
    if(atmProf.eof()){
      break;
    }

    // *************************************************
    // Sanity check: AltitudeM should always be increasing
    // *************************************************
    
    if(fAltKM.size()!=0 && fAltKM.at(fAltKM.size()-1)>alt){
      cout<<"Altitudes not in ascending order in atmProf file: "
	  <<atmProfileFileName<<endl;
      exit(EXIT_FAILURE);
    }

    // ******************************************************************
    // Now we build the tables we need
    // ******************************************************************
    // We will be doing linear interpolation along the tables so its best
    // if we keep functions of the variables that are linear or at least 
    // smoothly changing in the tables
    // For example, the log(Gms) is pretty linear, so those values in the
    // tables
    // ****************************************************************** 
    fAltKM.push_back(alt);
    fScaledRhoGMC3.push_back(rho*exp(alt/kAtmProfAltScaleHeightKM));
    fLog10GmsGMC2.push_back(log10(thick));
    fEta400.push_back(eta);   // Except for sea level (alt=0) we don't really use this eta
  }

  atmProf.close();

  // **********************************************************
  // For atmprof we have to find the set of sealevel eta's.
  // The etas that came with the atmprof file are for a lambda of 400 nm
  // (private communication: Gernot Maier Dec 17, 2010)
  // We use this to find the constant (density ratio) to convert the Caucy
  //  sealevel formula eta's to sealevel eta's for our atm.
  // (see  CRC handbook of Chem and Phy. 70'-71' pg E-231.) 
  // **********************************************************
  initEtaSeaLevelBasic();

  int atmEtaLambdaNM=(int)kAtmProfReferenceWavelength;
  int index=(atmEtaLambdaNM-180)/5;
  // ***********************************************
  // Note fEta400 argument is altitudeM, fEtaSeaLevel argument in lambda index
  // ***********************************************
  double densityRatio=fEta400.at(0)/fEtaSeaLevel.at(index);
  int numLambda=fEtaSeaLevel.size();
  for(int i=0;i<numLambda;i++){
    fEtaSeaLevel.at(i)=fEtaSeaLevel.at(i)*densityRatio;
  }
  return;
}
// *************************************************************************


double atmosphere::getEtaAtmProf(float* pAltitudeM, float* pLambdaNM)
// *********************************************************************
// Eta at altitudeM and lambda is eta at sealevel time density ratio
// *********************************************************************
// Etasea is precalculated on 5 nm steps from 180 to 700 nm and made
 // relative to the fEta400(0) read in. See initAtmProf
// *********************************************************************
{

  int i=((*pLambdaNM-180)/5);  //Index starts at 0 for C++;
  float seaLevelM = 0;
 
  double eta=fEtaSeaLevel.at(i)*
    getDensityAtmProf(pAltitudeM)/getDensityAtmProf(&seaLevelM);
  return eta;
}
// **********************************************************************

double atmosphere::getDensityAtmProf(float* pAltitudeM)
// ************************************************************************
// Find density in gm/cm**3 for this atmprof atmosphere from the lookup table.
// Interpolate. In table fScaledRhoGMC3 density is stored as 
// rho*exp(alt/kAtmProfAltScaleHeightKM) to make the linear interpolation
//  more accurate.
// ************************************************************************
{
  // ***************************************************
  // Find table indices, above and below and interpolation fraction
  // ***************************************************
  double fraction=getAtmosphereInterpFractionAtmProf(*pAltitudeM); 
                                        //Results comes back in interators
                                        //fBegin and fEnd
  double density=0;
  int lowAtmIndex=0;
  if(fraction<0){           //Altitude is out of range of table
    if( *pAltitudeM>0){     //Altitude is above table
      density=fScaledRhoGMC3.at(fScaledRhoGMC3.size()-1)/
	                       exp(fAltKM.at(fAltKM.size()-1)/kAtmProfAltScaleHeightKM);;
    }
    else{                  //altitude is below table
      density=fScaledRhoGMC3.at(0)/exp(fAltKM.at(0)/kAtmProfAltScaleHeightKM);
    }
  }

  else if(fBegin==fEnd){ //Altitude is exactly at one of the table entires
    lowAtmIndex=fBegin-fAltKM.begin();//Iterator math, but gives index in table. I think.
    density=fScaledRhoGMC3.at(lowAtmIndex)/exp(*fBegin/kAtmProfAltScaleHeightKM);;
  }

  else{   //Altitude is in table and we have to interpolate
    lowAtmIndex=fBegin-fAltKM.begin();  //Iterator math, but gives index in tables.

    double scaledDensityLow=fScaledRhoGMC3.at(lowAtmIndex);
    double scaledDensityHigh=fScaledRhoGMC3.at(lowAtmIndex+1);
 
    double scaledDensity=scaledDensityLow + fraction*(scaledDensityHigh-scaledDensityLow);
    density = scaledDensity/exp((*pAltitudeM/1000.0)/kAtmProfAltScaleHeightKM);
  }
  return density;
}
// ****************************************************************************************


double atmosphere::getDepthAtmProf(float* pAltitudeM)
// ************************************************************************
// Find depth in gm/cm**2 for this atmprof atmosphere from the lookup table.
// Interpolate. In table fLog10GmsGMC2 depth is stored as log10(depth(gm/cm**2))
// to make the linear interpolation more accurate.
// ************************************************************************
{
  // ***************************************************
  // Find table indices, above and below and interpolation fraction
  // ***************************************************
  double fraction=getAtmosphereInterpFractionAtmProf(*pAltitudeM); 
                                        //Results comes back in interators
                                        //fBegin and fEnd
  double depth=0;
  int lowAtmIndex=0;
  if(fraction<0){           //Altitude is out of range of table
    if( *pAltitudeM>0){     //Altitude is above table
      depth=pow(10.0,fLog10GmsGMC2.at(fLog10GmsGMC2.size()-1));
    }
    else{                  //altitude is below table
      depth=pow(10.0,fLog10GmsGMC2.at(0));
    }
  }

  else if(fBegin==fEnd){ //Altitude is exactly at one of the table entires
    lowAtmIndex=fBegin-fAltKM.begin();//Iterator math, but gives index in table. I think.
    depth=pow(10.0,fLog10GmsGMC2.at(lowAtmIndex));
  }

  else{   //Altitude is in table and we have to interpolate
    lowAtmIndex=fBegin-fAltKM.begin();  //Iterator math, but gives index in tables.

    double log10DepthLow=fLog10GmsGMC2.at(lowAtmIndex);
    double log10DepthHigh=fLog10GmsGMC2.at(lowAtmIndex+1);
    double log10Depth=log10DepthLow + fraction*(log10DepthHigh-log10DepthLow);
    depth=pow(10.0,log10Depth);
  }
  return depth;
}
// ****************************************************************************************

double atmosphere::getAltitudeMAtmProf(float* pDepth)
// ************************************************************************
// Find altitude in Meters for this atmprof atmosphere from the lookup table.
// Interpolate. In table fLog10GmsGMC2 depth is stored as log10(depth(gm/cm**2))
// to make the linear interpolation more accurate.
// ************************************************************************
{
  // ***************************************************
  // Find table indices, above and below and interpolation fraction
  // ***************************************************
  double fraction=getDepthInterpFractionAtmProf(*pDepth); 
                                        //Results comes back in interators
                                        //fBegin and fEnd
  double altitude=0;
  int lowDepthIndex=0;
  if(fraction<0){           //depth is out of range of table
    if( log10(*pDepth)>fLog10GmsGMC2.at(0)){     //Depth is below table
	  altitude=fAltKM.at(0);
    }
    else{                  //depth is above table
      altitude=fAltKM.at(fAltKM.size()-1);
    }
  }

  else if(fBegin==fEnd){ //depth is exactly at one of the table entires
    lowDepthIndex=fBegin-fLog10GmsGMC2.begin();//Iterator math, but gives index in table.
    altitude=fAltKM.at(lowDepthIndex);
  }

  else{   //Depth is in table and we have to interpolate
    lowDepthIndex=fBegin-fLog10GmsGMC2.begin();  //Iterator math, but gives index in tables.

    double altitudeLow=fAltKM.at(lowDepthIndex);
    double altitudeHigh=fAltKM.at(lowDepthIndex+1);
    altitude=altitudeLow + fraction*(altitudeHigh-altitudeLow);
  }
  return altitude*1000.0;
}
// ****************************************************************************************


double  atmosphere::getAtmosphereInterpFractionAtmProf(float altitudeM)
{
  // **********************************************************
  // Table is in KM
  // **********************************************************
  // I got the binary search code from:
  // http://en.literateprograms.org/Binary_search_(C_Plus_Plus)
  // ***********************************************************
  // Note that this returns  iterator.

  fBegin=fAltKM.begin();
  fEnd=fAltKM.end()-1;
  double target=altitudeM/1000.;
  double fraction=0;

  if(target <*fBegin || target>*fEnd ){
    return -1; //Flag that we are out of range.
  }
 
  while (fBegin != fEnd-1){
    fMiddle = fBegin + (fEnd - fBegin)/2; //try halfway between. Integer divide
    if (target < *fMiddle){
      fEnd = fMiddle;
    }
    else if (*fMiddle < target){
      fBegin = fMiddle;
    }
    else{
      fBegin = fMiddle;   //Right on the value.
      fEnd   = fMiddle;
      fraction=1;
      return fraction;
    }
  }
  fraction=(target-*fBegin)/(*fEnd-*fBegin);

  return fraction;
}
// ****************************************************************


double  atmosphere::getDepthInterpFractionAtmProf(float depth)
{
  // **********************************************************
  // Table is in log10(depth(gm/cm**2))M
  // **********************************************************
  // I got the binary search code from:
  // http://en.literateprograms.org/Binary_search_(C_Plus_Plus)
  // ***********************************************************
  // Note that this sets iterators and returns fraction between them.
  // ***********************************************************
  // This is same code as getAtmosphereInterpFractionAtmProf except that
  // fLog10GmsGMC2 decreases with increasing index.
  // **********************************************************
  fBegin=fLog10GmsGMC2.begin();
  fEnd=fLog10GmsGMC2.end()-1;
  double target=log10(depth);
  double fraction=0;

  if(target >*fBegin || target<=*fEnd ){
    //Flag that we are out of range.
    return -1;
  }

 
  while (fBegin != fEnd-1){
    fMiddle = fBegin + (fEnd - fBegin)/2; //try halfway between. Integer divide
    if (target > *fMiddle){
      fEnd = fMiddle;
    }
    else if (*fMiddle > target){
      fBegin = fMiddle;
    }
    else{
      fBegin = fMiddle;   //Right on the value.
      fEnd   = fMiddle;
      fraction=1;
      return fraction;
    }
  }
  fraction=(target-*fBegin)/(*fEnd-*fBegin);

  return fraction;
}
// ****************************************************************


