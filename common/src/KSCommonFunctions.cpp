//-*-mode:c++; mode:font-lock;-*-

#include <iostream>
#include <cmath>

extern "C" float pran(float* fXDummy);
extern "C" double Gauss();
extern "C" void  MassNumber2ChargeAndMass(int fIAtomicNum,int& fCharge, 
				      double& fMass);
extern "C" int   NFluct(double fMean);
extern "C" double  Rexp(double fScaleFactor);
extern "C" int   NearestInt(double fX);
// **********************************************************************

double Gauss()
{
  //	Modified:
  //    3/14/06 SRC  Converted program to C++ from FORTRAN	
  //	2/4/92 G.H.S. V:1:0:1:0:0.4
  //		Fix width of GAUSS. It was .6932. Make it 1.0 so nfluct has
  //		correct width.
  //	NOTE: This funciton only gives values out to 3/.6932=4.33 sigma and 
  //	above 3 sigma its not perfect(but its not bad!).
  //    This is petes for use by nfluct.
  float fXDummy;
  
  //	Sum 6 random numbers(whose mean will be 3)
  double fSum;
  for (int i=0;i<6;i++)
    {
      fSum= fSum + pran(&fXDummy);
    }
  double fGauss=((fSum-3.)/.6932);	//Put mean at 0. Correct width.
  return fGauss;
}
// **********************************************************************

static  bool  fFirstArgon = true;

void MassNumber2ChargeAndMass(int fIAtomicNum,int& fCharge, double& fMass)
// **************************************************************************
//   Determine charge Z of most stable nuclei of nuclear number A
//   Determine mass from Z and A using semi-empirical mass formula
// ***************************************************************************
//   Uses fromulae from "Physics of Nuclei and Particles, Vol 1", Marmier and 
//   Sheldon,1969, Acedemic Press, pg 36-38, formula: 2-35,2-38,2-39,2-40
// ***************************************************************************
//   This is from the liquid drop model of stable nuclei.

//  Written by:
//  Glenn Sembroski
//  Physics Dept.
//  Purdue Univ.
//  W. Lafayette, IN USA 47907

// Modified:
//	SRC 3/15/06 Converted from FORTRAN to C++
{
  double fAtomicNum  = (double)fIAtomicNum;
  double fAtomicNumTwoThirds = pow(fAtomicNum,(2./3.));

  const double  fMassProton  = 938.256e-6;  //Mass of proton (TeV)
  const double  fMassNeutron = 939.550e-6;  //Mass of neutron(TeV)
  const double  fPaircoef    = 33.5e-6;     //Pairing coef.(TeV)
  const double  fVolcoef     = 14.1e-6;     //Volume coef (TeV)
  const double  fSurfcoef    = 13.0e-6;     //Surface coef(TeV)
  const double  fCoulcoef    = 0.595e-6;    //Coulmb coef.(TeV)
  const double  fAsymcoef    = 19e-6;       //Assymetry coef(TeV)


  // ***********************************************************************
  //Correct our formula for elements up to a=56(Fe) which is as high as 
  //NUC_LIB goes.
  // ***********************************************************************
  switch(fIAtomicNum) 
    {  
    case 18:fCharge= 8;break;	  //Force Oxygen isotope
    case 24:fCharge=12;break;     //Force Magnesium
    case 28:fCharge=14;break;     //Force silicon
    case 32:fCharge=16;break;     //Force Sulpher
    case 33:fCharge=16;break;     //Force Sulpher
    case 35:fCharge=17;break;     //Force Chlorine
    case 39:fCharge=19;break;     //Force Potassium
    case 40:fCharge=18;           //Force Argon //Could he been calcium 40.
            if(fFirstArgon)
	      { 
		std::cout<<"Warning--Forcing Argon for all atomic masses of 40"
			 <<std::endl;
	  
		fFirstArgon=false;
	      }
	    break;
    case 56:fCharge=26;break;         //Force Iron.
    default:
      {
	double fChrg=(fAtomicNum/(1.98+(0.0155*fAtomicNumTwoThirds))); 
	fCharge= NearestInt(fChrg);
	//Use nearest integer 
      }
    }
  // *******************************************************************
  // Determine Mass from liquid drop model
  // First determine pairing mass term
  // *******************************************************************
  double fPairingMass;
  if((fCharge % 2)==0)   //even
    {
      if((fIAtomicNum % 2)==0)
	{
	  fPairingMass=-fPaircoef*(pow(fAtomicNum,-.75));   //even-even nuclei
	}
      else
	{
	  fPairingMass=0.;              //even-odd nuclei
	}
    }
  else
    { //charge odd
      if((fIAtomicNum % 2)==0)
	{
	  fPairingMass=0.;	  //Odd-even nuclei
	}
      else
	{
	  fPairingMass=fPaircoef*(pow(fAtomicNum,-.75)); //Odd-odd  nuclei
	}             
    }
  
  fMass = (float)( fCharge*fMassProton 
		   +(fAtomicNum-fCharge)*fMassNeutron 
		   -fVolcoef*fAtomicNum 
		   +fSurfcoef*fAtomicNumTwoThirds  
		   +fCoulcoef*(fCharge*fCharge/pow(fAtomicNum,(1./3.))) 
		   +fAsymcoef*pow((fAtomicNum-2*fCharge),2)/fAtomicNum  
		   +fPairingMass );
  return;
}
// ***************************************************************************

int NFluct(double fMean)
// *************************************************************************
//      Puts a statistical fluctuation on number of pe's(photons)
// *************************************************************************
//      written by:  Pete Palfrey
//                   Purdue
//                   4/1/89

//	Modified:
//	11/5/98 GHS V:1:1:5.3
//		Fix bug in fNFLUCT. For small values of x (x<12) check for
//		values of fNFluct out to 18. before we were limited to 4*x.
//		This was especially bad for x<.25. This error became very 
//		noticeable when we went to segment step sizes of .02 radiation
//		lengths.

{
  float fXDummy;
  // *****************************************************************
  //      See if fluctuation will be poisson.
  // *****************************************************************
  if(fMean<12.)         //Poisson
    {
      double fSum=exp(-fMean);
      double fOld=fSum;
      double fTestVal=pran(&fXDummy);
      //	11/5/98 GHS V:1:1:5.3 Let fMaxi always be 18.
      //                fMaxi=min(4.*fMean,18.)
      int fMaxI=18;
      for (int i=0;i<fMaxI;i++)
	{
	  if(fSum>fTestVal)
	    {
	      int fNFluct=i-1;
	      return fNFluct;
	    }    
	  fOld=fOld*fMean/i;
	  fSum=fSum+fOld;
	} 
      return fMaxI;
    }
  else
    {  
      // *****************************************************************
      //       Fluctuation is gaussian distributed.
      // *****************************************************************
      double fSca=sqrt(fMean)*Gauss() + fMean;// Remember this 
                           //gauss function is only good out to 4.33 sigma
      if(fSca>0.)
	{
	  return (int)(fSca);  //Round down
	}
      else
	{
	  return 0;
	}
    }
}

// ****************************************************************************


double  Rexp(double fScaleFactor)
// ***************************************************************************
//     This function randomly picks from an exponential distribution using
//     X as the scale factor.
//     Its used for interaction depth(Xis is gm/cm**2) and for decay length
//     (where x is in meters)
//     Rexp is distributed as exp(-s/X)
{
  float fXDummy;
  double fRandomNum = pran(&fXDummy);
  // ***************************************************  
  //      This produces the required random distribution.
  // ***************************************************
  double fRexp = -fScaleFactor*log(fRandomNum);
  return fRexp;
}
// ****************************************************************************

int NearestInt(double fX)
{
  // Return nearest integer
  int fN=(int)floor((double)fX);
  if((fX-fN)<(fN+1-fX))
    {
      return fN;
    }
  else
    {
      return fN+1;
    }
}
// ****************************************************************************

