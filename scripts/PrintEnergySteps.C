//Short root macro to print out energies for equal spacing in log(E)
//Also shows number of shwer we will need at ecah energy
#include <iomanip>

void PrintEnergySteps(std::string particleType = "G", int minNumShowers = 20)
{
  string pType;
  double fNumMin;
  double fNumShowersAt100;
  double fAlpha;
  //int     minNumShowers = 20;
  if ( particleType == "G") {
	pType = "Gamma Rays";
	fNumMin = minNumShowers;   //Gammas
    // fNumShowersAt100=100;   //gammas
	fNumShowersAt100=200;   //gammas
	fAlpha=-2.45; //Gamma Ray spectrum index
  }

  if ( particleType == "E") {
	pType = "Electrons";
	fNumMin = minNumShowers;   //electrons
    // fNumShowersAt100=100;   //electrons
	fNumShowersAt100=200;   //electrons
	fAlpha=-3.18; //electron spectrum index
  }
  
  if ( particleType == "P") {
	pType = "Protons";
	fNumMin = minNumShowers;   //Cosmic rays
	fNumShowersAt100=160;     // protons
	fAlpha=-2.77; //Proton spectrum Index
  }
  if ( particleType=="H") {
	pType = "Helium";
	fNumMin= minNumShowers;   //Cosmic rays
	fNumShowersAt100=108;     //helium
	fAlpha=-2.64;  //Helium spectrum Index
  }
  if ( particleType == "LT") { 
	pType="Lookup Tabvle gen";
	fNumMin=20;   //Lookup table/effective area: Gammas
	fNumShowersAt100=100;   //gammas
	fAlpha=-2.0; //Spectrum index
  }

  int fNumSteps=9;
  double fBaseE=.020;    //TeV base energy
  int fNumEntries=47;
  int fNStart=0;

  double fIAlpha  = fAlpha+1.0;
  double fStepLogE = (log(.100)-log(fBaseE))/fNumSteps;
  double fLogEBase = log(fBaseE);    
  int    fNumE     = 47;
  double fEOld     = .018;
 
  double fE[200];
  double fELow[200];
  double fEHigh[200];
  double fFlux[200];
  double fFrac[200];
  double fNumShowers[200];

  std::cout << " Table for " << pType << ":" <<satd::endl;
  std::cout << "Number of steps from " << fBaseE*1000 << " GeV to 100 GeV: "
			<< fNumSteps << std::endl;
  std::cout << "Spectral Index: " << fAlpha << std::endl;
  std::cout << "---------------------------------------------------"
			<< std::endl;
  std::cout << " Step  E(TeV) LowEdge  High Edge  Number  %BinWidth "
			<< std::endl;
  for(int i=0;i<fNumE;i++) {
	double fLogE=fLogEBase+i*fStepLogE;
	fE[i]=exp(fLogE);
	fFrac[i]=(fE[i]-fEOld)/fE[i];
	double fLogELow=fLogE-.5*fStepLogE;
	double fLogEHigh=fLogE+.5*fStepLogE;
	fELow[i]=exp(fLogELow);
	fEHigh[i]=exp(fLogEHigh);
	fFlux[i]=pow(fEHigh[i],fIAlpha)-pow(fELow[i],fIAlpha);
	fEOld=fE[i];
  }

  //Set flux to be 1 at 100 GeV i=fNumSteps
  int fNum=0;
  for(int i=0;i<fNumE;i++) {
	fNumShowers[i]=fNumShowersAt100*fFlux[i]/fFlux[fNumSteps];
	fNumShowers[i]=ceil(fNumShowers[i]);
	if(fNumShowers[i]<fNumMin) {
	  fNumShowers[i]=fNumMin;
	}
 
	std::cout << std::setw(5) << i <<": " << fE[i] << " " << std::setw(7) 
			  << std::setprecision(6) << fELow[i] << " " << std::setw(7) 
			  << std::setprecision(6) << fEHigh[i] << " " << std::setw(7) 
			  << std::setprecision(4) << fNumShowers[i] << " " << std::setw(7)
			  << std::setprecision(4) << fFrac[i] << std::endl;
	fNum+=(int)fNumShowers[i];
  }
  std::cout << "Total Number of Showers: " << fNum << std::endl;
  std::cout << "energy=([1]=";

  for(int i=fNStart;i<fNumEntries;i++) {
	double fEnergy=fE[i]*1000;
	if(fEnergy<200) {
	  std::cout << fEnergy << " ";
	}
	else {
	  int fEInt=(int)(fEnergy);
	  std::cout << fEInt << " ";
	}
  }
  std::cout << ")" << std::endl;

  std::cout << "start=([1]=";
  for(int i=fNStart;i<fNumEntries;i++) {
	std::cout << 1 << " ";
  }
  std::cout << ")" << std::endl;

  std::cout << "end=([1]=";
  for(int i=fNStart;i<fNumEntries;i++) {
	std::cout << fNumShowers[i] << " ";
  }
  std::cout << ")" << std::endl;
  return;
}
// ***********************************************************************
