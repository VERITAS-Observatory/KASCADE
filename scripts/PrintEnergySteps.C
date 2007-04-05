//Short root macro to print out energies for ewual spacing in log(E)

#include <iomanip>

PrintEnergySteps()
{
  //double fStepLogE=.18;
  //double fLogEBase=log(.100)-13*fStepLogE;    //Start at .1 TEV and backup 
  //int fNumSteps=9;
  int fNumSteps=9;
  
  double fAlpha=-2.45;
  double fIAlpha=fAlpha+1.0;
  double fStepLogE=(log(.100)-log(.02))/fNumSteps;
  double fLogEBase= log(.02);    
  int fNumE=100;
  double fEOld=.018;
  double fNumShowersAt100=100;
  double fNumMin=4;

  double fE[100];
  double fELow[100];
  double fEHigh[100];
  double fFlux[100];
  double fFrac[100];
  double fNumShowers[100];

  std::cout<<"Number of steps from 20 GeV to 100 GeV: "<<fNumSteps<<std::endl;

  for(int i=0;i<fNumE;i++)
    {
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
  for(int i=0;i<fNumE;i++)
    {
      fNumShowers[i]=fNumShowersAt100*fFlux[i]/fFlux[fNumSteps];
      fNumShowers[i]=ceil(fNumShowers[i]);
      //if(fNumShowers[i]<fNumMin)
      //	{
      //	  fNumShowers[i]=fNumMin;
      //	}
 
      std::cout<<std::setw(5)<<i<<": "<<std::setw(7)<<std::setprecision(4)
	       <<fE[i]<<" "<<std::setw(7)<<std::setprecision(4)<<fELow[i]
	       <<" "<<std::setw(7)<<std::setprecision(4)<<fEHigh[i]<<" "
	       <<std::setw(7)<<std::setprecision(4)<<fNumShowers[i]<<" "
	       <<std::setw(7)<<std::setprecision(4)<<fFrac[i]<<std::endl;
      fNum+=(int)fNumShowers[i];
    }
  std::cout<<"Total Number of Showers: "<<fNum<<std::endl;
  return;
}

