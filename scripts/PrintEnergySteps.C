//Short root macro to print out energies for ewual spacing in log(E)

PrintEnergySteps()
{
  //double fStepLogE=.18;
  //double fLogEBase=log(.100)-13*fStepLogE;    //Start at .1 TEV and backup 
  //int fNumSteps=9;
  int fNumSteps=18;
  
  double fStepLogE=(log(.100)-log(.02))/fNumSteps;
  double fLogEBase= log(.02);    
  int fNumE=75;
  for(int i=0;i<fNumE;i++)
    {
      double fLogE=fLogEBase+i*fStepLogE;
      double fE=exp(fLogE);
      std::cout<<i<<": "<<fE<<std::endl;
    }
  return;
}

