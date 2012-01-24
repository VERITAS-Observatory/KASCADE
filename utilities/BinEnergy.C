//Quick test of where bin centers should be
//Copy some code and tricks from Brians code in VASpectrumAnl.cpp


void getCorrectEnergy(double lowE, double hiE)
{
  cout<<"lowE,hiE,log10(loE),log10(hiE): "<<lowE<<" "<<hiE<<" "<<log10(lowE)
      <<" "<<log10(hiE)<<endl;


  TF1* integralFit = new TF1("fa1","pow(x,-2.45)",10,40000);
  
  double deltaE = hiE - lowE;
  double eMeanY = integralFit->Integral( lowE, hiE ) / deltaE;
  double eMean = integralFit->GetX( eMeanY, lowE, hiE );
  double eAve =pow(10,((log10(hiE)+log10(lowE))/2.));
  cout<<"eCorrect,eAve: "<<eMean<<" "<<eAve<<endl;
  return;
}


