
void HillasPlots(int fTelId,char* fFileName)
{
  TFile fHillasFile(fFileName," Input Hillas File");
  std::string fBasicCuts;
  if(fTelId==0)
    {
      fBasicCuts="H.fTelId==0";
    }
  else
    {
      fBasicCuts="H.fTelId==1";
    }

  fBasicCuts=fBasicCuts+" && H.fGoodImage && H.fPixelsInImage>2 && "
    "H.fTriggerCode==1";

  TTree* pfHParTree=NULL;
  pfHParTree=(TTree*)fHillasFile.Get("ParameterisedEvents/ParEventsTree");

  if(pfHParTree==NULL)
    {
      pfHParTree=(TTree*)fHillasFile.Get("ParameterisedEvents/ParEventTree");
      fBasicCuts=fBasicCuts + " && H.fGoodImage && H.fPixelsInImage>2";
    }

  TTree* pfSimTree=NULL;
  pfSimTree=(TTree*)fHillasFile.Get("SimulatedEvents/SimulatedEventsTree");
  TH1F* IntegralRate=NULL;
  //  if(pfSimTree!=NULL)
  // {
  //   IntegralRate = new TH1F("IntegralRate","IntegralRate",
  //				      12000,0,12000.0);
  //   IntegralRate->SetDirectory(0);
  // }

	 

   
  bool fDoC1=false;
  bool fDoC2=false;
  bool fDoC3=false;
  bool fDoC4=false;
  bool fDoC5=true;
  std::string fCuts;
  if(fDoC1)
    {
      TCanvas* fC1 = new TCanvas("fC1","Hillas Params pg1",600,700);
      fC1->Divide(2,3);
      fC1->cd(1);
       
      fCuts=fBasicCuts + "&& H.fWidth>0.0 && H.fWidth<.5" ;
      pfHParTree->Draw("H.fWidth",fCuts.c_str());
      
      
      fC1->cd(2);
      fCuts=fBasicCuts + "&& H.fLength>0.0 && H.fLength<.8";
      pfHParTree->Draw("H.fLength",fCuts.c_str());
      
      fC1->cd(3);
      fCuts=fBasicCuts + "&& H.fMiss>0.0 && H.fMiss<1.75"; 
      pfHParTree->Draw("H.fMiss",fCuts.c_str());
      
      fC1->cd(4);
      fCuts=fBasicCuts + "&& H.fDist>0.0 && H.fDist<1.75"; 
      pfHParTree->Draw("H.fDist",fCuts.c_str());
      
      fC1->cd(5);
      fCuts=fBasicCuts + "&&H.fAlpha>0"; 
      pfHParTree->Draw("H.fAlpha",fCuts.c_str());
      
      fC1->cd(6);
      fCuts=fBasicCuts + "&& H.fSize>0.0 && H.fSize<2000";  
      pfHParTree->Draw("H.fSize",fCuts.c_str());
    }
  if(fDoC2)
    {
      TCanvas* fC2 = new TCanvas("fC2","Hillas Params pg2",600,700);
      fC2->Divide(2,3);
      fC2->cd(1);
      fCuts=fBasicCuts + "&& H.fLengthOverSize>0.0 && H.fLengthOverSize<.0015";
      pfHParTree->Draw("H.fLengthOverSize",fCuts.c_str());
      
      fC2->cd(2);
      fCuts=fBasicCuts + "&& H.fAzWidth>0.0 && H.fAzWidth<.8 ";
      pfHParTree->Draw("H.fAzWidth",fCuts.c_str());
      
      fC2->cd(3);
      fCuts=fBasicCuts + "&& H.fMax1>0.0 && H.fMax1<200";
      pfHParTree->Draw("H.fMax1",fCuts.c_str());
      
      fC2->cd(4);
      fCuts=fBasicCuts + "&& H.fMax2>0.0 && H.fMax2<200";
      pfHParTree->Draw("H.fMax2",fCuts.c_str());
      
      fC2->cd(5);
      fCuts=fBasicCuts +"&& H.fMax3>0.0 && H.fMax3<200"; 
      pfHParTree->Draw("H.fMax3",fCuts.c_str());
      
      fC2->cd(6);
      fCuts=fBasicCuts + "&& H.fFrac1>0 "; 
      pfHParTree->Draw("H.fFrac1",fCuts.c_str());
    }
  
  if(fDoC3)
    {
      TCanvas* fC3 = new TCanvas("fC3","Hillas Params pg3",600,700);
      fC3->Divide(2,3);
      fC3->cd(1);
      fCuts=fBasicCuts + "&& H.fFrac2>0"; 
      pfHParTree->Draw("H.fFrac2",fCuts.c_str());
      
      fC3->cd(2);
      fCuts=fBasicCuts +"&& H.fFrac3>0"; 
      pfHParTree->Draw("H.fFrac3",fCuts.c_str());
      
      
      fC3->cd(3);
      fCuts=fBasicCuts +"&& abs(H.fAsymmetry)<1.0 && H.fAsymmetry!=0.0"; 
      pfHParTree->Draw("H.fAsymmetry",fCuts.c_str());
    }
  
  if(fDoC4)
    {
      
      TCanvas* fC4 = new TCanvas("fC4","Hillas Params pg4",600,700);
      fC4->Divide(2,2);
      fC4->cd(1);
      fCuts=fBasicCuts + "&& H.fLengthOverSize>0.0 && H.fLengthOverSize<.0015";
      pfHParTree->Draw("H.fLengthOverSize",fCuts.c_str());
      
      fC4->cd(2);
      fCuts=fBasicCuts + "&& H.fLength>0.0 && H.fLength<.8";
      pfHParTree->Draw("H.fLength",fCuts.c_str());
      
      fC4->cd(3);
      fCuts=fBasicCuts + "&& H.fSize>0.0 && H.fSize<2000"; 
      pfHParTree->Draw("H.fSize",fCuts.c_str());
      
      fC4->cd(4);
      fCuts=fBasicCuts + "&& H.fMax3>0.0 && H.fMax3<200";
      pfHParTree->Draw("H.fMax3",fCuts.c_str());
    }
  if(fDoC5)
    {
      if(pfSimTree!=NULL)
	{
	  TCanvas* fC5 = new TCanvas("fC5","Hillas Params pg4",600,700);
	  fC5->Divide(2,2);
	  gStyle->SetMarkerStyle(21);
	  // *********************************************************
	  //Trigger rate curve
	  // ********************************************************
	  fCuts="(Sim.fEnergyGeV<750.0)*Sim.fDifferentialRatePerEventHz";
	  
	  
	  fC5->cd(1);
	  pfSimTree->Draw("Sim.fEnergyGeV",fCuts.c_str(),"P");
	  
	  fCuts="(Sim.fEnergyGeV<2000.0)*Sim.fDifferentialRatePerEventHz";
	  fC5->cd(2);
	  pfSimTree->Draw("log(Sim.fEnergyGeV)",fCuts.c_str(),"P");
	  htemp->Fit("gaus");
	  TF1* fGaussFit=htemp->GetFunction("gaus");
	  
	  double fEnergyThreshold=fGaussFit->GetParameter(1);
	  cout<<"Energy Threshold(GeV): "<<exp(fEnergyThreshold)<<endl;;
	  
	  // ********************************************************
	  
	  
	  // ********************************************************
	  //Detection area curve
	  // ********************************************************
	  fC5->cd(3);
	  fCuts="(Sim.fEnergyGeV<10000.0)*Sim.fAomega "; 
	  pfSimTree->Draw("Sim.fEnergyGeV",fCuts.c_str(),"P");
	  // ********************************************************
	  
	  
	  // *********************************************************
	  //Total rate calculation
	  // ********************************************************
	  fCuts="(Sim.fEnergyGeV<12000.0)*Sim.fIntegralRatePerEventHz "; 
	  fC5->cd(4);
	  pfSimTree->Draw("Sim.fEnergyGeV",fCuts.c_str());
	  
	  cout<<"Trigger rate(Hz): "<<htemp->Integral()<<endl;
	  cout<<"Trigger rate(/min): "<<htemp->Integral()*60.0<<endl;
	  // ********************************************************
	}
    }
  return;
}

//Makes 3 Canvas' of Hillas plots
void HillasPlotsT1(char* fFileName)
{
  HillasPlots(0,fFileName);
  return;
}
// *****************************

void HillasPlotsT2(char* fFileName)
{
  HillasPlots(1,fFileName);
  return;
}
// *******************************

void HillasPlots(char* fFileName)
{
  HillasPlots(0,fFileName);
}
