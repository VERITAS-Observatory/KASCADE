//Makes 3 Canvas' of Hillas plots
void HillasPlots(char* fFileName)
{
  TFile fHillasFile(fFileName," Input Hillas File");
  std::string fBasicCuts=
                     "H.fGoodImage && H.fPixelsInImage>2 &&H.fTriggerCode==1";
  TTree* pfHParTree=NULL;
  pfHParTree=(TTree*)fHillasFile.Get("ParameterisedEvents/ParEventsTree");

  if(pfHParTree==NULL)
    {
      pfHParTree=(TTree*)fHillasFile.Get("ParameterisedEvents/ParEventTree");
      fBasicCuts="H.fGoodImage && H.fPixelsInImage>2";
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

	 

  TCanvas* fC1 = new TCanvas("fC1","Hillas Params pg1",600,700);
  TCanvas* fC2 = new TCanvas("fC2","Hillas Params pg2",600,700);
  TCanvas* fC3 = new TCanvas("fC3","Hillas Params pg3",600,700);
  TCanvas* fC4 = new TCanvas("fC4","Hillas Params pg4",600,700);
  fC1->Divide(2,3);
  fC2->Divide(2,3);
  fC3->Divide(2,3);
  fC4->Divide(2,2);
  
  fC1->cd(1);
  std::string fCuts=fBasicCuts + "&& H.fWidth>0.0 && H.fWidth<.5" ;

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

  
  fC3->cd(1);
  fCuts=fBasicCuts + "&& H.fFrac2>0"; 
  pfHParTree->Draw("H.fFrac2",fCuts.c_str());

  fC3->cd(2);
  fCuts=fBasicCuts +"&& H.fFrac3>0"; 
  pfHParTree->Draw("H.fFrac3",fCuts.c_str());

  fC3->cd(3);
  fCuts=fBasicCuts +"&& abs(H.fAsymmetry)<1.0 && H.fAsymmetry!=0.0"; 
  pfHParTree->Draw("H.fAsymmetry",fCuts.c_str());

  if(pfSimTree!=NULL)
    {
      fC3->cd(4);
      pfSimTree->Draw("Sim.fEnergyGeV",
		   "Sim.fDifferentialRatePerEventHz*(Sim.fEnergyGeV<500.0)");
      fC3->cd(5);
      pfSimTree->Draw("Sim.fEnergyGeV",
		   "Sim.fIntegralRatePerEventHz*(Sim.fEnergyGeV<12000.0)");
 
      cout<<"Trigger rate(Hz): "<<htemp->Integral()<<endl;
    }

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
    
  return;
}
