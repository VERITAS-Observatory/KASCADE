//Makes 3 Canvas' of Hillas plots
void HillasPlots(char* fFileName)
{
  TFile fHillasFile(fFileName," Input Hillas File");
     TTree* fHParTree=
       (TTree*)fHillasFile.Get("ParameterisedEvents/ParEventsTree");
  TCanvas* fC1 = new TCanvas("fC1","Hillas Params pg1",600,700);
  TCanvas* fC2 = new TCanvas("fC2","Hillas Params pg2",600,700);
  TCanvas* fC3 = new TCanvas("fC3","Hillas Params pg3",600,700);
  TCanvas* fC4 = new TCanvas("fC4","Hillas Params pg4",600,700);
  fC1->Divide(2,3);
  fC2->Divide(2,3);
  fC3->Divide(2,3);
  fC4->Divide(2,2);
  
  fC1->cd(1);
  fHParTree->Draw("H.fWidth",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fWidth>0.0 "
		  "&& H.fWidth<.5 && H.fPixelsInImage>2");
  fC1->cd(2);
  fHParTree->Draw("H.fLength",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fLength>0.0 " 
		  "&& H.fLength<.8 && H.fPixelsInImage>2");
  fC1->cd(3);
  fHParTree->Draw("H.fMiss",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fMiss>0.0 "
                  "&& H.fMiss<1.2 && H.fPixelsInImage>2");
  fC1->cd(4);
  fHParTree->Draw("H.fDist",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fDist>0.0 " 
                  " && H.fDist<1.2 && H.fPixelsInImage>2");
  fC1->cd(5);
  fHParTree->Draw("H.fAlpha"," H.fGoodImage && H.fTriggerCode==1 " 
                  "&& H.fAlpha>0 && H.fPixelsInImage>2");
  fC1->cd(6);
  fHParTree->Draw("H.fSize",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fSize>0.0 " 
                  "&& H.fSize<2000 && H.fPixelsInImage>2");

  fC2->cd(1);
  fHParTree->Draw("H.fLengthOverSize",
		  "H.fGoodImage && H.fTriggerCode==1 "
		  "&& H.fLengthOverSize>0.0 && H.fLengthOverSize<.0015 && "
		  "H.fPixelsInImage>2");
  fC2->cd(2);
  fHParTree->Draw("H.fAzWidth",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fAzWidth>0.0 " 
                  " && H.fAzWidth<.8 && H.fPixelsInImage>2");
  fC2->cd(3);
  fHParTree->Draw("H.fMax1","H.fGoodImage && H.fTriggerCode==1 "
		  "&& H.fMax1>0.0 && H.fMax1<200 && H.fPixelsInImage>2");
  fC2->cd(4);
  fHParTree->Draw("H.fMax2","H.fGoodImage && H.fTriggerCode==1 "
		  "&& H.fMax2>0.0 && H.fMax2<200 && H.fPixelsInImage>2");
  fC2->cd(5);
  fHParTree->Draw("H.fMax3","H.fGoodImage && H.fTriggerCode==1 "
		  "&& H.fMax3>0.0 && H.fMax3<200 && H.fPixelsInImage>2");
  fC2->cd(6);
  fHParTree->Draw("H.fFrac1",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fFrac1>0 && "
		  "H.fPixelsInImage>2");
  
  fC3->cd(1);
  fHParTree->Draw("H.fFrac2",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fFrac2>0 && "
		  "H.fPixelsInImage>2");
  fC3->cd(2);
  fHParTree->Draw("H.fFrac3",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fFrac3>0 && "
		  "H.fPixelsInImage>2");
  fC3->cd(3);
  fHParTree->Draw("H.fAsymmetry",
		  "H.fGoodImage && H.fTriggerCode==1 "
		  "&& abs(H.fAsymmetry)<1.0 && H.fAsymmetry!=0.0 && "
		  "H.fPixelsInImage>2");
  fC4->cd(1);
  fHParTree->Draw("H.fLengthOverSize",
		  "H.fGoodImage && H.fTriggerCode==1 "
		  "&& H.fLengthOverSize>0.0 && H.fLengthOverSize<.0015 && "
		  "H.fPixelsInImage>2");
  fC4->cd(2);
  fHParTree->Draw("H.fLength",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fLength>0.0 " 
		  "&& H.fLength<.8 && H.fPixelsInImage>2");
  fC4->cd(3);
  fHParTree->Draw("H.fSize",
		  "H.fGoodImage && H.fTriggerCode==1 && H.fSize>0.0 " 
                  "&& H.fSize<2000 && H.fPixelsInImage>2");

  fC4->cd(4);
  fHParTree->Draw("H.fMax3","H.fGoodImage && H.fTriggerCode==1 "
		  "&& H.fMax3>0.0 && H.fMax3<200 && H.fPixelsInImage>2");
    
  return;
}
