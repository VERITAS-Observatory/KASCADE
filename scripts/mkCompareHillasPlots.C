//Makes 3 Canvas' of Hillas plots
void ScaleAndPlot(TH1F* pfDataHist, TH1F* pfSimHist);
void CompareHillasPlots(char* fDataName, char* fSimName)
{
 //Open data file and get Hillas parameter tree and set up basic cuts
  TFile fDataFile(fDataName," Input Data File");
  std::string fBasicCuts=
                     "H.fGoodImage && H.fPixelsInImage>2 &&H.fTriggerCode==1";
  TTree* pfHParTree=NULL;
  pfHParTree=(TTree*)fDataFile.Get("ParameterisedEvents/ParEventsTree");

  if(pfHParTree==NULL)
    {
      pfHParTree=(TTree*)fHillasFile.Get("ParameterisedEvents/ParEventTree");
      fBasicCuts="H.fGoodImage && H.fPixelsInImage>2";
    }
	 
//Open simulation file and get Hillas parameter tree
  TFile fSimFile(fSimName," Input Simulation File");
 
  TTree* pfSimParTree=NULL;
  pfSimParTree=(TTree*)fSimFile.Get("ParameterisedEvents/ParEventsTree");

  if(pfSimParTree==NULL)
    {
      pfSimParTree=(TTree*)fSimFile.Get("ParameterisedEvents/ParEventTree");
    }
	 
//set up canvases  
//  TCanvas* fC1 = new TCanvas("fC1","Hillas Params pg1",600,700);
//  TCanvas* fC2 = new TCanvas("fC2","Hillas Params pg2",600,700);
//  TCanvas* fC3 = new TCanvas("fC3","Hillas Params pg3",600,700);
   TCanvas* fC4 = new TCanvas("fC4","Hillas Params pg4",600,700);
   //  fC1->Divide(2,3);
   // fC2->Divide(2,3);
   //fC3->Divide(2,3);
  fC4->Divide(2,2);

  // Set up histograms
  // data
  TH1F *pfDataLOS = new TH1F("pfDataLOS","Length Over Size",100,0,0.0016);
  TH1F *pfDataL = new TH1F("pfDataL","Length",100,0,1.0);
  TH1F *pfDataS = new TH1F("pfDataS","Size",100,0,2000);
  TH1F *pfDataM3 = new TH1F("pfDataM3","Max 3",100,0,200);
//simulations
  TH1F *pfSimLOS = new TH1F("pfSimLOS","Length Over Size",100,0,0.0016);
  TH1F *psSimL = new TH1F("pfSimL","Length",100,0,1.0);
  TH1F *pfSimS = new TH1F("pfSimS","Size",100,0,2000);
  TH1F *pfSimM3 = new TH1F("pfSimM3","Max 3",100,0,200);
//For now, skipping a bunch of plots and only doing page 4

  fC4->cd(1);
  fCuts=fBasicCuts + "&& H.fLengthOverSize>0.0 && H.fLengthOverSize<.0015";
  //project into histograms, get scale factor data/sim,scale sim hist and plot both  
pfHParTree->Draw("H.fLengthOverSize >> pfDataLOS",fCuts.c_str());
 pfSimParTree->Draw("H.fLengthOverSize >> pfSimLOS",fCuts.c_str());
 ScaleAndPlot(pfDataLOS,pfSimLOS);

  fC4->cd(2);
  fCuts=fBasicCuts;
  pfHParTree->Draw("H.fLength >> pfDataL",fCuts.c_str());
  pfSimParTree->Draw("H.fLength >> pfSimL",fCuts.c_str());
 ScaleAndPlot(pfDataL,pfSimL);

  fC4->cd(3);
  fCuts=fBasicCuts; 
  pfHParTree->Draw("H.fSize >> pfDataS",fCuts.c_str());
  pfSimParTree->Draw("H.fSize >> pfSimS",fCuts.c_str());
 ScaleAndPlot(pfDataS,pfSimS);

  fC4->cd(4);
  fCuts=fBasicCuts;
  pfHParTree->Draw("H.fMax3 >> pfDataM3",fCuts.c_str());
  pfSimParTree->Draw("H.fMax3 >> pfSimM3",fCuts.c_str());
 ScaleAndPlot(pfDataM3,pfSimM3);    
  return;
}
void ScaleAndPlot(TH1F* pfDataHist, TH1F* pfSimHist)
{
 double fSumData, fSumSim, fScale;
 // scale the simulation hist to the data hist
fSumData = pfDataHist->Integral();
fSumSim = pfSimHist->Integral();
fScale = fSumData/fSumSim;
pfSimHist->Scale(fScale);

//Find which one has maximun value and draw that one first
 double fDataMax, fSimMax;
pfDataHist->SetLineColor(kBlack);
pfSimHist->SetLineColor(kRed);
 fDataMax=pfDataHist->GetMaximum();
 fSimMax=pfSimHist->GetMaximum();
 TH1F* pfTallHist=pfDataHist;
TH1F* pfShortHist=pfSimHist;
 if(fSimMax>fDataMax) 
   {
     pfTallHist=pfSimHist;
     pfShortHist=pfDataHist;
   }
pfTallHist->Draw();
pfShortHist->Draw("same");
pfSimHist->SetDirectory(0);
pfDataHist->SetDirectory(0);
 return;
}
 
