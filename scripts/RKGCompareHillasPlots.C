//Makes 3 Canvas' of Hillas plots
void ScaleAndPlot(TH1F* pfDataHist, TH1F* pfSimHist,bool fNormalizePeak);
void RKGCompareAreaHillasPlots(char* fDataName, char* fSimName)
{
  bool fNormalizePeak=false;
  RKGCompareHillasPlots(fDataName,fSimName,fNormalizePeak);
}
// ****************************************************************

void RKGComparePeakHillasPlots(char* fDataName, char* fSimName)
{
  bool fNormalizePeak=true;
  RKGCompareHillasPlots(fDataName,fSimName,fNormalizePeak);
}
// ****************************************************************


void RKGCompareHillasPlots(char* fDataName, char* fSimName, 
			                            bool fNormalizePeak=false)
{
 //Open data file and get Hillas parameter tree and set up basic cuts
  TFile fDataFile(fDataName," Input Data File");
  std::string fBasicCuts=
      "H.fTelId==0 && H.fGoodImage && H.fPixelsInImage>2 &&H.fTriggerCode==1";
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
  //Create TTree chain for gernot sims.
  TChain* pfHParChain = new TChain("ParameterisedEvents/ParEventsTree");
  
  for(int i=0;i<64;i++)
    {
      char p[80];
      if(i<10)
	{
	  sprintf(p,"/project/veritas/sembrosk/veritas/gernot/protons/60000%i",i);
	}
      else
	{
	  sprintf(p,"/project/veritas/sembrosk/veritas/gernot/protons/6000%i",i);
	}
      std::string fileName(p);
      fileName=fileName+".root";
  
      std::cout<<"fileName: "<<fileName<<std::endl;
      pfHParChain->Add(fileName.c_str());
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
//gernot sims
  TH1F *pfGerLOS = new TH1F("pfGerLOS","Length Over Size",100,0,0.0016);
  TH1F *psGerL = new TH1F("pfGerL","Length",100,0,1.0);
  TH1F *pfGerS = new TH1F("pfGerS","Size",100,0,2000);
  TH1F *pfGerM3 = new TH1F("pfGerM3","Max 3",100,0,200);
//For now, skipping a bunch of plots and only doing page 4

  fC4->cd(1);
  fCuts=fBasicCuts + "&& H.fLengthOverSize>0.0 && H.fLengthOverSize<.0015";
  //project into histograms, get scale factor data/sim,scale sim hist and plot both  
  pfHParTree->Draw("H.fLengthOverSize >> pfDataLOS",fCuts.c_str());
  pfSimParTree->Draw("H.fLengthOverSize >> pfSimLOS",fCuts.c_str());
  pfHParChain->Draw("H.fLengthOverSize >> pfGerLOS",fCuts.c_str());
  ScaleAndPlot(pfDataLOS,pfSimLOS,pfGerLOS,fNormalizePeak);

  fC4->cd(2);
  fCuts=fBasicCuts;
  pfHParTree->Draw("H.fLength >> pfDataL",fCuts.c_str());
  pfSimParTree->Draw("H.fLength >> pfSimL",fCuts.c_str());
  pfHParChain->Draw("H.fLength >> pfGerL",fCuts.c_str());
  ScaleAndPlot(pfDataL,pfSimL,pfGerL,fNormalizePeak);

  fC4->cd(3);
  fCuts=fBasicCuts; 
  pfHParTree->Draw("H.fSize >> pfDataS",fCuts.c_str());
  pfSimParTree->Draw("H.fSize >> pfSimS",fCuts.c_str());
  pfHParChain->Draw("H.fSize >> pfGerS",fCuts.c_str());
  ScaleAndPlot(pfDataS,pfSimS,pfGerS,fNormalizePeak);

  fC4->cd(4);
  fCuts=fBasicCuts;
  pfHParTree->Draw("H.fMax3 >> pfDataM3",fCuts.c_str());
  pfSimParTree->Draw("H.fMax3 >> pfSimM3",fCuts.c_str());
  pfHParChain->Draw("H.fMax3 >> pfGerM3",fCuts.c_str());
  ScaleAndPlot(pfDataM3,pfSimM3,pfGerM3,fNormalizePeak);    
  return;
}
void ScaleAndPlot(TH1F* pfDataHist, TH1F* pfSimHist, TH1F* pfGerHist, 
		  bool fNormalizePeak)
{
 double fSumData, fSumSim, fScale;
 double fDataMax, fSimMax;

 //Normalize KASCADe sims (assumed to be  smaller)
 if(!fNormalizePeak)
   {
     // scale the simulation hist area to the data hist area
     fSumData = pfDataHist->Integral();
     fSumSim = pfSimHist->Integral();
     fScale = fSumData/fSumSim;
   }
 else
   {
     //Nomalize to peaks.
     fDataMax=pfDataHist->GetMaximum();
     fSimMax=pfSimHist->GetMaximum();
     fScale = fDataMax/fSimMax;
   }
pfSimHist->Scale(fScale);

//Normalize Gernot sims (assumed to be smaller)
 if(!fNormalizePeak)
   {
     // scale the simulation hist area to the data hist area
      fSumGer = pfGerHist->Integral();
     fScale = fSumData/fSumGer;
   }
 else
   {
     //Nomalize to peaks.
     fGerMax=pfGerHist->GetMaximum();
     fScale = fDataMax/fGerMax;
   }
 pfGerHist->Scale(fScale);





//Find which one has maximun value and draw that one first
 pfDataHist->SetLineColor(kBlack);
 pfSimHist->SetLineColor(kRed);
 pfGerHist->SetLineColor(kBlue);
 fDataMax=pfDataHist->GetMaximum();
 fSimMax=pfSimHist->GetMaximum();
cd TH1F* pfTallHist=pfDataHist;
 TH1F* pfShortHist=pfSimHist;
 if(fSimMax>fDataMax)
   {
     pfTallHist=pfSimHist;
     pfShortHist=pfDataHist;
   }

 pfGerHist->Draw();
 pfTallHist->Draw("same");
 pfShortHist->Draw("same");


 pfSimHist->SetDirectory(0);
 pfDataHist->SetDirectory(0);
 pfGerHist->SetDirectory(0);
 return;
}
 
