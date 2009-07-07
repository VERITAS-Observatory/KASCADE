//Makes 3 Canvas' of Hillas plots
void ScaleAndPlot(TH1F* pfDataHist, TH1F* pfSimHist,bool fNormalizePeak);
void CompareAreaHillasPlots(char* fDataName, char* fSimName)
{
  bool fNormalizePeak=false;
  CompareHillasPlots(fDataName,fSimName,fNormalizePeak);
}
// ****************************************************************

void ComparePeakHillasPlots(char* fDataName, char* fSimName)
{
  bool fNormalizePeak=true;
  int fTelId=0;
  CompareHillasPlots(fDataName,fSimName,fNormalizePeak,fTelId);
}
// ****************************************************************


void CompareHillasPlots(char* fDataName, char* fSimName, 
			bool fNormalizePeak=false, int fTelId=0)
{
  std::string fBasicCuts;
  if(fTelId==0)
    {
      fBasicCuts="H.fTelId==0";
    }
  else
    {
      fBasicCuts="H.fTelId==1";
    }

 //Open data file and get Hillas parameter tree and set up basic cuts
  TFile fDataFile(fDataName," Input Data File");
  TTree* pfHParTree=NULL;
  pfHParTree=(TTree*)fDataFile.Get("ParameterisedEvents/ParEventsTree");

  if(pfHParTree==NULL)
    {
      pfHParTree=(TTree*)fHillasFile.Get("ParameterisedEvents/ParEventTree");
      fBasicCuts=fBasicCuts+ " && H.fGoodImage && H.fPixelsInImage>2";
    }
  else
    {
      fBasicCuts=fBasicCuts+ " && H.fGoodImage && H.fPixelsInImage>2 && "
	"H.fTriggerCode==1";
    }
//Open simulation file and get Hillas parameter tree
  TFile fSimFile(fSimName," Input Simulation File");
 
  TTree* pfSimParTree=NULL;
  pfSimParTree=(TTree*)fSimFile.Get("ParameterisedEvents/ParEventsTree");

  if(pfSimParTree==NULL)
    {
      pfSimParTree=(TTree*)fSimFile.Get("ParameterisedEvents/ParEventTree");
    }

	 
//set up canvases and their hists  
  // Set up histograms
  // data
  TCanvas* fC1 = new TCanvas("fC1","Hillas Params pg1",600,700);
  fC1->Divide(2,3);
  TH1F *pfDataW = new TH1F("pfDataW","Width",100,0,0.5);
  TH1F *pfDataL = new TH1F("pfDataL","Length",100,0,.8);
  TH1F *pfDataM = new TH1F("pfDataM","Miss",100,0,1.75);
  TH1F *pfDataD = new TH1F("pfDataD","Dist",100,0,1.75);
  TH1F *pfDataA = new TH1F("pfDataA","Alpha",100,0,1.75);
  TH1F *pfDataS = new TH1F("pfDataS","Size",100,0,2000);

  TCanvas* fC2 = new TCanvas("fC2","Hillas Params pg2",600,700);
  fC2->Divide(2,3);
  TH1F *pfDataLOS = new TH1F("pfDataLOS","LengthOverSize",100,0.0,0.0015);
  TH1F *pfDataAzW = new TH1F("pfDataAzW","AzWidth",100,0,0.8);
  TH1F *pfDataM1  = new TH1F("pfDataM1","Max1",100,0,200);
  TH1F *pfDataM2  = new TH1F("pfDataM2","Max2",100,0,200);
  TH1F *pfDataM3  = new TH1F("pfDataM3","Max3",100,0,200);
  TH1F *pfDataF1  = new TH1F("pfDataF1","Frac1",100,0,1.1);


  TCanvas* fC3 = new TCanvas("fC3","Hillas Params pg3",600,700);
  fC3->Divide(2,3);
  TH1F *pfDataF2  = new TH1F("pfDataF2","Frac2",100,0,1.1);
  TH1F *pfDataF3  = new TH1F("pfDataF3","Frac3",100,0,1.1);
  TH1F *pfDataAs  = new TH1F("pfDataAs","Asymmetry",100,-1.0,1.0);
  TH1F *pfDataP   = new TH1F("pfDataP","NumPixelsInImage",100,0,20);

  TCanvas* fC4 = new TCanvas("fC4","Hillas Params pg4",600,700);
  fC4->Divide(2,2);
  TH1F *pfData4LOS = new TH1F("pfData4LOS","LengthOverSize",100,0.0,0.0015);
  TH1F *pfData4L   = new TH1F("pfData4L","Length",100,0,.8);
  TH1F *pfData4S   = new TH1F("pfData4S","Size",100,0,2000);
  TH1F *pfData4P   = new TH1F("pfData4P","NumPixelsInImage",100,0,20);

  // Set up histograms
  //simulations
  TH1F *pfSimW = new TH1F("pfSimW","Width",100,0,0.5);
  TH1F *pfSimL = new TH1F("pfSimL","Length",100,0,.8);
  TH1F *pfSimM = new TH1F("pfSimM","Miss",100,0,1.75);
  TH1F *pfSimD = new TH1F("pfSimD","Dist",100,0,1.75);
  TH1F *pfSimA = new TH1F("pfSimA","Alpha",100,0,1.75);
  TH1F *pfSimS = new TH1F("pfSimS","Size",100,0,2000);

  TH1F *pfSimLOS = new TH1F("pfSimLOS","LengthOverSize",100,0.00001,0.0015);
  TH1F *pfSimAzW = new TH1F("pfSimAzW","AzWidth",100,0,0.8);
  TH1F *pfSimM1  = new TH1F("pfSimM1","Max1",100,0,200);
  TH1F *pfSimM2  = new TH1F("pfSimM2","Max2",100,0,200);
  TH1F *pfSimM3  = new TH1F("pfSimM3","Max3",100,0,200);
  TH1F *pfSimF1  = new TH1F("pfSimF1","Frac1",100,0,1.1);

  TH1F *pfSimF2  = new TH1F("pfSimF2","Frac2",100,0,1.1);
  TH1F *pfSimF3  = new TH1F("pfSimF3","Frac3",100,0,1.1);
  TH1F *pfSimAs  = new TH1F("pfSimAs","Asymmetry",100,-1.0,1.0);
  TH1F *pfSimP   = new TH1F("pfSimP","NumPixelsInImage",100,0,20);

  TH1F *pfSim4LOS = new TH1F("pfSim4LOS","LengthOverSize",100,0.0,0.0015);
  TH1F *pfSim4L   = new TH1F("pfSim4L","Length",100,0,.8);
  TH1F *pfSim4S   = new TH1F("pfSim4S","Size",100,0,2000);
  TH1F *pfSim4P   = new TH1F("pfSim4P","NumPixelsInImage",100,0,20);


  bool fDoC1=true;
  bool fDoC2=true;
  bool fDoC3=true;
  bool fDoC4=true;
 
  bool goodPlot=true;

//For now, skipping a bunch of plots and only doing page 4
  if(fDoC1)
    {
      fC1->cd(1);
      pfHParTree->Draw("H.fWidth >> pfDataW",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fWidth >> pfSimW",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataW,pfSimW,fNormalizePeak);

      fC1->cd(2);
      pfHParTree->Draw("H.fLength >> pfDataL",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fLength >> pfSimL",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataL,pfSimL,fNormalizePeak);

      fC1->cd(3);
      pfHParTree->Draw("H.fMiss >> pfDataM",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fMiss >> pfSimM",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataM,pfSimM,fNormalizePeak);

      fC1->cd(4);
      pfHParTree->Draw("H.fDist >> pfDataD",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fDist >> pfSimD",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataD,pfSimD,fNormalizePeak);

      fC1->cd(5);
      pfHParTree->Draw("H.fAlpha >> pfDataA",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fAlpha >> pfSimA",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataA,pfSimA,fNormalizePeak);

      fC1->cd(6);
      pfHParTree->Draw("H.fSize >> pfDataS",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fSize >> pfSimS",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataS,pfSimS,fNormalizePeak);
      if(!goodPlot)
	{
	  std::cout<,"Empty plot on page 1"<<std::endl;
	}
    }     

  if(fDoC2)
    {
      fC2->cd(1);
      pfHParTree->Draw("H.fLengthOverSize >> pfDataLOS",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fLengthOverSize >> pfSimLOS",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataLOS,pfSimLOS,fNormalizePeak);
      

      fC2->cd(2);
      pfHParTree->Draw("H.fAzWidth >> pfDataAzW",fBasicCuts.c_str());

      pfSimParTree->Draw("H.fAzWidth >> pfSimAzW",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataAzW,pfSimAzW,fNormalizePeak);

      fC2->cd(3);
      pfHParTree->Draw("H.fMax1 >> pfDataM1",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fMax1 >> pfSimM1",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataM1,pfSimM1,fNormalizePeak);

      fC2->cd(4);
      pfHParTree->Draw("H.fMax2 >> pfDataM2",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fMax2 >> pfSimM2",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataM2,pfSimM2,fNormalizePeak);

      fC2->cd(5);
      pfHParTree->Draw("H.fMax3 >> pfDataM3",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fMax3 >> pfSimM3",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataM3,pfSimM3,fNormalizePeak);

      fC2->cd(6);
      pfHParTree->Draw("H.fFrac1 >> pfDataF1",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fFrac1 >> pfSimF1",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataF1,pfSimF1,fNormalizePeak);
       if(!goodPlot)
	{
	  std::cout<,"Empty plot on page 2"<<std::endl;
	}
   }     

  if(fDoC3)
    {
      fC3->cd(1);
      pfHParTree->Draw("H.fFrac2 >> pfDataF2",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fFrac2 >> pfSimF2",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataF2,pfSimF2,fNormalizePeak);

      fC3->cd(2);
      pfHParTree->Draw("H.fFrac3 >> pfDataF3",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fFrac3 >> pfSimF3",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataF3,pfSimF3,fNormalizePeak);

      fC3->cd(3);
      pfHParTree->Draw("H.fAsymmetry >> pfDataAs",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fAsymmetry >> pfSimAs",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataAs,pfSimAs,fNormalizePeak);

      fC3->cd(4);
      pfHParTree->Draw("H.fPixelsInImage >> pfDataP",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fPixelsInImage >> pfSimP",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfDataP,pfSimP,fNormalizePeak);
      if(!goodPlot)
	{
	  std::cout<,"Empty plot on page 3"<<std::endl;
	}

    }
 
  if(fDoC4)
    {
      fC4->cd(1);
      pfHParTree->Draw("H.fLengthOverSize >> pfData4LOS",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fLengthOverSize >> pfSim4LOS",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfData4LOS,pfSim4LOS,fNormalizePeak);

      
      fC4->cd(2);
      pfHParTree->Draw("H.fLength >> pfData4L",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fLength >> pfSim4L",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfData4L,pfSim4L,fNormalizePeak);
      
      fC4->cd(3);
      pfHParTree->Draw("H.fSize >> pfData4S",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fSize >> pfSim4S",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfData4S,pfSim4S,fNormalizePeak);
      
      fC4->cd(4);
      pfHParTree->Draw("H.fPixelsInImage >> pfData4P",fBasicCuts.c_str());
      pfSimParTree->Draw("H.fPixelsInImage >> pfSim4P",fBasicCuts.c_str());
      goodPlot=ScaleAndPlot(pfData4P,pfSim4P,fNormalizePeak);
      if(!goodPlot)
	{
	  std::cout<,"Empty plot on page 4"<<std::endl;
	}
    }
  return;
}
// *************************************************************************


bool ScaleAndPlot(TH1F* pfDataHist, TH1F* pfSimHist, bool fNormalizePeak)
{
 double fSumData, fSumSim, fScale;
 double fDataMax, fSimMax;

 if(!fNormalizePeak)
   {
     // scale the simulation hist area to the data hist area
     fSumData = pfDataHist->Integral();
     fSumSim = pfSimHist->Integral();
     if(fSumSim<=0)
       {
	 std::cout<<"Found histo with 0 area"<<std::endl;
	 return false;
       }
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

 //Find which one has maximun value and draw that one first
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
 return true;
}
 
