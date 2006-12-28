//Makes 1 Canvas of Hillas plots
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
  //bool fUseGernot=false;
  bool fUseGernot=true;
  // ***********************************************************************
  //Open data file and get Hillas parameter tree and set up basic cuts
  // ***********************************************************************
  TFile fDataFile(fDataName," Input Data File");

  // ********************************
  // Note single telescope requirement.
  // ********************************
  std::string fBasicCuts=
    "H.fTelId==0 && H.fGoodImage && H.fPixelsInImage>2 &&H.fTriggerCode==1";
  TTree* pfHParTree=NULL;
  pfHParTree=(TTree*)fDataFile.Get("ParameterisedEvents/ParEventsTree");
  
  if(pfHParTree==NULL)
    {
      pfHParTree=(TTree*)fHillasFile.Get("ParameterisedEvents/ParEventTree");
      fBasicCuts="H.fGoodImage && H.fPixelsInImage>2";
    }
  // ***********************************************************************


	 
  // ***********************************************************************
  //Open simulation file and get Hillas parameter tree
  // ***********************************************************************
  TFile fSimFile(fSimName," Input Simulation File");
 
  TTree* pfSimParTree=NULL;
  pfSimParTree=(TTree*)fSimFile.Get("ParameterisedEvents/ParEventsTree");

  if(pfSimParTree==NULL)
    {
      pfSimParTree=(TTree*)fSimFile.Get("ParameterisedEvents/ParEventTree");
    }
  // ***********************************************************************


  // ***********************************************************************
  // If enable make a chain out pf the Gernot simulations files.
  // ***********************************************************************
  if(fUseGernot)
    {
      //Create TTree chain for gernot sims.
      TChain* pfHParChain = new TChain("ParameterisedEvents/ParEventsTree");
      
      for(int i=0;i<64;i++)
	{
	  char p[80];
	  if(i<10)
	    {
	      sprintf(p,
		 "/simulations/veritas/gernot/protons/60000%i",i);
	    }
	  else
	    {
	      sprintf(p,
		 "/simulations/veritas/gernot/protons/6000%i",i);
	    }
	  std::string fileName(p);
	  fileName=fileName+".root";
	  
	  std::cout<<"fileName: "<<fileName<<std::endl;
	  pfHParChain->Add(fileName.c_str());
	}	
    }
  // ***********************************************************************

  // ***********************************************************************
  //set up canvas  
  // ***********************************************************************
  TCanvas* fC4 = new TCanvas("fC4","Hillas Params Comparison",600,700);
  fC4->Divide(2,2);

  // *****************************************************************
  // Set up histograms
  // *****************************************************************
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

  //Gernot
  TH1F *pfGerLOS=NULL;
  TH1F *pfGerL=NULL;
  TH1F *pfGerS=NULL;
  TH1F *pfGerM3=NULL;
  if(fUseGernot)
    {
      //gernot sims
      pfGerLOS = new TH1F("pfGerLOS","Length Over Size",100,0,0.0016);
      pfGerL = new TH1F("pfGerL","Length",100,0,1.0);
      pfGerS = new TH1F("pfGerS","Size",100,0,2000);
      pfGerM3 = new TH1F("pfGerM3","Max 3",100,0,200);
    }
  // *****************************************************************

  // *****************************************************************
  // Project into histograms, get scale factor data/sim,scale sim hist and 
  // plot both  
  // *****************************************************************


  // *************************************************************
  // Length Over Size comparison.
  // *************************************************************
  fC4->cd(1);
  fCuts=fBasicCuts + "&& H.fLengthOverSize>0.0 && H.fLengthOverSize<.0015";

  pfHParTree->Draw("H.fLengthOverSize >> pfDataLOS",fCuts.c_str());
  pfSimParTree->Draw("H.fLengthOverSize >> pfSimLOS",fCuts.c_str());
  
  if(fUseGernot)
    {
      pfHParChain->Draw("H.fLengthOverSize >> pfGerLOS",fCuts.c_str());
    }

  ScaleAndPlot(pfDataLOS,pfSimLOS,pfGerLOS,fNormalizePeak);
  // *************************************************************

  // *************************************************************
  // Length comparison
  // *************************************************************
  fC4->cd(2);
  fCuts=fBasicCuts;
  pfHParTree->Draw("H.fLength >> pfDataL",fCuts.c_str());
  pfSimParTree->Draw("H.fLength >> pfSimL",fCuts.c_str());

  if(fUseGernot)
    {
      pfHParChain->Draw("H.fLength >> pfGerL",fCuts.c_str());
    }
  ScaleAndPlot(pfDataL,pfSimL,pfGerL,fNormalizePeak);
  // *************************************************************



  // *************************************************************
  // Size comparison
  // *************************************************************
  fC4->cd(3);

  fCuts=fBasicCuts; 

  pfHParTree->Draw("H.fSize >> pfDataS",fCuts.c_str());
  pfSimParTree->Draw("H.fSize >> pfSimS",fCuts.c_str());

  if(fUseGernot)
    {
      pfHParChain->Draw("H.fSize >> pfGerS",fCuts.c_str());
    }
  ScaleAndPlot(pfDataS,pfSimS,pfGerS,fNormalizePeak);
  // *************************************************************

  // *************************************************************
  // Max3 comparison
  // *************************************************************
  fC4->cd(4);

  fCuts=fBasicCuts;

  pfHParTree->Draw("H.fMax3 >> pfDataM3",fCuts.c_str());
  pfSimParTree->Draw("H.fMax3 >> pfSimM3",fCuts.c_str());

  if(fUseGernot)
    {
      pfHParChain->Draw("H.fMax3 >> pfGerM3",fCuts.c_str());
    }
  ScaleAndPlot(pfDataM3,pfSimM3,pfGerM3,fNormalizePeak);    
  // *************************************************************

  return;
}
void ScaleAndPlot(TH1F* pfDataHist, TH1F* pfSimHist, TH1F* pfGerHist, 
		  bool fNormalizePeak)
{
  double fSumData, fSumSim, fScale;
  double fDataMax, fSimMax;
  
  // **********************************************************************
  //Normalize KASCADe sims (assumed to be  smaller)
  // **********************************************************************
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
  
  // **********************************************************************
  //Normalize Gernot sims (assumed to be smaller)
  // **********************************************************************
  if(pfGerHist!=NULL)
    {
      if(!fNormalizePeak)
	{
	  // scale the simulation hist area to the data hist area
	  double fSumGer = pfGerHist->Integral();
	  fScale = fSumData/fSumGer;
	}
      else
	{
	  //Nomalize to peaks.
	  double fGerMax=pfGerHist->GetMaximum();
	  fScale = fDataMax/fGerMax;
	}
      pfGerHist->Scale(fScale);
    }
  // **********************************************************************

  // **********************************************************************
  //Find which one has maximun value and draw that one first
  // **********************************************************************
  pfDataHist->SetLineColor(kBlack);
  pfSimHist->SetLineColor(kRed);
  if(pfGerHist!=NULL)
    {
      pfGerHist->SetLineColor(kBlue);
    }
  fDataMax=pfDataHist->GetMaximum();
  fSimMax=pfSimHist->GetMaximum();
  TH1F* pfTallHist=pfDataHist;
  TH1F* pfShortHist=pfSimHist;
  if(fSimMax>fDataMax)
    {
      pfTallHist=pfSimHist;
      pfShortHist=pfDataHist;
    }

  if(pfGerHist!=NULL)
    {
      pfGerHist->Draw();
    }

  pfTallHist->Draw("same");
  pfShortHist->Draw("same");


  pfSimHist->SetDirectory(0);
  pfDataHist->SetDirectory(0);
  if(pfGerHist!=NULL)
    {
      pfGerHist->SetDirectory(0);
    }
  // ******************************************************************
  // Add in legand
  // ******************************************************************
  TLegend *legend=NULL;
  if(pfGerHist!=NULL)
    {
      legend=new TLegend(0.33,.7 ,0.89,0.83);
    }
  else
    {
      legend=new TLegend(0.33,.75 ,0.89,0.83);
    }
  legend->AddEntry(pfDataHist,"31955","l");
  legend->AddEntry(pfSimHist,"CosmicRay1Deg50mv12++M2","l");
  if(pfGerHist!=NULL)
    {
      legend->AddEntry(pfGerHist,"Chained Gernot 6000XX.root","l");
    }
  legend->Draw();

  return;
}
 
