//-*-mode:c++; mode:font-lock;-*-
TH1F** pfR1Width;
TH1F** pfR1Length;
TH1F** pfR1Size;
TH1F** pfR1Max3;

TH1F**      pfMeanScaledWidth;
TH1F**      pfMeanScaledLength;
TProfile**  pfEnergyBias;
TH1F**      pfAomega;
TProfile**  pfTheta2Bias;




TCanvas* fC1;
std::string pfRunNames[5];
std::string pfTels[5];
bool fHaveSimBranch;

void CompareRuns(std::string* RunNames, int NumRuns, std::string* Tels);
void CompareRunsPlot(std::string RunName, std::string Tel,int i);
void ScaleAndPlot(TH1F** pfDataHist, int fNumRuns,  bool fNormalizePeak);
void FillShowerPlots(std::string* RunNames, int NumRuns);
void PlotProfiles(TProfile** pfProfileHists, int fNumRuns);


void ShowerCompare(std::string Run1Name, std::string Run2Name=" ",
		   std::string Run3Name=" ", 
		   std::string Run4Name=" ")
{
  pfRunNames[0]=Run1Name;
  pfTels[0]= " ";
  int fNumRuns=1;
  if(Run2Name!=" ")
    {
      pfRunNames[fNumRuns]=Run2Name;
      pfTels[fNumRuns]= " ";
      fNumRuns++;
    }
  if(Run3Name!=" ")
    {
      pfRunNames[fNumRuns]=Run3Name;
      pfTels[fNumRuns]= " ";
      fNumRuns++;
    }
  if(Run4Name!=" ")
    {
      pfRunNames[fNumRuns]=Run4Name;
      pfTels[fNumRuns]= " ";
      fNumRuns++;
    }

  pfMeanScaledWidth  = new TH1F*[4];
  pfMeanScaledLength = new TH1F*[4];
  pfEnergyBias       = new TProfile*[4];
  pfTheta2Bias       = new TProfile*[4];
  pfAomega           = new TH1F*[4];


  gStyle->SetOptStat(kFALSE);
  fC1 = new TCanvas("fC1","Hillas Params ");


  for(int i=0;i<fNumRuns;i++)
    {
      FillShowerPlots(pfRunNames[i],i);
    }

  fC1->Clear();
  fC1->Divide(2,2);
  fC1->cd(1);
  ScaleAndPlot(pfMeanScaledWidth,fNumRuns,false);
  fC1->cd(2);
  ScaleAndPlot(pfMeanScaledLength,fNumRuns,false);
  fC1->cd(3);
  PlotProfiles(pfEnergyBias,fNumRuns);
  fC1->cd(4);
  PlotProfiles(pfTheta2Bias,fNumRuns);
  //  fC1->cd(5);
  //  ScaleAndPlot(pfAomega,fNumRuns,false);
  return;
}

void  FillShowerPlots(std::string RunName, int fRun)
{
  TFile Run (RunName.c_str());
  TTree* R1=(TTree*) Run.Get("CTree");
  if(R1==NULL)
    {
      std::cout<<"Not a Cumulative file!"<<std::endl;
      return;
    }
  VAShowerData* pfShower =new VAShowerData;
  R1->SetBranchAddress("S",&pfShower);
  if(R1->GetBranch("Sim")==NULL)
    {
      fHaveSimBranch=false;
    }
  else
    {
      fHaveSimBranch=true;
      VAKascadeSimulationData* pfSim = new VAKascadeSimulationData;
      R1->SetBranchAddress("Sim",&pfSim);
    }
  Color_t fColor;
  if(fRun==0)
    {
      fColor=kBlack;
    }
  if(fRun==1)
    {
      fColor=kGreen;
    }
  if(fRun==2)
    {
      fColor=kRed;
    }
  if(fRun==3)
    {
      fColor=kBlue;
    }

  // ************************************************************************
  // Set up LOg(E) energy bins: Bin size= Log(100 GeV) - Log(20 Gev) /N
  // where N is some multiple of 9
  // ************************************************************************

  double fLog10EMin=log10(20.0/1000.);
  double fLog10EStep=(log10(100.0/1000.)-fLog10EMin)/36;
  int fNumLog10EBins=(int) ((log10(35000.0/1000.)-fLog10EMin)/fLog10EStep);
  double fLog10EMax=fLog10EMin+fNumLog10EBins*fLog10EStep;

  //Cut for MSW and MSL
  std::string cuts = "S.fTheta2<.04 && S.fIsDirection==1";

  //Mean Scaled Width plots
  std::string title = "Mean Scaled Width";
  std::string fMSWPlot = "S.fMSW >> pfMSW";
  TH1F* pfMSW = new TH1F("pfMSW",title.c_str(),100,.4,3.0);
  pfMSW->SetXTitle("MSW(deg)");
  pfMSW->SetYTitle("#events");
  R1->Draw(fMSWPlot.c_str(),cuts.c_str());
  pfMSW->SetDirectory(0);
  pfMSW->SetLineColor(fColor); 
  pfMeanScaledWidth[fRun] = pfMSW;


  //Mean Scaled Length plots
  title = "Mean Scaled Length";
  std::string fMSLPlot = "S.fMSL >> pfMSL";
  TH1F* pfMSL = new TH1F("pfMSL",title.c_str(),100,.4,3.0);
  pfMSL->SetXTitle("MSL(deg)");
  pfMSL->SetYTitle("#events");
  R1->Draw(fMSLPlot.c_str(),cuts.c_str());
  pfMSL->SetDirectory(0);
  pfMSL->SetLineColor(fColor); 
  pfMeanScaledLength[fRun] = pfMSL;


  // ********************************************************************
  // Need to check for Sim branch before we do this.
  // ********************************************************************
  if(fHaveSimBranch)
    {
      //Energy bias
      title = "Energy Bias";
      TProfile* pfEBias = new TProfile("pfEBias",title.c_str(),fNumLog10EBins,
				       fLog10EMin,fLog10EMax,-1.0,2.0);
      pfEBias->SetXTitle("Log10(ETrue(TeV))");
      pfEBias->SetYTitle("<Log10(EReconsturcted/ETrue)>");
      
      // Start loop for filling
      int fNumEntries=(int)R1->GetEntries();
      for(int i=0;i<fNumEntries;i++)
	{
	  R1->GetEntry(i);//Loads "S" data into pfShower. and "Sim" into pfSim
	  
	  if(pfShower->fIsEnergy==1)
	    // && pfShower->fTheta2<.1)
	    {
	      double fLog10ETrue=log10(pfSim->fEnergyGeV/1000.); //in TeV
	      double fLog10ERcn=log10(pfShower->fEnergy/1000.);
	      //double fDiffLog10E=(fLog10ETrue-fLog10ERcn)/fLog10ETrue;
	      double fDiffLog10E=(fLog10ERcn-fLog10ETrue);
	      pfEBias->Fill(fLog10ETrue,fDiffLog10E,1.0);
	    }
	}
      pfEBias->SetDirectory(0);
      pfEBias->SetLineColor(fColor); 
      pfEnergyBias[fRun] = pfEBias;


      //Theta2 bias
      title = "Theta Squared Bias";
      TProfile* pfT2Bias = new TProfile("pfT2Bias",title.c_str(),
					fNumLog10EBins,fLog10EMin,fLog10EMax,
					-.5,.45);
      pfT2Bias->SetXTitle("Log10(ETrue(TeV))");
      pfT2Bias->SetYTitle("<Theta2>");

      // Start loop for filling
      for(int i=0;i<fNumEntries;i++)
	{
	  R1->GetEntry(i);//Loads "S" data into pfShower. and "Sim" into pfSim
      
	  if(pfShower->fIsDirection==1)
	    {
	      double fLog10ETrue=log10(pfSim->fEnergyGeV/1000.); //in TeV
	      pfT2Bias->Fill(fLog10ETrue,pfShower->fTheta2,1.0);
	    }
	}
      pfT2Bias->SetDirectory(0);
      pfT2Bias->SetLineColor(fColor); 
      pfTheta2Bias[fRun] = pfT2Bias;
    }
  else
    {
      pfEnergyBias[fRun] = NULL;
      pfTheta2Bias[fRun] = NULL;
    }
     

  //Theta2
  //cuts="S.fIsDirection==1 && S.fTheta>0";
  //title = "Theta**2";
  //std::string fT2Plot = "S.fTheta2:log10(Sim.fEnergyGeV/1000) >> pfT2";
  //TH2F* pfT2 = (TH2F*)new TH2F("pfT2","log10(ERcn/ETrue) vs log10(ETrue(TeV))",
  //			       fNumLog10EBins,fLog10EMin,fLog10EMax24,
  //			       ,50,0.0,.45)
  //pfT2->SetXTitle("log10(ETrue(TeV))";
  //pfT2->SetYTitle("log10(EReconstructed/ETrue)");
  //R1->Draw(fT2Plot.c_str(),cuts.c_str());
  //pfT2->SetDirectory(0);
  //pfT2->SetLineColor(fColor); 
  //pfTheta2[fRun] = pfT2;
  return;
}

void CompareTels(std::string fTels, std::string fRunName,  
		 std::string fMCRunName=" ")
// *************************************************************************
// fTels has format: 123-
// *************************************************************************
{
  pfR1Width=new TH1F*[5];
  pfR1Length=new TH1F*[5];
  pfR1Size=new TH1F*[5];
  pfR1Max3=new TH1F*[5];

  gStyle->SetOptStat(kFALSE);

  fC1 = new TCanvas("fC1","Hillas Params ");
  int fNumPlots=0;
  
  if(fMCRunName!=" ")
    {
      pfRunNames[fNumPlots]=fMCRunName;
      pfTels[fNumPlots]="T1";
      fNumPlots++;
    }

  for(int i=0;i<4;i++)
    {
      if(fTels.substr(i,1)!="-")
	{
	  pfRunNames[fNumPlots]=fRunName;
	  char p[10];
	  sprintf(p,"T%i",i+1);
	  std::string fTel(p);  
	  pfTels[fNumPlots]=fTel;
	  fNumPlots++;
	}
    }
  
  CompareRuns(pfRunNames,fNumPlots,pfTels);
  return;
} 
 

void CompareRuns(std::string fTel, std::string Run1Name, 
		 std::string Run2Name=" ",std::string Run3Name=" ", 
		 std::string Run4Name=" ")
{
  pfRunNames[0]=Run1Name;
  pfTels[0]=fTel;
  int fNumRuns=1;
  if(Run2Name!=" ")
    {
      pfRunNames[fNumRuns]=Run2Name;
      pfTels[fNumRuns]=fTel;
      fNumRuns++;
    }
  if(Run3Name!=" ")
    {
      pfRunNames[fNumRuns]=Run3Name;
      pfTels[fNumRuns]=fTel;
      fNumRuns++;
    }
  if(Run4Name!=" ")
    {
      pfRunNames[fNumRuns]=Run4Name;
      pfTels[fNumRuns]=fTel;
      fNumRuns++;
    }

  pfR1Width=new TH1F*[4];
  pfR1Length=new TH1F*[4];
  pfR1Size=new TH1F*[4];
  pfR1Max3=new TH1F*[4];

  gStyle->SetOptStat(kFALSE);
  CompareRuns(pfRunNames,fNumRuns,pfTels);

  return;
}

void CompareRuns(std::string* pfRunNames, int fNumRuns, std::string* pfTels)
{
  fC1 = new TCanvas("fC1","Hillas Params ");
  for(int i=0;i<fNumRuns;i++)
    {
     CompareRunsPlot(pfRunNames[i],pfTels[i],i);
    }

  fC1->Clear();
  fC1->Divide(2,2);
  fC1->cd(1);
  ScaleAndPlot(pfR1Width,fNumRuns,false);
  fC1->cd(2);
  ScaleAndPlot(pfR1Length,fNumRuns,false);
  fC1->cd(3);
  ScaleAndPlot(pfR1Size,fNumRuns,false);
  fC1->cd(4);
  ScaleAndPlot(pfR1Max3,fNumRuns,false);

  return;
}

void CompareRunsPlot(std::string RunName, std::string Tel,int fRun)
{
  std::string cuts = "H" + Tel + ".fGoodImage && H" + Tel+ ".fPixelsInImage>2";
  
  TFile Run (RunName.c_str());
  TTree* R1=(TTree*) Run.Get("CTree");
  Color_t fColor;
  if(fRun==0)
    {
      fColor=kBlack;
    }
  if(fRun==1)
    {
      fColor=kGreen;
    }
  if(fRun==2)
    {
      fColor=kRed;
    }
  if(fRun==3)
    {
      fColor=kBlue;
    }

  //Width plots
  std::string title = "Width";
  TH1F* pfWidth = new TH1F("pfWidth",title.c_str(),100,0.01,0.51);
  std::string widthplot = "H" + Tel + ".fWidth >> pfWidth";
  R1->Draw(widthplot.c_str(),cuts.c_str());
  pfWidth->SetDirectory(0);
  pfWidth->SetLineColor(fColor); 
  pfR1Width[fRun] = pfWidth;

  title = "Length";
  TH1F* pfLength = new TH1F("pfLength",title.c_str(),100,0.01,1.01);
  std::string lengthplot = "H" + Tel + ".fLength >> pfLength";
  R1->Draw(lengthplot.c_str(),cuts.c_str());
  pfLength->SetDirectory(0);
  pfLength->SetLineColor(fColor); 
  pfR1Length[fRun] =pfLength;
  
  title = "Size";
  TH1F* pfSize = new TH1F("pfSize",title.c_str(),100,1,2001);
  std::string sizeplot = "H" + Tel + ".fSize >> pfSize";
  R1->Draw(sizeplot.c_str(),cuts.c_str());
  pfSize->SetDirectory(0);
  pfSize->SetLineColor(fColor); 
  pfR1Size[fRun] =pfSize;
  
  title = "Max3";
  TH1F* pfMax3 = new TH1F("pfMax3",title.c_str(),100,1,201);
  std::string max3plot = "H" + Tel + ".fMax3 >> pfMax3";
  R1->Draw(max3plot.c_str(),cuts.c_str());
  pfMax3->SetDirectory(0);
  pfMax3->SetLineColor(fColor); 
  pfR1Max3[fRun] =pfMax3;

  return;
}

void ScaleAndPlot(TH1F** pfDataHist, int fNumRuns,  bool fNormalizePeak)
{
  double fSum = pfDataHist[0]->Integral();
  double fMax = pfDataHist[0]->GetMaximum();

  double fMaxAll=fMax;   //Keep track of tallest
  int fMaxHist=0;
  if(fNumRuns>1)
    {
      for(int i=1;i<fNumRuns;i++)
	{
	  if(!fNormalizePeak)
	    {
	      // scale tall hist areas to first hist
	      double fSumRun = pfDataHist[i]->Integral();
	      double fScale = fSum/fSumRun;
	    }
	  else
	    {
	      //Nomalize to peaks.
	      double fMaxRun=pfDataHist[i]->GetMaximum();
	      fScale = fMax/fMaxRun;
	    }
	  pfDataHist[i]->Scale(fScale);
	  double fMaxRun=pfDataHist[i]->GetMaximum();
	  if(fMaxRun>fMaxAll)
	    {
	      fMaxAll=fMaxRun;
	      fMaxHist=i;
	    }
	}
    }
  //Find which one has maximun value and draw that one first
  //Also some micky mouse stuff to keep the historgrams on diplay.
  pfDataHist[fMaxHist]->Draw();

  //Now draw the rest
  if(fNumRuns>1)
    {
      for(int i=0;i<fNumRuns;i++)
	{
	  if(i!=fMaxHist)
	    {
	      pfDataHist[i]->Draw("same");
	    }
	}
    }
  // ******************************************************************
  // Add in legand
  // ******************************************************************
  TLegend* pfLegend=new TLegend(0.33,.7 ,0.89,0.89);
  
  for(int i=0;i<fNumRuns;i++)
    {
      std::string fLable=pfRunNames[i] + ":" + pfTels[i];
      pfLegend->AddEntry(pfDataHist[i],fLable.c_str(),"l");

    }
  pfLegend->Draw();
  return;
}
// *************************************************************************
 
void PlotProfiles(TProfile** pfDataHist, int fNumRuns)
{
  int fFirstHist;
  for(fFirstHist=0;fFirstHist<fNumRuns;fFirstHist++)
    {
      if(pfDataHist[fFirstHist]!=NULL)
	{
	  pfDataHist[fFirstHist]->Draw();
	  break;
	}
    }
  if(fNumRuns>fFirstHist+1)
    {
      for(int j=fFirstHist+1;j<fNumRuns;j++)
	{
	  if(pfDataHist[j]!=NULL)
	    {
	      pfDataHist[j]->Draw("same");
	    }
	}
    }
  // ******************************************************************
  // Add in legand
  // ******************************************************************
  TLegend* pfLegend=new TLegend(0.33,.7 ,0.89,0.89);
   
  for(int i=0;i<fNumRuns;i++)
    {
      if(pfDataHist[i]!=NULL)
	{
	  std::string fLable=pfRunNames[i] + ":" + pfTels[i];
	  pfLegend->AddEntry(pfDataHist[i],fLable.c_str(),"l");
	}
    }
  pfLegend->Draw();
  return;
}
 
