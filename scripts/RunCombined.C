//-*-mode:c++; mode:font-lock;-*-
// **********************************************************************************
// This is the original RunCombined.C script modified to handle the new combined Tree 
// sturcture (Vegas 2.4 or later).
// *********************************************************************************
// This produces all the values form either data or CR run files to makecomparions
// for detector parameter optimiozation.
// *********************************************************************************
// Run the data and or CR .cvbf/ .vbf files through Vegas(2.4 or later) using 
// whatever Quality and shower cuts and whatever LT you want. They don't matter.
// This  script does not look at any of the shower values or the cuts results.
// *********************************************************************************
//  This version is for the Old array where we need to:
//  1:Remove T1T4 only events
// *********************************************************************************
TH1F** pfR1Width;
TH1F** pfR1Length;
TH1F** pfR1Size;
TH1F** pfR1Max3;
TH1F** pfR1LOverS;
TH1F** pfR1NPixels;
TH1F**      pfMeanScaledWidth;
TH1F**      pfMeanScaledLength;
TProfile**  pfEnergyBias;
TH1F**      pfAomega;
TH1F**      pfAomegaSC;
TProfile**  pfTheta2Bias;

uint32_t fCombinedCutMask;
double fLiveTimeSec;

TCanvas* fC1;
std::string pfRunNames[5];
std::string pfTels[5];
bool fHaveSimBranch;

//Stings for Combined tree.
std::string fCTreeName="SelectedEvents/CombinedEventsTree";
std::string fPass2Cut=" ";
std::vector< std::string > fHStr;

//bool fSizeCut=true;
//std::string fNumMinPixels="5";

bool fSizeCut=false;
std::string fNumMinPixels="3";
std::string fTrigCut;
std::string fPlotOption=" ";

double fRunLengthSec=1200;
//double fRunLengthSec=1800;


bool useCutMask=false;
//bool useCutMask=true;

bool fNormPeaks=false;
//bool fNormPeaks=true;                  //For Compare Runs

bool fOldArray=false;


void CompareRuns(std::string* RunNames, int NumRuns, std::string* Tels);
void CompareRunsPlot(std::string RunName, std::string Tel,int i);
void ScaleAndPlot(TH1F** pfDataHist, int fNumRuns,  bool fNormalizePeak,
		  bool addLegend);
void FillShowerPlots(std::string* RunNames, int NumRuns);
void PlotProfiles(TProfile** pfProfileHists, int fNumRuns, bool addLegend);
// **********************************************************************************

void RunCombineUsage()
{
  cout<<"RunCombined commands:"<<endl;
  cout<<"       SetOldArray()          : Enable T1:T4 only cut"<<endl;
  cout<<"       SetNormalizeByPeaks() : Normalize plots to match peak heights."<<endl;
  cout<<"       SetNormalizeByAreas()  :Normalize plots to match areas "<<endl;
  cout<<"       CompareTels(\"1234\",\"RunFileName1.root\",\"RunFileName2.root\")"<<endl;
  cout<<"       CompareRuns(1,\"RunFileName1.root\",\"RunFileName2.root\",\"RunFileName3.root\",\"RunFileName4.root\")"<<endl;
}

  // *********************************************************************************

void SetOldArray()
{
  fOldArray=true;
  return;
}

void SetNormalizeByPeaks() 
{
  fNormPeaks=true;
 return;
}

void SetNormalizeByAreas()
{
  fNormPeaks=false;
  return;
}
// ****************************************************************************


int RunCombinedInit(std::string& Tel)
{
  fHStr.clear();
  fHStr.resize(4);
  fHStr.at(0)="vfTels[0].pfHillasData";
  fHStr.at(1)="vfTels[1].pfHillasData";
  fHStr.at(2)="vfTels[2].pfHillasData";
  fHStr.at(3)="vfTels[3].pfHillasData";
  // *************************************************************************
  // Require that at least 2 tels trigger
  // **************************************************************************
  fTrigCut= "(" +fHStr.at(0) + ".fIsL2Triggered +" + 
                 fHStr.at(1) + ".fIsL2Triggered +" +
                 fHStr.at(2) + ".fIsL2Triggered +" + 
                 fHStr.at(3) + ".fIsL2Triggered>1)";
  // ***************************************************************************
  // For Old Array also require that at we have atleast T2 and/or T3 in the trigger
  // This results in us ignoring T1T4 only events.
  // ****************************************************************************
  if( fOldArray) {
    fTrigCut += "&&(" +fHStr.at(1) + ".fIsL2Triggered +" +
                      +fHStr.at(2) + ".fIsL2Triggered>0)";
  }
  // ***************************************************************************   
  std::istringstream iss(Tel);
  int telIndex;
  iss>>telIndex;
  return telIndex-1;
}
// ****************************************************************************


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
  pfAomegaSC           = new TH1F*[4];


  gStyle->SetOptStat(kFALSE);
  fC1 = new TCanvas("fC1","Hillas Params ");



  for(int i=0;i<fNumRuns;i++)
    {
      FillShowerPlots(pfRunNames[i],i);
    }

  fC1->Clear();
  fPlotOption=" ";
  //fC1->Divide(2,2);
  fC1->Divide(2,3);
  fC1->cd(1);
  ScaleAndPlot(pfMeanScaledWidth,fNumRuns,false,true);
  fC1->cd(2);
  ScaleAndPlot(pfMeanScaledLength,fNumRuns,false,true);
  fC1->cd(3);
  PlotProfiles(pfEnergyBias,fNumRuns,true);
  fC1->cd(4);
  PlotProfiles(pfTheta2Bias,fNumRuns,true);
  fC1->cd(5);
  fPlotOption="E1";
  ScaleAndPlot(pfAomega,fNumRuns,false,true);
  fC1->cd(6);
  ScaleAndPlot(pfAomegaSC,fNumRuns,false,true);
  return;
}

void  FillShowerPlots(std::string RunName, int fRun)
{
  RunCombinedInit();
  
  TFile Run (RunName.c_str());
  TTree* CombinedEventsTree=(TTree*) Run.Get(fCTreeName.c_str());
  if(CombinedEventsTree==NULL)
    {
      std::cout<<"No CombinedEventsTree in file "<<RunName.c_str()<<std::endl;
      return;
    }
  VAShowerData* pfShower =new VAShowerData;
  CombinedEventsTree->SetBranchAddress("S",&pfShower);
  if(CombinedEventsTree->GetBranch("Sim")==NULL)
    {
      fHaveSimBranch=false;
    }
  else
    {
      fHaveSimBranch=true;
      VAKascadeSimulationData* pfSim = new VAKascadeSimulationData;
      CombinedEventsTree->SetBranchAddress("Sim",&pfSim);
    }

      CombinedEventsTree->SetBranchAddress("Cutmask",&fCombinedCutMask);


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

  std::cout<<"fLog10EMin: "<<fLog10EMin<<std::endl;
  std::cout<<"fLog10EMax: "<<fLog10EMax<<std::endl;
  std::cout<<"fNumLog10EBins: "<<fNumLog10EBins<<std::endl;

  std::string cuts;
  std::string fAOCuts;
  std::string fAOSCCuts;
  if(!useCutMask)
    {
      //Cut for MSW and MSL
      cuts = "S.fTheta2_Deg2<.04 && S.fIsDirection==1";

      //Cut for Aomega
      fAOCuts="Sim.fAomega*" +fTrigCut;
      fAOSCCuts="Sim.fAomega*(" +fTrigCut + "&& S.fMSW>.05 && S.fMSW<1.2 "
	"&& S.fMSL>0.05 && S.fMSL<1.4 && S.fTheta2_Deg2<.1 )";
    }
  else
    {
      cuts = "fCombinedCutMask==0";
      fAOCuts="Sim.fAomega*(fCombinedCutMask==0)";
      fAOSCCuts="Sim.fAomega*(fCombinedCutMask==0)";
    }

  //Mean Scaled Width plots
  std::string title = "Mean Scaled Width";
  std::string fMSWPlot = "S.fMSW >> pfMSW";
  TH1F* pfMSW = new TH1F("pfMSW",title.c_str(),100,.4,3.0);
  pfMSW->SetXTitle("MSW(deg)");
  pfMSW->SetYTitle("#events");
  CombinedEventsTree->Draw(fMSWPlot.c_str(),cuts.c_str());
  pfMSW->SetDirectory(0);
  pfMSW->SetLineColor(fColor); 
  pfMeanScaledWidth[fRun] = pfMSW;
  

  //Mean Scaled Length plots
  title = "Mean Scaled Length";
  std::string fMSLPlot = "S.fMSL >> pfMSL";
  TH1F* pfMSL = new TH1F("pfMSL",title.c_str(),100,.4,3.0);
  pfMSL->SetXTitle("MSL(deg)");
  pfMSL->SetYTitle("#events");
  CombinedEventsTree->Draw(fMSLPlot.c_str(),cuts.c_str());
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
      int fNumEntries=(int)CombinedEventsTree->GetEntries();
      for(int i=0;i<fNumEntries;i++)
	{
	  CombinedEventsTree->GetEntry(i);//Loads "S" data into pfShower. and "Sim" 
	                                  //into pfSim and Cutmask
	  if(pfShower->fIsEnergy==1 )
	    {
	      if(useCutMask)
		{
		  if(fCombinedCutMask!=0)
		    {
		      continue;
		    }
		}
	      //  if(fRun==0 || (fRun!=0 && fCombinedCutMask==0))
	      //{
	      double fLog10ETrue=log10(pfSim->fEnergyGeV/1000.); //in TeV
	      double fLog10ERcn=log10(pfShower->fEnergy_GeV/1000.);
	      double fDiffLog10E=(fLog10ERcn-fLog10ETrue);
	      pfEBias->Fill(fLog10ETrue,fDiffLog10E,1.0);
	      //}
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
	  CombinedEventsTree->GetEntry(i);//Loads "S" data into pfShower. and "Sim" into pfSim
      
	  if(pfShower->fIsDirection==1)
	    {
	      if(useCutMask)
		{
		  if(fCombinedCutMask!=0)
		    {
		      continue;
		    }
		}
	      double fLog10ETrue=log10(pfSim->fEnergyGeV/1000.); //in TeV
	      pfT2Bias->Fill(fLog10ETrue,pfShower->fTheta2_Deg2,1.0);
	    }
	}
      pfT2Bias->SetDirectory(0);
      pfT2Bias->SetLineColor(fColor); 
      pfTheta2Bias[fRun] = pfT2Bias;

      //Aomega plots
      title = "Effective Area-NoCuts";
      std::string fAomegaPlot = "log10(Sim.fEnergyGeV/1000.) >> pfAO";
      TH1F* pfAO = new TH1F("pfAO",title.c_str(),fNumLog10EBins,fLog10EMin,
                                                                   fLog10EMax);
      pfAO->SetXTitle("ln10(Energy(TeV))");
      pfAO->SetYTitle("M**2");
      CombinedEventsTree->Draw(fAomegaPlot.c_str(),fAOCuts.c_str(),"E1");
      pfAO->SetDirectory(0);
      pfAO->SetLineColor(fColor); 
      pfAomega[fRun] = pfAO;

      title = "Effective Area - Spectrum Cuts";
      std::string fAomegaSCPlot = "log10(Sim.fEnergyGeV/1000.) >> pfAOSC";
      TH1F* pfAOSC = new TH1F("pfAOSC",title.c_str(),fNumLog10EBins,fLog10EMin,
                                                                   fLog10EMax);
      pfAOSC->SetXTitle("ln10(Energy(TeV))");
      pfAOSC->SetYTitle("M**2");
      CombinedEventsTree->Draw(fAomegaSCPlot.c_str(),fAOSCCuts.c_str(),"E1");
      pfAOSC->SetDirectory(0);
      pfAOSC->SetLineColor(fColor); 
      pfAomegaSC[fRun] = pfAOSC;
    }
  else
    {
      pfEnergyBias[fRun] = NULL;
      pfTheta2Bias[fRun] = NULL;
    }
     

   return;
}
void CompareCuts(std::string RunName, std::string secondPassCut, 
		 std::string Tel)
{
  

  pfR1Width=new TH1F*[5];
  pfR1Length=new TH1F*[5];
  pfR1Size=new TH1F*[5];
  pfR1Max3=new TH1F*[5];
  pfR1LOverS=new TH1F*[5];
  pfR1NPixels=new TH1F*[5];

  gStyle->SetOptStat(kFALSE);

  fC1 = new TCanvas("fC1","Hillas Params ");
  
  pfRunNames[0]=RunName;
  pfRunNames[1]=RunName;
  pfTels[0]=Tel;
  pfTels[1]=Tel;
  int fNumPlots=2;

  fPass2Cut= " && " + secondPassCut;
  
  CompareRuns(pfRunNames,fNumPlots,pfTels);
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
  pfR1LOverS=new TH1F*[5];
  pfR1NPixels=new TH1F*[5];

  gStyle->SetOptStat(kFALSE);

  fC1 = new TCanvas("fC1","Hillas Params ");
  int fNumPlots=0;
  
  if(fMCRunName!=" ")
    {
      pfRunNames[fNumPlots]=fMCRunName;
      pfTels[fNumPlots]="1";
      fNumPlots++;
    }

  for(int i=0;i<4;i++)
    {
      if(fTels.substr(i,1)!="-")
	{
	  pfRunNames[fNumPlots]=fRunName;
	  char p[10];
	  sprintf(p,"%i",i+1);
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
  pfR1LOverS=new TH1F*[4];
  pfR1NPixels=new TH1F*[4];

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
     DetermineTriggerRates(pfRunNames[i],pfTels[i],i);
    }
  fC1->Clear();
  fC1->Divide(2,3);
  fC1->cd(1);
  fPlotOption=" ";
  ScaleAndPlot(pfR1Width,fNumRuns,fNormPeaks,true);
  fC1->cd(2);
  ScaleAndPlot(pfR1Length,fNumRuns,fNormPeaks,false);
  fC1->cd(3);
  ScaleAndPlot(pfR1Size,fNumRuns,fNormPeaks,false);
  fC1->cd(4);
  ScaleAndPlot(pfR1Max3,fNumRuns,fNormPeaks,false);
  fC1->cd(5);
  ScaleAndPlot(pfR1LOverS,fNumRuns,fNormPeaks,false);
  fC1->cd(6);
  ScaleAndPlot(pfR1NPixels,fNumRuns,fNormPeaks,false);


  return;
}

void DetermineTriggerRates(std::string RunName, std::string Tel,int fRun)	  
// *********************************************************
//Total rate calculation
// ********************************************************
{
  int telIndex= RunCombinedInit(Tel);   //defines fHStr and fTrigCut
  std::string cuts;
  std::string M2cuts;
  if(fSizeCut)
    {
      cuts = fHStr.at(telIndex)+".fIsL2Triggered && " +
	     fHStr.at(telIndex)+".fSize>200 &&" +
	     fHStr.at(telIndex)+".fPixelsInImage>="+fNumMinPixels+" && "+fTrigCut;
    }
  else
    {
      cuts = fHStr.at(telIndex)+".fIsL2Triggered && " +
	     fHStr.at(telIndex)+".fGoodImage && " +
	     fHStr.at(telIndex)+".fPixelsInImage>="+fNumMinPixels+" && "+fTrigCut;
    }

  TFile Run (RunName.c_str());
  TTree* CombinedEventsTree=(TTree*) Run.Get(fCTreeName.c_str());
 
  if(CombinedEventsTree->GetBranch("Sim")==NULL)
    {
      std::string fCuts=cuts;
      std::cout<<fCuts<<std::endl;
      TH1F* pInt = new TH1F("pInt","int rate",10,-2,2);
      CombinedEventsTree->Draw("S.fIsDirection>>pInt",fCuts.c_str());
      //std::cout<<"Run Trigger rate(Hz)(no deadtime correc tion): for T"<<Tel<<": "
      //	       <<pInt->GetEntries()/fRunLengthSec<<std::endl;
      std::cout<<"Live Trigger rate(Hz): for T"<<Tel<<": "
	       <<pInt->GetEntries()/fLiveTimeSec<<std::endl;
    }
  else
    {
      VAKascadeSimulationData* pfSim = new VAKascadeSimulationData;
      CombinedEventsTree->SetBranchAddress("Sim",&pfSim);
      std::string fCuts="*Sim.fIntegralRatePerEventHz"; 
      fCuts="("+cuts+")"+fCuts;
      //fCuts="(Sim.fEnergyGeV==Sim.fEnergyGeV)"+fCuts;
      std::cout<<fCuts<<std::endl;
      TH1F* pInt = new TH1F("pInt","int rate",100,0,40000);
      CombinedEventsTree->Draw("Sim.fEnergyGeV>>pInt",fCuts.c_str());
      cout<<"Live Trigger rate(Hz): for T"<<Tel<<": "<<pInt->Integral()<<endl;
    }
  
  // *******************************************************************
  // Now M2 rates No cuts
  std::string M2cuts= "Sim.fEnergyGeV==Sim.fEnergyGeV";
  if(CombinedEventsTree->GetBranch("Sim")==NULL)
    {
      int numEntries=CombinedEventsTree->GetEntries();
      std::cout<<"Overall Live Trigger rate(Hz): for T"<<Tel<<": "
	       <<numEntries/fLiveTimeSec<<std::endl;
      //std::cout<<"Overall Trigger rate(Hz): for T"<<Tel<<": "
      //	       <<numEntries/fRunLengthSec<<std::endl;
    }
  else
    {
      VAKascadeSimulationData* pfSim = new VAKascadeSimulationData;
      CombinedEventsTree->SetBranchAddress("Sim",&pfSim);
      std::string fCuts="*Sim.fIntegralRatePerEventHz"; 
      fCuts="(" + M2cuts + ")" + fCuts;
      //fCuts="(Sim.fEnergyGeV==Sim.fEnergyGeV)"+fCuts;
      std::cout<<fCuts<<std::endl;
      TH1F* pInt = new TH1F("pInt","int rate",100,0,40000);
      CombinedEventsTree->Draw("Sim.fEnergyGeV>>pInt",fCuts.c_str());
      cout<<"Overall Live Trigger rate(Hz): "<<pInt->Integral()
	  <<endl;
    }




  // ********************************************************
  return;
}


void CompareRunsPlot(std::string RunName, std::string Tel,int fRun)
{
  int telIndex=RunCombinedInit(Tel);
  //CUTS  
  std::string cuts;
  if(fSizeCut)
    {
      cuts =fHStr.at(telIndex)+".fIsL2Triggered && " +
	    fHStr.at(telIndex)+".fGoodImage && " +
            fHStr.at(telIndex)+".fSize>200 &&" + 
            fHStr.at(telIndex)+".fPixelsInImage>=" + fNumMinPixels;
    }
  else
    {
      cuts =fHStr.at(telIndex)+".fIsL2Triggered && " +
            fHStr.at(telIndex)+".fGoodImage && " +
            fHStr.at(telIndex)+".fPixelsInImage>="+fNumMinPixels+" && "+fTrigCut;
    }




  TTree* CombinedEventsTree=NULL;
  TFile Run (RunName.c_str());
  CombinedEventsTree=(TTree*) Run.Get(fCTreeName.c_str());
  if(CombinedEventsTree==NULL){
    std::cout<<"Could not find TTree "<<fCTreeName<<" in run file "<<RunName
	     <<std::endl;
    std::cout<<"Aborting display"<<std::endl;
    return;
  }
  else{
    int numEntries=CombinedEventsTree->GetEntries();
    VAParameterisedEventData* combParData = new VAParameterisedEventData();
    CombinedEventsTree->SetBranchAddress("P", &combParData);
    CombinedEventsTree->GetEntry(numEntries-1);
    for(int t=0;t<4;t++)
      {
	VAHillasData* hillasData = (VAHillasData*) combParData->getHillasData(t);
        if (hillasData != NULL) {
          break;
	}
      }
    fLiveTimeSec= hillasData->fLiveTime;
    cout<<"Run Livetime: "<<fLiveTimeSec<<" sec"<<endl;
    cout<<"All rates dead time corrected."<<endl;
  }

  Color_t fColor;
  if(fRun==0)
    {
      fColor=kBlack;
    }
  if(fRun==1)
    {
      fColor=kGreen;
      cuts = cuts+ fPass2Cut;
    }
  if(fRun==2)
    {
      fColor=kRed;
    }
  if(fRun==3)
    {
      fColor=kBlue;
    }
  std::cout<<"cuts:"<<cuts<<std::endl;

  //Width plots
  std::string title = "Width";
  TH1F* pfWidth = new TH1F("pfWidth",title.c_str(),100,0.01,0.51);
  std::string widthplot =fHStr.at(telIndex)+ ".fWidth >> pfWidth";
  CombinedEventsTree->Draw(widthplot.c_str(),cuts.c_str());
  pfWidth->SetDirectory(0);
  pfWidth->SetLineColor(fColor); 
  pfR1Width[fRun] = pfWidth;

  title = "Length";
  TH1F* pfLength = new TH1F("pfLength",title.c_str(),100,0.01,1.01);
  std::string lengthplot =fHStr.at(telIndex)+ ".fLength >> pfLength";
  CombinedEventsTree->Draw(lengthplot.c_str(),cuts.c_str());
  pfLength->SetDirectory(0);
  pfLength->SetLineColor(fColor); 
  pfR1Length[fRun] =pfLength;
  
  title = "Size";
  TH1F* pfSize = new TH1F("pfSize",title.c_str(),100,1,2001);
  std::string sizeplot =fHStr.at(telIndex)+ ".fSize >> pfSize";
  CombinedEventsTree->Draw(sizeplot.c_str(),cuts.c_str());
  pfSize->SetDirectory(0);
  pfSize->SetLineColor(fColor); 
  pfR1Size[fRun] =pfSize;
  
  title = "Max3";
  TH1F* pfMax3 = new TH1F("pfMax3",title.c_str(),100,1,201);
  std::string max3plot =fHStr.at(telIndex)+ ".fMax3 >> pfMax3";
  CombinedEventsTree->Draw(max3plot.c_str(),cuts.c_str());
  pfMax3->SetDirectory(0);
  pfMax3->SetLineColor(fColor); 
  pfR1Max3[fRun] =pfMax3;

  title = "LOverS";
  TH1F* pfLOverS = new TH1F("pfLOverS",title.c_str(),100,0,.001);
  std::string LOverSplot =fHStr.at(telIndex)+ ".fLengthOverSize >> pfLOverS";
  CombinedEventsTree->Draw(LOverSplot.c_str(),cuts.c_str());
  pfLOverS->SetDirectory(0);
  pfLOverS->SetLineColor(fColor); 
  pfR1LOverS[fRun] =pfLOverS;

  title = "NPixels";
  TH1F* pfNPixels = new TH1F("pfNPixels",title.c_str(),10,2,12);
  std::string NPixelsplot =fHStr.at(telIndex)+ ".fPixelsInImage >> pfNPixels";
  CombinedEventsTree->Draw(NPixelsplot.c_str(),cuts.c_str());
  pfNPixels->SetDirectory(0);
  pfNPixels->SetLineColor(fColor); 
  pfR1NPixels[fRun] =pfNPixels;
  
  return;
}

void ScaleAndPlot(TH1F** pfDataHist, int fNumRuns,  bool fNormalizePeak,
		  bool addLegend)
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
  std::string fPlotOptionSame;
  if(fPlotOption!=" ")
    {
      pfDataHist[fMaxHist]->Draw(fPlotOption.c_str());
      fPlotOptionSame=fPlotOption+"same";
    }
  else
    {
      pfDataHist[fMaxHist]->Draw();
      fPlotOptionSame="same";
     }

  //Now draw the rest
  //std:cout<<"fPlotOptionSame:"<<fPlotOptionSame<<":"<<std::endl;
  if(fNumRuns>1)
    {
      for(int i=0;i<fNumRuns;i++)
	{
	  if(i!=fMaxHist)
	    {
	      pfDataHist[i]->Draw(fPlotOptionSame.c_str());
	    }
	}
    }
  // ******************************************************************
  // Add in legand
  // ******************************************************************
  if(addLegend)
    {
      TLegend* pfLegend=new TLegend(0.33,.7 ,0.89,0.89);
  
      for(int i=0;i<fNumRuns;i++)
	{
	  std::string fLable=pfRunNames[i] + ":" + pfTels[i];
	  pfLegend->AddEntry(pfDataHist[i],fLable.c_str(),"l");
	  
	}
      pfLegend->Draw();
    }
  return;
}
// *************************************************************************
 
void PlotProfiles(TProfile** pfDataHist, int fNumRuns, bool addLegend)
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
  if(addLegend)
    {
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
    }
  return;
}
 
