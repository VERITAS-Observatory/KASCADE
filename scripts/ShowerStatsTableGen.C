#include <sstream>
#include <ctime>
TTree* pM4TableTree;
TTree* pM3TableTree;
TTree* pM2TableTree;
TCanvas * c = new TCanvas("c", "c");

TH1I* pEM4Hist;
TH1I* pEM3Hist;
TH1I* pEM2Hist;
TH1I* pHM4Hist;
TH1I* pHM3Hist;
TH1I* pHM2Hist;


void ShwrPlot(TH1I* pMHist, string pMHistStr,string distributionType,
              TTree* pMTableTree, TCut plotCut, string cutType, string MLevel,
              string Zn, string Az, string HiLo)
{
  gPad->SetLogy();
  if (pMHist != NULL) {
    delete pMHist;
    pMHist = NULL;
  }  
  pMHist = (TH1I*)new TH1I(pMHistStr.c_str(),pMHistStr.c_str() ,10000,0,10000);
  string MArg = distributionType + " >> " + pMHistStr;
  pMTableTree->Draw(MArg.c_str(), plotCut);
  //Swet up to determine median.
  double x, q;
  q = 0.5; // 0.5 for "median"
  pMHist->ComputeIntegral(); // just a precaution
  pMHist->GetQuantiles(1, &x, &q);
  cout << pMHistStr << " median = " << x << endl;
  std::ostringstream MMedian;
  MMedian << x;
  string MMedianStr = MMedian.str();
  string MTitle = "Shower " +  distributionType + "distribution " + cutType +
    HiLo + " " + Zn + "_" + Az + MLevel + " median = " + MMedianStr.c_str();;
  pMHist->SetTitle(MTitle.c_str());
  return;
}

void ShwrSelPlot(string Zn, string Az, string HiLo, string distributionType)
{
  // **********************************************************
  // Prepare to generate the shower distribution plots
  // Make TTrees of tables.
  // **********************************************************
  if (pM4TableTree != NULL) {
    delete pM4TableTree;
    pM4TableTree = NULL;
  }  
  if (pM3TableTree != NULL) {
    delete pM3TableTree;
    pM3TableTree = NULL;
  }  
  if (pM2TableTree != NULL) {
    delete pM2TableTree;
    pM2TableTree = NULL;
  }  

  pM4TableTree = (TTree*) new TTree();
  pM3TableTree = (TTree*) new TTree();
  pM2TableTree = (TTree*) new TTree();

  string electronFileListName = "electron" + HiLo + Zn + "_" + Az + "FileList";

  string M4TableFileName = electronFileListName + "M4.dat";
  string M3TableFileName = electronFileListName + "M3.dat";
  string M2TableFileName = electronFileListName + "M2.dat";

  // *************************
  // Note variable names must match distributiontype argument
  // *************************
  int M4Num = pM4TableTree->ReadFile(M4TableFileName.c_str(),
                              "type/I:cutType:z:a:e:sID:n:t:events:frac/F");
  int M3Num = pM3TableTree->ReadFile(M3TableFileName.c_str(),
                              "type/I:cutType:z:a:e:sID:n:t:events:frac/F");
  int M2Num = pM2TableTree->ReadFile(M2TableFileName.c_str(),
                              "type/I:cutType:z:a:e:sID:n:t:events:frac/F");
  cout <<"Num Showers for " 
       << " M4:" << M4Num << ", " << pM4TableTree->GetEntries()  
       << " M3:" << M3Num << ", " << pM3TableTree->GetEntries()
       << " M2:" << M2Num << ", " << pM2TableTree->GetEntries() << endl;

  c->Clear();
  c->Divide(2,3);

  // And plot, Add cut to ingore 0 bin
 
  string plotECuTStr =  distributionType  + " > 0 && cutType == 1";
  TCut plotECut = plotECuTStr.c_str();
  string plotHCuTStr =  distributionType  + " > 0 && cutType != 1";
  TCut plotHCut = plotHCuTStr.c_str();


  c->cd(1);
  ShwrPlot(pEM4Hist,"PEM4Hist", distributionType, pM4TableTree, plotECut,
           "ElectronCut","M4", Zn, Az, HiLo);
  c->cd(2);
  ShwrPlot(pHM4Hist,"PHM4Hist", distributionType, pM4TableTree, plotHCut,
           "HadronCut","M4", Zn, Az, HiLo);
  c->cd(3);
  ShwrPlot(pEM4Hist,"PEM3Hist", distributionType, pM3TableTree, plotECut,
           "ElectronCut","M3", Zn, Az, HiLo);
  c->cd(4);
  ShwrPlot(pHM4Hist,"PHM3Hist", distributionType, pM3TableTree, plotHCut,
           "HadronCut","M3", Zn, Az, HiLo);
  c->cd(5);
  ShwrPlot(pEM4Hist,"PEM2Hist", distributionType, pM2TableTree, plotECut,
           "ElectronCut","M2", Zn, Az, HiLo);
  c->cd(6);
  ShwrPlot(pHM2Hist,"PHM2Hist", distributionType, pM2TableTree, plotHCut,
           "HadronCut","M2", Zn, Az, HiLo);

 
  string plotName = "Shower" + distributionType + "Dist" +  HiLo + Zn + "_" 
    + Az + ".gif";
  c->Print(plotName.c_str());
  return;
}


void ShwrSelGen(string Zn, string Az, string HiLo, string distributionType)
// ***********************************************************************
// This macro creates the ShwrSelect Fraction Table. It also plots the selected
// distributions.
// ***********************************************************************
{
  cout << "Be sure to do .L $VEGASBASE/resultsExtractor/macros/ZAlphaGenShwrIDTable.C " << endl;

  string electronFileListName = "electron" + HiLo + Zn + "_" + Az + "FileList";

  int firstIndex;
  if(HiLo == "Lo") {
    firstIndex=0;
  }
  else{
    firstIndex=8;
  }

  // *********************************************************
  //Generate the tables. Include both protons and he4
  // *********************************************************
  time_t current_time;
  current_time = time(NULL);
  cout << "Starting table gen for " << electronFileListName   
       << " for array Multiplicity 4 at " << ctime(&current_time) << endl;

  ZAlphaGenShwrIDTable(electronFileListName.c_str(),4,firstIndex,Zn.c_str(),
                       Az.c_str(),"proton","ElectronCuts");
  current_time = time(NULL);
  cout <<" M4 proton,ElectronCuts done at " <<  ctime(&current_time) << endl; 

  ZAlphaGenShwrIDTable(electronFileListName.c_str(),4,firstIndex,Zn.c_str(),
                       Az.c_str(),"he4","ElectronCuts");
  current_time = time(NULL);
  cout <<" M4 He4,ElectronCuts done at " <<  ctime(&current_time) << endl; 

  ZAlphaGenShwrIDTable(electronFileListName.c_str(),4,firstIndex,Zn.c_str(),
                       Az.c_str(),"proton","HadronCuts");
  current_time = time(NULL);
  cout <<" M4 proton,HadronCuts done at " <<  ctime(&current_time) << endl; 

  ZAlphaGenShwrIDTable(electronFileListName.c_str(),4,firstIndex,Zn.c_str(),
                       Az.c_str(),"he4","HadronCuts");
  current_time = time(NULL);
  cout <<" M4 He4,hadronCuts done at " <<  ctime(&current_time) << endl; 
 
  // *******************
  cout << "Starting table gen for " << electronFileListName   
       << " for array Multiplicity 3 "<< endl;

  ZAlphaGenShwrIDTable(electronFileListName.c_str(),3,firstIndex,Zn.c_str(),
                       Az.c_str(),"proton","ElectronCuts");
  current_time = time(NULL);
  cout <<" M3 proton,ElectronCuts done at " <<  ctime(&current_time) << endl; 

  ZAlphaGenShwrIDTable(electronFileListName.c_str(),3,firstIndex,Zn.c_str(),
                       Az.c_str(),"he4","ElectronCuts");
  current_time = time(NULL);
  cout <<" M3 He4,ElectronCuts done at " <<  ctime(&current_time) << endl; 

  ZAlphaGenShwrIDTable(electronFileListName.c_str(),3,firstIndex,Zn.c_str(),
                       Az.c_str(),"proton","HadronCuts");
  current_time = time(NULL);
  cout <<" M3 proton,HadronCuts done at " <<  ctime(&current_time) << endl; 

  ZAlphaGenShwrIDTable(electronFileListName.c_str(),3,firstIndex,Zn.c_str(),
                       Az.c_str(),"he4","HadronCuts");
  current_time = time(NULL);
  cout <<" M3 He4,HadronCuts done at " <<  ctime(&current_time) << endl; 

  // *******************
  cout << "Starting table gen for " << electronFileListName   
       << " for array Multiplicity 2 " << endl;
  ZAlphaGenShwrIDTable(electronFileListName.c_str(),2,firstIndex,Zn.c_str(),
                       Az.c_str(),"proton","ElectronCuts");
  current_time = time(NULL);
  cout <<" M2 proton,ElectronCuts done at " <<  ctime(&current_time) << endl; 
  
  ZAlphaGenShwrIDTable(electronFileListName.c_str(),2,firstIndex,Zn.c_str(),
                       Az.c_str(),"he4","ElectronCuts");
  current_time = time(NULL);
  cout <<" M2 He4,ElectronCuts done at " <<  ctime(&current_time) << endl; 
  
  ZAlphaGenShwrIDTable(electronFileListName.c_str(),2,firstIndex,Zn.c_str(),
                       Az.c_str(),"proton","HadronCuts");
  current_time = time(NULL);
  cout <<" M2 proton,HadronCuts done at " <<  ctime(&current_time) << endl; 
  
  ZAlphaGenShwrIDTable(electronFileListName.c_str(),2,firstIndex,Zn.c_str(),
                       Az.c_str(),"he4","HadronCuts");
  current_time = time(NULL);
  cout <<" M2 He4,HadronCuts done at " <<  ctime(&current_time) << endl; 

  // ***********

  // And plot stuff and save plot to a file
  ShwrSelPlot(Zn, Az, HiLo, distributionType);
  return;
}

