#include "TH1.h"
#include "TChain.h"
#include "VAShowerData.h"
#include "VAKascadeSimulationData.h"
#include "VAZAlphaShowerSelect.h"
#include "TTree.h"

#include <vector>
#include <fstream>
#include <sstream>



TH1I* pShwrFrac;
TH1D* pELike = NULL;

TChain* pE;
TChain* pH;
TChain* pR;
TCanvas * c = new TCanvas("c", "c");

VAShowerData* s;
VAKascadeSimulationData* sim;
VAZAlphaShowerSelect* pShwrSelect;

bool ZAlphaMKChainSims(string ElectronLikeFileListName, TChain* pElectronChain, 
                       TChain* pHadronChain, TChain* pReconChain=NULL)
// *******************************************************************
// Chain togther all the combined trees from the Stage5 Electron files 
// listed in electonLikeFileList. Then create the chain for the same files 
// specs but for Hadrons (sustitute "Hadron" for "Electron" in all the 
// file names). 
// Then if the reconChain is not null, do the same for the Recon files.
// ******************************************************************* 
// Derived from Mary Kertzman's: ChainSims.C  2019-03-12
// *******************************************************************
{
  std::ifstream infile;
  infile.open(ElectronLikeFileListName.c_str(), std::ifstream::in);
  if(!infile.is_open()) {
    cout<< " Fatal-- Could not find/open ElectronLike list file:"
        << ElectronLikeFileListName << endl;
    return false;
  }

  vector < string > electronLikeFileList;
  vector < string > hadronLikeFileList;
  vector < string > reconFileList;

  // *******************************************
  // get all file names. See for replace explination:
  // http://www.cplusplus.com/forum/general/49520/
  // *******************************************
  string eTmpLine;
  string eStr = "Electron";
  string hStr = "Hadron";
  string rStr = "Recon";

  while(getline(infile, eTmpLine)) {
    electronLikeFileList.push_back(eTmpLine);
    string hTmpLine = eTmpLine; 
    hTmpLine.replace(hTmpLine.find(eStr),eStr.length(),"Hadron");
    hadronLikeFileList.push_back(hTmpLine);

    if ( pReconChain != NULL) {
      string rTmpLine=eTmpLine;
      rTmpLine.replace(rTmpLine.find(eStr),eStr.length(),"Recon");
      reconFileList.push_back( rTmpLine);
    }

  }
  // *************************************************
  // Assume chains are already defined (ie. the TTree name was set.)
  // load them up.
  // *************************************************
  for(uint32_t fileID = 0; fileID < electronLikeFileList.size(); fileID++) {
    pElectronChain->Add(electronLikeFileList.at(fileID).c_str());
    pHadronChain->Add(hadronLikeFileList.at(fileID).c_str());
    if ( pReconChain != NULL) {
      pReconChain->Add(reconFileList.at(fileID).c_str()); 
    }
  }
  return true;
}
// **************************************************************************


TH1D*  plotELike(string  ElectronFileListName,double UpperLimit,
                 int TelMult, string CutMode)
{

  TH1D* pELike = new TH1D("pELike","pElike", 50, -.1, 2.0);

  pE->SetBranchAddress("Sim", &sim);
  pE->SetBranchAddress("S", &s);
  
  //cout << "at 3" << endl;
  //int l = pE->GetEntry(k);
  //cout <<l<<endl;

  int numEntries = pE->GetEntries();

  cout<< "numEntries: " << numEntries << endl;

  string previousKey;
  int badKey = 0;
  int eventCount = 0;
  int badDistance = 0;
  int badMSW = 0;
  int badMult = 0;
  int badFrac = 0;

  for( int i = 0 ; i < numEntries; i++ ) {
    int j = pE->GetEntry(i);
    //Cut on aperture, and Array reconstruction Multiplcity, and image able 
    //to determine MSW
    double distance =
      sqrt(s->fDirectionXCamPlane_Deg*s->fDirectionXCamPlane_Deg + 
           s->fDirectionYCamPlane_Deg*s->fDirectionYCamPlane_Deg);
    if (distance < 1.0) {
      if (s->fMSW>0.5) {
        if(s->fImages >=TelMult) {

          //This is an event to enter into out plots,
          //if: Its not in a pecular showewr.(Electron)
          bool isGoodShwr;

          // Frac =  NumEvents(i)/(NumTotal-NumEvents(i)< Upperrlimit);

          if (CutMode == "frac") {
            isGoodShwr = pShwrSelect->isGoodByFraction(
                                               (int)sim->fCORSIKAParticleID,
                                               (int)sim->fPrimaryZenithDeg,
                                               (int)sim->fPrimaryAzimuthDeg,
                                               (int)sim->fEnergyGeV,
                                               (int)sim->fShowerID,
                                               UpperLimit);
          }
          else if( CutMode == "multAve") {
            // multAve =  NumEvents(i)/{(NumTotal-NumEvents(i))/NumShowwrs(E)}
            // < UpperLimit
            isGoodShwr = pShwrSelect->isGoodByNumEvents(
                                               (int)sim->fCORSIKAParticleID,
                                               (int)sim->fPrimaryZenithDeg,
                                               (int)sim->fPrimaryAzimuthDeg,
                                               (int)sim->fEnergyGeV,
                                               (int)sim->fShowerID,
                                               UpperLimit);
          }
          else{
            cout << "Invalid type selected: " << CutMode 
                 << ".  Valid are frac or multAve" << endl;
            exit(1);
          }
          if (isGoodShwr) {
            //Fille the histogram
            double log10E = log10(s->fEnergy_GeV/1000.);
            pELike->Fill(log10E);
            eventCount++;
          }
          else{
            // List showewrs that are rejected
            badFrac++;
            string showerIDKey = pShwrSelect->generateShowerIDKey(
                                                (int)sim->fCORSIKAParticleID,
                                                (int)sim->fPrimaryZenithDeg,
                                                (int)sim->fPrimaryAzimuthDeg,
                                                (int)sim->fEnergyGeV,
                                                (int)sim->fShowerID);
            
            if ( showerIDKey != previousKey) {
              cout << (int)sim->fCORSIKAParticleID<<" " 
                   << (int)sim->fPrimaryZenithDeg
                   << " " <<  (int)sim->fPrimaryAzimuthDeg<< " " 
                   << (int)sim->fEnergyGeV<< " " << (int)sim->fShowerID<< " " 
                   <<  UpperLimit << "  " << isGoodShwr << endl;
              previousKey = showerIDKey;
              badKey++;
            }
          }
        }
        else{
          badMult++;
        }
      }
      else{
        badMSW++;
      }
    }
    else{
      badDistance++;
    }
  }


  //pELike->Draw("same");
  cout <<endl << "Bad Showers: " << badKey << " Goodevents:" <<eventCount
       <<endl;
  cout << " Bad(frac,distance,msw,mult): " << badFrac <<" " << badDistance 
       << " " << badMSW << " " << badMult << endl;
  
  return pELike;
}
// ******************************************************************
  

void ShwrSelByFracElectronPlots(string Zn, string Az, string HiLo)
{
  
  vector <double> M4HiFracLimits = {5.0, 0.5, 0.4, 0.3,  0.25, 0.2};
  vector <double> M3HiFracLimits = {5.0, 0.5, 0.4, 0.3,  0.25, 0.2};
  vector <double> M2HiFracLimits = {5.0, 0.6, 0.5, 0.4,  0.3,  0.25};

  vector <double> M4LoFracLimits = {5.0, 0.6, 0.5, 0.4,  0.3,  0.25};
  vector <double> M3LoFracLimits = {5.0, 0.6, 0.5, 0.4,  0.3,  0.25};
  vector <double> M2LoFracLimits = {7.0, 1.5, 1.4, 1.25, 1.0,  0.9};
    
  string electronFileListName = "electron" + HiLo + Zn + "_" + Az + "FileList";
  if (pE != NULL) {
    delete pE;
  }
  if(pH!= NULL) {
    delete pH;
  }

  pE = (TChain*) new TChain("SelectedEvents/CombinedEventsTree");
  pH = (TChain*) new TChain("SelectedEvents/CombinedEventsTree");
  ZAlphaMKChainSims(electronFileListName, pE, pH);

  

  c->Clear();
  c->Divide(2,2);

  // Setup to record results of Powerlaw fit.
  string fitResltsFile = "fitResult"  + electronFileListName + "Frac.dat";
  std::ofstream fitResults(fitResltsFile.c_str());

  int TelMult;
  for (int j = 0; j < 3; j++) {
    // *****************
    //Iterate over Tel multiplicities
    // *****************

    // Setup frac cut levels.
    vector < double >*  pFracLimits;
    string multStr;
    if (j == 0) {
      TelMult=4;
      multStr="M4";
      if( HiLo == "Hi") {
        pFracLimits = &M4HiFracLimits;
      }
      else{
        pFracLimits = &M4LoFracLimits;
      }
    }
    else if ( j == 1) {
      TelMult=3;
      multStr="M3";
      if( HiLo == "Hi") {
        pFracLimits = &M3HiFracLimits;
      }
      else{
        pFracLimits = &M3LoFracLimits;
      }
    }
    else if (j == 2) {
      TelMult=2;
      multStr="M2";
      
      if( HiLo == "Hi") {
        pFracLimits = &M2HiFracLimits;
      }
      else{
        pFracLimits = &M2LoFracLimits;
      }
    }
    
    string shwrSelTableName = "ElectronLikeShowerSelTable" + multStr + " .dat";
    std::ifstream tabl( shwrSelTableName.c_str());
    if ( tabl) {
      tabl.close();
    }
    else{
      shwrSelTableName = electronFileListName + multStr + ".dat";
    }
    
    pShwrSelect = new VAZAlphaShowerSelect(shwrSelTableName); 
    
    std::ostringstream os;
    
    c->cd(j+1);
    gPad->SetLogy();
   
    for(int i =0; i < pFracLimits->size() ; i++) {
      string linColor = "Blck";
      if (i==1) {
        linColor = "Red";
      }
      else if (i == 2) {
        linColor = "Green";
      }
      else if ( i == 3 ) {
        linColor = "Blue";
      }
      else if ( i == 4 ) {
        linColor = "Purple";
      }
      else if ( i == 5 ) {
        linColor = "Cyan";
      }


      os<<pFracLimits->at(i) << linColor << " " ;
    }

    // Setup for power law fit.
    double log10ELow = 0;     //Set range (coud be done in Fit command.
    double log10EHi  = 1.2; 
    if( HiLo == "Lo") {
      log10ELow = -.1;
    }
    TF1* pF1 = new TF1("pF1","[1]*pow(10, (-[0]*x) )",log10ELow,log10EHi);



    for(int i =0; i < pFracLimits->size() ; i++) {
      
      pELike = plotELike(electronFileListName, pFracLimits->at(i), TelMult, 
                         "frac");
      if ( i < 4 ) {
        pELike->SetLineColor(i+1);
        pF1->SetLineColor(i+1);
      }
      else{
         pELike->SetLineColor(i+2);
         pF1->SetLineColor(i+2);
      }
      if (i == 0) {
        string eLikeTitle = "ElectronLike log10(Energy) " + HiLo + Zn + "_" + 
          Az + multStr + "(frac:" + os.str() + ")";
        //cout << eLikeTitle.c_str() << endl;
        pELike->SetTitle(eLikeTitle.c_str());
        pELike->Draw();
        
        
        //TH1D *h1 = (TH1D*)gPad->GetPrimitive("htemp");
        //h1->SetTitle(eLikeTitle.c_str());
      }
      else{
        pELike->Draw("same");
      } 

      //Now fit a powerlaw
      pF1->SetLineWidth(2);
      pF1->SetParameters(1,1);     //Inital vales uf parameters
      pELike->Fit("pF1","","",log10ELow,log10EHi);
      double chi       = pF1->GetChisquare();
      double alpha     = pF1->GetParameter(0);
      double alphaErr  = pF1->GetParError(0);
      double ampltd    = pF1->GetParameter(1);
      double ampltdErr = pF1->GetParError(1);
      int eventCount   = pELike->Integral();
      fitResults << i << " " <<  Zn << " " << Az << " " << TelMult << " " 
                 << pFracLimits->at(i) << "\t" << chi << " " << alpha << " " 
                 << alphaErr << " " << ampltd << " " << ampltdErr << " " 
                 << eventCount << endl;

    } // End frac level loop
  }// End multiplcity loop
  string plotName = "electronLikeEnergyDist" +  HiLo + Zn + "_" + Az + ".gif";
  c->Print(plotName.c_str());
}
// *************************************************************************


void ShwrSelByMultAveElectronPlots(string Zn, string Az, string HiLo)
{
  
  vector <double> M4HiLimits = {1000, 200, 100, 75, 50.};
  vector <double> M3HiLimits = {1000, 200, 100, 75, 50.};;
  vector <double> M2HiLimits = {1000, 200, 100, 75, 50.};

  vector <double> M4LoLimits = {1000, 200, 100, 75, 50.};
  vector <double> M3LoLimits = {1000, 200, 100, 75, 50.};
  vector <double> M2LoLimits = {1000, 200, 100, 75, 50.};
    
  string electronFileListName = "electron" + HiLo + Zn + "_" + Az + "FileList";
  if (pE != NULL) {
    delete pE;
  }
  if(pH!= NULL) {
    delete pH;
  }

  pE = (TChain*) new TChain("SelectedEvents/CombinedEventsTree");
  pH = (TChain*) new TChain("SelectedEvents/CombinedEventsTree");
  ZAlphaMKChainSims(electronFileListName, pE, pH);

  c->Clear();
  c->Divide(2,2);

  // Setup to record results of Powerlaw fit.
  string fitResltsFile = "fitResult"  + electronFileListName + "MultAve.dat";
  std::ofstream fitResults(fitResltsFile.c_str());

  int TelMult;
  for (int j = 0; j < 3; j++) {
    // *****************
    //Iterate over Tel multiplicities
    // *****************
    // Setup  cut levels.
    vector < double >*  pLimits;
    string multStr;
    if (j == 0) {
      TelMult=4;
      multStr="M4";
      if( HiLo == "Hi") {
        pLimits = &M4HiLimits;
      }
      else{
        pLimits = &M4LoLimits;
      }
    }
    else if ( j == 1) {
      TelMult=3;
      multStr="M3";
      if( HiLo == "Hi") {
        pLimits = &M3HiLimits;
      }
      else{
        pLimits = &M3LoLimits;
      }
    }
    else if (j == 2) {
      TelMult=2;
      multStr="M2";
      
      if( HiLo == "Hi") {
        pLimits = &M2HiLimits;
      }
      else{
        pLimits = &M2LoLimits;
      }
    }
    
    string shwrSelTableName = "ElectronLikeShowerSelTable" + multStr + " .dat";
    std::ifstream tabl( shwrSelTableName.c_str());
    if ( tabl) {
      tabl.close();
    }
    else{
      shwrSelTableName = electronFileListName + multStr + ".dat";
    }
    
    pShwrSelect = new VAZAlphaShowerSelect(shwrSelTableName); 
    
    std::ostringstream os;
    
    c->cd(j+1);
    gPad->SetLogy();
   
    for(int i =0; i < pLimits->size() ; i++) {
      string linColor = "Blck";
      if (i==1) {
        linColor = "Red";
      }
      else if (i == 2) {
        linColor = "Green";
      }
      else if ( i == 3 ) {
        linColor = "Blue";
      }
      else if ( i == 4 ) {
        linColor = "Purple";
      }
      else if ( i == 5 ) {
        linColor = "Cyan";
      }
      os<<pLimits->at(i) << linColor << " " ;
    }

    // Setup for power law fit.
    double log10ELow = 0;     //Set range (coud be done in Fit command.
    double log10EHi  = 1.2; 
    if( HiLo == "Lo") {
      log10ELow = -.1;
    }
    TF1* pF1 = new TF1("pF1","[1]*pow(10, (-[0]*x) )",log10ELow,log10EHi);

    for(int i =0; i < pLimits->size() ; i++) {
      
      pELike = plotELike(electronFileListName, pLimits->at(i), TelMult, 
                         "multAve");
      if ( i < 4 ) {
        pELike->SetLineColor(i+1);
        pF1->SetLineColor(i+1);
      }
      else{
         pELike->SetLineColor(i+2);
         pF1->SetLineColor(i+2);
      }
      if (i == 0) {
        string eLikeTitle = "ElectronLike log10(Energy) " + HiLo + Zn + "_" + 
          Az + multStr + "(multAve:" + os.str() + ")";
        //cout << eLikeTitle.c_str() << endl;
        pELike->SetTitle(eLikeTitle.c_str());
        pELike->Draw();
        
        
        //TH1D *h1 = (TH1D*)gPad->GetPrimitive("htemp");
        //h1->SetTitle(eLikeTitle.c_str());
      }
      else{
        pELike->Draw("same");
      } 

      //Now fit a powerlaw
      pF1->SetLineWidth(2);
      pF1->SetParameters(1,1);     //Inital vales uf parameters
      pELike->Fit("pF1","","",log10ELow,log10EHi);
      double chi       = pF1->GetChisquare();
      double alpha     = pF1->GetParameter(0);
      double alphaErr  = pF1->GetParError(0);
      double ampltd    = pF1->GetParameter(1);
      double ampltdErr = pF1->GetParError(1);
      int eventCount   = pELike->Integral();
      fitResults << i << " " <<  Zn << " " << Az << " " << TelMult << " " 
                 << pLimits->at(i) << "\t" << chi << " " << alpha << " " 
                 << alphaErr << " " << ampltd << " " << ampltdErr << " " 
                 << eventCount << endl;

    } // End mukltAve  level loop
  }// End multiplcity loop
  string plotName = "electronLikeEnergyDist" +  HiLo + Zn + "_" + Az +
                    "MultAve.gif";
  c->Print(plotName.c_str());
}
// *************************************************************************

void ShwrSelElectronPlots(string Zn, string Az, string HiLo, string CutMode)
{
  if  ( CutMode == "frac" ) {
    ShwrSelByFracElectronPlots(Zn, Az, HiLo);
  }
  else if ( CutMode == "multAve" ) {
    ShwrSelByMultAveElectronPlots(Zn, Az, HiLo);
  }
  else{
    std::cout << "Invalid CutMode Specified: " << CutMode 
              << "  Valid are: frac or multAve" << std::endl;
  }
  return;
}
// ************************************************************************
