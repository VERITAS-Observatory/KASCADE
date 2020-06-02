#include "TH1.h"
#include "TChain.h"
#include "VAShowerData.h"
#include "VAKascadeSimulationData.h"
#include "VAZAlphaShowerSelect.h"
#include "TTree.h"
#include <sstream>
#include <fstream>

TH1I* pShwrFrac;
TH1D* pELike;
TChain* pE;
TChain* pH;
TChain* pR;


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


void plotFractions(string ShwrSelTableName, double UpperLimit)
{
  pShwrFrac = new TH1I("pShwrFrac", "pShwrFrac",50,0,2);
  
  VAZAlphaShowerSelect shwrSelect(ShwrSelTableName); 

  TTree A;
  A.ReadFile(ShwrSelTableName.c_str(),
             "type/I:zen:az:E:sID:nShwr/F:total:n:frac");

  int type;
  int zen;
  int az;
  int energy;
  int shwrID;
  float frac;
  A.SetBranchAddress("type", &type);
  A.SetBranchAddress("zen", &zen);
  A.SetBranchAddress("az", &az);
  A.SetBranchAddress("E", &energy);
  A.SetBranchAddress("sID", &shwrID);
  A.SetBranchAddress("frac", &frac);

  int numEntries = A.GetEntries();
  for( int i = 0 ; i < numEntries; i++ ) {
    A.GetEntry(i);
    bool isGood = shwrSelect.isGoodFraction(type,zen,az,energy,shwrID,
                                            UpperLimit);
    if (isGood) {
      pShwrFrac->Fill(frac);
    }
  }
  pShwrFrac->Draw();
  return;
}
// ******************************************************************


void plotELike(string ElectronFileListName, double UpperLimit, int TelMult = 4)
{
  // gROOT->ProcessLine(".L $VEGASBASE/resultsExtractor/macros/ZAlphaUtilities.C");
  pE = (TChain*) new TChain("SelectedEvents/CombinedEventsTree");
  pH = (TChain*) new TChain("SelectedEvents/CombinedEventsTree");
  ZAlphaMKChainSims(ElectronFileListName, pE, pH);

  pELike = new TH1D("pELike","pElike", 50, -.1, 2.0);

  std::ostringstream os;
  os << ElectronFileListName <<"M" << TelMult << ".dat";
  string shwrSelTableName =  os.str();

  pShwrSelect = new VAZAlphaShowerSelect(shwrSelTableName); 

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
    bool isGoodShwr = pShwrSelect->isGoodFraction((int)sim->fCORSIKAParticleID,
                                                (int)sim->fPrimaryZenithDeg,
                                                (int)sim->fPrimaryAzimuthDeg,
                                                (int)sim->fEnergyGeV,
                                                (int)sim->fShowerID,
                                                UpperLimit);
    
    if (isGoodShwr) {
      double distance =
        sqrt(s->fDirectionXCamPlane_Deg*s->fDirectionXCamPlane_Deg + 
             s->fDirectionYCamPlane_Deg*s->fDirectionYCamPlane_Deg);
      if (distance < 1.0) {
        if (s->fMSW>0.5) {
          if(s->fImages >=TelMult) {
            double log10E = log10(s->fEnergy_GeV/1000.);
            pELike->Fill(log10E);
            eventCount++;
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
    else{
    
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
  pELike->Draw("same");
  cout <<endl << "Bad Showers: " << badKey << " Goodevents:" <<eventCount
       <<endl;
  cout << " Bad(frac,distance,msw,mult): " << badFrac <<" " << badDistance 
       << " " << badMSW << " " << badMult << endl;

  return;
}
// ******************************************************************
  
  


