// *************************************************************************
// Program to process KASCADE low gain traces and compare actual HG Size 
// (from KASCADE) with VEGAS generated equivalent HIg Gain Size using the 
// Low gain diagnostics.
// written by:                                
// Glenn Sembroski - Purdue Univiersity - 2016 
//
// example:
// ./TestLowGainConversion <PMT type> <trace template ROOT lookup file>  
//           <trace transfer funciton ROOT file>  <KASCSADE ksAomega log file>
// where <PMT type> has 2 options:  either "old" or "new"           
//
// where <trace template ROOT lookup file> is the templae lookup text file 
//           produced by vaBuildLowGainTemplateLookup program.
//
// where  <trace transfer funciton ROOT file> is the text file produced by the
//           vaBuildLowGainTransferFunction  (code at lowGainTFGenerator.cpp)
//           program.
//
// where <KASCSADE ksAomega log file> is the log file produced by running 
//            ksAomege on a shower file with fDebug set to true in KSFADC.cpp.
//            The log file then has lines starting with "**LogGainTrace##"
//
// for reference see: 
//    https://veritas.sao.arizona.edu/wiki/
//               index.php/Pulse_Shapes_and_Amplitude_Response
//************************************************************************


//STL includes
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <fstream>
#include <vector>
#include <iterator>
#include <numeric>
#include <sys/stat.h>
#include <unistd.h>

//ROOT includes
#include "TH1.h"
#include "TH2.h"
#include "TH2D.h"
#include "TF1.h"
#include "TF2.h"
#include "TProfile.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TROOT.h"
#include "TMath.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TMultiGraph.h"
#include "TLegend.h"
#include "TEllipse.h"
#include "TPad.h"
#include "TApplication.h"
#include "TVector3.h"
#include "TStyle.h"
#include "TColor.h"
#include "TColorWheel.h"
#include "TNtuple.h"
#include "TLine.h"
#include "TText.h"
#include "TGaxis.h"
#include "TTree.h"
#include "TBranch.h"
#include "TDirectoryFile.h"

#include "VALowGainTraceAnalysis.h"
#include "VATraceEvaluator.h"
#include "VAArrayInfoFactory.h"

using namespace std;


VALowGainTraceTemplateFitAnalysis* lgAnalyserInstance;
VATraceEvaluatorLowGainDiagnostics* evaluatorInstance;
VALowGainTraceFitter* lowGainTraceFitter;

VAArrayInfo* info;
VAQStatsData* peds;
VATOffsetData* tOffsets;
VARelGainData* relativeGains;

const double   kFADCSamplingPeriodNS  = 2.0;  //samble bin size in ns
const double   kHiLoGainRatio         = 4.91;  //hilo attenuation ratio.
const double   kUnsaturatedThreshold  = 0.5;  //Place on rising pulse to 
                                              // backup from to find start of 
                                              // pulse
const double   kUnsaturatedLookbackNS = 4.0;  // Start 2 samples before 
                                              // UnsaturatedThreshold
const double   kUnsaturatedWindowNS   = 14.0; // LG window

const uint16_t telID              = 1;  
const uint16_t chanID             = 1;
const double   samplePedestal     = 6.5;

const double kMVToDCN      = 7.84;           //Nepomuks values
const double kUpgradeGainN = 2.33;           //Nepomuks values
const double kPhotonisGainG = 1.3;           //Glenns values
const double kPEToDCAreaG  = 5.62;           //Glenns value
const double kBinSpacing   = 0.25;
// ***********************************************************************

// KSAomega TRACE TTree definiutions
int   fNumPes;
float fHighSize;
int   fTemplateIndex;
float fLinearity;
int   pfLGTrace[16];
float fLowSize;
// **************************************************************************

bool readInSimTraces(string KSAomegaLogFileName, string tracesRootFileName)
// ***********************************************************************
// Read in from the KASCADE ksSAomega Log file which had the fDepugPrint in 
// ksAomega KSFADC.cpp enabled, the debug low gain trace lines. Place into a 
// TTree
// ***********************************************************************
{
  // ********************************************************************
  // Inorder to speed things up the code will be able to use a ksAomega Log 
  // file as is. All the lowgain trace lines will start wiht "**LowGainTrace##"
  // ********************************************************************

  // ******************************************************************
  // Define the TTree
  // ******************************************************************
  TFile* pKSAomegaTraces = (TFile*) new TFile( tracesRootFileName.c_str(),
											  "RECREATE");
  TTree simTraceTree(  "simTraceTree", "simTraceTree");

  simTraceTree.Branch( "numPes",        &fNumPes,        "numPes/I");
  simTraceTree.Branch( "hiSize",      &fHighSize,        "hiSize/F");
  simTraceTree.Branch( "tIndex", &fTemplateIndex,        "tIndex/I");
  simTraceTree.Branch( "linearity",     &fLinearity,     "linearity/F");
  simTraceTree.Branch( "lgTrace",       &pfLGTrace,      "lgtrace[16]/I");
  // address of an address for an array (only on Branch , not SetBranchAddress)
  simTraceTree.Branch( "loSize",       &fLowSize,        "loSize/F");

  
  // ***************************************************
  //Open the input file and create the output file
  // ***************************************************
  std::ifstream ifs( KSAomegaLogFileName.c_str());
  if (! ifs) {
    std::cout<<" Fatal- Can not open "<< KSAomegaLogFileName << std::endl;
    exit(1);
  }

  // ***********************************************************************
  // Read in all the lines form the log file.
  // Only process those beginning with "**LowGain##"
  // Add them to the TTree
  // ***********************************************************************
  std::string line;
  string LGTraceFlag;
  while(1) {
	std::getline(ifs, line);
	if (ifs.eof() ) {
      break;   //and we are done!
    }
    // check if its a trace line
	istringstream is(line);
    is >> LGTraceFlag;
	if ( LGTraceFlag == "**LowGainTrace##" ) {
	  is >> fNumPes;
	  is >> fHighSize;
      is >> fTemplateIndex;
	  is >> fLinearity;
	  
	  //cout<<"fHighSize: "<<fHighSize<<std::endl;
	  
	  for ( int j = 0; j < 16; j++) {
		is >> pfLGTrace[j];
	  	if (ifs.eof() ) {
		  std::cout << " Error reading in ksAomageaLog file" << std::endl;
		  exit(1);
		}
	  }
	  is >> fLowSize;
	  // Add to TTree
	  simTraceTree.Fill();
	}
  }
  simTraceTree.Write();
  pKSAomegaTraces->Close();
  return true;
}
// ***********************************************************************

void fillChanEvent(VAChanEvent& event, std::vector<float>& samples)
{
  // *********************************************************************
  // Constructor of VAChanEvents fills:
  // gDebug(iDebug), pfDebug(NULL),
  // fChanID(999),
  // fHit(false), fCFDTrigger(false),
  // fHiLo(EGAIN_UNKNOWN), fLo(false),
  // fSample()  
  // **********************************************************************
  // Load samples (not ped subtracted at this point)
  event.fChanID = chanID;
  event.fHiLo   = EGAIN_LO;
  event.fLo     = true;

  event.fSample.clear();
  for (int i = 0; i < (int)samples.size(); i++) {
	event.fSample.push_back(samples.at(i));
  }
  return;
}
// **********************************************************************

void initVegasEvaluatorInstance(string LowGainTemplateROOTFileName,
								string LowGainTransFuncROOTFileName)
{
  VATime time;
  time.setFromSystemTime();   //Upgrade time. But we only use this to make the
                              //getArrayInfo work, could have been any time. We
                              // are not sensitive to it in this program
  info  = VAArrayInfoFactory::instance()->getArrayInfo(time);
  // *************************************************************
  // Set hiGainRatio to 0.  This would cause the sHiLoGainRatio to be used
  // *************************************************************
  info->telescope(telID)->channel(chanID)->setHiLoGainRatio(0.0); 

  peds          = NULL;
  tOffsets      = NULL;
  relativeGains = NULL;
  
  evaluatorInstance = new VATraceEvaluatorLowGainDiagnostics(info, peds, 
								 tOffsets, relativeGains);
  

  // ****************************************************************
  // The following are all static so we can set them before we generate an 
  // instance of VALowGainTraceTemplateFitAnalysis.
  //THE  VALowGainTraceTemplateFitAnalysis CONSTRUCTOR NEEDS THEM!
  // ****************************************************************
  VALowGainTraceTemplateFitAnalysis::sUnsaturatedThreshold = 
	                                                   kUnsaturatedThreshold;
  VALowGainTraceTemplateFitAnalysis::sUnsaturatedLookback  =
	                                                  kUnsaturatedLookbackNS;
  VALowGainTraceTemplateFitAnalysis::sUnsaturatedWindow  =
	                                                    kUnsaturatedWindowNS;
  VALowGainTraceTemplateFitAnalysis::sADCSamplingPeriod = 
                                                       kFADCSamplingPeriodNS;


  lgAnalyserInstance = new VALowGainTraceTemplateFitAnalysis(
				 LowGainTemplateROOTFileName,LowGainTransFuncROOTFileName);
  evaluatorInstance->setLowGainTraceAnalyzer(lgAnalyserInstance);
  // **********************************************************************
  // Set up to use the mutable pointeres and references.
  // Don't really understand how this works but gets us past the 
  // protected/public issue
  // **********************************************************************
  lowGainTraceFitter       = lgAnalyserInstance->getLowGainTraceFitter();

  // *******************************************************************
  // Set HiLo gain ratio. Default is 6.0 or 5.8  etc. 
  // I think its more like 4.92
  // used in VALowGainTraceAnalysisDefault::getCharge (For unsaturated)
  // *******************************************************************
  lgAnalyserInstance->sHiLoGainRatio = kHiLoGainRatio;
  
  // Not sure why I have to set the Sampling period again but apparently I do.
  //lgAnalyserInstance->sADCSamplingPeriod = kFADCSamplingPeriodNS;

  // ******************************************************************
  // Define the vsalues of the sTracePropertySpace object: used by
  // VATraceEvaluatorLowGainDiagnostics::computeLowGainDiagnostics function
  // to make inital determination of the trace properties 
  // (saturated/unsaturated etc.)
  // ****************************************************************** 
  // TE_LowGainSaturatedMinTracePeakSample,
  //                                    TE_LowGainSaturatedMaxTracePeakSample
   evaluatorInstance->
	           sTracePropertySpace.fLowGainSaturatedRegion.first.first = 1.5f;
   evaluatorInstance->
	         sTracePropertySpace.fLowGainSaturatedRegion.first.second = 16.5f;

   // TE_LowGainSaturatedMinTraceSum, TE_LowGainSaturatedMaxTraceSum
   //   evaluatorInstance->
   //	    sTracePropertySpace.fLowGainSaturatedRegion.second.first = 1500.f;

   evaluatorInstance->
	        sTracePropertySpace.fLowGainSaturatedRegion.second.first = 1100.f;
   evaluatorInstance->sTracePropertySpace.fLowGainSaturatedRegion.second.second
                                                                     = 4000.f;
 
   // Initialize ranges that correspond to a timing pathology characterized 
   // by an early trace peak.
   evaluatorInstance->
	          sTracePropertySpace.fLowGainPeakEarlyRegion.first.first  = 0.5f;
   evaluatorInstance->
	        sTracePropertySpace.fLowGainPeakEarlyRegion.first.second   = 1.5f;
   evaluatorInstance->
	          sTracePropertySpace.fLowGainPeakEarlyRegion.second.first  = 0.f;
   evaluatorInstance->
	       sTracePropertySpace.fLowGainPeakEarlyRegion.second.second = 4000.f;
    
    // Initialize ranges that correspond to a timing pathology characterized by a late trace peak.
     evaluatorInstance->
	           sTracePropertySpace.fLowGainPeakLateRegion.first.first = 10.5f;
     evaluatorInstance->
              sTracePropertySpace.fLowGainPeakLateRegion.first.second = 16.5f;
     evaluatorInstance->
                sTracePropertySpace.fLowGainPeakLateRegion.second.first = 0.f;
     evaluatorInstance->
            sTracePropertySpace.fLowGainPeakLateRegion.second.second = 1500.f;
    
    // Initialize ranges that correspond to nominally normal low gain pulses.
     evaluatorInstance->
                  sTracePropertySpace.fLowGainNormalRegion.first.first = 1.5f;
     evaluatorInstance->
                sTracePropertySpace.fLowGainNormalRegion.first.second = 10.5f;
     evaluatorInstance->
                  sTracePropertySpace.fLowGainNormalRegion.second.first = 0.f;
     evaluatorInstance->
              sTracePropertySpace.fLowGainNormalRegion.second.second = 1500.f;
	 return;
}
// **********************************************************************

int main(int argc, char** argv)
{

  stringstream getPMTstatus;
  stringstream getTraceFile;
  stringstream getTransferFunctionFile;
  stringstream getksAomegaLogFile;

  string pmtType;
  getPMTstatus << argv[1];
  pmtType = getPMTstatus.str();
  bool oldPMTs;
  if(pmtType=="old") oldPMTs=true;
  else oldPMTs=false;

  string  lowGainTemplateROOTFileName;
  getTraceFile << argv[2];
  lowGainTemplateROOTFileName = getTraceFile.str();

  string  lowGainTransFuncROOTFileName;
  getTransferFunctionFile << argv[3];
  lowGainTransFuncROOTFileName = getTransferFunctionFile.str();

  string  ksAomegaLogFileName;
  getksAomegaLogFile << argv[4];
  ksAomegaLogFileName = getksAomegaLogFile.str();
  
  
  // ***********************************************************************
  // Initalize the Trace classes we will use to determine the equivalen high
  // gain size from the LOw gain trace we will get form the ksAomega log file.
  // ***********************************************************************
  initVegasEvaluatorInstance(lowGainTemplateROOTFileName,
							 lowGainTransFuncROOTFileName);

  // ************************************************************************
  // Build a TTree of traces from the KSDAomega log file
  // ************************************************************************
  string tracesFileName = "KSAomegaTraces.root";
  readInSimTraces(ksAomegaLogFileName,  tracesFileName);
  
  // Reopen the trace root file
  TFile* pKSAomegaTraces = (TFile*) new TFile(tracesFileName.c_str() ,
											  "update");
  // Setup the TTree for input.
  int   simNumPes;
  float simHighGainSize;
  int   simTemplate;
  float linearity;
  int   pLGTrace[16];
  float simLoGainSize;

  TTree* pSimTraceTree = (TTree*) pKSAomegaTraces->Get("simTraceTree");
  pSimTraceTree->SetBranchAddress("numPes", &simNumPes);
  pSimTraceTree->SetBranchAddress("hiSize",&simHighGainSize);//No RoundDown
  pSimTraceTree->SetBranchAddress("tIndex",&simTemplate);
  pSimTraceTree->SetBranchAddress("linearity",&linearity);
  pSimTraceTree->SetBranchAddress("lgTrace",pLGTrace); // No "&" here: Pointer 
                                                       // is an address
  pSimTraceTree->SetBranchAddress("loSize",& simLoGainSize);//Before round down

  int numTraces = pSimTraceTree->GetEntries(); 

  // ***********************************************************************
  // simTemplate == 0  is unsaturated template 1.0, 15000  Ideal template
  // simTemplate == 1  is first saturated template 16300 0.945  

  bool & tracePropertiesEvaluated = 
	                        evaluatorInstance->getLGTracePropertiesEvaluated();
  bool & traceIsSaturated         = evaluatorInstance->getLGTraceIsSaturated();
  
  for (int j=0; j<numTraces; j++){
	pSimTraceTree->GetEntry(j);
	
	if(simTemplate <30 ) {
	  
	std::vector<float> samples;
	for (int k = 0; k < 16; k++) {
	  samples.push_back(pLGTrace[k]);
	}
	
	// Fill the VAChan event: needed for the getCharge call.
	VAChanEvent event(false);
	fillChanEvent(event,samples);
	
	const uint32_t windowStart = 0;      //in samples
	const uint32_t windowWidth = 16;     //in samples
	const float    pedestal    = samplePedestal*windowWidth;

	// ********************************************
	// Enables a new template/trace fitting call within the getCharge call
	// ********************************************	
	tracePropertiesEvaluated = false;
   
	double equivHGSize = evaluatorInstance->getCharge(event, samples, 
														windowStart, 
														windowWidth,
														pedestal, telID);
	double templateAmplitude = 0;
	double templateLinerity  = 0;
	int fitMinimizerStatus   = 0;
	int satFlag              = 0;
	if ( traceIsSaturated ) {
	  satFlag = 1;
	  templateAmplitude = 
		   lowGainTraceFitter->getBestResult().mTemplateTrace.getAmplitude();
	  
	  templateLinerity = 
		   lowGainTraceFitter->getBestResult().mTemplateTrace.getLinearity();
	  
	  fitMinimizerStatus =
		                lowGainTraceFitter->getBestResult().mMinuitFitStatus;
	}
	
	int numSamples=(int)samples.size();
	int sumSamples=0;
	for(int T=0; T<numSamples;T++) {
	  sumSamples += samples.at(T);
	}
	
	int LGMaxSmpl =  *std::max_element(samples.begin(), samples.end()); 

	std::cout << j << " " << equivHGSize   << " " << simHighGainSize << " " 
			  << simLoGainSize << " " << simNumPes       << " " 
			  << simTemplate   << " " << linearity      << " "
			  << simHighGainSize*kHiLoGainRatio/equivHGSize << " "
			  << simHighGainSize/simLoGainSize              <<" "
			  << sumSamples<< " " << LGMaxSmpl << " " << satFlag << " "
			  << templateAmplitude <<" " << templateLinerity << " " 
			  << fitMinimizerStatus;
	std::cout << std::endl;
	}
  }
  return 0;
}
// *************************************************************************
