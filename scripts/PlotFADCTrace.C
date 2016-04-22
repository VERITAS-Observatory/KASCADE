
#include "TTree.h"
#include "TH1D.h"
#include "TGraph.h"
#include "TF1.h"

#include <string>
#include <vector>
#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <cstdlib>
#include <iomanip>


const double kNSPerSsample = 2.0;
const int    kNumTraceBins = 32;   //Must always use the 32 bin inputs else we
                                   // get inf in various places. and this 
                                   // script hangs.

//const int    kNumTraceBins = 16;
const double kBinSpacingNS  = .25;
const double kLeadingEdgeFraction = .5;
const double kHiAmplitudeGain = 2.05;     //Upgrade:Hamamastsu PMT's
const double kHiToLowGainAmpUnsatRatio = 10.25; //Upgrade:Hamamastsu PMT's

//const double kHiAmplitudeGain = 1.32;     //V4-V5 photonis PMT's
//const double kHiToLowGainAmpUnsatRatio = 7.52; ///V4-V5 photonis PMT's




TTree   LGFADCTraces;
int   numClipped;
TH1D* pTemplateHist;
// KSAomega LGFADCTraces  TTree definiutions
float numPes;
int   tIndex;
int   amplitude;
float linearity;
float interpolatedLinearity;
int   numSmpl;
int   trace[kNumTraceBins];

float fHighSize;
float fLowSize;


TGraph* tmpltGraph;

// "Amp" and "Lin" for output file.
int     tmpltAmplitude;
double  tmpltLinearity;

// Vector to hold average lgain(n): Amplitude of pulse is Amp=lgain(n)*NPES
std::vector <double> timeSpreadLGain(50,0.0); 



class TracePair
{
 public:
  TracePair(){};
  ~TracePair(){};
  double time;
  double pulseHeight;
  // overloaded < operator
  inline bool operator <(const TracePair& tp) const;
};

inline bool TracePair::operator < (const TracePair& tp) const  {
  if(time < tp.time){
	return true;
  }
  return false;
}
// *********************************************************************

void usage()
{
  std::cout << " ********************Usage: " <<std::endl;

  std::cout << " TemplateFileGen(KSAomegaLogFileName,LGTemplatesFileName)" 
			<< std::endl;
  std::cout << " PlotTraceInit(fileName)" <<std::endl; 
  std::cout << " PlotTrace(n,hilo,tmpl,normalize=true,opt=Lsame)" << std::endl;
  std::cout << " PlotFADCTraces(hilo,tmpl,normalize=true,maxEnt=10,"
	"firstOpt=L)"<<std::endl;
}
// ********************************************************************

int Round(double number)
{
  return (number >= 0) ? (int)(number + 0.5) : (int)(number - 0.5);
}
// ***************************************************************************

bool ReadSimTracesFromLogFile(string KSAomegaLogFileName)
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
  // Define the branches in the internal TTree
  // ******************************************************************
  LGFADCTraces.Branch( "numPes",      &numPes,             "numPes/I");
  // LGFADCTraces.Branch( "hiSize",      &fHighSize,         "hiSize/F");

  LGFADCTraces.Branch( "tIndex",      &tIndex,             "tIndex/I");
  LGFADCTraces.Branch( "linearity",   &linearity,          "linearity/F");
  LGFADCTraces.Branch( "intrplin",    &interpolatedLinearity, "intrplin/F");
  LGFADCTraces.Branch( "amplitude",   &amplitude,          "amplitude/I");

  // LGFADCTraces.Branch( "loSize",       &fLowSize,         "loSize/F");

  LGFADCTraces.Branch( "numSmpl",     &numSmpl,            "numSmpl/I");
  LGFADCTraces.Branch( "trace",       &trace,              "trace[numSmpl]/I");
  // address of an address for an array (only on Branch , not SetBranchAddress)

  // ***************************************************
  //Open the input file
  // ***************************************************
  std::ifstream ifs( KSAomegaLogFileName.c_str());
  if (! ifs) {
    std::cout<<" Fatal- Can not open "<< KSAomegaLogFileName << std::endl;
    exit(1);
  }
  std::cout<<" Opened Input Trace file " << KSAomegaLogFileName << std::endl;
  // ***********************************************************************
  // Read in all the lines form the log file.
  // Only process those beginning with "**LowGain##"
  // Add them to the TTree
  // ***********************************************************************
  std::string line;
  string LGTraceFlag;
  int icount=0;
  while(1) {
	std::getline(ifs, line);
	if (ifs.eof() ) {
      break;   //and we are done!
    }
    // check if its a trace line
	istringstream is(line);
    is >> LGTraceFlag;
	if ( LGTraceFlag == "**LowGainTrace##" ) {
	  is >> numPes;
	  is >> fHighSize;
      is >> tIndex;
	  is >> linearity;
	  is >> interpolatedLinearity;
	  is >> amplitude;
	  is >> fLowSize;
	  is >> numSmpl;
	  for ( int j = 0; j <  numSmpl; j++) {
		is >> trace[j];
	  	if (ifs.eof() ) {
		  std::cout << " Error reading in ksAomageaLog file" << std::endl;
		  exit(1);
		}
	  }
	  // Add to TTree
	   LGFADCTraces.Fill();
	}
  }
  std::cout << "ReadSimTracesFromLogFile: All traces read in." << std::endl;
  return true;
}
// ***********************************************************************


void PlotTraceInit(std::string fileName)
{
  LGFADCTraces.ReadFile(fileName.c_str(),"n/I:hiSize/F:tmplt/I:Lin/F:Amp:L/F:loSize:numSmpl/I:trace[numSmpl]");

  LGFADCTraces.SetBranchAddress("tmplt",   &tIndex);
  LGFADCTraces.SetBranchAddress("Lin",     &linearity);
  LGFADCTraces.SetBranchAddress("intrpLin",&interpolatedLinearity);
  LGFADCTraces.SetBranchAddress("Amp",     &amplitude);
  LGFADCTraces.SetBranchAddress("numSmpl", &numSmpl);
  LGFADCTraces.SetBranchAddress("trace",   &trace);
}


bool PlotTrace(int n, TTree* hilo, int tmpl, bool normalize = true, std::string opt="Lsame")
{
  int pedestal=6.5;
  hilo->GetEntry(n);
  if (tIndex == tmpl) {
    TH1D* tr=new TH1D("tr","tr",kNumTraceBins,0,kNumTraceBins);
    double tMax=0;
    
    for(int m=0 ; m < kNumTraceBins ;m++) {
      tr->SetBinContent(m+1,trace[m]-pedestal);
      if(trace[m]-pedestal > tMax ) {
	tMax=trace[m]-pedestal;
      }
      std::cout<<" "<<trace[m]-pedestal<<" ";
    }
    std::cout<<std::endl;

    if( normalize) {
      tr->Scale(1./tMax);
    }
    int color=n%7;
    if(color==5) {
      color=7;
    }
    if(color==0) {
      color=1;
    }
    tr->SetLineColor(color);
    std::cout<<color<<std::endl;
    tr->SetMinimum(0.0);
    tr->SetLineWidth(2.0);
    tr->Draw(opt.c_str());
    return true;
  }
  else{
    return false;
  }
}
// ********************************************************************

void PlotFADCTraces(TTree* hilo, int tmpl, bool normalize=true, int maxEnt=10,std::string firstOpt="L")
{
  int plotCount=0;
  int numEntries=hilo->GetEntries();
  for ( int n=0;n<numEntries;n++) {
    bool plotted;
    if(plotCount==0) {
      plotted=PlotTrace(n, hilo, tmpl,normalize,firstOpt.c_str());
    }
    else{
      plotted=PlotTrace(n, hilo, tmpl, normalize);
    }
    if(plotted) {
      plotCount++;
      if(plotCount>=maxEnt) {
	return;
      }
    }
  }
  return;
}

// ********************************************************************
// What follows is stuff to make an average template file simular to what 
// came from washU.
// **********************************************************************

bool GetNextTrace(int noSpreadTemplateID, int& startTraceIndex) 
// ******************************
//Results are in in trace[]
// ******************************
{
  int numEntries=LGFADCTraces.GetEntries();
  while(1) {
    if ( startTraceIndex > numEntries -1) {
      return false;
    }
	// *************************************************
	//Fill trace,amplitude and linearity
    // *************************************************
	int numBytes = LGFADCTraces.GetEntry(startTraceIndex);
                                             
    if (numBytes == 0) {   //No such entry!!! How do we get here?
      return false;
    }
	// ****************************************************
	// We wat to ignore clipped traces
	// ****************************************************
	//"trace[numSmpl]
	int maxOfTrace = 0;
	for (int i = 0; i <numSmpl; i++) {
	  if (trace[i] > maxOfTrace ) {
		maxOfTrace=trace[i];
	  }
	}
    if (tIndex == noSpreadTemplateID) {
	  if ( maxOfTrace == 255) {
		numClipped ++;
	  }
	  else{
		return true;
	  }
	}
    startTraceIndex++;
  } 
}
// ***********************************************************************  

void GetLeadinEdgeAndPeakLocationsAndPeakValue(double& peakLocation, 
									double& leadingEdgeLocation, 
									double& traceAmplitudeDC)
// **********************************************************************
// Put the trace into a histogram, removed pedestal, fit a gaussian to it and 
// return the peak location in samples and the trace fitted max amplitude in DC
// Amplitude will be used to determine Time shifted Linearity
// **********************************************************************
{
  // bool debugPrint=true;
  bool debugPrint=false;
  if (pTemplateHist != NULL) {
	delete pTemplateHist;
  }
  pTemplateHist = new TH1D("templateHist","templateHist",kNumTraceBins,
								 0.5,kNumTraceBins+.5);
  
  // *****************************************************
  // Load it up with our trace
  // *****************************************************
  if ((int)(sizeof(trace)/sizeof(int)) < kNumTraceBins ) {
	std::cout<<"GetLeadinEdgeAndPeakLocationsAndPeakValue error: "
	           "trace.size()<< kNumTraceBins: trace.size(), kNumTraceBins: "
			 <<  sizeof(trace)/sizeof(int) << " " << kNumTraceBins << std::endl;
	exit(1);
  }

  for(int i=0; i<kNumTraceBins; i++) {
    pTemplateHist->SetBinContent(i+1,trace[i]);
  }
  double  minAmplitude= pTemplateHist->GetMinimum();
  // ************************************************
  // Restore trace[] with minimum(pedestal) removed and do same for 
  // historgram of trace
  // **********************************************
  if (debugPrint) {
	std::cout<<"  minAmplitude: "<< minAmplitude <<std::endl;
  }
  for(int i=0; i<kNumTraceBins; i++) {
    trace[i] = pTemplateHist->GetBinContent(i+1)-minAmplitude;
    pTemplateHist->SetBinContent(i+1,trace[i]);
  }

  // *************************************************
  // Now fit a gaussian to it.  This may need some refinement to center and 
  // limit the search
  // ***************************************************
  
  // *************************
  // Find max
  // ************************
  int maxSample= pTemplateHist->GetMaximumBin();
  double xmin=maxSample-4.0;
  double xmax=maxSample+2.5;
  //pTemplateHist->Draw("L");
  pTemplateHist->Fit("gaus","W"," ",xmin,xmax);
  TF1 *fit = pTemplateHist->GetFunction("gaus");
  peakLocation = fit->GetParameter(1);
  traceAmplitudeDC = fit->GetParameter(0);


  // **********************************************************************
  // Now search back down from the peak to find the leading edge at fraction 
  // fLeadingEdgeFraction
  // **********************************************************************
  double leadingEdgeAmplitude= traceAmplitudeDC*kLeadingEdgeFraction;
  int    peakIndex = pTemplateHist->GetXaxis()->FindBin(peakLocation);
  if (debugPrint) {
	std::cout<<"maxSample,xmin,xmax,peakLocation,traceAmplitudeDC,"
               "leadingEdgeAmplitude,peakIndex: "
			 <<maxSample << " " << xmin << " " << xmax << " " << peakLocation 
			 << " " << traceAmplitudeDC << " " << leadingEdgeAmplitude
			 << " " << peakIndex << std::endl;
  }


  for( int i= peakIndex; i>0; i--) {
    double levelLow = pTemplateHist->GetBinContent(i);
	 if(debugPrint) {
	   std::cout<< "i,levelLow: " << i << " " << levelLow << std::endl;
	 }

    if( levelLow < leadingEdgeAmplitude) {
	  // ******************************************************
	  // Interpolate
	  // ******************************************************
	  double leadingEdgeLocationLow= pTemplateHist->GetBinCenter(i);
	  double leadingEdgeLocationHigh= pTemplateHist->GetBinCenter(i+1);
	  double levelHigh= pTemplateHist->GetBinContent(i+1);
	  double ratio=(leadingEdgeAmplitude-levelLow)/(levelHigh-levelLow);
	  leadingEdgeLocation = leadingEdgeLocationLow + 
		ratio * (leadingEdgeLocationHigh - leadingEdgeLocationLow);
	  if(debugPrint) {
		std::cout<< " leadingEdgeAmplitude,peakIndex,levelLow,levelHigh, "
		  "leadingEdgeLocationLow,leadingEdgeLocationHigh,ratio,"
		  "leadingEdgeLocation: " 
				 <<leadingEdgeAmplitude << " " << peakIndex << " " << levelLow 
				 << " " << levelHigh << " " <<  leadingEdgeLocationLow << " " 
				 << leadingEdgeLocationHigh << " " << ratio << " " 
				 << leadingEdgeLocation <<std::endl;
	  }
	  break;
	}
  }
  return;
}

// *************************************************************************
void SaveShiftedNormalizedTrace(double timeShiftSample, double peakValue, 
				std::vector < TracePair >& tmplt)

// **************************************************************
// Save the time/pulse height pairs for this trace in the vector template
// The times should be shifted relative to the first trace to place the peaks 
// of all the traces in the same place. (Rising edge might be better, but this
//  is straight forward) 
// **************************************************************************
{ 
  for( int i=0; i<kNumTraceBins; i++) {
    TracePair tp;
    tp.time = ( ( double) i - timeShiftSample ) * kNSPerSsample;
    tp.pulseHeight = trace[i]/peakValue;
  
    //if(tp.time >= 0.0) {
      tmplt.push_back(tp);
      //}
  }
  return;
}
// ***************************************************************************

void GraphTemplate(std::vector < TracePair >& tmplt   )
// *************************************************************
// Debug plot of template pairs
// *************************************************************
{
  int numTmpltPoints = tmplt.size();
  tmpltGraph = new TGraph(numTmpltPoints);
  for (int i = 0; i<numTmpltPoints; i++ ) {
    tmpltGraph->SetPoint(i,tmplt.at(i).time,tmplt.at(i).pulseHeight);
  }
  
  //tmpltGraph->Draw("AL");
  tmpltGraph->Draw("AL");
  return;
}
// *****************************************************************

void SaveToTextFile(std::vector < TracePair >&tmplt, std::string fileOut)
// **********************************************************************
// Append a "*" (except for the first entry) then the amplitude and linearity 
// pair of this template (origen) to 
// the output file , Average this template over .1 ns and append the resulting
//  time/pulseHeight pairs to the output file.
// Don't forget to invert to match standard template format.
// ***********************************************************************
{
  bool debugPrint = false;
  // ******************************************************************
  // See if this file exsts
  // ******************************************************************
  std::ifstream inFile(fileOut.c_str());
  bool fileExists=inFile.good();
  inFile.close();
  
  // ******************************************************************
  // Now open (or create) the file for appending.
  // *****************************************************************
  std::fstream outFile;
  outFile.open (fileOut.c_str(), fstream::in | fstream::out | fstream::app);
  // *****************************************************************
  // If the file alreadyexists, this is the first entry. If not, put out 
  // a "*" to deleiniate a new template is coming
  // *****************************************************************
  if( fileExists) {
    outFile<<"*"<<std::endl;
  }

  // **********************************************************************
  // Transfer to the template file the size and linearity of this template
  // **********************************************************************
  outFile<<tmpltAmplitude<<" "<<tmpltLinearity<<endl;

  // ***********************************************************************
  // Now average over bin size of .1 ns
  // This code look alot like that in $KASCADEBASE/inputs/LoGainMeethods::
  // ***********************************************************************
  std::vector <double> pulse;   //used to fix min and peak
  pulse.clear();

  int    ibin              = 0;
  double sum               = 0;
  int    sumCount          = 0;
  bool   finishedPulse     = false;
  double oldBinCenterIndex = 0;
  double oldSumAverage     = 0;
  // ******************************************************
  // This outer loop is over output bins
  // ******************************************************
  int binCenterIndex       = 0;
  int tmpltIndex           = 0;
  int tmpltNumBins         = tmplt.size();
  if (debugPrint) {
	std::cout<<" tmpltNumBins:" <<  tmpltNumBins <<std::endl;
  }

  while(1) { 
    // **************************************************************
    //This is loop to produce one output bin
    // **************************************************************
    while (1){
	  if (debugPrint) {
		std::cout<<" tmpltIndex:" <<  tmpltIndex <<std::endl;
	  }
      
      if (tmpltIndex==tmpltNumBins ) {
		finishedPulse=true;
		break;           // done with this pulse, go on to the next one
      }
      
      // ***************************************
      // Use the first one to set 
      // our first output time (On the even .25 ns step
      // *************************************
      if (ibin == 0 ) {  //we are at the first bin in this pulse
		// init the time,find the offset
		binCenterIndex = Round(tmplt.at(tmpltIndex).time/kBinSpacingNS);
		oldBinCenterIndex=binCenterIndex-1;
		ibin=1;
      }
      
      int nextBinCenterIndex=Round(tmplt.at(tmpltIndex).time/kBinSpacingNS);
      
      if (nextBinCenterIndex==binCenterIndex) {
		sum=sum+tmplt.at(tmpltIndex).pulseHeight;
		sumCount++;
      }
      else {  // We are done with this bin and ready to start the next
	
		// ************************************************************
		// This is where we would check to see if we are skippinfg any 
		// time bins and if so we would interpolate
		// ************************************************************
		// oldSumAverage has previous pulse value and oldBinCenterIndex has 
		// old Index
		// See if we need to fill in bins
		// ***********************************************************
		double sumAverage=sum/sumCount;
		
		while( binCenterIndex-oldBinCenterIndex >1) {
		  // Interpolate
		  double weight= 1.0/((double)(binCenterIndex-oldBinCenterIndex));
	  
		  double interpolatedSumAverage=oldSumAverage +
			(sumAverage-oldSumAverage)*weight;
	  
		  oldBinCenterIndex++;
		  oldSumAverage = interpolatedSumAverage;
		  pulse.push_back(oldSumAverage);
		}
	
		pulse.push_back(sumAverage);
	
		oldSumAverage=sumAverage;
		oldBinCenterIndex=binCenterIndex;
	
		//and setup for the nect bin
		sum=tmplt.at(tmpltIndex).pulseHeight;
		sumCount=1;
		binCenterIndex=nextBinCenterIndex;
		ibin++;
      }
      tmpltIndex++;
    }
    if (finishedPulse) {
      break;
    }
  }
 
  // ***********************************************************************
  // Now need to rem min and normalize to peak at 1.00
  // ***********************************************************************
  std::vector< double >::iterator it;   //for pulse array
  // Find the min and max elements in the vector
  it = min_element(pulse.begin(),  pulse.end());
  double pulseMin  = *it;
  it = max_element(pulse.begin(), pulse.end());
  double pulseMaxHeight = *it;
  double pulseMax= pulseMaxHeight-pulseMin;


  std::vector < TracePair > aveTmplt;
  aveTmplt.clear();
  int numPulseBins=pulse.size();
  double binTime=0.0;
  for( int i=0; i < numPulseBins; i++ ){
    double plsHgt=(pulse.at(i)-pulseMin) / pulseMax;

    // This is where we invert the pulse also to match standard format.
    outFile<< std::fixed << std::setprecision(4) << binTime << " " << -plsHgt 
		   << std::endl;
    TracePair tp;
    tp.time=binTime;
    tp.pulseHeight=plsHgt;
    aveTmplt.push_back(tp);

    binTime = binTime + kBinSpacingNS;
  }
  
  GraphTemplate(aveTmplt);
  
  return;
}

// ********************************************************************

bool GenerateSimTemplates(int noSpreadTemplateID, int numToUse, 
			  std::string fileOut, bool saveTmplt=true)
// ************************************************************************
// Find numToUse traces for templates made from noSpreadTemplateID, line them 
// all up with the first one, normalize them and write out the time,height 
// pairs for all samples to the fileOut text file
// **************************************************************************
{
  bool debugPrint=false;
  //bool debugPrint=true;
  // ***********************************
  // Define place to keep the template (sum of all traces, shfited to have 
  // same peak)
  // ***********************************
  std::vector < TracePair > tmplt   ;
  tmplt.clear();

  // ******************************************
  // We have all the trace in the TTree A after running PlotTraceInit
  // We need to pickup the first trace with the noSpreadTemplateID
  // ******************************************
  int startTraceIndex=0;
  numClipped = 0;
  //Results in trace[], pedestal removed
  bool gotOne=GetNextTrace(noSpreadTemplateID,startTraceIndex);
  if(!gotOne) {
    std::cout<<"No such trace "<< noSpreadTemplateID << std::endl;
    return false;
  }
  if(debugPrint) {
	std::cout << "Got Trace 0 for template " << noSpreadTemplateID <<std::endl;
  }
  // **********************************************************************
  // Save the Amplitude and linearity of this template
  // **********************************************************************
  tmpltAmplitude=amplitude;
  tmpltLinearity=linearity;

  // ********************************************
  // Put this trace into a histogram and fit a gausion to it
  // Find the kLeadingEdgweFraction point up the leading edge
  // ********************************************
  double basePeakLocation;
  double baseLeadingEdgeLocation;
  double traceAmplitudeDC;
  GetLeadinEdgeAndPeakLocationsAndPeakValue(basePeakLocation, 
											baseLeadingEdgeLocation, 
											traceAmplitudeDC);

  double traceLGainSum =  traceAmplitudeDC/numPes;  //init the sum

  
  if (debugPrint) {
	std::cout<<"basePeakLocation,baseLeadingEdgeLocation,traceAmplitudeDC: "
			 <<basePeakLocation<<" " << baseLeadingEdgeLocation << " "
			 <<traceAmplitudeDC<<std::endl;
  }
  // *********************************************
  // Normalize and save this trace in vector pair
  // All traces will be shifted relative to this one
  // *********************************************
  
  SaveShiftedNormalizedTrace(0.0, traceAmplitudeDC, tmplt);

  // ********************************************
  // Now go through the remaining traces
  // ********************************************
  int numProcessed;
  for(numProcessed = 1;  numProcessed<numToUse; numProcessed++) {

    startTraceIndex++;  //Bump to next one to test.

    // Find next trace made from the specified base template.
    bool gotATrace = GetNextTrace(noSpreadTemplateID, startTraceIndex);
    if(!gotATrace) {
      std::cout<<"Not enough traces after first"<<std::endl;
      std::cout<<"Wanted "<<numToUse<< " got "<< numProcessed <<std::endl;
      break;
    }
	if(debugPrint) {
	  std::cout << "Got Trace " << numProcessed << " for template " 
				<< noSpreadTemplateID <<std::endl;
	}
    
    double peakLocation;
    double leadingEdgeLocation;
	GetLeadinEdgeAndPeakLocationsAndPeakValue(peakLocation, 
											  leadingEdgeLocation, 
											  traceAmplitudeDC);

	traceLGainSum += traceAmplitudeDC/numPes;

    //double sampleOffset= (peakLocation -basePeakLocation);
    double sampleOffset= (leadingEdgeLocation -baseLeadingEdgeLocation);

    if(debugPrint) {
	  std::cout<<"noSpreadTemplateID,startTraceIndex,leadingEdgeLocation,"
		"traceAmplitudeDC,sampleOffset: "
			   << noSpreadTemplateID << " " << startTraceIndex << " " 
			   << leadingEdgeLocation
			   <<" " <<traceAmplitudeDC << " " << sampleOffset << std::endl;
	}

    SaveShiftedNormalizedTrace(sampleOffset, traceAmplitudeDC, tmplt);

  }
  // ****************************************************************
  // Determine average amplitude this low gain pulse
  // We can improve this a bit. by look at HGSize logain size etc.
  // This is just the simplest
  // ****************************************************************
  timeSpreadLGain.at( noSpreadTemplateID ) = traceLGainSum/numProcessed;
  std::cout <<  noSpreadTemplateID << ": Old/new/newCorr Lin: " << tmpltLinearity ;
  // *********************************************************
  // Determin Linearity of timespread traces. Put into global variable
  // *********************************************************
  // -----------------
  // Try using actual Linearuity
  // -----------------
  //  // For template 0 its always 1.0 (actually linis ignored for template 0)
  //if (noSpreadTemplateID == 0) {
  //	tmpltLinearity = 1.0;
  //	std::cout << " " << tmpltLinearity;
  //	double correctedLinearity =  timeSpreadLGain.at(0) / 
  //	                          (kHiAmplitudeGain/kHiToLowGainAmpUnsatRatio);
  //std::cout << " " << correctedLinearity;
  if (noSpreadTemplateID == 0) {
	tmpltLinearity =  timeSpreadLGain.at(0) / 
	                          (kHiAmplitudeGain/kHiToLowGainAmpUnsatRatio);
  std::cout << " " << tmpltLinearity;
  }
  else {
	// ****************************************************
	// We want a linearity that is ratio of time-spread template (N) peak to 
	// that of peak pf Ideal template (0) no time spread.
	// That value is just (NPES*2.05/10.25)/NPES or just 
	//  kHiAmplitudeGain/kHiToLowGainAmpUnsatRatio 
	// 2.05/10.25 = .2(Hamamatsu) or 1.32/7.52(photonis) = .176.
	// (Note should they be the same value?, do I have the hi pulse gains 
	// correct?) 
	// *******************************************************************
	tmpltLinearity =  timeSpreadLGain.at(noSpreadTemplateID) / 
                                                   timeSpreadLGain.at(0);  
	std::cout << " " << tmpltLinearity << " ";
	tmpltLinearity = timeSpreadLGain.at(noSpreadTemplateID) / 
	                              (kHiAmplitudeGain/kHiToLowGainAmpUnsatRatio);
	std::cout << tmpltLinearity;
  }
  std::cout << std::endl;

  if ( numClipped != 0 ) {
	std::cout<< "Ignorred " << 	numClipped << " clipped traces! " 
			 << std::endl;
  }

  // Debug
  //GraphTemplate(tmplt);

  // **********************************************************************
  // Now sort the vector of time/pulse height pairs  by time and write
  // to the output file
  // **********************************************************************
  //std::cout<< "Sort started"<<std::endl;
  std::sort(tmplt.begin(), tmplt.end());  //TracePair has < operator that 
                                           //only checks .time relationships
  //std::cout<< "Sort finished"<<std::endl;
  //  for ( int j = 0; j<(int)tmplt.size();j++){
  //  std::cout<< "time,pulseHeight: "<<tmplt.at(j).time<<" "
  //	     <<tmplt.at(j).pulseHeight<<std::endl;
  //  }
  //GraphTemplate(tmplt);
  

  // **********************************************************************
  // Now save sorted pulse to text file
  // **********************************************************************
  if ( saveTmplt) {
	//"Lin" and "Amp" from global variab    tmpltLinearity and  tmpltAmplitude 
    SaveToTextFile(tmplt,fileOut); 
  }
  //GraphTemplate(tmplt);
  if (debugPrint) {
	std::cout<< " Template " <<  noSpreadTemplateID << "saved." <<std::endl;
  }

return true;
}
// ************************************************************************


void TemplateFileGen(string KSAomegaLogFileName, string LGTemplatesFileName)
// ************************************************************************
// 1.Parse through the KSAomega log file which was made with 
// KSFADC.cpp::fDebugPrint set to true. This file has all time spread low 
// gain traces that were generated as output lines which look like:
// **LowGainTrace## 998 5230.47 1 0.945  1019.86 16 9 19 49 97 151 172 150 108
//                   78 61 50 42 36 34 31 29
// Above is a 16 sample example. The lines are read in and placed in 
// TTree LGFADCTraces
// 2. Iterate through the template indexes generating the average trace shape 
// for each template.  Also calculate the expected linearity (template 0 
// (with no time spread, see below) is defined to have linearity of 1.0, all 
// other template linearitys are relative to its amplitude(not the time-spread
// template 0, there is ~ 20% difference!!).
// 3. Save each nomalized (peak ==1), averaged, with new linearity and old HG 
// amplitude (we may want to look at that some day).
// *************************************************************************
{
  // *****************************************************
  // Read the traces from the log file and place in the  LGFADCTraces TTree
  // *****************************************************
  ReadSimTracesFromLogFile(KSAomegaLogFileName);

  // ******************************************************
  // Loop through the indexes generating ave trace
  // Number of individual traces varies by template. Determine by hand and 
  // hardwired here. Default to 100 for now and 30 templates max
  // *******************************************************

  std::vector < int > numTracesToUse(100,25);

  int numTemplates = 100;  //Make extra room and let data tell us

  int templateID;
  for (templateID = 0; templateID < numTemplates; templateID++) {
	
	bool templateProcessed = GenerateSimTemplates(templateID, 
												numTracesToUse.at(templateID) ,
												LGTemplatesFileName, true);
  
	std::cout<< "Finished template " << templateID <<std::endl;
	if (!templateProcessed ) {
	  break;
	}
  }
  std::cout<<" Number of templates generated: " << templateID << std::endl;
  }

// *************************************************************************

