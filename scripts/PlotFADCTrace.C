
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
const int    kNumTraceBins = 32;
const double kBinSpacingNS  = .25;

int   trace[kNumTraceBins];
int   size;
float linearity;

TTree   A;
TGraph* tmpltGraph;
int     baseTraceIndex;
int     tmpltSize;
double  tmpltLinearity;

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

int Round(double number)
{
  return (number >= 0) ? (int)(number + 0.5) : (int)(number - 0.5);
}
// ***************************************************************************


void PlotTraceInit(std::string fileName)
{
  A.ReadFile(fileName.c_str(),"n/I:hiSize/F:hiDC:hiMV:i/I:loSize/F:cl/I:S:L/F:fwhm:trace[32]/I");
  A.SetBranchAddress("i",&baseTraceIndex);
  A.SetBranchAddress("trace",&trace);
  A.SetBranchAddress("S",&size);
  A.SetBranchAddress("L",&linearity);
}


bool PlotTrace(int n, TTree* hilo, int tmpl, bool normalize = true, std::string opt="Lsame")
{
  int pedestal=6.5;
  hilo->GetEntry(n);
  if (baseTraceIndex == tmpl) {
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
  int numEntries=A.GetEntries();
  while(1) {
    if ( startTraceIndex > numEntries -1) {
      return false;
    }
    int numBytes = A.GetEntry(startTraceIndex); //Fills trace,size and 
                                                //linearity
    if (numBytes == 0) {   //No such entry!!! How do we get here?
      return false;
    }
    if (baseTraceIndex == noSpreadTemplateID) {
      return true;
    }
    startTraceIndex++;
  } 
}
// ***********************************************************************  

void GetPeakLocationAndValue(double& basePeakLocation,  
			     double& traceAmplitudeDC)
// **********************************************************************
// Put the trace into a histogram, fit a gaussian to it and return the
// peak location in samples and the trace fitted max amplitude in DC
// **********************************************************************
{
  TH1D* pTemplateHist = new TH1D("templateHist","templateHist",kNumTraceBins,0.5,kNumTraceBins+.5);
  // *****************************************************
  // Load it up with our trace
  // *****************************************************
  for(int i=0; i<kNumTraceBins; i++) {
    pTemplateHist->SetBinContent(i+1,trace[i]);
  }
  double  minAmplitude= pTemplateHist->GetMinimum();
  // ************************************************
  // Restore trace[] with minimum(pedestal) removed and do same for 
  // historgram of trace
  // **********************************************
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
  basePeakLocation = fit->GetParameter(1);
  traceAmplitudeDC = fit->GetParameter(0);
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
    tmplt.push_back(tp);
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
  for (int i = 1; i<numTmpltPoints; i++ ) {
    tmpltGraph->SetPoint(i,tmplt.at(i).time,tmplt.at(i).pulseHeight);
  }
  
  tmpltGraph->Draw("AL");
  return;
}
// *****************************************************************

void SaveToTextFile(std::vector < TracePair >&tmplt, std::string fileOut)
// **********************************************************************
// Append a "*" (except for the first entry) then the size and linearity pair 
// of this template (origen) to 
// the output file , Average this template over .1 ns and append the resulting
//  time/pulseHeight pairs to the output file.
// Don't forget to invert to match standard template format.
// ***********************************************************************
{
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
  outFile<<tmpltSize<<" "<<tmpltLinearity<<endl;

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
  while(1) { 
    // **************************************************************
    //This is loop to produce one output bin
    // **************************************************************
    while (1){
      
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

void GenerateSimTemplates(int noSpreadTemplateID, int numToUse, 
			  std::string fileOut, bool saveTmplt=true)
// ************************************************************************
// Find numToUse traces for templates made from noSpreadTemplateID, line them 
// all up with the first one, normalize them and writwe out the time,height 
// pairs for all samples to the fileOut text file
// **************************************************************************
{
  // ***********************************
  // Define place to keep the template (sum of all traces, shfited to have 
  // same peak)
  // ***********************************
  std::vector < TracePair > tmplt   ;
  tmplt.clear();

  // ******************************************
  // We have all the trace infor in the TTree A after running PlotTraceInit
  // We need to pickup the firat trace with the noSpreadTemplateID
  // ******************************************
  int startTraceIndex=0;
  //Results in trace[], pedestal removed
  bool gotOne=GetNextTrace(noSpreadTemplateID,startTraceIndex);
  if(!gotOne) {
    std::cout<<"Fatal no such trace"<<std::endl;
  }
  // **********************************************************************
  // Save the size and linearity of this template
  // **********************************************************************
  tmpltSize=size;
  tmpltLinearity=linearity;

  // ********************************************
  // Put this trace into a histogram and fit a gausion to it
  // ********************************************
  double basePeakLocation;
  double traceAmplitude;
  GetPeakLocationAndValue(basePeakLocation,  traceAmplitude);
  std::cout<<"basePeakLocation,traceAmplitude: "<<basePeakLocation<<" "
	   <<traceAmplitude<<std::endl;

  // *********************************************
  // Normalize and save this trace in vector pair
  // All traces will be shifted relative to this one
  // *********************************************
  
  SaveShiftedNormalizedTrace(0.0, traceAmplitude, tmplt);

  // ********************************************
  // Now go through the remaining traces
  // ********************************************

  for(int i=1; i<numToUse; i++) {

    startTraceIndex++;  //Bump to next one to test.

    // Find next trace made form the specified base template.
    bool gotATrace = GetNextTrace(noSpreadTemplateID, startTraceIndex);
    if(!gotATrace) {
      std::cout<<"Not enough traces after first"<<std::endl;
      std::cout<<"Wanted "<<numToUse<< " got "<< i <<std::endl;
      break;
    }
    
    double peakLocation;
    GetPeakLocationAndValue(peakLocation, traceAmplitude);
    
    double sampleOffset= (peakLocation -basePeakLocation);
    std::cout<<"noSpreadTemplateID,startTraceIndex,peakLocation,traceAmplitude,sampleOffset: "
	     << noSpreadTemplateID << " " << startTraceIndex << " " 
	     << peakLocation
	     <<" " <<traceAmplitude << " " << sampleOffset << std::endl;

    SaveShiftedNormalizedTrace(sampleOffset, traceAmplitude, tmplt);

  }

  // **********************************************************************
  // Now sort the vector of time/pulse height pairs  by time and write
  // to the output file
  // **********************************************************************
  std::sort(tmplt.begin(), tmplt.end());  //TracPair has < operator that only 
                                      //checks .time relationships
  //  for ( int j = 0; j<(int)tmplt.size();j++){
  //  std::cout<< "time,pulseHeight: "<<tmplt.at(j).time<<" "
  //	     <<tmplt.at(j).pulseHeight<<std::endl;
  //  }
  
  

  // **********************************************************************
  // Now save sorted pulse to text file
  // **********************************************************************
  if ( saveTmplt) {
    SaveToTextFile(tmplt,fileOut);
  }
  return;
}



