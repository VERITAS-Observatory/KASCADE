//Root script
const double kMVToDCN         = 7.84;           //Nepomuks values
const double kUpgradeGainN    = 2.33;           //Nepomuks values
const double kPhotonisGainG   = 1.3;           //Glenns values
const double kPEToDCAreaG     = 5.62;            //Glenns value
const double kBinSpacingNS    = 0.25;
const double k20PercentPositionNS = 5.0;
const int    kMaxNumTemplates = 100;
const double kLengthOfTemplateNS = 64.0;
#include <vector>
#include <iostream>
#include <string>
#include <fstream>
#include <algorithm>
#include <iterator>
#include <cstdlib>
#include <sstream>
#include <cmath>

#include <TH1D.h>
#include <TFile.h>
#include <TTree.h>

//VALowGainTraceTemplateFitAnalysis* pfLowGainAnalyser;

std::vector < std::vector < double > > lowGainTemplates;
std::vector < double > highGainTemplate;
std::vector < double > highGainAreaDC;
std::vector < double > highGainAmplitudeDC;
std::vector < double > highGainAmplitudeMV;
std::vector < double > lowGainLinearity;
std::vector < double > lowGainStartBinNS;
std::vector < double > startBinsNS;
std::vector < TH1D >   templateHists;
std::vector < TH1D >   highGainTemplateHists;
std::vector < double > templateFWHM;
std::vector < double > templateOffsets;
std::vector < std::vector <double > > waveForms;


// KSAomega TRACE TTree definiutions
int   fNumPes;
int   fHighSize;
int   fTemplateIndex;
float fLinearity;
int   pLGTrace[16];
// *************************************************************************

void usage()
{
  std::cout<<"*********LowGainMethods usage: *****************"<<std::endl;
  std::cout<<"*   GenKASCADETemplateFile(inFileName,outFileName,upgrade=true,"
             "highgain=false)"<<std::endl<<std::endl;
  std::cout<<"*   ReadHighGainKASCADETemplate(fileName)"<<std::endl;
  std::cout<<"*   ReadLowGainKASCADETemplates(fileName)"<<std::endl<<std::endl;
  std::cout<<"*   LoadLowGainTemplateHists()"<<std::endl;
  std::cout<<"*   LoadHighGainTemplateHist(PMTType=Upgrade)"<<std::endl
		   <<std::endl;
  std::cout<<"*   PlotTemplateHists(opt=Lsame,hStart=0,hEnd=28)"<<std::endl;
  std::cout<<"*   PlotTemplateHists(histIndex,opt=L)"<<std::endl;
  std::cout<<"*   PlotHighGainTemplates(opt=Lsame)"<<std::endl;
  std::cout<<"*   PlotHighGainTemplateHist(histIndex,opt=L)"<<std::endl
		   <<std::endl;
  std::cout<<"*   LowGainChageConversionValidation(KSAomegaLogFileName,"
	"LowGainTemplateROOTFileName,"<<std::endl;
  std::cout<<"*                                 LowGainTransFuncROOTFileName)"
		   <<std::endl;
  std::cout<<"************************************************"<<std::endl;
  return;
}
// **********************************************************************

  
double sumPulse(std::vector < double >& p)
{
  int endBin=p.size()-1;
  double sumP=0;
  for (int i=0; i < endBin;i++) {
    sumP+=p.at(i);
  }
  return sumP;
}
// *************************************************************


void  ReadHighGainKASCADETemplate(std::string fileName)
{
  
  bool debugPrint=false;
  //bool debugPrint=true;


  std::ifstream ifs(fileName.c_str());
  if (! ifs) {
    std::cout<<" Fatal-problems opening input template file "<< fileName<<std::endl;
    exit(1);
  }
  int flag1;
  int flag2;

  ifs >> flag1 >> flag2;
  if ( ifs.eof() || ( flag1!= -9999 || flag2 != -9999 ) ) {
    std::cout<<"Fatal -Problems reading first line of file" << fileName<<std::endl;
    exit(1);
  }
 
  if(debugPrint) {
    std::cout<<"flag1,flag2: "<<flag1<<" "<<flag2<<std::endl;
  }
  
  bool finishedFile=false;
  double startBinNS;



  while (1) {                    //Over templates
    double equivHiGainArea;
    double linearity;
    ifs >> equivHiGainArea >> linearity;
    if ( ifs.eof() ) {
      std::cout<<"Fatal -Unexpected eof Problems reading first line of file" 
			   << fileName<<std::endl;
      exit(1);
    }
  
    std::cout<<"equivHiGainArea,linearity:"<< equivHiGainArea <<" "<<linearity
			 <<std::endl;

    highGainTemplate.clear();
    bool pulseInited=false;
    double oldBinTime=0;
    double time;
    double pulseHeight;
    while(1) {     //readin and save the pulse

      ifs >> time >> pulseHeight;
      if (ifs.eof() ) {
		finishedFile=true;
		break;
      }
      if ( (int) time == -9999) {
		finishedFile=false;
		break;
      }
	  // Make sure no missing bins
      if( ! pulseInited) {
		startBinNS=time;
		oldBinTime=time - kBinSpacingNS;
        pulseInited=true;
      }
      
      if(std:: fabs((float)(time-(oldBinTime+kBinSpacingNS)) )>
		 (float) kBinSpacingNS/1000. ) {
		std::cout<<"Missing bin time: "<<oldBinTime+kBinSpacingNS<<std::endl;
      }
      
      if(debugPrint) {
		std::cout<<"time,pulseHeight: "<< time << " "<< pulseHeight<<std::endl;
      }
      
      highGainTemplate.push_back(pulseHeight);
      oldBinTime=time;
    }
    std::cout<<"Sum: "<< sumPulse(highGainTemplate)<<std::endl;
    highGainAreaDC.push_back(equivHiGainArea);
    lowGainLinearity.push_back(linearity);
    lowGainStartBinNS.push_back(startBinNS);

    if (finishedFile) {
      break;
    }
  }
  std::cout<<"Length of Template:"<<highGainTemplate.size()<<std::endl;
  return;
}
// ***************************************************************************

void  ReadLowGainKASCADETemplates(std::string fileName)
{
  
  bool debugPrint=false;
  //bool debugPrint=true;


  std::ifstream ifs(fileName.c_str());
  if (! ifs) {
    std::cout<<" Fatal-problems opening input template file "<< fileName<<std::endl;
    exit(1);
  }
  int flag1;
  int flag2;

  ifs >> flag1 >> flag2;
  if ( ifs.eof() || ( flag1!= -9999 || flag2 != -9999 ) ) {
    std::cout<<"Fatal -Problems reading first line of file" << fileName<<std::endl;
    exit(1);
  }
 
  if(debugPrint) {
    std::cout<<"flag1,flag2: "<<flag1<<" "<<flag2<<std::endl;
  }
  
  bool finishedFile=false;
  double startBinNS;



  while (1) {                    //Over templates
    double equivHiGainArea;
    double linearity;
    ifs >> equivHiGainArea >> linearity;
    if ( ifs.eof() ) {
      std::cout<<"Fatal -Unexpected eof Problems reading first line of file" 
			   << fileName<<std::endl;
      exit(1);
    }
  
    std::cout<<"equivHiGainArea,linearity:"<< equivHiGainArea <<" "<<linearity
			 <<std::endl;

    std::vector <double > pulse;
    pulse.clear();
    bool pulseInited=false;
    double oldBinTime=0;
    double time;
    double pulseHeight;
    while(1) {     //readin and save the pulse

      ifs >> time >> pulseHeight;
      if (ifs.eof() ) {
		finishedFile=true;
		break;
      }
      if ( (int) time == -9999) {
		finishedFile=false;
		break;
      }
	  // Make sure no missing bins
      if( ! pulseInited) {
		startBinNS=time;
		oldBinTime=time - kBinSpacingNS;
        pulseInited=true;
      }
      
      if( std::fabs((float)(time-(oldBinTime+kBinSpacingNS)) )>
		  (float) kBinSpacingNS/1000. ) {
		std::cout<<"Missing bin time: "<<oldBinTime+kBinSpacingNS<<std::endl;
      }
      
      if(debugPrint) {
		std::cout<<"time,pulseHeight: "<< time << " "<< pulseHeight<<std::endl;
      }
      
      pulse.push_back(pulseHeight);
      oldBinTime=time;
    }
    if(debugPrint) {
      std::cout<<"startBinNS,Sum: "<< startBinNS << " "<< sumPulse(pulse)
			   <<std::endl;
    }
    lowGainTemplates.push_back(pulse);
    highGainAreaDC.push_back(equivHiGainArea);
    lowGainLinearity.push_back(linearity);
    lowGainStartBinNS.push_back(startBinNS);

    if (finishedFile) {
      break;
    }
  }
  std::cout<<"Number of Templates:"<<lowGainTemplates.size()<<std::endl;
  return;
}
// ***************************************************************************

int Round(double number)
{
  return (number >= 0) ? (int)(number + 0.5) : (int)(number - 0.5);
}
// ***************************************************************************

void CorrectAndWritePulses(std::ofstream& ofs, bool highGain,bool upgrade)
{
  // **********************************************************************
  // Going to use the tail of template 28 to fill out any missing tails in 
  // the other pulses
  // Gather some stuff about template 28
  // ***********************************************************************
  bool debugPrint=false;
  //bool debugPrint=true;

  int goodTailIndex;
  double pulseMaxGT;
  double pulseMinGT;
  bool addTail=false;
  std::vector< double >::iterator it;   //for pulse array
  int numWaveForms=waveForms.size();

  if (! highGain) {
    addTail=true;
	goodTailIndex=28;
	if(	goodTailIndex > numWaveForms-1 ) {
	  std::cout<< " For tail need at least " << goodTailIndex+1 
			   << "templates. Only have " << numWaveForms << std::endl;
	  exit(1);
	}

    // Find the min and max elements in the vector
    it = min_element(waveForms.at(goodTailIndex).begin(), 
					 waveForms.at(goodTailIndex).end());
    pulseMinGT = *it;
    it = max_element(waveForms.at(goodTailIndex).begin(), 
					 waveForms.at(goodTailIndex).end());
    pulseMaxGT = *it;
  }
  // ***********************************************************************


  int numWaveForms=waveForms.size();
  if( debugPrint) {
    std::cout<<"Num templates:           " << numWaveForms << std::endl;
	std::cout<<"Num highGainAmplitudeMV: " << highGainAmplitudeMV.size() 
			 << std::endl;
	std::cout<<"Num lowGainLinearity:    " << lowGainLinearity.size() 
			 << std::endl;
	std::cout<<"Num startBinsNS:         " << startBinsNS.size() 
			 << std::endl;
  	std::cout<<"goodTailIndex:           " << goodTailIndex 
			 << std::endl;}
    
  for(int i=0; i<numWaveForms; i++) {
    ofs << "-9999 -9999"<<std::endl;
    ofs << highGainAmplitudeMV.at(i)<<" "<<lowGainLinearity.at(i)<<std::endl;
    //ofs << highGainAmplitudeDC.at(i)<<" "<<lowGainLinearity.at(i)<<std::endl;
    //ofs << highGainAreaDC.at(i)     <<" "<<lowGainLinearity.at(i)<<std::endl;

    // Find the min and max elements in the vector, and index to max
    it = min_element(waveForms.at(i).begin(), waveForms.at(i).end());
    double pulseMin = *it;
    it = max_element(waveForms.at(i).begin(), waveForms.at(i).end());
    double pulseMax = *it;
	
	// *********************************************************************
	// Reset template offset to put 20% point of all pulses at 4 ns.
	// Find peak (max), back down to 20% point
	// *********************************************************************
	int jk = 0;
	for( jk = 0 ; jk < waveForms.at(i).size(); jk++) {
	  if( -(waveForms.at(i).at(jk)-pulseMax) >= .2 * (-pulseMin) ) {
		//Put 20% point it at 4ns
		templateOffsets.at(i)= k20PercentPositionNS - 
		  (startBinsNS.at(i) + jk * kBinSpacingNS);
		break;
	  }
	}
	
    if( debugPrint) {
	  std::cout << " template #"<<i<<" Min/max,startBinsNS,templateOffsetNS: " 
				<< pulseMin << "/" << pulseMax << " " << startBinsNS.at(i)
				<< " "  << jk << "  " << templateOffsets.at(i) << std::endl;
	}

    int startBinCenter = startBinsNS.at(i);
    int lastBinCenterIndex=
      ( startBinCenter+templateOffsets.at(i))/kBinSpacingNS-1;
    if  ( lastBinCenterIndex >=0){
      // *****************************************************************
      // Set missing starting bins to 0
      // *****************************************************************
      for (int j=0;j<=lastBinCenterIndex;j++) {
		double binCenterNS=j*kBinSpacingNS;
		ofs << binCenterNS << " " << 0.0 <<std::endl;
      }
	  if( debugPrint) {
		std::cout<<"Front bins of Template set to 0 to bin"
				 <<lastBinCenterIndex*kBinSpacingNS << std::endl;
	  }
    }
  

    double time=0;
    for(int j=0; j<(int)waveForms.at(i).size(); j++) {
      // ******************************************************************
      // normalize and remove pedestal. (note we invert it to a positive 
      // pulse also)
      // ******************************************************************
      double correctedPulse= ((waveForms.at(i).at( j )-pulseMax))/ 
		(pulseMin-pulseMax);
      time= startBinCenter + templateOffsets.at( i ) + 
		j * kBinSpacingNS;
      
      if(time >=0 && time < (kLengthOfTemplateNS + .1)  ) { //Limit length
		ofs << time << " " << correctedPulse <<std::endl;
      }
    }
	if( debugPrint) {
	  std::cout<<"Template pedestal removed and template normalized"
			   <<std::endl;
	}

	//Fill in tail  if last bin <  kLengthOfTemplateNS and this is low gain
    if( addTail) {
      if (time< kLengthOfTemplateNS + .1 * kBinSpacingNS ) {
		time = time + kBinSpacingNS;
		while ( (time / kBinSpacingNS)-1  < 
				                        waveForms.at(goodTailIndex).size() ) {
		  int index = (time / kBinSpacingNS)-1;
		  double correctedPulse= 
			(waveForms.at(goodTailIndex).at( index ) - pulseMaxGT )  / 
			( pulseMinGT - pulseMaxGT );
		  ofs << time << " " << correctedPulse <<std::endl;
		  time = time + kBinSpacingNS;
		}
      }
    
	}
	if( debugPrint) {
	  std::cout<<"Template corrected and written"<<std::endl;
	}
  }
  return;
}
// *************************************************************************


void GenKASCADETemplateFile(std::string inFileName, std::string outFileName,
							bool upgrade = true, bool highgain = false)
{
  // ***********************************************************************
  // Read in a nepomuk/hugh/cameron type template txt and convert to a .25 ns
  // step text file for the various pulses.
  // ***********************************************************************
  //Input file format:
  // pulse starts with "*" on a line(Except for first pulse)
  // second line: peak intensity of equivalent high gain pulse in mv,
  //             linearity (adjustment to 10.101 (1/.099) for saturation
  // remianing lines: time(ns)(in even or random small <=0.25ns steps),
  //                   pulse height
  // Then next pulse starts  (Flag of start is the "*" record)
  // ************************************************************************
  // Output file, sutable for reading in with 
  //                        KSSinglePe::readLowGainTemplateFile()
  // pulse starts with "-9999 -9999" on a line
  // second line: equivalent High Gain Pulse area in dc, linearity
  // remaining lines:  time in .25 ns steps, pulse height
  // Then next pulse starts. (Flag of start is the "9999 -9999" record)
  // *************************************************************************
  // Output pulses are normalized to max height == 1.0
  // Output values at each .25 ns step are average over any input pulse 
  // bins +/- 0.125ns around that time.
  // *************************************************************************
  
  //bool debugPrint=true;
  bool debugPrint=false;

  waveForms.clear();   //just to make enough room
  highGainAmplitudeDC.clear();
  highGainAmplitudeMV.clear();
  highGainAreaDC.clear();
  startBinsNS.clear();

  double dcToPeGain;
  templateOffsets.clear();
  templateOffsets.resize(kMaxNumTemplates);
  if (upgrade) {
    dcToPeGain= kUpgradeGainN;
  }
  else{           //Photonis
    dcToPeGain= kPhotonisGainG;
  }

  // ***************************************************
  //Open the input file and create the output file
  // ***************************************************
  std::ifstream ifs(inFileName.c_str());
  if (! ifs) {
    std::cout<<" Fatal- Can not open "<<inFileName<<std::endl;
    exit(1);
  }

  std::ofstream ofs(outFileName.c_str());
  if (! ofs) {
    std::cout<<" Fatal- Can not open "<<outFileName<<std::endl;
    exit(1);
  }
  std::cout<<" Input and Ouput files sucessfully opened"<<std::endl;
  

 
  // *****************************************************************
  // Start loop over templates
  // *****************************************************************
  bool finishedFile=false;
  std::string line;
  while(1) {
    //Read first line of next template
    double hiGainAmplitudeMV;
    double linearity;
    std::getline(ifs,line);
    

    //   ifs >> hiGainAmplitudeMV >> linearity;
    if (ifs.eof() ) {
      break;   //and we are done!
    }
    istringstream is(line);
    is >> hiGainAmplitudeMV;
    is >> linearity;
    
    // ******************************************************************
    // Convert hiGainAmplitudeMV to equivalent High gain area in DC
    // ******************************************************************
    double hiGainAmplitude = hiGainAmplitudeMV / kMVToDCN; 
    double hiGainPE        = hiGainAmplitude /  kUpgradeGainN;
    double hiGainArea      = hiGainPE * kPEToDCAreaG;

    std::cout << hiGainAmplitudeMV << " " << linearity << std::endl;

    std::vector <double> pulse;   //used to fix min and peak
    pulse.clear();

    // *********************************************************
    // Now loop through all the input pulse values. Use the first one to set 
    // our first output time (On the even .25 ns step
    // *********************************************************
    int ibin=0;
    double time;
    std::string flag1;
    std::string line;
    double value;
    double sum=0;
    int sumCount=0;
    bool finishedPulse=false;
    double oldBinCenterIndex=0;
    double oldSumAverage;
    double startBinCenterNS=0;
    // ******************************************************
    // This outer loop is over output bins
    // ******************************************************
    int binCenterIndex=0;
    while(1) {
      double binCenterNS;
      // **************************************************************
      //This is loop to produce one output bin
      // *************************************************************
	  while (1){
		std::getline(ifs,line); 
		if (ifs.eof() ) {

		  //CorrectAndWritePulse(ofs, pulse, startBinCenterNS, templateIndex);
		  highGainAmplitudeMV.push_back(hiGainAmplitudeMV);
		  highGainAmplitudeDC.push_back(hiGainAmplitude);
		  highGainAreaDC.push_back(hiGainArea);
		  lowGainLinearity.push_back(linearity);
		  startBinsNS.push_back(startBinCenterNS);
		  waveForms.push_back(pulse);
		  finishedFile=true;   //and we are done!
		  std::cout<<" Finished reading in input file. Found "
				   << waveForms.size() << " waveforms." << std::endl;
		  break;
		}

		istringstream iss(line);
		iss >> flag1;
		
		if ( flag1 == "*" ) {
		  //CorrectAndWritePulse(ofs, pulse, startBinCenterNS, templateIndex);
		  highGainAmplitudeMV.push_back(hiGainAmplitudeMV);
		  highGainAmplitudeDC.push_back(hiGainAmplitude);
		  highGainAreaDC.push_back(hiGainArea);
		  lowGainLinearity.push_back(linearity);
		  startBinsNS.push_back(startBinCenterNS);
		  waveForms.push_back(pulse);
		  //std::cout<<"finished template with a *"<<std::endl;
		  
		  //ofs << (int)time << " " << (int)value <<std::endl; //-9999 -9999
		  finishedFile=false;
		  finishedPulse=true;
		  break;           // done with this pulse, go on to the next one
		}
		istringstream isss(flag1);
		isss >> time;
		
		iss >> value;
		
		// ***************************************
		// Use the first one to set 
		// our first output time (On the even .25 ns step
		// *************************************
		if (ibin == 0 ) {  //we are at the first bin in this pulse
		  // init the time,find the offset
		  binCenterIndex = Round(time/kBinSpacingNS);
		  oldBinCenterIndex=binCenterIndex-1;
		  binCenterNS=binCenterIndex*kBinSpacingNS;
		  startBinCenterNS=binCenterNS;
		  ibin=1;
		}
		
		int nextBinCenterIndex=Round(time/kBinSpacingNS);
		if(debugPrint) {
		  std::cout<<"time,kBinSpacingNS,nextBinCenterIndex,binCenterIndex,"
			"oldBinCenterIndex,: "
				   <<time << " " << kBinSpacingNS << " " << nextBinCenterIndex 
				   << " " << binCenterIndex << " "<<oldBinCenterIndex
				   <<std::endl;
		}
		
		if (nextBinCenterIndex==binCenterIndex) {
		  sum=sum+value;
		  sumCount++;
		  if(debugPrint) {
			std::cout<<" binCenterIndex,nextBinCenterIndex,"
			  "sumCount,time,value,sum: "
					 <<binCenterIndex<<" "<<nextBinCenterIndex
					 <<" "<<sumCount<<" "<<time<<" "<<value<<" "<<sum
					 <<std::endl;
		  }
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
	    
			if(debugPrint) {
			  std::cout<< "weight,binCenterIndex,oldBinCenterIndex: " << weight
					   << " "
					   << binCenterIndex<< " " << oldBinCenterIndex 
					   << std::endl;
			  std::cout<< "interpolatedSumAverage,sumAverage,oldSumAverage:"
					   << interpolatedSumAverage << " " << sumAverage << " "
					   << oldSumAverage << std::endl;
			}
	  
			oldBinCenterIndex++;
			oldSumAverage = interpolatedSumAverage;
			pulse.push_back(oldSumAverage);
	  
			if(debugPrint) {
			  double oldBinCenter  = oldBinCenterIndex*kBinSpacingNS;
			  std::cout<<"BinCenter at "
					   <<oldBinCenter + templateOffsets.at(0)
					   <<" interpolated"<<std::endl;
			  std::cout<<"oldBinCenter,oldSumAverage: "<<oldBinCenter<<" "
					   <<oldSumAverage<<std::endl;
			}
	  
		  }
	
		  pulse.push_back(sumAverage);
			  
		  oldSumAverage=sumAverage;
		  oldBinCenterIndex=binCenterIndex;
	
		  //and setup for the nect bin
		  sum=value;
		  sumCount=1;
		  binCenterIndex=nextBinCenterIndex;
		  binCenterNS=binCenterIndex*kBinSpacingNS;
	
		  ibin++;
	
		  if(debugPrint) {
			std::cout<<"binCenterNS,sumAverage: "
					 <<binCenterNS + templateOffsets.at(0)<<" "
					 <<sumAverage<<std::endl;
			std::cout<<" binCenterIndex,sumCount,time,value,sum: "
					 <<binCenterIndex <<" "<<sumCount<<" "<<time<<" "<<value
					 <<" "<<sum<<std::endl;
		  }
	
		  break;
		}
      }
      if ( finishedFile || finishedPulse ) {   //Are we into the next pulse?
		//exit(1);
		break;
      }
    }
    if ( finishedFile ) {   //Are we into the next pulse?
	  std::cout<<" CorrectAndWritePulses"<<std::endl;
	  CorrectAndWritePulses(ofs,highgain,upgrade);
      break;
    }
  }
}
// **************************************************************************


void LoadLowGainTemplateHists ()
{
  // *****************************
  // First define all the histograms. Use the  highGainAreaDC values in the 
  // names
  // ****************************


  int numTemplates = lowGainTemplates.size();
  std::cout<<"numTemplates: "<<numTemplates<<std::endl;
  int numtemplateHists=0;
  //int numTemplates=1;
  templateHists.resize(numTemplates);
  for (int i=0; i<numTemplates;i++) {
	std::ostringstream os;
	os<<"template"<<(int)highGainAreaDC.at(i)<<"_"<<i;
	std::string tmpltName=os.str();
	TH1D tmplt(tmpltName.c_str(), tmpltName.c_str(),256, 
			   lowGainStartBinNS.at(i), 
			   lowGainStartBinNS.at(i) + kLengthOfTemplateNS );
	//and fill it
	int numPoints=lowGainTemplates.at(i).size();
    
	if(numPoints>256) {
	  numPoints=256;
	}
	double sum=0;
	for (int j=1;j<=numPoints;j++) {
       
	  double value=lowGainTemplates.at(i).at(j-1);
	  // if(j-1<160) {
	  sum += value;
	  //std::cout<<"j,value: "<<j<<" "<<value<<std::endl;
	  tmplt.SetBinContent(j,value);
	  //}
	}
	templateHists.at(i)=tmplt;
	std::cout<<"i,numTemplates,tmpltName: "<<i<<" "<<numTemplates<<" "
			 <<tmpltName<<" "<<lowGainTemplates.at(i).size()
			 <<" "<<numPoints
			 <<" "<<lowGainStartBinNS.at(i)<<" "<<sum<<std::endl;
  }
  return;
}
// **********************************************************

void PlotTemplateHists(string opt="Lsame",int hStart=0, int hEnd=28)
{
  bool debugPrint=false;
  int numHists=hEnd-hStart+1;
  //int numHists=5;
  int color=2;
  for(int i=hEnd; i>=hStart;i--) {
    if(debugPrint) {
      std::cout<<"size:"<<templateHists.size()<<std::endl;
    }
    if (i==hEnd) {
	  templateHists.at(i).Draw("l");
    }
    else{
      templateHists.at(i).SetLineColor(color);
      color++;
      if(color==5) {
		color=6;
      }
      else{
		if(color==7) {
		  color=1;
		}
      }
      templateHists.at(i).Draw(opt.c_str());
    }
  }
}
// *******************************************************************
  
void PlotTemplateHists(int histIndex, string opt="L")
{
  templateHists.at(histIndex).SetLineWidth(2.0);
  if ( opt == "L" ) {
	templateHists.at(histIndex).SetLineColor(2);
  }
  else{
	templateHists.at(histIndex).SetLineColor(3);
  }
  templateHists.at(histIndex).Draw(opt.c_str());
}  
// ********************************************************************

void LoadHighGainTemplateHist (std::string PMTType="Upgrade")
{
  // *****************************
  // First define the histogram. 
  // ****************************
  TH1D tmplt(PMTType.c_str(), PMTType.c_str(),256, 
			 lowGainStartBinNS.at(0), 
			 lowGainStartBinNS.at(0) + kLengthOfTemplateNS );
  //and fill it
  int numPoints=highGainTemplate.size();
    
  if(numPoints>256) {
    numPoints=256;
  }
  double sum=0;
  for (int j=1;j<=numPoints;j++) {
       
    double value=highGainTemplate.at(j-1);
	if(j-1<160) {
	  sum += value;
	  tmplt.SetBinContent(j,value);
	}
  }
  highGainTemplateHists.push_back(tmplt);
  std::cout<<"tmpltName,Num high gain hists,sum: "
		   <<PMTType<<" "<<highGainTemplateHists.size()<<" "<<numPoints
		   <<" "<<sum<<std::endl;
  return;
}
// **********************************************************

void PlotHighGainTemplates(string opt="Lsame")
{
  int numHists=highGainTemplateHists.size();
  //int numHists=5;
  int color=2;
  for(int i=numHists-1; i>=0;i--) {
    std::cout<<"size:"<<highGainTemplateHists.size()<<std::endl;
    if (i==numHists-1) {
	  highGainTemplateHists.at(i).Draw("l");
    }
    else{
      highGainTemplateHists.at(i).SetLineColor(color);
      color++;
      if(color==5) {
		color=6;
      }
      else{
		if(color==7) {
		  color=1;
		}
      }
      highGainTemplateHists.at(i).Draw(opt.c_str());
    }
  }
}
// **********************************************************************
  
void PlotHighGainTemplateHist(int histIndex, string opt="L")
{
  highGainTemplateHists.at(histIndex).Draw(opt.c_str());
}  
// **************************************************************************

bool ReadInSimTraces(string KSAomegaLogFileName)
// ***********************************************************************
// Read in from the KASCADE ksSAomega Log file which had the fDepugPrint in 
// ksAomega KSFADC.cpp enabled, the debug lowgain trace lines. Place into a 
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
  TFile KSAomegaTraces("KSAomegaTraces.root","RECREATE");
  TTree traceTree;
  traceTree.Branch("NumPes",&fNumPes,"n/I");
  traceTree.Branch("HighSize",&fHighSize,"hiSize/F");
  traceTree.Branch("TemplateIndex",&fTemplateIndex,"i/I");
  traceTree.Branch("Linearity",&fLinearity,"L/f");
  traceTree.Branch("LGTrace",pLGTrace,"trace[16]/I");

  
  // ***************************************************
  //Open the input file and create the output file
  // ***************************************************
  std::ifstream ifs(KSAomegaLogFileName.c_str());
  if (! ifs) {
    std::cout<<" Fatal- Can not open "<<KSAomegaLogFileName<<std::endl;
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
	std::getline(ifs,line);
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
	  for ( int j = 0; j < 16; j++) {
		is >> pLGTrace[j];
	  	if (ifs.eof() ) {
		  std::cout << " Error reading in ksAomageaLog file" << std::endl;
		  exit(1);
		}
	  }

	  // Add to TTree
	  traceTree.Fill();
	}
  }
  traceTree.Write();
  return true;
}
// ***********************************************************************

void InitVegasLowGainAnalysis(string LowGainTemplateROOTFileName,
							  string LowGainTransFuncROOTFileName)
{
  pfLowGainAnalyser = new VALowGainTraceTemplateFitAnalysis(
															LowGainTemplateROOTFileName,LowGainTransFuncROOTFileName);
}
// ***********************************************************************

float GetCharge(std::vector<float> const & samples)
{
  // *******************************************************************
  // This is the guts of the VALowGainTraceTemplateFitAnalysis::getCharge()
  // routine that doesn't use any of the arguments of that routine but 
  // "samples"
  // *******************************************************************
  // Set up default values that arn't used anywhere
  uint16_t telID=0;
  uint16_t chanID=0;
  
  //Needs: sADCSamplingPeriod (should be 2ns I think
  //       sHiLoGainRatio=6.0? 5.8? 5.2? 4.95?

  // construct vector of sample times
  std::vector<float> sampleTimes;
  for(int iSample = 0; iSample < samples.size(); ++iSample) {
	sampleTimes.push_back(iSample*sADCSamplingPeriod);
  }

  // encapsulate the trace data and metadata
  VALowGainTraceLookupDatum channelTraceDatum(telID, chanID, sampleTimes, 
											  samples);
  // set name and title appropriately
  std::stringstream traceName;
  std::stringstream traceTitle;
  traceName << "obsLowGainTrace_t" << telID << "_c" << chanID;
  traceTitle << "Observed Low Gain Trace for telescope " << telID 
			 << ", channel " << chanID;
  channelTraceDatum.SetNameTitle(traceName.str().c_str(), 
								 traceTitle.str().c_str());
    
  // The template fitting minimizer returns an integer status flag
  int fitMinimizerStatus(0);
  // The fit statistic returned by the template fitting algorithm
  float fitStat(-1.0);
  // invoke the trace fitting algorithm
  
  float charge(0.0);
  hiLoRatio = sHiLoGainRatio;
 
  bool foundMatchingTemplate = 
	fLowGainTraceAnalysis->deriveBestFitTemplate( channelTraceDatum, 
												  fitMinimizerStatus, fitStat);
  // retrieve the data and metadata that correspond to the best-fitting 
  // trace template.
  if(foundMatchingTemplate ) {
	VALowGainTraceLookupDatum const & bestMatchTrace = 
	  fLowGainTraceAnalysis->pfLowGainTraceFitter->getBestResult().mTemplateTrace;


	// integrate the trace to obtain an equivalent high-gain trace.
	bool gotLGCharge = 
	  fLowGainTraceAnalysis->computeLowGainCharge(charge, bestMatchTrace);
	if( gotLGCharge) {
	  return charge*hiLoRatio;
	}
  }
  return charge;
}
// *************************************************************************

void LowGainChageConversionValidation(string KSAomegaLogFileName,
									  string LowGainTemplateROOTFileName,
									  string LowGainTransFuncROOTFileName)
// **********************************************************************
// Tests of VEGAS LowGain charge calculation 
// **********************************************************************
// Read in the traces from the KASCADE geenerated LowGain Traces file
// Using VEGAS/ Logain conversion generate expected trace charge
// Make diagnostic plots of actual charge(hg) vs VEGAS generated charge
// **********************************************************************
{
  initVegasLowGainAnalysis(LowGainTemplateROOTFileName,
						   LowGainTransFuncROOTFileName);
  readInSimTraces(KSAomegaLogFileName);
  int numTraces = simTracesTree.getEntries(); 

  for (int i=0; i<numTraces; i++){
	simTracesTree.getEntry(i);
	std::vector<float> const  samples;
	for (int k=0; k < 16; k++) {
	  samples.push_back(pLGTrace[k]);
	}

	// This needs: sADCSamplingPeriod
	//             sHiLoGainRatio    
	double equivCharge=GetCharge

	  cout << i << ": " << fNumPes << " " << fTemplate << " " << fHighSize 
		   << " " << equivHGSize<< std::endl;
  }
  return;
}
// *************************************************************************

//CHECK on sADCSamplingPeriod,sHiLoGainRatio, low gain pedestal subtraction!!!!
