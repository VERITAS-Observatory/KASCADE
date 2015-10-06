//Root script
const double kMVToDCN      = 7.84;           //Nepomuks values
const double kUpgradeGainN = 2.33;           //Nepomuks values
const double kPhotonisGainN = 1.3;           //Glenns values
const double kPEToDCAreaG  = 5.62;            //Glenns value
const double kBinSpacing   = 0.25;


#include <vector>
#include <iostream>
#include <string>
#include <fstream>
#include <algorithm>
#include <iterator>
#include <cstdlib>
#include <sstream>

#include <TH1D.h>


std::vector < std::vector < double > > lowGainTemplates;
std::vector < double > highGainTemplate;
std::vector < double > highGainAreaDC;
std::vector < double > highGainAmplitudeDC;
std::vector < double > highGainAmplitudeMV;
std::vector < double > lowGainLinearity;
std::vector < double > lowGainStartBinNS;
std::vector < int>     startBin;
std::vector < TH1D >   templateHists;
std::vector < TH1D >   highGainTemplateHists;
std::vector < double > templateFWHM;
std::vector < double > templateOffsets;
std::vector < std::vector <double > > waveForms;

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


void  ReadHighGainTemplate(std::string fileName)
{
  
  //bool debugPrint=false;
  bool debugPrint=true;


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
	oldBinTime=time - kBinSpacing;
        pulseInited=true;
      }
      
      if( fabs((float)(time-(oldBinTime+kBinSpacing)) )>
	                                         (float) kBinSpacing/1000. ) {
	std::cout<<"Missing bin time: "<<oldBinTime+kBinSpacing<<std::endl;
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

void  ReadLowGainTemplates(std::string fileName)
{
  
  bool debugPrint=false;


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
	oldBinTime=time - kBinSpacing;
        pulseInited=true;
      }
      
      if( fabs((float)(time-(oldBinTime+kBinSpacing)) )>
	                                         (float) kBinSpacing/1000. ) {
	std::cout<<"Missing bin time: "<<oldBinTime+kBinSpacing<<std::endl;
      }
      
      if(debugPrint) {
	std::cout<<"time,pulseHeight: "<< time << " "<< pulseHeight<<std::endl;
      }
      
      pulse.push_back(pulseHeight);
      oldBinTime=time;
    }
    std::cout<<"Sum: "<< sumPulse(pulse)<<std::endl;

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

void CorrectAndWritePulses(std::ofstream& ofs, bool addTail)
{
  // **********************************************************************
  // Going to use the tail of template 28 to fill out any missing tails in 
  // the other pulses
  // Gather some sdtuff about template 28
  // ***********************************************************************
  //bool debugPrint=false;
  bool debugPrint=true;

  int goodTailIndex=28;
  double pulseMaxGT;
  double pulseMinGT;
  if ( addTail ) {
    std::vector< double >::iterator it;   //for pulse array
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
    std::cout<<"Num templates: "<< numWaveForms<<endl;
  }
    
  for(int i=0; i<numWaveForms; i++) {
    ofs << "-9999 -9999"<<std::endl;
    ofs << highGainAmplitudeMV.at(i)<<" "<<lowGainLinearity.at(i)<<std::endl;
    //ofs << highGainAmplitudeDC.at(i)<<" "<<lowGainLinearity.at(i)<<std::endl;
    //ofs << highGainAreaDC.at(i)     <<" "<<lowGainLinearity.at(i)<<std::endl;

    // Find the min and max elements in the vector
    it = min_element(waveForms.at(i).begin(), waveForms.at(i).end());
    double pulseMin = *it;
    it = max_element(waveForms.at(i).begin(), waveForms.at(i).end());
    double pulseMax = *it;
    if( debugPrint) {
      std::cout << " template #"<<i<<" Min/max are: " << pulseMin << " / " 
		<< pulseMax << std::endl;
    }
    int startBinCenter = startBin.at(i);
    int lastBinCenterIndex=
      ( startBinCenter+templateOffsets.at(i))/kBinSpacing-1;
    if  ( lastBinCenterIndex >=0){
      // *****************************************************************
      // Set missing starting bins to 0
      // *****************************************************************
      for (int j=0;j<=lastBinCenterIndex;j++) {
	double binCenter=j*kBinSpacing;
	ofs << binCenter << " " << 0.0 <<std::endl;
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
	j * kBinSpacing;
      
      if(time >=0 && time < 64.1 ) { //Limit to 64 ns long
	ofs << time << " " << correctedPulse <<std::endl;
      }
    }
    //Fill in tail  if last bin < 64 ns  and this is low gain
    if( addTail) {
      if (time<64.0-.1*kBinSpacing) {
	time = time + kBinSpacing;
	while ( time < 64+.1 * kBinSpacing ) {
	  int index = time / kBinSpacing-1;
	  double correctedPulse= 
	    (waveForms.at(goodTailIndex).at( index ) - pulseMaxGT )  / 
	    ( pulseMinGT - pulseMaxGT );
	  ofs << time << " " << correctedPulse <<std::endl;
	  time = time + kBinSpacing;
	}
      }
    }
  }
  return;
}
// *************************************************************************


void GenUpgradeTemplateFile(std::string inFileName, std::string outFileName,
			    bool highgain = false)
{
  // ***********************************************************************
  // Read in a nepomuk/hugh/cameron type template txt and convert to a .25 ns
  // step text file for the various pulses.
  // ***********************************************************************
  //Input file format:
  // pulse starts with "-9999 -9999" on a line(I edited this in, 
  //                                  Nepomuk had a "*")
  // second line: peak intensity of equivalent high gain pulse in mv,
  //             linearity (adjustment to 10.101 (1/.099) for saturation
  // remianing lines: time(ns)(in random very small <<0.25ns steps),
  //                   pulse height
 
  // Then next pulse starts  (Flag of start is the "9999 -9999" record)
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
  startBin.clear();

  //double baseOffset=7.0;  //ns note 2 ns = 1 sample
  double baseOffset=5.0;  //ns note 2 ns = 1 sample

  templateOffsets.resize(30);
  templateOffsets.at(0)=3.5+ baseOffset;
  templateOffsets.at(1)=3.25+ baseOffset;
  templateOffsets.at(2)=2.0+ baseOffset;
  templateOffsets.at(3)=1.5+ baseOffset;
  templateOffsets.at(4)=0.5+ baseOffset;
  templateOffsets.at(5)=0.75+ baseOffset;
  templateOffsets.at(6)=1.0+ baseOffset;
  templateOffsets.at(7)=2.0+ baseOffset;
  templateOffsets.at(8)=1.25+ baseOffset;
  templateOffsets.at(9)=1.75+ baseOffset;
  templateOffsets.at(10)=2.25+ baseOffset;
  templateOffsets.at(11)=2.5+ baseOffset;
  templateOffsets.at(12)=2.25+ baseOffset;
  templateOffsets.at(13)=2.0+ baseOffset;
  templateOffsets.at(14)=2.25+ baseOffset;
  templateOffsets.at(15)=2.5+ baseOffset;
  templateOffsets.at(16)=2.25+ baseOffset;
  templateOffsets.at(17)=3.0+ baseOffset;
  templateOffsets.at(18)=3.5+ baseOffset;
  templateOffsets.at(19)=4.25+ baseOffset;
  templateOffsets.at(20)=3.0+ baseOffset;
  templateOffsets.at(21)=3.25+ baseOffset;
  templateOffsets.at(22)=3.5+ baseOffset;
  templateOffsets.at(23)=3.75+ baseOffset;
  templateOffsets.at(24)=3.5+ baseOffset;
  templateOffsets.at(25)=3.5+ baseOffset;
  templateOffsets.at(26)=3.5+ baseOffset;
  templateOffsets.at(27)=3.5+ baseOffset;
  templateOffsets.at(28)=3.5+ baseOffset;
 



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
  
  double flag1;
  double flag2;

  ifs >> flag1 >> flag2;

  if(debugPrint) {
    std::cout<<"flage1,flage2: "<<flag1<<" "<<flag2<<std::endl;
  }

  if (flag1!=-9999 || flag2 != -9999) {
    std::cout<<"Fatal-Input file "<<inFileName
	<< "failed to start with -9999 -9999 record"<<std::endl;
    exit(1);
  }


  // *****************************************************************
  // Start loop over templates
  // *****************************************************************
  bool finishedFile=false;
  while(1) {
    //Read first line of next template
    double hiGainAmplitudeMV;
    double linearity;
    ifs >> hiGainAmplitudeMV >> linearity;
    if (ifs.eof() ) {
      break;   //and we are done!
    }
    
    // ******************************************************************
    // Convert hiGainAmplitudeMV to equivalent High gain area in DC
    // ******************************************************************
    double hiGainAmplitude = hiGainAmplitudeMV / kMVToDCN; 
    double hiGainPE        = hiGainAmplitude /  kUpgradeGainN;
    double hiGainArea      = hiGainPE * kPEToDCAreaG;

    // ***********************************************
    // Save to output file
    // ***********************************************
    // ofs<<hiGainAreaDC<<" "<<lowGainLinearity<<std::endl;
    // ***********************************************
    // AMP Test
    //    ofs << hiGainAmplitudeDC << " " << lowGainLinearity << std::endl;
    // ***********************************************
    //std::cout << hiGainAmplitude << " " << linearity << std::endl;
    std::cout << hiGainAmplitudeMV << " " << linearity << std::endl;

    std::vector <double> pulse;   //used to fix min and peak
    pulse.clear();

    // *********************************************************
    // Now loop through all the input pulse values. Use the first one to set 
    // our first output time (On the even .25 ns step
    // *********************************************************
    int ibin=0;
    double time;
    double value;
    double sum=0;
    int sumCount=0;
    bool finishedPulse=false;
    double oldBinCenterIndex=0;
    double oldSumAverage;
    double startBinCenter=0;
    // ******************************************************
    // This outer loop is over output bins
    // ******************************************************
    int binCenterIndex=0;
    while(1) {
      double binCenter;
      // **************************************************************
      //This is loop to produce one output bin
      while (1){
	ifs >> time >> value;
	if(debugPrint) {
	  std::cout<<time<<" "<<value<<std::endl;
	}
	if (ifs.eof() ) {

	  //CorrectAndWritePulse(ofs, pulse, startBinCenter, templateIndex);
	  highGainAmplitudeMV.push_back(hiGainAmplitudeMV);
	  highGainAmplitudeDC.push_back(hiGainAmplitude);
	  highGainAreaDC.push_back(hiGainArea);
	  lowGainLinearity.push_back(linearity);
	  startBin.push_back(startBinCenter);
	  waveForms.push_back(pulse);
	  finishedFile=true;   //and we are done!
	  std::cout<<" Finished reading in input file. Found "
		   << waveForms.size() << " waveforms." << std::endl;
	  break;
	}
	if ( (int)time ==-9999 ) {
	  //CorrectAndWritePulse(ofs, pulse, startBinCenter, templateIndex);
	  highGainAmplitudeMV.push_back(hiGainAmplitudeMV);
	  highGainAmplitudeDC.push_back(hiGainAmplitude);
	  highGainAreaDC.push_back(hiGainArea);
	  lowGainLinearity.push_back(linearity);
	  startBin.push_back(startBinCenter);
	  waveForms.push_back(pulse);

	  //ofs << (int)time << " " << (int)value <<std::endl; //-9999 -9999
	  finishedFile=false;
	  finishedPulse=true;
	  break;           // done with this pulse, go on to the next one
	}
	
	// ***************************************
	// Use the first one to set 
	// our first output time (On the even .25 ns step
	// *************************************
	if (ibin == 0 ) {  //we are at the first bin in this pulse
	  // init the time,find the offset
	  binCenterIndex = Round(time/kBinSpacing);
	  oldBinCenterIndex=binCenterIndex-1;
	  binCenter=binCenterIndex*kBinSpacing;
	  startBinCenter=binCenter;
	  ibin=1;
	}
	
	int nextBinCenterIndex=Round(time/kBinSpacing);
	if(debugPrint) {
	  std::cout<<"time,kBinSpacing,nextBinCenterIndex,binCenterIndex,"
                     "oldBinCenterIndex,: "
		   <<time << " " << kBinSpacing << " " << nextBinCenterIndex 
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
	  	   <<" "<<sumCount<<" "<<time<<" "<<value<<" "<<sum<<std::endl;
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
	      std::cout<<"weight,binCenterIndex,oldBinCenterIndex: "<<weight
		       <<" "
		  <<binCenterIndex<<" "<<oldBinCenterIndex<<std::endl;
	      std::cout<<"interpolatedSumAverage,sumAverage,oldSumAverage:"
		  <<interpolatedSumAverage<<" "<<sumAverage<<" "<<oldSumAverage
		  <<std::endl;
	    }
	  
	    oldBinCenterIndex++;
	    oldSumAverage = interpolatedSumAverage;
	    pulse.push_back(oldSumAverage);
	  
	    if(debugPrint) {
	      double oldBinCenter  = oldBinCenterIndex*kBinSpacing;
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
	  binCenter=binCenterIndex*kBinSpacing;
	
	  ibin++;
	
	  if(debugPrint) {
	    std::cout<<"binCenter,sumAverage: "
		     <<binCenter + templateOffsets.at(0)<<" "
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
      CorrectAndWritePulses(ofs,highgain);
      break;
    }
  }
}
// **************************************************************************

void GenPhotonisTemplateFile(std::string inFileName, std::string outFileName, bool addTail=false)
{
  // ***********************************************************************
  // Read in a nepomuk/hugh/cameron type photonis template txt and convert to 
  // a .25 ns step text file for the various pulses.
  // ***********************************************************************
  //Input file format:
  // pulse starts with "-9999 -9999" on a line(I edited this in, 
  //                                  Nepomuk had a "*")
  // second line: peak intensity of equivalent high gain pulse in mv,
  //             linearity (adjustment to 10.101 (1/.099) for saturation
  // remianing lines: time(ns)(in random very small <<0.25ns steps),
  //                   pulse height
   // Then next pulse starts  (Flag of start is the "9999 -9999" record)
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
  
  bool debugPrint=true;
  //  bool debugPrint=false;

  waveForms.clear();   //just to make enough room
  highGainAmplitudeDC.clear();
  highGainAmplitudeMV.clear();
  highGainAreaDC.clear();
  startBin.clear();

  //double baseOffset=7.0;  //ns note 2 ns = 1 sample
  double baseOffset=5.0;  //ns note 2 ns = 1 sample

  templateOffsets.resize(30);
  templateOffsets.at(0)=3.5+ baseOffset;
  templateOffsets.at(1)=3.25+ baseOffset;
  templateOffsets.at(2)=2.0+ baseOffset;
  templateOffsets.at(3)=1.5+ baseOffset;
  templateOffsets.at(4)=0.5+ baseOffset;
  templateOffsets.at(5)=0.75+ baseOffset;
  templateOffsets.at(6)=1.0+ baseOffset;
  templateOffsets.at(7)=2.0+ baseOffset;
  templateOffsets.at(8)=1.25+ baseOffset;
  templateOffsets.at(9)=1.75+ baseOffset;
  templateOffsets.at(10)=2.25+ baseOffset;
  templateOffsets.at(11)=2.5+ baseOffset;
  templateOffsets.at(12)=2.25+ baseOffset;
  templateOffsets.at(13)=2.0+ baseOffset;
  templateOffsets.at(14)=2.25+ baseOffset;
  templateOffsets.at(15)=2.5+ baseOffset;
  templateOffsets.at(16)=2.25+ baseOffset;
  templateOffsets.at(17)=3.0+ baseOffset;
  templateOffsets.at(18)=3.5+ baseOffset;
  templateOffsets.at(19)=4.25+ baseOffset;
  templateOffsets.at(20)=3.0+ baseOffset;
  templateOffsets.at(21)=3.25+ baseOffset;
  templateOffsets.at(22)=3.5+ baseOffset;
  templateOffsets.at(23)=3.75+ baseOffset;
  templateOffsets.at(24)=3.5+ baseOffset;
  templateOffsets.at(25)=3.5+ baseOffset;
  templateOffsets.at(26)=3.5+ baseOffset;
  templateOffsets.at(27)=3.5+ baseOffset;
  templateOffsets.at(28)=3.5+ baseOffset;
 



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
  
  double flag1;
  double flag2;

  ifs >> flag1 >> flag2;

  if(debugPrint) {
    std::cout<<"flage1,flage2: "<<flag1<<" "<<flag2<<std::endl;
  }

  if (flag1!=-9999 || flag2 != -9999) {
    std::cout<<"Fatal-Input file "<<inFileName
	<< "failed to start with -9999 -9999 record"<<std::endl;
    exit(1);
  }

  // Save start flags to output
  //  ofs << (int)flag1 <<" "<<(int)flag2<<std::endl;

  // *****************************************************************
  // Start loop over templates
  // *****************************************************************
  bool finishedFile=false;
  while(1) {
    //Read first line of next template
    double hiGainAmplitudeMV;
    double linearity;
    ifs >> hiGainAmplitudeMV >> linearity;
    if (ifs.eof() ) {
      break;   //and we are done!
    }
    
    // ******************************************************************
    // Convert hiGainAmplitudeMV to equivalent High gain area in DC
    // ******************************************************************
    double hiGainAmplitude = hiGainAmplitudeMV / kMVToDCN; 
    double hiGainPE        = hiGainAmplitude /  kPhotonisGainN;
    double hiGainArea      = hiGainPE * kPEToDCAreaG;

    // ***********************************************
    // Save to output file
    // ***********************************************
    // ofs<<hiGainAreaDC<<" "<<lowGainLinearity<<std::endl;
    // ***********************************************
    // AMP Test
    //    ofs << hiGainAmplitudeDC << " " << lowGainLinearity << std::endl;
    // ***********************************************
    //std::cout << hiGainAmplitude << " " << linearity << std::endl;
    std::cout << hiGainAmplitudeMV << " " << linearity << std::endl;

    std::vector <double> pulse;   //used to fix min and peak
    pulse.clear();

    // *********************************************************
    // Now loop through all the input pulse values. Use the first one to set 
    // our first output time (On the even .25 ns step
    // *********************************************************
    int ibin=0;
    double time;
    double value;
    double sum=0;
    int sumCount=0;
    bool finishedPulse=false;
    double oldBinCenterIndex=0;
    double oldSumAverage;
    double startBinCenter=0;
    // ******************************************************
    // This outer loop is over output bins
    // ******************************************************
    int binCenterIndex=0;
    while(1) {
      double binCenter;
      // **************************************************************
      //This is loop to produce one output bin
      while (1){
	ifs >> time >> value;
	if(debugPrint) {
	  std::cout<<time<<" "<<value<<std::endl;
	}
	if (ifs.eof() ) {

	  //CorrectAndWritePulse(ofs, pulse, startBinCenter, templateIndex);
	  highGainAmplitudeMV.push_back(hiGainAmplitudeMV);
	  highGainAmplitudeDC.push_back(hiGainAmplitude);
	  highGainAreaDC.push_back(hiGainArea);
	  lowGainLinearity.push_back(linearity);
	  startBin.push_back(startBinCenter);
	  waveForms.push_back(pulse);
	  finishedFile=true;   //and we are done!
	  break;
	}
	if ( (int)time ==-9999 ) {
	  //CorrectAndWritePulse(ofs, pulse, startBinCenter, templateIndex);
	  highGainAmplitudeMV.push_back(hiGainAmplitudeMV);
	  highGainAmplitudeDC.push_back(hiGainAmplitude);
	  highGainAreaDC.push_back(hiGainArea);
	  lowGainLinearity.push_back(linearity);
	  startBin.push_back(startBinCenter);
	  waveForms.push_back(pulse);

	  //ofs << (int)time << " " << (int)value <<std::endl; //-9999 -9999
	  finishedFile=false;
	  finishedPulse=true;
	  break;           // done with this pulse, go on to the next one
	}
	
	// ***************************************
	// Use the first one to set 
	// our first output time (On the even .25 ns step
	// *************************************
	if (ibin == 0 ) {  //we are at the first bin in this pulse
	  // init the time,find the offset
	  binCenterIndex = Round(time/kBinSpacing);
	  oldBinCenterIndex=binCenterIndex-1;
	  binCenter=binCenterIndex*kBinSpacing;
	  startBinCenter=binCenter;
	  ibin=1;
	}
	
	int nextBinCenterIndex=Round(time/kBinSpacing);
	if(debugPrint) {
	  std::cout<<"time,kBinSpacing,nextBinCenterIndex,binCenterIndex,"
                     "oldBinCenterIndex,: "
		   <<time << " " << kBinSpacing << " " << nextBinCenterIndex 
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
	  	   <<" "<<sumCount<<" "<<time<<" "<<value<<" "<<sum<<std::endl;
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
	      std::cout<<"weight,binCenterIndex,oldBinCenterIndex: "<<weight
		       <<" "
		  <<binCenterIndex<<" "<<oldBinCenterIndex<<std::endl;
	      std::cout<<"interpolatedSumAverage,sumAverage,oldSumAverage:"
		  <<interpolatedSumAverage<<" "<<sumAverage<<" "<<oldSumAverage
		  <<std::endl;
	    }
	  
	    oldBinCenterIndex++;
	    oldSumAverage = interpolatedSumAverage;
	    pulse.push_back(oldSumAverage);
	  
	    if(debugPrint) {
	      double oldBinCenter  = oldBinCenterIndex*kBinSpacing;
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
	  binCenter=binCenterIndex*kBinSpacing;
	
	  ibin++;
	
	  if(debugPrint) {
	    std::cout<<"binCenter,sumAverage: "
		     <<binCenter + templateOffsets.at(0)<<" "
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
      CorrectAndWritePulses(ofs, addTail);
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
			 lowGainStartBinNS.at(i)+64.);
     //and fill it
     int numPoints=lowGainTemplates.at(i).size();
    
     if(numPoints>256) {
       numPoints=256;
     }
     double sum=0;
     for (int j=1;j<=numPoints;j++) {
       
       double value=lowGainTemplates.at(i).at(j-1);
       if(j-1<160) {
	 sum += value;
	 tmplt.SetBinContent(j,value);
       }
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

void PlotTemplateHists(string opt="Lsame")
{
  int numHists=templateHists.size();
  //int numHists=5;
  int color=2;
  for(int i=numHists-1; i>=0;i--) {
    std::cout<<"size:"<<templateHists.size()<<std::endl;
    if (i==numHists-1) {
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

  
void PlotTemplateHists(int histIndex, string opt="L")
{
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
			 lowGainStartBinNS.at(0)+64.);
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
  
void PlotTemplateHist(int histIndex, string opt="L")
{
  highGainTemplateHists.at(histIndex).Draw(opt.c_str());
}  
