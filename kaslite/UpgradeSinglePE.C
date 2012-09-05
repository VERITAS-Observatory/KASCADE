//#include <KSSinglePe.h>

void readPulseDatFile(string inFileName, vector<double>& pulse, 
		 vector<double>& timeNS)
{
  double channelTimeNS;
  double amplitude;

  pulse.clear();
  timeNS.clear();


  ifstream in(inFileName.c_str());
  for(int i=0;;i++){
    in>>channelTimeNS>>amplitude;
    if(in.eof()){
      return;
    }
    pulse.push_back(amplitude);//invert
    timeNS.push_back(channelTimeNS);
    cout<<timeNS.at(i)<<" "<<pulse.at(i)<<endl;
  }
}

void convertPulseDatTo25NSSteps(vector<double>&pulse2, vector<double>&timeNS2,
				 vector<double>&pulse25,vector<double>&timeNS25)
{
  //Simple interpolate
  if(timeNS2.at(0)>0){
    return;  //nothing here yetassume always time start <0;
  }

  int numStart=-timeNS2.at(0)/.25;
  double timeStartNS=-numStart*.25;

  cout<<"timeNS2Start,timeNS25Start: "<<timeNS2.at(0)<<" "<<timeStartNS
      <<endl;

  int numBins=timeNS2.size();

  for(int i=0;;i++){
    double timeNowNS=timeStartNS+i*.25;
    if(timeNowNS>timeNS2.at(numBins-1)){
      return;
    }
    
    timeNS25.push_back(timeNowNS);
    int time2BinLow=i*.25/.2;
    if(timeNS2.at(time2BinLow)==timeNowNS){
      pulse25.push_back(pulse2.at(time2BinLow));
    }
    else{
      int time2BinHi=time2BinLow+1;
      double ratio=(timeNowNS-timeNS2.at(time2BinLow))/.2;
      double pulseNow=pulse2.at(time2BinLow)+
	                ratio*(pulse2.at(time2BinHi)-pulse2.at(time2BinLow)); 
      
      pulse25.push_back(pulseNow);
    }
  }
  return;
}


void ConditionUpgradeSingelPe(string inFileName,string outFileName)
{
  //Simple interpolate



  vector<double> pulse25;
  vector<double> timeNS25;

  vector<double> pulse2;
  vector<double> timeNS2;

  int numBins=0;
  cout<<"numBins.20: "<<numBins<<endl;

  readPulseDatFile(inFileName, pulse2, timeNS2);

  convertPulseDatTo25NSSteps(pulse2, timeNS2,pulse25,timeNS25);


  numBins=pulse25.size();

  cout<<"numBins.25: "<<numBins<<endl;

  double maxAmplitude=0;
  for(int i=0;i<numBins;i++){
    cout<<timeNS25.at(i)<<" "<<pulse25.at(i)<<endl;
    if(-pulse25.at(i)>maxAmplitude){
      maxAmplitude=-pulse25.at(i);
    }
  }
  cout<<endl<<"**********************"<<endl;
  cout<<"maxAmplitude: "<<maxAmplitude<<endl;
  cout<<"**********************"<<endl<<endl;;

  ofstream out(outFileName.c_str());

  //pulse25 is inverted.
  for(int i=0;i< numBins;i++){
    pulse25.at(i)=-pulse25.at(i)/maxAmplitude;
    cout<<timeNS25.at(i)<<" "<<pulse25.at(i)<<endl;
    out<<timeNS25.at(i)<<" "<<pulse25.at(i)<<endl;
  }
  return;
}

void AddTTreeHeader(string inFileName, string outFileName)
{
  ofstream out(outFileName.c_str());
  out<<"time/F:Amplitude"<<endl;

  ifstream in(inFileName.c_str());
  double channelTimeNS;
  double amplitude;
  for(int i=0;;i++){
    in>>channelTimeNS>>amplitude;
    if(in.eof()){
      return;
    }
    
    out<<channelTimeNS<<" "<<amplitude<<endl;
  }
}

 
void ConditionDatPulse(string inFileName, string outFileName, 
		       double shiftRightNS=0, double startNS=-10000,double endNS=10000)
{
  ofstream out(outFileName.c_str());
  ifstream in(inFileName.c_str());
  double channelTimeNS;
  double amplitude;
  for(int i=0;;i++){
    in>>channelTimeNS>>amplitude;
    if(in.eof()){
      return;
    }
    
    channelTimeNS=channelTimeNS+shiftRightNS;

    if(channelTimeNS>=startNS && channelTimeNS<=endNS){
      out<<channelTimeNS<<" "<<amplitude<<endl;
    }
  }
}

void GenerateSinglePEPulseDatFile(string outFileName, double rise=1.7,
				  double fall=4.75)
{
  KSSinglePe* pStdPmtPulse=new KSSinglePe(rise,fall);
  // ******************************************************************
  // Write it out
  // ******************************************************************
  double fSinglePeSizeNS      =pStdPmtPulse->getLengthNS();
  int    fSinglePeSizeNumBins =pStdPmtPulse->fNumBinsInPulse;
  cout<<"SinglePeSizeNS,SinglePeSizeNumBins,SinglePeArea: "<<fSinglePeSizeNS
      <<" "<<fSinglePeSizeNumBins<<endl; 
  ofstream out(outFileName.c_str());

  for(int i=0;i<fSinglePeSizeNumBins;i++)
    {
      double amplitude=pStdPmtPulse->pfSinglePulse.at(i);
      double time = i*gWaveFormBinSizeNS;
      out <<time<<" "<<amplitude<<endl;
    }
  return;
}
// ***********************************************************************

void GenerateBasePulseTextFromDat(string inFileName)
{
  
  ifstream in(inFileName.c_str());
  double channelTimeNS;
  double amplitude;
  cout<<"const static double kBasePulse[kBaseSize]="<<endl;
  cout<<"  {"<<endl;
  for(int i=0;;i++){
    in>>channelTimeNS>>amplitude;
    if(in.eof()){
      i--;
      break;
    }
    if(i%8==0&&i!=0){
      cout<<endl;
      cout<<"   ";
    }
    else if(i==0){
      cout<<"   ";
    }

    cout<<std::setw(8)<<setprecision(3)<<amplitude<<",";
  }
  cout<<"  }"<<endl;
  return;
}
