#include <stdio.h>
#include <stdlib.h>
#include <iomanip>
#include <iostream>


void readDir(string dirname,vector<string> *names) {

    string file;
  void *dir = gSystem->OpenDirectory(gSystem->ExpandPathName(dirname.c_str()));
    const char* strptr;
    if (dir) {
        while ((strptr = gSystem->GetDirEntry(dir))) {
          file = strptr;
          //if (file=="." || file==".." || file=="log") continue;
          //names->push_back(file);
          size_t found;
          found=file.find("isf");
          if (found==string::npos) continue;
          names->push_back(file);
        }
        gSystem->FreeDirectory(dir);
    }
     // for(size_t i(0); i<names->size(); i++)
     //   Printf("%s",(*names)[i].c_str());
}


void plotPulseShapeAverage(string DataDir)
{


 //Get all the filenames
  vector<string> * filenames = new vector<string>;
  readDir(DataDir,filenames);



 TCanvas *c =new TCanvas("c","",500);
  c->Divide(2,1);
  c->cd(1);

  TH1D *h = new TH1D("h","",400,-50e-8,40e-8);
  h->SetMinimum(-0.125);
  h->SetMaximum(0.01);
  h->GetXaxis()->SetTitle("Time [s]");
  h->GetYaxis()->SetTitle("Amplitude (integral normalized to 10^{-9})");
  h->Draw();

 gPad->SetGridx();
  gPad->SetGridy();


  Double_t yAverage[1000];
  Double_t xAverage[1000];

  for(int i=0;i<1000;i++)
    {
      yAverage[i]=0;
      xAverage[i]=0;
    }

 //Loop over all files
  Int_t NumFiles = 0;
  for(size_t f(0); f<filenames->size(); f++)
    {

      stringstream fnamestream;
      fnamestream<<DataDir<<"/"<<(*filenames)[f].c_str();
      string fstring(fnamestream.str());
      cout<<"opening file: "<<fstring<<endl;


      ifstream *PulseShapefile = new ifstream;
      PulseShapefile->open(fstring.c_str());
      if(!PulseShapefile){
	cout<<"could not open file with the sample Pulse Shape"<<endl;
	continue;
      }
 
  TGraph *grSamplePulseShape = new TGraph();
  int point = 0;

  int minpoint =0;
  Float_t minvalue =1e6;

  while(PulseShapefile->good()) {
    double time = 0;
    double amplitude = 0;
    char tmp;
    *PulseShapefile >> time;
    *PulseShapefile >> tmp;
    *PulseShapefile >> amplitude;

    // cout<<time<<"  "<<tmp<<"  "<<amplitude<<endl;

    if(amplitude<minvalue && point>1000)
      {
	minvalue=amplitude;
	minpoint=point;
	//cout<<minvalue<<endl;
      }
    grSamplePulseShape->SetPoint(point,time,amplitude);
    point++;
  }



  grSamplePulseShape->SetMarkerStyle(20+f);
  grSamplePulseShape->SetMarkerSize(0.4);

 
  c->cd(1);
  grSamplePulseShape->Draw("PL");

 
  c->cd(2);
  grSamplePulseShape->Draw("APL");

  Int_t NumPoints = grSamplePulseShape->GetN();
  Double_t *x = grSamplePulseShape->GetX();
  Double_t *y = grSamplePulseShape->GetY();

 
  //Find Base line value per sample
  Double_t pedestal=0.0;
  Int_t pedestalPoints=0;
  Double_t averageDT = 0.0;
  for(int i =0 ; i<NumPoints-10; i++)
    {
      //cout<<x[i]<<endl;
      if(x[i]<1000e-9 && x[i]>500e-9)
	{
	  //cout<<x[i]<<endl;
	  pedestalPoints++;
	  pedestal+=y[i];
	  averageDT+=x[i+1]-x[i];
	}	
    }


  pedestal=pedestal/pedestalPoints;
  averageDT=averageDT/pedestalPoints;
  cout<<"Pedestal per sample "<<pedestal<<endl;
  cout<<"average time between samples "<<averageDT<<endl;

  //Subtract pedestal shift time scale that 0 is at minimum
  Float_t DeltaT = x[minpoint];
 cout<<"Minimum found at "<<x[minpoint]<<" with Ampl "<<minvalue<<endl;

  for(int i =0 ; i<NumPoints-10; i++)
    {
      //cout<<i<<"  "<<x[i]<<"  "<<y[i]<<endl;
     

      y[i]-=pedestal;
      x[i]-=DeltaT;
    }
  


   //Integrate
  Double_t integralPulse = 0.0;
  Double_t integralPedestalRegion = 0.0;

  for(int i =0 ; i<NumPoints-10; i++)
    {
      if(x[i]<1000e-9 && x[i]>500e-9)
	{
	  integralPedestalRegion+=y[i];
	}
      if(x[i]<100e-9 && x[i]>-20e-9)
	{
	  integralPulse+=y[i];
	}
    }

  cout<<"Integral in pedestal region "<<integralPedestalRegion<<endl;
  cout<<"Integral in pulse region "<<integralPulse<<endl;


  //normalize pulse shape to integral
  for(int i =0 ; i<NumPoints-10; i++)
    {
      y[i]/=fabs(integralPulse*averageDT);
    }

  //Integrate
  Double_t integralPulse = 0.0;
  Double_t integralPedestalRegion = 0.0;
  for(int i =0 ; i<NumPoints-10; i++)
    {
      if(x[i]<1000e-9 && x[i]>500e-9)
	{
	  integralPedestalRegion+=y[i]*averageDT;
	}
      if(x[i]<100e-9 && x[i]>-20e-9)
	{
	  integralPulse+=y[i]*averageDT;
	}
    }

  cout<<"Integral in pedestal region after normalization "<<integralPedestalRegion<<endl;
  cout<<"Integral in pulse region after normalization "<<integralPulse<<endl;

 //scaling of peak amplitude to 2ns sampling
  for(int i =0 ; i<NumPoints-10; i++)
    {
      y[i]*=1.0e-9;

      if(i-minpoint+500>=0 && i-minpoint+500<1000)
	{
	  yAverage[i-minpoint+500]+=y[i]/filenames->size();
	  xAverage[i-minpoint+500]+=x[i]/filenames->size();
	}

    }

  


  //Integrate
  Double_t integralPulse = 0.0;
  Double_t integralPedestalRegion = 0.0;
  for(int i =0 ; i<NumPoints-10; i++)
    {
      if(x[i]<1000e-9 && x[i]>500e-9)
	{
	  integralPedestalRegion+=y[i]*averageDT;
	}
      if(x[i]<25e-9 && x[i]>-15e-9)
	{
	  integralPulse+=y[i]*averageDT;
	}
    }

   cout<<"Integral in pedestal region after scaling to 2 ns "<<integralPedestalRegion<<endl;
  cout<<"Integral in pulse region after scaling to 2ns sampling  "<<integralPulse<<endl;

    }




 //Find Base line value per sample
  Double_t pedestal=0.0;
  Int_t pedestalPoints=0;
  for(int i =0 ; i<1000; i++)
    {
      //cout<<x[i]<<endl;
      if(xAverage[i]<-20e-9)
	{
	  //cout<<x[i]<<endl;
	  pedestalPoints++;
	  pedestal+=yAverage[i];
	}	
    }


  pedestal=pedestal/pedestalPoints;

  Double_t integralPulse = 0.0;
  Double_t integralPedestalRegion = 0.0;
  for(int i =0 ; i<1000; i++)
    {
      yAverage[i]-=pedestal;
      if(xAverage[i]<-20e-9)
	{
	  integralPedestalRegion+=yAverage[i]*averageDT;
	}
      if(xAverage[i]<25e-9 && xAverage[i]>-15e-9)
	{
	  integralPulse+=yAverage[i]*averageDT;
	}
    }

  c->cd(1);
  TGraph *grAver = new TGraph(1000,xAverage,yAverage);
  grAver->SetMarkerStyle(20);
  grAver->SetMarkerColor(kRed);
  grAver->Draw("pl");



   cout<<"Average: Integral in pedestal region after scaling to 2 ns "<<integralPedestalRegion<<endl;
  cout<<"Average: Integral in pulse region after scaling to 2ns sampling  "<<integralPulse<<endl;

    

  h->Draw("same");
  gPad->Modified();
  gPad->Update();
   

  //outputing the whole thing into a file

  ofstream outfile("outX.txt");

  //KASCADE assumes .25 ns steps. Setup stuff that will go into the
  //KSSinglePe.h file for the connoical single pe pul.se shape.
  std::vector< double > spe;
  int jmax=0;
  double speMax=0;
  for(int j=0;j<120;j++)
    {
      double time=j*.25;    //.25 nsec bins
      for(int i=0 ; i<1000-10; i++)
	{
	  if(xAverage[i]+6.5e-9>time*1.0e-9)
	    {
	      double pDiff=yAverage[i+1]-yAverage[i];
	      double frac=(time*1.e-9-(xAverage[i]+6.5e-9))/
		(xAverage[i+1]-xAverage[i]);
	      double pulseHeight=yAverage[i]+frac*(yAverage[i+1]-yAverage[i]);
	      pulseHeight=pulseHeight*1/.105233;
	      outfile<<time<<"  "<<pulseHeight<<endl;
	      spe.push_back(-pulseHeight);
	      if(-pulseHeight>speMax)
		{
		  speMax=-pulseHeight;
		  jmax=j;
		}
	      break;
	    }
	}    
    }
  outfile.close();

  Int_t speSize=(Int_t)spe.size();
  ofstream KASCADE("KSSinglePe.h.txt");
  KASCADE<<"const double kBaseRiseTimeNS=3.2;  //ns"<<endl;
  KASCADE<<"const double kBaseFallTimeNS=8.5;  //ns"<<endl;
  KASCADE<<"const int    kBaseSize="<<speSize<<";"<<endl;
  KASCADE<<"const int    kBaseRiseSize="<<jmax+1<<";"<<endl;
  KASCADE<<"const int    kBaseFallSize=kBaseSize-kBaseRiseSize;"<<endl;
  KASCADE<<endl;
  KASCADE<<"const static double kBasePulse[kBaseSize]="<<endl;
  KASCADE<<"{"<<endl;
  
  
  for(int i=0;i<(int)speSize;i+=9)
    {
      
      KASCADE<<"     ";
      
      
      for(j=0;j<9;j++)
	{
	  int k=j+i;
	  if(k<speSize)
	    {
	      KASCADE<<std::fixed;
	      KASCADE<<std::setprecision(3);
	      KASCADE<<spe.at(k)<<", ";
	    }
	}
      
      KASCADE<<endl;
    }
  
  KASCADE<<"     };"<<endl;
  KASCADE.close();

}

