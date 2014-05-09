


//-*-mode:c++; mode:font-lock;-*-
//28 April 2014 MPK
// adapted from SegmentDisplay.cpp
//lots of code copied from ksPeSortMerge.cpp


//Written by:
// G.H.Sembroski& Mary Kertzman
//DePauw Univ.
//West LafayettGreencastle, In. 46135
//kertzman@depauw.edu
//765-658-4647

// 07/12/2011

//comments from SegmentDisplay
//Notes to myself for code improvements:
//-Currently can only handle showers inclined in NS plane, i.e. az = 0 or 180
//-Should automate plot limits based on obs altitude in header file
//-The map to particle names is incomplete
//-contains commented out tidbits of code so that I can see alternate ways to do things. Should remove these and keep them somewhere else.
//-Input file is handled bad; have to create a link to the file in yoru directory. 
//-Have a strange problem that the pad borders don;t quite line up like I think
// they should

#include "stdint.h"
#include <time.h>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>

//Remember to include every root command you use!!!

#include "TApplication.h"
#include "TLine.h"
#include "TMarker.h"
#include "TCanvas.h"
#include "TPad.h"
#include "TBox.h"
#include "TPaveLabel.h"
#include "TPaveText.h"
#include "TText.h"

#include "KSSegmentFile.h"
#include "KSSegmentDataClasses.h"6
#include "KSAzElRADecXY.h"
#include "KSCommon.h"
#include "KSPeFile.h"
#include "KSPeDataClasses.h"

#include <map>


using namespace std;

void MakeAndPlotLabels(float, float, float, float, string);


int main(int argc, char** argv)
{

  // *********************************************************************
  // Since this is just a test program no need for fancy inputs
  // Outputs can be root TTree::ReadFile()compatable.
  // *********************************************************************
 
 
  //Open the graphics window
  TApplication mainApp("mainApp", &argc, argv); 

 //Open the input seg file and read in the seg header
  // ------------------------------------------------------------------------  

string PeFileName = "pe.d1";    
  std::cout<<"PhotonsOnGround: Input Pe File: "<<PeFileName<<std::endl;
      
    KSSegmentHeadData* pfSegmentHead;
    pfSegmentHead= new KSSegmentHeadData ;
    KSPeHeadData* pfPeHead;
    pfPeHead= new KSPeHeadData;
    KSPeFile* pfPeFile;
    pfPeFile = new KSPeFile;

  pfPeFile->Open(PeFileName.c_str());

 if(pfPeFile==NULL)    {
      std::cout<<"PhotonsOnGround:Failed to open Input Pe File: "<<PeFileName
	       <<std::endl;
      return 1;
    }
  bool goodread=pfPeFile->ReadSegmentHead(pfSegmentHead);
  if(!goodread)
    {
      std::cout<<"PhotonsOnGround: Failed to read Segment Header from Pe File"
	       <<std::endl;
      return 1;
    }
  
  pfSegmentHead->PrintSegmentHead();
  
  goodread=pfPeFile->ReadPeHead(pfPeHead);
  if(!goodread)
    {
      std::cout<<"PhotonsOcGround: Failed to read Pe Header from Pe File"
	       <<std::endl;
      return 1;
    }
  
  pfPeHead->PrintPeHead();
  //the following are needed to determine the x and y ground position
  // of the photon
  double XAreaWidth=pfPeHead->fXAreaWidthM; 
  double YAreaWidth=pfPeHead->fYAreaWidthM;
  double XCoreOffset=pfPeHead->fXCoreOffsetM;
  double YCoreOffset=pfPeHead->fYCoreOffsetM;  

  KSPeData* pfPe;
  pfPe= new KSPeData;

  //done with file set up stuff; on to setting up the graphics.
  // At this point the segheader and peheader have been read it,
  // and the next read on the input file will be a pe data record. 

  //Set up color map so that each particle type draws in its ouwn color
  map<int,int> particlecolor;
  particlecolor[1]=1;     //gamma
  particlecolor[2]=632;   //positron
  particlecolor[3]=416;   //electron
  particlecolor[4]=618;   //muon +6
  particlecolor[5]=434;   //muon -
  particlecolor[6]=396;   //pi 0
  particlecolor[7]=894;   //pi +
  particlecolor[8]=844;   //pi -
  particlecolor[9]=631;    //kaon +
  particlecolor[10]=599;    //kaon -
  particlecolor[11]=398;    //kaon 0long
  particlecolor[12]=399;    //kaon 0short
  particlecolor[13]=807;    //proton
  particlecolor[14]=407;    //neutron
  particlecolor[15]=406;    //neutrino (electron)
  particlecolor[16]=390;    //anti neutrino (electron)
  particlecolor[17]=606;    //neutrino (muon)
  particlecolor[18]=590;    //anti neutrino (muon)
  particlecolor[19]=920;    //any other particle type

  //set up a map for incident particle type. I could use some arrays that are
  // aready defined in kssegmentdata.h which include kskascadenames 
  //(or something like that.for now, being lazy and only putting in gammas,
  // protons and He. Can add more as needed.

  map<int,string> particletype;
  particletype[1]="Gamma";
  particletype[2]="Positron";
  particletype[3]="Electron";
  particletype[4]="Muon +";
  particletype[5]="Muon -";
  particletype[6]="Pi 0";
  particletype[7]="Pi +";
  particletype[8]="Pi -";
  particletype[13]="Proton";
  particletype[24]="Helium";




  //Set up the canvas(make it square) and its coordinate range in (meters?)
  double_t w=600;
  double_t h=700;
  TCanvas *Mycanvas = new TCanvas("Mycanvas"," ",(int)w,(int)h);

  //this next line was in a cern reference about how to make a square canvas not sure if its needed, or what it does.   
// Mycanvas->SetWindowSize(w-(Mycanvas->GetWw()),h+(h-Mycanvas->GetWh()));
  
  //not using this, but saving it to see how to refernce subpads that were
  //created automatically with the Divide method
  //Mycanvas->Divide(2,1);
  //TPad* xzpad = (TPad*)(Mycanvas->GetPrimitive("Mycanvas_1"));
  //TPad* yzpad = (TPad*)(Mycanvas->GetPrimitive("Mycanvas_2"));

  //specifically creat sub pads so that I can control their size
  TPad* groundpad = new TPad("groundpad","subpad for pes on ground",0.01,0.01,0.99,0.85);
  TPad* toppad = new TPad("toppad","subpad for titles",0.01,0.85,0.99,0.99);

  groundpad->SetBorderMode(0);
  toppad->SetBorderMode(0);

  groundpad->Draw();
  toppad->Draw();


  //default limits for photons on ground plot (meters)
  float xmin=-300;
  float xmax=300;
  float ymin=-300;
  float ymax=300;



  
//2_17_2014 convert the initial direction cosines to elevation and azimuth. Doing this two ways...my brut force wa y and with the classes/methods from kascade. 
  //get the intial particle direction from the segment header
  double dli = pfSegmentHead->fDlInitial;
  double dmi = pfSegmentHead->fDmInitial;
  double dni = pfSegmentHead->fDnInitial;

double incident_zenith_degrees;
 double incident_azimuth_degrees;
 double thetax_degrees;
 thetax_degrees=atan2(dmi,dli)*57.28578;
  cout<<"thetax= "<<thetax_degrees<<endl;
if(thetax_degrees <0)thetax_degrees=thetax_degrees+360;
   cout<<"thetax= "<<thetax_degrees<<endl;
incident_zenith_degrees=acos(dni)*57.29578;
 incident_azimuth_degrees=thetax_degrees-90;
 cout<<"zenith= "<<incident_zenith_degrees<<" azimuth= "<<incident_azimuth_degrees<<endl;

 KSAzElRADecXY convertCoords(-1.93649, 0.552828); //this I copied from kaslLights and I have no idea why I need to put in numbers here

 double az_rad;
 double el_rad;
 double vegas_dmi=-dmi; //vegas and kascade have opposite definitions of teh directions of +y and +z. Hopefully this fixes it so I can use teh vegas version of slalib calls here
 double vegas_dni=-dni;
 double vegas_dli=-dli;
 convertCoords.DlDmDnToAzEl(vegas_dli,dmi,dni, az_rad, el_rad);
 double az=az_rad/gDeg2Rad; //convert to degrees
 double zn=90-el_rad/gDeg2Rad;
 cout<<" From slalib we get zenith = "<<zn<<" and azimuth = "<<az<<endl;



  groundpad->Range(xmin,ymin,xmax,ymax);



 
  //OKay, done with setting up the canvas and pads.
  //Now lets draw some boxes and add some text
  //title and information box at the top of plot

  toppad->cd();
  TBox *tb = new TBox(0,0,1,1);
  tb->SetLineColor(1);
  tb->SetLineWidth(3);
  tb->SetFillStyle(0);
  tb->Draw();
  int ispec = pfSegmentHead->fType;   //initial particle type
  double penergy = pfSegmentHead->fGeVEnergyPrimary; //initial particle energy
  int showerid=pfSegmentHead->fShowerID;  //shower id of this shower

  ostringstream os;
  string particle = particletype[ispec];
  os<<penergy<<" GeV "<<particle;
  string ptitle=os.str();
  os.str("");
  os<<"Incident zenith angle =  "<<zn<<" degrees;  azimuth =  "<<az<<" degrees";
  string direction = os.str();
  os.str("");
  os<<"Shower ID "<<showerid;
  string id=os.str();
  TPaveText* ptop=new TPaveText(0.3,0.02,0.7,0.96);
  TText *t0=ptop->AddText("Purdue/DePauw KASCADE Air Shower Simulation");
  TText *t0a=ptop->AddText("Photons on the Ground");
  TText *t1=ptop->AddText(ptitle.c_str());
  TText *t2=ptop->AddText(direction.c_str());
  TText *t3=ptop->AddText(id.c_str());
  TText *t3a=ptop->AddText("Our Website link goes here!");
  t0a->SetTextSize(0.15);
  t1->SetTextSize(0.2);
  t3->SetTextSize(0.08);
  t3a->SetTextSize(0.08);
  ptop->SetBorderSize(0);
  ptop->Draw();

  TPaveText* ptop_left = new TPaveText(0.02,0.02,0.3,0.96);
  TText *t4=ptop_left->AddText("Particle Type Legend (partial)");
  t4->SetTextAlign(12);
  t4->SetTextSize(0.1);
  TText *t5=ptop_left->AddText(" ");
  int j=1;
  string type;
  while(j<=8) { 
    type=particletype[j];
    t5=ptop_left->AddText(type.c_str());
    t5->SetTextAlign(12);
    t5->SetTextColor(particlecolor[j]);
    j++;
  }
  type=particletype[13];
  t5=ptop_left->AddText(type.c_str());
  t5->SetTextAlign(12);
  t5->SetTextColor(particlecolor[13]);
   
  ptop_left->SetBorderSize(0);
  ptop_left->Draw();


  groundpad->cd();
  TBox *bxz = new TBox(xmin,ymin,xmax,ymax);
  bxz->SetLineColor(1);
  bxz->SetLineWidth(3);
  bxz->SetFillStyle(0);
  bxz->Draw();

  MakeAndPlotLabels(xmin,ymin,xmax,ymax," ");



  //finally ready to read in pe's
  //  //skipping the first    200000 photons
  //
  //  int k=0;
  
  //  while(k<200000) {
  //  k++;
  //  bool goodread=pfPeFile->ReadPe(pfPe);
      
  //  }

  int i=0;
  int pcount = 0;
  int icount=0;
  int jcount=0;
  cout<<" number of photons read in units of 1000 "<<endl;
  //  while(i<200000) {
	while (1){
      i++;
      icount++;
      bool goodread=pfPeFile->ReadPe(pfPe);
      if(icount%1000 == 0) {
	jcount++;
	cout<<jcount<<" "<<flush;
      }
      if(goodread){
	int Nx=pfPe->fNx;
	int Ny=pfPe->fNy;
	double Time=pfPe->fTime;
	double PhotonDl=pfPe->fPhotonDl;
	double PhotonDm=pfPe->fPhotonDm;
	int SegId=pfPe->fSegmentID;
	double X=pfPe->fX;
	double Y=pfPe->fY;
	int nSpec=pfPe->fTrackType;
	int Lambda=pfPe->fLambda;
	double EmissionAlt=pfPe->fEmissionAltitude;

	double XRelCore = X + Nx*XAreaWidth - XCoreOffset;
	double YRelCore = Y + Ny*YAreaWidth - YCoreOffset;
	//debug prints
	if(pcount<=5){
	  pcount++;
	  cout<<"fX: "<<pfPe->fX<<endl;
	cout<<"X: "<<X<<" Nx: "<<Nx<<"X offset: "<<XCoreOffset<<endl;
	cout<<"XRelCore: "<<XRelCore<<endl;
}
       	if (Nx > -25 && Nx < 25 && Ny > -25 && Ny < 25) {
	    int pcolor=1;
	    //Plot the photons on the ground

	    groundpad->cd();
	    int markertype=1;
	    TMarker*  plotpe = new TMarker(XRelCore,YRelCore,markertype);
	    pcolor=particlecolor[nSpec];
	    plotpe->SetMarkerColor(pcolor);
	    plotpe->Draw();
	    //	    Mycanvas->Update();


	    Mycanvas->cd();
	    Mycanvas->SetLineColor(1);
	    Mycanvas->SetLineWidth(3);
	    //update the canvas so the lines draw.Not sure if thsi is necessary
	    Mycanvas->Update();
	}


      }
      else{
	std::cout<<"Done reading pefile"<<endl;
	break;
      } 
  }
  pfPeFile->Close();
  std::cout<<"PhotonsOnGround: KSPeFile reports Number of Photons "
	"read: "<<(long)pfPeFile->getNumPes()<<endl;
  std::cout<<"Finished plotting; hit control c to exit"<<endl; 
	Mycanvas->Print("my_pe_plot.ps");  
  mainApp.Run();  
return 0;
}
// **************************************************************************


void MakeAndPlotLabels(float xmin,float ymin,float xmax,float ymax, string lookDir)
{

  //Glenn's trick for putting a numerical value into a character string, 
  //which is needed for the TPaveLabel

  ostringstream os1;
  os1<<xmin<<" m";
  string label1=os1.str();
  TPaveLabel* l1x = new TPaveLabel(0.01,0.003,0.21,0.028,label1.c_str(),"NDC");
  l1x->SetTextAlign(12);
  l1x->SetBorderSize(0);
  l1x->Draw();

  ostringstream os2;
  os2<<xmax<<" m";
  string label2=os2.str();
  TPaveLabel* l2x = new TPaveLabel(0.79,0.003,0.99,0.028,label2.c_str(),"NDC");
  l2x->SetTextAlign(32);
  l2x->SetBorderSize(0);
  l2x->Draw();

  ostringstream os3;
  os3<<ymin<<" m";
  string label3=os3.str();
  TPaveLabel* l3x = new TPaveLabel(0.01,0.03,0.41,0.055,label3.c_str(),"NDC");
  l3x->SetTextAlign(12);
  l3x->SetBorderSize(0);
  //  l3x->Draw();

  ostringstream os4;
  os4<<ymax<<" m";
  string label4=os4.str();
  TPaveLabel* l4x = new TPaveLabel(0.01,0.97,0.31,0.995,label4.c_str(),"NDC");
  l4x->SetTextAlign(12);
  l4x->SetBorderSize(0);
  l4x->Draw();

  TPaveLabel* l5x = new TPaveLabel(0.6,0.95,0.99,0.995,lookDir.c_str(),"NDC");
  l5x->SetTextAlign(12);
  l5x->SetBorderSize(0);
  l5x->Draw();

  ostringstream osN;
  osN<<"N";
  string labelN = osN.str();
  TPaveLabel* lN = new TPaveLabel(0.48,0.95,0.52,0.99,labelN.c_str(),"NDC");
  lN->SetTextAlign(23);
  lN->SetBorderSize(0);
  lN->Draw();

  ostringstream osS;
  osS<<"S";
  string labelS=osS.str();
  TPaveLabel* lS = new TPaveLabel(0.48,0.01,0.52,0.05,labelS.c_str(),"NDC");
  lS->SetTextAlign(21);
  lS->SetBorderSize(0);
  lS->Draw();

  ostringstream osE;
  osE<<"E";
  string labelE=osE.str();
  TPaveLabel* lE = new TPaveLabel(0.95,0.48,0.99,0.52,labelE.c_str(),"NDC");
  lE->SetTextAlign(32);
  lE->SetBorderSize(0);
  lE->Draw();

  ostringstream osW;
  osW<<"W";
  string labelW=osW.str();
  TPaveLabel* lW = new TPaveLabel(0.01,0.48,0.05,0.52,labelW.c_str(),"NDC");
  lW->SetTextAlign(12);
  lW->SetBorderSize(0);
  lW->Draw();

  return;
}




// **************************************************************************
