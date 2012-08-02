//-*-mode:c++; mode:font-lock;-*-


//Written by:
// G.H.SembroskiMary Kertzman
//DePauw Univ.
//West LafayettGreencastle, In. 46135
//kertzman@depauw.edu
//765-658-4647

// 07/12/2011

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

#include "TApplication.h"
#include "TLine.h"
#include "TCanvas.h"
#include "TPad.h"
#include "TBox.h"
#include "TPaveLabel.h"
#include "TPaveText.h"
#include "TText.h"

#include "KSSegmentFile.h"
#include "KSSegmentDataClasses.h"

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

string SegFileName = "seg.d1";    
  std::cout<<"SegmentDisplay: Input Segment File: "<<SegFileName<<std::endl;
      
  KSSegmentFile*     pfSegFile;
  KSSegmentHeadData* pfSegmentHead;
  KSSegmentData*     pfSegment;
  pfSegmentHead= new KSSegmentHeadData ;
  pfSegFile=new KSSegmentFile();
  pfSegFile->Open(SegFileName.c_str());
  pfSegFile->ReadSegmentHead(pfSegmentHead);
  pfSegmentHead->PrintSegmentHead();

  pfSegment=new KSSegmentData;

  //Set up color map so that each particle type draws in its ouwn color
  map<int,int> particlecolor;
  particlecolor[1]=1;     //gamma
  particlecolor[2]=632;   //positron
  particlecolor[3]=416;   //electron
  particlecolor[4]=618;   //muon +
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




  //Set up the canvas(make it square) and its coordinate range in meters
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
  TPad* xzpad = new TPad("xzpad","subpad for xzplot",0.01,0.01,0.5,0.85);
  TPad* yzpad = new TPad("yzpad","subpad for yzplot",0.5,0.01,0.99,0.85);
  TPad* toppad = new TPad("toppad","subpad for titles",0.01,0.85,0.99,0.99);

  xzpad->SetBorderMode(0);
  yzpad->SetBorderMode(0);
  toppad->SetBorderMode(0);

  xzpad->Draw();
  yzpad->Draw();
  toppad->Draw();


  //default limits of xz  plot for vertical showers
  float xz_xmin=-4681;
  float xz_xmax=4681;
  float xz_ymin=1275;
  float xz_ymax=20000;
  //default limits of yz plot for vertical showers
  float yz_xmin=-4681;
  float yz_xmax=4681;
  float yz_ymin=1275;
  float yz_ymax=20000;
  //Get initial direction from segment header

  double dli = pfSegmentHead->fDlInitial;
  double dmi = pfSegmentHead->fDmInitial;
  double dni = pfSegmentHead->fDnInitial;

  double incident_direction_degrees;
  string incident_direction;

  //if shower is inclined in the NS plane, change the limits of the yz plot.
  //need to do something similar for the xz plot for E/W inclined showers
  //probably shoudl worry about random angles too!

  cout<<" dli= "<<dli<<" dmi= "<<dmi<<" dni= "<<dni<<endl;

  //from North  
  if (dmi > 0.005){  
    yz_xmin=-9364;
    yz_xmax=0;
    cout<<" ymin= "<<yz_ymin<<"  ymax= "<<yz_ymax<<endl;
    incident_direction = "North";
    incident_direction_degrees = asin(dmi)*57.29578;
  }
  //from South
  if(dmi < -0.005){  
    yz_xmin=0;
    yz_xmax=9364;
  }


  // xzpad->Range(-3256,1275,3256,20000);
  //yzpad->Range(-6492,1275,0,20000);
  xzpad->Range(xz_xmin,xz_ymin,xz_xmax,xz_ymax);
  yzpad->Range(yz_xmin,yz_ymin,yz_xmax,yz_ymax);


 
  //OKay, done with setting up the canvas and pads.
  //Now lets draw some boxes and add some text
  //  xzpad->SetBorderMode(1);
  //  xzpad->SetBorderSize(5);

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
  os<<"Incident  "<<incident_direction_degrees<<" deg from "<<incident_direction;
  string direction = os.str();
  os.str("");
  os<<"Shower ID "<<showerid;
  string id=os.str();
  TPaveText* ptop=new TPaveText(0.3,0.02,0.7,0.96);
  TText *t0=ptop->AddText("Purdue/DePauw KASCADE Air Shower Simulation");
  TText *t0a=ptop->AddText("Particle Tracks");
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


  xzpad->cd();
  TBox *bxz = new TBox(xz_xmin,xz_ymin,xz_xmax,xz_ymax);
  bxz->SetLineColor(1);
  bxz->SetLineWidth(3);
  bxz->SetFillStyle(0);
  bxz->Draw();

  MakeAndPlotLabels(xz_xmin,xz_ymin,xz_xmax,xz_ymax,"Looking North");



  yzpad->cd();
  TBox *byz = new TBox(yz_xmin,yz_ymin,yz_xmax,yz_ymax);
  byz->SetLineColor(1);
  byz->SetLineWidth(3);
  byz->SetFillStyle(0);
  byz->Draw();

  MakeAndPlotLabels(yz_xmin,yz_ymin,yz_xmax,yz_ymax,"Looking East");
    //  xzpad->Update();
    //yzpad->Update();  

  int i=0;
  
  //   while(i<10000) {
  while (1){
      i++;
      bool goodread=pfSegFile->ReadSegment(pfSegment);
      
      if(goodread){
	  double XStart=pfSegment->fXStart;
	  double YStart=pfSegment->fYStart;
	  double HStart=pfSegment->fHStart;
	  double DlStart=pfSegment->fDlStart; //Initial x,y direction cosigns of segment
	  double DmStart=pfSegment->fDmStart;
	  //double EndTime=pfSegment->fEndTime; //relative time(ns) at end of 
	                                        //segment.
	  double HEnd=pfSegment->fHEnd;   //final altitude of segment
	  //double DlEnd=pfSegment->fDlEnd;  //final direction cosigns of segment.
	  //double DmEnd=pfSegment->fDmEnd;
	  //double Gamma=pfSegment->fGamma;  //gamma at middle of segment.
	  double Spec=pfSegment->nspec;

	  // find x and z from the starting positions and the direction cosines
	  //first need to get dn from the other two
	  double DnStart=sqrt(1-DmStart*DmStart-DlStart*DlStart);
	    double SegLength=(HStart-HEnd)/DnStart;
	    double XEnd=XStart+SegLength*DlStart;
	    double YEnd=YStart+SegLength*DmStart;
	    // std::cout<<"HStart"<<HStart<<"  HEnd "<<HEnd<<" Dl "<<DlStart<<" Dm " <<DmStart<<" Dn "<<DnStart<<" seglength "<<SegLength <<endl;
	    
	    
	    int pcolor=1;
	    //draw segments in xz plane
	    //    Mycanvas->cd(1);
	    xzpad->cd();
	    TLine*  xz = new TLine(XStart,HStart,XEnd,HEnd);
	    pcolor=particlecolor[(int)Spec];
	    xz->SetLineColor(pcolor);
	    xz->Draw();
	    //	    Mycanvas->Update();

	    //draw segments in the yz plane
	    //Mycanvas->cd(2);
	    yzpad->cd();
	    TLine*  yz = new TLine(YStart,HStart,YEnd,HEnd);
	    pcolor=particlecolor[(int)Spec];
	    yz->SetLineColor(pcolor);
	    yz->Draw();


	    Mycanvas->cd();
	    Mycanvas->SetLineColor(1);
	    Mycanvas->SetLineWidth(3);
	    //update the canvas so the lines draw.Not sure if thsi is necessary
	    Mycanvas->Update();



      }
      else{
	break;
      } 
  }
  pfSegFile->Close();
  std::cout<<"SegmentDisplay: KSSegmentFile reports Number of Segements "
	"read: "<<(long)pfSegFile->getNumSegments()<<endl;
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
  os3<<"altitude= "<<ymin<<" m";
  string label3=os3.str();
  TPaveLabel* l3x = new TPaveLabel(0.01,0.03,0.41,0.055,label3.c_str(),"NDC");
  l3x->SetTextAlign(12);
  l3x->SetBorderSize(0);
  l3x->Draw();

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

  return;
}




// **************************************************************************
