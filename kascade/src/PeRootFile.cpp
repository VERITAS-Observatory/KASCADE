


//-*-mode:c++; mode:font-lock;-*-
//6 May 2014 MPK
// adapted from PhotonsOnGround.cpp



//Written by:
//Mary Kertzman
//DePauw Univ.
//Greencastle, In. 46135
//kertzman@depauw.edu
//765-658-4647



#include "stdint.h"
#include <time.h>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>

//Remember to include every root command you use!!!
#include "TFile.h"
#include "TTree.h"

#include "KSSegmentFile.h"
#include "KSSegmentDataClasses.h"
#include "KSAzElRADecXY.h"
#include "KSCommon.h"
#include "KSPeFile.h"
#include "KSPeDataClasses.h"

#include <map>


using namespace std;



int main(int argc, char** argv)
{

  // *********************************************************************
  // Since this is just a test program no need for fancy inputs
  // Outputs can be root TTree::ReadFile()compatable.
  // *********************************************************************
 
  //Not sure if I will need this so I will keep it arounf for now but comment
  //it out 
  //Open the graphics window
  //  TApplication mainApp("mainApp", &argc, argv); 

 //Open the input seg file and read in the seg header
  // ------------------------------------------------------------------------  

string PeFileName = "pe.d1";    
  std::cout<<"PeRootFIle: Input Pe File: "<<PeFileName<<std::endl;
      
    KSSegmentHeadData* pfSegmentHead;
    pfSegmentHead= new KSSegmentHeadData ;
    KSPeHeadData* pfPeHead;
    pfPeHead= new KSPeHeadData;
    KSPeFile* pfPeFile;
    pfPeFile = new KSPeFile;

  pfPeFile->Open(PeFileName.c_str());

 if(pfPeFile==NULL)    {
      std::cout<<"PeRootFile:Failed to open Input Pe File: "<<PeFileName
	       <<std::endl;
      return 1;
    }
  bool goodread=pfPeFile->ReadSegmentHead(pfSegmentHead);
  if(!goodread)
    {
      std::cout<<"PeRootFile: Failed to read Segment Header from Pe File"
	       <<std::endl;
      return 1;
    }
  
  pfSegmentHead->PrintSegmentHead();
  
  goodread=pfPeFile->ReadPeHead(pfPeHead);
  if(!goodread)
    {
      std::cout<<"PeRootFile: Failed to read Pe Header from Pe File"
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


 //Set up the root file that we want to make
 //see page 329 in ROOT Users Guide from MAy 13
   TFile f("mytree.root","recreate");
   TTree t1("t1","t1");

   //From the KSPeData
  int Nx;
  int Ny;
  double Time;
  double Dl;
  double Dm;
  int SegId;
  double X;
  double Y;
  int nSpec;
  int Lambda;
  double EmissionAlt;

  //calucated from the variables in KSPeData
  double Dn;
  double XRelCore;
  double YRelCore;
  double RadialDist;

  //Set these varioables up as branches in our tree
  t1.Branch("Nx",&Nx,"Nx/I");
  t1.Branch("Ny",&Ny,"Ny/I");
  t1.Branch("Time",&Time,"Time/D");
  t1.Branch("Dl",&Dl,"Dl/D");
  t1.Branch("Dm",&Dm,"Dm/D");
  t1.Branch("Dn",&Dn,"Dn/d");
  t1.Branch("SegId",&SegId,"SegId/I");
  t1.Branch("X",&X,"X/D");
  t1.Branch("Y",&Y,"Y/D");
  t1.Branch("nSpec",&nSpec,"nSpec/I");
  t1.Branch("Lambda",&Lambda,"Lambda/I");
  t1.Branch("EmissionAlt",&EmissionAlt,"EmissionAlt/D");
  t1.Branch("XRelCore",&XRelCore,"XRelCore/D");
  t1.Branch("YRelCore",&YRelCore,"YRelCore/D");
  t1.Branch("RadialDist",&RadialDist,"RadialDist/D");


  //finally ready to read in pe's

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
	Nx=pfPe->fNx;
	Ny=pfPe->fNy;
	Time=pfPe->fTime;
	Dl=pfPe->fPhotonDl;
	Dm=pfPe->fPhotonDm;
	SegId=pfPe->fSegmentID;
	X=pfPe->fX;
	Y=pfPe->fY;
	nSpec=pfPe->fTrackType;
	Lambda=pfPe->fLambda;
	EmissionAlt=pfPe->fEmissionAltitude;

	Dn = sqrt(Dl*Dl+Dm*Dm);
	XRelCore = X + Nx*XAreaWidth - XCoreOffset;
	YRelCore = Y + Ny*YAreaWidth - YCoreOffset;
	RadialDist = sqrt(XRelCore*XRelCore+YRelCore*YRelCore);

	t1.Fill();

	}


    
      else{
	std::cout<<"Done reading pefile"<<endl;
	break;
      } 
  }
  t1.Write();

  pfPeFile->Close();
  std::cout<<"PeRootFile: KSPeFile reports Number of Photons "
	"read: "<<(long)pfPeFile->getNumPes()<<endl;
  return 0;
}
// **************************************************************************


