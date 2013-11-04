// *****************************************************************
// This is script to make a text file for Energy estimator studies.
// Made from a vegas stage5 combined TTree.
// *****************************************************************

bool debug=false;

//Stings for Combined tree.
std::string fCTreeName="SelectedEvents/CombinedEventsTree";
double DegToRad=(TMath::Pi()/180.);
double RadToDeg=(180./TMath::Pi());


VAAzElRADecXY* pfAzERADecXY;


double GetRcnDistToShowerMax_KM(int telID,VAShowerData* pfShower, 
			     VAHillasData* pfHillas, VAArrayInfo* pfArrayInfo);
double GetTrueDistToShowerMax_KM(int t,VAShowerData* pfShower,
			      VAHillasData* pfHillas, 
			      VAKascadeSimulationData* pfSim,
			      VAArrayInfo* pfArrayInfo);
double DotProduct(vector< double >& pfA, vector< double >& pfB);

// *******************************************************************

void MakeEestTextFile(string RootFileName, string TextFileName, 
		                                   int NumberOfEvents=-1)
// *******************************************************************
// Attach to the combed TTree in RootFileName and write selected stuff
// to text file.
// *******************************************************************
{
  // **********************************************
  // Just print out NumberofEvents but do it randomly so we have a good 
  //energy distribution, else just keep the first Number of events
  // **********************************************
  bool keepRandom=true;
  double fractionToKeep=1.0;
  TRandom* random=new TRandom;

// *****************************************************************
  // Open the4 input files and get the combined TTree and ArrayInfo
  // ***************************************************************** 
  TFile Run (RootFileName.c_str());
  
  VAArrayInfo* pfArrayInfo = 
                          (VAArrayInfo*) Run.Get("RunHeader/VAArrayInfo");
  if (pfArrayInfo==NULL){
    std::cout" Failed to load VaArrayInfo from: "<<RootFileName<<endl;
    return;
  }
  pfAzERADecXY= new VAAzElRADecXY(pfArrayInfo->longitude(),
				  pfArrayInfo->latitude());

  TTree* CombinedEventsTree=NULL;
  CombinedEventsTree=(TTree*) Run.Get(fCTreeName.c_str());
  if(CombinedEventsTree==NULL){
    std::cout<<"Could not find TTree "<<fCTreeName<<" in run file "
	     <<RootFileName<<std::endl;
    return;
  }

  // *******************************************************************
  // Open up the output  text file
  // *******************************************************************
  ofstream os(TextFileName.c_str());
  
  int numEntries=CombinedEventsTree->GetEntries();
  cout<<"Number of events In Input file:: "<<numEntries<<endl;
  if (NumberOfEvents<0) then{
    NumberOfEvents=numEntries;
    keepRandom=false;

  }
  // *********************************************************************
  // Now we need to hook up to stuff
  // *********************************************************************
  
  VAParameterisedEventData* pfCombParam = new VAParameterisedEventData();
  VAShowerData* pfShower = new VAShowerData();
  VAKascadeSimulationData* pfSim = new VAKascadeSimulationData;


  vector < int > TRcn;
  vector < double > TImDist;
  vector < double > TTrueImDist;
  TRcn.resize(4);
  TImDist.resize(4);
  TTrueImDist.resize(4);
  
  for (int i=0;i<4;i++){
    //TRcn.at(i)=new int;
    TImDist.at(i)=new double;
    TTrueImDist.at(i)=new double;
  }
 
  CombinedEventsTree->SetBranchAddress("P", &pfCombParam);
  CombinedEventsTree->SetBranchAddress("S", &pfShower);
  CombinedEventsTree->SetBranchAddress("Sim", &pfSim);
  
  CombinedEventsTree->SetBranchAddress("T1Rcn", &TRcn.at(0));
  CombinedEventsTree->SetBranchAddress("T2Rcn", &TRcn.at(1));
  CombinedEventsTree->SetBranchAddress("T3Rcn", &TRcn.at(2));
  CombinedEventsTree->SetBranchAddress("T4Rcn", &TRcn.at(3));
  
  CombinedEventsTree->SetBranchAddress("T1ImpactDist", &TImDist.at(0));
  CombinedEventsTree->SetBranchAddress("T2ImpactDist", &TImDist.at(1));
  CombinedEventsTree->SetBranchAddress("T3ImpactDist", &TImDist.at(2));
  CombinedEventsTree->SetBranchAddress("T4ImpactDist", &TImDist.at(3));
  
  CombinedEventsTree->SetBranchAddress("T1TrueImpactDist", &TTrueImDist.at(0));
  CombinedEventsTree->SetBranchAddress("T2TrueImpactDist", &TTrueImDist.at(1));
  CombinedEventsTree->SetBranchAddress("T3TrueImpactDist", &TTrueImDist.at(2));
  CombinedEventsTree->SetBranchAddress("T4TrueImpactDist", &TTrueImDist.at(3));
  

  cout<<"Number of events with Rcn Energies to attempt to get: "
      <<NumberOfEvents<<endl;
  int numGoodEvents=0;
  if (keepRandom){
 
    // *****************************************************************
    // Find total number of events that we could keep
    // *****************************************************************

    for( int i=0;i<numEntries;i++){
      CombinedEventsTree->GetEntry(i);
      if (pfShower->fIsEnergy){
	numGoodEvents++;
      }
    }	
    cout<<"Number of Good Events: "<<numGoodEvents<<endl;

    if (NumberOfEvents>numGoodEvents){
      NumberOfEvents=numGoodEvents;
    }
     
    fractionToKeep=(double)NumberOfEvents/(double)numGoodEvents;
    
    cout<<" Will keep "<<fractionToKeep*100
	<<"% random events from file to keep energy distribution."
	<<endl;
  }



  if(debug){
    cout<<"Ready to process file"<<endl;
  }

  // **********************************************************************
  // We are now ready to process the file
  // This file can be used for either R or for root TTree input
  // The column definitions  would be:
  //"T1IsTrig/I:T1Rcn:T1Size/F:T1Dist:T1ImDist_M/F:T1TruImDist_M:T1DistToMax_KM:T1TrueDistToMax_KM:
  // T2IsTrig/I:T2Rcn:T2Size/F:T2Dist:T2ImDist_M/F:T2TruImDist_M:T2DistToMax_KM:T2TrueDistToMax_KM:
  // T3IsTrig/I:T3Rcn:T3Size/F:T3Dist:T3ImDist_M/F:T3TruImDist_M:T3DistToMax_KM:T3TrueDistToMax_KM:
  // T4IsTrig/I:T4Rcn:T4Size/F:T4Dist:T4ImDist_M/F:T4TruImDist_M_M:T4DistToMax_KM:T4TrueDistToMax_KM:
  // RcnTheta2:RcnEnergy_GeV:TrueEnergy_Gev:
  //
  // A root TTree header for the file couold look like:
  // Is1/I:R1:S1/F:D1:ID1:TID1:DM1:TDM1:Is2/I:R2:S2/F:D2:ID2:TID2:DM2:TDM1:Is3/I:R3:S3/F:D3:ID3:TID3:
  // DM3:TDM3:Is4/I:R4:S4/F:D4:ID4:TID4:DM4:TDM4:TH:RE:TE/F
  // **********************************************************************

  int numGoodEvents=0;
  for( int i=0;i<numEntries;i++){
    CombinedEventsTree->GetEntry(i);

    if (pfShower->fIsEnergy){
      double r=random->Uniform();
      //cout<<"r1: "<<r<<endl;
      if(r<fractionToKeep){
	     
	//cout<<"r2: "<<r<<endl;
	for(int t=0;t<4;t++){
	  VAHillasData* pfHillas = 
	                      (VAHillasData*) pfCombParam->getHillasData(t);
	  if (pfHillas != NULL) {
	    if(pfHillas->fIsL2Triggered){
	      os<<"1 ";
	    }
	    else{
	      os<<"0 ";
	    }
	    
	    os<<TRcn.at(t)<<" ";        //T1Rcn, etc
	    os<<pfHillas->fSize<<" ";    //Size
	    os<<pfHillas->fDist<<" ";    //Dist
	    os<<TImDist.at(t)<<" ";     //Impact distance(reconstructed)(m)
	    os<<TTrueImDist.at(t)<<" "; //TrueImpactDist (meters)
	    double TDistToMax_KM=GetRcnDistToShowerMax_KM(t,pfShower,pfHillas,
							  pfArrayInfo);
	    os<<TDistToMax_KM<<" ";
	    double TTrueDistToMax_KM=GetTrueDistToShowerMax_KM(t,pfShower,
							       pfHillas,
							       pfSim,
							       pfArrayInfo);
	    os<<TTrueDistToMax_KM<<" ";
	  }
	  else{
	    os<<"0 0 0 0 0 0 0 0 ";
	  }
	}
	
	os<<pfShower->fTheta2_Deg2<<" ";  //Theta2
	os<<pfShower->fEnergy_GeV<<" ";   //Std Rcn energy
	os<<pfSim->fEnergyGeV<<endl;       //True energy
	numGoodEvents++;
	if (numGoodEvents%1000 ==0){
	  cout<<"At event: "<<i<<" we have: "<<numGoodEvents<<" good events."
	      <<endl;
	}
      
	if(numGoodEvents>=NumberOfEvents){
	  break;
	}
      }
    }
  }
  cout<<"Number of event in output file: "<<numGoodEvents<<endl;
  return;
}
// ************************************************************************

double GetRcnDistToShowerMax_KM(int telID,VAShowerData* pfShower, 
			     VAHillasData* pfHillas, VAArrayInfo* pfArrayInfo)
// ************************************************************************
// Determine form the core location on the ground, the direction in the sky,
// and the dist angle and  the distance from tel t+1(ie t=0 is for T1) 
// to showerMax
// ************************************************************************
// Use vector arithmatic. I copy from my notes:
// A=Vector from tel to core loc(known)
// B^=Unit vector from core location on ground in sky direction(shower axis)
//    (known)
// B = vector in B^ direction that ends at showermax.
// C,C' = Vector from tel to showermax: Actully we have 2 of these. the one we 
//     can reconstruct from the measured location of the shower centroid in 
//     the measured image (C),  and the actual one that intercepts the shower 
//     axis(C'). 
//     These 2 should be very close but probably are not exactly the same. We 
//     may make the assumption that they are the same.
//     Actually this turns out to be a bad assumption. We need C' to get the
//     angle gamma.  
// T = Vector in tracking direction(axis from which X,Y of centroid is 
//     measured to derive vector C)
// Since by defn. B and C' vectors will intercept at shower max.  C' = A + B
// We make a big assumption that C ~= C'.
// Dist=90-Theta: theta is the  angle between A^ and C^: A^ dot C^ = cos(theta)
//
// For reconstructed values:
// We measure theta and A and B^
// => if we define phi as angle between A and B^  then we know cos(phi).
// => we know gamma=180-theta-phi
// Then law of sines (vector form):
// C  x B = B x A
// =>   |C||B|sin(gamma) = |A||B|sin(phi)
// and  |C|=|A|sin(phi)/sin(gamma)   This is distance to showerMax
// *****Trick:  we can measure gamma directly. its the angle between C' and B
//      which ar X,Y values in out image FOV.The distance bwteen the Cm of 
//      image and the reconstructed direction in the FOV is gamma!!!
// *************************************************************************
{
  // *********************************************************
  // Position vector(from tel to reconstructed core location)  (A above)
  // *********************************************************
  vector < double > A(3,0.); 
 
  //Reconstructed Core location
  double xEastGroundPlane_M  = pfShower->fCoreXEastGroundPlane_M;
  double yNorthGroundPlane_M = pfShower->fCoreYNorthGroundPlane_M;

  //Tel Location
  double xGround_M=pfArrayInfo->telescope(telID)->positionEW();
  double yGround_M=pfArrayInfo->telescope(telID)->positionNS();

  //generate relative vector
  A.at(0)=xEastGroundPlane_M-xGround_M;
  A.at(1)=yNorthGroundPlane_M-yGround_M;
  A.at(2)=0.0;

  // *********************************************************
  // Now we need the reconstructed shower axis unit vector (B^ above)
  // *********************************************************
  // Get reconstructed zn in radians and get az in radians
  double showerEl_Deg=pfShower->fDirectionElevation_Rad * RadToDeg;
  double showerZN_Rad=(90. - showerEl_Deg) * RadToDeg;
  double showerAz_Rad =pfShower->fDirectionAzimuth_Rad;

  // Make up unit vector in this direction
  
  vector <double> B(3,0);
  B.at(0) = sin(showerZN_Rad) * sin(showerAz_Rad);      //X East
  B.at(1) = sin(showerZN_Rad) * cos(showerAz_Rad);      //Y North
  B.at(2) = cos(showerZN_Rad);                  //Z Up

  // ************************************************************
  // To get the angle gamma between the Showermax vector direction and the 
  // direction of the source, use the measured locations in the FOV
  // *************************************************************
  if (debug){
    cout<<"X,Y: "<<pfHillas->fXC<<" "<<pfHillas->fYC<<endl;
    cout<<"SX,SY: "<<pfShower->fDirectionXCamPlane_Deg<<" "
	<<pfShower->fDirectionYCamPlane_Deg<<endl;
  }

  // Get angles: 
  // ***********************************************************
  double XDiff=pfShower->fDirectionXCamPlane_Deg-pfHillas->fXC;
  double YDiff=pfShower->fDirectionYCamPlane_Deg-pfHillas->fYC;
  double gamma_Deg=sqrt(XDiff*XDiff+YDiff*YDiff);
  double gamma_Rad=gamma_Deg*DegToRad;

  double ALength_M=sqrt(DotProduct(A,A)); //magnitude of core (|A|)
  double cosAlpha=DotProduct(A,B)/ALength_M;
  double alphaDeg=acos(cosAlpha) * RadToDeg;
  double alphaRad=alphaDeg*DegToRad;
  double phi_Deg=180.-alphaDeg;
  double phi_Rad=phi_Deg*(TMath::Pi() / 180.);

  //double CMagnitude_km = (ALength_M * sin(phi_Rad)/sin(gamma_Rad))/1000.;
  double CMagnitude_km = (ALength_M * sin(alphaRad)/sin(gamma_Rad))/1000.;
  if (debug){
    cout<<" A:"<<A.at(0)<<" "<<A.at(1)<<" "<<A.at(2)
	<<" B:"<<B.at(0)<<" "<<B.at(1)<<" "<<B.at(2)
      //<<" C:"<<C.at(0)<<" "<<C.at(1)<<" "<<C.at(2)
	<<"CMag_km: "<< CMagnitude_km
	<<" cosAlpha,alpha_Deg: "<<cosAlpha<<" "<<alphaDeg<<" "
	<<" phi_Deg,gamma_Deg: "<<phi_Deg<<" "<<" "<<gamma_Deg<<endl;
  }
  return CMagnitude_km;
}
// ***************************************************************


/** Dot product calc */
double DotProduct(vector< double >& pfA, vector< double >& pfB) 
// ****************************************************************
// Calculate dot product.
// ****************************************************************
{
  double fSum=0;
  for(int i=0;i<3;i++)
    {
      fSum+=pfA.at(i)*pfB.at(i);
    }
  return fSum;
}
// ******************************************************************

double GetTrueDistToShowerMax_KM(int telID,VAShowerData* pfShower,
			      VAHillasData* pfHillas, 
			      VAKascadeSimulationData* pfSim,
			      VAArrayInfo* pfArrayInfo)
// ******************************************************************
// Find the true distance to shower max using the emission altitude, 
// true core location and shower direction found in the simulation data.
// ******************************************************************
// Using same definitions given in GetRcnDistToShowerMax_KM
// |B|=emissionAlt/sin(theta)
// |C|=|A+B|
// *******************************************************************
{
  // *********************************************************
  // Position vector(from tel to reconstructed core location)  (A above)
  // *********************************************************
  vector < double > A(3,0.); 
 




  // ***Reconstructed Core location
  // double xEastGroundPlane_M  = pfShower->fCoreXEastGroundPlane_M;
  // double yNorthGroundPlane_M = pfShower->fCoreYNorthGroundPlane_M;
  // ***

  // ***True core location (note sign change for y)
  double xEastGroundPlane_M  = pfSim->fCoreEastM;
  double yNorthGroundPlane_M = -pfSim->fCoreSouthM;
  // ***

  //Tel Location(I think there is an offset here in origen)&&&&&&&&&&&&&
  double xGround_M=pfArrayInfo->telescope(telID)->positionEW();
  double yGround_M=pfArrayInfo->telescope(telID)->positionNS();

  //generate relative vector
  A.at(0)=xEastGroundPlane_M-xGround_M;
  A.at(1)=yNorthGroundPlane_M-yGround_M;
  A.at(2)=0.0;

  // *********************************************************
  // Now we need the shower axis unit vector (B^ above)
  // *********************************************************

  // ****Get reconstructed direction
  //double showerEl_Deg=pfShower->fDirectionElevation_Rad * RadToDeg;
  //double showerAz_Rad =pfShower->fDirectionAzimuth_Rad;
  // ****
  
  // ****Use True direction:
  double showerEl_Deg=pfSim->fPrimaryZenithDeg;
  double showerAz_Rad=pfSim->fPrimaryAzimuthDeg*DegToRad;
  // ****

  double showerZN_Rad=(90. - showerEl_Deg) * DegToRad;

 
  // Make up unit vector in this direction
  // Get emission altitude Corret for observatory altitude
  double emissionAlt_M=pfSim->fEmissionAltitudeM-pfSim->fCoreElevationMASL;
  
  vector <double> B(3,0);
  B.at(0) = (sin(showerZN_Rad) * sin(showerAz_Rad))*emissionAlt_M;   //X East
  B.at(1) = (sin(showerZN_Rad) * cos(showerAz_Rad))*emissionAlt_M;   //Y North
  B.at(2) =  cos(showerZN_Rad) * emissionAlt_M;                      //Z Up

  vector <double> C(3,0);
  C.at(0)=A.at(0)+B.at(0);
  C.at(1)=A.at(1)+B.at(1);
  C.at(2)=A.at(2)+B.at(2);
  double CMagnitude_M=sqrt(DotProduct(C,C));
  double CMagnitude_KM=CMagnitude_M/1000.0;
  
  if(debug){
    cout<<" A:"<<A.at(0)<<" "<<A.at(1)<<" "<<A.at(2)
	<<" B:"<<B.at(0)/emissionAlt_M<<" "<<B.at(1)/emissionAlt_M<<" "
	<<B.at(2)/emissionAlt_M
	<<" C:"<<C.at(0)/CMagnitude_M<<" "<<C.at(1)/CMagnitude_M<<" "
	<<C.at(2)/CMagnitude_M
	<<" EmAlt_m: "<<emissionAlt_M<<" CMag_KM: "<< CMagnitude_KM<<endl;
  }
  
  return CMagnitude_KM;
}
  
