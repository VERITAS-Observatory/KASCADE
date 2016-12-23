
int findPointIndex( TGraph* pEAGraph, double xValue)
// ****************************************************
// Find point in this graph closes to the xValue
// ****************************************************
{
  int numPoints = pEAGraph->GetN();
  double minDist;
  int minIndex = -1;
  for(int i = 0; i < numPoints; i++) {
	double x;
	double y;
	pEAGraph->GetPoint(i,x,y);
	double dist = abs(x-xValue);
    if(minIndex == -1) {
	  minIndex = i;
	  minDist  = dist;
	}
	else{
	  if ( dist < minDist ) {
		minIndex = i;
		minDist  = dist;
	  }
	}
  }
  return minIndex;
}
// ******************************************************

string getPedvarStr(int pedvarIndex, string pedvarType)
{
   // *****************************************************
  // This is for KASCADE upgrade MDL10UA or MDL12UA only for now
  // ****************************************************
  //cout<<"pedvarIndex,type: " << pedvarIndex<< " " << pedvarType << endl;
  if (pedvarType == "MDL10UA") {
	string pedvarStr[] =
	  {"4.73","5.55","6.51","7.64","8.97","10.52","12.35","14.49","17"};
    return  pedvarStr[pedvarIndex];
  }
  else {
	if (pedvarType == "MDL12UA") {
	  // ********MDL12UA******
	  string pedvarStr[] =
		{"5.18","5.55","6.51","7.64","8.97","10.52","12.35","14.49","17"};
	  return  pedvarStr[pedvarIndex];
	}
	else {
	  std::cout <<" Invalid pedvarType requested: " << pedvarType<< std:;endl;
	  std::exit(1);
	}
  }
}
// *************************************************************************

string getOffsetStr(int offsetIndex)
{
  string offsetStr[] = {"0","0.25","0.5","0.75","1","1.25","1.5","1.75","2"};
  return offsetStr[offsetIndex];
}
// *************************************************************************

double getEnergyGeV(int energyIndex)
{
  double energyGeV[] = {20, 23.9, 28.6, 34.2, 40.9, 48.9, 58.5, 69.9, 83.6, 
						100, 119.6, 143, 171, 204.5, 244.5, 292.4, 350, 418, 
						500, 598, 715, 855,	1022, 1223, 1462, 1748, 2091, 
						2500, 2990, 3575, 4275, 5112, 6113, 7310, 8741, 10453,
						12500, 14948, 17875, 21375, 25560, 30565, 36550, 
						43707, 52265};
  return energyGeV[energyIndex];
}
// *************************************************************************

void  getEA( TFile* f, string eaTableNameBase, int offsetIndex, 
			 double log10EnergyTeV, double& ea, double& eaErrLo, 
			 double& eaErrHi)
// *******************************************************
// Get the point source ea at this offset and energy
// *******************************************************
{
  string eaTableName =  eaTableNameBase + getOffsetStr(offsetIndex);
	  
  VAEffectiveArea* pfTable = 
		                    (VAEffectiveArea*)f->Get( eaTableName.c_str() );
								 
  if(!pfTable) {
	cerr << "unable to load table:" << eaTableName << endl;
	ea= -1;
	return;
  }
													  
  // ********************************
  //Get pointer to the table we want
  // ********************************
  TGraphAsymmErrors* pfHistoMC = pfTable->pfEffArea_MC;
  if(!pfHistoMC){
	cerr << "unable to load TGraph: pfEffArea_MC" << endl;
	ea = -1;
	return;
  }

  // ****************************************************************
  // Now we have the TGraph. Use the TGraph::Eval method.
  // The energy given should pretty much match the energy of some point
  // in the TGraph (I hope). 
  // ****************************************************************
  double ea =  pfHistoMC->Eval( log10EnergyTeV);

  // *********************************
  // Now we have index to point closesrt to our requested energy. 
  // Assume its errors are what we want!!!
  // *********************************
  int index = findPointIndex( pfHistoMC,  log10EnergyTeV);
  eaErrLo = pfHistoMC->GetErrorYlow(index);
  eaErrHi  = pfHistoMC->GetErrorYhigh(index);
  
  return;
}
// **********************************************************************


// ************
// Main function
// *************
void getESpecAOmega(int Zn, int Az, double ApertureDeg, int PedvarIndex, 
					double EnergyGeV, string EAFileName, double& AOmega,
					double& AOmegaErrLo, double& AOmegaErrHi)
// ****************************************************************
// This root macro find the electron(or gamma(?)) EA for an aperture at zn_az
// for a pedvar from the EAFileName at an energy EnergyGeV. The result thus
// has units Area(cm)*sr
// **************************************************************** 
// use code from $VEGAS/resultsExtractor/macros/drawTables.C
// ***************************************************************
{
  // ***********************
  // Open the EA root file
  // ***********************
  TFile* f = new TFile(EAFileName.c_str(), "read");
  TDirectory* dir = NULL;
  dir = (TDirectory*)f->Get("effective_areas");
  if(!dir) {
	cerr << "Couldn't get effective areas directory..." << endl;
	return 0;
  } 

  // *************************
  // Generate file table name base
  // *************************
  ostringstream os;
  os << "effective_areas/EffectiveArea_Azimuth_" << Az << "_Zenith_" << Zn 
	 <<"_Noise_"<< getPedvarStr(PedvarIndex, "MDL10UA") << "_AbsoluteOffset_";
  string eaTableNameBase=os.str();

  
  // *************************************************************************
  // In order to take advantage of the TGraph::Eval() function which can 
  // interpolate for us we are going to set up a TGraph for energy EnergyGeV  
  // with x as offset and y as ea
  // *************************************************************************
  int     numOffsets         = (2.0 / 0.25) + 1;  // should be 9 I think
  TGraphAsymmErrors* pEAGraph = new TGraphAsymmErrors(numOffsets);

  double log10EnergyTeV = log10(EnergyGeV/1000.);
  double ApertureRad    = ApertureDeg *  TMath::DegToRad();

  for ( int i =0; i < numOffsets; i++ ) {
	// **************************************************
	// Get the ea and its errors for this offset
	// **************************************************
  
	//	cout<<"  eaTableNameBase, i, log10EnergyTeV :" << eaTableNameBase
	//	<< " " << i << " " <<  log10EnergyTeV << endl;
	double ea;
	double eaErrLo;
	double eaErrHi;
	getEA( f, eaTableNameBase, i, log10EnergyTeV, ea, eaErrLo, eaErrHi);  

	double offsetDeg = .25* i;
	pEAGraph->SetPoint( i, offsetDeg,ea);
	pEAGraph->SetPointError( i, 0.0, 0.0, eaErrLo, eaErrHi);
  }


  // **************************
  // Loop through the offsets adding AOmega from each annulus
  // Later add stuff to handle the source exclusion area
  // **************************
  // Find last complete annulus index
  // **************************
 
  int maxCompleteOffsetIndex=(ApertureDeg-(0.25/2))/.25;  // Note round down
  if( maxCompleteOffsetIndex <0) {   //This taken care of below
	maxCompleteOffsetIndex = 0;
  }

  double innerRadiusRad = 0;
  double outerRadiusRad = 0;
  // **************
  // Center(offset= 0.0)  annulus first
  // **************
  double ea = pEAGraph->Eval(0.0);
  
  int index = findPointIndex( pEAGraph, 0.0);
  double eaErrLo = pEAGraph->GetErrorYlow(index);
  double eaErrHi = pEAGraph->GetErrorYhigh(index);
  cout<<"ea,eaErrLo:" << ea <<" " << eaErrLo <<endl;

  if (ea < 0 ) {
	return 0;
  }

  if (ApertureDeg < (0.25 / 2.0)  ) {
	outerRadiusRad = ApertureRad;
  }
  else {
	outerRadiusRad = ( 0.25 / 2.0 ) *  TMath::DegToRad(); 
  }
  
  double omegaSR = 2.0 * TMath::Pi() * (1.-cos(outerRadiusRad)); 
	  
  double aOmegaSum = ea * omegaSR;

  // ********************************
  // Now the errors in aOmegaSum: Assume add in quadrature. get sum of 
  // squares of propagated errors
  // ********************************
  double aOmegaErrLoSum2 = (omegaSR * omegaSR) * (eaErrLo * eaErrLo);
  double aOmegaErrHiSum2 = (omegaSR * omegaSR) * (eaErrHi * eaErrHi);
 
  // ***********************************************
  // Now the rest of the complete annulae
  // *********************************************** 

  for(int i = 1; i <= maxCompleteOffsetIndex; i++ ) {
	// *******************************************************
	// Going to assume EA at mid radius of annulus (offset) is point 
	// source ea for whole annulus
	// ******************************************************
	double centerOffsetDeg = i*.25;
	ea  =  pEAGraph->Eval(centerOffsetDeg);
	if (ea < 0 ) {
	  return 0;
	}
  
	int index = findPointIndex( pEAGraph,centerOffsetDeg);
	double eaErrLo = pEAGraph->GetErrorYlow(index);
	double eaErrHi = pEAGraph->GetErrorYhigh(index);
	
	// ****************************************************************
	// We have the EA for a point source. Convert to AOmega for annulus (in 
	// angle space, ie focal plane focal plane) by multiplying by the
	// angular area  in steradians of the annulus. Note this is on a sphere
	// and we use the sperical cap formulas.
	// https://en.wikipedia.org/wiki/Spherical_cap
	// ****************************************************************
	innerRadiusRad = ( (i-.5) * 0.25) * TMath::DegToRad();
	outerRadiusRad = ( (i+.5) * 0.25) * TMath::DegToRad();
	
	omegaSR = 2 * TMath::Pi() * ( (cos( innerRadiusRad) - 
								   cos( outerRadiusRad) ) );
	
	//cout<<"off,omega,ea: "<<centerOffsetDeg<<" " << omegaSR << " " << ea<<endl;
	// *******************
	// And add this contribution
	// *******************
	aOmegaSum += ea * omegaSR;

	// ********************************
	// And now the errors
	// **************************
	aOmegaErrLoSum2 += (omegaSR * omegaSR) * (eaErrLo * eaErrLo);
	aOmegaErrHiSum2 += (omegaSR * omegaSR) * (eaErrHi * eaErrHi);
  } 
  
  // ******************************************************************
  //Now we have to add in the aOmega from the last partial annulus.
  // We need to linear interpolate the ea to the middle of this partial
  // annulus. Do this with the fancy TGraph::Eval function.
  // ******************************************************************
  if ( ApertureRad > outerRadiusRad && maxCompleteOffsetIndex > 0 ) {
	// *******************************
	// This annulus will extend from outerRadiusRad to Aperture Rad
	// We assume linear interpolation of ea as function of offset(theta)
	// THis is real crude. We should do a better job but I think the errors
	// in ea swamp any fine scale correction here.
	// *******************************
	
	double centerOffsetDeg = 
	  (( ApertureDeg + maxCompleteOffsetIndex * 0.25 ) / 2.0);
	ea  =  pEAGraph->Eval(centerOffsetDeg);
	if (ea < 0 ) {
	  return 0;
	}
	int index = findPointIndex( pEAGraph, centerOffsetDeg);
	double eaErrLo = pEAGraph->GetErrorYlow(index);
	double eaErrHi = pEAGraph->GetErrorYhigh(index);

	innerRadiusRad=outerRadiusRad;
	outerRadiusRad=ApertureRad;
	omegaSR = 2 * TMath::Pi() * ( (cos( innerRadiusRad) - 
								   cos( outerRadiusRad) ) );
	
	//cout<<"off,omega,ea: "<<0.0<<" " << omegaSR << " " << ea<<endl;
	// *******************
	// And add this contribution
	// *******************
	aOmegaSum += ea * omegaSR;

	// ********************************
	// And now the errors
	// **************************
	aOmegaErrLoSum2 += (omegaSR * omegaSR) * (eaErrLo * eaErrLo);
	aOmegaErrHiSum2 += (omegaSR * omegaSR) * (eaErrHi * eaErrHi);
  } 

  // ********************
  // And Done!
  // ********************
  AOmega      = aOmegaSum;
  AOmegaErrLo = sqrt(aOmegaErrLoSum2);
  AOmegaErrHi = sqrt(aOmegaErrHiSum2);
  return;
}
// ***********************************************************************

void PlotAOmega(int Zn, int Az, double ApertureDeg, int PedvarIndex, 
				string EAFileName)
// **********************************************************************
// Plot AOmega for arguments using EA file EaFileName
// **********************************************************************
{
  // **************************************************
  // Iterate over energies.
  // We start at 350 GeV (index 16)  and go up to 5.112TeV (index 31)
  // **************************************************
  int eIndexFirst = 16; // 350 Gev
  //int eIndexLast  = 31;
  int eIndexLast  = 34;

  TGraphAsymmErrors* pAOmegaGraph  = 
                          new TGraphAsymmErrors(eIndexLast-eIndexFirst + 1);


  for (int i = eIndexFirst; i <=  eIndexLast; i++ ) {
	double energyGeV = getEnergyGeV(i);
	double log10EnergyTeV = log10(energyGeV/1000.0);
	//cout<<"Zn, Az, ApertureDeg, PedvarIndex, energyGeV,EAFileName: "
	//	<< Zn << " " << Az << " " << ApertureDeg << " " << PedvarIndex << " " 
	//	<< energyGeV << " " << EAFileName << endl;

	double aOmega;
	double aOmegaErrLo;
	double aOmegaErrHi;
	getESpecAOmega(Zn, Az, ApertureDeg, PedvarIndex, energyGeV,EAFileName,
				   aOmega, aOmegaErrLo, aOmegaErrHi);

	// ***************************************************
	// Enter it into a TGraph.
	// ***************************************************
	cout<<"i,log10EnergyTeV,aOmega,aOmegaError: " <<i<<" "<<log10EnergyTeV
		<< " " << aOmega << " " << aOmegaErrLo << " " <<  aOmegaErrHi 
		<< endl;
	pAOmegaGraph->SetPoint(i - eIndexFirst, log10EnergyTeV, aOmega);
  	pAOmegaGraph->SetPointError(i - eIndexFirst, 0.0, 0.0, 
								                 aOmegaErrLo, aOmegaErrHi);
  }
  pAOmegaGraph->SetMarkerStyle(20);
  pAOmegaGraph->Draw("AP");
  return;
}
// ************************************************************
