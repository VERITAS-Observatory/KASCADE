/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision: 1.3
 * $Tag$
 *
 **/
// ************************************************************************
// This is a class that holds  the Facet code.
// C++stuff 
// ************************************************************************

#ifndef KSFACETS_H
#define KSFACETS_H

#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <string>
#include <cstdlib>

using namespace std;


#include <TTree.h>


// *******************************************************
class KSFacets
{
 private:
  void FindFacetNormalWhipple(double FacetX, double FacetY, double FacetZ, 
			      double& FacetNormalX, double& FacetNormalY,
			      double& FacetNormalZ);

  void FindFacetNormalMcGill(double FacetX, double FacetY, double FacetZ, 
			      double& FacetNormalX, double& FacetNormalY,
			      double& FacetNormalZ);

  double fMirrorRadiusSquaredM2;
  double fFacetDiameterM;
  double fFocalLengthM;


  double fFocalPlaneLocationM;
  double fAlignmentPlaneLocationM;
  string fAlignmentMethod;
  string fFacetLocationFileName;

  TTree* pFacetTree;
  float  fFacetXM;
  float  fFacetYM;
  double fFacetZM;
  float  fFacetEXIST;

  vector < double > fFacetX;
  vector < double > fFacetY;
  vector < double > fFacetZ;
  vector < double > fExist;

  vector < double > fFacetNormalX;
  vector < double > fFacetNormalY;
  vector < double > fFacetNormalZ;

  int fNumFacets;

  float dummy;

public:
  KSFacets(double MirrorRadiusSquaredM2, double FacetDiameterM,
	  double FocalLengthM, double FocalPlaneLocationM,  
	  double AlignmentPlaneLocationM, string MirrorAlignmentMethod,
	  string FacetLocationFileName);
  virtual ~KSFacets();

  int FindFacetLocation( vector <double >& Pe, vector <double >& Facet,
			 int& facetIndex);
  int FindFacetNormal(vector <double >& Facet, vector <double >& FacetNormal,
		      int facetIndex);

};
// ***************************************************************************
#endif
