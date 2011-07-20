//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSTiltAndTrace
 * \brief Class to do all Facet work
 * Original Author: Glenn H. Sembroski 
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#include "KSFacets.h"

extern "C" float pran(float* dummy);

KSFacets::KSFacets(double MirrorRadiusSquaredM2, double FacetDiameterM,
		 double FocalLengthM, double FocalPlaneLocationM,  
		 double AlignmentPlaneLocationM, string MirrorAlignmentMethod,
		 string FacetLocationFileName)
{
  fMirrorRadiusSquaredM2   = MirrorRadiusSquaredM2;
  fFacetDiameterM          = FacetDiameterM;
  fFocalLengthM            = FocalLengthM;
  fFocalPlaneLocationM     = FocalPlaneLocationM;
  fAlignmentPlaneLocationM = AlignmentPlaneLocationM;
  fAlignmentMethod         = MirrorAlignmentMethod;
  fFacetLocationFileName   = FacetLocationFileName;

  // ******************************************************************
  // If we are generating random facets under photons (see mehtod 1: below)
  // There not much to do here.
  // If we are fed a file of facet locations there is (may be) a lot to do.
  // *******************************************************************
  if(fFacetLocationFileName!=" "){
    // ***************************************************************
    // We have a file of facet locations. To make life simple we assume this
    // is a file that can be read into a root TTree with the TTRee::ReadFile
    // method. We assume that there are at least variables XM,YM and EXIT are
    // defined for the file (Ex. First line in file: "FACETID/I:XM/F:YM:EXIST"
    // We will check for that. The EXIST variable if it exists(ha ha!) must 
    // have the value of 1 if this is a viable facet
    // ****************************************************************
    pFacetTree= new TTree();
    int numEntries=pFacetTree->ReadFile(fFacetLocationFileName.c_str());
    if(numEntries<=0){
      cout<<"KSFacets: Facet Location file "<<fFacetLocationFileName
	  <<" does not exist or has wrong format"<<endl;
      exit(EXIT_FAILURE);
    }
    // ******************************************************************
    // Now check that we have the variables we want
    // ******************************************************************
    if( pFacetTree->GetBranch("XM")!=NULL  &&  
	pFacetTree->GetBranch("YM")!=NULL  &&
	pFacetTree->GetBranch("EXIST")!=NULL){
      pFacetTree->SetBranchAddress("XM", &fFacetXM);
      pFacetTree->SetBranchAddress("YM", &fFacetYM);
      pFacetTree->SetBranchAddress("EXIST", &fFacetEXIST);
    }
    else{
      cout<<"KSFacets: Facet Location file "<<fFacetLocationFileName
	  <<" does not have XM and YM and EXIST variables (needs all 3)!"
	  <<endl;
      exit(EXIT_FAILURE);
    }

    // *******************************************************************
    // Read XM,YM from the TTRee. Reads EXITS flag is there
    // ******************************************************************
    cout<<"KSFacets: Facet Location file: "<<fFacetLocationFileName<<endl;
    fFacetX.clear();
    fFacetY.clear();
    fExist.clear();
    for(int i=0;i<numEntries;i++){
      pFacetTree->GetEntry(i);
      fFacetX.push_back(fFacetXM);
      fFacetY.push_back(fFacetYM);
      fExist.push_back(fFacetEXIST);
    }
    
    fNumFacets=fFacetX.size();
    // ************************************************************************
    // And we are done.
    // In the future we may want to be more clever on how we collect this 
    // facet location info to make lookup faster (maps etc?). We may also 
    // want to be able to read in a secondary file with alignment info for 
    // each segment to model flexing of the OSS.
  // ************************************************************************
  }
  else{
    pFacetTree=NULL;
    fNumFacets=0;
  }
}
// **************************************************************************

KSFacets::~KSFacets()
{
  // nothing to see here
}

// ***************************************************************************

int KSFacets::FindFacetLocation(vector < double >& Pe, 
				vector < double >& Facet)
// **************************************************************************
// Returns vector to the facet location the PE lands on.
// There are 2 methods:
// 1:If no Facet location file is specified we use the original Hillas method 
//   of randomly positioning  the photon on a facet.
// 2:If a Facet location file is specified we search the file for the facet
//   location. 
// ***************************************************************************
// Pe.at(0),Pe.at(1) are X,Y coordinates in m,irror plane of where the photon 
// hits
// ***************************************************************************
{
  int dump=0;

  // ******************************************************************
  // Is the the random facet method?
  // ******************************************************************
  if(fNumFacets==0){
    
    // ***********************************************************************
    // First: Position on facet where this photon hits.
    // ***********************************************************************
    // Search for a legal facet position
    while(1)
      {
	Facet.at(0)=(pran(&dummy)-.5)*fFacetDiameterM;
	Facet.at(1)=(pran(&dummy)-.5)*fFacetDiameterM;
	double rf=(Facet.at(0)*Facet.at(0))+(Facet.at(1)*Facet.at(1));
	if(rf<=((fFacetDiameterM/2)*(fFacetDiameterM/2)))
	  {
	    break;
	  }
      }

    // **********************************************************************
    // now use this position to position this fake facet.
    // Make facet vector Vfacet(from origen to center of facet).
    // Pe is postion vector of mirror plane intercept of the photon
    // ***********************************************************************
    Facet.at(0)+=Pe.at(0);
    Facet.at(1)+=Pe.at(1);  ;


  }
  else{
    // **********************************************************************
    // Find Facet location by searching through X,Y coords from file.
    // Going to do this mainly by brute force
    // **********************************************************************
    double r2Min=fFacetDiameterM*fFacetDiameterM;
    fMinIndex=-1;
    for(int i=0;i<fNumFacets;i++){

      // **************************************************************
      // We will try to put some performance improvement tests in this
      // Notice that we don't test for existance yet. We do that later 
      // We do that since our facets are not round but are rather hexagonal
      // **************************************************************
      double xDiff=abs(Pe.at(0)-fFacetX.at(i));
      if(xDiff>fFacetDiameterM){
	continue;
      }
      double yDiff=abs(Pe.at(1)-fFacetY.at(i));
      if(yDiff>fFacetDiameterM){
	continue;
      }
      double radius2M=xDiff*xDiff+yDiff*yDiff;
      if(radius2M<=r2Min){
	r2Min=radius2M;
	fMinIndex=i;
      }
    }
    // *******************************************************************
    // See if we found a facet
    // *******************************************************************
    if(fMinIndex==-1){
      dump=1;
      return dump;
    }
    // ******************************************************************
    // Now test for existance
    // ******************************************************************
    if(fExist.at(fMinIndex)!=1){
      dump=2;
      return dump;
    }
    // *****************************************************************
    // Its good.
    // ****************************************************************
    Facet.at(0)=fFacetX.at(fMinIndex);

    Facet.at(1)=fFacetY.at(fMinIndex);
  }
  // ***********************************************************************
  // Using the equation for a sphere (of radius fFocalLengthM) to determine 
  // the z component of fFacet
  // on the spherical surface of the mirror. It is assumed
  // that the photon is within the mirror aperature.
  // (but the facet may not be and thats an error for the aberration of the
  // edge of the telecope).
  // ************************************************************************
  double xyfMagSqr=Facet.at(0)*Facet.at(0)+Facet.at(1)*Facet.at(1);
                                              // This is for speed otimization
   
  Facet.at(2)=-sqrt(fFocalLengthM*fFocalLengthM - xyfMagSqr);
  // ***********************************************************************
  // Z is negative since vector goes from the MCC origin (at a 
  // distance radius=fFocalLengthM above mirror plane)
  // to the mirror surface at Facet.at(0),Facet.at(1).
  // thus  Facet.at(2)<=-12.0 meters (like -11.5 m))
  // ***********************************************************************
  dump=0;
  return dump;
}
// ***************************************************************************


int KSFacets::FindFacetNormal(vector < double >& Facet, 
			      vector < double >& FacetNormal)
// ************************************************************************
// MIRROR ALIGMENT METHODS
// We now want want the normal vector from the fake facet mirror that is 
// situated and centered at Facet. There are 2 ways to do this which are
// equivalent if fAlignmentPlaneLocationM == fFocalLength that is the 
// alignment focal plane is at the MCC,  but not otherwise. 
// This reflects the fact that both Whipple and Veritas have used 2 methods 
// for mirror alignment. I've labled them the WHIPPLE and MCGILL methods. 
// 1:WHIPPLE: This is the original method used to align the normal of all 
//   mirror segments for the WHIPPLE and VERITAAS telescopes before spring 
//   of 2009. In this method the normal of the center of each mirror facet
//   is pointed at a point 2 times the focal length from the center of the 
//   mirror plane along the optical axis (ie.at v2fl=0,0,+focal_length).
//  This effectivly put the alignment focal plane at fFocalLength.
// 2:MCGILL:  The second method, adopted in the spring of 2009 again has the
//   normal of each segment pointed to a spot on the optical axis of the 
//   telescope but at a point where the image of a star at infinity would be
//   focused at the center of the alignment focal plane (where the alignment
//   focal plane may not be at fFocalLength (MCC) or at fFoclaPlaneLengthM 
//   but is rather at fAlignmentPlaneLocationM). Thus the
//   angle between the normal to the facet and a ray parallel to the optic 
//   axis (from a star at the center of the FOV) is 1/2 the angle between a 
//   vector from the mirror facet to the center of the alignment focal plane
//   and the parallel ray.  This second method produces much tighter PSF's 
//   (not sure why, smaller focusing tolenece?) and is now the preferred 
//   method of alignment for Veritas and Whipple.
// ************************************************************************
{
  // *********************************************************
  // Old WHIPPLE method first
  // *********************************************************
  int dump=0;
  if(fAlignmentMethod=="WHIPPLE"){
    // ********************************************************************
    // Using WHIPPLE alignament method for determining facet normal.
    // This vector is Vrc=V2fl-Vf
    // First vector from center of facet to MCC (0,0,focal_length.)
    //  Note: This DOES NOT depend on fAlignmentPlaneLocationM or 
    //        fFoclaPlaneLocationM
    // ********************************************************************
     
    vector < double > fToFocalLength;//optical axis to focal plane in mirror 
                                      //plane systemfToFocalLength.at(0)=0.;
    fToFocalLength.clear();
    fToFocalLength.resize(3,0);
    fToFocalLength.at(2)=fFocalLengthM;
  
    FacetNormal.at(0)= fToFocalLength.at(0)-Facet.at(0);
    FacetNormal.at(1)= fToFocalLength.at(1)-Facet.at(1);
    FacetNormal.at(2)= fToFocalLength.at(2)-Facet.at(2);

    // ********************************************************************
    // Adjust the fFacet vector for fFocalPlaneLocationM != fFocalLengthM
    // fFacet's origin will no long be at the MCC but at 
    // fFocalLengthM-fFocalPlaneLocationM below it, which is where our Focal 
    // plane will be.
    // *******************************************************************
    Facet.at(2)= Facet.at(2) + (fFocalLengthM-fFocalPlaneLocationM);
    
  }
  else{
    // *********************************************************************
    // MCGILL alignament method used to determine facet normal direction.
    //  Note: This DOES NOT depend on fFoclalPlaneLoactionM but DOES depend
    //        on fAlignmentPlaneLocationM.
    // ********************************************************************
    // Adjust (for now) the fFacet vector for fAlignmentPlaneLocationM != 
    // fFoclaPlanM fFacet's origin will no long be at the MCC but at 
    // fFocalLengthM-fAlignmentPlaneLocationM below it, which is where our 
    // alignment Focal plane will be. Later we will adjust Facet.at(2) to be 
    // relative to the fFocalPlaneLocationM.
    // *******************************************************************
    Facet.at(2) = Facet.at(2) + (fFocalLengthM-fAlignmentPlaneLocationM);

    // *******************************************************************
    // The angle of this vector to the z axis (optical axis) is 2*theta.
    // WE neeed to find the vector which has the same X,Y components but
    // is at an angle of theta to the opticle axis
    // X,Y vector (A) is thus:
    // ******************************************************************
    vector < double > A(3);     //vector Perp to Z axis to center of facet
    A.at(0)=Facet.at(0);
    A.at(1)=Facet.at(1);
    A.at(2)=0;
    // ********************
    // length of A is sin(theta).
    // ********************
    double AMagnitude= sqrt(A.at(0)* A.at(0)+A.at(1)* A.at(1));
    // ***************************************************************
    // We are going to drop the central facet. Its shadowed anyway
    if(AMagnitude<.5*fFacetDiameterM){
      dump=1; // flag to drop this pe
      return dump;
    }
    

    // Facet.at(2) is length of FacetMagnitude*cos(2*theta)
    double FacetMagnitude= sqrt(Facet.at(0)*Facet.at(0)+ 
				Facet.at(1)*Facet.at(1)+ 
				Facet.at(2)*Facet.at(2));

    double theta=.5*acos(-Facet.at(2)/FacetMagnitude);  //Facet.at(2) will be -
    
    // ***********************************
    // Now we can contruct the Facet normal vector
    // *****************************************************
    FacetNormal.at(0)=-A.at(0);
    FacetNormal.at(1)=-A.at(1);
    
    // *****************************************************
    // The length of the facet normal (vector  at angle theta to optical 
    // axis to center of facet) will be:
    // **********************************
    double FacetNormalMagnitude=AMagnitude/sin(theta);
    FacetNormal.at(2)=cos(theta)*FacetNormalMagnitude;
  
    // *******************************************************************
    // Now adjust Facet.at(2) to be relative to fFocalPlaneLocationM
    // *******************************************************************
    Facet.at(2)= Facet.at(2)+ 
                            (fAlignmentPlaneLocationM-fFocalPlaneLocationM); 
  }
  dump=0;
  return dump;
}
// ************************************************************************
