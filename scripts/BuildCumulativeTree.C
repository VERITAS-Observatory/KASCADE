//-*-mode:c++; mode:font-lock;-*-

TFile* pfCFile;
TTree* pfCTree;
VAShowerData*            pfCShwr;
VAKascadeSimulationData* pfCSim;

VAHillasData* pfHillasT1;
VAHillas2DData* pfHillas2DT1;
VAHillasData* pfHillasT2;
VAHillas2DData* pfHillas2DT2;
VAHillasData* pfHillasT3;
VAHillas2DData* pfHillas2DT3;
VAHillasData* pfHillasT4;
VAHillas2DData* pfHillas2DT4;
int fT1;
int fT2;
int fT3;
int fT4;
int fT1Rcn;
int fT2Rcn;
int fT3Rcn;
int fT4Rcn;
double fT1ImpactDist;
double fT2ImpactDist;
double fT3ImpactDist;
double fT4ImpactDist;
double fT1TrueImpactDist;
double fT2TrueImpactDist;
double fT3TrueImpactDist;
double fT4TrueImpactDist;




void GetArrayPositonsInMirrorPlane(VAArrayInfo* pfArrayInfo, 
				   double fSourceAz, 
				   double fSourceElev, 
				   std::vector< double >& fArrayX, 
				   std::vector< double >& fArrayY);

int BuildCumulativeTree(char* fFileNameIn, char* fFileNameOut)
{
  // **********************************************************************
  // Setup input file for reading
  // **********************************************************************
  int fRet= InitRootFile(fFileNameIn);
  if(fRet!=0)
    {
      std::cout<<"Failed to InitRootFile: "<<fFileNameIn<<std::endl;
      return 1;
    }
  fRet=BuildRootFileIndexes();
  if(fRet!=0)
    {
      std::cout<<"Failed to BuildRootFileIndexes: "<<fFileNameIn<<std::endl;
      return 1;
    }
  // ************************************************
  // Set all indexs to access by fArrayEventNum(only Hillas tree access is by 
  // event index)
  // **************************************************
  if(pfParTree!=NULL)
    {
      pfParTree->SetTreeIndex(pfEventIndexIndex);
    }
  else
    {
      
      std::cout<<"Fatal: No Hillas tree. Got to have one to do this: " 
	       <<fFileNameIn<<std::endl;
      return 1;
    }
  // **********************************************************************
  // Create the new TFile and the Tree within it
  // **********************************************************************

  pfCFile=(TFile*) new TFile(fFileNameOut,"RECREATE");
  if(pfCFile==NULL)
    {
      std::cout<<"Failed to Open OutPut Cumulative TFile: "<<fFileNameOut
	       <<std::endl;      
      return=1;
    }

  std::cout<<"Created Cumulative file:                pfCFile"<<std::endl;


  pfCFile->cd();    //Makes new tree part of new file.
  pfCTree= (TTree*) new TTree("CTree","Cumulative Tree");
  if(pfCTree==NULL)
    {
      std::cout<<"Failed to Open OutPut Cumulative TTree: "<<fFileNameOut
	       <<std::endl;      
      return=1;
    }
  std::cout<<"Created Cumulative TTree:               pfCTree"<<std::endl;
  
  // ***********************************************************************
  // Set up all the brances. Standard stuff first then the stuff thats not
  // easy to get to, which we will make easy 
  // ***********************************************************************
  std::cout<<"***********Branches**************"<<std::endl;

  if(pfShwrTree!=NULL)
    {
      pfCShwr= new VAShowerData;
      pfCTree->Branch(gShowerEventsBranchName.c_str(),"VAShowerData",&pfCShwr,
		      16000,0);

      std::cout<<"Created Cumulative Shower Branch:       "
	       <<gShowerEventsBranchName.c_str()<<std::endl;
    }
  

  if(pfSimTree!=NULL)  //Assume its a KASCADE sim. We can clean this up later
    {                  // if needed.
      pfCSim=new VAKascadeSimulationData;
      pfCTree->Branch(gSimulatedEventsBranchName.c_str(),
		      "VAKascadeSimulationData",&pfCSim,16000,0);
      std::cout<<"Created Cumulative Simulation Branch:   "
	       <<gSimulatedEventsBranchName.c_str()<<std::endl;
    }

  if(pfParTree!=NULL)
    {
      pfHillasT1 = new VAHillasData;
      pfHillas2DT1 = new VAHillas2DData;
      pfCTree->Branch("HT1","VAHillasData",&pfHillasT1,16000,0);
      pfCTree->Branch("H2DT1", "VAHillas2DData",&pfHillas2DT1,16000,0);
      std::cout<<"Created Cumulative Hillas T1 Branch:    HT1"<<std::endl;
      std::cout<<"Created Cumulative Hillas2D T1 Branch:  H2DT1"<<std::endl;

      pfHillasT2 = new VAHillasData;
      pfHillas2DT2 = new VAHillas2DData;
      pfCTree->Branch("HT2","VAHillasData",&pfHillasT2,16000,0);
      pfCTree->Branch("H2DT2", "VAHillas2DData",&pfHillas2DT2,16000,0);
     std::cout<<"Created Cumulative Hillas T2 Branch:    HT2"<<std::endl;
      std::cout<<"Created Cumulative Hillas2D T2 Branch:  H2DT2"<<std::endl;

      pfHillasT3 = new VAHillasData;
      pfHillas2DT3 = new VAHillas2DData;
      pfCTree->Branch("HT3","VAHillasData",&pfHillasT3,16000,0);
      pfCTree->Branch("H2DT3", "VAHillas2DData",&pfHillas2DT3,16000,0);
     std::cout<<"Created Cumulative Hillas T3 Branch:    HT3"<<std::endl;
      std::cout<<"Created Cumulative Hillas2D T3 Branch:  H2DT3"<<std::endl;

      pfHillasT4 = new VAHillasData;
      pfHillas2DT4 = new VAHillas2DData;
      pfCTree->Branch("HT4","VAHillasData",&pfHillasT4,16000,0);
      pfCTree->Branch("H2DT4", "VAHillas2DData",&pfHillas2DT4,16000,0);
      std::cout<<"Created Cumulative Hillas T4 Branch:    HT4"<<std::endl;
      std::cout<<"Created Cumulative Hillas2D T4 Branch:  H2DT4"<<std::endl;

    }
  // *********************************************************************
  // Now the special stuff
  // *********************************************************************
  pfCTree->Branch("T1",&fT1,"T1/I");  //Flags for existance of hillas data
  pfCTree->Branch("T2",&fT2,"T2/I");
  pfCTree->Branch("T3",&fT3,"T3/I");
  pfCTree->Branch("T4",&fT4,"T4/I");
  std::cout<<"Created Cumulative Branch:              T1"<<std::endl;
  std::cout<<"Created Cumulative Branch:              T2"<<std::endl;
  std::cout<<"Created Cumulative Branch:              T3"<<std::endl;
  std::cout<<"Created Cumulative Branch:              T4"<<std::endl;
 
  pfCTree->Branch("T1Rcn",&fT1Rcn,"T1Rcn/I"); 
  pfCTree->Branch("T2Rcn",&fT2Rcn,"T2Rcn/I");
  pfCTree->Branch("T3Rcn",&fT3Rcn,"T3Rcn/I");
  pfCTree->Branch("T4Rcn",&fT4Rcn,"T4Rcn/I");
  std::cout<<"Created Cumulative Branch:              T1Rcn"<<std::endl;
  std::cout<<"Created Cumulative Branch:              T2Rcn"<<std::endl;
  std::cout<<"Created Cumulative Branch:              T3Rcn"<<std::endl;
  std::cout<<"Created Cumulative Branch:              T4Rcn"<<std::endl;

  pfCTree->Branch("T1ImpactDist",&fT1ImpactDist,"T1ImpactDist/D");
  pfCTree->Branch("T2ImpactDist",&fT2ImpactDist,"T2ImpactDist/D");
  pfCTree->Branch("T3ImpactDist",&fT3ImpactDist,"T3ImpactDist/D");
  pfCTree->Branch("T4ImpactDist",&fT4ImpactDist,"T4ImpactDist/D");
  std::cout<<"Created Cumulative Branch:              T1ImpactDist"
	   <<std::endl;
  std::cout<<"Created Cumulative Branch:              T2ImpactDist"
	   <<std::endl;
  std::cout<<"Created Cumulative Branch:              T3ImpactDist"
	   <<std::endl;
  std::cout<<"Created Cumulative Branch:              T4ImpactDist"
	   <<std::endl;

  pfCTree->Branch("T1TrueImpactDist",&fT1TrueImpactDist,"T1TrueImpactDist/D");
  pfCTree->Branch("T2TrueImpactDist",&fT2TrueImpactDist,"T2TrueImpactDist/D");
  pfCTree->Branch("T3TrueImpactDist",&fT3TrueImpactDist,"T3TrueImpactDist/D");
  pfCTree->Branch("T4TrueImpactDist",&fT4TrueImpactDist,"T4TrueImpactDist/D");
  std::cout<<"Created Cumulative Branch:              T1TrueImpactDist"
	   <<std::endl;
  std::cout<<"Created Cumulative Branch:              T2TrueImpactDist"
	   <<std::endl;
  std::cout<<"Created Cumulative Branch:              T3TrueImpactDist"
	   <<std::endl;
  std::cout<<"Created Cumulative Branch:              T4TrueImpactDist"
	   <<std::endl;

  std::vector< double* >fTrueImpDist;
  fTrueImpDist.push_back(&fT1TrueImpactDist);
  fTrueImpDist.push_back(&fT2TrueImpactDist);
  fTrueImpDist.push_back(&fT3TrueImpactDist);
  fTrueImpDist.push_back(&fT4TrueImpactDist);


  // **********************************************************************
  // Now we are ready to load the tree. Use the hillas data to drive the
  // filling loop, but use fArrayEventNum to insure we stay in synch
  // **********************************************************************
  // Get number of Entries to search in hillas tree (get fEventIndex of last 
  // hillas event)
  // **********************************************************************
  int fNumHillasEntries=(int)pfParTree->GetEntries();
  pfParTree->GetEntry(fNumHillasEntries-1);
  int fLastEventIndex=pfHillas->fEventIndex;
  std::cout<<"Last Event Index in pfParTree: "<<fLastEventIndex<<std::endl;

  // ***********************************************************************
  // Loop through the enrties filling the CTree
  // ***********************************************************************
  for(int i=0;i<fLastEventIndex+1;i++)
    {

      if(i%10000==0)
	{
	  std::cout<<"At EventIndex: "<<i<<std::endl;
	}

      // ******************************************************************
      // First we load up the hillas stuff. we are not using arrays so do it
      // all explictly. Not pretty but works.
      // ******************************************************************
      // Index for parTree was set for fEventIndex,fTelId




      int fArrayEventNum=-1;
      int fIndex = pfParTree->GetEntryNumberWithIndex(i,0);      //T1
      if(fIndex>=0)
	{
	  pfParTree->GetEntry(fIndex);
	  *pfHillasT1=*pfHillas;
	  *pfHillas2DT1=*pfHillas2D;
	  fArrayEventNum=pfHillas->fArrayEventNum;
	  fT1=1;
	}
      else
	{
	  // Should really fill pfHillasT1, and pfHillas2DT1 with 0 here.
	  fT1=0;
	}

      fIndex = pfParTree->GetEntryNumberWithIndex(i,1);      //T2
      if(fIndex>=0)
	{
	  pfParTree->GetEntry(fIndex);
	  *pfHillasT2=*pfHillas;
	  *pfHillas2DT2=*pfHillas2D;
	  fArrayEventNum=pfHillas->fArrayEventNum;
	  fT2=1;
	}
      else
	{
	  // Should really fill pfHillasT2, and pfHillas2DT2 with 0 here.
	  fT2=0;
	}

      fIndex = pfParTree->GetEntryNumberWithIndex(i,2);      //T3
      if(fIndex>=0)
	{
	  pfParTree->GetEntry(fIndex);
	  *pfHillasT3=*pfHillas;
	  *pfHillas2DT3=*pfHillas2D;
	  fArrayEventNum=pfHillas->fArrayEventNum;
	  fT3=1;
	}
      else
	{
	  // Should really fill pfHillasT3, and pfHillas2DT3 with 0 here.
	  fT3=0;
	}

      fIndex = pfParTree->GetEntryNumberWithIndex(i,3);      //T4
      if(fIndex>=0)
	{
	  pfParTree->GetEntry(fIndex);
	  *pfHillasT4=*pfHillas;
	  *pfHillas2DT4=*pfHillas2D;
	  fArrayEventNum=pfHillas->fArrayEventNum;
	  fT4=1;
	}
      else
	{
	  // Should really fill pfHillasT3, and pfHillas2DT3 with 0 here.
	  fT4=0;
	}
      // *******************************************************************



      // ****************************************************************
      // Check that we got any hillas for this event index
      // ****************************************************************
      if(fArrayEventNum<0)
	{
	  std::cout<<" No pfParTree entries with event Index :"<<i<<std::endl;
	  continue;
	}

     // *******************************************************************
      // Now fill in shower event
      // *******************************************************************
      if(pfShwrTree!=NULL)
	{
	  fIndex = pfShwrTree->GetEntryNumberWithIndex(fArrayEventNum,0);     
	  if(fIndex>=0)
	    {
	      pfShwrTree->GetEntry(fIndex);
	      *pfCShwr=*pfShower;

	      //Aparently we can't dynamically index vectors inside root.
	      //Therefore we do this long and cumbersome method to get the
	      //Impact dist data
	      double fImpactDists[4];
	      int fRcn[4];
	      int fNum=fT1+fT2+fT3+fT4;
	      if(fNum>0)
		{
		  fImpactDists[0]=pfShower->fTelData.fTelImpactDist[0];
		  fRcn[0]=pfShower->fTelData.fIsTelInReconstruction[0];
		}
	      if(fNum>1)
		{
		  fImpactDists[1]=pfShower->fTelData.fTelImpactDist[1];
		  fRcn[1]=pfShower->fTelData.fIsTelInReconstruction[1];
		}
	      if(fNum>2)
		{
		  fImpactDists[2]=pfShower->fTelData.fTelImpactDist[2];
		  fRcn[2]=pfShower->fTelData.fIsTelInReconstruction[2];
		}
	      if(fNum>3)
		{
		  fImpactDists[3]=pfShower->fTelData.fTelImpactDist[3];
		  fRcn[3]=pfShower->fTelData.fIsTelInReconstruction[3];
		}
	      fT1Rcn=0;
	      fT2Rcn=0;
	      fT3Rcn=0;
	      fT4Rcn=0;
	      fT1ImpactDist=-1;
	      fT2ImpactDist=-1;
	      fT3ImpactDist=-1;
	      fT4ImpactDist=-1;

	      int fTel=0;
	      for(int fTelId=0;fTelId<4;fTelId++)
		{
		  fIndex = 
		    pfParTree->GetEntryNumberWithIndex(i,fTel);
		  if(fIndex>=0)		  
		    if(fTelId==0)
		      {
			fT1ImpactDist=fImpactDists[fTel];
			fT1Rcn=fRcn[fTel];
			fTel++;
		      }
		    else if(fTelId==1)
		      {
			fT2ImpactDist=fImpactDists[fTel];
			fT2Rcn=fRcn[fTel];
			fTel++;
		      }		
		    else if(fTelId==2)
		      {
			fT3ImpactDist=fImpactDists[fTel];
			fT3Rcn=fRcn[fTel];
			fTel++;

		      }		
		    else if(fTelId==3)
		      {
			fT4ImpactDist=fImpactDists[fTel];
			fT4Rcn=fRcn[fTel];
			fTel++;
		      }
		}

	      // *********************************************************
	      // Fill in true impact dist.
	      // *********************************************************
	      if(pfSimTree!=NULL)
		{
		  // ******************************************************
		  // we need to get the location of each telecope in the 
		  // mirror plane for each  event. Get event direction 
		  // (mirror plane direction)?
		  double fSourceAz;
		  double fSourceElev;
		  pfConvertCoords->RADec2000ToAzEl(
						   pfShower->fArrayTrackingRA,
						   pfShower->fArrayTrackingDec,
						   pfShower->fTime,
						   fSourceAz,fSourceElev);
		  // *****************************************************
		  //  Now we need the "True" position in the
		  //  mirror plane.
		  // *****************************************************
		  // We now go into the Sim tree to get the True location
		  
		  int fParIndex=pfSimulationEventTree->
		                    GetEntryNumberWithIndex(fArrayEventNum,0);
		  if(fIndex<0)
		    {
		      std::cout<<" Missing entry in Simulation "
			"Event Tree for event Index :"<<i<<std::endl;
		      continue;
		    }
		  
		  pfSimulationEventTree->GetEntry(fIndex);
		  
		  double fXGround=pfSimulation->fCoreEastM;
		  double fYGround=-pfSimulation->fCoreSouthM;
		  //Note sign change
		  
		  // *****************************************************
		  // Project this back to the mirror plane
		  // *****************************************************
		  double fXMirror;
		  double fYMirror;
		  fPlane.PointPlaneBackToPlane(0.0,M_PI/2.0,fXGround,
					       fYGround,
					       0.0,fSourceAz,fSourceElev,
					       fXMirror,fYMirror);
		  
		  GetArrayPositonsInMirrorPlane(pfArrayInfo, fSourceAz, 
						fSourceElev, fArrayX, 
						fArrayY);
		  
		  for(int fTelId=0;fTelId<4;fTelId++)
		    {
		      // *************************************************
		      // Find the impact distance
		      // Now we can determine the Miss distance
		      // *************************************************
		      double fDiffX=(fXMirror- fArrayX.at(fTelId));
		      double fDiffY=(fYMirror- fArrayY.at(fTelId));
		      double fMissDistance=sqrt((fDiffX*fDiffX)+ 
						(fDiffY*fDiffY));
		      *fTrueImpDist[fTelId]=fMissDistance; 
		    }
		}
	    }
	  else
	    {
	      
	      std::cout<<" No pfShwrTree entries with fArrayEventNum :"
		   <<fArrayEventNum<<" at fEventIndex :"<<i<<std::endl;
	    }
	}
      
      // ********************************************************************
      // Now fill Sim event
      // ********************************************************************
        if(pfSimTree!=NULL)
	{
	  fIndex = pfSimTree->GetEntryNumberWithIndex(fArrayEventNum,0);     
	  if(fIndex>=0)
	    {
	      pfSimTree->GetEntry(fIndex);
	      *pfCSim=*pfSimulation;
	    }
	  else
	    {
	  
	  std::cout<<" No pfSimTree entries with fArrayEventNum :"
		   <<fArrayEventNum<<" at fEventIndex :"<<i<<std::endl;
	    }
	}

	// *****************************************************************
	// Save this event into the tree
	// *****************************************************************

	pfCTree->Fill();
    }

  pfCTree->Write();
  std::cout<<"Done!"<<std::endl;
  return 0;
}
// ************************************************************************
void GetArrayPositonsInMirrorPlane(VAArrayInfo* pfArrayInfo, 
				   double fSourceAz, 
				   double fSourceElev, 
				   std::vector< double >& fArrayX, 
				   std::vector< double >& fArrayY) 
{
  // ***********************************************************************
  // Get Array Positions on the ground
  // ***********************************************************************
  VAPlaneToPlane fPlane;
  for (int fTelId=0;fTelId<4;fTelId++)
    {
      double fXGround=pfArrayInfo->telescope(fTelId)->positionEW();
      double fYGround=pfArrayInfo->telescope(fTelId)->positionNS();
      double fZGround=pfArrayInfo->telescope(fTelId)->positionUD();
      double fXMirror;
      double fYMirror;
      fPlane.PointPlaneBackToPlane(0.0,M_PI/2.0,fXGround,fYGround,
				   fZGround,fSourceAz,fSourceElev,
				   fXMirror,fYMirror);
      fArrayX.at(fTelId)=fXMirror;
      fArrayY.at(fTelId)=fYMirror;
    }
  return;
}
// ***************************************************************************


