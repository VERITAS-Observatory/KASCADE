//-*-mode:c++; mode:font-lock;-*-
//Root script to draw the ground grid array for North-South triangular array.
// ************************************************************************

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cmath>
#include "TCanvas.h"
#include "TMath.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TBranch.h"
#include "TCut.h"
#include "TStyle.h"
#include "TChain.h"
#include "TGaxis.h"
#include "TMarker.h"



const int fGridSize=12;
//const double fXSeg=12.00;
//const double fYSeg=13.8;
const double fXSeg=12.77;
const double fYSeg=14.74;
const double fBC_1X=-37.6;
const double fBC_1Y=-23.7;

const double fBC_2X= 44.1;
const double fBC_2Y=-47.7;

const double fBC_3X= 29.4;
const double fBC_3Y= 60.1;

const double fBC_4X=-35.9;
const double fBC_4Y= 11.3;


void ksMakeGrid()
{
  // ********************************************************************
  // Most everything done in constructor.
  // ********************************************************************
  // Size of grid 
  double fGridSizeX=fXSeg*(fGridSize+1);
  double fGridSizeY=fYSeg*(fGridSize);
  double fGridXMin=-(fGridSizeX/2.0);
  double fGridYMin=-(fGridSizeY/2.0);
  
  double fSizeGrid=fGridSizeX;
  if(fGridSizeY>fSizeGrid)fSizeGrid=fGridSizeY;
  
  // Now we can draw the area into the canvas
  TCanvas* pfCanvas = new TCanvas("pfCanvas","North South Grid",700,700);
  pfCanvas->cd();
  pfCanvas->DrawFrame(fGridXMin,fGridYMin,fGridXMin+fSizeGrid,
		      fGridYMin+fSizeGrid);
  
  //Find min an number of x and y grid points.
  int fMinNX=(fGridXMin/fXSeg);
  int fMaxNX=(fGridXMin+fSizeGrid)/fXSeg;
  
  int fMinNY=(fGridYMin/fYSeg)-.5;
  int fMaxNY=(fGridYMin+fSizeGrid)/fYSeg;
  
  //Draw thew grid centers
  for(int i= fMinNX;i<fMaxNX+1;i++)
    {
      double fX=fXSeg*i;
      TLine* pfLine = new TLine(fX-fXSeg/2,fGridYMin,fX-fXSeg/2
				,fGridYMin+fSizeGrid);
      //pfLine->SetLineWidth(2);
      pfLine->Draw();

      for(int j= fMinNY;j<fMaxNY+1;j++)
	{
	  double fY=0;
	  if(i%2==0)
	    {
	      fY=fYSeg*j;
	    }
	  else
	    {
	      fY=fYSeg*(j+.5);
	    }
	  TMarker* pfGrid = new TMarker(fX,fY, 7);
	  pfGrid->SetMarkerSize(1);
	  pfGrid->SetMarkerColor(4);
	  pfGrid->Draw();
	  TLine* pfLine = new TLine(fX-fXSeg/2,fY+fYSeg/2,fX+fXSeg/2,
				    fY+fYSeg/2);
	  //pfLine->SetLineWidth(2);
	  pfLine->Draw();

	  // Heres where things get a little squirly. In KASCADE x is + East
	  // and y is +South. So we need to place the indicies accordingly

	  TLatex fLatexLable;
	  fLatexLable.SetTextAlign(22);
	  fLatexLable.SetTextSize(.01);
	  char fLable[80];
	  sprintf(fLable,"%i,%i\n",i,j);
	  fLatexLable.DrawLatex(fX,-fY-.25*fYSeg, fLable);
	}

    }

  //For KASCADE Y= + South, for Vegas y= +North
  TMarker* pfTel1    = new TMarker(fBC_1X,fBC_1Y, 24);
  pfTel1->SetMarkerSize(3);
  pfTel1->SetMarkerColor(4);
  pfTel1->Draw();
  TMarker* pfTel2    = new TMarker(fBC_2X,fBC_2Y, 24);
  pfTel2->SetMarkerSize(3);
  pfTel2->SetMarkerColor(4);
  pfTel2->Draw();
  TMarker* pfTel3    = new TMarker(fBC_3X,fBC_3Y, 24);
  pfTel3->SetMarkerSize(3);
  pfTel3->SetMarkerColor(4);
  pfTel3->Draw();
  TMarker* pfTel4    = new TMarker(fBC_4X,fBC_4Y, 24);
  pfTel4->SetMarkerSize(3);
  pfTel4->SetMarkerColor(4);
  pfTel4->Draw();

  pfCanvas->Update();
  return;
}
