// This named script reads in a list of stage2 files names, reads the 
// CameraAverageTraceVarTime (Run SAvergae PedVar) and plots them. This is so 
// that the user can determine which PeVar Lookupt Table to use.

void PlotPedvarLevels(std::string fListFileName,int numSamples=7,double minRange=.1, double maxRange=12.0)
{
  std::cout<<"Pedvars are for NumSamples="<<numSamples<<std::endl;
 
  TH1D* pedVarsT1= new TH1D("T1Pedvars","T1Pedvars",120, minRange, maxRange);
  TH1D* pedVarsT2= new TH1D("T2Pedvars","T2Pedvars",120, minRange, maxRange);
  TH1D* pedVarsT3= new TH1D("T3Pedvars","T3Pedvars",120, minRange, maxRange);
  TH1D* pedVarsT4= new TH1D("T4Pedvars","T4Pedvars",120, minRange, maxRange);
  TH1D* pedVarsAll= new TH1D("AllTelPedvars","AllTelPedvars",120, minRange, maxRange);
  pedVarsT1->SetDirectory(0);
  pedVarsT2->SetDirectory(0);
  pedVarsT3->SetDirectory(0);
  pedVarsT4->SetDirectory(0);
  pedVarsAll->SetDirectory(0);

  //std::cout<<"T1/F:T2/F:T3/F:t4/F"<<std::endl;

  std::ifstream fListIn;
  fListIn.open(fListFileName.c_str());
  std::string inputFileName;
  while(getline(fListIn,inputFileName))
    {
	VARootIO io(inputFileName.c_str(), true);
 	io.loadTheRootFile();
        const VAQStatsData *q = io.loadTheQStatsData();
        std::cout<<inputFileName<<": ";
	std::cout<<q->getCameraAverageTraceVarTimeIndpt(0, numSamples)<<" "
		 <<q->getCameraAverageTraceVarTimeIndpt(1, numSamples)<<" "
		 <<q->getCameraAverageTraceVarTimeIndpt(2, numSamples)<<" "
		 <<q->getCameraAverageTraceVarTimeIndpt(3, numSamples)<<" "
                 <<endl;
	Double_t pedvarT1=q->getCameraAverageTraceVarTimeIndpt(0, numSamples);
	Double_t pedvarT2=q->getCameraAverageTraceVarTimeIndpt(1, numSamples);
	Double_t pedvarT3=q->getCameraAverageTraceVarTimeIndpt(2, numSamples);
	Double_t pedvarT4=q->getCameraAverageTraceVarTimeIndpt(3, numSamples);
	pedVarsT1->Fill(pedvarT1);
	pedVarsT2->Fill(pedvarT2);
	pedVarsT3->Fill(pedvarT3);
	pedVarsT4->Fill(pedvarT4);

	pedVarsAll->Fill(pedvarT1);
	pedVarsAll->Fill(pedvarT2);
	pedVarsAll->Fill(pedvarT3);
	pedVarsAll->Fill(pedvarT4);

	io.closeTheRootFile();
    }
  TCanvas* T1=new TCanvas("T1","T1",300,300,500,500);
  pedVarsT1->Draw();

  TCanvas* T2=new TCanvas("T2","T2",300,300,500,500);
  pedVarsT2->Draw();

  TCanvas* T3=new TCanvas("T3","T3",300,300,500,500);
  pedVarsT3->Draw();

  TCanvas* T4=new TCanvas("T4","T4",300,300,500,500);
  pedVarsT4->Draw();

  TCanvas* AllTels=new TCanvas("AllTels","AllTels",300,300,500,500);
  pedVarsAll->Draw();
  return;
}
