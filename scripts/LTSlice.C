TH2F* VALTPlot::pfHisto=NULL;
TH2F* VALTPlot::pfHisto=NULL;
TFile* VALTPlot::pfFile=NULL;
VALookupTable* VALTPlot::pfTable=NULL;

void widthTest(string filename, string table, float impactDist,string option, 
	       int color,string pname=" ")
{
  pfFile = new TFile(filename.c_str(), "read");
  //  cout<<"file add: "<<pfFile<<endl;
  TDirectory *pfDir = NULL;
  pfDir = (TDirectory*)pfFile->Get("tables");
  if(pfDir==NULL){
    cerr<<"Couldn't get tables directory"<<endl;
    return;
  }

  pfTable = (VALookupTable*)pfDir->Get(table.c_str());
  if(pfTable==NULL){
    cerr<<"unable to load table"<<endl;
    return;
  }
  pfHisto = pfTable->pfTableHistogram; 
  if(pfHisto==NULL){
    cerr<<"unable to load histogram"<<endl;
    return;
  }

  pfHisto->SetDirectory(0);
  cout<<"Entries: "<<pfHisto->GetEntries()<<endl;

  int impactDistBin = pfHisto->GetYaxis()->FindBin(impactDist);
  int numBins       = pfHisto->GetXaxis()->GetNbins();
  double lowEdge    = pfHisto->GetXaxis()->GetBinLowEdge(1);
  double highEdge   = pfHisto->GetXaxis()->GetBinLowEdge(numBins);
  highEdge          = highEdge+ pfHisto->GetXaxis()->GetBinWidth(numBins);
  TH1F* pSlice      = new TH1F("slice","slice",numBins,lowEdge,highEdge);

  for(int i=1;i<=numBins;i++){
    double value=pfHisto->GetBinContent(i,impactDistBin);
    pSlice->SetBinContent(i,value);
  }
  pSlice->SetDirectory(0);
  
  ostringstream os;
  os<<pname<<" at Impact Dist:"<<impactDist<<"m";
  string ptit=os.str();

  pSlice->SetTitle(ptit.c_str());
  if(option==" "){
    pSlice->Draw();
  }
  else{
    pSlice->SetLineColor(color);
    pSlice->Draw(option.c_str());
  }
  return;
}
  
