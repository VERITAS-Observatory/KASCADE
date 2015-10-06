TH1D wave1("wave1","wave1",400,0.0,100.0);
TH1D wave2("wave2","wave2",400,0.0,100.0);
TH1D wave3("wave3","wave3",400,0.0,100.0);

TH1D FADC1("FADC1","FADC1",50,0.0,100.0);

void FillWaveForm(string fileName, TH1D* waveForm)
{
  ifstream stream1(fileName.c_str());
  double sample;
  int j=0;
  for ( int i=0;i<=400; i++) {
    waveForm->SetBinContent(i,0.0);
  }
  for ( int i=0;i<=400; i++) {
    if(! stream1.eof()) {
      stream1>>sample;
      waveForm->SetBinContent(i,sample);
    }
  }
}
