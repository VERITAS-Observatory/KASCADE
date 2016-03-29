
TTree* pLGTree;


void TestLG(string LGTestFileName)
{
  pLGTree = (TTree*) new TTree();
  pLGTree->ReadFile(LGTestFileName.c_str(),"TraceNMum/I:HGSize/F:sHGSize:"
					"sLGSize:NPES:sTMPLT:sLin:HiLoratrat:sHILORatio:samplSum:"
					"LGMaxSmpl:satFlag/I:tmpltAmp/F:tmpltLin:fitStatus/I");
 
  pLGTree->Draw("sLin:tmpltLin","fitStatus!=4","box");
  c1->Clear();
  c1->Divide(2,2);

  c1->cd(1);
  pLGTree->SetFillColor(2);
  pLGTree->Draw("(1/sLin):(1/tmpltLin)","fitStatus == 0 && satFlag==1 ","box");

  c1->cd(2);
  pLGTree->Draw("HGSize:sHGSize","(fitStatus == 0 &&  satFlag==1 )");

  c1->cd(3);
  pLGTree->Draw("(HGSize-sHGSize)/sHGSize","(fitStatus ==0 &&  satFlag==1)");

  c1->cd(4);
  pLGTree->Draw("HGSize/sHGSize","(fitStatus == 0 && satFlag==1)");
}




