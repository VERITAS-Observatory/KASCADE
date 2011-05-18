void MakeMDLPlots(string txtFile)
{
  TTree A;
  A.ReadFile(txtFile.c_str());
  A.SetMarkerStyle(20);
  A.Draw("Max3_50:T*G");
  c1->Clear();
  c1->Divide(2,2);
  c1->cd(1);

  A.Draw("Max3_50:T*G","t==1");
  A.SetMarkerColor(2);
  A.Draw("Max3_50:T*G","t==2","same");
  A.SetMarkerColor(3);
  A.Draw("Max3_50:T*G","t==3","same");
  A.SetMarkerColor(4);
  A.Draw("Max3_50:T*G","t==4","same");

  c1->cd(2);
  
  A.SetMarkerColor(3);
  A.Draw("R:Eff/T","t==3");
  A.SetMarkerColor(1);
  A.Draw("R:Eff/T","t==1","same");
  A.SetMarkerColor(2);
  A.Draw("R:Eff/T","t==2","same");
  A.SetMarkerColor(4);
  A.Draw("R:Eff/T","t==4","same");

  c1->cd(3);

  A.SetMarkerColor(1);
  A.Draw("PVar:sqrt(N)*G","t==1");
  A.SetMarkerColor(2);
  A.Draw("PVar:sqrt(N)*G","t==2","same");
  A.SetMarkerColor(3);
  A.Draw("PVar:sqrt(N)*G","t==3","same");
  A.SetMarkerColor(4);
  A.Draw("PVar:sqrt(N)*G","t==4","same");

  c1->cd(4);

  A.SetMarkerColor(1);
  A.Draw("S:(T*G)/sqrt(N)","t==1");
  A.SetMarkerColor(2);
  A.Draw("S:(T*G)/sqrt(N)","t==2","same");
  A.SetMarkerColor(3);
  A.Draw("S:(T*G)/sqrt(N)","t==3","same");
  A.SetMarkerColor(4);
  A.Draw("S:(T*G)/sqrt(N)","t==4","same");

}

