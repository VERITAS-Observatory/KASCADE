void DrawR10560()
{
  TTree A;
  TTree B;
  TTree C;
  TTree D;
  TTree E;
  A.ReadFile("R10560Qeff.txt");
  B.ReadFile("xp2970Qeff.txt");
  C.ReadFile("PurdueR10560Qeff.txt");
  D.ReadFile("CareR10560Qeff.txt");
  E.ReadFile("AkiraR10560.txt");

  B.SetLineColor(6);
  C.SetLineColor(2);
  D.SetLineColor(3);
  E.SetLineColor(4);

  A.Draw("qe*100:lambda","1","L");
  B.Draw("qe*100:lambda","1","Lsame");
  C.Draw("qe:lambda","1","Lsame");
  D.Draw("qe*100:lambda","1","Lsame");
  E.Draw("qe:lambda","1","Lsame");

}
