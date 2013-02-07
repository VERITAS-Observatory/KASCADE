{
VARootIO io("", true);
io.loadTheRootFile();
VAQStatsData *q = io.loadTheQStatsData();
std::ofstream ofs("PedVarBaseRatios.dat");
std::vector< double > ped;
ped.Resize(4,0.0);
ped.at(0)=q->getCameraAverageTraceVarTimeIndpt(0,7)
ped.at(1)=q->getCameraAverageTraceVarTimeIndpt(1,7)
ped.at(2)=q->getCameraAverageTraceVarTimeIndpt(2,7)
ped.at(3)=q->getCameraAverageTraceVarTimeIndpt(3,7)
double max= *max_element(ped.begin(),ped.end());
ofs<<ped.at(0)<<" "<<ped.at(1)<<" ";
   <<ped.at(2)<<" "<<ped.at(3)<<" ";
   <<max<<std::endl;
io.closeTheRootFile();
}
