#include <iostream>
#include <iomanip>
using namespace std;

void GenerateDatatxt(string fileName,string dat)
{
  int lambda;
  double t;
  TTree Data;
  string DataFileName=fileName+".txt";
  int numEntries=Data.ReadFile(DataFileName.c_str());
  Data.SetBranchAddress("lambda",&lambda);
  Data.SetBranchAddress(dat.c_str(),&t);
  
  cout<<"       DATA    "<<fileName<<"/"<<endl;
  cout<<"     1   ";
  int count=0;
  int Lbda=180;
  int Llow=180;
  cout.precision(3);
  cout<<fixed;
  for(int i=0;i<numEntries;i++){
    Data.GetEntry(i);
    if(lambda==Lbda){
      if(count!=0){
	cout<<","<<t;
      }
      else{
	cout<<t;
      }
 
      count++;
      if(count==10 && lambda!=700){
	count=0;
	cout<<",!"<<Llow<<"-"<<lambda<<endl;
	cout<<"     1   ";
	Llow=Lbda+5;
      }
      else if(lambda==700){
	cout<<"/                              !"<<Llow<<"-"<<lambda
	    <<endl;	
        break;
      }
      Lbda=Lbda+5;
    }
  }
}
