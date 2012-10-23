#include <iostream>
#include <iomanip>
using namespace std;

void GenerateMoonFilterDatatxt()
{
  int lambda;
  double t;
  TTree MoonFilter;
  int numEntries=MoonFilter.ReadFile("MoonFilter.txt");
  MoonFilter.SetBranchAddress("lambda",&lambda);
  MoonFilter.SetBranchAddress("t",&t);
  
  cout<<"	DATA	MoonFilter2012/"<<endl;

  cout<<"     1   "<<"0.000,0.000,0.000,0.000";
  int count=4;
  int Lbda=200;
  int Llow=180;
  cout.precision(3);
  cout<<fixed;
  for(int i=0;i<numEntries;i++){
    MoonFilter.GetEntry(i);
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
