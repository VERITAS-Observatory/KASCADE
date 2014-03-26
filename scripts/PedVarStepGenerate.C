void PedVarSteps()
{
  double firstStep=.68;  
  double pedvar=4.04;
  double ratio=sqrt((pedvar+firstStep)/pedvar);
  cout<<"Ratio: "<<ratio<<setprecision(4)<<endl;
  std::cout<<pedvar<<" ";
  for( int i=0;i<8;i++){
    pedvar=pow(  sqrt(pedvar)*ratio ,2);
    std::cout<<setprecision(5)<<pedvar<<" ";
  }
  cout<<endl;
}
