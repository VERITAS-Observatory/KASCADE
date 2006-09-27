#include <iostream>

class RandomNumbers
{

public:
  RandomNumbers( char* );
  ~RandomNumbers();
  double Uniform();
  double Exponential();
  double Normal();
  double Gamma( int );
  int Poisson( double );
  int Binomial( double , int );
  //  double PMTpe(int type=0);

private:
  RandomNumbers( const RandomNumbers & );
  RandomNumbers operator=( const RandomNumbers & );

  void ran2_read();
  void ran2_write();
  void ran2_init( long* );
  double ran2();
  double gammln( double );

  static const int NTAB=32;
  long ran2_idum1;
  long ran2_idum2;
  long ran2_iy;
  long ran2_iv[NTAB];

  char* pFileName;

};

