
void set_palette(int option = 5)
{
  const int n=128;
  int colors[n];
  double r=0, g=0, b=0;
  double nor, n2;
  const double t1=64, gamma = 0.85;
  int offset = 0;
  
  double step1 = 30.;
  double step2 = 60.;
  double step3 = 90.;
  double step4 = 120;
  
  for(int i=0; i<n; i++) {
    
    switch(option) {
    case 1: 
      if(i < t1) {
	r=i/t1;
	g=0.;
	b=0.2*(t1-i)/t1;
      } else {
	r=1.;
	g=(i-t1)/(n-t1);
	b=0;
      }
      offset = 800;
      break;
    case 2: 
      nor=i/((double)n);
      n2 = nor*2.0;
      if(nor <= 0.0) {
	r=g=b=0.0;
      } else if (n2<1) {
	b = TMath::Power(n2,0.4*gamma);
	r = TMath::Power(n2,0.8*gamma);
	g = TMath::Power(n2,0.6*gamma);
      } else {
	n2-=1;
	b = TMath::Power(1.0,0.4*gamma) - TMath::Power(n2,0.8*gamma);
	r = TMath::Power(1.0,0.8*gamma);
	g = TMath::Power(1.0,0.6*gamma) - TMath::Power(n2,0.8*gamma);
      }
      offset = 600;
      break;
    case 3: 
      if (i < 32) {
	r = 0. + 0.*i/32.;
	g = 0.;// + 0.1*i/32.;
	b = 0.25 + 0.3*i/32;
      }
      else if (i <= 64) {
	r = 0. + 0.3*(i-32)/32.;
	g = 0. + 0.1*(i-32)/32;
	b = 0.55 + 0.3*(i-32)/32.;
      }
      else if (i <= 96) {
	r = 0.3 + 0.7*(i-64)/32.;
	g = 0.1 - 0.1*(i-64)/32.;
	b = 0.85 - 0.85*(i-64)/32.;
      }
      else {
	r = 1.;
	g = (i-96)/32.;
	b = 0.;
      }
      offset = 400;
      break;
    case 4: // black-white
      b = 1.0 - i/((double)n);
      r = b;
      g = b;
      offset = 1000;
      break;
    case 5:
      if (i <= step1) {
	r = 0. + 0.*i/step1;
	g = 0.;// + 0.1*i/32.;
	b = 0.25 + 0.3*i/step1;
      } else if (i <= step2) {
	r = 0. + 0.3*(i-step1)/(step2-step1);
	g = 0. + 0.1*(i-step1)/(step2-step1);
	b = 0.55 + 0.3*(i-step1)/(step2-step1);
      } else if (i <= step3) {
	r = 0.3 + 0.7*(i-step2)/(step3-step2);
	g = 0.1 - 0.1*(i-step2)/(step3-step2);
	b = 0.85 - 0.85*(i-step2)/(step3-step2);
      } else if (i <= step4) {
	r = 1.;
	g = (i-step3)/(step4-step3);
	b = 0.;
      } else {
	r = 1.;
	g = 1.;
	b = (i-step4-1)/(n-step4);
      }
      offset = 1200;
      break;

    default:
      //
    break;
    
    }
    
    new TColor(offset+i,r,g,b,"user colour");
    colors[i]=offset+i;
  }
  
  gStyle->SetPalette(n, colors);
}

