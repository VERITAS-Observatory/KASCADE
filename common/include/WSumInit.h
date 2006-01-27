#ifndef WSUMINIT_H
#define WSUMINIT_H
// WSum_init.h 
// Include file for determining weights for events in function 
// WhippleEventWeight in file Veritas.cpp. For kasaomega.
// From file sum_init.kumac

//For Mount Hopkins Whipple 10m 490 pixel telescope.
//18/02/04 GHS
//Update to lastest proton spectra from: Weibel-Smith et.al. Astronomy and 
//Astrophysics, 300(1), pg.389-398,Feb. 1, 1998
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//	Gammas
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

//	New gamma flux parameters from Dave Lewis 15/5/96
float gAlpha = -2.45;		//Spectral index for gammas.
float gPhi0 = 7.16e-3;		//Spectral Amplitude for gammas
                                //Units=/m**2/sec/GeV
float gAreaSolid = 166.28;	//Area(m**2) for gridpoint for VERITAS
float gSparseTeV = 100.0;	//No sparse grid for VERITAS

//As of 18/02/04
float gGeV[18]=   {  84,  100,  120,  143,  172,  205,  246,  294, 353, 422,
                    505,  605,  867, 1243, 1781, 2553, 3660, 5246};
int gNshowers[18]={  50,   50,   50,   50,   50,   50,   50,   50,  54,  30,
                     30,   19,   37,   11,   10,    6,    6,    6};
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//   Protons
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Flux parameters As of Feb. 1 1998:See Wiebel-Sooth ref above
float pAlpha = -2.77;		//Spectral index for protons.
float pSparseTeV = 100.0;	//No sparse grid for VERITAS
//As of 15/10/03
float pGeV[24]=     {   20,   24,   28,   34,   41,    49,   58,   70,   84, 
                       100,  120,  143,  205,  294,   422,  605,  867, 1243, 
                      1781, 2553, 3660, 5246, 7519, 10777};
int pNshowers[24]= { 13800, 8300, 5400, 6400, 2800,  2900, 1430, 1750,  775,
                       800,  480,  385,  584,  165,   140,   55,   35,   16,
                        10,   10,   10,   10,   10,    10};

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//   He4
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Flux parameters As of Feb. 1 1998:See Wiebel-Sooth ref above
float he4Alpha =  -2.64;		//Spectral index for Helium
float he4SparseTeV = 100.0;		//No sparse grid for VERITAS
float he4GeV[19]= {     49, 58,  70,  84, 100, 120, 143, 205, 294, 422,
		       605,867,1243,1781,2553,3660,5246,7519,10777};

int he4Nshowers[19]= {2500,1590,1630,860, 800, 550, 400, 680, 200, 175,
		       70,  50,  25,  15,  10,  10,  10,  10,  10};

#endif //WSUM_INIT_H
