#ifndef VSUMINIT_H
#define VSUMINIT_H
// VSum_init.h 
// Include file for determining weights for events in function 
// VeritasEventWeight in file Veritas.cpp. For kasaomega.
// From file sum_init.kumac

//For Horseshoe Canyon (Kitt Peak) VERITAS 499 telescope.
//15/10/03 GHS
//Update to lastest proton spectra from: Weibel-Smith et.al. Astronomy and 
//Astrophysics, 300(1), pg.389-398,Feb. 1, 1998
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//	Gammas
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

//	New gamma flux parameters from Dave Lewis 15/5/96
float gAlpha = -2.45;		//Spectral index for gammas.
float gSparseTeV = 100.0;	//No sparse grid for VERITAS
//As of 18/04/01
float gGeV[26]=   {  20,   24,   28,   34,   41,   49,  58,  70,  84, 100,
	            120,  143,  172,  205,  246,  294, 353, 422, 505, 605, 
	            867, 1243, 1781, 2553, 3660, 5246 };
int gNshowers[26]={ 100,  100,  100,  100,  100,  100, 100, 100,  75,  75,
                     50,   50,   50,   50,   30,   30,  30,  30,  30,  30,
                     30,   20,   20,   10,   10,   10 };
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//   Protons
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Flux parameters As of Feb. 1 1998:See Wiebel-Sooth ref above
float pAlpha = -2.77;		//Spectral index for protons.
float pSparseTeV = 100.0;	//No sparse grid for VERITAS
//As of 15/10/03
float pGeV[22]=     {   20,   24,   28,   34,   41,   49,  58,  70,  84,
                      100,  120,  143,  172,  205,  246, 294, 353, 422,
		      505,  605,  867, 1243 };
int pNshowers[22]= { 5000, 3000, 2000, 2300, 1000, 1050, 525, 625, 275, 
                      373,  200,  200,  200,  200,  100, 100, 100, 100,
		       75,   50,   50,   30 };

#endif //VSUM_INIT_H
