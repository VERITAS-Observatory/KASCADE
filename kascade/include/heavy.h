	COMMON /ZEN/ ZNTH
	real znth
	COMMON /CKFRAG/ KODFRAG
	integer kodfrag
c	common/RSEEDS/ ISEED
!	integer iseed
!	common /rseed/ ised
!	integer ised

	COMMON /CLENNN/ SIGNUC(60), ALNUC(60)
	real signuc,alnuc

	integer iamax,iamax2
	parameter (iamax=56)
	parameter (iamax2=3136)

	real b,bmax
	integer ntry,na,nb,ni,nael,nbel,jja,jjb,jjint,jjael,jjbel
        COMMON /CNUCMS/ B, BMAX, NTRY, NA, NB, NI, NAEL, NBEL
     +         ,JJA(IAMAX), JJB(IAMAX), JJINT(IAMAX,IAMAX)
     +         ,JJAEL(IAMAX), JJBEL(IAMAX)

!	COMMON /C1STNC/ XX0(60),XX(60),YY(60),AX(60),AY(60)
!	real xx0,xx,yy,ax,ay

	COMMON /FRAGMENTS/ PPP(3,60)
	real ppp

        COMMON/FRAGMOD/A(10,10,20),AE(10,10,20),ERES(10,10)
     +	,NFLAGG(10,10)
	real a,ae,eres
	integer nflagg

        COMMON /CPROFA/ ZMIN, DZ, BBZ(401,IAMAX)
	real zmin,dz,bbz

        COMMON /CPROF/ DB, BMX, BB(401), TB(401), AAA
	real db,bmx,bb,tb,aaa

        COMMON /CC01/  B_cc01
	real b_cc01

        COMMON /CCDA/ JJAA
	integer jjaa

        COMMON /CWOOD/ RR0(19:56), AA0(19:56), CC0(19:56)
	real rr0,aa0,cc0

        COMMON /CSHELL/ RR0_cs(18), RR02(18)
	real rr0_cs,rr02

        COMMON  /CPROBAB/ PROBA(IAMAX), DPROBA(IAMAX), 
     +   PROBB(IAMAX), DPROBB(IAMAX), PROBI(IAMAX2), DPROBI(IAMAX2),
     +   P1AEL(0:IAMAX),DP1AEL(0:IAMAX),P1BEL(0:IAMAX), DP1BEL(0:IAMAX),
     +   P2AEL(0:IAMAX),DP2AEL(0:IAMAX),P2BEL(0:IAMAX), DP2BEL(0:IAMAX)
	real proba,dproba,probb,dprobb,probi,dprobi,p1ael,dp1ael,p1bel
	real dp1bel,p2ael,dp2ael,p2bel,dp2bel

! GHS 03/07/01: Increas dimension of ssig0,ssiga,alint from (41,2) to (48,2).
!               This is to accomidate lower energies for nucleons.
        COMMON /CSAIR/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +           SSIG0(48,2),SSIGA(48,2),ALINT(48,2)
!     +           SSIG0(41,2),SSIGA(41,2),ALINT(41,2)
	integer nsqs
	real asqsmin,asqsmax,dasqs,ssig0,ssiga,alint

        COMMON /CA0SH/ R0, R02
	real r0,r02

        COMMON /BLOCKC/ AA, BETA, S0, CC, AMU, DD, ALPHA, A0
	real aa,beta,s0,cc,amu,dd,alpha,a0

        COMMON /BLOCKD/ N_CP, DP, EP, CM, N_DM
	integer n_cp,n_dm
	real dp,ep,cm
