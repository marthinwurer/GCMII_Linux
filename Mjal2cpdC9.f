C DYNAM broken into subroutines
c  *********************************************************************
c  *********************************************************************
c  **
c  ** Model IImac
c  ** Based on GCMII code for IBM RS/6000 computers created at GISS
c  ** Modified to compile under Absoft Pro Fortran 8.0 for MacOS 9/X and
c  ** Pro Fortran 8.2 for MacOS X. Also compiles under IRIX & Linux.  
c  ** Based on MP030bC9
c  **
c  ** CHANGE HISTORY:
c  **
c  ** 09/26/01 Suki identified why we want to use MP030bC9 (SXS)
c  ** 09/26/01 minimal changes to get it to compile (MFS)
c  ** 09/28/01 put in some changes to make IO work (MFS)
c  ** 09/29/01 fixed critical bug with namelist (MFS)
c  ** 10/01/01 works (MFS)
c  ** 11/26/01 put back better stop messages, fixed date printout (MFS)
c  ** 11/27/01 deep ocean, kocean=2 (MFS/GLR)
c  ** 01/11/02 failed attempt to fix things (reverted) (MFS)
c  ** 01/14/02 fixed error message (MFS)
c  ** 01/15/02 fixed restart (MFS/RAR)
c  ** 02/05/02 restart bug fixed (RAR)
c  ** 02/07/02 :oda: -> :ocean: (MFS)
c  ** 02/13/02 OHT multiplier (deleted) (MFS/GLR)
c  ** 02/14/02 trend code, deep ocean fixes (MFS)
c  ** 02/19/02 trend code with debug, fixes (MFS)
c  ** 02/27/02 new trend code, data file trends only (MFS)
c  ** 05/27/02 try old S0X & old trends (MSC)
c  ** 06/03/02 put back mostly old trend code (MSC/MFS)
c  ** 06/07/02 add ktrend to the namelist (MSC/MFS)
c  ** 06/12/02 more trend fixes, path fixes (MFS)
c  ** 06/14/02 found year bug that killed trends (MFS)
c  ** 06/17/02 Andy says keep 1958 as the default for trends (MFS/AAL)
c  ** 06/27/02 trend still don't work, remove (MFS)
c  ** 06/28/02 older radiation, 20 char run numbers (MFS)
c  ** 07/01/02 pass ktrend again to keep it in scope (MFS)
c  ** 07/03/02 allow you to bypass trend code (MFS)
c  ** 07/23/02 Xserve test version (MFS)
c  ** 07/24/02 call to writer when trends change (MFS/RAR)
c  ** 09/18/02 disable calls to writer (MFS)
c  ** 09/26/02 command line version for MacOS X/IRIX (MFS)
c  ** 09/28/02 put in Jeff's RANVAX to replace Power series RANDU (MFS)
c  ** 04/08/04 2nd order version, modular dynamics, diag fixes,
c  **          observed ocean mode, drag preserves wind direction (JAL)
c  ** 04/09/04 merged in changes from MP030b (MFS/JAL)
c  ** 05/06/04 setpath for PC support (MFS)
c  ** 05/07/04 options experiment, works (MFS)
c  ** 05/19/04 climatological change (JAL)
c  ** 05/20/04 eccentricity, axial tilt, and omegat (JAL)
c  ** 05/24/04 removed tolerance for dates (MAC)
c  ** 06/22/04 try xlf, remove Absoft io options, iint to int (MFS)
c  ** 06/24/04 more xlf options (MFS)
c  ** 07/15/04 mrwe_clear to fix PC buffer overflow (MFS)
c  ** 01/27/05 work on German crash and CLOCKS to MCLOCK (MFS)
c  ** 04/07/05 solar trend fix and sync with PC for EdGCM 2.3.6 (MFS)
c  ** 04/26/05 use DLAT to fix bug with 8x10 in RADIA0 (GLR/MAC)
c  ** 07/22/05 fixes for Pro Fortran 9.2 & command line (MFS)
c  ** 07/28/05 fix for surface T(I,J,1)=TH1 bug (JAJ)
c  ** 10/17/05 fix added SHW and SHI from Previp (JAJ)
c  ** 10/27/05 test no stop ssw (MFS/MAC/JAJ)
c  ** 12/19/05 test new lunar frontend (MFS)
c  ** 01/17/06 reverted CLOCKS to support Windows, reverted surface (MFS)
c  ** 03/23/06 fix print to behave on Windows and readme2 (MFS)
c  **
c  ** NOTES:
c  ** Suki redeemed, she gave us MP030bC9 originally because of fixes 
c  ** not present in MP008C9. Since MP030bC9 is stable and MP008C9 is
c  ** no I have to go in and add in the deep ocean mode to MP030bC9.
c  **
c  ** old MP030bC9
c  ** 07/29/99 Applied SGI patch for Read(5) (MFS)
c  ** 08/01/99 IO changes for VAX compadiblity (MAC)
c  ** 08/04/99 redirected standard out to a file for speed (MFS)
c  ** 08/06/99 separate and correctly name montly prt files (MFS)
c  ** 08/06/99 namelist=unit 110, restart=unit 111 (MFS)
c  ** 08/13/99 finally fixed the prt files (MFS)
c  ** 01/08/00 fixed the rsf problem, optimized (MFS)
c  ** 01/09/00 fix to overwrite old output files, need for restart (MFS)
c  ** 03/06/00 allowed model to start with old startup.prt file (MFS)
c  ** 03/06/00 fixed bad read 9 to 111 (MFS)
c  ** 06/05/00 changed to new folder structure (MFS)
c  ** 08/08/00 changed code to use 'ssw.stopgcm' instead of unit 3 (MFS)
c  ** 10/04/00 fixed uset and useslp to output into the oda folder (MFS)
c  ** 10/05/00 changed name of file and some output (MFS)
c  **
c  ** old MP008macC9
c  ** 12/20/00 merged MP030aC9, MP008C9 and MA94CC9 (MAC)
c  ** 12/21/00 start to put together new version (MFS)
c  ** 12/26/00 put back the modifications to MP030bC9 that
c  **   David and I made from 07/27/99-10/04/00 (MFS/DCH/MAC)
c  ** 12/28/00 finish IO modifications (MFS/MAC)
c  ** 12/28/00 updated the error messages (MFS)
c  ** 12/29/00 try to read in old restart files (MFS)
c  ** 01/02/01 hacked istart 300 to read in old files (MFS)
c  ** 01/03/01 hacked acc files to work with old post-processors (MFS)
c  ** 01/03/01 started changes for kocean = 2 (MFS)
c  ** 01/05/01 inserted changes based on differcing MP030bC9 (MFS)
c  ** 01/23/01 changed unit 111 to 109 (originally 9) (MFS)
c  ** 01/25/01 fixed unit 20 to use unformatted (MFS)
c  ** 02/10/01 changed istart=5 to work for paleo runs (gdata) 
c  **          also changes to make deep ocean work (MFS)
c  ** 02/15/01 version given to Jeff (MFS)
c  ** 02/20/01 changed TAUT to 240 (MFS)
c  ** 02/21/01 all istarts distingish kocean = 2 (MFS)
c  ** 03/02/01 fix flux collection (MFS)
c  ** 03/12/01 impliment tauo (RAR)
c  ** 03/13/01 write out tau along with daily restart file (MFS)
c  ** 04/06/01 debug info for ktrend, volcanos are disabled (MFS)
c  ** 04/08/01 pass ktrend explicitly (MFS)
c  ** 04/09/01 try use JYEAR as reference instead of 1958 (MFS)
c  ** 04/12/01 added INPUTFORCINGS, removed stuff from INPUTZ (MFS)
c  ** 04/13/01 use new com file BRFRCmac.com (MFS)
c  ** 04/16/01 eliminate ktrend (MFS)
c  ** 04/16/01 trends finally work again, fixed comments (MFS)
c  ** 04/26/01 variable orbit parameters, eg different length days (MFS)
c  ** 04/27/01 take Reto's advice and change length of DT (MFS)
c  ** 04/30/01 use the new and improved ORBIT subroutine (GLR)
c  ** 04/30/01 modify ORBIT to handle variable orbits (MFS)
c  ** 05/01/01 added distance of orbit in solar units (MFS)
c  ** 05/02/01 fixed namelist to match orbitial parameters (MFS)
c  ** 05/07/01 fixed a few more date bugs (MFS)
c  ** 05/08/01 allow misalligned restarts (MFS)
c  ** 05/09/01 debug orbitial param (MFS)
c  ** 05/10/01 fix ocean bug with variable length years (MFS)
c  ** 05/11/01 more date corrections for JDAY (MFS)
c  ** 05/14/01 pass copies of orbitial param to ORBIT (MFS)
c  ** 05/21/01 problems with S0X (MFS)
c  ** 05/22/01 changed S0X calc to always use real units (MFS)
c  ** 05/24/01 change OHT strength via namelist (GLR)
c  ** 05/25/01 fixed some S0X bugs (MFS)
c  ** 06/15/01 split into Classic and MacOSX versions, this is X (MFS)
c  ** 06/15/01 altered paths to work with Unix (MFS)
c  ** 06/21/01 use randvax to use different random numbers, problems (MFS)
c  ** 06/22/01 fix special regions, don't read in unit 29 (MFS)
c  ** 07/03/01 revered to non-modular dynamics, readded tauo fix (MFS)
c  ** 07/10/01 put back old random numbers (MFS)
c  ** 07/11/01 try using dt multiplier of 4 (MFS)
c  ** 07/13/01 orbital parameter fixes (MFS)
c  ** 07/16/01 updates for Carbon version (MFS)
c  ** 07/23/01 put back Reto's stuff (MFS)
c  ** 07/24/01 IDACC checks for all debug write statements (MFS)
c  ** 07/24/01 put in 9/X switches to quickly change versions (MFS)
c  ** 07/24/01 moved inittrend to be after DAILY0 so JYEAR is right (MFS)
c  ** 07/27/01 updated daily message (MFS)
c  ** 07/31/01 this branch does not have the AIX revisions (MFS)
c  ** 08/02/01 put in Reto's revisions to checkt and other changes (RAR)
c  ** 08/10/01 removed debug write to radiation that filled up disk (MFS)
c  ** 08/21/01 fixed bug with long labels and file names (MFS)
c  ** 08/28/01 removed buggy oht multiplier (MFS)
c  **
c  ** MP008macC9 in Private Beta 1
c  ** 12/20/00 merged MP030aC9, MP008C9 and MA94CC9 (MAC)
c  ** 12/21/00 start to put together new version (MFS)
c  ** 12/26/00 put back the modifications to MP030bC9 that
c  **   David and I made from 07/27/99-10/04/00 (MFS/DCH/MAC)
c  ** 12/28/00 finish IO modifications (MFS/MAC)
c  ** 12/28/00 updated the error messages (MFS)
c  ** 12/29/00 try to read in old restart files (MFS)
c  ** 01/02/01 hacked istart 300 to read in old files (MFS)
c  ** 01/03/01 hacked acc files to work with old post-processors (MFS)
c  ** 01/03/01 started changes for kocean = 2 (MFS)
c  ** 01/05/01 inserted changes based on differcing MP030bC9 (MFS)
c  ** 01/23/01 changed unit 111 to 109 (originally 9) (MFS)
c  ** 09/17/01 few more changes to file names via SWITCH (MFS)
c  ** 09/24/01 file for EdGCM Private Beta 1 (MFS)
c  **
c  *********************************************************************
c  *********************************************************************

C**** MP030bC9 BA94jalC9 MP000C9              1/27/92                         0.1  
C**** basic model II with 1958 Atmosphere and mean strat aerosols (.12)    0.21 
C****   This update includes the correct orbit as well as 5 harmonics      0.22 
C****   for the ocean heat transport.                                      0.23 
C****                                                                      0.24 
C**** Thinner ocean ice with smaller leads: minimal thickness Z=.5m        0.3  
C**** rather than 1m, minimal leads = .06*(1/Z-1/5.) rather than .1/Z.     0.4  
C**** If warm ocean melts ice, it reduces the ice depth.                   0.5  
C**** The mixed layer ocean temperature is not affected by the sensible    0.6  
C**** heat of precipitation and evaporation (mixed layer conserves         0.71 
C**** energy). All computations are done in double precision.              0.8  
C**** subroutines in MB08M9 : PRECIP,GROUND,DAILY,OSTRUC                   0.85 
C****                                                                      0.9  
C**** MP005C9 BA94jalC9 MA94M9                  03/06/90                      0.1  
C****                                                                      0.3  
C**** basic model II (OA,PALMER,AIJL omitted) .5 box longitude shift       0.4  
C**** subroutines in MP005M9: all model routines                           0.5  
***** MA94M9(NewModel) BA94M9(CommonBlock) MA94M9(OldSource)    07/19/91   0.1  
***** OPT(3)                                                               0.2  
***** S35V MOD modified for double precision on work station     2/11/92   0.3  
***** MODEL II but with 1958 Freons and mean strat aerosols (.12)          0.4  
***** corrected line 6361.5 (diag) on 2/2/93                               0.5  

c      INCLUDE 'BA94jalC9.COM'                                              1.

C**** COMMON BLOCK (B35V)  FINE MODEL II DBLE.PREC. RS6000 10/29/91
C**** TO CHANGE THE GRID, MODIFY THE NEXT LINE ONLY
      PARAMETER (IM=36,JM=24,LM=9, KTD=6,KAIJ=80,KAJK=50)
C**** IM,JM,LM LIMITED TO 72,46,36 RESPECTIVELY BY RADCOM & SIGmas
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 LHE,LHM,LHS,KAPA,LAT,lat_dg
C**** THERE ARE 100 INTEGER PARAMETERS IN COMMON (JC-ARRAY)
      COMMON /IPARMB/IM0,JM0,LM0,JMM1,LMM1,   LS1,LTM,LBLM,LMCM,LSSM,
     *  KOCEAN,KDISK,KEYCT,KACC0,KCOPY,  IRAND,IJRA,MFILTR,NDYN,NCNDS,
     *  NRAD,NSURF,NGRND,NFILTR,NDAA,   NDA5D,NDA5K,NDA5S,NDA4,NDASF,
     *  MLAST,MDYN,MCNDS,MRAD,MSURF,    MDIAG,MELSE,MODRD,MODD5K,MODD5S,
     *  IYEAR,IDAY,IDAY0,JYEAR,JYEAR0,  JDAY,JDATE,JDATE0,NSTEP,MRCH,
     *  IDUM(4),NDZERO(13),NDPRNT(13),  IJD6(2,4),IDACC(12)
C**** THERE ARE 161 REAL NUMBERS IN COMMON (RC-ARRAY)
      COMMON /RPARMB/
     *  TAU,TAU0,TOFDAY,TOFDY0,DT,      TAUP,TAUI,TAUE,TAUT,TAUO,
     *  TWOPI,SDAY,LHE,LHM,LHS,         RADIUS,GRAV,RGAS,KAPA,OMEGA,
     *  CCMCX,ETA,S0X,CO2,SRCOR,        PTOP,PSF,PSL,PTRUNC,AREAG,
     *  XCDNST(2),XINT,DLAT,DLON,       SKIPSE,USESLP,USEP,USET,FIM,
     *  RSDIST,SIND,COSD,DOPK,SIG(36),SIGE(37),RDM2(44)
      CHARACTER*4 XLABEL,NAMD6,JMONTH,JMNTH0
      COMMON /TEXT/ XLABEL(33),NAMD6(4),JMONTH,JMNTH0
      COMMON /GEOMCB/ RAPVS(JM),RAPVN(JM),RAVPS(JM),RAVPN(JM),F(JM),
     *  DXYP(JM),DXP(JM),DYP(JM),DXYS(JM),SINP(JM),LAT(JM),
     *  DXYV(JM),DXV(JM),DYV(JM),DXYN(JM),COSP(JM),COSV(JM),lat_dg(jm,2)
      COMMON /BNDYCB/ FDATA(IM,JM,3),ODATA(IM,JM,5),GDATA(IM,JM,14),
     *  BLDATA(IM,JM,8),VDATA(IM,JM,10),
     *  Z1O(IM,JM),Z12O(IM,JM)
      COMMON /RADNCB/ RQT(IM,JM,3),SRHR(IM,JM,LM+1),TRHR(IM,JM,LM+1)
      COMMON /LAYACB/ DSIG(37),DSIGO(36)
      DIMENSION U(IM,JM,LM),V(IM,JM,LM),T(IM,JM,LM),P(IM,JM),Q(IM,JM,LM)

C**** DIAGNOSTIC ARRAYS
      PARAMETER (IMH=IM/2,     KACC=JM*80*3 + 24*80 +
     *   JM*3 + JM*LM*54 + JM*3*4 + IM*JM*KAIJ +
     *   IM*LM*16                      + 20*100 +
     *   JM*36 + (IMH+1)*20*8 + 8*2 + 24*50*4 +
     *   2*62*10*12 + JM*LM*KAJK + IM*JM*LM*6)
      COMMON/ACCUM/AJ(JM,80),BJ(JM,80),CJ(JM,80),DJ(24,80),
     *  APJ(JM,3),AJL(JM,LM,54),ASJL(JM,3,4),AIJ(IM,JM,KAIJ),
     *  AIL(IM,LM,16),ENERGY(20,100),
     *  CONSRV(JM,36),SPECA((IMH+1),20,8),ATPE(8,2),ADAILY(24,50,4),
     *  WAVE(2,62,10,12),AJK(JM,LM,KAJK),AIJK(IM,JM,LM,6),
     *  TSFREZ(IM,JM,2),TDIURN(IM,JM,KTD)
      COMMON/REGION/JREG(IM,JM)
      COMMON/KEYS/KEYNR(42,50),KDIAG(12)
c  ** MFS (ADDED)
      INCLUDE 'FORCINGSmac.COM'
c  ** extra ocean arrays for deep ocean
      COMMON/OCN/TG3M(IM,JM,12),RTGO(IM,JM,LM),STG3(IM,JM),               1.1  
     *  DTG3(IM,JM)                                                       1.2 
c  ** END (ADDED)
      COMMON U,V,T,P,Q                                                     2.   
            COMMON/WORKO/OA(IM,JM,11)                      
C****                                                                      7.12 
C****       DATA SAVED IN ORDER TO CALCULATE OCEAN TRANSPORTS              7.13 
C****                                                                      7.14 
C****       1  ACE1I+SNOWOI  (INSTANTANEOUS AT NOON GMT)                   7.15 
C****       2  TG1OI  (INSTANTANEOUS AT NOON GMT)                          7.16 
C****       3  TG2OI  (INSTANTANEOUS AT NOON GMT)                          7.17 
C****       4  ENRGP  (INTEGRATED OVER THE DAY)                            7.18 
C****       5  SRHDT  (INTEGRATED OVER THE DAY)                            7.19 
C****       6  TRHDT  (FOR OCEAN, INTEGRATED OVER THE DAY)                 7.2  
C****       7  SHDT  (FOR OCEAN, INTEGRATED OVER THE DAY)                  7.21 
C****       8  EVHDT  (FOR OCEAN, INTEGRATED OVER THE DAY)                 7.22 
C****       9  TRHDT  (FOR OCEAN ICE, INTEGRATED OVER THE DAY)             7.23 
C****      10  SHDT  (FOR OCEAN ICE, INTEGRATED OVER THE DAY)              7.24 
C****      11  EVHDT  (FOR OCEAN ICE, INTEGRATED OVER THE DAY)             7.25 
C****                                                                      7.26 

c  ** MFS (ADDED)
c  ** New variables required by modified code.  Most of these are used for
c  ** file IO.
         CHARACTER fName*80
         INTEGER*4 IOErr
c        INTEGER*4 JREG(IM,JM)
c  ** END (ADDED)

      CHARACTER C*4,CYEAR*4,CMMND*80                                       8.   
      DIMENSION C(39),JC(100),RC(161)                                      8.3  
      EQUIVALENCE (JC,IM0),(C,XLABEL,LABEL1),(RC,TAU)                      8.4  
c  ** MFS (CHANGED)
c      CHARACTER*8 LABSSW,LABEL1,OFFSSW/'XXXXXXXX'/                         8.5  
      CHARACTER*8 LABSSW,OFFSSW/'XXXXXXXX'/,LABEL1*21                         
c  ** END (CHANGED)
      LOGICAL EVENT                                                        9.   
C**** STATEMENT FUNCTION CONVERTING HOURS TO INTERNAL TIME UNITS           9.5  
      INTFX(XTAU)=INT(XTAU*XINT+.5)                                       10.   
C**** STATEMENT FUNCTION EVENT IS TRUE IF TAU IS A MULTIPLE OF XTAU       10.5  
      EVENT(XTAU)=MOD(ITAU,INTFX(XTAU)).LT.IDTHR                          11.   

c  ** MFS (ADDED)
c  ** use setpath get get the right folder
      CALL SETPATH
c  ** set xlf options to use old style namelists (XLF only!)
c      CALL SETRTEOPTS('namelist=old')
c  ** Writes some helpful information into the MWRE Window and then 
c  ** redirects all further output to a prt file.  The first prt file
c  ** is called Startup.prt because I don't know the day/month/year 
c  ** until the rundeck is read.
      CALL README
c  ** 9/X (SWITCH)
c      OPEN(6,FILE=':prt:Startup.prt',FILETYPE='TEXT',CREATOR='R*ch')
C      OPEN(6,FILE='./prt/Startup.prt',FILETYPE='TEXT',CREATOR='SSPP')
      OPEN(6,FILE='./prt/Startup.prt')
c      OPEN(6,FILE='./Startup.prt')
c  ** END (SWITCH)
      REWIND 6    
      WRITE(6,*) '            **** Model II Debug ****            '
      WRITE(6,*) 'Debug information from startup until after the'
      WRITE(6,*) 'restart file is read in and TAU is established.'
      WRITE(6,*) '                                               '
c  ** END (ADDED)

      CALL CLOCKS (MNOW)                                                  12.   
      MBEGIN=MNOW                                                         12.5  
      CALL INPUT                                                          17.   
      WRITE (3) OFFSSW                                                    18.   
      REWIND 3                                                            18.3  
      MSTART=MNOW+MDYN+MCNDS+MRAD+MSURF+MDIAG+MELSE                       19.   
C**** INITIALIZE TIME PARAMETERS                                          21.   
      HR24=24.                                                            21.1  
      HR12=12.                                                            21.2  
      DTHR=DT/3600.                                                       22.   
      IDTHR=INTFX(DTHR)                                                   23.   
      I24=INTFX(HR24)                                                     27.   
      NSTEP0=.5+TAUI/DTHR                                                 28.   
      NSTEP=INT(.5+TAU/DTHR)-NSTEP0                                       29.   
      ITAU=(NSTEP+NSTEP0)*IDTHR                                           30.   
      TAU=DFLOAT(ITAU)/XINT                                               31.   
      IDAY=1+ITAU/I24                                                     32.   
      TOFDAY=(ITAU-(IDAY-1)*I24)/XINT                                     33.   
C     TAUERR=.5*DTHR                                                      33.5  
      TAUERR=0.                                                           33.501
      CALL DAILY0                                                         34.   
      CALL CLOCKS (MLAST)                                                 35.   
      MINC=MNOW-MLAST                                                     36.   
      MELSE=MELSE+MINC                                                    37.   
      PERCNT=100.*MELSE/(MSTART-MLAST)                                    38.   
      WRITE (6,'(A,13X,A,I6,A,F6.2,I6,A5,I27,I7,F7.1,A,F11.2)')           39.
     *  '0CLIMATE MODEL STARTED UP','DAY',IDAY,', HR',TOFDAY,             39.1
     *   JDATE,JMONTH,MINC,MELSE,PERCNT,' TAU',TAU                        39.2
      DOPK=1.                                                             40.   
         MODD5K=1000                                                      41.   
      CALL CHECKT (1)                                                     41.4  
CF       PALMER(36,1)=-1.                                                 41.5  
      RUNON=-1.                                                           41.6  
      IF(TAU+TAUERR.GE.TAUE) GO TO 630                                    42.   
CF       PALMER(36,1)=1.                                                  42.5  
      RUNON=1.                                                            42.501
      RUNON=1.                                                            42.6  
C****                                                                     43.   
C**** MAIN LOOP                                                           44.   
C****                                                                     45.   
   98 CONTINUE                                                            45.5  
            IF(USET.LE.0.) GO TO 100                                      46.   
            IF(.NOT.EVENT(USET)) GO TO 100                                46.005
C****       ZERO OUT INTEGRATED QUANTITIES EVERY USET HOURS               46.01 
            DO 99 K=4,11                                                  46.02 
            DO 99 J=1,JM                                                  46.03 
            DO 99 I=1,IM                                                  46.04 
   99       OA(I,J,K)=0.                                                  46.05 
  100 IF(.NOT.EVENT(TAUT)) GO TO 200                                      46.06 
C**** WRITE RESTART INFORMATION ONTO DISK                                 47.   
  120 CALL RFINAL (IRAND)                                                 48.   
      REWIND KDISK                                                        49.   
      WRITE (KDISK) TAU,JC,C,RC,KEYNR,U,V,T,P,Q,ODATA,GDATA,BLDATA,       50.   
c  ** MFS (CHANGED)
c  ** write out deep ocean arrays as well as normal arrays
c  ** Reto says this is the correct write statment for istart=10
c     *  RQT,SRHR,TRHR,TSFREZ,(AJ(K,1),K=1,KACC),TDIURN,OA,TAU             51.   
     *  RQT,SRHR,TRHR,TSFREZ,TG3M,RTGO,STG3,DTG3,                         51.   
     *  (AJ(K,1),K=1,KACC),TDIURN,TAU                                     51.1  
c  ** END (CHANGED)
      REWIND KDISK                                                        52.   
      CALL CLOCKS (MNOW)                                                  53.   
      MINC=MLAST-MNOW                                                     54.   
      MELSE=MELSE+MINC                                                    55.   
      PERCNT=100.*MELSE/(MSTART-MNOW+1.D-5)                               56.   
      WRITE (6,'(A,I3,55X,2I7,F7.1,A,F11.2)') ' OUTPUT RECORD WRITTEN ON  57.
     * UNIT',KDISK,MINC,MELSE,PERCNT,' TAU',TAU                           57.1
      KDISK=3-KDISK                                                       58.   
      MLAST=MNOW                                                          59.   
C**** TEST FOR TERMINATION OF RUN                                         60.   
  200 READ (3,END=210) LABSSW                                             61.1  
  210 REWIND 3                                                            61.2  
      IF(LABSSW.EQ.LABEL1) KSS6=1                                         61.3  
c  ** MFS (ADDED)
c  ** Check if a file named SSW.STOPGCM exists and stop the model if
c  ** it does.  Errors retuned are both MacOS style and Fortran 
c  ** style.  Basically if SSW.STOPGCM is held open then the model
c  ** will not stop, but if the file is closed then the model
c  ** will stop. In this version of the code send switch also works.
      OPEN(3,FILE='SSW.STOPGCM',FORM='unformatted',
     * STATUS='old',IOSTAT=IOErr)
c  0 = NoErr
      IF(IOErr.EQ.0) THEN
        KSS6=1
        CLOSE(3,IOSTAT=IOErr)
      END IF
c  ** END (ADDED)
C     IF(MBEGIN-MNOW.GT.2880000) KSS6=1                                   61.5  

c  ** MFS (CHANGED)
c  ** For some reason the qflux/sst code used IF(KSS6.EQ.1) GO TO 800.
c  ** I don't know why the MODRD.EQ.NRAD-NDYN was added.
c  ** the old code in this model was
c      IF(KSS6.EQ.1) GO TO 800                                             62.   
c  ** but the old code in the deep ocean model was
c      IF(KSS6.EQ.1.AND.MODRD.EQ.NRAD-NDYN) GO TO 800                      62.   
      IF(KOCEAN.EQ.2) THEN
        IF(KSS6.EQ.1.AND.MODRD.EQ.NRAD-NDYN) GO TO 800            
      ELSE
      IF(KSS6.EQ.1) GO TO 800                                             62.   
      END IF   
c  ** END (CHANGED)

      IF(TAU+TAUERR.GE.TAUE) GO TO 810                                    63.   
C**** IF TIME TO ZERO OUT DIAGNOSTIC ACCUMULATING ARRAYS, DO SO           64.   
C****   (ALWAYS AT THE BEGINNING OF THE RUN ......                        65.   
         IF(TAU-TAUERR.LE.TAUI) GO TO 260                                 66.   
         IF(.NOT.EVENT(HR24)) GO TO 300                                   67.   
C****   .... AND NORMALLY ALSO AT THE BEGINNING OF EACH MONTH)            67.5  
         DO 250 K=1,13                                                    68.   
         IF(JDAY.EQ.NDZERO(K)) GO TO 260                                  69.   
  250    CONTINUE                                                         70.   
         GO TO 290                                                        71.   
  260    TAU0=TAU                                                         72.   
         IDAY0=IDAY                                                       73.   
         TOFDY0=TOFDAY                                                    74.   
         JDATE0=JDATE                                                     75.   
         JMNTH0=JMONTH                                                    76.   
         JYEAR0=JYEAR                                                     77.   
         DO 270 I=1,10                                                    78.   
  270    IDACC(I)=0                                                       79.   
         DO 275 K=1,KACC                                                  80.   
  275    AJ(K,1)=0.                                                       81.   
         DO 280 J=1,JM                                                    81.1  
         DO 280 I=1,IM                                                    81.2  
         AIJ(I,J,76)=-1000.                                               81.3  
         AIJ(I,J,77)=1000.                                                81.4  
  280    AIJ(I,J,78)=1000.                                                81.5  
C**** INITIALIZE SOME ARRAYS AT THE BEGINNING OF SPECIFIED DAYS           82.   

c  ** MFS (CHANGED)
c  ** Rename the prt files here to get the correct names and split the
c  ** prt into monthly files.  Because not everything has been 
c  ** initialized yet durring the first hour I have to init a few
c  ** variables.
c  290    IF(JDAY.NE.32) GO TO 294                                         83.   
  290    CONTINUE
c  init variables that I'm ahead of
         LLAB1 = INDEX(LABEL1,'(') -1
c         IF(LABEL1(4:4).EQ.' ') LLAB1=4
         WRITE(CYEAR,'(I4)')JYEAR0
c  Create the proper file name which is the month plus 1:
c  ** 9/X (SWITCH)
c         fName = ':prt:'//JMNTH0(1:3)//CYEAR//'.prt'//LABEL1(1:LLAB1)
         fName = './prt/'//JMNTH0(1:3)//CYEAR//'.prt'//LABEL1(1:LLAB1)
c         fName = './'//JMNTH0(1:3)//CYEAR//'.prt'//LABEL1(1:LLAB1)
c  Open a new file with that name:
c         OPEN(6,FILE=fName,FILETYPE='TEXT',CREATOR='R*ch')
c         OPEN(6,FILE=fName,FILETYPE='TEXT',CREATOR='SSPP')
         OPEN(6,FILE=fName)
c  ** END (SWITCH)
c  put back old code
         IF(JDAY.NE.32) GO TO 294
c  ** END (CHANGED)

         JEQ=1+JM/2                                                       83.01 
         DO 292 J=JEQ,JM                                                  83.02 
         DO 292 I=1,IM                                                    83.03 
  292    TSFREZ(I,J,1)=JDAY                                               83.04 
         JEQM1=JEQ-1                                                      83.05 
         DO 293 J=1,JEQM1                                                 83.06 
         DO 293 I=1,IM                                                    83.07 
  293    TSFREZ(I,J,2)=JDAY                                               83.08 
         GO TO 296                                                        83.09 
  294    IF(JDAY.NE.213) GO TO 296                                        83.1  
         JEQM1=JM/2                                                       83.11 
         DO 295 J=1,JEQM1                                                 83.12 
         DO 295 I=1,IM                                                    83.13 
  295    TSFREZ(I,J,1)=JDAY                                               83.14 
C**** INITIALIZE SOME ARRAYS AT THE BEGINNING OF EACH DAY                 83.4  
  296    DO 297 J=1,JM                                                    83.5  
         DO 297 I=1,IM                                                    83.51 
         TDIURN(I,J,1)=1000.                                              83.511
         TDIURN(I,J,2)=-1000.                                             83.512
         TDIURN(I,J,3)=1000.                                              83.513
         TDIURN(I,J,4)=-1000.                                             83.514
         TDIURN(I,J,5)=0.                                                 83.52 
         TDIURN(I,J,6)=-1000.                                             83.521
         PEARTH=FDATA(I,J,2)*(1.-FDATA(I,J,3))                            83.53 
         IF(PEARTH.GT.0.) GO TO 297                                       83.54 
         TSFREZ(I,J,1)=365.                                               83.55 
         TSFREZ(I,J,2)=365.                                               83.56 
  297    CONTINUE                                                         83.57 
C****                                                                     84.   
C**** INTEGRATE DYNAMIC TERMS                                             85.   
C****                                                                     86.   
  300    MODD5D=MOD(NSTEP,NDA5D)                                          87.   
         IF(MODD5D.EQ.0) CALL DIAG5A (2,0)                                88.   
         IF(MODD5D.EQ.0) CALL DIAG9A (1)                                  88.5  
      CALL DYNAM
      DOPK=1.                                                            123.   
      CALL CHECKT (2)                                                    123.5  
      CALL CLOCKS (MNOW)                                                 124.   
      MINC=MLAST-MNOW                                                    125.   
      MDYN=MDYN+MINC                                                     126.   
      MLAST=MNOW                                                         127.   
      PERCNT=100.*MDYN/(MSTART-MNOW)                                     128.   
C     WRITE (6,'(A,I1,3X,A,I6,A,F6.2,I6,A5,2I7,F7.1,21X,A,F11.2)')       129.
C    *  '   DYNAMIC TERMS INTEGRATED, MRCH=',MRCH,'DAY',IDAY,            129.1
C    *  ', HR',TOFDAY,JDATE,JMONTH,MINC,MDYN,PERCNT,'TAU',TAU            129.2
         IF(MODD5D.EQ.0) CALL DIAG5A (7,NDYN)                            130.   
         IF(MODD5D.EQ.0) CALL DIAG9A (2)                                 131.   
         IF(EVENT(HR12)) CALL DIAG7A                                     132.   
C****                                                                    133.   
C**** INTEGRATE SOURCE TERMS                                             134.   
C****                                                                    135.   
      MODRD=MOD(NSTEP,NRAD)                                              136.   
         MODD5S=MOD(NSTEP,NDA5S)                                         137.   
         IF(MODD5S.EQ.0) IDACC(8)=IDACC(8)+1                             138.   
         IF(MODD5S.EQ.0.AND.MODD5D.NE.0) CALL DIAG5A (1,0)               139.   
         IF(MODD5S.EQ.0.AND.MODD5D.NE.0) CALL DIAG9A (1)                 139.5  
C**** CONDENSTATION, SUPER SATURATION AND MOIST CONVECTION               140.   
C     IF(MOD(NSTEP,NCNDS).NE.0) GO TO 400                                140.5  
      CALL CONDSE                                                        141.   
      CALL CHECKT (3)                                                    141.5  
      CALL PRECIP                                                        142.   
      CALL CHECKT (4)                                                    142.5  
      CALL CLOCKS (MNOW)                                                 143.   
      MINC=MLAST-MNOW                                                    144.   
      MCNDS=MCNDS+MLAST-MNOW                                             145.   
      MLAST=MNOW                                                         146.   
         IF(MODD5S.EQ.0) CALL DIAG5A (9,NCNDS)                           147.   
         IF(MODD5S.EQ.0) CALL DIAG9A (3)                                 148.   
C**** RADIATION, SOLAR AND THERMAL                                       149.   
      CALL RADIA                                                         150.   
      CALL CHECKT (5)                                                    150.5  
      CALL CLOCKS (MNOW)                                                 151.   
      MINC=MINC+MLAST-MNOW                                               152.   
      MRAD=MRAD+MLAST-MNOW                                               153.   
      MLAST=MNOW                                                         154.   
         IF(MODD5S.EQ.0) CALL DIAG5A (11,NCNDS)                          155.   
         IF(MODD5S.EQ.0) CALL DIAG9A (4)                                 156.   
C**** SURFACE INTERACTION AND GROUND CALCULATION                         157.   
  400 CALL SURFCE                                                        158.   
      CALL CHECKT (6)                                                    158.5  
      CALL GROUND                                                        159.   
      CALL CHECKT (7)                                                    159.5  
      CALL DRYCNV                                                        160.   
      CALL CHECKT (8)                                                    160.5  
      CALL CLOCKS (MNOW)                                                 161.   
      MINC=MINC+MLAST-MNOW                                               162.   
      MSURF=MSURF+MLAST-MNOW                                             163.   
      MLAST=MNOW                                                         164.   
         IF(MODD5S.EQ.0) CALL DIAG9A (5)                                 165.   
C**** STRATOSPHERIC MOMENTUM DRAG                                        166.   
      CALL SDRAG                                                         167.   
      CALL CHECKT (9)                                                    167.5  
      CALL CLOCKS (MNOW)                                                 168.   
      MINC=MINC+MLAST-MNOW                                               169.   
      MSURF=MSURF+MLAST-MNOW                                             170.   
      MLAST=MNOW                                                         171.   
         IF(MODD5S.EQ.0) CALL DIAG5A (12,NCNDS)                          172.   
         IF(MODD5S.EQ.0) CALL DIAG9A (6)                                 173.   
      MSRCE=MCNDS+MRAD+MSURF                                             174.   
      PERCNT=100.*MSRCE/(MSTART-MNOW)                                    175.   
C     WRITE (6,'(A,64X,2I7,F7.1)')                                       176.
C    *  ' SOURCE TERMS INTEGRATED', MINC,MSRCE,PERCNT                    176.1
C**** SEA LEVEL PRESSURE FILTER                                          177.   
      IF(MFILTR.LE.0.OR.MOD(NSTEP,NFILTR).NE.0) GO TO 500                178.   
         IDACC(10)=IDACC(10)+1                                           179.   
         IF(MODD5S.NE.0) CALL DIAG5A (1,0)                               180.   
         CALL DIAG9A (1)                                                 180.5  
      CALL FILTER                                                        181.   
      CALL CHECKT (10)                                                   181.5  
      CALL CLOCKS (MNOW)                                                 182.   
      MDYN=MDYN+MLAST-MNOW                                               183.   
      MLAST=MNOW                                                         184.   
         CALL DIAG5A (14,NFILTR)                                         185.   
         CALL DIAG9A (7)                                                 186.   
C****                                                                    187.   
C**** UPDATE MODEL TIME AND CALL DAILY IF REQUIRED                       188.   
C****                                                                    189.   
  500 NSTEP=NSTEP+NDYN                                                   190.   
      ITAU=(NSTEP+NSTEP0)*IDTHR                                          191.   
      TAU=DFLOAT(ITAU)/XINT                                              192.   
      IDAY=1+ITAU/I24                                                    193.   
      TOFDAY=(ITAU-(IDAY-1)*I24)/XINT                                    194.   
      IF(.NOT.EVENT(HR24)) GO TO 590                                     195.   
         CALL DIAG5A (1,0)                                               196.   
         CALL DIAG9A (1)                                                 196.5  
      CALL DAILY                                                         197.   
      CALL CLOCKS (MNOW)                                                 198.   
      MELSE=MELSE+(MLAST-MNOW)                                           199.   
      MLAST=MNOW                                                         200.   
         NDAILY=SDAY/DT                                                  201.   
         CALL DIAG5A (16,NDAILY)                                         202.   
         CALL DIAG9A (8)                                                 203.   
         DO 530 J=1,JM                                                   203.21 
         IMAX=IM                                                         203.22 
         IF((J.EQ.1).OR.(J.EQ.JM)) IMAX=1                                203.23 
         DO 530 I=1,IMAX                                                 203.24 
         TSAVG=TDIURN(I,J,5)/(HR24*NSURF)                                203.31 
         IF(32.+1.8*TSAVG.LT.65.)AIJ(I,J,52)=AIJ(I,J,52)+(33.-1.8*TSAVG) 203.32 
         AIJ(I,J,54)=AIJ(I,J,54)+18.*((TDIURN(I,J,2)-TDIURN(I,J,1))      203.41 
     *     /(TDIURN(I,J,4)-TDIURN(I,J,3)+1.D-20)-1.)                     203.42 
         AIJ(I,J,30)=AIJ(I,J,30)+(TDIURN(I,J,4)-TDIURN(I,J,3))           203.425
         AIJ(I,J,80)=AIJ(I,J,80)+(TDIURN(I,J,4)-273.16)                  203.426
         IF (TDIURN(I,J,6).LT.AIJ(I,J,78)) AIJ(I,J,78)=TDIURN(I,J,6)     203.48 
  530 CONTINUE                                                           203.51 
c  ** MFS (CHANGED)
c  ** Old code assumed that kocean <> 1 was kocean = 0, which is no longer
c  ** true.
c      IF(KOCEAN.NE.1) GO TO 590                                          204.   
      IF(KOCEAN.EQ.0) GO TO 590                                     
c  ** END (CHANDED)
         DO 540 J=1,JM                                                   204.3  
         DO 540 I=1,IM                                                   204.4  
         AIJ(I,J,59)=AIJ(I,J,59)+ODATA(I,J,4)                            204.5  
  540    AIJ(I,J,60)=AIJ(I,J,60)+ODATA(I,J,5)                            204.6  

c  ** MFS (ADDED)
c  ** This code calls the ocean diffusion in the deep ocean mode (
c  ** kOcean = 2).  The diffusion code lets heat move downward from
c  ** the bottom of the mixed layer into the rest of the ocean.
c  ** in the deep ocean code the old line was
c      CALL ODIFS                                                         204.61 
      IF(KOCEAN.EQ.2)THEN
        CALL ODIFS
      END IF
c  ** END (ADDED)

C**** RESTRUCTURE THE OCEAN LAYERS AND ELIMINATE SMALL ICE BERGS         205.   
      CALL OSTRUC                                                        206.   
      CALL CHECKT (11)                                                   206.5  
      CALL CLOCKS (MNOW)                                                 207.   
      MSURF=MSURF+(MLAST-MNOW)                                           208.   
      MLAST=MNOW                                                         209.   
C****                                                                    210.   
C**** WRITE INFORMATION ONTO A TAPE EVERY USET HOURS                     211.   
C****                                                                    212.   
  590 CONTINUE                                                           212.5  
c  ** RAR (CHANGED)
c  ** Reto said that tauo never worked because the if was missing here.
c  ** "I guess we saved an if statement."  This has now been fixed so
c  ** that at the end of the first day after tauo ocean data is 
c  ** collected. Code works now, collection starts on the first day of
c  ** TAUO (the +.5 is to prevent the previous day from being saved).
      IF(USET.LE.0.) GO TO 600                                           213.   
      IF((TAUO+.5).GT.TAU) GO TO 600  
c  ** END (CHANDED)
            IF(.NOT.EVENT(USET/2.)) GO TO 600                            214.   
            IF(EVENT(USET)) GO TO 552                                    214.5  
C****       SAVE INSTANTANEOUS QUANTITIES                                215.   
            DO 551 J=1,JM                                                215.1  
            DO 551 I=1,IM                                                215.2  
            OA(I,J,1)=.1*916.6+GDATA(I,J,1)                              215.3  
            OA(I,J,2)=GDATA(I,J,3)                                       215.4  
  551       OA(I,J,3)=GDATA(I,J,7)                                       215.5  
            GO TO 600                                                    215.6  
  552       WRITE (20) TAU,OA                                            216.   
c  ** RAR (CHANGED)
c  ** old code to force an immediate write to tape, this doesn't work
c  ** anymore.
c      ENDFILE 20                                                         216.5  
c      BACKSPACE 20                                                       216.6  
c  ** END (CHANDED)
      CALL CLOCKS (MNOW)                                                 217.   
      MINC=MLAST-MNOW                                                    218.   
      MELSE=MELSE+MINC                                                   219.   
      PERCNT=100.*MELSE/(MSTART-MNOW)                                    220.   
COD   WRITE (6,'(A,78X,A,F11.2)') ' INFORMATION WRITTEN ON UNIT 20',     221.
COD  *     ' TAU',TAU                                                    221.1
C****                                                                    222.   
C**** CALL DIAGNOSTIC ROUTINES                                           223.   
C****                                                                    224.   
  600    IF(MOD(NSTEP+NDA4,NDA4).EQ.0) CALL DIAG4A                       225.   
CF    RUNON=PALMER(36,1)                                                 225.1  
         IF(USESLP.EQ.0.) GO TO 603                                      225.7  
         IF(USESLP.LT.0..AND.EVENT(-USESLP)) USESLP=-USESLP              225.8  
         IF(USESLP.GT.0..AND.EVENT(USESLP)) CALL DIAG10(0)               225.9  
  603    CONTINUE                                                        226.   
         IF(NDPRNT(1).GE.0) GO TO 610                                    227.   
C**** PRINT CURRENT DIAGNOSTICS (INCLUDING THE INITIAL CONDITIONS)       228.   
         IF(KDIAG(1).LT.9) CALL DIAGJ                                    229.   
         IF(KDIAG(2).LT.9) CALL DIAGJK                                   229.5  
         IF(KDIAG(2).LT.9) CALL DIAGJL                                   230.   
         IF(KDIAG(7).LT.9) CALL DIAG7P                                   231.   
         IF(KDIAG(3).LT.9) CALL DIAGIJ                                   232.   
         IF(KDIAG(9).LT.9) CALL DIAG9P                                   233.   
         IF(KDIAG(5).LT.9) CALL DIAG5P                                   234.   
         IF(KDIAG(4).LT.9) CALL DIAG4                                    235.   
         IF(TAU.LE.TAUI+DTHR*(NDYN+.5)) CALL DIAGKN                      236.   
         NDPRNT(1)=NDPRNT(1)+1                                           237.   
         IF(TAU.LE.TAUI+DTHR*(NDYN+.5)) GO TO 610                        237.1  
C**** RESET THE UNUSED KEYNUMBERS TO ZERO                                237.4  
         DO 605 I=1,42                                                   237.5  
  605    KEYNR(I,KEYCT)=0                                                237.6  
  610    IF(.NOT.EVENT(HR24)) GO TO 690                                  238.   
C**** PRINT DIAGNOSTIC TIME AVERAGED QUANTITIES ON NDPRNT-TH DAY OF RUN  239.   
         DO 620 K=1,13                                                   240.   
         IF(JDAY.EQ.NDPRNT(K)) GO TO 630                                 241.   
  620    CONTINUE                                                        242.   
         GO TO 640                                                       243.   
  630    WRITE (6,'("1"/64(1X/))')                                       244.
         IF(KDIAG(1).LT.9) CALL DIAGJ                                    245.   
         IF(KDIAG(2).LT.9) CALL DIAGJK                                   245.5  
         IF(KDIAG(2).LT.9) CALL DIAGJL                                   246.   
         IF(KDIAG(7).LT.9) CALL DIAG7P                                   247.   
         IF(KDIAG(3).LT.9) CALL DIAGIJ                                   248.   
         IF(KDIAG(9).LT.9) CALL DIAG9P                                   249.   
         IF(KDIAG(5).LT.9) CALL DIAG5P                                   250.   
         IF(KDIAG(6).LT.9) CALL DIAG6                                    251.   
         IF(KDIAG(4).LT.9) CALL DIAG4                                    252.   
C**** THINGS TO DO BEFORE ZEROING OUT THE ACCUMULATING ARRAYS            254.   
C****   (NORMALLY DONE AT THE END OF A MONTH)                            255.   
  640    DO 650 K=1,13                                                   256.   
         IF(JDAY.EQ.NDZERO(K)) GO TO 660                                 257.   
  650    CONTINUE                                                        258.   
         GO TO 690                                                       259.   
C**** PRINT THE KEY DIAGNOSTICS                                          260.   
  660    CALL DIAGKN                                                     261.   
      IF(RUNON.EQ.-1.) STOP 13                                           261.5  
C**** PRINT AND ZERO OUT THE TIMING NUMBERS                              262.   
      CALL CLOCKS (MNOW)                                                 263.   
      MDIAG=MDIAG+(MLAST-MNOW)                                           264.   
      MLAST=MNOW                                                         265.   
      TOTALT=.01*(MSTART-MNOW)                                           266.   
      PDYN=MDYN/TOTALT                                                   267.   
      PCDNS=MCNDS/TOTALT                                                 268.   
      PRAD=MRAD/TOTALT                                                   269.   
      PSURF=MSURF/TOTALT                                                 270.   
      PDIAG=MDIAG/TOTALT                                                 271.   
      PELSE=MELSE/TOTALT                                                 272.   
      DTIME=24.*TOTALT/(60.*(TAU-TAU0))                                  273.   
      WRITE (6,'(/A,F7.2,A,F5.1,A,F5.1,A,F5.1,A,F5.1,A,F5.1,A,F5.1//)')  274.   
     *  '0TIME',DTIME,'(MINUTES)    DYNAMICS',PDYN,                      274.1  
     *  '    CONDENSATION',PCDNS,'    RADIATION',PRAD,'    SURFACE',     274.2  
     *  PSURF,'    DIAGNOSTICS',PDIAG,'    OTHER',PELSE                  274.3  
      MDYN=0                                                             275.   
      MCNDS=0                                                            276.   
      MRAD=0                                                             277.   
      MSURF=0                                                            278.   
      MDIAG=0                                                            279.   
      MELSE=0                                                            280.   
      MSTART=MNOW                                                        281.   
         DELTAU=1.5*(TAU-TAU0)                                           281.5  
c  ** MFS (CHANGED)
c  ** This code does not really support deep ocean right now. There are
c  ** some fragments of kocean=2 mode but they don't work because the
c  ** arrays are missing. Extensive work will have to be done later to
c  ** make this code work right.

  680    IF(KCOPY.LE.0) GO TO 690                                        290.   
C**** SAVE ONE OR BOTH PARTS OF THE FINAL RESTART DATA SET               291.   
         WRITE(CYEAR,'(I4)')JYEAR0                                       291.2  
c         LLAB1=5                                                         291.4  
c         IF(LABEL1(4:4).EQ.' ') LLAB1=4                                  291.6  
         LLAB1 = INDEX(LABEL1,'(') -1
         REWIND KDISK                                                    291.8  
         IF(KCOPY.EQ.1) GO TO 685                                        292.   
C**** KCOPY > 1 : SAVE THE RESTART INFORMATION                           292.1  
         CALL RFINAL (IRAND)                                             292.2  

c  ** RAR (COMMENT)
c  ** Reto says this code is unncessary because the model writes out 
c  ** the rsf file differently from fort.1 and fort.2.
c         WRITE (KDISK) TAU,JC,C,RC,KEYNR,U,V,T,P,Q,ODATA,GDATA,BLDATA,   292.3  
c     *     RQT,SRHR,TRHR,TSFREZ,TAU                                      292.4  

c  ** old code of copy rsf file at command line
c         CMMND='cp fort.'//CHAR(KDISK+ICHAR('0'))//' '                   292.5  
c     &   //JMNTH0(1:3)//CYEAR//'.rsf'//LABEL1(1:LLAB1)                   292.6  
c         REWIND KDISK                                                    292.7  
c         CALL SYSTEM(CMMND)                                              292.8  
c  Create the proper file name:

c  ** 9/X (SWITCH)
c         fName = ':rsf:'//JMNTH0(1:3)//CYEAR//'.rsf'//LABEL1(1:LLAB1)
         fName = './rsf/'//JMNTH0(1:3)//CYEAR//'.rsf'//LABEL1(1:LLAB1)
c         fName = './'//JMNTH0(1:3)//CYEAR//'.rsf'//LABEL1(1:LLAB1)
c  ** END (SWITCH)
c  Open a new file with that name:
         OPEN(UNIT=2001, FILE=fName, FORM='unformatted')
C         OPEN(UNIT=2001, FILE=fName, FORM='unformatted',
C     *     FILETYPE='rsf_',CREATOR='MpCd')
c  Write out new style restart file
c  ** writes out TG3M,RTGO,STG3,DTG3 for deep ocean
         REWIND UNIT=2001
         WRITE (UNIT=2001) TAU,JC,C,RC,KEYNR,U,V,T,P,Q,ODATA,GDATA,
c     *      BLDATA,RQT,SRHR,TRHR,TSFREZ,TAU                
     *      BLDATA,RQT,SRHR,TRHR,TSFREZ,TG3M,RTGO,STG3,DTG3,TAU       
c  Close the monthly file:
	     CLOSE(UNIT=2001, STATUS='KEEP')


         IF(KCOPY.EQ.2) GO TO 685                                        292.805
C**** KCOPY > 2 : SAVE THE OCEAN DATA FOR INITIALIZING DEEP OCEAN RUNS   292.81 

c  ** write out the ODA file
c         WRITE (KDISK) TAU,ODATA,((AIJ(I,J,60),I=1,IM),J=1,JM)           292.82 
c         CMMND='cp fort.'//CHAR(KDISK+ICHAR('0'))//' '                   292.83 
c     &   //JMNTH0(1:3)//CYEAR//'.oda'//LABEL1(1:LLAB1)                   292.84 
c         REWIND KDISK                                                    292.86 
c         CALL SYSTEM(CMMND)                                              292.87 
c  Create the proper file name:
c  ** 9/X (SWITCH)
c         fName = ':ocean:'//JMNTH0(1:3)//CYEAR//'.oda'//LABEL1(1:LLAB1)
         fName = './ocean/'//JMNTH0(1:3)//CYEAR//'.oda'//LABEL1(1:LLAB1)
c         fName = './'//JMNTH0(1:3)//CYEAR//'.oda'//LABEL1(1:LLAB1)
c  ** END (SWITCH)
c  Open a new file with that name:
         OPEN(UNIT=2002, FILE=fName, FORM='unformatted')
c  Write out new style restart file
         REWIND UNIT=2002
         WRITE (UNIT=2002) TAU,ODATA,((AIJ(I,J,60),I=1,IM),J=1,JM)          
c  Close the monthly file:
	     CLOSE(UNIT=2002, STATUS='KEEP')

C**** SAVE THE DIAGNOSTIC ACCUM ARRAYS IN SINGLE PRECISION               292.9  
c  ** write out the ACC files
  685    IDUM1=IDUM(1)                                                   293.   
c  ** MFS (CHANGED)
c  ** Deep ocean uses a larger ACC file so it writes out a larger value
c  ** in the IDUM array.  I may remove the extra write statement from
c  ** oda file, but for the moment I'll just conditialize it.
c  ** in this code the old line was
c         IDUM(1)=IM*JM*2                                                 293.1  
c  ** but in the other code the old line was
c         IDUM(1)=IM*JM*(2+LM)                                            293.1  
         IF(KOCEAN.EQ.2)THEN
           IDUM(1)=IM*JM*(2+LM)                                      
         ELSE
           IDUM(1)=IM*JM*2
         END IF 
c  ** END (CHANGED)
         IDUM4=IDUM(4)                                                   293.2  
         IDUM(4)=0                                                       293.3  

c  ** Added code to write the monthly data to a separate file named
c  ** for the current month of simulation, to replace the
c  ** unix-shell script command used to copy the file:

c         WRITE (KDISK) SNGL(TAU),JC,C,(SNGL(RC(I)),I=1,161),KEYNR,       293.4  
c     *     (SNGL(TSFREZ(I,1,1)),I=1,IM*JM*2),                            293.5  
c     *     (((SNGL(RTGO(I,J,L)),I=1,IM),J=1,JM),L=2,LM),                 293.65 
c     *     (SNGL(AJ(I,1)),I=1,KACC),SNGL(TAU)                            293.7  

c  ** RAR (COMMENT)
c  ** Reto explained the history of the RTGO array in the acc file.
c  ** Since ocean temperatures change slowly some layers of the 
c  ** instantious ocean temperature from the RTGO are written out as if
c  ** they were accumulated diagnostics. Thus the RTGO in the acc file
c  ** is a duplicate of the RTGO in the rsf file, except in single 
c  ** format. No one was interested enough to create a real ocean 
c  ** diagnostics so RTGO is the next best thing.
c  **
c  ** I don't write RTGO because the old code put it in the MIDDLE of
c  ** the acc file which messed up all the post-processors. Eventually
c  ** I need to come up with a real solution to this problem.

         IDUM(1)=IDUM1                                                   293.8  
         IDUM(4)=IDUM4                                                   293.9  
c         CMMND='ksh nsend fort.'//CHAR(KDISK+ICHAR('0'))//' '            294.   
c     &   //JMNTH0(1:3)//CYEAR//'.acc'//LABEL1(1:LLAB1)                   294.2  
c         REWIND KDISK                                                    294.3  
c         CALL SYSTEM(CMMND)                                              294.4  
c  Create the proper file name:
c  ** 9/X (SWITCH)
c         fName = ':acc:'//JMNTH0(1:3)//CYEAR//'.acc'//LABEL1(1:LLAB1)
         fName = './acc/'//JMNTH0(1:3)//CYEAR//'.acc'//LABEL1(1:LLAB1)
c         fName = './'//JMNTH0(1:3)//CYEAR//'.acc'//LABEL1(1:LLAB1)
c  ** END (SWITCH)
c  Open a new file with that name:
         OPEN(UNIT=2003, FILE=fName, FORM='unformatted')
         REWIND UNIT=2003
c  Write the data to the monthly file:
         WRITE (UNIT=2003) REAL(TAU),JC,C,(REAL(RC(I)),I=1,161),KEYNR,         
     *     (REAL(TSFREZ(I,1,1)),I=1,IM*JM*2),                             
     *     (REAL(AJ(I,1)),I=1,KACC),REAL(TAU)                              
c  Close the monthly file:
	     CLOSE(UNIT=2003, STATUS='KEEP')
c  ** END (CHANGED)

C**** TIME FOR CALLING DIAGNOSTICS                                       296.   
  690 CALL CLOCKS (MNOW)                                                 297.   
      MDIAG=MDIAG+(MLAST-MNOW)                                           298.   
      MLAST=MNOW                                                         299.   
      IF(TAU.LE.TAUI+DTHR*(NDYN+.5).AND.TAUT.LT.TAU+1000.) GO TO 120     300.   
      GO TO 98                                                           301.   
C****                                                                    302.   
C**** END OF MAIN LOOP                                                   303.   
C****                                                                    304.   
C**** RUN TERMINATED BECAUSE SENSE SWITCH 6 WAS TURNED ON                305.   
  800 WRITE (6,'("0SENSE SWITCH 6 HAS BEEN TURNED ON.")')                306.
  810 IF(EVENT(TAUT).OR.TAUT.GE.TAU+1000.) GO TO 820                     307.   
      CALL RFINAL (IRAND)                                                308.   
      REWIND KDISK                                                       309.   
c  ** RAR (COMMENT)
c  ** Reto says that this write to fort.1 happens when a run ends or
c  ** a sense switch is called to stop the run. Occassionally if the
c  ** run has just written out the fort.1 file then this code is not
c  ** used. It should always be identical to the code on line 50-51.1
      WRITE (KDISK) TAU,JC,C,RC,KEYNR,U,V,T,P,Q,ODATA,GDATA,BLDATA,      310.   
c  ** MFS (CHANGED)
c  ** again extra arrays for deep ocean: TG3M,RTGO,STG3,DTG3
c     *  RQT,SRHR,TRHR,TSFREZ,(AJ(K,1),K=1,KACC),TDIURN,OA,TAU            311.   
     *  RQT,SRHR,TRHR,TSFREZ,TG3M,RTGO,STG3,DTG3,                        311.   
     *  (AJ(K,1),K=1,KACC),TDIURN,TAU                                    311.1  
      WRITE (6,'(A,I3,77X,A,F11.2)')                                     312.
     *  ' OUTPUT RECORD WRITTEN ON UNIT',KDISK,'TAU',TAU                 312.1
C**** RUN TERMINATED BECAUSE IT REACHED TAUE (OR SS6 WAS TURNED ON)      313.   
  820 WRITE (6,'(/////4(1X,33("****")/)//,A,F11.2,I6,F7.2                314.
     *             ///4(1X,33("****")/))')                               314.1
     *  ' PROGRAM TERMINATED NORMALLY.TAU,IDAY,TOFDAY=',TAU,IDAY,TOFDAY  315.
c      IF(KSS6.EQ.1) STOP 12                                              316.5  
c      STOP 13                                                            317.   
      IF(KSS6.EQ.1)THEN
        PRINT *, 'Run paused successfully.'
        STOP 'STOP 12   '
      ELSE
      	IF(TAUE.EQ.(TAUI+1))THEN
          PRINT *, 'First hour completed successfully!'
        ELSE
          PRINT *, 'Run completed successfully.'
        END IF
        STOP 'STOP 13   '
      END IF
c  ** END (CHANGED)
C****                                                                    319.   
      END                                                                339.   

      SUBROUTINE GEOM                                                    401.
C**** CALCULATE SPHERICAL GEOMETRY  (for 4x5 or 7.8x10)                  402.
C**** This is as in Model II'
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q                                                   403. 
      DATA EDPERD/1./,EDPERY/365./                                       404.
                                                                         405.
      LAT(1)  = -.25*TWOPI                                               406.
      LAT(JM) = -LAT(1)                                                  407.
      SINP(1)  = -1.                                                     408.
      SINP(JM) = 1.                                                      409.
      COSP(1)  = 0.                                                      410.
      COSP(JM) = 0.                                                      411.
      DXP(1)  = 0.                                                       412.
      DXP(JM) = 0.                                                       413.
      FJEQ = .5*(1+JM)                                                   414.
      DO 620 J=2,JM-1                                                    415.
        LAT(J)  = DLAT*(J-FJEQ)                                            416.
        SINP(J) = SIN(LAT(J))                                              417.
        COSP(J) = COS(LAT(J))                                              418.
  620   DXP(J)  = RADIUS*DLON*COSP(J)                                      419.
      DO 640 J=2,JM                                                      420.
        COSV(J) = .5*(COSP(J-1)+COSP(J))                                   421.
        DXV(J)  = .5*(DXP(J-1)+DXP(J))                                     422.
  640   DYV(J)  = RADIUS*(LAT(J)-LAT(J-1))                                 423.
      DYP(1)  = .5*DYV(2)                                                424.
      DYP(JM) = .5*DYV(JM)                                               425.
      DXYP(1) = .5*DXV(2)*DYP(1)                                         426.
      DXYP(JM)= .5*DXV(JM)*DYP(JM)                                       427.
      DXYS(1)  = 0.                                                      428.
      DXYS(JM) = DXYP(JM)                                                429.
      DXYN(1)  = DXYP(1)                                                 430.
      DXYN(JM) = 0.                                                      431.
      AREAG = DXYP(1)+DXYP(JM)                                           432.
      DO 660 J=2,JM-1                                                    433.
      DYP(J)  = .5*(DYV(J)+DYV(J+1))                                     434.
      DXYP(J) = .5*(DXV(J)+DXV(J+1))*DYP(J)                              435.
      DXYS(J) = .5*DXYP(J)                                               436.
      DXYN(J) = .5*DXYP(J)                                               437.
  660 AREAG = AREAG+DXYP(J)                                              438.
      AREAG = AREAG*FIM                                                  439.
      RAVPS(1)  = 0.                                                     440.
      RAVPN(JM) = 0.                                                     441.
      DO 680 J=2,JM                                                      442.
      DXYV(J) = DXYN(J-1)+DXYS(J)                                        443.
      RAPVS(J)   = .5*DXYS(J)/DXYV(J)                                    444.
      RAPVN(J-1) = .5*DXYN(J-1)/DXYV(J)                                  445.
      RAVPS(J)   = .5*DXYS(J)/DXYP(J)                                    446.
  680 RAVPN(J-1) = .5*DXYN(J-1)/DXYP(J-1)                                447.
C**** CALCULATE CORIOLIS PARAMETER                                       448.
      OMEGA = TWOPI*(EDPERD+EDPERY)/(EDPERD*EDPERY*SDAY)                 449.
      F(1)  = -RADIUS*OMEGA*.5*COSP(2)*DXV(2)                            450.
      F(JM) = -F(1)                                                      451.
      DO 690 J=2,JM-1                                                    452.
  690 F(J) = OMEGA*(DXV(J)*DXV(J)-DXV(J+1)*DXV(J+1))/DLON                453.
CLAT_DG latitude of mid points of primary and sec. grid boxs (deg)
C**** latitudinal spacing depends on whether you have even spacing or
C**** a partial box at the pole
      DLAT_DG=180./(JM-1)   ! 1/2 box at pole for 4x5 and 7.8x10
C**** LATITUDES (degrees); used extensively in the diagn. print routines
      LAT_DG(1,1)=-90.
      LAT_DG(1,2)=-90.
      LAT_DG(JM,1)=90.
      DO J=2,JM-1
        LAT_DG(J,1)=DLAT_DG*(J-FJEQ)    ! primary (tracer) latitudes
      END DO
      DO J=2,JM
        LAT_DG(J,2)=DLAT_DG*(J-JM/2-1)  ! secondary (velocity) latitudes
      END DO
      RETURN                                                             454.
      END                                                                455.

      SUBROUTINE GEOM_8x10
C**** CALCULATE SPHERICAL GEOMETRY (True 8x10)                         
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q                                                  
      DATA EDPERD/1./,EDPERY/365./                                    
      LAT(1)=-.25*TWOPI                                                  769.   
      LAT(JM)=-LAT(1)                                                    770.   
      SINP(1)=-1.                                                        771.   
      SINP(JM)=1.                                                        772.   
      COSP(1)=0.                                                         773.   
      COSP(JM)=0.                                                        774.   
      DXP(1)=0.                                                          775.   
      DXP(JM)=0.                                                         776.   
      FJEQ=.5*(1+JM)                                                     776.5  
      DO 620 J=2,JM-1                                                    777.   
      LAT(J)=DLAT*(J-FJEQ)                                               778.   
      SINP(J)=SIN(LAT(J))                                                779.   
      COSP(J)=COS(LAT(J))                                                780.   
  620 DXP(J)=RADIUS*DLON*COSP(J)                                         781.   
      COSV(2)=(COSP(2)+2.*COSP(1))/3.                                    781.1  
      COSV(JM)=(COSP(JM-1)+2.*COSP(JM))/3.                               781.2  
      DXV(2)=(DXP(2)+2.*DXP(1))/3.                                       781.3  
      DXV(JM)=(DXP(JM-1)+2.*DXP(JM))/3.                                  781.4  
      DO 640 J=3,JM-1                                                    782.   
      COSV(J)=.5*(COSP(J-1)+COSP(J))                                     783.   
      DXV(J)=.5*(DXP(J-1)+DXP(J))                                        784.   
  640 DYV(J)=RADIUS*(LAT(J)-LAT(J-1))                                    785.   
      DYV(2)=.75*DYV(3)                                                  785.1  
      DYV(JM)=.75*DYV(3)                                                 785.2  
      DYP(1)=.25*DYV(3)                                                  785.3  
      DYP(2)=DYV(3)                                                      785.4  
      DYP(JM-1)=DYP(2)                                                   786.   
      DYP(JM)=DYP(1)                                                     787.   
      DXYP(1)=.5*DXV(2)*DYP(1)                                           788.   
      DXYP(JM)=.5*DXV(JM)*DYP(JM)                                        789.   
      DXYS(1)=0.                                                         790.   
      DXYS(JM)=DXYP(JM)                                                  791.   
      DXYN(1)=DXYP(1)                                                    792.   
      DXYN(JM)=0.                                                        793.   
      AREAG=DXYP(1)+DXYP(JM)                                             794.   
      DO 659 J=3,JM-2                                                    794.1  
  659 DYP(J)=.5*(DYV(J)+DYV(J+1))                                        794.2  
      DO 660 J=2,JM-1                                                    795.   
C     DYP(J)=.5*(DYV(J)+DYV(J+1))                                        796.   
      DXYP(J)=.5*(DXV(J)+DXV(J+1))*DYP(J)                                797.   
      DXYS(J)=.5*DXYP(J)                                                 798.   
      DXYN(J)=.5*DXYP(J)                                                 799.   
  660 AREAG=AREAG+DXYP(J)                                                800.   
      AREAG=AREAG*FIM                                                    801.   
      RAVPS(1)=0.                                                        802.   
      RAVPN(JM)=0.                                                       803.   
      DO 680 J=2,JM                                                      804.   
      DXYV(J)=DXYN(J-1)+DXYS(J)                                          805.   
      RAPVS(J)=.5*DXYS(J)/DXYV(J)                                        806.   
      RAPVN(J-1)=.5*DXYN(J-1)/DXYV(J)                                    807.   
      RAVPS(J)=.5*DXYS(J)/DXYP(J)                                        808.   
  680 RAVPN(J-1)=.5*DXYN(J-1)/DXYP(J-1)                                  809.   
C**** CALCULATE CORIOLIS PARAMETER                                       810.   
      OMEGA=TWOPI*(EDPERD+EDPERY)/(EDPERD*EDPERY*SDAY)                   811.   
      F(1)=-RADIUS*OMEGA*.5*COSP(2)*DXV(2)                               812.   
      F(JM)=-F(1)                                                        813.   
      DO 690 J=2,JM-1                                                    814.   
  690 F(J)=OMEGA*(DXV(J)*DXV(J)-DXV(J+1)*DXV(J+1))/DLON                  815.   
CLAT_DG latitude of mid points of primary and sec. grid boxs (deg)
C**** latitudinal spacing depends on whether you have even spacing or
C**** a partial box at the pole
      DLAT_DG=180./(JM-1.5) ! 1/4 box at pole, 'real' 8x10
C**** LATITUDES (degrees); used extensively in the diagn. print routines
      LAT_DG(1,1)=-90.
      LAT_DG(1,2)=-90.
      LAT_DG(JM,1)=90.
      DO J=2,JM-1
        LAT_DG(J,1)=DLAT_DG*(J-FJEQ)    ! primary (tracer) latitudes
      END DO
      DO J=2,JM
        LAT_DG(J,2)=DLAT_DG*(J-JM/2-1)  ! secondary (velocity) latitudes
      END DO
      return
      end

      BLOCK DATA BDINP                                                   457.
C****                                                                    458.
C**** DEFAULT PARAMETERS FOR MODEL COMMON BLOCK                          459.
C****                                                                    460.
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q                                                   461.1
                                                                         461.2
      DATA IM0,JM0,LM0,  LS1,LBLM/                                       462.
     *     IM ,JM ,LM ,    8,   2/,                                      463.
     *  KOCEAN,KDISK,KEYCT,KCOPY,     IRAND,IJRA,MFILTR,NDYN/            464.
     *       1,    1,    1,   30, 123456789,   2,     1,   4/,           465.
     *       NSURF,NGRND,  IYEAR/                                        466.
     *           2,    1,   1976/,                                       467.
     *  MLAST,MDYN,MCNDS,MRAD,MSURF,  MDIAG,MELSE,MODRD,MODD5K,MODD5S/   468.
     *      0,   0,    0,   0,    0,      0,    0,    0,     0,     0/   469.
      DATA  DT,  TAUP,TAUI,TAUE,TAUT/                                    470.
     *    900.,   -1., -1.,  1., 24./,                                   471.
     *    SDAY,     LHE,    LHM,     LHS,   RADIUS,GRAV,RGAS,KAPA/       472.
     *  86400.,2500000.,334000.,2834000., 6375000.,9.81,287.,.286/,      473.
     *  CCMCX,ETA,S0X, CO2, SRCOR, PTOP, PSF,   PSL, PTRUNC/             474.
     *  50000.,0., 1., 315.,   1.,  10.,984., 1000.,    0./,             475.
     *  XCDNST(1),XCDNST(2),XINT, SKIPSE,USESLP,USEP,USET, DOPK/         476.
     *      .0005,   .00005,120.,     0.,    0.,  0.,  0.,   0./         477.
      DATA TWOPI/6.283185307179586477D0/                                 477.2
      DATA SIG/9*0.d0, 27*0./                                 
      DATA SIGE/    1.,.948665,.866530,.728953,.554415,.390144,
     *  .251540,.143737,.061602,28*0./                        
      DATA NAMD6 /'AUSD','MWST','SAHL','EPAC'/,                          482.
     *  NDZERO/ 0,1,32,60,91,121,152,182,213,244,274,305,335/,           483.
     *  NDPRNT/-1,1,32,60,91,121,152,182,213,244,274,305,335/,           484.
     *  IJD6/32,9,   9,18, 19,14,  7,12/                                 485.
      END                                                                486.

      SUBROUTINE INPUT                                                   501.   
C****                                                                    502.   
C**** THIS SUBROUTINE SETS THE PARAMETERS IN THE C ARRAY, READS IN THE   503.   
C**** INITIAL CONDITIONS, AND CALCULATES THE DISTANCE PROJECTION ARRAYS  504.   
C****                                                                    505.   
      INCLUDE 'BA94jalC9.COM'                                    
c  ** MFS (ADDED)
      INCLUDE 'FORCINGSmac.COM'
      INCLUDE 'pd_COMMON'
c  ** deep ocean arrays
      COMMON/OCN/TG3M(IM,JM,12),RTGO(IM,JM,LM),STG3(IM,JM),DTG3(IM,JM)
c  ** END (ADDED)
      COMMON U,V,T,P,Q                                                   507.   
      DIMENSION JC(100),C(39),RC(161),  JC1(100),C1(39),RC1(161)         507.1  
      EQUIVALENCE (JC,IM0),(C,XLABEL),(RC,TAU)                           507.2
      CHARACTER*4 C,C1,RUNID                                             507.3  
            COMMON/WORKO/OA(IM,JM,11)                                    507.5  
      REAL*4 TAU4,TAU4Y,XX4                                              507.6  
      COMMON/WORK1/NLREC(256)                                            508.   
      COMMON/RADCOM/VADATA(11,4,3)                                       509.   
      CHARACTER*8 RECORD(10),NLREC*80,TITREG*80,NAMREG(23)               510.   
      COMMON/TNKREG/TITREG,NAMREG,KREG                                   510.1  
      character*8 GEOMETRY
      character*16 ocean_input
      common/orbit_parms_com/OMEGT,OBLIQ,ECCN  ! used in DAILY
      common/ocean_input_com/ocean_input
      NAMELIST/INPUTZ/IM0,JM0,LM0,LS1,LBLM,LMCM,LSSM,KOCEAN,ISTART,      513.   
     *  KDISK,TAUP,TAUI,TAUE,TAUT,TAUO,NDYN,NCNDS,NRAD,NSURF,NGRND,      514.   
     *  NFILTR,NDAA,NDA5D,NDA5K,NDA5S,NDA4,NDASF,DT,TAU,XINT,IYEAR,      515.   
     *  LHE,LHM,LHS,RADIUS,GRAV,RGAS,KAPA,OMEGA,CCMCX,ETA,S0X,CO2,SRCOR, 516.   
     *  PTOP,PSF,PSL,PTRUNC,DLAT,DLON,AREAG,IRAND,IJRA,MFILTR,NDIFS,     517.   
     *  KACC0,KEYCT,SKIPSE,USESLP,USEP,USET,KCOPY,XCDNST,IDACC,KDIAG,    518.   
     *  GEOMETRY,ocean_input,OMEGT,OBLIQ,ECCN,
c  ** MFS (ADDED)
c  ** check that taux in the restart file is close to the current month
     *  KCHECKTAUX,
c  ** END (ADDED)
     *  NDZERO,NDPRNT,IJD6,NAMD6,SIG,SIGE                                519.   
c  ** MFS (ADDED)
c  ** trends
      NAMELIST/FORCINGS/
     *  KTRENDEXT,
     *  ICO2DATA, ICO2DATASTART, ICO2DATAEND,
     *  ZN2O, IN2ODATA, IN2ODATASTART, IN2ODATAEND,
     *  CH4, ICH4DATA, ICH4DATASTART, ICH4DATAEND,
     *  F11, IF11DATA, IF11DATASTART, IF11DATAEND,
     *  F12, IF12DATA, IF12DATASTART, IF12DATAEND,
     *  IS0XDATA, IS0XDATASTART, IS0XDATAEND,
     *  VOL, IVOLDATA, IVOLDATASTART, IVOLDATAEND
c  ** END (ADDED)
      NAMELIST/PRINTD/
     *  iprint_pd,iu_pd,iform_pd
      DATA EDPERY/365./
c
      ISTART=10                                                          536.   
      ISTOP=0    !  added 1-23-2001                                      536.001
      OMEGT=282.9
      OBLIQ=23.44
      ECCN=.0167
c  ** MFS (ADDED)
c  ** KCHECKTAUX says whether to look at TAUX in the restart file so that
c  ** you can purposely start runs out of sync with the restart files.
      KCHECKTAUX = 1
c  ** ktrend used to be -CO2 but now it is an independent variable
c  ** if ktrend < 0 then there is no trend
c  ** ktrend = 0 is my new trends
c  ** ktrend > 0 is the old a trend, b trend, etc.
      KTRENDEXT = -1
c  ** END (ADDED)
c  ** MFS (CHANGED)
c  ** CO2 is now in ppm instead of multiples of 315 ppm
c  ** END (CHANGED)
c  ** MFS (DELETED)
C**** READ SPECIAL REGIONS FROM UNIT 29 - IF AVAILABLE                   581.1
CC       KREG=0                                                          581.2
CC       READ(29,NUM=LEN,END=25) TITREG,(JRG,I=1,IM*JM+2*23+1)           581.3
CC 25    IF (LEN.EQ.80+4*IM*JM+8*23) THEN                                581.4
           REWIND 29                                                     581.5
           READ(29) TITREG,JREG,NAMREG                                   581.6
           WRITE(6,*) ' read REGIONS from unit 29: ',TITREG              581.7
           KREG=1                                                        581.8
CC       END IF                                                          581.9
         DO 45 K=1,12                                                    588.   
   45    KDIAG(K)=0                                                      589.   
      WRITE (6,'(A,40X,A/)') '0','GISS N LAYER WEATHER MODEL'            594.
      GEOMETRY = '36x24'
      ocean_input='climatological'   ! or 'observed'
C**** Read in Label and Namelist parameters from rundeck                 595.   
c  ** MAC (CHANGED)
c  ** Open the namelist file this is file 110
c      READ(5,'(A80)') NLREC(1),NLREC(2)                                  595.1        
      OPEN(110,status='old',iostat=iosq)
      IF(IOSQ.NE.0) WRITE(6,*) 'Error opening unit 110 (Rundeck)'
      READ(110,'(A80)') NLREC(1),NLREC(2)
c  ** END (CHANGED)
      NOFF=0                                                             595.2  
      IF(NLREC(1)(73:80).EQ.'        ') NOFF=2                           595.3  
      DO 55 I=1,20-NOFF                                                  595.4  
   55 XLABEL(I)=NLREC(1)((4*I-3):(4*I))                                  595.5  
      DO 56 I=1,12+NOFF                                                  595.6  
   56 XLABEL(20-NOFF+I)=NLREC(2)((4*I-3):(4*I))                          595.7  
      RUNID=XLABEL(1)                                                    595.9  
      XLABEL(33) = ' '                                                   596.   
      WRITE (6,'(A,33A4/)') '0',XLABEL                                   597.   
C**** COPY INPUTZ NAMELIST ONTO CORE TAPE AND TITLE PAGE                 598.   
      IREC=0                                                             598.5  
   60 IREC=IREC+1                                                        599.   
c  ** MAC (CHANGED)
c      READ  (5,904) NLREC(IREC)                                          600.   
      READ  (110,'(A80)') NLREC(IREC)                                          
c  ** END (CHANGED)
      WRITE (6,'(35X,A80)') NLREC(IREC)                                  601.   
      IF (NLREC(IREC)(1:5).NE.' &END')  GO TO 60                         602.
c     READ (NLREC,INPUTZ)                                                604.   
      REWIND 8                                                           604.1
      WRITE(8,'(A)') (NLREC(IR),IR=1,IREC)                               604.2
      REWIND 8                                                           604.3
      READ (8,NML=INPUTZ)                                                604.4
      REWIND 8                                                           604.5
c  ** MAC (ADDED)
      READ (110,NML=FORCINGS)
c  ** END (ADDED)
C START PD
C**** Set values for pd which prevent files from being written
c     READ (5,PRINTD)  ! don't read this (but you could)
      do k=1,12
        iprint_pd(k) = 9   ! 9 inactivates
        iform_pd(k) = 1
        iNetcdf_pd(k) = 0
        iu_pd(k) = 200+k
      end do
        iu_pd(10) = 202 ! JL coordinates on same unit as JK
C END PD
      IF(ISTART.GE.10) GO TO 90                                          606.   
C**** SET STRICTLY DEPENDENT QUANTITIES                                  607.   
      DLON=TWOPI/IM                                                      608.   
      IF(JM.EQ.24.AND.IM.EQ.12) DLON=DLON/3.                             608.1  
      IF (GEOMETRY.EQ.'8x10' .or. GEOMETRY.EQ.'8X10') THEN
        DLAT=NINT(180./(JM-1))*TWOPI/360  ! True 8x10                    609.   
      ELSE
        DLAT=.5*TWOPI/(JM-1)  ! 4x5 or 7.8x10 
      ENDIF
      JMM1=JM-1                                                          610.   
      FIM=IM                                                             611.   
      LMM1=LM-1                                                          612.   
      LTM=LS1-1                                                          613.   
      DO 70 L=1,LM                                                       613.02
   70 SIG(L)=.5*(SIGE(L)+SIGE(L+1))                                      613.03
C     TAUERR=.5*DT/3600.                                                 613.09 
      TAUERR=0.                                                          613.091
C**** SET OPTIONALLY DEPENDENT QUANTITIES                                613.1  
      LSSM=LM                                                            614.   
      LMCM=LTM                                                           615.   
c  ** MFS (DELETED)
c  ** TAUP has been eliminated because I can check if a restart file is
c  ** in sync without it.  Likewise TAUO works now so it is no longer set
c  ** to TAUP.
c      TAUO=TAUP                                                          616.   
c      IF(TAUP.LT.0.) TAUP=TAUI                                           616.5  
c  ** END (DELETED)
      NCNDS=NDYN                                                         617.   
      NRAD=5*NDYN                                                        618.   
      NFILTR=2*NDYN                                                      619.   
         NDAA=7*NDYN+2                                                   620.   
         NDA5D=7*NDYN                                                    621.   
         NDA5K=NDAA                                                      622.   
         NDA5S=7*NDYN                                                    623.   
         NDA4=24*NDYN                                                    624.   
         NDASF=2*NSURF-1                                                 625.   
         KACC0=KACC                                                      626.   
C**** IDUM(1-4)=INDICATE LENGTHS OF RESTART FILE SEGMENTS                627.   
      IDUM(2)=KAIJ                                                       628.   
      IDUM(3)=KAJK                                                       629.   

c  ** MFS (ADDED)
c  ** Writes some more helpful information into the MWRE Window  
      CALL README2(XLABEL,GEOMETRY) 
c  ** END (ADDED)

c  ** do the istart
   90 GO TO (100,200,300,320,360,890,890,890,890,400,440,440,400),ISTART 630.   
      GO TO 400                                                          631.   
C****                                                                    632.   
C**** FUNCTIONAL DETERMINATION OF PROGNOSTIC QUANTITIES, ISTART=1        633.   
C****                                                                    634.   
  100 TEMP=250.                                                          635.   
      DO 105 J=1,JM                                                      636.   
      DO 105 I=1,IM                                                      637.   
      P(I,J)=PSF-PTOP                                                    638.   
  105 BLDATA(I,J,2)=TEMP                                                 639.   
      DO 120 L=1,LM                                                      643.   
      DO 120 J=1,JM                                                      636.   
      DO 120 I=1,IM                                                      637.   
      U(I,J,L)=0.                                                        644.   
      V(I,J,L)=0.                                                        645.   
      T(I,J,L)=TEMP                                                      646.   
  120 Q(I,J,L)=3.D-6                                                     647.   
      GO TO 210                                                          649.   
CALT  DEFINE GDATA(1-14) AND GO TO 220                                   649.1  
C****                                                                    650.   
C**** INITIALIZE A RUN FROM ATMOSPHERIC CONDITIONS ON UNIT 9 AND         651.   
C**** GROUND CONDITIONS ON UNIT 7, ISTART=2                              652.   
C****                                                                    653.   
  200 READ (9,ERR=800) TAUX,JC1,C1,RC1,U,V,T,P,Q,                        657.   
     *  ((BLDATA(I,J,2),I=1,IM),J=1,JM)                                  658.   
      WRITE (6,'(A,I4,F11.2,3X,20A4/)')                                  659.   
     *  '0ATMOSPHERIC I.C. ISTART,TAUX=',ISTART,TAUX,(C1(K),K=1,20)           
      REWIND 9                                                           661.   
  210 READ (7,ERR=830) GDATA                                             663.   
      REWIND 7                                                           664.   
  220 TAU=TAUI                                                           665.   
C**** SET SURFACE WINDS FROM FIRST LAYER WINDS                           666.   
      BLDATA(1,1,1)=SQRT(U(1,2,1)*U(1,2,1)+V(1,2,1)*V(1,2,1))            667.   
      BLDATA(1,1,6)=U(1,2,1)                                             668.   
      BLDATA(1,1,7)=V(1,2,1)                                             669.   
      BLDATA(1,JM,1)=SQRT(U(1,JM,1)*U(1,JM,1)+V(1,JM,1)*V(1,JM,1))       670.   
      BLDATA(1,JM,6)=U(1,JM,1)                                           671.   
      BLDATA(1,JM,7)=V(1,JM,1)                                           672.   
      DO 225 J=2,JM-1                                                    673.   
      IM1=IM                                                             674.   
      DO 225 I=1,IM                                                      675.   
      BLDATA(I,J,1)=.25*SQRT(                                            676.   
     *   (U(IM1,J,1)+U(I,J,1)+U(IM1,J+1,1)+U(I,J+1,1))**2                677.   
     *  +(V(IM1,J,1)+V(I,J,1)+V(IM1,J+1,1)+V(I,J+1,1))**2)               678.   
      BLDATA(I,J,6)=.25*(U(IM1,J,1)+U(I,J,1)+U(IM1,J+1,1)+U(I,J+1,1))    679.   
      BLDATA(I,J,7)=.25*(V(IM1,J,1)+V(I,J,1)+V(IM1,J+1,1)+V(I,J+1,1))    680.   
  225 IM1=I                                                              681.   
      CDM=.001                                                           682.   
      DO 260 J=1,JM                                                      683.   
      DO 260 I=1,IM                                                      684.   
C**** SET SURFACE MOMENTUM TRANSFER TAU0                                 685.   
      BLDATA(I,J,8)=CDM*BLDATA(I,J,1)**2                                 686.   
C**** SET LAYER THROUGH WHICH DRY CONVECTION MIXES TO 1                  687.   
      BLDATA(I,J,4)=1.                                                   688.   
C**** SET SURFACE SPECIFIC HUMIDITY FROM FIRST LAYER HUMIDITY            689.   
      BLDATA(I,J,3)=Q(I,J,1)                                             690.   
C**** SET RADIATION EQUILIBRIUM TEMPERATURES FROM LAYER LM TEMPERATURE   691.   
      DO 230 K=1,3                                                       692.   
  230 RQT(I,J,K)=T(I,J,LM)                                               693.   
C**** REPLACE TEMPERATURE BY POTENTIAL TEMPERATURE                       694.   
      DO 240 L=1,LM                                                      695.   
  240 T(I,J,L)=T(I,J,L)/EXPBYK(SIG(L)*P(I,J)+PTOP)                       696.   
      IF(LS1.GT.LM) GO TO 260                                            697.   
C**** SET STRATOSPHERIC SPECIFIC HUMIDITY TO 3.D-6                       698.   
      DO 250 L=LS1,LM                                                    699.   
  250 Q(I,J,L)=3.D-6                                                     700.   
  260 CONTINUE                                                           701.   
C**** INITIALIZE TSFREZ AND PALMER                                       701.1  
  270    DO 280 J=1,JM                                                   701.2  
         DO 280 I=1,IM                                                   701.3  

c  ** MFS (ADDED)
c  ** initialize a bunch of deep ocean arrays
c  ** old code from other model was
c      DO 275 L=1,LM                                                      701.31 
c  275 RTGO(I,J,L)=SIGE(37)                                               701.32 
c      STG3(I,J)=0.                                                       701.33 
c      DTG3(I,J)=0.                                                       701.34 
      IF(KOCEAN.EQ.2)THEN
        DO 275 L=1,LM                                                   
  275     RTGO(I,J,L)=SIGE(37)                                          
        STG3(I,J)=0.                                                     
        DTG3(I,J)=0.                                                     
      END IF
c  ** END (ADDED)

         TSFREZ(I,J,1)=365.                                              701.4  
         TSFREZ(I,J,2)=365.                                              701.5  
CF       PALMER(I,J)=0.                                                  701.95 
  280 CONTINUE                                                           701.96 
      GO TO 500                                                          702.   
C****                                                                    703.   
C**** INITIALIZE RUN FROM PREVIOUS MODEL OUTPUT ON UNIT 9, ISTART=3,4,5  704.   
C****                                                                    705.   
C**** C ARRAY IS BUILT UP FROM DEAULTS AND NAMELIST, ODATA IS READ IN    706.   
c  ** MAC (CHANGED)
c  ** Unit 9 is reserved by Fortran. The corrected code uses unit 109.
c  300 READ (9,ERR=800,END=810) TAUX,JC1,C1,RC1,KEYNR,U,V,T,P,Q,ODATA,    707.   
c     *  GDATA,BLDATA,RQT                                                 707.1  
  300 CONTINUE
c      OPEN(109,FORM='unformatted',IOSTAT=iosq,STATUS='old')
c      IF(IOSQ.NE.0) WRITE(6,904) 'ERROR OPENING RESTART FILE, UNIT=109'
      IF(KOCEAN.NE.2)THEN
      READ (109,ERR=800,END=810) TAUX,JC1,C1,RC1,KEYNR,U,V,T,P,Q,ODATA,   
     *  GDATA,BLDATA,RQT 
      ELSE
      READ (109,ERR=800,END=810) TAUX,JC1,C1,RC1,KEYNR,U,V,T,P,Q,ODATA,   
     *  GDATA,BLDATA,RQT,SRHR,TRHR,TSFREZ,TG3M
      END IF                     
c  ** auto detect if the restart file is for the wrong month
c  ** corrected to use variable length years/months
c  ** now includes check to allow for misaligned restarts
c      IF(TAUX.LT.TAUP-TAUERR) GO TO 300                                  707.2  
c      IF(TAUX.GT.TAUP+TAUERR.AND.TAUP.GE.0.) GO TO 820 =                 707.3
C      IF((KCHECKTAUX.GT.0).AND.(MOD(TAUI-TAUX,8760.).GT.0.))GO TO 820
c  ** changed for xlf compadiblity so 8760 is now copied into YEARLEN
c  ** since otherwise it complained about mod intrinsics
      YEARLEN = 8760.
      IF((KCHECKTAUX.GT.0).AND.(MOD(TAUI-TAUX,YEARLEN).GT.0.))GO TO 820
c  ** END (CHANGED)
      GO TO 380                                                          708.   
C**** C ARRAY IS COPIED FROM INPUT DATA EXCEPT FOR XLABEL                709.   
c  ** MAC (CHANGED)
c  ** Unit 9 is reserved by Fortran. The corrected code uses unit 109.
c  320 READ(9,ERR=800)TAUX,JC1,C1,RC1,KEYNR,U,V,T,P,Q,ODATA,GDATA,BLDATA, 710.   
c     *  RQT,SRHR,TRHR,TSFREZ                                             711.   
c      REWIND 9                                                           712.   
  320 CONTINUE
c      OPEN(109,FORM='unformatted',IOSTAT=iosq,STATUS='old')
c      IF(IOSQ.NE.0) WRITE(6,904) 'ERROR OPENING RESTART FILE, UNIT=109'
      IF(KOCEAN.NE.2)THEN
      READ(109,ERR=800)TAUX,JC1,C1,RC1,KEYNR,U,V,T,P,Q,ODATA,GDATA,
     *  BLDATA,RQT,SRHR,TRHR,TSFREZ    
      ELSE
      READ(109,ERR=800)TAUX,JC1,C1,RC1,KEYNR,U,V,T,P,Q,ODATA,GDATA,
     *  BLDATA,RQT,SRHR,TRHR,TSFREZ,TG3M,RTGO,STG3,DTG3
      END IF   
      REWIND 109
c  ** END (CHANGED)
      WRITE (6,'(A,I4,F11.2,3X,20A4/)')                                  713.   
     *  '0ATMOSPHERIC I.C. ISTART,TAUX=',ISTART,TAUX,(C1(K),K=1,20)           
      DO 330 K=1,100                                                     714.   
  330 JC(K)=JC1(K)                                                       715.   
      DO 335 K=34,39                                                     716.   
  335 C(K)=C1(K)                                                         717.   
      DO 340 K=1,161                                                     717.1  
  340 RC(K)=RC1(K)                                                       717.2  
      GO TO 500                                                          718.   
C**** Like ISTART=3 except GDATA is read in from a separate file (7)     719.   
c  ** MAC (CHANGED)
c  ** Unit 9 is reserved by Fortran. The corrected code uses unit 109.
c  360 READ (9,ERR=800) TAUX,JC1,C1,RC1,KEYNR,U,V,T,P,Q,ODATA,            720.   
c     *  GDATA,BLDATA,RQT                                                 720.1  
c      CALL DREAD (7,GDATA,IM*JM*14,GDATA)                                720.2  
c      REWIND 7                                                           720.3  
c  380 REWIND 9                                                           721.   
  360 CONTINUE
c      OPEN(109,FORM='unformatted',IOSTAT=iosq,STATUS='old')
c      IF(IOSQ.NE.0) WRITE(6,904) 'ERROR OPENING RESTART FILE, UNIT=109'
      IF(KOCEAN.NE.2)THEN
      READ (109,ERR=800) TAUX,JC1,C1,RC1,KEYNR,U,V,T,P,Q,ODATA,            
     *  GDATA,BLDATA,RQT                                                 720.1  
      ELSE
      WRITE(6,*) 'You cannot start a deep ocean run on istart=5.'
      GOTO 890
      END IF
      CALL DREAD (7,GDATA,IM*JM*14,GDATA)                                720.2  
      REWIND 7                                                           720.3  
  380 REWIND 109
c  ** END (CHANGED)
      IF(TAUI.LT.0.) TAUI=TAUX                                           722.   
      TAU=TAUI                                                           723.   
      WRITE (6,'(A,I4,F11.2,3X,20A4/)')                                  724.   
     *  '0ATMOSPHERIC I.C. ISTART,TAUX=',ISTART,TAUX,(C1(K),K=1,20)           
      CDM=.001                                                           682.   
      GO TO 270                                                          725.   
C****                                                                    726.   
C**** RESTART ON DATA SETS 1 OR 2, ISTART=10-13                          727.   
C****                                                                    728.   
C**** CHOOSE DATA SET TO RESTART ON                                      729.   
  400 TAU1=-1.                                                           730.   
      READ (1,ERR=410) TAU1                                              731.   
  410 REWIND 1                                                           732.   
      TAU2=-1.                                                           733.   
      READ (2,ERR=420) TAU2                                              734.   
  420 REWIND 2                                                           735.   
      KDISK=1                                                            736.   
      IF(TAU1+TAU2.LE.-2.) GO TO 850                                     737.   
      IF(TAU2.GT.TAU1) KDISK=2                                           738.   
      IF(ISTART.GE.13) KDISK=3-KDISK                                     739.   
      GO TO 450                                                          740.   
  440 KDISK=ISTART-10                                                    741.   
C**** RESTART ON UNIT KDISK                                              742.   
  450 KDISK0=KDISK                                                       744.   
  460 READ (KDISK0,ERR=840) TAUX,JC,C,RC,KEYNR,U,V,T,P,Q,ODATA,GDATA,    745.   
     *  BLDATA,                                                          745.5  
c  ** MFS (CHANGED)
c  ** Read in the extra deep ocean arrays even if they aren't used.
c     *  RQT,SRHR,TRHR,TSFREZ,(AJ(K,1),K=1,KACC),TDIURN,OA,TAUY           746.   
     *  RQT,SRHR,TRHR,TSFREZ,TG3M,RTGO,STG3,DTG3,                        745.6  
     *  (AJ(K,1),K=1,KACC),TDIURN,TAUY                                   746.   
c  ** This code appears to write a longer IDUM array for deep ocean
c  ** I've tried to conditialize this but I may be wrong.
c  ** old version from this code
c      IDUM(1)=IM*JM*(4*LM+1 + 5+14+8 + 3+2*(LM+1) + 2)                   746.1  
c  ** old version from other code
c      IDUM(1)=IM*JM*(4*LM+1 + 5+14+8 + 3+2*(LM+1) + 2 + 12+LM+2)         746.1 
      IF(KOCEAN.EQ.2)THEN
        IDUM(1)=IM*JM*(4*LM+1 + 5+14+8 + 3+2*(LM+1) + 2 + 12+LM+2)
      ELSE     
        IDUM(1)=IM*JM*(4*LM+1 + 5+14+8 + 3+2*(LM+1) + 2) 
      END IF
c  ** END (CHANDED)
      IDUM(4)=IM*JM*KTD                                                  746.2  
      IDUM(4)=IM*JM*11+IM*JM*KTD                                         746.3  
      REWIND KDISK0                                                      747.   
      KDISK=KDISK0                                                       749.   
      IF (RUNID.NE.XLABEL(1)) THEN                                       749.5
         WRITE (6,'(A,A4,A,A4)')                                         749.6
     *     ' THIS RESTART FILE IS FOR RUN',XLABEL(1),' NOT RUN',RUNID    749.7
         STOP 'ERROR: WRONG RESTART FILES, MISMATCHED LABELS'            749.8
      ENDIF                                                              749.9
      IF(TAUX.NE.TAUY) GO TO 860                                         750.   
      WRITE (6,'(A,I2,A,F11.2,A,20A4/)') '0RESTART DISK READ, UNIT',     751.
     *  KDISK,', TAUX=',TAUX,' ',(C(K),K=1,20)                           751.1
      IF(ISTART.GT.10) KDISK=3-KDISK                                     752.   
      TAU=TAUX                                                           753.   
c  ** MFS (DELETED)
c      TAUP=TAUX                                                          754.   
c  ** END (DELETED)
C**** UPDATE C ARRAY FROM INPUTZ                                         755.   
  500 REWIND 8                                                           756.1
      WRITE(8,'(A)') (NLREC(IR),IR=1,IREC)                               756.2
      REWIND 8                                                           756.3
      READ (8,NML=INPUTZ)                                                756.4
      REWIND 8                                                           756.5
C     TAUERR=.5*DT/3600.                                                 757.1  
      TAUERR=0.                                                          757.101
      IF(TAU-TAUERR.GT.TAUE) STOP 13                                     757.2  
c  ** MFS (DELETED)
c  ** Reto says this is obsolete
c      IF(IM0.LT.IM.OR.JM0.LT.JM.OR.LM0.LT.LM) THEN                       757.3  
c      WRITE (6,'('' ARRAY-DIMENSIONS IM0,JM0,LM0 '',3I3,                 757.4  
c     *  '' ARE INSUFFICIENT FOR IM,JM,LM='',3I3)') IM0,JM0,LM0,IM,JM,LM  757.5  
c      STOP ' ERROR IN GRID SIZE DIMENSIONS '                             757.6  
c      END IF                                                             757.7  
c  ** END (DELETED)
c  ** MFS (CHANGED)
c      IF(ISTART.GE.10.AND.TAU.LT.TAUP-TAUERR) GO TO 900                  758.   
      IF(ISTART.GE.10.AND.TAU.LT.TAUX-TAUERR) GO TO 900 
c  ** END (CHANDED)
         IF(USESLP.LE.0.) GO TO 515                                      758.01 
C****    REPOSITION THE SEA LEVEL PRESSURE HISTORY DATA SET (UNIT 16)    758.02 
         REWIND 16                                                       758.03 
  510    READ (16,ERR=870,END=880) TAU4,((XX4,I=1,IM),J=1,JM),TAU4Y      758.04 
c  ** MFS (CHANGED)
c  ** This error now reads 'The ocean flux collection file is missing'
c  ** instead of 'Error on both restart files' which was the same error
c  ** as damaged restart files would give. Reto says that the old error
c  ** messages predate block if statements (if else) and that they 
c  ** should have used seperate messages for each error.
c         IF(TAU4.NE.TAU4Y) GO TO 860                                     758.05 
         IF(TAU4.NE.TAU4Y) GO TO 865                             
c  ** END (CHANGED)
         IF(TAU.GE.TAU4+USESLP-TAUERR) GO TO 510                         758.06 
         WRITE (6,'(A,F11.2/)')                                          758.07
     *   '0SLP HISTORY REPOSITIONED.  LAST TAU READ WAS',TAU4            758.071
  515    CONTINUE                                                        758.08 
      IF(USET.LE.0.) GO TO 600                                           759.   
C**** REPOSITION THE OUTPUT TAPE ON UNIT 20 FOR RESTARTING               760.   
c  ** MFS (ADDED)
c  ** Explicitly open unit 20 so that the ocean flux file is rewound
c  ** properly.
c  ** 9/X (SWITCH)
c      OPEN(UNIT=20,FILE=':ocean:OceanFlux',FORM='unformatted')
      OPEN(UNIT=20,FILE='./ocean/OceanFlux',FORM='unformatted')
c      OPEN(UNIT=20,FILE='./OceanFlux',FORM='unformatted')
c  ** END (SWITCH)
c  ** END (ADDED)
      REWIND 20                                                          760.01 
      IF(TAU.LT.TAUO+USET-TAUERR) GO TO 600                              761.   
  520 READ (20,ERR=870,END=880) TAUX                                     762.   
      IF(TAU.GE.TAUX+USET-TAUERR) GO TO 520                              763.   
      WRITE (6,'(A,F11.2/)')                                             764.
     *  '0OUTPUT TAPE REPOSITIONED.  LAST TAU READ WAS',TAUX             764.1
C****                                                                    765.   
C**** CONSTANT ARRAYS TO BE CALCULATED OR READ IN EACH RUN               766.   
C****                                                                    767.   
  600 IF(KEYCT.GT.1) GO TO 610                                           767.1  
      DO 605 K=1,2100                                                    767.2  
  605 KEYNR(K,1)=0                                                       767.3  
  610 CONTINUE
C**** CALCULATE SPHERICAL GEOMETRY                                       770.   
      IF (GEOMETRY.EQ.'8x10' .or. GEOMETRY.EQ.'8X10') THEN
        CALL GEOM_8x10   ! True 8x10
      ELSE
        CALL GEOM        ! 4x5 or 7.8x10   
      ENDIF
C**** CALCULATE DSIG AND DSIGO                                           816.   
      DO 700 L=1,LM                                                      817.   
  700 DSIG(L)=SIGE(L)-SIGE(L+1)                                          818.   
      DO 710 L=1,LM-1                                                    819.   
  710 DSIGO(L)=SIG(L)-SIG(L+1)                                           820.   
C**** READ IN FDATA: PHIS, PLAND AND RLICE                               821.   
      CALL DREAD (26,FDATA,IM*JM*3,FDATA)                                822.   
      REWIND 26                                                          823.   
C**** READ IN MAXIMUM MIXED LAYER DEPTHS FOR PREDICTED OCEAN RUNS        824.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.NE.1) GO TO 765                                          825.   
      IF(KOCEAN.EQ.0) GO TO 765                              
c  ** END (CHANDED)
      CALL DREAD (25,Z12O,IM*JM,Z12O)                                    826.   
      REWIND 25                                                          827.   
C**** IF GDATA(I,J,1)<0, THE OCEAN PART WAS CHANGED TO LAND ICE          827.1  
C**** BECAUSE THE OCEAN ICE REACHED THE MAX MIXED LAYER DEPTH            827.2  
      DO 763 J=1,JM                                                      827.3  
      DO 763 I=1,IM                                                      827.4  
      IF(GDATA(I,J,1).GE.-1.) GO TO 763                                  827.5  
      FDATA(I,J,3)=1.-FDATA(I,J,2)*(1.-FDATA(I,J,3))                     827.6  
      FDATA(I,J,2)=1.                                                    827.7  
      WRITE(6,'(2I3,'' OCEAN WAS CHANGED TO LAND ICE'')') I,J            827.8  
  763 CONTINUE                                                           827.9  
C**** READ IN VEGETATION DATA SET: VDATA AND VADATA                      828.   
C 765 READ (23) (((VDATA(I,J,K),I=1,IM),J=1,JM),K=1,8),                  829.   
C    *  (((VADATA(I,J,K),I=1,8),J=1,4),K=1,3)                            829.1  
  765 CALL DREAD (23,VDATA,IM*JM*8+8*4*3,VDATA)                          829.2  
C     ARRAY AFTER VDATA IS NOT YET DEFINED (Z1O)                         829.3  
      N=IM*JM*8+1                                                        829.4  
      DO 766 K=1,3                                                       829.5  
      DO 766 J=1,4                                                       829.6  
      DO 766 I=1,8                                                       829.7  
      VADATA(I,J,K)=VDATA(N,1,1)                                         829.8  
  766 N=N+1                                                              829.9  
      REWIND 23                                                          830.   
C**** MODIFY THE VADATA IF DESIRED                                       831.   
C     NO MODIFICATIONS                                                   832.   
C****                                                                    833.   
C**** COMPUTE WATER FIELD CAPACITIES FOR GROUND LAYERS 1 AND 2           833.1  
C****                                                                    833.2  
      IOFF=0                                                             833.3  
            IF(VADATA(4,2,3).LT.100.) IOFF=1                             833.4  
      ERROR=.001                                                         833.5  
      DEFLT=24.                                                          834.   
      DO 785 L=1,2                                                       835.   
      DO 780 J=1,JM                                                      836.   
      DO 780 I=1,IM                                                      837.   
      IF(ODATA(I,J,L).LT.-10.) ODATA(I,J,L)=-10.                         837.1  
      IF(ODATA(I,J,L+3).LT.-10.) ODATA(I,J,L+3)=-10.                     837.2  
      IF(AIJ(I,J,L+58).LT.-1.D20) AIJ(I,J,L+58)=-10.                     837.3  
      PEARTH=FDATA(I,J,2)*(1.-FDATA(I,J,3))                              838.   
      WFCIJL=0.                                                          839.   
      DO 770 K=1,8                                                       840.   
  770 WFCIJL=WFCIJL+VDATA(I,J,K)*VADATA(K,L+IOFF,3)                      841.   
      IF(WFCIJL.LT.1.) WFCIJL=DEFLT                                      842.   
      IF(PEARTH.LE.0.) GO TO 780                                         843.   
C**** CHECK AND CORRECT GDATA OVER EARTH (OR STOP RUN IF RESTART)        843.01 
      TGL=GDATA(I,J,4*L)                                                 843.02 
      WTRL=GDATA(I,J,4*L+1)                                              843.03 
      ACEL=GDATA(I,J,4*L+2)                                              843.04 
      SNOW=GDATA(I,J,2)                                                  843.05 
      IF(WTRL.GE.0..AND.ACEL.GE.0..AND.TGL*WTRL.GE.0..AND.TGL*ACEL.LE.0. 843.06 
     *  .AND.SNOW.GE.0..AND.SNOW*GDATA(I,J,4).LE.0.) GO TO 775           843.07 
      WRITE (6,774) I,J,L,(GDATA(I,J,K),K=2,10)                          843.08 
      WRITE(6,923) I,J,(FDATA(I,J,K),K=1,5)                              843.081
  923   FORMAT('... FDATA AT I,J ', 2I3, 5F10.4)                         843.082
  774 FORMAT (' ...BAD GDATA AT I,J,L ',3I3,10F10.4)                     843.09 
      IF(ISTART.GE.10.AND.WTRL.GE.-ERROR.AND.ACEL.GE.-ERROR.AND.         843.1  
     *  SNOW.GE.-ERROR) GO TO 775                                        843.11 
      IF(WTRL.LT.0.) GDATA(I,J,4*L+1)=0.                                 843.12 
      IF(ACEL.LT.0.) GDATA(I,J,4*L+2)=0.                                 843.13 
      IF(TGL.LT.0.) GDATA(I,J,4*L+2)=GDATA(I,J,4*L+1)+GDATA(I,J,4*L+2)   843.14 
      IF(TGL.LT.0.) GDATA(I,J,4*L+1)=0.                                  843.15 
      IF(TGL.GT.0.) GDATA(I,J,4*L+1)=GDATA(I,J,4*L+1)+GDATA(I,J,4*L+2)   843.16 
      IF(TGL.GT.0.) GDATA(I,J,4*L+2)=0.                                  843.17 
      IF(SNOW.LT.0..OR.GDATA(I,J,4).GT.0.) GDATA(I,J,2)=0.               843.18 
      IF(ISTART.GE.10) STOP 'ERROR: NEGATIVE WATER, ICE, OR SNOW'        843.19 
  775 IF(GDATA(I,J,4*L+1)+GDATA(I,J,4*L+2).LE.WFCIJL) GO TO 780          844.   
      WRITE (6,774) I,J,L,(GDATA(I,J,K),K=2,10),WFCIJL                   844.1  
      IF(ISTART.GE.10.AND.(GDATA(I,J,4*L+1)+GDATA(I,J,4*L+2)).LE.        844.2  
     *  (1.+ERROR)*WFCIJL) GO TO 780                                     844.3  
      X=WFCIJL/(GDATA(I,J,4*L+1)+GDATA(I,J,4*L+2)+1.D-3)                 845.   
      GDATA(I,J,4*L+1)=GDATA(I,J,4*L+1)*X                                846.   
      GDATA(I,J,4*L+2)=GDATA(I,J,4*L+2)*X                                847.   
      IF(ISTART.GE.10) STOP 'ERROR: WATER EXCEEDS WATER FIELD CAPACITY'  847.1  
  780 VDATA(I,J,L+8)=WFCIJL                                              848.   
      DEFLT=60.                                                          849.   
  785 CONTINUE                                                           850.   
C**** CONVERT MASKING DEPTHS FROM METERS TO KG/M**2                      850.1  
C        HERE WE USE 100 KG/M**3 FOR THE DENSITY OF SNOW (RHOS)          850.2  
C        IN SUBROUTINE SURFACE THE MORE PRECISE VALUE 99.66 IS USED      850.3  
      RHOS=100.                                                          850.4  
C           NO CONVERSION NEEDED FOR OLD VEGETATION FILES                850.5  
            IF(VADATA(4,2,3).LT.100.) RHOS=1.                            850.6  
      DO 790 K=1,11                                                      850.7  
  790 VADATA(K,4,3)=RHOS*VADATA(K,4,3)                                   850.8  
      CALL RINIT (IRAND)                                                 851.   
      CALL FRTR0 (IM)                                                    851.5  
C**** MAKE NRAD A MULTIPLE OF NCNDS                                      852.   
      NRAD=(MAX(NRAD,NCNDS)/NCNDS)*NCNDS                                 852.5  
      IF(KDIAG(2).EQ.9.AND.SKIPSE.EQ.0..AND.KDIAG(3).LT.9) KDIAG(2)=8    852.6  
         KACC0=KACC
      if (ocean_input.ne.'observed' .and. ocean_input.ne.'OBSERVED'
     *  .and. ocean_input.ne.'climatological' 
     *  .and. ocean_input.ne.'CLIMATOLOGICAL') then
        write(*,*) ' Invalid ocean_input: ',ocean_input
        stop       ' Invalid ocean_input'
      end if
C**** Upper-case the variable now so we don't have to do it ever again
      if (ocean_input.eq.'observed') ocean_input = 'OBSERVED'
      if (ocean_input.eq.'climatological')ocean_input='CLIMATOLOGICAL'
      WRITE (6,INPUTZ)                                                   853.   
      WRITE (6,FORCINGS)                                                 853.   
      RETURN                                                             854.   
C****                                                                    855.   
C**** TERMINATE BECAUSE OF IMPROPER PICK-UP                              856.   
C****                                                                    857.   
  800 WRITE (6,'(A,I4)')                                                 858.
     *  '0ERROR ENCOUNTERED READING I.C. ON UNIT 9.  ISTART=', ISTART    858.1
      STOP 'READ ERROR FOR I.C. ON UNIT 9 (or 109)'                      859.
  810 WRITE (6,'(A,2F11.2)')                                             860.
     *  '0EOF ON UNIT 9.  LATER I.C. NEEDED. TAUP,TAUX=', TAUP,TAUX      860.1
      STOP 'ERROR: ALL TAUS<TAUP ON I.C. FILE ON UNIT 9 (or 109)'        861.
  820 WRITE (6,'(A,2F11.2)')                                             862.   
     *  '0EARLIER I.C. NEEDED ON UNIT 9. TAUP,TAUX=',TAUP,TAUX           862.1  
      STOP 'ERROR: ALL TAUS>TAUP ON I.C. FILE ON UNIT 9 (or 109)'        863.   
  830 WRITE(6,*) '0ERROR READING GROUND CONDITIONS ON UNIT 7'            862.
      STOP 'READ ERROR ON UNIT 7'                                        863.
  840 IF(3-KDISK.EQ.KLAST) GO TO 850                                     866.   
      REWIND KDISK                                                       867.   
      KLAST=KDISK                                                        868.   
      KDISK=3-KDISK                                                      869.   
      WRITE (6,'(A,I3/,A,I1)')                                           870.
     *  '0ERROR ENCOUNTERED READING RESTART TAPE ON UNIT',KLAST,         870.1
     *  '  TRY TO RESTART THE JOB WITH ISTART=3,KDISK=',KDISK            870.2
      GO TO 450                                                          871.   
  850 WRITE (6,'(A)')                                                    872.
     *  '0ERRORS ON BOTH RESTART DATA SETS. TERMINATE THIS JOB'          872.1
      STOP 'ERRORS ON BOTH RESTART FILES'                                873.
  860 WRITE (6,'(A,2F11.2/,A)')                                          874.
     *  '0TAUX,TAUY=',TAUX,TAUY,'0DISK RESTART FILE DESTROYED'           874.01
  861 KDISK0=3-KDISK0                                                    874.1  
      IF(ISTART.EQ.57) GO TO 850                                         874.2  
      ISTART=57                                                          874.3  
      GO TO 460                                                          874.4  
c  ** MFS (ADDED)
  865 WRITE (6,'(A,2F11.2/A)') 
     *  '0TAU4,TAU4Y=',TAU4,TAU4Y,'0Ocean flux date mismatch'
      STOP 'The ocean flux collection file is damaged'
c  ** END (ADDED)
  870 WRITE (6,'(A,2F11.2)') '0ERROR ENCOUNTERED REPOSITIONING UNIT 16 O 876.
     *R 20.    TAUX,TAU=', TAUX,TAU                                      876.1
      IF (ISTART.EQ.10) GO TO 861  ! modified 1-23-2001                  876.5
      STOP 'READ ERROR ON OUTPUT FILE ON UNIT 16 OR 20'                  877.
  880 WRITE (6,'(A,2F11.2)') '0EOF ON UNIT 16 OR 20 WHILE REPOSITIONING. 878.
     *  TAUX,TAU=', TAUX,TAU                                             878.1
      IF (ISTART.EQ.10) GO TO 861  ! modified 1-23-2001                  878.5
      STOP 'POSITIONING ERROR: EOF REACHED ON UNIT 16 OR 20'             879.
  890 WRITE (6,'(A,I5)') '0INCORRECT VALUE OF ISTART',ISTART             880.
      STOP 'ERROR: ISTART-SPECIFICATION INVALID'                         881.
  900 WRITE (6,'(A,F11.2,A,F11.2,A)')                                    882.
     *  '0PREVIOUS TAUE=',TAUP,' WAS NOT YET REACHED. TAU=',TAU,         882.1
     *  ' RESUBMIT JOB WITH AN EARLIER TAUE CARD'                        882.2
      STOP 'ERROR: TAUE TOO LARGE, SINCE TAU<TAUP'                       883.
      END                                                                884.   

      SUBROUTINE DYNAM
C****
C**** INTEGRATE THE DYNAMICS TERMS
C****
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q      
      COMMON/WORK6/WORKX(IM,JM,LM),UT(IM,JM,LM),VT(IM,JM,LM),     
     *  TT(IM,JM,LM),PT(IM,JM),QT(IM,JM,LM)                      
      COMMON/WORK2/UX(IM,JM,LM),VX(IM,JM,LM),TX(IM,JM,LM),PX(IM,JM)
      DIMENSION PA(IM,JM),PB(IM,JM),PC(IM,JM)

      DTFS=DT*2./3.
      DTLF=2.*DT
      NDYNO=MOD(NDYN,2)
      DO 305 L=1,LM*3+1
      DO 305 J=1,JM
      DO 305 I=1,IM                                                       91.   
      UX(I,J,L)=U(I,J,L)                                                  92.   
  305 UT(I,J,L)=U(I,J,L)                                                  93.   
      DO 310 L=1,LM                                                       94.   
      DO 310 J=1,JM
      DO 310 I=1,IM                                                       95.   
  310 QT(I,J,L)=Q(I,J,L)                                                  96.   
      DO 311 J=1,JM
      DO 311 I=1,IM
      PA(I,J)=P(I,J)
      PB(I,J)=P(I,J)
  311 PC(I,J)=P(I,J)
C**** INITIAL FORWARD STEP, QX = Q + .667*DT*F(Q)                         97.   
      NS=0                                                                98.   
      MRCH=0                                                              99.   
C     CALL DYNAM (UX,VX,TX,PX,Q,U,V,T,P,Q,DTFS)                          100.   
      CALL AFLUX (U,V,P)
      CALL ADVECM (P,PB,DTFS)
      CALL ADVECV (P,UX,VX,PB,U,V,P,DTFS)
      CALL ADVECT (P,TX,PB,T,DTFS)
      CALL PGF (UX,VX,PB,U,V,T,P,DTFS)
      IF(NDYNO.EQ.1) GO TO 320                                           101.   
C**** INITIAL BACKWARD STEP IS ODD, QT = QT + DT*F(QX)                   102.   
      MRCH=-1                                                            103.   
C     CALL DYNAM (UT,VT,TT,PT,QT,UX,VX,TX,PX,Q,DT)                       104.   
      CALL AFLUX (UX,VX,PB)
      CALL ADVECM (P,PA,DT)
      CALL ADVECV (P,UT,VT,PA,UX,VX,PB,DT)
      CALL ADVECT (P,TT,PA,TX,DT)
      CALL ADVECQ (P,QT,PA,Q,DT)
      CALL PGF (UT,VT,PA,UX,VX,TX,PB,DT)
      GO TO 360                                                          105.   
C**** INITIAL BACKWARD STEP IS EVEN, Q = Q + DT*F(QX)                    106.   
  320 NS=1                                                               107.   
         MODD5K=MOD(NSTEP+NS-NDYN+NDA5K,NDA5K)                           108.   
      MRCH=1                                                             109.   
C     CALL DYNAM (U,V,T,P,Q,UX,VX,TX,PX,QT,DT)                           110.   
CD       DIAGA SHOULD BE CALLED HERE BUT THEN ARRAYS MUST BE CHANGED     111.   
C**** ODD LEAP FROG STEP, QT = QT + 2*DT*F(Q)                            112.   
  340 MRCH=-2                                                            113.   
C     CALL DYNAM (UT,VT,TT,PT,QT,U,V,T,P,Q,DTLF)                         114.   
      CALL AFLUX (U,V,P)
      CALL ADVECM (PA,PB,DTLF)
      CALL ADVECV (PA,UT,VT,PB,U,V,P,DTLF)
      CALL ADVECT (PA,TT,PB,T,DTLF)
      CALL ADVECQ (PA,QT,PB,Q,DTLF)
      CALL PGF (UT,VT,PB,U,V,T,P,DTLF)
C**** LOAD PB TO PA
      DO 341 J=1,JM
      DO 341 I=1,IM
  341 PA(I,J)=PB(I,J)
C**** EVEN LEAP FROG STEP, Q = Q + 2*DT*F(QT)                            115.   
  360 NS=NS+2                                                            116.   
         MODD5K=MOD(NSTEP+NS-NDYN+NDA5K,NDA5K)                           117.   
      MRCH=2                                                             118.   
C     CALL DYNAM (U,V,T,P,Q,UT,VT,TT,PT,QT,DTLF)                         119.   
      CALL AFLUX (UT,VT,PA)
      CALL ADVECM (PC,P,DTLF)
      CALL ADVECV (PC,U,V,P,UT,VT,PA,DTLF)
      CALL ADVECT (PC,T,P,TT,DTLF)
      CALL ADVECQ (PC,Q,P,QT,DTLF)
      CALL PGF (U,V,P,UT,VT,TT,PA,DTLF)
C**** LOAD P TO PC
      DO 371 J=1,JM
      DO 371 I=1,IM
  371 PC(I,J)=P(I,J)
         IF(MOD(NSTEP+NS-NDYN+NDAA,NDAA).LT.MRCH) THEN                   120.   
           CALL DIAGA (UT,VT,TT,PB,QT)
           CALL DIAGB (UT,VT,TT,PB,QT)
         END IF                                                          121.6  
      IF(NS.LT.NDYN) GO TO 340                                           122.   
      RETURN
      END

      SUBROUTINE AFLUX (U,V,P)                                          1001.
C****                                                                   1002.
C**** THIS SUBROUTINE CALCULATES THE HORIZONTAL AIR MASS FLUXES         1003.
C**** AND VERTICAL AIR MASS FLUXES AS DETERMINED BY U, V AND P.         1004.
C****                                                                   1005.
      INCLUDE 'BA94jalC9.COM'
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1),PU(IM,JM,LM)               1007.
     *      ,PV(IM,JM,LM)                                               1008.
      COMMON/WORK3/PHI(IM,JM,LM),SPA(IM,JM,LM)                          1009.
      COMMON/WORK4/FD(IM,0:JM+1),FLUXQ(IM),DUMMYS(IM),DUMMYN(IM)        1010.
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM)                          1011.
      DIMENSION UT(IM,JM,LM),VT(IM,JM,LM),TT(IM,JM,LM),                 1012.
     *  PA(IM,JM),PB(IM,JM),QT(IM,JM,LM),CONV(IM,JM,LM)                 1013.
      EQUIVALENCE (CONV,PIT)                                            1014.
C****                                                                   1015.
C****                                                                   1017.
C**** BEGINNING OF LAYER LOOP                                           1018.
C****                                                                   1019.
      L=LM                                                              1020.
C****                                                                   1021.
C**** COMPUTATION OF MASS FLUXES     P,T  PU     PRIMARY GRID ROW       1022.
C**** ARAKAWA'S SCHEME B             PV   U,V    SECONDARY GRID ROW     1023.
C****                                                                   1024.
C**** COMPUTE PU, THE WEST-EAST MASS FLUX, AT NON-POLAR POINTS          1025.
 2150 DO 2154 J=2,JM-1                                                  1026.
      DO 2154 I=1,IM                                                    1027.
 2154 SPA(I,J,L)=U(I,J,L)+U(I,J+1,L)                                    1028.
      CALL AVRX (SPA(1,1,L))                                            1029.
      I=IM                                                              1030.
      DO 2166 IP1=1,IM                                                  1031.
      DO 2165 J=2,JM-1                                                  1032.
 2165 PU(I,J,L)=.25*DYP(J)*SPA(I,J,L)*(P(I,J)+P(IP1,J))                 1033.
 2166 I=IP1                                                             1034.
C**** COMPUTE PV, THE SOUTH-NORTH MASS FLUX                             1035.
      IM1=IM                                                            1036.
      DO 2172 I=1,IM                                                    1037.
      DO 2170 J=2,JM                                                    1038.
 2170 PV(I,J,L)=.25*DXV(J)*(V(I,J,L)+V(IM1,J,L))*(P(I,J)+P(I,J-1))      1039.
 2172 IM1=I                                                             1040.
C**** COMPUTE PU*3 AT THE POLES                                         1041.
      PUS=0.                                                            1042.
      PUN=0.                                                            1043.
      PVS=0.                                                            1044.
      PVN=0.                                                            1045.
      DO 1110 I=1,IM                                                    1046.
      PUS=PUS+U(I,2,L)                                                  1047.
      PUN=PUN+U(I,JM,L)                                                 1048.
      PVS=PVS+PV(I,2,L)                                                 1049.
 1110 PVN=PVN+PV(I,JM,L)                                                1050.
      PUS=.25*DYP(2)*PUS*P(1,1)/FIM                                     1051.
      PUN=.25*DYP(JM-1)*PUN*P(1,JM)/FIM                                 1052.
      PVS=PVS/FIM                                                       1053.
      PVN=PVN/FIM                                                       1054.
      DUMMYS(1)=0.                                                      1055.
      DUMMYN(1)=0.                                                      1056.
      DO 1120 I=2,IM                                                    1057.
      DUMMYS(I)=DUMMYS(I-1)+(PV(I,2,L)-PVS)                             1058.
 1120 DUMMYN(I)=DUMMYN(I-1)+(PV(I,JM,L)-PVN)                            1059.
      PBS=0.                                                            1060.
      PBN=0.                                                            1061.
      DO 1130 I=1,IM                                                    1062.
      PBS=PBS+DUMMYS(I)                                                 1063.
 1130 PBN=PBN+DUMMYN(I)                                                 1064.
      PBS=PBS/FIM                                                       1065.
      PBN=PBN/FIM                                                       1066.
      DO 1140 I=1,IM                                                    1067.
      SPA(I,1,L)=4.*(PBS-DUMMYS(I)+PUS)/(DYP(2)*P(1,1))                 1068.
      SPA(I,JM,L)=4.*(DUMMYN(I)-PBN+PUN)/(DYP(JM-1)*P(1,JM))            1069.
      PU(I,1,L)=3.*(PBS-DUMMYS(I)+PUS)                                  1070.
 1140 PU(I,JM,L)=3.*(DUMMYN(I)-PBN+PUN)                                 1071.
C****                                                                   1072.
C**** CONTINUITY EQUATION                                               1073.
C****                                                                   1074.
C**** COMPUTE CONV, THE HORIZONTAL MASS CONVERGENCE                     1075.
      DO 1510 J=2,JM-1                                                  1076.
      IM1=IM                                                            1077.
      DO 1510 I=1,IM                                                    1078.
      CONV(I,J,L)=(PU(IM1,J,L)-PU(I,J,L)+PV(I,J,L)-PV(I,J+1,L))*DSIG(L) 1079.
 1510 IM1=I                                                             1080.
      CONV(1,1,L)=-PVS*DSIG(L)                                          1081.
      CONV(1,JM,L)=PVN*DSIG(L)                                          1082.
      L=L-1                                                             1083.
      IF(L.GE.1) GO TO 2150                                             1084.
C****                                                                   1085.
C**** END OF HORIZONTAL ADVECTION LAYER LOOP                            1086.
C****                                                                   1087.
C**** COMPUTE PIT, THE PRESSURE TENDENCY                                1088.
C     PIT(I,J)=CONV(I,J,1)                                              1089.
      DO 2420 LX=2,LM                                                   1090.
      L=2+LM-LX                                                         1091.
      PIT(1,1)=PIT(1,1)+CONV(1,1,L)                                     1092.
      PIT(1,JM)=PIT(1,JM)+CONV(1,JM,L)                                  1093.
      DO 2420 J=2,JM-1                                                  1094.
      DO 2420 I=1,IM                                                    1095.
 2420 PIT(I,J)=PIT(I,J)+CONV(I,J,L)                                     1096.
C**** COMPUTE SD, SIGMA DOT                                             1097.
      SD(1, 1,LM-1)=CONV(1, 1,LM)-DSIG(LM)*PIT(1, 1)                    1098.
      SD(1,JM,LM-1)=CONV(1,JM,LM)-DSIG(LM)*PIT(1,JM)                    1099.
      DO 2430 J=2,JM-1                                                  1100.
      DO 2430 I=1,IM                                                    1101.
 2430 SD(I,J,LM-1)=CONV(I,J,LM)-DSIG(LM)*PIT(I,J)                       1102.
      DO 2440 LX=2,LM-1                                                 1103.
      L=LM-LX                                                           1104.
      SD(1, 1,L)=SD(1, 1,L+1)+CONV(1, 1,L+1)-DSIG(L+1)*PIT(1, 1)        1105.
      SD(1,JM,L)=SD(1,JM,L+1)+CONV(1,JM,L+1)-DSIG(L+1)*PIT(1,JM)        1106.
      DO 2440 J=2,JM-1                                                  1107.
      DO 2440 I=1,IM                                                    1108.
 2440 SD(I,J,L)=SD(I,J,L+1)+CONV(I,J,L+1)-DSIG(L+1)*PIT(I,J)            1109.
      DO 2450 L=1,LM-1                                                  1110.
      SDSP=SD(1,1,L)                                                    1111.
      SDNP=SD(1,JM,L)                                                   1112.
      DO 2450 I=1,IM                                                    1113.
      SD(I,1,L)=SDSP                                                    1114.
 2450 SD(I,JM,L)=SDNP                                                   1115.
C****                                                                   1116.
C****                                                                   1117.
      RETURN                                                            1118.
      END                                                               1119.
      SUBROUTINE ADVECM (P,PA,DT1)                                      1501.
C****                                                                   1502.
C**** THIS SUBROUTINE CALCULATES UPDATED COLUMN PRESSURES AS            1503.
C**** DETERMINED BY DT1 AND THE CURRENT AIR MASS FLUXES.                1504.
C****                                                                   1505.
      INCLUDE 'BA94jalC9.COM'
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1),PU(IM,JM,LM)               1507.
     1     ,PV(IM,JM,LM)                                                1508.
      COMMON/WORK3/PHI(IM,JM,LM),SPA(IM,JM,LM)                          1509.
      COMMON/WORK4/FD(IM,0:JM+1),FLUXQ(IM),DUMMYS(IM),DUMMYN(IM)        1510.
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM)                          1511.
      COMMON U,V,T,XYZ,Q                                                1512.
      DIMENSION XYZ (IM,JM)                                             1513.
      DIMENSION UT(IM,JM,LM),VT(IM,JM,LM),TT(IM,JM,LM),                 1514.
     *  PA(IM,JM),PB(IM,JM),QT(IM,JM,LM),CONV(IM,JM,LM)                 1515.
C****                                                                   1516.
C****                                                                   1517.
C**** COMPUTE PA, THE NEW SURFACE PRESSURE                              1518.
      PA(1,1)=P(1,1)+(DT1*PIT(1,1)/DXYP(1)+PTRUNC)                      1519.
         IF(PA(1,1).GT.1150.) WRITE(6,991) L,L,MRCH,P(1,1),PA(1,1),     1520.
     *     (FDATA(1,1,K),K=1,22),(T(1,1,L),Q(1,1,L),L=1,LM)             1521.
      PA(1,JM)=P(1,JM)+(DT1*PIT(1,JM)/DXYP(JM)+PTRUNC)                  1522.
         IF(PA(1,JM).GT.1150.) WRITE(6,991) L,JM,MRCH,P(1,JM),PA(1,JM), 1523.
     *     (FDATA(1,JM,K),K=1,22),(T(1,JM,L),Q(1,JM,L),L=1,LM)          1524.
      DO 2424 I=2,IM                                                    1525.
      PA(I,1)=PA(1,1)                                                   1526.
 2424 PA(I,JM)=PA(1,JM)                                                 1527.
      DO 2426 J=2,JM-1                                                  1528.
      DO 2426 I=1,IM                                                    1529.
      PA(I,J)=P(I,J)+(DT1*PIT(I,J)/DXYP(J)+PTRUNC)                      1530.
         IF(PA(I,J).GT.1150.) WRITE (6,990) I,J,MRCH,P(I,J),PA(I,J),    1531.
     *     (FDATA(I,J,K),K=1,22),(U(I-1,J,L),U(I,J,L),U(I-1,J+1,L),     1532.
     *     U(I,J+1,L),V(I-1,J,L),V(I,J,L),V(I-1,J+1,L),V(I,J+1,L),      1533.
     *     T(I,J,L),Q(I,J,L),L=1,LM)                                    1534.
 2426 CONTINUE                                                          1535.
C****                                                                   1536.
      RETURN                                                            1537.
  990 FORMAT (/'0PRESSURE DIAGNOSTIC     I,J,MRCH,P,PA=',3I4,2F10.2/    1538.
     *  '     DATA=',11F10.3/10X,11F10.3/                               1539.
     *  '0    U(I-1,J)     U(I,J)   U(I-1,J+1)    U(I,J+1)    V(I-1,J)',1540.
     *   '     V(I,J)   V(I-1,J+1)    V(I,J+1)     T(I,J)     Q(I,J)'/  1541.
     *  (1X,9F12.3,F12.6))                                              1542.
  991 FORMAT (/'0PRESSURE DIAGNOSTIC     I,J,MRCH,P,PA=',3I4,2F10.2/    1543.
     *  '     DATA=',11F10.3/10X,11F10.3/                               1544.
     *  '0     T(I,J)      Q(I,J)'/(F13.3,F12.6))                       1545.
      END                                                               1546.
      SUBROUTINE ADVECV (PA,UT,VT,PB,U,V,P,DT1)                         2001.
C****                                                                   2002.
C**** THIS SUBROUTINE ADVECTS MOMENTUM (INCLUDING THE CORIOLIS FORCE)   2003.
C**** AS DETERMINED BY DT1 AND THE CURRENT AIR MASS FLUXES.             2004.
C****                                                                   2005.
      INCLUDE 'BA94jalC9.COM'
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1),PU(IM,JM,LM)               2007.
     1     ,PV(IM,JM,LM)                                                2008.
      COMMON/WORK3/PHI(IM,JM,LM),SPA(IM,JM,LM)                          2009.
      COMMON/WORK4/FD(IM,JM),FLUXQ(IM),DUMMYS(IM),DUMMYN(IM)            2010.
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM)                          2011.
      DIMENSION UT(IM,JM,LM),VT(IM,JM,LM),TT(IM,JM,LM),                 2012.
     *  PA(IM,JM),PB(IM,JM),QT(IM,JM,LM),CONV(IM,JM,LM)                 2013.
      REAL*8 KAPAP1                                                     2014.
C****                                                                   2015.
         IF(MODD5K.LT.MRCH) CALL DIAG5F (U)                             2016.
      SHA=RGAS/KAPA                                                     2017.
      KAPAP1=KAPA+1.                                                    2018.
      JMM2=JM-2                                                         2019.
      DT2=DT1/2.                                                        2021.
      DT4=DT1/4.                                                        2022.
      DT8=DT1/8.                                                        2023.
      DT12=DT1/12.                                                      2024.
      DT24=DT1/24.                                                      2025.
C****                                                                   2026.
C**** SCALE QT.  UT AND VT MAY THEN BE PHYSICALLY INTERPRETED AS        2027.
C**** MOMENTUM COMPONENTS, TT AS HEAT CONTENT, AND QT AS WATER CONTENT  2028.
C****                                                                   2029.
      DO 101 J=2,JM-1                                                   2030.
      DO 101 I=1,IM                                                     2031.
  101 FD(I,J)=PA(I,J)*DXYP(J)                                           2032.
      FDSP=PA(1,1)*DXYP(1)                                              2033.
      FDNP=PA(1,JM)*DXYP(JM)                                            2034.
      FDSP=FDSP+FDSP                                                    2035.
      FDNP=FDNP+FDNP                                                    2036.
      DO 120 I=1,IM                                                     2037.
      FD(I,1)=FDSP                                                      2038.
  120 FD(I,JM)=FDNP                                                     2039.
      I=IM                                                              2040.
      DO 140 IP1=1,IM                                                   2041.
      DO 130 J=2,JM                                                     2042.
      FDU=.25*(FD(I,J)+FD(IP1,J)+FD(I,J-1)+FD(IP1,J-1))                 2043.
      DO 130 L=1,LM+LM                                                  2044.
      DUT(I,J,L)=0.                                                     2045.
  130 UT(I,J,L)=UT(I,J,L)*FDU                                           2046.
  140 I=IP1                                                             2047.
C****                                                                   2048.
      L=LM                                                              2049.
 2150 CONTINUE                                                          2050.
C**** HORIZONTAL ADVECTION OF MOMENTUM                                  2051.
C****                                                                   2052.
 1400 I=IM                                                              2053.
      DO 2320 IP1=1,IM                                                  2054.
C**** CONTRIBUTION FROM THE WEST-EAST MASS FLUX                         2055.
      DO 2310 J=2,JM                                                    2056.
      FLUX=DT12*(PU(IP1,J,L)+PU(IP1,J-1,L)+PU(I,J,L)+PU(I,J-1,L))       2057.
      FLUXU=FLUX*(U(IP1,J,L)+U(I,J,L))                                  2058.
      DUT(IP1,J,L)=DUT(IP1,J,L)+FLUXU                                   2059.
      DUT(I,J,L)=DUT(I,J,L)-FLUXU                                       2060.
      FLUXV=FLUX*(V(IP1,J,L)+V(I,J,L))                                  2061.
      DVT(IP1,J,L)=DVT(IP1,J,L)+FLUXV                                   2062.
 2310 DVT(I,J,L)=DVT(I,J,L)-FLUXV                                       2063.
      DO 2315 J=2,JM-1                                                  2064.
C**** CONTRIBUTION FROM THE SOUTH-NORTH MASS FLUX                       2065.
      FLUX=DT12*(PV(I,J,L)+PV(IP1,J,L)+PV(I,J+1,L)+PV(IP1,J+1,L))       2066.
      FLUXU=FLUX*(U(I,J,L)+U(I,J+1,L))                                  2067.
      DUT(I,J+1,L)=DUT(I,J+1,L)+FLUXU                                   2068.
      DUT(I,J,L)=DUT(I,J,L)-FLUXU                                       2069.
      FLUXV=FLUX*(V(I,J,L)+V(I,J+1,L))                                  2070.
      DVT(I,J+1,L)=DVT(I,J+1,L)+FLUXV                                   2071.
      DVT(I,J,L)=DVT(I,J,L)-FLUXV                                       2072.
C**** CONTRIBUTION FROM THE SOUTHWEST-NORTHEAST MASS FLUX               2073.
      FLUX=DT24*(PU(IP1,J,L)+PU(I,J,L)+PV(IP1,J,L)+PV(IP1,J+1,L))       2074.
      FLUXU=FLUX*(U(IP1,J+1,L)+U(I,J,L))                                2075.
      DUT(IP1,J+1,L)=DUT(IP1,J+1,L)+FLUXU                               2076.
      DUT(I,J,L)=DUT(I,J,L)-FLUXU                                       2077.
      FLUXV=FLUX*(V(IP1,J+1,L)+V(I,J,L))                                2078.
      DVT(IP1,J+1,L)=DVT(IP1,J+1,L)+FLUXV                               2079.
      DVT(I,J,L)=DVT(I,J,L)-FLUXV                                       2080.
C**** CONTRIBUTION FROM THE SOUTHEAST-NORTHWEST MASS FLUX               2081.
      FLUX=DT24*(-PU(IP1,J,L)-PU(I,J,L)+PV(IP1,J,L)+PV(IP1,J+1,L))      2082.
      FLUXU=FLUX*(U(I,J+1,L)+U(IP1,J,L))                                2083.
      DUT(I,J+1,L)=DUT(I,J+1,L)+FLUXU                                   2084.
      DUT(IP1,J,L)=DUT(IP1,J,L)-FLUXU                                   2085.
      FLUXV=FLUX*(V(I,J+1,L)+V(IP1,J,L))                                2086.
      DVT(I,J+1,L)=DVT(I,J+1,L)+FLUXV                                   2087.
 2315 DVT(IP1,J,L)=DVT(IP1,J,L)-FLUXV                                   2088.
 2320 I=IP1                                                             2089.
C****                                                                   2090.
      L=L-1                                                             2091.
      IF(L.GE.1) GO TO 2150                                             2092.
C**** VERTICAL ADVECTION OF MOMENTUM                                    2093.
C****                                                                   2094.
      DO 2470 L=1,LM-1                                                  2095.
      LP1=L+1                                                           2096.
      DO 2470 J=2,JM                                                    2097.
      I=IM                                                              2098.
      DO 2470 IP1=1,IM                                                  2099.
      SDU=DT2*((SD(I,J-1,L)+SD(IP1,J-1,L))*RAVPN(J-1)+                  2100.
     *  (SD(I,J,L)+SD(IP1,J,L))*RAVPS(J))                               2101.
      SDUDN=SDU/DSIG(L)                                                 2102.
      SDUUP=SDU/DSIG(LP1)                                               2103.
      DUT(I,J,L)  =DUT(I,J,L)  +SDUDN*(U(I,J,L)+U(I,J,LP1))             2104.
      DUT(I,J,LP1)=DUT(I,J,LP1)-SDUUP*(U(I,J,L)+U(I,J,LP1))             2105.
      DVT(I,J,L)  =DVT(I,J,L)  +SDUDN*(V(I,J,L)+V(I,J,LP1))             2106.
      DVT(I,J,LP1)=DVT(I,J,LP1)-SDUUP*(V(I,J,L)+V(I,J,LP1))             2107.
 2470 I=IP1                                                             2108.
C**** ADD ADVECTION INCREMENTS TO UT AND VT, CALL DIAGNOSTICS           2109.
         IF(MODD5K.LT.MRCH) CALL DIAG5A (4,MRCH)                        2110.
         IF(MODD5K.LT.MRCH) CALL DIAG9D (1,DT1,U,V)                     2111.
      DO 2900 L=1,LM+LM                                                 2112.
      DO 2900 J=2,JM                                                    2113.
      DO 2900 I=1,IM                                                    2114.
      UT(I,J,L)=UT(I,J,L)+DUT(I,J,L)                                    2115.
 2900 DUT(I,J,L)=0.                                                     2116.
C****                                                                   2117.
C****                                                                   2118.
C**** CORIOLIS FORCE                                                    2119.
C****                                                                   2120.
 1800 DO 3130 L=1,LM                                                    2121.
      IM1=IM                                                            2122.
      DO 3130 I=1,IM                                                    2123.
      FD(I,1)=F(1)*2.-.5*(SPA(IM1,1,L)+SPA(I,1,L))*DXV(2)               2124.
      FD(I,JM)=F(JM)*2.+.5*(SPA(IM1,JM,L)+SPA(I,JM,L))*DXV(JM)          2125.
      DO 3110 J=2,JM-1                                                  2126.
 3110 FD(I,J)=F(J)+.25*(SPA(IM1,J,L)+SPA(I,J,L))*(DXV(J)-DXV(J+1))      2127.
      DO 3120 J=2,JM                                                    2128.
      ALPH=DT8*(P(I,J)+P(I,J-1))*(FD(I,J)+FD(I,J-1))                    2129.
      DUT(I,J,L)=DUT(I,J,L)+ALPH*V(I,J,L)                               2130.
      DUT(IM1,J,L)=DUT(IM1,J,L)+ALPH*V(IM1,J,L)                         2131.
      DVT(I,J,L)=DVT(I,J,L)-ALPH*U(I,J,L)                               2132.
 3120 DVT(IM1,J,L)=DVT(IM1,J,L)-ALPH*U(IM1,J,L)                         2133.
 3130 IM1=I                                                             2134.
C**** ADD CORIOLIS FORCE INCREMENTS TO UT AND VT, CALL DIAGNOSTICS      2135.
         IF(MODD5K.LT.MRCH) CALL DIAG5A (5,MRCH)                        2136.
         IF(MODD5K.LT.MRCH) CALL DIAG9D (2,DT1,U,V)                     2137.
      DO 3190 L=1,LM+LM                                                 2138.
      DO 3190 J=2,JM                                                    2139.
      DO 3190 I=1,IM                                                    2140.
      UT(I,J,L)=UT(I,J,L)+DUT(I,J,L)                                    2141.
 3190 DUT(I,J,L)=0.                                                     2142.
C****                                                                   2143.
C**** UNDO SCALING PERFORMED AT BEGINNING OF DYNAM                      2144.
C****                                                                   2145.
      DO 3410 J=2,JM-1                                                  2146.
      DO 3410 I=1,IM                                                    2147.
 3410 FD(I,J)=PB(I,J)*DXYP(J)                                           2148.
      FDSP=PB(1, 1)*DXYP( 1)                                            2149.
      FDNP=PB(1,JM)*DXYP(JM)                                            2150.
      RFDSP=1./FDSP                                                     2151.
      RFDNP=1./FDNP                                                     2152.
      FDSP=FDSP+FDSP                                                    2153.
      FDNP=FDNP+FDNP                                                    2154.
      DO 3520 I=1,IM                                                    2155.
      FD(I, 1)=FDSP                                                     2156.
 3520 FD(I,JM)=FDNP                                                     2157.
      I=IM                                                              2158.
      DO 3540 IP1=1,IM                                                  2159.
      DO 3530 J=2,JM                                                    2160.
      RFDU=4./(FD(I,J)+FD(IP1,J)+FD(I,J-1)+FD(IP1,J-1))                 2161.
      DO 3530 L=1,LM+LM                                                 2162.
 3530 UT(I,J,L)=(UT(I,J,L)+DUT(I,J,L))*RFDU                             2163.
 3540 I=IP1                                                             2164.
      RETURN                                                            2165.
      END                                                               2166.
      SUBROUTINE PGF (UT,VT,PB,U,V,T,P,DT1)                             2501.
C****                                                                   2502.
C**** THIS SUBROUTINE ADDS TO MOMENTUM THE TENDENCIES DETERMINED BY     2503.
C**** THE PRESSURE GRADIENT FORCE                                       2504.
C****                                                                   2505.
      INCLUDE 'BA94jalC9.COM'
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1),PU(IM,JM,LM)               2507.
     1     ,PV(IM,JM,LM)                                                2508.
      COMMON/WORK3/PHI(IM,JM,LM),SPA(IM,JM,LM)                          2509.
      COMMON/WORK4/FD(IM,0:JM+1),FLUXQ(IM),DUMMYS(IM),DUMMYN(IM)        2510.
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM)                          2511.
      DIMENSION UT(IM,JM,LM),VT(IM,JM,LM),TT(IM,JM,LM),                 2512.
     *  PA(IM,JM),PB(IM,JM),QT(IM,JM,LM),CONV(IM,JM,LM)                 2513.
      REAL*8 KAPAP1                                                     2514.
C****                                                                   2515.
      SHA=RGAS/KAPA                                                     2516.
      KAPAP1=KAPA+1.                                                    2517.
      JMM2=JM-2                                                         2518.
      DT4=DT1/4.                                                        2520.
C****                                                                   2521.
C**** VERTICAL DIFFERENCING                                             2522.
C****                                                                   2523.
      IMAX=1                                                            2524.
      DO 3071 J=1,JM                                                    2525.
      IF(J.EQ.JM) IMAX=1                                                2526.
      DO 3070 I=1,IMAX                                                  2527.
      SUM1=0.                                                           2528.
      SUM2=0.                                                           2529.
      SP=P(I,J)                                                         2530.
      PDN=SIG(1)*SP+PTOP                                                2531.
      PKDN=EXPBYK(PDN)                                                  2532.
      DO 3040 L=1,LM-1                                                  2533.
      LP1=L+1                                                           2534.
C**** CALCULATE SPA                                                     2535.
      SPA(I,J,L)=SIG(L)*SP*RGAS*T(I,J,L)*PKDN/PDN                       2536.
      SUM1=SUM1+SPA(I,J,L)*DSIG(L)                                      2537.
      PUP=SIG(LP1)*SP+PTOP                                              2538.
      PKUP=EXPBYK(PUP)                                                  2539.
      THETA=THBAR(T(I,J,LP1),T(I,J,L))                                  2540.
C**** CALCULATE THE DIFFERENCE IN PHI BETWEEN ODD LEVELS LP1 AND L      2541.
      PHI(I,J,LP1)=SHA*THETA*(PKDN-PKUP)                                2542.
      SUM2=SUM2+SIGE(LP1)*PHI(I,J,LP1)                                  2543.
      PDN=PUP                                                           2544.
 3040 PKDN=PKUP                                                         2545.
      SPA(I,J,LM)=SIG(LM)*SP*RGAS*T(I,J,LM)*PKDN/PDN                    2546.
      SUM1=SUM1+SPA(I,J,LM)*DSIG(LM)                                    2547.
 3050 PHI(I,J,1)=FDATA(I,J,1)+SUM1-SUM2                                 2548.
      DO 3070 L=2,LM                                                    2549.
 3070 PHI(I,J,L)=PHI(I,J,L)+PHI(I,J,L-1)                                2550.
 3071 IMAX=IM                                                           2551.
      DO 3080 L=1,LM                                                    2552.
      DO 3080 I=2,IM                                                    2553.
      SPA(I,1,L)=SPA(1,1,L)                                             2554.
      SPA(I,JM,L)=SPA(1,JM,L)                                           2555.
      PHI(I,1,L)=PHI(1,1,L)                                             2556.
 3080 PHI(I,JM,L)=PHI(1,JM,L)                                           2557.
C****                                                                   2558.
C**** PRESSURE GRADIENT FORCE                                           2559.
C****                                                                   2560.
      DO 3340 L=1,LM                                                    2561.
C**** NORTH-SOUTH DERIVATIVE AFFECTS THE V-COMPONENT OF MOMENTUM        2562.
      IM1=IM                                                            2563.
      DO 3236 I=1,IM                                                    2564.
      DO 3234 J=2,JM                                                    2565.
      FLUX=DT4*((P(I,J)+P(I,J-1))*(PHI(I,J,L)-PHI(I,J-1,L))+            2566.
     *  (SPA(I,J,L)+SPA(I,J-1,L))*(P(I,J)-P(I,J-1)))*DXV(J)             2567.
      DVT(I,J,L)=DVT(I,J,L)-FLUX                                        2568.
 3234 DVT(IM1,J,L)=DVT(IM1,J,L)-FLUX                                    2569.
 3236 IM1=I                                                             2570.
C**** SMOOTHED EAST-WEST DERIVATIVE AFFECTS THE U-COMPONENT             2571.
      I=IM                                                              2572.
      DO 3293 IP1=1,IM                                                  2573.
      PU(I,1,L)=0.                                                      2574.
      PU(I,JM,L)=0.                                                     2575.
      DO 3280 J=2,JM-1                                                  2576.
 3280 PU(I,J,L)=(P(IP1,J)+P(I,J))*(PHI(IP1,J,L)-PHI(I,J,L))+            2577.
     *  (SPA(IP1,J,L)+SPA(I,J,L))*(P(IP1,J)-P(I,J))                     2578.
 3293 I=IP1                                                             2579.
      CALL AVRX (PU(1,1,L))                                             2580.
      DO 3294 J=2,JM                                                    2581.
      DO 3294 I=1,IM                                                    2582.
 3294 DUT(I,J,L)=DUT(I,J,L)-DT4*DYV(J)*(PU(I,J,L)+PU(I,J-1,L))          2583.
 3340 CONTINUE                                                          2584.
C**** CALL DIAGNOSTICS                                                  2585.
      IF(MRCH.LE.0) GO TO 500                                           2586.
         IF(MODD5K.LT.MRCH) CALL DIAG5A (6,MRCH)                        2587.
         IF(MODD5K.LT.MRCH) CALL DIAG9D (3,DT1,U,V)                     2588.
C****                                                                   2589.
C****                                                                   2590.
C**** UNDO SCALING PERFORMED AT BEGINNING OF DYNAM                      2591.
C****                                                                   2592.
  500 CONTINUE                                                          2593.
      DO 3410 J=2,JM-1                                                  2594.
      DO 3410 I=1,IM                                                    2595.
 3410 FD(I,J)=PB(I,J)*DXYP(J)                                           2596.
      FDSP=PB(1, 1)*DXYP( 1)                                            2597.
      FDNP=PB(1,JM)*DXYP(JM)                                            2598.
      FDSP=FDSP+FDSP                                                    2599.
      FDNP=FDNP+FDNP                                                    2600.
      DO 3520 I=1,IM                                                    2601.
      FD(I, 1)=FDSP                                                     2602.
 3520 FD(I,JM)=FDNP                                                     2603.
      I=IM                                                              2604.
      DO 3540 IP1=1,IM                                                  2605.
      DO 3530 J=2,JM                                                    2606.
      RFDU=4./(FD(I,J)+FD(IP1,J)+FD(I,J-1)+FD(IP1,J-1))                 2607.
      DO 3530 L=1,LM+LM                                                 2608.
 3530 UT(I,J,L)=UT(I,J,L)+DUT(I,J,L)*RFDU                               2609.
 3540 I=IP1                                                             2610.
      RETURN                                                            2611.
      END                                                               2612.
      SUBROUTINE ADVECT (PA,TT,PB,T,DT1)                                3001.
C****                                                                   3002.
C**** THIS SUBROUTINE ADVECTS POTENTIAL TEMPERATURE                     3003.
C****                                                                   3004.
C****                                                                   3005.
      INCLUDE 'BA94jalC9.COM'
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1),PU(IM,JM,LM)               3007.
     1     ,PV(IM,JM,LM)                                                3008.
      COMMON/WORK3/PHI(IM,JM,LM),SPA(IM,JM,LM)                          3009.
      COMMON/WORK4/FD(IM,0:JM+1),FLUXQ(IM),DUMMYS(IM),DUMMYN(IM)        3010.
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM)                          3011.
      DIMENSION UT(IM,JM,LM),VT(IM,JM,LM),TT(IM,JM,LM),                 3012.
     *  PA(IM,JM),PB(IM,JM),QT(IM,JM,LM),CONV(IM,JM,LM)                 3013.
      REAL*8 KAPAP1                                                     3014.
C****                                                                   3015.
      SHA=RGAS/KAPA                                                     3016.
      KAPAP1=KAPA+1.                                                    3017.
      JMM2=JM-2                                                         3018.
      DT2=DT1/2.                                                        3020.
C****                                                                   3021.
C**** SCALE QT.  UT AND VT MAY THEN BE PHYSICALLY INTERPRETED AS        3022.
C**** MOMENTUM COMPONENTS, TT AS HEAT CONTENT, AND QT AS WATER CONTENT  3023.
C****                                                                   3024.
      DO 101 J=2,JM-1                                                   3025.
      DO 101 I=1,IM                                                     3026.
  101 FD(I,J)=PA(I,J)*DXYP(J)                                           3027.
      FDSP=PA(1,1)*DXYP(1)                                              3028.
      FDNP=PA(1,JM)*DXYP(JM)                                            3029.
  105 DO 110 L=1,LM                                                     3030.
      TT(1,1,L)=TT(1,1,L)*FDSP                                          3031.
      TT(1,JM,L)=TT(1,JM,L)*FDNP                                        3032.
      DO 110 J=2,JM-1                                                   3033.
      DO 110 I=1,IM                                                     3034.
  110 TT(I,J,L)=TT(I,J,L)*FD(I,J)                                       3035.
C****                                                                   3036.
      L=LM                                                              3037.
 2150 CONTINUE                                                          3038.
C****                                                                   3039.
C**** HORIZONTAL ADVECTION OF HEAT                                      3040.
C****                                                                   3041.
C**** WEST-EAST ADVECTION OF HEAT                                       3042.
      DO 2220 J=2,JM-1                                                  3043.
      I=IM                                                              3044.
      DO 2210 IP1=1,IM                                                  3045.
      FLUXQ(I)=DT2*PU(I,J,L)*(T(I,J,L)+T(IP1,J,L))                      3046.
 2210 I=IP1                                                             3047.
      IM1=IM                                                            3048.
      DO 2220 I=1,IM                                                    3049.
      TT(I,J,L)=TT(I,J,L)+(FLUXQ(IM1)-FLUXQ(I))                         3050.
 2220 IM1=I                                                             3051.
C**** SOUTH-NORTH ACVECTION OF HEAT                                     3052.
      SUMF=0.                                                           3053.
      DO 2230 I=1,IM                                                    3054.
      FLUXQ(I)=DT2*PV(I,2,L)*(T(I,2,L)+T(1,1,L))                        3055.
 2230 SUMF=SUMF-FLUXQ(I)                                                3056.
      TT(1,1,L)=TT(1,1,L)+SUMF/FIM                                      3057.
      DO 2240 J=2,JMM2                                                  3058.
      DO 2240 I=1,IM                                                    3059.
      FLUX=DT2*PV(I,J+1,L)*(T(I,J+1,L)+T(I,J,L))                        3060.
      TT(I,J,L)=TT(I,J,L)+(FLUXQ(I)-FLUX)                               3061.
 2240 FLUXQ(I)=FLUX                                                     3062.
      SUMF=0.                                                           3063.
      DO 2250 I=1,IM                                                    3064.
      FLUX=DT2*PV(I,JM,L)*(T(1,JM,L)+T(I,JM-1,L))                       3065.
      TT(I,JM-1,L)=TT(I,JM-1,L)+(FLUXQ(I)-FLUX)                         3066.
 2250 SUMF=SUMF+FLUX                                                    3067.
      TT(1,JM,L)=TT(1,JM,L)+SUMF/FIM                                    3068.
      L=L-1                                                             3069.
      IF(L.GE.1) GO TO 2150                                             3070.
C****                                                                   3071.
C**** VERTICAL ADVECTION OF POTENTIAL TEMPERATURE                       3072.
C****                                                                   3073.
      IMAX=1                                                            3074.
      DO 3071 J=1,JM                                                    3075.
      IF(J.EQ.JM) IMAX=1                                                3076.
      DO 3070 I=1,IMAX                                                  3077.
      FLUXDN=0.                                                         3078.
      DO 3040 L=1,LM-1                                                  3079.
      LP1=L+1                                                           3080.
      THETA=THBAR(T(I,J,LP1),T(I,J,L))                                  3081.
C**** VERTICAL ADVECTION OF POTENTIAL TEMPERATURE                       3082.
      FLUX=DT1*SD(I,J,L)*THETA                                          3083.
      TT(I,J,L)=TT(I,J,L)+(FLUX-FLUXDN)/DSIG(L)                         3084.
      FLUXDN=FLUX                                                       3085.
 3040 CONTINUE                                                          3086.
      TT(I,J,LM)=TT(I,J,LM)-FLUXDN/DSIG(LM)                             3087.
 3070 CONTINUE                                                          3088.
 3071 IMAX=IM                                                           3089.
C**** UNDO SCALING PERFORMED AT BEGINNING OF DYNAM                      3090.
C****                                                                   3091.
      DO 3410 J=2,JM-1                                                  3092.
      DO 3410 I=1,IM                                                    3093.
 3410 FD(I,J)=PB(I,J)*DXYP(J)                                           3094.
      FDSP=PB(1, 1)*DXYP( 1)                                            3095.
      FDNP=PB(1,JM)*DXYP(JM)                                            3096.
      RFDSP=1./FDSP                                                     3097.
      RFDNP=1./FDNP                                                     3098.
      DO 3505 L=1,LM                                                    3099.
      TT(1,1,L)=TT(1,1,L)*RFDSP                                         3100.
 3505 TT(1,JM,L)=TT(1,JM,L)*RFDNP                                       3101.
      DO 3510 J=2,JM-1                                                  3102.
      DO 3510 I=1,IM                                                    3103.
      RFDP=1./FD(I,J)                                                   3104.
      DO 3510 L=1,LM                                                    3105.
 3510 TT(I,J,L)=TT(I,J,L)*RFDP                                          3106.
      RETURN                                                            3107.
      END                                                               3108.
      SUBROUTINE ADVECQ (PA,QT,PB,Q,DT1)                                3501.
C****                                                                   3502.
C**** THIS SUBROUTINE ADVECTS HUMIDITY AS DETERMINED BY DT1 AND THE     3503.
C**** CURRENT AIR MASS FLUXES                                           3504.
C****                                                                   3505.
      INCLUDE 'BA94jalC9.COM'
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1),PU(IM,JM,LM)               3507.
     1     ,PV(IM,JM,LM)                                                3508.
      COMMON/WORK3/PHI(IM,JM,LM),SPA(IM,JM,LM)                          3509.
      COMMON/WORK4/FD(IM,0:JM+1),FLUXQ(IM),DUMMYS(IM),DUMMYN(IM)        3510.
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM)                          3511.
      DIMENSION UT(IM,JM,LM),VT(IM,JM,LM),TT(IM,JM,LM),                 3512.
     *  PA(IM,JM),PB(IM,JM),QT(IM,JM,LM),CONV(IM,JM,LM)                 3513.
      REAL*8 KAPAP1                                                     3514.
C****                                                                   3515.
      SHA=RGAS/KAPA                                                     3516.
      KAPAP1=KAPA+1.                                                    3517.
      JMM2=JM-2                                                         3518.
      DT2=DT1/2.                                                        3520.
C****                                                                   3521.
C**** SCALE QT.  UT AND VT MAY THEN BE PHYSICALLY INTERPRETED AS        3522.
C**** MOMENTUM COMPONENTS, TT AS HEAT CONTENT, AND QT AS WATER CONTENT  3523.
C****                                                                   3524.
      DO 101 J=2,JM-1                                                   3525.
      DO 101 I=1,IM                                                     3526.
  101 FD(I,J)=PA(I,J)*DXYP(J)                                           3527.
      FDSP=PA(1,1)*DXYP(1)                                              3528.
      FDNP=PA(1,JM)*DXYP(JM)                                            3529.
      DO 102 L=1,LM                                                     3530.
      QT(1,1,L)=QT(1,1,L)*FDSP                                          3531.
      QT(1,JM,L)=QT(1,JM,L)*FDNP                                        3532.
      DO 102 J=2,JM-1                                                   3533.
      DO 102 I=1,IM                                                     3534.
  102 QT(I,J,L)=QT(I,J,L)*FD(I,J)                                       3535.
C****                                                                   3536.
C**** BEGINNING OF LAYER LOOP                                           3537.
C****                                                                   3538.
      L=LM                                                              3539.
C****                                                                   3540.
 2150 CONTINUE                                                          3541.
C**** WEST-EAST ADVECTION OF MOISTURE                                   3542.
      DO 1320 J=2,JM-1                                                  3543.
      I=IM                                                              3544.
      DO 1310 IP1=1,IM                                                  3545.
      FLUXQ(I)=DT2*PU(I,J,L)*(Q(I,J,L)+Q(IP1,J,L))                      3546.
      IF(FLUXQ(I).GT..5*QT(I,J,L)) FLUXQ(I)=.5*QT(I,J,L)                3547.
      IF(FLUXQ(I).LT.-.5*QT(IP1,J,L)) FLUXQ(I)=-.5*QT(IP1,J,L)          3548.
 1310 I=IP1                                                             3549.
      IM1=IM                                                            3550.
      DO 1320 I=1,IM                                                    3551.
      QT(I,J,L)=QT(I,J,L)+(FLUXQ(IM1)-FLUXQ(I))                         3552.
 1320 IM1=I                                                             3553.
C**** SOUTH-NORTH ADVECTION OF MOISTURE                                 3554.
      SUMF=0.                                                           3555.
      DO 1330 I=1,IM                                                    3556.
      FLUXQ(I)=DT2*PV(I,2,L)*(Q(I,2,L)+Q(1,1,L))                        3557.
      IF(FLUXQ(I).GT.QT(1,1,L)) FLUXQ(I)=QT(1,1,L)                      3558.
      IF(FLUXQ(I).LT.-.5*QT(I,2,L)) FLUXQ(I)=-.5*QT(I,2,L)              3559.
 1330 SUMF=SUMF+FLUXQ(I)                                                3560.
      QT(1,1,L)=QT(1,1,L)-SUMF/FIM                                      3561.
      DO 1340 J=2,JMM2                                                  3562.
      DO 1340 I=1,IM                                                    3563.
      FLUX=DT2*PV(I,J+1,L)*(Q(I,J+1,L)+Q(I,J,L))                        3564.
      IF(FLUX.GT..5*QT(I,J,L)) FLUX=.5*QT(I,J,L)                        3565.
      IF(FLUX.LT.-.5*QT(I,J+1,L)) FLUX=-.5*QT(I,J+1,L)                  3566.
      QT(I,J,L)=QT(I,J,L)+(FLUXQ(I)-FLUX)                               3567.
 1340 FLUXQ(I)=FLUX                                                     3568.
      SUMF=0.                                                           3569.
      DO 1350 I=1,IM                                                    3570.
      FLUX=DT2*PV(I,JM,L)*(Q(1,JM,L)+Q(I,JM-1,L))                       3571.
      IF(FLUX.GT..5*QT(I,JM-1,L)) FLUX=.5*QT(I,JM-1,L)                  3572.
      IF(FLUX.LT.-QT(1,JM,L)) FLUX=-QT(1,JM,L)                          3573.
      QT(I,JM-1,L)=QT(I,JM-1,L)+(FLUXQ(I)-FLUX)                         3574.
 1350 SUMF=SUMF+FLUX                                                    3575.
      QT(1,JM,L)=QT(1,JM,L)+SUMF/FIM                                    3576.
C****                                                                   3577.
      L=L-1                                                             3578.
      IF(L.GE.1) GO TO 2150                                             3579.
C****                                                                   3580.
C**** END OF HORIZONTAL ADVECTION LAYER LOOP                            3581.
C****                                                                   3582.
C**** VERTICAL ADVECTION OF MOISTURE                                    3583.
C****                                                                   3584.
      DO 1715 L=1,LM-1                                                  3585.
      IMAX=1                                                            3586.
      LP1=L+1                                                           3587.
      DO 1715 J=1,JM                                                    3588.
      IF(J.EQ.JM) IMAX=1                                                3589.
      DO 1710 I=1,IMAX                                                  3590.
      FLUX=DT1*SD(I,J,L)*2.*Q(I,J,L)*Q(I,J,LP1)/(Q(I,J,L)+              3591.
     *  Q(I,J,LP1)+1.E-20)                                              3592.
      IF(FLUX.GT..5*QT(I,J,LP1)*DSIG(LP1)) FLUX=.5*QT(I,J,LP1)*         3593.
     *  DSIG(LP1)                                                       3594.
      IF(FLUX.LT.-.5*QT(I,J,L)*DSIG(L)) FLUX=-.5*QT(I,J,L)*DSIG(L)      3595.
      QT(I,J,L)=QT(I,J,L)+FLUX/DSIG(L)                                  3596.
 1710 QT(I,J,LP1)=QT(I,J,LP1)-FLUX/DSIG(LP1)                            3597.
 1715 IMAX=IM                                                           3598.
C****                                                                   3599.
C****                                                                   3600.
C**** UNDO SCALING PERFORMED AT BEGINNING OF DYNAM                      3601.
C****                                                                   3602.
      DO 3410 J=2,JM-1                                                  3603.
      DO 3410 I=1,IM                                                    3604.
 3410 FD(I,J)=PB(I,J)*DXYP(J)                                           3605.
      FDSP=PB(1, 1)*DXYP( 1)                                            3606.
      FDNP=PB(1,JM)*DXYP(JM)                                            3607.
      RFDSP=1./FDSP                                                     3608.
      RFDNP=1./FDNP                                                     3609.
      DO 3420 L=1,LM                                                    3610.
      QT(1,1,L)=QT(1,1,L)*RFDSP                                         3611.
      QT(1,JM,L)=QT(1,JM,L)*RFDNP                                       3612.
      DO 3420 J=2,JM-1                                                  3613.
      DO 3420 I=1,IM                                                    3614.
 3420 QT(I,J,L)=QT(I,J,L)/FD(I,J)                                       3615.
      RETURN                                                            3616.
      END                                                               3617.
      SUBROUTINE AVRX (PU)                                              1801.   
C****                                                                   1802.   
C**** THIS SUBROUTINE SMOOTHES THE ZONAL MASS FLUX AND GEOPOTENTIAL     1803.   
C**** GRADIENTS NEAR THE POLES TO HELP AVOID COMPUTATIONAL INSTABILITY. 1804.   
C**** THIS VERSION OF AVRX DOES SO BY TRUNCATING THE FOURIER SERIES.    1805.   
C****                                                                   1806.   
      INCLUDE 'BA94jalC9.COM'                                           
      COMMON U,V,T,P,Q                                                  1808.   
      DIMENSION PU(IM,JM)                                               1809.   
      COMMON/AVRXX/AVCOS(IM,JM),AVSIN(IM,JM),BYSN(IMH),CN(2,IMH+1)      1810.   
      DATA IFIRST/1/                                                    1811.   
      IF(IFIRST.NE.1) GO TO 100                                         1812.   
      IFIRST=0                                                          1813.   
      IMBY2=IM/2                                                        1814.   
      NMAX=IMBY2                                                        1815.   
      DO 10 I=1,IMBY2                                                   1816.   
      AVCOS(I,1)=COS((TWOPI/FIM)*I)                                     1817.   
      AVCOS(I+IMBY2,1)=-AVCOS(I,1)                                      1818.   
      AVSIN(I,1)=SIN((TWOPI/FIM)*I)                                     1819.   
   10 AVSIN(I+IMBY2,1)=-AVSIN(I,1)                                      1820.   
      DO 20 N=2,NMAX                                                    1821.   
      DO 20 I=1,IM                                                      1822.   
      IN=I*N-((I*N-1)/IM)*IM                                            1823.   
      AVCOS(I,N)=AVCOS(IN,1)                                            1824.   
   20 AVSIN(I,N)=AVSIN(IN,1)                                            1825.   
      DO 30 N=1,NMAX                                                    1826.   
   30 BYSN(N)=1./SIN(.5*(TWOPI/FIM)*N)                                  1827.   
C****                                                                   1828.   
  100 DO 140 J=2,JM-1                                                   1829.   
      DRAT=DYP(3)/DXP(J)                                                1830.   
      IF(DRAT.LT.1) GO TO 140                                           1831.   
      CALL GETAN (PU(1,J),CN)                                           1832.   
      DO 130 NX=1,NMAX                                                  1833.   
      N=1+NMAX-NX                                                       1834.   
      SM=1.-BYSN(N)/DRAT                                                1835.   
      IF(SM.LE.0.) GO TO 130                                            1836.   
      AN=SM*CN(1,N+1)                                                   1837.   
      BN=SM*CN(2,N+1)                                                   1838.   
      DO 120 I=1,IM                                                     1839.   
  120 PU(I,J)=PU(I,J)-AN*AVCOS(I,N)-BN*AVSIN(I,N)                       1840.   
  130 CONTINUE                                                          1841.   
  140 CONTINUE                                                          1841.5  
      RETURN                                                            1842.   
      END                                                               1843.   
      SUBROUTINE SDRAG                                                  7001.   
C****                                                                   7002.   
C**** THIS SUBROUTINE PUTS A DRAG ON THE WINDS ON THE TOP LAYER OF      7003.   
C**** THE ATMOSPHERE                                                    7004.   
C****                                                                   7005.   
      INCLUDE 'BA94jalC9.COM'     
      COMMON U,V,T,P,Q                                                  7007.   
      COMMON/WORK1/CONV(IM,JM,LM),PK(IM,JM,LM)                          7008.   
      DO 100 J=2,JM                                                     7009.   
      I=IM                                                              7010.   
      DO 100 IP1=1,IM                                                   7011.   
      PIJU=.25*(P(I,J-1)+P(IP1,J-1)+P(I,J)+P(IP1,J))                    7012.   
      WLM=SQRT(U(I,J,LM)*U(I,J,LM)+V(I,J,LM)*V(I,J,LM))                 7013.   
      RHO=PTOP/(RGAS*T(I,J,LM)*PK(I,J,LM))                              7014.   
      CDN=XCDNST(1)+XCDNST(2)*WLM                                       7015.   
      X=NDYN*DT*RHO*CDN*WLM*GRAV/(PIJU*DSIG(LM))                        7016.   
      if (x.gt.1.) x = 1.   ! Preserve wind direction
      U(I,J,LM)=U(I,J,LM)*(1.-X)                                        7017.   
      V(I,J,LM)=V(I,J,LM)*(1.-X)                                        7018.   
  100 I=IP1                                                             7019.   
      RETURN                                                            7020.   
      END                                                               7021.   
      SUBROUTINE FILTER                                                 7501.   
C****                                                                   7502.   
C**** THIS SUBROUTINE PERFORMS AN 8-TH ORDER SHAPIRO FILTER ON          7503.   
C**** SELECTED PROGNOSTIC QUANTITIES IN THE ZONAL DIRECTION             7504.   
C****                                                                   7505.   
C**** MFILTR=1  SMOOTH P USING SEA LEVEL PRESSURE FILTER                7506.   
C****        2  SMOOTH T USING TROPOSPHERIC STRATIFICATION OF TEMPER    7507.   
C****        3  SMOOTH P AND T                                          7508.   
C****                                                                   7509.   
      INCLUDE 'BA94jalC9.COM'                                           7510.   
      COMMON U,V,T,P,Q                                                  7511.   
      REAL*8 X,XS,Y                                                     7512.   
      COMMON/WORK2/X(IM,JM),XS(IM),Y(IM,JM)                             7513.   
      IF(MOD(MFILTR,2).NE.1) GO TO 200                                  7514.   
C****                                                                   7515.   
C**** SEA LEVEL PRESSURE FILTER ON P                                    7516.   
C****                                                                   7517.   
      BBYG=.0065/GRAV                                                   7518.   
      GBYRB=GRAV/(RGAS*.0065)                                           7519.   
      DO 120 J=2,JM-1                                                   7520.   
      DO 120 I=1,IM                                                     7521.   
      Y(I,J)=(1.+BBYG*FDATA(I,J,1)/BLDATA(I,J,2))**GBYRB                7522.   
  120 X(I,J)=(P(I,J)+PTOP)*Y(I,J)                                       7523.   
      CALL SHAP1D (8)                                                   7524.   
      DO 140 J=2,JM-1                                                   7525.   
      DO 140 I=1,IM                                                     7526.   
  140 P(I,J)=X(I,J)/Y(I,J)-PTOP+PTRUNC                                  7527.   
      DOPK=1.                                                           7528.   
  200 IF(MFILTR.LT.2) RETURN                                            7529.   
C****                                                                   7530.   
C**** TEMPERATURE STRATIFICATION FILTER ON T                            7531.   
C****                                                                   7532.   
      AKAP=KAPA-.205                                                    7533.   
      DO 260 L=1,LM                                                     7534.   
      DO 220 J=2,JM-1                                                   7535.   
      DO 220 I=1,IM                                                     7536.   
      Y(I,J)=(SIG(L)*P(I,J)+PTOP)**AKAP                                 7537.   
  220 X(I,J)=T(I,J,L)*Y(I,J)                                            7538.   
      CALL SHAP1D (8)                                                   7539.   
      DO 240 J=2,JM-1                                                   7540.   
      DO 240 I=1,IM                                                     7541.   
  240 T(I,J,L)=X(I,J)/Y(I,J)                                            7542.   
  260 CONTINUE                                                          7543.   
      RETURN                                                            7544.   
      END                                                               7545.   
      SUBROUTINE SHAP1D (NORDER)                                        7801.   
C****                                                                   7802.   
C**** THIS SUBROUTINE SMOOTHES THE ARRAY X IN THE ZONAL DIRECTION       7803.   
C**** USING AN N-TH ORDER SHAPIRO FILTER.  N MUST BE EVEN.              7804.   
C**** (USES ONLY IM,JM,JM-1,IM, AND JM FROM COMMON BLOCK)               7804.1  
C****                                                                   7805.   
      INCLUDE 'BA94jalC9.COM'                                           7805.5  
      REAL*8 X,XS                                                       7806.   
      COMMON/WORK2/X(IM,JM),XS(IM)                                      7807.   
      RE4TON=1./4.**NORDER                                              7810.   
      DO 180 J=2,JM-1                                                   7811.   
      DO 120 I=1,IM                                                     7812.   
  120 XS(I)=X(I,J)                                                      7813.   
      DO 160 N=1,NORDER                                                 7814.   
      XS1=XS(1)                                                         7815.   
      XSIM1=XS(IM)                                                      7816.   
      DO 140 I=1,IM-1                                                   7817.   
      XSI=XS(I)                                                         7818.   
      XS(I)=XSIM1-XSI-XSI+XS(I+1)                                       7819.   
  140 XSIM1=XSI                                                         7820.   
  160 XS(IM)=XSIM1-XS(IM)-XS(IM)+XS1                                    7821.   
      DO 180 I=1,IM                                                     7822.   
  180 X(I,J)=X(I,J)-XS(I)*RE4TON                                        7823.   
      RETURN                                                            7824.   
      END                                                               7825.   
      SUBROUTINE DAILY                                                  8001.   
C****                                                                   8002.   
C**** THIS SUBROUTINE PERFORMS THOSE FUNCTIONS OF THE PROGRAM WHICH     8003.   
C**** TAKE PLACE AT THE BEGINNING OF A NEW DAY.                         8004.   
C****                                                                   8005.   
      INCLUDE 'BA94jalC9.COM'                                           8006.   
      COMMON U,V,T,P,Q                                                  8007.   
      COMMON/WORK2/Z1OOLD(IM,JM),XO(IM,JM,3),XZO(IM,JM)                 8008.   
      DIMENSION AMONTH(12),JDOFM(13)                                    8009.   
      CHARACTER*4 AMONTH                                                8009.1  
      character*16 ocean_input
      common/orbit_parms_com/OMEGT,OBLIQ,ECCN
      common/ocean_input_com/ocean_input
      DATA AMONTH/'JAN','FEB','MAR','APR','MAY','JUNE','JULY','AUG',    8010.   
     *  'SEP','OCT','NOV','DEC'/                                        8011.   
      DATA JDOFM/0,31,59,90,120,151,181,212,243,273,304,334,365/        8012.   
      DATA JDPERY/365/,JMPERY/12/,EDPERY/365./,Z1I/.1/,RHOI/916.6/      8013.   
C**** added SHW and SHI from Previp
      DATA SHW/4185./,SHI/2060./                            
C**** ORBITAL PARAMETERS FOR EARTH FOR YEAR 2000 A.D.                   8014.   
Cold  DATA SOLS/173./,APHEL/186./,OBLIQ/23.44/,ECCN/.0167/              8015.   
C****                                                                   8016.   
C**** THE GLOBAL MEAN PRESSURE IS KEPT CONSTANT AT PSF MILLIBARS        8017.   
C****                                                                   8018.   
C**** CALCULATE THE CURRENT GLOBAL MEAN PRESSURE                        8019.   
  100 SMASS=0.                                                          8020.   
      DO 120 J=1,JM                                                     8021.   
      SPRESS=0.                                                         8022.   
      DO 110 I=1,IM                                                     8023.   
  110 SPRESS=SPRESS+P(I,J)                                              8024.   
  120 SMASS=SMASS+SPRESS*DXYP(J)                                        8025.   
      PBAR=SMASS/AREAG+PTOP                                             8026.   
C**** CORRECT PRESSURE FIELD FOR ANY LOSS OF MASS BY TRUNCATION ERROR   8027.   
      DELTAP=PSF-PBAR                                                   8028.   
      DO 140 J=1,JM                                                     8029.   
      DO 140 I=1,IM                                                     8030.   
  140 P(I,J)=P(I,J)+DELTAP                                              8031.   
      DOPK=1.                                                           8032.   
      WRITE (6,901) DELTAP                                              8033.   
      DOZ1O=1.                                                          8033.5  
C****                                                                   8034.   
C**** CALCULATE THE DAILY CALENDAR                                      8035.   
C****                                                                   8036.   
  200 JYEAR=IYEAR+(IDAY-1)/JDPERY                                       8037.   
      JDAY=IDAY-(JYEAR-IYEAR)*JDPERY                                    8038.   
      DO 210 MONTH=1,JMPERY                                             8039.   
      IF(JDAY.LE.JDOFM(MONTH+1)) GO TO 220                              8040.   
  210 CONTINUE                                                          8041.   
  220 JDATE=JDAY-JDOFM(MONTH)                                           8042.   
      JMONTH=AMONTH(MONTH)                                              8043.   

c  ** MFS (ADDED)
c  ** print out the daily tau for all interested parties.
c  ** this is the new format that Mark wants to replace my old
c  ** debug output.
C  Start date    Current date    End date      Zero hour     Hour 
C  ----------    ------------    ----------    ----------    --------
C  12/01/1900    12/01/1900      12/31/1910    01/01/1900    8016

C      PRINT 902, 'TAU, ',INT(TAU),'  Date=, ',IINT(MONTH),'/',
C     *  IINT(JDATE),'/',JYEAR

c  ** print the headers at the start of every month and the start
c  ** of the run.
      IF((JDATE.EQ.1).OR.(TAU.EQ.TAUI)) THEN
        PRINT *,'                                                     '
        PRINT *,'Start date    Current date    End date      Zero hour',
     *    '     Hour'
        PRINT *,'----------    ------------    ----------    ---------',
     *    '-    --------'
      END IF
c  ** more date calc and printout
      ISTAUI=TAUI/24+1
      ISYEAR=IYEAR+ISTAUI/JDPERY
      ISDAY=MOD(ISTAUI,JDPERY)
      DO 240 ISMONTH=1,JMPERY                                             
      IF(ISDAY.LE.JDOFM(ISMONTH+1)) GO TO 250                            
  240 CONTINUE                                                      
  250 ISDATE=ISDAY-JDOFM(ISMONTH)    
      IETAUI=TAUE/24+1
      IEYEAR=IYEAR+IETAUI/JDPERY
      IEDAY=MOD(IETAUI,JDPERY)
      DO 260 IEMONTH=1,JMPERY                                             
      IF(IEDAY.LE.JDOFM(IEMONTH+1)) GO TO 270                            
  260 CONTINUE                                                      
  270 IEDATE=IEDAY-JDOFM(IEMONTH)                                                                                 
      PRINT 904, INT(ISMONTH),INT(ISDATE),ISYEAR, INT(MONTH),
     *  INT(JDATE),JYEAR, INT(IEMONTH),INT(IEDATE),IEYEAR,
     *  1,1,IYEAR,  INT(TAU)

c  ** write out current date file to tell 4D what day it is
      OPEN(88,FILE='CURRENT.DATE',FORM='formatted',IOSTAT=IOErr)
      IF(IOErr.EQ.0) THEN
        WRITE(88,903) INT(MONTH),'/',INT(JDATE),'/',JYEAR
        CLOSE(88,IOSTAT=IOErr)
      END IF
c  ** END (ADDED)

C**** CALCULATE SOLAR ANGLES AND ORBIT POSITION                         8044.   
      CALL ORBIT (OBLIQ,ECCN,OMEGT,DFLOAT(JDAY)-.5,RSDIST,SIND,COSD,LAM)8048.5  
C****                                                                   8049.   
      IF(KOCEAN.EQ.1 .or. KOCEAN.EQ.2) GO TO 500                        8050.   
C****                                                                   8051.   
C**** CALCULATE DAILY OCEAN DATA FROM CLIMATOLOGY                       8052.   
C****                                                                   8053.   
C**** ODATA  1  OCEAN TEMPERATURE (C)                                   8054.   
C****        2  RATIO OF OCEAN ICE COVERAGE TO WATER COVERAGE (1)       8055.   
C****        3  OCEAN ICE AMOUNT OF SECOND LAYER (KG/M**2)              8056.   
C****                                                                   8057.   

      if (ocean_input.eq.'OBSERVED')then
C**** READ IN OBSERVED OCEAN DATA                                 
      MONTH1=MONTH-1+(JYEAR-IYEAR-NINT(TAUI)/8760)*JMPERY        
      MONTHO=MONTH1+1                                           
C****                                                          
C**** BEGINNING OF MONTH (1 < JDATE < 16)                     
C****                                                        
      IF(JDATE.GE.16) GO TO 1350                            
C**** READ IN LAST MONTH's MONTHLY DATA                    
 1300 READ(15) M                                          
      IF(M.LT.MONTH1) GO TO 1300                         
      BACKSPACE 15                                      
      CALL MREAD(15,M,0,ODATA,IM*JM*3,ODATA)           
      WRITE(6,'(a,i4,a,i4,a,i4,a,i4)')
     *  ' JDAY',JDAY,' JDATE',JDATE,' M',M,' MONTH1',MONTH1 
C**** READ IN THIS MONTH'S MONTHLY DATA                    
      CALL MREAD(15,M,0,XO,IM*JM*3,XO)                    
      WRITE(6,'(a,i4,a,i4,a,i4,a,i4)')
     *  ' JDAY',JDAY,' JDATE',JDATE,' M',M,' MONTHO',MONTHO             
      IF(JDAY.LT.16) THEN                                               
        MDMAX=31                                                        
      ELSE                                                              
        MDMAX=JDOFM(MONTH)-JDOFM(MONTH-1)                               
      END IF                                                            
      MD=MDMAX+JDATE-16                                                 
      WRITE(6,'(a,2i4,a,i4,a,i4)')
     *  ' MONTH,MONTH-1,',MONTH,MONTH-1,' MDMAX',MDMAX,' MD',MD                  
      GO TO 400                                                         
C****                                                                   
C**** MIDDLE OF MONTH ( JDATE = 16 )                                    
C****                                                                   
C**** READ IN THIS MONTH'S MONTHLY DATA                                 
 1350 READ(15) M                                                        
      IF(M.LT.MONTHO) GO TO 1350                                        
      BACKSPACE 15                                                      
      CALL MREAD(15,M,0,ODATA,IM*JM*3,ODATA)                            
      WRITE(6,'(a,i4,a,i4,a,i4,a,i4)')
     *  ' JDAY',JDAY,' JDATE',JDATE,' M',M,' MONTHO',MONTHO             
      IF(JDATE.EQ.16) GO TO 420                                         
C****                                                                   
C**** END OF MONTH ( 16 < JDATE < MDMAX ==> EITHER 31,30, OR 28 )       
C****                                                                   
C**** READ IN NEXT MONTH'S MONTHLY DATA (THIS MONTH's ALREADY READ)     
      CALL MREAD(15,M,0,XO,IM*JM*3,XO)                                  
      WRITE(6,'(a,i4,a,i4,a,i4,a,i4)')
     *  ' JDAY',JDAY,' JDATE',JDATE,' M',M
      MDMAX=JDOFM(MONTH+1)-JDOFM(MONTH)                                 
      MD=JDATE-16                                                       
      WRITE(6,'(a,2i4,a,i4,a,i4)')
     *  ' MONTH+1,MONTH',MONTH+1,MONTH,' MDMAX',MDMAX,' MD',MD                  
  
      else  ! ocean_input = 'CLIMATOLOGICAL'
C****
C**** READ IN TWO MONTHS OF OCEAN DATA                                  8058.   
C****
      IF(JDAY.GE.16) GO TO 320                                          8059.   
      MD=JDATE+15                                                       8060.   
      GO TO 330                                                         8061.   
  320 IF(JDAY.LE.350) GO TO 350                                         8062.   
      MD=JDATE-16                                                       8063.   
  330 CALL MREAD (15,M,0,XO,IM*JM*3,XO)                                 8064.   
      MDMAX=31                                                          8065.   
      DO 340 MX=1,10                                                    8066.   
  340 READ (15) M                                                       8067.   
      CALL MREAD (15,M,0,ODATA,IM*JM*3,ODATA)                           8068.   
      GO TO 400                                                         8069.   
  350 DO 360 MX=1,12                                                    8070.   
      CALL MREAD (15,M,0,ODATA,IM*JM*3,ODATA)                           8071.   
      IF(M.EQ.MONTH) GO TO 370                                          8072.   
      IF(M+1.EQ.MONTH.AND.JDATE.LT.16) GO TO 380                        8073.   
  360 CONTINUE                                                          8074.   
      STOP 'OCEAN FILE ERROR: ODATA NOT FOUND FOR CURRENT MONTH'        8075.   
  370 IF(JDATE.EQ.16) GO TO 420                                         8076.   
      MDMAX=JDOFM(MONTH+1)-JDOFM(MONTH)                                 8077.   
      MD=JDATE-16                                                       8078.   
      GO TO 390                                                         8079.   
  380 MDMAX=JDOFM(MONTH)-JDOFM(MONTH-1)                                 8080.   
      MD=MDMAX+JDATE-16                                                 8081.   
  390 CALL MREAD (15,M,0,XO,IM*JM*3,XO)                                 8082.   
      end if

C**** INTERPOLATE OCEAN DATA TO CURRENT DAY                             8083.   
  400 X1=DFLOAT(MDMAX-MD)/MDMAX                                         8084.   
      X2=1.-X1                                                          8085.   
      DO 410 K=1,3                                                      8086.   
      DO 410 J=1,JM                                                     8087.   
      DO 410 I=1,IM                                                     8088.   
  410 ODATA(I,J,K)=X1*ODATA(I,J,K)+X2*XO(I,J,K)                         8089.   
  420 REWIND 15                                                         8090.   
C**** WHEN TGO IS NOT DEFINED, MAKE IT A REASONABLE VALUE               8091.   
      DO 430 J=1,JM                                                     8092.   
      DO 430 I=1,IM                                                     8093.   
      IF(ODATA(I,J,1).LT.-10.) ODATA(I,J,1)=-10.                        8094.   
  430 CONTINUE                                                          8095.   
C**** REDUCE THE RATIO OF OCEAN ICE TO WATER BY .1*RHOI/ACEOI           8096.   
      DO 440 J=1,JM                                                     8097.   
      DO 440 I=1,IM                                                     8098.   
      IF(ODATA(I,J,2).LE.0.) GO TO 440                                  8099.   
      BYZICE=RHOI/(Z1I*RHOI+ODATA(I,J,3))                               8099.5  
      ODATA(I,J,2)=ODATA(I,J,2)*(1.-.05*(BYZICE-1./5.))                 8100.   
  440 CONTINUE                                                          8101.   
C**** ZERO OUT SNOWOI, TG1OI, TG2OI AND ACE2OI IF THERE IS NO OCEAN ICE 8102.   
      DO 450 J=1,JM                                                     8103.   
      DO 450 I=1,IM                                                     8104.   
      IF(ODATA(I,J,2).GT.0.) GO TO 450                                  8105.   
      GDATA(I,J,1)=0.                                                   8106.   
      GDATA(I,J,3)=0.                                                   8107.   
      GDATA(I,J,7)=0.                                                   8108.   
  450 CONTINUE                                                          8109.   
      RETURN                                                            8110.   
C****                                                                   8111.   
C**** CALCULATE DAILY OCEAN MIXED LAYER DEPTHS FROM CLIMATOLOGY         8112.   
C****                                                                   8113.   
C**** SAVE PREVIOUS DAY'S MIXED LAYER DEPTH IN WORK2                    8114.   
  500 DO 510 J=1,JM                                                     8115.   
      DO 510 I=1,IM                                                     8116.   
CORR  NEXT LINE NOT NEEDED IF Z1O WERE PART OF THE RESTART FILE         8116.4  
      Z1O(I,J)=BLDATA(I,J,5)                                            8116.5  
  510 Z1OOLD(I,J)=Z1O(I,J)                                              8117.   
C*** COMPUTE Z1O ONLY AT THE BEGINNING OF A DAY (OR AT TAUI)            8117.4  
      IF(DOZ1O.EQ.0.) RETURN                                            8117.5  
C**** READ IN TWO MONTHS OF OCEAN DATA                                  8118.   
      X1=1.                                                             8118.1  
      DO 515 J=1,JM                                                     8118.2  
      DO 515 I=1,IM                                                     8118.3  
  515 XZO(I,J)=0.                                                       8118.4  
      IF(JDAY.GE.16) GO TO 520                                          8119.   
      MD=JDATE+15                                                       8120.   
      GO TO 530                                                         8121.   
  520 IF(JDAY.LE.350) GO TO 550                                         8122.   
      MD=JDATE-16                                                       8123.   
  530 CALL MREAD (15,M,IM*JM*3,XZO,IM*JM,XZO)                           8124.   
      MDMAX=31                                                          8125.   
      DO 540 MX=1,10                                                    8126.   
  540 READ (15) M                                                       8127.   
      CALL MREAD (15,M,IM*JM*3,Z1O,IM*JM,Z1O)                           8128.   
      GO TO 600                                                         8129.   
  550 DO 560 MX=1,12                                                    8130.   
      CALL MREAD (15,M,IM*JM*3,Z1O,IM*JM,Z1O)                           8131.   
      IF(M.EQ.MONTH) GO TO 570                                          8132.   
      IF(M+1.EQ.MONTH.AND.JDATE.LT.16) GO TO 580                        8133.   
  560 CONTINUE                                                          8134.   
      STOP 'OCEAN FILE ERROR: MLD NOT FOUND FOR CURRENT MONTH'          8135.   
  570 IF(JDATE.EQ.16) GO TO 601                                         8136.   
      MDMAX=JDOFM(MONTH+1)-JDOFM(MONTH)                                 8137.   
      MD=JDATE-16                                                       8138.   
      GO TO 590                                                         8139.   
  580 MDMAX=JDOFM(MONTH)-JDOFM(MONTH-1)                                 8140.   
      MD=MDMAX+JDATE-16                                                 8141.   
  590 CALL MREAD (15,M,IM*JM*3,XZO,IM*JM,XZO)                           8142.   
C**** INTERPOLATE OCEAN DATA TO CURRENT DAY                             8143.   
  600 X1=DFLOAT(MDMAX-MD)/MDMAX                                         8144.   
  601 X2=1.-X1                                                          8145.   
      DO 610 J=1,JM                                                     8146.   
      DO 610 I=1,IM                                                     8147.   
      Z1O(I,J)=X1*Z1O(I,J)+X2*XZO(I,J)                                  8148.   
      IF(ODATA(I,J,2)*(1.-FDATA(I,J,2)).LE.0.) GO TO 610                8148.05 
C**** MIXED LAYER DEPTH IS INCREASED TO OCEAN ICE DEPTH + 1 METER       8148.06 
      Z1OMIN=1. +  .09166+.001*(GDATA(I,J,1)+ODATA(I,J,3))              8148.1  
      IF(Z1O(I,J).GE.Z1OMIN) GO TO 605                                  8148.2  
      WRITE(6,602) TAU,I,J,MONTH,Z1O(I,J),Z1OMIN                        8148.3  
  602 FORMAT (' INCREASE OF MIXED LAYER DEPTH ',F9.0,3I4,2F10.3)        8148.35 
      Z1O(I,J)=Z1OMIN                                                   8148.4  
  605 IF(Z1OMIN.LE.Z12O(I,J)) GO TO 610                                 8148.41 
C**** ICE DEPTH+1>MAX MIXED LAYER DEPTH : CHANGE OCEAN TO LAND ICE      8148.42 
      PLICE=FDATA(I,J,2)*FDATA(I,J,3)                                   8148.43 
      PLICEN=1.-FDATA(I,J,2)*(1.-FDATA(I,J,3))                          8148.44 
      POICE=(1.-FDATA(I,J,2))*ODATA(I,J,2)                              8148.45 
      POCEAN=(1.-FDATA(I,J,2))*(1.-ODATA(I,J,2))                        8148.46 
      GDATA(I,J,12)=(GDATA(I,J,12)*PLICE+GDATA(I,J,1)*POICE)/PLICEN     8148.47 
      GDATA(I,J,13)=(GDATA(I,J,13)*PLICE+GDATA(I,J,3)*POICE+            8148.5  
     *  (LHM+SHW*ODATA(I,J,1))*POCEAN/SHI)/PLICEN                       8148.51 
      GDATA(I,J,14)=(GDATA(I,J,14)*PLICE+GDATA(I,J,7)*POICE+            8148.6  
     *  (LHM+SHW*ODATA(I,J,1))*POCEAN/SHI)/PLICEN                       8148.61 
      FDATA(I,J,2)=1.                                                   8148.62 
      FDATA(I,J,3)=PLICEN                                               8148.63 
C**** MARK THE POINT FOR RESTART PURPOSES                               8148.635
      GDATA(I,J,1)=-10000.-GDATA(I,J,1)                                 8148.64 
      WRITE(6,606) 100.*POICE,100.*POCEAN,TAU,I,J                       8148.7  
  606 FORMAT(F6.1,'% OCEAN ICE AND',F6.1,'% OPEN OCEAN WERE',           8148.8  
     *  ' CHANGED TO LAND ICE AT TAU,I,J',F10.1,2I4)                    8148.9  
  610 CONTINUE                                                          8149.   
  620 REWIND 15                                                         8150.   
C**** PREVENT Z1O, THE MIXED LAYER DEPTH, FROM EXCEEDING Z12O           8151.   
      DO 630 J=1,JM                                                     8152.   
      DO 630 I=1,IM                                                     8153.   
      IF(Z1O(I,J).GT.Z12O(I,J)-.01) Z1O(I,J)=Z12O(I,J)                  8154.   
CORR  NEXT LINE NOT NEEDED IF Z1O WERE PART OF THE RESTART FILE         8154.4  
      BLDATA(I,J,5)=Z1O(I,J)                                            8154.5  
  630 CONTINUE                                                          8155.   
C**** SET MARKER INDICATING BLDATA(.,.,5)=Z1O                           8155.4  
      BLDATA(IM,1,5)=-9999.                                             8155.5  
      RETURN                                                            8156.   
C****                                                                   8157.   
      ENTRY DAILY0                                                      8158.   
C**** DO Z1O COMPUTATION ONLY IF BLDATA(I,J,5) DOES NOT CONTAIN Z1O     8158.4  
      DOZ1O=0.                                                          8158.5  
      IF(BLDATA(IM,1,5).NE.-9999.) DOZ1O=1.                             8158.6  
      IF(TAU.GT.TAUI+DT/7200.) GO TO 200                                8159.   
      GO TO 100                                                         8160.   
C*****                                                                  8161.   
  901 FORMAT ('0PRESSURE ADDED IN GMP IS',F10.6/)                       8162.   
c  ** MFS (ADDED)
  902 FORMAT (A5,I8,A9,I2,A1,I2,A1,I4)
  903 FORMAT (I2,A1,I2,A1,I4)
  904 FORMAT (T2,I2,'/',I2,'/',I4, T16,I2,'/',I2,'/',I4, T32,I2,'/',I2,
     *  '/',I4, T46,I2,'/',I2,'/',I4, T60,I8)
c  ** END (ADDED)
      END                                                               8167.   

      SUBROUTINE CHECKT (N)                                             9001.   
C****                                                                   9002.   
C**** THIS SUBROUTINE CHECKS WHETHER THE TEMPERATURES ARE REASONABLE    9003.   
C**** FOR DEBUGGING PURPOSES. IT IS TURNED ON BY SETTING IDACC(11)      9004.   
C**** TO BE POSITIVE.  REMEMBER TO SET IDACC(11) BACK TO ZERO AFTER     9005.   
C**** THE ERRORS ARE CORRECTED.                                         9006.   
C****                                                                   9007.   
      INCLUDE 'BA94jalC9.COM'                                     
      COMMON U,V,T,P,Q                                                  9008.2  
      IF(IDACC(11).LE.0) RETURN                                         9009.   
C****                                                                   9010.   
C**** CHECK WHETHER GDATA ARE REASONABLE AND CONSISTENT OVER EARTH      9011.   
C****                                                                   9012.   
      X=1.001                                                           9012.9  
      DO 110 J=1,JM                                                     9013.   
      IMAX=IM                                                           9014.   
      IF((J.EQ.1).OR.(J.EQ.JM)) IMAX=1                                  9015.   
      DO 110 I=1,IMAX                                                   9016.   
      PEARTH=FDATA(I,J,2)*(1.-FDATA(I,J,3))                             9017.   
      IF(PEARTH.LE.0.) GO TO 110                                        9018.   
      IF(GDATA(I,J,2).GE.0..AND.GDATA(I,J,2)*GDATA(I,J,4).LE.0.)GO TO 509018.1  
      WRITE (6,901) N,I,J,L,TAU,(GDATA(I,J,K),K=2,10)                   9018.2  
      STOP 'CHECKT ERROR: BOTH TG AND SNOW ARE POSITIVE'                9018.3  
   50 DO  70 L=1,2                                                      9018.4  
      TGL=GDATA(I,J,4*L)                                                9019.   
      WTRL=GDATA(I,J,4*L+1)                                             9019.1  
      ACEL=GDATA(I,J,4*L+2)                                             9019.2  
      IF((TGL+60.)*(60.-TGL).GT.0.) GO TO 60                            9020.   
      WRITE (6,901) N,I,J,L,TAU,(GDATA(I,J,K),K=2,10)                   9021.   
   60 IF(WTRL.GE.0..AND.ACEL.GE.0..AND.TGL*WTRL.GE.0..AND.              9021.1  
     *  TGL*ACEL.LE.0..AND.(WTRL+ACEL).LE.X*VDATA(I,J,8+L)) GO TO 70    9021.2  
      WRITE (6,901) N,I,J,L,TAU,(GDATA(I,J,K),K=2,10)                   9021.3  
      STOP 'CHECKT ERROR: TG/WATER/ICE ARE INCONSISTENT'                9021.4  
   70 CONTINUE                                                          9021.5  
  110 CONTINUE                                                          9022.   
      RETURN                                                            9023.   
C****                                                                   9024.   
  901 FORMAT ('0GDATA UNREASONABLE, N,I,J,L,TAU=',4I4,E14.5/            9025.   
     *  ' GDATA(2-10)=',9F12.4)                                         9025.1  
      END                                                               9026.   
