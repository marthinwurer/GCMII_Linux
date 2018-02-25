c  *********************************************************************
c  *********************************************************************
c  **
c  ** Model IImac
c  ** Based on GCMII code for IBM RS/6000 computers created at GISS
c  ** Modified to compile under Absoft Pro Fortran 6.2 for MacOS.
c  ** Based on MP008macC9, BA94C9 and MA94DC9
c  **
c  ** CHANGE HISTORY:
c  **
c  ** 07/27/99 First Successful Compile! (DCH)
c  ** 08/06/99 Changed to single precision (MFS)
c  ** 12/21/00 converted for new model (MFS)
c  ** 10/01/02 added 9/X switch for sea level pressure collection (MFS)
c  ** 12/02 Correct calculation of latitudes for this resolution
c  ** 04/12/04 Added code for writing post-processing files. (JAL)
c  **    Removed dummy DIAG8; minor corrections; geometry flexibility
c  ** 04/19/04 fix to allow F90 compiling, same as PD code (JAL)
c  ** 06/22/04 fix overflow lines (MFS)
c  ** 01/27/05 changed CLOCKS to MCLOCK, a to a16 (GLR)
c  ** 01/17/06 reverted to CLOCKS to fix Windows version (MFS)
c  **
c  ** NOTES:
c  **
c  *********************************************************************
c  *********************************************************************


c  ** DB11C9  BA94C9 DA94C9                  12/20/89
c  ** OPT(3)
c  **
c  ** MODEL II FINE GRID DIAGNOSTICS WITH HALF-BOX SHIFT IN LONGITUDE
c  ** subroutines in DB11F9: IJMAP
c  ** DA94C9  BA94C9 DA94M9                  12/20/89
c  ** OPT(3)
c  **
c  ** MODEL II FINE GRID DIAGNOSTICS
c  ** subroutines in DA94F9: all diagnostics routines
c  ** Model II diagnostics in double precision for work station  7/19/91
      SUBROUTINE DIAGA (U,V,T,P,Q)
c  **                                                             IDACC
c  ** CONTENTS OF AJ(J,N)  (SUM OVER LONGITUDE AND TIME OF)
c  **   1  SRINCP0 (W/M**2)                                        2 RD
c  **   2  SRNFP0 (W/M**2)                                         2 RD
c  **   3  SRNFP1 (W/M**2)                                         2 RD
c  **   4  SRABSATM=AJ(2)-AJ(6) (W/M**2)                           2 D1
c  **   5  SRINCG (W/M**2)                                         2 RD
c  **   6  SRNFG (W/M**2)                                          2 RD
c  **   7  TRNFP0=AJ(74)+A2BYA1*AJ(9)/DTSRCE (W/M**2)              2 D1
c  **   8  TRNFP1=AJ(75)+A2BYA1*AJ(9)/DTSRCE (W/M**2)              2 D1
c  **   9  TRHDT (J/M**2)                                          1 SF
c  **  10  RNFP0=AJ(2)+AJ(7) (W/M**2)                              2 D1
c  **  11  RNFP1=AJ(3)+AJ(8) (W/M**2)                              2 D1
c  **  12  RHDT=A1BYA2*AJ(6)*DTSRCE+AJ(9) (J/M**2)                 1 D1
c  **  13  SHEATDT (J/M**2)                                        1 SF
c  **  14  EVHDT (J/M**2)                                          1 SF
c  **  15  F2DT (J/M**2)                                           1 GD
c  **  16  HEATZ1=AJ(41)+AJ(42)                                    1 D1
c  **  17  TG2 (K-273.16)                                          1 GD
c  **  18  TG1 (K-273.16)                                          1 GD
c  **  19  EVAP (KG/M**2)                                          1 GD
c  **  20  PRCP=AJ(61)+AJ(62) (100 PA)                             1 D1
c  **  21  TX (K-273.16)  (INTEGRAL OVER ATMOSPHERE OF)            4 DA
c  **  22  TX1 (K-273.16)                                          4 DA
c  **  23  TS (K-273.16)                                           3 SF
c  **  24  DTH/DPHI  (STRATOSPHERE)                                4 DA
c  **  25  DTH/DPHI  (TROPOSPHERE)                                 4 DA
c  **  26  .0625*DTH*DLNP/(DU*DU+DV*DV)  (STRATOSPHERE)            4 DA
c  **  27  .0625*DTH*DLNP/(DU*DU+DV*DV)  (TROPOSPHERE)             4 DA
c  **  28  4*UMAX/(DX*SINJ)  (STRATOSPHERE)                        4 DA
c  **  29  4*UMAX/(DX*SINJ)  (TROPOSPHERE)                         4 DA
c  **  30  POICE (1)                                               1 GD
c  **  31  PSNOW (1)                                               4 DA
c  **  32  SW CORRECTION                                           2 RD
c  **  33  OCEAN TRANSPORT                                         1 GD
c  **  34  OCEAN TEMPERATURE AT MAX. MIXED LAYER DEPTH             1 GD
c  **  35  T(J+1)-T(J-1)  (SUM OVER STRATOSPHERE OF)               4 DA
c  **  36  T(J+1)-T(J-1)  (SUM OVER TROPOSPHERE OF)                4 DA
c  **  37  SQRT(DTH/DLNP)/SINJ  (STRATOSPHERE)                     4 DA
c  **  38  SQRT(DTH/DLNP)/SINJ  (TROPOSPHERE)                      4 DA
c  **  39  ENERGP (J/M**2)                                         1 PR
c  **  40  ERUN1 (J/M**2)                                          1 GP
c  **  41  EDIFS (J/M**2)                                          1 GP
c  **  42  F1DT (J/M**2)                                           1 GD
c  **  43  ERUN2 (J/M**2)                                          1 GP
c  **  44  HEATZ0=AJ(12)+AJ(13)+AJ(14)+AJ(39)-AJ(40) (J/M**2)      1 D1
c  **  45  DIFS (KG/M**2)                                          1 GP
c  **  46  AIFO ; BRUN2 ; CRUN2+CIFI                               1 GP
c  **  47  RUN2 (KG/M**2)                                          1 GP
c  **  48  DWTR2=AJ(45)-AJ(47) (KG/M**2)                           1 D1
c  **  49  WTR1 (KG/M**2)                                          1 GD
c  **  50  ACE1 (KG/M**2)                                          1 GD
c  **  51  WTR2 (KG/M**2)                                          1 GD
c  **  52  ACE2 (KG/M**2)                                          1 GD
c  **  53  SNOW (KG/M**2)                                          1 GD
c  **  54  RUN1 (KG/M**2)                                          1 GP
c  **  55  BTEMPW-TF                                               2 RD
c  **  56  HEATZ2=AJ(15)+AJ(43) (J/M**2)                           1 D1
c  **  57  PCLDSS (1)  (COMPOSITE OVER ATMOSPHERE)                 2 RD
c  **  58  PCLDMC (1)  (COMPOSITE OVER ATMOSPHERE)                 2 RD
c  **  59  PCLD (1)  (COMPOSITE OVER ATMOSPHERE)                   2 RD
c  **  60  CLDTOPMC=AJ(80)/AJ(58) (100 PA)                         0 D1
c  **  61  PRCPSS (100 PA)                                         1 CN
c  **  62  PRCPMC (100 PA)                                         1 CN
c  **  63  Q*P (100 PA)  (INTEGRAL OVER ATMOSPHERE OF)             4 DA
c  **  64  GAM  (K/M)  (*SIG(TROPOSPHERE)/GRAV)                    4 DA
c  **  65  GAMM  (K-S**2/M**2)  (SIG(TROPOSPHERE)/GAMD)            4 DA
c  **  66  GAMC  (K/M)                                             4 DA
c  **  67  TRINCG (W/M**2)                                         2 RD
c  **  68  ENERGY DIFFUSION INTO THERMOCLINE (W/M**2)           .5*9 MN
c  **  69  FREE
c  **  70  TRNFP0-TRNFG (W/M**2)                                   2 RD
c  **  71  TRNFP1-TRNFG (W/M**2)                                   2 RD
c  **  72  PLAVIS*S0*COSZ (W/M**2)                                 2 RD
c  **  73  PLANIR*S0*COSZ (W/M**2)                                 2 RD
c  **  74  ALBVIS*S0*COSZ (W/M**2)                                 2 RD
c  **  75  ALBNIR*S0*COSZ (W/M**2)                                 2 RD
c  **  76  SRRVIS*S0*COSZ (W/M**2)                                 2 RD
c  **  77  SRRNIR*S0*COSZ (W/M**2)                                 2 RD
c  **  78  SRAVIS*S0*COSZ (W/M**2)                                 2 RD
c  **  79  SRANIR*S0*COSZ (W/M**2)                                 2 RD
c  **  80  PBOTMC-PTOPMC (100 PA)                                  2 RD
c  **
c  ** CONTENTS OF APJ(J,N)  (SUM OVER LONGITUDE AND TIME OF)
c  **   1  P (100 PA)                                              4 DA
c  **   2  4*P4I (100 PA)  (UV GRID)                               4 DA
c  **
c  ** CONTENTS OF AJL(J,L,N)  (SUM OVER LONGITUDE AND TIME OF)
c  **   1  FREE                                                    4 DA
c  **   2  FREE                                                    4 DA
c  **   3  FREE                                                    4 DA
c  **   4  FREE                                                    4 DA
c  **   5  FREE                                                    4 DA
c  **   6  FREE                                                    4 DA
c  **   7  FREE                                                    4 DA
c  **   8  FMX(MC)*P (100 PA)                                      1 CN
c  **   9  SRHR (W/M**2)                                           2 RD
c  **  10  TRHR (W/M**2)                                           2 RD
c  **  11  DTX(SS)*P (100 K*PA)                                    1 CN
c  **  12  DT(DC)*P                                                1 CN
c  **  13  DT(MC)*P (100 PA*K)  DRY HEATING                        1 CN
c  **  14  FREE                                                    4 DA
c  **  15  FREE                                                    4 DA
c  **  16  (TH*SQRT(P)-THGM)**2/GMEAN(PR**(1-KAPA)*DTH/DPR)        4 DA
c  **  17  FREE                                                    4 DA
c  **  18  FREE                                                    4 DA
c  **  19  PCLD*P (TOTAL)                                          1 CN
c  **  20  FREE                                                    4 DA
c  **  21  FREE                                                    4 DA
c  **  22  FREE                                                    4 DA
c  **  23  FREE                                                    4 DA
c  **  24  FREE                                                    4 DA
c  **  25  FREE                                                    4 DA
c  **  26  FREE                                                    4 DA
c  **  27  FREE                                                    4 DA
c  **  28  PCLD*P (SS)                                             1 CN
c  **  29  PCLD*P (MC)                                             1 CN
c  **  30  FREE                                                    4 DA
c  **  31  FREE                                                    4 DA
c  **  32  FREE                                                    4 DA
c  **  33  FREE                                                    4 DA
c  **  34  FREE                                                    4 DA
c  **  35  FREE                                                    4 DA
c  **  36  FREE                                                    4 DA
c  **  37  FREE                                                    4 DA
c  **  38  DU(DC)*P  (UV GRID)                                       GD
c  **  39  DU(MC)*P (100 N/M/S)  (UV GRID)                         1 CN
c  **  40  DU(ED)*P*(DTSURF*DSIG*ED/DZ**2)  (UV GRID)                SF
c  **  41  U  (SUM OVER I FROM 5 TO 9)  (PV GRID) (COMMENTED OUT)  4 DA
c  **  41  P*V*((TH-THMEAN) * (DU/DP) / (DTH/DP) - U+UMEAN )       4 DA
c  **  42  V  (SUM OVER I FROM 5 TO 9)  (PV GRID)                  4 DA
c  **  43  SD  (SUM OVER I FROM 5 TO 9)                            4 DA
c  **  44  U  (SUM OVER I FROM 35 TO 3)  (PV GRID) (COMMENTED OUT) 4 DA
c  **  44  (2F-2D(UDX))*16PV(TH-THMEAN)/(DTH/DSIG)+(SD-SDMEAN)*8U  4 DA
c  **  45  V  (SUM OVER I FROM 35 TO 3)  (PV GRID)                 4 DA
c  **  46  SD  (SUM OVER I FROM 35 TO 3)                           4 DA
c  **  47  V-V*  =D((V-VI)*(T-TI)/DTHDP)/DP                        4 DA
c  **  48  4*PU4I*PV4I/P4I (100 N/S**2)  (UV GRID)                 4 DA
c  **  49  4*PUV4I (100 N/S**2)  (UV GRID)                         4 DA
c  **  50  DT(MC)*P (100 PA*K)  CHANGE OF PHASE                    1 CN
c  **  51  CLHE*DQ(MC BEFORE COND)*P (100 PA*K)                    1 CN
c  **  52  FREE                                                    4 DA
c  **  53  FREE                                                    4 DA
c  **  54  SIGMA  (VARIANCE FOR MOIST CONVECTION)                  1 CN
c  **
c  ** CONTENTS OF ASJL(J,L,N)  (SUM OVER LONGITUDE AND TIME OF)
c  **   1  TX (C)                                                  4 DA
c  **   2  PHI (M**2/S**2)                                         4 DA
c  **   3  SRHR (W/M**2)                                           2 RD
c  **   4  TRHR (W/M**2)                                           2 RD
c  **
c  ** CONTENTS OF AIJ(I,J,N)  (SUM OVER TIME OF)
c  **   1  POICE (1)                                               1 GD
c  **   2  PSNOW (1)                                               4 DA
c  **   3  SNOW (KG/M**2)                                          4 DA
c  **   4  SHDT (J/M**2)                                           1 SF
c  **   5  PREC (KG/M**2)                                          1 PR
c  **   6  EVAP (KG/M**2)                                          1 SF
c  **   7  BETA (1)                                                1 GD
c  **   8  SLP (100 PA-1000) (USING T1) (COMMENTED OUT)            4 DA
c  **   8  4*P4 (100 PA)  (UV GRID)                 (NO PRINTOUT)  4 DA
c  **   9  PHI1000 (M**2/S**2)                                     4 DA
c  **  10  PHI850 (M**2/S**2-1500*GRAV)                            4 DA
c  **  11  PHI700-3000*GRAV                                        4 DA
c  **  12  PHI500-5600*GRAV                                        4 DA
c  **  13  PHI300-9500*GRAV                                        4 DA
c  **  14  PHI100-16400*GRAV                                       4 DA
c  **  15  PHI30-24000*GRAV                                        4 DA
c  **  16  T850-273.16 (K-273.16)*GRAV)             (NO PRINTOUT)  4 DA
c  **  17  PCLDMC (1)  (COMPOSITE OVER ATMOSPHERE)                 2 RD
c  **  18  PBOTMC-PTOPMC (100 PA)                                  2 RD
c  **  19  PCLD (1)  (COMPOSITE OVER ATMOSPHERE)                   2 RD
c  **  20  16*P4*(SHA*T4+Z4)*V1*DSIG*DXV (100 W*M/S**2)  (UV GRID) 4 DA
c  **  21  TRNFP0 (W/M**2)                                         2 RS
c  **  22  SRHDT+TRHDT (J/M**2)                                    1 SF
c  **  23  SRHDT+TRHDT+SHDT+EVHDT+ENRGP (J/M**2)                   1 SP
c  **  24  SRNFP0 (W/M**2)                                         2 RD
c  **  25  SRINCP0 (W/M**2)                                        2 RD
c  **  26  SRNFG (W/M**2)                                          2 RD
c  **  27  SRINCG (W/M**2)                                         2 RD
c  **  28  TG1 (K-273.16)                                          1 GD
c  **  29  POICE+PLICE+(IF SNOW)PEARTH                             4 DA
c  **  30  DIURNAL DELTA TS (K)                 (NO PRINTOUT)   .5*9 MN
c  **  31  DTH/DPHI  (TROPOSPHERE)                                 4 DA
c  **  32  RUN1 OVER EARTH  (KG/M**2)                              1 PG
c  **  33  TS (K-273.16)  (USING LAPSE RATE FROM TX1)  (COMM'D OUT)4 DA
c  **  33  RUN1 OVER LAND ICE  (KG/M**2)            (NO PRINTOUT)  1 PG
c  **  34  ALPHA PRIME (1)                                         3 SF
c  **  35  TS (K-273.16)                                           3 SF
c  **  36  US (M/S)                                                3 SF
c  **  37  VS (M/S)                                                3 SF
c  **  38  PSL (100 PA-1000)  (USING TS)                           4 DA
c  **  39  UJET (M/S)                                              4 DA
c  **  40  VJET (M/S)                                              4 DA
c  **  41  PCLD(LOW) (1)                                           2 RD
c  **  42  PCLD(MID) (1)                                           2 RD
c  **  43  PCLD(HIGH) (1)                                          2 RD
c  **  44  BTEMPW-TF (K-273.16)                                    2 RD
c  **  45  PLAVIS*S0*COSZ (W/M**2)                                 2 RD
c  **  46  ALPHA0 (1)                                              3 SF
c  **  47  TAUS                                     (NO PRINTOUT)  3 SF
c  **  48  TAUUS                                    (NO PRINTOUT)  3 SF
c  **  49  TAUVS                                    (NO PRINTOUT)  3 SF
c  **  50  WATER1+WATER2+ICE1+ICE2  (FOR EARTH POINTS ONLY)        1 GD
c  **  51  QS                                       (NO PRINTOUT)  3 SF
c  **  52  MAX(0,33-1.8*DAILY MEAN ON TS IN C)                  .5*9 MN
c  **  53  40.6+.72*(2TS(C)-(QSATS-QS)*LHA/SHA)                    3 SF
c  **  54  18*(DEL(TG)/DEL(TS)-1), DEL=DIURNAL MAX-MIN          .5*9 MN
c  **
c  **     THE REMAINING ARRAYS ARE NOT USED IN THE STANDARD PRINTOUT
c  **
c  **  55  8*P*U*Q (VERTICALLY INTEGRATED)  (12.5 PA*M/S)          4 DA
c  **  56  8*P*V*Q (VERTICALLY INTEGRATED)  (12.5 PA*M/S)          4 DA
c  **  57  TGO=ODATA(1)  (C)                                       1 GD
c  **  58  ACE2OI=ODATA(3)*POICE  (KG/M**2)                        1 GD
c  **  59  TGO2=ODATA(4)  (C)                                   .5*9 MN
c  **  60  TGO12=ODATA(5)  (C)                                  .5*9 MN
c  **  61  EVAP*POCEAN  (KG/M**2)                                  1 GD
c  **  62  EVAP*POICE  (KG/M**2)                                   1 GD
c  **  63  EVAP OVER LAND ICE  (KG/M**2)                           1 GD
c  **  64  EVAP OVER EARTH  (KG/M**2)                              1 GD
c  **  65  F0DT*POCEAN, NET HEAT AT Z0  (J/M**2)                   1 GD
c  **  66  F0DT*POICE, NET HEAT AT Z0  (J/M**2)                    1 GD
c  **  67  F0DT, NET HEAT AT Z0 OVER LAND ICE  (J/M**2)            1 GD
c  **  68  F0DT, NET HEAT AT Z0 OVER EARTH  (J/M**2)               1 GD
c  **  69  F1DT OVER LAND ICE  (J/M**2)                            1 PG
c  **  70  SNOW FALL  (KG/M**2)                                    1 PR
c  **  71  SURF AIR TEMP OVER LAND ICE  (C)                  NSURF*1 SF
c  **  72  F2DT OVER LAND ICE  (J/M**2)                            1 PG
c  **  73  SHDT OVER LAND ICE  (J/M**2)                            3 SF
c  **  74  EVHDT OVER LAND ICE  (J/M**2)                           3 SF
c  **  75  TRHDT OVER LAND ICE  (J/M**2)                           3 SF
c  **  76  MAX(COMPOSITE TS)                                      12  SF
c  **  77  MIN(COMPOSITE TS)                                      12  SF
c  **  78  MIN(DIURNAL MAX OF COMPOSITE TS)                       12  MN
c  **  79  PEVAPS                                                  1  SF
c  **  80  FREE
c  **
c  ** CONTENTS OF AIL(I,L,N)  (SUM OVER TIME OF)
c  ** WE ARE NOT TAKING INTO ACCOUNT THE VARIATION OF MASS
c  **   1  U (M/S) (SUM FOR J=JEQ+1,JEQ,JEQ-1,JEQ-2)  (PU GRID)    4 DA
c  **   2  V (M/S) (SUM FOR J=JEQ+1,JEQ,JEQ-1,JEQ-2)  (PU GRID)    4 DA
c  **   3  SD (100 N/S) (SUM FOR J=JEQ,JEQ-1,JEQ-2)                4 DA
c  **   4  TX (K-273.16) (SUM FOR J=JEQ,JEQ-1,JEQ-2)               4 DA
c  **   5  RH (1) (SUM FOR J=JEQ,JEQ-1,JEQ-2)                      4 DA
c  **   6  DTX(MC)*P*DA (100 K*N) (SUM FOR J=JEQ,JEQ-1,JEQ-2)      1 CN
c  **   7  (SRHR+TRHR)*DA (W) (SUM FOR J=JEQ,JEQ-1,JEQ-2)          2 RD
c  **   9  SD (100 N/S) (AT LAT 50 N) (COMMENTED OUT)              4 DA
c  **  10  TX-273.16  (AT LAT 50 N)                                4 DA
c  **  11  SR+TR  (AT LAT 50 N)                                    2 RD
c  **  12  2*U  (AT LAT 50 N)                                      4 DA
c  **  13  SD  (AT LAT 70 N)         (COMMENTED OUT)               4 DA
c  **  14  TX-273.16  (AT LAT 70 N)  (COMMENTED OUT)               4 DA
c  **  15  SR+TR  (AT LAT 70 N)                                    2 RD
c  **  16  2*U  (AT LAT 70 N)        (COMMENTED OUT)               4 DA
c  **
c  ** CONTENTS OF AIJL(I,J,L,N)  (SUM OVER TIME OF)
c  **   1  4*P4*U1 (100 PA*M/S)  (UV GRID)  (COMMENTED OUT)        4 DA
c  **   1  FREQUENCY OF BEING TOP CLOUD LEVEL                      2 RD
c  **   2  4*P4*V1 (100 PA*M/S)  (UV GRID)  (COMMENTED OUT)        4 DA
c  **   2  TOP CLOUD TEMPERATURE (OR 0 IF NOT TOP CLOUD LEVEL)     2 RD
c  **   3  16*P4*(SHA*T4+Z4) (100 N/S**2)  (UV GRID) (COMM'D OUT)  4 DA
c  **   3  SWINC ON TOP CLOUD (OR 0 IF NOT TOP CLOUD LEVEL)        2 RD
c  **
c  ** CONTENTS OF IDACC(N), NUMBER OF ACCUMULATION TIMES OF
c  **   1  SOURCE TERMS  (DETERMINED BY NDYN)
c  **   2  RADIATION SOURCE TERMS  (DETERMINED BY NRAD)
c  **   3  SURFACE INTERACTION SOURCE TERMS  (DETERMINED BY NDASF)
c  **   4  QUANTITIES IN DIAGA  (DETERMINED BY NDAA)
c  **   5  ENERGY NUMBERS IN DIAG4  (DEYERMINED BY NDA4)
c  **   6  KINETIC ENERGY IN DIAG5 FROM DYNAMICS  (DETERMINED BY NDA5K)
c  **   7  ENERGY IN DIAG5 FROM DYNAMICS  (DETERMINED BY NDA5D)
c  **   8  ENERGY IN DIAG5 FROM SOURCES  (DETERMINED BY NDA5S)
c  **   9  WAVE ENERGY IN DIAG7  (EVERY 12 HOURS)
c  **  10  ENERGY IN DIAG5 FROM FILTER  (DETERMINED BY NFILTR)
c  **  11  USED FOR T-DIAGNOSTICS CHECKT:  0=OFF,1=ON
c  **  12  ALWAYS =1 (UNLESS SEVERAL RESTART FILES WERE ACCUMULATED)
c  **
c  ** CONTENTS OF AUXILIARY ARRAYS (TSFREZ(I,J,1-2),TDIURN(I,J,N))
c  **   1  FIRST DAY OF GROWING SEASON (JULIAN DAY)
c  **   2  LAST DAY OF GROWING SEASON (JULIAN DAY)
c  **
c  **   1  MIN TG1 OVER EARTH FOR CURRENT DAY (C)
c  **   2  MAX TG1 OVER EARTH FOR CURRENT DAY (C)
c  **   3  MIN TS OVER EARTH FOR CURRENT DAY (K)
c  **   4  MAX TS OVER EARTH FOR CURRENT DAY (K)
c  **   5  SUM OF COMPOSITE TS OVER TIME FOR CURRENT DAY (C)
c  **   6  MAX COMPOSITE TS FOR CURRENT DAY (K)
c  **
      INCLUDE 'BA94jalC9.COM'
      LOGICAL POLE
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1)
      COMMON/WORK2/PK(IM,JM,LM),W(IM,JM,LM),PHIE(IM,JM,LM-1),
     *  GMEAN(LM),THJL(JM,LM),THSQJL(JM,LM),SDMEAN(JM,LM-1),
     *  DUDVSQ(JM),EL(JM),RI(JM),SPI(JM,LM),PHIPI(JM,LM),
     *  TPI(JM,LM),TIL(JM),UI(JM),UMAX(JM),
     *  SOCEAN(JM),SLAND(JM),SOICE(JM),PUV(IM,JM),PI(JM),
     *  SQRTP(IM),PDA(IM),TRI(3)
      COMMON/WORK3/PHI(IM,JM,LM),TX(IM,JM,LM),
     *  THSEC(IM),PSEC(IM),SHETH(LM)
      DIMENSION LUPA(LM),LDNA(LM),D2SIG(LM)
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM),
     *  X1(IM),FCUV(2,IMH+1,JM,LM,2),
     *  FC(2,IMH+1)
      CHARACTER*16 TITLE
      DIMENSION PMB(7),GHT(7)
      DATA PMB/1000.,850.,700.,500.,300.,100.,30./,P1000/1000./
      DATA GHT/0.,1500.,3000.,5600.,9500.,16400.,24000./
      DATA IFIRST/1/,ONE/1./,ZERO20/1.E-20/
c  ** QSAT=(RAIR/RVAPOR)*6.1071*EXP((L/RVAPOR)*(1/TF-1/T))/P
      DATA AQSAT/3.797915/,BQSAT/7.93252E-6/,CQSAT/2.166847E-3/
      QSAT(TM,PR,QL)=AQSAT*EXP(QL*(BQSAT-CQSAT/TM))/PR
      CALL CLOCKS (MBEGIN)
      IDACC(4)=IDACC(4)+1
      IF (IFIRST.NE.1) GO TO 50
      IFIRST=0
c  ** INITIALIZE CERTAIN QUANTITIES
      L=LM+1
    3 L=L-1
      IF (L.EQ.1) GO TO 4
      IF (.25*(SIGE(L-1)+2*SIGE(L)+SIGE(L+1))*(PSF-PTOP)+PTOP.LT.250.)
     *   GO TO 3
    4 JET=L
      WRITE (6,888) JET
  888 FORMAT (' JET WIND LEVEL FOR DIAG',I3)
      BYIM=1./FIM
      SHA=RGAS/KAPA
      BETA=.0065
      BBYG=BETA/GRAV
      RBBYG=RGAS*BETA/GRAV
      GBYRB=GRAV/(RGAS*BETA)
      EPSLON=1.
      PTOPK=EXPBYK(PTOP)
      KM=0
      DO 5 K=1,7
      IF (PTOP.GT.PMB(K)) GO TO 6
    5 KM=KM+1
    6 JEQ=2.+.5*(JM-1)
      J50N=(50.+90.)*(JM-1)/180.+1.5
      J70N=(70.+90.)*(JM-1)/180.+1.5
      PRQ1=.75*PTOP
      DLNP12=LOG(.75/.35)
      DLNP23=LOG(.35/.1)
      DO 10 L=1,LM
      LUPA(L)=L+1
   10 LDNA(L)=L-1
      LDNA(1)=1
      LUPA(LM)=LM
      DO 20 L=1,LM
   20 D2SIG(L)=SIG(LUPA(L))-SIG(LDNA(L))
   50 CONTINUE
c  **
c  ** FILL IN HUMIDITY AND SIGMA DOT ARRAYS AT THE POLES
c  **
      DO 65 L=1,LM
      DO 65 I=2,IM
      Q(I,1,L)=Q(1,1,L)
   65 Q(I,JM,L)=Q(1,JM,L)
c  **
c  ** CALCULATE PK AND TX, THE REAL TEMPERATURE
c  **
      DO 80 L=1,LM
      PK(1,1,L)=EXPBYK(SIG(L)*P(1,1)+PTOP)
      TX(1,1,L)=T(1,1,L)*PK(1,1,L)
      PK(1,JM,L)=EXPBYK(SIG(L)*P(1,JM)+PTOP)
      TX(1,JM,L)=T(1,JM,L)*PK(1,JM,L)
      DO 70 I=2,IM
      T(I,1,L)=T(1,1,L)
      T(I,JM,L)=T(1,JM,L)
      PK(I,1,L)=PK(1,1,L)
      TX(I,1,L)=TX(1,1,L)
      PK(I,JM,L)=PK(1,JM,L)
   70 TX(I,JM,L)=TX(1,JM,L)
      DO 80 J=2,JM-1
      DO 80 I=1,IM
      PK(I,J,L)=EXPBYK(SIG(L)*P(I,J)+PTOP)
   80 TX(I,J,L)=T(I,J,L)*PK(I,J,L)
c  **
c  ** CALCULATE PUV, THE MASS WEIGHTED PRESSURE
c  **
      DO 90 J=2,JM
      I=IM
      DO 85 IP1=1,IM
      PUV(I,J)=RAVPN(J-1)*(P(I,J-1)+P(IP1,J-1))+
     *         RAVPS(  J)*(P(I,  J)+P(IP1,  J))
   85 I=IP1
   90 CONTINUE
c  **
c  ** J LOOPS FOR ALL PRIMARY GRID ROWS
c  **
      DO 190 J=1,JM
      POLE=.FALSE.
      IF (J.EQ.1.OR.J.EQ.JM) POLE=.TRUE.
      IMAX=IM
      IF (POLE) IMAX=1
      DXYPJ=DXYP(J)
c  ** NUMBERS ACCUMULATED FOR A SINGLE LEVEL
      AT1=0.
      BT1=0.
      CT1=0.
      BSCOV=0.
      CSCOV=0.
      PI(J)=0.
      SLAND(J)=0.
      SOICE(J)=0.
      SOCEAN(J)=0.
      DO 120 I=1,IMAX
      JR=JREG(I,J)
      PLAND=FDATA(I,J,2)
      POICE=ODATA(I,J,2)*(1.-PLAND)
      PLICE=FDATA(I,J,3)*PLAND
      POCEAN=(1.-PLAND)-POICE
      PEARTH=PLAND-PLICE
      SLAND(J)=SLAND(J)+PLAND
      SOICE(J)=SOICE(J)+POICE
      SOCEAN(J)=SOCEAN(J)+POCEAN
      AT1=AT1+(TX(I,J,1)-273.16)*POCEAN
      BT1=BT1+(TX(I,J,1)-273.16)*PLAND
      CT1=CT1+(TX(I,J,1)-273.16)*POICE
      DJ(JR,22)=DJ(JR,22)+(TX(I,J,1)-273.16)*DXYPJ
      SCOVL=0.
      IF (GDATA(I,J,2).GT.0.) SCOVL=PEARTH
      IF (GDATA(I,J,12).GT.0.) SCOVL=SCOVL+PLICE
      BSCOV=BSCOV+SCOVL
      SCOVOI=0.
      IF (GDATA(I,J,1).GT.0.) SCOVOI=POICE
      CSCOV=CSCOV+SCOVOI
      DJ(JR,31)=DJ(JR,31)+(SCOVL+SCOVOI)*DXYPJ
      PI(J)=PI(J)+P(I,J)
      AIJ(I,J,2)=AIJ(I,J,2)+(SCOVOI+SCOVL)
      AIJ(I,J,3)=AIJ(I,J,3)+(GDATA(I,J,1)*POICE+GDATA(I,J,2)*PEARTH+
     *  GDATA(I,J,12)*PLICE)
c     TS=TX(I,J,1)*((P(I,J)+PTOP)/(SIG(1)*P(I,J)+PTOP))**RBBYG
c     AIJ(I,J,8)=AIJ(I,J,8)+((P(I,J)+PTOP)*(1.+BBYG*FDATA(I,J,1)/TS)
c    *  **GBYRB-P1000)
      PSNOW=0.
      IF (GDATA(I,J,2).GT.0.) PSNOW=PEARTH
      AIJ(I,J,29)=AIJ(I,J,29)+POICE+PLICE+PSNOW
c     AIJ(I,J,33)=AIJ(I,J,33)+(TS-273.16)
      AIJ(I,J,38)=AIJ(I,J,38)+((P(I,J)+PTOP)*(1.+BBYG*FDATA(I,J,1)/
     *  BLDATA(I,J,2))**GBYRB-P1000)
  120 CONTINUE
      AJ(J,22)=AJ(J,22)+AT1
      BJ(J,22)=BJ(J,22)+BT1
      CJ(J,22)=CJ(J,22)+CT1
      BJ(J,31)=BJ(J,31)+BSCOV
      CJ(J,31)=CJ(J,31)+CSCOV
      APJ(J,1)=APJ(J,1)+PI(J)
c  ** GEOPOTENTIALS CALCULATED FOR EACH LAYER
      DO 160 I=1,IMAX
      P1=SIG(1)*P(I,J)+PTOP
      PUP=SIG(2)*P(I,J)+PTOP
      IF (ABS(TX(I,J,2)-TX(I,J,1)).LT.EPSLON) GO TO 152
      BBYGV=LOG(TX(I,J,1)/TX(I,J,2))/(RGAS*LOG(P1/PUP))
      PHI(I,J,1)=FDATA(I,J,1)+TX(I,J,1)
     *  *(((P(I,J)+PTOP)/P1)**(RGAS*BBYGV)-1.)/BBYGV
      PHI(I,J,2)=PHI(I,J,1)+(TX(I,J,1)-TX(I,J,2))/BBYGV
      GO TO 154
  152 PHI(I,J,1)=FDATA(I,J,1)+RGAS*TX(I,J,1)*LOG((P(I,J)+PTOP)/P1)
      PHI(I,J,2)=PHI(I,J,1)+RGAS*.5*(TX(I,J,1)+TX(I,J,2))*LOG(P1/PUP)
  154 DO 160 L=3,LM
      PDN=PUP
      PUP=SIG(L)*P(I,J)+PTOP
      IF (ABS(TX(I,J,L)-TX(I,J,L-1)).LT.EPSLON) GO TO 156
      BBYGV=LOG(TX(I,J,L-1)/TX(I,J,L))/(RGAS*LOG(PDN/PUP))
      PHI(I,J,L)=PHI(I,J,L-1)+(TX(I,J,L-1)-TX(I,J,L))/BBYGV
      GO TO 160
  156 PHI(I,J,L)=PHI(I,J,L-1)+RGAS*.5*(TX(I,J,L-1)+TX(I,J,L))
     *  *LOG(PDN/PUP)
  160 CONTINUE
      IF (.NOT.POLE) GO TO 170
      DO 162 L=1,LM
      DO 162 I=2,IM
  162 PHI(I,J,L)=PHI(1,J,L)
c  ** CALCULATE GEOPOTENTIAL HEIGHTS AT SPECIFIC MILLIBAR LEVELS
  170 DO 180 I=1,IMAX
      K=1
      L=1
  172 L=L+1
      PL=SIG(L)*P(I,J)+PTOP
      IF (PMB(K).LT.PL.AND.L.LT.LM) GO TO 172
      IF (ABS(TX(I,J,L)-TX(I,J,L-1)).LT.EPSLON) GO TO 176
      BBYGV=(TX(I,J,L-1)-TX(I,J,L))/(PHI(I,J,L)-PHI(I,J,L-1))
  174 AIJ(I,J,8+K)=AIJ(I,J,8+K)+(PHI(I,J,L)
     *  -TX(I,J,L)*((PMB(K)/PL)**(RGAS*BBYGV)-1.)/BBYGV-GHT(K)*GRAV)
      IF (K.EQ.2) AIJ(I,J,16)=AIJ(I,J,16)+(TX(I,J,L)-273.16+(TX(I,J,L-1)
     *  -TX(I,J,L))*LOG(PMB(K)/PL)/LOG((SIG(L-1)*P(I,J)+PTOP)/PL))
      IF (K.GE.KM) GO TO 180
      K=K+1
      IF (PMB(K).LT.PL.AND.L.LT.LM) GO TO 172
      GO TO 174
  176 AIJ(I,J,8+K)=AIJ(I,J,8+K)+(PHI(I,J,L)
     *  -RGAS*TX(I,J,L)*LOG(PMB(K)/PL)-GHT(K)*GRAV)
      IF (K.EQ.2) AIJ(I,J,16)=AIJ(I,J,16)+(TX(I,J,L)-273.16)
      IF (K.GE.KM) GO TO 180
      K=K+1
      IF (PMB(K).LT.PL.AND.L.LT.LM) GO TO 172
      GO TO 176
  180 CONTINUE
  190 CONTINUE
c  ** ACCUMULATION OF TEMP., POTENTIAL TEMP., Q, AND RH
      DO 250 J=1,JM
      IMAX=IM
      IF (J.EQ.1.OR.J.EQ.JM) IMAX=1
      DXYPJ=DXYP(J)
      DO 230 L=1,LM
      ATX=0.
      BTX=0.
      CTX=0.
      TPI(J,L)=0.
      AQ=0.
      BQ=0.
      CQ=0.
      PHIPI(J,L)=0.
c     QPI=0.
      SPI(J,L)=0.
c     RHPI=0.
      DO 220 I=1,IMAX
      JR=JREG(I,J)
      PLAND=FDATA(I,J,2)
      POICE=ODATA(I,J,2)*(1.-PLAND)
      POCEAN=(1.-PLAND)-POICE
      PIJ=P(I,J)
      ATX=ATX+(TX(I,J,L)-273.16)*POCEAN
      BTX=BTX+(TX(I,J,L)-273.16)*PLAND
      CTX=CTX+(TX(I,J,L)-273.16)*POICE
      AQ=AQ+Q(I,J,L)*PIJ*POCEAN
      BQ=BQ+Q(I,J,L)*PIJ*PLAND
      CQ=CQ+Q(I,J,L)*PIJ*POICE
      DJ(JR,63)=DJ(JR,63)+Q(I,J,L)*PIJ*DSIG(L)*DXYPJ
      DJ(JR,21)=DJ(JR,21)+(TX(I,J,L)-273.16)*DSIG(L)*DXYPJ
      TPI(J,L)=TPI(J,L)+(TX(I,J,L)-273.16)*PIJ
      PHIPI(J,L)=PHIPI(J,L)+PHI(I,J,L)*PIJ
c     QPI=QPI+Q(I,J,L)*PIJ
      SPI(J,L)=SPI(J,L)+T(I,J,L)*PIJ
c     QLH=LHE
c     QSATL=QSAT(TX(I,J,L),SIG(L)*PIJ+PTOP,QLH)
c     IF (QSATL.GT.1.) QSATL=1.
c     RHPI=RHPI+Q(I,J,L)*PIJ/QSATL
  220 CONTINUE
      AJ(J,21)=AJ(J,21)+ATX*DSIG(L)
      BJ(J,21)=BJ(J,21)+BTX*DSIG(L)
      CJ(J,21)=CJ(J,21)+CTX*DSIG(L)
      AJ(J,63)=AJ(J,63)+AQ*DSIG(L)
      BJ(J,63)=BJ(J,63)+BQ*DSIG(L)
      CJ(J,63)=CJ(J,63)+CQ*DSIG(L)
c     AJL(J,L,1)=AJL(J,L,1)+TPI(J,L)
c     AJL(J,L,2)=AJL(J,L,2)+PHIPI(J,L)
c     AJL(J,L,3)=AJL(J,L,3)+QPI
c     AJL(J,L,17)=AJL(J,L,17)+SPI(J,L)
c     AJL(J,L,18)=AJL(J,L,18)+RHPI
  230 CONTINUE
  250 CONTINUE
c  **
c  ** NORTHWARD GRADIENT OF TEMPERATURE: TROPOSPHERIC AND STRATOSPHERIC
c  **
      DO 385 J=2,JM-1
c  ** MEAN TROPOSPHERIC NORTHWARD TEMPERATURE GRADIENT
      DO 340 L=1,LTM
      ADTDL=0.
      BDTDL=0.
      CDTDL=0.
      DO 335 I=1,IM
      PLAND=FDATA(I,J,2)
      POICE=ODATA(I,J,2)*(1.-PLAND)
      POCEAN=(1.-PLAND)-POICE
      ADTDL=ADTDL+(TX(I,J+1,L)-TX(I,J-1,L))*POCEAN
      BDTDL=BDTDL+(TX(I,J+1,L)-TX(I,J-1,L))*PLAND
      CDTDL=CDTDL+(TX(I,J+1,L)-TX(I,J-1,L))*POICE
  335 CONTINUE
  338 AJ(J,36)=AJ(J,36)+ADTDL*DSIG(L)
      BJ(J,36)=BJ(J,36)+BDTDL*DSIG(L)
  340 CJ(J,36)=CJ(J,36)+CDTDL*DSIG(L)
c  ** MEAN STRATOSPHERIC NORTHWARD TEMPERATURE GRADIENT
      IF (LS1.GT.LM) GO TO 380
      DO 370 L=LS1,LM
      ADTDL=0.
      BDTDL=0.
      CDTDL=0.
      DO 350 I=1,IM
      PLAND=FDATA(I,J,2)
      POICE=ODATA(I,J,2)*(1.-PLAND)
      POCEAN=(1.-PLAND)-POICE
      ADTDL=ADTDL+(TX(I,J+1,L)-TX(I,J-1,L))*POCEAN
      BDTDL=BDTDL+(TX(I,J+1,L)-TX(I,J-1,L))*PLAND
      CDTDL=CDTDL+(TX(I,J+1,L)-TX(I,J-1,L))*POICE
  350 CONTINUE
  360 AJ(J,35)=AJ(J,35)+ADTDL*DSIG(L)
      BJ(J,35)=BJ(J,35)+BDTDL*DSIG(L)
  370 CJ(J,35)=CJ(J,35)+CDTDL*DSIG(L)
  380 CONTINUE
  385 CONTINUE
c  **
c  ** STATIC STABILITIES: TROPOSPHERIC AND STRATOSPHERIC
c  **
      DO 490 J=1,JM
      IMAX=IM
      IF (J.EQ.1.OR.J.EQ.JM) IMAX=1
      DXYPJ=DXYP(J)
c  ** OLD TROPOSPHERIC STATIC STABILITY
      ASS=0.
      BSS=0.
      CSS=0.
      DO 390 I=1,IMAX
      JR=JREG(I,J)
      PLAND=FDATA(I,J,2)
      POICE=ODATA(I,J,2)*(1.-PLAND)
      POCEAN=(1.-PLAND)-POICE
      SS=(T(I,J,LTM)-T(I,J,1))/(PHI(I,J,LTM)-PHI(I,J,1)+ZERO20)
      ASS=ASS+SS*POCEAN
      BSS=BSS+SS*PLAND
      CSS=CSS+SS*POICE
      DJ(JR,25)=DJ(JR,25)+SS*DXYPJ
  390 AIJ(I,J,31)=AIJ(I,J,31)+SS
      AJ(J,25)=AJ(J,25)+ASS
      BJ(J,25)=BJ(J,25)+BSS
      CJ(J,25)=CJ(J,25)+CSS
c  ** OLD STRATOSPHERIC STATIC STABILITY
      ASS=0.
      BSS=0.
      CSS=0.
      DO 440 I=1,IMAX
      JR=JREG(I,J)
      PLAND=FDATA(I,J,2)
      POICE=ODATA(I,J,2)*(1.-PLAND)
      POCEAN=(1.-PLAND)-POICE
      SS=(T(I,J,LM)-T(I,J,LTM))/((PHI(I,J,LM)-PHI(I,J,LTM))+ZERO20)
      ASS=ASS+SS*POCEAN
      BSS=BSS+SS*PLAND
      CSS=CSS+SS*POICE
      DJ(JR,24)=DJ(JR,24)+SS*DXYPJ
  440 CONTINUE
      AJ(J,24)=AJ(J,24)+ASS
      BJ(J,24)=BJ(J,24)+BSS
      CJ(J,24)=CJ(J,24)+CSS
c  **
c  ** NUMBERS ACCUMULATED FOR THE RADIATION EQUILIBRIUM LAYERS
c  **
      DO 470 LR=1,3
      TRI(LR)=0.
      DO 460 I=1,IMAX
  460 TRI(LR)=TRI(LR)+RQT(I,J,LR)
  470 ASJL(J,LR,1)=ASJL(J,LR,1)+(TRI(LR)-273.16*IMAX)
      PHIRI=0.
      DO 480 I=1,IMAX
  480 PHIRI=PHIRI+(PHI(I,J,LM)+RGAS*.5*(TX(I,J,LM)+RQT(I,J,1))
     *  *LOG((SIG(LM)*P(I,J)+PTOP)/PRQ1))
      ASJL(J,1,2)=ASJL(J,1,2)+PHIRI
      PHIRI=PHIRI+RGAS*.5*(TRI(1)+TRI(2))*DLNP12
      ASJL(J,2,2)=ASJL(J,2,2)+PHIRI
      PHIRI=PHIRI+RGAS*.5*(TRI(2)+TRI(3))*DLNP23
      ASJL(J,3,2)=ASJL(J,3,2)+PHIRI
  490 CONTINUE
c  **
c  ** RICHARDSON NUMBER , ROSSBY NUMBER , RADIUS OF DEFORMATION
c  **
c  ** NUMBERS ACCUMULATED OVER THE TROPOSPHERE
      DO 506 J=2,JM
      DUDVSQ(J)=0.
      UMAX(J)=0.
      DO 504 I=1,IM
      DU=U(I,J,LTM)-U(I,J,1)
      DV=V(I,J,LTM)-V(I,J,1)
      DUDVSQ(J)=DUDVSQ(J)+(DU*DU+DV*DV)*PUV(I,J)
  504 CONTINUE
  506 CONTINUE
      DO 510 J=2,JM-1
      PIBYIM=PI(J)*BYIM
      DLNP=LOG((SIG(1)*PIBYIM+PTOP)/(SIG(LTM)*PIBYIM+PTOP))
      DLNS=LOG(SPI(J,LTM)/SPI(J,1))
      DS=SPI(J,LTM)-SPI(J,1)
      EL(J)=SQRT(DLNS/DLNP)
      RI(J)=DS*DLNP/(.5*(DUDVSQ(J)+DUDVSQ(J+1)))
  510 CONTINUE
      DO 515 L=1,LTM
      DO 514 J=2,JM
      UI(J)=0.
      DO 512 I=1,IM
  512 UI(J)=UI(J)+U(I,J,L)
  514 CONTINUE
      DO 515 J=2,JM-1
      UAMAX=ABS(UI(J)+UI(J+1))
      IF (UAMAX.GT.UMAX(J)) UMAX(J)=UAMAX
  515 CONTINUE
      DO 520 J=2,JM-1
      ROSSX=DYP(J)/(DXYP(J)*SINP(J))
      ELX=1./SINP(J)
      AJ(J,27)=AJ(J,27)+RI(J)*SOCEAN(J)
      BJ(J,27)=BJ(J,27)+RI(J)*SLAND(J)
      CJ(J,27)=CJ(J,27)+RI(J)*SOICE(J)
      AJ(J,29)=AJ(J,29)+UMAX(J)*SOCEAN(J)*ROSSX
      BJ(J,29)=BJ(J,29)+UMAX(J)*SLAND(J)*ROSSX
      CJ(J,29)=CJ(J,29)+UMAX(J)*SOICE(J)*ROSSX
      AJ(J,38)=AJ(J,38)+EL(J)*SOCEAN(J)*ELX
      BJ(J,38)=BJ(J,38)+EL(J)*SLAND(J)*ELX
      CJ(J,38)=CJ(J,38)+EL(J)*SOICE(J)*ELX
  520 CONTINUE
c  ** NUMBERS ACCUMULATED OVER THE STRATOSPHERE
cNOST IF (LS1.GT.LM) GO TO 551    NEEDED FOR RUNS WITHOUT A STRATOSPHERE
      DO 532 J=2,JM
      DUDVSQ(J)=0.
      UMAX(J)=0.
  532 CONTINUE
      DO 536 J=2,JM
      DO 534 I=1,IM
      DU=U(I,J,LM)-U(I,J,LTM)
      DV=V(I,J,LM)-V(I,J,LTM)
      DUDVSQ(J)=DUDVSQ(J)+(DU*DU+DV*DV)*PUV(I,J)
  534 CONTINUE
  536 CONTINUE
      DO 540 J=2,JM-1
      PIBYIM=PI(J)*BYIM
      DLNP=LOG((SIG(LTM)*PIBYIM+PTOP)/(SIG(LM)*PIBYIM+PTOP))
      DLNS=LOG(SPI(J,LM)/SPI(J,LTM))
      DS=SPI(J,LM)-SPI(J,LTM)
      EL(J)=SQRT(DLNS/DLNP)
      RI(J)=DS*DLNP/(.5*(DUDVSQ(J)+DUDVSQ(J+1)))
  540 CONTINUE
      DO 545 L=LS1,LM
      DO 544 J=2,JM
      UI(J)=0.
      DO 542 I=1,IM
  542 UI(J)=UI(J)+U(I,J,L)
  544 CONTINUE
      DO 545 J=2,JM-1
      UAMAX=ABS(UI(J)+UI(J+1))
      IF (UAMAX.GT.UMAX(J)) UMAX(J)=UAMAX
  545 CONTINUE
      DO 550 J=2,JM-1
      ROSSX=DYP(J)/(DXYP(J)*SINP(J))
      ELX=1./SINP(J)
      AJ(J,26)=AJ(J,26)+RI(J)*SOCEAN(J)
      BJ(J,26)=BJ(J,26)+RI(J)*SLAND(J)
      CJ(J,26)=CJ(J,26)+RI(J)*SOICE(J)
      AJ(J,28)=AJ(J,28)+UMAX(J)*SOCEAN(J)*ROSSX
      BJ(J,28)=BJ(J,28)+UMAX(J)*SLAND(J)*ROSSX
      CJ(J,28)=CJ(J,28)+UMAX(J)*SOICE(J)*ROSSX
      AJ(J,37)=AJ(J,37)+EL(J)*SOCEAN(J)*ELX
      BJ(J,37)=BJ(J,37)+EL(J)*SLAND(J)*ELX
      CJ(J,37)=CJ(J,37)+EL(J)*SOICE(J)*ELX
  550 CONTINUE
c 551 CONTINUE
c  **
c  ** MEAN TROPOSPHERIC LAPSE RATES:  MOIST CONVECTIVE, ACTUAL,
c  **    DRY ADIABATIC
c  **
      X=RGAS*LHE*LHE/(SHA*461.5)
      DO 570 J=1,JM
      GAMM=0.
      DO 560 L=1,LTM
      TZL=TPI(J,L)/PI(J)+273.16
      PRT=(SIG(L)*PI(J)*BYIM+PTOP)*RGAS*TZL
      ESEPS=QSAT(TZL,ONE,LHE)
      GAMM=GAMM+(PRT+LHE*ESEPS)/(PRT+X*ESEPS/TZL)*DSIG(L)
  560 CONTINUE
      AJ(J,65)=AJ(J,65)+GAMM*SOCEAN(J)
      BJ(J,65)=BJ(J,65)+GAMM*SLAND(J)
      CJ(J,65)=CJ(J,65)+GAMM*SOICE(J)
      GAMX=(TPI(J,1)-TPI(J,LTM))/(PHIPI(J,LTM)-PHIPI(J,1))
      AJ(J,64)=AJ(J,64)+GAMX*SOCEAN(J)
      BJ(J,64)=BJ(J,64)+GAMX*SLAND(J)
      CJ(J,64)=CJ(J,64)+GAMX*SOICE(J)
  570 CONTINUE
c  ** DRY ADIABATIC LAPSE RATE
      GAMD=.0098
      DO 580 J=1,JM
      TPIL=0.
      DO 575 L=1,LTM
  575 TPIL=TPIL+TPI(J,L)*DSIG(L)
      TIL(J)=TPIL/(PI(J)*(SIGE(1)-SIGE(LTM+1)))
  580 CONTINUE
      DO 590 J=2,JM-1
      X=SINP(J)*GRAV/(COSP(J)*RGAS*2.*DLAT)
      DT2=TIL(J+1)-TIL(J-1)
      GAMC=GAMD+X*DT2/(TIL(J)+273.16)
      AJ(J,66)=AJ(J,66)+GAMC*SOCEAN(J)
      BJ(J,66)=BJ(J,66)+GAMC*SLAND(J)
      CJ(J,66)=CJ(J,66)+GAMC*SOICE(J)
  590 CONTINUE
c  **
c  ** EASTWARD TRANSPORTS
c  **
      I=IM
      DO 600 L=1,LM
      DO 600 J=2,JM-1
      DO 600 IP1=1,IM
      AIJ(I,J,55)=AIJ(I,J,55)+(P(I,J)+P(IP1,J))*(U(I,J,L)+U(I,J+1,L))
     *  *(Q(I,J,L)+Q(IP1,J,L))*DSIG(L)
  600 I=IP1
c  **
c  ** MOMENTUM, KINETIC ENERGY, NORTHWARD TRANSPORTS, ANGULAR MOMENTUM
c  **
      DO 640 J=2,JM
      P4I=0.
      I=IM
      DO 610 IP1=1,IM
      P4=P(I,J-1)+P(IP1,J-1)+P(I,J)+P(IP1,J)
      P4I=P4I+P4
      AIJ(I,J,8)=AIJ(I,J,8)+P4
      AIJ(I,J,39)=AIJ(I,J,39)+U(I,J,JET)
      AIJ(I,J,40)=AIJ(I,J,40)+V(I,J,JET)
  610 I=IP1
      APJ(J,2)=APJ(J,2)+P4I
      DO 640 L=1,LM
      PU4I=0.
      PV4I=0.
c     PWW4I=0.
c     PT16I=0.
c     PTV16I=0.
c     PZ16I=0.
c     PZV16I=0.
c     PQ16I=0.
c     PQV16I=0.
c     PWWV4I=0.
      PUV4I=0.
      I=IM
      DO 620 IP1=1,IM
      P4=P(I,J-1)+P(IP1,J-1)+P(I,J)+P(IP1,J)
      PU4I=PU4I+P4*U(I,J,L)
      PV4I=PV4I+P4*V(I,J,L)
c     PWW4I=PWW4I+P4*(U(I,J,L)*U(I,J,L)+V(I,J,L)*V(I,J,L))
c     PWWV4I=PWWV4I+P4*(U(I,J,L)*U(I,J,L)+V(I,J,L)*V(I,J,L))*V(I,J,L)
      PUV4I=PUV4I+P4*U(I,J,L)*V(I,J,L)
      T4=TX(I,J-1,L)+TX(IP1,J-1,L)+TX(I,J,L)+TX(IP1,J,L)
c     PT16I=PT16I+P4*T4
c     PTV16I=PTV16I+P4*T4*V(I,J,L)
      Z4=PHI(I,J-1,L)+PHI(IP1,J-1,L)+PHI(I,J,L)+PHI(IP1,J,L)
c     PZ16I=PZ16I+P4*Z4
c     PZV16I=PZV16I+P4*Z4*V(I,J,L)
c     Q4=Q(I,J-1,L)+Q(IP1,J-1,L)+Q(I,J,L)+Q(IP1,J,L)
c     PQ16I=PQ16I+P4*Q4
c     PQV16I=PQV16I+P4*Q4*V(I,J,L)
      AIJ(I,J,20)=AIJ(I,J,20)+P4*(SHA*T4+Z4)*V(I,J,L)*DSIG(L)*DXV(J)
cORR  AIJ(IP1,J,56)=AIJ(IP1,J,56)+(P(IP1,J-1)+P(IP1,J))
      AIJ(I,J,56)=AIJ(I,J,56)+(P(IP1,J-1)+P(IP1,J))
     *  *(V(I,J,L)+V(IP1,J,L))*(Q(IP1,J-1,L)+Q(IP1,J,L))*DSIG(L)
c        AIJL(I,J,L,1)=AIJL(I,J,L,1)+P4*U(I,J,L)
c        AIJL(I,J,L,2)=AIJL(I,J,L,2)+P4*V(I,J,L)
c        AIJL(I,J,L,3)=AIJL(I,J,L,3)+P4*(SHA*T4+Z4)
  620 I=IP1
c     AJL(J,L,4)=AJL(J,L,4)+PU4I
c     AJL(J,L,5)=AJL(J,L,5)+PV4I
c     AJL(J,L,14)=AJL(J,L,14)+(PU4I*PU4I+PV4I*PV4I)/P4I
c     AJL(J,L,15)=AJL(J,L,15)+PWW4I
c     AJL(J,L,20)=AJL(J,L,20)+PT16I*PV4I/P4I
c     AJL(J,L,21)=AJL(J,L,21)+PTV16I
c     AJL(J,L,22)=AJL(J,L,22)+PZ16I*PV4I/P4I
c     AJL(J,L,23)=AJL(J,L,23)+PZV16I
c     AJL(J,L,24)=AJL(J,L,24)+PQ16I*PV4I/P4I
c     AJL(J,L,25)=AJL(J,L,25)+PQV16I
c     AJL(J,L,26)=AJL(J,L,26)+PWW4I*PV4I/P4I
c     AJL(J,L,27)=AJL(J,L,27)+PWWV4I
      AJL(J,L,48)=AJL(J,L,48)+PU4I*PV4I/P4I
      AJL(J,L,49)=AJL(J,L,49)+PUV4I
  640 CONTINUE
c  **
c  ** EVEN LEVEL GEOPOTENTIALS, VERTICAL WINDS AND VERTICAL TRANSPORTS
c  **
      DO 655 J=1,JM
      IMAX=IM
      IF (J.EQ.1 .OR. J.EQ.JM) IMAX=1
c     PITI=0.
c     DO 648 I=1,IMAX
c 648 PITI=PITI+PIT(I,J)
      DO 655 L=1,LM-1
c     SDI=0.
c     PZI=0.
c     SDZI=0.
c     PDSE2I=0.
c     SDDS2I=0.
c     PQ2I=0.
c     SDQ2I=0.
      DO 650 I=1,IMAX
c     SDI=SDI+SD(I,J,L)
      PE=SIGE(L+1)*P(I,J)+PTOP
      PKE=EXPBYK(PE)
      THETA=THBAR(T(I,J,L+1),T(I,J,L))
      W(I,J,L)=SD(I,J,L)*THETA*PKE/PE
c     PHIE(I,J,L)=PHI(I,J,L)+SHA*THETA*(PK(I,J,L)-PKE)
c     PZI=PZI+PHIE(I,J,L)*P(I,J)
c     SDZI=SDZI+PHIE(I,J,L)*SD(I,J,L)
c     PDSE2I=PDSE2I+(SHA*(TX(I,J,L)+TX(I,J,L+1))+2.*PHIE(I,J,L))*P(I,J)
c     SDDS2I=SDDS2I+(SHA*(TX(I,J,L)+TX(I,J,L+1))+2.*PHIE(I,J,L))*
c    *  SD(I,J,L)
c     PQ2I=PQ2I+(Q(I,J,L)*Q(I,J,L+1)/(Q(I,J,L)+Q(I,J,L+1)+ZERO20))*
c    *   P(I,J)
c     SDQ2I=SDQ2I+(Q(I,J,L)*Q(I,J,L+1)/(Q(I,J,L)+Q(I,J,L+1)+
c    *  ZERO20))*SD(I,J,L)
  650 CONTINUE
c     SDMEAN(J,L)=SDI*BYIM
c     AJL(J,L,6)=AJL(J,L,6)+SDI+DSIG(L+1)*PITI
c     AJL(J,L,34)=AJL(J,L,34)+(SDZI-PZI*SDI/PI(J))
c     AJL(J,L,30)=AJL(J,L,30)+PDSE2I*SDI/PI(J)
c     AJL(J,L,31)=AJL(J,L,31)+SDDS2I
c     AJL(J,L,32)=AJL(J,L,32)+PQ2I*SDI/PI(J)
c     AJL(J,L,33)=AJL(J,L,33)+SDQ2I
  655 CONTINUE
c  **
c  ** VERTICAL TRANSPORT OF KINETIC ENERGY AND ANGULAR MOMENTUM
c  **
c  ** FILL IN AND/OR DOUBLE SD AND SDMEAN AT THE POLES
c     DO 657 L=1,LM-1
c     SDMEAN(1,L)=2.*FIM*SDMEAN(1,L)
c     SDMEAN(JM,L)=2.*FIM*SDMEAN(JM,L)
c     SDSP=2.*SD(1,1,L)
c     SDNP=2.*SD(1,JM,L)
c     DO 657 I=1,IM
c     SD(I,1,L)=SDSP
c 657 SD(I,JM,L)=SDNP
c     DO 670 J=2,JM
c     AMA=RADIUS*OMEGA*COSV(J)
c     DO 670 L=1,LM-1
c     TKEM=0.
c     TKET=0.
c     UM=0.
c     UT=0.
c     I=IM
c     DO 660 IP1=1,IM
c     SDU=SD(I,J,L)+SD(IP1,J,L)+SD(I,J-1,L)+SD(IP1,J-1,L)
c     UE=U(I,J,L)+U(I,J,L+1)
c     TKE=UE*UE+(V(I,J,L)+V(I,J,L+1))*(V(I,J,L)+V(I,J,L+1))
c     TKEM=TKEM+TKE
c     TKET=TKET+TKE*SDU
c     UM=UM+UE
c     UT=UT+UE*SDU
c 660 I=IP1
c     AJL(J,L,35)=AJL(J,L,35)+TKET
c     AJL(J,L,36)=AJL(J,L,36)+(UT-2.*UM*(SDMEAN(J,L)+SDMEAN(J-1,L)))
c     AJL(J,L,37)=AJL(J,L,37)+(UT+4*AMA*FIM*(SDMEAN(J,L)+SDMEAN(J-1,L)))
c 670 CONTINUE
c  **
c  ** AVAILABLE POTENTIAL ENERGY
c  **
c  ** SET UP FOR CALCULATION
      DO 710 L=1,LM
  710 GMEAN(L)=0.
      DO 740 J=1,JM
      IMAX=IM
      IF (J.EQ.1 .OR. J.EQ.JM) IMAX=1
      DO 720 I=1,IMAX
  720 SQRTP(I)=SQRT(P(I,J))
c  ** GMEAN CALCULATED FOR EACH LAYER, THJL, THSQJL ARRAYS FILLED
      DO 730 L=1,LM
      LDN=LDNA(L)
      LUP=LUPA(L)
      THJL(J,L)=0.
      THSQJL(J,L)=0.
      DO 730 I=1,IMAX
      THJL(J,L)=THJL(J,L)+T(I,J,L)*SQRTP(I)
      THSQJL(J,L)=THSQJL(J,L)+T(I,J,L)*T(I,J,L)*P(I,J)
  730 GMEAN(L)=GMEAN(L)+(SIG(L)*P(I,J)+PTOP)*(T(I,J,LUP)-T(I,J,LDN))*
     *  DXYP(J)/(P(I,J)*PK(I,J,L))
  740 CONTINUE
c  ** CALCULATE APE
      DO 760 L=1,LM
      LP1=LUPA(L)
      LM1=LDNA(L)
      THJL(1,L)=THJL(1,L)*FIM
      THJL(JM,L)=THJL(JM,L)*FIM
      THSQJL(1,L)=THSQJL(1,L)*FIM
      THSQJL(JM,L)=THSQJL(JM,L)*FIM
      THGM=0.
      DO 750 J=1,JM
  750 THGM=THGM+THJL(J,L)*DXYP(J)
      THGM=THGM/AREAG
      GMEANL=GMEAN(L)/((SIG(LM1)-SIG(LP1))*AREAG)
      DO 760 J=1,JM
  760 AJL(J,L,16)=AJL(J,L,16)+(THSQJL(J,L)-2.*THJL(J,L)*THGM+THGM*THGM*
     *  FIM)/GMEANL
c  **
c  ** OMEGA'*ALPHA' ;  BAROCLINIC EKE GENERATION
c  **
c     DO 770 L=1,LM
c     DO 770 J=2,JM-1
c     PWAI=0.
c     SPAI=0.
c     PWI=0.
c     IM1=IM
c     DO 766 I=1,IM
c     PL=SIG(L)*P(I,J)+PTOP
c     SPA=SIG(L)*P(I,J)*RGAS*TX(I,J,L)/PL
c     SVDX=(V(IM1,J+1,L)+V(I,J+1,L))*DXV(J+1)-(V(IM1,J,L)+V(I,J,L))*
c    *   DXV(J)
c     SUDY=(U(I,J+1,L)+U(I,J,L)-U(IM1,J+1,L)-U(IM1,J,L))*DYP(J)
c     PWA=-.5*SPA*(SUDY+SVDX)*DSIG(L)*P(I,J)
c     IF (L.NE.LM) PWA=PWA+SD(I,J,L)*((PHIE(I,J,L)-PHI(I,J,L))
c    *    +SPA)
c     IF (L.NE.1) PWA=PWA+SD(I,J,L-1)*((PHI(I,J,L)-PHIE(I,J,L-1))
c    *    -SPA)
c     PWAI=PWAI+PWA
c     SPAI=SPAI+SPA
c     PWI=PWI+PWA*P(I,J)/SPA
c 766 IM1=I
c 770 AJL(J,L,7)=AJL(J,L,7)-(PWAI-PWI*SPAI/PI(J))
c  **
c  ** P-K BY PRESSURE GRADIENT FORCE
c  **
c     DO 774 L=1,LM
c     DO 774 J=2,JM
c     PDA4I=0.
c     VPDA4I=0.
c     DVTI=0.
c     VDVTI=0.
c     DUTI=0.
c     UDUTI=0.
c     UPDA4I=0.
c     I=IM
c     DO 772 IP1=1,IM
c     PDA4=(P(I,J)+P(IP1,J))*DXYP(J) + (P(I,J-1)+P(IP1,J-1))*DXYP(J-1)
c     PDA4I=PDA4I+PDA4
c     VPDA4I=VPDA4I+V(I,J,L)*PDA4
c     DVTI=DVTI+DVT(I,J,L)
c     VDVTI=VDVTI+V(I,J,L)*DVT(I,J,L)
c     DUTI=DUTI+DUT(I,J,L)
c     UDUTI=UDUTI+U(I,J,L)*DUT(I,J,L)
c     UPDA4I=UPDA4I+U(I,J,L)*PDA4
c 772 I=IP1
c 774 AJL(J,L,53)=AJL(J,L,53)+VDVTI+UDUTI
c    *   -(UPDA4I*DUTI+VPDA4I*DVTI)/PDA4I
c        IF (JM.NE.24) GO TO 850
c  **
c  ** CERTAIN HORIZONTAL WIND AVERAGES
c  **
      DO 790 L=1,LM
      DO 780 J=2,JM
c     AJL(J,L,41)=AJL(J,L,41)+(.5*U(4,J,L)+U(5,J,L)+U(6,J,L)+U(7,J,L)+
c    *  U(8,J,L)+.5*U(9,J,L))
      AJL(J,L,42)=AJL(J,L,42)+(.5*V(4,J,L)+V(5,J,L)+V(6,J,L)+V(7,J,L)+
     *  V(8,J,L)+.5*V(9,J,L))
c     AJL(J,L,44)=AJL(J,L,44)+(.5*U(34,J,L)+U(35,J,L)+U(36,J,L)+
c    *  U(1,J,L)+U(2,J,L)+.5*U(3,J,L))
  780 AJL(J,L,45)=AJL(J,L,45)+(.5*V(IM-2,J,L)+V(IM-1,J,L)+V(IM,J,L)+
     *  V(1,J,L)+V(2,J,L)+.5*V(3,J,L))
      DO 790 I=1,IM
      AIL(I,L,1)=AIL(I,L,1)+(.5*U(I,JEQ-2,L)+U(I,JEQ-1,L)+U(I,JEQ,L)+
     *  .5*U(I,JEQ+1,L))
c     AIL(I,L,2)=AIL(I,L,2)+(.5*V(I,JEQ-2,L)+V(I,JEQ-1,L)+V(I,JEQ,L)+
c    *  .5*V(I,JEQ+1,L))
      AIL(I,L,4)=AIL(I,L,4)+(TX(I,JEQ-2,L)+TX(I,JEQ-1,L)+TX(I,JEQ,L)-
     *  819.48)
      QLH=LHE
      AIL(I,L,5)=AIL(I,L,5)+(
     *  Q(I,JEQ-2,L)/QSAT(TX(I,JEQ-2,L),SIG(L)*P(I,JEQ-2)+PTOP,QLH)+
     *  Q(I,JEQ-1,L)/QSAT(TX(I,JEQ-1,L),SIG(L)*P(I,JEQ-1)+PTOP,QLH)+
     *  Q(I,JEQ,L)/QSAT(TX(I,JEQ,L),SIG(L)*P(I,JEQ)+PTOP,QLH))
      AIL(I,L,10)=AIL(I,L,10)+(TX(I,J50N,L)-273.16)
      AIL(I,L,12)=AIL(I,L,12)+(U(I,J50N,L)+U(I,J50N+1,L))
c     AIL(I,L,14)=AIL(I,L,14)+(TX(I,J70N,L)-273.16)
c     AIL(I,L,16)=AIL(I,L,16)+(U(I,J70N,L)+U(I,J70N+1,L))
  790 CONTINUE
c  **
c  ** CERTAIN VERTICAL WIND AVERAGES
c  **
      DO 820 L=1,LM-1
      DO 820 J=2,JM-1
      AJL(J,L,43)=AJL(J,L,43)+(W( 5,J,L)+W( 6,J,L)+W(7,J,L)
     *                        +W( 8,J,L)+W( 9,J,L))
  820 AJL(J,L,46)=AJL(J,L,46)+(W(IM-1,J,L)+W(IM,J,L)+W(1,J,L)
     *                        +W( 2,J,L)+W( 3,J,L))
      DO 840 L=1,LM-1
      DO 840 I=1,IM
      AIL(I,L, 3)=AIL(I,L, 3)+(W(I,JEQ-2,L)+W(I,JEQ-1,L)+W(I,JEQ,L))
c     AIL(I,L, 9)=AIL(I,L, 9)+W(I,J50N,L)
c     AIL(I,L,13)=AIL(I,L,13)+W(I,J70N,L)
  840 CONTINUE
  850 CONTINUE
c  **
c  ** ELIASSEN PALM FLUX
c  **
c  ** NORTHWARD TRANSPORT
      DO 868 J=2,JM
      BYDXYV=1./DXYV(J)
      I=IM
      DO 862 IP1=1,IM
      PDA(I)=.5*((P(I,J)+P(IP1,J))*DXYS(J)+(P(I,J-1)+P(IP1,J-1))*
     *  DXYN(J-1))
      PSEC(I)=PDA(I)*BYDXYV
  862 I=IP1
      DO 868 L=1,LM
      DUDP=0.
      DTHDP=0.
      UMN=0.
      THMN=0.
      LDN=LDNA(L)
      LUP=LUPA(L)
      I=IM
      DO 864 IP1=1,IM
      DUDP=DUDP+U(I,J,LUP)-U(I,J,LDN)
      DTHDP=DTHDP+T(I,J,LUP)+T(I,J-1,LUP)-T(I,J,LDN)-T(I,J-1,LDN)
      UMN=UMN+U(I,J,L)
      THMN=THMN+T(I,J,L)+T(I,J-1,L)
      THSEC(I)=T(I,J,L)+T(IP1,J,L)+T(I,J-1,L)+T(IP1,J-1,L)
  864 I=IP1
      UMN=UMN*BYIM
      THMN=2.*THMN/FIM
      FPHI=0.
      SMALL=.0002*FIM*T(1,J,L)
      IF (DTHDP.LT.SMALL) WRITE (6,999) J,L,DTHDP,SMALL
      IF (DTHDP.LT.SMALL) DTHDP=SMALL
      DO 866 I=1,IM
  866 FPHI=FPHI+PSEC(I)*V(I,J,L)*(.5*(THSEC(I)-THMN)*DUDP/DTHDP
     *   -U(I,J,L)+UMN)
  868 AJL(J,L,41)=AJL(J,L,41)+FPHI
c  ** VERTICAL TRANSPORT
      DO 878 J=2,JM-1
      DO 878 L=1,LM-1
      THMN=0.
      SDMN=0.
      DTHDP=0.
      DO 872 I=1,IM
      DTHDP=DTHDP+T(I,J,L+1)-T(I,J,L)
      THMN=THMN+T(I,J,L+1)+T(I,J,L)
  872 SDMN=SDMN+SD(I,J,L)
      SMALL=.0001*FIM*T(1,J,L+1)
      IF (DTHDP.LT.SMALL) WRITE (6,999) J,L,DTHDP,SMALL
      IF (DTHDP.LT.SMALL) DTHDP=SMALL
      THMN=THMN/FIM
      SDMN=SDMN/FIM
      DUDX=0.
      PVTHP=0.
      SDPU=0.
      IM1=IM
      DO 874 I=1,IM
      DUDX=DUDX+DXV(J+1)*(U(I,J+1,L)+U(I,J+1,L+1))-DXV(J)*
     *   (U(I,J,L)+U(I,J,L+1))
      UPE=U(IM1,J,L)+U(IM1,J+1,L)+U(I,J,L)+U(I,J+1,L)+
     *    U(IM1,J,L+1)+U(IM1,J+1,L+1)+U(I,J,L+1)+U(I,J+1,L+1)
      VPE=V(IM1,J,L)+V(IM1,J+1,L)+V(I,J,L)+V(I,J+1,L)+
     *    V(IM1,J,L+1)+V(IM1,J+1,L+1)+V(I,J,L+1)+V(I,J+1,L+1)
      PVTHP=PVTHP+P(I,J)*VPE*(T(I,J,L)+T(I,J,L+1)-THMN)
      SDPU=SDPU+(SD(I,J,L)-SDMN)*UPE
  874 IM1=I
      AJL(J,L,44)=AJL(J,L,44)+(.5*FIM*F(J)-.25*DUDX)*DSIGO(L)*PVTHP
     *   /DTHDP + SDPU
  878 CONTINUE
c  **
c  ** POTENTIAL VORTICITY  (10/30/81)
c  **
c     DO 895 L=1,LM
c     LUP=LUPA(L)
c     LDN=LDNA(L)
c     DO 895 J=2,JM-1
c     PV2I=0.
c     I=IM
c     DO 890 IP1=1,IM
c     DS2=T(I,J,LUP)+T(IP1,J,LUP)-T(I,J,LDN)-T(IP1,J,LDN)
c     PV2I=PV2I+DS2*(U(I,J+1,L)*DXV(J+1)-U(I,J,L)*DXV(J) - F(J))
c 890 I=IP1
c 895 AJL(J,L,52)=AJL(J,L,52)+PV2I
c  **
c  ** TRANSFORMED STREAM FUNCTION
c  **
      DO 912 J=2,JM
      P2I=0.
      DO 904 I=1,IM
  904 P2I=P2I+(P(I,J-1)+P(I,J))
      DO 910 L=1,LM
      LUP=LUPA(L)
      LDN=LDNA(L)
      DS2=0.
      DO 906 I=1,IM
  906 DS2=DS2+T(I,J,LUP)+T(I,J-1,LUP)-T(I,J,LDN)-T(I,J-1,LDN)
      SMALL=.0002*FIM*T(1,J,L)
      IF (DS2.LT.SMALL) WRITE (6,999) J,L,DS2,SMALL
      IF (DS2.LT.SMALL) DS2=SMALL
      PV4I=0.
      PS16I=0.
      PSV16I=0.
      I=IM
      DO 908 IP1=1,IM
      P4=P(I,J-1)+P(I,J)+P(IP1,J-1)+P(IP1,J)
      S4=T(I,J-1,L)+T(I,J,L)+T(IP1,J-1,L)+T(IP1,J,L)
      PV4I=PV4I+P4*V(I,J,L)
      PS16I=PS16I+P4*S4
      PSV16I=PSV16I+P4*S4*V(I,J,L)
  908 I=IP1
  910 SHETH(L)=(PSV16I-.5*PS16I*PV4I/P2I)*D2SIG(L)/DS2
      DO 912 L=1,LM
      LUP=LUPA(L)
      LDN=LDNA(L)
  912 AJL(J,L,47)=AJL(J,L,47)+(SHETH(LUP)-SHETH(LDN))/D2SIG(L)
c  ** ACCUMULATE TIME USED IN DIAGA
      CALL CLOCKS (MEND)
      MINC=MBEGIN-MEND
      MDIAG=MDIAG+MINC
      MDYN=MDYN-MINC
c     WRITE (6,997) IDACC,MINC,MDIAG
      RETURN
  997 FORMAT (' DIAGNOSTICS ACCUMULATED ',12I4,15X,2I7)
  999 FORMAT (' DTHETA/DP IS TOO SMALL AT J=',I4,' L=',I4,2F15.6)
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE DIAGB (U,V,T,P,Q)
c  **
c  ** CONTENTS OF AJK(J,K,N)  (SUM OVER LONGITUDE AND TIME OF)
c  CP   1  DP  (PDN-PM(K+1);  PDN=MAX(PM(K+1),PS)
c  CP   2  DP4  (PDN-PM(K+1))   (UV GRID)
c  *1   3  (TX-273.16)*DP
c  *2   4  PHI*DP
c  *3   5  Q*DP
c  17   6  TH*DP
c  18   7  RH*DP
c  *4   8  U*DP4 (100 PA*M/S)  (UV GRID)
c  *5   9  V*DP4 (100 PA*M/S)  (UV GRID)
c  14  10  (PUI*PUI+PVI*PVI)/DPI (100 N/S**2)  (UV GRID) ..I=SUM OVER I
c  15  11  (U*DP4*U*DP4+V*DP4*V*DP4)/DP4 (100 N/S**2)  (UV GRID)
c  20  12  4*PT4I*PVI/DPI (100 PA*K*M/S)  (UV GRID)
c  21  13  4*PTV4I (100 PA*K*M/S)  (UV GRID)
c  22  14  4*PZ4I*PVI/DPI (100 W/S**2)  (UV GRID)
c  23  15  4*PZV4I (100 W/S**2)  (UV GRID)
c  24  16  4*PQ4I*PVI/DPI (100 PA*M/S)  (UV GRID)
c  25  17  4*PQV4I (100 PA*M/S)  (UV GRID)
c  26  18  PWWI*PVI/DPI (100 W/S**2)  (UV GRID)
c  27  19  PWWVI (100 W/S**2)  (UV GRID)
c  48  20  PUI*PVI/DPI (100 N/S**2)  (UV GRID)
c  49  21  PUVI (100 N/S**2)  (UV GRID)
c  53  22  VDVT - VDPI*DVT/DPI
c  **  23  DP**2  (UV GRID)
c  **  24  IM  (UV GRID)
c  *6  25  W*DA  (100 NT/S)
c  30  26  WI*(SHA*TI+ZI)/FIM
c  31  27  SHA*WTI+WZI
c  32  28  WI*QI/FIM
c  33  29  WQI
c  34  30  WZI-WI*ZI/FIM
c  *7  31  2*(WPA2I-W2I*PAI/FIM)  (ODD LAYERS)
c  52  32  PV = STB*(D(UDX)-F*DA)
c  **  33  WPV4I
c  **  33  WPV4I - W2I*PV2I/FIM
c  **  35  IM  (PT GRID)
c  35  36  WKE4I  (UV GRID)
c  36  37  WU4I - W4I*UKI/FIM  (UV GRID)
c  37  38  WU4I+W4I*UEARTH  (UV GRID)
c  **  39     4*(PSV4I-PS4I*PVI/DPI)/STB (MB*MB M/S)
c  **  40     ADVU=-(DUDY-F)*V-DUDP*W  (M/S/S)
c  **  41     ADVT=-DTDY*V-DTDP*W   (DEGK/S)
c  **  42     LADVU=-(DUDY-F)*V* - DUDP*W*  (M/S/S)
c  **  43     LADVT=-DTDY*V* - DTDP*W*  (DEGK/S)
c  **  44     FY= V'T'/DTDP*DUDP - U'V'
c  **  45     FP= -U'W' - V'T'/DTDP*(DUDY-F)
c  **  46     U1 = U AT IDACC(4)=1  (M/S) (NOT SUM OVER TIME)
c  **  47     U-U1   (M/S)                (NOT SUM OVER TIME)
c  **  48     T1 = T AT IDACC(4)=1  (DEG K  THETA) (NOT SUM OVER TIME)
c  **  49     T-T1    (DEG K   THETA)              (NOT SUM OVER TIME)
c  **  50     W'TH'      (DEG K-MB/S)
c  **
c  ** CONTENTS OF AIJK(I,J,K,N)   (SUM OVER TIME OF)
c  **   1  DP4*U (100 PA*M/S)  (UV GRID)
c  **   2  DP4*V (100 PA*M/S)  (UV GRID)
c  **   3  4*DP4*(SHA*T4+Z4) (100 N/S**2)  (UV GRID)
c  **   4  DP4 (100 PA)  (UV GRID)
c  **   5  4*DP4*T4 (100 K*PA)  (UV GRID)
c  **   6  4*DP4*Q4 (100 PA)  (UV GRID)
c  **
      INCLUDE 'BA94jalC9.COM'
c  ****
c  **** MFS (CHANGED CODE)
c  **** Changed the model from 64 bit reals to 32 bit reals.  The PowerPC
c  **** 601,603,604,750 all use 32 bit reals.  The double precision were
c  **** better for science but slower.
c      REAL*8 KE
      REAL*4 KE
c  ****
c  **** END (CHANGED CODE)
c  ****
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1)
      COMMON/WORK2/PK(IM,JM,LM),W(IM,JM,LM),PHIE(IM,JM,LM-1),
     *  DTH(LM),STJK(JM,LM),DPJK(JM,LM),DPM(LM),PL(LM),
     *  UJK(JM,LM),VJK(JM,LM),WJK(JM,LM),TJK(JM,LM),
     *  PSIJK(JM,LM),UP(JM,LM),TY(JM,LM),PSIP(JM,LM),
     *  WTJK(JM,LM),UVJK(JM,LM),WUJK(JM,LM)
      COMMON/WORK3/PHI(IM,JM,LM),TX(IM,JM,LM),
     *  THSEC(IM),PSEC(IM),SHETH(LM)
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM),
     *  X1(IM),FCUV(2,IMH+1,JM,LM,2),
     *  FC(2,IMH+1),KE(IMH+1,8),APE(IMH+1,8),VAR(IMH+1,4),TPE(2)
      DIMENSION ZX(IM,JM,LM),STB(IM,JM,LM)
      EQUIVALENCE (FCUV,ZX),(FCUV(1,1,1,1,2),STB)
      DIMENSION PM(LM+1),PMO(LM)
      DATA IFIRST/1/,ZERO20/1.E-20/,BIG/1.E20/
c  ** QSAT=(RAIR/RVAPOR)*6.1071*EXP((L/RVAPOR)*(1/TF-1/T))/P
      DATA AQSAT/3.797915/,BQSAT/7.93252E-6/,CQSAT/2.166847E-3/
      QSAT(TM,PR,QL)=AQSAT*EXP(QL*(BQSAT-CQSAT/TM))/PR
      CALL CLOCKS (MBEGIN)
      IF (IFIRST.NE.1) GO TO 50
      IFIRST=0
c  ** INITIALIZE CERTAIN QUANTITIES
      LMP1=LM+1
      JET=LTM
      BYIM=1./FIM
      SHA=RGAS/KAPA
      KM=LM
      KMM1=KM-1
      PM(1)=1200.
      DO 20 L=2,LMP1
   20 PM(L)=(PSF-PTOP)*SIGE(L)+PTOP
      DO 30 L=1,LM
   30 PMO(L)=.5*(PM(L)+PM(L+1))
   50 CONTINUE
c  **
c  ** INTERNAL QUANTITIES T,TH,Q,RH
c  **
      QLH=LHE
      DO 170 J=1,JM
      IMAX=IM
      IF (J.EQ.1 .OR. J.EQ.JM) IMAX=1
      DO 170 K=1,KM
      DPI=0.
      TPI=0.
      PHIPI=0.
      QPI=0.
      THPI=0.
      RHPI=0.
      FIMI=0.
      DO 160 I=1,IMAX
c  ** FIND L=L(K) AND LUP=L(K+1) S.T. P(LUP).GT.P(K+1)
      SP=P(I,J)
      PS=SP+PTOP
      IF (PM(K+1).GE.PS) GO TO 160
      L=1
      PDN=PS
      IF (PM(K).GE.PS) GO TO 120
      PDN=PM(K)
  110 IF (PM(K).GT.SP*SIGE(L+1)+PTOP) GO TO 120
      L=L+1
      GO TO 110
  120 LUP=L
  130 IF (PM(K+1).GE.SP*SIGE(LUP+1)+PTOP) GO TO 140
      LUP=LUP+1
      GO TO 130
  140 CONTINUE
c  ** ACCUMULATE HERE
      DPI=DPI+PDN-PM(K+1)
      FIMI=FIMI+1.
  150 PUP=SP*SIGE(L+1)+PTOP
      IF (LUP.EQ.L) PUP=PM(K+1)
      DP=PDN-PUP
      TPI=TPI+(TX(I,J,L)-273.16)*DP
      PHIPI=PHIPI+PHI(I,J,L)*DP
      QPI=QPI+Q(I,J,L)*DP
      THPI=THPI+T(I,J,L)*DP
      QSATL=QSAT(TX(I,J,L),SIG(L)*SP+PTOP,QLH)
      IF (QSATL.GT.1.) QSATL=1.
      RHPI=RHPI+Q(I,J,L)*DP/QSATL
      IF (L.EQ.LUP) GO TO 160
      L=L+1
      PDN=SP*SIGE(L)+PTOP
      GO TO 150
  160 CONTINUE
      AJK(J,K,35)=AJK(J,K,35)+FIMI
      AJK(J,K,1)=AJK(J,K,1)+DPI
      AJK(J,K,3)=AJK(J,K,3)+TPI
      AJK(J,K,4)=AJK(J,K,4)+PHIPI
      AJK(J,K,5)=AJK(J,K,5)+QPI
      AJK(J,K,6)=AJK(J,K,6)+THPI
      AJK(J,K,7)=AJK(J,K,7)+RHPI
         TJK(J,K)=THPI/(DPI+ZERO20)
         IF (IDACC(4).EQ.1) AJK(J,K,48)=TJK(J,K)
         AJK(J,K,49)=TJK(J,K)-AJK(J,K,48)
  170 CONTINUE
c  **
c  ** CALCULATE STABILITY AT ODD LEVELS ON PU GRID
c  **
      DO 230 J=1,JM
      IMAX=IM
      IF (J.EQ.1 .OR.J.EQ.JM) IMAX=1
      I=IMAX
      DO 230 IP1=1,IMAX
      SP2=P(I,J)+P(IP1,J)
      SP=.5*SP2
      DO 175 L=1,LM
  175 PL(L)=SP*SIGE(L)+PTOP
      DO 180 L=1,LM-1
      DTH(L)=(T(I,J,L)+T(IP1,J,L)-T(I,J,L+1)-T(IP1,J,L+1))/
     *  (SP2*DSIGO(L))
  180 CONTINUE
      DO 220 K=1,KM
      STB(I,J,K)=0.
      IF (PM(K+1).GE.PL(1)) GO TO 220
      PMK=PMO(K)
      IF (PM(K).GT.PL(1)) PMK=.5*(SP+PTOP+PM(K+1))
      L=2
      IF (PMK.GE.PL(2)) GO TO 210
  190 LUP=L+1
      IF (L.EQ.LM) GO TO 210
      IF (PMK.GE.PL(LUP)) GO TO 200
      L=LUP
      GO TO 190
  200 DPUP=PMK-PL(LUP)
      DPDN=PL(L)-PMK
      STB(I,J,K)=(DTH(L-1)*DPUP+DTH(L)*DPDN)/(DPUP+DPDN+ZERO20)
      GO TO 220
c  ** SPECIAL CASES,  L=2, L=LM
  210 STB(I,J,K)=DTH(L-1)
  220 CONTINUE
  230 I=IP1
c  ** CALCULATE STJK; THE MEAN STATIC STABILITY
      DO 260 J=1,JM
      IMAX=IM
      IF (J.EQ.1 .OR. J.EQ.JM) IMAX=1
      DO 260 K=1,KM
      STJK(J,K)=0.
      DPJK(J,K)=0.
      I=IMAX
      DO 250 IP1=1,IMAX
      PS=.5*(P(I,J)+P(IP1,J))+PTOP
      IF (PM(K+1).GT.PS) GO TO 250
      STJK(J,K)=STJK(J,K)+STB(I,J,K)
      DPJK(J,K)=DPJK(J,K)+1.
  250 I=IP1
      STJK(J,K)=STJK(J,K)/(DPJK(J,K)+ZERO20)
      SMALL=.0001
      IF (ABS(STJK(J,K)).LT.SMALL) STJK(J,K)=-SMALL
  260 CONTINUE
c  **
c  ** CONSTANT PRESSURE DIAGNOSTICS:  FLUX, ENERGY, ANGULAR MOMENTUM
c  **
      DO 390 J=2,JM
      I=IM
      DO 280 IP1=1,IM
      PSEC(I)=.25*(P(I,J-1)+P(IP1,J-1)+P(I,J)+P(IP1,J))
      DO 270 K=1,KM
  270 ZX(I,J,K)=0.
      DO 275 L=1,LM
      DUT(I,J,L)=DUT(I,J,L)/(PSEC(I)*DXYV(J))
  275 DVT(I,J,L)=DVT(I,J,L)/(PSEC(I)*DXYV(J))
  280 I=IP1
      DO 350 K=1,KM
      DPI=0.
      DPSQI=0.
      FIMI=0.
      PUI=0.
      PVI=0.
      PWWI=0.
      PT4I=0.
      PTV4I=0.
      PZ4I=0.
      PZV4I=0.
      PQ4I=0.
      PQV4I=0.
      PWWVI=0.
      PUVI=0.
      DVTI=0.
      VDVTI=0.
      DUTI=0.
      UDUTI=0.
      PS4I=0.
      PSV4I=0.
      I=IM
      DO 340 IP1=1,IM
      SP=PSEC(I)
      PS=SP+PTOP
      IF (PM(K+1).GE.PS) GO TO 336
      L=1
      PDN=PS
      IF (PM(K).GE.PS) GO TO 300
      PDN=PM(K)
  290 IF (PM(K).GT.SP*SIGE(L+1)+PTOP) GO TO 300
      L=L+1
      GO TO 290
  300 LUP=L
  310 IF (PM(K+1).GE.SP*SIGE(LUP+1)+PTOP) GO TO 320
      LUP=LUP+1
      GO TO 310
  320 CONTINUE
      DPK=PDN-PM(K+1)
      PUK=0.
      PVK=0.
      PT4K=0.
      PZ4K=0.
      PQ4K=0.
      DUTK=0.
      DVTK=0.
      PS4K=0.
c  ** INTERPOLATE HERE
  330 PUP=SP*SIGE(L+1)+PTOP
      IF (LUP.EQ.L) PUP=PM(K+1)
      DP=PDN-PUP
      PUK=PUK+DP*U(I,J,L)
      PVK=PVK+DP*V(I,J,L)
      PT4K=PT4K+DP*(TX(I,J-1,L)+TX(IP1,J-1,L)+TX(I,J,L)+TX(IP1,J,L))
      PZ4K=PZ4K+DP*(PHI(I,J-1,L)+PHI(IP1,J-1,L)+PHI(I,J,L)+PHI(IP1,J,L))
      PQ4K=PQ4K+DP*(Q(I,J-1,L)+Q(IP1,J-1,L)+Q(I,J,L)+Q(IP1,J,L))
      DUTK=DUTK+DP*DUT(I,J,L)
      DVTK=DVTK+DP*DVT(I,J,L)
      PS4K=PS4K+DP*(T(I,J-1,L)+T(IP1,J-1,L)+T(I,J,L)+T(IP1,J,L))
      IF (LUP.EQ.L) GO TO 332
      L=L+1
      PDN=SP*SIGE(L)+PTOP
      GO TO 330
c  ** ACCUMULATE HERE
  332 FIMI=FIMI+1.
      DPI=DPI+DPK
      DPSQI=DPSQI+DPK*DPK
      IF (DPK.LT.ZERO20) DPK=ZERO20
      BYDP=1./DPK
      PUI=PUI+PUK
      PVI=PVI+PVK
      PWWI=PWWI+BYDP*(PUK*PUK+PVK*PVK)
      PWWVI=PWWVI+BYDP*BYDP*(PUK*PUK+PVK*PVK)*PVK
      PUVI=PUVI+BYDP*PUK*PVK
      PT4I=PT4I+PT4K
      PTV4I=PTV4I+BYDP*PT4K*PVK
      PZ4I=PZ4I+PZ4K
      PZV4I=PZV4I+BYDP*PZ4K*PVK
      PQ4I=PQ4I+PQ4K
      PQV4I=PQV4I+BYDP*PQ4K*PVK
      DVTI=DVTI+DVTK
      VDVTI=VDVTI+BYDP*PVK*DVTK
      DUTI=DUTI+DUTK
      UDUTI=UDUTI+BYDP*PUK*DUTK
      IF (SKIPSE.EQ.1.) GO TO 334
      AIJK(I,J,K,1)=AIJK(I,J,K,1)+PUK
      AIJK(I,J,K,2)=AIJK(I,J,K,2)+PVK
      AIJK(I,J,K,3)=AIJK(I,J,K,3)+SHA*PT4K+PZ4K
      AIJK(I,J,K,4)=AIJK(I,J,K,4)+DPK
      AIJK(I,J,K,5)=AIJK(I,J,K,5)+PT4K
      AIJK(I,J,K,6)=AIJK(I,J,K,6)+PQ4K
c  ** EDDY TRANSPORT OF THETA;  VORTICITY
  334 PS4I=PS4I+PS4K
      PSV4I=PSV4I+BYDP*PVK*PS4K
      UDX=BYDP*PUK*DXV(J)
      ZX(I,J,K)=-UDX
      IF (ZX(I,J-1,K).LT.BIG) ZX(I,J-1,K)=ZX(I,J-1,K)+UDX
      IF (ZX(I,J-1,K).GE.BIG) ZX(I,J-1,K)=0.
      GO TO 340
  336 ZX(I,J,K)=BIG
      ZX(I,J-1,K)=0.
  340 I=IP1
      DPM(K)=DPI/(FIMI+ZERO20)
      DPJK(J,K)=DPI
      AJK(J,K,2)=AJK(J,K,2)+DPI
      AJK(J,K,23)=AJK(J,K,23)+DPSQI
      AJK(J,K,24)=AJK(J,K,24)+FIMI
      IF (DPI.LT.ZERO20) DPI=ZERO20
      AJK(J,K,8)=AJK(J,K,8)+PUI
      AJK(J,K,9)=AJK(J,K,9)+PVI
      AJK(J,K,10)=AJK(J,K,10)+(PUI*PUI+PVI*PVI)/DPI
      AJK(J,K,11)=AJK(J,K,11)+PWWI
      AJK(J,K,12)=AJK(J,K,12)+PT4I*PVI/DPI
      AJK(J,K,13)=AJK(J,K,13)+PTV4I
      AJK(J,K,14)=AJK(J,K,14)+PZ4I*PVI/DPI
      AJK(J,K,15)=AJK(J,K,15)+PZV4I
      AJK(J,K,16)=AJK(J,K,16)+PQ4I*PVI/DPI
      AJK(J,K,17)=AJK(J,K,17)+PQV4I
      AJK(J,K,18)=AJK(J,K,18)+PWWI*PVI/DPI
      AJK(J,K,19)=AJK(J,K,19)+PWWVI
      AJK(J,K,20)=AJK(J,K,20)+PUI*PVI/DPI
      AJK(J,K,21)=AJK(J,K,21)+PUVI
      AJK(J,K,22)=AJK(J,K,22)+VDVTI+UDUTI-
     *   (PUI*DUTI+PVI*DVTI)/DPI
      SHETH(K)=(PSV4I-PS4I*PVI/DPI)*DXYV(J)/(STJK(J-1,K)*DXYN(J-1)+
     *   STJK(J,K)*DXYS(J))
         UJK(J,K)=PUI/DPI
         VJK(J,K)=PVI/DPI
         PSIJK(J,K)=.25*SHETH(K)/DPI
         UVJK(J,K)=(PUVI-PUI*PVI/DPI)/DPI
         IF (IDACC(4).EQ.1) AJK(J,K,46)=UJK(J,K)
         AJK(J,K,47)=UJK(J,K)-AJK(J,K,46)
  350 AJK(J,K,39)=AJK(J,K,39)+SHETH(K)
  390 CONTINUE
c  **
c  ** VERTICAL MASS FLUXES  W(I,J,K)
c  **
      DO 400 I=1,IM
      DO 400 J=1,JM
      DO 400 K=1,KM
      DUT(I,J,K)=0.
      DVT(I,J,K)=0.
  400 W(I,J,K)=0.
c  ** EASTWARD MASS FLUX DUT (PU POINTS)
      DO 460 J=2,JM-1
      DO 460 K=1,KM
      I=IM
      DO 460 IP1=1,IM
      SP=.5*(P(I,J)+P(IP1,J))
      IF (PM(K+1).GE.SP+PTOP) GO TO 460
      L=1
      PDN=SP+PTOP
      IF (PM(K).GE.SP+PTOP) GO TO 420
      PDN=PM(K)
  410 IF (PM(K).GT.SP*SIGE(L+1)+PTOP) GO TO 420
      L=L+1
      GO TO 410
  420 LUP=L
  430 IF (PM(K+1).GE.SP*SIGE(LUP+1)+PTOP) GO TO 440
      LUP=LUP+1
      GO TO 430
  440 CONTINUE
c  ** CALCULATE HERE
  450 PUP=SP*SIGE(L+1)+PTOP
      IF (LUP.EQ.L) PUP=PM(K+1)
      DPDY=(PDN-PUP)*DYP(3)
      DUT(I,J,K)=DUT(I,J,K)+DPDY*(U(I,J,L)+U(I,J+1,L))
      IF (LUP.EQ.L) GO TO 460
      L=L+1
      PDN=SP*SIGE(L)+PTOP
      GO TO 450
  460 I=IP1
c  ** NORTHWARD MASS FLUX DVT (PV POINTS)
      DO 520 J=2,JM
      DO 520 K=1,KM
      IM1=IM
      DO 520 I=1,IM
      SP=.5*(P(I,J-1)+P(I,J))
      IF (PM(K+1).GE.SP+PTOP) GO TO 520
      L=1
      PDN=SP+PTOP
      IF (PM(K).GE.SP+PTOP) GO TO 480
      PDN=PM(K)
  470 IF (PM(K).GT.SP*SIGE(L+1)+PTOP) GO TO 480
      L=L+1
      GO TO 470
  480 LUP=L
  490 IF (PM(K+1).GE.SP*SIGE(LUP+1)+PTOP) GO TO 500
      LUP=LUP+1
      GO TO 490
  500 CONTINUE
c  ** CALCULATE HERE
  510 PUP=SP*SIGE(L+1)+PTOP
      IF (LUP.EQ.L) PUP=PM(K+1)
      DPDX=(PDN-PUP)*DXV(J)
      DVT(I,J,K)=DVT(I,J,K)+DPDX*(V(IM1,J,L)+V(I,J,L))
      IF (LUP.EQ.L) GO TO 520
      L=L+1
      PDN=SP*SIGE(L)+PTOP
      GO TO 510
  520 IM1=I
c  ** POLAR VERTICAL MASS FLUX
      DO 560 K=KM,1,-1
      W(1,1,K)=0.
      IF (K.LT.KM) W(1,1,K)=W(1,1,K+1)
      W(1,JM,K)=0.
      IF (K.LT.KM) W(1,JM,K)=W(1,JM,K+1)
  530 DO 540 I=1,IM
      W(1,1,K)=W(1,1,K)-.5*DVT(I,2,K)
  540 W(1,JM,K)=W(1,JM,K)+.5*DVT(I,JM,K)
c  ** NON-POLAR VERTICAL MASS FLUX
      WUP=0.
      DO 560 J=2,JM-1
      IM1=IM
      DO 560 I=1,IM
      IF (K.LT.KM) WUP=W(I,J,K+1)
      W(I,J,K)=WUP+.5*(DUT(IM1,J,K)-DUT(I,J,K)+
     *  DVT(I,J,K)-DVT(I,J+1,K))
c  ** ACCUMULATE ALL VERTICAL WINDS
  560 IM1=I
      DO 565 J=1,JM
      IMAX=IM
      IF (J.EQ.1 .OR. J.EQ.JM) IMAX=1
      DO 565 K=1,KM
      WI=0.
      DO 562 I=1,IMAX
  562 WI=WI+W(I,J,K)
  565 AJK(J,K,25)=AJK(J,K,25)+WI
c  ** ZERO OUT SUBSURFACE VERTICAL WINDS
      DO 568 J=1,JM
      DO 568 I=1,IM
      PS=P(I,J)+PTOP
      K=2
  566 IF (PM(K+1).LT.PS) GO TO 568
      W(I,J,K)=0.
      K=K+1
      GO TO 566
  568 CONTINUE
c  **
c  ** ACCUMULATE T,Z,Q VERTICAL TRANSPORTS
c  **
      DO 610 J=1,JM
      IMAX=IM
      IF (J.EQ.1 .OR. J.EQ.JM) IMAX=1
      DO 610 K=2,KM
      WI=0.
      TKI=0.
      QKI=0.
      ZKI=0.
      WTI=0.
      WQI=0.
      WZI=0.
         THKI=0.
         WTHI=0.
      FIMI=0.
      DO 600 I=1,IMAX
      SP=P(I,J)
      IF (PM(K).GE.SP+PTOP) GO TO 600
      L=1
      IF (PM(K).GE.SP*SIG(1)+PTOP) GO TO 580
  570 LUP=L+1
      IF (L.EQ.LM) GO TO 580
      IF (PM(K).GE.SP*SIG(LUP)+PTOP) GO TO 575
      L=LUP
      GO TO 570
  575 DPUP=PM(K)-SP*SIG(LUP)-PTOP
      DPDN=SP*SIG(L)+PTOP-PM(K)
      BYDP=1./(DPDN+DPUP)
      TK=BYDP*(TX(I,J,L)*DPUP+TX(I,J,LUP)*DPDN)
      QK=Q(I,J,L)*Q(I,J,LUP)/(BYDP*(Q(I,J,L)*DPDN+Q(I,J,LUP)*DPUP)+
     *  ZERO20)
      ZK=BYDP*(PHI(I,J,L)*DPUP+PHI(I,J,LUP)*DPDN)
         THK=BYDP*(T(I,J,L)*DPUP+T(I,J,LUP)*DPDN)
      GO TO 590
c  ** SPECIAL CASES;  L=1, L=LM
  580 TK=TX(I,J,L)
      QK=Q(I,J,L)
      ZK=PHI(I,J,L)
         THK=T(I,J,L)
c  ** MERIDIONAL AVERAGING
  590 WI=WI+W(I,J,K)
      TKI=TKI+TK
      QKI=QKI+QK
      ZKI=ZKI+ZK
      WTI=WTI+W(I,J,K)*TK
      WQI=WQI+W(I,J,K)*QK
      WZI=WZI+W(I,J,K)*ZK
         THKI=THKI+THK
         WTHI=WTHI+W(I,J,K)*THK
      FIMI=FIMI+1.
  600 CONTINUE
      BYFIM=ZERO20
      IF (FIMI.GT.ZERO20) BYFIM=1./FIMI
      AJK(J,K-1,26)=AJK(J,K-1,26)+BYFIM*(SHA*TKI+ZKI)*WI
      AJK(J,K-1,27)=AJK(J,K-1,27)+SHA*WTI+WZI
      AJK(J,K-1,28)=AJK(J,K-1,28)+BYFIM*QKI*WI
      AJK(J,K-1,29)=AJK(J,K-1,29)+WQI
      AJK(J,K-1,30)=AJK(J,K-1,30)+WZI-BYFIM*WI*ZKI
c     AJK(J,K-1,31)=AJK(J,K-1,31)+WTI-BYFIM*WI*TKI
         WJK(J,K)=BYFIM*WI/DXYP(J)
         WTJK(J,K)=BYFIM*(WTHI-BYFIM*WI*THKI)/DXYP(J)
         AJK(J,K-1,50)=AJK(J,K-1,50)+WTJK(J,K)
  610 CONTINUE
c  **
c  ** BAROCLINIC EDDY KINETIC ENERGY GENERATION
c  **
      DO 630 J=1,JM
      IMAX=IM
      IF (J.EQ.1.OR.J.EQ.JM) IMAX=1
      DO 630 K=1,KM
      FIMI=0.
      W2I=0.
      PAI=0.
      WPA2I=0.
      DO 626 I=1,IMAX
      SP=P(I,J)
      PS=SP+PTOP
      IF (PM(K+1).GE.PS) GO TO 626
      L=1
      PDN=PS
      IF (PM(K).GE.PS) GO TO 614
      PDN=PM(K)
  612 IF (PM(K).GT.SP*SIGE(L+1)+PTOP) GO TO 614
      L=L+1
      GO TO 612
  614 LUP=L
  616 IF (PM(K+1).GE.SP*SIGE(LUP+1)+PTOP) GO TO 618
      LUP=LUP+1
      GO TO 616
  618 CONTINUE
      PTK=0.
c  ** INTERPOLATE HERE
  620 PUP=SP*SIGE(L+1)+PTOP
      IF (LUP.EQ.L) PUP=PM(K+1)
      DP=PDN-PUP
      PTK=PTK+DP*TX(I,J,L)
      IF (LUP.EQ.L) GO TO 622
      L=L+1
      PDN=SP*SIGE(L)+PTOP
      GO TO 620
c  ** ACCUMULATE HERE
  622 FIMI=FIMI+1.
      WUP=0.
      IF (K.LT.KM) WUP=W(I,J,K+1)
      W2I=W2I+W(I,J,K)+WUP
      PY=PMO(K)
      IF (PM(K).GE.PS) PY=.5*(PS+PM(K+1))
      PAK=PTK/PY
      PAI=PAI+PAK
      WPA2I=WPA2I+(W(I,J,K)+WUP)*PAK
  626 CONTINUE
  630 AJK(J,K,31)=AJK(J,K,31)-(WPA2I-W2I*PAI/(FIMI+ZERO20))
c  **
c  ** ACCUMULATE UV VERTICAL TRANSPORTS
c  **
c  ** DOUBLE POLAR WINDS
      DO 640 K=1,KM
      WSP=2.*W(1,1,K)/FIM
      WNP=2.*W(1,JM,K)/FIM
      DO 640 I=1,IM
      W(I,1,K)=WSP
  640 W(I,JM,K)=WNP
      DO 710 J=2,JM
      UEARTH=RADIUS*OMEGA*COSV(J)
      I=IM
      DO 650 IP1=1,IM
      PSEC(I)=.25*(P(I,J-1)+P(IP1,J-1)+P(I,J)+P(IP1,J))
  650 I=IP1
      DO 710 K=2,KM
      W4I=0.
      UKI=0.
      WU4I=0.
      WKE4I=0.
      FIMI=0.
      I=IM
      DO 700 IP1=1,IM
      SP=PSEC(I)
      IF (PM(K).GE.SP+PTOP) GO TO 700
      L=1
      IF (PM(K).GE.SP*SIG(1)+PTOP) GO TO 680
  670 LUP=L+1
      IF (L.EQ.LM) GO TO 680
      IF (PM(K).GE.SP*SIG(LUP)+PTOP) GO TO 675
      L=LUP
      GO TO 670
  675 DPUP=PM(K)-SP*SIG(LUP)-PTOP
      DPDN=SP*SIG(L)+PTOP-PM(K)
      BYDP=1./(DPDN+DPUP)
      UK=BYDP*(U(I,J,L)*DPUP+U(I,J,LUP)*DPDN)
      VK=BYDP*(V(I,J,L)*DPUP+V(I,J,LUP)*DPDN)
      GO TO 690
c  ** SPECIAL CASES;  L=1,L=LM
  680 UK=U(I,J,L)
      VK=V(I,J,L)
c  ** MERIDIONAL AVERAGING
  690 W4=W(I,J-1,K)+W(IP1,J-1,K)+W(I,J,K)+W(IP1,J,K)
      W4I=W4I+W4
      UKI=UKI+UK
      WU4I=WU4I+W4*UK
      WKE4I=WKE4I+W4*(UK*UK+VK*VK)
      FIMI=FIMI+1.
  700 I=IP1
      BYFIM=1./(FIMI+ZERO20)
         WUJK(J,K)=.25*(WU4I-W4I*UKI*BYFIM)*BYFIM/DXYV(J)
      AJK(J,K-1,36)=AJK(J,K-1,36)+WKE4I
      AJK(J,K-1,37)=AJK(J,K-1,37)+WU4I-BYFIM*W4I*UKI
  710 AJK(J,K-1,38)=AJK(J,K-1,38)+WU4I+W4I*UEARTH
c  **
c  ** POTENTIAL VORTICITY AND VERTICAL TRANSPORT OF POT. VORT.
c  **
      DO 760 J=2,JM-1
      JHEMI=1
      IF (J.LT.1+JM/2) JHEMI=-1
      DO 730 K=1,KM
      PVI=0.
      DO 720 I=1,IM
      DUT(I,J,K)=JHEMI*STB(I,J,K)*(ZX(I,J,K)-F(J))
  720 PVI=PVI+DUT(I,J,K)
  730 AJK(J,K,32)=AJK(J,K,32)+PVI
      DO 760 K=2,KM
      W2I=0.
      PV2I=0.
      WPV4I=0.
      FIMI=0.
      I=IM
      DO 740 IP1=1,IM
      PS=.5*(P(I,J)+P(IP1,J))+PTOP
      IF (PM(K).GE.PS) GO TO 740
      W2=W(I,J,K)+W(IP1,J,K)
      W2I=W2I+W2
      PV2=DUT(I,J,K-1)+DUT(I,J,K)
      PV2I=PV2I+PV2
      WPV4I=WPV4I+W2*PV2
      FIMI=FIMI+1.
  740 I=IP1
      AJK(J,K-1,33)=AJK(J,K-1,33)+WPV4I
  760 AJK(J,K-1,34)=AJK(J,K-1,34)+WPV4I-W2I*PV2I/(FIMI+ZERO20)
c  **
c  ** SPECIAL MEAN/EDDY DIAGNOSTICS ARE CALCULATED
c  **
      DO 770 J=2,JM
      DO 765 K=2,KM
      DPE=PMO(K)-PMO(K-1)
      UP(J,K)=(UJK(J,K)-UJK(J,K-1))/DPE
  765 PSIP(J,K)=(PSIJK(J,K)-PSIJK(J,K-1))/DPE
      UP(J,1)=UP(J,2)
      PSIP(J,1)=PSIP(J,2)
  770 CONTINUE
      DO 780 K=1,KM
      KUP=K+1
      IF (K.EQ.KM) KUP=KM
      KDN=K-1
      IF (K.EQ.1) KDN=1
      DO 780 J=2,JM
      TY(J,K)=(TJK(J,K)-TJK(J-1,K))/DYV(J)
c  ** E-P FLUX NORTHWARD COMPONENT
      AJK(J,K,44)=AJK(J,K,44)+PSIJK(J,K)*(UJK(J,KUP)-UJK(J,KDN))/
     *  (PMO(KUP)-PMO(KDN))-UVJK(J,K)
  780 CONTINUE
      DO 800 J=2,JM-1
      DO 800 K=2,KM-1
      UY=(UJK(J+1,K)*DXV(J+1)-UJK(J,K)*DXV(J)-F(J))/DXYP(J)
      PSIY=(PSIJK(J+1,K)*DXV(J+1)-PSIJK(J,K)*DXV(J))/DXYP(J)
c  ** ZONAL MEAN MOMENTUM EQUATION   (MEAN ADVECTION)
      AJK(J,K,40)=AJK(J,K,40)-.5*UY*(VJK(J,K)+VJK(J+1,K))-
     *  .25*((UP(J+1,K+1)+UP(J,K+1))*WJK(J,K+1)+(UP(J+1,K)+UP(J,K))*
     *   WJK(J,K))
c  ** ZONAL MEAN HEAT EQUATION   (MEAN ADVECTION)
      AJK(J,K,41)=AJK(J,K,41)-.5*(TY(J,K)*VJK(J,K)+TY(J+1,K)*VJK(J+1,K))
     *  -.5*STJK(J,K)*(WJK(J,K+1)+WJK(J,K))
c  ** LAGRANGIAN MEAN MOMENTUM EQUATION  (MEAN ADVECTION)
      VSTAR=.5*(VJK(J,K)+VJK(J+1,K)-.5*(PSIP(J,K)+PSIP(J,K+1)
     *  +PSIP(J+1,K)+PSIP(J+1,K+1)))
      WSTAR=.5*(WJK(J,K)+WJK(J,K+1))+PSIY
      AJK(J,K,42)=AJK(J,K,42)-UY*VSTAR-.25*(UP(J,K)+UP(J+1,K)+
     *  UP(J,K+1)+UP(J+1,K+1))*WSTAR
      AJK(J,K,43)=AJK(J,K,43)-.5*(TY(J+1,K)+TY(J,K))*VSTAR-
     *  STJK(J,K)*WSTAR
c  ** VERTICAL E-P FLUX
      AJK(J,K-1,45)=AJK(J,K-1,45)-WUJK(J,K)-.5*PSIJK(J,K)*UY
      AJK(J,K,45)=AJK(J,K,45)-.5*PSIJK(J,K)*UY
  800 CONTINUE
c  **
c  ** SPECTRAL ANALYSIS OF KINETIC ENERGIES AT CONSTANT PRESSURE
c  **
      IZERO=0
      NM=1+IM/2
      NM8=NM*8
      JEQ=1+JM/2
      JEQM1=JEQ-1
      J45N=2+.75*(JM-1)
      KS1=LS1
c  ** TOTAL THE KINETIC ENERGIES
      DO 2010 N=1,NM8
 2010 KE(N,1)=0.
      DO 2140 J=2,JM
      I=IM
      DO 2020 IP1=1,IM
      PSEC(I)=.25*(P(I,J-1)+P(IP1,J-1)+P(I,J)+P(IP1,J))
 2020 I=IP1
      DO 2140 K=1,KM
      KSPHER=2
      IF (K.GE.KS1) KSPHER=1
      IF (J.GT.JEQ) KSPHER=KSPHER+2
      DO 2140 KX=IZERO,LM,LM
      DO 2090 I=1,IM
      DPUV=0.
      SP=PSEC(I)
      PS=SP+PTOP
      IF (PM(K+1).GE.SP*SIG(1)+PTOP) GO TO 2090
      L=1
      PDN=PS
      IF (PM(K).GE.SP*SIG(1)+PTOP) GO TO 2040
      PDN=PM(K)
 2030 IF (PM(K).GT.SP*SIGE(L+1)+PTOP) GO TO 2040
      L=L+1
      GO TO 2030
 2040 LUP=L
 2050 IF (PM(K+1).GE.SP*SIGE(LUP+1)+PTOP) GO TO 2060
      LUP=LUP+1
      GO TO 2050
 2060 CONTINUE
c  ** ACCUMULATE HERE
      SQRTDP=SQRT(PDN-PM(K+1))
 2070 PUP=SP*SIGE(L+1)+PTOP
      IF (LUP.EQ.L) PUP=PM(K+1)
      DP=PDN-PUP
      DPUV=DPUV+DP*U(I,J,L+KX)
      IF (LUP.EQ.L) GO TO 2080
      L=L+1
      PDN=SP*SIGE(L)+PTOP
      GO TO 2070
 2080 IF (SQRTDP.EQ.0.) SQRTDP=ZERO20
      DPUV=DPUV/SQRTDP
 2090 X1(I)=DPUV
      CALL FRTR (X1)
      IF (J.EQ.JEQ) GO TO 2120
      DO 2100 N=1,NM
 2100 KE(N,KSPHER)=KE(N,KSPHER)+X1(N)*DXYV(J)
      IF (J.NE.J45N) GO TO 2140
      DO 2110 N=1,NM
 2110 KE(N,KSPHER+4)=KE(N,KSPHER+4)+X1(N)*DXYV(J)
      GO TO 2140
 2120 DO 2130 N=1,NM
      KE(N,KSPHER+4)=KE(N,KSPHER+4)+X1(N)*DXYV(J)
      KE(N,KSPHER)=KE(N,KSPHER)+.5D0*X1(N)*DXYV(J)
 2130 KE(N,KSPHER+2)=KE(N,KSPHER+2)+.5D0*X1(N)*DXYV(J)
 2140 CONTINUE
      DO 2150 KS=1,8
      DO 2150 N=1,NM
 2150 SPECA(N,18,KS)=SPECA(N,18,KS)+KE(N,KS)
c  ** ACCUMULATE TIME USED IN DIAGA
      CALL CLOCKS (MEND)
      MINC=MBEGIN-MEND
      MDIAG=MDIAG+MINC
      MDYN=MDYN-MINC
c     WRITE (6,997) IDACC,MINC,MDIAG
      RETURN
  997 FORMAT (' DIAGNOSTICS ACCUMULATED ',12I4,15X,2I7)
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE DIAGJ
c  **
c  ** THIS SUBROUTINE PRODUCES AREA WEIGHTED STATISTICS OF
c  **
c   K   N
c  **
c   1   1  SOLAR RADIATION INCIDENT ON PLANET (W/M**2)
c  **
c  1A   2/1  PLANETARY ALBEDO (10**-2)
c  1B  72/1  PLANETARY ALBEDO VISUAL (10**-2)
c  1C  73/1  PLANETARY ALBEDO NEAR IR (10**-2)
c  1D   6/5  GROUND ALBEDO (10**-2)
c  1E  74/1  GROUND ALBEDO VISUAL (10**-2)
c  1F  75/1  GROUND ALBEDO NEAR IR (10**-2)
c  1G  76/1  ATMOSPHERIC ALBEDO VISUAL (10**-2)
c  1H  77/1  ATMOSPHERIC ALBEDO NEAR IR (10**-2)
c  1I  78/1  ATMOSPHERIC ABSORPTION VISUAL (10**-2)
c  1J  79/1  ATMOSPHERIC ABSORPTION NEAR IR (10**-2)
c  **
c   2   2  SOLAR RADIATION ABSORBED BY PLANET (W/M**2)
c   3   3  SOLAR RADIATION ABSORBED BELOW PTOP (W/M**2)
c   4   4  SOLAR RADIATION ABSORBED BY ATMOSPHERE (W/M**2)
c   5   5  SOLAR RADIATION INCIDENT ON GROUND (W/M**2)
c   6   6  SOLAR RADIATION ABSORBED BY GROUND (W/M**2)
c   7  32  SOLAR RADIATION WATER CORRECTION
c   8   7  THERMAL RADIATION EMITTED BY PLANET (W/M**2)
c   9   8  THERMAL RADIATION AT PTOP (W/M**2)
c  10   9  THERMAL RADIATION EMITTED BY GROUND (W/M**2)
c  **
c  11  67  THERMAL RADIATION INCIDENT ON GROUND (W/M**2)
c  **  55  BRIGHTNESS TEMPERATURE THROUGH WINDOW REGION (K-273.16)
c  **  10  NET RADIATION ABSORBED BY PLANET (W/M**2)
c  **  11  NET RADIATION ABSORBED BELOW PTOP (W/M**2)
c  **  12  NET RADIATION ABSORBED BY GROUND (W/M**2)
c  **  13  SENSIBLE HEAT FLUX INTO THE GROUND (W/M**2)
c  **  14  EVAPORATION HEAT FLUX INTO THE GROUND (W/M**2)
c  **  39  PRECIPITATION HEAT FLUX INTO THE GROUND (W/M**2)
c  **  40  HEAT RUNOFF FROM FIRST GROUND LAYER (W/M**2)
c  **  44  NET HEATING AT Z0 (W/M**2)
c  **
c  21  42  CONDUCTION AT -Z1 (W/M**2)
c  **  41  HEAT OF WATER OR ICE DUFFUSION AT -Z1 (W/M**2)
c  **  16  NET HEATING AT -Z1 (W/M**2)
c  **  15  CONDUCTION AT -Z1-Z2 (W/M**2)
c  **  43  ENERGY OF ICE MELTING (OR TRANSPORTING) AT -Z1-Z2 (W/M**2)
c  **  56  NET HEATING AT -Z1-Z2 (W/M**2)
c  **  33  OCEAN TRANSPORT (W/M**2)
c  **  48  HEAT RUNOFF THROUGH THE MIXED LAYER DEPTH (W/M**2)
c  **  68  ENERGY DIFFUSION INTO THE THERMOCLINE (W/M**2)
c  **  18  MEAN TEMPERATURE OF FIRST GROUND LAYER (.1 K-273.16)
c  **
c  31  17  MEAN TEMPERATURE OF SECOND GROUND LAYER (.1 K-273.16)
c  **  34  OCEAN TEMPERATURE AT THE MAXIMUM MIXED LAYER DEPTH
c  **  23  SURFACE AIR TEMPERATURE (.1 K-273.16)
c  **  22  FIRST LAYER AIR TEMPERATURE (.1 K-273.16)
c  **  21  COMPOSITE AIR TEMPERATURE (.1 K-273.16)
c  **  35  STRATO TEMPERATURE CHANGE PER DEGREE LATITUDE (10**-2 K)
c  **  36  TROPO TEMPERATURE CHANGE PER DEGREE LATITUDE (10**-2 K)
c  **  24  STRATOSPHERIC STATIC STABILITY (10**-3 K/M)
c  **  25  TROPOSPHERIC STATIC STABILITY (10**-3 K/M)
c  **  26  STRATOSPHERIC RICHARDSON NUMBER (1)
c  **
c  41  27  TROPOSPHERIC RICHARDSON NUMBER (1)
c  **  28  STRATOSPHERIC ROSSBY NUMBER (1)
c  **  29  TROPOSPHERIC ROSSBY NUMBER (1)
c  **  37  L IN THE STRATOSPHERE (10**5 M)
c  **  38  L IN THE TROPOSPHERE (10**5 M)
c  **  64  GAM  (10**-3 K/M)
c  **  65  GAMM  (10**-3 K/M)
c  **  66  GAMC  (10**-3 K/M)
c  **  57  INTEGRATED SUPER-SATURATION CLOUD COVER (10**-2)
c  **  58  INTEGRATED MOIST CONVECTIVE CLOUD COVER (10**-2)
c  **
c  51  59  INTEGRATED TOTAL CLOUD COVER (10**-2)
c  **  60  MOIST CONVECTIVE CLOUD DEPTH (100 N)
c  **  61  SUPER SATURATION PRECIPITATION (KG/M**2/86400 S)
c  **  62  MOIST CONVECTIVE PRECIPITATION (KG/M**2/86400 S)
c  **  20  PRECIPITATION (KG/M**2/86400 S)
c  **  19  EVAPORATION (KG/M**2/86400 S)
c  **  63  WATER CONTENT OF ATMOSPHERE (KG/M**2)
c  **  54  WATER RUNOFF AT Z0 (KG/M**2/86400 S)
c  **  45  WATER OR ICE DIFFUSION AT -Z1 (KG/M**2/86400 S)
c  **  46  ICE MELTING (OR TRANSPORTING) AT -Z1-Z2 (KG/M**2/86400 S)
c  **
c  61  47  WATER RUNOFF THROUGH MIXED LAYER DEPTH (KG/M**2/86400 S)
c  **  49  WATER CONTAINED IN FIRST GROUND LAYER (KG/M**2)
c  **  50  ICE CONTAINED IN FIRST GROUND LAYER (KG/M**2)
c  **  51  WATER CONTAINED IN SECOND GROUND LAYER (KG/M**2)
c  **  52  ICE CONTAINED IN SECOND GROUND LAYER (KG/M**2)
c  **  53  SNOW DEPTH (KG/M**2)
c  **  31  SNOW COVER (10**-2)
c  68  30  OCEAN ICE COVER (10**-2)
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      DIMENSION ABCJ(JM,80,3)
      EQUIVALENCE (AJ(1,1),ABCJ(1,1,1))
      DIMENSION JLAT(JM),S1(JM),SPOCEN(JM),SPOICE(JM),SPLAND(JM),
     *  SAREA(23+1),MLAT(JM),MLATR(23),FLAT(JM),FLATR(23),
     *  MHEM(2),FHEM(2),WTA(4),WTB(4),WTC(4),
     *  NDEX(70),INNUM(10),INDEN(10),IA(72),SCALE(72)
      EQUIVALENCE (MLAT(1),MLATR(1)),(FLAT(1),FLATR(1))
      CHARACTER*16 TERAIN(5)/'    (GLOBAL)','     (LAND)','    (OCEAN)',
     *  ' (OCEAN ICE)','   (REGIONS)'/
c 
C**** Save data for post-processing in BUDG
      include 'pd_COMMON'
      CHARACTER*16 TITLEB(78)   ! dimension is KD1M+10
      REAL*4 BUDG(JM+3,78)
      CHARACTER*53 details(5)/ 
     *  'Global Averages (land, ocean, and ice grid cells)',
     *  'Land Averages (land grid cells only)', 
     *  'Ocean Averages (ocean grid cells only)',
     *  'Ocean Ice Averages (ocean ice grid cells only)',
     *  'Regional Averages'/
      character*16 units(72),unitsa(10),unitsb(82)
c
      CHARACTER*16 TITLE(72),TITLE1(36),TITLE2(36),TITLEA(10)
      CHARACTER*4 TITREG*80,NAMREG(2,23)
      COMMON/TNKREG/TITREG,NAMREG,KREG
      EQUIVALENCE (TITLE(1),TITLE1(1)),(TITLE(37),TITLE2(1))
                         DATA TITLE1/
     1  ' INC SW(WT/M**2)', '0SW ABS BELOW P0', ' SW ABS BELOW P1',
     4  ' SW ABS BY ATMOS', ' SW INC ON Z0   ', ' SW ABS AT Z0   ',
     7  '0NET LW AT P0   ', ' NET LW AT P1   ', ' NET LW AT Z0  ',
     O  '0NET RAD AT P0  ', ' NET RAD AT P1  ', ' NET RAD AT Z0  ',
     3  '0SENSBL HEAT FLX', ' EVAPOR HEAT FLX', '0CONDC AT -Z1-Z2',
     6  ' NET HEAT AT -Z1', ' TG2 (.1 C)     ', '0TG1 (.1 C)     ',
     9  ' EVAPOR (MM/DAY)', ' PRECIP (MM/DAY)', ' T AIR (.1 C)   ',
     2  ' T1 (.1 C)      ', '0T SURF (.1 C)  ', '0STAT STB(STRAT)',
     5  ' STAT STB(TROPO)', '0RICH NUM(STRAT)', ' RICH NUM(TROPO)',
     8  ' ROSS NUM(STRAT)', ' ROSS NUM(TROPO)', ' OCEAN ICE COVER',
     1  '0SNOW COVER     ', ' SW CORRECTION  ', '0OCEAN TRANSPORT',
     4  ' TG3 (.1 C)     ', '1DT/DLAT(STRAT) ', ' DT/DLAT(TROPO) '/
                         DATA TITLE2/
     7  ' L(STRAT)(10**5)', ' L(TROP) (10**5)', ' PRECIP HEAT FLX',
     O  ' HEAT RUNOFF Z0 ', ' HT WTR DIFS -Z1', '0CONDUCTN AT -Z1',
     3  ' ICE ENRG -Z1-Z2', ' NET HEAT AT Z0 ', ' H2O DIFS AT -Z1',
     6  ' ICE THRU -Z1-Z2', ' WATR RUNOFF MLD', ' HEAT RUNOFF MLD',
     9  '0WATER IN G1    ', ' ICE IN G1      ', ' WATER IN G2    ',
     2  ' ICE IN G2      ', ' SNOW DEPTH     ', '0WATER RUNOFF Z0',
     5  ' LW WINDOW BTEMP', ' NET HEAT -Z1-Z2', '0TOT SUP SAT CLD',
     8  ' TOT MST CNV CLD', ' TOTAL CLD COVER', ' MC CLD DPTH(MB)',
     *  '0SS PRECIP(MM/D)', ' MC PRECIP(MM/D)', ' H2O OF ATM (MM)',
     4  '0GAM(K/KM)      ', ' GAMM(K/KM)     ', ' GAMC(K/KM)     ',
     *  ' LW INC ON Z0   ', ' HT INTO THRMOCL', 4*' '/
      DATA TITLEA/' PLANETARY ALBDO',' PLAN ALB VISUAL',
     *  ' PLAN ALB NEARIR', ' SURFACE G ALBDO', ' SURF ALB VISUAL',
     *  ' SURF ALB NEARIR', '0ATMO ALB VISUAL', ' ATMO ALB NEARIR',
     *  ' ATMO ABS VISUAL', ' ATMO ABS NEARIR'/
                         DATA units/
     1  ' W/m**2         ', ' W/m**2         ', ' W/m**2         ',
     4  ' W/m**2         ', ' W/m**2         ', ' W/m**2         ',
     7  ' W/m**2         ', ' W/m**2         ', ' W/m**2         ',
     O  ' W/m**2         ', ' W/m**2         ', ' W/m**2         ',
     3  ' W/m**2         ', ' W/m**2         ', ' W/m**w         ',
     6  ' W/m**2         ', ' 0.1_Degrees_C  ', ' 0.1_Degrees_C  ',
     9  ' mm/day         ', ' mm/day         ', ' 0.1_Degrees_C  ',
     2  ' 0.1_Degrees_C  ', ' 0.1_Degrees_C  ', ' deg/km         ',
     5  ' deg/km         ', ' 1              ', ' 1              ',
     8  ' 1              ', ' 1              ', ' percent        ',
     1  ' percent        ', ' W/m**2         ', ' W/m**2         ',
     4  ' 0.1_Degrees_C  ', ' deg.C/deg.lat. ', ' deg.C/deg.lat. ',
c 
     7  ' 10**5_m        ', ' 10**5_m        ', ' W/m**2         ',
     O  ' W/m**2         ', ' W/m**2         ', ' W/m**2         ',
     3  ' W/m**2         ', ' W/m**2         ', ' mm/day         ',
     6  ' mm/day         ', ' mm/day         ', ' W/m**2         ',
     9  ' mm             ', ' mm             ', ' mm             ',
     2  ' mm             ', ' mm             ', ' mm/day         ',
     5  ' Degrees_C      ', ' W/m**2         ', ' percent        ',
     8  ' percent        ', ' percent        ', ' mb             ',
     *  ' mm/day         ', ' mm/day         ', ' mm             ',
     4  ' K/km           ', ' K/km           ', ' K/km           ',
     *  ' W/m**2         ', ' W/m**2         ', 4*' '/
      DATA unitsA/' percent        ',' percent        ',   !albedos
     *  ' percent        ', ' percent        ', ' percent        ',
     *  ' percent        ', ' percent        ', ' percent        ',
     *  ' W/m**2         ', ' W/m**2         '/
c  **
      DATA WTA/1.,0.,1.,0./, WTB/1.,1.,0.,0./, WTC/1.,0.,0.,1./
      DATA NDEX/1,2,3,4,5,6,32,7,8,9,  67,55,10,11,12,13,14,39,40,44,
     *  42,41,16,15,43,56,33,48,68,18,  17,34,23,22,21,35,36,24,25,26,
     *  27,28,29,37,38,64,65,66,57,58,  59,60,61,62,20,19,63,54,45,46,
     *  47,49,50,51,52,53,31,30,2*0/
      DATA INNUM/2,72,73,6,74,75,76,77,78,79/, INDEN/3*1,5,6*1/
c  ** IA: 1 CONDENSATION, 2 RADIATION, 3 SURFACE, 4 DIAGA, 0 UNUSED
      DATA IA/6*2,  2,2,1,2,2,1,  6*1,  1,1,4,4,3,4,  5*4,1,
     *  4,2,1,1,4,4,  4,4,4*1,  6*1,  6*1,  2,1,4*2,  1,1,4*4,  2,9,4*0/
      DATA SCALE/6*1.,  6*1.,  4*1.,2*10.,  2*1.,4*10.,  6*100.,
     *  100.,2*1.,10.,2*100.,  6*1.,  6*1.,  6*1.,  2*1.,3*100.,1.,
     *  6*1.,  6*1./
      DATA IFIRST/1/,P1000/1000./
      IF (IFIRST.NE.1) GO TO 100
      IFIRST=0
c  ** INITIALIZE CERTAIN QUANTITIES  (KD1M LE 69)
      KD1M=68
      INC=1+(JM-1)/24
      DTSRCE=DT*NDYN
      DTCNDS=DT*NCNDS
      JMHALF=JM/2
      DO 10 JR=1,24
   10 SAREA(JR)=0.
      DO 30 J=1,JM
      S1(J)=IM
      SPLAND(J)=0.
      DO 20 I=1,IM
      SPLAND(J)=SPLAND(J)+FDATA(I,J,2)
      JR=JREG(I,J)
   20 SAREA(JR)=SAREA(JR)+DXYP(J)
   30 JLAT(J)=INT(LAT(J)*360./TWOPI+100.5)-100
      S1(1)=1.
      S1(JM)=1.
      SPLAND(1)=FDATA(1,1,2)
      SPLAND(JM)=FDATA(1,JM,2)
      SCALE(9)=1./DTSRCE
      SCALE(12)=1./DTSRCE
      SCALE(13)=1./DTSRCE
      SCALE(14)=1./DTSRCE
      SCALE(15)=1./DTSRCE
      SCALE(16)=1./DTSRCE
      SCALE(19)=SDAY/DTSRCE
      SCALE(20)=100.*SDAY/(DTCNDS*GRAV)
      SCALE(24)=1.D3*GRAV*EXPBYK(P1000)
      SCALE(25)=SCALE(24)
      SCALE(26)=16.*RGAS
      SCALE(27)=16.*RGAS
      SCALE(28)=.5/(2.*OMEGA*FIM)
      SCALE(29)=.5/(2.*OMEGA*FIM)
      SCALE(33)=1./DTSRCE
      SCALE(35)=.5D2*(JM-1)/((SIGE(LS1)-SIGE(LM+1)+1.D-12)*180.)
      SCALE(36)=.5E2*(JM-1)/((SIGE(1)-SIGE(LS1))*180.)
      SCALE(37)=1.D-5*SQRT(RGAS)/(2.*OMEGA)
      SCALE(38)=SCALE(37)
      SCALE(39)=1./DTSRCE
      SCALE(40)=1./DTSRCE
      SCALE(41)=1./DTSRCE
      SCALE(42)=1./DTSRCE
      SCALE(43)=1./DTSRCE
      SCALE(44)=1./DTSRCE
      SCALE(45)=SDAY/DTSRCE
      SCALE(46)=SDAY/DTSRCE
      SCALE(47)=SDAY/DTSRCE
      SCALE(48)=1./DTSRCE
      SCALE(54)=SDAY/DTSRCE
      SCALE(56)=1./DTSRCE
      SCALE(61)=SCALE(20)
      SCALE(62)=SCALE(20)
      SCALE(63)=100./GRAV
      SCALE(64)=1.D3*GRAV
      SCALE(65)=1.D3*.0098/(SIGE(1)-SIGE(LTM+1))
      SCALE(66)=1.D3
      SCALE(68)=2.E3*4185./SDAY
c  ** CALCULATE THE DERIVED QUANTITIES
  100 BYA1=1./(IDACC(1)+1.D-20)
      A2BYA1=DFLOAT(IDACC(2))/DFLOAT(IDACC(1))
      A1BYA2=IDACC(1)/(IDACC(2)+1.D-20)
      DO 200 JR=1,23
      DJ(JR,4)=DJ(JR,2)-DJ(JR,6)
      DJ(JR,7)=DJ(JR,70)+A2BYA1*DJ(JR,9)/DTSRCE
      DJ(JR,8)=DJ(JR,71)+A2BYA1*DJ(JR,9)/DTSRCE
      DJ(JR,10)=DJ(JR,2)+DJ(JR,7)
      DJ(JR,11)=DJ(JR,3)+DJ(JR,8)
      DJ(JR,12)=A1BYA2*DJ(JR,6)*DTSRCE+DJ(JR,9)
      DJ(JR,16)=DJ(JR,41)+DJ(JR,42)
      DJ(JR,20)=DJ(JR,61)+DJ(JR,62)
      DJ(JR,44)=DJ(JR,12)+DJ(JR,13)+DJ(JR,14)+DJ(JR,39)-DJ(JR,40)
      DJ(JR,56)=DJ(JR,15)+DJ(JR,43)
  200 DJ(JR,60)=IDACC(2)*SAREA(JR)*DJ(JR,80)/(DJ(JR,58)+1.D-20)
      DO 210 J=1,JM
      SPOICE(J)=CJ(J,30)*BYA1
      SPOCEN(J)=S1(J)-SPLAND(J)-SPOICE(J)
      AJ(J,32)=(1.-SRCOR)*AJ(J,6)
      CJ(J,32)=(1.-SRCOR)*CJ(J,6)
      AJ(J,60)=IDACC(2)*SPOCEN(J)*AJ(J,80)/(AJ(J,58)+1.D-20)
      BJ(J,60)=IDACC(2)*SPLAND(J)*BJ(J,80)/(BJ(J,58)+1.D-20)
      CJ(J,60)=IDACC(2)*SPOICE(J)*CJ(J,80)/(CJ(J,58)+1.D-20)
      DO 210 M=1,3
      ABCJ(J,4,M)=ABCJ(J,2,M)-ABCJ(J,6,M)
      ABCJ(J,7,M)=ABCJ(J,70,M)+A2BYA1*ABCJ(J,9,M)/DTSRCE
      ABCJ(J,8,M)=ABCJ(J,71,M)+A2BYA1*ABCJ(J,9,M)/DTSRCE
      ABCJ(J,10,M)=ABCJ(J,2,M)+ABCJ(J,7,M)
      ABCJ(J,11,M)=ABCJ(J,3,M)+ABCJ(J,8,M)
      ABCJ(J,12,M)=A1BYA2*ABCJ(J,6,M)*DTSRCE+ABCJ(J,9,M)
      ABCJ(J,16,M)=ABCJ(J,41,M)+ABCJ(J,42,M)
      ABCJ(J,20,M)=ABCJ(J,61,M)+ABCJ(J,62,M)
      ABCJ(J,44,M)=ABCJ(J,12,M)+ABCJ(J,13,M)+ABCJ(J,14,M)
     *  +ABCJ(J,39,M)-ABCJ(J,40,M)
      ABCJ(J,56,M)=ABCJ(J,15,M)+ABCJ(J,43,M)
  210 CONTINUE
      IHOUR0=TOFDY0+.5
      IHOUR=TOFDAY+.5
      TAUDIF=TAU-TAU0
c  **
c  ** LOOP OVER SURFACE TYPES: GLOBAL, LAND, OCEAN, AND OCEAN ICE
c  **
      if (iprint_pd(1).lt.9) then
        idid_titles = 0
        iu = iu_pd(1)
        write(iu,'(a)') ' Budget Pages'
        kx = index(clabel(1:),' ')
        ko = index(clabel(1:),'(')
        kc = max(index(clabel(1:),')'),80)
        write(iu,'(2a)')' Run Name: ',clabel(1:kx-1)
        write(iu,'(2a)')' Run Description: ',clabel(ko+1:kc-1)
        write(iu,'(a,i3,a5,i4,a,i3,a5,i4)') ' Model Dates: ',
     *      JDATE0,JMNTH0,JYEAR0,' TO', JDATE,JMONTH,JYEAR
        write(iu,'(a,3i5)') 
     *     ' Model Resolution (im, jm, lm): ',im,jm,lm
        write(iu,'(a,i5)') ' Number of Variables: ',kd1m+10
      end if
      IF (KDIAG(1).GT.7) GO TO 510
      M1=1
      NSTPDJ=1
      IF (KDIAG(1).GT.4) M1=KDIAG(1)-3
      IF (KDIAG(1).GT.3) NSTPDJ=4
      IF (KDIAG(1).EQ.3) NSTPDJ=2
      DO 500 M=M1,4,NSTPDJ
      IF (KDIAG(1).EQ.2.AND.M.GT.2) RETURN
      WRITE (6,901) XLABEL
      WRITE (6,902) TERAIN(M),IDAY0,IHOUR0,JDATE0,JMNTH0,JYEAR0,TAU0,
     *  IDAY,IHOUR,JDATE,JMONTH,JYEAR,TAU,TAUDIF
      WRITE (6,903) (NINT(LAT_DG(J,1)),J=JM,INC,-INC)
      WRITE (6,905)
      DO 490 K=1,KD1M
      N=NDEX(K)
      IACC=IDACC(IA(N))
      GSUM=0.
      GWT=0.
      DO 320 JHEMI=1,2
      HSUM=0.
      HWT=0.
      DO 310 JH=1,JMHALF
      J=(JHEMI-1)*JMHALF+JH
      QJ=(AJ(J,N)*WTA(M)+BJ(J,N)*WTB(M)+CJ(J,N)*WTC(M))*SCALE(N)
      WTJ=(SPOCEN (J)*WTA(M)+SPLAND(J)*WTB(M)+SPOICE(J)*WTC(M))*IACC
      FLAT(J)=QJ/(WTJ+1.D-20)
      MLAT(J)=NINT(FLAT(J))
      HSUM=HSUM+QJ*DXYP(J)*(FIM+1.-S1(J))
  310 HWT=HWT+WTJ*DXYP(J)*(FIM+1.-S1(J))
      FHEM(JHEMI)=HSUM/(HWT+1.D-20)
      GSUM=GSUM+HSUM
  320 GWT=GWT+HWT
      FGLOB=GSUM/(GWT+1.D-20)
         IF (M.EQ.1) CALL KEYDJ (N,FGLOB,FHEM(2))
C**** Save BUDG for full output
        do j=1,jm
          budg(j,k)=flat(j)
        end do
        budg(jm+1,k)=fhem(1)
        budg(jm+2,k)=fhem(2)
        budg(jm+3,k)=fglob
        titleb(k) = ' '
        titleb(k)(1:15)=title(n)(2:16)
        unitsB(K)=units(n)
      GO TO (350,350,350,350,350,350,  350,350,350,350,350,350,
     *       350,350,348,348,350,350,  345,345,350,350,350,350,
     *       340,350,350,345,345,350,  350,340,348,350,350,350,
     *       345,345,348,345,348,348,  348,348,345,345,345,345,
     *       350,350,350,350,350,345,  350,348,350,350,350,350,
     *       345,345,350,345,345,345,  350,345,350,350,350,350),N
  340 WRITE (6,906) TITLE(N),FGLOB,FHEM(2),FHEM(1),
     *  (FLAT(J),J=JM,INC,-INC)
      GO TO 490
  345 WRITE (6,911) TITLE(N),FGLOB,FHEM(2),FHEM(1),
     *  (FLAT(J),J=JM,INC,-INC)
      GO TO 490
  348 WRITE (6,912) TITLE(N),FGLOB,FHEM(2),FHEM(1),
     *  (MLAT(J),J=JM,INC,-INC)
      GO TO 490
  350 WRITE (6,907) TITLE(N),FGLOB,FHEM(2),FHEM(1),
     *  (MLAT(J),J=JM,INC,-INC)
      IF (N.NE.1) GO TO 490
c  ** CALCULATE AND PRINT ALBEDOS
  400 DO 430 KA=1,10
      NN=INNUM(KA)
      ND=INDEN(KA)
      AMULT=1.
      IF (KA.LE.1.OR.KA.EQ.4) AMULT=-1.
      GSUM=0.
      GSUM2=0.
      DO 420 JHEMI=1,2
      HSUM=0.
      HSUM2=0.
      DO 410 JH=1,JMHALF
      J=(JHEMI-1)*JMHALF+JH
      QNUM=AJ(J,NN)*WTA(M)+BJ(J,NN)*WTB(M)+CJ(J,NN)*WTC(M)
      QDEN=AJ(J,ND)*WTA(M)+BJ(J,ND)*WTB(M)+CJ(J,ND)*WTC(M)
      FLAT(J)=AMULT*(100.*     QNUM/(QDEN    +1.D-20)-50.)+50.
      MLAT(J)=FLAT(J)+.5
      HSUM=HSUM+QNUM*DXYP(J)*(FIM+1.-S1(J))
  410 HSUM2=HSUM2+QDEN*DXYP(J)*(FIM+1.-S1(J))
      FHEM(JHEMI)=50.+AMULT*(100.*HSUM/(HSUM2+1.D-20)-50.)
      GSUM=GSUM+HSUM
  420 GSUM2=GSUM2+HSUM2
      FGLOB=50.+AMULT*(100.*GSUM/(GSUM2+1.D-20)-50.)
         IF (M.EQ.1.AND.KA.EQ.1) CALL KEYDJA (FGLOB)
C**** Save BUDG for full output
        do j=1,jm
          budg(j,kd1m+ka)=flat(j)
        end do
        budg(jm+1,kd1m+ka)=fhem(1)
        budg(jm+2,kd1m+ka)=fhem(2)
        budg(jm+3,kd1m+ka)=fglob
        titleb(kd1m+ka) = ' '
        titleb(kd1m+ka)(1:15)=titlea(ka)(2:16)
        unitsb(kd1m+ka)=unitsa(ka)
      WRITE (6,912) TITLEA(KA),FGLOB,FHEM(2),FHEM(1),
     *  (MLAT(J),J=JM,INC,-INC)
  430 CONTINUE
  490 CONTINUE
      WRITE (6,903) (NINT(LAT_DG(J,1)),J=JM,INC,-INC)
      WRITE (6,905)
      if (iprint_pd(1).lt.9) then
        do k=1,kd1m+10 !Dress up the titles (EASIER IN Fortran90)
          ilen=16      !Find last non-blank character
          do i=16,2,-1
          if (titleb(k)(i:i).ne.' ') then
            ilen = i
            go to 1501
          end if
          end do
 1501 continue
          do i=2,ilen  !Place underscores in embedded blanks
            if (titleb(k)(i:i).eq.' ') titleb(k)(i:i)='_'
          end do
        end do
        if (idid_titles.eq.0) then
        write(iu,'(2a,99(a16,a))')' Latitude ',char(9),
     *    (titleb(k),char(9),k=1,KD1M+10)
        write(iu,'(2a,99(a16,a))')' Units    ',char(9),
     *    (unitsb(k),char(9),k=1,KD1M+10)
        idid_titles = 1
        end if
        write(iu,'(a)') ' '  !a blank line
        write(iu,'(2a)') ' ', details(m)
        do j=jm,1,-1
          write(iu,'(f10.2,a,99(e12.4,a))')
     *    lat_dg(j,1),char(9),(BUDG(j,k),char(9),k=1,kd1m+10)
        end do
        write(iu,'(2a,99(e12.4,a))')
     *   ' Global   ',char(9),(BUDG(jm+3,k),char(9),k=1,kd1m+10)
        write(iu,'(2a,99(e12.4,a))')
     *   ' N.Hem.   ',char(9),(BUDG(jm+2,k),char(9),k=1,kd1m+10)
        write(iu,'(2a,99(e12.4,a))')
     *   ' S.Hem.   ',char(9),(BUDG(jm+1,k),char(9),k=1,kd1m+10)
      end if
      IF (KDIAG(1).GT.3) RETURN
  500 CONTINUE
  510 IF (KDIAG(1).GT.0.AND.KDIAG(1).NE.8) RETURN
c  **
c  ** PRODUCE REGIONAL STATISTICS
c  **
      if (iprint_pd(1).lt.9) then
        iu = iu_pd(1)+12
        write(iu,'(a)') ' Budget Pages'
        kx = index(clabel(1:),' ')
        ko = index(clabel(1:),'(')
        kc = max(index(clabel(1:),')'),80)
        write(iu,'(2a)')' Run Name: ',clabel(1:kx-1)
        write(iu,'(2a)')' Run Description: ',clabel(ko+1:kc-1)
        write(iu,'(a,i3,a5,i4,a,i3,a5,i4)') ' Model Dates: ',
     *      JDATE0,JMNTH0,JYEAR0,' TO', JDATE,JMONTH,JYEAR
        write(iu,'(a,3i5)') 
     *     ' Model Resolution (im, jm, lm): ',im,jm,lm
        write(iu,'(a,i5)') ' Number of Variables: ',kd1m+10
        write(iu,'(2a,99(a16,a))')' Region   ',char(9),
     *     (titleb(k),char(9),k=1,KD1M+10)
        write(iu,'(2a,99(a16,a))')' Units    ',char(9),
     *     (unitsb(k),char(9),k=1,KD1M+10)
      end if
      WRITE (6,901) XLABEL
      WRITE (6,902) TERAIN(5),IDAY0,IHOUR0,JDATE0,JMNTH0,JYEAR0,TAU0,
     *  IDAY,IHOUR,JDATE,JMONTH,JYEAR,TAU,TAUDIF
      IF (KREG.EQ.0) WRITE (6,908)
      IF(KREG.EQ.1)WRITE(6,918)(NAMREG(1,K),K=1,23),(NAMREG(2,K),K=1,23)
      DO 700 K=1,KD1M
      N=NDEX(K)
      BYIACC=1./(IDACC(IA(N))+1.D-20)
      DO 520 JR=1,23
      FLAT(JR)=DJ(JR,N)*SCALE(N)*BYIACC/SAREA(JR)
C**** Save BUDG for full output
        budg(jr,k)=flat(jr)
  520 MLAT(JR)=NINT(FLAT(JR))
        titleb(k) = ' '
        titleb(k)(1:15)=title(n)(2:16)
        unitsB(k)=units(n)
      GO TO (550,550,550,550,550,550,  550,550,550,550,550,550,
     *       550,550,550,550,550,550,  540,540,550,550,550,550,
     *       540,550,550,540,540,540,  540,540,550,550,550,550,
     *       540,540,550,540,550,550,  550,550,540,540,540,540,
     *       540,540,550,550,540,540,  550,550,550,550,550,550,
     *       540,540,540,540,540,540,  550,540,550,550,550,550),N
  540 WRITE (6,910) TITLE(N),(FLAT(JR),JR=1,23)
      GO TO 700
  550 WRITE (6,909) TITLE(N),(MLAT(JR),JR=1,23)
      IF (N.NE.1) GO TO 700
      GO TO 600
c  ** CALCULATE AND PRINT ALBEDOS FOR REGIONAL STATISTICS
  600 DO 620 KA=1,10
      NN=INNUM(KA)
      ND=INDEN(KA)
      AMULT=1.
      IF (KA.LE.1.OR.KA.EQ.4) AMULT=-1.
      DO 610 JR=1,23
      FLAT(JR)=AMULT*(100.*DJ(JR,NN)/(DJ(JR,ND)+1.D-20)-50.)+50.
C**** Save BUDG for full output
        budg(jr,kd1m+ka)=flat(jr)
  610 MLAT(JR)=FLAT(JR)+.5
        titleb(kd1m+ka) = ' '
        titleb(kd1m+ka)(1:15)=titlea(ka)(2:16)
        unitsb(kd1m+ka)=unitsa(ka)
  620 WRITE (6,909) TITLEA(KA),(MLAT(JR),JR=1,23)
  700 CONTINUE
      WRITE (6,905)
      IF (KREG.EQ.0) WRITE (6,908)
      IF(KREG.EQ.1)WRITE(6,918)(NAMREG(1,K),K=1,23),(NAMREG(2,K),K=1,23)
      if (iprint_pd(1).lt.9) then
        do k=1,kd1m+10 !Dress up the titles (EASIER IN Fortran90)
          ilen=16      !Find last non-blank character
          do i=16,2,-1
          if (titleb(k)(i:i).ne.' ') then
            ilen = i
            go to 1601
          end if
          end do
 1601 continue
          do i=2,ilen  !Place underscores in embedded blanks
            if (titleb(k)(i:i).eq.' ') titleb(k)(i:i)='_'
          end do
        end do
        write(iu,'(a)') ' '  !a blank line
        write(iu,'(2a)') ' ', details(5)
        DO JR=1,23
          do i=1,4   ! put underscores in titles
            if (namreg(1,jr)(i:i).eq.' ') namreg(1,jr) = '_'
            if (namreg(2,jr)(i:i).eq.' ') namreg(2,jr) = '_'
          end do
          write(iu,'(1x,a4,a,a4,a,99(e12.4,a))') namreg(1,jr),'_',
     *      namreg(2,jr),char(9),(BUDG(jr,k),char(9),k=1,kd1m+10)
        END DO
      end if
      RETURN
c  **
  901 FORMAT ('1',33A4)
  902 FORMAT ('0** BUDGETS',A16,' **   DAY',I6,', HR',I2,' (',I2,A5,
     *  I4,')',F9.0,'   TO   DAY',I6,', HR',I2,' (',I2,A5,I4,')',
     *  F9.0,'   DIF',F5.0,' HR')
  903 FORMAT ('0',131('-')/20X,'G     NH    SH  ',24I4)
  904 FORMAT (A16,3I6,2X,24I4)
  905 FORMAT (1X,131('-'))
  906 FORMAT (A16,3F6.1,2X,24F4.1)
  907 FORMAT (A16,3F6.1,2X,24I4)
  908 FORMAT ('0',17X,'WEST MID- EAST SOU. GRN- MID- NOR. WEST SIBR SOU.
     * CHNA IND. AUS. NOR. SOU. AFR. AFR. AMZN NOR. MID- NOR. WEST EAST'
     * /18X,'U.S. U.S. U.S. CNDA LAND EUR. RUSS SIBR PLAT CHNA DSRT DSRT
     * DSRT SHRA SHRA SAHL RAIN RAIN ATL. ATL. PAC. PAC. PAC. '/1X,
     *  131('-'))
  909 FORMAT (A16,1X,23I5)
  910 FORMAT (A16,1X,23F5.1)
  911 FORMAT (A16,3F6.2,2X,24F4.1)
  912 FORMAT (A16,3F6.2,2X,24I4)
  918 FORMAT ('0',16X,23(1X,A4)/17X,23(1X,A4)/1X,131('-'))
      END

      BLOCK DATA BD_DIAGJK
c  **
c  ** TITLES FOR SUBROUTINE DIAGJK
c  **
      COMMON/DJKTTL/TITLE1,TITLE2,TITLE3,TITLE4,TITLE5
                CHARACTER*64 TITLE1(12)/
c  **                                                              1-12
     1'TEMPERATURE (DEGREES CENTIGRADE)  ',
     *'HEIGHT (HUNDREDS OF METERS) ',
     3'SPECIFIC HUMIDITY (10**-5 KG H2O/KG AIR) ',
     *'RELATIVE HUMIDITY (PERCENT) ',
     *'ZONAL WIND (U COMPONENT) (TENTHS OF METERS/SECOND)',
     6'MERIDIONAL WIND (V COMPONENT) (HUNDREDTHS OF METERS/SECOND)',
     *'    ',
     *'    ',
     9'BAROCLINIC EDDY KINETIC ENERGY GEN. (10**-1 WATTS/M**2/SIGMA)',
     A'NORTH. TRANS. OF EDDY Q-G POT. VORTICITY  (10**-6 M/S**2)',
     1'P-K BY EDDY PRESSURE GRADIENT FORCE  (10**-1 W/M**2/UNIT SIGMA',
     2'DYNAMIC CONVERGENCE OF EDDY GEOPOTENTIAL (.1 WATTS/M**2/DSIGMA)'/
                CHARACTER*64 TITLE2(8)/
c  **                                                             13-20
     3'NORTH. TRANS. OF SENSIBLE HEAT BY EDDIES (10**14 WATTS/DSIGMA)',
     4'DYNAMIC CONVERGENCE OF DRY STATIC ENERGY (10 WATTS/M**2/DSIGMA)',
     *'    ',
     *'    ',
     7'STANDING EDDY KINETIC ENERGY (10**4 JOULES/M**2/UNIT SIGMA)',
     8'EDDY KINETIC ENERGY (10**4 JOULES/M**2/UNIT SIGMA)',
     9'TOTAL KINETIC ENERGY (10**4 JOULES/M**2/UNIT SIGMA)',
     *'    '/
                CHARACTER*64 TITLE3(8)/
c  **                                                             21-28
     1'POTENTIAL TEMPERATURE (DEGREES KELVIN) ',
     2'NOR. TRANS. OF DRY STAT. ENERGY BY STAND. EDDIES (10**14 W/DSIG)'
     *,
     *'NORTH. TRANS. OF DRY STATIC ENERGY BY EDDIES (10**14 WATTS/DSIG)'
     *,
     4'TOTAL NORTH. TRANSPORT OF DRY STATIC ENERGY (10**15 WATTS/DSIG)',
     5'NORTHWARD TRANSPORT OF LATENT HEAT BY EDDIES (10**13 WATTS/DSIG)'
     *,
     6'TOTAL NORTHWARD TRANSPORT OF LATENT HEAT (10**14 WATTS/UNIT SIG)'
     *,
     7'NORTH.TRANSPORT OF STATIC ENERGY BY EDDIES (10**14 WATTS/DSIGMA)'
     *,
     8'TOTAL NORTHWARD TRANSPORT OF STATIC ENERGY (10**15 WATTS/DSIGMA)'
     */
                CHARACTER*64 TITLE4(8)/
c  **                                                             29-36
     9'    ',
     A'TOTAL NORTHWARD TRANSPORT OF KINETIC ENERGY (10**12 WATTS/DSIG)',
     1'NORTH. TRANS. OF ANG. MOMENTUM BY STAND. EDDIES (10**18 J/DSIG)',
     2'NORTH. TRANS. OF ANG. MOMENTUM BY EDDIES (10**18 JOULES/DSIGMA)',
     3'TOTAL NORTHWARD TRANSPORT OF ANG. MOMENTUM (10**19 JOULES/DSIG)',
     4'NORTHWARD WAVE ENERGY FLUX  (10**11 JOULES/METER/UNIT SIGMA)',
     5'VERTICAL WAVE ENERGY FLUX  (10**11 JOULES/METER) ',
     6'DIVERGENCE OF THE WAVE ENERGY FLUX  (10**-5 M/S**2)'/
                CHARACTER*64 TITLE5(8)/
c  **                                                             37-44
     7'REFRACTION INDEX FOR WAVE NUMBER 1  (10**-8 PER METER**2)',
     8'REFRACTION INDEX FOR WAVE NUMBER 2  (10**-8 PER METER**2)',
     9'REFRACTION INDEX FOR WAVE NUMBER 3  (10**-8 PER METER**2)',
     O'REFRACTION INDEX FOR WAVE NUMBER 6  (10**-8 PER METER**2)',
     1'REFRACTION INDEX FOR WAVE NUMBER 9  (10**-8 PER METER**2)',
     2'Q-G POT. VORTICITY CHANGE OVER LATITUDES (10**-12 1/(SEC-M))',
     *'    ',
     *'    '/
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE DIAGJK
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      COMMON/WORK2/SENDEG(IM,JM),CN(2,IMH+1),BYP(JM),BYPV(JM),
     *  BYDAPO(JM),BYPDA(JM),BYDXYP(JM),DXCOSV(JM),
     *  ONES(JM),ONESPO(JM),BYDPS(3),BYPKS(3),ARQX(JM,3),
     *  AX(JM,LM),BX(JM,LM),CX(JM,LM),DX(JM,LM),VX(JM,LM)
      COMMON/WORK5/FKEY(JM,LM),DSJK(JM,LM,2),DSHEM(2,LM,2),
     *  DSGLOB(LM,2)
      COMMON/DJLCOM/JLAT(JM,2),WTJ(JM,2,2),
     *  LINECT,JMHALF,INC,IHOUR0,IHOUR
      DIMENSION PM(LM),PME(LM+1),PMO(LM+3),PKM(LM),MW(5)
      DATA MW/1,2,3,6,9/,ONE/1./,P1000/1000./
c  ** INITIALIZE CERTAIN QUANTITIES
      IF (KDIAG(2).GE.8) GO TO 120
      XWON=TWOPI/(DLON*FIM)
      LMP1=LM+1
      KM=LM
      KMM1=KM-1
      INC=1+(JM-1)/24
      JMHALF=JM/2
      BYIM=1./FIM
      BY100G=.01/GRAV
      SHA=RGAS/KAPA
      P1000K=EXPBYK(P1000)
      DO 30 L=1,LM
      PMO(L)=(PSF-PTOP)*SIG(L)+PTOP
      PKM(L)=PMO(L)**KAPA
      PME(L)=(PSF-PTOP)*SIGE(L)+PTOP
   30 PM(L)=(PSF-PTOP)*SIGE(L+1)+PTOP
      PME(LM+1)=PTOP
      PMO(LM+1)=.75*PTOP
      PMO(LM+2)=.35*PTOP
      PMO(LM+3)=.1*PTOP
      BYDPS(1)=1./(.5*PTOP)
      BYDPS(2)=1./(.3*PTOP)
      BYDPS(3)=1./(.2*PTOP)
      BYPKS(1)=1./(.75*PTOP)**KAPA
      BYPKS(2)=1./(.35*PTOP)**KAPA
      BYPKS(3)=1./(.1*PTOP)**KAPA
      DO 40 J=1,JM
      ONES(J)=1.
      ONESPO(J)=1.
      BYDXYP(J)=1./DXYP(J)
      BYDAPO(J)=BYDXYP(J)
      JLAT(J,1)=INT(.5+(J-1.0)*180./(JM-1))-90
      JLAT(J,2)=INT(.5+(J-1.5)*180./(JM-1))-90
      WTJ(J,1,1)=1.
   40 WTJ(J,2,1)=2.*FIM*DXYP(J)/AREAG
      ONESPO(1)=FIM
      ONESPO(JM)=FIM
      BYDAPO(1)=BYDAPO(1)*FIM
      BYDAPO(JM)=BYDAPO(JM)*FIM
      DO 50 J=2,JM
      DXCOSV(J)=DXV(J)*COSV(J)
      WTJ(J,1,2)=1.
   50 WTJ(J,2,2)=2.*FIM*DXYV(J)/AREAG
      WTJ(JMHALF+1,1,2)=.5
      WTJ(JMHALF+1,2,2)=WTJ(JMHALF+1,2,2)/2.
      IHOUR0=TOFDY0+.5
      IHOUR=TOFDAY+.5
      LINECT=65
      WRITE (6,901)
      BYIACN=1./(IDACC(1)+1.D-20)
      BYIARD=1./(IDACC(2)+1.D-20)
      BYIADA=1./(IDACC(4)+1.D-20)
      BYIMDA=BYIADA*BYIM
      FIMDA=IDACC(4)*FIM
      DO 52 J=1,JM
      BYP(J)=1./(APJ(J,1)+1.D-20)
      BYPDA(J)=BYP(J)*BYDXYP(J)
   52 BYPV(J)=4./(APJ(J,2)+1.D-20)
c  **
c  ** INITIALIZE DELTA SIGMA IN PRESSURE COORDINATES
c  **
      DO 60 J1=1,2
      J0=J1-1
      RSCALE=J1*J1
      DO 60 K=1,KM
      DPG=0.
      PIG=0.
      DO 58 JHEMI=1,2
      DPH=0.
      PIH=0.
      DO 55 JH=1,JMHALF
      J=(JHEMI-1)*(JMHALF-J0)+JH+J0
      DSJK(J,K,J1)=RSCALE*AJK(J,K,J1)/(APJ(J,J1)+1.D-20)
      DPH=DPH+AJK(J,K,J1)*WTJ(J,2,J1)
   55 PIH=PIH+APJ(J,J1)*WTJ(J,2,J1)
      DSHEM(JHEMI,K,J1)=RSCALE*DPH/(PIH+1.D-20)
      DPG=DPG+DPH
   58 PIG=PIG+PIH
   60 DSGLOB(K,J1)=RSCALE*DPG/(PIG+1.D-20)
c  **
c  ** V-V* IS D/DP(V'TH'/DTH/DP) , DX=4*V'TH'/DTH/DP AT INTERFACES
c  **
      DO 70 J=2,JM
      KDN=1
      DO 70 K=1,KM
c  ** FIXUP NEEDED IF OLDER VERSION OF DIAGB WAS USED (NO AJK(,,44))
      VX(J,K)=AJK(J,K,39)
      IF (AJK(1,1,44).NE.0.) GO TO 70
c  ** END OF FIXUP
      CX(J,K)=FIM*IDACC(4)
      AX(J,K)=AJK(J,K,39)/(AJK(J,K,2)+1.D-20)
      KUP=K+1
      IF (K.EQ.KM) KUP=KM
      VX(J,K)=0.
      IF (AJK(J,K,2).EQ.0.) GO TO 70
      IF (AJK(J,KDN,2).EQ.0.) KDN=KDN+1
      VX(J,K)=AJK(J,K,2)*(AJK(J,KUP,39)/AJK(J,KUP,2)-
     *   AJK(J,KDN,39)/AJK(J,KDN,2))/(PM(KUP)-PM(KDN)+.5*
     *   (AJK(J,KUP,2)/AJK(J,KUP,24)-AJK(J,KDN,2)/AJK(J,KDN,24)))
   70 KDN=K
c  ** FURTHER FIXUP NEEDED FOR OLDER VERSION OF DIAGB (NO AJK(,,44))
      IF (AJK(1,1,44).EQ.0..AND.KDIAG(2).GE.0) GO TO 90
c     DX=D(THETA)/(DP*2*IM*IDACC(4)) (ZONAL MEAN PT-GRID)
      DO 77 J=1,JM
      THDN=AJK(J,1,6)/(AJK(J,1,1)+1.D-20)
      DO 75 K=1,KM-1
      THUP=AJK(J,K+1,6)/(AJK(J,K+1,1)+1.D-20)
      DX(J,K)=0.
      IF (AJK(J,K,1).GT.0.)DX(J,K)=(THDN-THUP)/(AJK(J,K,1)+AJK(J,K+1,1))
   75 THDN=THUP
   77 DX(J,KM)=DX(J,KM-1)
c     CX=.5/DX=IM*IDACC(4)*DP/DTHETA (ZONAL MEAN UV-GRID)
c     AX=4*(V'TH')/(IM*IDACC(4)) (UV'GRID LAYER CENTERS)
      WTS=DXYP(1)*FIM
      DO 85 J=2,JM
      WTN=DXYP(J)*ONESPO(J)
      DO 80 K=1,KM
      AX(J,K)=(AJK(J,K,13)-AJK(J,K,12))/PKM(K)/(AJK(J,K,2)+1.D-20)
      CX(J,K)=0.
      IF (AJK(J-1,K,1)+AJK(J,K,1).EQ.0.) GO TO 80
      WTSK=WTS
      IF (AJK(J-1,K,1).EQ.0.) WTSK=0.
      WTNK=WTN
      IF (AJK(J,K,1).EQ.0.) WTNK=0.
      DTHDP=(DX(J-1,K)*WTSK+DX(J,K)*WTNK)/(WTSK+WTNK)
      IF (DTHDP.NE.0.) CX(J,K)=.5/DTHDP
   80 CONTINUE
   85 WTS=WTN
c  ** END OF FIXUP
c     DX(J,K)=AX*CX=4*(TRANSFORMED STREAM FUNCTION-STREAM FUNCTION)
   90 DO 95 J=2,JM
      DX(J,KM)=AX(J,KM)*CX(J,KM)
      DO 95 K=1,KM-1
      WTKP1=AJK(J,K,2)/(AJK(J,K+1,2)+AJK(J,K,2)+1.D-20)
   95 DX(J,K)=(AX(J,K)*(1.-WTKP1)+AX(J,K+1)*WTKP1)*CX(J,K)
c  **
c  ** PROGNOSTIC QUANTITIES AT CONSTANT PRESSURE
c  **
c  ** # OF GRIDPOINTS, DELTA P, S.D. OF DELTA P
      CALL JLMAP (89,PMO,AJK(1,1,24),XWON*BYIADA,ONES,ONES,LTM,1,2)
      CALL JLMAP (91,PMO,AJK(1,1,2),BYIMDA,ONES,ONES,LTM,2,2)
      DO 98 J=2,JM
      DO 98 K=1,LTM
      BYN=1./(AJK(J,K,24)+1.D-10)
      AX(J,K)=0.
      SDDP=(AJK(J,K,23)-AJK(J,K,2)*AJK(J,K,2)*BYN)*BYN
   98 IF (SDDP.GT.0.) AX(J,K)=SQRT(SDDP)
      CALL JLMAP (68,PMO,AX,ONE,ONES,ONES,LTM,2,2)
c  ** TEMPERATURE, HEIGHT, SPECIFIC AND RELATIVE HUMIDITY
      CALL JKMAPS (1,PMO,AJK(1,1,3),ONES,BYP,ONES,KM,2,1,
     *  ASJL,BYIMDA,ONESPO,ONES)
      SCALES=BYIMDA*BY100G
      CALL JKMAPS (2,PMO,AJK(1,1,4),BY100G,BYP,ONES,KM,2,1,
     *  ASJL(1,1,2),SCALES,ONESPO,ONES)
      SCALE=1.D5
      CALL JKMAP (3,PMO,AJK(1,1,5),SCALE,BYP,ONES,KM,2,1)
      SCALE=100.
      CALL JKMAP (4,PMO,AJK(1,1,7),SCALE,BYP,ONES,KM,2,1)
c  ** U AND V WINDS, STREAM FUNCTION
      SCALE=10.
      CALL JKMAP (5,PMO,AJK(1,1,8),SCALE,BYPV,ONES,KM,2,2)
      SCALE=100.
      CALL JKMAP (6,PMO,AJK(1,1,9),SCALE,BYPV,ONES,KM,2,2)
c Obtain the stream function by integrating meridional velocity
c downward from the model top
c
      do j=2,jm
        ax(j,km) = 0.
        do k=km-1,1,-1
          ax(j,k)=ax(j,k+1)-ajk(j,k+1,9)
        enddo
      enddo
      SCALE=100.D-9*XWON*BYIADA/GRAV
      CALL JLMAP (61,PM,AX,SCALE,DXV,ONES,KM,2,2)
      DO 110 K=1,KM
      DO 110 J=2,JM
  110 BX(J,K)=AX(J,K)+.25*DX(J,K)
      CALL JLMAP (106,PM,BX,SCALE,DXV,ONES,KM,2,2)
c  ** VERTICAL WINDS
      SCALE=-1.D5*BYIMDA
      CALL JLMAP (62,PME,AJK(1,1,25),SCALE,BYDXYP,ONES,KM,2,1)
c  **
c  ** CALCULATIONS FOR STANDING EDDIES
c  **
      IF (SKIPSE.EQ.1.) GO TO 180
  120 DO 150 J=2,JM
      DO 150 K=1,KM
      AX(J,K)=0.
      BX(J,K)=0.
  150 CX(J,K)=0.
      DO 170 J=2,JM
      DO 155 I=1,IM
  155 SENDEG(I,J)=0.
      DO 170 K=1,KM
      DPTI=0.
      PUTI=0.
      PVTI=0.
      DE4TI=0.
      SKEI=0.
      SNDEGI=0.
      SNAMI=0.
      DO 160 I=1,IM
      IF (AIJK(I,J,K,4).EQ.0.) GO TO 160
      DPTI=DPTI+AIJK(I,J,K,4)
      BYDPK=1./(AIJK(I,J,K,4)+1.D-20)
      PUTI=PUTI+AIJK(I,J,K,1)
      PVTI=PVTI+AIJK(I,J,K,2)
      DE4TI=DE4TI+AIJK(I,J,K,3)
      SKEI=SKEI+(AIJK(I,J,K,1)*AIJK(I,J,K,1)
     *            +AIJK(I,J,K,2)*AIJK(I,J,K,2))*BYDPK
      SNDEGI=SNDEGI+(AIJK(I,J,K,3)*AIJK(I,J,K,2)*BYDPK)
      SENDEG(I,J)=SENDEG(I,J)
     *  +(AIJK(I,J,K,3)*AIJK(I,J,K,2)*BYDPK)
      SNAMI=SNAMI+AIJK(I,J,K,1)*AIJK(I,J,K,2)*BYDPK
  160 CONTINUE
      AX(J,K)=SKEI-(PUTI*PUTI+PVTI*PVTI)/(DPTI+1.D-20)
      SZNDEG=DE4TI*PVTI/(DPTI+1.D-20)
      DO 165 I=1,IM
  165 SENDEG(I,J)=SENDEG(I,J)-SZNDEG/FIM
      BX(J,K)=SNDEGI-SZNDEG
  170 CX(J,K)=SNAMI-PUTI*PVTI/(DPTI+1.D-20)
      IF (KDIAG(2).GE.8) RETURN
c  ** STANDING EDDY, EDDY AND TOTAL KINETIC ENERGY
  180 SCALE=50.D-4*BYIMDA/GRAV
      IF (SKIPSE.EQ.1.) GO TO 190
      CALL JKMAP (17,PMO,AX,SCALE,ONES,ONES,KM,2,2)
  190 DO 200 K=1,KM
      DO 200 J=2,JM
  200 AX(J,K)=AJK(J,K,11)-AJK(J,K,10)
      CALL JKMAP (18,PMO,AX,SCALE,ONES,ONES,KM,2,2)
      CALL JKMAP (19,PMO,AJK(1,1,11),SCALE,ONES,ONES,KM,2,2)
c  ** POTENTIAL TEMPERATURE, POTENTIAL VORTICITY
      DO 205 LR=1,3
      DO 205 J=1,JM
  205 ARQX(J,LR)=ASJL(J,LR,1)*BYIMDA*ONESPO(J)+273.16
      CALL JKMAPS (21,PMO,AJK(1,1,6),P1000K,BYP,ONES,KM,2,1,
     *  ARQX,P1000K,ONES,BYPKS)
      SCALE=1.D6*BYIMDA*P1000K
      CALL JLMAP (63,PMO,AJK(1,1,32),SCALE,BYDXYP,ONES,KM,2,1)
c  **
c  ** NORTHWARD TRANSPORTS AT CONSTANT PRESSURE
c  **
c  ** NORTHWARD TRANSPORT OF SENSIBLE HEAT BY EDDIES
      SCALE=25.D-14*XWON*SHA*BYIADA/GRAV
      DO 210 K=1,KM
      DO 210 J=2,JM
  210 AX(J,K)=AJK(J,K,13)-AJK(J,K,12)
      CALL JKMAP (13,PMO,AX,SCALE,DXV,ONES,KM,2,2)
c  ** NORTHWARD TRANSPORT OF DRY STATIC ENERGY BY STANDING EDDIES,
c  **   EDDIES, AND TOTAL
      SCALE=25.D-14*XWON*BYIADA/GRAV
      IF (SKIPSE.EQ.1.) GO TO 220
      CALL JKMAP (22,PMO,BX,SCALE,DXV,ONES,KM,2,2)
  220 DO 230 K=1,KM
      DO 230 J=2,JM
      AX(J,K)=SHA*(AJK(J,K,13)-AJK(J,K,12))+(AJK(J,K,15)-AJK(J,K,14))
  230 BX(J,K)=SHA*AJK(J,K,13)+AJK(J,K,15)
      CALL JKMAP (23,PMO,AX,SCALE,DXV,ONES,KM,2,2)
      SCALE=SCALE*.1
      CALL JKMAP (24,PMO,BX,SCALE,DXV,ONES,KM,2,2)
c  ** NORTHWARD TRANSPORT OF LATENT HEAT BY EDDIES AND TOTAL
      DO 240 K=1,KM
      DO 240 J=2,JM
      DX(J,K)=AJK(J,K,17)-AJK(J,K,16)
  240 AX(J,K)=AX(J,K)+LHE*DX(J,K)
      SCALE=25.D-13*XWON*LHE*BYIADA/GRAV
      CALL JKMAP (25,PMO,DX,SCALE,DXV,ONES,KM,2,2)
      SCALE=SCALE*.1
      CALL JKMAP (26,PMO,AJK(1,1,17),SCALE,DXV,ONES,KM,2,2)
c  ** NORTHWARD TRANSPORT OF STATIC ENERGY BY EDDIES AND TOTAL
      DO 245 K=1,KM
      DO 245 J=2,JM
  245 DX(J,K)=BX(J,K)+LHE*AJK(J,K,17)
      SCALE=25.D-14*XWON*BYIADA/GRAV
      CALL JKMAP (27,PMO,AX,SCALE,DXV,ONES,KM,2,2)
      SCALE=SCALE*.1
      CALL JKMAP (28,PMO,DX,SCALE,DXV,ONES,KM,2,2)
c  ** NORTHWARD TRANSPORT OF KINETIC ENERGY
      SCALE=50.D-12*XWON*BYIADA/GRAV
      CALL JKMAP (30,PMO,AJK(1,1,19),SCALE,DXV,ONES,KM,2,2)
c  ** NOR. TRANS. OF ANG. MOMENTUM BY STANDING EDDIES, EDDIES AND TOTAL
      SCALE=100.D-18*XWON*RADIUS*BYIADA/GRAV
      IF (SKIPSE.EQ.1.) GO TO 250
      CALL JKMAP (31,PMO,CX,SCALE,DXCOSV,ONES,KM,2,2)
  250 DO 260 K=1,KM
      DO 260 J=2,JM
      CX(J,K)=AJK(J,K,21)-AJK(J,K,20)
  260 DX(J,K)=AJK(J,K,21)+RADIUS*OMEGA*COSV(J)*AJK(J,K,9)
      CALL JKMAP (32,PMO,CX,SCALE,DXCOSV,ONES,KM,2,2)
      SCALE=.1*SCALE
      CALL JKMAP (33,PMO,DX,SCALE,DXCOSV,ONES,KM,2,2)
c  **
c  ** DYNAMIC CONVERGENCE OF ENERGY
c  **
      SCALE=25.D-1*BYIMDA/GRAV
      DO 370 K=1,KM
      CX(1,K)=-BX(2,K)*SCALE*DXV(2)
      CX(JM,K)=BX(JM,K)*SCALE*DXV(JM)
      DX(1,K)=-(AJK(2,K,15)-AJK(2,K,14))*SCALE*DXV(2)
      DX(JM,K)=(AJK(JM,K,15)-AJK(JM,K,14))*SCALE*DXV(JM)
      DO 370 J=2,JM-1
      CX(J,K)=(BX(J,K)*DXV(J)-BX(J+1,K)*DXV(J+1))*SCALE
      DX(J,K)=((AJK(J,K,15)-AJK(J,K,14))*DXV(J) -
     *  (AJK(J+1,K,15)-AJK(J+1,K,14))*DXV(J+1))*SCALE
  370 CONTINUE
      SCALE=-100.D-1*BYIMDA/GRAV
      DO 380 K=1,KM-1
      DO 380 J=1,JM
      CX(J,K)=CX(J,K)-AJK(J,K,27)*SCALE
      CX(J,K+1)=CX(J,K+1)+AJK(J,K,27)*SCALE
      DX(J,K)=DX(J,K)-AJK(J,K,30)*SCALE
  380 DX(J,K+1)=DX(J,K+1)+AJK(J,K,30)*SCALE
      CALL JKMAP (14,PMO,CX,ONE,BYDXYP,ONES,KM,2,1)
      SCALE=100.
      CALL JKMAP (12,PMO,DX,SCALE,BYDXYP,ONES,KM,2,1)
c  ** BAROCLINIC EKE GENERATION, P-K BY EDDY PRESSURE GRADIENT FORCE
      SCALE=50.E1*BYIMDA*RGAS/GRAV
      CALL JKMAP (9,PMO,AJK(1,1,31),SCALE,BYDXYP,ONES,KM,2,1)
      SCALE=100.E1*BYIMDA/(GRAV*2.*DT)
      CALL JKMAP (11,PMO,AJK(1,1,22),SCALE,ONES,ONES,KM,2,2)
c  **
c  ** VERTICAL TRANSPORTS
c  **
c  ** VERTICAL TRANSPORT OF GEOPOTENTIAL ENERGY BY EDDIES
      SCALE=-100.D-12*XWON*BYIADA/GRAV
      CALL JLMAP (99,PM,AJK(1,1,30),SCALE,ONES,ONES,KM-1,1,1)
c  ** VERTICAL TRANSPORT OF DRY STATIC ENERGY BY EDDIES AND TOTAL
      DO 390 K=1,KM-1
      DO 390 J=1,JM
      AX(J,K)=AJK(J,K,27)-AJK(J,K,26)
  390 BX(J,K)=AJK(J,K,29)-AJK(J,K,28)
      SCALE=-100.D-12*XWON*BYIADA/GRAV
      CALL JLMAP (100,PM,AX,SCALE,ONES,ONES,KM-1,1,1)
      SCALE=SCALE*.01
      CALL JLMAP (101,PM,AJK(1,1,27),SCALE,ONES,ONES,KM-1,1,1)
c  ** VERTICAL TRANSPORT OF LATENT HEAT BY EDDIES AND TOTAL
      SCALE=-100.D-12*XWON*LHE*BYIADA/GRAV
      CALL JLMAP (102,PM,BX,SCALE,ONES,ONES,KM-1,1,1)
      SCALE=SCALE*.1
      CALL JLMAP (103,PM,AJK(1,1,29),SCALE,ONES,ONES,KM-1,1,1)
c  ** VERTICAL TRANSPORT OF STATIC ENERGY BY EDDIES AND TOTAL
      DO 420 K=1,KM-1
      DO 420 J=1,JM
      AX(J,K)=AX(J,K)+LHE*BX(J,K)
  420 BX(J,K)=AJK(J,K,27)+LHE*AJK(J,K,29)
      SCALE=-100.D-13*XWON*BYIADA/GRAV
      CALL JLMAP (104,PM,AX,SCALE,ONES,ONES,KM-1,1,1)
      SCALE=SCALE*.1
      CALL JLMAP (105,PM,BX,SCALE,ONES,ONES,KM-1,1,1)
c  ** VERTICAL TRANSPORT OF KINETIC ENERGY
      SCALE=-12.5E-11*XWON*BYIADA/GRAV
      CALL JLMAP (110,PM,AJK(1,1,36),SCALE,ONES,ONES,KM-1,1,2)
c  ** VERTICAL TRANSPORT OF ANGULAR MOMENTUM BY LARGE SCALE MOTIONS
      SCALE=-25.D-16*XWON*RADIUS*BYIADA/GRAV
      CALL JLMAP (111,PM,AJK(1,1,37),SCALE,COSV,ONES,KM-1,1,2)
      SCALE=1.D-2*SCALE
      CALL JLMAP (112,PM,AJK(1,1,38),SCALE,COSV,ONES,KM-1,1,2)
c  ** VERTICAL TRANSPORT OF POTENTIAL VORTICITY TOTAL AND BY EDDIES
      SCALE=-25.D-4*XWON*P1000K*BYIADA/GRAV
      CALL JLMAP (107,PM,AJK(1,1,33),SCALE,BYDXYP,ONES,KM-1,1,1)
      CALL JLMAP (108,PM,AJK(1,1,34),SCALE,BYDXYP,ONES,KM-1,1,1)
c  ** NOR. TRANSPORT OF QUASI-GEOSTROPHIC POT. VORTICITY BY EDDIES
      DO 490 K=1,KM
      AX(1,K)=0.
      AX(JM,K)=0.
      DX(1,K)=0.
      DX(JM,K)=0.
      DO 490 J=2,JM-1
      AX(J,K)=((AJK(J,K,21)-AJK(J,K,20))*DXCOSV(J)/(AJK(J,K,2)+1.D-20)-
     *  (AJK(J+1,K,21)-AJK(J+1,K,20))*DXCOSV(J+1)/
     *  (AJK(J+1,K,2)+1.D-20))/COSP(J)
      DX(J,K)=F(J)*(VX(J,K)+VX(J+1,K))/(AJK(J,K,2)+AJK(J+1,K,2)+
     *  1.D-20)
  490 CONTINUE
      DO 500 J=2,JM-1
      DO 500 K=1,KM
  500 AX(J,K)=AJK(J,K,1)*(AX(J,K)+.25*DX(J,K))
      SCALE=1.D6
      CALL JKMAP (10,PMO,AX,SCALE,BYPDA,ONES,KM,2,1)
c  **
c  ** ELIASSEN PALM FLUX:  NORTHWARD, VERTICAL, DIVERGENCE
c  **
      SCALE=25.D-11*XWON*BYIADA/GRAV
      SMALL=1.D-20
      DO 510 K=1,KM
      AX(1,K)=0.
      DO 510 J=2,JM
      UX=AJK(J,K,8)/(AJK(J,K,2)+1.D-20)
      IF (ABS(UX).GE.SMALL) GO TO 510
      SN=+1.
      IF (UX.LT.0.) SN=-1.
      UX=SN*SMALL
  510 AX(J,K)=(AJK(J,K,15)-AJK(J,K,14))/UX*DXV(J)
      CALL JKMAP (34,PMO,AX,SCALE,ONES,ONES,KM,2,2)
      SCALE=-100.D-11*XWON*BYIADA/GRAV
      DO 520 K=1,KM-1
      BX(1,K)=0.
      BX(JM,K)=0.
      DO 520 J=1,JM
      IF (J.NE.1.AND.J.NE.JM) GO TO 516
      IF (J.EQ.1) UX=.5*(AJK(J+1,K,8)+AJK(J+1,K+1,8))/
     *     (AJK(J+1,K,2)+AJK(J+1,K+1,2)+1.D-20)
      IF (J.EQ.JM) UX=.5*(AJK(J,K,8)+AJK(J,K+1,8))/
     *     (AJK(J,K,2)+AJK(J,K+1,2)+1.D-20)
      GO TO 518
  516 UX=(AJK(J,K,8)+AJK(J+1,K,8)+AJK(J,K+1,8)+AJK(J+1,K+1,8))/
     *  (AJK(J,K,2)+AJK(J+1,K,2)+AJK(J,K+1,2)+AJK(J+1,K+1,2)+1.D-20)
  518 IF (ABS(UX).GE.SMALL) GO TO 520
      SN=+1.
      IF (UX.LT.0.) SN=-1.
      UX=SN*SMALL
  520 BX(J,K)=AJK(J,K,30)/UX
      CALL JLMAP (113,PM,BX,SCALE,ONES,ONES,KM-1,1,1)
      DO 530 K=1,KM
      CX(1,K)=0.
      CX(JM,K)=0.
      DO 530 J=2,JM-1
  530 CX(J,K)=.25*(AX(J+1,K)-AX(J,K))
      DO 540 K=1,KM-1
      DO 540 J=1,JM
      CX(J,K)=CX(J,K)-BX(J,K)
  540 CX(J,K+1)=CX(J,K+1)+BX(J,K)
      SCALE=1.D5
      CALL JKMAP (36,PMO,CX,SCALE,BYPDA,ONES,KM,2,1)
c  **
c  ** D/DY OF Q-G POTENTIAL VORTICITY AND REFRACTION INDICES
c  **
c  ** PRELIMINARIES:  VERTICAL DERIVATIVES AND N**2
      GSQ=GRAV*GRAV
      GBYRSQ=GRAV*GRAV/(RGAS*RGAS)
      DO 600 J=1,JM
      K1=1
  580 IF (AJK(J,K1,1).GT.1.D-20) GO TO 590
      AX(J,K1)=0.
      BX(J,K1)=0.
      DX(J,K1)=0.
      K1=K1+1
      GO TO 580
  590 KDN=K1
      PDN=PM(KDN)+.5*AJK(J,KDN,1)/(AJK(J,KDN,35)+1.D-20)
      DO 600 K=K1,KM
      DP=AJK(J,K,1)
      PMK=PM(K)+.5*AJK(J,K,1)/(AJK(J,K,35)+1.D-20)
      KUP=K+1
      IF (K.EQ.KM) KUP=KM
      PUP=PM(KUP)+.5*AJK(J,KUP,1)/(AJK(J,KUP,35)+1.D-20)
      DALPHA=(AJK(J,KUP,3)/(AJK(J,KUP,1)+1.D-20)+273.16)/PUP-
     *  (AJK(J,KDN,3)/(AJK(J,KDN,1)+1.D-20)+273.16)/PDN
      DTHETA=AJK(J,KUP,6)/(AJK(J,KUP,1)+1.D-20)-
     *  AJK(J,KDN,6)/(AJK(J,KDN,1)+1.D-20)
      THETA=AJK(J,K,6)/(AJK(J,K,1)+1.D-20)
      TX=AJK(J,K,3)/(AJK(J,K,1)+1.D-20)+273.16
      IF (ABS(DTHETA).GE.1.D-20) GO TO 595
      SN=+1.
      IF (DTHETA.LT.0.) SN=-1.
      DTHETA=SN*1.D-20
  595 DX(J,K)=DP*F(J)*PMK*THETA*(PUP-PDN)/(TX*DTHETA*DXYP(J))
      AX(J,K)=DALPHA/(PUP-PDN-1.D-20)
c  ** CALCULATE N**2 AT PRESSURE LATITUDES
      BX(J,K)=-DP*GSQ*PMK*DTHETA/(RGAS*TX*THETA*(PUP-PDN-1.D-20))
      KDN=K
  600 PDN=PMK
c  ** CALCULATE  Q12 = (D(UDX) + F*DA)/DA
      DO 620 K=1,KM
      UDXS=0.
      DO 610 J=1,JM-1
      UDXN=AJK(J+1,K,8)/(AJK(J+1,K,2)+1.D-20)*DXV(J+1)
      CX(J,K)=(UDXS-UDXN+F(J))/DXYP(J)
  610 UDXS=UDXN
      CX(JM,K)=(UDXS+F(JM))/DXYP(JM)
c  ** FIND DQ/DY = (Q12(J)-Q12(J-1)+Q3(J)-Q3(J-1))/DY
      DO 620 J=JM,2,-1
      DP=AJK(J,K,2)
      AX(J,K)=DP*(CX(J,K)-CX(J-1,K) + (AX(J,K)-AX(J-1,K))*
     *  (DX(J,K)+DX(J-1,K))/(AJK(J,K,1)+AJK(J-1,K,1)+1.D-20))/DYP(3)
  620 CONTINUE
      SCALE=1.D12
      CALL JKMAP (42,PMO,AX,SCALE,BYPV,ONES,KM,2,2)
c  ** TERMS FOR THE REFRACTION INDEX EXPRESSION
      DO 640 J=2,JM
      BYFSQ=2.*DXYV(J)*DXYV(J)/(F(J-1)*F(J-1)+F(J)*F(J))
      DO 640 K=1,KM
      BYDP2=1./(AJK(J-1,K,1)+AJK(J,K,1)+1.D-20)
      TX=BYDP2*(AJK(J-1,K,3)+AJK(J,K,3))+273.16
      DX(J,K)=GBYRSQ/(TX*TX)
      SQN=BYDP2*(BX(J-1,K)+BX(J,K))
      CX(J,K)=SQN*BYFSQ
      UX=AJK(J,K,8)
      IF (ABS(UX).GE.1.D-20) GO TO 635
      SN=+1.
      IF (UX.LT.0.) SN=-1.
      UX=SN*1.D-20
  635 AX(J,K)=AX(J,K)/UX
  640 CONTINUE
c  ** COMBINE TERMS, PRINT OUT REFRACTION INDICES
      SCALE=1.D8
      DO 660 M=1,5
      SQM=MW(M)*MW(M)
      DO 650 J=2,JM
      BYRCOS=1./(RADIUS*RADIUS*COSV(J)*COSV(J))
      DO 650 K=1,KM
      DP=AJK(J,K,2)
  650 BX(J,K)=DP*(CX(J,K)*(AX(J,K)-SQM*BYRCOS)-.25*DX(J,K))
  660 CALL JKMAP (M+36,PMO,BX,SCALE,BYPV,ONES,KM,2,2)
  670 CONTINUE
c  ** SKIP REMAINING MAPS IF DATA NOT AVAILABLE
      IF (AJK(1,1,44).NE.0.) GO TO 799
c  **
c  ** CHANGE OF THE MEAN FIELDS OF WIND AND TEMPERATURE
c  **
      DO 710 K=1,KM
      DO 710 J=1,JM
      AX(J,K)=0.
      BX(J,K)=0.
      CX(J,K)=0.
  710 DX(J,K)=0.
      DO 720 J=2,JM-1
      DO 720 K=2,KM-1
      AX(J,K)=((AJK(J,K,21)-AJK(J,K,20))*DXV(J)-(AJK(J+1,K,21)-
     *  AJK(J+1,K,20))*DXV(J+1))/(AJK(J,K,1)*DXYP(J)+1.D-20)+
     *  .125*((AJK(J,K,37)-AJK(J,K-1,37))/(AJK(J,K,2)*DXYV(J)+1.D-20)+
     * (AJK(J+1,K,37)-AJK(J+1,K-1,37))/(AJK(J+1,K,2)*DXYV(J+1)+1.D-20))
      BX(J,K)=(AJK(J+1,K,44)*DXCOSV(J+1)-AJK(J,K,44)*DXCOSV(J))/
     *  (DXYP(J)*COSP(J))+.5*(AJK(J,K-1,45)-AJK(J,K,45)+
     *  AJK(J+1,K-1,45)-AJK(J+1,K,45))/(PM(K-1)-PM(K))
      CX(J,K)=.25*((AJK(J,K,13)-AJK(J,K,12))*DXV(J)-(AJK(J+1,K,13)-
     *  AJK(J+1,K,12))*DXV(J+1))/(AJK(J,K,1)*DXYP(J)+1.D-20)+
     *  (AJK(J,K,50)-AJK(J,K-1,50))/(PM(K-1)-PM(K))*BYIADA*PKM(K)
  720 CONTINUE
c  ** WIND: RATE OF CHANGE, ADVECTION, EDDY CONVERGENCE
      IF (IDACC(4).LE.1) GO TO 730
      SCALE=1.D6/((IDACC(4)-1)*DT*NDAA)
      CALL JLMAP (40,PMO,AJK(1,1,47),SCALE,ONES,ONES,KM,2,2)
  730 SCALE=1.D6*BYIADA
      CALL JLMAP (59,PMO,AJK(1,1,40),SCALE,ONES,ONES,KM-1,2,1)
      SCALE=1.D6
      CALL JLMAP (60,PMO,AX,SCALE,ONES,ONES,KM-1,2,1)
c  ** WIND: TRANSFORMED ADVECTION, LAGRANGIAN CONVERGENCE (DEL.F)
      SCALE=1.D6*BYIADA
      CALL JLMAP (64,PMO,AJK(1,1,42),SCALE,ONES,ONES,KM-1,2,1)
      CALL JLMAP (65,PMO,BX,SCALE,ONES,ONES,KM-1,2,1)
c  ** TEMPERATURE: RATE OF CHANGE, ADVECTION, EDDY CONVERGENCE
      IF (IDACC(4).LE.1) GO TO 750
      SCALE=1.D1*SDAY/((IDACC(4)-1)*DT*NDAA)
      CALL JLMAP (66,PMO,AJK(1,1,49),SCALE,ONES,PKM,KM,2,1)
  750 SCALE=1.D1*SDAY*BYIADA
      CALL JLMAP (67,PMO,AJK(1,1,41),SCALE,ONES,PKM,KM-1,2,1)
      SCALE=1.D1*SDAY
      CALL JLMAP (69,PMO,CX,SCALE,ONES,ONES,KM-1,2,1)
c  ** TEMPERATURE: TRANSFORMED ADVECTION
      SCALE=1.D1*SDAY*BYIADA
      CALL JLMAP (70,PMO,AJK(1,1,43),SCALE,ONES,PKM,KM-1,2,1)
  799 CONTINUE
      RETURN
  901 FORMAT (////////
     *  '010**14 WATTS = .2067 * 10**19 CALORIES/DAY'/
     *  '010**18 JOULES = .864 * 10**30 GM*CM**2/SEC/DAY')
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE JKMAP (NT,PM,AX,SCALE,SCALEJ,SCALEK,KMAX,JWT,J1)
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      include 'pd_COMMON'
      COMMON/WORK4/MLAT(JM),FLAT(JM),ASUM(JM),AHEM(2)
      COMMON/WORK5/FKEY(JM,LM),DSJK(JM,LM,2),DSHEM(2,LM,2),
     *  DSGLOB(LM,2),CX(JM,LM)
      COMMON/DJLCOM/JLAT(JM,2),WTJ(JM,2,2),
     *  LINECT,JMHALF,INC,IHOUR0,IHOUR
      COMMON/DJKTTL/TITLE(1)
      DIMENSION AX(JM,*),ARQX(JM,*)
      DIMENSION PM(*),SCALEJ(*),SCALEK(*),SCALJR(*),SCALLR(*)
      CHARACTER*4 DASH,WORD(4),TITLE*64
      REAL*4 XJL(JM+3,LM+4)    ! Save
      CHARACTER XLB*16,CLAT*16/'LATITUDE'/,CPRES*16/'PRESSURE (MB)'/,
     *  CBLANK*16/' '/
      DATA DASH/'----'/,WORD/'SUM','MEAN',' ','.1*'/
c  **
c  ** PRODUCE A LATITUDE BY LAYER TABLE OF THE ARRAY A
c  **
   10 LINECT=LINECT+KMAX+7
      IF (LINECT.LE.60) GO TO 20
      JY0=JYEAR0-1900
      JY=JYEAR-1900
      WRITE (6,907) (XLABEL(K),K=1,27),JDATE0,JMNTH0,JY0,JDATE,JMONTH,JY
      LINECT=KMAX+8
   20 WRITE (6,901) TITLE(NT),(DASH,J=J1,JM,INC)
      WRITE (6,904) WORD(JWT),(NINT(LAT_DG(J,J1)),J=JM,J1,-INC)
      WRITE (6,905) (DASH,J=J1,JM,INC)
      J0=J1-1
         DO 40 L=1,LM+4
         DO 40 J=1,JM+3
   40    XJL(J,L) = -1.E30
         KSX = 0            ! KSX = LAYERS GENERATED AT ENTRY
  100 DO 110 J=J1,JM
      DO 110 K=1,KMAX
  110 CX(J,K)=AX(J,K)*SCALE*SCALEJ(J)*SCALEK(K)
         KLMAX = KMAX  ! +KSX
c  ** HORIZONTAL SUMS AND TABLE ENTRIES
      DO 140 K=KMAX,1,-1
      AGLOB=0.
      DO 130 JHEMI=1,2
      AHEM(JHEMI)=0.
      DO 120 JH=1,JMHALF
      J=(JHEMI-1)*(JMHALF-J0)+JH+J0
      FLAT(J)=CX(J,K)/(DSJK(J,K,J1)+1.D-20)
         XJL(J,K) = FLAT(J)
         IF (DSJK(J,K,J1).EQ.0.) XJL(J,K) = -1.E30
      MLAT(J)=NINT(FLAT(J))
  120 AHEM(JHEMI)=AHEM(JHEMI)+CX(J,K)*WTJ(J,JWT,J1)
  130 AGLOB=AGLOB+AHEM(JHEMI)/JWT
      H1=AHEM(1)/(DSHEM(1,K,J1)+1.D-20)
      H2=AHEM(2)/(DSHEM(2,K,J1)+1.D-20)
      G1=AGLOB/(DSGLOB(K,J1)+1.D-20)
         XJL(JM+3,K)=H1   ! SOUTHERN HEM
         XJL(JM+2,K)=H2   ! NORTHERN HEM
         XJL(JM+1,K)=G1   ! GLOBAL
      WRITE (6,902) PM(K),G1,H2,H1,(MLAT(J),J=JM,J1,-INC)
         IF (NT.EQ.5) CALL KEYJKJ (K,FLAT)
  140 CONTINUE
c  ** VERTICAL SUMS
      WRITE (6,905) (DASH,J=J1,JM,INC)
      IF (NT.GE.80.AND.NT.LE.87) RETURN
      SUMFAC=1.
      IWORD=3
      IF (NT.NE.1.AND.NT.NE.6.AND.NT.NE.24.AND.NT.NE.26.AND.NT.NE.28
     *  .AND.NT.NE.33) GO TO 160
      SUMFAC=10.
      IWORD=4
  160 CONTINUE
      DO 180 J=J1,JM
      ASUM(J)=0.
      DO 170 K=1,KMAX
  170 ASUM(J)=ASUM(J)+CX(J,K)
         XJL(J,LM+4)=ASUM(J)
  180 MLAT(J)=NINT(ASUM(J)*SUMFAC)
      AGLOB=0.
      DO 200 JHEMI=1,2
      AHEM(JHEMI)=0.
      DO 190 JH=1,JMHALF
      J=(JHEMI-1)*(JMHALF-J0)+JH+J0
      AHEM(JHEMI)=AHEM(JHEMI)+ASUM(J)*WTJ(J,JWT,J1)*SUMFAC
  190 CONTINUE
  200 AGLOB=AGLOB+AHEM(JHEMI)/JWT
      if (iprint_pd(2).lt.9) then
         XJL(JM+3,LM+4)=AHEM(1)/SUMFAC   ! SOUTHERN HEM
         XJL(JM+2,LM+4)=AHEM(2)/SUMFAC   ! NORTHERN HEM
         XJL(JM+1,LM+4)=AGLOB/SUMFAC     ! GLOBAL
         WRITE(XLB,'(1X,A3,I5)') JMNTH0,JYEAR0
         WRITE (iu_pd(2)) TITLE(NT),XLB,JM-J1+1,KLMAX,1,1,
     *     ((XJL(J,L),J=J1,JM),L=1,KLMAX),
     *     (SNGL(LAT_DG(J,J1)),J=J1,JM),(SNGL(PM(L)),L=1,KLMAX),1.,1.,
     *     CLAT,CPRES,CBLANK,CBLANK,'NASAGISS',
     *     (XJL(J,LM+4),J=J1,JM+3),((XJL(J,L),J=JM+1,JM+3),L=1,KLMAX)
      end if
      WRITE (6,903) WORD(IWORD),AGLOB,AHEM(2),AHEM(1),
     *  (MLAT(J),J=JM,J1,-INC)
         IF (NT.EQ.1) CALL KEYJKT (AGLOB,ASUM)
         IF (NT.EQ.18.OR.NT.EQ.19) CALL KEYJKE (NT,AHEM,ASUM)
         IF (NT.GE.22.AND.NT.LE.33) CALL KEYJKN (NT,ASUM,SUMFAC)
      RETURN
c  **
      ENTRY JKMAPS (NT,PM,AX,SCALE,SCALEJ,SCALEK,KMAX,JWT,J1,
     *  ARQX,SCALER,SCALJR,SCALLR)
         KSX = 3
         DO 205 L=1,LM+4
         DO 205 J=1,JM+3
  205    XJL(J,L) = -1.E30
      LINECT=LINECT+KMAX+10
      IF (LINECT.LE.60) GO TO 230
      JY0=JYEAR0-1900
      JY=JYEAR-1900
      WRITE (6,907) (XLABEL(K),K=1,27),JDATE0,JMNTH0,JY0,JDATE,JMONTH,JY
      LINECT=KMAX+11
  230 J0=J1-1
c  ** PRODUCE UPPER STRATOSPHERE NUMBERS FIRST
      WRITE (6,901) TITLE(NT),(DASH,J=J1,JM,INC)
      WRITE (6,904) WORD(JWT),(NINT(LAT_DG(J,J1)),J=JM,J1,-INC)
      WRITE (6,905) (DASH,J=J1,JM,INC)
      DO 260 L=3,1,-1
      FGLOB=0.
      DO 250 JHEMI=1,2
      AHEM(JHEMI)=0.
      DO 240 JH=1,JMHALF
      J=(JHEMI-1)*(JMHALF-J0)+JH-J0
      FLATJ=ARQX(J,L)*SCALER*SCALJR(J)*SCALLR(L)
         XJL(J,L+KMAX) = FLATJ
      MLAT(J)=NINT(FLATJ)
  240 AHEM(JHEMI)=AHEM(JHEMI)+FLATJ*WTJ(J,JWT,J1)
  250 FGLOB=FGLOB+AHEM(JHEMI)/JWT
         XJL(JM+3,L+KMAX)=AHEM(1)   ! SOUTHERN HEM
         XJL(JM+2,L+KMAX)=AHEM(2)   ! NORTHERN HEM
         XJL(JM+1,L+KMAX)=FGLOB     ! GLOBAL
  260 WRITE (6,902) PM(L+LM),FGLOB,AHEM(2),AHEM(1),
     *  (MLAT(J),J=JM,J1,-INC)
      GO TO 100
  901 FORMAT ('0',30X,A64,'  CP'/1X,30('-'),24A4)
  902 FORMAT (1X,F5.1,3F8.1,1X,24I4)
  903 FORMAT (A6,3F8.1,1X,24I4)
  904 FORMAT (' P(MB) ',A4,' G      NH      SH  ',24I4)
  905 FORMAT (1X,30('-'),24A4)
  907 FORMAT ('1',27A4,I4,1X,A3,I3,' TO',I3,1X,A3,I3)
      END

      BLOCK DATA BD_DIAGJL
c  **
c  ** TITLES FOR SUBROUTINE DIAGJL
c  **
      COMMON/DJLTTL/
     *  TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6,TITLE7,
     *  TITLE8,TITLE9,TITLEA,TITLEB,TITLEC,TITLED,TITLEE
                CHARACTER*64 TITLE1(13)/
c  **                                                              1-13
     1'TEMPERATURE (DEGREES CENTIGRADE)  ',
     2'HEIGHT (HUNDREDS OF METERS) ',
     3'SPECIFIC HUMIDITY (10**-5 KG H2O/KG AIR) ',
     4'RELATIVE HUMIDITY (PERCENT) ',
     5'ZONAL WIND (U COMPONENT) (TENTHS OF METERS/SECOND)',
     6'MERIDIONAL WIND (V COMPONENT) (HUNDREDTHS OF METERS/SECOND)',
     7'STREAM FUNCTION (10**9 KILOGRAMS/SECOND) ',
     8'VERTICAL VELOCITY (10**-5 MILLIBARS/SECOND) ',
     9'BAROCLINIC EDDY KINETIC ENERGY GEN. (10**-1 WATTS/M**2/SIGMA)',
     A'VERTICAL MASS EXCHANGE FROM MOIST CONVECTION (10**9 KG/SECOND)',
     B'SOLAR RADIATION HEATING RATE (HUNDREDTHS OF DEGREES KELVIN/DAY)',
c    *   'EES KELVIN/DAY)',
     C'THERMAL RADIATION COOLING RATE (HUNDREDTHS OF DEGREES KELVN/DAY)'
     *,
     D'TOTAL RADIATION COOLING RATE (10**13 WATTS/UNIT SIGMA)'/
                CHARACTER*64 TITLE2(8)/
c  **                                                             14-21
     1'HEATING BY LARGE SCALE CONDENSATION (10**13 WATTS/UNIT SIGMA)',
     2'HEATING BY DRY CONVECTION (10**13 WATTS/UNIT SIGMA)',
     3'DRY HEATING BY MOIST CONVECTION (10**13 WATTS/UNIT SIGMA)',
     4'STANDING EDDY KINETIC ENERGY (10**4 JOULES/M**2/UNIT SIGMA)',
     5'EDDY KINETIC ENERGY (10**4 JOULES/M**2/UNIT SIGMA)',
     6'TOTAL KINETIC ENERGY (10**4 JOULES/M**2/UNIT SIGMA)',
     7'AVAILABLE POTENTIAL ENERGY (10**5 JOULES/M**2/UNIT SIGMA)',
     8'POTENTIAL TEMPERATURE (DEGREES KELVIN) '/
                CHARACTER*64 TITLE3(7)/
c  **                                                             22-28
     2'NOR. TRANS. OF DRY STAT. ENERGY BY STAND. EDDIES (10**14 W/DSIG)'
     *,
     3'NORTH. TRANS. OF DRY STATIC ENERGY BY EDDIES (10**14 WATTS/DSIG)'
     *,
     4'TOTAL NORTH. TRANSPORT OF DRY STATIC ENERGY (10**15 WATTS/DSIG)'
     *,
     5'NORTHWARD TRANSPORT OF LATENT HEAT BY EDDIES (10**13 WATTS/DSIG)'
     *,
     5'TOTAL NORTHWARD TRANSPORT OF LATENT HEAT (10**14 WATTS/UNIT SIG)'
     *,
     6'NORTH.TRANSPORT OF STATIC ENERGY BY EDDIES (10**14 WATTS/DSIGMA)'
     *,
     9'TOTAL NORTHWARD TRANSPORT OF STATIC ENERGY (10**15 WATTS/DSIGMA)'
     */
                CHARACTER*64 TITLE4(5)/
c  **                                                             29-33
     9'NORTH. TRANSPORT OF KINETIC ENERGY BY EDDIES (10**12 WATTS/DSIG)'
     *,
     *'TOTAL NORTHWARD TRANSPORT OF KINETIC ENERGY (10**12 WATTS/DSIG)'
     *,
     1'NORTH. TRANS. OF ANG. MOMENTUM BY STAND. EDDIES (10**18 J/DSIG)'
     *,
     2'NORTH. TRANS. OF ANG. MOMENTUM BY EDDIES (10**18 JOULES/DSIGMA)'
     *,
     *'TOTAL NORTHWARD TRANSPORT OF ANG. MOMENTUM (10**19 JOULES/DSIG)'
     */
                CHARACTER*64 TITLE5(6)/
c  **                                                             34-39
     4'VERT. TRANS. OFDRY STATIC ENERGY BY EDDIES (10**12 WATTS)',
     5'TOTAL LARGE SCALE VERT. TRANS. OF DRY STAT. ENER. (10**14 WATTS)'
     *,
     6'VERTICAL TRANSPORT OF LATENT HEAT BY EDDIES (10**12 WATTS)',
     7'TOTAL LARGE SCALE VERT. TRANS. OF LATENT HEAT (10**13 WATTS)',
     8'VERTICAL TRANSPORT OF STATIC ENERGY BY EDDIES (10**13 WATTS)',
     9'TOTAL LARGE SCALE VERT. TRANS. OF STATIC ENERGY (10**14 WATTS)'/
                CHARACTER*64 TITLE6(4)/
c  **                                                             40-43
     *'DU/DT   TOTAL CHANGE  (10**-6 M/S/S)                 CP',
     1'TOTAL LARGE SCALE VERT. TRANS. OF KINETIC ENERGY (10**11 WATTS)'
     *,
     2'VERT. TRANS. OFANG. MOMENTUM BY EDDIES (10**16JOULES)',
     3'TOTAL LARGE SCALE VERT. TRANS. OF ANG. MOMENTUM (10**18 JOULES)'/
                 CHARACTER*64 TITLE7(9)/
c  **                                                             44-52
     4'CHANGE OF ANG. MOMENTUM BY DRY CONVEC (10**18 JOULES/UNIT SIGMA)'
     *,
     5'CHANGE OF ANG. MOMENTUM BY MOIST CONV (10**18 JOULES/UNIT SIGMA)'
     *,
     6'CHANGE OF ANG. MOMENTUM BY DIFFUSION (10**18 JOULES/UNIT SIGMA)',
c    7'U WIND AVERAGED OVER I=5-9 (TENTHS OF METERS/SECOND)',
     7'NORTHWARD ELIASSEN-PALM FLUX (10**17 JOULES/UNIT SIGMA)'
     8,'V WIND AVERAGED OVER I=5-9 (TENTHS OF METERS/SECOND)',
     9'VERTICAL VELOCITY FOR I=5-9 (10**-5 METERS/SECOND)',
c    A'U WIND AVERAGED OVER I=35-3 (TENTHS OF METERS/SECOND)',
     A'VERTICAL ELIASSEN-PALM FLUX (10**17 JOULES) ',
     B'V WIND AVERAGED OVER I=35-3 (TENTHS OF METERS/SECOND)',
     C'VERTICAL VELOCITY FOR I=35-3 (10**-5 METERS/SECOND)'/
                CHARACTER*64 TITLE8(8)/
c  **                                                             53-60
     3'POTENTIAL VORTICITY (10**-6 K/(MB-S)) ',
     4'NORTHWARD TRANSPORT OF Q-G POT. VORTICITY  (10**18 JOULES/DSIG)',
     1'P-K BY EDDY PRESSURE GRADIENT FORCE  (10**-1 W/M**2/UNIT SIGMA)',
     6'Q-G POT. VORTICITY CHANGE OVER LATITUDES (10**-12 1/(SEC-M))',
     7'TRANSFORMED STREAM FUNCTION  (10**9 KILOGRAMS/SECOND)',
     8'DYNAMIC CONVERGENCE OF EDDY GEOPOTENTIAL (.1 WATTS/M**2/DSIGMA)',
     *'DU/DT BY MEAN ADVECTION  (10**-6 M/S/S)              CP',
     *'DU/DT BY EDDY CONVERGENCE  (10**-6 M/S/S)            CP'/
                CHARACTER*64 TITLE9(12)/
c  **                                                             61-72
     1'STREAM FUNCTION (10**9 KILOGRAMS/SECOND)             CP',
     3'VERTICAL VELOCITY (10**-5 MILLIBARS/SECOND) ',
     3'POTENTIAL VORTICITY (10**-6 K/(MB-S))                CP',
     4'DU/DT BY TRANSFORMED ADVECTION  (10**-6 M/S/S)       CP',
     5'DU/DT BY ELIASSEN-PALM DIVERGENCE  (10**-6 M/S/S)    CP',
     6'DTEMP/DT   TOTAL CHANGE  (10**-1 DEG-K/DAY)          CP',
     7'DTEMP/DT BY MEAN ADVECTION  (10**-1 DEG-K/DAY)       CP',
     8'STANDARD DEVIATION OF PRESSURE DIFFERENCES  (MB) ',
     9'DTEMP/DT BY EDDY CONVERGENCE  (10**-1 DEG-K/DAY)     CP',
     O'DTEMP/DT BY TRANSFORMED ADVECTION (10**-1 DEG-K/DAY) CP',
     1'REFRACTION INDEX FOR WAVE NUMBER 1  (10**-8 PER METER**2)',
     2'REFRACTION INDEX FOR WAVE NUMBER 2  (10**-8 PER METER**2)'/
                CHARACTER*64 TITLEA(11)/
c  **                                                             73-83
     3'REFRACTION INDEX FOR WAVE NUMBER 3  (10**-8 PER METER**2)',
     4'REFRACTION INDEX FOR WAVE NUMBER 6  (10**-8 PER METER**2)',
     5'REFRACTION INDEX FOR WAVE NUMBER 9  (10**-8 PER METER**2)',
     6'    ',
     7'TOTAL CLOUD COVER (PERCENT)  ',
     8'SUPER SATURATION CLOUD COVER (PERCENT) ',
     9'MOIST CONVECTIVE CLOUD COVER (PERCENT) ',
     O'AMPLITUDE OF GEOPOTENTIAL HEIGHT FOR WAVE NUMBER 1 (METERS)',
     1'AMPLITUDE OF GEOPOTENTIAL HEIGHT FOR WAVE NUMBER 2 (METERS)',
     2'AMPLITUDE OF GEOPOTENTIAL HEIGHT FOR WAVE NUMBER 3 (METERS)',
     3'AMPLITUDE OF GEOPOTENTIAL HEIGHT FOR WAVE NUMBER 4 (METERS)'/
                CHARACTER*64 TITLEB(9)/
c  **                                                             84-92
     4'PHASE OF GEOPOTENTIAL HEIGHT FOR WAVE NUMBER 1 (DEG WEST LONG)',
     5'PHASE OF GEOPOTENTIAL HEIGHT FOR WAVE NUMBER 2 (DEG WEST LONG)',
     6'PHASE OF GEOPOTENTIAL HEIGHT FOR WAVE NUMBER 3 (DEG WEST LONG)',
     7'PHASE OF GEOPOTENTIAL HEIGHT FOR WAVE NUMBER 4 (DEG WEST LONG)',
     8'NORTH. TRANS. OF SENSIBLE HEAT BY EDDIES (10**14 WATTS/DSIGMA)',
c    9'TOTAL NORTHWARD TRANSPORT OF SENSIBLE HEAT (10**15 WATTS/DSIGMA)'
c    *,
     9'NUMBER OF GRIDPOINTS INCLUDED IN AVERAGE             CP',
     O'VERT. TRANS. OF GEOPOTENTIAL ENERGY BY EDDIES (10**12 WATTS)',
c    1'TOTAL LARGE SCALE VERT. TRANS. OF GEOPOTEN. ENER. (10**14 WATTS)'
c    *,
     1'PRESSURE DIFFERENCES  (MB)                           CP',
     2'SUBGRID SCALE TEMPERATURE DEVIATION (HUNDREDTHS OF DEGREES KEL.)'
     */
                CHARACTER*64 TITLEC(6)/
c  **                                                             93-98
     3'DYNAMIC CONVERGENCE OF DRY STATIC ENERGY (10 WATTS/M**2/DSIGMA)',
     4'DIVERGENCE OF THE ELIASSEN-PALM FLUX (10**17 JOULES/UNIT SIGMA)',
     5'    ',
     6'    ',
     7'CHANGE OF LATENT HEAT BY MST CNV BEFORE CNDNS (10*13 WATTS/DSIG)'
     *,
     8'CHANGE OF PHASE HEATING BY MOIST CONVECTION (10*13 WATTS/DSIG)'/
                CHARACTER*64 TITLED(8)/
c  **                                                            99-106
     1'VERT. TRANS. OF GEOPOTENTIAL ENERGY BY EDDIES (10**12 WATTS)  CP'
     *,
     2'VERT. TRANS. OF DRY STATIC ENERGY BY EDDIES (10**12 WATTS)    CP'
     *,
     3'TOTAL LGE SCALE VERT. TRANS. OF DRY STAT. ENER. (10**14 WATTS)CP'
     *,
     4'VERTICAL TRANSPORT OF LATENT HEAT BY EDDIES (10**12 WATTS)    CP'
     *,
     5'TOTAL LARGE SCALE VERT. TRANS. OF LATENT HEAT (10**13 WATTS)  CP'
     *,
     6'VERTICAL TRANSPORT OF STATIC ENERGY BY EDDIES (10**13 WATTS)  CP'
     *,
     7'TOTAL LARGE SCALE VERT. TRANS. OF STATIC ENERGY (10**14 WATTS)CP'
     *,
     8'TRANSFORMED STREAM FUNCTION  (10**9 KG/SEC)                   CP'
     */
                CHARACTER*64 TITLEE(8)/
c  **                                                           107-114
     7'VERT. TRANSPORT OF POTENTIAL VORTICITY (10**4 KG-DEG K/MB/S/S)CP'
     *,
     8'VERT. TRANS. OF POT. VORT. BY EDDIES (10**4 KG-DEG K/MB/S/S)  CP'
     *,
     9 ' ',
     A'TOTAL LGE SCALE VERT. TRANS. OF KINETIC ENERGY (10**11 WATTS) CP'
     *,
     5'VERT. TRANS. OF ANG. MOMENTUM BY EDDIES (10**16 JOULES)       CP'
     *,
     6'TOTAL LGE SCALE VERT. TRANS. OF ANG. MOMENTUM (10**18 JOULES) CP'
     *,
     7'VERTICAL ELIASSEN-PALM FLUX (10**11 JOULES/METER)    CP',
     E ' '/
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE DIAGJL
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      COMMON/WORK2/SENDEG(IM,JM),CN(2,IMH+1),BYP(JM),BYPV(JM),
     *  BYDAPO(JM),BYPDA(JM),BYDXYP(JM),DXCOSV(JM),
     *  DACOSV(JM),DXYPPO(JM),ONES(JM),ONESPO(JM),BYDPS(3),BYPKS(3),
     *  AX(JM,LM),ARQX(JM,3),BX(JM,LM),CX(JM,LM),DX(JM,LM),
     *  AMPLTD(JM,8,4),PHASE(JM,8,4)
      COMMON/DJLCOM/JLAT(JM,2),WTJ(JM,2,2),
     *  LINECT,JMHALF,INC,IHOUR0,IHOUR
      DIMENSION PL(LM+3),PLE(LM+1),PKM(LM),BYDSIG(LM),BYD2SG(LM),
     *  PMB(7),MW(5)
      DATA MW/1,2,3,6,9/
      DATA PMB/999.9,850.,700.,500.,300.,100.,30./,P1000/1000./
c  ** INITIALIZE CERTAIN QUANTITIES
      XWON=TWOPI/(DLON*FIM)
      INC=1+(JM-1)/24
      JMHALF=JM/2
      BYIM=1./FIM
      BY100G=.01/GRAV
      SHA=RGAS/KAPA
      DTCNDS=NCNDS*DT
      P1000K=EXPBYK(P1000)
      KM=0
      DO 5 K=1,7
      IF (PTOP.GT.PMB(K)) GO TO 6
    5 KM=KM+1
    6 ELOFIM=.5*TWOPI-TWOPI/FIM
      DO 20 L=1,LM
      LUP=L+1
      LDN=L-1
      IF (L.EQ.LM) LUP=LM
      IF (L.EQ.1) LDN=1
      BYD2SG(L)=1./(SIG(LUP)-SIG(LDN))
      BYDSIG(L)=1./DSIG(L)
   20 PL(L)=SIG(L)*(PSF-PTOP)+PTOP
      PL(LM+1)=.75*PTOP
      PL(LM+2)=.35*PTOP
      PL(LM+3)=.1*PTOP
      BYDPS(1)=1./(.5*PTOP)
      BYDPS(2)=1./(.3*PTOP)
      BYDPS(3)=1./(.2*PTOP)
      BYPKS(1)=1./(.75*PTOP)**KAPA
      BYPKS(2)=1./(.35*PTOP)**KAPA
      BYPKS(3)=1./(.1*PTOP)**KAPA
      DO 30 L=1,LM
   30 PLE(L)=SIGE(L+1)*(PSF-PTOP)+PTOP
      DO 40 J=1,JM
      DXYPPO(J)=DXYP(J)
      BYDXYP(J)=1./DXYP(J)
      BYDAPO(J)=BYDXYP(J)
      ONES(J)=1.
      ONESPO(J)=1.
      JLAT(J,1)=INT(.5+(J-1.0)*180./(JM-1))-90
      JLAT(J,2)=INT(.5+(J-1.5)*180./(JM-1))-90
      WTJ(J,1,1)=1.
   40 WTJ(J,2,1)=2.*FIM*DXYP(J)/AREAG
      DXYPPO(JM)=DXYP(JM)*FIM
      DXYPPO(1)=DXYP(1)*FIM
      BYDAPO(1)=BYDAPO(1)*FIM
      BYDAPO(JM)=BYDAPO(JM)*FIM
      ONESPO(1)=FIM
      ONESPO(JM)=FIM
      DO 50 J=2,JM
      DXCOSV(J)=DXV(J)*COSV(J)
      DACOSV(J)=DXYV(J)*COSV(J)
      WTJ(J,1,2)=1.
   50 WTJ(J,2,2)=2.*FIM*DXYV(J)/AREAG
      WTJ(JMHALF+1,1,2)=.5
      WTJ(JMHALF+1,2,2)=WTJ(JMHALF+1,2,2)/2.
      IHOUR0=TOFDY0+.5
      IHOUR=TOFDAY+.5
      LINECT=65
      BYIACN=1./(IDACC(1)+1.D-20)
      BYIARD=1./(IDACC(2)+1.D-20)
      BYIADA=1./(IDACC(4)+1.D-20)
      BYIMDA=BYIADA*BYIM
      FIMDA=IDACC(4)*FIM
      DO 120 J=1,JM
      BYPDA(J)=1./(APJ(J,1)*DXYP(J)+1.D-20)
      BYP(J)=1./(APJ(J,1)+1.D-20)
  120 BYPV(J)=1./(APJ(J,2)+1.D-20)
c  **
c  ** PROGNOSTIC QUANTITIES
c  **
c  ** TEMPERATURE, HEIGHT, SPECIFIC HUMIDITY, AND RELATIVE HUMIDITY
c     CALL JLMAPS (1,PL,AJL,ONES,BYP,ONES,LM,2,1,
c    *  ASJL,BYIMDA,ONESPO,ONES)
c     SCALES=BYIMDA*BY100G
c     CALL JLMAPS (2,PL,AJL(1,1,2),BY100G,BYP,ONES,LM,2,1,
c    *  ASJL(1,1,2),SCALES,ONESPO,ONES)
c     SCALE=1.D5
c     CALL JLMAP (3,PL,AJL(1,1,3),SCALE,BYP,ONES,LM,2,1)
c     SCALE=100.
c     CALL JLMAP (4,PL,AJL(1,1,18),SCALE,BYP,ONES,LM,2,1)
c  ** U WIND, V WIND, AND STREAM FUNCTION
c     SCALE=10.
c     CALL JLMAP (5,PL,AJL(1,1,4),SCALE,BYPV,ONES,LM,2,2)
c     SCALE=100.
c     CALL JLMAP (6,PL,AJL(1,1,5),SCALE,BYPV,ONES,LM,2,2)
c     DO 220 J=2,JM
c     AX(J,1)=AJL(J,1,5)*DSIG(1)
c     BX(J,1)=(AJL(J,1,5)-.5*AJL(J,1,47)*FIM)*DSIG(1)
c     DO 220 L=2,LM
c     BX(J,L)=BX(J,L-1)+(AJL(J,L,5)-.5*AJL(J,L,47)*FIM)*DSIG(L)
c 220 AX(J,L)=AX(J,L-1)+AJL(J,L,5)*DSIG(L)
c     SCALE=25.D-9*BYIADA/GRAV
c     CALL JLMAP (7,PLE,AX,SCALE,DXV,ONES,LM,2,2)
c     CALL JLMAP (57,PLE,BX,SCALE,DXV,ONES,LM,2,2)
c  ** VERTICAL VELOCITY AND MASS FLUX MOIST CONVECTION
c     SCALE=-1.D5*BYIMDA
c     CALL JLMAP (8,PLE,AJL(1,1,6),SCALE,BYDAPO,ONES,LM-1,2,1)
      SCALE=100.D-9*XWON*BYIACN/(GRAV*DTCNDS)
      CALL JLMAP (10,PLE,AJL(1,1,8),SCALE,DXYPPO,ONES,LM-1,1,1)
c  **
c  ** RADIATION, CONDENSATION AND CONVECTION
c  **
c  ** SOLAR AND THERMAL RADIATION HEATING
      SCALE=100.D-2*GRAV*SDAY*IDACC(4)*BYIARD/SHA
      SCALES=100.D-2*GRAV*SDAY*BYIM*BYIARD/SHA
      CALL JLMAPS (11,PL,AJL(1,1,9),SCALE,BYP,BYDSIG,LM,2,1,
     *  ASJL(1,1,3),SCALES,ONESPO,BYDPS)
      SCALES=-SCALES
      SCALE=-SCALE
      CALL JLMAPS (12,PL,AJL(1,1,10),SCALE,BYP,BYDSIG,LM,2,1,
     *  ASJL(1,1,4),SCALES,ONESPO,BYDPS)
      DO 250 J=1,JM
      DO 240 LS=1,3
  240 ARQX(J,LS)=ASJL(J,LS,3)+ASJL(J,LS,4)
      DO 250 L=1,LM
  250 AX(J,L)=AJL(J,L,9)+AJL(J,L,10)
      SCALE=-1.D-13*XWON*BYIARD
      SCALES=SCALE*(PSF-PTOP)
      CALL JLMAPS (13,PL,AX,SCALE,DXYPPO,BYDSIG,LM,1,1,
     *  ARQX,SCALES,DXYPPO,BYDPS)
c  ** TOTAL, SUPER SATURATION, AND CONVECTIVE CLOUD COVER
      SCALE=100.*BYIARD*BYIM
      CALL JLMAP (77,PL,AJL(1,1,19),SCALE,ONESPO,ONES,LM,2,1)
      CALL JLMAP (78,PL,AJL(1,1,28),SCALE,ONESPO,ONES,LM,2,1)
      CALL JLMAP (79,PL,AJL(1,1,29),SCALE,ONESPO,ONES,LM,2,1)
c  ** SUBGRID SCALE TEMPERATURE DEVIATION
      SCALE=1.D2*SQRT(2.)*BYIACN
      CALL JLMAP (92,PL,AJL(1,1,54),SCALE,ONES,ONES,LM,2,1)
c  ** HEATING BY LARGE SCALE CONDENSATION AND DRY AND MOIST CONVECTION
      SCALE=100.D-13*XWON*SHA*BYIACN/(GRAV*DTCNDS)
      CALL JLMAP (14,PL,AJL(1,1,11),SCALE,DXYPPO,ONES,LM,1,1)
      CALL JLMAP (15,PL,AJL(1,1,12),SCALE,DXYPPO,ONES,LM,1,1)
      CALL JLMAP (16,PL,AJL(1,1,13),SCALE,DXYPPO,ONES,LM,1,1)
      CALL JLMAP (98,PL,AJL(1,1,50),SCALE,DXYPPO,ONES,LM,1,1)
      CALL JLMAP (97,PL,AJL(1,1,51),SCALE,DXYPPO,ONES,LM,1,1)
c  **
c  ** ENERGY
c  **
c  ** AVAILABLE POTENTIAL ENERGY
      SCALE=50.D-5*RGAS*BYIMDA/GRAV
      CALL JLMAP (20,PL,AJL(1,1,16),SCALE,ONES,ONES,LM,2,1)
c  **
c  ** NORTHWARD TRANSPORTS
c  **
c  ** NOR. TRANSPORT OF QUASI-GEOSTROPHIC POT. VORTICITY BY EDDIES
      DO 366 L=1,LM
      CX(1,L)=0.
      CX(2,L)=DXCOSV(2)*(AJL(2,L,49)-AJL(2,L,48))+.25*FIM*F(2)*
     *  COSP(2)*(AJL(2,L,47)+AJL(3,L,47))
      DO 364 J=3,JM-1
      DAM4=DXCOSV(J)*(AJL(J,L,49)-AJL(J,L,48))
      CX(J,L)=DAM4+.25*FIM*F(J)*COSP(J)*(AJL(J,L,47)+AJL(J-1,L,47))
      CX(J-1,L)=CX(J-1,L)-DAM4
  364 CONTINUE
      CX(JM-1,L)=CX(JM-1,L)-DXCOSV(JM)*(AJL(JM,L,49)-AJL(JM,L,48))
      CX(JM,L)=0.
  366 CONTINUE
      SCALE=25.D-18*XWON*BYIADA*RADIUS/GRAV
      CALL JLMAP (54,PL,CX,SCALE,ONES,ONES,LM,1,1)
c  **
c  ** VERTICAL TRANSPORTS
c  **
c  ** VERTICAL TRANSPORT OF ANGULAR MOMENTUM BY SMALL SCALE MOTIONS
      SCALE=100.D-18*XWON*RADIUS*BYIACN/(GRAV*DTCNDS)
      CALL JLMAP (44,PL,AJL(1,1,38),SCALE,DACOSV,ONES,LM,1,2)
      CALL JLMAP (45,PL,AJL(1,1,39),SCALE,DACOSV,ONES,LM,1,2)
c     CALL JLMAP (46,PL,AJL(1,1,40),SCALE,DACOSV,BYDSIG,LM,1,2)
c        IF (JM.NE.24) GO TO 500
c  **
c  ** MERIDIONAL LUNES
c  **
c  ** U, V AND W VELOCITY FOR I=5-9
      SCALE=.2E+1*BYIADA
c     CALL JLMAP (47,PL,AJL(1,1,41),SCALE,ONES,ONES,LM,2,2)
      CALL JLMAP (48,PL,AJL(1,1,42),SCALE,ONES,ONES,LM,2,2)
      SCALE2=-1.D5*BYIADA*RGAS/(5.*GRAV)
      CALL JLMAP (49,PLE,AJL(1,1,43),SCALE2,BYDXYP,ONES,LM-1,2,1)
c  ** U, V AND W VELOCITY FOR I=35-3
c     CALL JLMAP (50,PL,AJL(1,1,44),SCALE,ONES,ONES,LM,2,2)
      CALL JLMAP (51,PL,AJL(1,1,45),SCALE,ONES,ONES,LM,2,2)
      CALL JLMAP (52,PLE,AJL(1,1,46),SCALE2,BYDXYP,ONES,LM-1,2,1)
  500 CONTINUE
c  **
c  ** ELIASSEN-PALM FLUX : NORTHWARD, VERTICAL, DIVERGENCE
c  **
      SCALE=100.D-17*XWON*BYIADA*RADIUS/GRAV
      CALL JLMAP (47,PL,AJL(1,1,41),SCALE,DXCOSV,ONES,LM,1,2)
      SCALEV=.125*SCALE
      CALL JLMAP (50,PLE,AJL(1,1,44),SCALEV,COSP,ONES,LM-1,1,1)
      DXCVS=DXCOSV(2)
      DO 540 J=2,JM-1
      BDN=0.
      DXCVN=DXCOSV(J+1)
      DO 530 L=1,LM
      BUP=AJL(J,L,44)*COSP(J)
      AX(J,L)=AJL(J+1,L,41)*DXCVN-AJL(J,L,41)*DXCVS+
     *   .125*(BUP-BDN)/DSIG(L)
  530 BDN=BUP
  540 DXCVS=DXCVN
      DO 550 L=1,LM
      AX(1,L)=0.
  550 AX(JM,L)=0.
      CALL JLMAP (94,PL,AX,SCALE,ONES,ONES,LM,1,1)
c  **
c  ** FOURIER ANALYSIS OF GEOPOTENTIAL HEIGHTS FOR WAVE NUMBERS 1 TO 4,
c  **   AMPLITUDE AND PHASE
c  **
            LINECT=63
      DO 620 K=1,KM
      DO 610 N=1,4
      AMPLTD(1,K,N)=0.
      AMPLTD(JM,K,N)=0.
      PHASE(1,K,N)=0.
  610 PHASE(JM,K,N)=0.
      DO 620 J=2,JM-1
      CALL GETAN (AIJ(1,J,8+K),CN)
      DO 620 N=1,4
      AMPLTD(J,K,N)=SQRT(CN(1,N+1)*CN(1,N+1)+CN(2,N+1)*CN(2,N+1))
      PHASE(J,K,N)=(ATAN2(CN(2,N+1),CN(1,N+1))-TWOPI)/N+ELOFIM
      IF (PHASE(J,K,N).LE.-.5*TWOPI) PHASE(J,K,N)=PHASE(J,K,N)+TWOPI
      PHASE(J,K,N)=-PHASE(J,K,N)
  620 CONTINUE
      SCALE=BYIADA/GRAV
      DO 630 N=1,4
  630 CALL JLMAP (N+79,PMB,AMPLTD(1,1,N),SCALE,ONES,ONES,KM,2,1)
      SCALE=360./TWOPI
      DO 640 N=1,4
  640 CALL JLMAP (N+83,PMB,PHASE(1,1,N),SCALE,ONES,ONES,KM,2,1)
      IF (KDIAG(10).EQ.0) CALL DIAGIL
      RETURN
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE JLMAP (NT,PL,AX,SCALE,SCALEJ,SCALEL,LMAX,JWT,J1)
c  **
c  ** THIS SUBROUTINE PRODUCES LAYER BY LATITUDE TABLES ON THE LINE
c  ** PRINTER.  THE INTERIOR NUMBERS OF THE TABLE ARE CALCULATED AS
c  **               AX * SCALE * SCALEJ * SCALEL.
c  ** WHEN JWT=1, THE INSIDE NUMBERS ARE NOT AREA WEIGHTED AND THE
c  **    HEMISPHERIC AND GLOBAL NUMBERS ARE SUMMATIONS.
c  ** WHEN JWT=2, ALL NUMBERS ARE PER UNIT AREA.
c  ** J1 INDICATES PRIMARY OR SECONDARY GRID.
c  ** THE BOTTOM LINE IS CALCULATED AS THE SUMMATION OF DSIG TIMES THE
c  ** NUMBERS ABOVE (POSSIBLY MULTIPLIED BY A FACTOR OF 10)
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      include 'pd_COMMON'
      REAL*4 XJL(JM+3,LM+4)    ! Save
      CHARACTER XLB*16,CLAT*16/'LATITUDE'/,CPRES*16/'PRESSURE (MB)'/,
     *  CBLANK*16/' '/
      COMMON/WORK4/MLAT(JM),FLAT(JM),ASUM(JM),FHEM(2),HSUM(2)
      COMMON/DJLCOM/JLAT(JM,2),WTJ(JM,2,2),
     *  LINECT,JMHALF,INC,IHOUR0,IHOUR
      COMMON/DJLTTL/TITLE(1)
      DIMENSION AX(JM,*),ARQX(JM,*)
      DIMENSION PL(*),SCALEJ(*),SCALEL(*),SCALJR(*),SCALLR(*)
      CHARACTER*4 DASH,WORD(4),TITLE*64
      DATA DASH/'----'/,WORD/'SUM','MEAN',' ','.1*'/
c  **
c  ** PRODUCE A LATITUDE BY LAYER TABLE OF THE ARRAY A
c  **
   10 LINECT=LINECT+LMAX+7
      IF (LINECT.LE.60) GO TO 20
      JY0=JYEAR0-1900
      JY=JYEAR-1900
      WRITE (6,907) (XLABEL(K),K=1,27),JDATE0,JMNTH0,JY0,JDATE,JMONTH,JY
      LINECT=LMAX+8
   20 WRITE (6,901) TITLE(NT),(DASH,J=J1,JM,INC)
      WRITE (6,904) WORD(JWT),(NINT(LAT_DG(J,J1)),J=JM,J1,-INC)
      WRITE (6,905) (DASH,J=J1,JM,INC)
      J0=J1-1
         DO 40 L=1,LM+4
         DO 40 J=1,JM+3
   40    XJL(J,L) = -1.E30
         KSX = 0            ! KSX = LAYERS GENERATED AT ENTRY
  100 SDSIG=1.-SIGE(LMAX+1)
         KLMAX = LMAX  ! +KSX
      DO 110 J=1,JM
  110 ASUM(J)=0.
      HSUM(1)=0.
      HSUM(2)=0.
      GSUM=0.
      SUMFAC=1.
      IWORD=3
      IF (NT.NE.1.AND.NT.NE.6.AND.NT.NE.24.AND.NT.NE.26.AND.NT.NE.28
     *  .AND.NT.NE.33) GO TO 112
      SUMFAC=10.
      IWORD=4
  112 DO 140 L=LMAX,1,-1
      FGLOB=0.
      DO 130 JHEMI=1,2
      FHEM(JHEMI)=0.
      DO 120 JH=1,JMHALF
      J=(JHEMI-1)*(JMHALF-J0)+JH+J0
      FLAT(J)=AX(J,L)*SCALE*SCALEJ(J)*SCALEL(L)
         XJL(J,L) = FLAT(J)
      MLAT(J)=NINT(FLAT(J))
  115 ASUM(J)=ASUM(J)+FLAT(J)*DSIG(L)/SDSIG
  120 FHEM(JHEMI)=FHEM(JHEMI)+FLAT(J)*WTJ(J,JWT,J1)
  130 FGLOB=FGLOB+FHEM(JHEMI)/JWT
         XJL(JM+3,L)=FHEM(1)   ! SOUTHERN HEM
         XJL(JM+2,L)=FHEM(2)   ! NORTHERN HEM
         XJL(JM+1,L)=FGLOB     ! GLOBAL
      WRITE (6,902) PL(L),FGLOB,FHEM(2),FHEM(1),(MLAT(J),J=JM,J1,-INC)
c        IF (NT.EQ.5) CALL KEYJLJ (L,FLAT)
         IF (NT.EQ.61) CALL KEYJLS (L,FLAT)
  136 HSUM(1)=HSUM(1)+FHEM(1)*SUMFAC*DSIG(L)/SDSIG
      HSUM(2)=HSUM(2)+FHEM(2)*SUMFAC*DSIG(L)/SDSIG
  140 GSUM=GSUM+FGLOB*SUMFAC*DSIG(L)/SDSIG
      WRITE (6,905) (DASH,J=J1,JM,INC)
C     IF (NT.GE.80.AND.NT.LE.87) RETURN  !moved. this may be a problem
      ASUM(JMHALF+1)=ASUM(JMHALF+1)/J1
      DO 150 J=J1,JM
  150 MLAT(J)=NINT(ASUM(J)*SUMFAC)
      if (iprint_pd(10).lt.9) then
         DO 180 J=J1,JM
  180    XJL(J   ,LM+4)=ASUM(J)
         XJL(JM+3,LM+4)=HSUM(1)/SUMFAC   ! SOUTHERN HEM
         XJL(JM+2,LM+4)=HSUM(2)/SUMFAC   ! NORTHERN HEM
         XJL(JM+1,LM+4)=GSUM/SUMFAC      ! GLOBAL
         WRITE(XLB,'(1X,A3,I5)') JMNTH0,JYEAR0
         WRITE (iu_pd(10)) TITLE(NT),XLB,JM-J1+1,KLMAX,1,1,
     *     ((XJL(J,L),J=J1,JM),L=1,KLMAX),
     *     (SNGL(LAT_DG(J,J1)),J=J1,JM),(SNGL(PL(L)),L=1,KLMAX),1.,1.,
     *     CLAT,CPRES,CBLANK,CBLANK,'NASAGISS',
     *     (XJL(J,LM+4),J=J1,JM+3),((XJL(J,L),J=JM+1,JM+3),L=1,KLMAX)
      end if
      IF (NT.GE.80.AND.NT.LE.87) RETURN
      WRITE (6,903) WORD(IWORD),GSUM,HSUM(2),HSUM(1),
     *  (MLAT(J),J=JM,J1,-INC)
c        IF (NT.EQ.1) CALL KEYJLT (GSUM,ASUM)
c        IF (NT.EQ.18.OR.NT.EQ.19) CALL KEYJLE (NT,HSUM,ASUM)
c        IF (NT.GE.22.AND.NT.LE.33) CALL KEYJLN (NT,ASUM,SUMFAC)
      RETURN
c  **
      ENTRY JLMAPS (NT,PL,AX,SCALE,SCALEJ,SCALEL,LMAX,JWT,J1,
     *  ARQX,SCALER,SCALJR,SCALLR)
         KSX = 3
         DO 205 L=1,LM+3
         DO 205 J=1,JM
  205    XJL(J,L) = -1.E30
      LINECT=LINECT+LMAX+10
      IF (LINECT.LE.60) GO TO 200
      JY0=JYEAR0-1900
      JY=JYEAR-1900
      WRITE (6,907) (XLABEL(K),K=1,27),JDATE0,JMNTH0,JY0,JDATE,JMONTH,JY
      LINECT=LMAX+11
  200 J0=J1-1
c  ** PRODUCE UPPER STRATOSPHERE NUMBERS FIRST
      WRITE (6,901) TITLE(NT),(DASH,J=J1,JM,INC)
      WRITE (6,904) WORD(JWT),(NINT(LAT_DG(J,J1)),J=JM,J1,-INC)
      WRITE (6,905) (DASH,J=J1,JM,INC)
      DO 230 L=3,1,-1
      FGLOB=0.
      DO 220 JHEMI=1,2
      FHEM(JHEMI)=0.
      DO 210 JH=1,JMHALF
      J=(JHEMI-1)*(JMHALF-J0)+JH-J0
      FLATJ=ARQX(J,L)*SCALER*SCALJR(J)*SCALLR(L)
         XJL(J,L+LMAX) = FLATJ
      MLAT(J)=NINT(FLATJ)
  210 FHEM(JHEMI)=FHEM(JHEMI)+FLATJ*WTJ(J,JWT,J1)
  220 FGLOB=FGLOB+FHEM(JHEMI)/JWT
         XJL(JM+3,L+LMAX)=FHEM(1)   ! SOUTHERN HEM
         XJL(JM+2,L+LMAX)=FHEM(2)   ! NORTHERN HEM
         XJL(JM+1,L+LMAX)=FGLOB     ! GLOBAL
  230 WRITE (6,902) PL(L+LM),FGLOB,FHEM(2),FHEM(1),
     *  (MLAT(J),J=JM,J1,-INC)
      GO TO 100
  901 FORMAT ('0',30X,A64/1X,30('-'),24A4)
  902 FORMAT (F6.1,3F8.1,1X,24I4)
  903 FORMAT (A6,3F8.1,1X,24I4)
  904 FORMAT (' P(MB) ',A4,' G      NH      SH  ',24I4)
  905 FORMAT (1X,30('-'),24A4)
  907 FORMAT ('1',27A4,I4,1X,A3,I3,' TO',I3,1X,A3,I3)
      END

      BLOCK DATA BD_DIAGIL
c  **
c  ** TITLES FOR SUBROUTINE DIAGIL
c  **
      COMMON/DILTTL/TITLE1,TITLE2
                CHARACTER*64 TITLE1(8)/
c  **                                                              1-8
     1'ZONAL WIND (U COMPONENT) IN SOUTH TROPICS (METERS/SECOND)',
     2'MERIDIONAL WIND (V COMPONENT) IN SOUTH TROPICS (METERS/SECOND)',
     3'VERTICAL VELOCITY IN SOUTH TROPICS (10**-4 METERS/SECOND)',
     4'TEMPERATURE IN SOUTH TROPICS (DEGREES CENTIGRADE)',
     5'RELATIVE HUMIDITY IN SOUTH TROPICS (PERCENT) ',
     6'MOIST CONVECTIVE HEATING IN SOUTH TROPICS (10**13 WATTS/DSIG)',
     7'TOTAL RADIATIVE COOLING IN SOUTH TROPICS (10**13 WATTS/DSIG)',
     8' '/
                CHARACTER*64 TITLE2(8)/
c  **                                                              9-16
     9'VERTICAL VELOCITY AT 50 N (10**-4 METERS/SECOND)',
     O'TEMPERATURE AT 50 N (DEGREES CENTIGRADE)',
     1'TOTAL RADIATIVE COOLING AT 50 N (10**13 WATTS/UNIT SIGMA)',
     2'ZONAL WIND AT 50 N (METERS/SECOND)',
     3'VERTICAL VELOCITY AT 70 N (10**-4 METERS/SECOND)',
     4'TEMPERATURE AT 70 N (DEGREES CENTIGRADE)',
     5'TOTAL RADIATIVE COOLING AT 70 N (10**13 WATTS/UNIT SIGMA)',
     6'ZONAL WIND AT 70 N (METERS/SECOND)'/
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE DIAGIL
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      COMMON/DILCOM/JLAT(JM,2),WTJ(JM,2,2),
     *  LINECT,JMHALF,INC,IHOUR0,IHOUR
      DIMENSION PL(LM+3),PLE(LM),ONES(LM),BYDSIG(LM)
c  ** INITIALIZE CERTAIN QUANTITIES
c        IF (JM.NE.24) RETURN
      INC=1+(JM-1)/24
      JMHALF=JM/2
      JEQ=2.+.5*(JM-1)
      J50N=(50.+90.)*(JM-1)/180.+1.5
      J70N=(70.+90.)*(JM-1)/180.+1.5
      SHA=RGAS/KAPA
      DTCNDS=NCNDS*DT
      DO 20 L=1,LM
      ONES(L)=1.
      BYDSIG(L)=1./DSIG(L)
   20 PL(L)=SIG(L)*(PSF-PTOP)+PTOP
      PL(LM+1)=.75*PTOP
      PL(LM+2)=.35*PTOP
      PL(LM+3)=.1*PTOP
      DO 30 L=1,LM
   30 PLE(L)=SIGE(L+1)*(PSF-PTOP)+PTOP
      DO 40 J=1,JM
      JLAT(J,1)=INT(.5+(J-1.0)*180./(JM-1))-90
      JLAT(J,2)=INT(.5+(J-1.5)*180./(JM-1))-90
      WTJ(J,1,1)=1.
   40 WTJ(J,2,1)=2.*FIM*DXYP(J)/AREAG
      DO 50 J=2,JM
      WTJ(J,1,2)=1.
   50 WTJ(J,2,2)=2.*FIM*DXYV(J)/AREAG
      WTJ(JMHALF+1,1,2)=.5
      WTJ(JMHALF+1,2,2)=WTJ(JMHALF+1,2,2)/2.
      IHOUR0=TOFDY0+.5
      IHOUR=TOFDAY+.5
      LINECT=65
      BYIACN=1./(IDACC(1)+1.D-20)
      BYIARD=1./(IDACC(2)+1.D-20)
      BYIADA=1./(IDACC(4)+1.D-20)
c  **
c  ** PROGNOSTIC QUANTITIES
c  **
c  ** U, V AND W VELOCITY FOR J=11-13 VS. LONGITUDE
      SCALE=BYIADA/3.
      CALL ILMAP (1,PL,AIL(1,1,1),SCALE,ONES,LM,2,2)
c     CALL ILMAP (2,PL,AIL(1,1,2),SCALE,ONES,LM,2,2)
      SCALE =-1.D4*BYIADA*RGAS/GRAV/(DXYP(JEQ)+DXYP(JEQ-1)+DXYP(JEQ-2))
      CALL ILMAP (3,PLE,AIL(1,1,3),SCALE,ONES,LM-1,2,1)
c  ** TEMPERATURE, RELATIVE HUMIDITY, MOIS CONVECTIVE HEATING, AND
c  **   RADIATIVE COOLING FOR J=11-13 VS. LONGITUDE
      SCALE=BYIADA/3.
      CALL ILMAP (4,PL,AIL(1,1,4),SCALE,ONES,LM,2,1)
      SCALE=1.D2*SCALE
      CALL ILMAP (5,PL,AIL(1,1,5),SCALE,ONES,LM,2,1)
      SCALE=100.D-13*SHA*BYIACN/(GRAV*DTCNDS)
      CALL ILMAP (6,PL,AIL(1,1,6),SCALE,ONES,LM,1,1)
      SCALE=-1.D-13*BYIARD
      CALL ILMAP (7,PL,AIL(1,1,7),SCALE,BYDSIG,LM,1,1)
c  ** AT J=19: W VELOCITY, TEMPERATURE, RADIATION, AND U VELOCITY
c     SCALE =-1.D4*BYIADA*RGAS/(GRAV* DXYP(J50N))
c     CALL ILMAP (9,PLE,AIL(1,1,9),SCALE,ONES,LM-1,2,1)
      CALL ILMAP (10,PL,AIL(1,1,10),BYIADA,ONES,LM,2,1)
c     SCALE=-1.D-13*BYIARD
c     CALL ILMAP (11,PL,AIL(1,1,11),SCALE,BYDSIG,LM,1,1)
      SCALE=BYIADA/2.
      CALL ILMAP (12,PL,AIL(1,1,12),SCALE,ONES,LM,2,2)
c  ** AT J=21: W VELOCITY, TEMPERATURE, RADIATION, AND U VELOCITY
c     SCALE =-1.D4*BYIADA*RGAS/(GRAV* DXYP(J70N))
c     CALL ILMAP (13,PLE,AIL(1,1,13),SCALE,ONES,LM-1,2,1)
c     CALL ILMAP (14,PL,AIL(1,1,14),BYIADA,ONES,LM,2,1)
c     SCALE=-1.D-13*BYIARD
c     CALL ILMAP (15,PL,AIL(1,1,15),SCALE,BYDSIG,LM,1,1)
c     SCALE=BYIADA/2.
c     CALL ILMAP (16,PL,AIL(1,1,16),SCALE,ONES,LM,2,2)
      RETURN
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE ILMAP (NT,PL,AX,SCALE,SCALEL,LMAX,JWT,ISHIFT)
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      include 'pd_COMMON'
      COMMON/WORK4/MLON(IM),ASUM(IM)
      COMMON/DILCOM/JLAT(JM,2),WTJ(JM,2,2),
     *  LINECT,JMHALF,INC,IHOUR0,IHOUR
      COMMON/DILTTL/TITLE(1)
      DIMENSION AX(IM,*)
      DIMENSION PL(*),SCALEL(*)
      CHARACTER*4 DASH,WORD(2),TITLE*64
      DATA DASH/'----'/,WORD/'SUM','MEAN'/
c  **
c  ** PRODUCE A LONGITUDE BY LAYER TABLE OF THE ARRAY A
c  **
      LINECT=LINECT+LMAX+7
      IF (LINECT.LE.60) GO TO 20
      JY0=JYEAR0-1900
      JY=JYEAR-1900
      WRITE (6,907) (XLABEL(K),K=1,27),JDATE0,JMNTH0,JY0,JDATE,JMONTH,JY
      LINECT=LMAX+8
   20 SDSIG=1.-SIGE(LMAX+1)
      WRITE (6,901) TITLE(NT),(DASH,I=1,IM,INC)
      IF (ISHIFT.NE.2) WRITE (6,904) WORD(JWT),(I,I=1,IM,INC)
      IF (ISHIFT.EQ.2) WRITE (6,906) WORD(JWT),(I,I=1,IM,INC)
      WRITE (6,905) (DASH,I=1,IM,INC)
      DO 110 I=1,IM
  110 ASUM(I)=0.
      GSUM=0.
      DO 130 L=LMAX,1,-1
      FGLOB=0.
      DO 120 I=1,IM
      FLON=AX(I,L)*SCALE*SCALEL(L)
      MLON(I)=NINT(FLON)
  115 ASUM(I)=ASUM(I)+FLON*DSIG(L)/SDSIG
  120 FGLOB=FGLOB+FLON
      FGLOB=FGLOB/IM
      IF (JWT.EQ.1) FGLOB=FGLOB*TWOPI/DLON
      WRITE (6,902) PL(L),FGLOB,(MLON(I),I=1,IM,INC)
  130 GSUM=GSUM+FGLOB*DSIG(L)/SDSIG
      DO 140 I=1,IM
  140 MLON(I)=NINT(ASUM(I))
      WRITE (6,905) (DASH,I=1,IM,INC)
      WRITE (6,903) GSUM,(MLON(I),I=1,IM,INC)
      RETURN
  901 FORMAT ('0',30X,A64/1X,14('-'),36A3)
  902 FORMAT (F6.1,F8.1,1X,36I3)
  903 FORMAT (F14.1,1X,36I3)
  904 FORMAT (' P(MB)',4X,A4,1X,36I3)
  905 FORMAT (1X,14('-'),36A3)
  906 FORMAT (' P(MB)',4X,A4,I2,8I3,I4,26I3)
  907 FORMAT ('1',27A4,I4,1X,A3,I3,' TO',I3,1X,A3,I3)
      END

      BLOCK DATA BD_DIAG7
c  **
c  ** TITLES FOR SUBROUTINE DIAG7
c  **
      COMMON/D7COM/TITLE
                CHARACTER*64 TITLE(12)/
     1'WAVE POWER FOR U NEAR 850 MB AND EQUATOR (DAY*(M/S)**2)',
     2'WAVE POWER FOR V NEAR 850 MB AND EQUATOR (DAY*(M/S)**2)',
     3'WAVE POWER FOR U NEAR 300 MB AND EQUATOR (10 DAY*(M/S)**2)',
     4'WAVE POWER FOR V NEAR 300 MB AND EQUATOR (DAY*(M/S)**2)',
     5'WAVE POWER FOR U NEAR 50 MB AND EQUATOR (10 DAY*(M/S)**2)',
     6'WAVE POWER FOR V NEAR 50 MB AND EQUATOR (DAY*(M/S)**2)',
     7'WAVE POWER FOR PHI AT 922 MB AND 50 DEG NORTH (10**3 DAY*M**2)',
     8'WAVE POWER FOR PHI AT 700 MB AND 50 DEG NORTH (10**3 DAY*M**2)',
     9'WAVE POWER FOR PHI AT 500 MB AND 50 DEG NORTH (10**3 DAY*M**2)',
     A'WAVE POWER FOR PHI AT 300 MB AND 50 DEG NORTH (10**3 DAY*M**2)',
     B'WAVE POWER FOR PHI AT 100 MB AND 50 DEG NORTH (10**4 DAY*M**2)',
     C'WAVE POWER FOR PHI AT 10 MB AND 50 DEG NORTH (10**4 DAY*M**2)'/
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE DIAG7A
c  **
c  ** THIS SUBROUTINE ACCUMULATES A TIME SEQUENCE FOR SELECTED
c  ** QUANTITIES AND FROM THAT PRINTS A TABLE OF WAVE FREQUENCIES.
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      COMMON/WORK3/PHI(IM,JM,LM),HTRD(IM,6)
      COMMON/WORK4/CN(2,IMH+1),POWER(120),IPOWER(44),XPOWER(43),
     *  FPE(13),FPOWER(43,9,3,4),FFPE(13,3,4)
      COMMON/D7COM/TITLE
      CHARACTER*64 TITLE(12)
      DIMENSION JLKDEX(6),SCALE(12),PMB(6),GHT(6)
      DATA KM,PMB/6,922.,700.,500.,300.,100.,10./
      DATA NMAX/9/,KQMAX/12/,MMAX/12/,NUAMAX/120/,NUBMAX/15/
      DATA SCALE/1.,1., .1,1., .1,1., 4*1.D-3,1.D-4,1.D-5/
      DATA GHT/500.,2600.,5100.,8500.,15400.,30000./
      DATA IFIRST/1/
      IDACC9=IDACC(9)+1
      IDACC(9)=IDACC9
      IF (IDACC9.GT.62) RETURN
      NMAX=MIN0(9,IM/2)
      IF (IFIRST.NE.1) GO TO 100
      IFIRST=0
      JEQ=1+JM/2
      J50N=1.5+(140./180.)*(JM-1)
      L850=LM
      L300=LM
      L50=LM
      DO 10 L=2,LM
      LX=LM+1-L
      PLE=.25*(SIGE(LX)+2.*SIGE(LX+1)+SIGE(LX+2))*(PSF-PTOP)+PTOP
      IF (PLE.LT.850.) L850=LX
      IF (PLE.LT.300.) L300=LX
   10 IF (PLE.LT.50.) L50=LX
      WRITE (6,889) L850,L300,L50
  889 FORMAT (' LEVELS FOR WIND WAVE POWER DIAG  L850=',I3,
     *  ' L300=',I3,' L50=',I3)
      JLKDEX(1)=JEQ+JM*(L850-1)
      JLKDEX(2)=JEQ+JM*(L850-1+LM)
      JLKDEX(3)=JEQ+JM*(L300-1)
      JLKDEX(4)=JEQ+JM*(L300-1+LM)
      JLKDEX(5)=JEQ+JM*(L50-1)
      JLKDEX(6)=JEQ+JM*(L50-1+LM)
  100 DO 120 KQ=1,6
      JLK=JLKDEX(KQ)
      CALL GETAN (U(1,JLK,1),CN)
      DO 120 N=1,NMAX
      WAVE(1,IDACC9,N,KQ)=CN(1,N+1)
  120 WAVE(2,IDACC9,N,KQ)=CN(2,N+1)
      DO 150 I=1,IM
      K=1
      L=1
      PL=SIG(1)*P(I,J50N)+PTOP
  130 L=L+1
      PLM1=PL
      PL=SIG(L)*P(I,J50N)+PTOP
      IF (PMB(K).LT.PL.AND.L.LT.LM) GO TO 130
c  ** ASSUME THAT PHI IS LINEAR IN LOG P
      SLOPE=(PHI(I,J50N,L-1)-PHI(I,J50N,L))/LOG(PLM1/PL)
  140 HTRD(I,K)=(PHI(I,J50N,L)+SLOPE*LOG(PMB(K)/PL))/GRAV-GHT(K)
      IF (K.GE.KM) GO TO 150
      K=K+1
      IF (PMB(K).LT.PL.AND.L.LT.LM) GO TO 130
      GO TO 140
  150 CONTINUE
      DO 160 KQ=7,KQMAX
      CALL GETAN(HTRD(1,KQ-6),CN)
      DO 160 N=1,NMAX
      WAVE(1,IDACC9,N,KQ)=CN(1,N+1)
  160 WAVE(2,IDACC9,N,KQ)=CN(2,N+1)
      CALL CLOCKS (MNOW)
      MDIAG=MDIAG+MLAST-MNOW
      MLAST=MNOW
      RETURN
c  **
c  ** THIS ENTRY PRINTS THE TABLES
c  **
      ENTRY DIAG7P
      NMAX=MIN0(9,IM/2)
      IDACC9=IDACC(9)
      IF (IDACC9.LE.MMAX) RETURN
c  ** PATCH NEEDED IF SEVERAL RESTART FILES WERE ACCUMULATED
      IF (IDACC(12).LE.1) GO TO 320
      IDACC9=56
      IA9X=240*IDACC9
      BYIA12=1./IDACC(12)
      DO 310 N=1,IA9X
  310 WAVE(N,1,1,1)=WAVE(N,1,1,1)*BYIA12
  320 CONTINUE
      IF (IDACC9.GT.62) IDACC9=62
c  **
c  ** OUTPUT WAVE POWER AT THE EQUATOR
c  **
      MMAXP1=MMAX+1
      DO 400 KPAGE=1,2
      JY0=JYEAR0-1900
      JY=JYEAR-1900
      WRITE (6,907) (XLABEL(K),K=1,27),JDATE0,JMNTH0,JY0,JDATE,JMONTH,JY
      DO 390 KTABLE=1,3
      KQ=3*(KPAGE-1)+KTABLE
      WRITE (6,901) TITLE(KQ)
      DO 380 NX=1,NMAX
      N=NMAX+1-NX
      CALL MEM (WAVE(1,1,N,KQ),IDACC9,MMAX,NUAMAX,NUBMAX,POWER,FPE,
     *  VAR,PNU)
      POWX=.5*POWER(1)
      DO 330 NUA=2,27
  330 POWX=POWX+POWER(NUA)
      XPOWER(1)=SCALE(KQ)*POWX/26.5
      POWX=0.
      DO 340 NUA=28,34
  340 POWX=POWX+POWER(NUA)
      XPOWER(2)=SCALE(KQ)*POWX/7.
      XPOWER(3)=SCALE(KQ)*(POWER(35)+POWER(36)+POWER(37)+POWER(38))/4.
      XPOWER(4)=SCALE(KQ)*(POWER(39)+POWER(40))/2.
      DO 350 NUA=41,76
  350 XPOWER(NUA-36)=SCALE(KQ)*POWER(NUA)
      POWX=.5*POWER(1)
      DO 360 NUA=77,120
  360 POWX=POWX+POWER(NUA)
      XPOWER(41)=SCALE(KQ)*POWX/44.5
      XPOWER(42)=10.*SCALE(KQ)*VAR
      XPOWER(43)=1000.*SCALE(KQ)*(VAR-PNU)
      DO 370 NS=1,43
         FPOWER(NS,NX,KTABLE,KPAGE)=XPOWER(NS)
      IPOWER(NS)=XPOWER(NS)+.5
  370 CONTINUE
  380 WRITE (6,902) N,(IPOWER(NS),NS=1,43)
         DO 385 M=1,MMAXP1
  385    FFPE(M,KTABLE,KPAGE)=FPE(M)
  390 WRITE (6,903) (FPE(M),M=1,MMAXP1)
  400 CONTINUE
c  **
c  ** OUTPUT WAVE POWER AT 50 DEG NORTH
c  **
      DO 500 KPAGE=3,4
      JY0=JYEAR0-1900
      JY=JYEAR-1900
      WRITE (6,907) (XLABEL(K),K=1,27),JDATE0,JMNTH0,JY0,JDATE,JMONTH,JY
      DO 490 KTABLE=1,3
      KQ=3*(KPAGE-1)+KTABLE
  410 WRITE (6,911) TITLE(KQ)
      DO 480 NX=1,NMAX
      N=NMAX+1-NX
      CALL MEM (WAVE(1,1,N,KQ),IDACC9,MMAX,NUAMAX,NUBMAX,POWER,FPE,
     *  VAR,PNU)
      DO 420 M=1,MMAXP1
  420 FPE(M)=1000.*SCALE(KQ)*FPE(M)
      POWX=.5*POWER(1)
      DO 430 NUA=2,45
  430 POWX=POWX+POWER(NUA)
      XPOWER(1)=SCALE(KQ)*POWX/44.5
      DO 440 NUA=46,81
  440 XPOWER(NUA-44)=SCALE(KQ)*POWER(NUA)
      XPOWER(38)=SCALE(KQ)*(POWER(82)+POWER(83))/2.
      XPOWER(39)=SCALE(KQ)*(POWER(84)+POWER(85)+POWER(86)+POWER(87))/4.
      POWX=0.
      DO 450 NUA=88,94
  450 POWX=POWX+POWER(NUA)
      XPOWER(40)=SCALE(KQ)*POWX/7.
      POWX=.5*POWER(1)
      DO 460 NUA=95,120
  460 POWX=POWX+POWER(NUA)
      XPOWER(41)=SCALE(KQ)*POWX/26.5
      XPOWER(42)=10.*SCALE(KQ)*VAR
      XPOWER(43)=1000.*SCALE(KQ)*(VAR-PNU)
      DO 470 NS=1,43
         FPOWER(NS,NX,KTABLE,KPAGE)=XPOWER(NS)
      IPOWER(NS)=XPOWER(NS)+.5
  470 CONTINUE
  480 WRITE (6,902) N,(IPOWER(NS),NS=1,43)
         DO 485 M=1,MMAXP1
  485    FFPE(M,KTABLE,KPAGE)=FPE(M)
  490 WRITE (6,903) (FPE(M),M=1,MMAXP1)
  500 CONTINUE
      RETURN
c  **
  901 FORMAT ('0',30X,A64,8X,'*1/60 (1/DAY)'/'   PERIOD EASTWARD--',
     *   35('---')/' N    -2      *-3   -3.3      -4       -5    -6   -7
     *.5  -10-12-15-20-30-60    60 30 20 15 12 10    7.5    6     5
     *   4*   VAR ERR'/'   --',40('---'))
  902 FORMAT (I2,41I3,I4,I4)
  903 FORMAT ('   --',40('---')/(1X,13F10.4))
  907 FORMAT ('1',27A4,I4,1X,A3,I3,' T0',I3,1X,A3,I3)
  911 FORMAT ('0',30X,A64,8X,'*1/60 (1/DAY)'/'   PERIOD EASTWARD--',
     *  35('---')/               ' N   *-4       -5    -6   -7.5  -10-12
     *-15-20-30-60    60 30 20 15 12 10    7.5    6     5        4
     * 3.3    3*       2    VAR ERR'/'   --',40('---'))
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE MEM (SERIES,ITM,MMAX,NUAMAX,NUBMAX,POWER,FPE,VAR,PNU)
c  ****
c  **** MFS (CHANGED CODE)
c  **** Changed the model from 64 bit reals to 32 bit reals.  The PowerPC
c  **** 601,603,604,750 all use 32 bit reals.  The double precision were
c  **** better for science but slower.
      IMPLICIT REAL*8 (A-H,O-Z)
C     IMPLICIT REAL*4 (A-H,O-Z)
c  ****
c  **** END (CHANGED CODE)
c  ****
      DIMENSION C(1800),S(1800),B1(62),B2(62),A(12),AA(11),P(13)
      DIMENSION SERIES(*),POWER(*),FPE(*)
c  DOUBLE PRECISION
      DOUBLE PRECISION PI,ARG,PP,POWERX,P,C,S
      COMPLEX*16 CI,CSUM,SS,A,AA,B1,B2,ANOM,ADEN
      COMPLEX*16 SERIES
      PI=3.141592653589793D0
      CI=DCMPLX(0.D0,1.D0)
      MMAXP1=MMAX+1
c  COSINE AND SINE FUNCTION
      NUMAX=NUAMAX*NUBMAX
      DO 20 NU=1,NUMAX
      ARG=2.0*PI*DFLOAT(NU)/DFLOAT(NUMAX)
      C(NU)=DCOS(ARG)
   20 S(NU)=DSIN(ARG)
   50 PP=0.0
      DO 60 I=1,ITM
   60 PP=PP+SERIES(I)*CONJG(SERIES(I))
      P(1)=PP/DFLOAT(ITM)
      VAR=P(1)
      M=1
      B1(1)=SERIES(1)
      B2(ITM-1)=SERIES(ITM)
      DO 70 I=2,ITM-1
      B1(I)=SERIES(I)
   70 B2(I-1)=SERIES(I)
      GO TO 80
  100 DO 110 I=1,M
  110 AA(I)=A(I)
      M=M+1
      ITMMM=ITM-M
      DO 120 I=1,ITMMM
      B1(I)=B1(I)-DCONJG(AA(M-1))*B2(I)
  120 B2(I)=B2(I+1)-AA(M-1)*B1(I+1)
   80 ANOM=DCMPLX(0.D0,0.D0)
      ADEN=DCMPLX(0.D0,0.D0)
      ITMMM=ITM-M
      DO 90 I=1,ITMMM
      ANOM=ANOM+DCONJG(B1(I))*B2(I)
   90 ADEN=ADEN+B1(I)*DCONJG(B1(I))+B2(I)*DCONJG(B2(I))
      A(M)=(ANOM+ANOM)/ADEN
      P(M+1)=P(M)*(1.0-DCONJG(A(M))*A(M))
      IF (M.EQ.1) GO TO 100
  130 MM1=M-1
      DO 140 I=1,MM1
  140 A(I)=AA(I)-A(M)*DCONJG(AA(M-I))
      IF (M.LT.MMAX) GO TO 100
c  FINAL PREDICTION ERROR
      DO 150 M=1,MMAXP1
  150 FPE(M)=P(M)*DFLOAT(ITM+M-1)/DFLOAT(ITM-M+1)
      DO 180 NUA=1,NUAMAX
      POWERX=0.
c  FREQUENCY BAND AVERAGE
      DO 170 NUB=1,NUBMAX
      NU=NUB+NUA*NUBMAX+(NUMAX-3*NUBMAX-1)/2
      CSUM=1.
      DO 160 M=1,MMAX
      NUTM=MOD(NU*M-1,NUMAX)+1
  160 CSUM=CSUM-A(M)*(C(NUTM)-CI*S(NUTM))
  170 POWERX=POWERX+P(MMAXP1)/(CSUM*DCONJG(CSUM))
      POWER(NUA)=.5*POWERX/DFLOAT(NUBMAX)
  180 CONTINUE
      PNU=0.0
      DO 210 L=1,NUAMAX
  210 PNU=PNU+POWER(L)
      PNU=PNU/(.5*NUAMAX)
      RETURN
      END

      BLOCK DATA BD_DIAGIJ
c  **
c  ** TITLES, LEGENDS AND CHARACTERS FOR DIAGIJ
c  **
      COMMON/DIJCOM/TITLE1(3,6),TITLE2(3,6),TITLE3(3,6),
     *  LEGND1(10),LEGND2(14),ACHAR,BCHAR,CCHAR,DCHAR,ECHAR
c  **
                          CHARACTER*32 TITLE1/
     1   'TOPOGRAPHY (METERS)',
     *   'LAND COVERAGE    ',
     *   'OCEAN ICE COVERAGE',
     *   'SNOW COVERAGE    ',
     *   'SNOW DEPTH (MM H2O)',
     *   'SNOW AND ICE COVERAGE',
c
     7   'PRECIPITATION (MM/DAY)',
     *   'EVAPORATION (MM/DAY)',
     *   'SENSIBLE HEAT FLUX (WATTS/M**2)',
     *   'GROUND WETNESS   ',
     *   'GROUND RUNOFF (MM/DAY)',
     *   'GROUND TEMPERATURE (DEGREES C)',
c13
     3   'SURFACE CROSS ISOBAR ANGLE (DEG)',
     *   'JET SPEED (METERS/SEC',
     *   'SURFACE WIND SPEED (METERS/SEC)',
     *   'SURF. CROSS ISOBAR ADJ. ANGLE',
     *   'JET DIRECTION (CW NOR)',
     *   'SURFACE WIND DIRECTION (CW NOR) '/
                          CHARACTER*32 TITLE2/
     9   'TOTAL CLOUD COVER',
     *   'CONVECTIVE CLOUD COVER',
     *   'CLOUD TOP PRESSURE (MB)',
     *   'LOW LEVEL CLOUDINESS',
     *   'MIDDLE LEVEL CLOUDINESS',
     *   'HIGH LEVEL CLOUDINESS',
c25:
     5   'NET RAD. OF PLANET (WATTS/M**2)',
     *   'NET RADIATION AT Z0 (WATTS/M**2)',
     *   'BRIGHTNESS TEMP THRU WNDW(DEG C)',
     *   'PLANETARY ALBEDO ',
     *   'GROUND ALBEDO    ',
     *   'VISUAL ALBEDO    ',
c31:
     1   'NET THRML RADIATION (WATTS/M**2)',
     *   'NET HEAT AT Z0 (WATTS/M**2)',
     *   'TROP STATIC STABILITY (DEG K/KM)',
     *   'TOTAL NT DRY STAT ENR(10**14 WT)',
     *   'NT DRY STAT ENR BY ST ED(E14 WT)',
     *   'NT DRY STAT ENR BY TR ED(E14 WT)'/
                          CHARACTER*32 TITLE3/
     7   '850 MB HEIGHT (METERS-1500)',
     *   '700 MB HEIGHT (METERS-3000)',
     *   '500 MB HEIGHT (METERS-5600)',
     *   '300 MB HEIGHT (METERS-9500)',
     *   '100 MB HEIGHT (METERS-16400)',
     *   ' 30 MB HEIGHT (METERS-24000)',
c43:
     3   'THICKNESS TEMPERATURE 1000-850',
     *   'THICKNESS TEMPERATURE 850-700',
     *   'THICKNESS TEMPERATURE 700-500',
     *   'THICKNESS TEMPERATURE 500-300',
     *   'THICKNESS TEMPERATURE 300-100',
     *   'THICKNESS TEMPERATURE 100-30',
c49:
     *   'TOTAL EARTH WATER (KG/M**2)',
     *   'LENGTH OF GROWING SEASON (DAYS)',
     *   'MONTHLY HEATING (DEGREE DAYS)',
     *   'PLANT WATER STRESS (DEGREE DAYS)',
     *   'PALMER DROUGHT INDEX',
     *   'HEAT HUMIDITY INDEX (DEGREES F)'/
c  **
      CHARACTER*40 LEGND1/
     *  '0=0,1=5...9=45,A=50...K=100 ',
     *  '0=0...9=90,A=100...I=180...R=270 ',
     *  '1=.5...9=4.5,A=5...Z=17.5,+=MORE ',
     *  '1=1...9=9,A=10...Z=35,+=MORE ',
     *  '1=2...9=18,A=20...Z=70,+=MORE ',
c
     *  '1=50...9=450,A=500...Z=1750,+=MORE',
     *  '1=100...9=900,A=1000...Z=3500,+=MORE',
     *  '1=20...9=180,A=200...Z=700,+=MORE',
     *  'A=1...Z=26,3=30...9=90,+=100-150,*=MORE',
     *  '0=0,A=.1...Z=2.6,3=3...9=9,+=10-15'/
      CHARACTER*40 LEGND2/
     *  '-=LESS,Z=-78...0=0...9=27,+=MORE ',
     *  '-=LESS,Z=-260...0=0...9=90,+=MORE',
     *  '-=LESS,Z=-520...0=0...9=180,+=MORE',
     *  '-=LESS,Z=-1300...0=0...9=450,+=MORE',
     *  '-=LESS,Z=-2600...0=0...9=900,+=MORE',
c
     *  '-=LESS,Z=-3900...0=0...9=1350,+=MORE',
     *  '-=LESS,Z=-5200...0=0...9=1800,+=MORE',
     *  '-=LESS,9=-.9...0=0,A=.1...Z=2.6,+=MORE',
     *  '-=LESS,9=-45...0=0,A=5...K=45...+=MORE',
     *  '-=LESS,9=-90...0=0,A=10...Z=260,+=MORE',
c
     *  '-=LESS,9=-180...A=20...Z=520,+=MORE',
     *  '-=LESS,9=-9...0=0,A=1...Z=26,+=MORE',
     *  '-=LESS,9=-36...0=0,A=4...Z=104,+=MOR',
     *  '1=5...9=45,A=50...Z=175,+=MORE '/
c  **
      CHARACTER ACHAR*38,BCHAR*23,CCHAR*38,DCHAR*37,ECHAR*38
      DATA ACHAR/' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+'/
      DATA BCHAR/' 0123456789ABCDEFGHIJKX'/
      DATA CCHAR/'-9876543210ABCDEFGHIJKLMNOPQRSTUVWXYZ+'/
      DATA DCHAR/' 0ABCDEFGHIJKLMNOPQRSTUVWXYZ3456789+*'/
      DATA ECHAR/'-ZYXWVUTSRQPONMLKJIHGFEDCBA0123456789+'/
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE DIAGIJ
c  **
c  ** THIS SUBROUTINE PRODUCES LATITUDE BY LONGITUDE MAPS OF
c  **
c   K  IND                                                        IDACC
c  **
c  *1      TOPOGRAPHY (M)
c  *2      LAND COVERAGE (10**-2)
c  *3   1  OCEAN ICE COVERAGE (10**-2)                                4
c  **   2  SNOW COVERAGE (10**-2)                                     4
c  **   3  SNOW DEPTH (KG H2O/M**2)
c  *6  29  SNOW AND ICE COVERAGE (PERCENT)
c  **
c  *7   5  PRECIPITATION (KG/M**2/86400 S)                            1
c  **   6  EVAPORATION (KG/M**2/86400 S)                              1
c  *9   4  SENSIBLE HEAT FLUX (WATTS/METER**2)
c  10   7  BETA, GROUND WETNESS (10**-2)                              3
c  11  32  GROUND RUNOFF FROM SURFACE (KG/M**2/86400 S)               1
c  12  28  FIRST LAYER GROUND TEMPERATURE (K-273.16)                  1
c  **
c  13  46  ALPHA0, SURFACE CROSS ISOBAR ANGLE (DEG)                   3
c  14  39,40  JET SPEED (M/S)                                         4
c  15  36,37  SURFACE WIND SPEED (M/S)                                3
c  16  34  SURFACE CROSS ISOBAR ADJUSTMENT ANGLE (DEG)                3
c  17  39,40  JET DIRECTION (CLOCKWISE FROM NORTH)                    0
c  18  36,37  SURFACE WIND DIRECTION (CLOCKWISE FROM NORTH)           0
c  **
c  19  19  TOTAL CLOUD COVERAGE (PERCENT)
c  20  17  CLOUD COVERAGE FROM MOIST CONVECTION (PERCENT)
c  21  18/19   CLOUD TOP PRESSURE (MILLIBARS)
c  22  41  LOW LEVEL CLOUDINESS (PERCENT)
c  23  42  RMIDDLE LEVEL CLOUDINESS (PERCENT)
c  24  43  HIGH LEVEL CLOUDINESS (PERCENT)
c  **
c  25  21+24  RADIATION BALANCE OF PLANET (WATTS/METER**2)
c  26  22  RADIATION BALANCE OF GROUND (WATTS/METER**2)
c  27  44  BRIGHTNESS TEMPERATURE THROUGH WINDOW REGION (K-273.16)
c  28  24/25  PLANETARY ALBEDO (PERCENT)
c  29  26/27  GROUND ALBEDO (PERCENT)
c  30  45/25  VISUAL ALBEDO (PERCENT)
c  **
c  31  21  NET THERMAL RADIATION  (WATTA/METER**2)
c  32  23  NET HEAT AT GROUND (WATTS/METER**2)
c  33  31  TROPOSPHERIC STATIC STABILITY
c  34  20  TOTAL NORTH. TRANS. OF DRY STATIC ENERGY  (10**14 WATTS)
c  35      STAND. EDDY NORTH. TRANS. OF DRY STATIC ENERGY (10**14 WATTS)
c  36      TRANS. EDDY NORTH. TRANS. OF DRY STATIC ENERGY (10**14 WATTS)
c  **
c  37  10  850 MB GEOPOTENTIAL HEIGHT (METERS-1500)
c  **  11  700 MB GEOPOTENTIAL HEIGHT (METERS-3000)
c  **  12  500 MB GEOPOTENTIAL HEIGHT (METERS-5600)
c  **  13  300 MB GEOPOTENTIAL HEIGHT (METERS-9500)
c  **  14  100 MB GEOPOTENTIAL HEIGHT (METERS-16400)
c  **  15  30 MB GEOPOTENTIAL HEIGHT (METERS-24000)
c  **
c  43   9,10  THICKNESS TEMPERATURE FROM 1000 TO 850 MB (DEGREES CENT.)
c  **  10,11  THICKNESS TEMPERATURE FROM 850 TO 700 MB (DEGREES CENT.)
c  **  11,12  THICKNESS TEMPERATURE FROM 700 TO 500 MB (DEGREES CENT.)
c  **  12,13  THICKNESS TEMPERATURE FROM 500 TO 300 MB (DEGREES CENT.)
c  **  13,14  THICKNESS TEMPERATURE FROM 300 TO 100 MB (DEGREES CENT.)
c  **  14,15  THICKNESS TEMPERATURE FROM 100 TO 30 MB (DEGREES CENT.)
c  **
c  49  50  TOTAL EARTH WATER (KG H2O/M**2)
c  **      LENGTH OF GROWING SEASON (DAYS)
c  **  52  MONTHLY HEATING (FAHRENHEIT DEGREE DAYS)
c  **  54  PLANT WATER STRESS (DEGREE DAYS)
c  **      PALMER DROUGHT INDEX (1)
c  **  53  HEAT HUMIDITY INDEX (DEGREES FAHRENHEIT)
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      include 'pd_COMMON'
      REAL*4 SMAP,SMAPJ
      COMMON/WORK2/ENDE16(IM,JM,2),PRAVG(IM,JM),PRSD(IM,JM),
     *  FLAT(3),FNH(3),FGLOBE(3),MLAT(3),MGLOBE(3),GNUM(3),GDEN(3)
     *  ,SMAP(IM,JM,3),SMAPJ(JM,3)
      CHARACTER XLB*48,CRUN*24/' '/
      COMMON/DIJCOM/TITLE(3,18),LEGEND(10,24),ACHAR(38),BCHAR(23),
     *  CCHAR(38),DCHAR(37),ECHAR(38)
      CHARACTER*4 LEGEND,TITLE*32
      CHARACTER*1 ACHAR,BCHAR,CCHAR,DCHAR,ECHAR
c  ** ACHAR/ ,0,1,...,8,9,A,B,...,Y,Z,+/
c  ** BCHAR/ ,0,1,...,8,9,A,B,...,K,X/
c  ** CCHAR/-,9,8,...,1,0,A,B,...,Y,Z,+/
c  ** DCHAR/ ,0,A,B,...,Y,Z,3,4,...,8,9,+,*/
c  ** ECHAR/-,Z,Y,...,B,A,0,1,...,8,9,+/
      CHARACTER*1 LINE(IM,3),LONGTD(36)
      DIMENSION IND(54),IA(54),ILEG(3,18),SCALE(54),FAC(54),JGRID(54),
     *  PMB(7),GHT(7)
      DATA LONGTD/'+',35*' '/
      DATA IND/3*1,2,3,29,    5, 6, 4, 7,32,28,   46,39,36,34,39,36,
     *  19,17,18,41,42,43,   21,22,44,24,26,45,   21,23,31,20, 1, 2,
     *  10,11,12,13,14,15,    9,10,11,12,13,14,   50, 0,52,54, 0,53/
      DATA IA/0,0,1,3*4,      1, 1, 1, 1, 1, 1,    3, 4, 3, 3, 0, 0,
     *   2, 2, 0, 2, 2, 2,    2, 1, 2, 0, 0, 0,    2, 1, 4, 4, 4, 4,
     *   4, 4, 4, 4, 4, 4,    4, 4, 4, 4, 4, 4,    1, 0,12,12, 0, 3/
      DATA ILEG/7,3*1,9,1,   10,10,12, 1,18,11,   19, 5, 3,19, 2, 2,
     *   1, 1, 6, 1, 1, 1,   13,20,11, 1, 1, 1,   13,13, 3,20,20,18,
     *  12,13,14,15,15,16,   11,11,11,11,11,11,    8,24, 7,21,22,23/
      DATA SCALE/1.,3*100.,1.,100.,  3*1.,100.,2*1.,  6*1.,
     *  2*100.,1.,3*100.,  3*1.,3*100.,  2*1.,2.,21*1./
      DATA FAC/.01,3*.2,1.,.2,  2*10.,.1,.2,10.,.3333333,
     *  .2,.5,2.,.2,2*.1,  2*.2,.02,3*.2,  .05,.1,.3333333,3*.2,
     *  2*.05,2.,2*.1,10.,  .1,.05,.02,.01,.01,.006666667,  6*.3333333,
     *  .05,.2,.01,.05,1.,.25/
      DATA JGRID/19*1,2,15*1,1,1,1,2,2,14*1/
      DATA PMB/1000.,850.,700.,500.,300.,100.,30./,P1000/1000./
      DATA GHT/0.,1500.,3000.,5600.,9500.,16400.,24000./
c  ** INITIALIZE CERTAIN QUANTITIES
        XLB = ' '
        WRITE(CRUN,'(6A)') (XLABEL(K),K=1,6)
        KXLB = INDEX(CRUN,'(')-1
        WRITE(XLB(39:),'(A)') CRUN(1:MIN(10,KXLB))
        WRITE(XLB(29:36),'(A3,I5)') JMNTH0,JYEAR0
      SHA=RGAS/KAPA
      INC=1+(JM-1)/24
      ILINE=36*INC
      IQ1=1+IM/(4*INC)
      LONGTD(IQ1)=LONGTD(1)
      IQ2=1+IM/(2*INC)
      LONGTD(IQ2)=LONGTD(1)
      IQ3=1+3*IM/(4*INC)
      LONGTD(IQ3)=LONGTD(1)
      JEQ=2.+.5*(JM-1)
      BYIM=1./FIM
      DTSRCE=NDYN*DT
      DTCNDS=NCNDS*DT
      SCALE(7)=SDAY/DTCNDS
      SCALE(8)=SDAY/DTSRCE
      SCALE(9)=1./DTSRCE
      SCALE(11)=SDAY/DTSRCE
      SCALE(13)=360./TWOPI
      SCALE(16)=360./TWOPI
      SCALE(26)=1./DTSRCE
      SCALE(32)=1./DTSRCE
      SCALE(33)=1.D3*GRAV*EXPBYK(P1000)
      SCALE(34)=6.25E-14/GRAV
      SCALE(35)=SCALE(34)
      SCALE(36)=SCALE(34)
      DO 70 M=37,42
   70 SCALE(M)=1./GRAV
      DO 80 M=43,48
   80 SCALE(M)=1./(RGAS*LOG(PMB(M-42)/PMB(M-41)))
      IF (IDACC(12).LT.1) IDACC(12)=1
c  **
      IHOUR0=TOFDY0+.5
      IHOUR = TOFDAY + .5
      TAUDIF=TAU-TAU0
c  ** NO PALMER INDEX FOR FINE GRID RUNS
      BYIADA=1./(IDACC(4)+1.D-20)
      IF (SKIPSE.EQ.1.) GO TO 160
c  ** CACULATE STANDING AND TRANSIENT EDDY NORTHWARD TRANSPORT OF DSE
      DO 130 J=2,JM
      ZNDE16=0.
      DO 120 L=1,LM
  120 ZNDE16=ZNDE16+(SHA*AJK(J,L,12)+AJK(J,L,14))
      ZNDE16=4.*ZNDE16*DXV(J)/FIM
      DO 130 I=1,IM
      ENDE16(I,J,1)=4.*ENDE16(I,J,1)*DXV(J)
  130 ENDE16(I,J,2)=AIJ(I,J,20)-ZNDE16-ENDE16(I,J,1)
      DO 140 I=1,IM
      ENDE16(I,1,1)=0.
  140 ENDE16(I,1,2)=0.
c  **
  160 NDIAG=54
      DO 180 N=1,NDIAG
      IF (JGRID(N).EQ.2) GO TO 180
      DO 170 I=1,IM
      AIJ(I,1,N)=AIJ(1,1,N)
  170 AIJ(I,JM,N)=AIJ(1,JM,N)
  180 CONTINUE
      IFRSTP=1
      LASTP=9
      IF (KDIAG(3).GT.0) LASTP=9-KDIAG(3)
      IF (KDIAG(3).LT.0) IFRSTP=-KDIAG(3)
      IF (KDIAG(3).LT.0) LASTP=IFRSTP
      DO 610 KPAGE=IFRSTP,LASTP
      WRITE (6,901) XLABEL
      WRITE (6,902) IDAY0,IHOUR0,JDATE0,JMNTH0,JYEAR0,TAU0,IDAY,IHOUR,
     *  JDATE,JMONTH,JYEAR,TAU,TAUDIF
      DO 610 KROW=1,2
      KR=2*(KPAGE-1)+KROW
      WRITE (6,903) (TITLE(K,KR),K=1,3)
      DO 200 KCOLMN=1,3
      FNH(KCOLMN)=0.
      FGLOBE(KCOLMN)=0.
      GNUM(KCOLMN)=0.
  200 GDEN(KCOLMN)=0.
      DO 550 J=JM,1,-1
      DO 210 I=1,IM*3
  210 LINE(I,1)=' '
      DO 510 KCOLMN=1,3
      FLATK=0.
      K=3*KR+KCOLMN-3
      NDEX=IND(K)
      BYIACC=1./(IDACC(IA(K))+1.D-20)
      GO TO (320,340,400,400,440,400, 440,440,460,400,420,460,
     *       420,300,300,420,240,240, 400,400,260,400,400,400,
     *       220,420,460,260,260,260, 460,460,380,420,280,280,
     *       460,460,460,460,460,460, 360,360,360,360,360,360,
     *       380,491,380,420,493,420                     ,480),K
c  ** SUM OF TWO ARRAYS
  220 DO 230 I=1,IM
      A=(AIJ(I,J,21)+AIJ(I,J,24))*SCALE(K)*BYIACC
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=28.5+A*FAC(K)
      IF (N.LT.1 ) N=1
      IF (N.GT.38) N=38
  230 LINE(I,KCOLMN)=ECHAR(N)
      GO TO 500
c  ** WIND DIRECTION
  240 IF (J.EQ.1) GO TO 500
      DO 250 I=1,IM
      A=360.*ATAN2(AIJ(I,J,NDEX)+1.D-20,AIJ(I,J,NDEX+1)+1.D-20)/TWOPI
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=2.5+A*FAC(K)
      IF (N.LT.2) N=N+36
  250 LINE(I,KCOLMN)=ACHAR(N)
      GO TO 500
c  ** RATIO OF 2 ARRAYS (MAINLY FOR ALBEDO)
  260 FNUM=0.
      FDEN=0.
      NDEX2=NDEX+1
      IF (NDEX.EQ.45) NDEX2=25
      DO 270 I=1,IM
      A=SCALE(K)*AIJ(I,J,NDEX)/(AIJ(I,J,NDEX2)+1.D-20)
      IF (NDEX.EQ.24 .OR. NDEX.EQ.26) A=100.-A
      FNUM=FNUM+AIJ(I,J,NDEX)
      FDEN=FDEN+AIJ(I,J,NDEX2)
      N=2.5+A*FAC(K)
      IF (A*FAC(K).GE.20.) N=23
      IF (AIJ(I,J,NDEX2).LE.0.) N=1
         IF (AIJ(I,J,NDEX2).LE.0.) A=0.
           SMAP(I,J,KCOLMN)=A
  270 LINE(I,KCOLMN)=ACHAR(N)
      FLAT(KCOLMN)=SCALE(K)*FNUM/(FDEN+1.D-20)
      IF (NDEX.EQ.24 .OR. NDEX.EQ.26) FLAT(KCOLMN)=100.-FLAT(KCOLMN)
      MLAT(KCOLMN)=NINT(FLAT(KCOLMN))
      GNUM(KCOLMN)=GNUM(KCOLMN)+FNUM*DXYP(J)
      GDEN(KCOLMN)=GDEN(KCOLMN)+FDEN*DXYP(J)
      IF (J.GT.INC) GO TO 510
      FGLOBE(KCOLMN)=SCALE(K)*GNUM(KCOLMN)/(GDEN(KCOLMN)+1.D-20)
      IF (NDEX.EQ.24.OR.NDEX.EQ.26) FGLOBE(KCOLMN)=100.-FGLOBE(KCOLMN)
      FGLOBE(KCOLMN)=FGLOBE(KCOLMN)*AREAG/ FIM
      GO TO 510
c  ** STANDING AND TRANSIENT EDDY NORTHWARD TRANSPORTS OF DSE
  280 IF (SKIPSE.EQ.1.) GO TO 510
      DO 290 I=1,IM
      A=ENDE16(I,J,NDEX)*SCALE(K)*BYIACC
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=11.5+A*FAC(K)
      IF (N.LT.1) N=1
      IF (N.GT.38) N=38
  290 LINE(I,KCOLMN)=CCHAR(N)
      FLAT(KCOLMN)=FLATK
      DAREA=DXYV(J)
      GO TO 505
c  ** MAGNITUDE OF TWO PERPENDICULAR COMPONENTS
  300 IF (J.EQ.1) GO TO 500
      DO 310 I=1,IM
      A=SQRT(AIJ(I,J,NDEX)**2+AIJ(I,J,NDEX+1)**2)*SCALE(K)*BYIACC
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=2.5+A*FAC(K)
      IF (N.GT.38) N=38
  310 LINE(I,KCOLMN)=ACHAR(N)
      GO TO 500
c  ** SURFACE TOPOGRAPHY
  320 DO 330 I=1,IM
      ZS=FDATA(I,J,1)/GRAV
           SMAP(I,J,KCOLMN)=ZS
      FLATK=FLATK+ZS
      N=2.5+.01*ZS
      IF (ZS.LE.0.) N=1
      IF (N.GT.38) N=38
  330 LINE(I,KCOLMN)=ACHAR(N)
      GO TO 500
c  ** LAND COVERAGE
  340 DO 350 I=1,IM
      PLAND=FDATA(I,J,2)*100.
           SMAP(I,J,KCOLMN)=PLAND
      FLATK=FLATK+PLAND
      N=2.5+PLAND*.2
      IF (PLAND.LE.0.) N=1
      IF (PLAND.GE.100.) N=23
  350 LINE(I,KCOLMN)=BCHAR(N)
      GO TO 500
c  ** THICKNESS TEMPERATURES
  360 DO 370 I=1,IM
      A=((AIJ(I,J,NDEX+1)-AIJ(I,J,NDEX))*BYIACC
     *  +(GHT(NDEX-7)-GHT(NDEX-8))*GRAV)*SCALE(K)-273.16
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=28.5+A*FAC(K)
      IF (N.LT.1) N=1
      IF (N.GT.38) N=38
  370 LINE(I,KCOLMN)=ECHAR(N)
      GO TO 500
c  ** POSITIVE QUANTITIES UNIFORMLY SCALED
  380 DO 390 I=1,IM
      A=AIJ(I,J,NDEX)*SCALE(K)*BYIACC
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=2.5+A*FAC(K)
      IF (A.EQ.0.) N=1
      IF (N.GT.38) N=38
  390 LINE(I,KCOLMN)=ACHAR(N)
      GO TO 500
c  ** PERCENTAGES
  400 DO 410 I=1,IM
      A=AIJ(I,J,NDEX)*SCALE(K)*BYIACC
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=2.5+A*FAC(K)
      IF (A.LE.0.) N=1
      IF (A*FAC(K).GE.20.) N=23
  410 LINE(I,KCOLMN)=BCHAR(N)
      GO TO 500
c  ** SIGNED QUANTITIES UNIFORMLY SCALED (LETTERS +, NUMBERS -)
  420 DO 430 I=1,IM
      A=AIJ(I,J,NDEX)*SCALE(K)*BYIACC
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=11.5+A*FAC(K)
      IF (N.LT.1) N=1
      IF (N.GT.38) N=38
  430 LINE(I,KCOLMN)=CCHAR(N)
      IF (K.EQ.34) FLATK=FLATK*FIM
      GO TO 500
c  ** PRECIPITATION AND EVAPORATION
  440 DO 450 I=1,IM
      A=AIJ(I,J,NDEX)*SCALE(K)*BYIACC
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=1
      IF (A.LE.0.) GO TO 450
      N=2.5+A*FAC(K)
      IF (N.GT.28) N=(N+263)/10
      IF (N.GT.35) N=(N+180)/6
      IF (N.GT.37) N=37
  450 LINE(I,KCOLMN)=DCHAR(N)
      GO TO 500
c  ** SIGNED QUANTITIES UNIFORMLY SCALED (NUMBERS +, LETTERS -)
  460 DO 470 I=1,IM
      A=AIJ(I,J,NDEX)*SCALE(K)*BYIACC
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=28.5+A*FAC(K)
      IF (N.LT.1 ) N=1
      IF (N.GT.38) N=38
  470 LINE(I,KCOLMN)=ECHAR(N)
      GO TO 500
c  ** POSITIVE QUANTITIES NON-UNIFORMLY SCALED
  480 DO 490 I=1,IM
      A=AIJ(I,J,NDEX)*SCALE(K)*BYIACC
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=2.5+A*FAC(K)
      IF (N.GE.13) N=(N+123)/10
      IF (N.GT.38) N=38
  490 LINE(I,KCOLMN)=ACHAR(N)
      GO TO 500
c  ** LENGTH OF GROWING SEASON
  491 DO 492 I=1,IM
      A=TSFREZ(I,J,2)-TSFREZ(I,J,1)
      IF (A.LT.0.) A=A+365.
           SMAP(I,J,KCOLMN)=A
      FLATK=FLATK+A
      N=2.5+A*FAC(K)
      IF (A.LE.0.) N=1
      IF (N.GT.38) N=38
  492 LINE(I,KCOLMN)=ACHAR(N)
      GO TO 500
c  ** PALMER DROUGHT INDEX
  493 DO 494 I=1,IM
      A=0.
      FLATK=FLATK+A
      N=11.5+A*FAC(K)
      IF (N.LT.1 ) N=1
      IF (N.GT.38) N=38
  494 LINE(I,KCOLMN)=CCHAR(N)
  500 FLAT(KCOLMN)=FLATK*BYIM
      DAREA=DXYP(J)
      IF (JGRID(NDEX).EQ.2) DAREA=DXYV(J)
      IF (J.GE.JEQ) FNH(KCOLMN)=FNH(KCOLMN)+FLAT(KCOLMN)*DAREA
  505 FGLOBE(KCOLMN)=FGLOBE(KCOLMN)+FLAT(KCOLMN)*DAREA
      MLAT(KCOLMN)=NINT(FLAT(KCOLMN))
  510 CONTINUE
        DO KCOLMN = 1,3
        SMAPJ(J,KCOLMN) = FLAT(KCOLMN)
        END DO
      IF (MOD(J,INC).NE.0) GO TO 550
      GO TO (524,520, 520,520, 520,520, 521,520, 526,520, 526,524,
     *       527,527, 520,520, 527,524, 527,527),KR
  520 WRITE (6,910) (FLAT(KC),(LINE(I,KC),I=1,ILINE,INC),KC=1,3)
      GO TO 530
  521 WRITE (6,911) (FLAT(KC),(LINE(I,KC),I=1,ILINE,INC),KC=1,2),
     *  MLAT(3),(LINE(I,3),I=1,ILINE,INC)
      GO TO 530
  524 WRITE (6,914) MLAT(1),(LINE(I,1),I=1,ILINE,INC),
     *  (FLAT(KC),(LINE(I,KC),I=1,ILINE,INC),KC=2,3)
      GO TO 530
  526 WRITE (6,916) (MLAT(KC),(LINE(I,KC),I=1,ILINE,INC),KC=1,2),
     *  FLAT(3),(LINE(I,3),I=1,ILINE,INC)
      GO TO 530
  527 WRITE (6,917) (MLAT(KC),(LINE(I,KC),I=1,ILINE,INC),KC=1,3)
  530 DO 540 I=1,IM
      IF (FDATA(I,J,2).GE..5) GO TO 540
      LINE(I,1)=' '
      LINE(I,2)=' '
      LINE(I,3)=' '
  540 CONTINUE
c     WRITE (6,906) ((LINE(I,KC),I=1,ILINE,INC),KC=1,3)
      WRITE (6,906) ((LINE(I,KC),I=1,ILINE,INC),KC=1,3)
  550 WRITE (6,906) ((LINE(I,KC),I=1,ILINE,INC),KC=1,3)
      DO 555 KCOLMN=1,3
      FNH(KCOLMN)=2.*FNH(KCOLMN)*FIM/AREAG
      FGLOBE(KCOLMN)=FGLOBE(KCOLMN)*FIM/AREAG
  555 MGLOBE(KCOLMN)=NINT(FGLOBE(KCOLMN))
      IF (KR.EQ.2) CALL KEYIJ (FGLOBE(3),FNH(3))
      GO TO (574,570, 570,570, 570,570, 571,570, 577,570, 576,570,
     *       577,577, 570,570, 577,574, 577,577),KR
  570 WRITE (6,910) (FGLOBE(KC),LONGTD,KC=1,3)
      GO TO 600
  571 WRITE (6,911) FGLOBE(1),LONGTD,FGLOBE(2),LONGTD,MGLOBE(3),LONGTD
      GO TO 600
  574 WRITE (6,914) MGLOBE(1),LONGTD,FGLOBE(2),LONGTD,FGLOBE(3),LONGTD
      GO TO 600
  576 WRITE (6,916) MGLOBE(1),LONGTD,MGLOBE(2),LONGTD,FGLOBE(3),LONGTD
      GO TO 600
  577 WRITE (6,917) (MGLOBE(KC),LONGTD,KC=1,3)
  600 WRITE (6,909) ((LEGEND(KT,ILEG(KCOLMN,KR)),KT=1,10),KCOLMN=1,2),
     *  (LEGEND(KT,ILEG(3,KR)),KT=1,9)
      if (iprint_pd(3).lt.9) then
        DO KC=1,3
        WRITE(iu_pd(3))TITLE(KC,KR),XLB,((SMAP(I,J,KC),I=1,IM),J=1,JM),
     *    (SMAPJ(J,KC),J=1,JM),SNGL(FGLOBE(KC))
        END DO
      end if
  610 CONTINUE
  690 CONTINUE
c  **
c  ** PRODUCE FULL PAGE I,J MAPS
c  **
      CALL IJMAP (1,AIJ(1,1,38),BYIADA)
      BYIACN=1./(IDACC(3)+1.D-20)
      CALL IJMAP (2,AIJ(1,1,35),BYIACN)
c     CALL IJMAP (4,AIJ(1,1,8),BYIADA)
c     CALL IJMAP (5,AIJ(1,1,33),BYIADA)

      if (iprint_pd(3).lt.9) CALL SIJMAP

      RETURN
c  **
  901 FORMAT ('1',33A4)
  902 FORMAT ('0',15X,'DAY',I6,', HR',I2,' (',I2,A5,I4,')',F9.0,
     *      '   TO   DAY',I6,', HR',I2,' (',I2,A5,I4,')',F9.0,
     *  '    DIF',F5.0,' HR')
  903 FORMAT ('0',6X,A32,13X,A32,13X,A32)
  906 FORMAT ('+',6X,36A1,9X,36A1,9X,36A1)
  909 FORMAT (7X,10A4,5X,10A4,5X,9A4)
  910 FORMAT (1X,F5.1,1X,36A1,F8.1,1X,36A1,F8.1,1X,36A1)
  911 FORMAT (1X,F5.1,1X,36A1,F8.1,1X,36A1,I8,1X,36A1)
  914 FORMAT (1X,I5,1X,36A1,F8.1,1X,36A1,F8.1,1X,36A1)
  916 FORMAT (1X,I5,1X,36A1,I8,1X,36A1,F8.1,1X,36A1)
  917 FORMAT (1X,I5,1X,36A1,I8,1X,36A1,I8,1X,36A1)
      END
      SUBROUTINE IJMAP (NT,ARRAY,BYIACC)
      INCLUDE 'BA94jalC9.COM'
      include 'pd_COMMON'
      COMMON/WORK2/SMAP(IM,JM),SMAPJ(JM)
      CHARACTER XLB*32,CRUN*24/' '/
      REAL*4 SMAP,SMAPJ
      CHARACTER*4 CPRESS(LM)
      DIMENSION LON(IM+1),ARRAY(IM,JM)
      CHARACTER*1 LINE(3,IM),IDX(12),BLANK,TITLE(5)*48,AVG(9),
     *  LINE3(IM)*3,MEAN*9
      EQUIVALENCE (MEAN,AVG(1)),(LINE3(1),LINE(1,1))
      DATA IDX/'0','1','2','3','4','5','6','7','8','9','-','*'/
      DATA BLANK/' '/
      DATA TITLE/
c  **
c  ** THIS SUBROUTINE PRODUCES NUMERICAL LATITUDE BY LONGITUDE MAPS OF
c  **
     *  'SEA LEVEL PRESSURE (MB-1000) ',
     *  'SURFACE TEMPERATURE (DEGREES C) ',
     *  'INSTANTANEOUS 850 MB HEIGHTS (DEKAMETERS-100)',
     *  'SEA LEVEL PRESSURE (MB-1000)  (USING T1)',
     *  'SURFACE TEMPERATURE (DEG C)  (LAPSE RATE FROM T1'/
c  **
c  ** INITIALIZE CERTAIN QUANTITIES
c  **
      XLB = ' '
      WRITE(CRUN,'(6A)') (XLABEL(K),K=1,6)
      KXLB = INDEX(CRUN,'(')-1
      WRITE(XLB(23:),'(A)') CRUN(1:MIN(10,KXLB))
      WRITE(XLB(13:20),'(A3,I5)') JMNTH0,JYEAR0
      BYIM=1./IM
      INC=(IM+35)/36
      LON(IM+1)=180
      LD=360/IM
      DO 40 I=1,IM
      WRITE(LINE3(I),'(I3)') I
   40 LON(I)=-180+(I-1)*LD
      MEAN='     MEAN'
      IHOUR0=TOFDY0+.5
      IHOUR = TOFDAY + .5
      TAUDIF=TAU-TAU0
      WRITE(6,901)XLABEL
      WRITE(6,902)IDAY0,IHOUR0,JDATE0,JMNTH0,JYEAR0,TAU0,IDAY,IHOUR,
     *  JDATE,JMONTH,JYEAR,TAU,TAUDIF
      WRITE(6,900) TITLE(NT)
      WRITE (6,910) ((LINE(K,I),K=1,3),I=1,IM,INC),AVG
      WRITE(6,940)
      WRITE(6,940)
c  ** OUTSIDE J LOOP
      DO 300 JX=1,JM
      FLAT=0.
      J=1+JM-JX
      DO 250 I=1,IM
      A=ARRAY(I,J)*BYIACC
           SMAP(I,J)   =A
      FLAT=FLAT+A
      IF (A.LT.999.5.AND.A.GE.-99.5) GO TO 140
      DO 100 K=1,3
  100 LINE(K,I)=IDX(12)
      GO TO 250
  140 DO 150 K=1,3
  150 LINE(K,I)=BLANK
      JA=NINT(A)
      IA=IABS(JA)
      IF (IA.GT.99) GO TO 210
      IF (IA-9) 230,230,220
  210 LINE(1,I)=IDX(IA/100+1)
      IA=MOD(IA,100)
  220 LINE(2,I)=IDX(IA/10+1)
      IA=MOD(IA,10)
  230 LINE(3,I)=IDX(IA+1)
      IF (JA.GE.0) GO TO 250
      IF (JA+9) 240,245,245
  240 LINE(1,I)=IDX(11)
      GO TO 250
  245 LINE(2,I)=IDX(11)
  250 CONTINUE
      FLAT=FLAT*BYIM
         SMAPJ(J)=FLAT
      WRITE(MEAN,'(F9.2)') FLAT
      WRITE (6,920) 
     *  NINT(LAT_DG(J,1)),J,((LINE(K,I),K=1,3),I=1,IM,INC),AVG
      DO 260 I=1,IM
      IF (FDATA(I,J,2).GE..5) GO TO 260
      DO 255 K=1,3
  255 LINE(K,I)=BLANK
  260 CONTINUE
c     WRITE (6,925) ((LINE(K,I),K=1,3),I=1,IM,INC)
      WRITE (6,925) ((LINE(K,I),K=1,3),I=1,IM,INC)
      WRITE (6,925) ((LINE(K,I),K=1,3),I=1,IM,INC)
  300 IF (JM.LE.24) WRITE (6,940)
      WRITE (6,930) (LON(I),I=1,IM+1,INC*2)
         if (iprint_pd(3).lt.9)
     *   WRITE(iu_pd(3)) TITLE(NT),XLB,SMAP,SMAPJ,-1.E30
      RETURN
c  **
  900 FORMAT('0',45X,A48)
  901 FORMAT ('1',33A4)
  902 FORMAT ('0',15X,'DAY',I6,', HR',I2,' (',I2,A5,I4,')',F9.0,
     *      '   TO   DAY',I6,', HR',I2,' (',I2,A5,I4,')',F9.0,
     *  '    DIF',F5.0,' HR')
  910 FORMAT('0LAT  J/I  ',117A1)
  920 FORMAT(2I4,3X,117A1)
  925 FORMAT('+',10X,108A1)
  930 FORMAT('0  LONG',19I6)
  940 FORMAT(' ')
      END

      BLOCK DATA BD_DIAG9
c  **
c  ** TITLES FOR SUBROUTINE DIAG9
c  **
      COMMON/D9COM/TITLE1,TITLE2,TITLE3,TITLE4
      CHARACTER*32 TITLE1(11)/
     *  ' INSTANTANE AM (10**9 J*S/M**2) ',
     *  ' CHANGE OF AM BY ADVECTION      ',
     *  ' CHANGE OF AM BY CORIOLIS FORCE ',
     *  ' CHANGE OF AM BY ADVEC + COR    ',
     *  ' CHANGE OF AM BY PRESSURE GRAD  ',
     *  ' CHANGE OF AM BY DYNAMICS       ',
     *  ' CHANGE OF AM BY SURFACE FRIC   ',
     *  ' CHANGE OF AM BY STRATOS DRAG   ',
     *  ' CHANGE OF AM BY FILTER         ',
     *  ' CHANGE OF AM BY DAILY RESTOR   ',
     *  ' SUM OF CHANGES (10**2 J/M**2)  '/
      CHARACTER*32 TITLE2(12)/
     *  '0INSTANTANEOUS KE (10**3 J/M**2)',
     *  ' CHANGE OF KE BY ADVECTION      ',
     *  ' CHANGE OF KE BY CORIOLIS FORCE ',
     *  ' CHANGE OF KE BY ADVEC + COR    ',
     *  ' CHANGE OF KE BY PRESSURE GRAD  ',
     *  ' CHANGE OF KE BY DYNAMICS       ',
     *  ' CHANGE OF KE BY MOIST CONVEC   ',
     *  ' CHANGE OF KE BY SURF + DRY CONV',
     *  ' CHANGE OF KE BY STRATOS DRAG   ',
     *  ' CHANGE OF KE BY FILTER         ',
     *  ' CHANGE OF KE BY DAILY RESTOR   ',
     *  ' SUM OF CHANGES (10**-3 W/M**2) '/
      CHARACTER*32 TITLE3(5)/
     *  ' INSTANTANEOUS MASS (KG/M**2)   ',
     *  ' CHANGE OF MASS BY DYNAMICS     ',
     *  ' CHANGE OF MASS BY FILTER       ',
     *  ' CHANGE OF MASS BY DAILY RESTOR ',
     *  ' SUM CHANGES (10**-8 KG/S/M**2) '/
      CHARACTER*32 TITLE4(8)/
     *  '0INSTANTANE TPE (10**5 J/M**2)  ',
     *  ' CHANGE OF TPE BY DYNAMICS      ',
     *  ' CHANGE OF TPE BY CONDENSATION  ',
     *  ' CHANGE OF TPE BY RADIATION     ',
     *  ' CHANGE OF TPE BY SURFACE INTER ',
     *  ' CHANGE OF TPE BY FILTER        ',
     *  ' CHANGE OF TPE BY DAILY RESTOR  ',
     *  ' SUM OF CHANGES (10**-2 W/M**2) '/
      END

c  *********************************************************************
c  *********************************************************************
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE DIAG9A (M)
c  **
c  ** THIS DIAGNOSTIC ROUTINE KEEPS TRACK OF THE CONSERVATION
c  ** PROPERTIES OF ANGULAR MOMENTUM, KINETIC ENERGY, MASS, AND
c  ** TOTAL POTENTIAL ENERGY
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      DIMENSION UX(IM,JM,*),VX(IM,JM,*)
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1),PK(IM,JM,LM)
      COMMON/WORK2/JLATP(JM),JLATV(JM),SCALE(36),FGLOB(36),FHEM(2,36),
     *  MLAT(JM,36),MAREA(JM)
      COMMON/WORK3/GBUDG(JM+3,80,4),CNSLAT(JM+3,36)
      COMMON/WORK4/PI(JM),AM(JM),RKE(JM),RMASS(JM),TPE(JM)
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM)
      COMMON/D9COM/TITLE(36)
      INTEGER NAMOFM(8)/1,6,1,1,7,8,9,10/
      INTEGER NKEOFM(8)/1,17,18,1,19,20,21,22/
      INTEGER NMSOFM(8)/1,25,1,1,1,1,26,27/
      INTEGER NPEOFM(8)/1,30,31,32,33,1,34,35/
      CHARACTER*4 HEMIS(2)/' SH ',' NH '/,DASH/'----'/,TITLE*32
c  **
c  ** THE PARAMETER M INDICATES WHEN DIAG9A IS BEING CALLED
c  ** M=1  INITIALIZE CURRENT A.M., K.E., MASS, AND T.P.E.
c  **   2  AFTER DYNAMICS
c  **   3  AFTER CONDENSATION
c  **   4  AFTER RADIATION
c  **   5  AFTER SURFACE INTERACTION AND DRY CONVECTION
c  **   6  AFTER STRATOSPHERIC DRAG
c  **   7  AFTER FILTER
c  **   8  AFTER DAILY RESTORATION
c  **
      GO TO (100,100,200,400,100,100,100,100),M
c  **
c  ** ANGULAR MOMENTUM
c  **
  100 PI(1)=FIM*P(1,1)
      PI(JM)=FIM*P(1,JM)
      DO 120 J=2,JM-1
      PI(J)=0.
      DO 120 I=1,IM
  120 PI(J)=PI(J)+P(I,J)
      DO 150 J=2,JM
      UMIL=0.
      DO 140 L=1,LM
      UMI=0.
      I=IM
      DO 130 IP1=1,IM
      UMI=UMI+U(I,J,L)*((P(I,J-1)+P(IP1,J-1))*DXYN(J-1)
     *  +(P(I,J)+P(IP1,J))*DXYS(J))
  130 I=IP1
  140 UMIL=UMIL+UMI*DSIG(L)
  150 AM(J)=RADIUS*OMEGA*COSV(J)*(PI(J-1)*DXYN(J-1)+PI(J)*DXYS(J))
     *  +.5*UMIL
      IF (M.LE.1) GO TO 180
      N=NAMOFM(M)
      DO 160 J=2,JM
  160 CONSRV(J,N)=CONSRV(J,N)+(AM(J)-CONSRV(J,1))
  180 DO 190 J=2,JM
  190 CONSRV(J,1)=AM(J)
c  **
c  ** KINETIC ENERGY
c  **
  200 DO 240 J=2,JM
      RKEIL=0.
      DO 230 L=1,LM
      RKEI=0.
      I=IM
      DO 220 IP1=1,IM
      RKEI=RKEI+(U(I,J,L)*U(I,J,L)+V(I,J,L)*V(I,J,L))
     *  *((P(I,J-1)+P(IP1,J-1))*DXYN(J-1)+(P(I,J)+P(IP1,J))*DXYS(J))
  220 I=IP1
  230 RKEIL=RKEIL+RKEI*DSIG(L)
  240 RKE(J)=RKEIL
      IF (M.LE.1) GO TO 280
      N=NKEOFM(M)
      DO 260 J=2,JM
  260 CONSRV(J,N)=CONSRV(J,N)+(RKE(J)-CONSRV(J,12))
  280 DO 290 J=2,JM
  290 CONSRV(J,12)=RKE(J)
      IF (M.EQ.6) GO TO 495
      IF (M.EQ.3.OR.M.EQ.5) GO TO 400
c  **
c  ** MASS
c  **
  300 RMASS(1)=FIM*P(1,1)
      RMASS(JM)=FIM*P(1,JM)
      DO 320 J=2,JM-1
      RMASS(J)=0.
      DO 320 I=1,IM
  320 RMASS(J)=RMASS(J)+P(I,J)
      IF (M.LE.1) GO TO 380
      N=NMSOFM(M)
      DO 360 J=1,JM
  360 CONSRV(J,N)=CONSRV(J,N)+(RMASS(J)-CONSRV(J,24))
  380 DO 390 J=1,JM
  390 CONSRV(J,24)=RMASS(J)
c  **
c  ** TOTAL POTENTIAL ENERGY
c  **
  400 SHA=RGAS/KAPA
      IF (DOPK.LE.0.) GO TO 420
      DO 410 L=1,LM
      DO 410 J=1,JM
      DO 410 I=1,IM
  410 PK(I,J,L)=EXPBYK(SIG(L)*P(I,J)+PTOP)
      DOPK=0.
  420 DO 460 J=1,JM
      IMAX=IM
      IF (J.EQ.1.OR.J.EQ.JM) IMAX=1
      TPEIL=0.
      DO 440 L=1,LM
      TPEI=0.
      DO 430 I=1,IMAX
  430 TPEI=TPEI+T(I,J,L)*PK(I,J,L)*P(I,J)
  440 TPEIL=TPEIL+TPEI*DSIG(L)
      SGEOI=0.
      DO 450 I=1,IMAX
  450 SGEOI=SGEOI+FDATA(I,J,1)*(P(I,J)+PTOP)
  460 TPE(J)=SGEOI+TPEIL*SHA
      TPE(1)=FIM*TPE(1)
      TPE(JM)=FIM*TPE(JM)
      IF (M.LE.1) GO TO 480
      N=NPEOFM(M)
      DO 470 J=1,JM
  470 CONSRV(J,N)=CONSRV(J,N)+(TPE(J)-CONSRV(J,29))
  480 DO 490 J=1,JM
  490 CONSRV(J,29)=TPE(J)
c  **
  495 CALL CLOCKS (MNOW)
      MDIAG=MDIAG+(MLAST-MNOW)
      MLAST=MNOW
      RETURN
c  **
c  **
      ENTRY DIAG9D (M,DT1,UX,VX)
      CALL CLOCKS (MBEGIN)
c  **
c  ** THE PARAMETER M INDICATES WHEN DIAG9D IS BEING CALLED
c  ** M=1  AFTER ADVECTION IN DYNAMICS
c  **   2  AFTER CORIOLIS FORCE IN DYNAMICS
c  **   3  AFTER PRESSURE GRADIENT FORCE IN DYNAMICS
c  **
      GO TO (500,600,600),M
c  **
c  ** CHANGE OF ANGULAR MOMENTUM AND KINETIC ENERGY BY ADVECTION
c  **
  500 PI(1)=FIM*PIT(1,1)
      PI(JM)=FIM*PIT(1,JM)
      DO 520 J=2,JM-1
      PI(J)=0.
      DO 520 I=1,IM
  520 PI(J)=PI(J)+PIT(I,J)
      DO 580 J=2,JM
      DUTIL=0.
      RKEIL=0.
      DO 560 L=1,LM
      DUTI=0.
      RKEI=0.
      DO 540 I=1,IM
      DUTI=DUTI+DUT(I,J,L)
  540 RKEI=RKEI+(UX(I,J,L)*DUT(I,J,L)+VX(I,J,L)*DVT(I,J,L))
      DUTIL=DUTIL+DUTI*DSIG(L)
  560 RKEIL=RKEIL+RKEI*DSIG(L)
      CONSRV(J,2)=CONSRV(J,2)+(DUTIL
     *  +DT1*RADIUS*OMEGA*COSV(J)*(PI(J-1)*RAVPN(J-1)+PI(J)*RAVPS(J)))
  580 CONSRV(J,13)=CONSRV(J,13)+RKEIL
      GO TO 680
c  **
c  ** CHANGE OF ANGULAR MOMENTUM AND KINETIC ENERGY BY CORIOLIS AND
c  ** PRESSURE GRADIENT FORCES
c  **
  600 DO 660 J=2,JM
      DUTIL=0.
      RKEIL=0.
      DO 640 L=1,LM
      DUTI=0.
      RKEI=0.
      DO 620 I=1,IM
      DUTI=DUTI+DUT(I,J,L)
  620 RKEI=RKEI+(UX(I,J,L)*DUT(I,J,L)+VX(I,J,L)*DVT(I,J,L))
      DUTIL=DUTIL+DUTI*DSIG(L)
  640 RKEIL=RKEIL+RKEI*DSIG(L)
      CONSRV(J,2*M-1)=CONSRV(J,2*M-1)+DUTIL
  660 CONSRV(J,2*M+10)=CONSRV(J,2*M+10)+RKEIL
c  **
  680 CALL CLOCKS (MEND)
      MINC=MBEGIN-MEND
      MDIAG=MDIAG+MINC
      MDYN=MDYN-MINC
      RETURN
c  **
c  **
      ENTRY DIAG9P
c  **
c  ** THIS ENTRY PRODUCES TABLES OF CONSERVATION QUANTITIES
c  **
      DO 720 J=1,JM
      JLATP(J)=INT(.5+(J-1.)*180./(JM-1))-90
  720 JLATV(J)=INT(.5+(J-1.5)*180./(JM-1))-90
c  ** CALCULATE SCALING FACTORS
      XWON=TWOPI/(DLON*FIM)
      IF (IDACC(12).LT.1) IDACC(12)=1
      NDAYS=IDACC(9)/2
      DTSRCE=DT*NDYN
      SCALE(1)=100.D-9*RADIUS/(GRAV*IDACC(12))
      SCALE(2)=100.D-2*RADIUS/(GRAV*IDACC(6)*DTSRCE+1.D-20)
      SCALE(3)=SCALE(2)
      SCALE(4)=SCALE(2)
      SCALE(5)=SCALE(2)
      SCALE(6)=100.D-2*RADIUS/(DTSRCE*GRAV*IDACC(7)+1.D-20)
      SCALE(7)=100.D-2*RADIUS/(DTSRCE*GRAV*IDACC(8)+1.D-20)
      SCALE(8)=SCALE(7)
      SCALE(9)=100.D-2*RADIUS/(NFILTR*DT*GRAV*IDACC(10)+1.D-20)
      SCALE(10)=100.D-2*RADIUS/(SDAY*GRAV*NDAYS+1.D-20)
      SCALE(11)=1.
      SCALE(12)=25.D-3/(GRAV*IDACC(12))
      SCALE(13)=100.E3/(DTSRCE*GRAV*IDACC(6)+1.D-20)
      SCALE(14)=SCALE(13)
      SCALE(15)=SCALE(13)
      SCALE(16)=SCALE(13)
      SCALE(17)=25.E3/(DTSRCE*GRAV*IDACC(7)+1.D-20)
      SCALE(18)=25.E3/(DTSRCE*GRAV*IDACC(8)+1.D-20)
      SCALE(19)=SCALE(18)
      SCALE(20)=SCALE(18)
      SCALE(21)=25.E3/(NFILTR*DT*GRAV*IDACC(10)+1.D-20)
      SCALE(22)=25.E3/(SDAY*GRAV*NDAYS+1.D-20)
      SCALE(23)=1.
      SCALE(24)=100.E0/(GRAV*IDACC(12))
      SCALE(25)=100.E8/(DTSRCE*GRAV*IDACC(7)+1.D-20)
      SCALE(26)=100.E8/(NFILTR*DT*GRAV*IDACC(10)+1.D-20)
      SCALE(27)=100.E8/(SDAY*GRAV*NDAYS+1.D-20)
      SCALE(28)=1.
      SCALE(29)=100.D-5/(GRAV*IDACC(12))
      SCALE(30)=100.E2/(DTSRCE*GRAV*IDACC(7)+1.D-20)
      SCALE(31)=100.E2/(DTSRCE*GRAV*IDACC(8)+1.D-20)
      SCALE(32)=SCALE(31)
      SCALE(33)=SCALE(31)
      SCALE(34)=100.E2/(NFILTR*DT*GRAV*IDACC(10)+1.D-20)
      SCALE(35)=100.E2/(SDAY*GRAV*NDAYS+1.D-20)
      SCALE(36)=1.
c  ** CALCULATE SUMMED QUANTITIES
      DO 740 J=1,JM
      CONSRV(J,4)=CONSRV(J,2)+CONSRV(J,3)
      CONSRV(J,11)=CONSRV(J,6)*SCALE(6)+CONSRV(J,7)*SCALE(7)
     *  +CONSRV(J,8)*SCALE(8)+CONSRV(J,9)*SCALE(9)
     *  +CONSRV(J,10)*SCALE(10)
      CONSRV(J,15)=CONSRV(J,13)+CONSRV(J,14)
      CONSRV(J,23)=CONSRV(J,17)*SCALE(17)+CONSRV(J,18)*SCALE(18)
     *  +CONSRV(J,19)*SCALE(19)+CONSRV(J,20)*SCALE(20)
     *  +CONSRV(J,21)*SCALE(21)+CONSRV(J,22)*SCALE(22)
      CONSRV(J,28)=CONSRV(J,25)*SCALE(25)+CONSRV(J,26)*SCALE(26)
     *  +CONSRV(J,27)*SCALE(27)
  740 CONSRV(J,36)=CONSRV(J,30)*SCALE(30)+CONSRV(J,31)*SCALE(31)
     *  +CONSRV(J,32)*SCALE(32)+CONSRV(J,33)*SCALE(33)
     *  +CONSRV(J,34)*SCALE(34)+CONSRV(J,35)*SCALE(35)
c  ** CALCULATE FINAL ANGULAR MOMENTUM
      JEQ=1+JM/2
      JEQM1=JEQ-1
      DO 760 N=1,11
      FEQ=CONSRV(JEQ,N)*SCALE(N)*COSV(JEQ)
      FGLOB(N)=FEQ
      FHEM(1,N)=.5*FEQ
      FHEM(2,N)=.5*FEQ
      CNSLAT(JEQ,N)=FEQ/(FIM*DXYV(JEQ))
      DO 750 JSH=2,JEQM1
      JNH=2+JM-JSH
      FSH=CONSRV(JSH,N)*SCALE(N)*COSV(JSH)
      FNH=CONSRV(JNH,N)*SCALE(N)*COSV(JNH)
      FGLOB(N)=FGLOB(N)+(FSH+FNH)
      FHEM(1,N)=FHEM(1,N)+FSH
      FHEM(2,N)=FHEM(2,N)+FNH
      CNSLAT(JSH,N)=FSH/(FIM*DXYV(JSH))
  750 CNSLAT(JNH,N)=FNH/(FIM*DXYV(JNH))
      FGLOB(N)=FGLOB(N)/AREAG
      FHEM(1,N)=FHEM(1,N)/(.5*AREAG)
  760 FHEM(2,N)=FHEM(2,N)/(.5*AREAG)
c  ** CALCULATE FINAL KINETIC ENERGY
      DO 780 N=12,23
      FEQ=CONSRV(JEQ,N)*SCALE(N)
      FGLOB(N)=FEQ
      FHEM(1,N)=.5*FEQ
      FHEM(2,N)=.5*FEQ
      CNSLAT(JEQ,N)=FEQ/(FIM*DXYV(JEQ))
      DO 770 JSH=2,JEQM1
      JNH=2+JM-JSH
      FSH=CONSRV(JSH,N)*SCALE(N)
      FNH=CONSRV(JNH,N)*SCALE(N)
      FGLOB(N)=FGLOB(N)+(FSH+FNH)
      FHEM(1,N)=FHEM(1,N)+FSH
      FHEM(2,N)=FHEM(2,N)+FNH
      CNSLAT(JSH,N)=FSH/(FIM*DXYV(JSH))
  770 CNSLAT(JNH,N)=FNH/(FIM*DXYV(JNH))
      FGLOB(N)=FGLOB(N)/AREAG
      FHEM(1,N)=FHEM(1,N)/(.5*AREAG)
  780 FHEM(2,N)=FHEM(2,N)/(.5*AREAG)
c  ** CALCUALTE FINAL MASS AND TOTAL POTENTIAL ENERGY
      DO 800 N=24,36
      FGLOB(N)=0.
      FHEM(1,N)=0.
      FHEM(2,N)=0.
      DO 790 JSH=1,JEQM1
      JNH=1+JM-JSH
      FSH=CONSRV(JSH,N)*SCALE(N)
      FNH=CONSRV(JNH,N)*SCALE(N)
      FGLOB(N)=FGLOB(N)+(FSH+FNH)*DXYP(JSH)
      FHEM(1,N)=FHEM(1,N)+FSH*DXYP(JSH)
      FHEM(2,N)=FHEM(2,N)+FNH*DXYP(JNH)
      CNSLAT(JSH,N)=FSH/FIM
  790 CNSLAT(JNH,N)=FNH/FIM
      FGLOB(N)=FGLOB(N)/AREAG
      FHEM(1,N)=FHEM(1,N)/(.5*AREAG)
  800 FHEM(2,N)=FHEM(2,N)/(.5*AREAG)
      AGLOB=1.D-10*AREAG*XWON
      AHEM=1.D-10*(.5*AREAG)*XWON
c  ** LOOP OVER HEMISPHERES
      INC=1+(JM-1)/24
      IHOUR0=TOFDY0+.5
      IHOUR=TOFDAY+.5
      TAUDIF=TAU-TAU0
      DO 810 N=1,36
      DO 805 J=1,JM
  805 MLAT(J,N)=NINT(CNSLAT(J,N))
         CNSLAT(JM+1,N)=FHEM(1,N)
         CNSLAT(JM+2,N)=FHEM(2,N)
         CNSLAT(JM+3,N)=FGLOB(N)
  810 CONTINUE
      DO 870 JHEMI=2,1,-1
      WRITE (6,901) XLABEL
      WRITE (6,902) IDAY0,IHOUR0,JDATE0,JMNTH0,JYEAR0,TAU0,IDAY,IHOUR,
     *  JDATE,JMONTH,JYEAR,TAU,TAUDIF
      JP1=1+(JHEMI-1)*(JEQ-1)
      JPM=JHEMI*(JEQ-1)
      JV1=2+(JHEMI-1)*(JEQ-2)
      JVM=JEQ+(JHEMI-1)*(JEQ-2)
c  ** PRODUCE TABLES FOR ANGULAR MOMENTUM AND KINETIC ENERGY
      WRITE (6,903) (DASH,J=JV1,JVM,INC)
      WRITE (6,904) HEMIS(JHEMI),(NINT(LAT_DG(JX,2)),JX=JVM,JV1,-INC)
      WRITE (6,903) (DASH,J=JV1,JVM,INC)
      DO 820 N=1,23
  820 WRITE (6,905) TITLE(N),FGLOB(N),FHEM(JHEMI,N),
     *  (MLAT(JX,N),JX=JVM,JV1,-INC)
      DO 830 J=JV1,JVM
  830 MAREA(J)=1.D-10*XWON*FIM*DXYV(J)+.5
      WRITE (6,906) AGLOB,AHEM,(MAREA(JX),JX=JVM,JV1,-INC)
c  ** PRODUCE TABLES FOR MASS AND TOTAL POTENTIAL ENERGY
      WRITE (6,907)
      WRITE (6,903) (DASH,J=JP1,JPM,INC)
      WRITE (6,904) HEMIS(JHEMI),(NINT(LAT_DG(JX,1)),JX=JPM,JP1,-INC)
      WRITE (6,903) (DASH,J=JP1,JPM,INC)
      DO 840 N=24,36
  840 WRITE (6,905) TITLE(N),FGLOB(N),FHEM(JHEMI,N),
     *  (MLAT(JX,N),JX=JPM,JP1,-INC)
      DO 850 J=JP1,JPM
  850 MAREA(J)=1.D-10*XWON*FIM*DXYP(J)+.5
      WRITE (6,906) AGLOB,AHEM,(MAREA(JX),JX=JPM,JP1,-INC)
  870 CONTINUE
      RETURN
c  **
  901 FORMAT ('1',33A4)
  902 FORMAT ('0CONSERVATION QUANTITIES   DAY',I6,', HR',I2,' (',I2,
     *  A5,I4,')',F9.0,'   TO   DAY',I6,', HR',I2,' (',I2,A5,I4,')',
     *  F9.0,'   DIF',F5.0,' HR'/)
  903 FORMAT (1X,25('--'),13(A4,'--'))
  904 FORMAT (35X,'GLOBAL',A7,2X,13I6)
  905 FORMAT (A32,2F9.2,1X,13I6)
  906 FORMAT ('0AREA (10**10 M**2)',F22.1,F9.1,1X,13I6)
  907 FORMAT ('0')
      END
      SUBROUTINE DIAG5A (M5,NDT)
c  **
c  ** THIS DIAGNOSTICS ROUTINE PRODUCES A SPECTRAL ANALYSIS OF KINETIC
c  ** AND AVAILABLE POTENTIAL ENERGIES AND THEIR TRANSFER RATES BY
c  ** VARIOUS ATMOSPHERIC PROCESSES.
c  **
c  ** THE PARAMETER M INDICATES WHAT IS STORED IN SPECA(N,M,KSPHER),
c  ** IT ALSO INDICATES WHEN DIAG5A IS BEING CALLED.
c  ** M=1  MEAN STANDING KINETIC ENERGY            BEFORE SOURCES
c  **   2  MEAN KINETIC ENERGY                     BEFORE DYNAMICS
c  **   3  MEAN POTENTIAL ENERGY
c  **   4  CONVERSION OF K.E. BY ADVECTION         AFTER ADVECTION
c  **   5  CONVERSION OF K.E. BY CORIOLIS FORCE    AFTER CORIOLIS TERM
c  **   6  CONVERSION FROM P.E. INTO K.E.          AFTER PRESS GRAD FORC
c  **   7  CHANGE OF K.E. BY DYNAMICS              AFTER DYNAMICS
c  **   8  CHANGE OF P.E. BY DYNAMICS
c  **   9  CHANGE OF K.E. BY CONDENSATION          AFTER CONDENSATION
c  **  10  CHANGE OF P.E. BY CONDENSATION
c  **  11  CHANGE OF P.E. BY RADIATION             AFTER RADIATION
c  **  12  CHANGE OF K.E. BY SURFACE               AFTER SURFACE
c  **  13  CHANGE OF P.E. BY SURFACE
c  **  14  CHANGE OF K.E. BY FILTER                AFTER FILTER
c  **  15  CHANGE OF P.E. BY FILTER
c  **  16  CHANGE OF K.E. BY DAILY                 AFTER DAILY
c  **  17  CHANGE OF P.E. BY DAILY
c  **  18  UNUSED
c  **  19  LAST KINETIC ENERGY
c  **  20  LAST POTENTIAL ENERGY
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
c  ****
c  **** MFS (CHANGED CODE)
c  **** Changed the model from 64 bit reals to 32 bit reals.  The PowerPC
c  **** 601,603,604,750 all use 32 bit reals.  The double precision were
c  **** better for science but slower.

c      REAL*8 KE
      REAL*4 KE
c  ****
c  **** END (CHANGED CODE)
c  ****
      DOUBLE PRECISION TPE,SUMI,SUMT
      COMMON/WORK1/PIT(IM,JM),SD(IM,JM,LM-1),PK(IM,JM,LM)
      COMMON/WORK3/GBUDG(JM+3,80,4),FATPE(8,2)
      COMMON/WORK5/DUT(IM,JM,LM),DVT(IM,JM,LM),
     *  X(IM),FCUV(2,IMH+1,JM,LM,2),
     *  FC(2,IMH+1),KE(IMH+1,8),APE(IMH+1,8),VAR(IMH+1,4),TPE(2),
     *  SQRTM(IM,JM),SQRTP(IM,JM),THJSP(LM),THJNP(LM),THGM(LM),
     *  SCALE(20),MN(20),F0(20),FNSUM(20)
      DIMENSION UX(IM,JM,*)
      DIMENSION MTPEOF(20),MAPEOF(8)
      CHARACTER*8 LATITD(4)/'SOUTHERN','NORTHERN',' EQUATOR','45 NORTH'/
      CHARACTER*16 SPHERE(2)/'STRATOSPHERE','TROPOSPHERE'/
      DATA MTPEOF/0,0,1,0,0,0,0,2,0,3,  4,0,5,0,6,0,7,0,0,8/
      DATA MAPEOF/3,8,10,11,13,15,17,20/,IZERO/0/
      NM=1+IM/2
      NM8=NM*8
      JEQ=1+JM/2
      JEQM1=JEQ-1
      J45N=2.+.75*(JM-1)
      IJL2=IM*JM*LM*2
      SHA=RGAS/KAPA
      MKE=M5
      MAPE=M5
c  **
c  ** KSPHER=1 SOUTHERN STRATOSPHERE       3 NORTHERN STRATOSPHERE
c  **        2 SOUTHERN TROPOSPHERE        4 NORTHERN TROPOSPHERE
c  **
c  **        5 EQUATORIAL STRATOSPHERE     7 45 DEG NORTH STRATOSPHERE
c  **        6 EQUATORIAL TROPOSPHERE      8 45 DEG NORTH TROPOSPHERE
c  **
      GO TO (200,200,810,100,100,  100,200,810,205,810,
     *       296,205,810,205,810,  205,810,810,810,810),M5
c  **
c  ** KINETIC ENERGY
c  **
c  ** TRANSFER RATES FOR KINETIC ENERGY IN THE DYNAMICS
  100 CALL CLOCKS (MBEGIN)
      DO 110 N=1,NM8
  110 KE(N,1)=0.
      DO 170 L=1,LM
      KSPHER=2
      IF (L.GE.LS1) KSPHER=1
      DO 170 J=2,JM
      DO 170 K=IZERO,LM,LM
      CALL GETAN(DUT(1,J,L+K),FC)
      DO 120 N=1,NM
  120 X(N)=.5*FIM*(FC(1,N)*FCUV(1,N,J,L+K,1)+FC(2,N)*FCUV(2,N,J,L+K,1))
      X(1)=X(1)+X(1)
      X(NM)=X(NM)+X(NM)
      IF (J.EQ.JEQ) GO TO 150
      DO 130 N=1,NM
  130 KE(N,KSPHER)=KE(N,KSPHER)+X(N)*DSIG(L)
      IF (J.NE.J45N) GO TO 170
      DO 140 N=1,NM
  140 KE(N,KSPHER+4)=KE(N,KSPHER+4)+X(N)*DSIG(L)
      GO TO 170
  150 DO 160 N=1,NM
      KE(N,KSPHER+4)=KE(N,KSPHER+4)+X(N)*DSIG(L)
      KE(N,KSPHER)=KE(N,KSPHER)+.5D0*X(N)*DSIG(L)
  160 KE(N,KSPHER+2)=KE(N,KSPHER+2)+.5D0*X(N)*DSIG(L)
      IF (K.EQ.LM) KSPHER=KSPHER+2
  170 CONTINUE
      DO 180 KS=1,8
      DO 180 N=1,NM
  180 SPECA(N,MKE,KS)=SPECA(N,MKE,KS)+KE(N,KS)/NDT
      CALL CLOCKS (MEND)
      MINC=MBEGIN-MEND
      MDIAG=MDIAG+MINC
      MDYN=MDYN-MINC
      RETURN
c  ** MASS FOR KINETIC ENERGY
  200 I=IM
      DO 202 J=2,JM
      DO 202 IP1=1,IM
      SQRTM(I,J)=SQRT(.5*((P(I,J)+P(IP1,J))*DXYS(J)+(P(I,J-1)+
     *  P(IP1,J-1))*DXYN(J-1)))
  202 I=IP1
c  **
  205 MAPE=MKE+1
      DO 206 N=1,NM8
  206 KE(N,1)=0.
c  ** CURRENT KINETIC ENERGY
      DO 240 L=1,LM
      KSPHER=2
      IF (L.GE.LS1) KSPHER=1
      DO 240 J=2,JM
      DO 240 K=IZERO,LM,LM
      DO 210 I=1,IM
  210 X(I)=U(I,J,L+K)*SQRTM(I,J)
      CALL FRTR (X)
      IF (J.EQ.JEQ) GO TO 225
      DO 220 N=1,NM
  220 KE(N,KSPHER)=KE(N,KSPHER)+X(N)*DSIG(L)
      IF (J.NE.J45N) GO TO 240
      DO 222 N=1,NM
  222 KE(N,KSPHER+4)=KE(N,KSPHER+4)+X(N)*DSIG(L)
      GO TO 240
  225 DO 230 N=1,NM
      KE(N,KSPHER+4)=KE(N,KSPHER+4)+X(N)*DSIG(L)
      KE(N,KSPHER)=KE(N,KSPHER)+.5D0*X(N)*DSIG(L)
  230 KE(N,KSPHER+2)=KE(N,KSPHER+2)+.5D0*X(N)*DSIG(L)
      IF (K.EQ.LM) KSPHER=KSPHER+2
  240 CONTINUE
      IF (NDT.EQ.0) GO TO 260
c  ** TRANSFER RATES AS DIFFERENCES OF KINETIC ENERGY
      DO 250 KS=1,8
      DO 250 N=1,NM
  250 SPECA(N,MKE,KS)=SPECA(N,MKE,KS)+(KE(N,KS)-SPECA(N,19,KS))/NDT
  260 DO 270 KS=1,8
      DO 270 N=1,NM
  270 SPECA(N,19,KS)=KE(N,KS)
c  **
c  ** POTENTIAL ENERGY
c  **
      IF (DOPK.EQ.-1.) GO TO 296
c  ** COMPUTE SQRTP = SQRT(P) AND PK = P**KAPA
      SQRTP1=SQRT(P(1,1))
      SQRTPM=SQRT(P(1,JM))
      DO 290 J=2,JM-1
      DO 290 I=1,IM
  290 SQRTP(I,J)=SQRT(P(I,J))
      DO 292 I=1,IM
      SQRTP(I,1)=SQRTP1
  292 SQRTP(I,JM)=SQRTPM
      IF (DOPK.EQ.0.) GO TO 296
      DO 294 L=1,LM
      DO 294 J=1,JM
      DO 294 I=1,IM
  294 PK(I,J,L)=EXPBYK(SIG(L)*P(I,J)+PTOP)
  296 DOPK=-1.
      DO 298 N=1,NM8
  298 APE(N,1)=0.
c  ** CURRENT AVAILABLE POTENTIAL ENERGY
      LUP=0
  300 LUP=LUP+1
      THJSP(LUP)=T(1,1,LUP)*SQRTP(1,1)
      THJNP(LUP)=T(1,JM,LUP)*SQRTP(1,JM)
      THGSUM=FIM*(THJSP(LUP)*DXYP(1)+THJNP(LUP)*DXYP(JM))
      DO 320 J=2,JM-1
      THJSUM=0.
      DO 310 I=1,IM
  310 THJSUM=THJSUM+T(I,J,LUP)*SQRTP(I,J)
  320 THGSUM=THGSUM+THJSUM*DXYP(J)
      THGM(LUP)=THGSUM/AREAG
      IF (LUP.GE.2) GO TO 350
      LDN=LUP
      L=LUP
      GO TO 300
  350 DO 360 JHEMI=1,2
      DO 360 N=2,NM
  360 VAR(N,JHEMI)=0.
      VAR(1,1)=.5*(THJSP(L)-THGM(L))**2*DXYP(1)*FIM
      VAR(1,2)=.5*(THJNP(L)-THGM(L))**2*DXYP(JM)*FIM
      GMEAN=((THJSP(LUP)-THJSP(LDN))*DXYP(1)*(SIG(L)*P(1,1)+PTOP)/
     *  (SQRTP1*P(1,1)*PK(1,1,L)) + (THJNP(LUP)-THJNP(LDN))*DXYP(JM)*
     *  (SIG(L)*P(1,JM)+PTOP)/(SQRTPM*P(1,JM)*PK(1,JM,L)))*FIM
      JHEMI=1
      DO 388 J=2,JM-1
      GMSUM=0.
      DO 370 I=1,IM
      X(I)=T(I,J,L)*SQRTP(I,J)-THGM(L)
  370 GMSUM=GMSUM+(T(I,J,LUP)-T(I,J,LDN))*(SIG(L)*P(I,J)+PTOP)/
     *  (P(I,J)*PK(I,J,L))
      GMEAN=GMEAN+GMSUM*DXYP(J)
      CALL FRTR (X)
      DO 380 N=1,NM
  380 VAR(N,JHEMI)=VAR(N,JHEMI)+X(N)*DXYP(J)
      IF (J.NE.JEQ-1) GO TO 384
      DO 382 N=1,NM
  382 VAR(N,3)=X(N)*DXYP(J)
      JHEMI=2
  384 IF (J.NE.J45N-1) GO TO 388
      DO 386 N=1,NM
  386 VAR(N,4)=X(N)*DXYP(J)
  388 CONTINUE
      GMEAN=DSIG(L)*AREAG*(SIG(LDN)-SIG(LUP))/GMEAN
      KS=2
      IF (L.GE.LS1) KS=1
      DO 400 JHEMI=1,4
      DO 390 N=1,NM
  390 APE(N,KS)=APE(N,KS)+VAR(N,JHEMI)*GMEAN
  400 KS=KS+2
      IF (L.EQ.LM) GO TO 450
      LDN=L
      L=LUP
      IF (LUP.LT.LM) GO TO 300
      GO TO 350
c  ** CURRENT TOTAL POTENTIAL ENERGY
  450 DO 480 JHEMI=1,2
      JP=1+(JM-1)*(JHEMI-1)
      SUMT=0.
      DO 455 L=1,LM
  455 SUMT=SUMT+T(1,JP,L)*PK(1,JP,L)*DSIG(L)
      TPE(JHEMI)=FIM*DXYP(JP)*(FDATA(1,JP,1)*(P(1,JP)+PTOP)+
     *  SUMT*SHA*P(1,JP))
      DO 480 JH=2,JEQM1
      J=JH+(JEQM1-1)*(JHEMI-1)
      SUMI=0.
      DO 470 I=1,IM
      SUMT=0.
      DO 460 L=1,LM
  460 SUMT=SUMT+T(I,J,L)*PK(I,J,L)*DSIG(L)
  470 SUMI=SUMI+FDATA(I,J,1)*(P(I,J)+PTOP)+SUMT*SHA*P(I,J)
  480 TPE(JHEMI)=TPE(JHEMI)+SUMI*DXYP(J)
      IF (NDT.EQ.0) GO TO 520
      MTPE=MTPEOF(MAPE)
c  ** TRANSFER RATES AS DIFFERENCES FOR POTENTIAL ENERGY
      DO 510 KS=1,8
      DO 510 N=1,NM
  510 SPECA(N,MAPE,KS)=SPECA(N,MAPE,KS)+(APE(N,KS)-SPECA(N,20,KS))/NDT
      ATPE(MTPE,1)=ATPE(MTPE,1)+(TPE(1)-ATPE(8,1))/NDT
      ATPE(MTPE,2)=ATPE(MTPE,2)+(TPE(2)-ATPE(8,2))/NDT
  520 DO 530 KS=1,8
      DO 530 N=1,NM
  530 SPECA(N,20,KS)=APE(N,KS)
      ATPE(8,1)=TPE(1)
      ATPE(8,2)=TPE(2)
      CALL CLOCKS (MNOW)
      MDIAG=MDIAG+MLAST-MNOW
      MLAST=MNOW
      IF (M5.NE.2) RETURN
c  ** ACCUMULATE MEAN KINETIC ENERGY AND MEAN POTENTIAL ENERGY
      IDACC(7)=IDACC(7)+1
      DO 550 KS=1,8
      DO 550 N=1,NM
      SPECA(N,2,KS)=SPECA(N,2,KS)+KE(N,KS)
  550 SPECA(N,3,KS)=SPECA(N,3,KS)+APE(N,KS)
      ATPE(1,1)=ATPE(1,1)+TPE(1)
      ATPE(1,2)=ATPE(1,2)+TPE(2)
      RETURN
c  **
      ENTRY DIAG5F(UX)
c  ** FOURIER COEFFICIENTS FOR CURRENT WIND FIELD
c  **
      CALL CLOCKS (MBEGIN)
      DO 590 K=IZERO,LM,LM
      DO 590 L=1,LM
      DO 590 J=2,JM
  590 CALL GETAN(UX(1,J,L+K),FCUV(1,1,J,L+K,1))
      IDACC(6)=IDACC(6)+1
      CALL CLOCKS (MEND)
      MINC=MBEGIN-MEND
      MDIAG=MDIAG+MINC
      MDYN=MDYN-MINC
      RETURN
c  **
      ENTRY DIAG5P
c  ** THIS ENTRY PRINTS THE SPECTRAL ANALYSIS TABLES
c  **
      NM=1+IM/2
      IF (IDACC(12).LT.1) IDACC(12)=1
      IF (SKIPSE.GE.1.) GO TO 600
      JEQ=1+JM/2
      J45N=2.+.75*(JM-1)
c  **
c  ** STANDING KINETIC ENERGY
c  **
      DO 710 K=1,8
      DO 710 N=1,NM
  710 SPECA(N,1,K)=0.
      DO 770 L=1,LM
      KSPHER=2
      IF (L.GE.LS1) KSPHER=1
      DO 770 J=2,JM
      IF (AJK(J,L,2).LE.1.D-20) GO TO 770
      FACTOR=FIM*DXYV(J)/AJK(J,L,2)
      DO 769 K=IZERO,LM,LM
      DO 720 I=1,IM
  720 X(I)=AIJK(I,J,L+K,1)
      CALL FRTR (X)
      IF (J.EQ.JEQ) GO TO 750
      DO 730 N=1,NM
  730 SPECA(N,1,KSPHER)=SPECA(N,1,KSPHER)+X(N)*FACTOR
      IF (J.NE.J45N) GO TO 769
      DO 740 N=1,NM
  740 SPECA(N,1,KSPHER+4)=SPECA(N,1,KSPHER+4)+X(N)*FACTOR
      GO TO 769
  750 DO 760 N=1,NM
      SPECA(N,1,KSPHER+4)=SPECA(N,1,KSPHER+4)+X(N)*FACTOR
      SPECA(N,1,KSPHER)=SPECA(N,1,KSPHER)+.5*X(N)*FACTOR
  760 SPECA(N,1,KSPHER+2)=SPECA(N,1,KSPHER+2)+.5*X(N)*FACTOR
      IF (K.EQ.LM) KSPHER=KSPHER+2
  769 CONTINUE
  770 CONTINUE
c  **
  600 SCALE(1)=100.D-17/(GRAV*IDACC(4)+1.D-20)
      SCALE(19)=100.D-17/(GRAV*IDACC(12))
      SCALE(20)=SCALE(19)*RGAS
      SCALE(2)=SCALE(19)*IDACC(12)/(IDACC(7)+1.D-20)
      SCALE(3)=SCALE(2)*RGAS
      SCALE(4)=100.D-12/(GRAV*DT*IDACC(6)+1.D-20)
      SCALE(5)=SCALE(4)
      SCALE(6)=SCALE(4)
      SCALE(7)=100.D-12/(GRAV*DT*(IDACC(7)+1.D-20))
      SCALE(8)=SCALE(7)*RGAS
      SCALE(9)=100.D-12/(GRAV*DT*(IDACC(8)+1.D-20))
      SCALE(10)=SCALE(9)*RGAS
      SCALE(11)=SCALE(10)
      SCALE(12)=SCALE(9)
      SCALE(13)=SCALE(10)
      SCALE(14)=100.D-12/(GRAV*DT*(IDACC(10)+1.D-20))
      SCALE(15)=SCALE(14)*RGAS
      SCALE(16)=100.D-12/(GRAV*DT*(IDAY-IDAY0+1.D-20))
      SCALE(17)=SCALE(16)*RGAS
      SCALE(18)=100.D-17/(GRAV*IDACC(4)+1.D-20)
      DO 605 K=1,20
  605 SCALE(K)=(TWOPI/(DLON*FIM))*SCALE(K)
      IUNITJ=17
      IUNITW=12
      IHOUR0=TOFDY0+.5
      IHOUR=TOFDAY+.5
      DO 690 KPAGE=1,4
c  ** WRITE HEADINGS
      WRITE (6,901) XLABEL
      WRITE (6,902) IDAY0,IHOUR0,JDATE0,JMNTH0,JYEAR0,IDAY,IHOUR,JDATE,
     *  JMONTH,JYEAR,IUNITJ,IUNITW
      DO 670 KROW=1,2
      IF (JM.GE.25.AND.KROW.EQ.2) WRITE (6,901)
      WRITE (6,903) LATITD(KPAGE),SPHERE(KROW)
      KSPHER=2*(KPAGE-1)+KROW
c  ** WRITE KINETIC AND AVAILABLE POTENTIAL ENERGY BY WAVE NUMBER
      DO 610 M=1,20
      F0(M)=SPECA(1,M,KSPHER)*SCALE(M)
      MN(M)=NINT(F0(M))
  610 FNSUM(M)=0.
      WRITE (6,904) MN
      DO 630 N=2,NM
      KSPHER=2*(KPAGE-1)+KROW
      DO 620 M=1,20
      FNM=SPECA(N,M,KSPHER)*SCALE(M)
      MN(M)=NINT(FNM)
  620 FNSUM(M)=FNSUM(M)+FNM
      NM1=N-1
  630 WRITE (6,905) NM1,MN
      DO 640 M=1,20
  640 MN(M)=NINT(FNSUM(M))
      WRITE (6,906) MN
      DO 650 M=1,20
  650 MN(M)=NINT(FNSUM(M)+F0(M))
      WRITE (6,907) MN
  670 CONTINUE
      IF (KPAGE.GE.3) GO TO 690
c  ** WRITE TOTAL POTENTIAL ENERGY
      DO 680 MTPE=1,8
      MAPE=MAPEOF(MTPE)
         FATPE(MTPE,KPAGE)=ATPE(MTPE,KPAGE)*SCALE(MAPE)/RGAS
  680 MN(MTPE)=NINT(FATPE(MTPE,KPAGE))
      WRITE (6,909) (MN(MTPE),MTPE=1,8)
      IF (KPAGE.NE.2) GO TO 690
      DO 685 M=1,20
  685 SCALE(M)=SCALE(M)*10.
      IUNITJ=16
      IUNITW=11
  690 CONTINUE
      RETURN
c  **
  810 WRITE (6,910) M
      STOP 29
  901 FORMAT ('1',33A4)
  902 FORMAT ('0**  SPECTRAL ANALYSIS **  DAY',I6,', HR',I2,' (',I2,
     *  A5,I4,')   TO   DAY',I6,', HR',I2,' (',I2,A5,I4,
     *  ')   UNITS 10**',I2,' JOULES AND 10**',I2,' WATTS')
  903 FORMAT ('0',50X,A8,1X,A16/
     *  13X,'MEAN',19X,'DYNAMICS',25X,'SOURCES',16X,'FILTER',8X,
     *     'DAILY',4X,'PR SURF',5X,'LAST'/
     *'   N    SKE   KE   APE    KADV  KCOR   P-K  KDYN  PDYN   ',
     *     'KCNDS PCNDS   PRAD KSURF PSURF   KFIL  PFIL   KGMP  PGMP',
     *     '    KE',6X,'KE   APE')
  904 FORMAT ( '0  0',I7,I5,I6,I8,4I6,I8,I6,I7,2I6,I7,I6,I7,2I6,I8,I6/)
  905 FORMAT (     I4,I7,I5,I6,I8,4I6,I8,I6,I7,2I6,I7,I6,I7,2I6,I8,I6)
  906 FORMAT (' EDDY',I6,I5,I6,I8,4I6,I8,I6,I7,2I6,I7,I6,I7,2I6,I8,I6)
  907 FORMAT ('0TOTL',I6,I5,I6,I8,4I6,I8,I6,I7,2I6,I7,I6,I7,2I6,I8,I6)
  908 FORMAT ('0')
  909 FORMAT (/'0TPE',I18,I32,I14,I7,I12,2I13,I20)
  910 FORMAT ('0INCORRECT VALUE OF M WHEN CALLING DIAG5A.  M=',I5)
      END

      BLOCK DATA BD_DIAG6
c  **
c  ** TITLES FOR SUBROUTINE DIAG6
c  **
      COMMON/D6COM/TITLE
      CHARACTER*8 TITLE(50)/
     *  '0INC SW ',' P ALBD ',' G ALBD ',' ABS ATM',' E CNDS ',
     *  '0SRF PRS',' PT 5   ',' PT 4   ',' PT 3   ',' PT 2   ',
     *  ' PT 1   ',' TS     ',' TG1    ','0Q 5   ',' Q 4   ',
     *  ' Q 3   ',' Q 2   ',' Q 1   ',' QS    ',' QG    ',
     *  '0CLD 6  ',' CLD 5  ',' CLD 4  ',' CLD 3  ',' CLD 2  ',
     *  ' CLD 1  ',' COVER  ','0SW ON G',' LW AT G',' SNSB HT',
     *  ' LAT HT ',' HEAT Z0','0UG*10  ',' VG*10  ',' WG*10  ',
     *  ' US*10  ',' VS*10  ',' WS*10  ',' ALPHA0 ','0RIS1*E2',
     *  ' RIGS*E2',' CDM*E4 ',' CDH*E4 ',' DGS*10 ',' EDS1*10',
     *  '0PPBL   ',' DC FREQ',' LDC*10 ','0PRC*10 ',' EVP*10 '/
      END
      SUBROUTINE DIAG6
c  **
c  ** THIS SUBROUTINE PRINTS THE DIURNAL CYCLE OF SOME QUANTITIES
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      DIMENSION SCALE(50),MHOUR(25),XHOUR(25)
      COMMON/D6COM/TITLE(50)
      CHARACTER*8 TITLE
      DATA SCALE/1.,2*100.,2*1.,  5*1.,  3*1.,2*1.D5,  5*1.D5,
     *  5*100.,  2*100.,3*1.,  2*1.,3*10.,  3*10.,1.,100.,
     *  100.,2*1.D4,2*10.,  1.,100.,10.,2*1./
c  **
      NDAYS=IDACC(9)/2
      IF (NDAYS.LE.0) RETURN
      DTCNDS=NCNDS*DT
      DTSURF=NDYN*DT/NSURF
      BYIDAC=1./NDAYS
      SCALE(5)=100.*RGAS/(KAPA*GRAV*DTCNDS)
      SCALE(28)=1./DTSURF
      SCALE(29)=1./DTSURF
      SCALE(30)=1./DTSURF
      SCALE(31)=1./DTSURF
      SCALE(32)=1./DTSURF
      SCALE(39)=360./TWOPI
      SCALE(49)=100.*100.*SDAY/(DTCNDS*GRAV)
      SCALE(50)=100.*SDAY/DTSURF
c  **
      IREGF=1
      IREGL=4
      IF (KDIAG(6).GT.0) IREGL=4-KDIAG(6)
      IF (KDIAG(6).LT.0.AND.KDIAG(6).GT.-5) IREGF=-KDIAG(6)
      IF (KDIAG(6).LT.0) IREGL=IREGF
      DO 500 KR=IREGF,IREGL
      JY0=JYEAR0-1900
      JY=JYEAR-1900
      WRITE (6,901) (XLABEL(K),K=1,27),JDATE0,JMNTH0,JY0,JDATE,JMONTH,JY
      WRITE (6,903) NAMD6(KR),IJD6(1,KR),IJD6(2,KR),(I,I=1,24)
      DO 500 KQ=1,50
      IF (KQ.EQ.48) GO TO 200
c  ** NORMAL QUANTITIES
      AVE=0.
      DO 120 IH=1,24
      AVE=AVE+ADAILY(IH,KQ,KR)
  120 XHOUR(IH)=ADAILY(IH,KQ,KR)*SCALE(KQ)*BYIDAC
      XHOUR(25)=AVE/24.*SCALE(KQ)*BYIDAC
      GO TO 480
c  ** RATIO OF TWO QUANTITIES
  200 AVEN=0.
      AVED=0.
      DO 220 IH=1,24
      AVEN=AVEN+ADAILY(IH,KQ,KR)
      AVED=AVED+ADAILY(IH,KQ-1,KR)
  220 XHOUR(IH)=ADAILY(IH,KQ,KR)*SCALE(KQ)/(ADAILY(IH,KQ-1,KR)+1.D-20)
      XHOUR(25)=AVEN*SCALE(KQ)/(AVED+1.D-20)
  480 CONTINUE
      DO 490 IS=1,25
      MHOUR(IS)=NINT(XHOUR(IS))
  490 CONTINUE
  500 WRITE (6,904) TITLE(KQ),MHOUR
      RETURN
c  **
  901 FORMAT ('1',27A4,I4,1X,A3,I3,' TO',I3,1X,A3,I3)
  903 FORMAT ('0',A4,I2,',',I2,' ',I2,23I5,'  AVE')
  904 FORMAT (A8,25I5)
      END
      SUBROUTINE DIAG4A
c  **
c  ** THIS SUBROUTINE PRODUCES A TIME HISTORY OF ENERGIES
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      COMMON/WORK1/SUM(20),IK(20)
      COMMON/WORK3/GBUDG(JM+3,80,4),EHIST(20,101)
      DIMENSION SCALE(20)
      IF (IDACC(4).LE.0.OR.IDACC(7).LE.0) RETURN
      JEQ=2.+.5*(JM-1)
      NM=1+IM/2
c  **
c  ** LOAD ENERGIES INTO TIME HISTORY ARRAY
c  **
      IDACC5=IDACC(5)+1
      IF (IDACC5.GT.100) RETURN
      IF (SKIPSE.EQ.1.) GO TO 540
c  ** CALCULATE CURRENT SEKE
      BYIADA=1./IDACC(4)
      DO 530 L=1,LM
      KS=5
      IF (L.GE.LS1) KS=15
      DO 530 J=2,JM
      IF (AJK(J,L,2).LE.1.D-20) GO TO 530
      PU4TI=0.
      PV4TI=0.
      SKE4I=0.
      DO 510 I=1,IM
      PU4TI=PU4TI+AIJK(I,J,L,1)
      PV4TI=PV4TI+AIJK(I,J,L,2)
  510 SKE4I=SKE4I+(AIJK(I,J,L,1)*AIJK(I,J,L,1)
     *            +AIJK(I,J,L,2)*AIJK(I,J,L,2))/(AIJK(I,J,L,4)+1.D-20)
      SEKE=(SKE4I-(PU4TI*PU4TI+PV4TI*PV4TI)/AJK(J,L,2))*DXYV(J)*BYIADA
      IF (J.EQ.JEQ) GO TO 520
      ENERGY(KS,IDACC5)=ENERGY(KS,IDACC5)+SEKE
      GO TO 530
  520 ENERGY(KS,IDACC5)=ENERGY(KS,IDACC5)+.5*SEKE
      ENERGY(KS+1,IDACC5)=ENERGY(KS+1,IDACC5)+.5*SEKE
      KS=KS+1
  530 CONTINUE
c  ** OTHER ENERGIES COME FROM LATEST SPECTRAL ANALYSIS
  540 ENERGY(1,IDACC5)=SPECA(1,19,2)
      ENERGY(2,IDACC5)=SPECA(1,19,4)
      ENERGY(7,IDACC5)=SPECA(1,20,2)
      ENERGY(8,IDACC5)=SPECA(1,20,4)
      ENERGY(11,IDACC5)=SPECA(1,19,1)
      ENERGY(12,IDACC5)=SPECA(1,19,3)
      ENERGY(17,IDACC5)=SPECA(1,20,1)
      ENERGY(18,IDACC5)=SPECA(1,20,3)
      DO 550 N=2,NM
      ENERGY(3,IDACC5)=ENERGY(3,IDACC5)+SPECA(N,19,2)
      ENERGY(4,IDACC5)=ENERGY(4,IDACC5)+SPECA(N,19,4)
      ENERGY(9,IDACC5)=ENERGY(9,IDACC5)+SPECA(N,20,2)
      ENERGY(10,IDACC5)=ENERGY(10,IDACC5)+SPECA(N,20,4)
      ENERGY(13,IDACC5)=ENERGY(13,IDACC5)+SPECA(N,19,1)
      ENERGY(14,IDACC5)=ENERGY(14,IDACC5)+SPECA(N,19,3)
      ENERGY(19,IDACC5)=ENERGY(19,IDACC5)+SPECA(N,20,1)
  550 ENERGY(20,IDACC5)=ENERGY(20,IDACC5)+SPECA(N,20,3)
      IDACC(5)=IDACC5
      RETURN
c  **
      ENTRY DIAG4
c  ** THIS ENTRY PRODUCES A TIME HISTORY TABLE OF ENERGIES
c  **
      IDACC5=IDACC(5)
      IF (IDACC5.LE.0) RETURN
      IF (IDACC(12).LT.1) IDACC(12)=1
      SCALE(1)=100.D-18/GRAV
      SCALE(2)=SCALE(1)
      SCALE(3)=SCALE(1)
      SCALE(4)=SCALE(1)
      SCALE(5)=.5*SCALE(1)
      SCALE(6)=SCALE(5)
      SCALE(7)=SCALE(1)*RGAS
      SCALE(8)=SCALE(7)
      SCALE(9)=SCALE(7)
      SCALE(10)=SCALE(7)
      SCALE(11)=SCALE(1)
      SCALE(12)=SCALE(1)
      SCALE(13)=SCALE(1)
      SCALE(14)=SCALE(1)
      SCALE(15)=SCALE(5)
      SCALE(16)=SCALE(5)
      SCALE(17)=SCALE(7)
      SCALE(18)=SCALE(7)
      SCALE(19)=SCALE(7)
      SCALE(20)=SCALE(7)
      DO 60 K=1,20
   60 SCALE(K)=(TWOPI/(DLON*FIM))*SCALE(K)/IDACC(12)
c  **
      IHOUR0=TOFDY0+.5
      IHOUR=TOFDAY+.5
      WRITE (6,901) XLABEL
      WRITE (6,902) IDAY0,IHOUR0,JDATE0,JMNTH0,JYEAR0,IDAY,IHOUR,JDATE,
     *  JMONTH,JYEAR
      DO 110 K=1,20
  110 SUM(K)=0.
      WRITE (6,903)
      DO 200 I=1,IDACC5
      TAUX=TAU0+(I*NDA4-NDYN)*DT/3600.
      IDAYX=NINT(1.+TAUX/24.)
      IDAYXM=MOD(IDAYX,100000)
      TOFDYX=TAUX-24.*(IDAYX-1)
      DO 150 K=1,20
         EHIST(K,I)=ENERGY(K,I)*SCALE(K)
      IK(K)=EHIST(K,I)+.5
  150 SUM(K)=SUM(K)+ENERGY(K,I)
      WRITE (6,904) IDAYXM,TOFDYX,IK
  200 CONTINUE
      DO 250 K=1,20
         EHIST(K,101)=SUM(K)*SCALE(K)/IDACC5
  250 IK(K)=EHIST(K,101)+.5
      WRITE (6,905) IK
         CALL KEYD4 (IK)
      RETURN
c  **
  901 FORMAT ('1',33A4)
  902 FORMAT ('0** ENERGY HISTORY **   DAY',I6,', HR',I3,' (',I2,A5,I5,
     *  ')    TO    DAY',I6,', HR',I3,' (',I2,A5,I5,
     *  ')    UNITS OF 10**18 JOULES')
  903 FORMAT ('0',15X,21('-'),' TROPOSPHERE ',22('-'),5X,21('-'),
     *  ' STRATOSPHERE ',21('-')/8X,2(11X,'ZKE',8X,'EKE',7X,'SEKE',9X,
     * 'ZPE',10X,'EPE')/3X,'DAY  HOUR     SH   NH    SH   NH    SH   NH
     *    SH    NH     SH    NH      SH   NH    SH   NH    SH   NH     S
     *H    NH     SH    NH'/1X,132('='))
  904 FORMAT (I6,F6.1,1X,3(I6,I5),2(I7,I6),2X,3(I6,I5),2(I7,I6))
  905 FORMAT (1X,132('=')/8X,'MEAN ',3(I6,I5),2(I7,I6),2X,3(I6,I5),
     *  2(I7,I6))
      END
      SUBROUTINE DIAGKS
c  **
c  ** THIS SUBROUTINE PRODUCES A SUMMARY OF KEY NUMBERS CALCULATED IN
c  ** OTHER DIAGNOSTIC SUBROUTINES
c  **
c  ** CONTENTS OF KEYNR
c  **
c   K   N
c  **
c  *1  1 MONTH
c  *2  2 TOTAL CLOUD COVER (PERCENT)
c  **  3 SNOW COVER--NORTHERN HEMSIPHERE (PERCENT)
c  **  4 ICE COVER--NORTHERN HEMISPHERE (PERCENT)
c  **  5 PLANETARY ALBEDO (PERCENT)
c  **  6 SOLAR RADIATION ABSORBED BY ATMOSPHERE (WT/M**2)
c  **  7 SOLAR RADIATION ABSORBED BY PLANET (WT/M**2)
c  **  8 NET HEAT AT GROUND (WT/M**2)
c  **  8 ANGULAR MOMENTUM PER UNIT AREA (10**10 J*SEC/M**2)
c  **  9 EVAPORATION (.1 MM/DAY)
c  **  9 PRECIPITATION (.1 MM/DAY)
c  ** 10 SENSIBLE HEAT FLUX INTO GROUND (ABS.VALUE)
c  ** 11 LATENT HEAT FLUX INTO GROUND (ABS.VALUE)
c  ** 12 MEAN GROUND TEMPERATURE (DEGREES K)
c  ** 13 MEAN GLOBAL ATMOSPHERIC TEMPERATURE (DEGREES K)
c  ** 14 MERID. TEMPERATURE GRADIENT (N.HEMISPHERE)
c  ** 15 MERID. TEMPERATURE GRADIENT (S.HEMISPHERE)
c  ** 16 MEAN TROPOSPHERIC EKE-NORTHERN HEMISPHERE
c  ** 17 MEAN TROPOSPHERIC EKE-SOUTHERNN HEMISPHERE
c  ** 18 MEAN TROPOSPHERIC ZKE-NORTHERN HEMISPHERE
c  ** 19 MEAN TROPOSPHERIC ZKE-SOUTHERN HEMISPHERE
c  ** 20 MEAN TROPOSPHERIC EPE-NORTHERN HEMISPHERE
c  ** 21 MEAN TROPOSPHERIC ZPE-NORTHERN HEMISPHERE
c  ** 22 MEAN EDDY KINETIC ENERGY AT EQUATOR
c  ** 23 MAX. MEAN EDDY KINETIC ENERGY IN MID NORTH LATITUDES
c  ** 24 MAX. ZONAL WIND (U COMPONENT) IN TROPOSPHERE (NH), M/SEC
c  ** 25 LATITUDE CORRESPONDING TO 24
c  ** 26 MAX. ZONAL WIND (U COMPONENT) IN TROPOSPHERE (SH), M/SEC
c  ** 27 LATITUDE CORRESPONDING TO 26
c  ** 28-30: 29 IS LARGEST VALUE OF STREAM FUNCTION, POSITIVE OR
c  **    NEGATIVE; 28 AND 30 ARE THE MAGNITUDES OF THE LARGEST VALUES OF
c  **    OPPOSITE SIGN TO THE NORTH AND SOUTH RESPECTIVELY
c  *3 31 SNOW AND ICE COVERAGE OF GLOBE (PERCENT)
c  *4 32 SNOW AND ICE COVERAGE OF NORTHERN HEMISPHERE (PERCENT)
c  **   33-39 REFER TO NORTHERN HEMISPHERE ONLY
c  ** 33 MAX.NORTHWARD TRANS. OF DRY STATIC ENERGY BY STANDING EDDIES
c  ** 34 MAX.NORTHWARD TRANS. OF DRY STATIC ENERGY BY EDDIES
c  ** 35 MAX. TOTAL NORTH. TRANS. OF DRY STATIC ENERGY
c  ** 36 MAX.NORTHWARD TRANS. OF STATIC ENERGY BY EDDIES
c  ** 37 MAX.TOTAL NORTH. TRANS. OF STATIC ENERGY
c  ** 38 LATITUDE CORRESPONDING TO 37
c  ** 39 MAX. NORTH. TRANS. OF ANGULAR MOMENTUM BY STANDING EDDIES
c  ** 40 MAX. NORTH. TRANS. OF ANGULAR MOMENTUM BY EDDIES
c  ** 41 MAX. TOTAL NORTH. TRANS. OF ANGULAR MOMENTUM
c  ** 42 LATITUDE CORRESPONDING TO 41
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      COMMON/WORK5/FKEY(JM,LM)
      COMMON/DJLCOM/JLAT(JM,2)
      INTEGER*4 NDEX(42)/1,2,31,32,3,   4,5,6,7,8,   9,10,11,12,13,
     *             14,15,16,17,18,  19,20,21,22,23,  24,25,26,27,28,
     *             29,30,33,34,35,  36,37,38,39,40,  41,42/
      DIMENSION HSUM(*),ASUM(*),FLAT(*),IK(*)
      CHARACTER*4 CKEYNR(42,50)
      EQUIVALENCE (CKEYNR,KEYNR)
c  **
c  ** ENTRIES CALLED FROM DIAGJ
c  **
      ENTRY KEYDJ (N,FGLOB,FNH)
      GO TO (                100,100,100,110,100, 100,100,100,100,115,
     *  100,100,120,125,100, 100,100,130,100,135, 100,100,100,100,100,
     *  100,100,100,100,140, 145,100,100,100,100, 100,100,100,100,100,
     *  100,100,100,150,100, 100,100,100,100,100, 100,100,100,100,100,
     *  100,100,100,155),N
  100 RETURN
  110 KEYNR(6,KEYCT)=NINT(FGLOB)
      RETURN
  115 KEYNR(7,KEYCT)=NINT(FGLOB)
      RETURN
  120 KEYNR(10,KEYCT)=NINT(-FGLOB)
      RETURN
  125 KEYNR(11,KEYCT)=NINT(-FGLOB)
      RETURN
  130 KEYNR(12,KEYCT)=NINT(.1*FGLOB)
      RETURN
  135 KEYNR(9,KEYCT)=NINT(10.*FGLOB)
      RETURN
  140 KEYNR(4,KEYCT)=NINT(FNH)
      RETURN
  145 KEYNR(3,KEYCT)=NINT(FNH)
      RETURN
  150 KEYNR(8,KEYCT)=NINT(FGLOB)
      RETURN
  155 KEYNR(2,KEYCT)=NINT(FGLOB)
      RETURN
c  **
      ENTRY KEYDJA (FGLOB)
      KEYNR(5,KEYCT)=NINT(10.*FGLOB)
      RETURN
c  **
c  ** ENTRIES CALLED FROM DIAGJL VIA JLMAP OR FROM DIAGJK VIA JKMAP
c  **
      ENTRY KEYJKT (GSUM,ASUM)
c  ** TEMPERATURES
      JEQ=2.+.5*(JM-1)
      TEQ=.5*(ASUM(JEQ-1)+ASUM(JEQ))
      X60=TWOPI/(12.*DLAT)
      J60=.5+X60
      A=DXYP(J60+1)*(X60+.5-J60)
      TSOU=ASUM(J60+1)*A
      TNOR=ASUM(JM-J60)*A
      DO 210 J=1,J60
      A=A+DXYP(J)
      TSOU=TSOU+ASUM(J)*DXYP(J)
  210 TNOR=TNOR+ASUM(JM+1-J)*DXYP(J)
      KEYNR(14,KEYCT)=NINT(TEQ-TNOR/A)
      KEYNR(15,KEYCT)=NINT(TEQ-TSOU/A)
      KEYNR(13,KEYCT)=NINT(.1*GSUM)
      RETURN
c  **
      ENTRY KEYJKJ (L,FLAT)
c  ** JET STREAMS
      IF (L.LT.LM) GO TO 220
      DO 216 LL=1,LM
      IF ((PSF-PTOP)*SIG(LL)+PTOP.LT.200.) GO TO 218
  216 CONTINUE
  218 LMAX=LL-1
  220 IF (L.GT.LMAX) RETURN
      USLM=-999999.
      DO 222 J=3,JEQ
      IF (FLAT(J).LT.USLM) GO TO 222
      USLM=FLAT(J)
      JMAX=J
  222 CONTINUE
      CEPT=.5*(FLAT(JMAX-1)-FLAT(JMAX+1))/
     *  (FLAT(JMAX-1)-2.*FLAT(JMAX)+FLAT(JMAX+1))
      LSLM=INT((JMAX-1.5+CEPT)*DLAT*360/TWOPI+.5)-90
      UNLM=-999999.
      DO 224 J=JEQ,JM-1
      IF (FLAT(J).LT.UNLM) GO TO 224
      UNLM=FLAT(J)
      JMAX=J
  224 CONTINUE
      CEPT=.5*(FLAT(JMAX-1)-FLAT(JMAX+1))/
     *  (FLAT(JMAX-1)-2.*FLAT(JMAX)+FLAT(JMAX+1))
      LNLM=INT((JMAX-1.5+CEPT)*DLAT*360/TWOPI+.5)-90
      IF (L.LT.LMAX) GO TO 226
      USM=USLM
      LSM=LSLM
      UNM=UNLM
      LNM=LNLM
      RETURN
  226 IF (USLM.LT.USM) GO TO 228
      USM=USLM
      LSM=LSLM
  228 IF (UNLM.LT.UNM) GO TO 230
      UNM=UNLM
      LNM=LNLM
  230 IF (L.NE.1) RETURN
      KEYNR(24,KEYCT)=.1*UNM+.5
      KEYNR(25,KEYCT)=LNM
      KEYNR(26,KEYCT)=.1*USM+.5
      KEYNR(27,KEYCT)=-LSM
      RETURN
c  **
      ENTRY KEYJLS (L,FLAT)
c  ** STREAM FUNCTION
      DO 290 J=2,JM
  290 FKEY(J,L)=FLAT(J)
      IF (L.NE.1) RETURN
  300 SAVE=0.
      HS=0.
      HN=0.
      DO 310 K=1,LM
      DO 310 I=2,JM
      CHECK=ABS(FKEY(I,K))
      IF (CHECK.LT.SAVE) GO TO 310
      SAVE=CHECK
      JNDEX=I
      KNDEX=K
  310 CONTINUE
      SAVE=FKEY(JNDEX,KNDEX)
      ISIGN=1
      IF (SAVE.GT.0.0) ISIGN=-1
      IF (JNDEX.LT.4) GO TO 325
      IEND=JNDEX-1
      DO 320 K=1,LM
      DO 320 I=2,IEND
      CHECK=FKEY(I,K)*ISIGN
  320 IF (CHECK.GT.HS)HS=CHECK
  325 CONTINUE
      IF (JNDEX.GT.(JM-2))GO TO 335
      JSTART=JNDEX+1
      DO 330 K=1,LM
      DO 330 I=JSTART,JM
      CHECK=FKEY(I,K)*ISIGN
  330 IF (CHECK.GT.HN)HN=CHECK
  335 CONTINUE
      KEYNR(28,KEYCT)=ABS(HN)+0.5
      KEYNR(29,KEYCT)=NINT(SAVE)
      KEYNR(30,KEYCT)=ABS(HS)+0.5
      RETURN
c  **
      ENTRY KEYJKE (NT,HSUM,ASUM)
c  ** EDDY AND ZONAL KINETIC ENERGY
      IF (NT.EQ.19) GO TO 450
      KEYNR(16,KEYCT)=NINT(HSUM(2))
      KEYNR(17,KEYCT)=NINT(HSUM(1))
      KEYNR(18,KEYCT)=KEYNR(18,KEYCT)-NINT(HSUM(2))
      KEYNR(19,KEYCT)=KEYNR(19,KEYCT)-NINT(HSUM(1))
      KEYNR(22,KEYCT)=NINT(ASUM(JEQ))
      BIG=-99999.
      I35=2.+(JM-1)*125./180.
      I70=2.+(JM-1)*160./180.
      DO 440 I=I35,I70
      IF (ASUM(I).LT.BIG) GO TO 440
      BIG=ASUM(I)
  440 CONTINUE
      KEYNR(23,KEYCT)=NINT(BIG)
      RETURN
  450 KEYNR(18,KEYCT)=KEYNR(18,KEYCT)+NINT(HSUM(2))
      KEYNR(19,KEYCT)=KEYNR(19,KEYCT)+NINT(HSUM(1))
      RETURN
c  **
      ENTRY KEYJKN (NT,ASUM,SUMFAC)
c  ** NORTHWARD TRANSPORTS
  500 BIG=-99999.
      JEQP1=JEQ+1
      DO 510 I=JEQP1,JM
      IF (ASUM(I).LT.BIG) GO TO 510
      BIG=ASUM(I)
      JNDEX=I
  510 CONTINUE
      BIG=BIG*SUMFAC
      NTDIF=NT-21
      GO TO (392,392,392,390,390,396,394,390,390,400,400,398),NTDIF
  390 CONTINUE
  392 KEYNR(NT+11,KEYCT)=NINT(BIG)
      RETURN
  394 KEYNR(38,KEYCT)=NINT(LAT_DG(JNDEX,2))
  396 KEYNR(NT+9,KEYCT)=NINT(BIG)
      RETURN
  398 KEYNR(42,KEYCT)=NINT(LAT_DG(JNDEX,2))
  400 KEYNR(NT+8,KEYCT)=NINT(BIG)
      RETURN
c  **
c  ** ENTRY CALLED FROM DIAGIJ
c  **
      ENTRY KEYIJ(PISG,PISN)
      KEYNR(31,KEYCT)=NINT(PISG)
      KEYNR(32,KEYCT)=NINT(PISN)
      RETURN
c  **
c  ** ENTRY CALLED FROM DIAG4
c  **
      ENTRY KEYD4 (IK)
      KEYNR(20,KEYCT)=(IK(10)+IK(20)+5)/10
      KEYNR(21,KEYCT)=(IK(8)+IK(18)+5)/10
      RETURN
c  **
      ENTRY DIAGKN
c  ** PRINTS THE TABLE OF KEY NUMBERS
c  **
      IHOUR0=TOFDY0+.5
      IHOUR=TOFDAY+.5
      TAUDIF=TAU-TAU0
      CKEYNR(1,KEYCT)=JMNTH0
      IF (TAU.LE.TAUI+(DT/3600.)*(NDYN+.5)) CKEYNR(1,KEYCT)='IC'
      IF (KEYCT.GE.2.AND.CKEYNR(1,KEYCT-1).EQ.JMNTH0) KEYCT=KEYCT-1
      WRITE(6,901) XLABEL
      WRITE(6,910) IDAY0,IHOUR0,JDATE0,JMNTH0,JYEAR0,TAU0,IDAY,IHOUR,
     *  JDATE,JMONTH,JYEAR,TAU,TAUDIF
      WRITE(6,902)
      DO 810 I=1,KEYCT
      IF (CKEYNR(1,I).EQ.'JAN') WRITE (6,905)
  810 WRITE(6,905) (KEYNR(NDEX(K),I),K=1,42)
      WRITE (6,915)
      KEYCT=KEYCT+1
      KEYMAX=49
      IF (CKEYNR(1,1).NE.'IC') KEYMAX=48
      IF (KEYCT.LE.KEYMAX) RETURN
c  ** ROLL UP KEY NUMBERS 1 YEAR AT A TIME
      DO 820 K=1,36
      DO 820 I=1,42
  820 KEYNR(I,K)=KEYNR(I,K+KEYMAX-36)
      DO 880 K=37,50
      DO 880 I=1,42
  880 KEYNR(I,K)=0
      KEYCT=37
      RETURN
  901 FORMAT('1',33A4)
  902 FORMAT ('0',7X,'SN+IC NH NH AL AB NT NT PR        T   T-OF-ATM  EK
     *E   ZKE           EKE   JET-STREAMS STREAM-FN NOR-TRAN NOR-TRAN NO
     *RTH-TRANS'/
     *         5X,'CL GL    SN OI BE BY RD HT EC SN LAT OF  GL  GRAD ---
     *-- ----- EPE ZPE ------ NORTH SOUTH --------- DRY-STAT STAT-ENR AN
     *G MOMENTM'/
     *         5X,'CV OB NH CV CV DO AT P0 Z0 IP HT  HT GD  OB NH SH NH
     *SH NH SH  NH  NH EQ  ML VL LT VL LT NH MAX SH SE ED TL ED TL LT SE
     * ED TL LT'/)
  905 FORMAT (1X,A3,4I3,I2,I4,5I3,I4,I3,I4,6I3,2I4,I3,I4,5I3,I4,11I3)
  910 FORMAT ('0',13X,'DAY',I6,', HR',I3,' (',I2,A5,I5,')',F9.1,
     *  '   TO   DAY',I6,', HR',I3,' (',I2,A5,I5,')',F9.1,'   DIF',
     *  F6.1,' HR',7X,I5,I5)
  915 FORMAT('0')
      END

      SUBROUTINE DIAG10(IPFLAG)
c  **
c  ** THIS SUBROUTINE SAVES THE INSTANTANEOUS SEA LEVEL PRESSURES
c  ** EVERY ABS(USESLP) HOURS. IF USESLP.LT.0 THE FIRST RECORD IS
c  ** WRITTEN TO THE BEGINNING OF UNIT 16.
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      COMMON/WORK1/SLP(IM,JM)
      BBYG=.0065/GRAV
      GBYRB=GRAV/(.0065*RGAS)
      DO 10 J=1,JM
      DO 10 I=1,IM
   10 SLP(I,J)=(P(I,J)+PTOP)*(1.+BBYG*FDATA(I,J,1)/BLDATA(I,J,2))**GBYRB
c  ****
c  **** MFS (ADDED CODE)
c  **** Added ability to save sea level pressure data to folder
c  ** 9/X (SWITCH)
c      OPEN(UNIT=16, NAME=':oda:SeaLevelPressure',POSITION='APPEND')
c      OPEN(UNIT=16, NAME='./oda/SeaLevelPressure',POSITION='APPEND')
C!    OPEN(UNIT=16, NAME='SeaLevelPressure')
      OPEN(UNIT=16, file='SeaLevelPressure')
c  ** END (SWITCH)
c  ****
c  **** END (ADDED CODE)
c  ****
c  ****
c  **** MFS (CHANGED CODE)
c  **** The orignal code used SNGL to convert types which requires the
c  **** double type as input.  I switched the code to use REAL which
c  **** gives the same results but takes any numerical type.
c      WRITE (16) SNGL(TAU),((SNGL(SLP(I,J)),I=1,IM),J=1,JM),SNGL(TAU)
      WRITE (16) REAL(TAU),((REAL(SLP(I,J)),I=1,IM),J=1,JM),REAL(TAU)
c  ****
c  **** END (CHANGED CODE)
c  ****
      ENDFILE 16
      BACKSPACE 16
      RETURN
      END

      SUBROUTINE SIJMAP
C**** Some of this is from extractAIJ8X10.  There are many AIJ arrays
C**** that never get printed.  Most are wanted by EdGCM.  This routine
C**** saves the quantities not handled by DIAGIJ.
c  **
c  ** THIS SUBROUTINE PRODUCES LATITUDE BY LONGITUDE MAPS OF:
c  **
      INCLUDE 'BA94jalC9.COM'
      COMMON U,V,T,P,Q
      character*48 title(80) /
     1 'OCEAN ICE COVERAGE                  (percent)',
     2 'SNOW COVERAGE                       (percent)',
     3 'SNOW DEPTH                          (kg/m**2)',
     4 'SENSIBLE HEAT FLUX                  (W/m**2)',
     * 'PRECIPITATION                       (mm/day)',  
     * 'EVAPORATION                         (mm/day)',
     * 'GROUND WETNESS over GROUND          (percent)',
     * ' ',
     * '1000 mb GEOPOTENTIAL HEIGHT         (m)',
     * '850 mb GEOPOTENTIAL HEIGHT          (m-1500)',  
c ZNAM11=
     * '700 mb GEOPOTENTIAL HEIGHT          (m-3000)',
     2 '500 mb GEOPOTENTIAL HEIGHT          (m-5600)',
     * '300 mb GEOPOTENTIAL HEIGHT          (m-9500)',
     4 '100 mb GEOPOTENTIAL HEIGHT          (m-16400)',
     * '30 mb GEOPOTENTIAL HEIGHT           (m-24000)',  
     6 'TEMPERATURE at 850 mb               (C)',
     * 'MOIST CONVECTIVE CLOUD COVER        (percent)',
     8 'CLOUD TOP PRESSURE * CLOUD COVER    (mb)',
     * 'TOTAL CLOUD COVER                   (percent)',
     * 'TOTL NOR. TRANS of DRY STATIC ENRG  (E+14 W,UV)',  
c ZNAM21=
     * 'OUTGOING THERMAL RAD. of PLANET     (W/m**2)',
     * 'NET RADIATION at SURFACE            (W/m**2)',
     * 'NET HEATING at SURFACE              (W/m**2)',
     * 'ABSORBED SOLAR RADIATION of PLANET  (W/m**2)',
     5 'INCIDENT SOLAR RADIATION of PLANET  (W/m**2)',  
     * 'ABSORBED SOLAR RADIATION at SURFACE (W/m**2)',
     * 'INCIDENT SOLAR RADIATION at SURFACE (W/m**2)',
     * 'GROUND TEMPERATURE                  (C of L1)',
     * 'SNOW and ICE COVERAGE               (percent)',
     * 'DIURNAL SURF AIR TEMPER. VARIATION  (C)',  
c ZNAM31=
     * 'TROPOSPHERIC STATIC STABILITY       (K/km)',
     * 'SURFACE RUNOFF over GROUND          (mm/day)',
     * 'SURFACE RUNOFF over GLACIAL ICE     (mm/day)',
     * 'SURF. CROSS ISOBAR ANGLE ADJUSTMENT (radians)',
     5 'SURFACE AIR TEMPERATURE             (C)',  
     * 'U COMPONENT of SURFACE AIR WIND     (m/s)',
     * 'V COMPONENT of SURFACE AIR WIND     (m/s)',
     * 'SEA LEVEL PRESSURE                  (mb-1000)',
     * 'U JET LEVEL WIND                    (m/s, L7)',
     * 'V JET LEVEL WIND                    (m/s, L7)',  
c ZNAM41=
     * 'LOW LEVEL CLOUD COVER (Layers 1,2,3)(percent)',
     * 'MID LEVEL CLOUD COVER (Layers 4,5)  (percent)',
     * 'HIGH LEVEL CLOUD COVER (Layers 6,7) (percent)',
     * 'BRIGHTNESS TEMP thru WINDOW REGION  (C)',
     5 'ABSORBED SOLAR RADIATION in VISUAL  (W/m**2)',  
     * 'SURFACE CROSS ISOBAR ANGLE, ALPHA0  (radians)',
     * 'MAGNITUDE of SURF. MOMENTUM STRESS  (N/m**2)',
     * 'U COMP. of SURF. MOMENTUM STRESS    (N/m**2)',
     * 'V COMP. of SURF. MOMENTUM STRESS    (N/m**2)',
     * 'TOTL WATER AND ICE MASS over GROUND (kg/m**2)',  
c ZNAM51=
     * 'SURFACE SPECIFIC HUMIDITY           (1E-4)',
     * 'MONTHLY HEATING                     (degr daysF)',
     * 'HEAT HUMIDITY INDEX                 (degrees F)',
     * 'PLANT WATER STRESS                  (degr days)',
C The following are not PRINTED but are saved binary
     5 'VERTICAL SUM of E->W HUMIDITY FLUX  (mb*m/s)',
     6 'VERTICAL SUM of N->S HUMIDITY FLUX  (mb*m/s)',
     7 'OCEAN MIXED-LAYER TEMPERATURE (SST) (C)',
     8 'ICE AMOUNT OF 2ND LAYER times FWICE (tons/m**2)',
     9 'OCEAN TEMPERATURE OF SECOND LAYER   (C)',
     a 'ANNUAL-MAX MIXED-LAYER TEMPERATURE  (C)',
c ZNAM61=
     1 'EVAPORATION times FOWATR            (mm/day)',
     2 'EVAPORATION times FWICE             (mm/day)',
     3 'EVAPORATION over GLACIAL ICE        (mm/day)',
     4 'EVAPORATION over GROUND             (mm/day)',
     5 'NET HEAT at SURFACE times FOWATR    (W/m**2)',
     6 'NET HEAT at SURFACE times FWICE     (W/m**2)',
     7 'NET HEAT at SURF. over GLACIAL ICE  (W/m**2)',
     8 'NET HEAT at SURFACE over GROUND     (W/m**2)',
     9 'F1DT over GLACIAL ICE               (W/m**2)',
     a 'SNOW FALL                           (kg/m**2)',
c ZNAM71=
     1 'SURFACE AIR TEMP. over GLACIAL ICE  (C)',
     2 'F2DT over GLACIAL ICE               (W/m**2)',
     3 'SENSIBLE HEAT over GLACIAL ICE      (W/m**2)',
     4 'LATENT HEAT over GLACIAL ICE        (W/m**2)',
     5 'THERMAL RAD at GLACIAL ICE SURF.    (W/m**2)',
     6 'MAX SURFACE AIR TEMPERATURE         (K)',
     7 'MIN SURFACE AIR TEMPERATURE         (K)',
     8 'MIN OF DIURN MAX OF SURF AIR TEMP   (K)',
     9 'POTENTIAL EVAPORATION               (mm/day)',
     a '                                          '/
c  **
      include 'pd_COMMON'
      REAL*4 SMAP
      COMMON/WORK2/ENDE16(IM,JM,2),PRAVG(IM,JM),PRSD(IM,JM),
     *  FLAT(3),FNH(3),FGLOBE(3),MLAT(3),MGLOBE(3),GNUM(3),GDEN(3)
     *  ,SMAP(IM,JM)
      CHARACTER XLB*32,CRUN*24/' '/
      DIMENSION IND(80),IA(80),SCALE(80),JGRID(80)
      integer ipick(80)
      data ipick/
     *          0,0,0,0,0,  0,0,0,1,0,  0,0,0,0,0,  1,0,0,0,0,
     *          0,0,0,0,0,  0,0,0,0,1,  0,0,0,0,0,  1,1,0,1,1,
     *          0,0,0,0,0,  0,1,1,1,0,  0,0,0,0,1,  1,1,1,1,1,
     *          1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1/
      data ia/  1,4,4,1,1,  1,1,4,4,4,  4,4,4,4,4,  4,2,2,2,4,
     *          2,1,1,2,2,  2,2,1,4,9,  4,1,1,3,3,  3,3,4,4,4,
     *          2,2,2,2,2,  3,3,3,3,1,  3,12,3,12,4,  4,1,1,9,9,
     *          1,1,1,1,1,  1,1,1,1,1,  1,1,1,1,1,  3*12 ,1,0/
      DATA JGRID/19*1,2,15*1,1,1,1,2,2,14*1, 26*1/

c  ** INITIALIZE CERTAIN QUANTITIES
        XLB = ' '
        WRITE(CRUN,'(6A)') (XLABEL(K),K=1,6)
        KXLB = INDEX(CRUN,'(')-1
        WRITE(XLB(23:),'(A)') CRUN(1:MIN(10,KXLB))
        WRITE(XLB(13:20),'(A3,I5)') JMNTH0,JYEAR0
      DTSRCE=NDYN*DT
      DTCNDS=NCNDS*DT
   50 scale( 1) = 100.
      scale( 2) = 100.
      scale( 4) = 1./dtsrce
      scale( 5) = 24.
      scale( 6) = 24.
      scale( 7) = 100.
      scale( 8) = .25
      scale( 9) = 1./grav
      scale(10) = 1./grav
      scale(11) = 1./grav
      scale(12) = 1./grav
      scale(13) = 1./grav
      scale(14) = 1./grav
      scale(15) = 1./grav
      scale(17) = 100.
      scale(19) = 100.
      scale(20) = 1.e-14*100./(16.*9.81)
      scale(21) = -1.
      scale(22) = 1/dtsrce
      scale(23) = 1/dtsrce
      scale(29) = 100.
      scale(30) = 2.
      scale(31) = 1.e3*grav*1000.**.286
      scale(32) = 24.
      scale(33) = 24.
      scale(41) = 100.
      scale(42) = 100.
      scale(43) = 100.
      scale(50) = 100.
      scale(51)=1.e4
      scale(52)=.01
      scale(54)=.05
      scale(55)=1./8.
      scale(56)=1./8.
      scale(58)=.001
      scale(59)=2.
      scale(60)=2.
      scale(61) = 24.
      scale(62) = 24.
      scale(63) = 24.
      scale(64) = 24.
      scale(65) = 1./dtsrce
      scale(66) = 1./dtsrce
      scale(67) = 1./dtsrce
      scale(68) = 1./dtsrce
      scale(69) = 1./dtsrce
      scale(70)=24.
      scale(71)=1./nsurf
      scale(72)=1./dtsrce
      scale(73)=1./dtsrce
      scale(74)=1./dtsrce
      scale(75)=1./dtsrce
      scale(79)=24.
c  **
  160 NDIAG=79
      DO 180 k=1,ndiag
      IF (JGRID(k).EQ.2) GO TO 180
      DO 170 I=1,IM
      AIJ(I,1,k)=AIJ(1,1,k)
  170 AIJ(I,JM,k)=AIJ(1,JM,k)
  180 CONTINUE
      DO 610 K=1,ndiag
        if (ipick(k).eq.1) then
        BYIACC=1./(IDACC(IA(K))+1.D-20)
        do 320 i=1,im*jm
          smap(i,1) = aij(i,1,k)*scale(k)*byiacc
  320   continue
        WRITE(iu_pd(3))TITLE(K),XLB,smap
        end if
  610 CONTINUE
      RETURN
      END
