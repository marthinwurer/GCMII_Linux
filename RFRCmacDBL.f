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
c  ** 12/26/00 moved one dimesion statment (MFS)
c  ** 06/02/02 decided to use unalterd code (MFS)
c  ** 07/01/02 pass ktrend again to keep it in scope (MFS)
c  ** 07/24/02 doctored copy which replaces atrend (MFS)
c  **
c  ** NOTES:
c  **
c  *********************************************************************
c  *********************************************************************

C**** RFRCDBL B83XXDBL RFRCDBL                      2/24/92                0.1  
C**** OPT(3)                                                               0.2  
C****                                                                      0.3  
C**** Atmospheric forcing by trace gases and volcanoes 1850-2000           0.4  
***** RFRCDBL B83XXDBL RFRCDBL                      2/24/92                0.1  
***** OPT(3)                                                               0.2  
*****                                                                      0.3  
***** Atmospheric forcing by trace gases and volcanoes 1850-2000           0.4  
      SUBROUTINE FORSET(TREF,KTREND,KWRITE)                                1.   
      INCLUDE 'B83XXDBL.COM'                                               2.   
C                                                                          3.   
      DIMENSION XNOW(5),XREF(5),XDT0(5),XDAT(5),XRAT(5),KFOR(5)            4.   
      DIMENSION YNOW(5),YREF(5),ZNOW(5),ZREF(5),YDT0(5),ZDT0(5)            5.   
      DIMENSION QREF(5),QNOW(5)                                            6.   
C                                                                          7.   
      LAPGAS=2                                                             8.   
      NGAS=5                                                               9.   
      DO 110 I=1,NGAS                                                     10.   
      YREF(I)=1.D-10                                                      11.   
      YNOW(I)=1.D-10                                                      12.   
      ZREF(I)=1.D-10                                                      13.   
  110 ZNOW(I)=1.D-10                                                      14.   
c  ** MFS (CHANGED)
c  ** use my trend instead of atrend
c      IF(KTREND.EQ.1) CALL ATREND(XREF,TREF,NGAS)                         15.   
      IF(KTREND.EQ.1) CALL DATAREFYEAR(XREF,TREF,NGAS)           
c  ** END (CHANGED)
      IF(KTREND.EQ.2) CALL BTREND(XREF,TREF,NGAS)                         16.   
      IF(KTREND.EQ.3) CALL CTREND(XREF,TREF,NGAS)                         17.   
C                                                                         18.   
      IF(KTREND.GE.4) CALL DTREND(XREF,TREF,NGAS)                         19.   
C                                                                         20.   
      XREFY4=0.D0                                                         21.   
      XREFZ5=0.D0                                                         22.   
C                                                                         23.   
      DO 120 I=1,NGAS                                                     24.   
      IF(XREF(I).LT.1.D-06) XREF(I)=1.D-06                                25.   
 120  KFOR(I)=1                                                           26.   
      PPMV58(2)=XREF(1)                                                   27.   
      PPMV58(6)=XREF(2)                                                   28.   
      PPMV58(7)=XREF(3)                                                   29.   
      PPMV58(8)=XREF(4)/1000.D0                                           30.   
      PPMV58(9)=XREF(5)/1000.D0                                           31.   
C                                                                         32.   
      IF(KTREND.GE.4) CALL DTREND(XREF,TREF,NGAS)                         33.   
      IF(KTREND.EQ.5) CALL YTREND(YREF,TREF,NGAS)                         34.   
      IF(KTREND.EQ.5) CALL ZTREND(ZREF,TREF,NGAS)                         35.   
C                                                                         36.   
      IF(KWRITE.NE.1) GO TO 140                                           37.   
      DO 130 I=1,NGAS                                                     38.   
 130  XDAT(I)=XREF(I)                                                     39.   
      IF(KTREND.EQ.1) WRITE(6,6001) KTREND                                40.   
      IF(KTREND.EQ.2) WRITE(6,6002) KTREND                                41.   
      IF(KTREND.EQ.3) WRITE(6,6003) KTREND                                42.   
      IF(KTREND.EQ.4) WRITE(6,6004) KTREND                                43.   
      IF(KTREND.EQ.5) WRITE(6,6005) KTREND                                44.   
      WRITE(6,6100)                                                       45.   
 140  CONTINUE                                                            46.   
 6001 FORMAT(1H1,'KTREND=',I2,1X                                          47.   
     +      ,T12,'(OUTPUT)   GCM RAD EXPECTED TEMPERATURES'               48.   
     +      ,T55,'PRESENT TREND FORSET INPUT DATA TO GCM'                 49.   
     +      ,T96,'RATE OF CHANGE/YR OF TRACE GAS AMOUNTS')                50.   
 6002 FORMAT(1H1,'KTREND=',I2,1X                                          51.   
     +      ,T12,'(OUTPUT)   GCM RAD EXPECTED TEMPERATURES'               52.   
     +      ,T55,'REDUCED TREND FORSET INPUT DATA TO GCM'                 53.   
     +      ,T96,'RATE OF CHANGE/YR OF TRACE GAS AMOUNTS')                54.   
 6003 FORMAT(1H1,'KTREND=',I2,1X                                          55.   
     +      ,T12,'(OUTPUT)   GCM RAD EXPECTED TEMPERATURES'               56.   
     +      ,T55,'CURTAIL TREND FORSET INPUT DATA TO GCM'                 57.   
     +      ,T96,'RATE OF CHANGE/YR OF TRACE GAS AMOUNTS')                58.   
 6004 FORMAT(1H1,'KTREND=',I2,1X                                          59.   
     +      ,T12,'(OUTPUT)   GCM RAD EXPECTED TEMPERATURES'               60.   
     +      ,T55,'PRATHER TREND FORSET INPUT DATA TO GCM'                 61.   
     +      ,T96,'RATE OF CHANGE/YR OF TRACE GAS AMOUNTS')                62.   
 6005 FORMAT(1H1,'KTREND=',I2,1X                                          63.   
     +      ,T12,'(OUTPUT)   GCM RAD EXPECTED TEMPERATURES'               64.   
     +      ,T55,'PRATHER+TREND FORSET INPUT DATA TO GCM'                 65.   
     +      ,T96,'RATE OF CHANGE/YR OF TRACE GAS AMOUNTS')                66.   
 6100 FORMAT(6X,6('-'),'(* 3-D)',32('-'),3X,38('-'),3X,38('-')            67.   
     +      /1X,'YEAR DTSUM  *DTCO2   DTN2O   DTCH4   DTF11   DTF12'      68.   
     +         ,         '   PPMCO2  PPMN20  PPMCH4  PPTF11  PPTF12'      69.   
     +         ,         '   RATCO2  RATN2O  RATCH4  RATF11  RATF12')     70.   
C                                                                         71.   
      RETURN                                                              72.   
C                                                                         73.   
C------------------------------                                           74.   
c  ** MFS (CHANGED)
c      ENTRY FORGET(TNOW,KWRITE)                                           75.   
      ENTRY FORGET(TNOW,KTREND,KWRITE)                
c  ** END (CHANGED)
C------------------------------                                           76.   
C                                                                         77.   
c  ** MFS (CHANGED)
c  ** use my trend instead of atrend
c      IF(KTREND.EQ.1) CALL ATREND(XNOW,TNOW,NGAS)                         78.   
      IF(KTREND.EQ.1) CALL DATAFILETREND(XNOW,TNOW,NGAS)            
c  ** END (CHANGED)
      IF(KTREND.EQ.2) CALL BTREND(XNOW,TNOW,NGAS)                         79.   
      IF(KTREND.EQ.3) CALL CTREND(XNOW,TNOW,NGAS)                         80.   
      IF(KTREND.GE.4) CALL DTREND(XNOW,TNOW,NGAS)                         81.   
      IF(KTREND.EQ.5) CALL YTREND(YNOW,TNOW,NGAS)                         82.   
      IF(KTREND.EQ.5) CALL ZTREND(ZNOW,TNOW,NGAS)                         83.   
      CALL DTXCFY(DT0Y4,YDT0,YNOW,YREF,NGAS)                              84.   
      CALL DTXCFZ(DT0Z5,ZDT0,ZNOW,ZREF,NGAS)                              85.   
      XNOWY4=DT0Y4/0.066D0                                                86.   
      XNOWZ5=DT0Z5/0.084D0                                                87.   
C                                                                         88.   
      CALL DTDX1D(XNOW,XREF,XDT0,SDT0,KFOR,NGAS)                          89.   
      CALL DTDX3D(XNOW,XREF,XDT0,SDT0,KFOR,1)                             90.   
      CALL DXDT3D(XNOW,XREF,XDT0,SDT0,KFOR,NGAS)                          91.   
      FULGAS(2)=XNOW(1)/PPMV58(2)                                         92.   
      FULGAS(6)=XNOW(2)/PPMV58(6)                                         93.   
      FULGAS(7)=XNOW(3)/PPMV58(7)                                         94.   
      XNOW(4)=XNOW(4)+XNOWY4-XREFY4                                       95.   
      XNOW(5)=XNOW(5)+XNOWZ5-XREFZ5                                       96.   
      FULGAS(8)=XNOW(4)/(1000.D0*PPMV58(8))                               97.   
      FULGAS(9)=XNOW(5)/(1000.D0*PPMV58(9))                               98.   
C                                                                         99.   
      IF(KWRITE.NE.1) GO TO 220                                          100.   
      XDT0(4)=XDT0(4)+0.066D0*(XNOWY4-XREFY4)                            101.   
      XDT0(5)=XDT0(5)+0.084D0*(XNOWZ5-XREFZ5)                            102.   
      SDT0=0.D0                                                          103.   
      DO 210 I=1,NGAS                                                    104.   
      SDT0=SDT0+XDT0(I)                                                  105.   
      XRAT(I)=(XNOW(I)-XDAT(I))/(1.D-10+XDAT(I))                         106.   
  210 XDAT(I)=XNOW(I)                                                    107.   
      IYEAR=TNOW                                                         108.   
      WRITE(6,6200) IYEAR,SDT0,(XDT0(I),I=1,5),(XNOW(I),I=1,5)           109.   
     +             ,(XRAT(I),I=1,5)                                      110.   
 6200 FORMAT(1X,I4,F6.3,5F8.4,1X,F8.2,4F8.4,1X,5F8.4)                    111.   
      NSPACE=IYEAR-(IYEAR/10)*10                                         112.   
      IF(NSPACE.EQ.0) WRITE(6,6010)                                      113.   
 6010 FORMAT(1H )                                                        114.   
 220  CONTINUE                                                           115.   
C                                                                        116.   
      RETURN                                                             117.   
      END                                                                118.   
      SUBROUTINE ATREND(XGAS,YEAR,NGAS)                                 1001.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         1002.   
C                                                                       1003.   
C-----------------------------------------------------------------------1004.   
C     T-GAS SCENARIO  A:   PRESENT TREND FOR TRACE GAS INCREASES        1005.   
C                          EXPONENTIAL FORCING PROJECTED BEYOND 2000    1006.   
C                                                                       1007.   
C                   CO2:   OBSERVED TREND 1958-1984,                    1008.   
C                          0.015/YR RATE OF INCREASE                    1009.   
C                          ON 1.5 PPM INCREMENT BEYOND 1984             1010.   
C                                                                       1011.   
C                   N2O:   WEISS FORMULA THRU 1978,                     1012.   
C                          0.035/YR INCREASE                            1013.   
C                          ON 1978 INCREMENT BEYOND 1978                1014.   
C                                                                       1015.   
C                   CH4:   OBSERVED RATES THRU 1980,                    1016.   
C                          0.015/YR INCREASE BEYOND 1980                1017.   
C                                                                       1018.   
C                   CFC:   OBSERVED CONCENTRATION THRU 1984,            1019.   
C                          0.03 EMISSION RATE INCREASE BEYOND 1984,     1020.   
C                          DOUBLED FORCING TO ACCOUNT FOR OTHER CFCS    1021.   
C                          SLOW 75YR, 150YR DECAY RATE BEYOND 2000      1022.   
C                                                                       1023.   
C-----------------------------------------------------------------------1024.   
C                                                                       1025.   
      DIMENSION XGAS(1)                                                 1026.   
      DIMENSION XXF11(45),XXF12(45),XXCO2(27)                           1027.   
      DATA XXF11/                                                       1028.   
     10.00001,0.00001,0.00001,0.00001,0.00001,0.00004,0.00010,0.00023,  1029.   
     20.00041,0.00066,0.00100,0.00148,0.00215,0.00297,0.00397,0.00523,  1030.   
     30.00662,0.00791,0.00921,0.01093,0.01316,0.01596,0.01939,0.02345,  1031.   
     40.02807,0.03321,0.03904,0.04569,0.05343,0.06224,0.07186,0.08267,  1032.   
     50.09505,0.10867,0.12174,0.13422,0.14665,0.15820,0.16871,0.17841,  1033.   
     60.18919,0.19983,0.21032,0.22068,0.23090/                          1034.   
      DATA XXF12/                                                       1035.   
     10.00001,0.00001,0.00001,0.00040,0.00112,0.00198,0.00298,0.00411,  1036.   
     20.00539,0.00679,0.00836,0.00998,0.01181,0.01387,0.01619,0.01888,  1037.   
     30.02195,0.02513,0.02868,0.03292,0.03766,0.04312,0.04954,0.05702,  1038.   
     40.06546,0.07485,0.08545,0.09756,0.11130,0.12634,0.14250,0.16011,  1039.   
     50.17962,0.20082,0.22144,0.24105,0.25951,0.27630,0.29247,0.30831,  1040.   
     60.32585,0.34328,0.36059,0.37778,0.39486/                          1041.   
      DATA XXCO2/                                                       1042.   
     1 315.00, 315.69, 316.66, 317.27, 318.24, 318.93, 319.33, 319.95,  1043.   
     2 320.80, 321.44, 322.25, 323.44, 324.82, 326.01, 326.97, 328.86,  1044.   
     3 330.35, 330.71, 331.69, 332.83, 334.58, 335.95, 337.65, 339.04,  1045.   
     4 340.01, 341.56, 343.81/                                          1046.   
C                                                                       1047.   
C                                                                       1048.   
C                                                      CO2 SCENARIOS    1049.   
C-------------------------------------------------------------------    1050.   
  100 N=1                                                               1051.   
      IF(YEAR.GT.1958.D0) GO TO 110                                     1052.   
      DT=(YEAR-1850.D0)/(1958.D0-1850.D0)                               1053.   
      XX=270.D0*(315.D0/270.D0)**DT                                     1054.   
      GO TO 120                                                         1055.   
  110 IF(YEAR.GT.1984.D0) GO TO 115                                     1056.   
      I=YEAR                                                            1057.   
      DELTA=YEAR-I                                                      1058.   
      I=I-1957                                                          1059.   
      XX=XXCO2(I)+DELTA*(XXCO2(I+1)-XXCO2(I))                           1060.   
      GO TO 120                                                         1061.   
  115 DT=YEAR-1984.D0                                                   1062.   
      XX0=343.81                                                        1063.   
      ETA=1.50                                                          1064.   
      XMU=0.015                                                         1065.   
      XX=XX0+ETA*(EXP(XMU*DT)-1.D0)/XMU                                 1066.   
  120 CONTINUE                                                          1067.   
      XGAS(N)=XX                                                        1068.   
C                                                                       1069.   
C                                                      N2O SCENARIOS    1070.   
C-------------------------------------------------------------------    1071.   
  200 N=2                                                               1072.   
      XSS=282.D0                                                        1073.   
      TAU=150.D0                                                        1074.   
      BNA=16.7D0                                                        1075.   
      IF(YEAR.GT.1912.D0) GO TO 205                                     1076.   
      X0=285.355                                                        1077.   
      XMU=0.04                                                          1078.   
      T0=1912.D0                                                        1079.   
      XJ0=11.D0*EXP(-0.01D0*33.D0-0.035D0*33.D0)                        1080.   
      GO TO 219                                                         1081.   
  205 IF(YEAR.GT.1945.D0) GO TO 210                                     1082.   
      X0=289.96                                                         1083.   
      XMU=0.01                                                          1084.   
      T0=1945.D0                                                        1085.   
      XJ0=11.D0*EXP(-0.035D0*33.D0)                                     1086.   
      GO TO 219                                                         1087.   
  210 IF(YEAR.GT.1978.D0) GO TO 215                                     1088.   
      X0=300.2                                                          1089.   
      XMU=0.035                                                         1090.   
      T0=1978.D0                                                        1091.   
      XJ0=11.D0                                                         1092.   
      GO TO 219                                                         1093.   
  215 CONTINUE                                                          1094.   
      X0=300.2                                                          1095.   
      XMU=0.035                                                         1096.   
      T0=1978.D0                                                        1097.   
      XJ0=11.D0                                                         1098.   
  219 DT=YEAR-T0                                                        1099.   
      XX=XSS+(X0-XSS)*EXP(-DT/TAU)                                      1100.   
     +  +XJ0*TAU/BNA/(1.D0+XMU*TAU)*(EXP(XMU*DT)-EXP(-DT/TAU))          1101.   
  220 XX=XX*1.D-03                                                      1102.   
      XGAS(N)=XX                                                        1103.   
C                                                                       1104.   
C                                                      CH4 SCENARIOS    1105.   
C-------------------------------------------------------------------    1106.   
  300 N=3                                                               1107.   
      IF(YEAR.GT.1958.D0) GO TO 305                                     1108.   
      DT=(YEAR-1850.D0)/108.D0                                          1109.   
      XX=1.D0*(1.40D0/1.D0)**DT                                         1110.   
      GO TO 320                                                         1111.   
  305 IF(YEAR.GT.1970.D0) GO TO 310                                     1112.   
      DT=(YEAR-1958.D0)/12.D0                                           1113.   
      XX=1.40D0*(1.50D0/1.40D0)**DT                                     1114.   
      GO TO 320                                                         1115.   
  310 IF(YEAR.GT.1980.D0) GO TO 315                                     1116.   
      DT=(YEAR-1970.D0)/10.D0                                           1117.   
      XX=1.50D0*(1.65D0/1.50D0)**DT                                     1118.   
      GO TO 320                                                         1119.   
  315 DT=YEAR-1980.D0                                                   1120.   
      XX=1.65D0*1.015D0**DT                                             1121.   
  320 CONTINUE                                                          1122.   
      XGAS(N)=XX                                                        1123.   
C                                                                       1124.   
C                                                      F11 SCENARIOS    1125.   
C-------------------------------------------------------------------    1126.   
  400 N=4                                                               1127.   
      IF(YEAR.LT.1985.D0) GO TO 410                                     1128.   
      UAT=1.D0/75.D0                                                    1129.   
      XMU=0.03                                                          1130.   
      XX0=XXF11(45)                                                     1131.   
      ETA=XXF11(45)-XXF11(44)*EXP(-UAT)                                 1132.   
      DT=YEAR-1985.D0                                                   1133.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      1134.   
      GO TO 420                                                         1135.   
  410 XX=0.D0                                                           1136.   
      IF(YEAR.LT.1945.D0) GO TO 420                                     1137.   
      I=YEAR                                                            1138.   
      DELTA=YEAR-I                                                      1139.   
      I=I-1940                                                          1140.   
      XX=XXF11(I)+DELTA*(XXF11(I+1)-XXF11(I))                           1141.   
  420 XX=XX*2.D0                                                        1142.   
      XGAS(N)=XX                                                        1143.   
C                                                                       1144.   
C                                                      F12 SCENARIOS    1145.   
C-------------------------------------------------------------------    1146.   
  500 N=5                                                               1147.   
      IF(YEAR.LT.1985.D0) GO TO 510                                     1148.   
      UAT=1.D0/150.D0                                                   1149.   
      XMU=0.03                                                          1150.   
      XX0=XXF12(45)                                                     1151.   
      ETA=XXF12(45)-XXF12(44)*EXP(-UAT)                                 1152.   
      DT=YEAR-1985.D0                                                   1153.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      1154.   
      GO TO 520                                                         1155.   
  510 XX=0.D0                                                           1156.   
      IF(YEAR.LT.1943.D0) GO TO 520                                     1157.   
      I=YEAR                                                            1158.   
      DELTA=YEAR-I                                                      1159.   
      I=I-1940                                                          1160.   
      XX=XXF12(I)+DELTA*(XXF12(I+1)-XXF12(I))                           1161.   
  520 XX=XX*2.D0                                                        1162.   
      XGAS(N)=XX                                                        1163.   
C                                                                       1164.   
      RETURN                                                            1165.   
      END                                                               1166.   
      SUBROUTINE BTREND(XGAS,YEAR,NGAS)                                 2001.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         2002.   
C                                                                       2003.   
C-----------------------------------------------------------------------2004.   
C     T-GAS SCENARIO  B:   REDUCED TREND FOR TRACE GAS INCREASES        2005.   
C                          REDUCED FORCING BEYOND 2000)                 2006.   
C                                                                       2007.   
C                   CO2:   OBSERVED TREND 1958-1984,                    2008.   
C                          0.015/YR INCREASE IN 1.5 PPM INCREMENT       2009.   
C                          THRU 1990, 0.010/YR THRU 2000, 0.005/YR      2010.   
C                          2010, CONSTANT INCREMENT BEYOND 2010         2011.   
C                                                                       2012.   
C                   N2O:   WEISS FORMULA THRU 1978, 0.035/YR INCREASE   2013.   
C                          IN 1978 INCREMENT THRU 1990, 0.025/YR        2014.   
C                          THRU 2000, 0.015/YR THRU 2010, 0.005/YR      2015.   
C                          INCREASE IN INCREMENT BEYOND 2010            2016.   
C                                                                       2017.   
C                   CH4:   OBSERVED RATES THRU 1980,                    2018.   
C                          0.015/YR INCREASE THRU 1990, 0.010/YR        2019.   
C                          INCREASE THRU 2000, 0.005/YR BEYOND 2000     2020.   
C                                                                       2021.   
C                   CFC:   OBSERVED CONCENTRATION THRU 1984,            2022.   
C                          0.03 EMISSION RATE INCREASE TO 1990,         2023.   
C                          0.02 EMISSION RATE INCREASE TO 2000,         2024.   
C                          0.01 EMISSION RATE INCREASE TO 2010,         2025.   
C                          CONSTANT EMISSION RATE BEYOND 2010           2026.   
C                                                                       2027.   
C-----------------------------------------------------------------------2028.   
C                                                                       2029.   
      DIMENSION XGAS(1)                                                 2030.   
      DIMENSION XXF11(45),XXF12(45),XXCO2(27)                           2031.   
      DATA XXF11/                                                       2032.   
     10.00001,0.00001,0.00001,0.00001,0.00001,0.00004,0.00010,0.00023,  2033.   
     20.00041,0.00066,0.00100,0.00148,0.00215,0.00297,0.00397,0.00523,  2034.   
     30.00662,0.00791,0.00921,0.01093,0.01316,0.01596,0.01939,0.02345,  2035.   
     40.02807,0.03321,0.03904,0.04569,0.05343,0.06224,0.07186,0.08267,  2036.   
     50.09505,0.10867,0.12174,0.13422,0.14665,0.15820,0.16871,0.17841,  2037.   
     60.18919,0.19983,0.21032,0.22068,0.23090/                          2038.   
      DATA XXF12/                                                       2039.   
     10.00001,0.00001,0.00001,0.00040,0.00112,0.00198,0.00298,0.00411,  2040.   
     20.00539,0.00679,0.00836,0.00998,0.01181,0.01387,0.01619,0.01888,  2041.   
     30.02195,0.02513,0.02868,0.03292,0.03766,0.04312,0.04954,0.05702,  2042.   
     40.06546,0.07485,0.08545,0.09756,0.11130,0.12634,0.14250,0.16011,  2043.   
     50.17962,0.20082,0.22144,0.24105,0.25951,0.27630,0.29247,0.30831,  2044.   
     60.32585,0.34328,0.36059,0.37778,0.39486/                          2045.   
      DATA XXCO2/                                                       2046.   
     1 315.00, 315.69, 316.66, 317.27, 318.24, 318.93, 319.33, 319.95,  2047.   
     2 320.80, 321.44, 322.25, 323.44, 324.82, 326.01, 326.97, 328.86,  2048.   
     3 330.35, 330.71, 331.69, 332.83, 334.58, 335.95, 337.65, 339.04,  2049.   
     4 340.01, 341.56, 343.81/                                          2050.   
C                                                                       2051.   
C                                                                       2052.   
C                                                      CO2 SCENARIOS    2053.   
C-------------------------------------------------------------------    2054.   
  100 N=1                                                               2055.   
      IF(YEAR.GT.1958.D0) GO TO 110                                     2056.   
      DT=(YEAR-1850.D0)/(1958.D0-1850.D0)                               2057.   
      XX=270.D0*(315.0D0/270.D0)**DT                                    2058.   
      GO TO 120                                                         2059.   
  110 IF(YEAR.GT.1984.D0) GO TO 115                                     2060.   
      I=YEAR                                                            2061.   
      DELTA=YEAR-I                                                      2062.   
      I=I-1957                                                          2063.   
      XX=XXCO2(I)+DELTA*(XXCO2(I+1)-XXCO2(I))                           2064.   
      GO TO 120                                                         2065.   
  115 XX0=343.81                                                        2066.   
      ETA=1.50                                                          2067.   
      XMU=0.015                                                         2068.   
      IF(YEAR.GT.1990.D0) GO TO 116                                     2069.   
      DT=YEAR-1984.D0                                                   2070.   
      GO TO 119                                                         2071.   
  116 DT=1990.D0-1984.D0                                                2072.   
      XX0=XX0+ETA*(EXP(XMU*DT)-1.D0)/XMU                                2073.   
      ETA=ETA*EXP(XMU*DT)                                               2074.   
      XMU=0.010                                                         2075.   
      IF(YEAR.GT.2000.D0) GO TO 117                                     2076.   
      DT=YEAR-1990.D0                                                   2077.   
      GO TO 119                                                         2078.   
  117 DT=2000.D0-1990.D0                                                2079.   
      XX0=XX0+ETA*(EXP(XMU*DT)-1.D0)/XMU                                2080.   
      ETA=ETA*EXP(XMU*DT)                                               2081.   
      XMU=0.005                                                         2082.   
      IF(YEAR.GT.2010.D0) GO TO 118                                     2083.   
      DT=YEAR-2000.D0                                                   2084.   
      GO TO 119                                                         2085.   
  118 DT=2010.D0-2000.D0                                                2086.   
      XX0=XX0+ETA*(EXP(XMU*DT)-1.D0)/XMU                                2087.   
      ETA=ETA*EXP(XMU*DT)                                               2088.   
      DT=YEAR-2010.D0                                                   2089.   
      XX=XX0+ETA*DT                                                     2090.   
      GO TO 120                                                         2091.   
  119 XX=XX0+ETA*(EXP(XMU*DT)-1.D0)/XMU                                 2092.   
  120 CONTINUE                                                          2093.   
      XGAS(N)=XX                                                        2094.   
C                                                                       2095.   
C                                                      N2O SCENARIOS    2096.   
C-------------------------------------------------------------------    2097.   
  200 N=2                                                               2098.   
      XSS=282.D0                                                        2099.   
      UAT=1.0D0/150.D0                                                  2100.   
      IF(YEAR.GT.1912.D0) GO TO 205                                     2101.   
      XX0=285.355                                                       2102.   
      XMU=0.04                                                          2103.   
      T0=1912.D0                                                        2104.   
      ETA=11.0D0/16.7D0*EXP(-0.01D0*33.D0-0.035D0*33.D0)                2105.   
      GO TO 219                                                         2106.   
  205 IF(YEAR.GT.1945.D0) GO TO 210                                     2107.   
      XX0=289.96                                                        2108.   
      XMU=0.01                                                          2109.   
      T0=1945.D0                                                        2110.   
      ETA=11.0D0/16.7D0*EXP(-0.035D0*33.D0)                             2111.   
      GO TO 219                                                         2112.   
  210 XX0=300.2                                                         2113.   
      XMU=0.035                                                         2114.   
      T0=1978.D0                                                        2115.   
      ETA=11.0D0/16.7D0                                                 2116.   
      IF(YEAR.GT.1990.D0) GO TO 215                                     2117.   
      GO TO 219                                                         2118.   
  215 DT=1990.D0-T0                                                     2119.   
      XX0=XSS+(XX0-XSS)*EXP(-DT*UAT)                                    2120.   
     +  +ETA/(XMU+UAT)*(EXP(XMU*DT)-EXP(-DT*UAT))                       2121.   
      ETA=ETA*EXP(XMU*DT)                                               2122.   
      XMU=0.025                                                         2123.   
      T0=1990.D0                                                        2124.   
      IF(YEAR.GT.2000.D0) GO TO 216                                     2125.   
      GO TO 219                                                         2126.   
  216 DT=2000.D0-T0                                                     2127.   
      XX0=XSS+(XX0-XSS)*EXP(-DT*UAT)                                    2128.   
     +  +ETA/(XMU+UAT)*(EXP(XMU*DT)-EXP(-DT*UAT))                       2129.   
      ETA=ETA*EXP(XMU*DT)                                               2130.   
      XMU=0.015                                                         2131.   
      T0=2000.D0                                                        2132.   
      IF(YEAR.GT.2010.D0) GO TO 217                                     2133.   
      GO TO 219                                                         2134.   
  217 DT=2010.D0-T0                                                     2135.   
      XX0=XSS+(XX0-XSS)*EXP(-DT*UAT)                                    2136.   
     +  +ETA/(XMU+UAT)*(EXP(XMU*DT)-EXP(-DT*UAT))                       2137.   
      ETA=ETA*EXP(XMU*DT)                                               2138.   
      XMU=0.005                                                         2139.   
      T0=2010.D0                                                        2140.   
  219 DT=YEAR-T0                                                        2141.   
      XX=XSS+(XX0-XSS)*EXP(-DT*UAT)                                     2142.   
     +  +ETA/(XMU+UAT)*(EXP(XMU*DT)-EXP(-DT*UAT))                       2143.   
  220 XX=XX*1.D-03                                                      2144.   
      XGAS(N)=XX                                                        2145.   
C                                                                       2146.   
C                                                      CH4 SCENARIOS    2147.   
C-------------------------------------------------------------------    2148.   
  300 N=3                                                               2149.   
      IF(YEAR.GT.1958.D0) GO TO 305                                     2150.   
      DT=(YEAR-1850.D0)/(1958.D0-1850.D0)                               2151.   
      XX=1.D0*(1.4D0/1.D0)**DT                                          2152.   
      GO TO 320                                                         2153.   
  305 IF(YEAR.GT.1970.D0) GO TO 310                                     2154.   
      DT=(YEAR-1958.D0)/12.D0                                           2155.   
      XX=1.4D0*(1.5D0/1.4D0)**DT                                        2156.   
      GO TO 320                                                         2157.   
  310 IF(YEAR.GT.1980.D0) GO TO 315                                     2158.   
      DT=(YEAR-1970.D0)/10.D0                                           2159.   
      XX=1.5D0*(1.65D0/1.5D0)**DT                                       2160.   
      GO TO 320                                                         2161.   
  315 IF(YEAR.GT.1990.D0) GO TO 316                                     2162.   
      DT=YEAR-1980.D0                                                   2163.   
      XX=1.65D0*1.015D0**DT                                             2164.   
      GO TO 320                                                         2165.   
  316 IF(YEAR.GT.2000.D0) GO TO 317                                     2166.   
      DT=YEAR-1990.D0                                                   2167.   
      XX=1.65D0*1.015D0**10.D0*1.01D0**DT                               2168.   
      GO TO 320                                                         2169.   
  317 IF(YEAR.GT.2010.D0) GO TO 318                                     2170.   
      DT=YEAR-2000.D0                                                   2171.   
      XX=1.65D0*1.015D0**10.D0*1.010D0**10.D0*1.005D0**DT               2172.   
      GO TO 320                                                         2173.   
  318 DT=YEAR-2010.D0                                                   2174.   
      XX=1.65D0*1.015D0**10.D0*1.01D0**10.D0*1.005D0**10.D0*1.005D0**DT 2175.   
  320 CONTINUE                                                          2176.   
      XGAS(N)=XX                                                        2177.   
C                                                                       2178.   
C                                                      F11 SCENARIOS    2179.   
C-------------------------------------------------------------------    2180.   
  400 N=4                                                               2181.   
      XX=0.D0                                                           2182.   
      IF(YEAR.LT.1945.D0) GO TO 420                                     2183.   
      IF(YEAR.GT.1985.D0) GO TO 410                                     2184.   
      I=YEAR                                                            2185.   
      DELTA=YEAR-I                                                      2186.   
      I=I-1940                                                          2187.   
      XX=XXF11(I)+DELTA*(XXF11(I+1)-XXF11(I))                           2188.   
      GO TO 420                                                         2189.   
  410 UAT=1.0D0/75.D0                                                   2190.   
      XMU=0.03                                                          2191.   
      XX0=XXF11(45)                                                     2192.   
      ETA=XXF11(45)-XXF11(44)*EXP(-UAT)                                 2193.   
      IF(YEAR.GT.1990.D0) GO TO 415                                     2194.   
      DT=YEAR-1985.D0                                                   2195.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      2196.   
      GO TO 420                                                         2197.   
  415 DT=1990.D0-1985.D0                                                2198.   
      XX0=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))     2199.   
      ETA=ETA*EXP(XMU*DT)                                               2200.   
      XMU=0.02                                                          2201.   
      IF(YEAR.GT.2000.D0) GO TO 416                                     2202.   
      DT=YEAR-1990.D0                                                   2203.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      2204.   
      GO TO 420                                                         2205.   
  416 DT=2000.D0-1990.D0                                                2206.   
      XX0=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))     2207.   
      ETA=ETA*EXP(XMU*DT)                                               2208.   
      XMU=0.01                                                          2209.   
      IF(YEAR.GT.2010.D0) GO TO 417                                     2210.   
      DT=YEAR-2000.D0                                                   2211.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      2212.   
      GO TO 420                                                         2213.   
  417 DT=2010.D0-2000.D0                                                2214.   
      XX0=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))     2215.   
      ETA=ETA*EXP(XMU*DT)                                               2216.   
      XMU=0.D0                                                          2217.   
      DT=YEAR-2010.D0                                                   2218.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      2219.   
  420 XX=XX*1.D0                                                        2220.   
      XGAS(N)=XX                                                        2221.   
C                                                                       2222.   
C                                                      F12 SCENARIOS    2223.   
C-------------------------------------------------------------------    2224.   
  500 N=5                                                               2225.   
      XX=0.D0                                                           2226.   
      IF(YEAR.LT.1943.D0) GO TO 520                                     2227.   
      IF(YEAR.GT.1985.D0) GO TO 510                                     2228.   
      I=YEAR                                                            2229.   
      DELTA=YEAR-I                                                      2230.   
      I=I-1940                                                          2231.   
      XX=XXF12(I)+DELTA*(XXF12(I+1)-XXF12(I))                           2232.   
      GO TO 520                                                         2233.   
  510 UAT=1.0D0/150.D0                                                  2234.   
      XMU=0.03                                                          2235.   
      XX0=XXF12(45)                                                     2236.   
      ETA=XXF12(45)-XXF12(44)*EXP(-UAT)                                 2237.   
      IF(YEAR.GT.1990.D0) GO TO 515                                     2238.   
      DT=YEAR-1985.D0                                                   2239.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      2240.   
      GO TO 520                                                         2241.   
  515 DT=1990.D0-1985.D0                                                2242.   
      XX0=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))     2243.   
      ETA=ETA*EXP(XMU*DT)                                               2244.   
      XMU=0.02                                                          2245.   
      IF(YEAR.GT.2000.D0) GO TO 516                                     2246.   
      DT=YEAR-1990.D0                                                   2247.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      2248.   
      GO TO 520                                                         2249.   
  516 DT=2000.D0-1990.D0                                                2250.   
      XX0=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))     2251.   
      ETA=ETA*EXP(XMU*DT)                                               2252.   
      XMU=0.01                                                          2253.   
      IF(YEAR.GT.2010.D0) GO TO 517                                     2254.   
      DT=YEAR-2000.D0                                                   2255.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      2256.   
      GO TO 520                                                         2257.   
  517 DT=2010.D0-2000.D0                                                2258.   
      XX0=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))     2259.   
      ETA=ETA*EXP(XMU*DT)                                               2260.   
      XMU=0.D0                                                          2261.   
      DT=YEAR-2010.D0                                                   2262.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      2263.   
  520 XX=XX*1.D0                                                        2264.   
      XGAS(N)=XX                                                        2265.   
C                                                                       2266.   
      RETURN                                                            2267.   
      END                                                               2268.   
      SUBROUTINE CTREND(XGAS,YEAR,NGAS)                                 3001.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         3002.   
C                                                                       3003.   
C-----------------------------------------------------------------------3004.   
C     T-GAS SCENARIO  C:   CURTAILED TREND FOR TRACE GAS INCREASES      3005.   
C                            (EFFECTIVE FORCING CUTOFF BEYOND 2000)     3006.   
C                                                                       3007.   
C                   CO2:   OBSERVED TREND 1958-1984,                    3008.   
C                          1.5PPM FIXED INCREMENTS 1984-2000            3009.   
C                          FIXED CONCENTRATION BEYOND 2000              3010.   
C                                                                       3011.   
C                   N2O:   WEISS FORMULA THRU 1984,                     3012.   
C                          FIXED INCREMENT 1984-2000,                   3013.   
C                          FIXED CONCENTRATION BEYOND 2000              3014.   
C                                                                       3015.   
C                   CH4:   OBSERVED RATES THRU 1980,                    3016.   
C                          0.01 INCREMENTS TO 1990, 0.005 TO 2000       3017.   
C                          FIXED CONCENTRATION BEYOND 2000              3018.   
C                                                                       3019.   
C                   CFC:   OBSERVED CONCENTRATION THRU 1984,            3020.   
C                          0.03 EMISSION RATE INCREASE TO 1990,         3021.   
C                          EMISSIONS TO ZERO DURING 1990-2000,          3022.   
C                          SLOW 75YR, 150YR DECAY RATE BEYOND 2000      3023.   
C                                                                       3024.   
C-----------------------------------------------------------------------3025.   
C                                                                       3026.   
      DIMENSION XGAS(1)                                                 3027.   
      DIMENSION XXF11(45),XXF12(45),XXCO2(27)                           3028.   
      DATA XXF11/                                                       3029.   
     10.00001,0.00001,0.00001,0.00001,0.00001,0.00004,0.00010,0.00023,  3030.   
     20.00041,0.00066,0.00100,0.00148,0.00215,0.00297,0.00397,0.00523,  3031.   
     30.00662,0.00791,0.00921,0.01093,0.01316,0.01596,0.01939,0.02345,  3032.   
     40.02807,0.03321,0.03904,0.04569,0.05343,0.06224,0.07186,0.08267,  3033.   
     50.09505,0.10867,0.12174,0.13422,0.14665,0.15820,0.16871,0.17841,  3034.   
     60.18919,0.19983,0.21032,0.22068,0.23090/                          3035.   
      DATA XXF12/                                                       3036.   
     10.00001,0.00001,0.00001,0.00040,0.00112,0.00198,0.00298,0.00411,  3037.   
     20.00539,0.00679,0.00836,0.00998,0.01181,0.01387,0.01619,0.01888,  3038.   
     30.02195,0.02513,0.02868,0.03292,0.03766,0.04312,0.04954,0.05702,  3039.   
     40.06546,0.07485,0.08545,0.09756,0.11130,0.12634,0.14250,0.16011,  3040.   
     50.17962,0.20082,0.22144,0.24105,0.25951,0.27630,0.29247,0.30831,  3041.   
     60.32585,0.34328,0.36059,0.37778,0.39486/                          3042.   
      DATA XXCO2/                                                       3043.   
     1 315.00, 315.69, 316.66, 317.27, 318.24, 318.93, 319.33, 319.95,  3044.   
     2 320.80, 321.44, 322.25, 323.44, 324.82, 326.01, 326.97, 328.86,  3045.   
     3 330.35, 330.71, 331.69, 332.83, 334.58, 335.95, 337.65, 339.04,  3046.   
     4 340.01, 341.56, 343.81/                                          3047.   
C                                                                       3048.   
C                                                                       3049.   
C                                                      CO2 SCENARIOS    3050.   
C-------------------------------------------------------------------    3051.   
  100 N=1                                                               3052.   
      IF(YEAR.GT.1958.D0) GO TO 110                                     3053.   
      DT=(YEAR-1850.D0)/(1958.D0-1850.D0)                               3054.   
      XX=270.D0*(315.0D0/270.D0)**DT                                    3055.   
      GO TO 120                                                         3056.   
  110 IF(YEAR.GT.1984.D0) GO TO 115                                     3057.   
      I=YEAR                                                            3058.   
      DELTA=YEAR-I                                                      3059.   
      I=I-1957                                                          3060.   
      XX=XXCO2(I)+DELTA*(XXCO2(I+1)-XXCO2(I))                           3061.   
      GO TO 120                                                         3062.   
  115 XX0=343.81                                                        3063.   
      ETA=1.50                                                          3064.   
      XMU=0.D0                                                          3065.   
      IF(YEAR.GT.2000.D0) GO TO 116                                     3066.   
      DT=YEAR-1984.D0                                                   3067.   
      GO TO 119                                                         3068.   
  116 DT=2000.D0-1984.D0                                                3069.   
      XX0=XX0+ETA*DT                                                    3070.   
      DT=0.D0                                                           3071.   
  119 XX=XX0+ETA*DT                                                     3072.   
  120 CONTINUE                                                          3073.   
      XGAS(N)=XX                                                        3074.   
C                                                                       3075.   
C                                                      N2O SCENARIOS    3076.   
C-------------------------------------------------------------------    3077.   
  200 N=2                                                               3078.   
      XSS=282.D0                                                        3079.   
      UAT=1.0D0/150.D0                                                  3080.   
      IF(YEAR.GT.1912.D0) GO TO 205                                     3081.   
      XX0=285.355                                                       3082.   
      XMU=0.04                                                          3083.   
      T0=1912.D0                                                        3084.   
      ETA=11.0D0/16.7D0*EXP(-0.01D0*33.D0-0.035D0*33.D0)                3085.   
      GO TO 219                                                         3086.   
  205 IF(YEAR.GT.1945.D0) GO TO 210                                     3087.   
      XX0=289.96                                                        3088.   
      XMU=0.01                                                          3089.   
      T0=1945.D0                                                        3090.   
      ETA=11.0D0/16.7D0*EXP(-0.035D0*33.D0)                             3091.   
      GO TO 219                                                         3092.   
  210 XX0=300.2                                                         3093.   
      XMU=0.035                                                         3094.   
      T0=1978.D0                                                        3095.   
      ETA=11.0D0/16.7D0                                                 3096.   
      IF(YEAR.GT.1984.D0) GO TO 215                                     3097.   
      GO TO 219                                                         3098.   
  215 DT=1984.D0-T0                                                     3099.   
      XX0=XSS+(XX0-XSS)*EXP(-DT*UAT)                                    3100.   
     +  +ETA/(XMU+UAT)*(EXP(XMU*DT)-EXP(-DT*UAT))                       3101.   
      ETA=ETA*EXP(XMU*DT)                                               3102.   
      XMU=0.D0                                                          3103.   
      T0=1984.D0                                                        3104.   
      IF(YEAR.GT.2000.D0) GO TO 216                                     3105.   
      GO TO 219                                                         3106.   
  216 DT=2000.D0-T0                                                     3107.   
      XX0=XSS+(XX0-XSS)*EXP(-DT*UAT)                                    3108.   
     +  +ETA/(XMU+UAT)*(EXP(XMU*DT)-EXP(-DT*UAT))                       3109.   
      ETA=ETA*EXP(XMU*DT)                                               3110.   
      T0=YEAR                                                           3111.   
  219 DT=YEAR-T0                                                        3112.   
      XX=XSS+(XX0-XSS)*EXP(-DT*UAT)                                     3113.   
     +  +ETA/(XMU+UAT)*(EXP(XMU*DT)-EXP(-DT*UAT))                       3114.   
  220 XX=XX*1.D-03                                                      3115.   
      XGAS(N)=XX                                                        3116.   
C                                                                       3117.   
C                                                      CH4 SCENARIOS    3118.   
C-------------------------------------------------------------------    3119.   
  300 N=3                                                               3120.   
      IF(YEAR.GT.1958.D0) GO TO 305                                     3121.   
      DT=(YEAR-1850.D0)/(1958.D0-1850.D0)                               3122.   
      XX=1.D0*(1.4D0/1.D0)**DT                                          3123.   
      GO TO 320                                                         3124.   
  305 IF(YEAR.GT.1970.D0) GO TO 310                                     3125.   
      DT=(YEAR-1958.D0)/12.D0                                           3126.   
      XX=1.4D0*(1.5D0/1.4D0)**DT                                        3127.   
      GO TO 320                                                         3128.   
  310 IF(YEAR.GT.1980.D0) GO TO 315                                     3129.   
      DT=(YEAR-1970.D0)/10.D0                                           3130.   
      XX=1.5D0*(1.65D0/1.5D0)**DT                                       3131.   
      GO TO 320                                                         3132.   
  315 IF(YEAR.GT.1990.D0) GO TO 316                                     3133.   
      DT=YEAR-1980.D0                                                   3134.   
      XX=1.65D0*1.01D0**DT                                              3135.   
      GO TO 320                                                         3136.   
  316 IF(YEAR.GT.2000.D0) GO TO 317                                     3137.   
      DT=YEAR-1990.D0                                                   3138.   
      XX=1.65D0*1.01D0**10.D0*1.005D0**DT                               3139.   
      GO TO 320                                                         3140.   
  317 DT=YEAR-2000.D0                                                   3141.   
      XX=1.65D0*1.01D0**10.D0*1.005D0**10.D0                            3142.   
  320 CONTINUE                                                          3143.   
      XGAS(N)=XX                                                        3144.   
C                                                                       3145.   
C                                                      F11 SCENARIOS    3146.   
C-------------------------------------------------------------------    3147.   
  400 N=4                                                               3148.   
      XX=0.D0                                                           3149.   
      IF(YEAR.LT.1945.D0) GO TO 420                                     3150.   
      IF(YEAR.GE.1985.D0) GO TO 410                                     3151.   
      I=YEAR                                                            3152.   
      DELTA=YEAR-I                                                      3153.   
      I=I-1940                                                          3154.   
      XX=XXF11(I)+DELTA*(XXF11(I+1)-XXF11(I))                           3155.   
      GO TO 420                                                         3156.   
  410 UAT=1.0D0/75.D0                                                   3157.   
      TAU=75.D0                                                         3158.   
      XMU=0.03                                                          3159.   
      XX0=XXF11(45)                                                     3160.   
      ETA=XXF11(45)-XXF11(44)*EXP(-UAT)                                 3161.   
      IF(YEAR.GT.1990.D0) GO TO 415                                     3162.   
      DT=YEAR-1985.D0                                                   3163.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      3164.   
      GO TO 420                                                         3165.   
  415 DT=1990.D0-1985.D0                                                3166.   
      XX0=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))     3167.   
      ETA=ETA*EXP(XMU*DT)                                               3168.   
      IF(YEAR.GT.2000.D0) GO TO 416                                     3169.   
      DT=YEAR-1990.D0                                                   3170.   
      TAUETA=TAU*ETA+TAU*TAU*ETA/10.D0                                  3171.   
      XX=(XX0-TAUETA)*EXP(-UAT*DT)+TAUETA-TAU*ETA/10.D0*DT              3172.   
      GO TO 420                                                         3173.   
  416 DT=2000.D0-1990.D0                                                3174.   
      TAUETA=TAU*ETA+TAU*TAU*ETA/10.D0                                  3175.   
      XX0=(XX0-TAUETA)*EXP(-UAT*DT)+TAUETA-TAU*ETA/10.D0*DT             3176.   
      DT=YEAR-2000.D0                                                   3177.   
      XX=XX0*EXP(-UAT*DT)                                               3178.   
  420 XX=XX*1.D0                                                        3179.   
      XGAS(N)=XX                                                        3180.   
C                                                                       3181.   
C                                                      F12 SCENARIOS    3182.   
C-------------------------------------------------------------------    3183.   
  500 N=5                                                               3184.   
      XX=0.D0                                                           3185.   
      IF(YEAR.LT.1943.D0) GO TO 520                                     3186.   
      IF(YEAR.GE.1985.D0) GO TO 510                                     3187.   
      I=YEAR                                                            3188.   
      DELTA=YEAR-I                                                      3189.   
      I=I-1940                                                          3190.   
      XX=XXF12(I)+DELTA*(XXF12(I+1)-XXF12(I))                           3191.   
      GO TO 520                                                         3192.   
  510 UAT=1.0D0/150.D0                                                  3193.   
      TAU=150.D0                                                        3194.   
      XMU=0.03                                                          3195.   
      XX0=XXF12(45)                                                     3196.   
      ETA=XXF12(45)-XXF12(44)*EXP(-UAT)                                 3197.   
      IF(YEAR.GT.1990.D0) GO TO 515                                     3198.   
      DT=YEAR-1985.D0                                                   3199.   
      XX=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))      3200.   
      GO TO 520                                                         3201.   
  515 DT=1990.D0-1985.D0                                                3202.   
      XX0=XX0*EXP(-UAT*DT)+ETA/(UAT+XMU)*(EXP(XMU*DT)-EXP(-UAT*DT))     3203.   
      ETA=ETA*EXP(XMU*DT)                                               3204.   
      IF(YEAR.GT.2000.D0) GO TO 516                                     3205.   
      DT=YEAR-1990.D0                                                   3206.   
      TAUETA=TAU*ETA+TAU*TAU*ETA/10.D0                                  3207.   
      XX=(XX0-TAUETA)*EXP(-UAT*DT)+TAUETA-TAU*ETA/10.D0*DT              3208.   
      GO TO 520                                                         3209.   
  516 DT=2000.D0-1990.D0                                                3210.   
      TAUETA=TAU*ETA+TAU*TAU*ETA/10.D0                                  3211.   
      XX0=(XX0-TAUETA)*EXP(-UAT*DT)+TAUETA-TAU*ETA/10.D0*DT             3212.   
      DT=YEAR-2000.D0                                                   3213.   
      XX=XX0*EXP(-UAT*DT)                                               3214.   
  520 XX=XX*1.D0                                                        3215.   
      XGAS(N)=XX                                                        3216.   
C                                                                       3217.   
      RETURN                                                            3218.   
      END                                                               3219.   
      SUBROUTINE DTREND(XGAS,YEAR,NGAS)                                 4001.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         4002.   
C                                                                       4003.   
C-----------------------------------------------------------------------4004.   
C     T-GAS SCENARIO  D:   CURTAILED TREND FOR TRACE GAS INCREASES      4005.   
C                            (EFFECTIVE FORCING CUTOFF BEYOND 2000)     4006.   
C                                                                       4007.   
C                   CO2:   OBSERVED TREND 1958-1984,                    4008.   
C                          0.015/YR RATE OF INCREASE                    4009.   
C                          ON 1.5 PPM INCREMENT BEYOND 1984             4010.   
C                                                                       4011.   
C                   N2O:   OBSERVED RATES THRU 1985,                    4012.   
C                          0.0025/YR RATE OF INCREASE BEYOND 1985       4013.   
C                                                                       4014.   
C                   CH4:   OBSERVED RATES THRU 1990,                    4015.   
C                          DECADAL RATE 1980 TO 1990,                   4016.   
C                          IS PROJECTED BEYOND 1990                     4017.   
C                                                                       4018.   
C                   CFC:   OBSERVED CONCENTRATION THRU 1985,            4019.   
C                          FIXED EMISSION RATE THEREAFTER,              4020.   
C                          1985 EMISSION RATE: .35 (F11); .45 (F12),    4021.   
C                          60YR (F11), 120YR (F12) DECAY RATE           4022.   
C                                                                       4023.   
C-----------------------------------------------------------------------4024.   
C                                                                       4025.   
      DIMENSION XGAS(1)                                                 4026.   
      DIMENSION XXCO2(27),XXN2O(6),XXCH4(6),XXF11(6),XXF12(6)           4027.   
      DIMENSION YRCO2(7),XRCO2(7)                                       4028.   
C                                                                       4029.   
      DATA YRCO2/1700.0,1800.0,1850.0,1900.0,1920.0,1940.0,1958.0/      4030.   
C                                                                       4031.   
      DATA XRCO2/ 275.0, 280.0, 285.0, 295.0, 302.0, 308.0, 315.0/      4032.   
C                                                                       4033.   
      DATA XXCO2/                                                       4034.   
     1 315.00, 315.69, 316.66, 317.27, 318.24, 318.93, 319.33, 319.95,  4035.   
     2 320.80, 321.44, 322.25, 323.44, 324.82, 326.01, 326.97, 328.86,  4036.   
     3 330.35, 330.71, 331.69, 332.83, 334.58, 335.95, 337.65, 339.04,  4037.   
     4 340.01, 341.56, 343.81/                                          4038.   
C                                                                       4039.   
      DATA XXN2O/ 0.289,  0.292,  0.295,  0.298,  0.302,  0.306/        4040.   
C     DATA XXCH4/ 1.255,  1.316,  1.375,  1.450,  1.525,  1.600/        4041.   
      DATA XXF11/ 0.011,  0.027,  0.060,  0.116,  0.173,  0.220/        4042.   
      DATA XXF12/ 0.033,  0.064,  0.121,  0.207,  0.297,  0.375/        4043.   
C                                                                       4044.   
C                                                                       4045.   
C                                                                       4046.   
C                                                      CO2 SCENARIOS    4047.   
C-------------------------------------------------------------------    4048.   
  100 N=1                                                               4049.   
      XX=275.D0                                                         4050.   
      IF(YEAR.LE.1700.D0) GO TO 150                                     4051.   
      IF(YEAR.GE.1958.D0) GO TO 130                                     4052.   
      YRCO2I=YRCO2(1)                                                   4053.   
      XRCO2I=XRCO2(1)                                                   4054.   
      DO 110 J=2,7                                                      4055.   
      YRCO2J=YRCO2(J)                                                   4056.   
      XRCO2J=XRCO2(J)                                                   4057.   
      IF(YEAR.LT.YRCO2J) GO TO 120                                      4058.   
      YRCO2I=YRCO2J                                                     4059.   
  110 XRCO2I=XRCO2J                                                     4060.   
      GO TO 130                                                         4061.   
  120 DT=(YEAR-YRCO2I)/(YRCO2J-YRCO2I)                                  4062.   
      XX=XRCO2I*(XRCO2J/XRCO2I)**DT                                     4063.   
      GO TO 150                                                         4064.   
  130 IF(YEAR.GT.1984.D0) GO TO 140                                     4065.   
      IY=YEAR                                                           4066.   
      DELTA=YEAR-IY                                                     4067.   
      IY=IY-1957                                                        4068.   
      XX=XXCO2(IY)+DELTA*(XXCO2(IY+1)-XXCO2(IY))                        4069.   
      GO TO 150                                                         4070.   
  140 DT=YEAR-1984.D0                                                   4071.   
      XX0=343.81                                                        4072.   
      ETA=1.50                                                          4073.   
      XMU=0.015                                                         4074.   
      XX=XX0+ETA*(EXP(XMU*DT)-1.D0)/XMU                                 4075.   
  150 CONTINUE                                                          4076.   
      XGAS(N)=XX                                                        4077.   
C                                                                       4078.   
C                                                      N2O SCENARIOS    4079.   
C-------------------------------------------------------------------    4080.   
  200 N=2                                                               4081.   
      IF(YEAR.GT.1960.D0) GO TO 210                                     4082.   
      DT=(YEAR-1850.D0)/(1960.D0-1850.D0)                               4083.   
      XX=0.285D0*(0.289D0/0.285D0)**DT                                  4084.   
      GO TO 220                                                         4085.   
  210 IF(YEAR.GE.1985.D0) GO TO 215                                     4086.   
      I=YEAR/5.D0                                                       4087.   
      DT=(YEAR-I*5)/5.D0                                                4088.   
      I=I-391                                                           4089.   
      XX=XXN2O(I)*(XXN2O(I+1)/XXN2O(I))**DT                             4090.   
      GO TO 220                                                         4091.   
  215 XX0=0.306                                                         4092.   
      DT=YEAR-1985.D0                                                   4093.   
      XX=0.306D0*1.0025D0**DT                                           4094.   
  220 CONTINUE                                                          4095.   
      XGAS(N)=XX                                                        4096.   
C                                                                       4097.   
C                                                      CH4 SCENARIOS    4098.   
C-------------------------------------------------------------------    4099.   
  300 N=3                                                               4100.   
      XX=0.750                                                          4101.   
      IF(YEAR.LT.1800.D0) GO TO 370                                     4102.   
      IF(YEAR.GT.1850.D0) GO TO 310                                     4103.   
      DT=(YEAR-1800.D0)/(1850.D0-1800.D0)                               4104.   
      XX=0.75D0*(0.8D0/0.75D0)**DT                                      4105.   
      GO TO 370                                                         4106.   
  310 IF(YEAR.GT.1900.D0) GO TO 320                                     4107.   
      DT=(YEAR-1850.D0)/(1900.D0-1850.D0)                               4108.   
      XX=0.8D0*(0.95D0/0.8D0)**DT                                       4109.   
      GO TO 370                                                         4110.   
  320 CONTINUE                                                          4111.   
      IF(YEAR.GT.1950.D0) GO TO 330                                     4112.   
      DT=(YEAR-1900.D0)/(1950.D0-1900.D0)                               4113.   
      XX=0.95D0*(1.2D0/0.95D0)**DT                                      4114.   
      GO TO 370                                                         4115.   
  330 IF(YEAR.GT.1960.D0) GO TO 340                                     4116.   
      DT=(YEAR-1950.D0)/(1960.D0-1950.D0)                               4117.   
      XX=1.2D0*(1.3D0/1.2D0)**DT                                        4118.   
      GO TO 370                                                         4119.   
  340 IF(YEAR.GT.1970.D0) GO TO 350                                     4120.   
      DT=(YEAR-1960.D0)/(1970.D0-1960.D0)                               4121.   
      XX=1.3D0*(1.4D0/1.3D0)**DT                                        4122.   
      GO TO 370                                                         4123.   
  350 IF(YEAR.GT.1980.D0) GO TO 360                                     4124.   
      DT=(YEAR-1970.D0)/(1980.D0-1970.D0)                               4125.   
      XX=1.4D0*(1.555D0/1.4D0)**DT                                      4126.   
      GO TO 370                                                         4127.   
  360 CONTINUE                                                          4128.   
      DT=(YEAR-1980.D0)/(1990.D0-1980.D0)                               4129.   
      XX=1.555D0*(1.725D0/1.555D0)**DT                                  4130.   
      GO TO 370                                                         4131.   
  370 CONTINUE                                                          4132.   
      XGAS(N)=XX                                                        4133.   
C                                                                       4134.   
C                                                      F11 SCENARIOS    4135.   
C-------------------------------------------------------------------    4136.   
  400 N=4                                                               4137.   
      IF(YEAR.GT.1960.D0) GO TO 410                                     4138.   
      DT=(YEAR-1950.D0)/(1960.D0-1950.D0)                               4139.   
      XX=DT*XXF11(1)                                                    4140.   
      IF(XX.LT.0.D0) XX=0.D0                                            4141.   
      GO TO 420                                                         4142.   
  410 IF(YEAR.GE.1985.D0) GO TO 415                                     4143.   
      I=YEAR/5.D0                                                       4144.   
      DT=(YEAR-I*5)/5.D0                                                4145.   
      I=I-391                                                           4146.   
      XX=XXF11(I)*(XXF11(I+1)/XXF11(I))**DT                             4147.   
      GO TO 420                                                         4148.   
  415 DT=YEAR-1985.D0                                                   4149.   
      TAU=60.D0                                                         4150.   
      DLOSS=EXP(-DT/TAU)                                                4151.   
      FLXC=0.35D0/23.2D0                                                4152.   
      DGAIN=FLXC*TAU*(1.D0-DLOSS)                                       4153.   
      XX1=0.220                                                         4154.   
      XX=XX1*DLOSS+DGAIN                                                4155.   
  420 CONTINUE                                                          4156.   
      XGAS(N)=XX                                                        4157.   
C                                                                       4158.   
C                                                      F12 SCENARIOS    4159.   
C-------------------------------------------------------------------    4160.   
  500 N=5                                                               4161.   
      IF(YEAR.GT.1960.D0) GO TO 510                                     4162.   
      DT=(YEAR-1950.D0)/(1960.D0-1950.D0)                               4163.   
      XX=DT*XXF12(1)                                                    4164.   
      IF(XX.LT.0.D0) XX=0.D0                                            4165.   
      GO TO 520                                                         4166.   
  510 IF(YEAR.GE.1985.D0) GO TO 515                                     4167.   
      I=YEAR/5.D0                                                       4168.   
      DT=(YEAR-I*5)/5.D0                                                4169.   
      I=I-391                                                           4170.   
      XX=XXF12(I)*(XXF12(I+1)/XXF12(I))**DT                             4171.   
      GO TO 520                                                         4172.   
  515 DT=YEAR-1985.D0                                                   4173.   
      TAU=120.D0                                                        4174.   
      DLOSS=EXP(-DT/TAU)                                                4175.   
      FLXC=0.45D0/20.4D0                                                4176.   
      DGAIN=FLXC*TAU*(1.D0-DLOSS)                                       4177.   
      XX1=0.375                                                         4178.   
      XX=XX1*DLOSS+DGAIN                                                4179.   
  520 CONTINUE                                                          4180.   
      XGAS(N)=XX                                                        4181.   
C                                                                       4182.   
      RETURN                                                            4183.   
      END                                                               4184.   
      SUBROUTINE YTREND(XGAS,YEAR,NGAS)                                 5001.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         5002.   
C                                                                       5003.   
C-----------------------------------------------------------------------5004.   
C     T-GAS SCENARIO  Y:   CFC TRACE GAS INCREASES                      5005.   
C                                                                       5006.   
C                   F22:   OBSERVED TREND THRU 1985,                    5007.   
C                                                                       5008.   
C                                                                       5009.   
C                  F113:   OBSERVED TREND THRU 1985,                    5010.   
C                                                                       5011.   
C                                                                       5012.   
C                  F114:   OBSERVED TREND THRU 1985,                    5013.   
C                                                                       5014.   
C                                                                       5015.   
C                  CCL4:   OBSERVED TREND THRU 1985,                    5016.   
C                                                                       5017.   
C                                                                       5018.   
C-----------------------------------------------------------------------5019.   
C                                                                       5020.   
      DIMENSION XGAS(1)                                                 5021.   
      DIMENSION XXF113(6),XXF114(6),XXF115(6),XXCCL(6),XXF22(6)         5022.   
c  ** MFS (MOVED)
      DIMENSION XXFLX(4),XXTAU(4),XXFLC(4)                              
c  ** END (MOVED)
      DATA XXF22/ 0.001,  0.004,  0.010,  0.027,  0.054,  0.080/        5023.   
      DATA XXF113/0.0002, 0.0008, 0.0023, 0.0063, 0.0153, 0.0300/       5024.   
      DATA XXF114/0.0002, 0.0006, 0.0014, 0.0024, 0.0038, 0.0050/       5025.   
      DATA XXF115/0.0001, 0.0001, 0.0002, 0.0008, 0.0021, 0.0040/       5026.   
      DATA XXCCL/ 0.075,  0.080,  0.085,  0.090,  0.095,  0.100/        5027.   
C                                                                       5028.   
c  ** MFS (MOVED)
c      DIMENSION XXFLX(4),XXTAU(4),XXFLC(4)                              5029.   
c  ** END (MOVED)
      DATA XXFLX/ 0.140, 0.150, 0.015, 0.005/                           5030.   
      DATA XXTAU/  20.0,  90.0, 200.0, 400.0/                           5031.   
      DATA XXFLC/  14.6,  31.6,  28.9,  26.1/                           5032.   
C                                                                       5033.   
C                                                                       5034.   
C                                                                       5035.   
C                                                      F22 SCENARIO     5036.   
C-------------------------------------------------------------------    5037.   
  100 N=1                                                               5038.   
      IF(YEAR.GT.1960.D0) GO TO 110                                     5039.   
      DT=(YEAR-1950.D0)/(1960.D0-1950.D0)                               5040.   
      XX=DT*XXF22(1)                                                    5041.   
      IF(XX.LT.0.D0) XX=0.D0                                            5042.   
      GO TO 120                                                         5043.   
  110 IF(YEAR.GE.1985.D0) GO TO 115                                     5044.   
      I=YEAR/5.D0                                                       5045.   
      DT=(YEAR-I*5)/5.D0                                                5046.   
      I=I-391                                                           5047.   
      XX=XXF22(I)*(XXF22(I+1)/XXF22(I))**DT                             5048.   
      GO TO 120                                                         5049.   
  115 DT=YEAR-1985.D0                                                   5050.   
      TAU=XXTAU(N)                                                      5051.   
      DLOSS=EXP(-DT/TAU)                                                5052.   
      FLXC=XXFLX(N)/XXFLC(N)                                            5053.   
      DFLX=0.007D0/XXFLX(N)                                             5054.   
      TAUETA=TAU*FLXC*(1.D0-TAU*DFLX)                                   5055.   
      XX1=XXF22(6)                                                      5056.   
      XX=(XX1-TAUETA)*DLOSS+TAUETA+TAU*FLXC*DFLX*DT                     5057.   
  120 CONTINUE                                                          5058.   
      XGAS(N)=XX                                                        5059.   
C                                                                       5060.   
C                                                      F113 SCENARIO    5061.   
C-------------------------------------------------------------------    5062.   
  200 N=2                                                               5063.   
      IF(YEAR.GT.1960.D0) GO TO 210                                     5064.   
      DT=(YEAR-1950.D0)/(1960.D0-1950.D0)                               5065.   
      XX=DT*XXF113(1)                                                   5066.   
      IF(XX.LT.0.D0) XX=0.D0                                            5067.   
      GO TO 220                                                         5068.   
  210 IF(YEAR.GE.1985.D0) GO TO 215                                     5069.   
      I=YEAR/5.D0                                                       5070.   
      DT=(YEAR-I*5)/5.D0                                                5071.   
      I=I-391                                                           5072.   
      XX=XXF113(I)*(XXF113(I+1)/XXF113(I))**DT                          5073.   
      GO TO 220                                                         5074.   
  215 DT=YEAR-1985.D0                                                   5075.   
      TAU=XXTAU(N)                                                      5076.   
      DLOSS=EXP(-DT/TAU)                                                5077.   
      FLXC=XXFLX(N)/XXFLC(N)                                            5078.   
      DGAIN=FLXC*TAU*(1.D0-DLOSS)                                       5079.   
      XX1=XXF113(6)                                                     5080.   
      XX=XX1*DLOSS+DGAIN                                                5081.   
  220 CONTINUE                                                          5082.   
      XGAS(N)=XX                                                        5083.   
C                                                                       5084.   
C                                                      F114 SCENARIO    5085.   
C-------------------------------------------------------------------    5086.   
  300 N=3                                                               5087.   
      IF(YEAR.GT.1960.D0) GO TO 310                                     5088.   
      DT=(YEAR-1950.D0)/(1960.D0-1950.D0)                               5089.   
      XX=DT*XXF114(1)                                                   5090.   
      IF(XX.LT.0.D0) XX=0.D0                                            5091.   
      GO TO 320                                                         5092.   
  310 IF(YEAR.GE.1985.D0) GO TO 315                                     5093.   
      I=YEAR/5.D0                                                       5094.   
      DT=(YEAR-I*5)/5.D0                                                5095.   
      I=I-391                                                           5096.   
      XX=XXF114(I)*(XXF114(I+1)/XXF114(I))**DT                          5097.   
      GO TO 320                                                         5098.   
  315 DT=YEAR-1985.D0                                                   5099.   
      TAU=XXTAU(N)                                                      5100.   
      DLOSS=EXP(-DT/TAU)                                                5101.   
      FLXC=XXFLX(N)/XXFLC(N)                                            5102.   
      DGAIN=FLXC*TAU*(1.D0-DLOSS)                                       5103.   
      XX1=XXF114(6)                                                     5104.   
      XX=XX1*DLOSS+DGAIN                                                5105.   
  320 CONTINUE                                                          5106.   
      XGAS(N)=XX                                                        5107.   
C                                                                       5108.   
C                                                      F115 SCENARIO    5109.   
C-------------------------------------------------------------------    5110.   
  400 N=4                                                               5111.   
      IF(YEAR.GT.1960.D0) GO TO 410                                     5112.   
      DT=(YEAR-1950.D0)/(1960.D0-1950.D0)                               5113.   
      XX=DT*XXF115(1)                                                   5114.   
      IF(XX.LT.0.D0) XX=0.D0                                            5115.   
      GO TO 420                                                         5116.   
  410 IF(YEAR.GE.1985.D0) GO TO 415                                     5117.   
      I=YEAR/5.D0                                                       5118.   
      DT=(YEAR-I*5)/5.D0                                                5119.   
      I=I-391                                                           5120.   
      XX=XXF115(I)*(XXF115(I+1)/XXF115(I))**DT                          5121.   
      GO TO 420                                                         5122.   
  415 DT=YEAR-1985.D0                                                   5123.   
      TAU=XXTAU(N)                                                      5124.   
      DLOSS=EXP(-DT/TAU)                                                5125.   
      FLXC=XXFLX(N)/XXFLC(N)                                            5126.   
      DGAIN=FLXC*TAU*(1.D0-DLOSS)                                       5127.   
      XX1=XXF115(6)                                                     5128.   
      XX=XX1*DLOSS+DGAIN                                                5129.   
  420 CONTINUE                                                          5130.   
      XGAS(N)=XX                                                        5131.   
C                                                                       5132.   
C                                                      CCL4 SCENARIO    5133.   
C-------------------------------------------------------------------    5134.   
  500 N=5                                                               5135.   
      IF(YEAR.GT.1960.D0) GO TO 510                                     5136.   
      DT=(YEAR-1885.D0)/(1960.D0-1885.D0)                               5137.   
      XX=DT*XXCCL(1)                                                    5138.   
      IF(XX.LT.0.D0) XX=0.D0                                            5139.   
      GO TO 520                                                         5140.   
  510 IF(YEAR.GE.1985.D0) GO TO 515                                     5141.   
      I=YEAR/5.D0                                                       5142.   
      DT=(YEAR-I*5)/5.D0                                                5143.   
      I=I-391                                                           5144.   
      XX=XXCCL(I)*(XXCCL(I+1)/XXCCL(I))**DT                             5145.   
      GO TO 520                                                         5146.   
  515 DT=YEAR-1985.D0                                                   5147.   
      XX=XXCCL(6)+DT/1000.D0                                            5148.   
  520 CONTINUE                                                          5149.   
      XGAS(N)=XX                                                        5150.   
C                                                                       5151.   
      RETURN                                                            5152.   
      END                                                               5153.   
      SUBROUTINE ZTREND(XGAS,YEAR,NGAS)                                 6001.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         6002.   
C                                                                       6003.   
C-----------------------------------------------------------------------6004.   
C     T-GAS SCENARIO  Z:   CFC TRACE GAS INCREASES                      6005.   
C                                                                       6006.   
C                   FXX:   OBSERVED TREND THRU 1985,                    6007.   
C                                                                       6008.   
C                                                                       6009.   
C                  FXXX:   OBSERVED TREND THRU 1985,                    6010.   
C                                                                       6011.   
C                                                                       6012.   
C                  FXXX:   OBSERVED TREND THRU 1985,                    6013.   
C                                                                       6014.   
C                                                                       6015.   
C                  CFXX:   OBSERVED TREND THRU 1985,                    6016.   
C                                                                       6017.   
C                                                                       6018.   
C                                                                       6019.   
C-----------------------------------------------------------------------6020.   
C                                                                       6021.   
      DIMENSION XGAS(1)                                                 6022.   
      DIMENSION XX1980(5),XX2030(5)                                     6023.   
      DATA XX1980/0.070, 0.004, 0.007, 0.030, 0.140/                    6024.   
      DATA XX2030/0.240, 0.020, 0.060, 0.200, 1.500/                    6025.   
C                                                                       6026.   
      DO 100 N=1,NGAS                                                   6027.   
      DT=(YEAR-1980.D0)/(2030.D0-1980.D0)                               6028.   
      XX=XX1980(N)*(XX2030(N)/XX1980(N))**DT                            6029.   
  100 XGAS(N)=XX                                                        6030.   
C                                                                       6031.   
      RETURN                                                            6032.   
      END                                                               6033.   
      SUBROUTINE DTDX1D(XNOW,XREF,XDT0,SDT0,KFOR,NFOR)                  7101.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         7102.   
      DIMENSION XNOW(5),XREF(5),XDT0(5),KFOR(5)                         7103.   
C     FCO2(X)=LOG(1.0+1.3365*X+9.29E-03*X*X+2.1766E-06*X**3)            7104.   
C     FFNC(X,Y)=1.54*LOG(1.0+1.11*X**0.77) - 0.068*LOG(1.0+X*Y)         7105.   
C    +          +0.231*Y**0.65/(1.0+0.113*Y**0.71)                      7106.   
C                                                                       7107.   
      FCO2(X)=LOG(1.D0+0.942D0*X/(1.D0+6.2D-04*X)+8.8D-03*X*X           7108.   
     +        +3.26D-06*X**3+0.156D0*X**1.3D0*EXP(-X/760.D0))           7109.   
C                                                                       7110.   
      FFNC(X,Y)=1.556D0*LOG(1.D0+1.098D0*X**0.77D0*(1.D0+0.032D0*X)     7111.   
     +                                       /(1.D0+0.0014D0*X*X))      7112.   
     +          +(0.394D0*Y**0.66D0+0.16D0*Y*EXP(-1.6D0*Y))             7113.   
     +                         /(1.D0+0.169D0*Y**0.62D0)                7114.   
     +   -0.14D0*LOG(1.D0+0.636D0*(X*Y)**0.75D0+0.007D0*Y*(X*Y)**1.52D0)7115.   
C                                                                       7116.   
      SUM=0.D0                                                          7117.   
      DO 200 K=1,NFOR                                                   7118.   
      XDT0(K)=0.D0                                                      7119.   
      XX=XNOW(K)                                                        7120.   
      XR=XREF(K)                                                        7121.   
      IF(KFOR(K).EQ.0) GO TO 200                                        7122.   
      GO TO (110,120,130,140,150),K                                     7123.   
      GO TO 200                                                         7124.   
 110  XDTX=FCO2(XX)                                                     7125.   
      XDTR=FCO2(XR)                                                     7126.   
      GO TO 190                                                         7127.   
 120  YY=XNOW(3)                                                        7128.   
      YR=XREF(3)                                                        7129.   
      XDTX=FFNC(XX,YR)                                                  7130.   
      XDTR=FFNC(XR,YR)                                                  7131.   
      GO TO 190                                                         7132.   
 130  XR=XREF(2)                                                        7133.   
      YY=XNOW(3)                                                        7134.   
      YR=XREF(3)                                                        7135.   
      XDTX=FFNC(XR,YY)                                                  7136.   
      XDTR=FFNC(XR,YR)                                                  7137.   
      GO TO 190                                                         7138.   
 140  XX=XNOW(4)                                                        7139.   
      XR=XREF(4)                                                        7140.   
      XDTX=0.066D0*XX                                                   7141.   
      XDTR=0.066D0*XR                                                   7142.   
      GO TO 190                                                         7143.   
 150  XX=XNOW(5)                                                        7144.   
      XR=XREF(5)                                                        7145.   
      XDTX=0.084D0*XX                                                   7146.   
      XDTR=0.084D0*XR                                                   7147.   
      GO TO 190                                                         7148.   
 190  XDT0(K)=XDTX-XDTR                                                 7149.   
      SUM=SUM+XDT0(K)                                                   7150.   
 200  CONTINUE                                                          7151.   
      SDT0=SUM                                                          7152.   
      RETURN                                                            7153.   
      END                                                               7154.   
      SUBROUTINE DXDT1D(XNOW,XREF,XDT0,SDT0,KFOR,NFOR)                  7201.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         7202.   
      DIMENSION XNOW(5),XREF(5),XDT0(5),KFOR(5)                         7203.   
C     FCO2(X)=LOG(1.0+1.3365*X+9.29E-03*X*X+2.1766E-06*X**3)            7204.   
C     FFNC(X,Y)=1.54*LOG(1.0+1.11*X**0.77) - 0.068*LOG(1.0+X*Y)         7205.   
C    +          +0.231*Y**0.65/(1.0+0.113*Y**0.71)                      7206.   
C                                                                       7207.   
      FCO2(X)=LOG(1.D0+0.942D0*X/(1.D0+6.2D-04*X)+8.8D-03*X*X           7208.   
     +        +3.26D-06*X**3+0.156D0*X**1.3D0*EXP(-X/760.D0))           7209.   
C                                                                       7210.   
      FFNC(X,Y)=1.556D0*LOG(1.D0+1.098D0*X**0.77D0*(1.D0+0.032D0*X)     7211.   
     +                                       /(1.D0+0.0014D0*X*X))      7212.   
     +          +(0.394D0*Y**0.66D0+0.16D0*Y*EXP(-1.6D0*Y))             7213.   
     +                         /(1.D0+0.169D0*Y**0.62D0)                7214.   
     +   -0.14D0*LOG(1.D0+0.636D0*(X*Y)**0.75D0+0.007D0*Y*(X*Y)**1.52D0)7215.   
C                                                                       7216.   
      DO 200 K=1,NFOR                                                   7217.   
      XN=XNOW(K)                                                        7218.   
      XR=XREF(K)                                                        7219.   
      ITER=0                                                            7220.   
      DTK=XDT0(K)                                                       7221.   
      IF(ABS(XN-XR).LT.1.D-05) GO TO 200                                7222.   
      IF(KFOR(K).EQ.0) GO TO 200                                        7223.   
      GO TO (110,120,130,140,150),K                                     7224.   
      GO TO 200                                                         7225.   
 110  TR=FCO2(XR)                                                       7226.   
 111  TN=FCO2(XN)                                                       7227.   
      DTNR=TN-TR                                                        7228.   
      ITER=ITER+1                                                       7229.   
      SLOPE=DTNR/(XN-XR)                                                7230.   
      DT=DTK-DTNR                                                       7231.   
      DX=DT/SLOPE                                                       7232.   
      XN=XN+DX                                                          7233.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 111                    7234.   
      GO TO 190                                                         7235.   
  120 YR=XREF(3)                                                        7236.   
      TR=FFNC(XR,YR)                                                    7237.   
  121 TN=FFNC(XN,YR)                                                    7238.   
      DTNR=TN-TR                                                        7239.   
      ITER=ITER+1                                                       7240.   
      SLOPE=DTNR/(XN-XR)                                                7241.   
      DT=DTK-DTNR                                                       7242.   
      DX=DT/SLOPE                                                       7243.   
      XN=XN+DX                                                          7244.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 121                    7245.   
      GO TO 190                                                         7246.   
 130  XR=XREF(2)                                                        7247.   
      YN=XNOW(3)                                                        7248.   
      YR=XREF(3)                                                        7249.   
      TR=FFNC(XR,YR)                                                    7250.   
  131 TN=FFNC(XR,YN)                                                    7251.   
      DTNR=TN-TR                                                        7252.   
      ITER=ITER+1                                                       7253.   
      SLOPE=DTNR/(YN-YR)                                                7254.   
      DT=DTK-DTNR                                                       7255.   
      DY=DT/SLOPE                                                       7256.   
      YN=YN+DY                                                          7257.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 131                    7258.   
      XN=YN                                                             7259.   
      GO TO 190                                                         7260.   
 140  XN=XNOW(4)                                                        7261.   
      XR=XREF(4)                                                        7262.   
  141 TN=0.066D0*XN                                                     7263.   
      TR=0.066D0*XR                                                     7264.   
      DTNR=TN-TR                                                        7265.   
      ITER=ITER+1                                                       7266.   
      SLOPE=DTNR/(XN-XR)                                                7267.   
      DT=DTK-DTNR                                                       7268.   
      DX=DT/SLOPE                                                       7269.   
      XN=XN+DX                                                          7270.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 141                    7271.   
      TR=0.066D0*XR                                                     7272.   
      GO TO 190                                                         7273.   
 150  XN=XNOW(5)                                                        7274.   
      XR=XREF(5)                                                        7275.   
      TR=0.084D0*XR                                                     7276.   
  151 TN=0.084D0*XN                                                     7277.   
      DTNR=TN-TR                                                        7278.   
      ITER=ITER+1                                                       7279.   
      SLOPE=DTNR/(XN-XR)                                                7280.   
      DT=DTK-DTNR                                                       7281.   
      DX=DT/SLOPE                                                       7282.   
      XN=XN+DX                                                          7283.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 151                    7284.   
      GO TO 190                                                         7285.   
 190  XNOW(K)=XN                                                        7286.   
 200  CONTINUE                                                          7287.   
      RETURN                                                            7288.   
      END                                                               7289.   
      SUBROUTINE DXDT3D(XNOW,XREF,XDT0,SDT0,KFOR,NFOR)                  7301.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         7302.   
      DIMENSION XNOW(5),XREF(5),XDT0(5),KFOR(5)                         7303.   
C     FCO2(X)=LOG(1.D0+ 0.71*X**1.1 + 0.0084*X**1.9 + 2.8E-07*X**3.3)   7304.   
      FCO2(X)=LOG(1.D0+ 1.2D0*X + 0.005D0*X**2 + 1.4D-06*X**3)          7305.   
      FFNC(X,Y)=3.1515D0*X/(1.D0+0.21448D0*X**0.87D0) - 0.056D0*X*Y     7306.   
     +          +0.3234D0*Y/(1.D0+0.052D0*Y**0.84D0)                    7307.   
C                                                                       7308.   
      DO 200 K=1,NFOR                                                   7309.   
      XN=XNOW(K)                                                        7310.   
      XR=XREF(K)                                                        7311.   
      ITER=0                                                            7312.   
      DTK=XDT0(K)                                                       7313.   
      IF(ABS(XN-XR).LT.1.D-05) GO TO 200                                7314.   
      IF(KFOR(K).EQ.0) GO TO 200                                        7315.   
      GO TO (110,120,130,140,150),K                                     7316.   
      GO TO 200                                                         7317.   
 110  TR=FCO2(XR)                                                       7318.   
 111  TN=FCO2(XN)                                                       7319.   
      DTNR=TN-TR                                                        7320.   
      ITER=ITER+1                                                       7321.   
      SLOPE=DTNR/(XN-XR)                                                7322.   
      DT=DTK-DTNR                                                       7323.   
      DX=DT/SLOPE                                                       7324.   
      XN=XN+DX                                                          7325.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 111                    7326.   
      GO TO 190                                                         7327.   
  120 YR=XREF(3)                                                        7328.   
      TR=FFNC(XR,YR)                                                    7329.   
  121 TN=FFNC(XN,YR)                                                    7330.   
      DTNR=TN-TR                                                        7331.   
      ITER=ITER+1                                                       7332.   
      SLOPE=DTNR/(XN-XR)                                                7333.   
      DT=DTK-DTNR                                                       7334.   
      DX=DT/SLOPE                                                       7335.   
      XN=XN+DX                                                          7336.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 121                    7337.   
      GO TO 190                                                         7338.   
 130  XR=XREF(2)                                                        7339.   
      YN=XNOW(3)                                                        7340.   
      YR=XREF(3)                                                        7341.   
      TR=FFNC(XR,YR)                                                    7342.   
  131 TN=FFNC(XR,YN)                                                    7343.   
      DTNR=TN-TR                                                        7344.   
      ITER=ITER+1                                                       7345.   
      SLOPE=DTNR/(YN-YR)                                                7346.   
      DT=DTK-DTNR                                                       7347.   
      DY=DT/SLOPE                                                       7348.   
      YN=YN+DY                                                          7349.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 131                    7350.   
      XN=YN                                                             7351.   
      GO TO 190                                                         7352.   
 140  XN=XNOW(4)                                                        7353.   
      XR=XREF(4)                                                        7354.   
  141 TN=0.066D0*XN                                                     7355.   
      TR=0.066D0*XR                                                     7356.   
      DTNR=TN-TR                                                        7357.   
      ITER=ITER+1                                                       7358.   
      SLOPE=DTNR/(XN-XR)                                                7359.   
      DT=DTK-DTNR                                                       7360.   
      DX=DT/SLOPE                                                       7361.   
      XN=XN+DX                                                          7362.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 141                    7363.   
      TR=0.066D0*XR                                                     7364.   
      GO TO 190                                                         7365.   
 150  XN=XNOW(5)                                                        7366.   
      XR=XREF(5)                                                        7367.   
      TR=0.084D0*XR                                                     7368.   
  151 TN=0.084D0*XN                                                     7369.   
      DTNR=TN-TR                                                        7370.   
      ITER=ITER+1                                                       7371.   
      SLOPE=DTNR/(XN-XR)                                                7372.   
      DT=DTK-DTNR                                                       7373.   
      DX=DT/SLOPE                                                       7374.   
      XN=XN+DX                                                          7375.   
      IF(ITER.LT.15.AND.ABS(DT).GT.1.D-05) GO TO 151                    7376.   
      GO TO 190                                                         7377.   
 190  XNOW(K)=XN                                                        7378.   
 200  CONTINUE                                                          7379.   
      RETURN                                                            7380.   
      END                                                               7381.   
      SUBROUTINE DTDX3D(XNOW,XREF,XDT0,SDT0,KFOR,NFOR)                  7401.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         7402.   
      DIMENSION XNOW(5),XREF(5),XDT0(5),KFOR(5)                         7403.   
C     FCO2(X)=LOG(1.D0+ 0.71*X**1.1 + 0.0084*X**1.9 + 2.8E-07*X**3.3)   7404.   
C     FCO2(X)=LOG(1.0+1.018*X+6.05E-03*X*X+0.6122E-06*X**3)             7405.   
      FCO2(X)=LOG(1.D0+ 1.2D0*X + 0.005D0*X**2 + 1.4D-06*X**3)          7406.   
      FFNC(X,Y)=3.1515D0*X/(1.D0+0.21448D0*X**0.87D0) - 0.056D0*X*Y     7407.   
     +          +0.3234D0*Y/(1.D0+0.052D0*Y**0.84D0)                    7408.   
C                                                                       7409.   
      SUM=0.D0                                                          7410.   
      DO 200 K=1,NFOR                                                   7411.   
      XDT0(K)=0.D0                                                      7412.   
      IF(KFOR(K).EQ.0) GO TO 200                                        7413.   
      GO TO (110,120,130,140,150),K                                     7414.   
      GO TO 200                                                         7415.   
 110  XX=XNOW(1)                                                        7416.   
      XR=XREF(1)                                                        7417.   
      XDTX=FCO2(XX)                                                     7418.   
      XDTR=FCO2(XR)                                                     7419.   
      GO TO 190                                                         7420.   
 120  XX=XNOW(2)                                                        7421.   
      XR=XREF(2)                                                        7422.   
      YR=XREF(3)                                                        7423.   
      XDTX=FFNC(XX,YR)                                                  7424.   
      XDTR=FFNC(XR,YR)                                                  7425.   
      GO TO 190                                                         7426.   
 130  XR=XREF(2)                                                        7427.   
      YY=XNOW(3)                                                        7428.   
      YR=XREF(3)                                                        7429.   
      XDTX=FFNC(XR,YY)                                                  7430.   
      XDTR=FFNC(XR,YR)                                                  7431.   
      GO TO 190                                                         7432.   
 140  XX=XNOW(4)                                                        7433.   
      XR=XREF(4)                                                        7434.   
      XDTX=0.066D0*XX                                                   7435.   
      XDTR=0.066D0*XR                                                   7436.   
      GO TO 190                                                         7437.   
 150  XX=XNOW(5)                                                        7438.   
      XR=XREF(5)                                                        7439.   
      XDTX=0.084D0*XX                                                   7440.   
      XDTR=0.084D0*XR                                                   7441.   
      GO TO 190                                                         7442.   
 190  XDT0(K)=XDTX-XDTR                                                 7443.   
      SUM=SUM+XDT0(K)                                                   7444.   
 200  CONTINUE                                                          7445.   
      SDT0=SUM                                                          7446.   
      RETURN                                                            7447.   
      END                                                               7448.   
      SUBROUTINE DTXCFY(DTSUM,DTNR,XNOW,XREF,NGAS)                      7501.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         7502.   
      DIMENSION DTNR(1),XNOW(1),XREF(1)                                 7503.   
      DIMENSION FGAS(5)                                                 7504.   
      DATA FGAS/                                                        7505.   
     +          0.0565,0.1160,0.1160,0.1200,0.044/                      7506.   
C                                                                       7507.   
      DO 100 N=1,NGAS                                                   7508.   
  100 DTNR(N)=FGAS(N)*(XNOW(N)-XREF(N))                                 7509.   
C                                                                       7510.   
      DTSUM=0.D0                                                        7511.   
      DO 200 N=1,NGAS                                                   7512.   
  200 DTSUM=DTSUM+DTNR(N)                                               7513.   
C                                                                       7514.   
      RETURN                                                            7515.   
      END                                                               7516.   
      SUBROUTINE DTXCFZ(DTSUM,DTNR,XNOW,XREF,NGAS)                      7601.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         7602.   
      DIMENSION DTNR(1),XNOW(1),XREF(1)                                 7603.   
      DIMENSION FGAS(5)                                                 7604.   
      DATA FGAS/                                                        7605.   
     +          0.045, 0.068, 0.116, 0.013, 0.004/                      7606.   
C                                                                       7607.   
      DO 100 N=1,NGAS                                                   7608.   
  100 DTNR(N)=FGAS(N)*(XNOW(N)-XREF(N))                                 7609.   
C                                                                       7610.   
      DTSUM=0.D0                                                        7611.   
      DO 200 N=1,NGAS                                                   7612.   
  200 DTSUM=DTSUM+DTNR(N)                                               7613.   
C                                                                       7614.   
      RETURN                                                            7615.   
      END                                                               7616.   
      SUBROUTINE VMSSET(KDTAUA,KWRITE)                                  8001.   
      INCLUDE 'B83XXDBL.COM'                                            8002.   
      REAL*4 TAUA(1800,27)                                              8003.   
      DIMENSION YRBVOL(10),FL(12),FS(12)                                8004.   
      DATA YRBVOL/1856.041,1883.625,1886.458,1888.458,1892.458          8005.   
     +           ,1902.458,1912.458,1963.208,1982.208,1991.458/         8006.   
C                                                                       8007.   
C                                                                       8008.   
      AGOLDH(1,1)=0.0                                                   8009.   
C                                                                       8010.   
      NTRACE=2                                                          8011.   
      ITR(1)=2                                                          8012.   
      ITR(2)=10                                                         8013.   
      FGOLDH(6)=1.0                                                     8014.   
      FGOLDH(7)=1.0                                                     8015.   
C                                                                       8016.   
      KD=KDTAUA                                                         8017.   
      REWIND KD                                                         8018.   
      DO 120 IY=1850,1999                                               8019.   
      IYW=IY/2-(IY-1)/2                                                 8020.   
      IF(KWRITE.EQ.1) THEN                                              8021.   
      IF(IYW.EQ.1) WRITE(6,6000)                                        8022.   
 6000 FORMAT(1H1,' YR JLAT  JAN   FEB   MAR   APR   MAY   JUN   JUL'    8023.   
     +          ,'   AUG   SEP   OCT   NOV   DEC')                      8024.   
      IF(IYW.NE.1) WRITE(6,6010)                                        8025.   
 6010 FORMAT(1H ,' YR JLAT  JAN   FEB   MAR   APR   MAY   JUN   JUL'    8026.   
     +          ,'   AUG   SEP   OCT   NOV   DEC')                      8027.   
      ENDIF                                                             8028.   
      M1=12*(IY-1850)+1                                                 8029.   
      M12=M1+11                                                         8030.   
      DO 110 JI=1,27                                                    8031.   
      J=28-JI                                                           8032.   
      IF(JI.EQ.1) READ(KD,5000) IYEAR,JJ,(TAUA(M,J),M=M1,M12)           8033.   
      IF(JI.NE.1) READ(KD,5010)       JJ,(TAUA(M,J),M=M1,M12)           8034.   
C                                                                       8035.   
      IF(KWRITE.EQ.1) THEN                                              8036.   
      IF(JI.EQ.1) WRITE(6,5000) IYEAR,JJ,(TAUA(M,J),M=M1,M12)           8037.   
      IF(JI.NE.1) WRITE(6,5010)       JJ,(TAUA(M,J),M=M1,M12)           8038.   
 5000 FORMAT(I5,I3,12(1X,F5.4))                                         8039.   
 5010 FORMAT(5X,I3,12(1X,F5.4))                                         8040.   
      ENDIF                                                             8041.   
  110 CONTINUE                                                          8042.   
C                                                                       8043.   
      IF(KWRITE.NE.1) GO TO 120                                         8044.   
      DO 114 MM=1,12                                                    8045.   
      XYR=IY+(MM-0.5)/12.0                                              8046.   
      FSMALL=1.0                                                        8047.   
      FLARGE=0.0                                                        8048.   
      DYRVOL=XYR-YRBVOL(1)                                              8049.   
      IF(DYRVOL.LT.0.0) GO TO 113                                       8050.   
      DO 111 I=2,10                                                     8051.   
      IF(XYR.LT.YRBVOL(I)) GO TO 112                                    8052.   
      DYRVOL=XYR-YRBVOL(I)                                              8053.   
  111 CONTINUE                                                          8054.   
C     GO TO 113                                                         8055.   
  112 FLARGE=1.0-2.0*DYRVOL                                             8056.   
      IF(FLARGE.LT.0.0) FLARGE=0.0                                      8057.   
      FSMALL=1.0-FLARGE                                                 8058.   
  113 CONTINUE                                                          8059.   
      FL(MM)=FLARGE                                                     8060.   
      FS(MM)=FSMALL                                                     8061.   
  114 CONTINUE                                                          8062.   
      WRITE(6,6100) (FL(MM),MM=1,12)                                    8063.   
 6100 FORMAT(/1X,'FLARGE=',12F6.3)                                      8064.   
      WRITE(6,6200) (FS(MM),MM=1,12)                                    8065.   
 6200 FORMAT(1X,'FSMALL=',12F6.3//)                                     8066.   
  120 CONTINUE                                                          8067.   
      REWIND KD                                                         8068.   
C                                                                       8069.   
      RETURN                                                            8070.   
C                                                                       8071.   
C------------------------------                                         8072.   
      ENTRY VMSGET(TNOW,KWRITE)                                         8073.   
C------------------------------                                         8074.   
C                                                                       8075.   
      YMI=12.0*(TNOW-1850.0)+0.50001                                    8076.   
      IF(YMI.LT.1.0001) YMI=1.0001                                      8077.   
      IF(YMI.GE.1800.-.0001) YMI=1800.-.0001                            8078.   
      IMY=YMI                                                           8079.   
      DMY=YMI-IMY                                                       8080.   
      VOLTAU=(1.0-DMY)*TAUA(IMY,JLAT)+DMY*TAUA(IMY+1,JLAT)              8081.   
C                                                                       8082.   
      FSMALL=1.0                                                        8083.   
      FLARGE=0.0                                                        8084.   
      DYRVOL=TNOW-YRBVOL(1)                                             8085.   
      IF(DYRVOL.LT.0.0) GO TO 230                                       8086.   
      DO 210 I=2,10                                                     8087.   
      IF(TNOW.LT.YRBVOL(I)) GO TO 220                                   8088.   
      DYRVOL=TNOW-YRBVOL(I)                                             8089.   
  210 CONTINUE                                                          8090.   
  220 FLARGE=1.0-2.0*DYRVOL                                             8091.   
      IF(FLARGE.LT.0.0) FLARGE=0.0                                      8092.   
      FSMALL=1.0-FLARGE                                                 8093.   
  230 CONTINUE                                                          8094.   
      TAU189=0.5*VOLTAU*FSMALL                                          8095.   
      TAU289=0.5*VOLTAU*FLARGE                                          8096.   
C                                                                       8097.   
      DO 240 I=1,NL                                                     8098.   
      TRACER(I,1)=0.0                                                   8099.   
      TRACER(I,2)=0.0                                                   8100.   
  240 CONTINUE                                                          8101.   
      TRACER(8,1)=TAU189                                                8102.   
      TRACER(9,1)=TAU189                                                8103.   
      TRACER(8,2)=TAU289                                                8104.   
      TRACER(9,2)=TAU289                                                8105.   
C                                                                       8106.   
      IF(KWRITE.EQ.1) WRITE(6,6300)                                     8107.   
     +   TNOW,LATJ,VOLTAU,FGOLDH(6),FGOLDH(7),NTRACE,ITR(1),ITR(2)      8108.   
     +   ,(TRACER(I,1),I=1,12),(TRACER(L,2),L=1,12),FSMALL,FLARGE       8109.   
 6300 FORMAT(1X,F8.3,I3,1X,F6.5,2F4.1,1X,3I2,1X,7(1X,F2.1),1X,F5.4      8110.   
     +         ,1X,F5.4,3(1X,F2.1),1X,7(1X,F2.1),1X,F5.4                8111.   
     +         ,1X,F5.4,3(1X,F2.1),1X,2F6.3)                            8112.   
C                                                                       8113.   
      RETURN                                                            8114.   
      END                                                               8115.   
