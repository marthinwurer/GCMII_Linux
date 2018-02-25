      SUBROUTINE CONDSE                                                 2001.   
C****                                                                   2002.   
C**** THIS SUBROUTINE MIXES AIR CAUSED BY MOIST CONVECTION, AND         2003.   
C**** CALCULATES PRECIPITATION AND CLOUD COVER DUE TO CONVECTIVE        2004.   
C**** AND SUPER SATURATION CLOUDS.                                      2005.   
C****                                                                   2005.5  
      INCLUDE 'BA94jalC9.COM'                                           2006.   
      REAL*8 LHX,LHXUP,MPLUME,MCLOUD                                    2008.   
      COMMON U,V,T,P,Q                                                  2009.   
      COMMON/WORK1/CONV(IM,JM,LM),PK(IM,JM,LM),PREC(IM,JM),             2010.   
     *  TPREC(IM,JM),UC(IM,JM,LM),VC(IM,JM,LM)                          2011.   
      COMMON/WORK2/CLDSS(IM,JM,LM),CLDMC(IM,JM,LM),                     2012.   
     *  SIGMA1(JM,LM),PL(LM),PLK(LM),TL(LM),QL(LM),                     2013.   
     *  SM(LM),QM(LM),DSM(LM),DQM(LM),DM(LM),AIRM(LM),                  2014.   
     *  COND(LM),CDHEAT(LM),SMOLD(LM),QMOLD(LM),CM(LM),                 2015.   
     *  ID(2*IM),RA(2*IM),AJ8(LM),AJ13(LM),AJ50(LM),AJ51(LM),           2016.   
     *  X(IM),UM(2*IM,LM),DUM(2*IM,LM),UMP(2*IM)                        2017.   
      DIMENSION DSG0(LM)                                                2018.   
      LOGICAL POLE                                                      2019.   
C     DATA RVAP/461.5/                                                  2020.   
      DATA TF/273.16/,TI/233.16/,IFIRST/1/                              2021.   
      QSAT(TM,PR)=.622*EXP(AXCONS+LHX*BXCONS*(BYTF-1./TM))/PR           2022.   
      ERFCPI(XY)=.5-XY*(.548-XY*XY*(.139-.0171*XY*XY))                  2023.   
C****                                                                   2024.   
C**** FDATA  2  LAND COVERAGE (1)                                       2025.   
C**** ODATA  2  RATIO OF OCEAN ICE COVERAGE TO WATER COVERAGE (1)       2026.   
C**** GDATA 11  AGE OF SNOW (DAYS)                                      2027.   
C****                                                                   2028.   
C**** COMPUTE GLOBAL PARAMETERS                                         2029.   
      IDACC(1)=IDACC(1)+1                                               2030.   
      IF(IFIRST.NE.1) GO TO 50                                          2031.   
         JEQ=JM/2+1                                                     2032.   
      DTCNDS=NCNDS*DT                                                   2033.   
      SHA=RGAS/KAPA                                                     2034.   
      BXCONS=.622/RGAS                                                  2035.   
      AXCONS=LOG(6.1071D0)                                              2036.   
      SLHE=LHE/SHA                                                      2037.   
      SLHS=LHS/SHA                                                      2038.   
      BYTF=1./TF                                                        2039.   
      DTPERD=DTCNDS/SDAY                                                2040.   
      AGESNX=1.-DTPERD/50.                                              2041.   
      BX=CCMCX/DTCNDS                                                   2042.   
      SDSIG=0.                                                          2043.   
      DO 30 L=1,LM                                                      2044.   
      SDSIG=SDSIG+DSIG(L)                                               2045.   
   30 DSG0(L)=SDSIG/L                                                   2046.   
C**** PARAMETERS FOR STANDARD DEVIATION OF TEMPERATURE                  2047.   
      IQ1=IM/4+1                                                        2048.   
      IQ2=IM/2+1                                                        2049.   
      IMBY2=IM/2                                                        2050.   
      NMAX=MIN(IMBY2,17)                                                2051.   
      NMIN=MIN(IQ1,7)                                                   2052.   
      BYDELN=1./(NMAX+1-NMIN)                                           2053.   
      SL1=0.                                                            2054.   
      SL4=0.                                                            2055.   
      DO 40 N=NMIN,NMAX                                                 2056.   
      ALOGN=LOG(DFLOAT(N))                                              2057.   
      SL1=SL1+ALOGN                                                     2058.   
   40 SL4=SL4+ALOGN*ALOGN                                               2059.   
      SL4=SL4-SL1*SL1*BYDELN                                            2060.   
      SL1=SL1*BYDELN                                                    2061.   
   50 IFIRST=0                                                          2062.   
C**** CALCULATE PK = P**KAPA                                            2063.   
      IF(DOPK.NE.1.) GO TO 60                                           2064.   
      DO 55 J=1,JM                                                      2065.   
      DO 55 I=1,IM                                                      2066.   
      PIJ=P(I,J)                                                        2067.   
      DO 55 L=1,LM                                                      2068.   
   55 PK(I,J,L)=EXPBYK(SIG(L)*PIJ+PTOP)                                 2069.   
      DOPK=0.                                                           2070.   
C**** SAVE UC AND VC, AND ZERO OUT CLDSS AND CLDMC                      2071.   
   60 DO 65 L=1,LM                                                      2072.   
      DO 65 J=1,JM                                                      2073.   
      DO 65 I=1,IM                                                      2074.   
      UC(I,J,L)=U(I,J,L)                                                2075.   
      VC(I,J,L)=V(I,J,L)                                                2076.   
      CLDMC(I,J,L)=0.                                                   2077.   
   65 CLDSS(I,J,L)=0.                                                   2078.   
         IHOUR=1.5+TOFDAY                                               2079.   
C****                                                                   2080.   
C**** MAIN J LOOP                                                       2081.   
C****                                                                   2082.   
      DO 810 J=1,JM                                                     2083.   
      IF ((J-1)*(JM-J).NE.0) GO TO 90                                   2084.   
C**** CONDITIONS AT THE POLES                                           2085.   
      POLE=.TRUE.                                                       2086.   
      IMAX=1                                                            2087.   
      IF(J.EQ.JM) GO TO 75                                              2088.   
      JP=2                                                              2089.   
      JVPO=2                                                            2090.   
      RAPO=2.*RAPVN(1)                                                  2091.   
      GO TO 80                                                          2092.   
   75 JP=JM-1                                                           2093.   
      JVPO=JM                                                           2094.   
      RAPO=2.*RAPVS(JM)                                                 2095.   
   80 KMAX=2*IM                                                         2096.   
      DO 85 I=1,IM                                                      2097.   
      RA(I)=RAPO                                                        2098.   
      RA(I+IM)=RAPO                                                     2099.   
      ID(I)=I+(JVPO-1)*IM                                               2100.   
   85 ID(I+IM)=I+(JVPO-1)*IM+IM*JM*LM                                   2101.   
      GO TO 110                                                         2102.   
C**** CONDITIONS AT NON-POLAR POINTS                                    2103.   
   90 POLE=.FALSE.                                                      2104.   
      JP=J                                                              2105.   
      IMAX=IM                                                           2106.   
      KMAX=8                                                            2107.   
      DO 100 K=1,4                                                      2108.   
      RA(K)=RAPVS(J)                                                    2109.   
  100 RA(K+4)=RAPVN(J)                                                  2110.   
C**** STANDARD DEVIATION FOR TEMPERATURE                                2111.   
  110 DO 120 L=1,LM                                                     2112.   
      TVAR=0.                                                           2113.   
      SUMT=0.                                                           2114.   
      PKJ=0.                                                            2115.   
      DO 111 I=1,IM                                                     2116.   
      PKJ=PKJ+PK(I,JP,L)                                                2117.   
  111 SUMT=SUMT+T(I,JP,L)                                               2118.   
      DO 112 I=1,IM                                                     2119.   
      TDEV=T(I,JP,L)-SUMT/FIM                                           2120.   
      X(I)=TDEV                                                         2121.   
  112 TVAR=TVAR+TDEV*TDEV                                               2122.   
      TVAR=TVAR/FIM                                                     2123.   
      CALL FRTR(X)                                                      2124.   
      SL2=0.                                                            2125.   
      SL3=0.                                                            2126.   
      DO 113 N=NMIN,NMAX                                                2127.   
      ALOGA=LOG(X(N)+1.D-20)                                            2128.   
      SL2=SL2+ALOGA                                                     2129.   
  113 SL3=SL3+ALOGA*LOG(DFLOAT(N))                                      2130.   
      SLOPE=(SL1*SL2-SL3)/SL4                                           2131.   
      IF (SLOPE.LT.1.67) SLOPE=1.67                                     2132.   
      IF (SLOPE.GT.3.) SLOPE=3.                                         2133.   
      SUMXN=0.                                                          2134.   
      DO 114 N=1,IMBY2                                                  2135.   
  114 SUMXN=SUMXN+X(N)                                                  2136.   
      SUMAMK=0.                                                         2137.   
      DO 115 N=NMIN,NMAX                                                2138.   
  115 SUMAMK=SUMAMK+X(N)*(N**SLOPE)                                     2139.   
      SLOPM1=SLOPE-1.                                                   2140.   
      XEPE=2.*SUMAMK*BYDELN/((SUMXN+1.D-20)*SLOPM1*(IQ2**SLOPM1))       2141.   
      SIGMA1(J,L)=SQRT(XEPE*TVAR)*PKJ/FIM                               2142.   
  120    AJL(J,L,54)=AJL(J,L,54)+SIGMA1(J,L)                            2143.   
C**** ZERO OUT DIAGNOSTIC QUANTITIES FOR THE BUDGET PAGES (DIAGJ)       2144.   
         APRCPM=0.                                                      2145.   
         BPRCPM=0.                                                      2146.   
         CPRCPM=0.                                                      2147.   
         APRCPS=0.                                                      2148.   
         BPRCPS=0.                                                      2149.   
         CPRCPS=0.                                                      2150.   
C****                                                                   2151.   
C**** MAIN I LOOP                                                       2152.   
C****                                                                   2153.   
      IM1=IM                                                            2154.   
      DO 800 I=1,IMAX                                                   2155.   
         JR=JREG(I,J)                                                   2156.   
      PLAND=FDATA(I,J,2)                                                2157.   
      POICE=ODATA(I,J,2)*(1.-PLAND)                                     2158.   
      POCEAN=(1.-PLAND)-POICE                                           2159.   
      IF(POLE) GO TO 130                                                2160.   
      ID(1)=IM1+(J-1)*IM                                                2161.   
      ID(2)=ID(1)+IM*JM*LM                                              2162.   
      ID(3)=I+(J-1)*IM                                                  2163.   
      ID(4)=ID(3)+IM*JM*LM                                              2164.   
      ID(5)=IM1+J*IM                                                    2165.   
      ID(6)=ID(5)+IM*JM*LM                                              2166.   
      ID(7)=I+J*IM                                                      2167.   
      ID(8)=ID(7)+IM*JM*LM                                              2168.   
C****                                                                   2169.   
C**** SET UP VERTICAL ARRAYS, OMITTING THE J AND I SUBSCRIPTS           2170.   
C****                                                                   2171.   
  130 PIJ=P(I,J)                                                        2172.   
C**** PRESSURES, AND PRESSURE TO THE KAPA                               2173.   
      DO 150 L=1,LM                                                     2174.   
      PL(L)=SIG(L)*PIJ+PTOP                                             2175.   
      PLK(L)=PK(I,J,L)                                                  2176.   
      AIRM(L)=PIJ*DSIG(L)                                               2177.   
C**** MOISTURE (SPECIFIC HUMIDITY)                                      2178.   
      QL(L)=Q(I,J,L)                                                    2179.   
      QM(L)=Q(I,J,L)*AIRM(L)                                            2180.   
      QMOLD(L)=QM(L)                                                    2181.   
      COND(L)=0.                                                        2182.   
      CDHEAT(L)=0.                                                      2183.   
      DM(L)=0.                                                          2184.   
      DSM(L)=0.                                                         2185.   
      DQM(L)=0.                                                         2186.   
C**** SURROUNDING WINDS                                                 2187.   
      DO 140 K=1,KMAX                                                   2188.   
      DUM(K,L)=0.                                                       2189.   
  140 UM(K,L)=UC(ID(K),1,L)*AIRM(L)                                     2190.   
C**** TEMPERATURES                                                      2191.   
      TL(L)=T(I,J,L)*PLK(L)                                             2192.   
      SM(L)=T(I,J,L)*AIRM(L)                                            2193.   
      SMOLD(L)=SM(L)                                                    2194.   
         AJ8(L)=0.                                                      2195.   
         AJ13(L)=0.                                                     2196.   
         AJ50(L)=0.                                                     2197.   
         AJ51(L)=0.                                                     2198.   
  150 CONTINUE                                                          2199.   
      TPREC(I,J)=TL(1)-TF                                               2200.   
C****                                                                   2201.   
C**** MOIST CONVECTION                                                  2202.   
C****                                                                   2203.   
C**** CONVECTION OCCURS AT THE LOWEST MOIST CONVECTIVELY UNSTABLE       2204.   
C**** LEVEL AND CONTINUES UNTIL A STABLE LAYER PAIR IS REACHED.  RE-    2205.   
C**** EVAPORATION AND PRECIPITATION ARE COMPUTED AT THE END OF THIS     2206.   
C**** CYCLE.  THE WHOLE PROCESS MAY BE REPEATED FROM A NEW LOWEST       2207.   
C**** UNSTABLE LEVEL.                                                   2208.   
C****                                                                   2209.   
      PRCPMC=0.                                                         2210.   
         HCNDMC=0.                                                      2211.   
      LMCMIN=0                                                          2212.   
      LMCMAX=0                                                          2213.   
      LMIN=0                                                            2214.   
C**** OUTER MC LOOP OVER BASE LAYER                                     2215.   
  200 LMIN=LMIN+1                                                       2216.   
      IF(LMIN.GE.LMCM) GO TO 600                                        2217.   
C**** CREATE A PLUME IN THE BOTTOM LAYER                                2218.   
      LHX=LHE                                                           2219.   
      MPLUME=MIN(.5*AIRM(LMIN),.5*AIRM(LMIN+1))                         2220.   
      FPLUME=MPLUME/AIRM(LMIN)                                          2221.   
      SMP=SM(LMIN)*FPLUME                                               2222.   
      QMP=QM(LMIN)*FPLUME                                               2223.   
      DM(LMIN)=-MPLUME                                                  2224.   
      DSM(LMIN)=-SMP                                                    2225.   
      DQM(LMIN)=-QMP                                                    2226.   
      DO 210 K=1,KMAX                                                   2227.   
      UMP(K)=UM(K,LMIN)*FPLUME                                          2228.   
  210 DUM(K,LMIN)=-UMP(K)                                               2229.   
C****                                                                   2230.   
C**** RAISE THE PLUME TO THE TOP OF CONVECTION AND CALCULATE            2231.   
C**** ENTRAINMENT, CONDENSATION, AND SECONDARY MIXING                   2232.   
C****                                                                   2233.   
         CDHSUM=0.                                                      2234.   
      DO 330 LMAX=LMIN,LM-1                                             2235.   
      L=LMAX+1                                                          2236.   
C**** TEST FOR SUFFICIENT AIR, MOIST STATIC STABILITY AND ENERGY        2237.   
      IF(MPLUME.LE..001*AIRM(L)) GO TO 340                              2238.   
      SDN=SMP/MPLUME                                                    2239.   
      SUP=SM(L)/AIRM(L)                                                 2240.   
      SEDGE=THBAR(SUP,SDN)                                              2241.   
      QDN=QMP/MPLUME                                                    2242.   
      QUP=QM(L)/AIRM(L)                                                 2243.   
      LHX=LHE                                                           2244.   
      DMSE=(SUP-SEDGE)*PLK(L)+(SEDGE-SDN)*PLK(L-1)+                     2245.   
     *  SLHE*(QSAT(SUP*PLK(L),PL(L))-QDN)                               2246.   
      IF(DMSE.GT.-1.D-10) GO TO 340                                     2247.   
      IF(PLK(L-1)*(SUP-SDN)+SLHE*(QUP-QDN).GE.0.) GO TO 340             2248.   
C****                                                                   2249.   
C**** DEPOSIT PART OF THE PLUME IN LOWER LAYER                          2250.   
C****                                                                   2251.   
      DELTA=0.                                                          2252.   
      IF(MPLUME.LE.AIRM(L)) GO TO 285                                   2253.   
      DELTA=(MPLUME-AIRM(L))/MPLUME                                     2255.   
      DSM(L-1)=DELTA*SMP                                                2256.   
      DQM(L-1)=DELTA*QMP                                                2257.   
      DM(L-1)=DELTA*MPLUME                                              2258.   
      SMP=SMP-DSM(L-1)                                                  2259.   
      QMP=QMP-DQM(L-1)                                                  2260.   
      MPLUME=AIRM(L)                                                    2261.   
      DO 250 K=1,KMAX                                                   2262.   
  250 DUM(K,L-1)=UMP(K)*DELTA                                           2263.   
C****                                                                   2264.   
C**** CONVECTION IN UPPER LAYER   (WORK DONE COOLS THE PLUME)           2265.   
C****                                                                   2266.   
  285 WORK=MPLUME*(SUP-SDN)*(PLK(L-1)-PLK(L))/PLK(L)                    2267.   
      SMP=SMP-WORK                                                      2268.   
      CM(L-1)=MPLUME                                                    2269.   
C**** TEST FOR CONENSATION ALSO DETERMINES IF PLUME REACHES UPPER LAYER 2270.   
      TP=SMP*PLK(L)/MPLUME                                              2271.   
      IF(TP.LT.TI) LHX=LHS                                              2272.   
      QSATMP=MPLUME*QSAT(TP,PL(L))                                      2273.   
      IF(QMP.LT.QSATMP) GO TO 340                                       2274.   
      IF(TP.GE.TF.OR.LHX.EQ.LHS) GO TO 290                              2275.   
      LHX=LHS                                                           2276.   
      QSATMP=MPLUME*QSAT(TP,PL(L))                                      2277.   
C****                                                                   2278.   
C**** CONDENSE VAPOR IN THE PLUME AND ADD LATENT HEAT                   2279.   
C****                                                                   2280.   
  290 SLH=LHX/SHA                                                       2281.   
      GAMA=SLH*LHX*BXCONS*QSATMP/(TP*TP*MPLUME)                         2282.   
      COND(L)=(QMP-QSATMP)/(1.+GAMA)                                    2283.   
      CDHEAT(L)=SLH*COND(L)                                             2284.   
         CDHSUM=CDHSUM+CDHEAT(L)                                        2285.   
      SMP=SMP+SLH*COND(L)/PLK(L)                                        2286.   
      QMP=QMP-COND(L)                                                   2287.   
C****                                                                   2288.   
C**** UPDATE ALL QUANTITIES CARRIED BY THE PLUME                        2289.   
C****                                                                   2290.   
      SMPMAX=SMP                                                        2291.   
      QMPMAX=QMP                                                        2292.   
      MPMAX=MPLUME                                                      2293.   
      DO 300 K=1,KMAX                                                   2294.   
  300 UMP(K)=UMP(K)-UMP(K)*DELTA                                        2295.   
  330 CONTINUE                                                          2296.   
      LMAX=LM                                                           2296.5  
C**** UPDATE CHANGES CARRIED BY THE PLUME IN THE TOP CLOUD LAYER        2297.   
  340 IF(LMIN.EQ.LMAX) GO TO 200                                        2298.   
      DM(LMAX)=MPLUME                                                   2299.   
      DSM(LMAX)=SMPMAX                                                  2300.   
      DQM(LMAX)=QMPMAX                                                  2301.   
      CM(LMAX)=0.                                                       2302.   
      DO 350 K=1,KMAX                                                   2303.   
  350 DUM(K,LMAX)=UMP(K)                                                2304.   
C****                                                                   2305.   
C**** SUBSIDENCE AND MIXING                                             2306.   
C****                                                                   2307.   
      ALPHA=0.                                                          2309.   
      DO 380 L=LMIN,LMAX                                                2310.   
      BETA=CM(L)/(AIRM(L+1)+1.D-20)                                     2311.   
         AJ8(L)=AJ8(L)+CM(L)                                            2312.   
         AJ13(L)=AJ13(L)+PLK(L)*(BETA*SM(L+1)-ALPHA*SM(L)+DSM(L))       2313.   
CORIG*     -CDHEAT(L)                                                   2313.001
         AJ51(L)=AJ51(L)+(BETA*QM(L+1)-ALPHA*QM(L)+DQM(L)+COND(L))      2314.   
      SM(L)=SM(L)*(1.-ALPHA)+BETA*SM(L+1)+DSM(L)                        2315.   
      QM(L)=QM(L)*(1.-ALPHA)+BETA*QM(L+1)+DQM(L)                        2316.   
      DO 360 K=1,KMAX                                                   2317.   
  360 UM(K,L)=UM(K,L)+(-ALPHA*UM(K,L)+BETA*UM(K,L+1)+DUM(K,L))*RA(K)    2318.   
  380 ALPHA=BETA                                                        2319.   
CORIG    CDHSUM=0.                                                      2319.999
         AJ13(LMAX)=AJ13(LMAX)-CDHSUM                                   2320.   
C****                                                                   2321.   
C**** REEVAPORATION AND PRECIPITATION                                   2322.   
C****                                                                   2323.   
      LMAXM1=LMAX-1                                                     2324.   
      PRCP=COND(LMAX)                                                   2325.   
      PRHEAT=CDHEAT(LMAX)                                               2326.   
CORIG    AJ50(LMAX)=AJ50(LMAX)+CDHEAT(LMAX)                             2326.999
         AJ50(LMAX)=AJ50(LMAX)+CDHSUM                                   2327.   
      DO 540 LX=1,LMAXM1                                                2328.   
      L=LMAX-LX                                                         2329.   
      IF(PRCP.LE.0.) GO TO 530                                          2330.   
CORIG CLDMC(I,J,L+1)=.5*BX*DSG0(LMAX)                                   2330.999
      CLDMC(I,J,L+1)=CLDMC(I,J,L+1)+.5*BX*DSG0(LMAX)                    2331.   
C**** REEVAPORATE ALL PRECIPITATION FROM ABOVE                          2332.   
      EVAP=PRCP                                                         2333.   
      PRCP=0.                                                           2334.   
C**** FORWARD STEP COMPUTES HUMIDITY CHANGE BY RECONDENSATION           2335.   
C**** Q = Q + F(TOLD,PRHEAT,QOLD+EVAP)                                  2336.   
      MCLOUD=.25*AIRM(L)                                                2337.   
      IF(L.LE.LMIN) MCLOUD=.5*AIRM(L)                                   2338.   
      TOLD=SMOLD(L)*PLK(L)/AIRM(L)                                      2339.   
      TN=TOLD-PRHEAT/MCLOUD                                             2340.   
      QN=QMOLD(L)/AIRM(L)+EVAP/MCLOUD                                   2341.   
      LHX=LHE                                                           2342.   
      IF(TOLD.LT.TF) LHX=LHS                                            2343.   
      SLH=LHX/SHA                                                       2344.   
      QSATC=QSAT(TN,PL(L))                                              2345.   
      IF(QN-QSATC.LE.0.) GO TO 520                                      2346.   
      DO 510 N=1,3                                                      2347.   
      IF(N.NE.1) QSATC=QSAT(TN,PL(L))                                   2348.   
      DQ=(QN-QSATC)/(1.+SLH*LHX*BXCONS*QSATC/(TN*TN))                   2349.   
      TN=TN+SLH*DQ                                                      2350.   
      QN=QN-DQ                                                          2351.   
  510 PRCP=PRCP+DQ                                                      2352.   
      PRCP=MCLOUD*PRCP                                                  2353.   
      IF(PRCP.GT.EVAP) PRCP=EVAP                                        2354.   
C**** UPDATE TEMPERATURE AND HUMIDITY DUE TO NET REVAPORATION IN CLOUD  2355.   
  520 SM(L)=SM(L)+(SLH*PRCP-PRHEAT)/PLK(L)                              2356.   
      QM(L)=QM(L)+EVAP-PRCP                                             2357.   
CORIG    AJ50(L)=AJ50(L)+CDHEAT(L)+SLH*PRCP-PRHEAT                      2357.999
         AJ50(L)=AJ50(L)+SLH*PRCP-PRHEAT                                2358.   
C**** ADD PRECIPITATION AND LATENT HEAT BELOW                           2359.   
  530 PRHEAT=CDHEAT(L)+SLH*PRCP                                         2360.   
      PRCP=PRCP+COND(L)                                                 2361.   
C****                                                                   2362.   
  540 CONTINUE                                                          2363.   
C****                                                                   2364.   
      IF(PRCP.GT.0.) CLDMC(I,J,1)=.5*BX*DSG0(LMAX)                      2364.999
CORR  IF(PRCP.GT.0.) CLDMC(I,J,1)=CLDMC(I,J,1)+.5*BX*DSG0(LMAX)         2365.   
      PRCPMC=PRCPMC+PRCP                                                2366.   
C**** END OF CONVECTION CYCLE.  SET SOME VARIABLES TO DEFAULTS.         2367.   
      DO 560 L=1,LMAX                                                   2368.   
      COND(L)=0.                                                        2369.   
      CDHEAT(L)=0.                                                      2370.   
      DM(L)=0.                                                          2371.   
      DSM(L)=0.                                                         2372.   
      DQM(L)=0.                                                         2373.   
      DO 550 K=1,KMAX                                                   2374.   
  550 DUM(K,L)=0.                                                       2375.   
  560 CONTINUE                                                          2376.   
      DO 570 L=1,LMAXM1                                                 2377.   
      SMOLD(L)=SM(L)                                                    2378.   
  570 QMOLD(L)=QM(L)                                                    2379.   
C****                                                                   2380.   
C**** END OF OUTER LOOP OVER CLOUD BASE                                 2381.   
C****                                                                   2382.   
      IF(LMCMIN.EQ.0) LMCMIN=LMIN                                       2383.   
      IF(LMCMAX.LT.LMAX) LMCMAX=LMAX                                    2384.   
      GO TO 200                                                         2385.   
C**** NO MOIST CONVECTION OCCURRED                                      2386.   
  600 IF(LMCMIN.EQ.0) GO TO 700                                         2387.   
C**** ACCUMULATE MOIST CONVECTION DIAGNOSTICS                           2388.   
         DO 610 L=1,LMCMAX                                              2389.   
         HCNDMC=HCNDMC+AJ13(L)+AJ50(L)                                  2390.   
         AJL(J,L,13)=AJL(J,L,13)+AJ13(L)/DSIG(L)                        2391.   
         AJL(J,L,50)=AJL(J,L,50)+AJ50(L)/DSIG(L)                        2392.   
         AJL(J,L,51)=AJL(J,L,51)+SLHE*AJ51(L)/DSIG(L)                   2393.   
         IF(J.GE.JEQ-2.AND.J.LE.JEQ) AIL(I,L,6)=AIL(I,L,6)+             2394.   
     *     (AJ13(L)+AJ50(L))/DSIG(L)*DXYP(J)                            2395.   
  610    AJL(J,L,8)=AJL(J,L,8)+AJ8(L)                                   2396.   
         APRCPM=APRCPM+PRCPMC*POCEAN                                    2397.   
         BPRCPM=BPRCPM+PRCPMC*PLAND                                     2398.   
         CPRCPM=CPRCPM+PRCPMC*POICE                                     2399.   
         DJ(JR,62)=DJ(JR,62)+PRCPMC*DXYP(J)                             2400.   
C**** UPDATE THE MODEL WINDS                                            2401.   
  640 DO 650 L=LMCMIN,LMCMAX                                            2402.   
      DO 650 K=1,KMAX                                                   2403.   
  650 U(ID(K),1,L)=U(ID(K),1,L)+(UM(K,L)/AIRM(L)-UC(ID(K),1,L))         2404.   
C**** UPDATE MODEL TEMPERATURE, SPECIFIC HUMIDITY AND PRECIPITATION     2405.   
      DO 690 L=1,LMCMAX                                                 2406.   
      TL(L)=SM(L)/AIRM(L)*PLK(L)                                        2407.   
  690 QL(L)=QM(L)/AIRM(L)                                               2408.   
C****                                                                   2501.   
C**** LARGE SCALE PRECIPITATION                                         2502.   
C****                                                                   2503.   
  700 PRCPSS=0.                                                         2504.   
         HCNDSS=0.                                                      2505.   
      IF (MODRD.NE.0) GO TO 750                                         2506.   
      DQUP=0.                                                           2507.   
      LHXUP=LHE                                                         2508.   
      DO 740 LX=1,LM                                                    2509.   
      L=1+LM-LX                                                         2510.   
      TOLD=TL(L)                                                        2511.   
      QOLD=QL(L)                                                        2512.   
      LHX= LHE                                                          2513.   
      IF(TOLD.LT.TI) LHX= LHS                                           2514.   
      IF (LHXUP.EQ.LHS.AND.TOLD.LT.TF) LHX=LHS                          2515.   
      EX=DQUP*DSIGUP/DSIG(L)                                            2516.   
      TNEW=TOLD-SLH*EX                                                  2517.   
      QNEW=QOLD+EX                                                      2518.   
      DQUP=0.                                                           2519.   
      QSATL=QSAT(TNEW,PL(L))                                            2520.   
      LHXUP=LHE                                                         2521.   
C**** DETERMINE THE CLOUD COVER                                         2522.   
      IF (QNEW.LE.1.D-10) GO TO 710                                     2523.   
      BXQSAT=LHX*BXCONS                                                 2524.   
      AXQSAT=AXCONS+BXQSAT*BYTF                                         2525.   
      TCOND=BXQSAT/(AXQSAT-LOG(1.608*PL(L)*QNEW))                       2526.   
      DTCX=(TNEW-TCOND)/(SIGMA1(J,L)+1.D-10)                            2527.   
      IF (DTCX.GT.1.8) GO TO 710                                        2528.   
      FCL=ERFCPI(DTCX)                                                  2529.   
      IF (DTCX.LT.-1.8) FCL=1.                                          2530.   
      CLDSS(I,J,L)=FCL                                                  2531.   
  710 IF (QNEW.LT.QSATL) GO TO 730                                      2532.   
      LHX=LHE                                                           2533.   
      IF (TOLD.LT.TF) LHX=LHS                                           2534.   
      RHNEW=1.                                                          2535.   
      SLH=LHX/SHA                                                       2536.   
      GAMFAC=SLH*BXCONS*LHX                                             2537.   
      DO 720 N=1,3                                                      2538.   
      GAMA=GAMFAC*QSATL/(TNEW*TNEW)                                     2539.   
      DQ1=(QNEW-QSATL*RHNEW)/(1.+GAMA*RHNEW)                            2540.   
      DQUP=DQUP+DQ1                                                     2541.   
      TNEW=TNEW+SLH*DQ1                                                 2542.   
      QNEW=QNEW-DQ1                                                     2543.   
  720 QSATL=QSAT(TNEW,PL(L))                                            2544.   
      DSIGUP=DSIG(L)                                                    2545.   
      LHXUP=LHX                                                         2546.   
  730 TL(L)=TNEW                                                        2547.   
      QL(L)=QNEW                                                        2548.   
C**** ACCUMULATE SOME DIAGNOSTICS                                       2549.   
         HCNDSS=HCNDSS+(TNEW-TOLD)*DSIG(L)                              2550.   
  740    AJL(J,L,11)=AJL(J,L,11)+(TNEW-TOLD)*PIJ                        2551.   
      PRCPSS=DQUP*DSIG(1)*PIJ                                           2552.   
         APRCPS=APRCPS+PRCPSS*POCEAN                                    2553.   
         BPRCPS=BPRCPS+PRCPSS*PLAND                                     2554.   
         CPRCPS=CPRCPS+PRCPSS*POICE                                     2555.   
         DJ(JR,61)=DJ(JR,61)+PRCPSS*DXYP(J)                             2556.   
  750    CONTINUE                                                       2557.   
         DO 760 KR=1,4                                                  2558.   
         IF(I.EQ.IJD6(1,KR).AND.J.EQ.IJD6(2,KR)) GO TO 770              2559.   
  760    CONTINUE                                                       2560.   
      GO TO 780                                                         2561.   
  770    ADAILY(IHOUR,5,KR)=ADAILY(IHOUR,5,KR)+HCNDMC+HCNDSS*PIJ        2562.   
         ADAILY(IHOUR,49,KR)=ADAILY(IHOUR,49,KR)+(PRCPMC+PRCPSS)        2563.   
C**** TOTAL PRECIPITATION AND AGE OF SNOW                               2564.   
  780 PREC(I,J)=(PRCPMC+PRCPSS)*100./GRAV                               2565.   
      PRCP=PREC(I,J)                                                    2566.   
      IF(TPREC(I,J).GE.0.) PRCP=0.                                      2567.   
      GDATA(I,J,11)=(DTPERD+GDATA(I,J,11)*AGESNX)*EXP(-PRCP)            2568.   
C**** TOTAL HEATING AND MOISTURE ADJUSTMENT                             2569.   
      DO 790 L=1,LM                                                     2570.   
      T(I,J,L)=TL(L)/PLK(L)                                             2571.   
  790 Q(I,J,L)=QL(L)                                                    2572.   
  800 IM1=I                                                             2573.   
C**** END OF MAIN LOOP FOR I INDEX                                      2574.   
C**** QUANTITIES ACCUMULATED FOR BUDGET PAGES (DIAGJ)                   2574.1  
         AJ(J,61)=AJ(J,61)+APRCPS                                       2574.2  
         BJ(J,61)=BJ(J,61)+BPRCPS                                       2574.3  
         CJ(J,61)=CJ(J,61)+CPRCPS                                       2574.4  
         AJ(J,62)=AJ(J,62)+APRCPM                                       2574.5  
         BJ(J,62)=BJ(J,62)+BPRCPM                                       2574.6  
         CJ(J,62)=CJ(J,62)+CPRCPM                                       2574.7  
  810 CONTINUE                                                          2575.   
C****                                                                   2576.   
C**** END OF MAIN LOOP FOR J INDEX                                      2577.   
C****                                                                   2578.   
C**** ADD IN CHANGE OF ANG. MOMENTUM BY MOIST CONVECTION FOR DIAGNOSTIC 2579.   
         DO 880 L=1,LM                                                  2580.   
         DO 880 J=2,JM                                                  2581.   
         DO 880 I=1,IM                                                  2582.   
  880    AJL(J,L,39)=AJL(J,L,39)+(U(I,J,L)-UC(I,J,L))*P(I,J)            2583.   
      RETURN                                                            2584.   
      END                                                               2585.   
c  ** MFS (DELETED)
c  ** use Jeff's RANVAX instead because it is portable
c      FUNCTION RANDU (X)                                                2801.   
c      IMPLICIT REAL*8 (A-H,O-Z)                                         2801.1  
cC**** THIS FUNCTION GENERATES RANDOM NUMBERS ON AN IBM 360 OR 370       2802.   
c   10 IY=IX*65539                                                       2803.   
c      IF(IY) 20,40,30                                                   2804.   
c   20 IY=(IY+2147483647)+1                                              2805.   
c   30 IX=IY                                                             2806.   
c      RANDU=DFLOAT(IY)*.465661287308D-9                                 2807.   
c      RETURN                                                            2808.   
c   40 IX=1                                                              2809.   
c      GO TO 10                                                          2810.   
c      ENTRY RINIT (INIT)                                                2811.   
c      IX=INIT                                                           2812.   
c      RETURN                                                            2813.   
c      ENTRY RFINAL (IFINAL)                                             2814.   
c      IFINAL=IX                                                         2815.   
c      RETURN                                                            2816.   
c      END                                                               2817.   
c  ** END (DELETED)
      SUBROUTINE PRECIP                                                 3001.   
C****                                                                   3002.   
C**** THIS SUBROUTINE USES THE PRECIPITATION TO CALCULATE THE GROUND    3003.   
C**** WATER, GROUND ICE, SNOW COVER, AND RUNOFF                         3004.   
C****                                                                   3005.   
C**** RUN1 IS NOT ACUMULATED IN ADAILY FOR DIAG6                        3006.   
C****                                                                   3007.   
      INCLUDE 'BA94jalC9.COM'                                           3008.   
      COMMON U,V,T,P,Q                                                  3009.   
      COMMON/WORK1/CONV(IM,JM,LM),PK(IM,JM,LM),PREC(IM,JM),TPREC(IM,JM) 
            COMMON/WORKO/OA(IM,JM,11)                                   3010.2  
      DATA SHW/4185./,SHI/2060./,RHOI/916.6/                            3011.   
      DATA Z1I/.1/,Z1E/.1/,Z2LI/2.9/                                    3012.   
      DATA RHOW/1000./,Z2OIM/.4/,TFO/-1.56/                             3013.   
      DATA TTRUNC/0./                                                   3014.   
      DATA IFIRST/1/                                                    3015.   
C****                                                                   3016.   
C**** FDATA  2  LAND COVERAGE (1)                                       3017.   
C****        3  RATIO OF LAND ICE COVERAGE TO LAND COVERAGE (1)         3018.   
C****                                                                   3019.   
C**** ODATA  1  OCEAN TEMPERATURE (C)                                   3020.   
C****        2  RATIO OF OCEAN ICE COVERAGE TO WATER COVERAGE (1)       3021.   
C****        3  OCEAN ICE AMOUNT OF SECOND LAYER (KG/M**2)              3022.   
C****                                                                   3023.   
C**** GDATA  1  OCEAN ICE SNOW AMOUNT (KG/M**2)                         3024.   
C****        2  EARTH SNOW AMOUNT (KG/M**2)                             3025.   
C****        3  OCEAN ICE TEMPERATURE OF FIRST LAYER (C)                3026.   
C****        4  EARTH TEMPERATURE OF FIRST LAYER (C)                    3027.   
C****        5  EARTH WATER OF FIRST LAYER (KG/M**2)                    3028.   
C****        6  EARTH ICE OF FIRST LAYER (KG/M**2)                      3029.   
C****        7  OCEAN ICE TEMPERATURE OF SECOND LAYER (C)               3030.   
C****       12  LAND ICE SNOW AMOUNT (KG/M**2)                          3031.   
C****       13  LAND ICE TEMPERATURE OF FIRST LAYER (C)                 3032.   
C****       14  LAND ICE TEMPERATURE OF SECOND LAYER (C)                3033.   
C****                                                                   3034.   
C**** VDATA  9  WATER FIELD CAPACITY OF FIRST LAYER (KG/M**2)           3035.   
C****                                                                   3036.   
      IF(IFIRST.NE.1) GO TO 10                                          3037.   
      IFIRST=0                                                          3038.   
      ACE1I=Z1I*RHOI                                                    3040.   
      AC2OIM=Z2OIM*RHOI                                                 3041.   
      ACE2LI=Z2LI*RHOI                                                  3043.   
      HC1I=ACE1I*SHI                                                    3044.   
      HC1DE=Z1E*1129950.                                                3045.   
C****                                                                   3046.   
C**** OUTSIDE LOOP OVER J AND I, EXECUTED ONCE FOR EACH GRID POINT      3047.   
C****                                                                   3048.   
   10 DO 980 J=1,JM                                                     3049.   
      IMAX=IM                                                           3050.   
      IF(J.EQ.1.OR.J.EQ.JM) IMAX=1                                      3051.   
         AENRGP=0.                                                      3052.   
         BENRGP=0.                                                      3053.   
         CENRGP=0.                                                      3054.   
         BEDIFS=0.                                                      3055.   
         CEDIFS=0.                                                      3056.   
         AEFO=0.                                                        3057.   
         BERUN0=0.                                                      3058.   
         BERUN2=0.                                                      3059.   
         CERUN2=0.                                                      3060.   
         AERUN4=0.                                                      3061.   
         CERUN4=0.                                                      3062.   
         BDIFS=0.                                                       3063.   
         CDIFS=0.                                                       3064.   
         AIFO=0.                                                        3065.   
         BRUN0=0.                                                       3066.   
         CRUN0=0.                                                       3067.   
         BRUN2=0.                                                       3068.   
         CRUN2=0.                                                       3069.   
         ARUN4=0.                                                       3070.   
         CRUN4=0.                                                       3071.   
      DO 960 I=1,IMAX                                                   3072.   
      IF(PREC(I,J).LE.0.) GO TO 960                                     3073.   
C****                                                                   3074.   
C**** DETERMINE SURFACE CONDITIONS                                      3075.   
C****                                                                   3076.   
      PLAND=FDATA(I,J,2)                                                3077.   
      PWATER=1.-PLAND                                                   3078.   
      PLICE=FDATA(I,J,3)*PLAND                                          3079.   
      PEARTH=PLAND-PLICE                                                3080.   
      ROICE=ODATA(I,J,2)                                                3081.   
      POICE=ROICE*PWATER                                                3082.   
      POCEAN=PWATER-POICE                                               3083.   
         JR=JREG(I,J)                                                   3084.   
         DXYPJ=DXYP(J)                                                  3085.   
         RUN0S=0.                                                       3086.   
         DIFSS=0.                                                       3087.   
C**** CALCULATE PRECIPITATION HEAT FLUX (FALLS AT 0 DEGREES CENTIGRADE) 3088.   
      PRCP=PREC(I,J)                                                    3089.   
      TPRCP=TPREC(I,J)                                                  3090.   
      IF(TPRCP.LT.0.) GO TO 30                                          3091.   
C     EPRCP=PRCP*TPRCP*SHW                                              3092.   
      EPRCP=0.                                                          3093.   
      ENRGP=EPRCP                                                       3094.   
      GO TO 50                                                          3095.   
C     EPRCP=PRCP*TPRCP*SHI                                              3096.   
   30 EPRCP=0.                                                          3097.   
      ENRGP=EPRCP-PRCP*LHM                                              3098.   
         AIJ(I,J,70)=AIJ(I,J,70)+PRCP                                   3098.5  
C****                                                                   3099.   
   50 IF(PWATER.LE.0.) GO TO 400                                        3100.   
C****                                                                   3101.   
C**** OCEAN                                                             3102.   
C****                                                                   3103.   
            OA(I,J,4)=OA(I,J,4)+ENRGP                                   3103.5  
         AENRGP=AENRGP+ENRGP*POCEAN                                     3104.   
         AIJ(I,J,65)=AIJ(I,J,65)+ENRGP*POCEAN                           3104.5  
c  ** MFS (CHANGED)
c  ** Correction because kocean <> 1 is no longer equivlient to kocean = 0
c      IF(KOCEAN.NE.1) GO TO 100                                         3105.   
      IF(KOCEAN.EQ.0) GO TO 100                                   
c  ** END (CHANGED)
      TGW=ODATA(I,J,1)                                                  3106.   
      WTRO=Z1O(I,J)*RHOW                                                3107.   
      ENRGO0=WTRO*TGW*SHW                                               3108.   
      EOFRZ=WTRO*TFO*SHW                                                3109.   
      RUN4=0.                                                           3110.   
      ERUN4=RUN4*TGW*SHW                                                3111.   
         AERUN4=AERUN4+ERUN4*POCEAN                                     3112.   
         ARUN4=ARUN4+RUN4*POCEAN                                        3113.   
      ENRGO=ENRGP-ERUN4                                                 3114.   
      IF(ENRGO0+ENRGO.LT.EOFRZ) GO TO 80                                3115.   
C**** OCEAN TEMPERATURE IS STILL ABOVE FREEZING, NO ICE IS FORMED       3116.   
      ACEFO=0.                                                          3117.   
      ENRGFO=0.                                                         3118.   
      IF(ROICE.GT.0.) GO TO 110                                         3119.   
      ODATA(I,J,1)=TGW+(ENRGO/(WTRO*SHW)+TTRUNC)                        3120.   
      GO TO 400                                                         3121.   
C**** SNOW COOLS TGO TO FREEZING POINT FOR OCEAN AND FORMS SOME ICE     3122.   
   80 ACEFO=(ENRGO0+ENRGO-EOFRZ)/(TFO*(SHI-SHW)-LHM)                    3123.   
      ENRGFO=ACEFO*(TFO*SHI-LHM)                                        3124.   
         AEFO=AEFO-ENRGFO*POCEAN                                        3125.   
         AIFO=AIFO-ACEFO*POCEAN                                         3126.   
      IF(ROICE.GT.0.) GO TO 110                                         3127.   
      ROICE=ACEFO/(ACE1I+AC2OIM)                                        3128.   
      ODATA(I,J,1)=TFO                                                  3129.   
      ODATA(I,J,2)=ROICE                                                3130.   
      GDATA(I,J,1)=0.                                                   3131.   
      GDATA(I,J,3)=TFO                                                  3132.   
      GDATA(I,J,7)=TFO                                                  3133.   
      ODATA(I,J,3)=AC2OIM                                               3134.   
      GO TO 400                                                         3135.   
C****                                                                   3136.   
  100 IF(POICE.LE.0.) GO TO 400                                         3137.   
C****                                                                   3138.   
C**** OCEAN ICE                                                         3139.   
C****                                                                   3140.   
  110 SNOW=GDATA(I,J,1)                                                 3141.   
      TG1=GDATA(I,J,3)                                                  3142.   
      TG2=GDATA(I,J,7)                                                  3143.   
      ACE2=ODATA(I,J,3)                                                 3144.   
         CENRGP=CENRGP+ENRGP*POICE                                      3145.   
         AIJ(I,J,66)=AIJ(I,J,66)+ENRGP*POICE                            3145.5  
      HC1=HC1I+SNOW*SHI                                                 3146.   
      RUN0=0.                                                           3147.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.NE.1) GO TO 120                                         3148.   
      IF(KOCEAN.EQ.0) GO TO 120                           
c  ** END (CHANGED)
         CERUN4=CERUN4+ERUN4*POICE                                      3149.   
         CRUN4=CRUN4+RUN4*POICE                                         3150.   
      HC2=ACE2*SHI                                                      3151.   
      WTRW0=WTRO-ROICE*(SNOW+ACE1I+ACE2)                                3152.   
      ENRGW0=WTRW0*TGW*SHW                                              3153.   
      DIFS=0.                                                           3154.   
      EDIFS=0.                                                          3155.   
  120 IF(TPRCP.LT.0.) GO TO 150                                         3156.   
      IF(EPRCP.LT.-TG1*HC1) GO TO 140                                   3157.   
C**** RAIN HEATS UP TG1 TO FREEZING POINT AND MELTS SOME SNOW OR ICE    3158.   
      DWATER=(TG1*HC1+EPRCP)/LHM                                        3159.   
      TG1=0.                                                            3160.   
      RUN0=DWATER+PRCP                                                  3161.   
      IF(DWATER.GT.SNOW) GO TO 130                                      3162.   
C**** RAIN MELTS SOME SNOW                                              3163.   
      SNOW=SNOW-DWATER                                                  3164.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.NE.1) GO TO 380                                         3165.   
      IF(KOCEAN.EQ.0) GO TO 380                                
c  ** END (CHANGED)
      GO TO 300                                                         3166.   
C**** RAIN MELTS ALL SNOW AND SOME ICE, ICE FROM LAYER 2 GOES TO LAYER 13167.   
  130 DIFS=SNOW-DWATER                                                  3168.   
      SNOW=0.                                                           3169.   
      TG1=-TG2*DIFS/ACE1I                                               3170.   
      EDIFS=DIFS*(TG2*SHI-LHM)                                          3171.   
      ERUN2=EDIFS                                                       3172.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.NE.1) GO TO 170                                         3173.   
      IF(KOCEAN.EQ.0) GO TO 170                                       
c  ** END (CHANGED)
      GO TO 210                                                         3174.   
C**** RAIN COOLS TO FREEZING POINT AND HEATS UP TG1                     3175.   
  140 TG1=TG1+EPRCP/HC1                                                 3176.   
      RUN0=PRCP                                                         3177.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.NE.1) GO TO 390                                         3178.   
      IF(KOCEAN.EQ.0) GO TO 390                   
c  ** END (CHANGED)
      GO TO 300                                                         3179.   
C**** SNOW INCREASES SNOW AMOUNT AND SNOW TEMPERATURE RECOMPUTES TG1    3180.   
  150 TG1=(TG1*HC1+EPRCP)/(HC1+PRCP*SHI)                                3181.   
      SNOW=SNOW+PRCP                                                    3182.   
      IF(SNOW.GT.ACE1I) GO TO 160                                       3183.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.NE.1) GO TO 380                                         3184.   
      IF(KOCEAN.EQ.0) GO TO 380                                  
c  ** END (CHANGED)
      GO TO 300                                                         3185.   
C**** SNOW IS COMPACTED INTO ICE, ICE FROM LAYER 1 GOES DOWN TO LAYER 2 3186.   
  160 DIFS=SNOW-.9*ACE1I                                                3187.   
      SNOW=.9*ACE1I                                                     3188.   
      EDIFS=DIFS*(TG1*SHI-LHM)                                          3189.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.EQ.1) GO TO 200                                         3190.   
      IF(KOCEAN.NE.0) GO TO 200                                         3190.   
c  ** END (CHANGED)
      ERUN2=DIFS*(TG2*SHI-LHM)                                          3191.   
      GDATA(I,J,7)=TG2+(TG1-TG2)*DIFS/ACE2                              3192.   
  170    CEDIFS=CEDIFS+EDIFS*POICE                                      3193.   
         CDIFS=CDIFS+DIFS*POICE                                         3194.   
         DIFSS=DIFSS+DIFS*POICE                                         3195.   
         CERUN2=CERUN2+ERUN2*POICE                                      3196.   
         CRUN2=CRUN2+DIFS*POICE                                         3197.   
      GO TO 380                                                         3198.   
C**** DIFFUSION CHANGES ICE AMOUNT AND TEMPERATURE OF SECOND LAYER      3199.   
  200 TG2=TG2+(TG1-TG2)*DIFS/(ACE2+DIFS)                                3200.   
  210 ACE2=ACE2+(DIFS+PTRUNC)                                           3201.   
         CEDIFS=CEDIFS+EDIFS*POICE                                      3202.   
         CDIFS=CDIFS+DIFS*POICE                                         3203.   
C**** CALCULATE THE COMPOSITE WATER AND WATER ENERGY                    3204.   
  300 WTRW=WTRW0-(1.-ROICE)*ACEFO+ROICE*(RUN0-RUN4)                     3205.   
      ENRGW=ENRGW0+(1.-ROICE)*(ENRGP-ENRGFO)-ROICE*ERUN4                3206.   
      TGW=ENRGW/(WTRW*SHW)                                              3207.   
      IF(ACEFO.LE.0.) GO TO 310                                         3208.   
C**** NEW ICE FORMED ON THE OCEAN SURFACE                               3209.   
      DRO=(1.-ROICE)*ACEFO/(ACE1I+AC2OIM)                               3210.   
      TG1=TG1+(TFO-TG1)*DRO*ACE1I/(ROICE*(SNOW+ACE1I)+DRO*ACE1I)        3211.   
      TG2=TG2+(TFO-TG2)*DRO*AC2OIM/(ROICE*ACE2+DRO*AC2OIM)              3212.   
      SNOW=SNOW*ROICE/(ROICE+DRO)                                       3213.   
      ROICE=ROICE+DRO                                                   3214.   
      ACE2=ACE2+(DRO*(AC2OIM-ACE2)/ROICE+PTRUNC)                        3215.   
  310 ODATA(I,J,1)=TGW                                                  3216.   
      ODATA(I,J,2)=ROICE                                                3217.   
      ODATA(I,J,3)=ACE2                                                 3218.   
      GDATA(I,J,7)=TG2                                                  3219.   
  380 GDATA(I,J,1)=SNOW                                                 3220.   
  390 GDATA(I,J,3)=TG1                                                  3221.   
         CRUN0=CRUN0+RUN0*POICE                                         3222.   
         RUN0S=RUN0S+RUN0*POICE                                         3223.   
C****                                                                   3224.   
  400 IF(PLICE.LE.0.) GO TO 600                                         3225.   
C****                                                                   3226.   
C**** LAND ICE                                                          3227.   
C****                                                                   3228.   
      SNOW=GDATA(I,J,12)                                                3229.   
      TG1=GDATA(I,J,13)                                                 3230.   
      TG2=GDATA(I,J,14)                                                 3231.   
         BENRGP=BENRGP+ENRGP*PLICE                                      3232.   
         AIJ(I,J,67)=AIJ(I,J,67)+ENRGP                                  3232.5  
      HC1=HC1I+SNOW*SHI                                                 3233.   
      RUN0=0.                                                           3234.   
      IF(TPRCP.LT.0.) GO TO 480                                         3235.   
      IF(EPRCP.LT.-TG1*HC1) GO TO 460                                   3236.   
C**** RAIN HEATS UP TG1 TO FREEZING POINT AND MELTS SOME SNOW OR ICE    3237.   
      DWATER=(TG1*HC1+EPRCP)/LHM                                        3238.   
      TG1=0.                                                            3239.   
      RUN0=DWATER+PRCP                                                  3240.   
      IF(DWATER.GT.SNOW) GO TO 440                                      3241.   
C**** RAIN MELTS SOME SNOW                                              3242.   
      SNOW=SNOW-DWATER                                                  3243.   
      GO TO 580                                                         3244.   
C**** RAIN MELTS ALL SNOW AND SOME ICE, ICE MOVES UP THROUGH THE LAYERS 3245.   
  440 DIFS=SNOW-DWATER                                                  3246.   
      SNOW=0.                                                           3247.   
      TG1=-TG2*DIFS/ACE1I                                               3248.   
      EDIFS=DIFS*(TG2*SHI-LHM)                                          3249.   
      ERUN2=EDIFS                                                       3250.   
      GO TO 560                                                         3251.   
C**** RAIN COOLS TO FREEZING POINT AND HEATS UP TG1                     3252.   
  460 TG1=TG1+EPRCP/HC1                                                 3253.   
      RUN0=PRCP                                                         3254.   
      GO TO 590                                                         3255.   
C**** SNOW INCREASES SNOW AMOUNT AND SNOW TEMPERATURE RECOMPUTES TG1    3256.   
  480 TG1=(TG1*HC1+EPRCP)/(HC1+PRCP*SHI)                                3257.   
      SNOW=SNOW+PRCP                                                    3258.   
      IF(SNOW.LE.ACE1I) GO TO 580                                       3259.   
C**** SNOW IS COMPACTED INTO ICE, ICE MOVES DOWN THROUGH THE LAYERS     3260.   
      DIFS=SNOW-.9*ACE1I                                                3261.   
      SNOW=.9*ACE1I                                                     3262.   
      EDIFS=DIFS*(TG1*SHI-LHM)                                          3263.   
      ERUN2=DIFS*(TG2*SHI-LHM)                                          3264.   
      GDATA(I,J,14)=TG2+(TG1-TG2)*DIFS/ACE2LI                           3265.   
  560    BEDIFS=BEDIFS+EDIFS*PLICE                                      3266.   
         AIJ(I,J,69)=AIJ(I,J,69)+EDIFS                                  3266.5  
         BDIFS=BDIFS+DIFS*PLICE                                         3267.   
         DIFSS=DIFSS+DIFS*PLICE                                         3268.   
         BERUN2=BERUN2+ERUN2*PLICE                                      3269.   
         AIJ(I,J,72)=AIJ(I,J,72)+ERUN2                                  3269.5  
         BRUN2=BRUN2+DIFS*PLICE                                         3270.   
  580 GDATA(I,J,12)=SNOW                                                3271.   
  590 GDATA(I,J,13)=TG1                                                 3272.   
         BRUN0=BRUN0+RUN0*PLICE                                         3273.   
         RUN0S=RUN0S+RUN0*PLICE                                         3274.   
         AIJ(I,J,33)=AIJ(I,J,33)+RUN0                                   3274.5  
C****                                                                   3275.   
  600 IF(PEARTH.LE.0.) GO TO 940                                        3276.   
C****                                                                   3277.   
C**** EARTH                                                             3278.   
C****                                                                   3279.   
      SNOW=GDATA(I,J,2)                                                 3280.   
      TG1=GDATA(I,J,4)                                                  3281.   
      WTR1=GDATA(I,J,5)                                                 3282.   
      ACE1=GDATA(I,J,6)                                                 3283.   
         BENRGP=BENRGP+ENRGP*PEARTH                                     3284.   
         AIJ(I,J,68)=AIJ(I,J,68)+ENRGP                                  3284.5  
      WFC1=VDATA(I,J,9)                                                 3285.   
      CHI1=(WTR1+ACE1)/WFC1                                             3286.   
      HC1=HC1DE+WTR1*SHW+(ACE1+SNOW)*SHI                                3287.   
      RUN0=0.                                                           3288.   
      ERUN0=0.                                                          3289.   
      IF(TPRCP.LT.0.) GO TO 660                                         3290.   
      IF(TG1.LE.0.) GO TO 620                                           3291.   
C**** RAIN ON GROUND ABOVE FREEZING POINT, RECOMPUTE TG1                3292.   
      TG1=(TG1*HC1+EPRCP)/(HC1+PRCP*SHW)                                3293.   
      RUN0=MAX(PRCP*.5*CHI1,PRCP+WTR1-WFC1)                             3294.   
      WTR1=WTR1+(PRCP-RUN0)                                             3295.   
      ERUN0=TG1*RUN0*SHW                                                3296.   
      GO TO 890                                                         3297.   
  620 IF(EPRCP.LT.-TG1*HC1) GO TO 640                                   3298.   
C**** RAIN HEATS UP TG1 TO FREEZING POINT                               3299.   
      EPRCP=EPRCP+TG1*HC1                                               3300.   
      TG1=0.                                                            3301.   
      IF(EPRCP.LT.(ACE1+SNOW)*LHM) GO TO 630                            3302.   
C**** RAIN MELTS SNOW AND ICE AND HEATS UP TG1 ABOVE FREEZING POINT     3303.   
      RUN0=MAX((PRCP+SNOW)*.5*CHI1,PRCP+SNOW+(WTR1+ACE1-WFC1))          3304.   
      WTR1=WTR1+ACE1+SNOW+(PRCP-RUN0)                                   3305.   
      TG1=(EPRCP-(ACE1+SNOW)*LHM)/(HC1DE+(WTR1+RUN0)*SHW)               3306.   
      ACE1=0.                                                           3307.   
      SNOW=0.                                                           3308.   
      ERUN0=TG1*RUN0*SHW                                                3309.   
      GO TO 880                                                         3310.   
C**** RAIN MELTS SOME SNOW AND ICE, TG1 IS AT FREEZING POINT            3311.   
  630 DWATER=EPRCP/LHM                                                  3312.   
      DSNOW=MIN(SNOW,DWATER)                                            3313.   
      RUN0=MAX((PRCP+DSNOW)*.5*CHI1,PRCP+DSNOW+(WTR1+ACE1-WFC1))        3314.   
      WTR1=WTR1+DWATER+PRCP-RUN0                                        3315.   
CW    IF(WTR1.LT.0.) WRITE(6,*) '---WTR 3315',WTR1                      3315.1  
      IF(WTR1.LT..000001) WTR1=0.                                       3315.2  
      SNOW=SNOW-DSNOW                                                   3316.   
      ACE1=ACE1-DWATER+DSNOW                                            3317.   
      GO TO 880                                                         3318.   
C**** RAIN COOLS TO FREEZING POINT AND HEATS UP TG1                     3319.   
  640 TG1=TG1+EPRCP/HC1                                                 3320.   
      RUN0=MAX(PRCP*.5*CHI1,PRCP+(ACE1-WFC1))                           3321.   
      PRCP=PRCP-RUN0                                                    3322.   
      IF(PRCP*LHM.LT.-TG1*HC1) GO TO 650                                3323.   
C**** SOME RAIN FREEZES AND TG1 HEATS UP TO FREEZING POINT              3324.   
      DICE=-TG1*HC1/LHM                                                 3325.   
      TG1=0.                                                            3326.   
      ACE1=ACE1+DICE                                                    3327.   
      WTR1=PRCP-DICE                                                    3328.   
      GO TO 890                                                         3329.   
C**** RAIN FREEZES AND HEATS UP TG1, BUT STILL BELOW FREEZING POINT     3330.   
  650 TG1=(TG1*HC1+PRCP*LHM)/(HC1+PRCP*SHI)                             3331.   
      ACE1=ACE1+PRCP                                                    3332.   
      GO TO 890                                                         3333.   
  660 IF(TG1.LE.0.) GO TO 690                                           3334.   
      IF(-EPRCP.LT.TG1*HC1) GO TO 670                                   3335.   
C**** NEW SNOW HEATS UP AND COOLS TG1 TO FREEZING POINT                 3336.   
      EPRCP=EPRCP+TG1*HC1                                               3337.   
      TG1=0.                                                            3338.   
      SNOW=PRCP                                                         3339.   
      GO TO 700                                                         3340.   
C**** NEW SNOW HEATS UP TO FREEZING POINT AND COOLS TG1                 3341.   
  670 TG1=TG1+EPRCP/HC1                                                 3342.   
      IF(PRCP.LT.TG1*HC1/LHM) GO TO 680                                 3343.   
C**** SOME NEW SNOW MELTS UNTIL TG1 COOLS TO FREEZING POINT             3344.   
      DWATER=TG1*HC1/LHM                                                3345.   
      TG1=0.                                                            3346.   
      SNOW=PRCP-DWATER                                                  3347.   
      RUN0=MAX(DWATER*.5*CHI1,DWATER+(WTR1-WFC1))                       3348.   
      WTR1=WTR1+(DWATER-RUN0)                                           3349.   
      GO TO 880                                                         3350.   
C**** ALL NEW SNOW MELTS, RECOMPUTE TG1                                 3351.   
  680 TG1=(TG1*HC1-PRCP*LHM)/(HC1+PRCP*SHW)                             3352.   
      RUN0=MAX(PRCP*.5*CHI1,PRCP+(WTR1-WFC1))                           3353.   
      WTR1=WTR1+(PRCP-RUN0)                                             3354.   
      ERUN0=TG1*RUN0*SHW                                                3355.   
      GO TO 890                                                         3356.   
  690 SNOW=SNOW+PRCP                                                    3357.   
      IF(WTR1.GT.0.) GO TO 700                                          3358.   
C**** NEW SNOW INCREASES SNOW AMOUNT AND SNOW TEMP RECOMPUTES TG1       3359.   
      TG1=(TG1*HC1+EPRCP)/(HC1+PRCP*SHI)                                3360.   
      GO TO 880                                                         3361.   
  700 IF(-EPRCP.LT.WTR1*LHM) GO TO 710                                  3362.   
C**** GROUND WATER FREEZES, RECOMPUTE TG1                               3363.   
      ACE1=ACE1+WTR1                                                    3364.   
      HC1=HC1DE+(ACE1+SNOW)*SHI                                         3365.   
      TG1=(EPRCP+WTR1*LHM)/HC1                                          3366.   
      WTR1=0.                                                           3367.   
      GO TO 880                                                         3368.   
C**** SOME GROUND WATER FREEZES UNTIL SNOW TEMP HEATS TO FREEZING POINT 3369.   
  710 DICE=-EPRCP/LHM                                                   3370.   
      WTR1=WTR1-DICE                                                    3371.   
      ACE1=ACE1+DICE                                                    3372.   
  880 GDATA(I,J,2)=SNOW                                                 3373.   
  890 GDATA(I,J,4)=TG1                                                  3374.   
      IF(WTR1+ACE1+.000001.GT.WFC1) THEN                                3374.1  
C**** PREVENT OVER-SATURATION DUE TO ROUND-OFF                          3374.2  
CW       WRITE(6,*) '---WFC1 3374',WTR1,ACE1,WFC1                       3374.3  
         WTR1=.99999*WTR1                                               3374.4  
         ACE1=.99999*ACE1                                               3374.5  
      END IF                                                            3374.6  
      GDATA(I,J,5)=WTR1                                                 3375.   
      GDATA(I,J,6)=ACE1                                                 3376.   
         BERUN0=BERUN0+ERUN0*PEARTH                                     3377.   
         BRUN0=BRUN0+RUN0*PEARTH                                        3378.   
         RUN0S=RUN0S+RUN0*PEARTH                                        3379.   
         AIJ(I,J,32)=AIJ(I,J,32)+RUN0                                   3379.5  
C****                                                                   3380.   
C**** ACCUMULATE DIAGNOSTICS                                            3381.   
C****                                                                   3382.   
  940    DJ(JR,39)=DJ(JR,39)+ENRGP*DXYPJ                                3383.   
         DJ(JR,40)=DJ(JR,40)+ERUN0*PEARTH*DXYPJ                         3383.5  
         DJ(JR,45)=DJ(JR,45)+DIFSS*DXYPJ                                3384.   
         DJ(JR,54)=DJ(JR,54)+RUN0S*DXYPJ                                3385.   
         AIJ(I,J,5)=AIJ(I,J,5)+PREC(I,J)                                3386.   
         AIJ(I,J,23)=AIJ(I,J,23)+ENRGP                                  3387.   
  960 CONTINUE                                                          3389.   
         AJ(J,39)=AJ(J,39)+AENRGP                                       3390.   
         BJ(J,39)=BJ(J,39)+BENRGP                                       3391.   
         CJ(J,39)=CJ(J,39)+CENRGP                                       3392.   
         BJ(J,40)=BJ(J,40)+BERUN0                                       3393.   
         BJ(J,41)=BJ(J,41)+BEDIFS                                       3394.   
         CJ(J,41)=CJ(J,41)+CEDIFS                                       3395.   
         AJ(J,43)=AJ(J,43)+AEFO                                         3396.   
         BJ(J,43)=BJ(J,43)+BERUN2                                       3397.   
         CJ(J,43)=CJ(J,43)+CERUN2                                       3398.   
         BJ(J,45)=BJ(J,45)+BDIFS                                        3399.   
         CJ(J,45)=CJ(J,45)+CDIFS                                        3400.   
         BJ(J,54)=BJ(J,54)+BRUN0                                        3401.   
         CJ(J,54)=CJ(J,54)+CRUN0                                        3402.   
         AJ(J,46)=AJ(J,46)+AIFO                                         3403.   
         BJ(J,46)=BJ(J,46)+BRUN2                                        3404.   
         CJ(J,46)=CJ(J,46)+CRUN2                                        3405.   
         AJ(J,47)=AJ(J,47)+ARUN4                                        3406.   
         CJ(J,47)=CJ(J,47)+CRUN4                                        3407.   
         AJ(J,48)=AJ(J,48)+AERUN4                                       3408.   
         CJ(J,48)=CJ(J,48)+CERUN4                                       3409.   
  980 CONTINUE                                                          3410.   
      RETURN                                                            3411.   
      END                                                               3412.   
      SUBROUTINE RADIA0                                                 3501.   
C****                                                                   3502.   
C**** THIS SUBROUTINE SETS THE RADIATION CONTROL PARAMETERS AND         3503.   
C**** CALCULATES AREA WEIGHTED LATITUDES FOR A STANDARD GRID ETC        3504.   
C****                                                                   3505.   
      INCLUDE 'BA94jalC9.COM'                                              3505.5  
c  ** MFS (ADDED)
      INCLUDE 'FORCINGSmac.COM'
c  ** END (ADDED)
      REAL*8 LT1(IM),LT2(IM)                                            3506.   
      DIMENSION COSZ(IM,JM),COSZA(IM,JM)                                3507.   
      DIMENSION SINJ(JM),COSJ(JM),RI(IM),SINI(IM),COSI(IM)              3508.   
      COMMON/WORK5/LT1,LT2,SLT1(IM),SLT2(IM),S2LT1(IM),S2LT2(IM)        3509.   
C                                                                       3511.   
C                   RADCOM:      CONTROL/INPUT PARAMETERS               3512.   
C                                                                       3513.   
      COMMON/RADCOM/VADATA(11,4,3),DGLAT(46),DGLON(72),TMINSR,FULGAS(18)3515.   
     A             ,FRACSL,RATQSL,FOGTSL,PTLISO,TLGRAD,TKCICE,FGOLDU(18)3515.1  
     B             ,FLONO3,FRAYLE,FCLDTR,FCLDSR,FALGAE,FMARCL,FEMTRA(6) 3515.2  
     C             ,WETTRA,WETSRA,DMOICE,DMLICE,LICETK,NTRCE,FZASRA(6)  3515.3  
     D             ,ID5(5),ITR(4),IMG(2),ILG(2),LAPGAS,KWVCON,NORMS0,NV 3515.4  
     E             ,KEEPRH,KEEPAL,ISOSCT,IHGSCT,KFRACC,KGASSR,KAERSR    3515.5  
     F             ,MARCLD,LAYTOP,LMR,LMRP,JMLAT,IMLON,KFORCE,LASTVC    3515.6  
C                                                                       3515.7  
C                                BASIC RADCOM INPUT DATA                3515.8  
C                                                                       3515.9  
     G             ,PLE(40),HLB(40),TLB(40),TLT(40),TL(40),U0GAS(40,9)  3516.   
     H             ,ULGAS(40,9),TRACER(40,4),RTAU(40),QL(40),RHL(40)    3516.1  
     I             ,POCEAN,PEARTH,POICE,PLICE,AGESN,SNOWE,SNOWOI,SNOWLI 3516.2  
     J             ,TGO,TGE,TGOI,TGLI,TS,WS,WEARTH,ZOICE,FSPARE(200)    3516.3  
     K             ,S0,COSZN,PVT(11),BXA(153),SRBXAL(15,2),FRC(5),LUXGAS3516.4  
     L             ,JYEARR,JDAYR,JLAT,ILON,MEANAL,KALVIS,ISPARE(25),SGPS3516.5  
C**** ZERO1 HAS TO EQUAL THE CUT-OFF VALUE FOR COSZ USED IN SOLAR       3519.   
C**** COSZS WORKS CORRECTLY ONLY IF ZERO1 >> 1.D-3                      3520.   
      DATA ZERO1/1.D-2/                                                 3521.   
C**** COMPUTE THE AREA WEIGHTED LATITUDES AND THEIR SINES AND COSINES   3522.   
      PHIS=-.25*TWOPI                                                   3524.   
      SPHIS=-1.                                                         3525.   
      CPHIS=0.                                                          3526.   
      DO 20 J=1,JM-1                                                    3527.   
C**** the equation on 3527.3 or 3527.4 are for any geometry             3527.1
Cold   PHIN=(TWOPI/(JMM1+JMM1))*(J-.5*JM)                               3527.2   
      PHIN=DLAT*(J-.5*JM)                                               3527.3
C      PHIN=LAT_DG(J+1,2)*TWOPI/360.                                    3527.4
      SPHIN=SIN(PHIN)                                                   3529.   
      CPHIN=COS(PHIN)                                                   3530.         
      PHIM=(PHIN*SPHIN+CPHIN-PHIS*SPHIS-CPHIS)/(SPHIN-SPHIS)            3531.   
      DGLAT(J)=(360./TWOPI)*PHIM                                        3532.   
      SINJ(J)=SIN(PHIM)                                                 3533.   
      COSJ(J)=COS(PHIM)                                                 3534.   
      PHIS=PHIN                                                         3535.   
      SPHIS=SPHIN                                                       3536.   
   20 CPHIS=CPHIN                                                       3537.   
      PHIN=.25*TWOPI                                                    3538.   
      SPHIN=1.                                                          3539.   
      CPHIN=0.                                                          3540.   
      PHIM=(PHIN*SPHIN+CPHIN-PHIS*SPHIS-CPHIS)/(SPHIN-SPHIS)            3541.   
      DGLAT(JM)=(360./TWOPI)*PHIM                                       3542.   
      SINJ(JM)=SIN(PHIM)                                                3543.   
      COSJ(JM)=COS(PHIM)                                                3544.   
C**** COMPUTE THE SINES AND COSINES OF LONGITUDE                        3545.   
      DO 40 I=1,IM                                                      3546.   
      RI(I)=(TWOPI/IM)*(I-.5)                                           3547.   
      DGLON(I)=(360./IM)*(I-.5)                                         3548.   
      SINI(I)=SIN(RI(I))                                                3549.   
   40 COSI(I)=COS(RI(I))                                                3550.   
C**** MODIFY AND PRINT OUT THE RADIATION CONTROL PARAMETERS             3551.   
C     IF CO2 > 0 USE 1958 DATA, MEAN STRATOSPHERIC AEROSOLS             3551.8  
C     FULGAS(1)=(.005/.012)                    .012  IS USED            3551.9  
C     IF(CO2.GT.0.) FULGAS(2)=(285./315.)*CO2   315. IS USED            3552.   
c  ** MFS (CHANGED)
c  ** I always call FORSET and FORGET now
c      IF(CO2.GT.0.) FULGAS(2)=CO2                                       3552.01 
      IF(CO2.GT.0.) FULGAS(2)=CO2/315.
c  ** END (CHANGED)
C     FULGAS(6)=285./295.                       295. IS USED            3552.1  
C     FULGAS(7)=8./14.                           14. IS USED            3552.2  
      FULGAS(8)=1.                                                      3552.3  
      FULGAS(9)=1.                                                      3552.4  
c  ** MFS (CHANGED)
c      KTREND=-CO2                                                       3552.5  
C**** FORSET/GET REDEFINES FULGAS 2,6-9                                 3552.55 
C**** FORSET/GET REDEFINES FULGAS 2,6-9                                 3552.6  
c  ** I use ktrend=1 for my trends
c      IF(KTREND.LE.0) GO TO 50                                          3552.7  
      TREF=1958.                                                        3552.75 
cc      CALL FORSET (TREF,KTREND,0)                                       3552.8  
      KTREND=KTRENDEXT
      IF(KTREND.LE.0) GO TO 50
      CALL FORSET (TREF,KTREND,1)        
cc      CALL FORGET (TREF,1)                                              3552.9  
      CALL FORGET (TREF,KTREND,1)                             
      JDAYR=0                                                           3552.99 
   50 CALL RCOMP1 (21,0,0)                                              3553.   
CVOL  IF (KTREND.GT.0) CALL VOLSET                                      3553.1  
c      IF (KTREND.GT.0) CALL VMSSET (55,1)                               3553.11 
c         CALL WRITER (1,0)                                              3554.  
c  ** END (CHANGED)
      RETURN                                                            3555.   
C****                                                                   3556.   
C****                                                                   3557.   
      ENTRY COSZT (ROT1,ROT2,COSZ)                                      3558.   
C****                                                                   3559.   
C**** THIS ENTRY COMPUTES THE ZENITH ANGLE WEIGHTED BY DAYTIME          3560.   
C**** HOURS FROM ROT1 TO ROT2, GREENWICH MEAN TIME IN RADIANS.  ROT1    3561.   
C**** MUST BE BETWEEN 0 AND 2*PI.  ROT2 MUST BE BETWEEN ROT1 AND        3562.   
C**** ROT1+2*PI.  I=1 MUST LIE ON THE INTERNATIONAL DATE LINE.          3563.   
C****                                                                   3564.   
      DROT=ROT2-ROT1                                                    3565.   
C**** COMPUTE THE SINES AND COSINES OF THE INITIAL AND FINAL GMT'S      3566.   
  100 SR1=SIN(ROT1)                                                     3567.   
      CR1=COS(ROT1)                                                     3568.   
      SR2=SIN(ROT2)                                                     3569.   
      CR2=COS(ROT2)                                                     3570.   
C**** COMPUTE THE INITIAL AND FINAL LOCAL TIMES (MEASURED FROM NOON TO  3571.   
C****   NOON) AND THEIR SINES AND COSINES                               3572.   
      DO 120 I=1,IM                                                     3573.   
      LT1(I)=ROT1+RI(I)                                                 3574.   
      SLT1(I)=SR1*COSI(I)+CR1*SINI(I)                                   3575.   
      LT2(I)=ROT2+RI(I)                                                 3576.   
  120 SLT2(I)=SR2*COSI(I)+CR2*SINI(I)                                   3577.   
C****                                                                   3578.   
C**** CALCULATION FOR POLAR GRID BOXES                                  3579.   
C****                                                                   3580.   
      DO 200 J=1,JM,JM-1                                                3581.   
      SJSD=SINJ(J)*SIND                                                 3582.   
      CJCD=COSJ(J)*COSD                                                 3583.   
      IF(SJSD+CJCD.LE.ZERO1) GO TO 180                                  3584.   
      IF(SJSD-CJCD.GE.0.) GO TO 160                                     3585.   
C**** AVERAGE COSZ FROM DAWN TO DUSK NEAR THE POLES                     3586.   
      DUSK=ACOS(-SJSD/CJCD)                                             3587.   
      SDUSK=SQRT(CJCD*CJCD-SJSD*SJSD)/CJCD                              3588.   
      DAWN=-DUSK                                                        3589.   
      SDAWN=-SDUSK                                                      3590.   
      COSZ(1,J)=(SJSD*(DUSK-DAWN)+CJCD*(SDUSK-SDAWN))/TWOPI             3591.   
      GO TO 200                                                         3592.   
C**** CONSTANT DAYLIGHT NEAR THE POLES                                  3593.   
  160 COSZ(1,J)=SJSD                                                    3594.   
      GO TO 200                                                         3595.   
C**** CONSTANT NIGHTIME NEAR THE POLES                                  3596.   
  180 COSZ(1,J)=0.                                                      3597.   
  200 CONTINUE                                                          3598.   
C****                                                                   3599.   
C**** LOOP OVER NON-POLAR LATITUDES                                     3600.   
C****                                                                   3601.   
      DO 500 J=2,JM-1                                                   3602.   
      SJSD=SINJ(J)*SIND                                                 3603.   
      CJCD=COSJ(J)*COSD                                                 3604.   
      IF(SJSD+CJCD.LE.ZERO1) GO TO 460                                  3605.   
      IF(SJSD-CJCD.GE.0.) GO TO 420                                     3606.   
C**** COMPUTE DAWN AND DUSK (AT LOCAL TIME) AND THEIR SINES             3607.   
      DUSK=ACOS(-SJSD/CJCD)                                             3608.   
      SDUSK=SQRT(CJCD*CJCD-SJSD*SJSD)/CJCD                              3609.   
      DAWN=-DUSK                                                        3610.   
      SDAWN=-SDUSK                                                      3611.   
C**** NEITHER CONSTANT DAYTIME NOR CONSTANT NIGHTIME AT THIS LATITUDE,  3612.   
C**** LOOP OVER LONGITUDES                                              3613.   
      ZERO2=ZERO1/CJCD                                                  3613.5  
      DO 400 I=1,IM                                                     3614.   
C**** FORCE DUSK TO LIE BETWEEN LT1 AND LT1+2*PI                        3615.   
      IF(DUSK.GT.LT1(I)+ZERO2) GO TO 220                                3616.   
      DUSK=DUSK+TWOPI                                                   3617.   
      DAWN=DAWN+TWOPI                                                   3618.   
  220 IF(DAWN.LT.LT2(I)-ZERO2) GO TO 240                                3619.   
C**** CONTINUOUS NIGHTIME FROM INITIAL TO FINAL TIME                    3620.   
      COSZ(I,J)=0.                                                      3621.   
      GO TO 400                                                         3622.   
  240 IF(DAWN.GE.LT1(I)) GO TO 300                                      3623.   
      IF(DUSK.LT.LT2(I)) GO TO 260                                      3624.   
C**** CONTINUOUS DAYLIGHT FROM INITIAL TIME TO FINAL TIME               3625.   
      COSZ(I,J)=SJSD+CJCD*(SLT2(I)-SLT1(I))/DROT                        3626.   
      GO TO 400                                                         3627.   
  260 IF(DAWN+TWOPI.LT.LT2(I)-ZERO2) GO TO 280                          3628.   
C**** DAYLIGHT AT INITIAL TIME AND NIGHT AT FINAL TIME                  3629.   
      COSZ(I,J)=(SJSD*(DUSK-LT1(I))+CJCD*(SDUSK-SLT1(I)))/DROT          3630.   
      GO TO 400                                                         3631.   
C**** DAYLIGHT AT INITIAL AND FINAL TIMES WITH NIGHTIME IN BETWEEN      3632.   
  280 COSZ(I,J)=(SJSD*(LT2(I)-DAWN-TWOPI+DUSK-LT1(I))+CJCD*             3633.   
     *  (SLT2(I)-SDAWN+SDUSK-SLT1(I)))/DROT                             3634.   
      GO TO 400                                                         3635.   
  300 IF(DUSK.LT.LT2(I)) GO TO 320                                      3636.   
C**** NIGHT AT INITIAL TIME AND DAYLIGHT AT FINAL TIME                  3637.   
      COSZ(I,J)=(SJSD*(LT2(I)-DAWN)+CJCD*(SLT2(I)-SDAWN))/DROT          3638.   
      GO TO 400                                                         3639.   
C**** NIGHTIME AT INITIAL AND FINAL TIMES WITH DAYLIGHT IN BETWEEN      3640.   
  320 COSZ(I,J)=(SJSD*(DUSK-DAWN)+CJCD*(SDUSK-SDAWN))/DROT              3641.   
  400 CONTINUE                                                          3642.   
      GO TO 500                                                         3643.   
C**** CONSTANT DAYLIGHT AT THIS LATITUDE                                3644.   
  420 DO 440 I=1,IM                                                     3645.   
  440 COSZ(I,J)=SJSD+CJCD*(SLT2(I)-SLT1(I))/DROT                        3646.   
      GO TO 500                                                         3647.   
C**** CONSTANT NIGHTIME AT THIS LATITUDE                                3648.   
  460 DO 480 I=1,IM                                                     3649.   
  480 COSZ(I,J)=0.                                                      3650.   
  500 CONTINUE                                                          3651.   
      RETURN                                                            3652.   
C****                                                                   3653.   
C****                                                                   3654.   
      ENTRY COSZS (ROT1,ROT2,COSZ,COSZA)                                3655.   
C****                                                                   3656.   
C**** THIS ENTRY COMPUTES THE ZENITH ANGLE TWICE, FIRST WEIGHTED BY THE 3657.   
C**** DAYTIME HOURS FROM ROT1 TO ROT2 AND SECONDLY WEIGHTED BY THE      3658.   
C**** INCIDENT SUN LIGHT FROM ROT1 TO ROT2.  COSZT MUST HAVE BEEN       3659.   
C**** CALLED JUST PREVIOUSLY.                                           3660.   
C****                                                                   3661.   
      DROT=ROT2-ROT1                                                    3662.   
C**** COMPUTE THE SINES AND COSINES OF THE INITIAL AND FINAL GMT'S      3663.   
      SR1=SIN(ROT1)                                                     3664.   
      CR1=COS(ROT1)                                                     3665.   
      SR2=SIN(ROT2)                                                     3666.   
      CR2=COS(ROT2)                                                     3667.   
C**** COMPUTE THE INITIAL AND FINAL LOCAL TIMES (MEASURED FROM NOON TO  3668.   
C****   NOON) AND THEIR SINES AND COSINES                               3669.   
      DO 520 I=1,IM                                                     3670.   
      LT1(I)=ROT1+RI(I)                                                 3671.   
      SLT1(I)=SR1*COSI(I)+CR1*SINI(I)                                   3672.   
      CLT1=CR1*COSI(I)-SR1*SINI(I)                                      3673.   
      S2LT1(I)=2.*SLT1(I)*CLT1                                          3674.   
      LT2(I)=ROT2+RI(I)                                                 3675.   
      SLT2(I)=SR2*COSI(I)+CR2*SINI(I)                                   3676.   
      CLT2=CR2*COSI(I)-SR2*SINI(I)                                      3677.   
  520 S2LT2(I)=2.*SLT2(I)*CLT2                                          3678.   
C****                                                                   3679.   
C**** CALCULATION FOR POLAR GRID BOXES                                  3680.   
C****                                                                   3681.   
      DO 600 J=1,JM,JM-1                                                3682.   
      SJSD=SINJ(J)*SIND                                                 3683.   
      CJCD=COSJ(J)*COSD                                                 3684.   
      IF(SJSD+CJCD.LE.ZERO1) GO TO 580                                  3685.   
      IF(SJSD-CJCD.GE.0.) GO TO 560                                     3686.   
C**** AVERAGE COSZ FROM DAWN TO DUSK NEAR THE POLES                     3687.   
      CDUSK=-SJSD/CJCD                                                  3688.   
      DUSK=ACOS(CDUSK)                                                  3689.   
      SDUSK=SQRT(CJCD*CJCD-SJSD*SJSD)/CJCD                              3690.   
      S2DUSK=2.*SDUSK*CDUSK                                             3691.   
      DAWN=-DUSK                                                        3692.   
      SDAWN=-SDUSK                                                      3693.   
      S2DAWN=-S2DUSK                                                    3694.   
      ECOSZ=SJSD*(DUSK-DAWN)+CJCD*(SDUSK-SDAWN)                         3695.   
      ECOSQZ=SJSD*ECOSZ+CJCD*(SJSD*(SDUSK-SDAWN)+                       3696.   
     *  .5*CJCD*(DUSK-DAWN+.5*(S2DUSK-S2DAWN)))                         3697.   
      COSZ(1,J)=ECOSZ/TWOPI                                             3698.   
      COSZA(1,J)=ECOSQZ/ECOSZ                                           3699.   
      GO TO 600                                                         3700.   
C**** CONSTANT DAYLIGHT NEAR THE POLES                                  3701.   
  560 ECOSZ=SJSD*TWOPI                                                  3702.   
      ECOSQZ=SJSD*ECOSZ+.5*CJCD*CJCD*TWOPI                              3703.   
      COSZ(1,J)=ECOSZ/TWOPI                                             3704.   
      COSZA(1,J)=ECOSQZ/ECOSZ                                           3705.   
      GO TO 600                                                         3706.   
C**** CONSTANT NIGHTIME NEAR THE POLES                                  3707.   
  580 COSZ(1,J)=0.                                                      3708.   
      COSZA(1,J)=0.                                                     3709.   
  600 CONTINUE                                                          3710.   
C****                                                                   3711.   
C**** LOOP OVER NON-POLAR LATITUDES                                     3712.   
C****                                                                   3713.   
      DO 900 J=2,JM-1                                                   3714.   
      SJSD=SINJ(J)*SIND                                                 3715.   
      CJCD=COSJ(J)*COSD                                                 3716.   
      IF(SJSD+CJCD.LE.ZERO1) GO TO 860                                  3717.   
      IF(SJSD-CJCD.GE.0.) GO TO 820                                     3718.   
C**** COMPUTE DAWN AND DUSK (AT LOCAL TIME) AND THEIR SINES             3719.   
      CDUSK=-SJSD/CJCD                                                  3720.   
      DUSK=ACOS(CDUSK)                                                  3721.   
      SDUSK=SQRT(CJCD*CJCD-SJSD*SJSD)/CJCD                              3722.   
      S2DUSK=2.*SDUSK*CDUSK                                             3723.   
      DAWN=-DUSK                                                        3724.   
      SDAWN=-SDUSK                                                      3725.   
      S2DAWN=-S2DUSK                                                    3726.   
C**** NEITHER CONSTANT DAYTIME NOR CONSTANT NIGHTIME AT THIS LATITUDE,  3727.   
C**** LOOP OVER LONGITUDES                                              3728.   
      ZERO2=ZERO1/CJCD                                                  3728.5  
      DO 800 I=1,IM                                                     3729.   
C**** FORCE DUSK TO LIE BETWEEN LT1 AND LT1+2*PI                        3730.   
      IF(DUSK.GT.LT1(I)+ZERO2) GO TO 620                                3731.   
      DUSK=DUSK+TWOPI                                                   3732.   
      DAWN=DAWN+TWOPI                                                   3733.   
  620 IF(DAWN.LT.LT2(I)-ZERO2) GO TO 640                                3734.   
C**** CONTINUOUS NIGHTIME FROM INITIAL TO FINAL TIME                    3735.   
      COSZ(I,J)=0.                                                      3736.   
      COSZA(I,J)=0.                                                     3737.   
      GO TO 800                                                         3738.   
  640 IF(DAWN.GE.LT1(I)) GO TO 700                                      3739.   
      IF(DUSK.LT.LT2(I)) GO TO 660                                      3740.   
C**** CONTINUOUS DAYLIGHT FROM INITIAL TIME TO FINAL TIME               3741.   
      ECOSZ=SJSD*DROT+CJCD*(SLT2(I)-SLT1(I))                            3742.   
      ECOSQZ=SJSD*ECOSZ+CJCD*(SJSD*(SLT2(I)-SLT1(I))+                   3743.   
     *  .5*CJCD*(DROT+.5*(S2LT2(I)-S2LT1(I))))                          3744.   
      COSZ(I,J)=ECOSZ/DROT                                              3745.   
      COSZA(I,J)=ECOSQZ/ECOSZ                                           3746.   
      GO TO 800                                                         3747.   
  660 IF(DAWN+TWOPI.LT.LT2(I)-ZERO2) GO TO 680                          3748.   
C**** DAYLIGHT AT INITIAL TIME AND NIGHT AT FINAL TIME                  3749.   
      ECOSZ=SJSD*(DUSK-LT1(I))+CJCD*(SDUSK-SLT1(I))                     3750.   
      ECOSQZ=SJSD*ECOSZ+CJCD*(SJSD*(SDUSK-SLT1(I))+                     3751.   
     *  .5*CJCD*(DUSK-LT1(I)+.5*(S2DUSK-S2LT1(I))))                     3752.   
      COSZ(I,J)=ECOSZ/DROT                                              3753.   
      COSZA(I,J)=ECOSQZ/ECOSZ                                           3754.   
      GO TO 800                                                         3755.   
C**** DAYLIGHT AT INITIAL AND FINAL TIMES WITH NIGHTIME IN BETWEEN      3756.   
  680 ECOSZ=SJSD*(DROT-DAWN-TWOPI+DUSK)+                                3757.   
     *  CJCD*(SLT2(I)-SDAWN+SDUSK-SLT1(I))                              3758.   
      ECOSQZ=SJSD*ECOSZ+CJCD*(SJSD*(SDUSK-SLT1(I)+SLT2(I)-SDAWN)+       3759.   
     *  .5*CJCD*(DUSK+DROT-DAWN-TWOPI+                                  3760.   
     *  .5*(S2DUSK-S2LT1(I)+S2LT2(I)-S2DAWN)))                          3761.   
      COSZ(I,J)=ECOSZ/DROT                                              3762.   
      COSZA(I,J)=ECOSQZ/ECOSZ                                           3763.   
      GO TO 800                                                         3764.   
  700 IF(DUSK.LT.LT2(I)) GO TO 720                                      3765.   
C**** NIGHT AT INITIAL TIME AND DAYLIGHT AT FINAL TIME                  3766.   
      ECOSZ=SJSD*(LT2(I)-DAWN)+CJCD*(SLT2(I)-SDAWN)                     3767.   
      ECOSQZ=SJSD*ECOSZ+CJCD*(SJSD*(SLT2(I)-SDAWN)+                     3768.   
     *  .5*CJCD*(LT2(I)-DAWN+.5*(S2LT2(I)-S2DAWN)))                     3769.   
      COSZ(I,J)=ECOSZ/DROT                                              3770.   
      COSZA(I,J)=ECOSQZ/ECOSZ                                           3771.   
      GO TO 800                                                         3772.   
C**** NIGHTIME AT INITIAL AND FINAL TIMES WITH DAYLIGHT IN BETWEEN      3773.   
  720 ECOSZ=SJSD*(DUSK-DAWN)+CJCD*(SDUSK-SDAWN)                         3774.   
      ECOSQZ=SJSD*ECOSZ+CJCD*(SJSD*(SDUSK-SDAWN)+                       3775.   
     *  .5*CJCD*(DUSK-DAWN+.5*(S2DUSK-S2DAWN)))                         3776.   
      COSZ(I,J)=ECOSZ/DROT                                              3777.   
      COSZA(I,J)=ECOSQZ/ECOSZ                                           3778.   
  800 CONTINUE                                                          3779.   
      GO TO 900                                                         3780.   
C**** CONSTANT DAYLIGHT AT THIS LATITUDE                                3781.   
  820 DO 840 I=1,IM                                                     3782.   
      ECOSZ=SJSD*DROT+CJCD*(SLT2(I)-SLT1(I))                            3783.   
      ECOSQZ=SJSD*ECOSZ+CJCD*(SJSD*(SLT2(I)-SLT1(I))+                   3784.   
     *  .5*CJCD*(DROT+.5*(S2LT2(I)-S2LT1(I))))                          3785.   
      COSZ(I,J)=ECOSZ/DROT                                              3786.   
  840 COSZA(I,J)=ECOSQZ/ECOSZ                                           3787.   
      GO TO 900                                                         3788.   
C**** CONSTANT NIGHTIME AT THIS LATITUDE                                3789.   
  860 DO 880 I=1,IM                                                     3790.   
      COSZ(I,J)=0.                                                      3791.   
  880 COSZA(I,J)=0.                                                     3792.   
  900 CONTINUE                                                          3793.   
      RETURN                                                            3794.   
      END                                                               3795.   
      SUBROUTINE RADIA                                                  4001.   
C****                                                                   4002.   
C**** THIS SUBROUTINES ADDS THE RADIATION HEATING TO THE TEMPERATURES   4003.   
C****                                                                   4004.   
      INCLUDE 'BA94jalC9.COM'                                           4005.   
c  ** MFS (ADDED)
      INCLUDE 'FORCINGSmac.COM'
c  ** END (ADDED)
      COMMON U,V,T,P,Q                                                  4006.   
      COMMON/WORK1/CONV(IM,JM,LM),PK(IM,JM,LM),PREC(IM,JM),             4007.   
     *  TPREC(IM,JM),COSZ1(IM,JM),COSZ2(IM,JM),COSZA(IM,JM),            4008.   
     *  TRINCG(IM,JM),BTMPW(IM,JM),SNFS(IM,JM,4),TNFS(IM,JM,4),         4009.   
     *  TRHRS(IM,JM,3),SRHRS(IM,JM,3),ALB(IM,JM,9)                      4010.   
      COMMON/WORK2/CLDSS(IM,JM,LM),CLDMC(IM,JM,LM),TOTCLD(LM)
C     COMMON/WORK4/ IS BEING USED BY THE RADIATION ROUTINES             4013.   
C                                                                       4014.   
C                   RADCOM:      CONTROL/INPUT PARAMETERS               4015.   
C                                                                       4016.   
      COMMON/RADCOM/VADATA(11,4,3),DGLAT(46),DGLON(72),TMINSR,FULGAS(18)4017.   
     A             ,FRACSL,RATQSL,FOGTSL,PTLISO,TLGRAD,TKCICE,FGOLDU(18)4018.   
     B             ,FLONO3,FRAYLE,FCLDTR,FCLDSR,FALGAE,FMARCL,FEMTRA(6) 4019.   
     C             ,WETTRA,WETSRA,DMOICE,DMLICE,LICETK,NTRCE,FZASRA(6)  4020.   
     D             ,ID5(5),ITR(4),IMG(2),ILG(2),LAPGAS,KWVCON,NORMS0,NV 4021.   
     E             ,KEEPRH,KEEPAL,ISOSCT,IHGSCT,KFRACC,KGASSR,KAERSR    4022.   
     F             ,MARCLD,LAYTOP,LMR,LMRP,JMLAT,IMLON,KFORCE,LASTVC    4023.   
C                                                                       4024.   
C                                BASIC RADCOM INPUT DATA                4025.   
C                                                                       4026.   
     G             ,PLE(40),HLB(40),TLB(40),TLT(40),TL(40),U0GAS(40,9)  4027.   
     H             ,ULGAS(40,9),TRACER(40,4),RTAU(40),QL(40),RHL(40)    4028.   
     I             ,POCEAN,PEARTH,POICE,PLICE,AGESN,SNOWE,SNOWOI,SNOWLI 4029.   
     J             ,TGO,TGE,TGOI,TGLI,TS,WS,WEARTH,ZOICE,FSPARE(200)    4030.   
     K             ,S0,COSZ,PVT(11),BXA(153),SRBXAL(15,2),FRC(5),LUXGAS 4031.   
     L             ,JYEARR,JDAYR,JLAT,ILON,MEANAL,KALVIS,ISPARE(25),SGPS4032.   
C                                                                       4033.   
C                                BASIC RADCOM OUTPUT DATA               4034.   
C                                                                       4035.   
     M             ,TRDFLB(40),TRUFLB(40),TRNFLB(40),TRFCRL(40),TRSLCR  4036.   
     N             ,SRDFLB(40),SRUFLB(40),SRNFLB(40),SRFHRL(40),SRSLHR  4037.   
     O             ,SRIVIS,SROVIS,PLAVIS,SRINIR,SRONIR,PLANIR,SRXATM(4) 4038.   
     P             ,SRDVIS,SRUVIS,ALBVIS,SRDNIR,SRUNIR,ALBNIR,FSRNFG(4) 4039.   
     Q             ,SRTVIS,SRRVIS,SRAVIS,SRTNIR,SRRNIR,SRANIR,FTRUFG(4) 4040.   
     R             ,TRDFGW,TRUFGW,TRUFTW,BTEMPW,TRDFSL,TRUFSL,DTRUFG(4) 4041.   
     S             ,TRSLTS,TRSLTG,TRSLWV,TRSLBS,TTRUFG,LBOTCL,LTOPCL    4042.   
      DIMENSION COE(LM+3)                                               4043.   
      LOGICAL POLE                                                      4044.   
      DATA TF/273.16/,TCIR/258.16/,STBO/.567257D-7/,IFIRST/1/,JDLAST/-9/4045.   
C****                                                                   4046.   
C**** FDATA  2  LAND COVERAGE (1)                                       4047.   
C****        3  RATIO OF LAND ICE COVERAGE TO LAND COVERAGE (1)         4048.   
C****                                                                   4049.   
C**** ODATA  1  OCEAN TEMPERATURE (C)                                   4050.   
C****        2  RATIO OF OCEAN ICE COVERAGE TO WATER COVERAGE (1)       4051.   
C****                                                                   4052.   
C**** GDATA  1  OCEAN ICE SNOW AMOUNT (KG/M**2)                         4053.   
C****        2  EARTH SNOW AMOUNT (KG/M**2)                             4054.   
C****        3  OCEAN ICE TEMPERATURE OF FIRST LAYER (C)                4055.   
C****        4  EARTH TEMPERATURE OF FIRST LAYER (C)                    4056.   
C****        5  EARTH WATER OF FIRST LAYER (KG/M**2)                    4057.   
C****        6  EARTH ICE OF FIRST LAYER (KG/M**2)                      4058.   
C****       11  AGE OF SNOW (DAYS)                                      4059.   
C****       12  LAND ICE SNOW AMOUNT (KG/M**2)                          4060.   
C****       13  LAND ICE TEMPERATURE OF FIRST LAYER (C)                 4061.   
C****                                                                   4062.   
C**** BLDATA 1  COMPOSITE SURFACE WIND MAGNITUDE (M/S)                  4063.   
C****        2  COMPOSITE SURFACE AIR TEMPERATURE (K)                   4064.   
C****        5  MIXED LAYER DEPTH (Z1O NOT YET PART OF RESTART FILE)    4065.   
C****                                                                   4066.   
C**** VDATA  1-8  EARTH RATIOS FOR THE 8 VEGETATION TYPES (1)           4067.   
C****        9  WATER FIELD CAPACITY OF FIRST LAYER (KG/M**2)           4068.   
C****                                                                   4069.   
         IF(MODRD.EQ.0) IDACC(2)=IDACC(2)+1                             4070.   
      IF(IFIRST.NE.1) GO TO 50                                          4071.   
      IFIRST=0                                                          4072.   
      LMP1=LM+1                                                         4073.   
      DTCNDS=NCNDS*DT                                                   4074.   
C**** SET THE CONTROL PARAMETERS FOR THE RADIATION                      4075.   
      JMLAT=JM                                                          4075.1  
      IMLON=IM                                                          4075.2  
      LMR=LM+3                                                          4076.   
      COEX=.01*GRAV*KAPA/RGAS                                           4077.   
      PSFMPT=PSF-PTOP                                                   4078.   
      DO 30 L=1,LM                                                      4079.   
      COE(L)=DTCNDS*COEX/DSIG(L)                                        4080.   
   30 PLE(L)=SIGE(L)*(PSF-PTOP)+PTOP                                    4081.   
      PLE(LM+1)=PTOP                                                    4082.   
      PLE(LM+2)=.5*PTOP                                                 4083.   
      PLE(LMR)=.2*PTOP                                                  4084.   
      PLE(LMR+1)=1.D-5                                                  4085.   
      DO 40 LR=LMP1,LMR                                                 4086.   
      COE(LR)=DT*NRAD*COEX/(PLE(LR)-PLE(LR+1))                          4087.   
Cerr  QL(LR)=.3D-5  is reset in RADIA0 (RCOMP1)                         4088.   
   40 RTAU(LR)=0.                                                       4089.   
      CALL RADIA0                                                       4091.   
         INCHM=NRAD/NDYN                                                4092.   
         JEQ=1+JM/2                                                     4092.1  
         J50N=(50.+90.)*JMM1/180.+1.5                                   4092.2  
         J70N=(70.+90.)*JMM1/180.+1.5                                   4092.3  
C**** CLOUD LAYER INDICES USED FOR DIAGNOSTICS                          4093.   
         DO 43 L=1,LTM                                                  4094.   
         LLOW=L                                                         4095.   
         IF(.5*(PLE(L+1)+PLE(L+2)).LT.786.) GO TO 44                    4096.   
   43    CONTINUE                                                       4097.   
   44    LMID1=LLOW+1                                                   4098.   
         DO 45 L=LMID1,LTM                                              4099.   
         LMID=L                                                         4100.   
         IF(.5*(PLE(L+1)+PLE(L+2)).LT.430.) GO TO 46                    4101.   
   45    CONTINUE                                                       4102.   
   46    LHI1=LMID+1                                                    4103.   
         LHI=LTM                                                        4104.   
         IF(LHI1.GT.LHI) LHI=LHI1                                       4105.   
         WRITE (6,47) LLOW,LMID1,LMID,LHI1,LHI                          4106.   
   47    FORMAT (' LOW CLOUDS IN LAYERS 1-',I2,'   MID LEVEL CLOUDS IN',4107.   
     *     ' LAYERS',I3,'-',I2,'   HIGH CLOUDS IN LAYERS',I3,'-',I2)    4108.   
C**** NO RADIATION AVERAGING          IJRA=1   JRA=1   IRA=1            4109.   
C**** RADIATION AVERAGING IN I             2       1       2            4110.   
C**** RADIATION AVERAGING IN I AND J       4       2       2            4111.   
      JRA=(IJRA+2)/3                                                    4112.   
      IRA=IJRA/JRA                                                      4113.   
   50 JALTER=MOD(NSTEP,NRAD*JRA)/NRAD                                   4114.   
      IALTER=MOD(NSTEP,NRAD*IJRA)/(NRAD*JRA)                            4115.     
        YEAR=JYEAR+(JDAY-1.+TOFDAY/24.)/365.                            4115.5  
CS0X    S0X=1.+.0005*COS(TWOPI*(YEAR-1980.82)/10.95)                    4115.6
c  ** MFS (ADDED)
c  ** code to call SOLARFORCINGS for S0X trend
      IF((IS0XDATA.GT.0).AND.((JDAY.EQ.1).OR.(TAU.EQ.TAUI)))THEN
        CALL SOLARFORCING
      END IF
c      S0=S0X*1367./RSDIST                                               4116.   
      S0=S0X/RSDIST                               
c  ** END (ADDED)  
C**** CALCULATE AVERAGE COSINE OF ZENITH ANGLE FOR CURRENT COMP3 STEP   4117.   
C****   AND RADIATION PERIOD                                            4118.   
      ROT1=TWOPI*TOFDAY/24.                                             4119.   
      ROT2=ROT1+TWOPI*DTCNDS/SDAY                                       4120.   
      CALL COSZT (ROT1,ROT2,COSZ1)                                      4121.   
      IF(MODRD.NE.0) GO TO 840                                          4122.   
      ROT2=ROT1+TWOPI*NRAD*DT/SDAY                                      4123.   
      CALL COSZS (ROT1,ROT2,COSZ2,COSZA)                                4124.   
C****                                                                   4125.   
C**** COMPUTE EARTH ALBEDOS AND OTHER PARAMETERS FOR BEGINNING OF DAY   4126.   
C****                                                                   4127.   
      JDAYR=JDAY                                                        4128.   
      JYEARR=JYEAR                                                      4128.5  
      TNOW=YEAR                                                         4128.6  
c  ** MFS (DELETED)
c      KTREND=-CO2                                                       4128.7  
c  ** END (DELETED)
c  ** MFS/RAR (CHANGED)
c  ** I use ktrend=1 for my trends and replace Jim's ATrend
c      IF (JDAY.NE.JDLAST.AND.KTREND.GT.0) CALL FORGET(TNOW,IDACC(11))   4128.8  
      KTREND=KTRENDEXT
      IF (JDAY.NE.JDLAST.AND.KTREND.GT.0) CALL FORGET(TNOW,KTREND,1) 
c  ** END (CHANGED)
      IF(JDAY.NE.JDLAST) CALL RCOMPT                                    4129.   
      JDLAST=JDAY                                                       4130.   
         IHOUR=1.5+TOFDAY                                               4131.   
C****                                                                   4132.   
C**** MAIN J LOOP                                                       4133.   
C****                                                                   4134.   
      DO 600 J=1,JM                                                     4135.   
      IF((J-1)*(JM-J).NE.0) GO TO 140                                   4136.   
C**** CONDITIONS AT THE POLES                                           4137.   
      POLE=.TRUE.                                                       4138.   
      MODRJ=0                                                           4139.   
      IMAX=1                                                            4140.   
      GO TO 160                                                         4141.   
C**** CONDITIONS AT NON-POLAR POINTS                                    4142.   
  140 POLE=.FALSE.                                                      4143.   
      MODRJ=MOD(J+JALTER,JRA)                                           4144.   
      IMAX=IM                                                           4145.   
  160 XFRADJ=.2+1.2*COSP(J)*COSP(J)                                     4146.   
      JLAT=J                                                            4146.5 
      
c  ** MFS (ADDED) 
c  ** Volcanic aerosol code is called here.  I'm not sure that this 
c  ** needs deep ocean either but I'll force it to for now.
CVOL    IF(MODRJ.EQ.0) CALL VOLGET (TNOW,0)                             4146.55 
c  ** uncomment the next line to enable volcanoes!!!
c        IF(MODRJ.EQ.0) CALL VMSGET (TNOW,1)                             4146.551
c  ** END (ADDED)          
       
      IF(MODRJ.EQ.0) CALL RCOMPJ                                        4147.   
C****                                                                   4148.   
C**** MAIN I LOOP                                                       4149.   
C****                                                                   4150.   
      IM1=IM                                                            4151.   
      DO 500 I=1,IMAX                                                   4152.   
      MODRIJ=MODRJ+MOD(I+IALTER,IRA)                                    4153.   
      IF(POLE) MODRIJ=0                                                 4154.   
         JR=JREG(I,J)                                                   4155.   
C**** DETERMINE FRACTIONS FOR SURFACE TYPES AND COLUMN PRESSURE         4156.   
      PLAND=FDATA(I,J,2)                                                4157.   
      POICE=ODATA(I,J,2)*(1.-PLAND)                                     4158.   
      POCEAN=(1.-PLAND)-POICE                                           4159.   
      PLICE=FDATA(I,J,3)*PLAND                                          4160.   
      PEARTH=PLAND-PLICE                                                4161.   
      PIJ=P(I,J)                                                        4162.   
C****                                                                   4163.   
C**** DETERMINE CLOUDS (AND THEIR OPTICAL DEPTHS) SEEN BY RADIATION     4164.   
C****                                                                   4165.   
      RANDSS=RANDU(X)                                                   4166.   
      RANDMC=RANDU(X)                                                   4167.   
         CSS=0.                                                         4168.   
         CMC=0.                                                         4169.   
         DEPTH=0.                                                       4170.   
CF       LTOP=0                                                         4170.5  
      DO 210 L=1,LM                                                     4171.   
      RTAU(L)=0.                                                        4172.   
  210    TOTCLD(L)=0.                                                   4173.   
CORR  DO 240 L=1,LM     WAS REPLACED BY NEXT LINE BY AN OVERSIGHT       4174.   
      DO 240 L=1,LTM                                                    4174.001
      IF(CLDSS(I,J,L).LE.RANDSS) GO TO 220                              4175.   
      RTAUSS=.013333*(PTOP-100.+SIG(L)*PIJ)                             4176.   
      IF(RTAUSS.LT.0.) RTAUSS=0.                                        4177.   
      IF(T(I,J,L)*PK(I,J,L).LT.TCIR) RTAUSS=.3333333                    4178.   
      RTAU(L)=RTAUSS                                                    4179.   
         CSS=1.                                                         4180.   
         AJL(J,L,28)=AJL(J,L,28)+CSS                                    4181.   
         TOTCLD(L)=1.                                                   4182.   
CF       LTOP=L                                                         4182.5  
  220 IF(CLDMC(I,J,L).LE.RANDMC) GO TO 240                              4183.   
      RTAUMC=DSIG(L)*PIJ*.08                                            4184.   
      IF(RTAUMC.GT.RTAU(L)) RTAU(L)=RTAUMC                              4185.   
         CMC=1.                                                         4186.   
         AJL(J,L,29)=AJL(J,L,29)+CMC                                    4187.   
         TOTCLD(L)=1.                                                   4188.   
CF       LTOP=L                                                         4188.5  
         DEPTH=DEPTH+PIJ*DSIG(L)                                        4189.   
  240    AJL(J,L,19)=AJL(J,L,19)+TOTCLD(L)                              4190.   
         AJ(J,57)=AJ(J,57)+CSS*POCEAN                                   4191.   
         BJ(J,57)=BJ(J,57)+CSS*PLAND                                    4192.   
         CJ(J,57)=CJ(J,57)+CSS*POICE                                    4193.   
         DJ(JR,57)=DJ(JR,57)+CSS*DXYP(J)                                4194.   
         AJ(J,58)=AJ(J,58)+CMC*POCEAN                                   4195.   
         BJ(J,58)=BJ(J,58)+CMC*PLAND                                    4196.   
         CJ(J,58)=CJ(J,58)+CMC*POICE                                    4197.   
         DJ(JR,58)=DJ(JR,58)+CMC*DXYP(J)                                4198.   
         AIJ(I,J,17)=AIJ(I,J,17)+CMC                                    4199.   
         AJ(J,80)=AJ(J,80)+DEPTH*POCEAN                                 4200.   
         BJ(J,80)=BJ(J,80)+DEPTH*PLAND                                  4201.   
         CJ(J,80)=CJ(J,80)+DEPTH*POICE                                  4202.   
         DJ(JR,80)=DJ(JR,80)+DEPTH*DXYP(J)                              4203.   
         CLDCV=CMC+CSS-CMC*CSS                                          4204.   
         AJ(J,59)=AJ(J,59)+CLDCV*POCEAN                                 4205.   
         BJ(J,59)=BJ(J,59)+CLDCV*PLAND                                  4206.   
         CJ(J,59)=CJ(J,59)+CLDCV*POICE                                  4207.   
         DJ(JR,59)=DJ(JR,59)+CLDCV*DXYP(J)                              4208.   
         AIJ(I,J,19)=AIJ(I,J,19)+CLDCV                                  4209.   
         DO 250 L=1,LLOW                                                4210.   
         IF(TOTCLD(L).NE.1.) GO TO 250                                  4211.   
         AIJ(I,J,41)=AIJ(I,J,41)+1.                                     4212.   
         GO TO 255                                                      4213.   
  250    CONTINUE                                                       4214.   
  255    DO 260 L=LMID1,LMID                                            4215.   
         IF(TOTCLD(L).NE.1.) GO TO 260                                  4216.   
         AIJ(I,J,42)=AIJ(I,J,42)+1.                                     4217.   
         GO TO 265                                                      4218.   
  260    CONTINUE                                                       4219.   
  265    DO 270 L=LHI1,LHI                                              4220.   
         IF(TOTCLD(L).NE.1.) GO TO 270                                  4221.   
         AIJ(I,J,43)=AIJ(I,J,43)+1.                                     4222.   
         GO TO 275                                                      4223.   
  270    CONTINUE                                                       4224.   
  275    DO 280 LX=1,LTM                                                4225.   
         L=1+LTM-LX                                                     4226.   
         IF(TOTCLD(L).NE.1.) GO TO 280                                  4227.   
         AIJ(I,J,18)=AIJ(I,J,18)+SIGE(L+1)*PIJ+PTOP                     4228.   
         GO TO 285                                                      4229.   
  280    CONTINUE                                                       4230.   
  285    DO 290 KR=1,4                                                  4231.   
         IF(I.EQ.IJD6(1,KR).AND.J.EQ.IJD6(2,KR)) GO TO 292              4232.   
  290    CONTINUE                                                       4233.   
         GO TO 300                                                      4234.   
  292    IH=IHOUR                                                       4235.   
         DO 294 INCH=1,INCHM                                            4236.   
         IF(IH.GT.24) IH=IH-24                                          4237.   
         ADAILY(IH,21,KR)=ADAILY(IH,21,KR)+TOTCLD(6)                    4238.   
         ADAILY(IH,22,KR)=ADAILY(IH,22,KR)+TOTCLD(5)                    4239.   
         ADAILY(IH,23,KR)=ADAILY(IH,23,KR)+TOTCLD(4)                    4240.   
         ADAILY(IH,24,KR)=ADAILY(IH,24,KR)+TOTCLD(3)                    4241.   
         ADAILY(IH,25,KR)=ADAILY(IH,25,KR)+TOTCLD(2)                    4242.   
         ADAILY(IH,26,KR)=ADAILY(IH,26,KR)+TOTCLD(1)                    4243.   
         ADAILY(IH,27,KR)=ADAILY(IH,27,KR)+CLDCV                        4244.   
  294    IH=IH+1                                                        4245.   
C****                                                                   4246.   
  300 IF(MODRIJ.NE.0) GO TO 500                                         4247.   
C****                                                                   4248.   
C**** SET UP VERTICAL ARRAYS OMITTING THE I AND J INDICES               4249.   
C****                                                                   4250.   
C**** EVEN PRESSURES                                                    4251.   
      DO 340 L=1,LM                                                     4252.   
      PLE(L)=SIGE(L)*PIJ+PTOP                                           4253.   
C**** TEMPERATURES                                                      4254.   
      TL(L)=T(I,J,L)*PK(I,J,L)                                          4255.   
C**** MOISTURE VARIABLES                                                4256.   
      QL(L)=Q(I,J,L)                                                    4257.   
  340 CONTINUE                                                          4258.   
C****                                                                   4259.   
C**** RADIATION, SOLAR AND THERMAL                                      4260.   
C****                                                                   4261.   
      DO 420 K=1,3                                                      4262.   
  420 TL(LM+K)=RQT(I,J,K)                                               4263.   
      COSZ=COSZA(I,J)                                                   4264.   
      TGO=ODATA(I,J,1)+TF                                               4265.   
      TGOI=GDATA(I,J,3)+TF                                              4266.   
      TGLI=GDATA(I,J,13)+TF                                             4267.   
      TGE=GDATA(I,J,4)+TF                                               4268.   
      TS=BLDATA(I,J,2)                                                  4269.   
      SNOWOI=GDATA(I,J,1)                                               4270.   
      SNOWLI=GDATA(I,J,12)                                              4271.   
      SNOWE=GDATA(I,J,2)                                                4272.   
      AGESN=GDATA(I,J,11)                                               4273.   
      WEARTH=(GDATA(I,J,5)+GDATA(I,J,6))/(VDATA(I,J,9)+1.D-20)          4274.   
      DO 430 K=1,8                                                      4275.   
  430 PVT(K)=VDATA(I,J,K)                                               4276.   
      WS=BLDATA(I,J,1)                                                  4277.   
      FGOLDU(2)=XFRADJ*(1.-PEARTH)                                      4278.   
      FGOLDU(3)=XFRADJ*PEARTH                                           4279.   
      ILON=I                                                            4279.5  
      JLAT=J                                                            4279.6  
      CALL RCOMPX                                                       4280.   
C     CALL WRITER (13,0)                                                4281.   
      SRHR(I,J,1)=SRNFLB(1)                                             4282.   
      TRHR(I,J,1)=STBO*(POCEAN*TGO**4+POICE*TGOI**4+PLICE*TGLI**4       4283.   
     *  +PEARTH*TGE**4)-TRNFLB(1)                                       4284.   
      DO 440 L=1,LM                                                     4285.   
      SRHR(I,J,L+1)=SRFHRL(L)                                           4286.   
  440 TRHR(I,J,L+1)=-TRFCRL(L)                                          4287.   
      DO 450 LR=1,3                                                     4288.   
      SRHRS(I,J,LR)=SRFHRL(LM+LR)                                       4289.   
  450 TRHRS(I,J,LR)=-TRFCRL(LM+LR)                                      4290.   
      DO 460 K=1,4                                                      4291.   
      SNFS(I,J,K)=SRNFLB(K+LM)                                          4292.   
  460 TNFS(I,J,K)=TRNFLB(K+LM)-TRNFLB(1)                                4293.   
         TRINCG(I,J)=TRDFLB(1)                                          4294.   
         BTMPW(I,J)=BTEMPW-TF                                           4295.   
         ALB(I,J,1)=SRNFLB(1)/(SRDFLB(1)+1.D-20)                        4296.   
         ALB(I,J,2)=PLAVIS                                              4297.   
         ALB(I,J,3)=PLANIR                                              4298.   
         ALB(I,J,4)=ALBVIS                                              4299.   
         ALB(I,J,5)=ALBNIR                                              4300.   
         ALB(I,J,6)=SRRVIS                                              4301.   
         ALB(I,J,7)=SRRNIR                                              4302.   
         ALB(I,J,8)=SRAVIS                                              4303.   
         ALB(I,J,9)=SRANIR                                              4304.   
  500 IM1=I                                                             4305.   
C****                                                                   4306.   
C**** END OF MAIN LOOP FOR I INDEX                                      4307.   
C****                                                                   4308.   
      IF(MODRJ.GT.IRA-2) GO TO 600                                      4309.   
      IF(POLE) GO TO 600                                                4310.   
C**** AVERAGING RADIATION NUMBERS AT ROW J AND EVERY OTHER COLUMN IN I  4311.   
      IM1=IM-IALTER                                                     4312.   
      I=IM1+1                                                           4313.   
      IF(I.GT.IM) I=1                                                   4314.   
      IP11=2-IALTER                                                     4315.   
      DO 580 IP1=IP11,IM,2                                              4316.   
         JR=JREG(I,J)                                                   4317.   
      SUMSR=0.                                                          4318.   
      SUMTR=0.                                                          4319.   
      DO 520 L=2,LMP1                                                   4320.   
      SRHR(I,J,L)=P(I,J)*.5*(SRHR(IM1,J,L)/P(IM1,J)+                    4321.   
     *  SRHR(IP1,J,L)/P(IP1,J))                                         4322.   
      SUMSR=SUMSR+SRHR(I,J,L)                                           4323.   
      TRHR(I,J,L)=P(I,J)*.5*(TRHR(IM1,J,L)/P(IM1,J)+                    4324.   
     *  TRHR(IP1,J,L)/P(IP1,J))                                         4325.   
  520 SUMTR=SUMTR+TRHR(I,J,L)                                           4326.   
      DO 530 LR=1,3                                                     4327.   
C     CORRECTION FOR THE RESTART OF RUN A05 W9 AT YEAR 51               4328.   
      SRHRS(I,J,LR)=.5*(SRHRS(IM1,J,LR)+SRHRS(IP1,J,LR))                4329.   
  530 TRHRS(I,J,LR)=.5*(TRHRS(IM1,J,LR)+TRHRS(IP1,J,LR))                4330.   
         DENOM=1./(COSZ2(IM1,J)+COSZ2(IP1,J)+1.D-20)                    4332.   
         DO 540 K=1,9                                                   4333.   
  540    ALB(I,J,K)=(ALB(IM1,J,K)*COSZ2(IM1,J)+ALB(IP1,J,K)             4334.   
     *     *COSZ2(IP1,J))*DENOM                                         4335.   
      DTR=SUMTR+.5*(TNFS(IM1,J,1)+TNFS(IP1,J,1))                        4336.   
      DO 560 K=1,4                                                      4337.   
      SNFS(I,J,K)=.5*(SNFS(IM1,J,K)+SNFS(IP1,J,K))                      4338.   
  560 TNFS(I,J,K)=.5*(TNFS(IM1,J,K)+TNFS(IP1,J,K))-DTR                  4339.   
      SRHR(I,J,1)=SNFS(I,J,1)-SUMSR                                     4340.   
      TRHR(I,J,1)=.5*(TRHR(IM1,J,1)+TRHR(IP1,J,1))                      4341.   
         TRINCG(I,J)=.5*(TRINCG(IM1,J)+TRINCG(IP1,J))                   4342.   
         BTMPW(I,J)=.5*(BTMPW(IM1,J)+BTMPW(IP1,J))                      4343.   
      IM1=IP1                                                           4344.   
  580 I=IM1+1                                                           4345.   
  600 CONTINUE                                                          4346.   
C****                                                                   4347.   
C**** END OF MAIN LOOP FOR J INDEX                                      4348.   
C****                                                                   4349.   
      IF(JRA.LE.1) GO TO 700                                            4350.   
C**** AVERAGING RADIATION NUMBERS AT EVERY OTHER ROW IN J               4351.   
      DO 620 K=1,LM*2+2                                                 4353.   
      DO 620 I=2,IM                                                     4354.   
      SRHR(I,1,K)=SRHR(1,1,K)                                           4355.   
  620 SRHR(I,JM,K)=SRHR(1,JM,K)                                         4356.   
      DO 640 K=1,14                                                     4357.   
      DO 640 I=2,IM                                                     4358.   
      SNFS(I,1,K)=SNFS(1,1,K)                                           4359.   
  640 SNFS(I,JM,K)=SNFS(1,JM,K)                                         4360.   
      J1=3-JALTER                                                       4361.   
      DO 690 J=J1,JM-1,2                                                4362.   
      JP1=J+1                                                           4363.   
      JM1=J-1                                                           4364.   
      DO 690 I=1,IM                                                     4365.   
         JR=JREG(I,J)                                                   4366.   
      SUMSR=0.                                                          4367.   
      SUMTR=0.                                                          4368.   
      DO 660 L=2,LMP1                                                   4369.   
      SRHR(I,J,L)=P(I,J)*.5*(SRHR(I,JP1,L)/P(I,JP1)+                    4370.   
     *  SRHR(I,JM1,L)/P(I,JM1))                                         4371.   
      SUMSR=SUMSR+SRHR(I,J,L)                                           4372.   
      TRHR(I,J,L)=P(I,J)*.5*(TRHR(I,JP1,L)/P(I,JP1)+                    4373.   
     *  TRHR(I,JM1,L)/P(I,JM1))                                         4374.   
  660 SUMTR=SUMTR+TRHR(I,J,L)                                           4375.   
      DO 665 LR=1,3                                                     4376.   
      SRHRS(I,J,LR)=P(I,J)*.5*(SRHRS(I,JP1,LR)/P(I,JP1)+                4377.   
     *  SRHRS(I,JM1,LR)/P(I,JM1))                                       4378.   
  665 TRHRS(I,J,LR)=P(I,J)*.5*(TRHRS(I,JP1,LR)/P(I,JP1)+                4379.   
     *  TRHRS(I,JM1,LR)/P(I,JM1))                                       4380.   
         DENOM=1./(COSZ2(I,JP1)+COSZ2(I,JM1)+1.D-20)                    4381.   
         DO 670 K=1,9                                                   4382.   
  670    ALB(I,J,K)=(ALB(I,JP1,K)*COSZ2(I,JP1)+ALB(I,JM1,K)*            4383.   
     *     COSZ2(I,JM1))*DENOM                                          4384.   
      DTR=SUMTR+.5*(TNFS(I,JP1,1)+TNFS(I,JM1,1))                        4385.   
      DO 680 K=1,4                                                      4386.   
      SNFS(I,J,K)=.5*(SNFS(I,JP1,K)+SNFS(I,JM1,K))                      4387.   
  680 TNFS(I,J,K)=.5*(TNFS(I,JP1,K)+TNFS(I,JM1,K))-DTR                  4388.   
      SRHR(I,J,1)=SNFS(I,J,1)-SUMSR                                     4389.   
      TRHR(I,J,1)=.5*(TRHR(I,JP1,1)+TRHR(I,JM1,1))                      4390.   
         TRINCG(I,J)=.5*(TRINCG(I,JP1)+TRINCG(I,JM1))                   4391.   
         BTMPW(I,J)=.5*(BTMPW(I,JP1)+BTMPW(I,JM1))                      4392.   
  690 CONTINUE                                                          4393.   
C****                                                                   4394.   
C**** ACCUMULATE THE RADIATION DIAGNOSTICS                              4395.   
C****                                                                   4396.   
  700 CONTINUE                                                          4396.5  
         DO 780 J=1,JM                                                  4397.   
         DXYPJ=DXYP(J)                                                  4398.   
         IMAX=IM                                                        4399.   
         IF(J.EQ.1.OR.J.EQ.JM) IMAX=1                                   4400.   
         DO 720 L=1,LM                                                  4401.   
         ASRHR=0.                                                       4402.   
         ATRHR=0.                                                       4403.   
         DO 710 I=1,IMAX                                                4404.   
         ASRHR=ASRHR+SRHR(I,J,L+1)*COSZ2(I,J)                           4405.   
  710    ATRHR=ATRHR+TRHR(I,J,L+1)                                      4406.   
         AJL(J,L,9)=AJL(J,L,9)+ASRHR                                    4407.   
  720    AJL(J,L,10)=AJL(J,L,10)+ATRHR                                  4408.   
         ASNFS1=0.                                                      4409.   
         BSNFS1=0.                                                      4410.   
         CSNFS1=0.                                                      4411.   
         ATNFS1=0.                                                      4412.   
         BTNFS1=0.                                                      4413.   
         CTNFS1=0.                                                      4414.   
         DO 770 I=1,IMAX                                                4415.   
         COSZ=COSZ2(I,J)                                                4417.   
         PLAND=FDATA(I,J,2)                                             4418.   
         POICE=ODATA(I,J,2)*(1.-PLAND)                                  4419.   
         POCEAN=(1.-PLAND)-POICE                                        4420.   
         JR=JREG(I,J)                                                   4421.   
         DO 740 LR=1,3                                                  4422.   
         ASJL(J,LR,3)=ASJL(J,LR,3)+SRHRS(I,J,LR)*COSZ                   4423.   
  740    ASJL(J,LR,4)=ASJL(J,LR,4)+TRHRS(I,J,LR)                        4424.   
         DO 742 KR=1,4                                                  4425.   
         IF(I.EQ.IJD6(1,KR).AND.J.EQ.IJD6(2,KR)) GO TO 744              4426.   
  742    CONTINUE                                                       4427.   
         GO TO 750                                                      4428.   
  744    IH=IHOUR                                                       4429.   
         DO 746 INCH=1,INCHM                                            4430.   
         IF(IH.GT.24) IH=IH-24                                          4431.   
         ADAILY(IH,2,KR)=ADAILY(IH,2,KR)+(1.-SNFS(I,J,4)/S0)            4432.   
         ADAILY(IH,3,KR)=ADAILY(IH,3,KR)+(1.-ALB(I,J,1))                4433.   
         ADAILY(IH,4,KR)=ADAILY(IH,4,KR)                                4434.   
     *      +((SNFS(I,J,4)-SNFS(I,J,1))*COSZ-TNFS(I,J,4)+TNFS(I,J,1))   4435.   
  746    IH=IH+1                                                        4436.   
  750    CONTINUE                                                       4437.   
         AJ(J,1)=AJ(J,1)+(S0*COSZ)*POCEAN                               4438.   
         BJ(J,1)=BJ(J,1)+(S0*COSZ)*PLAND                                4439.   
         CJ(J,1)=CJ(J,1)+(S0*COSZ)*POICE                                4440.   
         DJ(JR,1)=DJ(JR,1)+(S0*COSZ)*DXYPJ                              4441.   
         AJ(J,2)=AJ(J,2)+(SNFS(I,J,4)*COSZ)*POCEAN                      4442.   
         BJ(J,2)=BJ(J,2)+(SNFS(I,J,4)*COSZ)*PLAND                       4443.   
         CJ(J,2)=CJ(J,2)+(SNFS(I,J,4)*COSZ)*POICE                       4444.   
         DJ(JR,2)=DJ(JR,2)+(SNFS(I,J,4)*COSZ)*DXYPJ                     4445.   
         ASNFS1=ASNFS1+(SNFS(I,J,1)*COSZ)*POCEAN                        4446.   
         BSNFS1=BSNFS1+(SNFS(I,J,1)*COSZ)*PLAND                         4447.   
         CSNFS1=CSNFS1+(SNFS(I,J,1)*COSZ)*POICE                         4448.   
         DJ(JR,3)=DJ(JR,3)+(SNFS(I,J,1)*COSZ)*DXYPJ                     4449.   
         AJ(J,5)=AJ(J,5)+(SRHR(I,J,1)*COSZ/(ALB(I,J,1)+1.D-20))*POCEAN  4450.   
         BJ(J,5)=BJ(J,5)+(SRHR(I,J,1)*COSZ/(ALB(I,J,1)+1.D-20))*PLAND   4451.   
         CJ(J,5)=CJ(J,5)+(SRHR(I,J,1)*COSZ/(ALB(I,J,1)+1.D-20))*POICE   4452.   
         DJ(JR,5)=DJ(JR,5)+(SRHR(I,J,1)*COSZ/(ALB(I,J,1)+1.D-20))*DXYPJ 4453.   
         AJ(J,6)=AJ(J,6)+(SRHR(I,J,1)*COSZ)*POCEAN                      4454.   
         BJ(J,6)=BJ(J,6)+(SRHR(I,J,1)*COSZ)*PLAND                       4455.   
         CJ(J,6)=CJ(J,6)+(SRHR(I,J,1)*COSZ)*POICE                       4456.   
         DJ(JR,6)=DJ(JR,6)+(SRHR(I,J,1)*COSZ)*DXYPJ                     4457.   
         AJ(J,55)=AJ(J,55)+BTMPW(I,J)*POCEAN                            4458.   
         BJ(J,55)=BJ(J,55)+BTMPW(I,J)*PLAND                             4459.   
         CJ(J,55)=CJ(J,55)+BTMPW(I,J)*POICE                             4460.   
         DJ(JR,55)=DJ(JR,55)+BTMPW(I,J)*DXYPJ                           4461.   
         AJ(J,67)=AJ(J,67)+TRINCG(I,J)*POCEAN                           4462.   
         BJ(J,67)=BJ(J,67)+TRINCG(I,J)*PLAND                            4463.   
         CJ(J,67)=CJ(J,67)+TRINCG(I,J)*POICE                            4464.   
         DJ(JR,67)=DJ(JR,67)+TRINCG(I,J)*DXYPJ                          4465.   
         AJ(J,70)=AJ(J,70)-TNFS(I,J,4)*POCEAN                           4466.   
         BJ(J,70)=BJ(J,70)-TNFS(I,J,4)*PLAND                            4467.   
         CJ(J,70)=CJ(J,70)-TNFS(I,J,4)*POICE                            4468.   
         DJ(JR,70)=DJ(JR,70)-TNFS(I,J,4)*DXYPJ                          4469.   
         ATNFS1=ATNFS1-TNFS(I,J,1)*POCEAN                               4470.   
         BTNFS1=BTNFS1-TNFS(I,J,1)*PLAND                                4471.   
         CTNFS1=CTNFS1-TNFS(I,J,1)*POICE                                4472.   
         DJ(JR,71)=DJ(JR,71)-TNFS(I,J,1)*DXYPJ                          4473.   
         DO 760 K=2,9                                                   4474.   
         AJ(J,K+70)=AJ(J,K+70)+(S0*COSZ)*ALB(I,J,K)*POCEAN              4475.   
         BJ(J,K+70)=BJ(J,K+70)+(S0*COSZ)*ALB(I,J,K)*PLAND               4476.   
         CJ(J,K+70)=CJ(J,K+70)+(S0*COSZ)*ALB(I,J,K)*POICE               4477.   
  760    DJ(JR,K+70)=DJ(JR,K+70)+(S0*COSZ)*ALB(I,J,K)*DXYPJ             4478.   
         AIJ(I,J,21)=AIJ(I,J,21)-TNFS(I,J,4)                            4479.   
         AIJ(I,J,24)=AIJ(I,J,24)+(SNFS(I,J,4)*COSZ)                     4480.   
         AIJ(I,J,25)=AIJ(I,J,25)+(S0*COSZ)                              4481.   
         AIJ(I,J,26)=AIJ(I,J,26)+(SRHR(I,J,1)*COSZ)                     4482.   
         AIJ(I,J,27)=AIJ(I,J,27)+(SRHR(I,J,1)*COSZ/(ALB(I,J,1)+1.D-20)) 4483.   
         AIJ(I,J,44)=AIJ(I,J,44)+BTMPW(I,J)                             4484.   
         AIJ(I,J,45)=AIJ(I,J,45)+S0*COSZ*ALB(I,J,2)                     4485.   
  770    CONTINUE                                                       4486.   
         AJ(J,3)=AJ(J,3)+ASNFS1                                         4487.   
         BJ(J,3)=BJ(J,3)+BSNFS1                                         4488.   
         CJ(J,3)=CJ(J,3)+CSNFS1                                         4489.   
         AJ(J,71)=AJ(J,71)+ATNFS1                                       4490.   
         BJ(J,71)=BJ(J,71)+BTNFS1                                       4491.   
         CJ(J,71)=CJ(J,71)+CTNFS1                                       4492.   
  780    CONTINUE                                                       4493.   
         DO 790 L=1,LM                                                  4495.   
         DO 790 I=1,IM                                                  4496.   
         AIL(I,L,7)=AIL(I,L,7)+((SRHR(I,JEQ-2,L+1)*COSZ2(I,JEQ-2)+      4497.   
     *     TRHR(I,JEQ-2,L+1))*DXYP(JEQ-2)+(SRHR(I,JEQ-1,L+1)*           4498.   
     *     COSZ2(I,JEQ-1)+TRHR(I,JEQ-1,L+1))*DXYP(JEQ-1)+               4499.   
     *     (SRHR(I,JEQ,L+1)*COSZ2(I,JEQ)+TRHR(I,JEQ,L+1))*DXYP(JEQ))    4500.   
         AIL(I,L,11)=AIL(I,L,11)+(SRHR(I,J50N,L+1)*COSZ2(I,J50N)+       4501.   
     *     TRHR(I,J50N,L+1))*DXYP(J50N)                                 4502.   
  790    AIL(I,L,15)=AIL(I,L,15)+(SRHR(I,J70N,L+1)*COSZ2(I,J70N)+       4503.   
     *     TRHR(I,J70N,L+1))*DXYP(J70N)                                 4504.   
C****                                                                   4505.   
C**** UPDATE THE TEMPERATURES BY RADIATION                              4506.   
C****                                                                   4507.   
  800 DO 820 J=1,JM                                                     4508.   
      IMAX=IM                                                           4509.   
      IF(J.EQ.1.OR.J.EQ.JM) IMAX=1                                      4510.   
      DO 820 LR=1,3                                                     4511.   
      DO 820 I=1,IMAX                                                   4512.   
  820 RQT(I,J,LR)=RQT(I,J,LR)+(SRHRS(I,J,LR)*COSZ2(I,J)                 4513.   
     *  +TRHRS(I,J,LR))*COE(LR+LM)                                      4514.   
  840 DO 860 J=1,JM                                                     4515.   
      IMAX=IM                                                           4516.   
      IF(J.EQ.1.OR.J.EQ.JM) IMAX=1                                      4517.   
      DO 860 L=1,LM                                                     4518.   
      DO 860 I=1,IMAX                                                   4519.   
  860 T(I,J,L)=T(I,J,L)+(SRHR(I,J,L+1)*COSZ1(I,J)+TRHR(I,J,L+1))        4520.   
     *  *COE(L)/(P(I,J)*PK(I,J,L))                                      4521.   
      RETURN                                                            4522.   
      END                                                               4523.   
      SUBROUTINE SURFCE                                                 4801.   
C****                                                                   4802.   
C**** THIS SUBROUTINE CALCULATES THE SURFACE FLUXES WHICH INCLUDE       4803.   
C**** SENSIBLE HEAT, EVAPORATION, THERMAL RADIATION, AND MOMENTUM       4804.   
C**** DRAG.  IT ALSO CALCULATES INSTANTANEOUSLY SURFACE TEMPERATURE,    4805.   
C**** SURFACE SPECIFIC HUMIDITY, AND SURFACE WIND COMPONENTS.           4806.   
C****                                                                   4807.   
      INCLUDE 'BA94jalC9.COM'                                           4808.   
      COMMON U,V,T,P,Q                                                  4809.   
      COMMON/WORK1/CONV(IM,JM,LM),PK(IM,JM,LM),PREC(IM,JM),             4810.   
     *  TPREC(IM,JM),COSZ1(IM,JM)                                       4811.   
      COMMON/WORK2/UT(IM,JM,LM),VT(IM,JM,LM),DU1(IM,JM),                4812.   
     *  DV1(IM,JM),RA(8),ID(8),UMS(8)                                   4813.   
      COMMON/WORK3/E0(IM,JM,4),E1(IM,JM,4),EVAPOR(IM,JM,4),             4814.   
     *  TGRND(IM,JM,4)                                                  4815.   
            COMMON/WORKO/OA(IM,JM,11)                                   4815.5  
      COMMON/RDATA/ROUGHL(IM,JM),CHDAT(IM,JM)                           4816.   
      DIMENSION SINI(IM),COSI(IM)                                       4817.   
      DIMENSION AROUGH(20),BROUGH(20),CROUGH(20),DROUGH(20),EROUGH(20)     
      LOGICAL POLE                                                      4818.   
      REAL*8 B,TGV,TKV,TSV0,TSV1,TSV                                    4819.   
      DATA RVAP/461.5/,P1000/1000./                                     4820.   
      DATA SHV/0./,SHW/4185./,SHI/2060./,RHOW/1000./,RHOI/916.6/,       4821.   
     *  ALAMI/2.1762/,STBO/.5672573D-7/,TF/273.16/,TFO/-1.56/           4822.   
      DATA Z1I/.1/,Z2LI/2.9/,Z1E/.1/,Z2E/4./,RHOS/91.66/,ALAMS/.35/     4823.   
      DATA  AROUGH/16.59,13.99,10.4,7.35,5.241,3.926,3.126,2.632,2.319, 4825.   
     *2.116,1.982,1.893,1.832,1.788,1.757,1.733,1.714,1.699,1.687,1.677/4826.   
      DATA BROUGH/3.245,1.733,0.8481,0.3899,0.1832,0.09026,0.04622,     4827.   
     * .241D-1,.1254D-1,.6414D-2,.3199D-2,.1549D-2,.7275D-3,.3319D-3,   4828.   
     * .1474D-3,.6392D-4,.2713D-4,.1130D-4,.4630D-5,.1868D-5/           4829.   
      DATA  CROUGH/5.111,3.088,1.682,.9239,.5626,.3994,.3282,.3017,.299 4830.   
     *,.3114,.3324,.3587,.3881,.4186,.4492,.4792,.5082,.5361,.5627,     4831.   
     * .5882/                                                           4832.   
      DATA DROUGH/1.24,1.02,0.806,0.682,0.661,0.771,0.797,0.895,0.994,  4833.   
     * 1.09,1.18,1.27,1.35,1.43,1.50,1.58,1.65,1.71,1.78,1.84/          4834.   
      DATA EROUGH/0.128,0.130,0.141,0.174,0.238,0.330,0.438,0.550,0.660,4835.   
     * 0.766,0.866,0.962,1.05,1.14,1.22,1.30,1.37,1.45,1.52,1.58/       4836.   
      QSAT(TM,PR,QLH)=3.797915*EXP(QLH*(7.93252D-6-2.166847D-3/TM))/PR  4837.   
      TLOG(Z0)=LOG(.36*RTTAU/(FMAG*Z0))+2.302585*ROUGH-.08              4838.   
      DATA IFIRST/1/                                                    4839.   
C****                                                                   4840.   
C**** FDATA  2  LAND COVERAGE (1)                                       4841.   
C****        3  RATIO OF LAND ICE COVERAGE TO LAND COVERAGE (1)         4842.   
C****                                                                   4843.   
C**** ODATA  1  OCEAN TEMPERATURE (C)                                   4844.   
C****        2  RATIO OF OCEAN ICE COVERAGE TO WATER COVERAGE (1)       4845.   
C****        3  OCEAN ICE AMOUNT OF SECOND LAYER (KG/M**2)              4846.   
C****                                                                   4847.   
C**** GDATA  1  OCEAN ICE SNOW AMOUNT (KG/M**2)                         4848.   
C****        2  EARTH SNOW AMOUNT (KG/M**2)                             4849.   
C****        3  OCEAN ICE TEMPERATURE OF FIRST LAYER (C)                4850.   
C****        4  EARTH TEMPERATURE OF FIRST LAYER (C)                    4851.   
C****        5  EARTH WATER OF FIRST LAYER (KG/M**2)                    4852.   
C****        6  EARTH ICE OF FIRST LAYER (KG/M**2)                      4853.   
C****        7  OCEAN ICE TEMPERATURE OF SECOND LAYER (C)               4854.   
C****        8  EARTH TEMPERATURE OF SECOND LAYER (C)                   4855.   
C****        9  EARTH WATER OF SECOND LAYER (KG/M**2)                   4856.   
C****       10  EARTH ICE OF SECOND LAYER (KG/M**2)                     4857.   
C****       12  LAND ICE SNOW AMOUNT (KG/M**2)                          4858.   
C****       13  LAND ICE TEMPERATURE OF FIRST LAYER (C)                 4859.   
C****       14  LAND ICE TEMPERATURE OF SECOND LAYER (C)                4860.   
C****                                                                   4861.   
C**** BLDATA 1  COMPOSITE SURFACE WIND MAGNITUDE (M/S)                  4862.   
C****        2  COMPOSITE SURFACE AIR TEMPERATURE (K)                   4863.   
C****        3  COMPOSITE SURFACE AIR SPECIFIC HUMIDITY (1)             4864.   
C****        4  LAYER TO WHICH DRY CONVECTION MIXES (1)                 4865.   
C****        5  MIXED LAYER DEPTH (Z1O NOT YET PART OF RESTART FILE)    4866.   
C****        6  COMPOSITE SURFACE U WIND                                4867.   
C****        7  COMPOSITE SURFACE V WIND                                4868.   
C****        8  COMPOSITE SURFACE MOMENTUM TRANSFER (TAU)               4869.   
C****                                                                   4870.   
C**** VDATA  9  WATER FIELD CAPACITY OF FIRST LAYER (KG/M**2)           4871.   
C****       10  WATER FIELD CAPACITY OF SECOND LAYER (KG/M**2)          4872.   
C****                                                                   4873.   
C**** ROUGHL    LOG(ZGS/ROUGHNESS LENGTH) (LOGARITHM TO BASE 10)        4874.   
C****                                                                   4875.   
      NSTEPS=NSURF*NSTEP/NDYN                                           4876.   
      IF(IFIRST.NE.1) GO TO 30                                          4877.   
      IFIRST=0                                                          4878.   
      CALL DREAD (19,ROUGHL,IM*JM,ROUGHL)                               4879.   
      REWIND 19                                                         4880.   
      IQ1=IM/4+1                                                        4882.   
      IQ2=IM/2+1                                                        4883.   
      IQ3=3*IM/4+1                                                      4884.   
      DTSURF=NDYN*DT/NSURF                                              4885.   
         DTSRCE=DT*NDYN                                                 4886.   
      SHA=RGAS/KAPA                                                     4887.   
      RVX=0.                                                            4888.   
      ACE1I=Z1I*RHOI                                                    4889.   
      HC1I=ACE1I*SHI                                                    4890.   
      HC2LI=Z2LI*RHOI*SHI                                               4891.   
      HC1DE=Z1E*1129950.                                                4892.   
      HC2DE=Z2E*1129950.+3.5*.125*RHOW*3100.                            4893.   
      Z1IBYL=Z1I/ALAMI                                                  4894.   
      Z2LI3L=Z2LI/(3.*ALAMI)                                            4895.   
      BYRSL=1./(RHOS*ALAMS)                                             4896.   
      ZS1CO=.5*DSIG(1)*RGAS/GRAV                                        4897.   
      P1000K=EXPBYK(P1000)                                              4898.   
      DO 20 I=1,IM                                                      4902.   
      SINI(I)=SIN((I-1)*TWOPI/FIM)                                      4903.   
   20 COSI(I)=COS((I-1)*TWOPI/FIM)                                      4904.   
c  ** MFS (CHANGED)
c   30 S0=S0X*1367./RSDIST                                               4905.   
   30 S0=S0X/RSDIST                                    
c  ** END (CHANGED)
         SPRING=-1.                                                     4906.5  
         IF((JDAY.GE.32).AND.(JDAY.LE.212)) SPRING=1.                   4906.6  
C**** ZERO OUT ENERGY AND EVAPORATION FOR GROUND AND INITIALIZE TGRND   4907.   
      DO 40 J=1,JM                                                      4908.   
      DO 40 I=1,IM                                                      4909.   
      TGRND(I,J,2)=GDATA(I,J,3)                                         4910.   
      TGRND(I,J,3)=GDATA(I,J,13)                                        4911.   
      TGRND(I,J,4)=GDATA(I,J,4)                                         4912.   
      DO 40 K=1,12                                                      4913.   
   40 E0(I,J,K)=0.                                                      4914.   
         IHOUR=1.5+TOFDAY                                               4915.   
C****                                                                   4916.   
C**** OUTSIDE LOOP OVER TIME STEPS, EXECUTED NSURF TIMES EVERY HOUR     4917.   
C****                                                                   4918.   
      DO 9000 NS=1,NSURF                                                4919.   
         MODDSF=MOD(NSTEPS+NS-1,NDASF)                                  4920.   
         IF(MODDSF.EQ.0) IDACC(3)=IDACC(3)+1                            4921.   
         MODD6=MOD(IDAY+NS,NSURF)                                       4922.   
         TIMEZ=JDAY+(TOFDAY+(NS-1.)/NSURF)/24.                          4922.5  
         IF(JDAY.LE.31) TIMEZ=TIMEZ+365.                                4922.6  
C**** ZERO OUT LAYER 1 WIND INCREMENTS                                  4923.   
      DO 60 J=1,JM                                                      4924.   
      DO 60 I=1,IM                                                      4925.   
      DU1(I,J)=0.                                                       4926.   
   60 DV1(I,J)=0.                                                       4927.   
C****                                                                   4928.   
C**** OUTSIDE LOOP OVER J AND I, EXECUTED ONCE FOR EACH GRID POINT      4929.   
C****                                                                   4930.   
      DO 7000 J=1,JM                                                    4931.   
      HEMI=1.                                                           4932.   
      IF(J.LE.JM/2) HEMI=-1.                                            4933.   
      FCORJ=2.*OMEGA*SINP(J)                                            4934.   
      FMAG=FCORJ*HEMI                                                   4935.   
      ROOT2F=SQRT(2.*FMAG)                                              4936.   
      IF(J.EQ.1) GO TO 80                                               4937.   
      IF(J.EQ.JM) GO TO 90                                              4938.   
      POLE=.FALSE.                                                      4939.   
      IMAX=IM                                                           4940.   
      GO TO 100                                                         4941.   
C**** CONDITIONS AT THE SOUTH POLE                                      4942.   
   80 POLE=.TRUE.                                                       4943.   
      IMAX=1                                                            4944.   
      JVPO=2                                                            4945.   
      RAPO=2.*RAPVN(1)                                                  4946.   
      U1=.25*(U(1,2,1)+V(IQ1,2,1)-U(IQ2,2,1)-V(IQ3,2,1))                4947.   
      V1=.25*(V(1,2,1)-U(IQ1,2,1)-V(IQ2,2,1)+U(IQ3,2,1))                4948.   
      GO TO 100                                                         4949.   
C**** CONDITIONS AT THE NORTH POLE                                      4950.   
   90 POLE=.TRUE.                                                       4951.   
      IMAX=1                                                            4952.   
      JVPO=JM                                                           4953.   
      RAPO=2.*RAPVS(JM)                                                 4954.   
      U1=.25*(U(1,JM,1)-V(IQ1,JM,1)-U(IQ2,JM,1)+V(IQ3,JM,1))            4955.   
      V1=.25*(V(1,JM,1)+U(IQ1,JM,1)-V(IQ2,JM,1)-U(IQ3,JM,1))            4956.   
C**** ZERO OUT SURFACE DIAGNOSTICS WHICH WILL BE SUMMED OVER LONGITUDE  4957.   
  100    ATRHDT=0.                                                      4958.   
         BTRHDT=0.                                                      4959.   
         CTRHDT=0.                                                      4960.   
         ASHDT=0.                                                       4961.   
         BSHDT=0.                                                       4962.   
         CSHDT=0.                                                       4963.   
         AEVHDT=0.                                                      4964.   
         BEVHDT=0.                                                      4965.   
         CEVHDT=0.                                                      4966.   
         ATS=0.                                                         4967.   
         BTS=0.                                                         4968.   
         CTS=0.                                                         4969.   
         JEQ=1+JM/2                                                     4969.5  
         IF(J.LT.JEQ) WARMER=-SPRING                                    4969.6  
         IF(J.GE.JEQ) WARMER=SPRING                                     4969.7  
      IM1=IM                                                            4970.   
      DO 6000 I=1,IMAX                                                  4971.   
C****                                                                   4972.   
C**** DETERMINE SURFACE CONDITIONS                                      4973.   
C****                                                                   4974.   
      PLAND=FDATA(I,J,2)                                                4975.   
      PWATER=1.-PLAND                                                   4976.   
      PLICE=FDATA(I,J,3)*PLAND                                          4977.   
      PEARTH=PLAND-PLICE                                                4978.   
      POICE=ODATA(I,J,2)*PWATER                                         4979.   
      POCEAN=PWATER-POICE                                               4980.   
      PIJ=P(I,J)                                                        4981.   
      PS=PIJ+PTOP                                                       4982.   
      PSK=EXPBYK(PS)                                                    4983.   
      P1=SIG(1)*PIJ+PTOP                                                4984.   
      P1K=EXPBYK(P1)                                                    4985.   
      WSOLD=BLDATA(I,J,1)                                               4986.   
      USOLD=BLDATA(I,J,6)                                               4987.   
      VSOLD=BLDATA(I,J,7)                                               4988.   
      RTTAU=SQRT(BLDATA(I,J,8))                                         4990.   
      GKBYFW=.1296*GRAV/(FCORJ*FMAG*WSOLD+1.D-20)                       4991.   
      COSWS=GKBYFW*USOLD                                                4992.   
      SINWS=GKBYFW*VSOLD                                                4993.   
      IF(POLE) GO TO 1200                                               4994.   
      U1=.25*(U(IM1,J,1)+U(I,J,1)+U(IM1,J+1,1)+U(I,J+1,1))              4995.   
      V1=.25*(V(IM1,J,1)+V(I,J,1)+V(IM1,J+1,1)+V(I,J+1,1))              4996.   
 1200 TH1=T(I,J,1)                                                      4997.   
      Q1=Q(I,J,1)                                                       4998.   
      THV1=TH1*(1.+Q1*RVX)                                              4999.   
      SRH=SRHR(I,J,1)*COSZ1(I,J)                                        5000.   
         SRHDTS=SRH*DTSURF                                              5001.   
            OA(I,J,5)=OA(I,J,5)+SRHDTS                                  5001.01 
      RMBYA=100.*PIJ*DSIG(1)/GRAV                                       5002.   
C**** ZERO OUT QUANTITIES TO BE SUMMED OVER SURFACE TYPES               5003.   
      USS=0.                                                            5004.   
      VSS=0.                                                            5005.   
      WSS=0.                                                            5006.   
      TSS=0.                                                            5007.   
      QSS=0.                                                            5008.   
      TAUS=0.                                                           5009.   
         RTAUS=0.                                                       5010.   
         RTAUUS=0.                                                      5011.   
         RTAUVS=0.                                                      5012.   
         SINAPS=0.                                                      5013.   
         COSAPS=0.                                                      5014.   
         JR=JREG(I,J)                                                   5015.   
         DXYPJ=DXYP(J)                                                  5016.   
         TG1S=0.                                                        5017.   
         QGS=0.                                                         5018.   
         TRHDTS=0.                                                      5020.   
         SHDTS=0.                                                       5021.   
         EVHDTS=0.                                                      5022.   
         UGS=0.                                                         5023.   
         VGS=0.                                                         5024.   
         WGS=0.                                                         5025.   
         USRS=0.                                                        5026.   
         VSRS=0.                                                        5027.   
         RIS1S=0.                                                       5028.   
         RIGSS=0.                                                       5029.   
         CDMS=0.                                                        5030.   
         CDHS=0.                                                        5031.   
         DGSS=0.                                                        5032.   
         EDS1S=0.                                                       5033.   
         PPBLS=0.                                                       5034.   
         EVAPS=0.                                                       5035.   
         PEVAPS=0.                                                      5035.2  
         QSATSS=0.                                                      5035.5  
C****                                                                   5036.   
      IF(POCEAN.LE.0.) GO TO 2200                                       5037.   
C****                                                                   5038.   
C**** OCEAN                                                             5039.   
C****                                                                   5040.   
      ITYPE=1                                                           5041.   
      PTYPE=POCEAN                                                      5042.   
      ROUGH=7.126-1.068*LOG(WSOLD+1.D-12)                               5043.   
      CDN=.00075+.000067*WSOLD                                          5044.   
      ZGS=10.                                                           5045.   
      ALOGT=TLOG(ZGS)                                                   5046.   
      NGRNDZ=1                                                          5047.   
      TG1=ODATA(I,J,1)                                                  5048.   
      SRHEAT=SRH*SRCOR                                                  5049.   
      BETA=1.                                                           5050.   
      ELHX=LHE                                                          5051.   
      GO TO 3000                                                        5052.   
C****                                                                   5053.   
 2200 IF(POICE.LE.0.) GO TO 2400                                        5054.   
C****                                                                   5055.   
C**** OCEAN ICE                                                         5056.   
C****                                                                   5057.   
      ITYPE=2                                                           5058.   
      PTYPE=POICE                                                       5059.   
      NGRNDZ=NGRND                                                      5060.   
      SNOW=GDATA(I,J,1)                                                 5061.   
      TG1=TGRND(I,J,2)                                                  5062.   
      TG2=GDATA(I,J,7)                                                  5063.   
      ACE2=ODATA(I,J,3)                                                 5064.   
      SRHEAT=SRH*SRCOR                                                  5065.   
      Z2=ACE2/RHOI                                                      5066.   
      Z2BY4L=Z2/(4.*ALAMI)                                              5067.   
      Z1BY6L=(Z1IBYL+SNOW*BYRSL)*.1666667                               5068.   
      CDTERM=1.5*TG2-.5*TFO                                             5069.   
      CDENOM=1./(2.*Z1BY6L+Z2BY4L)                                      5070.   
      ROUGH=4.37                                                        5071.   
      CDN=.0231/(ROUGH*ROUGH)                                           5072.   
      ZGS=10.                                                           5073.   
      ALOGT=TLOG(ZGS)                                                   5074.   
      HC1=HC1I+SNOW*SHI                                                 5075.   
      BETA=1.                                                           5076.   
      ELHX=LHS                                                          5077.   
      GO TO 3000                                                        5078.   
C****                                                                   5079.   
 2400 IF(PLAND.LE.0.) GO TO 5000                                        5080.   
      NGRNDZ=NGRND                                                      5081.   
      ROUGH=ROUGHL(I,J)                                                 5082.   
      CDN=.0231/(ROUGH*ROUGH)                                           5083.   
      ZGS=30.                                                           5084.   
      ALOGT=TLOG(ZGS)                                                   5085.   
      IF(PLICE.LE.0.) GO TO 2600                                        5086.   
C****                                                                   5087.   
C**** LAND ICE                                                          5088.   
C****                                                                   5089.   
      ITYPE=3                                                           5090.   
      PTYPE=PLICE                                                       5091.   
      SNOW=GDATA(I,J,12)                                                5092.   
      TG1=TGRND(I,J,3)                                                  5093.   
      TG2=GDATA(I,J,14)                                                 5094.   
      SRHEAT=SRH                                                        5095.   
      Z1BY6L=(Z1IBYL+SNOW*BYRSL)*.1666667                               5096.   
      CDTERM=TG2                                                        5097.   
      CDENOM=1./(2.*Z1BY6L+Z2LI3L)                                      5098.   
      HC1=HC1I+SNOW*SHI                                                 5099.   
      BETA=1.                                                           5100.   
      ELHX=LHS                                                          5101.   
      GO TO 3000                                                        5102.   
C****                                                                   5103.   
 2600 IF(PEARTH.LE.0.) GO TO 5000                                       5104.   
C****                                                                   5105.   
C**** EARTH                                                             5106.   
C****                                                                   5107.   
      ITYPE=4                                                           5108.   
      PTYPE=PEARTH                                                      5109.   
      SNOW=GDATA(I,J,2)                                                 5110.   
      TG1=TGRND(I,J,4)                                                  5111.   
      WTR1=GDATA(I,J,5)                                                 5112.   
      ACE1=GDATA(I,J,6)                                                 5113.   
      TG2=GDATA(I,J,8)                                                  5114.   
      WTR2=GDATA(I,J,9)                                                 5115.   
      ACE2=GDATA(I,J,10)                                                5116.   
      WFC1=VDATA(I,J,9)                                                 5117.   
      WFC2=VDATA(I,J,10)                                                5118.   
      SRHEAT=SRH                                                        5119.   
      HC1=HC1DE+WTR1*SHW+(ACE1+SNOW)*SHI                                5120.   
      ALAM1D=2.+.5*(1.+2.*WTR1/WFC1)                                    5121.   
      ALAM2D=4.                                                         5122.   
      RMULCH=1.                                                         5123.   
      IF((SINP(J).GT..5).AND.(JDAY-91)*(273-JDAY).LT.0) RMULCH=.25      5124.   
      IF((SINP(J).LT.-.5).AND.(JDAY-91)*(273-JDAY).GE.0) RMULCH=.25     5125.   
      ALAM1V=RMULCH*(.4185+1.2555*WTR1/WFC1+ALAMI*ACE1/(Z1E*RHOI))      5126.   
      ALAM3V=.8370                                                      5127.   
      IF(TG2.LT.0.) ALAM3V=.4185+ALAMI*.15                              5128.   
      ALAM2V=.125*(.4185+1.2555*WTR2/WFC2+ALAMI*ACE2/(5.*Z1E*RHOI))     5129.   
     *  +.875*ALAM3V                                                    5130.   
      ALAM1E=VDATA(I,J,1)*ALAM1D+(1.-VDATA(I,J,1))*ALAM1V               5131.   
      ALAM2E=VDATA(I,J,1)*ALAM2D+(1.-VDATA(I,J,1))*ALAM2V               5132.   
      Z1BY6L=(Z1E/ALAM1E+SNOW*BYRSL)*.1666667                           5133.   
      CDTERM=TG2                                                        5134.   
      CDENOM=1./(2.*Z1BY6L+Z2E/(3.*ALAM2E))                             5135.   
      BETA=1.                                                           5136.   
      ELHX=LHS                                                          5137.   
      IF(SNOW.GT.0.) GO TO 3000                                         5138.   
      BETA=(WTR1+ACE1)/WFC1                                             5139.   
      PFROZN=ACE1/(WTR1+ACE1+1.D-20)                                    5140.   
      ELHX=LHE+LHM*PFROZN                                               5141.   
C****                                                                   5142.   
C**** BOUNDARY LAYER INTERACTION                                        5143.   
C****                                                                   5144.   
 3000 TKV=THV1*PSK                                                      5145.   
      ZS1=ZS1CO*TKV*PIJ/PS                                              5146.   
      P1=SIG(1)*PIJ+PTOP                                                5147.   
      LR=ROUGH*2.-.5                                                    5148.   
      IF(LR.GT.20) LR=20                                                5149.   
      IF(LR.LT.1) LR=1                                                  5150.   
      DTGRND=DTSURF/NGRNDZ                                              5151.   
      SHDT=0.                                                           5152.   
      EVHDT=0.                                                          5153.   
         PEVHDT=0.                                                      5153.1  
      TRHDT=0.                                                          5154.   
      F1DT=0.                                                           5155.   
C**** LOOP OVER GROUND TIME STEPS                                       5156.   
      DO 3600 NG=1,NGRNDZ                                               5157.   
      TG=TG1+TF                                                         5158.   
      QG=QSAT(TG,PS,ELHX)                                               5159.   
      TGV=TG*(1.+QG*RVX)                                                5160.   
      IF(TKV.GT.TGV) GO TO 3300                                         5161.   
C****                                                                   5162.   
C**** ATMOSPHERE IS UNSTABLE WITH RESPECT TO THE GROUND                 5163.   
C****                                                                   5164.   
C**** CALCULATE PPBL, UG AND VG                                         5165.   
      LDC=BLDATA(I,J,4)                                                 5166.   
      PPBL=SIGE(LDC+1)*PIJ+PTOP                                         5167.   
      IF(POLE) GO TO 3160                                               5168.   
      UG=.125*(U(IM1,J,LDC)+U(I,J,LDC)+U(IM1,J+1,LDC)+U(I,J+1,LDC)      5169.   
     *  +U(IM1,J,LDC+1)+U(I,J,LDC+1)+U(IM1,J+1,LDC+1)+U(I,J+1,LDC+1))   5170.   
      VG=.125*(V(IM1,J,LDC)+V(I,J,LDC)+V(IM1,J+1,LDC)+V(I,J+1,LDC)      5171.   
     *  +V(IM1,J,LDC+1)+V(I,J,LDC+1)+V(IM1,J+1,LDC+1)+V(I,J+1,LDC+1))   5172.   
      GO TO 3180                                                        5173.   
 3160 UG=.125*(U(1,JVPO,LDC)-U(IQ2,JVPO,LDC)-(V(IQ1,JVPO,LDC)           5174.   
     *  -V(IQ3,JVPO,LDC))*HEMI+U(1,JVPO,LDC+1)-U(IQ2,JVPO,LDC+1)        5175.   
     *  -(V(IQ1,JVPO,LDC+1)-V(IQ3,JVPO,LDC+1))*HEMI)                    5176.   
      VG=.125*(V(1,JVPO,LDC)-V(IQ2,JVPO,LDC)+(U(IQ1,JVPO,LDC)           5177.   
     *  -U(IQ3,JVPO,LDC))*HEMI+V(1,JVPO,LDC+1)-V(IQ2,JVPO,LDC+1)        5178.   
     *  +(U(IQ1,JVPO,LDC+1)-U(IQ3,JVPO,LDC+1))*HEMI)                    5179.   
 3180 WG=SQRT(UG*UG+VG*VG)                                              5180.   
C**** CALCULATE CROSS-ISOBAR ADJUSTMENT ANGLE IN CONSTANT PRESSURE      5181.   
C****      COORDINATES                                                  5182.   
      SINAP=0.                                                          5183.   
      COSAP=1.                                                          5184.   
      IF(POLE.OR.J.EQ.JM-1.OR.J.EQ.2) GO TO 3219                        5185.   
      JD=1                                                              5186.   
      IF(VSOLD.GE.0.) JD=-1                                             5187.   
      JX=J+JD                                                           5188.   
      PPBLK=EXPBYK(PPBL)                                                5189.   
 3190 L=0                                                               5190.   
 3195 L=L+1                                                             5191.   
      PEUP=P(I,JX)*SIGE(L+1)+PTOP                                       5192.   
      IF(PEUP.GE.PPBL) GO TO 3195                                       5193.   
      IF(PEUP.GT.PS) GO TO 3200                                         5194.   
      DPHI=SHA*T(I,JX,L)*(PPBLK-PSK)                                    5195.   
      GO TO 3212                                                        5196.   
 3200 PUPK=EXPBYK(PEUP)                                                 5197.   
      DPHI=SHA*T(I,JX,L)*(PPBLK-PUPK)                                   5198.   
 3205 L=L+1                                                             5199.   
      PEUP=SIGE(L+1)*P(I,JX)+PTOP                                       5200.   
      IF(PEUP.LE.PS) GO TO 3210                                         5201.   
      PDNK=PUPK                                                         5202.   
      PUPK=EXPBYK(PEUP)                                                 5203.   
      DPHI=DPHI+SHA*T(I,JX,L)*(PDNK-PUPK)                               5204.   
      GO TO 3205                                                        5205.   
 3210 DPHI=DPHI+SHA*T(I,JX,L)*(PUPK-PSK)                                5206.   
 3212 IF(JX.EQ.J) GO TO 3215                                            5207.   
      JX=J                                                              5208.   
      DPHID=DPHI                                                        5209.   
      GO TO 3190                                                        5210.   
 3215 SXA=-JD*(DPHID-DPHI)/(DPHI*DYV(3))                                5211.   
      SXR=SXA*COSWS                                                     5212.   
      SYR=-HEMI*SINWS*SXA                                               5213.   
      DEN=ALOGT-.2*SXR+.04*SYR                                          5213.1  
      IF(DEN.EQ.0.) GO TO 3219                                          5213.2  
      TANAP=(4.3-.32*SXR+.33*SYR)/DEN-4.3/ALOGT                         5214.   
      COSAP=1./SQRT(1.+TANAP*TANAP)                                     5215.   
      SINAP=COSAP*TANAP                                                 5216.   
 3219 TSV0=.25*TGV+.75*TKV                                              5217.   
      SINA0=.5                                                          5218.   
      NNN=1                                                             5219.   
C**** DETERMINE WS FROM SINA0,WG                                        5220.   
 3220 COSA0=SQRT(1.-SINA0*SINA0)                                        5221.   
      WSSQ=WG*WG*(1.-2.*SINA0*COSA0)                                    5222.   
      WS=SQRT(WSSQ)                                                     5223.   
C**** DETERMINE DM,TSV1,EDS1 FROM WS,TSV0                               5224.   
      RIGS=ZGS*GRAV*(TSV0-TGV)/(TGV*WS*WS+1.D-20)                       5225.   
      DM=SQRT((1.-AROUGH(LR)*RIGS)*(1.-BROUGH(LR)*RIGS)/                5226.   
     *  (1.-CROUGH(LR)*RIGS))                                           5227.   
      DH=1.35*SQRT((1.-DROUGH(LR)*RIGS)/(1.-EROUGH(LR)*RIGS))           5228.   
      DGS=ZS1*CDN*WS*DM*DH                                              5229.   
      X=1156.D4*P1000K/(PSK*ZS1)                                        5230.   
      Y=17520.*P1000K/(PSK*ZS1)                                         5231.   
      B=DGS*((TGV-TKV)*Y-1.)-60.                                        5232.   
      Z=(-B-DSQRT(B*B-4.*(DGS*Y+X)*DGS*(TKV-TGV)))/(2.*(DGS*Y+X))       5233.   
      TSV1=TKV-Z                                                        5234.   
      EDS1=(60.-Z*X)/(1.-Z*Y)                                           5235.   
C**** DETERMINE SINA FROM DM,EDS1                                       5236.   
      EDSL=(EDS1*(PS-P1)+60.*(P1-PPBL))/(PS-PPBL)                       5237.   
      GAMMA=SQRT(2.*FMAG*EDSL)/(CDN*DM*WG+1.D-20)                       5238.   
      SINA=1./(2.+GAMMA)                                                5239.   
      DO 3240 N=1,3                                                     5240.   
      COSA=SQRT(1.-SINA*SINA)                                           5241.   
      FST=1.-2.*SINA*COSA-GAMMA*SINA                                    5242.   
      DFDS=(4.*SINA*SINA-2.)/COSA-GAMMA                                 5243.   
 3240 SINA=SINA-FST/DFDS                                                5244.   
      TSV=TSV1                                                          5245.   
      IF(ABS(SINA-SINA0).LE..01) GO TO 3280                             5246.   
C**** DETERMINE NEW SINA AND LOOP BACK FOR NEXT ITERATION               5247.   
      IF(NNN.EQ.5)WRITE(6,9991) I,J,ITYPE,NG,ZGS,WG,TGV,TKV,PS,FCORJ,CDN5248.   
      IF(NNN.GE.5) WRITE (6,9992) NNN,                                  5249.   
     *  TSV0,SINA0,WSSQ,RIGS,DM,RIS1,EDS1,GAMMA,DGS,FST,TSV1,SINA,TSV   5250.   
      IF(NNN.GE.20) STOP 'ERROR-SURFCE: 20 ITERATIONS IN UNSTABLE CASE' 5251.   
      SINA0=SINA                                                        5252.   
      TSV0=TSV                                                          5253.   
      NNN=NNN+1                                                         5254.   
      GO TO 3220                                                        5255.   
C**** CALCULATE FINAL US,VS                                             5256.   
 3280 COSA=SQRT(1.-SINA*SINA)                                           5257.   
      WS=WG*SQRT(1.-2.*SINA*COSA)                                       5258.   
      CDH=CDN*DM*DH                                                     5259.   
      RCDHWS=CDH*WS*100.*PS/(RGAS*TSV)                                  5260.   
      IF(SINAP.LE..6427876) GO TO 3285                                  5261.   
      SINAP=.6427876                                                    5262.   
      COSAP=.7660444                                                    5263.   
      GO TO 3290                                                        5264.   
 3285 IF(-SINAP.LE..5*SINA) GO TO 3290                                  5265.   
      SINAP=-.5*SINA                                                    5266.   
      COSAP=SQRT(1.-SINAP*SINAP)                                        5267.   
 3290 SINAA=SINA*COSAP+COSA*SINAP                                       5268.   
      COSAA=COSA*COSAP-SINA*SINAP                                       5269.   
      IF(SINAA.LE..6427876) GO TO 3295                                  5270.   
      SINAA=.6427876                                                    5271.   
      COSAA=.7660444                                                    5272.   
 3295 USR=1.-SINAA*(COSAA+SINAA)                                        5273.   
      VSR=HEMI*SINAA*(COSAA-SINAA)                                      5274.   
      US=USR*UG-VSR*VG                                                  5275.   
      VS=VSR*UG+USR*VG                                                  5276.   
 3299 WS=WG*SQRT(1.-2.*SINAA*COSAA)                                     5277.   
         RIS1=Z*GRAV*ZS1/(TKV*(2.*WG*WG*SINAA*SINAA+ZS1*ZS1*1.D-12))    5278.   
      GO TO 3400                                                        5279.   
C****                                                                   5280.   
C**** ATMOSPHERE IS STABLE WITH RESPECT TO THE GROUND                   5281.   
C****                                                                   5282.   
 3300 IF(LBLM.GT.1) GO TO 3301                                          5283.   
C**** WITH ONLY 1 LAYER IN THE BOUNDARY LAYER, LEVEL 1 IS THE           5284.   
C****   GEOSTROPHIC LEVEL                                               5285.   
      UG=U1                                                             5286.   
      VG=V1                                                             5287.   
         PPBL=P1                                                        5288.   
      GO TO 3309                                                        5289.   
C**** WITH SEVERAL LAYERS IN THE BOUNDARY LAYER, THE GEOSTROPHIC LEVEL  5290.   
C****   IS VARIABLE.  (SOMETIMES IT SWITCHES TO THE UNSTABLE REGIME.)   5291.   
 3301 UL=U1                                                             5292.   
      VL=V1                                                             5293.   
      TKVL=TKV                                                          5294.   
      ZBL=ZS1                                                           5295.   
      DO 3306 L=1,LBLM-1                                                5296.   
      IF(POLE) GO TO 3302                                               5297.   
      ULP1=.25*(U(IM1,J,L+1)+U(I,J,L+1)+U(IM1,J+1,L+1)+U(I,J+1,L+1))    5298.   
      VLP1=.25*(V(IM1,J,L+1)+V(I,J,L+1)+V(IM1,J+1,L+1)+V(I,J+1,L+1))    5299.   
      GO TO 3303                                                        5300.   
 3302 ULP1=.25*(U(1,JVPO,L+1)-U(IQ2,JVPO,L+1)                           5301.   
     *      -(V(IQ1,JVPO,L+1)-V(IQ3,JVPO,L+1))*HEMI)                    5302.   
      VLP1=.25*(V(1,JVPO,L+1)-V(IQ2,JVPO,L+1)                           5303.   
     *      +(U(IQ1,JVPO,L+1)-U(IQ3,JVPO,L+1))*HEMI)                    5304.   
 3303 TKVLP1=T(I,J,L+1)*(1.+Q(I,J,L+1)*RVX)*PSK                         5305.   
      DPHI=SHA*.5*(TKVL+TKVLP1)*(PK(I,J,L)-PK(I,J,L+1))/PSK             5306.   
      IF(TKVL.GE.TKVLP1) GO TO 3304                                     5307.   
      RI=(TKVLP1-TKVL)*DPHI/(TKVLP1                                     5308.   
     *  *((ULP1-UL)*(ULP1-UL)+(VLP1-VL)*(VLP1-VL)+DPHI*DPHI*1.D-12))    5309.   
      EDH=60./(1.+50.*RI)                                               5310.   
      IF(EDH.LE..01) GO TO 3308                                         5311.   
 3304 UL=ULP1                                                           5312.   
      VL=VLP1                                                           5313.   
      TKVL=TKVLP1                                                       5314.   
 3306 ZBL=ZBL+DPHI/GRAV                                                 5315.   
      L=LBLM                                                            5316.   
 3308 UG=UL                                                             5317.   
      VG=VL                                                             5318.   
      TKV=TKVL                                                          5319.   
      ZS1=ZBL                                                           5320.   
      PPBL=SIG(L)*PIJ+PTOP                                              5321.   
      P1=PPBL                                                           5322.   
      IF(TKV.LE.TGV) GO TO 3180                                         5323.   
 3309 WG=SQRT(UG*UG+VG*VG)                                              5324.   
C**** CALCULATE CROSS-ISOBAR ADJUSTMENT ANGLE IN CONSTANT PRESSURE      5325.   
C****      COORDINATES                                                  5326.   
      SINAP=0.                                                          5327.   
      COSAP=1.                                                          5328.   
      IF(POLE.OR.J.EQ.JM-1.OR.J.EQ.2) GO TO 3318                        5329.   
      JD=1                                                              5330.   
      IF(VSOLD.GE.0.) JD=-1                                             5331.   
      JX=J+JD                                                           5332.   
      PPBLK=EXPBYK(PPBL)                                                5333.   
 3310 L=0                                                               5334.   
 3311 L=L+1                                                             5335.   
      PEUP=P(I,JX)*SIGE(L+1)+PTOP                                       5336.   
      IF(PEUP.GE.PPBL) GO TO 3311                                       5337.   
      IF(PEUP.GT.PS) GO TO 3312                                         5338.   
      DPHI=SHA*T(I,JX,L)*(PPBLK-PSK)                                    5339.   
      GO TO 3315                                                        5340.   
 3312 PUPK=EXPBYK(PEUP)                                                 5341.   
      DPHI=SHA*T(I,JX,L)*(PPBLK-PUPK)                                   5342.   
 3313 L=L+1                                                             5343.   
      PEUP=SIGE(L+1)*P(I,JX)+PTOP                                       5344.   
      IF(PEUP.LE.PS) GO TO 3314                                         5345.   
      PDNK=PUPK                                                         5346.   
      PUPK=EXPBYK(PEUP)                                                 5347.   
      DPHI=DPHI+SHA*T(I,JX,L)*(PDNK-PUPK)                               5348.   
      GO TO 3313                                                        5349.   
 3314 DPHI=DPHI+SHA*T(I,JX,L)*(PUPK-PSK)                                5350.   
 3315 IF(JX.EQ.J) GO TO 3316                                            5351.   
      JX=J                                                              5352.   
      DPHID=DPHI                                                        5353.   
      GO TO 3310                                                        5354.   
 3316 SXA=-JD*(DPHID-DPHI)/(DPHI*DYV(3))                                5355.   
      SXR=SXA*COSWS                                                     5356.   
      SYR=-HEMI*SINWS*SXA                                               5357.   
      DEN=ALOGT-.2*SXR+.04*SYR                                          5357.1  
      IF(DEN.EQ.0.) GO TO 3318                                          5357.2  
      TANAP=(4.3-.32*SXR+.33*SYR)/DEN-4.3/ALOGT                         5358.   
      COSAP=1./SQRT(1.+TANAP*TANAP)                                     5359.   
      SINAP=COSAP*TANAP                                                 5360.   
C**** ITERATION LOOP TO SOLVE FOR TSV0                                  5361.   
 3318 TSV0=.5*(TGV+TKV)                                                 5362.   
      ITTS=1                                                            5363.   
      SINA=.4                                                           5364.   
      COSA=SQRT(1.-SINA*SINA)                                           5365.   
      TSV=TSV0                                                          5365.5  
      EDS1=60.                                                          5365.6  
      DM=1.                                                             5365.7  
      DH=1.35                                                           5365.8  
         RIGS=0.                                                        5365.9  
         RIS1=0.                                                        5365.95 
      IF(TKV-TGV.LE..001) GO TO 3390                                    5366.   
C**** ITERATION LOOP TO SOLVE FOR FST (ASSUMING A FIXED TSV0)           5367.   
 3319 ITSF=1                                                            5368.   
      SINAF=SINA                                                        5369.   
      COSAF=COSA                                                        5370.   
 3320 WSSQ=WG*WG*(1.-2.*SINAF*COSAF)                                    5371.   
      RIGS=ZGS*GRAV*(TSV0-TGV)/(TGV*WSSQ+1.D-20)                        5372.   
      DMR=1.+(11.238+89.9*RIGS)*RIGS                                    5373.   
      RIS1=(TKV-TSV0)*GRAV*ZS1/(TKV*2.*WG*WG*SINAF*SINAF+1.D-20)        5374.   
      EDS1=60./(1.+50.*RIS1)                                            5375.   
      REDS1=SQRT(EDS1)                                                  5376.   
      FST=WSSQ-SINAF*REDS1*DMR*(WG*ROOT2F/CDN)                          5377.   
      DWSQDS=WG*WG*(4.*SINAF*SINAF-2.)/COSAF                            5378.   
      DRGSDS=-RIGS*DWSQDS/(WSSQ+1.D-20)                                 5379.   
      DDMRDS=(11.238+179.8*RIGS)*DRGSDS                                 5380.   
      DRS1DS=-2.*RIS1/SINAF                                             5381.   
      DREDDS=-EDS1*REDS1*DRS1DS*(5./12.)                                5382.   
      DFDS=DWSQDS-(REDS1*DMR+SINAF*DREDDS*DMR+SINAF*REDS1*DDMRDS)       5383.   
     *  *(WG*ROOT2F/CDN)-1.D-10                                         5384.   
      SINAF1=SINAF-FST/DFDS                                             5385.   
      IF(ABS(SINAF1-SINAF).LT..001) GO TO 3330                          5386.   
      IF(ITSF.GE.15) THEN                                               5387.   
         WRITE (6,9991) I,J,ITYPE,NG,ZGS,WG,TGV,TKV,PS,FMAG,CDN,DSIG(1) 5387.1  
         WRITE (6,9992)                                                 5387.2  
     *     ITSF,TSV0,SINAF,WSSQ,RIGS,DMR,RIS1,EDS1,REDS1,FST,SINAF1,    5387.3  
     *     DWSQDS,DRGSDS,DDMRDS,DRS1DS,DREDDS,DFDS                      5387.4  
         GO TO 3330                                                     5387.5  
      END IF                                                            5387.6  
      IF(SINAF1.GT..1*SINAF+.9*.707107) SINAF1=.3*SINAF+.7*.707107      5388.   
      SINAF=SINAF1                                                      5389.   
      COSAF=SQRT(1.-SINAF*SINAF)                                        5390.   
      ITSF=ITSF+1                                                       5391.   
      GO TO 3320                                                        5392.   
 3330 DRGSDT=RIGS/(TSV0-TGV)                                            5393.   
      DDMRDT=(11.238+179.8*RIGS)*DRGSDT                                 5394.   
      DRS1DT=-RIS1/(TKV-TSV0)                                           5395.   
      DREDDT=-EDS1*REDS1*DRS1DT*(5./12.)                                5396.   
      DFDT=-SINAF*(DREDDT*DMR+REDS1*DDMRDT)*(WG*ROOT2F/CDN)             5397.   
C**** ITERATION LOOP TO SOLVE FOR GST (ASSUMING A FIXED TSV0)           5398.   
      ITSG=1                                                            5399.   
      SINAG=SINA                                                        5400.   
      COSAG=COSA                                                        5401.   
 3340 WSSQ=WG*WG*(1.-2.*SINAG*COSAG)                                    5402.   
      WS=SQRT(WSSQ)                                                     5403.   
      RIGS=(TSV0-TGV)*GRAV*ZGS/(TGV*WSSQ+1.D-20)                        5404.   
      DM=1./(1.+(11.238+89.9*RIGS)*RIGS)                                5405.   
      DH=1.35/(1.+1.93*RIGS)                                            5406.   
      RIS1=(TKV-TSV0)*GRAV*ZS1/(TKV*2.*WG*WG*SINAG*SINAG+1.D-20)        5407.   
      EDS1=60./(1.+50.*RIS1)                                            5408.   
      GST=EDS1*(TKV-TSV0)-ZS1*CDN*DM*DH*WS*(TSV0-TGV)                   5409.   
      DWSDS=WG*WG*(4.*SINAG*SINAG-2.)/(COSAG*2.*WS+1.D-20)              5410.   
      DRGSDS=-RIGS*DWSQDS/(WSSQ+1.D-20)                                 5411.   
      DDMDS=-DM*DM*(11.238+179.8*RIGS)*DRGSDS                           5412.   
      DDHDS=-DH*DH*1.42963*DRGSDS                                       5413.   
      DRS1DS=-2.*RIS1/SINAG                                             5414.   
      DES1DS=-EDS1*EDS1*DRS1DS*(50./60.)                                5415.   
      DGDS=DES1DS*(TKV-TSV0)                                            5416.   
     *  -ZS1*CDN*(DDMDS*DH*WS+DM*DDHDS*WS+DM*DH*DWSDS)*(TSV0-TGV)       5417.   
      SINAG1=SINAG-GST/DGDS                                             5418.   
      IF(ABS(SINAG1-SINAG).LT..001) GO TO 3350                          5419.   
      IF(ITSG.GE.10) THEN                                               5420.   
         WRITE (6,9991) I,J,ITYPE,NG,ZGS,WG,TGV,TKV,PS,FMAG,CDN,DSIG(1) 5420.1  
         WRITE (6,9993)                                                 5420.2  
     *     ITSG,TSV0,SINAG,WS,RIGS,DM,DH,RIS1,EDS1,GST,SINAG1,          5420.3  
     *     DWSDS,DRGSDS,DDMDS,DDHDS,DRS1DS,DES1DS,DGDS                  5420.4  
         GO TO 3350                                                     5420.5  
      END IF                                                            5420.6  
      IF(SINAG1.LT..1*SINAG) SINAG1=.3*SINAG                            5421.   
      IF(SINAG1.GT..1*SINAG+.9*.707107) SINAG1=.3*SINAG+.7*.707107      5422.   
      SINAG=SINAG1                                                      5423.   
      COSAG=SQRT(1.-SINAG*SINAG)                                        5424.   
      ITSG=ITSG+1                                                       5425.   
      GO TO 3340                                                        5426.   
 3350 DRGSDT=RIGS/(TSV0-TGV)                                            5427.   
      DDMDT=-DM*DM*(11.238+179.8*RIGS)*DRGSDT                           5428.   
      DDHDT=-DH*DH*1.42963*DRGSDT                                       5429.   
      DRS1DT=-RIS1/(TKV-TSV0)                                           5430.   
      DES1DT=-EDS1*EDS1*DRS1DT*(50./60.)                                5431.   
      DGDT=DES1DT*(TKV-TSV0)-EDS1-ZS1*WS*CDN                            5432.   
     *  *((DDMDT*DH+DM*DDHDT)*(TSV0-TGV)+DM*DH)                         5433.   
C**** TEST FOR SOLUTION, IF NOT FOUND DETERMINE NEXT ITERATION          5434.   
      TSV=TSV0+(FST*DGDS-GST*DFDS+(SINAG-SINAF)*DFDS*DGDS)              5435.   
     *  /(DFDS*DGDT-DFDT*DGDS+1.D-20)                                   5436.   
      SINA=SINAF-(FST+(TSV-TSV0)*DFDT)/DFDS                             5437.   
      IF(SINAF.GT.SINAG) GO TO 3370                                     5438.   
      IF((TSV0-TGV)/(TSV0-TSV+1.D-20).LT.1.1) TSV=.7*TGV+.3*TSV0        5439.   
      IF((SINA.LT.SINAF).OR.(SINAG.LE.SINA)) GO TO 3375                 5440.   
 3365 COSA=SQRT(1.-SINA*SINA)                                           5441.   
      GO TO 3380                                                        5442.   
 3370 IF((TKV-TSV0)/(TSV-TSV0+1.D-20).LT.1.1) TSV=.3*TSV0+.7*TKV        5443.   
      IF((SINAG.LT.SINA).AND.(SINA.LE.SINAF)) GO TO 3365                5444.   
 3375 SINA=SINAG                                                        5445.   
      COSA=COSAG                                                        5446.   
 3380 IF(ABS(SINAG-SINAF).LT..002) GO TO 3390                           5447.   
      IF(ITTS.GT.15) THEN                                               5448.   
         WRITE (6,9991) I,J,ITYPE,NG,ZGS,WG,TGV,TKV,PS,FMAG,CDN,DSIG(1) 5448.1  
         WRITE (6,9992)                                                 5448.15 
     *     ITSF,TSV0,SINAF,WSSQ,RIGS,DMR,RIS1,EDS1,REDS1,FST,SINAF1,    5448.2  
     *     DWSQDS,DRGSDS,DDMRDS,DRS1DS,DREDDS,DFDS,                     5448.25 
     *            DRGSDT,DDMRDT,DRS1DT,DREDDT,DFDT                      5448.3  
         WRITE (6,9993)                                                 5448.35 
     *     ITSG,TSV0,SINAG,WS,RIGS,DM,DH,RIS1,EDS1,GST,SINAG1,          5448.4  
     *     DWSDS,DRGSDS,DDMDS,DDHDS,DRS1DS,DES1DS,DGDS,                 5448.45 
     *           DRGSDT,DDMDT,DDHDT,DRS1DT,DES1DT,DGDT                  5448.5  
         WRITE (6,9994)                                                 5448.55 
     *     ITTS,TSV0,SINAF,FST,DFDS,DFDT,SINAG,GST,DGDS,DGDT,TSV,SINA   5448.6  
         GO TO 3390                                                     5448.65 
      END IF                                                            5448.7  
      TSV0=TSV                                                          5449.   
      ITTS=ITTS+1                                                       5450.   
      GO TO 3319                                                        5451.   
 3390 WS=WG*SQRT(1.-2.*SINA*COSA)                                       5452.   
      CDH=CDN*DM*DH                                                     5453.   
      RCDHWS=CDH*WS*100.*PS/(RGAS*TSV)                                  5454.   
      IF(SINAP.LE..6427876) GO TO 3392                                  5455.   
      SINAP=.6427876                                                    5456.   
      COSAP=.7660444                                                    5457.   
      GO TO 3395                                                        5458.   
 3392 IF(-SINAP.LE..5*SINA) GO TO 3395                                  5459.   
      SINAP=-.5*SINA                                                    5460.   
      COSAP=SQRT(1.-SINAP*SINAP)                                        5461.   
 3395 SINAA=SINA*COSAP+COSA*SINAP                                       5462.   
      COSAA=COSA*COSAP-SINA*SINAP                                       5463.   
      IF(SINAA.LE..6427876) GO TO 3396                                  5464.   
      SINAA=.6427876                                                    5465.   
      COSAA=.7660444                                                    5466.   
 3396 USR=1.-SINAA*(COSAA+SINAA)                                        5467.   
      VSR=HEMI*SINAA*(COSAA-SINAA)                                      5468.   
      US=USR*UG-VSR*VG                                                  5469.   
      VS=VSR*UG+USR*VG                                                  5470.   
 3397 WS=WG*SQRT(1.-2.*SINAA*COSAA)                                     5471.   
      DGS=ZS1*CDN*DM*DH*WS                                              5472.   
C**** CALCULATE QS                                                      5473.   
 3400 QS=(QG*DGS+Q1*EDS1)/(DGS+EDS1)                                    5474.   
      TS=TSV/(1.+QS*RVX)                                                5475.   
         QSATS=QSAT(TS,PS,ELHX)                                         5476.   
C**** CALCULATE RHOS*CDM*WS AND RHOS*CDH*WS                             5482.   
 3500 CDM=CDN*DM                                                        5483.   
      CHDAT(I,J)=CDH                                                    5483.4  
      RCDMWS=CDM*WS*100.*PS/(RGAS*TS)                                   5484.   
      RCDMWS=RCDMWS*SINAA/SINA                                          5485.   
C**** CALCULATE FLUXES OF SENSIBLE HEAT, LATENT HEAT, THERMAL           5486.   
C****   RADIATION, AND CONDUCTION HEAT (WATTS/M**2)                     5487.   
      SHEAT=SHA*RCDHWS*(TS-TG)                                          5488.   
      BETAUP=BETA                                                       5489.   
      IF(QS.GT.QG) BETAUP=1.                                            5490.   
      EVHEAT=(LHE+TG1*SHV)*BETAUP*RCDHWS*(QS-QG)                        5491.   
         POTEVH=LHE*RCDHWS*(QS-QG)                                      5491.2  
      TRHEAT=TRHR(I,J,1)-STBO*(TG*TG)*(TG*TG)                           5492.   
      IF(ITYPE.EQ.1) GO TO 3620                                         5493.   
C**** CALCULATE FLUXES USING IMPLICIT TIME STEP FOR NON-OCEAN POINTS    5494.   
      F0=SRHEAT+TRHEAT+SHEAT+EVHEAT                                     5495.   
      F1=(TG1-CDTERM-F0*Z1BY6L)*CDENOM                                  5496.   
      DSHDTG=-RCDHWS*EDS1*SHA/(DGS+EDS1)                                5497.   
      DQGDTG=QG*ELHX/(RVAP*TG*TG)                                       5498.   
      DEVDTG=-RCDHWS*EDS1*LHE*BETAUP*DQGDTG/(DGS+EDS1)                  5499.   
         DPEVDT=-RCDHWS*EDS1*LHE*DQGDTG/(DGS+EDS1)                      5499.1  
      DTRDTG=-4.*STBO*TG*TG*TG                                          5500.   
      DF0DTG=DSHDTG+DEVDTG+DTRDTG                                       5501.   
      DFDTG=DF0DTG-(1.-DF0DTG*Z1BY6L)*CDENOM                            5502.   
      DTG=(F0-F1)*DTGRND/(HC1-DTGRND*DFDTG)                             5503.   
      SHDT=SHDT+DTGRND*(SHEAT+DTG*DSHDTG)                               5504.   
      EVHDT=EVHDT+DTGRND*(EVHEAT+DTG*DEVDTG)                            5505.   
         PEVHDT=PEVHDT+DTGRND*(POTEVH+DTG*DPEVDT)                       5505.1  
      TRHDT=TRHDT+DTGRND*(TRHEAT+DTG*DTRDTG)                            5506.   
      F1DT=F1DT+DTGRND*(TG1-CDTERM-(F0+DTG*DFDTG)*Z1BY6L)*CDENOM        5507.   
      DU1(I,J)=DU1(I,J)+PTYPE*DTGRND*RCDMWS*US/RMBYA                    5508.   
      DV1(I,J)=DV1(I,J)+PTYPE*DTGRND*RCDMWS*VS/RMBYA                    5509.   
      TG1=TG1+DTG                                                       5510.   
 3600 CONTINUE                                                          5511.   
      GO TO 3700                                                        5512.   
C**** CALCULATE FLUXES USING EXPLICIT TIME STEP FOR OCEAN POINTS        5513.   
 3620 SHDT=DTSURF*SHEAT                                                 5514.   
      EVHDT=DTSURF*EVHEAT                                               5515.   
         PEVHDT=DTSURF*POTEVH                                           5515.1  
      TRHDT=DTSURF*TRHEAT                                               5516.   
      DU1(I,J)=DU1(I,J)+PTYPE*DTSURF*RCDMWS*US/RMBYA                    5517.   
      DV1(I,J)=DV1(I,J)+PTYPE*DTSURF*RCDMWS*VS/RMBYA                    5518.   
C**** CALCULATE EVAPORATION                                             5519.   
 3700 DQ1=EVHDT/((LHE+TG1*SHV)*RMBYA)                                   5520.   
      IF(DQ1*PTYPE.LE.Q1) GO TO 3720                                    5521.   
      DQ1=Q1/PTYPE                                                      5522.   
      EVHDT=DQ1*(LHE+TG1*SHV)*RMBYA                                     5523.   
 3720 EVAP=-DQ1*RMBYA                                                   5524.   
         POTEVP=0.                                                      5524.1  
         IF(PEVHDT.LT.0.) POTEVP=-PEVHDT/LHE                            5524.2  
C**** ACCUMULATE SURFACE FLUXES AND PROGNOSTIC AND DIAGNOSTIC QUANTITIES5525.   
      F0DT=DTSURF*SRHEAT+TRHDT+SHDT+EVHDT                               5526.   
      E0(I,J,ITYPE)=E0(I,J,ITYPE)+F0DT                                  5527.   
      E1(I,J,ITYPE)=E1(I,J,ITYPE)+F1DT                                  5528.   
      EVAPOR(I,J,ITYPE)=EVAPOR(I,J,ITYPE)+EVAP                          5529.   
      TGRND(I,J,ITYPE)=TG1                                              5530.   
      TH1=TH1-SHDT*PTYPE/(SHA*RMBYA*P1K)                                5531.   
      Q1=Q1-DQ1*PTYPE                                                   5532.   
      USS=USS+US*PTYPE                                                  5533.   
      VSS=VSS+VS*PTYPE                                                  5534.   
      WSS=WSS+WS*PTYPE                                                  5535.   
      TSS=TSS+TS*PTYPE                                                  5536.   
      QSS=QSS+QS*PTYPE                                                  5537.   
      TAUS=TAUS+CDM*WS*WS*PTYPE                                         5538.   
         RTAUS=RTAUS+RCDMWS*WS*PTYPE                                    5539.   
         RTAUUS=RTAUUS+RCDMWS*US*PTYPE                                  5540.   
         RTAUVS=RTAUVS+RCDMWS*VS*PTYPE                                  5541.   
         SINAPS=SINAPS+SINAP*PTYPE                                      5542.   
         COSAPS=COSAPS+COSAP*PTYPE                                      5543.   
         TG1S=TG1S+TG1*PTYPE                                            5544.   
         QGS=QGS+QG*PTYPE                                               5545.   
         TRHDTS=TRHDTS+TRHDT*PTYPE                                      5547.   
         SHDTS=SHDTS+SHDT*PTYPE                                         5548.   
         EVHDTS=EVHDTS+EVHDT*PTYPE                                      5549.   
         UGS=UGS+UG*PTYPE                                               5550.   
         VGS=VGS+VG*PTYPE                                               5551.   
         WGS=WGS+WG*PTYPE                                               5552.   
         USRS=USRS+USR*PTYPE                                            5553.   
         VSRS=VSRS+VSR*PTYPE                                            5554.   
         RIS1S=RIS1S+RIS1*PTYPE                                         5555.   
         RIGSS=RIGSS+RIGS*PTYPE                                         5556.   
         CDMS=CDMS+CDM*PTYPE                                            5557.   
         CDHS=CDHS+CDH*PTYPE                                            5558.   
         DGSS=DGSS+DGS*PTYPE                                            5559.   
         EDS1S=EDS1S+EDS1*PTYPE                                         5560.   
         PPBLS=PPBLS+PPBL*PTYPE                                         5561.   
         EVAPS=EVAPS+EVAP*PTYPE                                         5562.   
         PEVAPS=PEVAPS+POTEVP*PTYPE                                     5562.1  
         QSATSS=QSATSS+QSATS*PTYPE                                      5562.5  
      GO TO (4000,4100,4400,4600),ITYPE                                 5563.   
C****                                                                   5564.   
C**** OCEAN                                                             5565.   
C****                                                                   5566.   
 4000    ASHDT=ASHDT+SHDT*POCEAN                                        5567.   
         AEVHDT=AEVHDT+EVHDT*POCEAN                                     5568.   
         ATRHDT=ATRHDT+TRHDT*POCEAN                                     5569.   
         ATS=ATS+(TS-TF)*POCEAN                                         5570.   
            OA(I,J,6)=OA(I,J,6)+TRHDT                                   5570.1  
            OA(I,J,7)=OA(I,J,7)+SHDT                                    5570.2  
            OA(I,J,8)=OA(I,J,8)+EVHDT                                   5570.3  
      GO TO 2200                                                        5571.   
C****                                                                   5572.   
C**** OCEAN ICE                                                         5573.   
C****                                                                   5574.   
 4100    CSHDT=CSHDT+SHDT*POICE                                         5575.   
         CEVHDT=CEVHDT+EVHDT*POICE                                      5576.   
         CTRHDT=CTRHDT+TRHDT*POICE                                      5577.   
         CTS=CTS+(TS-TF)*POICE                                          5578.   
            OA(I,J,9)=OA(I,J,9)+TRHDT                                   5578.1  
            OA(I,J,10)=OA(I,J,10)+SHDT                                  5578.2  
            OA(I,J,11)=OA(I,J,11)+EVHDT                                 5578.3  
      GO TO 2400                                                        5579.   
C****                                                                   5580.   
C**** LAND ICE                                                          5581.   
C****                                                                   5582.   
 4400    BSHDT=BSHDT+SHDT*PLICE                                         5583.   
         BEVHDT=BEVHDT+EVHDT*PLICE                                      5584.   
         BTRHDT=BTRHDT+TRHDT*PLICE                                      5585.   
         BTS=BTS+(TS-TF)*PLICE                                          5586.   
         AIJ(I,J,71)=AIJ(I,J,71)+(TS-TF)                                5586.5  
         AIJ(I,J,73)=AIJ(I,J,73)+SHDT                                   5586.6  
         AIJ(I,J,74)=AIJ(I,J,74)+EVHDT                                  5586.7  
         AIJ(I,J,75)=AIJ(I,J,75)+TRHDT                                  5586.8  
      GO TO 2600                                                        5587.   
C****                                                                   5588.   
C**** EARTH                                                             5589.   
C****                                                                   5590.   
 4600    BSHDT=BSHDT+SHDT*PEARTH                                        5591.   
         BEVHDT=BEVHDT+EVHDT*PEARTH                                     5592.   
         BTRHDT=BTRHDT+TRHDT*PEARTH                                     5593.   
         BTS=BTS+(TS-TF)*PEARTH                                         5594.   
         IF(WARMER.LT.0.) GO TO 4610                                    5594.11 
         IF(TS.LT.TF) TSFREZ(I,J,1)=TIMEZ                               5594.12 
         TSFREZ(I,J,2)=TIMEZ                                            5594.13 
         GO TO 4620                                                     5594.14 
 4610    IF(TSFREZ(I,J,2)+.03.LT.TIMEZ) GO TO 4620                      5594.15 
         IF(TS.GE.TF) TSFREZ(I,J,2)=TIMEZ                               5594.16 
 4620    IF(TG1.LT.TDIURN(I,J,1)) TDIURN(I,J,1)=TG1                     5594.21 
         IF(TG1.GT.TDIURN(I,J,2)) TDIURN(I,J,2)=TG1                     5594.22 
         IF(TS.LT.TDIURN(I,J,3)) TDIURN(I,J,3)=TS                       5594.23 
         IF(TS.GT.TDIURN(I,J,4)) TDIURN(I,J,4)=TS                       5594.24 
C**** NON-OCEAN POINTS WHICH ARE NOT MELTING OR FREEZING WATER USE      5595.   
C****   IMPLICIT TIME STEPS                                             5596.   
C****                                                                   5597.   
C**** UPDATE SURFACE AND FIRST LAYER QUANTITIES                         5598.   
C****                                                                   5599.   
 5000 T(I,J,1)=TH1                                                      5600.   
      Q(I,J,1)=Q1                                                       5601.   
      BLDATA(I,J,1)=WSS                                                 5602.   
      BLDATA(I,J,2)=TSS                                                 5603.   
      BLDATA(I,J,3)=QSS                                                 5604.   
      BLDATA(I,J,6)=USS                                                 5605.   
      BLDATA(I,J,7)=VSS                                                 5606.   
      BLDATA(I,J,8)=TAUS                                                5607.   
C****                                                                   5608.   
C**** ACCUMULATE DIAGNOSTICS                                            5609.   
C****                                                                   5610.   
C**** QUANTITIES ACCUMULATED FOR REGIONS IN DIAGJ                       5611.   
         IF(JR.EQ.24) GO TO 5700                                        5612.   
         DJ(JR,9)=DJ(JR,9)+TRHDTS*DXYPJ                                 5613.   
         DJ(JR,13)=DJ(JR,13)+SHDTS*DXYPJ                                5614.   
         DJ(JR,14)=DJ(JR,14)+EVHDTS*DXYPJ                               5615.   
         DJ(JR,19)=DJ(JR,19)+EVAPS*DXYPJ                                5616.   
         IF(MODDSF.NE.0) GO TO 5700                                     5617.   
         DJ(JR,23)=DJ(JR,23)+(TSS-TF)*DXYPJ                             5618.   
C**** QUANTITIES ACCUMULATED FOR LATITUDE-LONGITUDE MAPS IN DIAGIJ      5619.   
 5700    AIJ(I,J,4)=AIJ(I,J,4)+SHDTS                                    5620.   
         IF(MODRD.EQ.0) AIJ(I,J,21)=AIJ(I,J,21)+TRHDTS/DTSRCE           5621.   
         AIJ(I,J,22)=AIJ(I,J,22)+(SRHDTS+TRHDTS)                        5622.   
         AIJ(I,J,23)=AIJ(I,J,23)+(SRHDTS+TRHDTS+SHDTS+EVHDTS)           5623.   
         AIJ(I,J,79)=AIJ(I,J,79)+PEVAPS                                 5623.1  
         TDIURN(I,J,5)=TDIURN(I,J,5)+(TSS-TF)                           5623.5  
         IF (TSS.GT.TDIURN(I,J,6)) TDIURN(I,J,6)=TSS                    5623.6  
         IF (TSS.GT.AIJ(I,J,76)) AIJ(I,J,76)=TSS                        5623.7  
         IF (TSS.LT.AIJ(I,J,77)) AIJ(I,J,77)=TSS                        5623.8  
         IF(MODDSF.NE.0) GO TO 5800                                     5624.   
         AIJ(I,J,34)=AIJ(I,J,34)+ATAN(SINAPS/COSAPS)                    5626.   
         AIJ(I,J,35)=AIJ(I,J,35)+(TSS-TF)                               5627.   
         AIJ(I,J,36)=AIJ(I,J,36)+USS                                    5628.   
         AIJ(I,J,37)=AIJ(I,J,37)+VSS                                    5629.   
         AIJ(I,J,46)=AIJ(I,J,46)+ATAN(VSRS/USRS)                        5630.   
         AIJ(I,J,47)=AIJ(I,J,47)+RTAUS                                  5631.   
         AIJ(I,J,48)=AIJ(I,J,48)+RTAUUS                                 5632.   
         AIJ(I,J,49)=AIJ(I,J,49)+RTAUVS                                 5633.   
         AIJ(I,J,51)=AIJ(I,J,51)+QSS                                    5633.4  
         AIJ(I,J,53)=AIJ(I,J,53)                                        5633.5  
     *     +(40.6+.72*(2.*(TSS-TF)-(QSATSS-QSS)*LHE/SHA))               5633.6  
C**** QUANTITIES ACCUMULATED HOURLY FOR DIAG6                           5634.   
 5800    IF(MODD6.NE.0) GO TO 6000                                      5635.   
         DO 5820 KR=1,4                                                 5636.   
         IF(I.EQ.IJD6(1,KR).AND.J.EQ.IJD6(2,KR)) GO TO 5840             5637.   
 5820    CONTINUE                                                       5638.   
         GO TO 6000                                                     5639.   
 5840    ADAILY(IHOUR,1,KR)=ADAILY(IHOUR,1,KR)+S0*COSZ1(I,J)            5640.   
         ADAILY(IHOUR,6,KR)=ADAILY(IHOUR,6,KR)+PS                       5641.   
         ADAILY(IHOUR,7,KR)=ADAILY(IHOUR,7,KR)+PSK*T(I,J,5)             5642.   
         ADAILY(IHOUR,8,KR)=ADAILY(IHOUR,8,KR)+PSK*T(I,J,4)             5643.   
         ADAILY(IHOUR,9,KR)=ADAILY(IHOUR,9,KR)+PSK*T(I,J,3)             5644.   
         ADAILY(IHOUR,10,KR)=ADAILY(IHOUR,10,KR)+PSK*T(I,J,2)           5645.   
         ADAILY(IHOUR,11,KR)=ADAILY(IHOUR,11,KR)+PSK*T(I,J,1)           5646.   
         ADAILY(IHOUR,12,KR)=ADAILY(IHOUR,12,KR)+TSS                    5647.   
         ADAILY(IHOUR,13,KR)=ADAILY(IHOUR,13,KR)+(TG1S+TF)              5648.   
         ADAILY(IHOUR,14,KR)=ADAILY(IHOUR,14,KR)+Q(I,J,5)               5649.   
         ADAILY(IHOUR,15,KR)=ADAILY(IHOUR,15,KR)+Q(I,J,4)               5650.   
         ADAILY(IHOUR,16,KR)=ADAILY(IHOUR,16,KR)+Q(I,J,3)               5651.   
         ADAILY(IHOUR,17,KR)=ADAILY(IHOUR,17,KR)+Q(I,J,2)               5652.   
         ADAILY(IHOUR,18,KR)=ADAILY(IHOUR,18,KR)+Q1                     5653.   
         ADAILY(IHOUR,19,KR)=ADAILY(IHOUR,19,KR)+QSS                    5654.   
         ADAILY(IHOUR,20,KR)=ADAILY(IHOUR,20,KR)+QGS                    5655.   
         ADAILY(IHOUR,28,KR)=ADAILY(IHOUR,28,KR)+SRHDTS                 5656.   
         ADAILY(IHOUR,29,KR)=ADAILY(IHOUR,29,KR)+TRHDTS                 5657.   
         ADAILY(IHOUR,30,KR)=ADAILY(IHOUR,30,KR)+SHDTS                  5658.   
         ADAILY(IHOUR,31,KR)=ADAILY(IHOUR,31,KR)+EVHDTS                 5659.   
         ADAILY(IHOUR,32,KR)=ADAILY(IHOUR,32,KR)                        5660.   
     *       +SRHDTS+TRHDTS+SHDTS+EVHDTS                                5661.   
         ADAILY(IHOUR,33,KR)=ADAILY(IHOUR,33,KR)+UGS                    5662.   
         ADAILY(IHOUR,34,KR)=ADAILY(IHOUR,34,KR)+VGS                    5663.   
         ADAILY(IHOUR,35,KR)=ADAILY(IHOUR,35,KR)+WGS                    5664.   
         ADAILY(IHOUR,36,KR)=ADAILY(IHOUR,36,KR)+USS                    5665.   
         ADAILY(IHOUR,37,KR)=ADAILY(IHOUR,37,KR)+VSS                    5666.   
         ADAILY(IHOUR,38,KR)=ADAILY(IHOUR,38,KR)+WSS                    5667.   
         ADAILY(IHOUR,39,KR)=ADAILY(IHOUR,39,KR)+ATAN(VSRS/USRS)        5668.   
         ADAILY(IHOUR,40,KR)=ADAILY(IHOUR,40,KR)+RIS1S                  5669.   
         ADAILY(IHOUR,41,KR)=ADAILY(IHOUR,41,KR)+RIGSS                  5670.   
         ADAILY(IHOUR,42,KR)=ADAILY(IHOUR,42,KR)+CDMS                   5671.   
         ADAILY(IHOUR,43,KR)=ADAILY(IHOUR,43,KR)+CDHS                   5672.   
         ADAILY(IHOUR,44,KR)=ADAILY(IHOUR,44,KR)+DGSS                   5673.   
         ADAILY(IHOUR,45,KR)=ADAILY(IHOUR,45,KR)+EDS1S                  5674.   
         ADAILY(IHOUR,46,KR)=ADAILY(IHOUR,46,KR)+PPBLS                  5675.   
         ADAILY(IHOUR,50,KR)=ADAILY(IHOUR,50,KR)+EVAPS                  5676.   
 6000 IM1=I                                                             5677.   
C**** QUANTITIES ACCUMULATED FOR SURFACE TYPE TABLES IN DIAGJ           5678.   
         AJ(J,9)=AJ(J,9)+ATRHDT                                         5679.   
         BJ(J,9)=BJ(J,9)+BTRHDT                                         5680.   
         CJ(J,9)=CJ(J,9)+CTRHDT                                         5681.   
         AJ(J,13)=AJ(J,13)+ASHDT                                        5682.   
         BJ(J,13)=BJ(J,13)+BSHDT                                        5683.   
         CJ(J,13)=CJ(J,13)+CSHDT                                        5684.   
         AJ(J,14)=AJ(J,14)+AEVHDT                                       5685.   
         BJ(J,14)=BJ(J,14)+BEVHDT                                       5686.   
         CJ(J,14)=CJ(J,14)+CEVHDT                                       5687.   
         IF(MODDSF.NE.0) GO TO 7000                                     5688.   
         AJ(J,23)=AJ(J,23)+ATS                                          5689.   
         BJ(J,23)=BJ(J,23)+BTS                                          5690.   
         CJ(J,23)=CJ(J,23)+CTS                                          5691.   
 7000 CONTINUE                                                          5692.   
C****                                                                   5693.   
C**** ADD IN SURFACE FRICTION TO FIRST LAYER WIND                       5694.   
C****                                                                   5695.   
      DO 7600 I=1,IM                                                    5696.   
      U(I,2,1)=U(I,2,1)-2.*(DU1(1,1)*COSI(I)-DV1(1,1)*SINI(I))*RAPVN(1) 5697.   
      V(I,2,1)=V(I,2,1)-2.*(DV1(1,1)*COSI(I)+DU1(1,1)*SINI(I))*RAPVN(1) 5698.   
      U(I,JM,1)=U(I,JM,1)                                               5699.   
     *  -2.*(DU1(1,JM)*COSI(I)+DV1(1,JM)*SINI(I))*RAPVS(JM)             5700.   
 7600 V(I,JM,1)=V(I,JM,1)                                               5701.   
     *  -2.*(DV1(1,JM)*COSI(I)-DU1(1,JM)*SINI(I))*RAPVS(JM)             5702.   
      DO 7700 J=2,JM-1                                                  5703.   
      I=IM                                                              5704.   
      DO 7700 IP1=1,IM                                                  5705.   
      U(I,J,1)=U(I,J,1)-(DU1(I,J)+DU1(IP1,J))*RAPVS(J)                  5706.   
      V(I,J,1)=V(I,J,1)-(DV1(I,J)+DV1(IP1,J))*RAPVS(J)                  5707.   
      U(I,J+1,1)=U(I,J+1,1)-(DU1(I,J)+DU1(IP1,J))*RAPVN(J)              5708.   
      V(I,J+1,1)=V(I,J+1,1)-(DV1(I,J)+DV1(IP1,J))*RAPVN(J)              5709.   
 7700 I=IP1                                                             5710.   
C****                                                                   5711.   
C**** DRY CONVECTION ORIGINATING FROM THE FIRST LAYER                   5712.   
C****                                                                   5713.   
C**** LOAD U,V INTO UT,VT.  UT,VT WILL BE FIXED DURING DRY CONVECTION   5714.   
C****   WHILE U,V WILL BE UPDATED.                                      5715.   
      DO 8050 L=1,LM                                                    5716.   
      DO 8050 J=2,JM                                                    5717.   
      DO 8050 I=1,IM                                                    5718.   
      UT(I,J,L)=U(I,J,L)                                                5719.   
 8050 VT(I,J,L)=V(I,J,L)                                                5720.   
C**** OUTSIDE LOOPS OVER J AND I                                        5721.   
      DO 8500 J=1,JM                                                    5722.   
      POLE=.FALSE.                                                      5723.   
      IF(J.EQ.1.OR.J.EQ.JM) POLE=.TRUE.                                 5724.   
      IMAX=IM                                                           5725.   
      IF(POLE) IMAX=1                                                   5726.   
      DO 8120 K=1,4                                                     5727.   
      RA(K)=RAPVS(J)                                                    5728.   
 8120 RA(K+4)=RAPVN(J)                                                  5729.   
      IM1=IM                                                            5730.   
      DO 8500 I=1,IMAX                                                  5731.   
      BLDATA(I,J,4)=1.                                                  5732.   
      IF(T(I,J,1)*(1.+Q(I,J,1)*RVX).LE.                                 5733.   
     *   T(I,J,2)*(1.+Q(I,J,2)*RVX)) GO TO 8500                         5734.   
C**** MIX HEAT AND MOISTURE THROUGHOUT THE BOUNDARY LAYER               5735.   
      PKMS=PK(I,J,1)*DSIG(1)+PK(I,J,2)*DSIG(2)                          5736.   
      THPKMS=T(I,J,1)*(PK(I,J,1)*DSIG(1))+T(I,J,2)*(PK(I,J,2)*DSIG(2))  5737.   
      QMS=Q(I,J,1)*DSIG(1)+Q(I,J,2)*DSIG(2)                             5738.   
      TVMS=T(I,J,1)*(1.+Q(I,J,1)*RVX)*(PK(I,J,1)*DSIG(1))               5739.   
     *    +T(I,J,2)*(1.+Q(I,J,2)*RVX)*(PK(I,J,2)*DSIG(2))               5740.   
      THETA=TVMS/PKMS                                                   5741.   
      DO 8140 L=3,LM                                                    5742.   
      IF(THETA.LT.T(I,J,L)*(1.+Q(I,J,L)*RVX)) GO TO 8160                5743.   
      PKMS=PKMS+(PK(I,J,L)*DSIG(L))                                     5744.   
      THPKMS=THPKMS+T(I,J,L)*(PK(I,J,L)*DSIG(L))                        5745.   
      QMS=QMS+Q(I,J,L)*DSIG(L)                                          5746.   
      TVMS=TVMS+T(I,J,L)*(1.+Q(I,J,L)*RVX)*(PK(I,J,L)*DSIG(L))          5747.   
 8140 THETA=TVMS/PKMS                                                   5748.   
      L=LM+1                                                            5749.   
 8160 LMAX=L-1                                                          5750.   
      RDSIGS=1./(SIGE(1)-SIGE(LMAX+1))                                  5751.   
      THM=THPKMS/PKMS                                                   5752.   
      QMS=QMS*RDSIGS                                                    5753.   
      BLDATA(I,J,4)=LMAX                                                5754.   
      DO 8180 L=1,LMAX                                                  5755.   
         AJL(J,L,12)=AJL(J,L,12)+(THM-T(I,J,L))*PK(I,J,L)*P(I,J)        5756.   
      T(I,J,L)=THM                                                      5757.   
 8180 Q(I,J,L)=QMS                                                      5758.   
      IF(POLE) GO TO 8300                                               5759.   
C**** MIX MOMENTUM THROUGHOUT THE BOUNDARY LAYER AT NON-POLAR GRID BOXES5760.   
      ID(1)=IM1+(J-1)*IM                                                5761.   
      ID(2)=ID(1)+IM*JM*LM                                              5762.   
      ID(3)=I+(J-1)*IM                                                  5763.   
      ID(4)=ID(3)+IM*JM*LM                                              5764.   
      ID(5)=IM1+J*IM                                                    5765.   
      ID(6)=ID(5)+IM*JM*LM                                              5766.   
      ID(7)=I+J*IM                                                      5767.   
      ID(8)=ID(7)+IM*JM*LM                                              5768.   
      DO 8240 K=1,8                                                     5769.   
      UMS(K)=0.                                                         5770.   
      DO 8220 L=1,LMAX                                                  5771.   
 8220 UMS(K)=UMS(K)+UT(ID(K),1,L)*DSIG(L)                               5772.   
 8240 UMS(K)=UMS(K)*RDSIGS                                              5773.   
      DO 8260 L=1,LMAX                                                  5774.   
         AJL(J,L,38)=AJL(J,L,38)+(UMS(1)+UMS(3)-UT(IM1,J,L)-UT(I,J,L))* 5775.   
     *     P(I,J)*RA(1)                                                 5776.   
         AJL(J+1,L,38)=AJL(J+1,L,38)+(UMS(5)+UMS(7)-UT(IM1,J+1,L)-      5777.   
     *     UT(I,J+1,L))*P(I,J)*RA(5)                                    5778.   
      DO 8260 K=1,8                                                     5779.   
 8260 U(ID(K),1,L)=U(ID(K),1,L)+(UMS(K)-UT(ID(K),1,L))*RA(K)            5780.   
      GO TO 8400                                                        5781.   
C**** MIX MOMENTUM THROUGHOUT THE BOUNDARY LAYER AT POLAR GRID BOXES    5782.   
 8300 JVPO=2                                                            5783.   
      IF(J.EQ.JM) JVPO=JM                                               5784.   
      RAPO=2.*RAPVN(1)                                                  5785.   
      DO 8360 IPO=1,IM                                                  5786.   
      UMSPO=0.                                                          5787.   
      VMSPO=0.                                                          5788.   
      DO 8320 L=1,LMAX                                                  5789.   
      UMSPO=UMSPO+UT(IPO,JVPO,L)*DSIG(L)                                5790.   
 8320 VMSPO=VMSPO+VT(IPO,JVPO,L)*DSIG(L)                                5791.   
      UMSPO=UMSPO*RDSIGS                                                5792.   
      VMSPO=VMSPO*RDSIGS                                                5793.   
      DO 8340 L=1,LMAX                                                  5794.   
      U(IPO,JVPO,L)=U(IPO,JVPO,L)+(UMSPO-UT(IPO,JVPO,L))*RAPO           5795.   
      V(IPO,JVPO,L)=V(IPO,JVPO,L)+(VMSPO-VT(IPO,JVPO,L))*RAPO           5796.   
 8340    AJL(JVPO,L,38)=AJL(JVPO,L,38)                                  5797.   
     *  +(UMSPO-UT(IPO,JVPO,L))*P(1,J)*RAPO                             5798.   
 8360 CONTINUE                                                          5799.   
C**** ACCUMULATE BOUNDARY LAYER DIAGNOSTICS                             5800.   
 8400    IF(MODD6.NE.0) GO TO 8500                                      5801.   
         DO 8420 KR=1,4                                                 5802.   
         IF(I.EQ.IJD6(1,KR).AND.J.EQ.IJD6(2,KR)) GO TO 8440             5803.   
 8420    CONTINUE                                                       5804.   
         GO TO 8500                                                     5805.   
 8440    ADAILY(IHOUR,47,KR)=ADAILY(IHOUR,47,KR)+1.                     5806.   
         ADAILY(IHOUR,48,KR)=ADAILY(IHOUR,48,KR)+LMAX                   5807.   
 8500 IM1=I                                                             5808.   
 9000 CONTINUE                                                          5809.   
      RETURN                                                            5810.   
 9991 FORMAT ('0SURFACE ',4I4,5F10.4,3F11.7)                            5834.   
 9992 FORMAT ('0',I2,10F10.4/23X,4F10.4,10X,2F10.4/                     5835.   
     *  33X,3F10.4,10X,2F10.4)                                          5836.   
 9993 FORMAT ('0',I2,10F10.4/23X,7F10.4/33X,7F10.4)                     5837.   
 9994 FORMAT ('0',I2,11F10.4)                                           5838.   
      END                                                               5839.   
      SUBROUTINE GROUND                                                 6001.   
C****                                                                   6002.   
C**** THIS SUBROUTINE USES THE SURFACE FLUXES TO PREDICT IN TIME THE    6003.   
C**** GROUND TEMPERATURE, GROUND WATER AND ICE, AND SNOW MELTING.       6004.   
C****                                                                   6005.   
      INCLUDE 'BA94jalC9.COM'                                           6006.   
      COMMON U,V,T,P,Q                                                  6007.   
      COMMON/WORK1/CONV(IM,JM,LM),PK(IM,JM,LM),PREC(IM,JM),             6007.4  
     *  TPREC(IM,JM),COSZ1(IM,JM)                                       6007.5  
      COMMON/WORK3/E0(IM,JM,4),E1(IM,JM,4),EVAPOR(IM,JM,4)              6008.   
      COMMON/OT/OTA1(IM,JM),OTB1(IM,JM),OTC(IM,JM),                     6009.   
     *          OTA2(IM,JM),OTB2(IM,JM),OTA3(IM,JM),                    6009.1  
     *          OTB3(IM,JM),OTA4(IM,JM),OTB4(IM,JM)                     6009.2  
      COMMON/RDATA/ROUGHL(IM,JM),CHDAT(IM,JM)                           6009.4  
      DATA SHV/0./,SHW/4185./,SHI/2060./,RHOW/1000./,RHOI/916.6/,       6010.   
     *  ALAMI/2.1762/,TFO/-1.56/,Z1I/.1/,Z2LI/2.9/,Z1E/.1/,Z2E/4./      6011.   
      DATA Z2OIM/.4/,Z2OIX/4.9/                                         6012.   
      DATA TTRUNC/0./                                                   6013.   
      DATA IFIRST/1/                                                    6014.   
      DATA TF/273.16D0/,STBO/.567257D-7/                                6014.6  
C****                                                                   6015.   
C**** FDATA  2  LAND COVERAGE (1)                                       6016.   
C****        3  RATIO OF LAND ICE COVERAGE TO LAND COVERAGE (1)         6017.   
C****                                                                   6018.   
C**** ODATA  1  OCEAN TEMPERATURE (C)                                   6019.   
C****        2  RATIO OF OCEAN ICE COVERAGE TO WATER COVERAGE (1)       6020.   
C****        3  OCEAN ICE AMOUNT OF SECOND LAYER (KG/M**2)              6021.   
C****                                                                   6022.   
C**** GDATA  1  OCEAN ICE SNOW AMOUNT (KG/M**2)                         6023.   
C****        2  EARTH SNOW AMOUNT (KG/M**2)                             6024.   
C****        3  OCEAN ICE TEMPERATURE OF FIRST LAYER (C)                6025.   
C****        4  EARTH TEMPERATURE OF FIRST LAYER (C)                    6026.   
C****        5  EARTH WATER OF FIRST LAYER (KG/M**2)                    6027.   
C****        6  EARTH ICE OF FIRST LAYER (KG/M**2)                      6028.   
C****        7  OCEAN ICE TEMPERATURE OF SECOND LAYER (C)               6029.   
C****        8  EARTH TEMPERATURE OF SECOND LAYER (C)                   6030.   
C****        9  EARTH WATER OF SECOND LAYER (KG/M**2)                   6031.   
C****       10  EARTH ICE OF SECOND LAYER (KG/M**2)                     6032.   
C****       12  LAND ICE SNOW AMOUNT (KG/M**2)                          6033.   
C****       13  LAND ICE TEMPERATURE OF FIRST LAYER (C)                 6034.   
C****       14  LAND ICE TEMPERATURE OF SECOND LAYER (C)                6035.   
C****                                                                   6036.   
C**** VDATA  9  WATER FIELD CAPACITY OF FIRST LAYER (KG/M**2)           6037.   
C****       10  WATER FIELD CAPACITY OF SECOND LAYER (KG/M**2)          6038.   
C****                                                                   6039.   
      QSAT(TM,PR,QLH)=3.797915*EXP(QLH*(7.93252E-6-2.166847E-3/TM))/PR  6039.4  
      IF(IFIRST.NE.1) GO TO 50                                          6040.   
      IFIRST=0                                                          6041.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.NE.1) GO TO 10                                          6042.   
      IF(KOCEAN.EQ.0) GO TO 10                                 
c  ** END (CHANDED)
      READ (17) OTB1,OTB2,OTB3,OTB4,OTA1,OTA2,OTA3,OTA4,OTC             6042.9  
CDBL  CALL DREAD (17,OTA,IM*JM*3,OTA)  transports are in R*8            6043.   
      REWIND 17                                                         6044.   
   10 DTSRCE=NDYN*DT                                                    6045.   
      ACE1I=Z1I*RHOI                                                    6046.   
      AC2OIM=Z2OIM*RHOI                                                 6047.   
      BYZICX=1./(Z1I+Z2OIX)                                             6048.1  
      HC1I=ACE1I*SHI                                                    6049.   
      HC2LI=Z2LI*RHOI*SHI                                               6050.   
      HC1DE=Z1E*1129950.                                                6051.   
      HC2DE=Z2E*1129950.+3.5*.125*RHOW*3100.                            6052.   
      DIFFUS=DTSRCE/SDAY                                                6053.   
   50 ANGLE=TWOPI*JDAY/365.                                             6054.   
      SINANG=SIN(ANGLE)                                                 6055.   
      COSANG=COS(ANGLE)                                                 6056.   
      SIN2ANG=SIN(2.*ANGLE)                                             6056.1  
      COS2ANG=COS(2.*ANGLE)                                             6056.2  
      SIN3ANG=SIN(3.*ANGLE)                                             6056.3  
      COS3ANG=COS(3.*ANGLE)                                             6056.4  
      SIN4ANG=SIN(4.*ANGLE)                                             6056.5  
      COS4ANG=COS(4.*ANGLE)                                             6056.6  
                                                                        6056.7  
C****                                                                   6057.   
C**** OUTSIDE LOOP OVER J AND I, EXECUTED ONCE FOR EACH GRID POINT      6058.   
C****                                                                   6059.   
      DO 980 J=1,JM                                                     6060.   
      IMAX=IM                                                           6061.   
      IF((J.EQ.1).OR.(J.EQ.JM)) IMAX=1                                  6062.   
         BF1DT=0.                                                       6063.   
         CF1DT=0.                                                       6064.   
         AOTDT=0.                                                       6065.   
         COTDT=0.                                                       6066.   
         AEFO=0.                                                        6067.   
         CEFI=0.                                                        6068.   
         BEDIFS=0.                                                      6069.   
         CEDIFS=0.                                                      6070.   
         BERUN0=0.                                                      6071.   
         CF2DT=0.                                                       6072.   
         BERUN2=0.                                                      6073.   
         CERUN2=0.                                                      6074.   
         AERUN4=0.                                                      6075.   
         CERUN4=0.                                                      6076.   
         ATG1=0.                                                        6077.   
         BTG1=0.                                                        6078.   
         CTG1=0.                                                        6079.   
         ATG2=0.                                                        6080.   
         BTG2=0.                                                        6081.   
         CTG2=0.                                                        6082.   
         ATG3=0.                                                        6083.   
         AEVAP=0.                                                       6084.   
         BEVAP=0.                                                       6085.   
         CEVAP=0.                                                       6086.   
         BDIFS=0.                                                       6087.   
         CDIFS=0.                                                       6088.   
         AIFO=0.                                                        6089.   
         CIFI=0.                                                        6090.   
         BRUN0=0.                                                       6091.   
         CRUN0=0.                                                       6092.   
         BRUN2=0.                                                       6093.   
         CRUN2=0.                                                       6094.   
         ARUN4=0.                                                       6095.   
         CRUN4=0.                                                       6096.   
         BWTR1=0.                                                       6097.   
         BACE1=0.                                                       6098.   
         BWTR2=0.                                                       6099.   
         BACE2=0.                                                       6100.   
         CACE2=0.                                                       6101.   
         BSNOW=0.                                                       6102.   
         CSNOW=0.                                                       6103.   
         CICOV=0.                                                       6104.   
      DO 960 I=1,IMAX                                                   6105.   
C****                                                                   6106.   
C**** DETERMINE SURFACE CONDITIONS                                      6107.   
C****                                                                   6108.   
      PLAND=FDATA(I,J,2)                                                6109.   
      PWATER=1.-PLAND                                                   6110.   
      PLICE=FDATA(I,J,3)*PLAND                                          6111.   
      PEARTH=PLAND-PLICE                                                6112.   
      ROICE=ODATA(I,J,2)                                                6113.   
      POICE=ROICE*PWATER                                                6114.   
      POCEAN=PWATER-POICE                                               6115.   
         JR=JREG(I,J)                                                   6116.   
         DXYPJ=DXYP(J)                                                  6117.   
         SNOWS=0.                                                       6118.   
         WTR1S=0.                                                       6119.   
         ACE1S=0.                                                       6120.   
         WTR2S=0.                                                       6121.   
         ACE2S=0.                                                       6122.   
         TG1S=0.                                                        6123.   
         TG2S=0.                                                        6124.   
         EVAPS=0.                                                       6125.   
         RUN0S=0.                                                       6126.   
         DIFSS=0.                                                       6127.   
C****                                                                   6128.   
      IF(PWATER.LE.0.) GO TO 400                                        6129.   
C****                                                                   6130.   
C**** OCEAN                                                             6131.   
C****                                                                   6132.   
      EVAP=EVAPOR(I,J,1)                                                6133.   
         ATG1=ATG1+ODATA(I,J,1)*POCEAN                                  6134.   
         TG1S=TG1S+ODATA(I,J,1)*POCEAN                                  6135.   
         AEVAP=AEVAP+EVAP*POCEAN                                        6136.   
         EVAPS=EVAPS+EVAP*POCEAN                                        6137.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.EQ.1) GO TO 60                                          6138.   
      IF(KOCEAN.NE.0) GO TO 60                     
c  ** MFS (CHANGED)
         ATG2=ATG2+ODATA(I,J,1)*POCEAN                                  6139.   
         TG2S=TG2S+ODATA(I,J,1)*POCEAN                                  6140.   
      IF(POICE.GT.0.) GO TO 110                                         6141.   
      GO TO 400                                                         6142.   
   60 TGW=ODATA(I,J,1)                                                  6143.   
         AIJ(I,J,57)=AIJ(I,J,57)+TGW                                    6143.5  
      WTRO=Z1O(I,J)*RHOW                                                6144.   
      ENRGO0=WTRO*TGW*SHW                                               6145.   
      EOFRZ=WTRO*TFO*SHW                                                6146.   
      F0DT=E0(I,J,1)                                                    6147.   
         AIJ(I,J,65)=AIJ(I,J,65)+F0DT*POCEAN                            6147.5  
      OTDT=DTSRCE*(OTB4(I,J)*SIN4ANG+OTA4(I,J)*COS4ANG                  6148.   
     * +OTB3(I,J)*SIN3ANG+OTA3(I,J)*COS3ANG                             6148.01 
     * +OTB2(I,J)*SIN2ANG+OTA2(I,J)*COS2ANG                             6148.02 
     * +OTB1(I,J)*SINANG+OTA1(I,J)*COSANG+OTC(I,J))                     6148.03 
         ATG2=ATG2+ODATA(I,J,4)*POCEAN                                  6149.   
         TG2S=TG2S+ODATA(I,J,4)*POCEAN                                  6150.   
         ATG3=ATG3+ODATA(I,J,5)*POCEAN                                  6151.   
         AOTDT=AOTDT+OTDT*POCEAN                                        6152.   
      RUN4=0.                                                           6153.   
      ERUN4=RUN4*TGW*SHW                                                6154.   
         AERUN4=AERUN4+ERUN4*POCEAN                                     6155.   
         ARUN4=ARUN4+RUN4*POCEAN                                        6156.   
      ENRGO=F0DT+OTDT-ERUN4                                             6157.   
      IF(ENRGO0+ENRGO.LT.EOFRZ) GO TO 80                                6158.   
C**** FLUXES RECOMPUTE TGO WHICH IS ABOVE FREEZING POINT FOR OCEAN      6159.   
      ENRGFO=0.                                                         6160.   
      ACEFO=0.                                                          6161.   
      IF(ROICE.GT.0.) GO TO 100                                         6162.   
      ODATA(I,J,1)=TGW+(ENRGO/(WTRO*SHW)+TTRUNC)                        6163.   
      GO TO 400                                                         6164.   
C**** FLUXES COOL TGO TO FREEZING POINT FOR OCEAN AND FORM SOME ICE     6165.   
   80 ACEFO=(ENRGO0+ENRGO-EOFRZ)/(TFO*(SHI-SHW)-LHM)                    6166.   
      ENRGFO=ACEFO*(TFO*SHI-LHM)                                        6167.   
         AEFO=AEFO-ENRGFO*POCEAN                                        6168.   
         AIFO=AIFO-ACEFO*POCEAN                                         6169.   
      IF(ROICE.GT.0.) GO TO 100                                         6170.   
      ROICE=ACEFO/(ACE1I+AC2OIM)                                        6171.   
      ODATA(I,J,1)=TFO                                                  6172.   
      ODATA(I,J,2)=ROICE                                                6173.   
      GDATA(I,J,1)=0.                                                   6174.   
      GDATA(I,J,3)=TFO                                                  6175.   
      GDATA(I,J,7)=TFO                                                  6176.   
      ODATA(I,J,3)=AC2OIM                                               6177.   
      GO TO 400                                                         6178.   
C****                                                                   6179.   
  100 ACE2F=0.                                                          6180.   
      ACE2M=0.                                                          6181.   
C****                                                                   6182.   
C**** OCEAN ICE                                                         6183.   
C****                                                                   6184.   
  110 SNOW=GDATA(I,J,1)                                                 6185.   
      TG1=GDATA(I,J,3)                                                  6186.   
      TG2=GDATA(I,J,7)                                                  6187.   
      ACE2=ODATA(I,J,3)                                                 6188.   
         AIJ(I,J,1)=AIJ(I,J,1)+POICE                                    6188.4  
         AIJ(I,J,58)=AIJ(I,J,58)+ACE2*POICE                             6188.5  
      F0DT=E0(I,J,2)                                                    6189.   
         AIJ(I,J,66)=AIJ(I,J,66)+F0DT*POICE                             6189.5  
      F1DT=E1(I,J,2)                                                    6190.   
      EVAP=EVAPOR(I,J,2)                                                6191.   
         AIJ(I,J,62)=AIJ(I,J,62)+EVAP*POICE                             6191.5  
      Z2=ACE2/RHOI                                                      6192.   
c  ** MFS (CHANGED)
c         IF(KOCEAN.NE.1) GO TO 120                                      6193.   
         IF(KOCEAN.EQ.0) GO TO 120                                    
c  ** MFS (CHANGED)
      WTRI0=WTRO-(SNOW+ACE1I+ACE2)                                      6194.   
      EIW0=WTRI0*TGW*SHW                                                6195.   
      WTRW0=WTRO-ROICE*(SNOW+ACE1I+ACE2)                                6196.   
      ENRGW0=WTRW0*TGW*SHW                                              6197.   
      RUN0=0.                                                           6198.   
      DIFSI=0.                                                          6199.   
      EDIFSI=0.                                                         6200.   
      RUN4=-EVAP                                                        6201.   
      ERUN4=TGW*RUN4*SHW                                                6202.   
         CERUN4=CERUN4+ERUN4*POICE                                      6203.   
         CRUN4=CRUN4+RUN4*POICE                                         6204.   
C****                                                                   6205.   
C**** OCEAN ICE, CALCULATE TG1                                          6206.   
C****                                                                   6207.   
  120 SNANDI=SNOW+ACE1I-EVAP                                            6208.   
      HC1=SNANDI*SHI                                                    6209.   
      ENRG1=F0DT+EVAP*(TG1*SHI-LHM)-F1DT                                6210.   
      IF(ENRG1.LE.-TG1*HC1) GO TO 130                                   6211.   
C**** FLUXES HEAT UP TG1 TO FREEZING POINT AND MELT SOME SNOW AND ICE   6212.   
      RUN0=(ENRG1+TG1*HC1)/LHM                                          6213.   
      TG1=0.                                                            6214.   
      SNANDI=SNANDI-RUN0                                                6215.   
         CRUN0=CRUN0+RUN0*POICE                                         6216.   
         RUN0S=RUN0S+RUN0*POICE                                         6217.   
      GO TO 140                                                         6218.   
C**** FLUXES RECOMPUTE TG1 WHICH IS BELOW FREEZING POINT                6219.   
  130 TG1=TG1+ENRG1/HC1                                                 6220.   
  140 IF(SNANDI.GE.ACE1I) GO TO 160                                     6221.   
C**** SOME ICE HAS MELTED OR EVAPORATED, TAKE IT FROM G2                6222.   
      SNOW=0.                                                           6223.   
      DIFS=SNANDI-ACE1I                                                 6224.   
      TG1=(TG1*SNANDI-TG2*DIFS)/ACE1I                                   6225.   
      EDIFS=DIFS*(TG2*SHI-LHM)                                          6226.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.EQ.1) GO TO 150                                         6227.   
      IF(KOCEAN.NE.0) GO TO 150                                      
c  ** END (CHANGED)
         CEDIFS=CEDIFS+EDIFS*POICE                                      6228.   
         CDIFS=CDIFS+DIFS*POICE                                         6229.   
         CERUN2=CERUN2+EDIFS*POICE                                      6230.   
         CRUN2=CRUN2+DIFS*POICE                                         6231.   
         DIFSS=DIFSS+DIFS*POICE                                         6232.   
      GO TO 200                                                         6233.   
  150 ACE2=ACE2+(DIFS+PTRUNC)                                           6234.   
      DIFSI=ROICE*DIFS                                                  6235.   
      EDIFSI=ROICE*EDIFS                                                6236.   
      GO TO 210                                                         6237.   
  160 SNOW=SNANDI-ACE1I                                                 6238.   
c  ** MFS (CHANGED)
c      IF(KOCEAN.EQ.1) GO TO 210                                         6239.   
      IF(KOCEAN.NE.0) GO TO 210                                
c  ** END (CHANGED)
C****                                                                   6240.   
C**** OCEAN ICE, CALCULATE TG2                                          6241.   
C****                                                                   6242.   
  200 F2DT=DTSRCE*(TG2-TFO)*ALAMI*3./Z2-.5*F1DT                         6243.   
      TG2=TG2+(F1DT-F2DT)/(ACE2*SHI)                                    6244.   
      GO TO 370                                                         6245.   
  210 F2DT=DTSRCE*(TG2-TGW)*ALAMI*3./Z2-.5*F1DT                         6246.   
      ENRG2=F1DT-F2DT                                                   6247.   
      ENRGIW=F2DT+OTDT-ERUN4                                            6248.   
      ENRGFI=0.                                                         6249.   
      HC2=ACE2*SHI                                                      6250.   
      IF(ENRG2.LE.-TG2*HC2) GO TO 220                                   6251.   
C**** FLUXES HEAT UP TG2 TO FREEZING POINT AND MELT SOME ICE            6252.   
      ACE2M=(ENRG2+TG2*HC2)/LHM                                         6253.   
      TG2=0.                                                            6254.   
      ACE2=ACE2+(-ACE2M+PTRUNC)                                         6255.   
         AIFI=AIFI+ACE2M*POICE                                          6256.   
      GO TO 300                                                         6257.   
C**** CALCULATE THE ENERGY OF THE WATER BELOW THE ICE AT THE FREEZING   6258.   
C****   POINT AND TEST WHETHER NEW ICE MUST BE FORMED                   6259.   
  220 WTRI1=WTRO-(SNOW+ACE1I+ACE2)                                      6260.   
      EFIW=WTRI1*TFO*SHW                                                6261.   
      IF(EIW0+ENRGIW.LT.EFIW) GO TO 240                                 6262.   
C**** FLUXES RECOMPUTE TG2, THE WATER BELOW THE ICE IS ABOVE FREEZING   6263.   
      TG2=TG2+ENRG2/HC2                                                 6264.   
      GO TO 300                                                         6265.   
C**** FLUXES WOULD COOL TGIW TO BELOW FREEZING, FREEZE SOME MORE ICE    6266.   
  240 ACE2F=(EIW0+ENRGIW-EFIW)/(TFO*(SHI-SHW)-LHM)                      6267.   
      ENRGFI=ACE2F*(TFO*SHI-LHM)                                        6268.   
         CEFI=CEFI-ENRGFI*POICE                                         6269.   
         CIFI=CIFI-ACE2F*POICE                                          6270.   
      ACE2=ACE2+(ACE2F+PTRUNC)                                          6271.   
      TG2=TG2+(ENRG2+ACE2F*(TFO-TG2)*SHI)/(ACE2*SHI)                    6272.   
C****                                                                   6273.   
C**** CALCULATE COMPOSITE TEMPERATURES AND REDISTRIBUTION OF ICE        6274.   
C****                                                                   6275.   
  300 WTRW=WTRW0-(1.-ROICE)*ACEFO+ROICE*(RUN0-RUN4+ACE2M-ACE2F)         6276.   
      ENRGW=ENRGW0+(1.-ROICE)*(ENRGO-ENRGFO)+ROICE*(ENRGIW-ENRGFI)      6277.   
      TGW=ENRGW/(WTRW*SHW)+TTRUNC                                       6278.   
      IF(ACEFO.LE.0.) GO TO 310                                         6279.   
C**** NEW ICE FORMED ON THE OCEAN SURFACE                               6280.   
      DRO=(1.-ROICE)*ACEFO/(ACE1I+AC2OIM)                               6281.   
      TG1=TG1+(TFO-TG1)*DRO*ACE1I/(ROICE*(SNOW+ACE1I)+DRO*ACE1I)        6282.   
      TG2=TG2+(TFO-TG2)*DRO*AC2OIM/(ROICE*ACE2+DRO*AC2OIM)              6283.   
      SNOW=SNOW*ROICE/(ROICE+DRO)                                       6284.   
      ROICE=ROICE+DRO                                                   6285.   
      ACE2=ACE2+(DRO*(AC2OIM-ACE2)/ROICE+PTRUNC)                        6286.   
  310 IF(ACE2.GE.AC2OIM) GO TO 320                                      6287.   
C**** ICE IS TOO THIN, COMPRESS IT HORIZONTALLY                         6288.   
      ROICEN=ROICE*(ACE1I+ACE2)/(ACE1I+AC2OIM)                          6289.   
      GO TO 340                                                         6290.   
  320 OPNOCN=.05*(RHOI/(ACE1I+ACE2)-BYZICX)                             6291.   
      IF(1.-ROICE.GT.OPNOCN) GO TO 360                                  6291.1  
C**** TOO LITTLE OPEN OCEAN, COMPRESS THE ICE HORIZONTALLY              6292.   
      ROICEN=1.-OPNOCN                                                  6293.   
  340 DRI=ROICE-ROICEN                                                  6294.   
      DIFS=DRI*ACE1I/ROICE                                              6295.   
      SNOW=SNOW*(ROICE/ROICEN)                                          6296.   
      TG2=TG2+(TG1-TG2)*DIFS/(ACE2+DIFS)                                6297.   
      ACE2=ACE2+(DRI*(ACE1I+ACE2)/ROICEN+PTRUNC)                        6298.   
      EDIFSI=EDIFSI+ROICE*DIFS*(TG1*SHI-LHM)                            6299.   
      DIFSI=DIFSI+ROICE*DIFS                                            6300.   
      ROICE=ROICEN                                                      6301.   
C**** RESAVE PROGNOSTIC QUANTITIES                                      6302.   
  360 ODATA(I,J,1)=TGW                                                  6303.   
      ODATA(I,J,2)=ROICE                                                6304.   
      ODATA(I,J,3)=ACE2                                                 6305.   
         COTDT=COTDT+OTDT*POICE                                         6306.   
         CEDIFS=CEDIFS+EDIFSI*PWATER                                    6307.   
         CDIFS=CDIFS+DIFSI*PWATER                                       6308.   
         DIFSS=DIFSS+DIFSI*PWATER                                       6309.   
  370 GDATA(I,J,1)=SNOW                                                 6310.   
      GDATA(I,J,3)=TG1                                                  6311.   
      GDATA(I,J,7)=TG2                                                  6312.   
         CSNOW=CSNOW+SNOW*POICE                                         6313.   
         CTG1=CTG1+TG1*POICE                                            6314.   
         CTG2=CTG2+TG2*POICE                                            6315.   
         CACE2=CACE2+ACE2*POICE                                         6316.   
         CF1DT=CF1DT+F1DT*POICE                                         6317.   
         CF2DT=CF2DT+F2DT*POICE                                         6318.   
         CEVAP=CEVAP+EVAP*POICE                                         6319.   
         CICOV=CICOV+POICE                                              6320.   
         SNOWS=SNOWS+SNOW*POICE                                         6321.   
         TG1S=TG1S+TG1*POICE                                            6322.   
         ACE1S=ACE1S+ACE1I*POICE                                        6323.   
         ACE2S=ACE2S+ACE2*POICE                                         6324.   
         TG2S=TG2S+TG2*POICE                                            6325.   
         EVAPS=EVAPS+EVAP*POICE                                         6326.   
C****                                                                   6327.   
  400 IF(PLICE.LE.0.) GO TO 600                                         6328.   
C****                                                                   6329.   
C**** LAND ICE                                                          6330.   
C****                                                                   6331.   
      SNOW=GDATA(I,J,12)                                                6332.   
      TG1=GDATA(I,J,13)                                                 6333.   
      TG2=GDATA(I,J,14)                                                 6334.   
      F0DT=E0(I,J,3)                                                    6335.   
         AIJ(I,J,67)=AIJ(I,J,67)+F0DT                                   6335.5  
      F1DT=E1(I,J,3)                                                    6336.   
      EVAP=EVAPOR(I,J,3)                                                6337.   
         AIJ(I,J,63)=AIJ(I,J,63)+EVAP                                   6337.5  
C**** CALCULATE TG1                                                     6338.   
      SNANDI=SNOW+ACE1I-EVAP                                            6339.   
      HC1=SNANDI*SHI                                                    6340.   
      ENRG1=F0DT+EVAP*(TG1*SHI-LHM)-F1DT                                6341.   
      IF(ENRG1.LE.-TG1*HC1) GO TO 420                                   6342.   
C**** FLUXES HEAT UP TG1 TO FREEZING POINT AND MELT SOME SNOW AND ICE   6343.   
      RUN0=(ENRG1+TG1*HC1)/LHM                                          6344.   
      TG1=0.                                                            6345.   
      SNANDI=SNANDI-RUN0                                                6346.   
         BRUN0=BRUN0+RUN0*PLICE                                         6347.   
         RUN0S=RUN0S+RUN0*PLICE                                         6348.   
         AIJ(I,J,33)=AIJ(I,J,33)+RUN0                                   6348.5  
      GO TO 440                                                         6349.   
C**** FLUXES RECOMPUTE TG1 WHICH IS BELOW FREEZING POINT                6350.   
  420 TG1=TG1+ENRG1/HC1                                                 6351.   
  440 IF(SNANDI.GE.ACE1I) GO TO 460                                     6352.   
C**** SOME ICE HAS MELTED OR EVAPORATED, TAKE IT FROM G2                6353.   
      SNOW=0.                                                           6354.   
      DIFS=SNANDI-ACE1I                                                 6355.   
      TG1=(TG1*SNANDI-TG2*DIFS)/ACE1I                                   6356.   
      EDIFS=DIFS*(TG2*SHI-LHM)                                          6357.   
         BEDIFS=BEDIFS+EDIFS*PLICE                                      6358.   
         AIJ(I,J,69)=AIJ(I,J,69)+EDIFS                                  6358.5  
         BDIFS=BDIFS+DIFS*PLICE                                         6359.   
         DIFSS=DIFSS+DIFS*PLICE                                         6360.   
         BERUN2=BERUN2+EDIFS*PLICE                                      6361.   
         AIJ(I,J,72)=AIJ(I,J,72)+EDIFS                                  6361.5  
         BRUN2=BRUN2+DIFS*PLICE                                         6362.   
      GO TO 500                                                         6363.   
  460 SNOW=SNANDI-ACE1I                                                 6364.   
C**** CALCULATE TG2                                                     6365.   
  500 TG2=TG2+F1DT/HC2LI                                                6366.   
C**** RESAVE PROGNOSTIC QUANTITIES                                      6367.   
      GDATA(I,J,12)=SNOW                                                6368.   
      GDATA(I,J,13)=TG1                                                 6369.   
      GDATA(I,J,14)=TG2                                                 6370.   
         BSNOW=BSNOW+SNOW*PLICE                                         6371.   
         BTG1=BTG1+TG1*PLICE                                            6372.   
         BTG2=BTG2+TG2*PLICE                                            6373.   
         BF1DT=BF1DT+F1DT*PLICE                                         6374.   
         AIJ(I,J,69)=AIJ(I,J,69)+F1DT                                   6374.5  
         BEVAP=BEVAP+EVAP*PLICE                                         6375.   
         SNOWS=SNOWS+SNOW*PLICE                                         6376.   
         TG1S=TG1S+TG1*PLICE                                            6377.   
         ACE1S=ACE1S+ACE1I*PLICE                                        6378.   
         ACE2S=ACE2S+Z2LI*RHOI*PLICE                                    6379.   
         TG2S=TG2S+TG2*PLICE                                            6380.   
         EVAPS=EVAPS+EVAP*PLICE                                         6381.   
C****                                                                   6382.   
  600 IF(PEARTH.LE.0.) GO TO 940                                        6383.   
C****                                                                   6384.   
C**** EARTH                                                             6385.   
C****                                                                   6386.   
      SNOW=GDATA(I,J,2)                                                 6387.   
      TG1=GDATA(I,J,4)                                                  6388.   
      WTR1=GDATA(I,J,5)                                                 6389.   
      ACE1=GDATA(I,J,6)                                                 6390.   
      TG2=GDATA(I,J,8)                                                  6391.   
      WTR2=GDATA(I,J,9)                                                 6392.   
      ACE2=GDATA(I,J,10)                                                6393.   
      WFC1=VDATA(I,J,9)                                                 6394.   
      WFC2=VDATA(I,J,10)                                                6395.   
      CHI1=(WTR1+ACE1)/WFC1                                             6396.   
      GFAC=WFC2/WFC1                                                    6397.   
      HC1=HC1DE+WTR1*SHW+(ACE1+SNOW)*SHI                                6398.   
      F0DT=E0(I,J,4)                                                    6399.   
         AIJ(I,J,68)=AIJ(I,J,68)+F0DT                                   6399.5  
      F1DT=E1(I,J,4)                                                    6400.   
      EVAP=EVAPOR(I,J,4)                                                6401.   
         EVAPS=EVAPS+EVAP*PEARTH                                        6401.1  
         AIJ(I,J,64)=AIJ(I,J,64)+EVAP                                   6401.5  
C****                                                                   6402.   
C**** EARTH, DETERMINE EVAPORATION AND DIFFUSION OF WATER               6403.   
C****                                                                   6404.   
      ENRG1=0.                                                          6405.   
      IF(SNOW.LE.0.) GO TO 625                                          6406.   
      IF(EVAP.GT.SNOW) GO TO 620                                        6407.   
C**** SOME SNOW EVAPORATES                                              6408.   
  610 ENRG1=EVAP*(TG1*SHI-LHM)                                          6409.   
      SNOW=SNOW-EVAP                                                    6410.   
      GO TO 660                                                         6411.   
C**** ALL SNOW EVAPORATES                                               6412.   
  620 EVAP=EVAP-SNOW                                                    6413.   
      ENRG1=SNOW*(TG1*SHI-LHM)                                          6414.   
      SNOW=0.                                                           6415.   
      GO TO 630                                                         6416.   
  625 IF(WTR1+ACE1-EVAP.LE.WFC1) GO TO 630                              6417.   
C**** DEW+WTR+ACE EXCEEDS WFC, DO NOT BOTHER TO CORRECT ALL DIAGNOSTICS 6418.   
      IF(TG1.LE.0.) GO TO 610                                           6419.   
      RUN0=WTR1+ACE1-EVAP-WFC1                                          6419.1  
      ERUN0=RUN0*TG1*SHW                                                6419.2  
      WTR1=WFC1-ACE1                                                    6419.3  
      ENRG1=-ERUN0                                                      6419.4  
      GO TO 660                                                         6420.   
  630 IF(EVAP.LE.WTR1+ACE1) GO TO 640                                   6421.   
C**** ALL WATER AND ICE EVAPORATES,RECOMPUTE EVHDT AND EVAP             6422.   
      DEVAP=EVAP-(WTR1+ACE1)                                            6423.   
      DEVHDT=-DEVAP*(LHE+TG1*SHV)                                       6424.   
      ENRG1=ENRG1+WTR1*TG1*SHW+ACE1*(TG1*SHI-LHM)                       6425.   
      EVAP=WTR1+ACE1                                                    6426.   
      WTR1=0.                                                           6427.   
      ACE1=0.                                                           6428.   
      F0DT=F0DT-DEVHDT                                                  6429.   
         BJ(J,14)=BJ(J,14)-DEVHDT*PEARTH                                6430.   
         AIJ(I,J,23)=AIJ(I,J,23)-DEVHDT*PEARTH                          6431.   
         AIJ(I,J,68)=AIJ(I,J,68)-DEVHDT                                 6431.5  
         AIJ(I,J,64)=AIJ(I,J,64)-DEVAP                                  6431.6  
         EVAPS=EVAPS-DEVAP*PEARTH                                       6431.7  
      GO TO 660                                                         6432.   
C**** EVAPORATION FROM WATER AND ICE                                    6433.   
  640 CONTINUE                                                          6433.1  
CW    IF(EVAP.GT.WTR1+ACE1) WRITE(6,*) '---EVAP 6433',EVAP,WTR1+ACE1    6433.5  
      IF(EVAP.GT.WTR1+ACE1) EVAP=WTR1+ACE1                              6434.   
      DWET=EVAP/(WTR1+ACE1+1.D-20)                                      6434.5  
      ENRG1=ENRG1+DWET*(WTR1*TG1*SHW+ACE1*(TG1*SHI-LHM))                6435.   
      WTR1=WTR1*(1.-DWET)                                               6436.   
      ACE1=ACE1*(1.-DWET)                                               6437.   
C**** DETERMINE DIFFUSION OF WATER                                      6438.   
  660 X=(WTR1+ACE1)/(WTR2+ACE2+1.D-20)                                  6439.   
      IF(1..LT.X*GFAC) GO TO 670                                        6440.   
      GROW=1.                                                           6441.   
      IF((SINP(J).GT..5).AND.(JDAY-121)*(243-JDAY).LT.0) GROW=0.        6442.   
      IF((SINP(J).LT.-.5).AND.(JDAY-60)*(304-JDAY).GE.0) GROW=0.        6443.   
      DIFS=GROW*(1.-VDATA(I,J,1))*WTR2*(X*GFAC-1.)/(1.+GFAC)            6444.   
      EDIFS=TG2*DIFS*SHW                                                6445.   
      GO TO 690                                                         6446.   
  670 DIFS=DIFFUS*WTR1*(GFAC-1./X)/(1.+GFAC)                            6447.   
      EDIFS=TG1*DIFS*SHW                                                6448.   
  690    BEDIFS=BEDIFS+EDIFS*PEARTH                                     6449.   
         BDIFS=BDIFS+DIFS*PEARTH                                        6450.   
         DIFSS=DIFSS+DIFS*PEARTH                                        6451.   
C****                                                                   6451.004
C**** EARTH, CALCULATE PENMAN EVAPORATION                               6451.005
C****                                                                   6451.006
      H0=-STBO*TG**4.+(COSZ1(I,J)*SRHR(I,J,1))+TRHR(I,J,1)              6451.01 
      CH=CHDAT(I,J)                                                     6451.017
      CNA=CH*BLDATA(I,J,1)                                              6451.02 
      SHA=RGAS/KAPA                                                     6451.022
      TS=BLDATA(I,J,2)                                                  6451.024
      PPPRES=P(I,J)+PTOP                                                6451.025
      RHO=PPPRES/(RGAS*TS)                                              6451.026
      CPFAC=SHA*RHO*CNA                                                 6451.028
      QS=BLDATA(I,J,3)                                                  6451.033
      QLH=LHE                                                           6451.035
      IF(TS.LT.TF) QLH=LHS                                              6451.037
      EDELT=100.*PPPRES*(QSAT(TS,PPPRES,QLH)-QS)/0.622                  6451.04 
      GAMMA=SHA*100.*PPPRES/(0.622*LHE)                                 6451.045
      DELT=(100./.622)*3.797915*EXP(QLH*(7.93252E-6-2.166847E-3/TS))    6451.053
     *  *QLH*2.166847E-3/TS**2                                          6451.054
      EPEN=(DELT*H0+CPFAC*EDELT)/(LHE*(DELT+GAMMA))                     6451.055
      AEPP=EPEN*NDYN*DT                                                 6451.06 
      AIJ(I,J,61)=AIJ(I,J,61)+AEPP                                      6451.65 
C****                                                                   6452.   
C**** EARTH, CALCULATE TG1                                              6453.   
C****                                                                   6454.   
      ENRG1=ENRG1+F0DT-F1DT-EDIFS                                       6455.   
      IF(TG1) 710,740,750                                               6456.   
C**** FREEZE THE WATER THAT DIFFUSES INTO G1                            6457.   
  710 ENRG1=ENRG1+DIFS*(TG1*SHI-LHM)                                    6458.   
      ACE1=ACE1-DIFS                                                    6459.   
      HC1=HC1DE+(ACE1+SNOW)*SHI                                         6460.   
      IF(ENRG1.LE.-TG1*HC1) GO TO 780                                   6461.   
C**** FLUXES HEAT UP TG1 TO FREEZING POINT                              6462.   
      ENRG1=ENRG1+TG1*HC1                                               6463.   
      TG1=0.                                                            6464.   
  720 IF(ENRG1.LE.(ACE1+SNOW)*LHM) GO TO 730                            6465.   
C**** SNOW AND GROUND ICE MELTS, RECOMPUTE TG1                          6466.   
      RUN0=MAX(SNOW*.5*CHI1,SNOW+(WTR1+ACE1-WFC1))                      6467.   
      WTR1=WTR1+ACE1+SNOW-RUN0                                          6468.   
      TG1=(ENRG1-(ACE1+SNOW)*LHM)/(HC1DE+WTR1*SHW)                      6469.   
      ACE1=0.                                                           6470.   
      SNOW=0.                                                           6471.   
      GO TO 790                                                         6472.   
C**** SOME SNOW AND GROUND ICE MELTS, TG1 IS AT FREEZING POINT          6473.   
  730 DWATER=ENRG1/LHM                                                  6474.   
      DSNOW=MIN(SNOW,DWATER)                                            6475.   
      RUN0=MAX(DSNOW*.5*CHI1,DSNOW+(WTR1+ACE1-WFC1))                    6476.   
      SNOW=SNOW-DSNOW                                                   6477.   
      ACE1=ACE1-(DWATER-DSNOW)                                          6478.   
      WTR1=WTR1+(DWATER-RUN0)                                           6479.   
      GO TO 790                                                         6480.   
C**** TG1 IS AT FREEZING POINT, SUBTRACT WATER THAT DIFFUSES OUT OF G1  6481.   
  740 WTR1=WTR1-DIFS                                                    6482.   
      HC1=HC1DE+WTR1*SHW+(ACE1+SNOW)*SHI                                6483.   
      IF(ENRG1.GT.0.) GO TO 720                                         6484.   
      GO TO 760                                                         6485.   
C**** THE WATER THAT DIFFUSES OUT OF G1 IS ABOVE THE FREEZING POINT     6486.   
  750 ENRG1=ENRG1+TG1*DIFS*SHW                                          6487.   
      WTR1=WTR1-DIFS                                                    6488.   
      HC1=HC1DE+WTR1*SHW                                                6489.   
      IF(-ENRG1.LE.TG1*HC1) GO TO 780                                   6490.   
C**** FLUXES COOL TG1 TO FREEZING POINT                                 6491.   
      ENRG1=ENRG1+TG1*HC1                                               6492.   
      TG1=0.                                                            6493.   
  760 IF(-ENRG1.LE.WTR1*LHM) GO TO 770                                  6494.   
C**** GROUND WATER FREEZES, RECOMPUTE TG1                               6495.   
      ACE1=WTR1+ACE1                                                    6496.   
      TG1=(ENRG1+WTR1*LHM)/(HC1DE+(ACE1+SNOW)*SHI)                      6497.   
      WTR1=0.                                                           6498.   
      GO TO 800                                                         6499.   
C**** SOME GROUND WATER FREEZES, TG1 IS AT FREEZING POINT               6500.   
  770 DICE=-ENRG1/LHM                                                   6501.   
      WTR1=WTR1-DICE                                                    6502.   
      ACE1=ACE1+DICE                                                    6503.   
      GO TO 800                                                         6504.   
C**** FLUXES DO NOT CAUSE TG1 TO CROSS THE FREEZING POINT               6505.   
  780 TG1=TG1+ENRG1/HC1                                                 6506.   
      GO TO 800                                                         6507.   
  790    BRUN0=BRUN0+RUN0*PEARTH                                        6508.   
         RUN0S=RUN0S+RUN0*PEARTH                                        6509.   
         AIJ(I,J,32)=AIJ(I,J,32)+RUN0                                   6509.5  
C****                                                                   6510.   
C**** EARTH, CALCULATE TG2                                              6511.   
C****                                                                   6512.   
  800 ENRG2=F1DT+EDIFS                                                  6513.   
      HC2=HC2DE+WTR2*SHW+ACE2*SHI                                       6514.   
      IF(TG2) 810,840,850                                               6515.   
C**** FREEZE THE WATER THAT DIFFUSES AND PERCOLATES INTO G2             6516.   
  810 ENRG2=ENRG2-DIFS*(TG2*SHI-LHM)                                    6517.   
      ACE2=ACE2+DIFS                                                    6518.   
      HC2=HC2DE+ACE2*SHI                                                6519.   
      IF(ENRG2.LE.-TG2*HC2) GO TO 880                                   6520.   
C**** FLUXES HEAT UP TG2 TO FREEZING POINT                              6521.   
      ENRG2=ENRG2+TG2*HC2                                               6522.   
      TG2=0.                                                            6523.   
  820 IF(ENRG2.LE.ACE2*LHM) GO TO 830                                   6524.   
C**** GROUND ICE MELTS, RECOMPUTE TG2                                   6525.   
      WTR2=WTR2+ACE2                                                    6526.   
      TG2=(ENRG2-ACE2*LHM)/(HC2DE+WTR2*SHW)                             6527.   
      ACE2=0.                                                           6528.   
      GO TO 890                                                         6529.   
C**** SOME GROUND ICE MELTS, TG2 IS AT FREEZING POINT                   6530.   
  830 DWATER=ENRG2/LHM                                                  6531.   
      WTR2=WTR2+DWATER                                                  6532.   
      ACE2=ACE2-DWATER                                                  6533.   
      GO TO 890                                                         6534.   
C**** TG2 IS AT FREEZING POINT, ADD IN WATER THAT DIFFUSES OR PERCOLATE 6535.   
  840 WTR2=WTR2+DIFS                                                    6536.   
      HC2=HC2DE+WTR2*SHW+ACE2*SHI                                       6537.   
      IF(ENRG2.GT.0.) GO TO 820                                         6538.   
      GO TO 860                                                         6539.   
C**** WATER THAT DIFFUSES OR PERCOLATES IS ABOVE FREEZING POINT         6540.   
  850 ENRG2=ENRG2-TG2*DIFS*SHW                                          6541.   
      WTR2=WTR2+DIFS                                                    6542.   
      HC2=HC2DE+WTR2*SHW                                                6543.   
      IF(-ENRG2.LE.TG2*HC2) GO TO 880                                   6544.   
C**** FLUXES COOL TG2 TO FREEZING POINT                                 6545.   
      ENRG2=ENRG2+TG2*HC2                                               6546.   
      TG2=0.                                                            6547.   
  860 IF(-ENRG2.LE.WTR2*LHM) GO TO 870                                  6548.   
C**** GROUND WATER FREEZES, RECOMPUTE TG2                               6549.   
      ACE2=WTR2+ACE2                                                    6550.   
      TG2=(ENRG2+WTR2*LHM)/(HC2DE+ACE2*SHI)                             6551.   
      WTR2=0.                                                           6552.   
      GO TO 890                                                         6553.   
C**** SOME GROUND WATER FREEZES, TG2 IS AT FREEZING POINT               6554.   
  870 DICE=-ENRG2/LHM                                                   6555.   
      WTR2=WTR2-DICE                                                    6556.   
      ACE2=ACE2+DICE                                                    6557.   
      GO TO 890                                                         6558.   
C**** FLUXES DO NOT CAUSE TG2 TO CROSS THE FREEZING POINT               6559.   
  880 TG2=TG2+ENRG2/HC2                                                 6560.   
  890 CONTINUE                                                          6561.   
C**** RESAVE PROGNOSTIC QUANTITIES                                      6562.   
      GDATA(I,J,2)=SNOW                                                 6563.   
      GDATA(I,J,4)=TG1                                                  6564.   
      IF(WTR1+ACE1+.000001.GT.WFC1) THEN                                6564.1  
C**** PREVENT OVER-SATURATION DUE TO ROUND-OFF                          6564.2  
CW       WRITE(6,*) '---WFC1 6564',WTR1,ACE1,WFC1                       6564.3  
         WTR1=.99999*WTR1                                               6564.4  
         ACE1=.99999*ACE1                                               6564.5  
      END IF                                                            6564.6  
      GDATA(I,J,5)=WTR1                                                 6565.   
      GDATA(I,J,6)=ACE1                                                 6566.   
      GDATA(I,J,8)=TG2                                                  6567.   
      GDATA(I,J,9)=WTR2                                                 6568.   
      GDATA(I,J,10)=ACE2                                                6569.   
         BSNOW=BSNOW+SNOW*PEARTH                                        6570.   
         BTG1=BTG1+TG1*PEARTH                                           6571.   
         BTG2=BTG2+TG2*PEARTH                                           6572.   
         BWTR1=BWTR1+WTR1*PEARTH                                        6573.   
         BACE1=BACE1+ACE1*PEARTH                                        6574.   
         BWTR2=BWTR2+WTR2*PEARTH                                        6575.   
         BACE2=BACE2+ACE2*PEARTH                                        6576.   
         BF1DT=BF1DT+F1DT*PEARTH                                        6577.   
         BEVAP=BEVAP+EVAP*PEARTH                                        6578.   
         SNOWS=SNOWS+SNOW*PEARTH                                        6579.   
         TG1S=TG1S+TG1*PEARTH                                           6580.   
         WTR1S=WTR1S+WTR1*PEARTH                                        6581.   
         ACE1S=ACE1S+ACE1*PEARTH                                        6582.   
         WTR2S=WTR2S+WTR2*PEARTH                                        6583.   
         ACE2S=ACE2S+ACE2*PEARTH                                        6584.   
         TG2S=TG2S+TG2*PEARTH                                           6585.   
         AIJ(I,J,7)=AIJ(I,J,7)+(WTR1+ACE1)/WFC1                         6586.2  
         AIJ(I,J,50)=AIJ(I,J,50)+(WTR1+ACE1+WTR2+ACE2)                  6586.6  
C****                                                                   6587.   
C**** ACCUMULATE DIAGNOSTICS                                            6588.   
C****                                                                   6589.   
C**** QUANTITIES ACCUMULATED FOR REGIONS IN DIAGJ                       6590.   
  940    IF(JR.EQ.24) GO TO 950                                         6591.   
         DJ(JR,17)=DJ(JR,17)+TG2S*DXYPJ                                 6592.   
         DJ(JR,18)=DJ(JR,18)+TG1S*DXYPJ                                 6593.   
         DJ(JR,30)=DJ(JR,30)+POICE*DXYPJ                                6594.   
         DJ(JR,45)=DJ(JR,45)+DIFSS*DXYPJ                                6595.   
         DJ(JR,49)=DJ(JR,49)+WTR1S*DXYPJ                                6596.   
         DJ(JR,50)=DJ(JR,50)+ACE1S*DXYPJ                                6597.   
         DJ(JR,51)=DJ(JR,51)+WTR2S*DXYPJ                                6598.   
         DJ(JR,52)=DJ(JR,52)+ACE2S*DXYPJ                                6599.   
         DJ(JR,53)=DJ(JR,53)+SNOWS*DXYPJ                                6600.   
         DJ(JR,54)=DJ(JR,54)+RUN0S*DXYPJ                                6601.   
C**** QUANTITIES ACCUMULATED FOR LATITUDE-LONGITUDE MAPS IN DIAGIJ      6602.   
  950    AIJ(I,J,6)=AIJ(I,J,6)+EVAPS                                    6603.   
         AIJ(I,J,28)=AIJ(I,J,28)+TG1S                                   6605.   
  960 CONTINUE                                                          6606.   
C**** LONGITUDINALLY INTEGRATED QUANTITIES FOR DIAGJ                    6607.   
         CJ(J,15)=CJ(J,15)+CF2DT                                        6608.   
         AJ(J,17)=AJ(J,17)+ATG2                                         6609.   
         BJ(J,17)=BJ(J,17)+BTG2                                         6610.   
         CJ(J,17)=CJ(J,17)+CTG2                                         6611.   
         AJ(J,18)=AJ(J,18)+ATG1                                         6612.   
         BJ(J,18)=BJ(J,18)+BTG1                                         6613.   
         CJ(J,18)=CJ(J,18)+CTG1                                         6614.   
         AJ(J,19)=AJ(J,19)+AEVAP                                        6615.   
         BJ(J,19)=BJ(J,19)+BEVAP                                        6616.   
         CJ(J,19)=CJ(J,19)+CEVAP                                        6617.   
         CJ(J,30)=CJ(J,30)+CICOV                                        6618.   
         AJ(J,33)=AJ(J,33)+AOTDT                                        6619.   
         CJ(J,33)=CJ(J,33)+COTDT                                        6620.   
         AJ(J,34)=AJ(J,34)+ATG3                                         6621.   
C        BJ(J,40)=BJ(J,40)+BERUN0                                       6622.   
         BJ(J,41)=BJ(J,41)+BEDIFS                                       6623.   
         CJ(J,41)=CJ(J,41)+CEDIFS                                       6624.   
         BJ(J,42)=BJ(J,42)+BF1DT                                        6625.   
         CJ(J,42)=CJ(J,42)+CF1DT                                        6626.   
         AJ(J,43)=AJ(J,43)+AEFO                                         6627.   
         BJ(J,43)=BJ(J,43)+BERUN2                                       6628.   
         CJ(J,43)=CJ(J,43)+(CERUN2+CEFI)                                6629.   
         BJ(J,45)=BJ(J,45)+BDIFS                                        6630.   
         CJ(J,45)=CJ(J,45)+CDIFS                                        6631.   
         AJ(J,46)=AJ(J,46)+AIFO                                         6632.   
         BJ(J,46)=BJ(J,46)+BRUN2                                        6633.   
         CJ(J,46)=CJ(J,46)+(CRUN2+CIFI)                                 6634.   
         AJ(J,47)=AJ(J,47)+ARUN4                                        6635.   
         CJ(J,47)=CJ(J,47)+CRUN4                                        6636.   
         AJ(J,48)=AJ(J,48)+AERUN4                                       6637.   
         CJ(J,48)=CJ(J,48)+CERUN4                                       6638.   
         BJ(J,49)=BJ(J,49)+BWTR1                                        6639.   
         BJ(J,50)=BJ(J,50)+BACE1                                        6640.   
         BJ(J,51)=BJ(J,51)+BWTR2                                        6641.   
         BJ(J,52)=BJ(J,52)+BACE2                                        6642.   
         CJ(J,52)=CJ(J,52)+CACE2                                        6643.   
         BJ(J,53)=BJ(J,53)+BSNOW                                        6644.   
         CJ(J,53)=CJ(J,53)+CSNOW                                        6645.   
         BJ(J,54)=BJ(J,54)+BRUN0                                        6646.   
         CJ(J,54)=CJ(J,54)+CRUN0                                        6647.   
  980 CONTINUE                                                          6648.   
      RETURN                                                            6649.   
      END                                                               6650.   
      SUBROUTINE DRYCNV                                                 6801.   
C****                                                                   6802.   
C**** THIS SUBROUTINE MIXES AIR CAUSED BY DRY CONVECTION.  SINCE DRY    6803.   
C**** CONVECTION IN THE BOUNDARY LAYER IS DONE IN SUBROUTINE SURFCE,    6804.   
C**** THIS ROUTINE ONLY CHECKS LAYERS 2 TO LM.                          6805.   
C****                                                                   6806.   
      INCLUDE 'BA94jalC9.COM'                                           6807.   
      COMMON U,V,T,P,Q                                                  6808.   
      COMMON/WORK1/CONV(IM,JM,LM),PK(IM,JM,LM)                          6809.   
      COMMON/WORK2/UT(IM,JM,LM),VT(IM,JM,LM),                           6810.   
     *  RA(8),ID(8),UMS(8)                                              6811.   
      LOGICAL POLE                                                      6812.   
C     DATA RVAP/461.5/                                                  6813.   
      RVX=0.                                                            6814.   
C**** LOAD U,V INTO UT,VT.  UT,VT WILL BE FIXED DURING DRY CONVECTION   6815.   
C****   WHILE U,V WILL BE UPDATED.                                      6816.   
      DO 50 L=1,LM                                                      6817.   
      DO 50 J=2,JM                                                      6818.   
      DO 50 I=1,IM                                                      6819.   
      UT(I,J,L)=U(I,J,L)                                                6820.   
   50 VT(I,J,L)=V(I,J,L)                                                6821.   
C**** OUTSIDE LOOPS OVER J AND I                                        6822.   
      DO 500 J=1,JM                                                     6823.   
      POLE=.FALSE.                                                      6824.   
      IF(J.EQ.1.OR.J.EQ.JM) POLE=.TRUE.                                 6825.   
      IMAX=IM                                                           6826.   
      IF(POLE) IMAX=1                                                   6827.   
      DO 120 K=1,4                                                      6828.   
      RA(K)=RAPVS(J)                                                    6829.   
  120 RA(K+4)=RAPVN(J)                                                  6830.   
      IM1=IM                                                            6831.   
      DO 500 I=1,IMAX                                                   6832.   
      LMAX=1                                                            6833.   
  130 LMIN=LMAX+1                                                       6834.   
      IF(LMIN.GE.LM) GO TO 500                                          6835.   
      LMAX=LMIN                                                         6836.   
      IF(T(I,J,LMIN)*(1.+Q(I,J,LMIN)*RVX).LE.                           6837.   
     *   T(I,J,LMIN+1)*(1.+Q(I,J,LMIN+1)*RVX)) GO TO 130                6838.   
C**** MIX HEAT AND MOISTURE THROUGHOUT THE UNSTABLE LAYERS              6839.   
      PKMS=PK(I,J,LMIN)*DSIG(LMIN)+PK(I,J,LMIN+1)*DSIG(LMIN+1)          6840.   
      THPKMS=T(I,J,LMIN)*(PK(I,J,LMIN)*DSIG(LMIN))                      6841.   
     *  +T(I,J,LMIN+1)*(PK(I,J,LMIN+1)*DSIG(LMIN+1))                    6842.   
      QMS=Q(I,J,LMIN)*DSIG(LMIN)+Q(I,J,LMIN+1)*DSIG(LMIN+1)             6843.   
      IF(LMIN+1.GE.LM) GO TO 150                                        6844.   
      TVMS=T(I,J,LMIN)*(1.+Q(I,J,LMIN)*RVX)*(PK(I,J,LMIN)*DSIG(LMIN))   6845.   
     *    +T(I,J,LMIN+1)*(1.+Q(I,J,LMIN+1)*RVX)                         6846.   
     *                                  *(PK(I,J,LMIN+1)*DSIG(LMIN+1))  6847.   
      THETA=TVMS/PKMS                                                   6848.   
      LMINP2=LMIN+2                                                     6849.   
      DO 140 L=LMINP2,LM                                                6850.   
      IF(THETA.LT.T(I,J,L)*(1.+Q(I,J,L)*RVX)) GO TO 160                 6851.   
      PKMS=PKMS+(PK(I,J,L)*DSIG(L))                                     6852.   
      THPKMS=THPKMS+T(I,J,L)*(PK(I,J,L)*DSIG(L))                        6853.   
      QMS=QMS+Q(I,J,L)*DSIG(L)                                          6854.   
      TVMS=TVMS+T(I,J,L)*(1.+Q(I,J,L)*RVX)*(PK(I,J,L)*DSIG(L))          6855.   
  140 THETA=TVMS/PKMS                                                   6856.   
  150 L=LM+1                                                            6857.   
  160 LMAX=L-1                                                          6858.   
      RDSIGS=1./(SIGE(LMIN)-SIGE(LMAX+1))                               6859.   
      THM=THPKMS/PKMS                                                   6860.   
      QMS=QMS*RDSIGS                                                    6861.   
      DO 180 L=LMIN,LMAX                                                6862.   
         AJL(J,L,12)=AJL(J,L,12)+(THM-T(I,J,L))*PK(I,J,L)*P(I,J)        6863.   
      T(I,J,L)=THM                                                      6864.   
  180 Q(I,J,L)=QMS                                                      6865.   
      IF(POLE) GO TO 300                                                6866.   
C**** MIX MOMENTUM THROUGHOUT UNSTABLE LAYERS AT NON-POLAR GRID BOXES   6867.   
      ID(1)=IM1+(J-1)*IM                                                6868.   
      ID(2)=ID(1)+IM*JM*LM                                              6869.   
      ID(3)=I+(J-1)*IM                                                  6870.   
      ID(4)=ID(3)+IM*JM*LM                                              6871.   
      ID(5)=IM1+J*IM                                                    6872.   
      ID(6)=ID(5)+IM*JM*LM                                              6873.   
      ID(7)=I+J*IM                                                      6874.   
      ID(8)=ID(7)+IM*JM*LM                                              6875.   
      DO 240 K=1,8                                                      6876.   
      UMS(K)=0.                                                         6877.   
      DO 220 L=LMIN,LMAX                                                6878.   
  220 UMS(K)=UMS(K)+UT(ID(K),1,L)*DSIG(L)                               6879.   
  240 UMS(K)=UMS(K)*RDSIGS                                              6880.   
      DO 260 L=LMIN,LMAX                                                6881.   
         AJL(J,L,38)=AJL(J,L,38)+(UMS(1)+UMS(3)-UT(IM1,J,L)-UT(I,J,L))* 6882.   
     *     P(I,J)*RA(1)                                                 6883.   
         AJL(J+1,L,38)=AJL(J+1,L,38)+(UMS(5)+UMS(7)-UT(IM1,J+1,L)-      6884.   
     *     UT(I,J+1,L))*P(I,J)*RA(5)                                    6885.   
      DO 260 K=1,8                                                      6886.   
  260 U(ID(K),1,L)=U(ID(K),1,L)+(UMS(K)-UT(ID(K),1,L))*RA(K)            6887.   
      GO TO 130                                                         6888.   
C**** MIX MOMENTUM THROUGHOUT UNSTABLE LAYERS AT POLAR GRID BOXES       6889.   
  300 JVPO=2                                                            6890.   
      IF(J.EQ.JM) JVPO=JM                                               6891.   
      RAPO=2.*RAPVN(1)                                                  6892.   
      DO 360 IPO=1,IM                                                   6893.   
      UMSPO=0.                                                          6894.   
      VMSPO=0.                                                          6895.   
      DO 320 L=LMIN,LMAX                                                6896.   
      UMSPO=UMSPO+UT(IPO,JVPO,L)*DSIG(L)                                6897.   
  320 VMSPO=VMSPO+VT(IPO,JVPO,L)*DSIG(L)                                6898.   
      UMSPO=UMSPO*RDSIGS                                                6899.   
      VMSPO=VMSPO*RDSIGS                                                6900.   
      DO 340 L=LMIN,LMAX                                                6901.   
      U(IPO,JVPO,L)=U(IPO,JVPO,L)+(UMSPO-UT(IPO,JVPO,L))*RAPO           6902.   
      V(IPO,JVPO,L)=V(IPO,JVPO,L)+(VMSPO-VT(IPO,JVPO,L))*RAPO           6903.   
  340    AJL(JVPO,L,38)=AJL(JVPO,L,38)                                  6904.   
     *  +(UMSPO-UT(IPO,JVPO,L))*P(1,J)*RAPO                             6905.   
  360 CONTINUE                                                          6906.   
      GO TO 130                                                         6907.   
  500 IM1=I                                                             6908.   
      RETURN                                                            6909.   
      END                                                               6910.   
      SUBROUTINE ORBIT (OBLIQ,ECCN,OMEGT,DAY,SDIST,SIND,COSD,LAMBDA)    8201.   
C****                                                                   8202.   
C**** ORBIT receives the orbital parameters and time of year, and       8203.   
C**** returns the distance from the sun and its declination angle.      8204.   
C**** The reference for the following caculations is: V.M.Blanco        8205.   
C**** and S.W.McCuskey, 1961, "Basic Physics of the Solar System",      8206.   
C**** pages 135 - 151.                                                  8207.   
C****                                                                   8208.   
C**** Program authors: Gary L. Russell and Robert J. Suozzo, 12/13/85   8209.   
C****                                                                   8210.   
C****        All computations are in double-precision;                  8211.   
C****        but the arguments are single-precision.                    8212.   
C**** Input: OBLIQ = latitude of tropics in degrees                     8213.   
C****        ECCEN = eccentricity of the orbital ellipse                8214.   
C****        OMEGT = angle from vernal equinox to perihelion in degrees 8215.   
C****        DAY   = day of the year in days; 0 = Jan 1, hour 0         8216.   
C****                                                                   8217.   
C**** Constants: EDAYPY = Earth days per year = 365                     8218.   
C****            VERQNX = occurence of vernal equinox = day 79 = Mar 21 8219.   
C****                                                                   8220.   
C**** Intermediate quantities:                                          8221.   
C****    PERIHE = perihelion during the year in temporal radians        8222.   
C****    MA     = mean anomaly in temporal radians = 2J DAY/365 - PERIHE8223.   
C****    EA     = eccentric anomaly in radians                          8224.   
C****    TA     = true anomaly in radians                               8225.   
C****    BSEMI  = semi minor axis in units of the semi major axis       8226.   
C****    GREENW = longitude of Greenwich in the Earth's reference frame 8227.   
C****                                                                   8228.   
C**** Output: DIST = distance to the sun in units of the semi major axis8229.   
C****        SDIST = square of DIST                                     8229.5  
C****         SIND = sine of the declination angle                      8230.   
C****         COSD = cosine of the declination angle                    8231.   
C****       LAMBDA = sun longitude in Earth's rotating reference frame  8232.   
C****                                                                   8233.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         8234.   
      REAL*8 MA                                                         8235.   
C     REAL*4 SIND,COSD,SDIST,LAMBDA,OBLIQ,ECCN,OMEGT,DAY                8236.   
C****                                                                   8237.   
      PI = 3.14159265358979D0                                           8238.   
      EDAYPY = 365.                                                     8239.   
      VERQNX = 79.                                                      8240.   
      OMEGA=OMEGT*(PI/180.D0)                                           8241.   
      DOBLIQ=OBLIQ*(PI/180.D0)                                          8242.   
      ECCEN=ECCN                                                        8243.   
C****                                                                   8244.   
C**** Determine time of perihelion using Kepler's equation:             8245.   
C**** PERIHE-VERQNX = OMEGA - ECCEN sin(OMEGA)                          8246.   
C****                                                                   8247.   
      PERIHE = OMEGA-ECCEN*SIN(OMEGA)+VERQNX*2.*PI/365.                 8248.   
C     PERIHE = DMOD(PERIHE,2.*PI)                                       8249.   
      MA = 2.*PI*DAY/365.-PERIHE                                        8250.   
      MA = DMOD(MA,2.*PI)                                               8251.   
C****                                                                   8252.   
C**** Numerically solve Kepler's equation: MA = EA - ECCEN sin(EA)      8253.   
C****                                                                   8254.   
      EA = MA+ECCEN*(SIN(MA)+ECCEN*SIN(2.*MA)/2.)                       8255.   
  110 DEA = (MA-EA+ECCEN*SIN(MA))/(1.-ECCEN*COS(EA))                    8256.   
      EA = EA+DEA                                                       8257.   
      IF(DABS(DEA).GT.1.D-8)  GO TO 110                                 8258.   
C****                                                                   8259.   
C**** Calculate the distance to the sun and the true anomaly            8260.   
C****                                                                   8261.   
      BSEMI = DSQRT(1.-ECCEN*ECCEN)                                     8262.   
      COSEA = COS(EA)                                                   8263.   
      SINEA = SIN(EA)                                                   8264.   
      SDIST  = (1.-ECCEN*COSEA)*(1.-ECCEN*COSEA)                        8265.   
      TA = DATAN2(SINEA*BSEMI,COSEA-ECCEN)                              8266.   
C****                                                                   8267.   
C**** Change the reference frame to be the Earth's equatorial plane     8268.   
C**** with the Earth at the center and the positive x axis parallel to  8269.   
C**** the ray from the sun to the Earth were it at vernal equinox.      8270.   
C**** The distance from the current Earth to that ray (or x axis) is:   8271.   
C**** DIST sin(TA+OMEGA).  The sun is located at:                       8272.   
C****                                                                   8273.   
C**** SUN    = (-DIST cos(TA+OMEGA),                                    8274.   
C****           -DIST sin(TA+OMEGA) cos(OBLIQ),                         8275.   
C****            DIST sin(TA+OMEGA) sin(OBLIQ))                         8276.   
C**** SIND   = sin(TA+OMEGA) sin(OBLIQ)                                 8277.   
C**** COSD   = sqrt(1-SIND**2)                                          8278.   
C**** LAMBDA = atan[tan(TA+OMEGA) cos(OBLIQ)] - GREENW                  8279.   
C**** GREENW = 2*3.14159 DAY (EDAYPY-1)/EDAYPY                          8280.   
C****                                                                   8281.   
      SINDD = SIN(TA+OMEGA)*SIN(DOBLIQ)                                 8282.   
      COSD = DSQRT(1.-SINDD*SINDD)                                      8283.   
      SIND = SINDD                                                      8284.   
C     GREENW = 2.*PI*(DAY-VERQNX)*(EDAYPY+1.)/EDAYPY                    8285.   
C     SUNX = -COS(TA+OMEGA)                                             8286.   
C     SUNY = -SIN(TA+OMEGA)*COS(DOBLIQ)                                 8287.   
C     LAMBDA = DATAN2(SUNY,SUNX)-GREENW                                 8288.   
C     LAMBDA = DMOD(LAMBDA,2.*PI)                                       8289.   
C****                                                                   8290.   
      RETURN                                                            8291.   
      END                                                               8292.   
      SUBROUTINE OSTRUC                                                 8501.   
C****                                                                   8502.   
C**** THIS SUBROUTINE RESTRUCTURES THE OCEAN TEMPERATURE PROFILE        8503.   
C**** WHEN THE MIXED LAYER DEPTHS ARE CHANGED (NORMALLY DONE ONCE       8504.   
C**** A DAY).                                                           8505.   
C**** THE SUBROUTINE ALSO MELTS ICE WHEN TGO > 0 (C).                   8506.   
C****                                                                   8507.   
      INCLUDE 'BA94jalC9.COM' 
      COMMON U,V,T,P,Q                                                  8509.   
      COMMON/WORK2/Z1OOLD(IM,JM)                                        8510.   
      DATA SHW/4185./,SHI/2060./,RHOW/1000./,RHOI/916.6/,Z1I/.1/        8511.   
      DATA Z2OIM/.4/                                                    8511.5  
      DATA TTRUNC/0./                                                   8512.   
C****                                                                   8513.   
C**** FDATA  2  LAND COVERAGE (1)                                       8514.   
C****                                                                   8515.   
C**** ODATA  1  OCEAN TEMPERATURE OF FIRST LAYER (C)                    8516.   
C****        2  RATIO OF OCEAN ICE COVERAGE TO WATER COVERAGE (1)       8517.   
C****        3  OCEAN ICE AMOUNT OF SECOND LAYER (KG/M**2)              8518.   
C****        4  MEAN OCEAN TEMPERATURE OF SECOND LAYER (C)              8519.   
C****        5  OCEAN TEMPERATURE AT BOTTOM OF SECOND LAYER (C)         8520.   
C****                                                                   8521.   
C**** GDATA  1  OCEAN ICE SNOW AMOUNT (KG/M**2)                         8522.   
C****        3  OCEAN ICE TEMPERATURE OF FIRST LAYER (C)                8523.   
C****        7  OCEAN ICE TEMPERATURE OF SECOND LAYER (C)               8524.   
C****                                                                   8525.   
      ACE1I=Z1I*RHOI                                                    8526.   
      AC2OIM=RHOI*Z2OIM                                                 8526.5  
C****                                                                   8527.   
C**** RESTRUCTURE OCEAN LAYERS                                          8528.   
C****                                                                   8529.   
      DO 200 J=1,JM                                                     8530.   
      IMAX=IM                                                           8531.   
      IF(J.EQ.1.OR.J.EQ.JM) IMAX=1                                      8532.   
      DO 200 I=1,IMAX                                                   8533.   
      IF(FDATA(I,J,2).GE.1.) GO TO 200                                  8534.   
      IF(Z1OOLD(I,J).GE.Z12O(I,J)) GO TO 140                            8535.   
      IF(Z1O(I,J).EQ.Z1OOLD(I,J)) GO TO 200                             8536.   
      WTR1O=RHOW*Z1O(I,J)-ODATA(I,J,2)*(GDATA(I,J,1)+ACE1I+ODATA(I,J,3))8537.   
      DWTRO=RHOW*(Z1O(I,J)-Z1OOLD(I,J))                                 8538.   
      WTR2O=RHOW*(Z12O(I,J)-Z1O(I,J))                                   8539.   
      IF(DWTRO.GT.0.) GO TO 120                                         8540.   
C**** MIX LAYER DEPTH IS GETTING SHALLOWER                              8541.   
      ODATA(I,J,4)=ODATA(I,J,4)                                         8542.   
     *  +((ODATA(I,J,4)-ODATA(I,J,1))*DWTRO/WTR2O+TTRUNC)               8543.   
      GO TO 200                                                         8544.   
C**** MIX LAYER DEPTH IS GETTING DEEPER                                 8545.   
  120 TGAVE=(ODATA(I,J,4)*DWTRO+(2.*ODATA(I,J,4)-ODATA(I,J,5))*WTR2O)   8546.   
     *  /(WTR2O+DWTRO)                                                  8547.   
      ODATA(I,J,1)=ODATA(I,J,1)+((TGAVE-ODATA(I,J,1))*DWTRO/WTR1O       8548.   
     *  +TTRUNC)                                                        8549.   
      IF(Z1O(I,J).GE.Z12O(I,J)) GO TO 140                               8550.   
      ODATA(I,J,4)=ODATA(I,J,4)                                         8551.   
     *  +((ODATA(I,J,5)-ODATA(I,J,4))*DWTRO/(WTR2O+DWTRO)+TTRUNC)       8552.   
      GO TO 200                                                         8553.   
C**** MIXED LAYER DEPTH IS AT ITS MAXIMUM OR TEMP PROFILE IS UNIFORM    8554.   
  140 ODATA(I,J,4)=ODATA(I,J,1)                                         8555.   
      ODATA(I,J,5)=ODATA(I,J,1)                                         8556.   
  200 CONTINUE                                                          8557.   
C****                                                                   8558.   
C**** REDUCE THE HORIZONTAL EXTENT OF ICE IF OCEAN TEMPERATURE IS WARM  8559.   
C****                                                                   8560.   
      DO 300 J=1,JM                                                     8561.   
      IMAX=IM                                                           8562.   
      IF(J.EQ.1.OR.J.EQ.JM) IMAX=1                                      8563.   
      DO 300 I=1,IMAX                                                   8564.   
      IF(ODATA(I,J,2).LE.0.) GO TO 300                                  8565.   
      IF(FDATA(I,J,2).GE.1.) GO TO 300                                  8566.   
C**** REDUCE ICE EXTENT IF OCEAN TEMPERATURE IS GREATER THAN 0 DEGREES  8567.   
      IF(ODATA(I,J,1).LE.0.) GO TO 300                                  8568.   
      TGW=ODATA(I,J,1)                                                  8569.   
      ROICE=ODATA(I,J,2)                                                8570.   
      ACE2=ODATA(I,J,3)                                                 8570.5  
      ACE=GDATA(I,J,1)+ACE1I+ACE2                                       8571.   
      ENRGI=((GDATA(I,J,1)+ACE1I)*GDATA(I,J,3)                          8572.   
     *  +ACE2*GDATA(I,J,7))*SHI-ACE*LHM                                 8573.   
      WTRO=Z1O(I,J)*RHOW                                                8574.   
      WTRW=WTRO-ROICE*ACE                                               8575.   
      ENRGW=WTRW*TGW*SHW                                                8576.   
      IF(ROICE*ENRGI+ENRGW.LT.0.) GO TO 230                             8577.   
C**** THE WARM OCEAN MELTS ALL THE SNOW AND ICE                         8578.   
      ODATA(I,J,1)=(ROICE*ENRGI+ENRGW)/(WTRO*SHW)                       8579.   
      GO TO 270                                                         8580.   
C**** THE WARM OCEAN COOLS TO 0 DEGREES MELTING SOME SNOW AND ICE       8581.   
C**** Reduce the ice depth                                              8581.1  
  230 ODATA(I,J,1)=0.                                                   8582.   
      DACE2=ENRGW/(ROICE*(GDATA(I,J,7)*SHI-LHM))                        8583.   
      IF(DACE2.LT.AC2OIM-ACE2) DACE2=AC2OIM-ACE2                        8583.1  
      DENRGI = DACE2*(GDATA(I,J,7)*SHI-LHM)                             8583.2  
      ODATA(I,J,2)=(ROICE*ENRGI+ENRGW)/(ENRGI+DENRGI)                   8583.3  
      ODATA(I,J,3)=ACE2+DACE2                                           8583.4  
      GO TO 300                                                         8584.   
  270 ODATA(I,J,2)=0.                                                   8585.   
      ODATA(I,J,3)=0.                                                   8586.   
      GDATA(I,J,1)=0.                                                   8587.   
      GDATA(I,J,3)=0.                                                   8588.   
      GDATA(I,J,7)=0.                                                   8589.   
  300 CONTINUE                                                          8590.   
      RETURN                                                            8591.   
      END                                                               8592.   

c  ** GLR (ADDED)
c  ** Deep ocean code
      SUBROUTINE ODIFS                                                  8601.   
C****                                                                   8602.   
C**** THIS SUBROUTINE CALCULATES THE ANNUAL OCEAN TEMPERATURE AT THE    8603.   
C**** MAXIMUM MIXED LAYER, COMPARES THAT TO THE CONTROL RUN''S          8604.   
C**** TEMPERATURE, CALLS SUBROUTINE DIFFUS, AND REDUCES THE UPPER       8605.   
C**** OCEAN TEMPERATURES BY THE AMOUNT OF HEAT THAT IS DIFFUSED INTO    8606.   
C**** THE THERMOCLINE                                                   8607.   
C****                                                                   8608.   
      INCLUDE 'BA94jalC9.COM'                                         
      COMMON/OCN/TG3M(IM,JM,12),RTGO(IM,JM,LM),STG3(IM,JM),             8609.1  
     *  DTG3(IM,JM)                                                     8609.2  
C****                                                                   8610.   
C**** ACCUMULATE OCEAN TEMPERATURE AT MAXIMUM MIXED LAYER               8611.   
C****                                                                   8612.   
      DIMENSION EDO(IM,JM),ADTG3(IM,JM),PWATER(IM,JM)                   8613.   
      DATA IFIRST/1/                                                    8614.   
      IF (IFIRST.EQ.1) CALL DREAD (62,EDO,IM*JM,EDO)                    8615.   
      IFIRST=0                                                          8616.   
      DO 110 J=1,JM                                                     8617.   
      IMAX=IM                                                           8618.   
      IF ((J.EQ.1).OR.(J.EQ.JM)) IMAX=1                                 8619.   
      DO 110 I=1,IMAX                                                   8620.   
  110 STG3(I,J)=STG3(I,J)+ODATA(I,J,5)                                  8621.   
C****                                                                   8622.   
C**** AT THE END OF EACH MONTH, UPDATE THE OCEAN TEMPERATURE            8623.   
C**** DIFFERENCE AND REPLACE THE MONTHLY SUMMED TEMPERATURE             8624.   
C****                                                                   8625.   
      IF(JDATE.NE.1) GO TO 300                                          8626.   
      MONTH=(JDAY+15)/30                                                8627.   
      IF (MONTH.EQ.0) MONTH=12                                          8628.   
      DO 210 J=1,JM                                                     8629.   
      IMAX=IM                                                           8630.   
      IF((J.EQ.1).OR.(J.EQ.JM)) IMAX=1                                  8631.   
      DO 210 I=1,IMAX                                                   8632.   
      DTG3(I,J)=DTG3(I,J)+(STG3(I,J)-TG3M(I,J,MONTH))                   8633.   
      TG3M(I,J,MONTH)=STG3(I,J)                                         8634.   
  210 STG3(I,J)=0.                                                      8635.   
C****                                                                   8636.   
C**** DIFFUSE THE OCEAN TEMPERATURE DIFFERENCE OF THE UPPER LAYERS      8637.   
C**** INTO THE THERMOCLINE AND REDUCE THE UPPER TEMPERATURES BY THE     8638.   
C**** HEAT THAT IS DIFFUSED DOWNWARD                                    8639.   
C****                                                                   8640.   
  300 DO 310 J=1,JM                                                     8641.   
      IMAX=IM                                                           8642.   
      IF((J.EQ.1).OR.(J.EQ.JM)) IMAX=1                                  8643.   
      DO 310 I=1,IMAX                                                   8644.   
      ADTG3(I,J)=DTG3(I,J)/365.                                         8645.   
  310 RTGO(I,J,1)=ADTG3(I,J)                                            8646.   
C****                                                                   8647.   
      DO 320 J=1,JM                                                     8648.   
      DO 320 I=1,IM                                                     8649.   
  320 PWATER(I,J)=1.-FDATA(I,J,2)                                       8650.   
      CALL DIFFUS (IM,JM,SDAY,.5D0,EDO,Z12O,PWATER,RTGO)                8651.   
C****                                                                   8652.   
      DO 340 J=1,JM                                                     8653.   
         ADT=0.                                                         8654.   
         CDT=0.                                                         8655.   
      IMAX=IM                                                           8656.   
      IF((J.EQ.1).OR.(J.EQ.JM)) IMAX=1                                  8657.   
      DO 330 I=1,IMAX                                                   8658.   
      IF(PWATER(I,J).LE.0.) GO TO 330                                   8659.   
      ODATA(I,J,1)=ODATA(I,J,1)+(RTGO(I,J,1)-ADTG3(I,J))                8660.   
      ODATA(I,J,4)=ODATA(I,J,4)+(RTGO(I,J,1)-ADTG3(I,J))                8661.   
      ODATA(I,J,5)=ODATA(I,J,5)+(RTGO(I,J,1)-ADTG3(I,J))                8662.   
         ADT=ADT+(RTGO(I,J,1)-ADTG3(I,J))*Z12O(I,J)*PWATER(I,J)         8663.   
     *     *(1.-ODATA(I,J,2))                                           8664.   
         CDT=CDT+(RTGO(I,J,1)-ADTG3(I,J))*Z12O(I,J)*PWATER(I,J)         8665.   
     *     *ODATA(I,J,2)                                                8666.   
  330 CONTINUE                                                          8667.   
         AJ(J,68)=AJ(J,68)-ADT                                          8668.   
         CJ(J,68)=CJ(J,68)-CDT                                          8669.   
  340 CONTINUE                                                          8670.   
      RETURN                                                            8671.   
      END                                                               8672.   
      SUBROUTINE DIFFUS (IM,JM,DT,ALPHA,ED,Z12O,PWATER,R)               8701.   
      IMPLICIT REAL*8 (A-H,O-Z)                                         8701.5  
C****                                                                   8702.   
C**** THIS SUBROUTINE CALCULATES THE VERTICAL MIXING OF A TRACER, R,    8703.   
C**** BY DIFFUSION.  LM IS THE NUMBER OF VERTICAL LAYERS.  DT (S) IS    8704.   
C**** THE TIME STEP.  ALPHA DETERMINES THE TIME SCHEME RANGING FROM     8705.   
C**** 0 FOR EXPLICIT TO 1 FOR FULLY IMPLICIT.  DZ (M) IS THE DEPTH OF   8706.   
C**** THE LAYERS, AND DZO (M) IS THE DISTANCE BETWEEN THE CENTERS OF    8707.   
C**** THE LAYERS.  ED (M**2/S) IS THE DIFFUSION COEFFICIENT BETWEEN     8708.   
C**** ADJACENT LAYERS.  R IS THE TRACER CONCENTRATION.                  8709.   
C****                                                                   8710.   
C??   DIMENSION ED(IM,JM),Z12O(IM,JM),PWATER(IM,JM),R(IM,JM,*)          8711.   
      DIMENSION ED(36,24),Z12O(36,24),PWATER(36,24),R(36,24,*)          8711.1  
      DIMENSION DZ(9),DZO(8),AM(9),BM(9),CM(9),DM(9)                    8712.   
C**** DEFINE THE VERTICAL LAYERING                                      8713.   
      LM=9                                                              8714.   
      DZO(1)=10./SQRT(1.7010587)                                        8716.   
      DZ(2)=10.                                                         8717.   
      DO 10 L=2,LM-1                                                    8718.   
      DZO(L)=DZO(L-1)*1.7010587                                         8719.   
   10 DZ(L+1)=DZ(L)*1.7010587                                           8720.   
C**** LOOP OVER THE HORIZONTAL GRID POINTS                              8721.   
  100 DO 500 J=1,JM                                                     8722.   
      IMAX=IM                                                           8723.   
      IF((J.EQ.1).OR.(J.EQ.JM)) IMAX=1                                  8724.   
      DO 500 I=1,IMAX                                                   8725.   
      IF(PWATER(I,J).LE.0.) GO TO 500                                   8726.   
      DZ(1)=Z12O(I,J)                                                   8727.   
C**** SET UP TRIDIAGONAL MATRIX ENTRIES AND RIGHT HAND SIDES            8728.   
      AM(1)=DZ(1)+ALPHA*DT*ED(I,J)/DZO(1)                               8729.   
      DM(1)=DZ(1)*R(I,J,1)                                              8730.   
     *  -(1.-ALPHA)*DT*ED(I,J)*(R(I,J,1)-R(I,J,2))/DZO(1)               8731.   
      L=1                                                               8732.   
  110 BM(L)=-ALPHA*DT*ED(I,J)/DZO(L)                                    8733.   
      CM(L+1)=-ALPHA*DT*ED(I,J)/DZO(L)                                  8734.   
      IF(L.GE.LM-1) GO TO 120                                           8735.   
      L=L+1                                                             8736.   
      AM(L)=DZ(L)+ALPHA*DT*(ED(I,J)/DZO(L-1)+ED(I,J)/DZO(L))            8737.   
      DM(L)=DZ(L)*R(I,J,L)                                              8738.   
     *  +(1.-ALPHA)*DT*(ED(I,J)*(R(I,J,L-1)-R(I,J,L))/DZO(L-1)          8739.   
     *  -ED(I,J)*(R(I,J,L)-R(I,J,L+1))/DZO(L))                          8740.   
      GO TO 110                                                         8741.   
  120 AM(LM)=DZ(LM)+ALPHA*DT*ED(I,J)/DZO(LM-1)                          8742.   
      DM(LM)=DZ(LM)*R(I,J,LM)                                           8743.   
     *  +(1.-ALPHA)*DT*ED(I,J)*(R(I,J,LM-1)-R(I,J,LM))/DZO(LM-1)        8744.   
C**** ELIMINATE LOWER OFF-DIAGONAL ENTRIES FROM THE MATRIX EQUATION     8745.   
      DO 210 L=2,LM                                                     8746.   
      AM(L)=AM(L)-BM(L-1)*CM(L)/AM(L-1)                                 8747.   
  210 DM(L)=DM(L)-DM(L-1)*CM(L)/AM(L-1)                                 8748.   
C**** ELIMINATE UPPER OFF-DIAGONAL ENTRIES FROM THE MATRIX EQUATION     8749.   
      DO 310 LX=2,LM                                                    8750.   
      L=1+LM-LX                                                         8751.   
  310 DM(L)=DM(L)-DM(L+1)*BM(L)/AM(L+1)                                 8752.   
C**** SOLVE THE REDUCED MATRIX EQUATION TO CALCULATE THE NEW R          8753.   
      DO 410 L=1,LM                                                     8754.   
  410 R(I,J,L)=DM(L)/AM(L)                                              8755.   
  500 CONTINUE                                                          8756.   
      RETURN                                                            8757.   
      END                                                               8758.   
c  ** END (ADDED)
