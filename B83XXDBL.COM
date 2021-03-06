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
c  ** 12/21/00
c  **
c  ** NOTES:
c  **
c  *********************************************************************
c  *********************************************************************


C                                                                       
C                   RADCOM:      CONTROL/INPUT PARAMETERS
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/RADCOM/VADATA(11, 4, 3),DLAT(46),DLON(72),TAUMIN,FULGAS(18)
     A             ,FRACSL,RATQSL,FOGTSL,PTLISO,TLGRAD,TKCICE,FGOLDH(18)
     B             ,FLONO3,FRAYLE,FCLDTR,FCLDSR,FALGAE,FMARCL,FEMTRA(6)
     C             ,WETTRA,WETSRA,DMOICE,DMLICE,LICETK,NTRACE,FZASRA(6)
     D             ,ID5(5),ITR(4),IMG(2),ILG(2),LAPGAS,KWVCON,NORMS0,NV
     E             ,KEEPRH,KEEPAL,ISOSCT,IHGSCT,KGASSR,KAERSR,KFRACC
     F             ,MARCLD,LAYRAD,NL,NLP,JMLAT ,IMLON ,KFORCE,LASTVC
C
C                                BASIC RADCOM INPUT DATA
C
     G             ,PLB(40),HLB(40),TLB(40),TLT(40),TLM(40),U0GAS(40,9)
     H             ,ULGAS(40,9),TRACER(40,4),CLDTAU(40),SHL(40),RHL(40)
     I             ,POCEAN,PEARTH,POICE,PLICE,AGESN,SNOWE,SNOWOI,SNOWLI
     J             ,TGO,TGE,TGOI,TGLI,TSL,WMAG,WEARTH,ZOICE,FSPARE(200)
     K             ,S0,COSZ,PVT(11),BXA(153),SRBXAL(15,2),FRC(5),LUXGAS
     L             ,JYEAR,JDAY,JLAT,ILON,MEANAL,KALVIS,ISPARE(25),PSIG0
C
C                                BASIC RADCOM OUTPUT DATA
C
     M             ,TRDFLB(40),TRUFLB(40),TRNFLB(40),TRFCRL(40),TRSLCR
     N             ,SRDFLB(40),SRUFLB(40),SRNFLB(40),SRFHRL(40),SRSLHR
     O             ,SRIVIS,SROVIS,PLAVIS,SRINIR,SRONIR,PLANIR,SRXATM(4)
     P             ,SRDVIS,SRUVIS,ALBVIS,SRDNIR,SRUNIR,ALBNIR,FSRNFG(4)
     Q             ,SRTVIS,SRRVIS,SRAVIS,SRTNIR,SRRNIR,SRANIR,FTRUFG(4)
     R             ,TRDFGW,TRUFGW,TRUFTW,BTEMPW,TRDFSL,TRUFSL,DTRUFG(4)
     S             ,TRSLTS,TRSLTG,TRSLWV,TRSLBS,TTRUFG,LBOTCL,LTOPCL
C
C                                BLOCKD INITIALIZED DEFAULT DATA
C
      COMMON/BLOCKD/AGOLDH(11, 5),BGOLDH(11, 5),CGOLDH(11, 5)
     T             ,TRAQEX(25,11),TRAQSC(25,11),TRACOS(25,11)
     T             ,TRCQEX(25, 2),TRCQSC(25, 2),TRCCOS(25, 2)
     S             ,SRAQEX( 6,11),SRAQSC( 6,11),SRACOS( 6,11)
     S             ,SRCQEX( 6, 2),SRCQSC( 6, 2),SRCCOS( 6, 2)
     X             ,AOCEAN(25   ),AGSIDV(25, 4),CLDALB(25, 2)
     Y             ,CMANO2(42   ),TRACEG(25,16),PPMV58(9),Z0(9),ZH(9)
     Z             ,ASNALB(15),AOIALB(15),ALIALB(15),NAERO,NGOLDH,NKSR
C
      COMMON/WORK4/ ITLB(40),ITLT(40),PL(40),DPL(40),UO3L(40),PSIG(40)
     T           ,COSLAT(46),TRCALB(40),TRGALB(40),BGFEMT(40),BGFEMD(40)
     T             ,TRAEXT(40,25),TAUN(1000),TAUSL(25),FTAUSL(25)
     T             ,ENA(40),ENB(40),ENC(40),TRA(40),TRB(40),TRC(40)
     T             ,WTLB(40),WTLT(40)
     T             ,DFLB(40,25),UFLB(40,25),WFLB(40,25)
     T             ,DFSL(25),UFSL(25),WFSL(25)
     S             ,EXTAER(40,6),SCTAER(40,6),COSAER(40,6),PI0AER(40,6)
     S             ,RNB(40),RNX(40),TNB(40),TNX(40),XNB(40),XNX(40)
     S             ,SRB(40),SRX(40),VRU(40),VRD(40),FAC(40),O3A(40)
     S             ,BVSURF,BNSURF,XVSURF,XNSURF
     X             ,UXGAS(40,9),SRTAU(600)
C
C
      COMMON/TRDCOM/TRAX(40,25,5),TRCX(25,2),PLANCK(6250),TAUTBL(80000)
     A             ,TAULAP(1000),TKPFW(630)
     B             ,ITRHDR(100),MLGAS(30),MLLAP(30),NKTR,IT0,ITNEXT
     C             ,JNORTH,ALVISD(22),ALNIRD(22)
C$   D             ,ISRHDR(100),SRTBL(44800)
C
