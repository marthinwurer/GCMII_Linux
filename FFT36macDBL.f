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
c  ** 12/21/00 converted for new model (MFS)
c  **
c  ** NOTES:
c  **
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE FRTR0 (KM)
c  ** INITIALIZATION ENTRY TO CALCULATE SIN VALUES AND CHECK THAT KM=36
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/FCOM/BYKM,BYKMH,BYKM2,SIN10,SIN20,SIN30,SIN40,SIN50,SIN60,
     *  SIN70,SIN80
c     REAL*8 TWOPI/6.283185307179586477/
      DATA   TWOPI/6.283185307179586477/
      IF(KM.NE.36) GO TO 220
      BYKM=1./KM
      BYKMH=2./KM
      BYKM2=1./(2.*KM)
      SIN10=DSIN(TWOPI/36.)
      SIN20=DSIN(TWOPI/18.)
      SIN30=1.D0/2.
      SIN40=DSIN(TWOPI/9.)
      SIN50=DCOS(TWOPI/9.)
      SIN60=DSQRT(3.D0)/2.
      SIN70=DCOS(TWOPI/18.)
      SIN80=DCOS(TWOPI/36.)
c  BL SIN10=DSIN(TWOPI/36.)
c  BL SIN20=DSIN(TWOPI/18.)
c  BL SIN30=1.D0/2.
c  BL SIN40=DSIN(TWOPI/9.)
c  BL SIN50=DCOS(TWOPI/9.)
c  BL SIN60=DSQRT(3.D0)/2.
c  BL SIN70=DCOS(TWOPI/18.)
c  BL SIN80=DCOS(TWOPI/36.)
c     SIN10=SIN(TWOPI/36.)
c     SIN20=SIN(TWOPI/18.)
c     SIN30=.5
c     SIN40=SIN(TWOPI/9.)
c     SIN50=COS(TWOPI/9.)
c     SIN60=SQRT(3.)/2.
c     SIN70=COS(TWOPI/18.)
c     SIN80=COS(TWOPI/36.)
      RETURN
  220 WRITE (6,901) KM
      STOP
  901 FORMAT ('0THIS FOURT SUBROUTINE NOT SUITED FOR KM = ',I8)
      END
      SUBROUTINE FRTR (F)
c  ** THIS SUBROUTINE PERFORMS A FOURIER ANALYSIS ON THE ONE DIMENSIONAL
c  ** ARRAY F WHICH MUST BE DIMENSIONED 36.  IT RETURNS IN F THE ENERGY
c  ** ASSOCIATED WITH EACH WAVE NUMBER.  UPON ENTERING THIS ROUTINE,
c  ** THE TOTAL ENERGY IS
c  **   .5*SUM(F(K)*F(K))
c  ** WITH THE SUM BEING TAKEN OVER ALL K FROM 1 TO 36.  UPON LEAVING
c  ** THIS ROUTINE, THE TOTAL ENERGY IS
c  **   SUM(F(N+1))
c  ** WITH THE SUM BEING TAKEN OVER ALL WAVE NUMBERS FROM 0 TO 18.
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/FCOM/BYKM,BYKMH,BYKM2,SIN10,SIN20,SIN30,SIN40,SIN50,SIN60,
     *  SIN70,SIN80
      DIMENSION F(36)
   10 CC00=F(12)+F(24)+F(36)
      CC01=F(36)-(F(12)+F(24))*SIN30
      CC10=F(1)+F(13)+F(25)
      CC11=F(1)*SIN80-F(13)*SIN40-F(25)*SIN20
      CC20=F(2)+F(14)+F(26)
      CC21=F(2)*SIN70-F(14)*SIN50-F(26)*SIN10
      CC30=F(3)+F(15)+F(27)
      CC31=(F(3)-F(15))*SIN60
      CC40=F(4)+F(16)+F(28)
      CC41=F(4)*SIN50-F(16)*SIN70+F(28)*SIN10
      CC50=F(5)+F(17)+F(29)
      CC51=F(5)*SIN40-F(17)*SIN80+F(29)*SIN20
      CC60=F(6)+F(18)+F(30)
      CC61=(F(6)+F(30))*SIN30-F(18)
      CC70=F(7)+F(19)+F(31)
      CC71=F(7)*SIN20-F(19)*SIN80+F(31)*SIN40
      CC80=F(8)+F(20)+F(32)
      CC81=F(8)*SIN10-F(20)*SIN70+F(32)*SIN50
      CC90=F(9)+F(21)+F(33)
      CC91=(F(33)-F(21))*SIN60
      CCA0=F(10)+F(22)+F(34)
      CCA1=F(34)*SIN70-F(10)*SIN10-F(22)*SIN50
      CCB0=F(11)+F(23)+F(35)
      CCB1=F(35)*SIN80-F(11)*SIN20-F(23)*SIN40
      SC01=(F(12)-F(24))*SIN60
      SC11=F(1)*SIN10+F(13)*SIN50-F(25)*SIN70
      SC21=F(2)*SIN20+F(14)*SIN40-F(26)*SIN80
      SC31=(F(3)+F(15))*SIN30-F(27)
      SC41=F(4)*SIN40+F(16)*SIN20-F(28)*SIN80
      SC51=F(5)*SIN50+F(17)*SIN10-F(29)*SIN70
      SC61=(F(6)-F(30))*SIN60
      SC71=F(7)*SIN70-F(19)*SIN10-F(31)*SIN50
      SC81=F(8)*SIN80-F(20)*SIN20-F(32)*SIN40
      SC91=F(9)-(F(21)+F(33))*SIN30
      SCA1=F(10)*SIN80-F(22)*SIN40-F(34)*SIN20
      SCB1=F(11)*SIN70-F(23)*SIN50-F(35)*SIN10
c  ** CALCULATE EXPRESSIONS SUMMED BY INCREMENTS OF 4
      C400=CC00+CC40+CC80
      C401=CC01+CC41+CC81
      C403=CC00-(CC40+CC80)*SIN30
      C402=(CC01-(CC41+CC81)*SIN30)+((SC41-SC81)*SIN60)
      C404=(CC01-(CC41+CC81)*SIN30)-((SC41-SC81)*SIN60)
      C410=CC10+CC50+CC90
      C411=CC11+CC51+CC91
      C413=(CC10-CC50)*SIN60
      C412=((CC11-CC51)*SIN60)+((SC11+SC51)*SIN30-SC91)
      C414=((CC11-CC51)*SIN60)-((SC11+SC51)*SIN30-SC91)
      C420=CC20+CC60+CCA0
      C421=CC21+CC61+CCA1
      C423=(CC20+CCA0)*SIN30-CC60
      C422=((CC21+CCA1)*SIN30-CC61)+((SC21-SCA1)*SIN60)
      C424=((CC21+CCA1)*SIN30-CC61)-((SC21-SCA1)*SIN60)
      C430=CC30+CC70+CCB0
      C431=CC31+CC71+CCB1
      C433=(CCB0-CC70)*SIN60
      C432=((CCB1-CC71)*SIN60)+(SC31-(SC71+SCB1)*SIN30)
      C434=((CCB1-CC71)*SIN60)-(SC31-(SC71+SCB1)*SIN30)
      S401=SC01+SC41+SC81
      S403=(CC40-CC80)*SIN60
      S402=((CC41-CC81)*SIN60)+((SC41+SC81)*SIN30-SC01)
      S404=((CC41-CC81)*SIN60)-((SC41+SC81)*SIN30-SC01)
      S411=SC11+SC51+SC91
      S413=(CC10+CC50)*SIN30-CC90
      S412=((CC11+CC51)*SIN30-CC91)+((SC51-SC11)*SIN60)
      S414=((CC11+CC51)*SIN30-CC91)-((SC51-SC11)*SIN60)
      S421=SC21+SC61+SCA1
      S423=(CC20-CCA0)*SIN60
      S422=((CC21-CCA1)*SIN60)+(SC61-(SC21+SCA1)*SIN30)
      S424=((CC21-CCA1)*SIN60)-(SC61-(SC21+SCA1)*SIN30)
      S431=SC31+SC71+SCB1
      S433=CC30-(CC70+CCB0)*SIN30
      S432=(CC31-(CC71+CCB1)*SIN30)+((SC71-SCB1)*SIN60)
      S434=(CC31-(CC71+CCB1)*SIN30)-((SC71-SCB1)*SIN60)
c  ** CALCULATE EXPRESSIONS SUMMED BY INCREMENTS OF 2
      C200=C400+C420
      C201=C401+C421
      C202=C402+C422
      C203=C403+C423
      C204=C404+C424
      C205=C404-C424
      C206=C403-C423
      C207=C402-C422
      C208=C401-C421
c     C209=C400-C420
      C210=C410+C430
      C211=C411+C431
      C212=C412+C432
      C213=C413+C433
      C214=C414+C434
      C215=S414-S434
      C216=S413-S433
      C217=S412-S432
      C218=S411-S431
c     C219=0
c     S200=0
      S201=S401+S421
      S202=S402+S422
      S203=S403+S423
      S204=S404+S424
      S205=S424-S404
      S206=S423-S403
      S207=S422-S402
      S208=S421-S401
c     S209=0
c     S210=0
      S211=S411+S431
      S212=S412+S432
      S213=S413+S433
      S214=S414+S434
      S215=C414-C434
      S216=C413-C433
      S217=C412-C432
      S218=C411-C431
c     S219=C410-C430
c  ** CALCULATE THE SQUARE OF THE MAGNITUDE OF G(1,N)+I*G(2,N)
   20 F(1)=(C200+C210)*(C200+C210)*BYKM2
      F(2)=((C201+C211)*(C201+C211)+(S201+S211)*(S201+S211))*BYKM
      F(3)=((C202+C212)*(C202+C212)+(S202+S212)*(S202+S212))*BYKM
      F(4)=((C203+C213)*(C203+C213)+(S203+S213)*(S203+S213))*BYKM
      F(5)=((C204+C214)*(C204+C214)+(S204+S214)*(S204+S214))*BYKM
      F(6)=((C205+C215)*(C205+C215)+(S205+S215)*(S205+S215))*BYKM
      F(7)=((C206+C216)*(C206+C216)+(S206+S216)*(S206+S216))*BYKM
      F(8)=((C207+C217)*(C207+C217)+(S207+S217)*(S207+S217))*BYKM
      F(9)=((C208+C218)*(C208+C218)+(S208+S218)*(S208+S218))*BYKM
      F(10)=((C400-C420)*(C400-C420)+(C410-C430)*(C410-C430))*BYKM
      F(11)=((C208-C218)*(C208-C218)+(S218-S208)*(S218-S208))*BYKM
      F(12)=((C207-C217)*(C207-C217)+(S217-S207)*(S217-S207))*BYKM
      F(13)=((C206-C216)*(C206-C216)+(S216-S206)*(S216-S206))*BYKM
      F(14)=((C205-C215)*(C205-C215)+(S215-S205)*(S215-S205))*BYKM
      F(15)=((C204-C214)*(C204-C214)+(S214-S204)*(S214-S204))*BYKM
      F(16)=((C203-C213)*(C203-C213)+(S213-S203)*(S213-S203))*BYKM
      F(17)=((C202-C212)*(C202-C212)+(S212-S202)*(S212-S202))*BYKM
      F(18)=((C201-C211)*(C201-C211)+(S211-S201)*(S211-S201))*BYKM
      F(19)=(C200-C210)*(C200-C210)*BYKM2
      RETURN
      END
      SUBROUTINE GETAN (F,G)
c  ** GETAN RETRIEVES THE FOURIER COEFFICIENTS CONTAINED IN AN
c  ** ARRAY G DIMENSIONED 2 BY 19 AND DEFINED BY
c  **   G(1,N+1)+I*G(2,N+1)=SUM(F(K)*EXP(-2*PI*I*N*K/KM))/KMH
c  ** WITH THE SUM TAKEN OVER ALL K FROM 1 TO KM.  KMH = KM FOR N = 0
c  ** OR 18, OTHERWISE KMH = KM/2.  THE INTERNAL NOTATION CPQN MEANS
c  **   CPQN = SUM(F(K)*COS(2*PI*N*K/KM))
c  ** WITH THE SUM BEING TAKEN OVER ALL K FROM 1 TO KM WHICH ARE EQUAL
c  ** TO Q MODULO(P).  SPQN IS THE SAME BUT WITH COS REPLACED BY SIN.
c  ** THE NOTATION A=10, B=11, ETC. IS USED FOR  P, Q AND N.
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/FCOM/BYKM,BYKMH,BYKM2,SIN10,SIN20,SIN30,SIN40,SIN50,SIN60,
     *  SIN70,SIN80
      DIMENSION F(36),G(2,19)
c  ** CALCULATE EXPRESSIONS SUMMED BY INCREMENTS OF 12
   10 CC00=F(12)+F(24)+F(36)
      CC01=F(36)-(F(12)+F(24))*SIN30
      CC10=F(1)+F(13)+F(25)
      CC11=F(1)*SIN80-F(13)*SIN40-F(25)*SIN20
      CC20=F(2)+F(14)+F(26)
      CC21=F(2)*SIN70-F(14)*SIN50-F(26)*SIN10
      CC30=F(3)+F(15)+F(27)
      CC31=(F(3)-F(15))*SIN60
      CC40=F(4)+F(16)+F(28)
      CC41=F(4)*SIN50-F(16)*SIN70+F(28)*SIN10
      CC50=F(5)+F(17)+F(29)
      CC51=F(5)*SIN40-F(17)*SIN80+F(29)*SIN20
      CC60=F(6)+F(18)+F(30)
      CC61=(F(6)+F(30))*SIN30-F(18)
      CC70=F(7)+F(19)+F(31)
      CC71=F(7)*SIN20-F(19)*SIN80+F(31)*SIN40
      CC80=F(8)+F(20)+F(32)
      CC81=F(8)*SIN10-F(20)*SIN70+F(32)*SIN50
      CC90=F(9)+F(21)+F(33)
      CC91=(F(33)-F(21))*SIN60
      CCA0=F(10)+F(22)+F(34)
      CCA1=F(34)*SIN70-F(10)*SIN10-F(22)*SIN50
      CCB0=F(11)+F(23)+F(35)
      CCB1=F(35)*SIN80-F(11)*SIN20-F(23)*SIN40
      SC01=(F(12)-F(24))*SIN60
      SC11=F(1)*SIN10+F(13)*SIN50-F(25)*SIN70
      SC21=F(2)*SIN20+F(14)*SIN40-F(26)*SIN80
      SC31=(F(3)+F(15))*SIN30-F(27)
      SC41=F(4)*SIN40+F(16)*SIN20-F(28)*SIN80
      SC51=F(5)*SIN50+F(17)*SIN10-F(29)*SIN70
      SC61=(F(6)-F(30))*SIN60
      SC71=F(7)*SIN70-F(19)*SIN10-F(31)*SIN50
      SC81=F(8)*SIN80-F(20)*SIN20-F(32)*SIN40
      SC91=F(9)-(F(21)+F(33))*SIN30
      SCA1=F(10)*SIN80-F(22)*SIN40-F(34)*SIN20
      SCB1=F(11)*SIN70-F(23)*SIN50-F(35)*SIN10
c  ** CALCULATE EXPRESSIONS SUMMED BY INCREMENTS OF 4
      C400=CC00+CC40+CC80
      C401=CC01+CC41+CC81
      C403=CC00-(CC40+CC80)*SIN30
      C402=(CC01-(CC41+CC81)*SIN30)+((SC41-SC81)*SIN60)
      C404=(CC01-(CC41+CC81)*SIN30)-((SC41-SC81)*SIN60)
      C410=CC10+CC50+CC90
      C411=CC11+CC51+CC91
      C413=(CC10-CC50)*SIN60
      C412=((CC11-CC51)*SIN60)+((SC11+SC51)*SIN30-SC91)
      C414=((CC11-CC51)*SIN60)-((SC11+SC51)*SIN30-SC91)
      C420=CC20+CC60+CCA0
      C421=CC21+CC61+CCA1
      C423=(CC20+CCA0)*SIN30-CC60
      C422=((CC21+CCA1)*SIN30-CC61)+((SC21-SCA1)*SIN60)
      C424=((CC21+CCA1)*SIN30-CC61)-((SC21-SCA1)*SIN60)
      C430=CC30+CC70+CCB0
      C431=CC31+CC71+CCB1
      C433=(CCB0-CC70)*SIN60
      C432=((CCB1-CC71)*SIN60)+(SC31-(SC71+SCB1)*SIN30)
      C434=((CCB1-CC71)*SIN60)-(SC31-(SC71+SCB1)*SIN30)
      S401=SC01+SC41+SC81
      S403=(CC40-CC80)*SIN60
      S402=((CC41-CC81)*SIN60)+((SC41+SC81)*SIN30-SC01)
      S404=((CC41-CC81)*SIN60)-((SC41+SC81)*SIN30-SC01)
      S411=SC11+SC51+SC91
      S413=(CC10+CC50)*SIN30-CC90
      S412=((CC11+CC51)*SIN30-CC91)+((SC51-SC11)*SIN60)
      S414=((CC11+CC51)*SIN30-CC91)-((SC51-SC11)*SIN60)
      S421=SC21+SC61+SCA1
      S423=(CC20-CCA0)*SIN60
      S422=((CC21-CCA1)*SIN60)+(SC61-(SC21+SCA1)*SIN30)
      S424=((CC21-CCA1)*SIN60)-(SC61-(SC21+SCA1)*SIN30)
      S431=SC31+SC71+SCB1
      S433=CC30-(CC70+CCB0)*SIN30
      S432=(CC31-(CC71+CCB1)*SIN30)+((SC71-SCB1)*SIN60)
      S434=(CC31-(CC71+CCB1)*SIN30)-((SC71-SCB1)*SIN60)
c  ** CALCULATE EXPRESSIONS SUMMED BY INCREMENTS OF 2
      C200=C400+C420
      C201=C401+C421
      C202=C402+C422
      C203=C403+C423
      C204=C404+C424
      C205=C404-C424
      C206=C403-C423
      C207=C402-C422
      C208=C401-C421
c     C209=C400-C420
      C210=C410+C430
      C211=C411+C431
      C212=C412+C432
      C213=C413+C433
      C214=C414+C434
      C215=S414-S434
      C216=S413-S433
      C217=S412-S432
      C218=S411-S431
c     C219=0
c     S200=0
      S201=S401+S421
      S202=S402+S422
      S203=S403+S423
      S204=S404+S424
      S205=S424-S404
      S206=S423-S403
      S207=S422-S402
      S208=S421-S401
c     S209=0
c     S210=0
      S211=S411+S431
      S212=S412+S432
      S213=S413+S433
      S214=S414+S434
      S215=C414-C434
      S216=C413-C433
      S217=C412-C432
      S218=C411-C431
c     S219=C410-C430
c  ** CALCULATE FINAL COEFFICIENTS OF FOURIER EXPANSION
      G(1,1)=(C200+C210)*BYKM
      G(1,2)=(C201+C211)*BYKMH
      G(1,3)=(C202+C212)*BYKMH
      G(1,4)=(C203+C213)*BYKMH
      G(1,5)=(C204+C214)*BYKMH
      G(1,6)=(C205+C215)*BYKMH
      G(1,7)=(C206+C216)*BYKMH
      G(1,8)=(C207+C217)*BYKMH
      G(1,9)=(C208+C218)*BYKMH
      G(1,10)=(C400-C420)*BYKMH
      G(1,11)=(C208-C218)*BYKMH
      G(1,12)=(C207-C217)*BYKMH
      G(1,13)=(C206-C216)*BYKMH
      G(1,14)=(C205-C215)*BYKMH
      G(1,15)=(C204-C214)*BYKMH
      G(1,16)=(C203-C213)*BYKMH
      G(1,17)=(C202-C212)*BYKMH
      G(1,18)=(C201-C211)*BYKMH
      G(1,19)=(C200-C210)*BYKM
      G(2,1)=0.
      G(2,2)=(S201+S211)*BYKMH
      G(2,3)=(S202+S212)*BYKMH
      G(2,4)=(S203+S213)*BYKMH
      G(2,5)=(S204+S214)*BYKMH
      G(2,6)=(S205+S215)*BYKMH
      G(2,7)=(S206+S216)*BYKMH
      G(2,8)=(S207+S217)*BYKMH
      G(2,9)=(S208+S218)*BYKMH
      G(2,10)=(C410-C430)*BYKMH
      G(2,11)=(S218-S208)*BYKMH
      G(2,12)=(S217-S207)*BYKMH
      G(2,13)=(S216-S206)*BYKMH
      G(2,14)=(S215-S205)*BYKMH
      G(2,15)=(S214-S204)*BYKMH
      G(2,16)=(S213-S203)*BYKMH
      G(2,17)=(S212-S202)*BYKMH
      G(2,18)=(S211-S201)*BYKMH
      G(2,19)=0.
      RETURN
      END
