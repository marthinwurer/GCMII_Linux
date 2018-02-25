c  *********************************************************************
c  *********************************************************************
c  **
c  ** Model IImac
c  ** Based on GCMII code for IBM RS/6000 computers created at GISS
c  ** Modified to compile under Absoft Pro Fortran 7.0 for MacOS 9/X.  
c  ** Code to do trends
c  ** 
c  ** CHANGE HISTORY:
c  ** 
c  ** 05/27/02 new S0X code (MSC/MFS)
c  ** 06/03/02 new greenhouse gas trend code (MSC/MFS)
c  ** 06/10/02 fixed bug so it reads the first year (MFS)
c  ** 06/17/02 fixed logic error in code (MFS)
c  ** 06/19/02 moved joldyear to the common block (MFS)
c  ** 05/06/04 added fixed var to include END (MAC)
c  ** 05/27/04 changed JOLDYEAR to array with 5 values (=NGAS) (MAC)
c  ** 06/22/04 removed extra defined jyear (MFS)
c  ** 03/16/05 fix format statement (MFS/Patrick Lee/JAL)
c  ** 
c  ** NOTES:
c  ** This code replaces RFRC
c  ** 
c  *********************************************************************
c  *********************************************************************
      
      SUBROUTINE SOLARFORCING
c  ** reads the solar trend, replaces code in RFRC
c  ** does both trends and fixed
      INCLUDE 'BA94jalC9.COM'
      INCLUDE 'FORCINGSmac.COM'
c  ** var
      INTEGER*4 JDATAYEAR
      REAL*8 DATAVALUE
      REAL*8 XXGAS
      INTEGER*4 OLDYEAR
      DATA OLDYEAR/-999/
      
      IF((IS0XDATA.GT.0).AND.(JYEAR.GE.IS0XDATASTART))THEN
        IF(OLDYEAR.NE.JYEAR)THEN
          XXGAS=GREENHOUSEGASFORCING(IS0XDATA,IS0XDATASTART,
     *      IS0XDATAEND)
          OLDYEAR=JYEAR
         END IF
        S0X=XXGAS
      END IF
      END
      
      SUBROUTINE DATAREFYEAR(XGAS,YEAR,NGAS)
c  ** for reference purposes 1958 values always have to be certian
c  ** constants or the scaling goes very weird, Andy warned me
c  **
c  ** Here are the original lines from R83ZAmacC9
C                 H2O   CO2  O3      O2 NO2   N2O   CH4   CCL3F1 CCL2F2 4211.   
C     DATA FULGAS/1.0,  1.0,1.0,    1.0,1.0,  1.0,  1.0,    1.0,    1.0/4212.   
c     DATA PPMV58/0.0,315.0,0.0,210000.,0.0,0.295,1.400,8.00E-6,25.0E-6/4213.   
      INCLUDE 'FORCINGSmac.COM'
c  ** var
      REAL*8 XGAS(5)
      REAL*8 YEAR
      INTEGER*4 NGAS
c  ** set xgas to 1958 values
      XGAS(1) = 315.
      XGAS(2) = 0.295
      XGAS(3) = 1.400
      XGAS(4) = 8.00E-6 * 1000.D0
      XGAS(5) = 25.0E-6 * 1000.D0
c  ** setup joldyear
      DO 100 I=1,NGAS
      JOLDYEAR(I) = -999
  100 CONTINUE
      END
      
      SUBROUTINE DATAFILETREND(XGAS,YEAR,NGAS)
c  ** file based trend code to run any trend you want without having to
c  ** recompile the model. updates the trends from the file once a year
c  ** and caches the values inbetween. 
      INCLUDE 'BA94jalC9.COM'
      INCLUDE 'FORCINGSmac.COM'

      REAL*8 XGAS(5)
      REAL*8 XXGAS(5)
      REAL*8 YEAR
      INTEGER*4 NGAS
        
      IF((ICO2DATA.GT.0).AND.(JYEAR.GE.ICO2DATASTART))THEN
        IF(JOLDYEAR(1).NE.JYEAR)THEN
          XXGAS(1)=GREENHOUSEGASFORCING(ICO2DATA,ICO2DATASTART,
     *      ICO2DATAEND)
          JOLDYEAR(1)=JYEAR
         END IF
        XGAS(1)=XXGAS(1)
      ELSE
        XGAS(1)=CO2
      END IF
        
      IF((IN2ODATA.GT.0).AND.(JYEAR.GE.IN2ODATASTART))THEN
        IF(JOLDYEAR(2).NE.JYEAR)THEN
          XXGAS(2)=GREENHOUSEGASFORCING(IN2ODATA,IN2ODATASTART,
     *      IN2ODATAEND)
          JOLDYEAR(2)=JYEAR
        END IF
        XGAS(2)=XXGAS(2)
      ELSE
        XGAS(2)=ZN2O
      END IF
        
      IF((ICH4DATA.GT.0).AND.(JYEAR.GE.ICH4DATASTART))THEN
        IF(JOLDYEAR(3).NE.JYEAR)THEN
          XXGAS(3)=GREENHOUSEGASFORCING(ICH4DATA,ICH4DATASTART,
     *      ICH4DATAEND)
          JOLDYEAR(3)=JYEAR
        END IF
        XGAS(3)=XXGAS(3)
      ELSE
        XGAS(3)=CH4
      END IF
        
      IF((IF11DATA.GT.0).AND.(JYEAR.GE.IF11DATASTART))THEN
        IF(JOLDYEAR(4).NE.JYEAR)THEN
          XXGAS(4)=GREENHOUSEGASFORCING(IF11DATA,IF11DATASTART,
     *      IF11DATAEND)
          JOLDYEAR(4)=JYEAR
        END IF
        XGAS(4)=XXGAS(4)
      ELSE
        XGAS(4)=F11
      END IF
      
      IF((IF12DATA.GT.0).AND.(JYEAR.GE.IF12DATASTART))THEN
        IF(JOLDYEAR(5).NE.JYEAR)THEN
          XXGAS(5)=GREENHOUSEGASFORCING(IF12DATA,IF12DATASTART,
     *      IF12DATAEND)
          JOLDYEAR(5)=JYEAR
        END IF
        XGAS(5)=XXGAS(5)
      ELSE
        XGAS(5)=F12
      END IF
        
      END
      
      FUNCTION GREENHOUSEGASFORCING(IDATAUNIT,ISTARTDATA,IENDDATA)
c  ** reads a greenhouse gas trend and gives the result back
c  ** to the rest of the code, replaces code in RFRC
      INCLUDE 'BA94jalC9.COM'
      INCLUDE 'FORCINGSmac.COM'
c  ** var
      INTEGER*4 IDATAUNIT,ISTARTDATA,IENDDATA
      INTEGER*4 JDATAYEAR
      REAL*8 DATAVALUE
      REAL*8 GREENHOUSEGASFORCING
      
      IF(IDATAUNIT.GT.0)THEN
   10   READ(IDATAUNIT,900,ERR=100,END=100) JDATAYEAR,DATAVALUE
        IF(JDATAYEAR.EQ.JYEAR)THEN
          GREENHOUSEGASFORCING=DATAVALUE
        ELSE IF(JDATAYEAR.LT.JYEAR)THEN
          GO TO 10
        ELSE
          GO TO 110
        END IF
      ELSE
c  ** default is to do nothing        
      END IF 
c  ** sucess      
      RETURN
c  ** errors     
  100 STOP 'Error: Data file ends before current year.\n'
  110 STOP 'Error: Data file begins after current year.\n'
c  ** format
  900 FORMAT(I4,T6,F15.8)
      END

      
