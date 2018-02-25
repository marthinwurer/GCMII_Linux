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
c  ** 7/27/99 First Successful Compile! (DCH)
c  ** 7/27/99 Changed MCLOCK() call to DTIME() unix call (DCH)
c  ** 7/30/99 Patched DTIME to 4 to make it compile
c  ** 11/01/99 Another patch to work as a shared lib (MFS)
c  ** 12/21/00 Try with another version of Model II (MFS)
c  ** 09/28/02 Use clock() to replace MCLOCK() (MFS)
c  ** 01/27/05 working MCLOCK (GLR)
c  ** 01/17/06 reverted to CLOCKS, secnds on Mac, clock on Win (MFS)
c  **
c  ** NOTES:
c  **  Not sure if DTIME does the same thing as MCLOCK, but Reto believe
c  **  all calls to CLOCKS are not important to the running of the model
c  **
c  *********************************************************************
c  *********************************************************************

c  MS SYSTEM ROUTINES EMULATION FOR IBM RS/6000
c
      SUBROUTINE CLOCKS(IHSC)
c  THIS VERSION OF CLOCKS RETURNS PROCESS TIME OF USER AND
c  SYSTEM TIME OF CHILD PROCESSES
c  NOTE: MCLOCK IS REALLY IN HUNDREDTHS OF A SECOND, NOT SIXTIETHS.
      IHSC = secnds(0.0)
c  ** Windows should use
c      IHSC = clock()
      RETURN
      END

      FUNCTION THBAR (X,Y)
c  **
c  ** TH-mean used for vertical differencing (Arakawa)
c  ** THBAR(T1,T2) = (ln(T1) - ln(T2))/(1/T2 - 1/T1)
c  **              = T1*g(x) with x=T1/T2 , g(x)=ln(x)/(x-1)
c  **      g(x) is replaced by a rational function
c  **           (a+bx+cxx+dxxx+cx**4)/(e+fx+gxx)
c  **      approx.error <1.E-6 for x between .9 and 1.7
c  **
c     REAL*8 A,B,C,D,E,F,G,Q,AL
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (
     *  A=113.4977618974100d0,B=438.5012518098521d0,
     *  C=88.49964112645850d0,D=-11.50111432385882d0,
     *  E=30.00033943846368d0,F=299.9975118132485d0,
     *  G=299.9994728900967d0)
c     DATA A,B,C,D,E,F,G/113.4977618974100,438.5012518098521,
c    *  88.49964112645850,-11.50111432385882,
c    *  30.00033943846368,299.9975118132485,299.9994728900967/
      Q=X/Y
      AL=(A+Q*(B+Q*(C+Q*(D+Q))))/(E+Q*(F+G*Q))
      THBAR=X*AL
      RETURN
      END

      FUNCTION EXPBYK (X)
      IMPLICIT REAL*8 (A-H,O-Z)
      EXPBYK=X**.286
      RETURN
      END

      SUBROUTINE DREAD (IUNIT,AIN,LENGTH,AOUT)
c  **
c  ** READ IN REAL*4 ARRAY AND CONVERT TO REAL*8
c  **
      REAL*4 AIN(LENGTH)
      REAL*8 AOUT(LENGTH)
      READ (IUNIT) AIN
      DO 10 N=LENGTH,1,-1
   10 AOUT(N)=AIN(N)
      RETURN
      END

      SUBROUTINE MREAD (IUNIT,M,NSKIP,AIN,LENGTH,AOUT)
c  **
c  ** READ IN INTEGER & REAL*4 ARRAY AND CONVERT TO REAL*8
c  **
      REAL*4 AIN(LENGTH),X
      REAL*8 AOUT(LENGTH)
      READ (IUNIT) M,(X,N=1,NSKIP),AIN
      DO 10 N=LENGTH,1,-1
   10 AOUT(N)=AIN(N)
      RETURN
      END

      SUBROUTINE READT (IUNIT,NSKIP,AIN,LENGTH,AOUT,IPOS)
c  **
c  ** READ IN TITLE & REAL*4 ARRAY AND CONVERT TO REAL*8
c  **
      REAL*4 AIN(LENGTH),X
      REAL*8 AOUT(LENGTH)
      CHARACTER*80 TITLE
      DO 10 N=1,IPOS-1
   10 READ (IUNIT,END=920)
      READ (IUNIT,ERR=910,END=920) TITLE,(X,N=1,NSKIP),AIN
c     IF(LEN.LT.4*(20+NSKIP+LENGTH)) GO TO 930
      DO 100 N=LENGTH,1,-1
  100 AOUT(N)=AIN(N)
      WRITE(6,'('' Read from Unit '',I2,'':'',A80)') IUNIT,TITLE
      RETURN
  910 WRITE(6,*) 'READ ERROR ON UNIT',IUNIT
      STOP 'READ ERROR'
  920 WRITE(6,*) 'END OF FILE ENCOUNTERED ON UNIT',IUNIT
      STOP 'NO DATA TO READ'
c  30 WRITE(6,*) LEN/4,' RATHER THAN',20+NSKIP+LENGTH,' WORDS ON UNIT',
c    *  IUNIT
c     STOP 'NOT ENOUGH DATA FOUND'
      END
      SUBROUTINE TIMER (MNOW,MINC,MSUM)
c  **
c  ** OUTPUT: MNOW (.01 S) = CURRENT CPU TIME
c  **         MINC (.01 S) = TIME SINCE LAST CALL TO SUBROUTINE TIMER
c  **         MSUM (.01 S) = MSUM + MINC
c  **
      SAVE MLAST
      MNOW  = secnds (0.0)      
c  ** Windows should use
c      MNOW = clock()
      MINC  = MNOW - MLAST
      MSUM  = MSUM + MINC
      MLAST = MNOW
      RETURN
      END
