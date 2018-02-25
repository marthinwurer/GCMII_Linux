c  *********************************************************************
c  *********************************************************************
c  **
c  ** Model IImac
c  ** Based on GCMII code for IBM RS/6000 computers created at GISS
c  ** Modified to compile under Absoft Pro Fortran 6.2 for MacOS.  
c  ** Based on MP030BmacC9, BA94C9 and MA94DC9
c  **
c  ** CHANGE HISTORY:
c  **
c  ** 05/25/01 Linux needed new random number generator (JAJ)
c  ** 09/28/02 to unify model use new style ran_ (MFS)
c  ** 06/22/04 use old random function for xlf (MFS)
c  ** 01/27/05 restored to old version (MFS)
c  **
c  ** NOTES:
c  **
c  *********************************************************************
c  *********************************************************************

      FUNCTION RANDU (X)
      IMPLICIT REAL*8 (A-H,O-Z)
c      REAL*4 ran
      RANDU=rand(IX)
C**** THIS FUNCTION GENERATES RANDOM NUMBERS ON AN IBM 360 OR 370
C  10 IY=IX*65539
C     IF (IY) 20,40,30
C  20 IY=(IY+2147483647)+1
C  30 IX=IY
C     RANDU=DFLOAT(IY)*.465661287308D-9
      RETURN
C  40 IX=1
C     GO TO 10
      ENTRY RINIT (INIT)
      IX=INIT
      RINIT=0.0 
      RETURN
      ENTRY RFINAL (IFINAL)
      IFINAL=IX
      RFINAL=0.0 
      RETURN
      END
