c  *********************************************************************
c  *********************************************************************
c  **
c  ** Model IImac
c  ** Based on GCMII code for IBM RS/6000 computers created at GISS
c  ** Modified to compile under Absoft Pro Fortran 6.2 for MacOS.  
c  ** Based on BRCRFmac.COM
c  **
c  ** CHANGE HISTORY:
c  **
c  ** 05/27/02 new S0X code (MFS)
c  ** 06/07/02 added ktrend (MFS)
c  ** 06/15/02 renamed ktrend to ktrendext(MFS)
c  ** 06/19/02 moved joldyear to here (MFS)
c  ** 05/27/04 changed JOLDYEAR to array with MGAS values (=NGAS) (MAC)
c  **
c  ** NOTES:
c  **
c  *********************************************************************
c  *********************************************************************

c  ** force reals to be real*8
c      IMPLICIT REAL*8 (A-H,O-Z)
c  ** new common for INPUTFORCINGS
      PARAMETER (MGAS=5)
      COMMON/IFORCINGS/
     *  KTRENDEXT, JOLDYEAR(MGAS),
     *  ICO2DATA, ICO2DATASTART, ICO2DATAEND,
     *  IN2ODATA, IN2ODATASTART, IN2ODATAEND,
     *  ICH4DATA, ICH4DATASTART, ICH4DATAEND,
     *  IF11DATA, IF11DATASTART, IF11DATAEND,
     *  IF12DATA, IF12DATASTART, IF12DATAEND,
     *  IS0XDATA, IS0XDATASTART, IS0XDATAEND,
     *  IVOLDATA, IVOLDATASTART, IVOLDATAEND
      COMMON/RFORCINGS/
     *  ZN2O, CH4, F11, F12, VOL
