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
c  ** 12/26/00 first official readme file (MFS)
c  ** 01/02/01 updated readme (MFS)
c  ** ...
c  ** 01/25/01 roman from http://www.schnoggo.com/figlet.html (MFS)
c  ** 02/10/01 revised readme format (MFS)
c  ** ...
c  ** 04/08/04 changed font (JAL)
c  ** 04/09/04 redesigned readme (MFS)
c  ** 04/12/04 updated readme (MFS)
c  ** ...
c  ** 05/06/04 alternate PC version (MFS)
c  ** 05/07/04 updated readme (MFS)
c  ** ...
c  ** 07/22/05 changes for command line (MFS)
c  ** 07/28/05 updated readme (MFS)
c  ** 08/02/05 revised for subversion (MFS)
c  ** 10/17/05 updated readme (MFS)
c  ** 12/19/05 updated readme (MFS)
c  ** 01/17/06 updated readme (MFS)
c  ** 03/23/06 updated readme (MFS)
c  **
c  ** NOTES:
c  ** Many of the comments about the code can be found either in the
c  ** source files or in svn.
c  **
c  *********************************************************************
c  *********************************************************************

c  ** Print out the initial text in the MWRE window.
      SUBROUTINE readme
      PRINT *, ' EEEEEEEEEEE       ddd    .GGGGGG.      .CCCCCC.   MMM',
     *  '        MMMMM '
      PRINT *, ' EEE       |       ddd   GGG''  `GGG    CCC''  `cCC  ',
     *  '`MM.       .MMM'''
      PRINT *, ' EEE          .ddddddd  GGG           CCC           MM',
     *  'MM     M''MMM  '
      PRINT *, ' EEEEEEEE    ddd'' `ddd  GGG           CCC           M',
     *  ' MMM. .M  MMM  '
      PRINT *, ' EEE         dd     dd  GGG     gGGGG CCC           M ',
     *  ' `MMM''   MMM  '
      PRINT *, ' EEE       | ddd   ddd  `GG.    .GG''  `CCC    cCC   M',
     *  '    V     MMM  '
      PRINT *, ' EEEEEEEEEEE `dddddddd   `GGGGGGGG''    `CCCCCCCC''  M',
     *  'MM        MMMMM '
      PRINT *, '                                                      '
      PRINT *, 'Model: Model II (8x10x9)                              '
      PRINT *, 'Revision: '
cDAT
      PRINT *, 'Platform: MacOS X 10.3.9-10.4/Windows 2000/XP/Vista   '
      PRINT *, 'Contact: Mark Chandler, mac59@columbia.edu            '
      PRINT *, '         Ken Mankoff, mankoff@giss.nasa.gov           '
      PRINT *, '                                                      '
      END

c  ** print out more info into the window and set the window
c  ** title using Mac specific code (no other way)      
      SUBROUTINE readme2 (runnumber,geomname)
      CHARACTER*21 runnumber, RUNSHORT
      CHARACTER*8 geomname
      INTEGER LLAB1
      LLAB1 = INDEX(runnumber,'(') -1
      RUNSHORT = runnumber(1:LLAB1)
C      PRINT *, 'Geometry: ',TRIM(geomname)
      PRINT *, 'Screen output (this window) saved to: "',TRIM(RUNSHORT),
     *   '" output folder'
      PRINT *, 'Accumulated diagnostics: "acc" folder                 '
      PRINT *, 'Restart files saved to: "rsf" folder                  '
      PRINT *, 'Monthly diagnostic printouts saved to: "prt" folder   '
      PRINT *, '                                                      '
      END
