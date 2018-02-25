c  *********************************************************************
c  *********************************************************************
c  **
c  ** setpath
c  ** SetPath changes the cwd (current working directory) based on a 
c  ** parameter passed in to the command line. In Unix the path can
c  ** be anything but under Windoz it has to be a realtive path. This
c  ** code works around a bug where AP ShellExecute in Windows sets the
c  ** path to the database folder instead of the fortran program`s 
c  ** folder.
c  **
c  ** CHANGE HISTORY:
c  **
c  ** 05/04/04 developed from GetArgs (MFS)
c  ** 05/06/04 no error version so it can work on the Mac too (MFS)
c  ** 05/07/04 model sepecific version with underscore (MFS)
c  **
c  ** NOTES:
c  ** f77 -f -lU77 -N15 setpath.f
c  **
c  *********************************************************************
c  *********************************************************************

      SUBROUTINE SETPATH
      INTEGER*4 ARGC             ! arg count
      CHARACTER*255 ARGV         ! arg values (path)
      INTEGER*4 IERR             ! path error

      ARGC = IARGC_()                         
      IF (ARGC .GE. 1) THEN
          CALL GETARG_(1,ARGV)    ! get path as arg
          IERR = CHDIR_(ARGV)     ! change working directory
      END IF

      END

