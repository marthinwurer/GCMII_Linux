

SVNVER=$(shell svn info | grep "Last Changed Rev" | cut -d" " -f4)
TODAY=$(shell date +%Y%m%d)

all:
#	Encode version into GCM README outputs
	sed s/cREV/\ \ \ \ \ \ \PRINT\ *,\ \'Revision\:\ ${SVNVER}\'/ README.f.in > README.f

#	build model(s) for PPC and Intel
	@echo " "
	@echo " "
	@echo "To build model, run the following commands:"
	@echo " "
	@echo "ifort.setup"
	@echo "make -f Makefile.ifort clean; make -f Makefile.ifort "
	@echo " "
	@echo "absoft.ppc.setup"
	@echo "make -f Makefile.Mac.PPC clean; make -f Makefile.Mac.PPC"
	@echo " "
	@echo "make install"
	@echo " "

install: 
	cp modelII_intel.exe modelII_ppc.exe ../../../EdGCM/Applications/Model/

clean:
	rm -f *.o
	rm -f model.command
	rm -f modelII_intel.exe
	rm -f modelII_ppc.exe
