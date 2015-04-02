.SUFFIXES : 

.SUFFIXES : .f90 .c .o 
FFLAGS = 

CFLAGS =

OPTIMIZ = -O 2

.PRECIOUS:

VER = 2.0

RMNLIB = rmn_015.2

default: Xvoir Voir

.f90.o:
	s.f90 $(OPTIMIZ) $(FFLAGS) -c $<

.c.o:
	s.cc $(OPTIMIZ) $(CFLAGS) -c $<

# xrecsel.c doit etre compile avec -O 0, sinon ca plante a l'execution (M.Valin Jan 2014)
xrecsel.o:	xrecsel.c
	s.cc -O 0 $(CFLAGS) -c $<
	
OBJECTS= fstd98.o excdes_new.o xdf98.o aux.o get_nbrecs_actifs.o xfsl-xvoir-2000.o xvoir.o  \
flush.o langue.o  parent.o  strutil.o  xinit.o  xrecsel.o widgets-util.o

OBJECTS2=fstd98.o voir.o excdes_new.o xdf98.o

COMDECKS= xfsl-voir.cdk   xfsl.cdk

Xvoir: xvoir_$(VER)-$(BASE_ARCH)
	ln -sf xvoir_$(VER)-$(BASE_ARCH) Xvoir

xvoir_$(VER)-$(BASE_ARCH): $(OBJECTS)
	s.f90 $(OBJECTS) -o xvoir_$(VER)-$(BASE_ARCH) -LLIBS0 -lXm -lMrm -lXmu -lXp -lXt -lXext -lX11 -lXft -lXau -lXrender -l$(RMNLIB)

Voir: voir_$(VER)-$(BASE_ARCH)
	ln -sf voir_$(VER)-$(BASE_ARCH) Voir

voir_$(VER)-$(BASE_ARCH): $(OBJECTS2)
	s.f90 $(OBJECTS2) -o voir_$(VER)-$(BASE_ARCH) -l$(RMNLIB)

clean:
	/bin/rm -f *.mod *.o

veryclean:
	/bin/rm -f *.mod *.o xvoir_$(VER)-$(BASE_ARCH) voir_$(VER)-$(BASE_ARCH) Xvoir Voir
