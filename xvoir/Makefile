.SUFFIXES : 

.SUFFIXES : .f90 .c .o 
FFLAGS = 

CFLAGS =

OPTIMIZ = -O 2

.PRECIOUS:

VER = 2.0

default: xvoir

.f90.o:
	s.f90 $(OPTIMIZ) $(FFLAGS) -c $<

.c.o:
	s.cc $(OPTIMIZ) $(CFLAGS) -c $<

# xrecsel.c doit etre compile avec -O 0, sinon ca plante a l'execution (M.Valin Jan 2014)
xrecsel.o:	xrecsel.c
	s.cc -O 0 $(CFLAGS) -c $<
	
OBJECTS= convert_ip123.o fstd98.o aux.o get_nbrecs_actifs.o xfsl-xvoir-2000.o xvoir.o  \
flush.o langue.o  parent.o  strutil.o  xinit.o  xrecsel.o widgets-util.o

COMDECKS= xfsl-voir.cdk   xfsl.cdk

xvoir: $(OBJECTS)
	s.f90 $(OBJECTS) -o xvoir_$(VER)-$(BASE_ARCH) -lXm -lMrm -lXmu -lXp -lXt -lXext -lX11 -lXft -lXau -lXrender -lrmnbeta_014

clean:
	/bin/rm -f *.mod *.o xvoir_$(VER)-$(BASE_ARCH)
