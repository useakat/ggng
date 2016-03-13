CC            = cc
CFLAGS        = -O
#FC            = fort77
FC            = g77
#FC	      = gfortran
PHYSHOME      = $(PHYSLIB)
CFMGHOME      = ../QCDProcesses
INCLUDES      = -I./ -I$(PHYSHOME)/hml/inc -I$(CFMGHOME)/HELAS
FFLAGS	      = -O3 $(INCLUDES)
EXTHDRS	      = $(PHYSHOME)/hml/inc/bsfile.inc \
		$(PHYSHOME)/hml/inc/bsxdim.inc \
		$(PHYSHOME)/hml/inc/crndm.inc \
		$(PHYSHOME)/hml/inc/fvevnt.inc \
		$(PHYSHOME)/hml/inc/gnbeam.inc \
		$(PHYSHOME)/hml/inc/gudat1.inc \
		$(PHYSHOME)/hml/inc/gujets.inc \
		$(PHYSHOME)/hml/inc/hmparm.inc \
		$(PHYSHOME)/hml/inc/hmunit.inc \
		$(PHYSHOME)/hml/inc/smcupl.inc \
		$(PHYSHOME)/hml/inc/smptab.inc
INSTALL	      = install
#LD	      =  fort77
#LD	      =  gfortran
LD	      =  g77
LIBPHY        = $(PHYSHOME)/lib_kek
LIBCFMG       = $(CFMGHOME)/HELAS/lib
LIBDIR        = -L$(LIBPHY) -L$(LIBCERN) -L$(LIBCFMG)
LDFLAGS	      = $(LIBDIR)
LIBS          = -lhml -lhmlutil -ldhelas3 -lbases50_seed -lcfqcdv4 -llibrary -lmylib

MAKEFILE      = Makefile

OBJS          = bfunc.o userin.o pshk0.o opendata.o pdf.o pdfwrap.o Ctq6Pdf.o \
		GLUON6_0.o jgggcf.o ggggcf.o sumw.o ipnext2.o VVV1_0.o VVV1_1.o matrix.o\
		apply_cuts.o set_pdf.o alfas_functions.o GLUON4_0.o GLUON5_0.o GLUON7_0.o GLUON8_0.o\
		GLUON9_0.o GLUON10_0.o pshk0_dphi.o pshk0_dphi_deta.o

OBJS_BASES = mainb.o usrout.o $(OBJS)

OBJS_TEST = testamp.o

OBJS_SPRING = mains.o spinit.o spevnt.o $(OBJS)

OBJS_PHASE = mainp.o phase.o

SYSHDRS	      =

.f.o:
	$(FC) -c $(FFLAGS) $<

bases:  $(OBJS_BASES)
	$(LD) $(LDFLAGS) $(OBJS_BASES) $(LIBS) -o $@

get_sum: get_sum.o
	$(LD) $(LDFLAGS) get_sum.o $(LIBS) -o $@

test:  $(OBJS_TEST)
	$(LD) $(LDFLAGS) $(OBJS_TEST) $(LIBS) -o testamp

spring:  $(OBJS_SPRING)
	$(LD) $(LDFLAGS) $(OBJS_SPRING) $(LIBS) -o $@

clean:
	@rm *.o; rm *~
###
