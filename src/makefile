# Makefile for anapaw used in nov09ivsm experiment
#  make
#+
PROGRAM = anapaw
ANAPAW_LIB = /usr/local/anapaw-ribf28/lib
#CERNLIB = /usr/local/cernlib/2005/lib

#-
#
########## NEVER CHANGE ############
#FC       = g77
FC       = gfortran
CC       = gcc34
CPP      = g++34
FFLAGS   =  -g -O3 -Wall -ff2c -funroll-loops -fbounds-check -rdynamic \
           -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -Wtabs
LFLAGS   = -L$(CERNLIB) -lpawlib -lgraflib -lgrafX11  -lpacklib -lmathlib \
           -lkernlib -Icern -lnsl -lreadline -ltermcap 
#LDLIBS   = -L/usr/X11R6/lib64 -lX11 -ldl -lcrypt -L$(ANAPAW_LIB) -lanalib \
#	   -lXt -lXaw -lpthread -L$(CERNLIB) -lpawlib -lgraflib -lgrafX11 \
#           -lpacklib -lmathlib -lkernlib -Icern -lnsl -lreadline -ltermcap  \
3           -lstdc++ -llapack3 -lblas -lexpat
LDLIBS   = -L/usr/X11R6/lib64 -lX11 -ldl -lcrypt -L$(ANAPAW_LIB) -lanalib \
	   -lpthread -L$(CERNLIB) -lpawlib -lgraflib -lgrafX11 \
           -lpacklib -lmathlib -lkernlib -Icern -lnsl -lreadline -ltermcap  \
           -lstdc++ -llapack3 -lblas -lexpat
#RTLIBS   = -L/usr/cern/lib/root -lCore -lCint -lHist -lGraf -lGraf3d -lGpad \
#           -lTree -lRint -lPostscript -lMatrix -lPhysics -pthread -lm -ldl -rdynamic
RTLIBS   =  $(shell $(ROOTSYS)/bin/root-config --glibs)

CPPFLAGS = 
LDFLAGS  = 
########## NEVER CHANGE ############

##
ADD      = enc_coin.o \
	   enc_ssd.o \
	   enc_rf.o enc_dssd.o enc_mssd.o enc_ppac.o\
	   enc_tel.o\
           dssd_prm.o ssd_prm.o mssd_prm.o ppac_prm.o

#	   encic.o loadicprm.o \
#	   encimage.o loadimageprm.o \
#	   enctotale.o loadtotaleprm.o \
#	   encpileup.o enclabr.o
#           encpid.o loadpidprm.o \
#	   encmatrix.o loadmatrixconf.o loadmatrixprm.o \
#	   encgeneric.o enc_ssd.o \
#           encgammappac.o loadgppacconf.o loadgppacprm.o calcgppac.o \
#           encpla.o loadplaconf.o loadplaprm.o \
#	   encdali.o loaddaliprm.o \

##
#OBJS  =  usersrc.o usersub.o add_ntuple.o util.o
OBJS  =  usersrc.o usersub.o add_ntuple.o 
# rayfit.o \
#         readmat.o rec_delta.o rec_beta.o physutil.o
OBJS += $(ADD)
OBJS += $(ANAPAW_LIB)/anapaw.o

all: $(PROGRAM)

usersrc.o: rawdata.fh analysevent.fh

$(PROGRAM): $(OBJS)  $(ANAPAW_LIB)/libanalib.a 
	$(FC) -o $@ $^ $(LDFLAGS) $(LDLIBS) $(RTLIBS)

clean::
	rm -f $(PROGRAM) *.f~ *.c~ *~ *.o

.F.f: 
	$(CPP) -o $@ $^
