#
# Makefile for the SPY ala Alberto Fit
#
SHELL = /bin/sh

#include $(NOMADSRC)/Rules/Nomad.versions

INCLUDES = .

F_SOURCES=$(wildcard *.F) 
F_SRCS=$(notdir $(F_SOURCES))
FOBJS=$(F_SRCS:%.F=%.o)

# ./xsec/ subdirectory:
#F_SOURCES_XSEC=$(wildcard ./xsec/*.F)
#F_SRCS_XSEC=$(notdir $(F_SOURCES_XSEC))
#FOBS_XSEC=$(F_SRCS_XSEC:%.F=%.o)
#LIBPATH_XSEC=./xsec

mains = main.o
OBJS = $(filter-out $(mains),$(FOBJS))

LOCPATH = 
CERNPATH= -L$(CERN)/pro/lib
LIBPATH = $(LOCPATH) $(CERNPATH) 

FFLAGS  += -g
LDFLAGS += -g

#
# To select what kind of fit we're going to do:
#
#FFLAGS += -DSTANDARDFIT 

link_F = g77

default: main

main: main.o $(OBJS)
	$(RM) $@
	$(link_F) $(LDFLAGS) -o $@ $@.o \
	$(OBJS) \
	$(LIBPATH)  `cernlib pdflib pawlib graflib packlib mathlib kernlib`

