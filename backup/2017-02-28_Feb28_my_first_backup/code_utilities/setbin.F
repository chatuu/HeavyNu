      subroutine setbin(var,extin,minin,maxin,binin,tagin)
      implicit none

      integer var !--> Variable number
      integer extin !--> extra cut type
      real minin !--> Lower histogram limit
      real maxin !--> Upper histogram limit
      integer binin !--> number of bins
      character*10 tagin !--> Binning tag

      !-------------------------!
      ! This group of variables
      ! occurs here and in 
      ! bookhistos.F, and both 
      ! must agree exactly.
      integer nbinvarMAX !--> Maximum number of bins per variable 
      integer nvarMAX !--> Maximum number of variables
      integer extypMAX !--> Maximum number of extra cut types
      parameter (nbinvarMAX=20)
      parameter (nvarMAX=99)
      parameter (extypMAX=500)
#include "booking.inc"
      !-------------------------!

      integer currvar !--> Current Variable number
      save currvar
      data currvar/-999/
      integer bin !--> Current Binning number
      save bin
      data bin/0/

      !---------------------------------------------------!
      ! Check Input:
       if(var.lt.1.or.var.gt.nvarMAX) then
        call printerror(1,499382019,'setbin.F            ')
        print*,'var out of range: ',var
        print*,''
        stop
       endif
       if(extin.lt.0.or.extin.gt.extypMAX) then
        call printerror(1,558746901,'setbin.F            ')
        print*,'extin out of range: ',extin
        print*,''
        stop
       endif
       if(binin.lt.1) then
        call printerror(1,928830019,'setbin.F            ')
        print*,'binin out of range: ',binin
        print*,''
        stop
       endif
      !---------------------------------------------------!

      ! Reset the binning number if loading a new variable:
      if(var.ne.currvar) then
       currvar=var
       bin=0
      endif
      bin=bin+1

      ! Set histogram information:
       hmin(var,bin,extin)= minin
       hmax(var,bin,extin)= maxin
       nbin(var,bin,extin)= binin
       bintag(var,bin)=tagin
       nbintypT(var)=bin



      return
      end
