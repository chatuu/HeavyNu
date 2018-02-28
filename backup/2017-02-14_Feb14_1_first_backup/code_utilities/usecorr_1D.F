      subroutine usecorr_1D(nbin,binlim1,Corr,
     |                              Var1,DScorr)
      implicit none

      integer nbin
      double precision binlim1(nbin+1)
      double precision Corr(nbin)
      double precision Var1
      double precision DScorr

      integer ii


       do ii=1,nbin
       if(Var1.ge.binlim1(ii  ) .and.
     |    Var1.lt.binlim1(ii+1) )
     |  DScorr=DScorr*Corr(ii)
       enddo


      return
      end
