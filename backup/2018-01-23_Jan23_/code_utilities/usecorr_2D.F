      subroutine usecorr_2D(nbin1,nbin2,binlim1,binlim2,Corr,
     |                              Var1,Var2,DScorr)
      implicit none

      integer nbin1,nbin2
      double precision binlim1(nbin1+1)
      double precision binlim2(nbin2+1)
      double precision Corr(nbin1,nbin2)
      double precision Var1,Var2
      double precision DScorr

      integer ii,jj


       do ii=1,nbin1
       do jj=1,nbin2

       if(Var1.ge.binlim1(ii  ) .and.
     |    Var1.lt.binlim1(ii+1) .and.
     |    Var2.ge.binlim2(jj  ) .and.
     |    Var2.lt.binlim2(jj+1) )
     |  DScorr=DScorr*Corr(ii,jj)
       enddo
       enddo


      return
      end
