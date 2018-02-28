      subroutine calcjpsi(hid,hidbkg,hidsig,par,
     |                hidjpsi,hidbkgfullh,
     |                    ityp,idet,incand,
     |                    typtag,dettag,ncndtag,
     |                    nbin,nfitbin,binbegin,binend,
     |                    xbegin,xstep,siglo,sighi,
     |                    plotjpsi,plotjpsierr,
     |                    nbtyp,bintag,binnum)
      implicit none

      ! This function takes the main, full data plot, and makes
      ! a signal sample by subtracting a 4th order polynomial
      ! which was obtained by fitting the bkg sample (signal data
      ! range removed from the fit)

      ! Input parameters for the bkg fit function
      ! (A1 + A2*x + A3*x^2 +...)

      integer hid
      integer hidbkg
      integer hidsig
      real par(5)
      integer hidjpsi
      integer hidbkgfullh
      integer ityp
      integer idet
      integer incand
      character*6 typtag(0:3)
      character*4 dettag(0:3)
      character*6 ncndtag(2:5)
      integer nbin
      integer nfitbin
      integer binbegin
      integer binend
      real xbegin
      real xstep
      real siglo(3)
      real sighi(3)
      real plotjpsi(nbin)
      real plotjpsierr(nbin)
      integer nbtyp
      character*6 bintag(nbtyp)
      integer binnum


      ! Do loop variables
      integer ii,ibin
      
      real plot(nbin) 

      real bkgd(nfitbin)
      real sig(nfitbin)
      real sigerr(nfitbin)
      real bkgh(nbin)
      real bkgherr(nbin)
      ! J/Psi MC in fit range:
      real jpsi(nfitbin) 
      real jpsierr(nfitbin)
      


      ! counts:
      real njpsi(3) 
      real staterr(3) 
      real back(3) 

      real xpos  
      integer idx 

      character*80 filename


      ! Load the full data plot:
      call HRIN(hid,9999,0)
      call hunpak(hid,plot,' ',0)
      ! Load the background plot:
      call HRIN(hidbkg,9999,0)

      ! Initialize the full range background histogram:
      do ii=1,nbin
       bkgh(ii)=0.0
       bkgherr(ii)=0.0
      enddo



      !=============================================================
      ! Find the value of the background fit function at the 
      ! center position of each bin, and store the values into "bkgd":
      do ii=1,nfitbin 

        ! Current position along x-axis:
        xpos = xbegin + 0.5*xstep + (ii-1)*xstep

        bkgd(ii)= par(1) +
     |            par(2)*xpos + 
     |            par(3)*xpos**2 + 
     |            par(4)*xpos**3 + 
     |            par(5)*xpos**4
        if(bkgd(ii).lt.0.0) bkgd(ii)=0.0
      enddo 
      !=============================================================

      ! Initialize sums for all three signal ranges:
      do ii=1,3
       njpsi(ii)=0.0 
       staterr(ii)=0.0 
       back(ii)=0.0 
      enddo


      ! Loop over the fitted bins and calculate the signal:
      do ibin=binbegin,binend

         ! Index counting from 1 to the number of bins looped over:
         idx=ibin-binbegin+1 


         ! Calculate the signal value (bkg fit subtracted from full plot)
         sig(idx)=plot(ibin)-bkgd(idx)
         if(sig(idx).lt.0.0) sig(idx)=0.0

         ! Calculate the error in signal based on number of data
         sigerr(idx)=sqrt(plot(ibin))

c  c       I think Leslie had a small shifting error.
c  c       The very first bin should be "xbegin+0.5*xstep",
c  c       so for ibin=binbegin we want the factor next to xstep to be
c  c       zero. 
cc         xpos=xbegin+0.5*xstep+(idx)*xstep 

         ! Current position along x-axis:
         xpos=xbegin+0.5*xstep+(idx-1)*xstep 

         ! If the position is within the signal cut limits:
         do ii=1,3
         if ((xpos.ge.siglo(ii)).and.(xpos.le.sighi(ii))) then
           njpsi(ii)= njpsi(ii) + sig(idx)
           staterr(ii) = staterr(ii) + sigerr(idx)**2 
           back(ii) = back(ii) + bkgd(idx)
         endif
         enddo

         ! Fill the full range background plot:
         bkgh(ibin)=bkgd(idx)
         bkgherr(ibin)=sigerr(idx)

         ! Fill the fit range J/Psi MC:
         jpsi(idx)=plotjpsi(ibin)
         jpsierr(idx)=plotjpsierr(ibin)

      enddo 


      ! Calculate final statistical error within signal region
      do ii=1,3
       staterr(ii)=sqrt(staterr(ii))
      enddo

      !========================================================!
      ! Print signal calculation info to file:
      filename='tables/sigcalc_'//typtag(ityp)//'_'//
     |                            dettag(idet)//'_'//
     |                          ncndtag(incand)//'_'//
     |                          bintag(binnum)//'.tex'
      open(44,file=filename,status='UNKNOWN')
CC      write(44,*) '\\begin{table}[h!]\\centering'
      write(44,*) '{\\large{'
      write(44,*) '\\begin{tabular}{||l r||}'
      write(44,*) '\\hline'
      write(44,*) '\\hline'
66    format(A20,f4.2,'-',f4.2,' \\\\')
77    format(A20,f7.2,' \\\\')
78    format(A20,'\\textbf{',f7.2,'} \\\\')
      do ii=1,3
      write(44,66) 'Signal range &      ',siglo(ii),sighi(ii)
      write(44,78) 'Number of signal &  ',njpsi(ii)
      write(44,77) 'Background &        ',back(ii)
      write(44,77) 'Statistical error & ',staterr(ii)
      write(44,78) 'Significance &      ',
     |                     njpsi(ii)/sqrt(njpsi(ii)+back(ii))
      if(ii.le.2) write(44,*) '\\hline'
      enddo
      write(44,*) '\\hline'
      write(44,*) '\\hline'
      write(44,*) '\\end{tabular}'
CC      write(44,*) '\\caption{Calculated J/Psi signal}'
CC      write(44,*) '\\label{tab-calc-sig}'
      write(44,*) '}}'
CC      write(44,*) '\\end{table}'
      write(44,*) '\\endinput'
      close(44)
      !========================================================!
      ! Print calculated signal to file for J/Psi MC 
      ! normalization in plots:
88    format(F10.3)
      filename='norms/sigcount_'//typtag(ityp)//'_'//
     |                            dettag(idet)//'_'//
     |                          ncndtag(incand)//'_'//
     |                          bintag(binnum)//'.txt'
      open(44,file=filename,status='UNKNOWN')
      do ii=1,3 !--> Give the signal count for the 3 ranges
      write(44,88) njpsi(ii)
      enddo
      close(44)
      !========================================================!


      ! Put signal and errors into output plot:
      call hpak(hidsig,sig)
      call hpake(hidsig,sigerr)
      ! J/Psi MC in fit range:
      call hpak(hidjpsi,jpsi)
      call hpake(hidjpsi,jpsierr)
      ! Full range bkg hist:
      call hpak(hidbkgfullh,bkgh)
      call hpake(hidbkgfullh,bkgherr)

      return
      end

       
