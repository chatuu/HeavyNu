      program main
      implicit none     

      ! This program simply calculates the number of signal J/Psi events
      ! by subtracting a fit of the background from the data.
      ! Plots are made giving the signal calculations in output.h.

      ! Set the cut number to use:
      integer cnum
      parameter (cnum=29)

      ! Number of binning types:
      integer nbtyp
      parameter (nbtyp=2)

      !--------------------------------------------------!
      ! Fitting parameters:
      ! Number of bins in main plot 
      integer nbin(nbtyp)
      data nbin/46,69/
      ! Lowest bin limit of main plot:
      real lowbinlim(nbtyp)
      data lowbinlim/0.021916,0.046916/
      ! The bin size:
      real xstep(nbtyp)
      data xstep/0.15,0.1/
      !-----------------
      ! Lower limit of fit (fit will start in the next closest full bin):
      real fitlowlim
      parameter (fitlowlim=1.0)
      ! Upper limit of fit (fit will end in the closest previous full bin):
      real fituplim
      parameter (fituplim=7.0)
      !-----------------
      ! Ranges used to calculate signal numbers:
      ! (it is preferable that these fall on/near bin
      !  limits of the histogram, and not near the center
      !  of the bin, else one may end up with a signal range
      !  that is not symmetric about the J/Psi mass)
c     1)   2.75-3.45
c     2)   2.85-3.35
c     3)   2.95-3.25
      real siglo(3,nbtyp) 
      data siglo/2.721916,2.871916,3.021916, !--> For binning #1 (150MeV)
     |           2.746916,2.846916,2.946916/ !--> For binning #2 (100MeV) 
      real sighi(3,nbtyp) 
      data sighi/3.471916,3.321916,3.171916, !--> For binning #1 (150MeV)
     |           3.446916,3.346916,3.246916/ !--> For binning #2 (100MeV) 
      !--------------------------------------------------!

      integer ibin,ii !--> do loop variable(s)
      real tmplo(3)
      real tmphi(3)

      ! Binning labels:
      character*6 bintag(nbtyp)

      ! Set binninb labels:
      bintag(1)='150mev'
      bintag(2)='100mev'


      ! Loop over binnings:
      do ibin=1,nbtyp

        do ii=1,3
         tmplo(ii)=siglo(ii,ibin)
         tmphi(ii)=sighi(ii,ibin)
        enddo

      call process(cnum,ibin,nbin(ibin),lowbinlim(ibin),xstep(ibin),
     |             fitlowlim,fituplim,tmplo,tmphi,nbtyp,bintag)

      enddo




      print*,' '
      print*,' Done calculating signal'
      print*,' '

      stop
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine process(cnum,binnum,nbin,lowbinlim,xstep,
     |               fitlowlim,fituplim,siglo,sighi,nbtyp,bintag)

      integer cnum
      integer binnum
      integer nbin
      real lowbinlim
      real xstep
      real fitlowlim
      real fituplim
      real siglo(3) 
      real sighi(3) 
      integer nbtyp
      character*6 bintag(nbtyp)

      ! Paw variables:
      integer nhbook
      parameter (nhbook=3000000)
      real paw
      common / pawc / paw(nhbook)
      integer istat,icycle
  
      integer ii,ityp,idet,incand

      real par(5)

      character*6 typtag(0:3)
      character*4 dettag(0:3) 
      character*6 ncndtag(2:5)
      character*80 filename

      ! Function to get histogram ID:
      integer getid

      integer hid !--> Full histogram ID
      integer hidbkg !--> Mass Blind histogram ID
      integer hidsig !--> Calculated signal histogram
      integer hidjpsi !--> J/Psi MC mass plot in the fit range
      integer hidbkgfullh !--> Histogram version of the background fit with the range of
                          !    the full input plot.  Values outside of the fit range are set to 0.
      integer jpsiid !--> ID of the J/Psi plot to use from the analysis
                     !    (want a DC ncand=2 plot for J/Psi, as using the coil cuts, for
                     !     example, will produce no J/Psi MC)
      integer shiftid



      ! First bin of the fit:
      integer binbegin
      ! Last bin of the fit :
      integer binend
      ! Number of bins in the fit (binend-binbegin+1):
      integer nfitbin
      ! The lower x-position of the fit:
      real xbegin
      ! The upper x-position of the fit:
      real xend

      real tmplow,tmpup

      ! J/Psi MC full-range plot:
      real plotjpsi(nbin)
      real plotjpsierr(nbin)

      !ooooooooooooooooooooooooooooooooooooooo!
      ! File tags:
      typtag(0)='numucc'
      typtag(1)='osdimu'
      typtag(2)='lsdimu'
      typtag(3)='osmupl'
      !---------
      dettag(0)='drft'
      dettag(1)='othr'
      dettag(2)='coil'
      dettag(3)='upst'
      !---------
      ncndtag(2)='ncand2'
      ncndtag(3)='ncand3'
      ncndtag(4)='ncand4'
      ncndtag(5)='ncnd34'
      !ooooooooooooooooooooooooooooooooooooooo!


      !================================================!
      ! Make calculations based on input histogram
      ! properties and fit range:
      binbegin=0
      binend=0
      tmplow=lowbinlim
      tmpup=tmplow+xstep
      do ii=1,nbin !--> loop over bins of full histogram
       if(ii.ne.1) tmplow=tmplow+xstep
       if(ii.ne.1) tmpup =tmpup +xstep
       if(fitlowlim.lt.tmplow.and.binbegin.eq.0) binbegin=ii
       if(fituplim.gt.tmpup.and.fituplim.lt.tmpup+xstep) binend=ii
      enddo
      if(binbegin.eq.0.or.binend.eq.0) then
        print*,''
        print*,'Error in J/Psi Calculator!'
        print*,'Something is wrong with fit binning calculations.'
        print*,'binbegin: ',binbegin
        print*,'binend: ',binend
        print*,'STOPPING!'
        print*,''
        stop
      endif
      nfitbin=binend-binbegin+1
      xbegin=lowbinlim+xstep*real(binbegin-1)
      xend=xbegin+real(nfitbin)*xstep 
      !================================================!


      call HLIMIT(nhbook)

      filename='output_'//bintag(binnum)//'.h'
      call HROPEN(80,'OUTPUT',filename,'N',1024,istat)
      call HCDIR('//OUTPUT',' ')

      ! Book output histograms:
      do ityp=0,3
      do idet=0,3
      do incand=2,5
        ! Calculated signal histograms:
        hidsig=500 + ityp*100 + 10*idet + incand
        call hbook1(hidsig,'J/Psi Signal',nfitbin,xbegin,xend,0.0)
        ! Histogram of background fit:
        hidjpsi=500 + ityp*100 + 10*idet + incand + 4
        call hbook1(hidjpsi,'J/Psi MC',nfitbin,xbegin,xend,0.0)
        ! Bkg hist. fit in full plot range:
        hidbkgfullh=100 + ityp*100 + 10*idet + incand + 4
        call hbook1(hidbkgfullh,'Background fit',nbin,lowbinlim,
     |                  lowbinlim+nbin*xstep,0.0)
      enddo 
      enddo 
      enddo 

      call hidopt(0,'stat') ! Option to correctly calculate errors  (must come BEFORE the call to hbarx)
      call hbarx(0) !Puts errors into memory and superimposes them onto plots.  Must be called after booking but before filling

      ! Open intput file:
      call HROPEN(50,'INPUT','../cohjpsi-mumu_data.h',' ',1024,istat)
      call HCDIR('//INPUT',' ')


      do ityp=0,3 !--> Loop over event types
      do idet=0,3 !--> Loop over detector sections
      do incand=2,5 !--> Loop over ncand types

        !===========================================!
        ! Get histogram ID numbers
        ! Full plot:
        hid=getid(ityp,cnum,1,idet,incand,binnum,30)
        ! Background plot:
        hidbkg=getid(ityp,cnum,2,idet,incand,binnum,30)
        ! Signal plot (calculated here):
        hidsig=500 + ityp*100 + 10*idet + incand
        ! Histogram version of background fit:
        hidjpsi=500 + ityp*100 + 10*idet + incand + 4
        ! Bkg fit histogram in full plot range:
        hidbkgfullh=100 + ityp*100 + 10*idet + incand + 4
        ! Source J/Psi MC histogram:
        jpsiid=getid(1,cnum,1,0,2,binnum,30)
CC        jpsiid=hid !--> If want cuts used in each sample
        !===========================================!

        if(hid.ne.0.and.hidbkg.ne.0) then

        ! Get the full J/Psi MC plot:
        call HROPEN(52,'JPSI','../cohjpsi-mumu_jpsi.h',' ',1024,istat)
        call HCDIR('//JPSI',' ')
        shiftid=1000000 + 1000000*ityp + 100000*idet + 10000*incand 
        call HRIN(jpsiid,9999,shiftid)
        call hunpak(jpsiid+shiftid,plotjpsi,' ',0)
        call hunpke(jpsiid+shiftid,plotjpsierr,' ',0)
        call hrend('JPSI')
        close(52)
        call HCDIR('//INPUT',' ')


        !-------------------------------------!
        ! Load the fit parameters:
33      format(F15.7)
        filename='par/par_'//typtag(ityp)//'_'//
     |                       dettag(idet)//'_'//
     |                       ncndtag(incand)//'_'//
     |                       bintag(binnum)//'.txt'
        open(22,file=filename,status='OLD')
        do ii=1,5
        read(22,33) par(ii)
        enddo
        close(22)
        !-------------------------------------!

        ! Find the number of signal:
        call calcjpsi(hid,hidbkg,hidsig,par,
     |                hidjpsi,hidbkgfullh,
     |                ityp,idet,incand,
     |                typtag,dettag,ncndtag,
     |                    nbin,nfitbin,binbegin,binend,
     |                    xbegin,xstep,siglo,sighi,
     |                    plotjpsi,plotjpsierr,
     |                    nbtyp,bintag,binnum)

        endif


      enddo
      enddo
      enddo

      ! Close input histogram file:
      call hrend('INPUT')
      close(50)

      call HCDIR('//OUTPUT',' ') 
      call HROUT(0,icycle,' ') ! Write all memmory resident histograms to current directory
      call HREND('OUTPUT') ! Close output histogram file
      close (80) ! Obligatory Fortran close

     
      return
      end
