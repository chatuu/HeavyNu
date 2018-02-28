      subroutine chi(ntMAX,nfMAX,nhMAX,itMAX,
     | ntyp,nhist,nfloat,iteration,nbinmax,nbins,
     | nstep,istep,useOverflow,useMCerror,
     | filename,mainid,outdir,ftag,chitit,
     | norms)
      implicit none
 
      ! Input/Output: 
      integer ntMAX,nfMAX,nhMAX,itMAX
      integer ntyp
      integer nhist
      integer nfloat
      integer iteration
      integer nbinmax
      integer nbins(nfMAX)
      integer nstep
      double precision istep
      integer useOverflow
      integer useMCerror
      character*80 filename(ntMAX,nfMAX)
      integer mainid(nhMAX,nfMAX)
      character*45 outdir(nhMAX) 
      character*10 ftag(nfMAX) 
      character*20 chitit(nfMAX) 
      double precision norms(nfMAX,nhMAX,0:itMAX,0:4)

      ! Paw Declarations:
      integer nhbook
      parameter (nhbook=300000000)
      real paw
      common / pawc / paw(nhbook)
      integer istat,icycle
      real HI,HIE

      ! Do loop variables
      integer ii,jj,kk 
      integer ihist,ityp
      integer ifloat
      integer inorm

      ! Contents of histogram bins:
      double precision histvec(ntyp,nfloat,nbinmax)
      double precision histerr(ntyp,nfloat,nbinmax)

      ! Chisq. array 
      double precision chisq(nstep)
      real chisqr(nstep) !--> Input to Hbook call

      integer hid(ntyp,nhist,nfloat) !--> Histogram IDs

      integer nzerobins(nfloat) !--> Count of bins with no data events with

      integer itadd(nfloat) !--> Index addition for normalizations 

      double precision tnorm(2:ntyp) !--> Temporary normalizations

      ! Temporary values:
      double precision MaxNorm,MinNorm
      double precision norm
      double precision minchi
      double precision chisqtmp
      double precision errortmp
      double precision sigma
      real tmpvec(nbinmax)

      character*80 hfile
      integer length,len2





      call HLIMIT(nhbook)



      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!
       ! Add constant to mainid for each component type so
       ! that histogram ID's do not overlap in memory.
       do ifloat=1,nfloat
       do ihist=1,nhist
        do ityp=1,ntyp
         hid(ityp,ihist,ifloat) =  10000*ifloat +
     |                               100*ityp +
     |                                   ihist
        enddo
       enddo
       enddo
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!


      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
      ! Open input hbook file and get histograms into memory
      do ifloat=1,nfloat
      do ityp=1,ntyp
      call hropen(1,'input',filename(ityp,ifloat),' ',1024,istat)
      call hcdir('//input',' ')
       do ihist=1,nhist
        call hrin(mainid(ihist,ifloat),9999,
     |                             10000*ifloat+
     |                               100*ityp  +
     |                                   ihist -
     |                        mainid(ihist,ifloat))
       enddo
      call hrend('input')
      close(1)
      enddo
      enddo
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

      !|||||||||||||||||||||||||||||||||||||||||||||||||||!
      !+++++++++++++++++++++++++++++++++++++++++++++++++++!
      !                                                   !
      do ihist = 1, nhist !--> Loop over histogram sets   !
      !                                                   !
      !+++++++++++++++++++++++++++++++++++++++++++++++++++!
      !|||||||||||||||||||||||||||||||||||||||||||||||||||!


      !------------------------------------------------
      ! Dump histogram contents into "histvec" array:         
        do ityp=1,ntyp
        do ifloat=1,nfloat

c      ! Read histogram contents into temporary vector:
      call hunpak(hid(ityp,ihist,ifloat),tmpvec,' ',0) 


      ! Put temporary vector into output array:
      do ii=1,nbins(ifloat)
       histvec(ityp,ifloat,ii) = dble(tmpvec(ii))
      enddo

        enddo
        enddo
      !------------------------------------------------
      ! Dump histogram errors into "histerr" array:        
        ! Data
        do ifloat=1,nfloat
        do ii=1,nbins(ifloat)
        histerr(1,ifloat,ii) = dsqrt(histvec(1,ifloat,ii))
        enddo
        enddo

        ! MC
        do ityp=2,ntyp
        do ifloat=1,nfloat

      ! Read histogram contents into temporary vector:
      call hunpke(hid(ityp,ihist,ifloat),tmpvec,' ',0) 

      ! Put temporary vector into output array:
      do ii=1,nbins(ifloat)
       histerr(ityp,ifloat,ii) = dble(tmpvec(ii))
      enddo

        enddo
        enddo
      !------------------------------------------------
      ! Adding overflow/underflow to last/first bin if desired:
       if (useOverflow.eq.1) then 
        do ifloat=1,nfloat
        do ityp=1,ntyp
         ! Overflow:
         histvec(ityp,ifloat,nbins(ifloat)) = 
     |   histvec(ityp,ifloat,nbins(ifloat)) +
     |  dble(HI(hid(ityp,ihist,ifloat),nbins(ifloat)+1))
         histerr(ityp,ifloat,nbins(ifloat)) =  
     |    dsqrt(histerr(ityp,ifloat,nbins(ifloat))**2 + 
     | dble(HIE(hid(ityp,ihist,ifloat),nbins(ifloat)+1))**2)
         ! Underflow:
         histvec(ityp,ifloat,1) = 
     |   histvec(ityp,ifloat,1) +
     |  dble(HI(hid(ityp,ihist,ifloat),0))
         histerr(ityp,ifloat,1) =  
     |    dsqrt(histerr(ityp,ifloat,1)**2 + 
     | dble(HIE(hid(ityp,ihist,ifloat),0))**2)
        enddo
        enddo
       endif
      !------------------------------------------------


      !=========================================!
      !       Data Bin Value Checks             !    
      length=LEN_TRIM(outdir(ihist))            !
      hfile = 'output/'//outdir(ihist)(1:length)!
     |        //'/warning.txt'                  !
      open(55,file=hfile,status='UNKNOWN')      !
      do ifloat=1,nfloat                        !
      nzerobins(ifloat) = 0                     !
      do ii=1,nbins(ifloat)                     !
       if(histvec(1,ifloat,ii).eq.0.0D0) then   !
        write(55,*)''                           !
        write(55,*)'!!! Warning !!!'            !
        write(55,*)'Data in Bin is Zero'        !
        write(55,*)'Not using Bin Number: ',ii  !
        write(55,*)'Variable Number: ',ihist    !
        write(55,*)'Float type: ',ifloat        !
        write(55,*)'HistID: ',                  !
     |              mainid(ihist,ifloat)        !
        write(55,*)''                           !
        nzerobins(ifloat)=                      !
     |  nzerobins(ifloat) + 1                   !
       endif                                    !
      enddo                                     !
      enddo                                     !
      close(55)                                 !
      !=========================================!


      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      ! Set the additive index:
      do ifloat=1,nfloat
       itadd(ifloat)=1
      enddo

      ! Prepare temporary MC normalizations:
      do ityp=2,ntyp
       tnorm(ityp)=1.0D0
      enddo
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!


      do ifloat=1,nfloat !--> Loop over floated MC compnents.


      !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      ! Set current Norm Factors for floated components:
      do ii=1,nfloat
       tnorm(ii+1)=norms(ii,ihist,iteration-itadd(ii),0)
      enddo
      itadd(ifloat)=0 !--> On the next ifloat iteration use
                      !    the current normalization value, 
                      !    rather than the previous.
      !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


      !------------------------------------------------------------------!
      !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!  
      ! Compute Chi^2 array:
      minchi=99999999999999.0D0 !--> Initialize chisq-minimum variable
      do inorm = 1, nstep !--> Loop over scalings (DL-01)

        !=====================================================!
        ! Set temporary floating normalization:               !
          norm = dble(inorm-1)*istep                          !
          tnorm(ifloat+1)=norm !--> modify current float norm !
        !=====================================================!
           

      chisq(inorm)=0.0D0
      do ii=1,nbins(ifloat) !--> Loop over bins
       !-------------------------------
       ! Subtract MC from data
       chisqtmp= histvec(1,ifloat,ii)  
       do ityp=2,ntyp 
        chisqtmp=chisqtmp-
     |           histvec(ityp,ifloat,ii)*tnorm(ityp)
       enddo
       !-------------------------------
       ! Calculate Errors
       errortmp = dsqrt(histerr(1,ifloat,ii)**2)
       if(useMCerror.eq.1) then
        do ityp=2,ntyp
        errortmp = dsqrt( errortmp**2 + 
     |              (histerr(ityp,ifloat,ii)*tnorm(ityp))**2)
        enddo
       endif
       !-------------------------------
       ! Sum Chisq. for each bin
       if(histvec(1,ifloat,ii).gt.0.0D0) 
     |  chisq(inorm) = chisq(inorm) + (chisqtmp**2 / errortmp**2) 
       !-------------------------------
      enddo !--> Loop over bins

        ! Look for Chi^2 minimum:            
        if(chisq(inorm).lt.minchi) then
          minchi=chisq(inorm)
          norms(ifloat,ihist,iteration,0)=norm
        endif

      enddo !--> Loop over normalizations
      !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!  
      !------------------------------------------------------------------!


      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
      ! Find deviation of the Chi^2 Minimum:
       sigma=dsqrt(minchi/dble(nbins(ifloat)-nzerobins(ifloat)))
       MaxNorm =-999999999.0D0
       MinNorm = 999999999.0D0
       do inorm=1,nstep
          norm = dble(inorm-1)*istep      
          if(dabs((chisq(inorm)-minchi)).lt.sigma) then
             if(norm.lt.MinNorm) MinNorm=norm
             if(norm.gt.MaxNorm) MaxNorm=norm
          endif
       enddo
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

      ![][][][][][][][][][][][][][][][][][][][][][][]!
      ! Set Output Values:
       norms(ifloat,ihist,iteration,1) = MaxNorm 
       norms(ifloat,ihist,iteration,2) = MinNorm 
       norms(ifloat,ihist,iteration,3) = minchi
       norms(ifloat,ihist,iteration,4) =
     |      dble(nbins(ifloat)-nzerobins(ifloat))
      ![][][][][][][][][][][][][][][][][][][][][][][]!


      !000000000000000000000000000000000000000000000000000000000000000
      ! Make output Histogram:

      ! Fill real array:
      do ii=1,nstep
       chisqr(ii)=real(chisq(ii))
      enddo

      ! Set Filename
      length=LEN_TRIM(outdir(ihist)) 
      len2=LEN_TRIM(ftag(ifloat))
      hfile = 'output/'//outdir(ihist)(1:length)//
     |        '/chisq_'//ftag(ifloat)(1:len2)//'.h'
      ! Open output histogram file:
      call hropen(60,'OUTPUT',hfile,'N',1024,istat)
      call hcdir('//OUTPUT',' ')
      ! Book Histogram: 
      len2=LEN_TRIM(chitit(ifloat))
      call hbook1(101,'Chisq. For '//chitit(ifloat)(1:len2),
     |                      nstep,0.0,3.0,0.0)
      ! Pack Histogram
      call hpak(101,chisqr)
      ! Close Output Histograms
      call HCDIR('//OUTPUT',' ')
      call hrout(0,icycle, ' ')
      call hrend('OUTPUT')
      call hdelet(101)
      close(60)
      !000000000000000000000000000000000000000000000000000000000000000



      enddo !--> Loop over floated components

      !|||||||||||||||||||||||||||||||||||||||||||||||||||!
      !+++++++++++++++++++++++++++++++++++++++++++++++++++!
      !                                                   !
       enddo !--> Loop over ihist                         ! 
      !                                                   !
      !+++++++++++++++++++++++++++++++++++++++++++++++++++!
      !|||||||||||||||||||||||||||||||||||||||||||||||||||!
 
      
      return
      end
