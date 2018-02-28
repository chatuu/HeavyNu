      subroutine read_datacard(ntMAX,nfMAX,nhMAX,
     |               datfile,ntyp,nbins,nhist,nfloat, 
     |               nbinmax,filename,mainid,title,outdir,
     |                   ftag,chitit,cohmain,cohadd)
      implicit none

      integer ntMAX,nfMAX,nhMAX
      character*80 datfile
      integer ntyp
      integer nbins(nfMAX)
      integer nhist
      integer nfloat
      integer nbinmax
      character*80 filename(ntMAX,nfMAX)
      integer mainid(nhMAX,nfMAX)
      character*55 title(nhMAX,nfMAX)
      character*45 outdir(nhMAX)
      character*10 ftag(nfMAX)
      character*20 chitit(nfMAX) 
      integer cohmain
      integer cohadd

      character*40 histlist(nhMAX) !--> Histogram names
      character*40 dirnames(nfMAX)  !--> Directory names
      character*3 junk
      integer length
      integer ii,jj

      ! Temporary integers for reading hist ID parameters:
      integer ityp,icut,iext,idet,incand,ibin,ivar
      integer extnum

      ! Functions to get the actual histogram number:
      integer getid


      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ! Read Datacard and get input histogram ID numbers:
      ! (datacard format defined here)
      open(21,file=datfile,status='old')
       !------------------------------------------------
       ! Get list of histogram files:
       read(21,'(a3)') junk
       read(21,'(I2)') ntyp !--> Number of event types.
          if(ntyp.gt.ntMAX) then
          print*,''
          print*,'Error 799388'
          print*,'Number of MC types too large.'
          print*,'Must reduce the number or change arrays'
          print*,'ntyp: ',ntyp
          print*,'ntMAX: ',ntMAX
          stop
          endif
          if(ntyp.lt.2) then
          print*,''
          print*,'Error 22339388'
          print*,'Number of MC types too small.'
          print*,'Require a data and at least one MC.'
          print*,'ntyp: ',ntyp
          stop
          endif
       do ii=1,ntyp
       read(21,'(a40)') histlist(ii) !--> Histogram Filename(s)
       enddo
       !------------------------------------------------
       ! Input directories containing histogram files
       ! for each floated MC component.
       read(21,'(a3)') junk
       read(21,'(a3)') junk
       read(21,'(I2)') nfloat !--> Number of floated MC components.
          if(nfloat.lt.1.or.nfloat.gt.nfMAX) then
          print*,''
          print*,'Error 657748322'
          print*,'Number of floated MC components out of range.'
          print*,'nfloat: ',nfloat
          stop
          endif
       do ii=1,nfloat
       read(21,'(a3)') junk
       read(21,'(a40)') dirnames(ii)
       enddo
       !------------------------------------------------
       ! Indecies of components added to each other (and
       ! normalized together):
       read(21,'(a3)') junk
       read(21,'(a3)') junk
       read(21,'(I2)') cohmain
       read(21,'(a3)') junk
       read(21,'(I2)') cohadd
           if(cohmain.le.1.or.cohmain.gt.nfloat+1) then
           print*,''
           print*,'Error 388823882'
           print*,'Main floated component index out of range.'
           print*,'cohmain: ',cohmain
           print*,'nfloat+1: ',nfloat+1
           stop
           endif
           if(cohadd.eq.cohmain.or.cohadd.le.nfloat+1) then
           print*,''
           print*,'Error 12448484'
           print*,'Added coherent component out of range.'
           print*,'cohadd: ',cohadd
           print*,'cohmain: ',cohmain
           print*,'nfloat+1: ',nfloat+1
           stop
           endif
       !------------------------------------------------
       ! Get output histogram filenames for floated components:
       read(21,'(a3)') junk
       read(21,'(a3)') junk
       do ii=1,nfloat
       read(21,'(a10)') ftag(ii)
       enddo
       !------------------------------------------------
       ! Get chisq histogram titles for floated components:
       read(21,'(a3)') junk
       read(21,'(a3)') junk
       do ii=1,nfloat
       read(21,'(a20)') chitit(ii)
       enddo
       !------------------------------------------------
       ! Get the output directories for histogram sets:
       read(21,'(a3)') junk
       read(21,'(a3)') junk
       read(21,*) nhist !--> Number of histogram seta
          if(nhist.gt.nhMAX.or.nhist.lt.1) then
          print*,''
          print*,'Error 38844003'
          print*,'# of sets of histograms too large (or too small).'
          print*,'Must change array sizes or reduce number.'
          print*,'nhist: ',nhist
          print*,'nhMAX: ',nhMAX
          stop
          endif
       do ii=1,nhist
       read(21,'(a45)') outdir(ii)
       enddo
       !------------------------------------------------
       ! Get plot ID numbers and number of bins:
55     format(I2,2I3,2I2,2I3,a55)
       do jj=1,nfloat
       read(21,'(a3)') junk
       read(21,'(a3)') junk
       read(21,*) nbins(jj) !--> Number of bins in input histograms 
          if(nbins(jj).gt.nbinmax.or.nbins(jj).lt.1) then
            print*,''
            print*,'Error 858399'
            print*,'Histogram bin size too large (or too small).'
            print*,'Must change nbinmax variable.'
            print*,'nbins: ',nbins(jj),jj
            print*,'nbinmax: ',nbinmax
            stop
          endif
       do ii = 1, nhist
        read(21,55) ityp,icut,iext,idet,incand,ibin,ivar,title(ii,jj)
        mainid(ii,jj)=getid(ityp,icut,iext,idet,incand,ibin,ivar)
       enddo
       enddo
      close(21) 
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


      ! Concatenate directory names and filenames:
      do ii=1,ntyp
       do jj=1,nfloat
        length=LEN_TRIM(dirnames(jj)) !Gets length of dirnames without trailing spaces
        filename(ii,jj)= dirnames(jj)(1:length)//histlist(ii)
       enddo
      enddo

      !====================================================================!
      ! Print datacard information:
      open(22,file='output/datacard-info.txt',status='UNKNOWN')
      write(22,*) 'ntMAX: ',ntMAX
      write(22,*) 'ntyp: ',ntyp
      do ii=1,ntyp
      write(22,'(a9,i2,a3,a40)') 'histlist(',ii,'): ',histlist(ii) !--> Histogram Filename(s)
      enddo
      write(22,*) 'nfMAX: ',nfMAX
      write(22,*) 'nfloat: ',nfloat
      do ii=1,nfloat
      write(22,'(a9,i2,a3,a40)') 'dirnames(',ii,'): ',dirnames(ii)
      enddo
      do ii=1,nfloat
      write(22,'(a5,i2,a3,a10)') 'ftag(',ii,'): ',ftag(ii)
      enddo
      do ii=1,nfloat
      write(22,'(a7,i2,a3,a10)') 'chitit(',ii,'): ',chitit(ii)
      enddo
      write(22,*) 'nhMAX: ',nhMAX
      write(22,*) 'nhist: ',nhist
      do ii=1,nhist
      write(22,'(a7,i2,a3,a45)') 'outdir(',ii,'): ',outdir(ii)
      enddo
      write(22,*) 'nbinmax: ',nbinmax
      do jj=1,nfloat
      write(22,'(a6,i2,a3,i4)') 'nbins(',jj,'): ',nbins(jj) !--> Number of bins in input histograms 
      do ii = 1, nhist
      write(22,'(a7,i2,a1,i2,a3,i9)') 'mainid(',ii,',',jj,'): ',
     |                                 mainid(ii,jj)
      write(22,'(a6,i2,a1,i2,a3,a55)') 'title(',ii,',',jj,'): ',
     |                                 title(ii,jj)
      enddo
      enddo
      close(22)
      !====================================================================!

      return
      end
