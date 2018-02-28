      subroutine load_bins(filename,nbin,binnings)
      implicit none

      character*80 filename
      integer nbin
      double precision binnings(nbin+1)

      integer ii
      integer length

      integer UN

      length=LEN_TRIM(filename)

      call FindOpenUnit(UN)
      
      open(UN,file='corr/binnings/'//filename(1:length),status='old')
       do ii=1,nbin+1
        read(UN,'(F15.9)') binnings(ii)
       enddo
      close(UN)


      return
      end
