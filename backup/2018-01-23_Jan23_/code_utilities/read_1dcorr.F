      subroutine read_1dcorr(filename,nbin,Corr)
      implicit none

      character*80 filename
      integer nbin
      double precision Corr(nbin)

      integer ii
      integer length
      
      integer UN

      length=LEN_TRIM(filename)

      call FindOpenUnit(UN)

      open(UN,file='corr/corrections/'//filename(1:length),status='old')
      do ii=1,nbin
       read(UN,'(F12.7)') Corr(ii)
      enddo
      close(UN)


      return
      end
