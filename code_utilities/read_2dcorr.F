      subroutine read_2dcorr(filename,nbin1,nbin2,Corr)
      implicit none

      character*80 filename
      integer nbin1,nbin2
      double precision Corr(nbin1,nbin2)

      integer ii,jj
      integer length

      integer UN

      length=LEN_TRIM(filename)

      call FindOpenUnit(UN)

      open(UN,file='corr/corrections/'//filename(1:length),status='old')
      do ii=1,nbin1
      do jj=1,nbin2
       read(UN,'(F12.7)') Corr(ii,jj)
      enddo
      enddo
      close(UN)


      return
      end
