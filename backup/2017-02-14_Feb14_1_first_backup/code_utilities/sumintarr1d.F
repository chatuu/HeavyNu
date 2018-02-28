      integer function sumintarr1d(arrsize,array)
      implicit none

      integer arrsize
      integer array(arrsize)

      integer ii
      integer sums

      sums=0
      do ii=1,arrsize
       sums=sums+array(ii)
      enddo

      sumintarr1d=sums

      return
      end
