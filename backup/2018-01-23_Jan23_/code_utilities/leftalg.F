      subroutine leftalg(string)
      implicit none

      ! Left-align the given character string

      character*10 string

      integer ii

      integer idx

      ! Find the start of the numbers
      idx=0
      do ii=1,10
       if(string(ii:ii).ne." ") then
        idx=ii
        goto 11
       endif
      enddo
11    continue !--> Found start of string
      if(idx.eq.0) then
       call printerror(2,995505832,'leftalg.F           ')
       print*,'String is blank!'
       print*,''
      endif

      ! Start from the given index and shift all characters to the left
      do ii=idx,10
       string(ii-idx+1:ii-idx+1)=string(ii:ii)
      enddo

      ! Now fill the rest of the string with blanks:
      do ii=1,idx-1 !--> Loop over the number of blank spaces
       string(10-ii+1:10-ii+1)=" "
      enddo

      return
      end
