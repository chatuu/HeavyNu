      subroutine closeevtpic(evtID)
      implicit none

      integer evtID(2)
     
      integer ID



      !---------------------------------------
      ! Print footer to summary info. table:
      ID=evtID(2)
      write(ID,*) '\\hline'
      write(ID,*) '\\end{tabular}'
      write(ID,*) '}'
      ! Close it:
      close(ID)


      return
      end
