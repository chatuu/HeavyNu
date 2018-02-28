      subroutine FindOpenUnit(UnitNum)

      ! This subroutine finds an open (available) file unit number 
      ! and returns it to the given integer.  If no unit numbers are 
      ! open it stops the program.

      integer UnitNum

      integer ii
      logical UnitOpened

      UnitNum=0
      do ii=1,99 !--> Loop over allowable file units 
       if(ii.ne.5.and.ii.ne.6) then  !--> (5&6 reserved by Fortran)
        INQUIRE (UNIT=ii,OPENED=UnitOpened) !--> Check if unit number is already being used
        if ((.not.UnitOpened) .and. UnitNum.eq.0) then
         UnitNum=ii
         goto 99 !--> Available unit found... stop checking
        endif
       endif
      enddo
99    continue

      if(UnitNum.eq.0) then
       print*,''
       print*,''
       print*,'**************************************'
       print*,'********        ERROR         ********'
       print*,'*** No Available File Unit Numbers ***'
       print*,'***       Stopping program!        ***'
       print*,'**************************************'
       print*,''
       print*,''
       print*,'UnitNum: ',UnitNum
       stop
      endif

      return
      end
