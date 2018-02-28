      subroutine findzsec(posz,zmin,zsec)
      implicit none

      ! This subroutine takes a vertex z-position and returns the
      ! section of the detector the event lies within. 

      double precision posz !--> vertex z-position
      double precision zmin
      integer zsec !--> Gives the section of the detector in which the event
                   !    vertex lies.
                   !   1 = DC (zmin to 405)
                   !   2 = Other (-24 to zmin)
                   !   3 = Coil (-50 to -24)
                   !   4 = Upstream (-70 to -50)

      double precision seclimits(5) !--> Section limits

      integer ii !--> Do loop variable


      ! Set section limits:
      seclimits(1)= 405.0D0
      seclimits(2)= zmin
      seclimits(3)= -24.0D0
      seclimits(4)= -50.0D0
      seclimits(5)= -70.0D0


      ! Initialize zsec:
      zsec=-999

      ! Find the current section number:
      do ii=1,4 !--> Loop over the sections
       if(  posz.le.seclimits(ii) .and. 
     |      posz.gt.seclimits(ii+1) ) then
        zsec=ii
       endif
      enddo


      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      ! Check for errors
      if(zsec.eq.-999.and.posz.ge.-70.0D0.and.posz.le.405.0D0) then
       call printerror(2,555588291,'findzsec.F          ')   
       print*,'No section found in the detector!'
       print*,'zsec: ',zsec
       print*,'posz: ',posz
       print*,'zmin: ',zmin
       print*,''
      elseif(zsec.eq.-999.and.(posz.lt.-70.0D0.or.posz.gt.405.0D0)) then
        continue
      elseif(zsec.lt.1.or.zsec.gt.4) then
       call printerror(2,555588291,'findzsec.F          ')   
       print*,'Error in zsec!'
       print*,'zsec: ',zsec
       print*,'posz: ',posz
       print*,'zmin: ',zmin
       print*,''
       stop
      endif
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!



      return
      end
