      subroutine closentuple(ntID)
      implicit none

      ! This subroutine simply closes an ntuple opened by
      ! "openntuple.F".  The ntuple is assumed to have the name
      ! "INTUPLE" (given in the HROPEN call).

      integer ntID

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      ! Check input for error:                                     !
      !                                                            !
       if(ntID.eq.5.or.ntID.eq.6.or.ntID.lt.1.or.ntID.gt.99) then  !
        call printerror(1,775644371,'closentuple.F       ')        !
        print*,'Ntuple Unit number out of range.'                  !
        print*,'ntID: ',ntID                                       !
        print*,''                                                  ! 
        stop                                                       ! 
       endif                                                       !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!

          call hrend('INTUPLE')
          close(ntID)

      return
      end
