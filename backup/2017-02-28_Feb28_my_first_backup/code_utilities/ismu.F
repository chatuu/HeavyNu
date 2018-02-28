      logical function ismu(trk)
      implicit none

#include "ntuple.inc"

      ! This subroutine returns ".true." if track "trk"
      ! is a Phase2 muon.

      integer trk !--> Current track number

      ismu=.false.

      ! Chack that "trk" is within range:
      if(trk.le.0 .or. trk.gt.ncand) then
       call printerror(1,999849210,'ismu.F              ')
       print*,'trk out of range: ',trk
       print*,'ncand: ',ncand
       print*,''
       stop
      endif


      ! Check if the track is a muon:
      if(probmu(trk).eq.1 .or.
     |    ((nx1(trk).gt.0.or.nx2(trk).gt.0).and.
     |     (ny1(trk).gt.0.or.ny2(trk).gt.0)) .or.
     |    (isstub(trk).eq.1) )  ismu=.true.


      return
      end
