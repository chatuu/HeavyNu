      subroutine faildimu(nevtyp,typpass)
      implicit none

      ! This subroutine is called when a DiMuon event fails
      ! to pass a cut. 

      integer nevtyp
      logical typpass(nevtyp)

      integer ii

      do ii=2,3
       typpass(ii)=.false.
      enddo

      return
      end
