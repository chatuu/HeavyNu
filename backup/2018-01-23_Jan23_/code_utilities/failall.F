      subroutine failall(nevtyp,typpass)
      implicit none

      ! This subroutine is called when an event fails
      ! to pass a cut applied to all event classifications.

      integer nevtyp
      logical typpass(nevtyp)

      integer ii

      do ii=1,nevtyp
       typpass(ii)=.false.
      enddo

      return
      end
