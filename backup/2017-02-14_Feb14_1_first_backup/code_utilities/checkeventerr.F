      subroutine checkeventerr(id,eventnum,err)
      implicit none

      integer id,eventnum,err

      call hgnt(id,eventnum,err)
      if (err.ne.0) then
       call printerror(2,92883174,'checkeventerr.F     ')
       print*,'Error reading ntuple event #',eventnum
       print*,'err: ',err
       print*,''
      endif

      return
      end
