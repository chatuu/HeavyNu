      subroutine countevent(cnum,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,
     |       ndetsec,cuts,typpass,wt,iCorr,renorm,DScorr,
     |       extyp,check,ncndsw,detsw)
      implicit none

      ! This subroutine simply adds the event to the cut count arrray

      integer cnum !--> Cut Number
      integer ncuts !--> Total number of cuts
      integer nmc !--> Number of MC components
      integer nttyp !--> Component number
      integer nevtyp
      integer nextyp
      integer nncand
      integer ndetsec
      double precision cuts(3,ncuts,0:nmc,nevtyp,nextyp,nncand,ndetsec) !--> Cut array
      logical typpass(nevtyp) !--> If type has passed cuts so far
      double precision wt !--> Event weight
      integer iCorr !--> Index of corrected MC
      double precision renorm !--> DS correction renormalization factor
      double precision DScorr !--> DS shape correction factor
      integer extyp
      integer check(extyp)
      integer ncndsw(nncand)
      integer detsw(ndetsec)

      integer ii,jj,kk,ll !--> Do Loop Variables

      integer getcheckid

      integer idx
      integer idxdet
      integer idxncnd


      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      ! Error Checking:                                        !
      if(ncuts.lt.1) then                                      !
       call printerror(1,333291873,'countevent.F         ')    !
       print*,'ncuts out of range: ',ncuts                     !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(cnum.lt.1.or.cnum.gt.ncuts) then                      !
       call printerror(1,486610498,'countevent.F        ')     !
       print*,'cnum out of range: ',cnum                       !
       print*,'ncuts: ',ncuts                                  !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
c      do ii=1,3                                                !
c       do jj=1,ncuts                                           !
c        do kk=0,nmc                                            !
c         do ll=1,nevtyp                                        !
c         if(cuts(ii,jj,kk,ll).lt.0.0D0) then                   !
c          call printerror(1,887388749,'countevent.F        ')  !
c          print*,'cuts out of range!'                          !
c          print*,'i,j,k,l,cuts(i,j,k,l): ',ii,' ',jj,' ',      !
c     |                      kk,' ',ll,' ',cuts(ii,jj,kk,ll)    !         
c          print*,''                                            !
c          stop                                                 !
c         endif                                                 !
c         enddo                                                 !
c        enddo                                                  !
c       enddo                                                   !
c      enddo                                                    !
      if(nmc.lt.1) then                                        !
       call printerror(1,220119465,'countevent.F        ')     !
       print*,'nmc out of range: ',nmc                         !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(nttyp.lt.0.or.nttyp.gt.nmc) then                      !
       call printerror(1,955488210,'countevent.F        ')     !
       print*,'nttyp out of range: ',nttyp                     !
       print*,'nmc: ',nmc                                      ! 
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(wt.lt.0.0D0.or.(nttyp.eq.0.and.wt.ne.1.0D0)) then     !
       call printerror(1,776477361,'countevent.F        ')     !
       print*,'wt out of range: ',wt                           !
       print*,'nttyp: ',nttyp                                  !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(iCorr.lt.1.or.iCorr.gt.nmc) then                      !
       call printerror(1,756109835,'countevent.F        ')     !
       print*,'iCorr out of range: ',iCorr                     !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(renorm.lt.0.0D0 .or.                                  !
     |       (nttyp.ne.iCorr.and.renorm.ne.1.0D0)) then        !
       call printerror(1,112000934,'countevent.F        ')     !
       print*,'renorm out of range: ',renorm                   !
       print*,'nttyp: ',nttyp                                  !
       print*,'iCorr: ',iCorr                                  !
       print*,'cnum:  ',cnum
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(DScorr.lt.0.0D0 .or.                                  !
     |       (nttyp.ne.iCorr.and.DScorr.ne.1.0D0)) then        !
       call printerror(1,994870106,'countevent.F        ')     !
       print*,'DScorr out of range: ',DScorr                   !
       print*,'nttyp: ',nttyp                                  !
       print*,'iCorr: ',iCorr                                  !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!

      do ii=1,nevtyp !--> Loop over event classifications:
       if (typpass(ii)) then
       do jj=1,nextyp
        idxncnd=0
        do kk=1,nncand
        if(ncndsw(kk).eq.1) then
          idxncnd=idxncnd+1
          idxdet=0
          do ll=1,ndetsec
          if(detsw(ll).eq.1) then
            idxdet=idxdet+1

            idx=getcheckid(jj,idxdet,idxncnd)
            if(check(idx).eq.1) then
        
        cuts(1,cnum,nttyp,ii,jj,kk,ll)= 
     |  cuts(1,cnum,nttyp,ii,jj,kk,ll)  + 1.0D0
        cuts(2,cnum,nttyp,ii,jj,kk,ll)= 
     |  cuts(2,cnum,nttyp,ii,jj,kk,ll)  + wt
        cuts(3,cnum,nttyp,ii,jj,kk,ll)= 
     |  cuts(3,cnum,nttyp,ii,jj,kk,ll)  + wt*renorm*DScorr
            endif
          endif
          enddo
        endif
        enddo
       enddo
       endif
      enddo

      return
      end
