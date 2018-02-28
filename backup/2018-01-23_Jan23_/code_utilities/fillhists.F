      subroutine fillhists(cnum,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |                  cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                  iCorr,renorm,DScorr,evtsw,idxcut)
      implicit none

      integer cnum !--> Cut Number
      integer nmc
      integer nvar
      integer ncuts
      integer extyp
      integer nttyp
      integer nevtyp
      integer check(extyp)
      integer cuthists(ncuts)
      integer nbintyp(nvar)
      logical typpass(nevtyp)
      double precision KinVar(nvar)
      double precision norm(0:1,0:nmc)
      double precision wt
      integer iCorr
      double precision renorm
      double precision DScorr
      integer evtsw(nevtyp)
      integer idxcut

      integer ii,jj,kk,tt !--> Do Loop Variables
      integer hid !--> Histogram ID Number
      integer gethistid !--> Function to get sequential histogram ID
      integer idxevt

 
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      ! Error Checking:                                        !
      if(nmc.lt.1) then                                        !
       call printerror(1,333849123,'fillhists.F         ')     !
       print*,'nmc out of range: ',nmc                         !
       print*,'cnum: ',cnum                                    !
       print*,''                                               !
       stop                                                    ! 
      endif                                                    !
      if(ncuts.lt.1) then                                      !
       call printerror(1,222938761,'fillhists.F         ')     !
       print*,'ncuts out of range: ',ncuts                     !
       print*,'cnum: ',cnum                                    !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(nvar.lt.1.or.nvar.gt.99) then                         !
       call printerror(1,885744691,'fillhists.F         ')     !
       print*,'nvar out of range: ',nvar                       !
       print*,'Must modify histogram booking for nvar>99'      !
       print*,'cnum: ',cnum                                    !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(extyp.lt.1) then                       !
       call printerror(1,665478210,'fillhists.F         ')     !
       print*,'extyp out of range: ',extyp                     !
       print*,'cnum: ',cnum                                    !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(nttyp.lt.0.or.nttyp.gt.nmc) then                      !
       call printerror(1,484957321,'fillhists.F         ')     !
       print*,'nttyp out of range: ',nttyp                     !
       print*,'cnum: ',cnum                                    !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(cnum.lt.1.or.cnum.gt.ncuts) then                      !
       call printerror(1,129783654,'fillhists.F         ')     !
       print*,'cnum out of range: ',cnum                       !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      do ii=1,ncuts                                            !
       if(cuthists(ii).ne.0.and.cuthists(ii).ne.1) then        !
        call printerror(1,456754328,'fillhists.F         ')    !
        print*,'cuthists out of range!'                        !
        print*,'ii, cuthists(ii): ',ii,' ',cuthists(ii)        !
        print*,'cnum: ',cnum                                   !
        print*,''                                              !
        stop                                                   !
       endif                                                   !
      enddo                                                    !
      do ii=1,extyp                                            !
       if(check(ii).ne.0.and.check(ii).ne.1) then              !
        call printerror(1,805736132,'fillhists.F         ')    !
        print*,'check out of range!'                           !
        print*,'ii,check(ii): ',ii,' ',check(ii)               !
        print*,'cnum: ',cnum                                   !
        print*,''                                              !
        stop                                                   !
       endif                                                   !
      enddo                                                    !
      do ii=1,nvar                                             !
       if(nbintyp(ii).lt.1) then                               !
        call printerror(1,904873621,'fillhists.F         ')    !
        print*,'ntinbyp out of range!'                         !
        print*,'ii,nbintyp(ii): ',ii,' ',nbintyp(ii)           !
        print*,'cnum: ',cnum                                   !
        print*,''                                              !
        stop                                                   !
       endif                                                   !
      enddo                                                    !
      if(wt.lt.0.0D0.or.(nttyp.eq.0.and.wt.ne.1.0D0)) then     !
       call printerror(1,384761057,'fillhists.F         ')     !
       print*,'wt out of range: ',wt                           !
       print*,'nttyp: ',nttyp                                  !
       print*,'cnum: ',cnum                                    !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      do ii=0,1                                                !
       do jj=0,nmc                                             !
        if(norm(ii,jj).lt.0D0) then                            !
         call printerror(1,668577401,'fillhists.F         ')   !
         print*,'norm out of range!'                           !
         print*,'ii,jj,norm(ii,jj): ',ii,' ',jj,' ',norm(ii,jj)!
         print*,'cnum: ',cnum                                  !
         print*,''                                             !
         stop                                                  !
        endif                                                  !
       enddo                                                   !
      enddo                                                    !
      if(norm(0,0).ne.1.0D0.or.norm(1,0).ne.1.0D0) then        !
       call printerror(1,283775647,'fillhists.F         ')     !
       print*,'Data norm not 1.0!: ',norm(0,0),' ',norm(1,0)   !
       print*,'cnum: ',cnum                                    !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(iCorr.lt.1.or.iCorr.gt.nmc) then                      !
       call printerror(1,993887110,'fillhists.F         ')     !
       print*,'iCorr out of range: ',iCorr                     !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(renorm.lt.0.0D0 .or.                                  !
     |       (nttyp.ne.iCorr.and.renorm.ne.1.0D0)) then        !
       call printerror(1,888574661,'fillhists.F         ')     !
       print*,'renorm out of range: ',renorm                   !
       print*,'nttyp: ',nttyp                                  !
       print*,'iCorr: ',iCorr                                  !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(DScorr.lt.0.0D0 .or.                                  !
     |       (nttyp.ne.iCorr.and.DScorr.ne.1.0D0)) then        !
       call printerror(1,119982255,'fillhists.F         ')     !
       print*,'DScorr out of range: ',DScorr                   !
       print*,'nttyp: ',nttyp                                  !
       print*,'iCorr: ',iCorr                                  !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!



      if (cuthists(cnum).ne.1) return !--> Skip Filling for this cut if desired
      idxcut=idxcut+1


      idxevt=0
      do tt=1,nevtyp !--> Loop over event classifications
      if(evtsw(tt).eq.1) then !--> Only fill desired event classifications
      idxevt=idxevt+1

      if(typpass(tt)) then !--> Only plot if cuts have been passed
      do ii=1,nvar !--> Kinematic Variables
       do jj=1,nbintyp(ii) !--> Bin Types
        do kk=1,extyp !--> Extra Cut Types
                 ! kk=0 --> No Cut
                 ! kk=1 --> Signal Cut
                 ! kk=2 --> Bkg. Cut
                 !   ...
          hid=gethistid(idxevt,idxcut,kk,ii,jj)
          if(check(kk).eq.1) then 
           call hf1(hid,real(KinVar(ii)),
     |                  real(wt*norm(1,nttyp)*renorm*DScorr)) 
          endif
        enddo
       enddo
      enddo
      endif
      endif
      enddo

      return
      end
