      subroutine check_print_settings(nvar,ncuts,nmc,
     |                 maxevts,usents,fullnts,runtype,cuthists,
     |                 norm,datacard,filetag,gtyp,gennt,
     |                 usecorr2D,usecorrMass,usecorrZeta,
     |                 formRenorm,iCorr,
     |                 ndetsec,nncand,nevttyp,
     |                 detsw,ncndsw,evtsw,
     |                 ndetsw,nncndsw,nevtsw,ncuthists)
      implicit none

      integer nvar
      integer ncuts
      integer nmc
      integer maxevts
      integer usents
      integer fullnts
      integer runtype(0:nmc)
      integer cuthists(ncuts)
      double precision norm(0:1,0:nmc) 
      character*20 datacard(0:nmc,0:1)
      character*20 filetag(0:nmc)
      integer gtyp(0:nmc)
      double precision gennt(nmc,3) ! Number of generated events in each ntuple
      integer usecorr2D
      integer usecorrMass
      integer usecorrZeta
      integer formRenorm
      integer iCorr
      integer ndetsec,nncand,nevttyp
      integer detsw(ndetsec)
      integer ncndsw(nncand)
      integer evtsw(nevttyp)
      integer ndetsw
      integer nncndsw
      integer nevtsw
      integer ncuthists

      integer ii,jj !--> Do loop variables
      integer UNum !--> FORTRAN unit number to print settings



      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      ! Check user input for errors:                                    !
      if(nvar.lt.1.or.nvar.gt.99) then                                  !
       call printerror(1,677854432,'main.F              ')              !
       print*,'nvar out of range: ',nvar                                !
       print*,'Must modify histogram booking for nvar>99'               !
       print*,''                                                        !
       stop                                                             !
      endif                                                             !
      if(ncuts.lt.1) then                                               !
       call printerror(1,577483986,'main.F              ')              !
       print*,'ncuts out of range: ',ncuts                              !
       print*,''                                                        !  
       stop                                                             !
      endif                                                             !
      if(nmc.lt.1) then                                                 !
       call printerror(1,229388471,'main.F              ')              !
       print*,'nmc out of range: ',nmc                                  !
       stop                                                             !
      endif                                                             !
      if(maxevts.lt.0) then                                             !
       call printerror(1,847711119,'main.F              ')              !
       print*,'maxevts out of range: ',maxevts                          !
       stop                                                             !
      endif                                                             !
      if(usents.lt.0) then                                              !
       call printerror(1,111992854,'main.F              ')              !
       print*,'usents out of range: ',usents                            !
       stop                                                             !
      endif                                                             !
      if(fullnts.ne.1.and.fullnts.ne.0) then                            !
       call printerror(1,222999331,'main.F              ')              !
       print*,'fullnts out of range: ',fullnts                          !
       print*,''                                                        !
       stop                                                             !
      endif                                                             !
      do ii=0,nmc                                                       !
      if(runtype(ii).ne.1.and.runtype(ii).ne.0) then                    !
       call printerror(1,122211847,'main.F              ')              !
       print*,'runtype out of range!'                                   !
       print*,'ii, runtype(ii): ',ii,' ',runtype(ii)                    !
       print*,''                                                        !
       stop                                                             !
      endif                                                             !
      enddo                                                             !
      do ii=1,ncuts                                                     !
      if(cuthists(ii).ne.1.and.cuthists(ii).ne.0) then                  !
       call printerror(1,448833001,'main.F              ')              !
       print*,'cuthists out of range!'                                  !
       print*,'ii, cuthists(ii): ',ii,' ',cuthists(ii)                  !
       stop                                                             !
      endif                                                             !
      enddo                                                             !
      do ii=0,1                                                         !
      do jj=0,nmc                                                       !
      if(norm(ii,jj).lt.0.0D0) then                                     !
       call printerror(1,687775921,'main.F              ')              !
       print*,'norm out of range!'                                      !
       print*,'ii, jj, norm(ii,jj): ',ii,' ',jj,' ',norm(ii,jj)         !
       print*,''                                                        !
       stop                                                             !
      endif                                                             !
      enddo                                                             !
      enddo                                                             !
      do ii=0,nmc                                                       !
      if(gtyp(ii).lt.1.or.gtyp(ii).gt.7) then                           !
       call printerror(1,668740012,'main.F              ')              !
       print*,'gtyp out of range!'                                      !
       print*,'ii, gtyp(ii): ',ii,' ',gtyp(ii)                          !
       print*,''                                                        !
       stop                                                             !
      endif                                                             !
      enddo                                                             !
      do ii=1,nmc                                                       !
      if(gennt(ii,gtyp(ii)).le.0.0D0) then                              !
       call printerror(1,100029837,'main.F              ')              !
       print*,'gennt out of range!'                                     !
       print*,'ii, gtyp(ii): ',ii,' ',gtyp(ii)                          !
       print*,'gennt(',ii,',',gtyp(ii),'): ',gennt(ii,gtyp(ii))         !
       print*,''                                                        !
       stop                                                             !
      endif                                                             !
      enddo                                                             !
      if(usecorr2D.ne.0.and.usecorr2D.ne.1) then                        !
       call printerror(1,774665367,'main.F              ')              !
       print*,'usecorr2D out of range: ',usecorr2D                      !
       stop                                                             !
      endif                                                             !
      if(usecorrMass.ne.0.and.usecorrMass.ne.1) then                    !
       call printerror(1,111982763,'main.F              ')              !
       print*,'usecorrMass out of range: ',usecorrMass                  !
       stop                                                             !
      endif                                                             !
      if(usecorrZeta.ne.0.and.usecorrZeta.ne.1) then                    !
       call printerror(1,222988777,'main.F              ')              !
       print*,'usecorrZeta out of range: ',usecorrZeta                  !
       stop                                                             !
      endif                                                             !
      if(formRenorm.ne.0.and.formRenorm.ne.1) then                      !
       call printerror(1,229938000,'main.F              ')              !
       print*,'formRenorm out of range: ',formRenorm                    !
       stop                                                             !
      endif                                                             !
      if(iCorr.le.0.or.iCorr.gt.nmc) then                               !
       call printerror(1,228837641,'main.F              ')              !
       print*,'iCorr out of range: ',iCorr                              !
       stop                                                             !
      endif                                                             !
      if(ndetsec.lt.1) then                                             !
       call printerror(1,487652108,'main.F              ')              !
       print*,'ndetsec out of range: ',ndetsec                          !
       print*,''                                                        !  
       stop                                                             !
      endif                                                             !
      do ii=1,ndetsec                                                   !
      if(detsw(ii).ne.1.and.detsw(ii).ne.0) then                        !
       call printerror(1,115458857,'main.F              ')              !
       print*,'detsw out of range!'                                     !
       print*,'ii, detsw(ii): ',ii,' ',detsw(ii)                        !
       stop                                                             !
      endif                                                             !
      enddo                                                             !
      if(ndetsw.lt.1) then                                              !
       call printerror(1,909087623,'main.F              ')              !
       print*,'ndetsw out of range: ',ndetsw                            !
       print*,''                                                        !  
       stop                                                             !
      endif                                                             !
      if(nncand.lt.1) then                                              !
       call printerror(1,228327738,'main.F              ')              !
       print*,'nncand out of range: ',nncand                            !
       print*,''                                                        !  
       stop                                                             !
      endif                                                             !
      do ii=1,nncand                                                    !
      if(ncndsw(ii).ne.1.and.ncndsw(ii).ne.0) then                      !
       call printerror(1,003994881,'main.F              ')              !
       print*,'ncndsw out of range!'                                    !
       print*,'ii, ncndsw(ii): ',ii,' ',ncndsw(ii)                      !
       stop                                                             !
      endif                                                             !
      enddo                                                             !
      if(nncndsw.lt.1) then                                             !
       call printerror(1,098768761,'main.F              ')              !
       print*,'nncndsw out of range: ',nncndsw                          !
       print*,''                                                        !  
       stop                                                             !
      endif                                                             !
      if(nevttyp.lt.1) then                                             !
       call printerror(1,333728875,'main.F              ')              !
       print*,'nevttyp out of range: ',nevttyp                          !
       print*,''                                                        !  
       stop                                                             !
      endif                                                             !
      do ii=1,nevttyp                                                   !
      if(evtsw(ii).ne.1.and.evtsw(ii).ne.0) then                        !
       call printerror(1,577462198,'main.F              ')              !
       print*,'evtsw out of range!'                                     !
       print*,'ii, evtsw(ii): ',ii,' ',evtsw(ii)                        !
       stop                                                             !
      endif                                                             !
      enddo                                                             !
      if(nevtsw.lt.1) then                                              !
       call printerror(1,577648910,'main.F              ')              !
       print*,'nevtsw out of range: ',nevtsw                            !
       print*,''                                                        !  
       stop                                                             !
      endif                                                             !
      if(ncuthists.lt.1) then                                           !
       call printerror(1,222938816,'main.F              ')              !
       print*,'ncuthists out of range: ',ncuthists                      !
       print*,''                                                        !  
       stop                                                             !
      endif                                                             !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!



      if(maxevts.ne.0) then
      print*,''
      print*,''
      print*,''
      print*,'************************************************'
      print*,'***                 WARNING!                 ***'
      print*,'*** May not be using the full set of events! ***'
      print*,'************************************************'
      print*,''
      print*,'maxevts: ',maxevts
      print*,''
      print*,''
      endif
      if(usents.ne.0) then
      print*,''
      print*,''
      print*,''
      print*,'*************************************************'
      print*,'***                   WARNING!                ***'
      print*,'*** May not be using the full set of ntuples! ***'
      print*,'*************************************************'
      print*,''
      print*,'usents: ',usents
      print*,''
      print*,''
      endif



2     format(A8,I2,A3,A20)
3     format(A35,I10)
5     format(A42,I3)
55    format(A42,I3)
6     format(A8,i2,A3,i2)
7     format(A9,i2,a1,i2,a4,a20)
8     format(A7,i2,A3,F10.6,a1,F10.6)
9     format(A9,i2,A3,i2)
10    format(A5,i2,A3,i2)
11    format(A6,i2,A3,F12.2)
    
      ! Print settings to output:
      call FindOpenUnit(UNum)
      open(UNum,file='outputs/settings.txt',status='UNKNOWN')
       write(UNum,*) 'User Settings:'
       write(UNum,*)''
       write(UNum,5)'nmc (# of MC components):                 ',nmc
       write(UNum,5)'fullnts (full (1) or baby (0) ntuples):   ',fullnts
       write(UNum,5)'maxevts (# of events to process (0=all)): ',maxevts
       write(UNum,5)'usents (# of ntuples to process (0=all)): ',usents
       write(UNum,5)'nvar (# of variables plotted):            ',nvar
       write(UNum,5)'ncuts (# of cuts exacted):                ',ncuts
       write(UNum,55)'usecorr2D:          ',usecorr2D
       write(UNum,55)'usecorrMass:        ',usecorrMass
       write(UNum,55)'usecorrZeta:        ',usecorrZeta
       write(UNum,55)'formRenorm:         ',formRenorm
       write(UNum,55)'iCorr:              ',iCorr
       write(UNum,*) '-------------------------------------------------'
       write(UNum,*) 'runtypes (1=run, 0=skip):'
       do ii=0,nmc
        write(UNum,6) 'runtype(',ii,'): ',runtype(ii)
       enddo
       write(UNum,*) '-------------------------------------------------'
       write(UNum,*) 'Datacards used:'
       do ii=0,nmc
        if(runtype(ii).ne.0) then
        write(UNum,7) 'datacard(',ii,',',fullnts,') : ',
     |                         datacard(ii,fullnts)
        endif
       enddo
       write(UNum,*) '-------------------------------------------------'
       write(UNum,*) 'MC types used (1=Nuage, 2=Neglib, 3=Genie):      '
       write(UNum,*) '              ( 4-7 are permutations)            '
       do ii=0,nmc
        if(runtype(ii).ne.0) then
        write(UNum,10) 'gtyp(',ii,'): ',gtyp(ii)
        endif
       enddo
       write(UNum,*) '-------------------------------------------------'
       write(UNum,*) 'Generated number of MC events in ntuples:        '
       do ii=1,nmc
        if(runtype(ii).ne.0) then
        write(UNum,11) 'gennt(',ii,'): ',gennt(ii,gtyp(ii))
        endif
       enddo
       write(UNum,*) '-------------------------------------------------'
       write(UNum,*) 'Normalizations used (norm0 then normTot):'
       do ii=0,nmc
        if(runtype(ii).ne.0) then
        write(UNum,8) 'norm(*,',ii,'): ',norm(0,ii),' ',norm(1,ii)
        endif
       enddo
       write(UNum,*) '-------------------------------------------------'
       write(UNum,*) 'cuthists (1=plot for cut, 0=skip:'
       do ii=1,ncuts
        write(UNum,9) 'cuthists(',ii,'): ',cuthists(ii)
       enddo
       write(UNum,*) '-------------------------------------------------'
       write(UNum,*) 'Histogram naming tags:'
       do ii=0,nmc
        write(UNum,2) 'filetag(',ii,'): ',filetag(ii)
       enddo
       write(UNum,*) '-------------------------------------------------'
       write(UNum,*) 'END OF SETTINGS'
      close(UNum)
22    format(A11,A200)

      return
      end
