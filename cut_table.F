      subroutine cut_table(nmc,filetag,ttag,ncuts,
     |           nevtyp,nextyp,nncand,ndetsec,
     |           detsw,ncndsw,evtsw,cuts,norm,nttyp)
      implicit none

      integer nmc
      character*20 filetag(0:nmc)
      character*10 ttag(0:nmc)
      integer ncuts
      integer nevtyp
      integer nextyp
      integer nncand
      integer ndetsec
      integer detsw(ndetsec)
      integer ncndsw(nncand)
      integer evtsw(nevtyp)
      double precision cuts(3,ncuts,0:nmc,nevtyp,nextyp,nncand,ndetsec) 
      double precision norm(0:1,0:nmc)
      integer nttyp

      integer ii,jj,kk,ll,mm,nn,oo !--> Do Loop Variables
      character*20 cutname(ncuts)

      character*6 classtag(nevtyp) !--> Event classification file tag
      character*4 dettag(ndetsec)
      character*6 ncndtag(nncand)
      character*6 exttag(nextyp)

      integer length !--> String length
      integer UN !--> File fortran unit number

      double precision ratio
      double precision eff

      ! Initialize cut names in case the user forgets to set them:
      do ii=1,ncuts
       cutname(ii) = 'ERROR Value not set!'
      enddo

      ! Initialize event classification file tags:
      classtag(1)='numucc'
      classtag(2)='osdimu'
      classtag(3)='lsdimu'
      classtag(4)='osmupl'

      ! Initialize detector section file tags:
      dettag(1)='drft'
      dettag(2)='othr'
      dettag(3)='coil'
      dettag(4)='upst'

      ! Initialize ncand file tags:
      ncndtag(1)='ncand2'
      ncndtag(2)='ncand3'
      ncndtag(3)='ncand4'
      ncndtag(4)='ncnd34'

      ! Initialize extra cut tags:
      exttag(1)='_allev'
      exttag(2)='_mblnd'
      exttag(3)='_m-sig'
      exttag(4)='_mnorm'


      ! Find an available file unit number to use:
      call FindOpenUnit(UN)

      !***************************************************!
      ! List Cut Names:
      cutname(1) = 'Raw Events          '
      cutname(2) = 'OBGfid,Trig+CohGenTh'
      cutname(3) = 'Pfermi \\& W2        '
      cutname(4) = 'Focus Positive      '
      cutname(5) = 'Fid. Vol. -X        '
      cutname(6) = 'Fid. Vol. -Y        '
      cutname(7) = 'Fid. Vol. -Z (OFF)  '
      cutname(8) = 'At Least 1 Mu       '
      cutname(9) = 'ncand=2,3,4         '
      cutname(10) = 'tnchgd=2            '
c      cutname(9) = 'tnchgd=2 (OFF)      '
      cutname(11)= '+/- Tracks (V0)     '
      cutname(12)= 'Tube/Veto Cut       '
      cutname(13)= '2 Muons (1mux)      '
      cutname(14)= 'PmuAsym<0.0         '
      cutname(15)= 'Theta$<$2.62 rad    '
      cutname(16)= 'Pt+wrt- $>$0.05     '
      cutname(17)= 'Mee $>$ 2.0  (OFF)  '
      cutname(18)= 'Upstream Hanger cut '
      cutname(19)= 'nsecond$<$4         '
      cutname(20)= 'Fid. Vol. Hanger cut'
      cutname(21)= 'No Hangers fromPVert'
      cutname(22)= 'Pz$>$0 for tracks   '
      cutname(23)= 'Thprimord$<$0.4     '
      cutname(24)= 'Nunh*fracunh$<$200  '
      cutname(25)= 'Emumu$>$2GeV        '
      cutname(26)= 'P+,P-$>$0.5         ' 
      cutname(27)= 'P+,P-$>$1.0 (2.5mux)' 
      cutname(28)= 'Emumu$>$5GeV  (8mux)'
      cutname(29)= 'Phi12$>$90deg  (OFF)'
      cutname(30)= 'Pmumu$>$10GeV  (OFF)'
c     cutname(30)= 'No cut, not set     '
      !***************************************************!


      ! Check that the user has set all cut values:
      do ii=1,ncuts
       if(cutname(ii).eq.'ERROR Value not set!') then
        call printerror(2,488291948,'cut_table.F         ')
        print*,'User has not set all cut label values!'
        print*,'Cut number: ',ii
        print*,'Current value: ',cutname(ii)
        print*,''
       endif
      enddo
     
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      ! Check input for errors:                                        !
       if(nmc.lt.1) then                                               !
        call printerror(1,488372661,'cut_table.F         ')            !
        print*,'nmc out of range: ',nmc                                !
        print*,'ncuts: ',ncuts                                         !
        print*,''                                                      !
        stop                                                           !
       endif                                                           !
       if(ncuts.lt.1) then                                             !
        call printerror(1,987685431,'cut_table.F         ')            !
        print*,'ncuts out of range: ',ncuts                            !
        print*,'nmc: ',nmc                                             !
        print*,''                                                      ! 
        stop                                                           !
       endif                                                           !
       if(nttyp.lt.0.or.nttyp.gt.nmc) then                             !
        call printerror(1,444859210,'cut_table.F         ')            !
        print*,'nttyp out of range! nttyp: ',nttyp                     !
        print*,''                                                      !
        stop                                                           !
       endif                                                           !
c       do ii=1,3                                                       !
c       do jj=1,ncuts                                                   !
c       do kk=0,nmc                                                     !
c       do ll=1,nevtyp                                                  !
c        if(cuts(ii,jj,kk,ll).lt.0.0D0) then                            !
c         call printerror(1,578229117,'cut_table.F         ')           !
c         print*,'Negative cut value!'                                  !
c         print*,'ii, jj, kk, ll, cuts(ii,jj,kk,ll): ',                 !
c     |           ii,' ',jj,' ',kk,' ',ll,' ',cuts(ii,jj,kk,ll)         !
c         print*,''                                                     !
cc         stop                                                          !
c        endif                                                          !
c       enddo                                                           !
c       enddo                                                           !
c       enddo                                                           !
c       enddo                                                           !
       do ii=0,1                                                       !
       do jj=0,nmc                                                     !
        if(norm(ii,jj).lt.0.0D0) then                                  !
         call printerror(1,388447104,'cut_table.F         ')           !
         print*,'Negative normalization!'                              !
         print*,'ii, jj, norm(ii,jj): ',ii,' ',jj,' ',norm(ii,jj)      !
         print*,''                                                     !
         stop                                                          !
        endif                                                          !
       enddo                                                           !
       enddo                                                           !
       if(norm(0,0).ne.1.0D0.or.norm(1,0).ne.1.0D0) then               !
         call printerror(1,288948588,'cut_table.F         ')           !
         print*,'Data norm not 1.0!: ',norm(0,0),' ',norm(1,0)         !
         print*,''                                                     !
         stop                                                          !
       endif                                                           !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!



      jj=nttyp !--> Put nttyp into a variable with a shorter name
      do ll=1,nevtyp !--> Event classification loop (DL-00)
       if(evtsw(ll).eq.1) then
      do mm=1,nextyp
      do nn=1,nncand
       if(ncndsw(nn).eq.1) then
      do oo=1,ndetsec
       if(detsw(oo).eq.1) then
      

      !==============================================================!
      ! Write .txt Cut Tables:
       length=LEN_TRIM(filetag(jj))
       open (UN,file='outputs/cuts/'//
     |  classtag(ll)//'/'//dettag(oo)//'/'//ncndtag(nn)//'/'//
     | 'cut_'//filetag(jj)(1:length)//exttag(mm)//'.txt',
     |                                     status='UNKNOWN')


       write(UN,66) '','       Raw  ',
     |                 '      AOT   ',
     |                 '     norm0  ',
     |                 '     Final  ',
     |    '  |',       '    Ratio   ',
     |    '  |',       '     eff.   '
       write(UN,68) '---------------------------------------------------
     |------------------------|--------------|--------------'

       do kk=1,ncuts !--> Loop over Cuts (DL-02)

      ! Ratio between current and previous cut:
      if(kk.eq.1) ratio =100.0D0
      if(kk.ne.1) then
        if(cuts(3,kk-1,jj,ll,mm,nn,oo).gt.0.0D0) then
            ratio =( cuts(3,kk  ,jj,ll,mm,nn,oo) /
     |               cuts(3,kk-1,jj,ll,mm,nn,oo)   )*100.0D0
        else
            ratio = 0.0D0
        endif
      endif
      ! Total efficiency:
      if(kk.eq.1)   eff =100.0D0
      if(kk.ne.1)   then
        if(cuts(3,1,jj,ll,mm,nn,oo).gt.0.0D0) then
              eff =( cuts(3,kk,jj,ll,mm,nn,oo) /
     |               cuts(3, 1,jj,ll,mm,nn,oo)   )*100.0D0
        else
            eff = 0.0D0
        endif
      endif

      write(UN,55) kk,') ',cutname(kk),
     |                     cuts(1,kk,jj,ll,mm,nn,oo),
     |                     cuts(2,kk,jj,ll,mm,nn,oo),
     |                     cuts(2,kk,jj,ll,mm,nn,oo)*norm(0,jj),
     |                     cuts(3,kk,jj,ll,mm,nn,oo)*norm(1,jj),
     |        '  |   (', ratio,'%) ',
     |        '  |   (',   eff,'%) '
      write(UN,67)'','|              |'
       
       if (kk.eq.ncuts) then 
       write(UN,68) '===================================================
     |======================================================'
       write(UN,*)''
       endif

      enddo !(DL-02)
      close(UN)

55    format(I3,A2,a20,4f12.1,2(a7,F5.1,a3))
66    format(a25,4a12,2(a3,a12))
67    format(a75,a16)
68    format(a105)
      !==============================================================!



      !==============================================================!
      !------ Form Latex Tables ---------!
77    format(I3,A2,a20,4(a3,f12.1),2(a4,f5.1,a3),a3)
       length=LEN_TRIM(filetag(jj))
       open (UN,file='outputs/cuts/'//
     |  classtag(ll)//'/'//dettag(oo)//'/'//ncndtag(nn)//'/'//
     | 'cut_'//filetag(jj)(1:length)//exttag(mm)//'_latex.tex',
     |                                     status='UNKNOWN')
        write(UN,*)'\\begin{table}[h!]\\centering'
        write(UN,*)'\\begin{tabular}{||l||r|r|r|r|r|r||}'
        write(UN,*)'\\hline'
        write(UN,*)'\\hline'
        write(UN,*) 'Cut & Raw & AOT & Norm0 & Final & Ratio & eff.
     |  \\\\'
        write(UN,*)'\\hline'

        do kk=1,ncuts 
        if(kk.eq.1) ratio =100.0D0
        if(kk.ne.1) then
          if(cuts(3,kk-1,jj,ll,mm,nn,oo).gt.0.0D0) then
                      ratio =( cuts(3,kk  ,jj,ll,mm,nn,oo) /
     |                         cuts(3,kk-1,jj,ll,mm,nn,oo)  )*100.0D0
          else
            ratio = 0.0D0
          endif
        endif
        if(kk.eq.1)   eff =100.0D0
        if(kk.ne.1)   then
          if(cuts(3,1,jj,ll,mm,nn,oo).gt.0.0D0) then
                        eff =( cuts(3,kk,jj,ll,mm,nn,oo) /
     |                         cuts(3, 1,jj,ll,mm,nn,oo)   )*100.0D0
          else
            eff = 0.0D0
          endif
        endif
         write(UN,77) kk,') ',cutname(kk),
     |                      ' & ',cuts(1,kk,jj,ll,mm,nn,oo),
     |                      ' & ',cuts(2,kk,jj,ll,mm,nn,oo),
     |                      ' & ',cuts(2,kk,jj,ll,mm,nn,oo)*norm(0,jj),
     |                      ' & ',cuts(3,kk,jj,ll,mm,nn,oo)*norm(1,jj),
     |                      ' & (',ratio,'\\%)',
     |                      ' & (',eff,'\\%)',
     |                      ' \\\\'
        enddo 
        write(UN,*)'\\hline'
        write(UN,*)'\\hline'
        write(UN,*)'\\end{tabular}'
        write(UN,*)'\\caption{Cut Table '//ttag(jj)//'}'
        write(UN,*)'\\label{tab-cut'//filetag(jj)(1:length)//'}'
        write(UN,*)'\\end{table}'
        close(UN)
      !==============================================================!

       endif
      enddo
       endif
      enddo
      enddo
       endif
      enddo !(DL-00)

      return
      end
