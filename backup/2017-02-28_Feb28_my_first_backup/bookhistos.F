      subroutine bookhistos(nvar,extyp,ncuts,zetaS,nbintyp,cuthists,
     |                 ndetsec,nncand,nevtyp,nextyp,
     |                 detsw,ncndsw,evtsw,
     |                 ndetsw,nncndsw,nevtsw,ncuthists,
     |                 detidx,ncndidx,evtidx)
      implicit none

      integer nvar
      integer extyp
      integer ncuts
      double precision zetaS
      integer nbintyp(nvar)
      integer cuthists(ncuts)
      integer ndetsec,nncand,nevtyp,nextyp
      integer detsw(ndetsec)
      integer ncndsw(nncand)
      integer evtsw(nevtyp)
      integer ndetsw
      integer nncndsw
      integer nevtsw
      integer ncuthists
      integer detidx(ndetsec)
      integer ncndidx(nncand)
      integer evtidx(nevtyp)

      !-------------------------!
      ! This group of variables
      ! occurs here and in 
      ! setbin.F, and both 
      ! must agree exactly.
      integer nbinvarMAX !--> Maximum number of bins per variable 
      integer nvarMAX !--> Maximum number of variables
      integer extypMAX !--> Maximum number of extra cut types
      parameter (nbinvarMAX=20)
      parameter (nvarMAX=99)
      parameter (extypMAX=500)
#include "booking.inc"
      !-------------------------!

      integer ii,jj,kk,ll,tt !--> Do Loop Variables
      integer hid !--> Histogram ID Number
      integer var !--> Temporary number for easier editing
      integer idx !--> Temporary index
      integer idxdet
      integer idxncnd
      integer idxevt
      integer idxcut
      character*20 Var_name(nvar) !--> Histogram name for each variable
      character*8 chartag(nevtyp) !--> Event characterization tag
      character*80 hname !--> Full histogram name
      character*5 zcut !--> Zeta cut value string

      character*31 exttag(0:extyp) !--> Extra cut type tags
      character*17 excuttag(nextyp)
      character*7  dettag(ndetsec) !--> Detector section labels
      character*7 ncandtag(nncand)



      !------------------------------------!
      ! Variables for numbering histograms:
       integer nSet !--> Number of sets
       parameter (nSet=5)
       integer nval(nSet) !--> Number of possible values for each set
       integer iset
       integer UN
       integer gethistid !--> function to get sequential histogram number
      !------------------------------------!


      !============================================!
      ! Variables for numbering checks:
      integer nSetext
      parameter (nSetext=3) !--> Number of extra cut sets
      integer nvalext(nSetext) !--> Number of possible values for each set
      integer getcheckid !--> function to get sequential histogram number
      !============================================!





      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      ! Input Error Checking:                                  !
      if(nvar.lt.1.or.nvar.gt.nvarMAX) then                    !
       call printerror(1,458225832,'bookhistos.F        ')     !
       print*,'nvar out of range: ',nvar                       !
       print*,'Must modify histogram booking for nvar>99'      !
       print*,'nvarMAX: ',nvarMAX                              !
       print*,'extyp: ',extyp                                  !
       print*,'ncuts: ',ncuts                                  !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(extyp.lt.1.or.extyp.gt.extypMAX) then                 !
       call printerror(1,785734432,'bookhistos.F        ')     !
       print*,'extyp out of range: ',extyp                     !
       print*,'extypMAX: ',extypMAX                            !
       print*,'nvar: ',nvar                                    !
       print*,'ncuts: ',ncuts                                  !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      if(ncuts.lt.0.or.ncuts.gt.99) then                       !
       call printerror(1,788166752,'bookhistos.F        ')     !
       print*,'ncuts out of range: ',ncuts                     !
       print*,'Must change histogram booking for ncuts>99'     !
c       print*,'Must change booking for VERY large ncuts.'     !
c       print*,'The largest possible signed integer is:'       !
c       print*,'2,147,483,647'                                 !
c       print*,' Histogram IDs will become negative if this'   !
c       print*,' number is surpassed.'                         !
       print*,'nvar: ',nvar                                    !
       print*,'extyp: ',extyp                                  !
       print*,''                                               !
       stop                                                    !
      endif                                                    !
      do ii=1,ncuts                                            !
       if(cuthists(ii).ne.0.and.cuthists(ii).ne.1) then        !
        call printerror(1,688001253,'bookhistos.F        ')    !
        print*,'cuthist out of range!'                         !
        print*,'ii, cuthists(ii): ',ii,' ',cuthists(ii)        !
        print*,'ncuts: ',ncuts                                 !
        print*,'nvar: ',nvar                                   !
        print*,'extyp: ',extyp                                 !
        stop                                                   !
       endif                                                   !
      enddo                                                    !
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!


      !ivivivivivivivivivivivivivivivivivivivivivivii
      !ivivivivivivivivivivivivivivivivivivivivivivii
      !ivivi     Variable Initialization     ivivivii
      !---------------------------------------------!
       do ii=1,nvarMAX                              !
        nbintypT(ii) = 1                            !
       enddo                                        !
                                                    !
       do ii=1,nvarMAX                              !
        do jj=1,nbinvarMAX                          !
         bintag(ii,jj)='(Error)'                    !
         do kk=0,extyp                              !
          nbin(ii,jj,kk) = 1                        !
          hmin(ii,jj,kk) = -999.0                   !
          hmax(ii,jj,kk) = -998.0                   !
         enddo                                      !
        enddo                                       !
       enddo                                        !
      !ivivivivivivivivivivivivivivivivivivivivivivii
      !ivivivivivivivivivivivivivivivivivivivivivivii




      !========================================================!
       var=1
       Var_name(var)='P?[m]1! X (GeV/c)'
       call setbin(var,0,  -1.5,  1.5, 75,'       ')
      !========================================================!
       var=2
       Var_name(var)='P?[m]1! Y (GeV/c)'
       call setbin(var,0,  -1.5,  1.5, 75,'       ')
      !========================================================!
       var=3
       Var_name(var)='P?[m]1! Z (GeV/c)'
       call setbin(var,0,  -1.0, 30.0, 31,'       ')
      !========================================================!
       var=4
       Var_name(var)='P?[m]1! (GeV/c)'
       call setbin(var,0,  0.0, 30.0, 30,'       ')
      !========================================================!
       var=5
       Var_name(var)='E?[m]1! (GeV)'
       call setbin(var,0,  0.0, 30.0, 30,'       ')
      !========================================================!
       var=6
       Var_name(var)='Pt 1 (GeV/c)'
       call setbin(var,0,  0.0, 2.0, 20,'       ')
      !========================================================!
       var=7
       Var_name(var)='[Q]?[m]1! (Rad)'
       call setbin(var,0,  0.0, 0.5, 32,'       ')
      !========================================================!
       var=8
       Var_name(var)='[z]?[m]1!'
       call setbin(var,0, 0.0, 0.1, 40,'       ')
       call setbin(var,0, 0.0, 0.2, 40,'       ')
       call setbin(var,0, 0.0, 0.3, 30,'       ')
       call setbin(var,0, 0.0, 0.5, 50,'       ')
      !========================================================!
       var=9
       Var_name(var)='[F]?[m]1! (Deg)'
       call setbin(var,0, 0.0, 360.0, 18,'       ')
      !========================================================!
       var=10
       Var_name(var)='Charge [m]?1!'
       call setbin(var,0, -2.5, 2.5, 5,'       ')
      !========================================================!
       var=11
       Var_name(var)='P?[m]2! X (GeV/c)'
       call setbin(var,0,  -1.5,  1.5, 75,'       ')
      !========================================================!
       var=12
       Var_name(var)='P?[m]2! Y (GeV/c)'
       call setbin(var,0,  -1.5,  1.5, 75,'       ')
      !========================================================!
       var=13
       Var_name(var)='P?[m]2! Z (GeV/c)'
       call setbin(var,0,  -1.0, 30.0, 31,'       ')
      !========================================================!
       var=14
       Var_name(var)='P?[m]2! (GeV/c)'
       call setbin(var,0,  0.0, 30.0, 30,'       ')
      !========================================================!
       var=15
       Var_name(var)='E?[m]2! (GeV)'
       call setbin(var,0,  0.0, 30.0, 30,'       ')
      !========================================================!
       var=16
       Var_name(var)='Pt 2 (GeV/c)'
       call setbin(var,0,  0.0, 2.0, 20,'       ')
      !========================================================!
       var=17
       Var_name(var)='[Q]?[m]2! (Rad)'
       call setbin(var,0,  0.0, 0.5, 32,'       ')
      !========================================================!
       var=18
       Var_name(var)='[z]?[m]2!'
       call setbin(var,0, 0.0, 0.1, 40,'       ')
       call setbin(var,0, 0.0, 0.2, 40,'       ')
       call setbin(var,0, 0.0, 0.3, 30,'       ')
       call setbin(var,0, 0.0, 0.5, 50,'       ')
      !========================================================!
       var=19
       Var_name(var)='[F]?[m]2! (Deg)'
       call setbin(var,0, 0.0, 360.0, 18,'       ')
      !========================================================!
       var=20
       Var_name(var)='Charge [m]?2!'
       call setbin(var,0, -2.5, 2.5, 5,'       ')
      !========================================================!
       var=21
       Var_name(var)='P?[mm]! X (GeV/c)'
       call setbin(var,0,  -2.0,  2.0, 100,'       ')
      !========================================================!
       var=22
       Var_name(var)='P?[mm]! Y (GeV/c)'
       call setbin(var,0,  -2.0,  2.0, 100,'       ')
      !========================================================!
       var=23
       Var_name(var)='P?[mm]! Z (GeV/c)'
       call setbin(var,0,  -1.0, 10.0, 44,'       ')
      !========================================================!
       var=24
       Var_name(var)='P?[mm]! (GeV/c)'
       call setbin(var,0,  0.0, 40.0, 40,'       ')
      !========================================================!
       var=25
       Var_name(var)='E?[mm]! (GeV)'
       call setbin(var,0,  0.0, 30.0, 30,'       ')
       call setbin(var,0,  0.0, 20.0, 100,'       ')
      !========================================================!
       var=26
       Var_name(var)='Pt[mm] (GeV/c)'
       call setbin(var,0,  0.0, 2.5, 50,'       ')
      !========================================================!
       var=27
       Var_name(var)='[Q]?[mm]! (Rad)'
       call setbin(var,0,  0.0, 0.15, 32,'       ')
      !========================================================!
       var=28
       Var_name(var)='[z]?[mm]!'
       call setbin(var,0, 0.0, 0.06, 30,'       ')
       call setbin(var,0, 0.0, 0.1, 30,'       ')
       call setbin(var,0, 0.0, 0.2, 40,'       ')
       call setbin(var,0, 0.0, 0.3, 30,'       ')
       call setbin(var,0, 0.0, 0.5, 50,'       ')
      !========================================================!
       var=29
       Var_name(var)='[F]?[mm] (Deg)'
       call setbin(var,0, 0.0, 360.0, 18,'       ')
      !========================================================!
       ! All mass binnings have a bin centered on the J/Psi
       ! mass of 3.096916 GeV/c^2
       var=30
       Var_name(var)='M?[mm]! (GeV/c^2!)'
       !--------------
       ! 0 to 7:
       call setbin(var,0,  0.021916, 6.921916, 46,'150MeV ') ! 1 
       call setbin(var,0,  0.046916, 6.946916, 69,'100MeV ') ! 2 
c       call setbin(var,0,  0.0, 7.0, 70,'100MeV ') ! Leslie's Old Binning
       call setbin(var,0,  0.059416, 6.959416, 92,' 75MeV ') ! 3 
       call setbin(var,0,  0.021916, 6.971916,139,' 50MeV ') ! 4 
       !--------------
       ! 0 to 5:
       call setbin(var,0,  0.246916, 4.746916 , 15,'300MeV ') ! 5
       call setbin(var,0,  0.021916, 4.971916 , 33,'150MeV ') ! 6
       call setbin(var,0,  0.046916, 4.946916 , 49,'100MeV ') ! 7
       call setbin(var,0,  0.059416, 4.934416 , 65,' 75MeV ') ! 8
       call setbin(var,0,  0.021916, 4.971916 , 99,' 50MeV ') ! 9
       call setbin(var,0,  0.009416, 4.984416 ,199,' 25MeV ') ! 10
       !--------------
       ! 0.9 to 5:
       call setbin(var,0,  0.846916, 5.046916, 14,'300MeV ') ! 11
       call setbin(var,0,  0.771916, 5.121916, 29,'150MeV ') ! 12
       call setbin(var,0,  0.846916, 5.046916, 42,'100MeV ') ! 13
       call setbin(var,0,  0.884416, 5.009416, 55,' 75MeV ') ! 14
       call setbin(var,0,  0.871916, 5.021916, 83,' 50MeV ') ! 15
       call setbin(var,0,  0.884416, 5.009416,165,' 25MeV ') ! 16
       !--------------
       ! 2 to 4 (J/Psi mass zoom):
       call setbin(var,0,  1.984416, 4.184416, 44,'(25MeV)') ! 17
       call setbin(var,0,  1.971916, 4.171916, 44,'(50MeV)') ! 18
       call setbin(var,0,  2.009416, 4.109416, 28,'(75MeV)') ! 19
       call setbin(var,0,  1.946916, 4.146916, 22,'100MeV ') ! 20
      !========================================================!
       var=31
       Var_name(var)='Eclust'
       call setbin(var,0,  0.0, 1.0, 20,'       ')
      !========================================================!
       var=32
       Var_name(var)='[Q]?12! (Rad)'
       call setbin(var,0,  0.0, 0.7, 32,'       ')
      !========================================================!
       var=33
       Var_name(var)='[F]?12! (Deg)'
       call setbin(var,0, 0.0, 180.0, 36,'       ')
      !========================================================!
       var=34
       Var_name(var)='P?[m]! Asym'
       call setbin(var,0, -1.0, 0.0, 18,'       ')
      !========================================================!
       var=35
       Var_name(var)='XVR'
       call setbin(var,0,  -140.0, 140.0, 28,'       ')
      !========================================================!
       var=36
       Var_name(var)='YVR'
       call setbin(var,0,  -140.0, 140.0, 28,'       ')
      !========================================================!
       var=37
       Var_name(var)='ZVR'
       call setbin(var,0,  -20.0, 500.0, 52,'       ')
       call setbin(var,0,  -20.0, 500.0, 26,'       ')
       call setbin(var,0,  -70.0,  40.0, 44,'       ')
      !========================================================!
       var=38
       Var_name(var)='[Q]?x!1 (Rad)'
       call setbin(var,0,  -1.0, 1.0, 30,'       ')
      !========================================================!
       var=39
       Var_name(var)='[Q]?x!2 (Rad)'
       call setbin(var,0,  -1.0, 1.0, 30,'       ')
      !========================================================!
       var=40
       Var_name(var)='[Q]?y!1 (Rad)'
       call setbin(var,0,  -1.0, 1.0, 30,'       ')
      !========================================================!
       var=41
       Var_name(var)='[Q]?y!2 (Rad)'
       call setbin(var,0,  -1.0, 1.0, 30,'       ')
      !========================================================!
       var=42
       Var_name(var)='E HCAL'
       call setbin(var,0,  0.0, 10.0, 40,'       ')
      !========================================================!
       var=43
       Var_name(var)='PtNeut Had'
       call setbin(var,0,  0.0, 1.5, 20,'       ')
      !========================================================!
       var=44
       Var_name(var)='PNeut Had'
       call setbin(var,0,  0.0, 5.0, 40,'       ')
      !========================================================!
       var=45
       Var_name(var)='Diff. First Hits Z'
       call setbin(var,0,  -200.0, 200.0, 100,'       ')
      !========================================================!
       var=46
       Var_name(var)='posz - zvr'
       call setbin(var,0,  -40.0, 40.0, 80,'       ')
      !========================================================!
       var=47
       Var_name(var)='Ntuple zvr'
       call setbin(var,0,  -20.0, 500.0, 52,'       ')
       call setbin(var,0,  -20.0, 500.0, 26,'       ')
       call setbin(var,0,  -70.0,  40.0, 44,'       ')
      !========================================================!





      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!
      ! Copy temporary nbintypT variable to main array
      ! after user settings has completed:
      do ii=1,nvar
       nbintyp(ii)=nbintypT(ii)
      enddo
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!



      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      !ivivivivivivivivivivivivivivivivivivivivivivivivivivivivivivi!
      !----     Post User-Set Error check and Variable Init.    ----!
       do ii=1,nvar                                                 !
        if(nbintyp(ii).gt.nbinvarMAX) then                          !
         call printerror(1,384756621,'bookhistos.F        ')        !
         print*,'Array Overflow caused by nbintyp!'                 !
         print*,'ii, nbintyp(ii): ',ii,' ',nbintyp(ii)              !
         print*,'nbinvarMAX: ',nbinvarMAX                           !
         print*,''                                                  !
         stop                                                       !
        endif                                                       !
        if(nbintyp(ii).le.0) then                                   !
         call printerror(1,684999277,'bookhistos.F        ')        !
         print*,'nbintyp out of range!'                             !
         print*,'ii, nbintyp(ii): ',ii,' ',nbintyp(ii)              !
         print*,'nbinvarMAX: ',nbinvarMAX                           !
         print*,''                                                  !
         stop                                                       !
        endif                                                       !
       enddo                                                        ! 
                                                                    !
       ! Set "Extra Cut" plot limits and bins to standard           !
       ! if not specified otherwise above.                          !
       do ll=1,nvar                                                 !
        do ii=1,extyp                                               !
         do kk=1,nbintyp(ll)                                        !
          if(hmin(ll,kk,ii).eq.-999.0) hmin(ll,kk,ii) =             !
     |                                 hmin(ll,kk,0)                !
          if(hmax(ll,kk,ii).eq.-998.0) hmax(ll,kk,ii) =             !
     |                                 hmax(ll,kk,0)                !
          if(nbin(ll,kk,ii).eq.1) nbin(ll,kk,ii) = nbin(ll,kk,0)    !
         enddo                                                      !
        enddo                                                       !
       enddo                                                        !
      !ivivivivivivivivivivivivivivivivivivivivivivivivivivivivivivi!
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!



      !============================================================================!
      !============================================================================!
      !----------------------------------------------------------------------------!
      ! This section makes settings for gethistid.F
      
      !------------------------------------!
      ! Event type:
      !    0 = CC DIS (all)
      !    1 = OS DiMu
      !    2 = LS DiMu
      !    3 = OS Mu + <other>
      nval(1)=nevtsw
      !------------------------------------!
      ! Main Cuts:
      nval(2)=ncuthists
      !------------------------------------!
      ! Extra Cuts:
      nval(3)=extyp
      !------------------------------------!
      ! Variables:
      nval(4)=nvar
      !------------------------------------!
      ! Bins (must be set within variable loop):
      !------------------------------------!
      !====================================!
      ! Write set info to files:
223   format(a10,I10)
224   format('nval(',I3,'): ',I10)
      call FindOpenUnit(UN)
      ! nSet file:
      open(UN,file='outputs/hbook/settings/settings_nset.txt',
     |                                           status='UNKNOWN')
      write(UN,223) 'nSet:     ',nSet
      close (UN)
      ! nval file:
      open(UN,file='outputs/hbook/settings/settings_nval.txt',
     |                                           status='UNKNOWN')
      do ii=1,nSet-1
      write(UN,224) ii,nval(ii) 
      enddo
      close(UN)
      ! nval(5) values:
      open(UN,file='outputs/hbook/settings/settings_nval5.txt',
     |                                           status='UNKNOWN')
      write(UN,'(I10)') nvar
      do ii=1,nvar
      write(UN,'(I4,I10)') ii,nbintyp(ii)
      enddo
      close(UN)
      !====================================!
      ! Write switch info to file:
      open(UN,file='outputs/hbook/settings/evtsw.txt',status='UNKNOWN')
      do ii=1,nevtyp
       write(UN,'(I5)') evtsw(ii)
      enddo
      close(UN)
      open(UN,file='outputs/hbook/settings/cuthists.txt',
     |                                           status='UNKNOWN')
      do ii=1,ncuts
       write(UN,'(I5)') cuthists(ii)
      enddo
      close(UN)
      !====================================!
      ! Write user-level input values to file
      ! (not used in program, just set and written)
      open(UN,file='outputs/hbook/settings/evtidx.txt',status='UNKNOWN')
      do ii=1,nevtyp
       write(UN,'(I5)') evtidx(ii)
      enddo
      close(UN)
      !====================================!
      ! Write all array sizes to file:
301   format(I5,A15)
      open(UN,file='outputs/hbook/settings/array_sizes.txt',
     |                                            status='UNKNOWN')
      write(UN,301) ncuts,  ' : ncuts       '
      write(UN,301) ndetsec,' : ndetsec     '
      write(UN,301) nevtyp, ' : nevtyp      '
      write(UN,301) nncand, ' : nncand      '
      write(UN,301) nvar,   ' : nvar        '
      write(UN,301) nextyp, ' : nextyp      '
      close(UN)
      !----------------------------------------------------------------------------!
      !============================================================================!
      !============================================================================!

      !ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
      !ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
      ! This section makes settings for getcheckid.F

        !--------------------!
        ! Extra cuts
        nvalext(1)=nextyp
        !--------------------!
        ! Detector Sections:
        !  0 = DC
        !  1 = Other
        !  2 = Coil
        !  3 = Upstream
        nvalext(2)=ndetsw
        !--------------------!
        ! Ncand:
        ! 2 --> ncand=2
        ! 3 --> ncand=3
        ! 4 --> ncand=4
        ! 5 --> ncand={3,4}
        nvalext(3)=nncndsw
        !--------------------!
323   format(a10,I10)
324   format('nvalext(',I3,'): ',I10)
      call FindOpenUnit(UN)
      ! nSet file:
      open(UN,file='outputs/hbook/settings/settings_nsetext.txt',
     |                                    status='UNKNOWN')
      write(UN,323) 'nSetext:  ',nSetext
      close (UN)
      ! nval file:
      open(UN,file='outputs/hbook/settings/settings_nvalext.txt',
     |                                    status='UNKNOWN')
      do ii=1,nSetext
      write(UN,324) ii,nvalext(ii) 
      enddo
      close(UN)
      !====================================!
      ! Write switch info to file:
      open(UN,file='outputs/hbook/settings/detsw.txt',status='UNKNOWN')
      do ii=1,ndetsec
       write(UN,'(I5)') detsw(ii)
      enddo
      close(UN)
      open(UN,file='outputs/hbook/settings/ncndsw.txt',status='UNKNOWN')
      do ii=1,nncand
       write(UN,'(I5)') ncndsw(ii)
      enddo
      close(UN)
      !====================================!
      ! Write user-level input values to file
      ! (not used in program, just set and written)
      open(UN,file='outputs/hbook/settings/detidx.txt',status='UNKNOWN')
      do ii=1,ndetsec
       write(UN,'(I5)') detidx(ii)
      enddo
      close(UN)
      open(UN,file='outputs/hbook/settings/ncndidx.txt',
     |                                                 status='UNKNOWN')
      do ii=1,nncand
       write(UN,'(I5)') ncndidx(ii)
      enddo
      close(UN)
      !ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
      !ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


      !============================================================================!
      !============================================================================!
      !---------------------------  Kinematic Plots  ------------------------------!
      ! ID = SCCTTbb##   Where
      !    S = Event Characterization
      !   CC = Cut number
      !   TT = Cut type
      !   bb = bin type
      !   ## = Variable number
      ! Set extra cut tags in case the user forgets to:
       do ii=1,extyp
        exttag(ii)='ERROR Value Not Set!!    '
       enddo
      ! Zeta cut:
      write(unit=zcut,fmt='(F5.3)') zetaS
      ! Cut tags:
      excuttag(1)='                 '
      excuttag(2)=' Mass Blind      '
      excuttag(3)=' Mass Sig        '
      excuttag(4)=' Mass Norm Reg.  '
      ! Detector sections:
      dettag(1)=' -DC-  '
      dettag(2)='-Other-'
      dettag(3)='-Coil- '
      dettag(4)='-Upstr-'
      ! Ncand type:
      ncandtag(1)=' 2trk  '
      ncandtag(2)=' 3trk  '
      ncandtag(3)=' 4trk  '
      ncandtag(4)='34trk  '
      !---------------------------------------!
      ! Set extra tag values:
      do ii=1,nextyp !--> Loop over extra cuts
       idxdet=0
       do jj=1,ndetsec !--> Loop over detector sections
       if(detsw(jj).eq.1) then
        idxdet=idxdet+1
        idxncnd=0
        do kk=1,nncand !--> Loop over ncand values
        if(ncndsw(kk).eq.1) then
         idxncnd=idxncnd+1
        
         idx=getcheckid(ii,idxdet,idxncnd)
         exttag(idx)=ncandtag(kk)//dettag(jj)//excuttag(ii)
        endif
        enddo
       endif
       enddo
      enddo
      !---------------------------------------!
      !---------------------------------------!
      ! Event characterization tags:
      chartag(1)='(NuMuCC)'
      chartag(2)='(OSDiMu)'
      chartag(3)='(LSDiMu)'
      chartag(4)='(OSMuX) '
      !---------------------------------------!

      idxevt=0
      do tt=1,nevtyp !--> Event type:
      if(evtsw(tt).eq.0) goto 55
      idxevt=idxevt+1
                !    1 = CC DIS (all)
                !    2 = OS DiMu
                !    3 = LS DiMu
                !    4 = OS Mu + X
      idxcut=0
      do ii=1,ncuts  !--> After Different Cuts 
       if(cuthists(ii).eq.0) goto 44
       idxcut=idxcut+1
       do ll=1,nvar !-->  Variables
        do jj=1,nbintyp(ll) !--> Bin Types
         do kk=1,extyp !--> Extra Cut Types
          hid=gethistid(idxevt,idxcut,kk,ll,jj)
          hname=Var_name(ll)//' '//bintag(ll,jj)//' '//chartag(tt)//
     |                        ' '//exttag(kk)
          call hbook1(hid,hname,nbin(ll,jj,kk),
     |                          hmin(ll,jj,kk),
     |                          hmax(ll,jj,kk),0.0)
         enddo
        enddo
       enddo
44     continue
      enddo
55    continue
      enddo
      !----------------------------------------------------------------------------!
      !============================================================================!
      !============================================================================!


      
      call hidopt(0,'STAT')
      call hbarx(0)


      return
      end
