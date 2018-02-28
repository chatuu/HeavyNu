      subroutine openntuple(ntID,ntname,ID)

      implicit none
#include "ntuple.inc"

      ! This subroutine openes a PAW format ntuple.  One must set
      ! InitLREC to the correct record length of the ntuple (or set it
      ! to 0), and one must also set the block variable names at the
      ! bottom of this file.

      !  The ntuple "directory name" will be set to "//INTUPLE"

      integer ntID !--> FORTRAN File Unit Number of Ntuple
      character*100 ntname !--> File name for ntuple
      integer ID !--> ID # of ntuple block (can be gotten from paw with nt/list)

      !---------------------------------------------------!
      ! Ntuple record length (usually 1024, 4096 or 8190).
      ! If the record length is not known then set InitLREC
      ! to zero, and the program will output the correct
      ! record length for future use.
      integer InitLREC !--> Expected value of LREC
      parameter (InitLREC = 4096)
      !---------------------------------------------------!

      integer LREC !--> Record length 
      character*1 opChar !--> Character for the 4th argument of HROPEN
      integer err !--> HROPEN return code (=0 if success)


      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      ! Check input for error:                                     !
      !                                                            !
       if(ntID.eq.5.or.ntID.eq.6.or.ntID.lt.1.or.ntID.gt.99) then  !
        call printerror(1,954837224,'openntuple.F        ')        !
        print*,'Ntuple Unit number out of range.'                  !
        print*,'ntID: ',ntID                                       !
        print*,'ntname: ',ntname                                   !
        print*,'ID: ',ID                                           !
        print*,''                                                  ! 
        stop                                                       ! 
       endif                                                       !
       if(ID.le.0) then                                            !
        call printerror(1,488571192,'openntuple.F        ')        !
        print*,'Ntuple ID type out of range.'                      !
        print*,'ID: ',ID                                           !
        print*,'ntID: ',ntID                                       !
        print*,'ntname: ',ntname                                   !
        print*,''                                                  ! 
        stop                                                       ! 
       endif                                                       !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!


        !-----------------------------------------------------------------!
        ! Open the ntuple:
        LREC= InitLREC !--> If one does not know the record length of 
                       !    the ntuple (5th argument in HROPEN) then
                       !    the ntuple must be opened in "exchange mode"
                       !    by passing a 0.  However, if one simply
                       !    passes the integer 0 a segmentation fault
                       !    will occur.  This is because HROPEN returns
                       !    the value of the record length to the 5th
                       !    argument when in exchange mode.  So one must
                       !    pass a variable set to 0 instead.
                       !    Additionally, in exchange mode the 4th
                       !    argument must be 'X' (rather than the 
                       !    usual ' ').
        if(LREC.eq.0) opChar='X'
        if(LREC.ne.0) opChar=' '
        call HROPEN(ntID,'INTUPLE',ntname,opChar,LREC,err)
        ! Output the record length for future use:
        if(InitLREC.eq.0) print*,'LREC (NT record length): ',LREC 
        !-----------------------------------------------------------------!


        !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!
        ! Check for error opening ntuple:
        if(err.ne.0) then
        call printerror(1,394882185,'openntuple.F        ')
        print*,'Error when opening ntuple!'
        print*,'err: ',err
        print*,'ntID: ',ntID
        print*,'ntname: ',ntname
        print*,'ID: ',ID
        print*,''
        stop
        endif
        !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!


        call HCDIR('//INTUPLE',' ') !--> Gives the directory name for
                                    !    the opened ntuple, used to refer 
                                    !    to the ntuple in other "HR" calls.
        call HRIN(ID,999,0)
        call HBNAME(ID,' ',0,'$clear')
        !=============================================!
        ! The following information can be gotten by opening the
        ! ntuple in paw, getting the ID from nt/list,
        ! and then printing the ntuple contents with
        ! "nt/print ID" (like "nt/print 1000").
        ! One must list here the first variable in each
        ! block, and give the name of that block.
        call HBNAME(ID,'HEADER',Run,'$set')
        call HBNAME(ID,'MCGENE',XVS,'$set')
        call HBNAME(ID,'GLOBREC',ISele,'$set')
        call HBNAME(ID,'CCEVEREC',ncand,'$set')
        !=============================================!


      return
      end
