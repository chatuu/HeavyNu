      integer function getid(cc,ii,ee,dd,nn,bb,vv)
      implicit none

      integer cc,ii,ee,dd,nn,bb,vv 

C     cc --> Event topology
C     ii --> Cut number
C     ee --> Extra cut
C     dd --> Detector section
C     nn --> Ncand value
C     bb --> Bin number
C     vv --> Variable number

C     This function simply translates the event topology (cc),
C     the detector section (dd), and the ncand type (nn) from
C     values meaningful to the user to those used by the 
C     program (which are simply indexed sequentially from 1).
C     The values used are set in main.F at the bottom of the 
C     user settings in the section labeled:
C     "Set user index values for plotting"

C     It also checks to make sure that the requested plot
C     is available, given the values of "Plotting switches"
C     found in main.F.

C     If one enters a raw histogram ID number followed by
C     6 zeors then the parameters associated with that
C     ID are printed.

C     -------------------------
C     Error mode:
C       q = quiet
C       v = verbose
      character*1 errmod
      parameter (errmod='q')
C     -------------------------

      integer uncuts
      parameter (uncuts=29)
      integer undetsec
      parameter (undetsec=4)
      integer unevtyp
      parameter (unevtyp=4)
      integer unncand
      parameter (unncand=4)
      integer unvar
      parameter (unvar=47)

      integer ncuts
      save ncuts
      integer ndetsec
      save ndetsec
      integer nevtyp
      save nevtyp
      integer nncand
      save nncand
      integer nvar
      save nvar
      integer nextyp
      save nextyp

c      Tag for the first run:
      logical firstrun
      save firstrun
      data firstrun/.true./

      integer cutsw(uncuts)
      save cutsw
      integer detsw(undetsec)
      save detsw
      integer detidx(undetsec)
      save detidx
      integer evtsw(unevtyp)
      save evtsw
      integer evtidx(unevtyp)
      save evtidx
      integer ncndsw(unncand)
      save ncndsw
      integer ncndidx(unncand)
      save ncndidx

C     Parameter Array Index:
      integer ddidx
      integer ccidx
      integer nnidx

C     Index among "on" values:
      integer iival
      integer ddval
      integer ccval
      integer nnval

      integer extid

      integer getcheckid
      integer gethistid

      integer UN
      character*50 TOPDIR
      integer leng

c     Number of binnings:
      integer nval5(unvar)
      save nval5

      character*80 filename

      integer junkint

      integer jj

      getid=0

C     Give parameters if actual histogram ID is given:
      if (ii.eq.0 .and. ee.eq.0 .and. dd.eq.0 .and.
     |    nn.eq.0 .and. bb.eq.0 .and. vv.eq.0      ) then
       call getparam(cc)
       return
      endif


      if (firstrun) then
C     Check array sizes:
      call findunit(UN)
      call gettopdir(UN,TOPDIR,leng)
      open(UN,file=TOPDIR(1:leng)//'/settings/array_sizes.txt',
     |                                                  status='OLD')
      read(UN,'(I5)') ncuts
      read(UN,'(I5)') ndetsec
      read(UN,'(I5)') nevtyp
      read(UN,'(I5)') nncand
      read(UN,'(I5)') nvar
      read(UN,'(I5)') nextyp
      close(UN)
      if(ncuts.ne.uncuts) then 
       call errarrsize(uncuts,ncuts,'uncuts  ')
       return
      endif
      if(ndetsec.ne.undetsec) then
       call errarrsize(undetsec,ndetsec,'undetsec')
       return
      endif
      if(nevtyp.ne.unevtyp) then
       call errarrsize(unevtyp,nevtyp,'unevtyp ')
       return
      endif
      if(nncand.ne.unncand) then
       call errarrsize(unncand,nncand,'unncand ')
       return
      endif

C     Load On/Off switches:
      filename=TOPDIR(1:leng)//'/settings/cuthists.txt'
      call readintarray(UN,filename,uncuts,cutsw)
      filename=TOPDIR(1:leng)//'/settings/detsw.txt'
      call readintarray(UN,filename,undetsec,detsw)
      filename=TOPDIR(1:leng)//'/settings/evtsw.txt'
      call readintarray(UN,filename,unevtyp,evtsw)
      filename=TOPDIR(1:leng)//'/settings/ncndsw.txt'
      call readintarray(UN,filename,unncand,ncndsw)

C     Load user-level values:
      filename=TOPDIR(1:leng)//'/settings/detidx.txt'
      call readintarray(UN,filename,undetsec,detidx)
      filename=TOPDIR(1:leng)//'/settings/evtidx.txt'
      call readintarray(UN,filename,unevtyp,evtidx)
      filename=TOPDIR(1:leng)//'/settings/ncndidx.txt'
      call readintarray(UN,filename,unncand,ncndidx)

C     Load number of bins to check if given valid value later:
      if(nvar.ne.unvar) then
        print*,''
        print*,'Must change unvar value in getid'
        print*,'to match value of nvar.'
        print*,'nvar: ',nvar
        print*,'unvar: ',unvar
        return
      endif
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nval5.txt',
     |                                                  status='OLD')
       read(UN,'(I10)') junkint
       do jj=1,unvar
       read(UN,'(I4,I10)') junkint,nval5(jj)
       enddo
       close(UN)


      firstrun=.false.
      endif

C     0000000000000000000000000000000000000000000000
C      Get index values from user input:

C      Detector section (ddidx):
      call getindexval(undetsec,detidx,ddidx,dd)
       if (ddidx.eq.0) then
        call errnousrval(dd,'detector section!   ',
     |                      'detidx.txt          ')
        return
       endif
C      Event topology (ccidx):
      call getindexval(unevtyp,evtidx,ccidx,cc)
       if (ccidx.eq.0) then 
        call errnousrval(cc,'event topology!     ',
     |                      'evtidx.txt          ')
        return
       endif
C      Ncand value (nnidx):
      call getindexval(unncand,ncndidx,nnidx,nn)
       if (nnidx.eq.0) then
        call errnousrval(nn,'ncand value!        ',
     |                      'ncndidx.txt         ')
        return
       endif
C     0000000000000000000000000000000000000000000000



C     -----------------------------------------------------------------
C      Change the index value to omit values that have been
C      switched off, and ensure that this value is on.

C      Cut number (iival):
      if (cutsw(ii).ne.1) then 
        call errnotavail(ii,ii,'cut value           ','cutsw.txt   ',
     |                                                          errmod)
        return
      endif
      call getavailindex(ii,uncuts,cutsw,iival)
C      Detctor section (ddval):
      if (detsw(ddidx).ne.1) then 
        call errnotavail(ddidx,dd,'detector section    ','detsw.txt   ',
     |                                                          errmod)
        return
      endif
      call getavailindex(ddidx,undetsec,detsw,ddval)
C      Event topology (ccval):
      if (evtsw(ccidx).ne.1) then
        call errnotavail(ccidx,cc,'event topology      ','evtsw.txt   ',
     |                                                          errmod)
        return
      endif
      call getavailindex(ccidx,unevtyp,evtsw,ccval)
C      Ncand value (nnval):
      if (ncndsw(nnidx).ne.1) then
        call errnotavail(nnidx,nn,'ncand value         ','ncndsw.txt  ',
     |                                                          errmod)
        return
      endif
      call getavailindex(nnidx,unncand,ncndsw,nnval)

C     Check that input values are avilable for other parameters:
      if(ee.le.0.or.ee.gt.nextyp) then
        print*,''
        print*,'Given extra cut number not available!'
        print*,'Given ee: ',ee
        print*,'nextyp: ',nextyp
        return
      endif
      if(vv.le.0.or.vv.gt.nvar) then
        print*,''
        print*,'Given variable number not available!'
        print*,'Given vv: ',vv
        print*,'nvar: ',nvar
        return
      endif
      if(bb.le.0.or.bb.gt.nval5(vv)) then
        print*,''
        print*,'Given binning not available!'
        print*,'Given variable (vv): ',vv
        print*,'Given binning (bb): ',bb
        print*,'Number of binnings for variable: ',nval5(vv)
        return
      endif


C      Get the extra cut ID number:
      extid=getcheckid(ee,ddval,nnval)

C      Histogram ID number to be read from each file:
      getid=gethistid(ccval,iival,extid,vv,bb)


      return 
      end


C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      integer function getcheckid(v1,v2,v3)
      implicit none

c      This function simply returns the integer in a
c      sequential set of numbers mapped to the input
c      values.

c      The function is formatted so that it can be used
c      by PAW as well (no special FORTRAN functions,
c      no exclamation comments).

      integer v1,v2,v3

      integer nSet
      save nSet

c      ooooooooooooooooooooooooooooooooooooo
c      Settings read from file:
c
c      These upper limits  must match nSet=3
c
c      Number of possible values for each set:
      integer nval(0:3) 
      save nval
c      Current set values given by v1, v2...
      integer ival(3)
c      ooooooooooooooooooooooooooooooooooooo

c      Tag for the first run:
      logical firstrun
      save firstrun
      data firstrun/.true./

      integer iset
      integer ii
      integer PP
      integer UN
      integer leng
      integer junkint
      character*10 junk10
      character*14 junk14
      character*50 TOPDIR

c      Sequential histogram ID:
      integer checknum 


c      Set the input values array:
c      Extra Cuts:
      ival(1)=v1
c      Detector section:
      ival(2)=v2
c      Ncand:
      ival(3)=v3

c      Set the zeroth nval value
      nval(0)=1

c      ooooooooooooooooooooooooooooooooooooooooooooooooooo
c      Load settings from file on first running 
c      of this subroutine:
      if(firstrun) then
      call findunit(UN)
      call gettopdir(UN,TOPDIR,leng)
323    format(a10,I10)
324    format(a14,I10)
c       Get number of sets:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nsetext.txt',
     |                                                status='OLD')
       read(UN,323) junk10,nSet
       close (UN)
c       nval file:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nvalext.txt',
     |                                                status='OLD')
       do ii=1,nSet
       read(UN,324) junk14,nval(ii) 
       enddo
       close(UN)
c       Check the value of nvar and nSet for the saved array sizes:
       if(nSet.ne.3) then
        print*,''
        print*,'Error! Change the array sizes in getcheckid.F'
        print*,'Error number: 44f88h3hodhh'
        print*,'nSet: ',nSet
        print*,'Stopping program!'
        print*,''
        return
       endif
       firstrun=.false.
      endif
c      ooooooooooooooooooooooooooooooooooooooooooooooooooo

      checknum=1
      do iset=1,nSet
        PP=1
        do ii=0,(iset-1)
         PP=PP*nval(ii)
        enddo
        checknum=checknum+(ival(iset)-1)*PP
      enddo

      getcheckid=checknum

      return
      end



C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      integer function gethistid(v1,v2,v3,v4,v5)
      implicit none

c      This function simply returns the integer in a
c      sequential set of numbers mapped to the input
c      values.

c      The function is formatted so that it can be used
c      by PAW as well (no special FORTRAN functions,
c      no exclamation comments).

      integer v1,v2,v3,v4,v5

      integer nSet
      save nSet
      integer nvar
      save nvar

c      ooooooooooooooooooooooooooooooooooooo
c      Settings read from file:
c
c      These upper limits  must match nSet=5
c      and nvar=47
c
c      Number of possible values for each set:
      integer nval(0:5) 
      save nval
c      Different values of nval(5):
      integer nval5(47)
      save nval5
c      Current set values given by v1, v2...
      integer ival(5)
c      ooooooooooooooooooooooooooooooooooooo

c      Tag for the first run:
      logical firstrun
      save firstrun
      data firstrun/.true./

      integer iset
      integer ii
      integer PP
      integer UN
      integer leng
      integer junkint
      character*10 junk10
      character*11 junk11
      character*50 TOPDIR

c      Sequential histogram ID:
      integer hid 


c      Set the input values array:
c      Event type (os,ls...)
      ival(1)=v1
c      Main Cuts:
      ival(2)=v2
c      Extra Cuts:
      ival(3)=v3
c      Variables:
      ival(4)=v4
c      Bins:
      ival(5)=v5

c      Set the zeroth nval value
      nval(0)=1


c      ooooooooooooooooooooooooooooooooooooooooooooooooooo
c      Load settings from file on first running 
c      of this subroutine:
      if(firstrun) then
      call findunit(UN)
      call gettopdir(UN,TOPDIR,leng)
223    format(a10,I10)
224    format(a11,I10)
c       Get number of sets:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nset.txt',
     |                                                  status='OLD')
       read(UN,223) junk10,nSet
       close (UN)
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nval5.txt',
     |                                                  status='OLD')
       read(UN,'(I10)') nvar
       close(UN)
c       nval file:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nval.txt',
     |                                                  status='OLD')
       do ii=1,nSet-1
       read(UN,224) junk11,nval(ii) 
       enddo
       close(UN)
c       nval(5) values:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nval5.txt',
     |                                                  status='OLD')
       read(UN,'(I10)') junkint
       do ii=1,nvar
       read(UN,'(I4,I10)') junkint,nval5(ii)
       enddo
       close(UN)
c       Check the value of nvar and nSet for the saved array sizes:
       if(nvar.ne.47.or.nSet.ne.5) then
        print*,''
        print*,'Error! Change the array sizes in gethistid.F'
        print*,'Error number: 39f88h3hodhh'
        print*,'nvar: ',nvar
        print*,'nSet: ',nset
        print*,'Stopping program!'
        print*,''
        return
       endif
       firstrun=.false.
      endif
c      ooooooooooooooooooooooooooooooooooooooooooooooooooo

c      Set nval(5):
      nval(5)=nval5(v4)

c      Start histogram IDs at 1000 to keep from overlapping
c      with FORTRAN file unit numbers.
      hid=1000
      do iset=1,nSet
        PP=1
        do ii=0,(iset-1)
         PP=PP*nval(ii)
        enddo
        hid=hid+(ival(iset)-1)*PP
      enddo

      gethistid=hid

      return
      end

C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine getparam(NN)
      implicit none

c      This function returns the integer parameters given
c      the sequential integer mapped to the parameters.

c      The function is formatted so that it can be used
c      by PAW as well (no special FORTRAN functions,
c      no exclamation comments).

      integer NN

      integer param(7)

      integer nSeth
      save nSeth
      integer nvarh
      save nvarh


c      ooooooooooooooooooooooooooooooooooooo
c      Settings read from file for the histID:
c
c      These upper limits  must match nSet=5
c      and nvar=47
c
c      Number of possible values for each set:
      integer nvalh(0:5) 
      save nvalh
c      Different values of nval(5):
      integer nval5h(47)
      save nval5h
c      Current set values given by v1, v2...
      integer ivalh(5)
c      ooooooooooooooooooooooooooooooooooooo

      integer nSetc
      save nSetc

c      ooooooooooooooooooooooooooooooooooooo
c      Settings read from file:
c
c      These upper limits  must match nSet=3
c
c      Number of possible values for each set:
      integer nvalc(0:3) 
      save nvalc
c      Current set values given by v1, v2...
      integer ivalc(3)
c      ooooooooooooooooooooooooooooooooooooo



c      Tag for the first run:
      logical firstrun
      save firstrun
      data firstrun/.true./

      integer iset
      integer ii
      integer PP
      integer UN
      integer junkint
      character*14 junk14
      character*11 junk11
      character*10 junk10
      character*50 TOPDIR
      integer leng


      integer uncuts
      parameter (uncuts=29)
      integer undetsec
      parameter (undetsec=4)
      integer unevtyp
      parameter (unevtyp=4)
      integer unncand
      parameter (unncand=4)

      integer ncuts
      integer ndetsec
      integer nevtyp
      integer nncand

      integer cutsw(uncuts)
      save cutsw
      integer detsw(undetsec)
      save detsw
      integer detidx(undetsec)
      save detidx
      integer evtsw(unevtyp)
      save evtsw
      integer evtidx(unevtyp)
      save evtidx
      integer ncndsw(unncand)
      save ncndsw
      integer ncndidx(unncand)
      save ncndidx

C       Histogram ID parameters (valh(3) will get broken down with the check ID)
      integer valh(5)
C       Check ID prameters:
      integer valc(3)

      character*80 filename

      integer ccidx,iiidx,ddidx,nnidx
      integer ccisw,iiisw,ddisw,nnisw
      integer ccusr,iiusr,ddusr,nnusr

c      Set the zeroth nvalh value
      nvalh(0)=1
      nvalc(0)=1


c      ooooooooooooooooooooooooooooooooooooooooooooooooooo
c      Load settings from file on first running 
c      of this subroutine:
      if(firstrun) then
      call findunit(UN)
      call gettopdir(UN,TOPDIR,leng)
423    format(a10,I10)
424    format(a11,I10)
c       Get number of sets:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nset.txt',
     |                                                  status='OLD')
       read(UN,423) junk10,nSeth
       close (UN)
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nval5.txt',
     |                                                  status='OLD')
       read(UN,'(I10)') nvarh
       close(UN)
c       nval file:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nval.txt',
     |                                                  status='OLD')
       do ii=1,nSeth-1
       read(UN,424) junk11,nvalh(ii) 
       enddo
       close(UN)
c       nval(5) values:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nval5.txt',
     |                                                  status='OLD')
       read(UN,'(I10)') junkint
       do ii=1,nvarh
       read(UN,'(I4,I10)') junkint,nval5h(ii)
       enddo
       close(UN)
c       Check the value of nvarh and nSet for the saved array sizes:
       if(nvarh.ne.47.or.nSeth.ne.5) then
        print*,''
        print*,'Error! Change the array sizes in getparam.F'
        print*,'Error number: 39f88h3hodhh'
        print*,'nvarh: ',nvarh
        print*,'nSeth: ',nseth
        print*,'Stopping program!'
        print*,''
        return
       endif
C       ===============================================
C       -------------------------------
C       Check variables:
523    format(a10,I10)
524    format(a14,I10)
c       Get number of sets:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nsetext.txt',
     |                                                status='OLD')
       read(UN,523) junk10,nSetc
       close (UN)
c       nval file:
       open(UN,file=TOPDIR(1:leng)//'/settings/settings_nvalext.txt',
     |                                                status='OLD')
       do ii=1,nSetc
       read(UN,524) junk14,nvalc(ii) 
       enddo
       close(UN)
c       Check the value of nvar and nSet for the saved array sizes:
       if(nSetc.ne.3) then
        print*,''
        print*,'Error! Change the array sizes in getparam.F'
        print*,'Error number: 435121hodhh'
        print*,'nSetc: ',nSetc
        print*,'Stopping program!'
        print*,''
        return
       endif
C       ===============================================
      open(UN,file=TOPDIR(1:leng)//'/settings/array_sizes.txt',
     |                                                  status='OLD')
      read(UN,'(I5)') ncuts
      read(UN,'(I5)') ndetsec
      read(UN,'(I5)') nevtyp
      read(UN,'(I5)') nncand
      close(UN)
      if(ncuts.ne.uncuts) then 
       call errarrsize(uncuts,ncuts,'uncuts  ')
       return
      endif
      if(ndetsec.ne.undetsec) then
       call errarrsize(undetsec,ndetsec,'undetsec')
       return
      endif
      if(nevtyp.ne.unevtyp) then
       call errarrsize(unevtyp,nevtyp,'unevtyp ')
       return
      endif
      if(nncand.ne.unncand) then
       call errarrsize(unncand,nncand,'unncand ')
       return
      endif

C     Load On/Off switches:
      filename=TOPDIR(1:leng)//'/settings/cuthists.txt'
      call readintarray(UN,filename,uncuts,cutsw)
      filename=TOPDIR(1:leng)//'/settings/detsw.txt'
      call readintarray(UN,filename,undetsec,detsw)
      filename=TOPDIR(1:leng)//'/settings/evtsw.txt'
      call readintarray(UN,filename,unevtyp,evtsw)
      filename=TOPDIR(1:leng)//'/settings/ncndsw.txt'
      call readintarray(UN,filename,unncand,ncndsw)

C     Load user-level values:
      filename=TOPDIR(1:leng)//'/settings/detidx.txt'
      call readintarray(UN,filename,undetsec,detidx)
      filename=TOPDIR(1:leng)//'/settings/evtidx.txt'
      call readintarray(UN,filename,unevtyp,evtidx)
      filename=TOPDIR(1:leng)//'/settings/ncndidx.txt'
      call readintarray(UN,filename,unncand,ncndidx)
       firstrun=.false.
      endif
c      ooooooooooooooooooooooooooooooooooooooooooooooooooo


C      Get histID parameter values:
      do iset=1,nSeth 
        PP=1
        do ii=0,(iset-1)
         PP=PP*nvalh(ii)
        enddo
       if(iset.eq.nSeth) nvalh(iset)=nval5h(valh(iset-1))
       valh(ii)=mod(int((NN-1000)/PP),nvalh(ii))+1
      enddo
      
      
C     Get checkID parameter values from the 3rd histogramID parameter:
      do iset=1,nSetc 
        PP=1
        do ii=0,(iset-1)
         PP=PP*nvalc(ii)
        enddo
       valc(ii)=mod(int((valh(3)-1)/PP),nvalc(ii))+1
      enddo


      
C     Fill the output parameter array with base available index:
C     cc --> Event topology (base index):
      param(1)=valh(1)
C     ii --> Cut number (base index):
      param(2)=valh(2)
C     ee --> Extra cut number:
      param(3)=valc(1)
C     dd --> Detector number (base index):
      param(4)=valc(2)
C     nn --> Ncand value (base index):
      param(5)=valc(3)
C     bb --> Binning:
      param(6)=valh(5)
C     vv --> Variable:
      param(7)=valh(4)


C     Parameters 1,2,4 and 5 must be taken from the base available index
C     to the user values.  I will first simply rename them with
C     something familiar:
      ccisw=param(1)
      iiisw=param(2)
      ddisw=param(4)
      nnisw=param(5)

C     I must now find the actual array index for each:
      call findactindx(unevtyp,evtsw,ccisw,ccidx)
      call findactindx(uncuts,cutsw,iiisw,iiidx)
      call findactindx(undetsec,detsw,ddisw,ddidx)
      call findactindx(undetsec,ncndsw,nnisw,nnidx)

C     And, finally, the user values:
      param(1)=evtidx(ccidx)
      param(2)=iiidx
      param(4)=detidx(ddidx)
      param(5)=ncndidx(nnidx)


C     Print the parameters:
      write(*,'(A12,7I4)') 'Parameters: ',(param(ii),ii=1,7)

      return
      end


C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine gettopdir(UN,TOPDIR,leng)
      implicit none
      integer UN
      character*50 TOPDIR
      integer leng
      integer jj
       open(UN,file='getid.dir',status='OLD')
       read(UN,'(A50)') TOPDIR
       close(UN)
       do jj=1,50
       if(TOPDIR(jj:jj).ne.' ') leng=jj
       enddo
      return
      end
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine findunit(UN)
      implicit none
      integer UN
      integer jj
      logical UnitOpened
       UN=0
       do jj=1,99 
        if(jj.ne.5.and.jj.ne.6) then  
         INQUIRE (UNIT=jj,OPENED=UnitOpened) 
         if ((.not.UnitOpened) .and. UN.eq.0) then
          UN=jj
          goto 99 
         endif
        endif
       enddo
99     continue
      return
      end
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine errnousrval(inint,typstr,filestr)
      implicit none
      integer inint
      character*20 typstr
      character*20 filestr
       print*,''
       print*,'ERROR!!!'
       print*,'No user value found for given ',typstr
       print*,'Value given: ',inint
       print*,'Availale values in ',filestr
       print*,'Killing function'
       print*,''
      return
      end
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine errarrsize(uval,readval,uname)
      implicit none
      integer uval
      integer readval
      character*8 uname
       print*,''
       print*,'Must change ',uname,' value to ',readval,' in getid.F'
       print*,'Current value of ',uname,': ',uval
       print*,'Stopping!'
       print*,''
      return
      end
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine readintarray(UN,filename,asize,array)
      implicit none
      integer UN
      character*80 filename
      integer asize
      integer array(asize)
      integer jj
      open(UN,file=filename,status='OLD')
      do jj=1,asize
       read(UN,'(I5)') array(jj)
      enddo
      close(UN)
      return
      end
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine getindexval(asize,array,idx,valuein)
      implicit none
      integer asize
      integer array(asize)
      integer idx
      integer valuein
      integer jj
      idx=0
      do jj=1,asize
       if (valuein.eq.array(jj)) then
         idx = jj
       endif
      enddo
      return
      end
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine errnotavail(idx,inval,strtype,strfile,errmod)
      implicit none
      integer idx
      integer inval
      character*20 strtype
      character*12 strfile
      character*1 errmod
      if(errmod.eq.'v') then
       print*,''
       print*,'The ',strtype,': ',idx,' (user=',inval,')'
       print*,'is not available!'
       print*,' Switches in ',strfile
       print*,' Stopping function'
       print*,''
      endif
      return
      end
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine getavailindex(inidx,asize,array,outidx)
      implicit none
      integer inidx
      integer asize
      integer array(asize)
      integer outidx
      integer jj
      outidx=0
      do jj=1,inidx
       if (array(jj).eq.1) then
        outidx=outidx+1
       endif
      enddo
      return
      end
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine findactindx(asiz,array,idxin,idxout)
      implicit none
      integer asiz
      integer array(asiz)
      integer idxin,idxout
      integer icount
      integer jj
      icount=0
      idxout=0
      do jj=1,asiz
       if(array(jj).eq.1) icount=icount+1
       if(icount.eq.idxin.and.idxout.eq.0) idxout=jj
      enddo
      return
      end
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
