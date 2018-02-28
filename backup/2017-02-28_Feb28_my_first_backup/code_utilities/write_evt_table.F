      subroutine write_evt_table(evtID,run,event,
     |          posx,posy,posz,
     |          Mparent,Eparent,
     |          ZetaParent,
     |          Pasym,Phi12,
     |          P1,P2,
     |          Eneut,PneutAsym,Ehcal,
     |          Zstart1,Zstart2)
      implicit none

      integer evtID(2)
      integer run,event
      double precision posx,posy,posz
      double precision Mparent,Eparent
      double precision ZetaParent
      double precision Pasym,Phi12
      double precision P1,P2
      double precision Eneut,PneutAsym
      double precision Ehcal
      real Zstart1,Zstart2

      integer ID
      integer lrun,levt
      character*10 runch,evtch
      character*80 filename

      integer maxcnt
      parameter (maxcnt=24) !--> Number of events to include in reach run/info file
      integer evtcnt
      save evtcnt
      data evtcnt/0/
      integer fcount
      save fcount
      data fcount/1/



      !=============================================================!
      !=============================================================!
      ! Create an individual run information file for the event:

      ! Write run and event numbers to character variables:
      write(unit=runch,fmt='(I10)') run
      write(unit=evtch,fmt='(I10)') event
      ! left-align the strings
      call leftalg(runch)
      call leftalg(evtch)
      ! get the string lengths
      lrun=LEN_TRIM(runch)
      levt=LEN_TRIM(evtch)

      !-----------------------------------------------------!
      ! Write first table file (t1) with:
      ! posx,posy,posz,
      ! Mparent,Eparent,
      ! Zetaparent,
      ! P1,P2
      ! 
      call FindOpenUnit(ID)
      filename='outputs/evtpic/'//runch(1:lrun)//'-'//
     |                            evtch(1:levt)//'-v1.tex'
      open(ID,file=filename,status='UNKNOWN')
      !-------
      ! Print table header lines:
      write(ID,*) '\\small{'
      write(ID,*) '\\begin{tabular}{||c c c|c c c|c c||}'
      write(ID,*) '\\hline'
      write(ID,*) '\\textbf{XVR} & \\textbf{YVR} & \\textbf{ZVR} & 
     | {\\color{dblue}$\\boldsymbol{M_{\\mu\\mu}}$} & 
     | {\\color{dblue}$\\boldsymbol{E_{\\mu\\mu}}$} & 
     | {\\color{dblue}$\\boldsymbol{\\zeta_{\\mu\\mu}}$} &
     | {\\color{red}$\\boldsymbol{P_-}$} & 
     | {\\color{red}$\\boldsymbol{P_+}$} \\\\ [0.5ex]'
      write(ID,*) '\\hline\\hline'
      !---
      ! Print table entries:
11    format(3('{',F6.1,'} & '),  ! Vertex
     | '{\\color{dblue}',F5.3,'} & ', ! Mparent
     | '{\\color{dblue}',F6.2,'} & ', ! Eparent
     | '{\\color{dblue}',F6.4,'} & ', ! ZetaParent
     | '{\\color{red}',F6.2,'} & ', !P-
     | '{\\color{red}',F6.2,'}', !P+
     | ' \\\\ [0.5ex]') 
      write(ID,11) posx,posy,posz,
     |             Mparent,Eparent,
     |             Zetaparent,
     |             P1,P2
      !---
      ! Print table footer lines:
      write(ID,*) '\\hline'
      write(ID,*) '\\end{tabular}'
      write(ID,*) '}'
      close(ID)
      !-----------------------------------------------------!
      ! Write second table file (t2) with:
      ! Pasym,Phi12,
      ! Enuet,PneutAsym
      ! 
      filename='outputs/evtpic/'//runch(1:lrun)//'-'//
     |                            evtch(1:levt)//'-v2.tex'
      open(ID,file=filename,status='UNKNOWN')
      !-------
      ! Print table header lines:
      write(ID,*) '\\small{'
      write(ID,*) '\\begin{tabular}{||c c|c c|c|c c||}'
      write(ID,*) '\\hline'
      write(ID,*) '\\textbf{\\color{dviolet}Pasym} & 
     | {\\color{dviolet}$\\boldsymbol{\\phi_{12}}$} & 
     | {\\color{dviolet}\\textbf{Eneut}} & 
     | {\\color{dviolet}\\textbf{PAN}} & 
     | {\\color{dviolet}\\textbf{Ehcal}} &
     |  Zstart1 & Zstart2 \\\\ [0.5ex]'
      write(ID,*) '\\hline\\hline'
      !---
      ! Print table entries:
12    format(
     | '{\\color{dviolet}',F6.2,'} & ', ! Pasym
     | '{\\color{dviolet}',F5.1,'} & ', ! Phi12
     | '{\\color{dviolet}',F7.2,'} & ', ! Eneut 
     | '{\\color{dviolet}',F7.3,'} & ', ! PAN 
     | '{\\color{dviolet}',F7.2,'} & ', ! Ehcal 
     | F8.2,'  & ', ! Zstart1 
     | F8.2,' ', ! Zstart2 
     | ' \\\\ [0.5ex]') 
      write(ID,12) Pasym,Phi12,
     |             Eneut,PneutAsym,
     |             Ehcal,zstart1,zstart2
      !---
      ! Print table footer lines:
      write(ID,*) '\\hline'
      write(ID,*) '\\end{tabular}'
      write(ID,*) '}'
      close(ID)
      !=============================================================!
      !=============================================================!

      ! Write to the summary info table file:
      ID=evtID(2)
22    format( 2(I10,' & '), ! Run/Event
     | 3('{',F6.1,'} & '),  ! Vertex
     | '{\\color{dblue}',F5.3,'} & ', ! Mparent
     | '{\\color{dblue}',F6.2,'} & ', ! Eparent
     | '{\\color{dblue}',F6.4,'} & ', ! ZetaParent
     | '{\\color{dviolet}',F6.2,'} & ', ! Pasym
     | '{\\color{dviolet}',F5.1,'} & ', ! Phi12
     | '{\\color{red}',F6.2,'} & ', !P-
     | '{\\color{red}',F6.2,'} & ', !P+
     | '{\\color{dviolet}',F7.2,'} & ', ! Eneut 
     | '{\\color{dviolet}',F7.3,'} & ', ! PAN 
     | '{\\color{dviolet}',F7.2,'} & ', ! Ehcal 
     | '{\\color{dblue}',F6.1,'} & ', ! Zstart1
     | '{\\color{dblue}',F6.1,'}', ! Zstart2
     | ' \\\\ [0.5ex]') 
      write(ID,22) run,event,
     |             posx,posy,posz,
     |             Mparent,Eparent,
     |             Zetaparent,
     |             Pasym,Phi12,
     |             P1,P2, 
     |             Eneut,PneutAsym,
     |             Ehcal,
     |             Zstart1,Zstart2

      ! Open a new file if needed
      evtcnt=evtcnt+1
      if(evtcnt.ge.maxcnt) then
          fcount=fcount+1
          evtcnt=0
          call closeevtpic(evtID) 
          call openevtpic(evtID,fcount)
      endif


      return
      end
