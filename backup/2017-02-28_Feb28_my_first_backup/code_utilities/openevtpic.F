      subroutine openevtpic(evtID,fnum)
      implicit none

      integer evtID(2)
      integer fnum !--> File number

      character*10 fnumch
      integer fnuml

      integer ID


      if(fnum.eq.1) then
      ! Open run/event list
      call FindOpenUnit(evtID(1))
      open(evtID(1),file='outputs/evtpic/runevent.txt',
     |                              status='UNKNOWN')
      endif
      
      !-------------------------------------------------------------------
      ! Open run info. summary:

      write(unit=fnumch,fmt='(I10)') fnum
      call leftalg(fnumch)
      fnuml=LEN_TRIM(fnumch)


      call FindOpenUnit(evtID(2))
      ID=evtID(2)
      open(ID,file='outputs/evtpic/runinfo'//fnumch(1:fnuml)//'.tex',
     |                              status='UNKNOWN')
      !-------
      ! Print table header lines:
      write(ID,*) '\\small{'
      write(ID,*) '\\begin{tabular}{||c c|c c c|c c c|c c|c c|ccc|cc||}'
      write(ID,*) '\\hline'
      write(ID,*) ' \\textbf{Run} & \\textbf{Event} &
     |  \\textbf{XVR} & \\textbf{YVR} & \\textbf{ZVR} & 
     | {\\color{dblue}$\\boldsymbol{M_{\\mu\\mu}}$} & 
     | {\\color{dblue}$\\boldsymbol{E_{\\mu\\mu}}$} & 
     | {\\color{dblue}$\\boldsymbol{\\zeta_{\\mu\\mu}}$} &
     | \\textbf{\\color{dviolet}Pasym} & 
     | {\\color{dviolet}$\\boldsymbol{\\phi_{12}}$} & 
     | {\\color{red}$\\boldsymbol{P_-}$} & 
     | {\\color{red}$\\boldsymbol{P_+}$} & 
     | {\\color{dviolet}\\textbf{Eneut}} & 
     | {\\color{dviolet}\\textbf{PAN}} & 
     | {\\color{dviolet}\\textbf{HCalNeu}} &
     | \\textbf{Zstart1} & \\textbf{Zstart2} \\\\ [0.5ex]'
      write(ID,*) '\\hline\\hline'
      !---

      return
      end
