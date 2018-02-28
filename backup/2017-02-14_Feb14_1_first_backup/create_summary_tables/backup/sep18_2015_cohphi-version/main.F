      program main
      implicit none

      character*200 filename(10,3)
      character*200 inputfile(10)
      character*25 cutname(100)
      real cut(4,10,100)
      real cuts(4,10,100)

      character*2 junk
      character*20 junkl

      character*200 TOPdir
      character*21 filetag(10)

      integer ictyp,ii,jj,kk

      integer numcuts

      integer length
      integer lengthf(10,3)
      integer lengthinput(10)

      real normnc(3)
      real normcc(3)
      real normcoh(3)
      real normobg(3)
     
      character*7 tag(3)
      character*4 tag1(10)
      character*8 tag2(10)

      open (23,file='location.dat',status='UNKNOWN')
       read(23,'(A200)') TOPdir
      close(23)
    
       numcuts = 28 !--> Cannot be greater than 100 
       
      ! OBG Cut Table:
      filetag(1)="cut_cohrho_obg.txt   "
      ! Data Cut Table:
      filetag(2)="cut_cohrho_data.txt  "
      ! NCDIS Cut Table:
      filetag(3)="cut_cohrho_ncdis.txt "
      ! CCDIS Cut Table:
      filetag(4)="cut_cohrho_ccdis.txt "
      ! CohRho Cut Table:
      filetag(5)="cut_cohrho_cohrho.txt"
      ! CohPi+ Cut Table:
      filetag(6)="cut_cohrho_cohpip.txt"
      ! QE Cut Table:
      filetag(7)="cut_cohrho_qe.txt    "
      ! Coherent Phi0
      filetag(8)="cut_cohrho_cohphi.txt"
      ! Coherent Pi0
      filetag(9)="cut_cohrho_cohpi0.txt"
      ! NC-Res 
      filetag(10)="cut_cohrho_ncres.txt"

      tag(1)='ppm0.0/'
      tag(2)='ppm0.5/'
      tag(3)='ppm1.0/'


      tag1(1) = '_obg'
      tag1(2) = '__dt'
      tag1(3) = '__nc'
      tag1(4) = '__cc'
      tag1(5) = '_sig'
      tag1(6) = '_cpi'
      tag1(7) = '__qe'
      tag1(8) = '_phi'
      tag1(9) = '_pi0'
      tag1(10)= '_res'


      tag2(1) = ' OBG    '
      tag2(2) = ' Data   '
      tag2(3) = ' NCDIS  '
      tag2(4) = ' CCDIS  '
      tag2(5) = '\\cohr   '
      tag2(6) = '\\cohpip '
      tag2(7) = ' QE     '
      tag2(8) = ' CohPhi '
      tag2(9) = '\\cohp   '
      tag2(10)= ' NC-Res '


      length=LEN_TRIM(TOPdir)
      do ii=1,10
       inputfile(ii)=TOPdir(1:length)//filetag(ii)
       lengthinput(ii)=LEN_TRIM(inputfile(ii))
       do jj=1,3
       filename(ii,jj) =TOPdir(1:length)//tag(jj)//filetag(ii)
       lengthf(ii,jj)=LEN_TRIM(filename(ii,jj))
       enddo
      enddo


      !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>!
      ! Get Info. From Input Files:

      ! Read Normalizations
      open(66,file=TOPdir(1:length)//'normlist_ncdis.txt',
     |               status='OLD')
      do ii=1,3
       read(66,'(F10.4)') normnc(ii) 
      enddo
      close(66)
      open(66,file=TOPdir(1:length)//'normlist_ccdis.txt',
     |               status='OLD')
      do ii=1,3
       read(66,'(F10.4)') normcc(ii) 
      enddo
      close(66)
      open(66,file=TOPdir(1:length)//'normlist_obg.txt',
     |               status='OLD')
      do ii=1,3
       read(66,'(F10.6)') normobg(ii) 
      enddo
      close(66)
      open(66,file=TOPdir(1:length)//'normlist_coh.txt',
     |               status='OLD')
      do ii=1,3
       read(66,'(F10.4)') normcoh(ii) 
      enddo
      close(66)

      ! Read Cuts:
      do jj = 1,10 !--> Loop over input files
       print *,''
       print *,'-------------------------------------------'
       print *,'Working file: '
       print *,inputfile(jj)

       open (33,file=inputfile(jj),status='OLD')
      
       read(33,'(a2)') junk
       read(33,'(a2)') junk

       do ii=1,numcuts !--> Loop over cuts
        read(33,55) cutname(ii),cut(1,jj,ii),cut(2,jj,ii),
     |                          cut(3,jj,ii),cut(4,jj,ii)
        read(33,'(a2)') junk
       enddo !--> Loop over cuts
       
55    format(a25,4f11.1)
      close(33)

      enddo !--> Loop over input files
      !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>!




      do ictyp=1,3 !--> Loop over cut types. 1 == {P+,P-} > 0.0GeV
                   !                         2 == {P+,P-} > 0.5GeV
                   !     (DL-01)             3 == {P+,P-} > 1.0GeV

       ! Reset "cuts" array to original, unscaled values in "cut" array
       do ii=1,4
        do jj=1,10
         do kk=1,numcuts
          cuts(ii,jj,kk) = cut(ii,jj,kk)
         enddo
        enddo
       enddo

       ! Apply final norms to CohRho,NCDIS and OBG:
       do ii=1,numcuts
        cuts(4,5,ii) = cuts(4,5,ii)*normcoh(ictyp)
        cuts(4,4,ii) = cuts(4,4,ii)*normcc(ictyp)
        cuts(4,3,ii) = cuts(4,3,ii)*normnc(ictyp)
        cuts(4,1,ii) = cuts(4,1,ii)*normobg(ictyp)
       enddo
      

      !-------------------------------------------------------------------------
      ! Re-write Final-Norm column in original cut tables:
      do ii=1,10
59    format(a20,a9,a9,a9,a9,a9)
      open(24,file=filename(ii,ictyp)(1:lengthf(ii,ictyp)-4)//
     |'.txt',status='UNKNOWN')
      write(24,59)'','Raw','Z-wt','norm0','chi-norm'
      write(24,*)' '

      do jj=1,numcuts 
       write(24,55) cutname(jj),cuts(1,ii,jj),cuts(2,ii,jj),
     |                          cuts(3,ii,jj),cuts(4,ii,jj)
       write(24,*)''
       
       if (jj.eq.numcuts) then 
      write(24,*)'===================================================='
      write(24,*)''
       endif
       enddo
      enddo
      close(24)
      ! Latex Tables:
77    format(a25,4(a3,f11.1),a3)
      do ii=1,9 !--> Event type loop 
      open(24,file=filename(ii,ictyp)(1:lengthf(ii,ictyp)-4)//
     |'_latex.tex',status='UNKNOWN')
        write(24,*)'\\begin{table}[h!]\\centering'
        write(24,*)'\\begin{tabular}{||l||r|r|r|r||}'
        write(24,*)'\\hline'
        write(24,*)'\\hline'
        write(24,*) 'Cut & Raw & Z-weight & Norm0 & Final \\\\'
        write(24,*)'\\hline'

        do jj=1,numcuts 
         write(24,77) cutname(jj),' & ',cuts(1,ii,jj),
     |                            ' & ',cuts(2,ii,jj),
     |                            ' & ',cuts(3,ii,jj),
     |                            ' & ',cuts(4,ii,jj),
     |                                   ' \\\\'
        enddo 
        write(24,*)'\\hline'
        write(24,*)'\\hline'
        write(24,*)'\\end{tabular}'
        write(24,*)'\\caption{Cut Table '//tag2(ii)//'}'
        write(24,*)'\\label{tab-cut'//tag1(ii)//'}'
        write(24,*)'\\end{table}'
      enddo 
      !-------------------------------------------------------------------------


      !-------------------------------------------------------------------------
      ! Make Summary Table
101   format(a3,a25,a1,f10.1,a2,f10.1,a2,f10.1,a2,f10.1,a2,
     |   F10.1,a2, f10.1,a2,f10.1,a2)
      open (33,file=TOPdir(1:length)//tag(ictyp)//'/cut_summary'//
     |                                '.txt',status='UNKNOWN')
      write(33,*) '|----------------------------------------------------
     |----------------------------------------------------------|'
      write(33,*) '|  Cut Name                |  CohRho   |    NC     | 
     |  CC      |   OBG     |   Other   |   Total   |   Data    |'
      write(33,*) '|--------------------------|-----------|-----------|-
     |----------|-----------|-----------|-----------|-----------|'
      do ii=1,numcuts
       write(33,101)' | ',cutname(ii),'|',cuts(4,5,ii),'|',
     |                                   cuts(4,3,ii),' |',cuts(4,4,ii),
     |                              ' |',cuts(4,1,ii),' |',
     |cuts(4,6,ii)+cuts(4,7,ii)+cuts(4,8,ii)+cuts(4,9,ii)+cuts(4,10,ii),
     |' |',
     |cuts(4,1,ii)+cuts(4,3,ii)+cuts(4,4,ii)+cuts(4,5,ii)+
     |cuts(4,6,ii)+cuts(4,7,ii)+cuts(4,8,ii)+cuts(4,9,ii)+cuts(4,10,ii),
     |' |',
     |                                   cuts(4,2,ii),' |'
      write(33,*) '|                          |           |           | 
     |          |           |           |           |           |'
      enddo



      write(33,*) '|----------------------------------------------------
     |----------------------------------------------------------|'
      close(33)
      !-------------------------------------------------------------------------


      !-------------------------------------------------------------------------
      ! Make Latex Format Summary Table
102   format(a25,a2,f10.1,a2,f10.1,a2,f10.1,a2,f10.1,a2,
     |   F10.1,a2, f10.1,a2,f10.1,a3)
      open(33,file=TOPdir(1:length)//tag(ictyp)//'cut_summary_latex'//
     |                               '.tex',status='UNKNOWN')
      write(33,*) '\\begin{table}[h!]\\centering'
      write(33,*) '{\\small{'
      write(33,*) '\\begin{tabular}{||l||r|r|r|r|r||r||r||}'
      write(33,*) '\\hline'
      write(33,*) 'Cut Name           &  CohRho   &    NC     &   CC 
     |     &   OBG     & Other  &   Total   &   Data    \\\\ \\hline
     |  \\hline'
      do ii=1,numcuts
       write(33,102) cutname(ii),' &',cuts(4,5,ii),'&',
     |                              cuts(4,3,ii),' &',cuts(4,4,ii),' &',
     |   cuts(4,1,ii),' &', 
     |cuts(4,6,ii)+cuts(4,7,ii)+cuts(4,8,ii)+cuts(4,9,ii)+cuts(4,10,ii),
     |' &',
     |cuts(4,1,ii)+cuts(4,3,ii)+cuts(4,4,ii)+cuts(4,5,ii)+
     |cuts(4,6,ii)+cuts(4,7,ii)+cuts(4,8,ii)+cuts(4,9,ii)+cuts(4,10,ii),
     |' &',
     |                                    cuts(4,2,ii),' \\\\'
      enddo
      write(33,*) '\\hline'
      write(33,*) '\\hline'
      write(33,*) '\\end{tabular}'
      write(33,*) '\\caption{Summary Cut Table}'
      write(33,*) '\\label{tab-sumcut}'
      write(33,*) '}}'
      write(33,*) '\\end{table}'
      write(33,*) '\\endinput'
      close(33)
      !-------------------------------------------------------------------------
     
      enddo !--> (DL-01)



      print *,''
      print *,'***** Job Done *****'
      print *,''


      stop
      end
