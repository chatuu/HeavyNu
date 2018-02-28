      program main
      implicit none

      integer nmc
      parameter (nmc=17) !--> Number of MC components (data has index 0)

      ! If one changes "nmain" one must also modify the matching integer
      ! in the format statments for the summary table output (.txt and .tex)
      integer nmain !--> number of main MC types to show in summary table
      parameter(nmain=6)

      character*300 filename(0:nmc)
      character*300 inputfile(0:nmc)
      character*25 cutname(100)
      real cut(4,0:nmc,100)
      real cuts(4,0:nmc,100)

      character*2 junk
      character*20 junkl

      character*200 TOPdir
      character*100 filetag(0:nmc)

      integer ityp,iext,idet,incnd
      integer ii,jj,kk

      integer numcuts

      integer length
      integer lengthf(0:nmc)

      real normcc(8)
      real normcoh(8)
      real normjpsi(8)
     
      character*6 tag1(0:nmc)
      character*8 tag2(0:nmc)
      character*8 tag3(0:nmc)
      character*20 ftag(0:nmc)
      integer ftlen
       
      integer id(nmain) !--> Index numbers for main components

      real total
      real other
      logical othertyp

      character*7 dirtype(4)
      character*5 dirdet(4)
      character*7 dirncnd(4)

      integer typesw(4)
      integer detsw(4)
      integer ncndsw(4)

      character*6 cuttag(4)
      character*13 captag(4)

      integer idx

      ! Set the MC components that should be explicitly shown in the
      ! table.  All other MC components will be summed into an "other"
      ! column.  The number of these should be the same as "nmain".
      id(1)=16 !CCDIS
      id(2)=1 !CohPi+
      id(3)=17
      id(4)=5 !CohRho+
      id(5)=6 !CohJ/Psi
      id(6)=3 !Heavy Nutrino 1GeV
      ! Set subdirectory names:
      dirtype(1)='numucc/'
      dirtype(2)='osdimu/'
      dirtype(3)='lsdimu/'
      dirtype(4)='osmupl/'
      !-------
      dirdet(1)='drft/'
      dirdet(2)='othr/'
      dirdet(3)='coil/'
      dirdet(4)='upst/'
      !-------
      dirncnd(1)='ncand2/'
      dirncnd(2)='ncand3/'
      dirncnd(3)='ncand4/'
      dirncnd(4)='ncnd34/'


      ! Read the datacard giving the location of the top directory
      ! (contains all input cut tables).
      open (23,file='location.dat',status='UNKNOWN')
       read(23,'(A200)') TOPdir
      close(23)
      length=LEN_TRIM(TOPdir)
    


      ! Number of cuts in the cut tables:
      numcuts = 30 !--> Cannot be greater than 100 



      ! Read directory switches:
      open(22,file=TOPdir(1:length)//'settings/evtsw.txt',
     |                                            status='OLD')
      do ii=1,4
       read(22,'(I5)') typesw(ii)
      enddo
      close(22)
      open(22,file=TOPdir(1:length)//'settings/detsw.txt',
     |                                            status='OLD')
      do ii=1,4
       read(22,'(I5)') detsw(ii)
      enddo
      close(22)
      open(22,file=TOPdir(1:length)//'settings/ncndsw.txt',
     |                                            status='OLD')
      do ii=1,4
       read(22,'(I5)') ncndsw(ii)
      enddo


      ! filetags from the main program (for input cut table naming):
      ftag(0) = 'cohjpsi-mumu_data   ' 
      ftag(1) = 'cohjpsi-mumu_ccdis  '
      ftag(2) = 'cohjpsi-mumu_ncdis  '
      ftag(3) = 'cohjpsi-mumu_jpsi   '
      ftag(4) = 'cohjpsi-mumu_obg    '
      ftag(5) = 'cohjpsi-mumu_cohpip '
      ftag(6) = 'cohjpsi-mumu_cohrhop'
      ftag(7) = 'cohjpsi-mumu_anumucc'
      ftag(8) = 'cohjpsi-mumu_qe     '
      ftag(9) = 'cohjpsi-mumu_cohpi0 '
      ftag(10)= 'cohjpsi-mumu_nuecc  '
      ftag(11)= 'cohjpsi-mumu_anuecc '
      ftag(12)= 'cohjpsi-mumu_anumunc'
      ftag(13)= 'cohjpsi-mumu_cohrho0'
      ftag(14)= 'cohjpsi-mumu_res    '
      ftag(15)= 'cohjpsi-mumu_cohphi0'
      ftag(16)= 'heavy_neutrino_1.000'
      ftag(17)= 'CohPi-'

      ! Set extra cut file tags:
      cuttag(1)='_allev'
      cuttag(2)='_m-sig'
      cuttag(3)='_mblnd'
      cuttag(4)='_mnorm'
      ! Set caption tags for extra cuts:
      captag(1)=' (all events)'
      captag(2)=' (mass sig.) '
      captag(3)=' (mass blind)'
      captag(4)=' (mass norm) '



      ! Tags for latex labeling
      tag1(0) = '__data'
      tag1(1) = '_ccdis'
      tag1(2) = '_ncdis'
      tag1(3) = '__jpsi'
      tag1(4) = '___obg'
      tag1(5) = '_copip'
      tag1(6) = '_crhop'
      tag1(7) = '_anmcc'
      tag1(8) = '____qe'
      tag1(9) = '_c0pi0'
      tag1(10)= '__necc'
      tag1(11)= '_anecc'
      tag1(12)= '_anmnc'
      tag1(13)= '_crho0'
      tag1(14)= '_rescc'
      tag1(15)= '_cophi'


      ! Latex table/Caption tags
      tag2(0) = ' Data   '
      tag2(1) = ' CCDIS  '
      tag2(2) = ' NCDIS  '
      tag2(3) = '\\cohjp  '
      tag2(4) = ' OBG    '
      tag2(5) = '\\cohpip '
      tag2(6) = '\\cohrp  '
      tag2(7) = ' anMuCC '
      tag2(8) = ' QE     '
      tag2(9) = '\\cohpi0 '
      tag2(10)= ' Nue-CC '
      tag2(11)= ' aNueCC '
      tag2(12)= ' anMuNC '
      tag2(13)= '\\cohr0  '
      tag2(14)= ' CC-Res '
      tag2(15)= '\\cohphi '
      tag2(16)= 'Signal'
      tag2(17)= '\\cohpim'
      ! Text file table lables:
      ! (if different from latex version)
      do ii=0,nmc
       tag3(ii)=tag2(ii)
      enddo
      tag3(3) = 'CohJPsi '
      tag3(5) = ' CohPi+ '
      tag3(6) = 'CohRho+ '
      tag3(9) = ' CohPi0 '
      tag3(13)= 'CohRho0 '
      tag3(15)= 'CohPhi0 '
       do ii=0,nmc
        print*,tag3(ii)
       enddo

      ! Read Normalizations
      open(66,file=TOPdir(1:length)//'normlist_ccdis.txt',
     |               status='OLD')
       do ii=1,8
        read(66,'(F10.4)') normcc(ii)
       enddo
      close(66)
      open(66,file=TOPdir(1:length)//'normlist_jpsi.txt',
     |               status='OLD')
       do ii=1,8
        read(66,'(F10.4)') normjpsi(ii)
       enddo
      close(66)
      open(66,file=TOPdir(1:length)//'normlist_cohpip.txt',
     |               status='OLD')
       do ii=1,8
        read(66,'(F10.4)') normcoh(ii)
       enddo
      close(66)



      !=========================================================================!
      !=========================================================================!

      do iext=1,4 !--> Loop over extra cut types (DL-00)

      print*,''
      print*,'|=================================================|'
      print*,'|=== Working extra cut type: ',cuttag(iext)(2:6),
     |                                         '             ===|'


      !Set the filenames of the input cut tables:
      do ii=0,nmc
      ftlen=LEN_TRIM(ftag(ii))
      filetag(ii)='cut_'//ftag(ii)(1:ftlen)//cuttag(iext)//'.txt'
      enddo


      do ityp=1,4 !--> Loop over topology types (DL-01)
      do idet=1,4 !--> Loop over detector sections (DL-02)
      do incnd=1,4 !--> Loop over ncand types (DL-03)

      ! Check that the switches are on:
      if(typesw(ityp)*detsw(idet)*ncndsw(incnd).eq.0) goto 777



      do ii=0,nmc
       ! Set intput files:
       inputfile(ii)=TOPdir(1:length)//dirtype(ityp)//
     |                                 dirdet(idet)//
     |                                 dirncnd(incnd)//filetag(ii)
       ! Set output files:
       filename(ii) =TOPdir(1:length)//dirtype(ityp)//
     |                                 dirdet(idet)//
     |                                 dirncnd(incnd)//
     |                                        'norm/'//filetag(ii)
       lengthf(ii)=LEN_TRIM(filename(ii))
      enddo


      !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>!
      ! Get Info. From Input Files:

      ! Read Cuts:
      do jj = 0,nmc !--> Loop over input files

       open (33,file=inputfile(jj),status='OLD')
      
       read(33,'(a2)') junk
       read(33,'(a2)') junk

       do ii=1,numcuts !--> Loop over cuts
        read(33,55) cutname(ii),cut(1,jj,ii),cut(2,jj,ii),
     |                          cut(3,jj,ii),cut(4,jj,ii)
        read(33,'(a2)') junk
       enddo !--> Loop over cuts
       
55    format(a25,4f12.1)
      close(33)

      enddo !--> Loop over input files
      !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>!




       ! Reset "cuts" array to original, unscaled values in "cut" array
       do ii=1,4
        do jj=0,nmc
         do kk=1,numcuts
          cuts(ii,jj,kk) = cut(ii,jj,kk)
         enddo
        enddo
       enddo

       !==========================================================!
       !==========================================================!
       ! Apply final norms to CohJ/Psi,CCDIS and CohRho+/CohPi+:
       idx=0 !--> Index for normalization factor.
             !    1 = MuMu ncand=2
             !    2 = MuMu ncand=3
             !    3 = MuMu ncand=4
             !    4 = MuMu ncand={3,4}
             !    5 = Mu-X ncand=2
             !    6 = Mu-X ncand=3
             !    7 = Mu-X ncand=4
             !    8 = Mu-X ncand={3,4}
       if(idet.eq.1) then !--> DC only
        if(ityp.eq.2) then !--> OS-Dimu
           idx=incnd
        elseif(ityp.eq.4) then !--> Mu+X
           idx=incnd+4
        endif
       endif
       if(idx.ne.0) then
       do ii=1,numcuts
        ! CCDIS:
        cuts(4,1,ii) = cuts(4,1,ii)*normcc(idx)
        ! J/Psi:
        cuts(4,3,ii) = cuts(4,3,ii)*normjpsi(idx)
        ! CohPi+ and CohRho+ are normalized together:
        ! CohPi+:
        cuts(4,5,ii) = cuts(4,5,ii)*normcoh(idx)
        ! CohRho+:
        cuts(4,6,ii) = cuts(4,6,ii)*normcoh(idx)
       enddo
       endif
       !==========================================================!
       !==========================================================!
      

      !-------------------------------------------------------------------------
      ! Re-write Final-Norm column in original cut tables:
      do ii=0,nmc
59    format(a20,a9,a9,a9,a9,a9)
      open(24,file=filename(ii)(1:lengthf(ii)-4)//
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
      open(24,file=filename(ii)(1:lengthf(ii)-4)//
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
      ! Make .txt Summary Table
      open(33,file=TOPdir(1:length)//dirtype(ityp)//
     |                               dirdet(idet)//
     |                               dirncnd(incnd)//'norm/'//
     |                     '/cut_summary'//cuttag(iext)//'.txt',
     |                                           status='UNKNOWN')
      !----------------------
      ! The format statements must be changed to match "nmain"!!!!!
      ! (each format statement has an integer that matches "nmain")
121   format(a2,a25,a1,6(f10.1,' |'),4(f10.1,a2))
123   format(a25,6('  | ',a8),a51)
124   format('|---------------------------',
     |           6('------------'),
     |       '-----------------------------------------------|',a1)
125   format('|--------------------------|',
     |           6('-----------|'),
     |       '-----------|-----------|-----------|-----------|',a1)
126   format('|                          |',
     |           6('           |'),
     |       '           |           |           |           |',a1)
      !----------------------
      write(33,124) ' ' !--> Top line
      write(33,123) '|  Cut Name              ',
     |                     (tag3(id(ii)),ii=1,nmain),
     |'  |   Other   |   Total   |   Data    | Data-Total|'
      write(33,125) ' ' !--> Solid line spacer
      do ii=1,numcuts
       ! Add up the "other" and "total" components:
       other=0.0
       total=0.0
       do jj=1,nmc
        othertyp=.true.
        do kk=1,nmain
         if(jj.eq.id(kk)) othertyp=.false.
        enddo
        if(othertyp) other=other+cuts(4,jj,ii)
        total=total+cuts(4,jj,ii)
       enddo
       write(33,121)'| ',cutname(ii),'|',
     |                      (cuts(4,id(jj),ii),jj=1,nmain),
     | other,' |',total,' |',cuts(4,0,ii),' |',(cuts(4,0,ii)-total),' |'
      write(33,126) ' ' !--> Empty line spacer
      enddo

      write(33,124) ' ' !--> Bottom line
      close(33)
      !-------------------------------------------------------------------------


      !-------------------------------------------------------------------------
      ! Make Latex Format Summary Table
      open(33,file=TOPdir(1:length)//dirtype(ityp)//
     |                                dirdet(idet)//
     |                                dirncnd(incnd)//'norm/'//
     |                'cut_summary_latex'//cuttag(iext)//'.tex',
     |                                           status='UNKNOWN')
      !-------------------
      ! These formats must be modified to match "nmain"
135   format(a25,a2,6(f10.1,' &'),f10.1,a2,f10.1,a2,f10.1,a2,f10.1,a3)
145   format('\\begin{tabular}{||l||',6('r|'),'r||r||r||r||}',a1)
136   format(a20,6(' ',a8,'  &'),a58)
      !-------------------
      write(33,*) '\\begin{table}[h!]\\centering'
      write(33,*) '{\\small{'
      write(33,145) ' ' !--> begin tabular line
      write(33,*) '\\hline'
      write(33,136) 'Cut Name           &',
     |                     (tag2(id(ii)),ii=1,nmain),
     |' Other  &   Total   &   Data  & Data-Total \\\\ \\hline  \\hline'
      do ii=1,numcuts
       ! Add up the "other" and "total" components:
       other=0.0
       total=0.0
       do jj=1,nmc
        othertyp=.true.
        do kk=1,nmain
         if(jj.eq.id(kk)) othertyp=.false.
        enddo
        if(othertyp) other=other+cuts(4,jj,ii)
        total=total+cuts(4,jj,ii)
       enddo
       write(33,135) cutname(ii),' &',
     |   (cuts(4,id(jj),ii),jj=1,nmain),
     |   other,' &',total,' &',cuts(4,0,ii),' &',
     |   cuts(4,0,ii)-total,' \\\\'
      enddo
      write(33,*) '\\hline'
      write(33,*) '\\hline'
      write(33,*) '\\end{tabular}'
      write(33,*) '\\caption{Summary Cut Table \\textbf{'//
     |                        captag(iext)//'}}'
      write(33,*) '\\label{tab-sumcut}'
      write(33,*) '}}'
      write(33,*) '\\end{table}'
      write(33,*) '\\endinput'
      close(33)
      !-------------------------------------------------------------------------
     

777   continue !--> Switch check
      enddo !--> DL-03
      enddo !--> DL-02
      enddo !--> DL-01

      print*,'|=== Finished extra cut type: ',cuttag(iext)(2:6),
     |                                         '            ===|'
      print*,'|=================================================|'
      print*,''
      enddo !--> DL-00
      !=========================================================================!
      !=========================================================================!

      print *,''
      print *,'***** Job Done *****'
      print *,''


      stop
      end

