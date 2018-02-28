      subroutine printmain(nhist,nfloat,nhMAX,nfMAX,itMAX,
     |             nbins,iteration,title,outdir,ftag,chitit,norms)
      implicit none

      ! Input/Output:
      integer nhist
      integer nfloat
      integer nhMAX,nfMAX,itMAX
      integer nbins(nfMAX)
      integer iteration
      character*55 title(nhMAX,nfMAX) !--> Plot Titles 
      character*45 outdir(nhMAX) !--> Output directories
      character*10 ftag(nfMAX) !--> Type tag for floated components
      character*20 chitit(nfMAX) !--> Floated component titles 
      ! Normalizatinos:
      ! 1st Index is nhist, 2nd is for iteration number
      ! Third index gives:
      !  0 = Central Normalization
      !  1 = +1 Sigma
      !  2 = -1 Sigma
      !  3 = MinChisq
      !  4 = NbinsUsed
      double precision norms(nfMAX,nhMAX,0:itMAX,0:4)


      integer ihist,ifloat !--> Do loop 

      double precision Sigma

      integer length
      integer lentit
      integer flen
      integer lentag
      character*80 filename



      !----------------------------------------------------------------!
      !  Write normlist files:
      do ifloat=1,nfloat
       length=LEN_TRIM(ftag(ifloat)) 
       ! Central normalization:
       filename='output/normlist_'//ftag(ifloat)(1:length)//'.txt'
       open(25,file=filename,status='UNKNOWN')
       do ihist=1,nhist
       write(25,101) norms(ifloat,ihist,iteration,0)
       enddo
       close(25)
       ! -1 Sigma:
       filename=
     | 'output/normlist_'//ftag(ifloat)(1:length)//'_-1sigma.txt'
       open(25,file=filename,status='UNKNOWN')
       do ihist=1,nhist
       write(25,101) norms(ifloat,ihist,iteration,2)
       enddo
       close(25)
       ! +1 Sigma:
       filename=
     | 'output/normlist_'//ftag(ifloat)(1:length)//'_+1sigma.txt'
       open(25,file=filename,status='UNKNOWN')
       do ihist=1,nhist
       write(25,101) norms(ifloat,ihist,iteration,1)
       enddo
       close(25)
      enddo
101   format(F10.4)
      !----------------------------------------------------------------!




      do ihist=1,nhist !--> Loop over sets of histograms
      length=LEN_TRIM(outdir(ihist)) !Gets length of dirnames without trailing spaces

      do ifloat=1,nfloat !--> Loop over floated components
      flen=LEN_TRIM(ftag(ifloat))
      lentag=LEN_TRIM(chitit(ifloat))


      !=================================================!
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      ! Calculate Sigma:

      Sigma=dsqrt(norms(ifloat,ihist,iteration,3)/
     |            norms(ifloat,ihist,iteration,4))
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      !=================================================!
     


104   format(a37,F8.3,a6)  
105   format(a26,F7.3,a5)  
107   format(a16,F7.3,a8,F5.1,a9)  

      !{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}!
      ! Create Latex Files:
      lentit=LEN_TRIM(title(ihist,ifloat)) 

      filename='output/'//outdir(ihist)(1:length)//
     |         '/chisq_'//ftag(ifloat)(1:flen)//'.tex'
      open(55,file=filename,status='UNKNOWN')
       
      write(55,*) '\\begin{table}[h!]\\centering'
      write(55,*) '{\\small{'
      write(55,*) '\\begin{tabular}{||l||r||r||}'
      write(55,*) '\\hline'
      write(55,*) '\\hline'
      write(55,104) '\\multicolumn{2}{||c||}{$\\chi^{2}$ Min: ',
     |                      norms(ifloat,ihist,iteration,3),'} & \\\\'
       write(55,*) '\\multicolumn{2}{||c||}{Number of bins used: ',
     |                      norms(ifloat,ihist,iteration,4),'} & \\\\'
       write(55,104) '\\multicolumn{2}{||c||}{One $\\sigma$: ',
     |                                      Sigma,'} & \\\\'
       write(55,*) '\\hline'
       write(55,*) '\\hline'
       write(55,*) '   & '//chitit(ifloat)(1:lentag)//' & \\\\'
       write(55,105) 'Norm at Min $\\chi^{2}$  & ',
     |                      norms(ifloat,ihist,iteration,0),' & \\\\'
       write(55,107) '$-1$ $\\sigma$ & ',
     |                      norms(ifloat,ihist,iteration,2),'  &  $($',
     |((norms(ifloat,ihist,iteration,0)-
     |  norms(ifloat,ihist,iteration,2))/
     |  norms(ifloat,ihist,iteration,0))*100.0D0,'$\\%)$  \\\\'
       write(55,107) '$+1$ $\\sigma$ & ',
     |                      norms(ifloat,ihist,iteration,1),'  &  $($',
     |((norms(ifloat,ihist,iteration,1)-
     |  norms(ifloat,ihist,iteration,0))/
     |  norms(ifloat,ihist,iteration,0))*100.0D0,'$\\%)$  \\\\'
       write(55,*) '\\hline'
       write(55,*) '\\hline'
       write(55,*) '\\end{tabular}'
       write(55,*) '\\caption{$\\chi^{2}$ for '//
     |                         chitit(ifloat)(1:lentag)//' on plot:'//
     |                         title(ihist,ifloat)(1:lentit)//'}' 
       write(55,*) '\\label{tab-chi'//ftag(ifloat)(1:flen)//'}' 
       write(55,*) '}}'
       write(55,*) '\\end{table}'
       write(55,*) '\\endinput'

      close(55)
      !{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}!



      enddo !--> Loop over floated components
      enddo !--> Loop over sets of histograms



      return
      end
