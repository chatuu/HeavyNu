      program main
      implicit none


      ! Each MC is normalized in order, and then
      ! the process is iterated until the set
      ! of normalizations converges.

      ! Maximum possible array size values:
      integer ntMAX
      parameter (ntMAX=10) !--> ntyp 
      integer nfMAX
      parameter (nfMAX=20) !--> nfloat
      integer nhMAX
      parameter (nhMAX=22) !--> nhist
      ! Maximum possible iterations:
      integer itMAX
      parameter (itMAX=25) !--> iteration



      character*80 datfile !--> Datacard filename

      ! User information from the Datacard:
      integer ntyp !--> Number of Data/MC types
      integer nfloat !--> Number of floated MC components
      integer nhist !--> Number of sets of histograms
                    !    (each set for a different cut, for example)
      integer nbins(nfMAX) !--> Number of bins to use for floated MC
                           !    (each floated MC can use a different plot)
      integer mainid(nhMAX,nfMAX) !--> Histogram ID numbers
      character*55 title(nhMAX,nfMAX) !--> Histogram Titles
      character*45 outdir(nhMAX) !--> Output directory for each histogram set
      character*80 filename(ntMAX,nfMAX) !--> Histogram files
      character*10 ftag(nfMAX) !--> Type tag for floated components
      character*20 chitit(nfMAX) !--> Floated component titles 

      ! Do loop variables:
      integer ii,jj,kk,ll,ihist

      integer iteration !--> Nmber of iterations completed
      integer ipass !--> Pass/Fail final iteration test
      integer fpass !--> Check if all floated componetns pass
      integer useOverflow
      integer useMCerror
      integer nstep !--> Number of units to loop over


      ! Normalizations:
      ! 1st index gives the floated MC
      ! 2nd index is nhist, 3rd is for iteration number
      ! 4th index gives:
      !  0 = Central Normalization
      !  1 = +1 Sigma
      !  2 = -1 Sigma
      !  3 = MinChisq
      !  4 = NbinsUsed
      double precision norms(nfMAX,nhMAX,0:itMAX,0:4)

      double precision istep !--> Normalization step to use
      integer nbinmax !--> Madimum number of bins in histograms


      !========================================
      !========================================
      ! User Settings:

      ! Step size for normalizations:
      parameter (istep=0.0001D0)

      ! Maximum number of bins in each histogram:
      parameter (nbinmax=50)

      ! Datacard file:
      datfile='chisq.dat'

      useOverflow = 0 !--> Use (1) or do not use (0) overflow bin in total Chisq
      useMCerror = 1  !--> Use (1) or do not use (0) MC error when calculating Chisq
      !========================================
      !========================================




      !********************************************************
      ! Read Datacard:
      print*,''
      print*,''
      print*,'---=== Reading Datacard ===---'
      print*,''
      call read_datacard(ntMAX,nfMAX,nhMAX,
     |                   datfile,ntyp,nbins,nhist,nfloat,   
     |                   nbinmax,filename,mainid,title,outdir,
     |                   ftag,chitit)
      !********************************************************


      !-----------------------------!
      ! Initialize normalizations:
      do ii=1,nfloat
       do jj=1,nhist
        do kk=0,itMAX
         do ll=0,4
          norms(ii,jj,kk,ll)=1.0D0
         enddo
        enddo
       enddo
      enddo
      !-----------------------------!



      !=========================================================!
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      iteration=0
      do while (.true.) !--> Main Iteration Loop
      iteration=iteration+1

              
      print*,'---=== Iteration loop ',iteration,' ===---'

       !Loop from 0.0 to 3.0 in steps of "istep".
       !A small fraction is added to fix int() rounding
       !error. "nstep" gives the number of steps.
       nstep=int(((3.0D0+istep)/istep)+0.00000001D0)


      call chi(ntMAX,nfMAX,nhMAX,itMAX,
     | ntyp,nhist,nfloat,iteration,nbinmax,nbins,
     | nstep,istep,useOverflow,useMCerror,
     | filename,mainid,outdir,ftag,chitit,
     | norms)



      !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx!
      !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx!
      ! Convergence Check:
      if (iteration.gt.1) then
      ipass=0
      do ihist=1,nhist
       ! Check that each floated component passes:
       fpass=0
       do ii=1,nfloat
        if( norms(ii,ihist,iteration  ,0)- 
     |      norms(ii,ihist,iteration-1,0).lt.istep/10.0D0)
     |  fpass=fpass+1
       enddo
       if(fpass.eq.nfloat) ipass=ipass+1
      enddo
      ! Check that all histogram types pass:
      if(ipass.eq.nhist) then
         open(44,file='output/iterations.txt',status='unknown')
         write(44,'(I4)') iteration
         close(44)
         print*,''
         print*,''
         print*,'============================'
         print*,'==  Iteration Converged!  =='
         print*,'============================'
         print*,''
         print*,'---=== Printing Output ===---'
         call printmain(nhist,nfloat,nhMAX,nfMAX,itMAX,
     |             nbins,iteration,title,outdir,ftag,chitit,norms)
         goto 88
      endif
      endif !--> Iteration Check (if higher than 1st)
      !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx!
      !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx!

      if(iteration.eq.itMAX) then
         print*,''
         print*,''
         print*,' Normalizations did not converge!!!'
         print*,''
         print*,''
         stop
      endif

      enddo !--> Iteration Loop
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      !=========================================================!


88    continue !--> Normalizations converged
      print*,''
      print*,''
      print*,'******************'
      print*,'***  JOB DONE  ***'
      print*,'******************'
      print*,''
      stop
      end
