      subroutine print_zeroth_norms(nmc,nttypobg,ttag,gtyp,norm,ntyp,
     |                                                          gennt)
      implicit none

      ! Print a latex formatted table with zeroth normalization information.

      integer nmc
      integer nttypobg
      character*10 ttag(0:nmc) !--> table tags
      integer gtyp(0:nmc) !--> MC generator used (for 2nd index of "gennt"
      double precision norm(0:1,0:nmc) ! In the first index a "0" will give the deisired zeroth-level 
                                       ! number of events (MC will be scaled to these numbers) 
      integer ntyp
      double precision gennt(nmc,ntyp) ! Number of generated events in each ntuple
                                    ! Second index gives MC generator type (gtyp)

      integer ii
      integer UN
      
      print*,' '
      print*,'|-----------------------|'
      print*,'| Printing Zeroth Norms |'
      print*,'|-----------------------|'
      print*,' '

      call FindOpenUnit(UN)
      open (UN,file='outputs/zeroth_norms.tex',status='UNKNOWN') 

      write(UN,*) '\\begin{table}[h!]\\centering'
      write(UN,*) '{\\large{'
      write(UN,*) '\\begin{tabular}{||l|r|r||}'
      write(UN,*) '\\hline'
      write(UN,*) '\\hline'
      write(UN,*) '      & Gen Number   & Zroth Norm  \\\\'
      write(UN,*) '\\hline'

55    format(a10,' & ',F10.1,' & ',F10.1,' \\\\')
      do ii=1,nmc
         if(ii.ne.nttypobg)
     |   write(UN,55) ttag(ii), gennt(ii,gtyp(ii)), norm(0,ii)
      enddo
      write(UN,*) '\\hline'
      write(UN,*) '\\hline'
      write(UN,*) '\\end{tabular}'
      write(UN,*) '\\caption{Generated Number of MC Events}'
      write(UN,*) '\\label{tab-gen-numbers}'
      write(UN,*) '}}'
      write(UN,*) '\\end{table}'
      write(UN,*) '\\endinput'
      close(UN)

      return
      end
