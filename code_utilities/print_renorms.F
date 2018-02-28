      subroutine print_renorms(reCounts)
      implicit none

      double precision reCounts(2)

      integer ii
    
      integer UN

      call FindOpenUnit(UN)

      open(UN,file='outputs/renorm.txt',status='UNKNOWN')
       write(UN,'(F12.8)') reCounts(1)/reCounts(2) !--> Renormalization Factor
      close(UN)
    
      open(UN,file='outputs/renorm_counts_raw.txt',status='UNKNOWN')
       write(UN,'(F12.2)') reCounts(1)
      close(UN)

      open(UN,file='outputs/renorm_counts_cor.txt',status='UNKNOWN')
       write(UN,'(F12.2)') reCounts(2)
      close(UN)


      return
      end
      
