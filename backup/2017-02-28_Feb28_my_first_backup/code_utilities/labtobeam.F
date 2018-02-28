      subroutine labtobeam (x,y,z)
      implicit none
      
      double precision x,y,z
      double precision a,b
       
      ! Beam angle (from neglib.inc file):
      ! 2.4393 Deg
      ! 0.0425738164439 rad

      a=y
      b=z
     
      ! Double Precision Version:
      y=0.999093871954449122D0*a-0.0425609565572347316D0*b
      z=0.999093871954449122D0*b+0.0425609565572347316D0*a
      !       ^                           ^
      !       |                           |
      !       |                           |
      !     cos(th)                     sin(th)



      ! Single precision:
c      y=0.99909387*a-0.042560957*b
c      z=0.99909387*b+0.042560957*a
      !    ^             ^
      !    |             |
      !    |             |
      !  cos(th)       sin(th)

      return
      end

      
