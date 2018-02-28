      subroutine calcphi(phi,vec)
      implicit none
     
      ! Assumes Z-axis is index #3 and calculates phi.
      ! (4th index is the vector's length, 0th the energy)
      ! Phi is the angle of the vector in the x-y plane, 
      ! with 0deg located along the positive x-axis.  
      ! Phi is calculated in degrees.

      double precision phi
      double precision vec(0:4)

      real pi 
      parameter (pi=3.14159265359D0)

      double precision scaler !--> conversion from radians to degrees
      parameter (scaler = 180.0D0/pi)

      ! Initialize to unrealistic value:
      phi = -8888.0D0


      if     (vec(2).ge.0.0D0.and.vec(1).ge.0.0D0) then
       phi =         scaler*datan(vec(2)/vec(1))
      elseif (vec(2).ge.0.0D0.and.vec(1).lt.0.0D0) then
       phi = 180.0D0+scaler*datan(vec(2)/vec(1))
      elseif (vec(2).lt.0.0D0.and.vec(1).lt.0.0D0) then
       phi = 180.0D0+scaler*datan(vec(2)/vec(1))
      elseif (vec(2).lt.0.0D0.and.vec(1).ge.0.0D0) then
       phi = 360.0D0+scaler*datan(vec(2)/vec(1))
      endif 


      return
      end
