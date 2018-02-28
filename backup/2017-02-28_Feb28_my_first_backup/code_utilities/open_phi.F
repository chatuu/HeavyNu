      subroutine open_phi(openphi,V1,V2)
      implicit none
     
      ! Assumes the X,Y and Z-axes are the first 3 indices, respectively,
      ! and calculates phi between both vectors.  Phi is the angle of the 
      ! vector in the x-y plane, with 0deg located on the positive x-axis.  
      ! Phi is calculated in degrees.
      ! (4th vector index is the vector's length, 
      !  0th index is the particle's energy)

      double precision openphi
      double precision V1(0:4),V2(0:4)

      double precision phi1,phi2

      double precision pi 
      parameter (pi=3.14159265359D0)

      double precision scaler !--> conversion from radians to degrees
      parameter (scaler = 180.0/pi)


      ! Initialize to unrealistic value:
      openphi = -9999.0D0

      ! Calculate phi1:
      ! (the comments are alternate versions of the
      !  calculation that can be used as a check)
      if     (V1(2).ge.0.0D0.and.V1(1).ge.0.0D0) then
       phi1 =         scaler*datan(V1(2)/V1(1))
c       phi1 =         scaler*dasin(V1(2)/dsqrt(V1(1)**2+V1(2)**2))
c       phi1 =         scaler*dacos(V1(1)/dsqrt(V1(1)**2+V1(2)**2))
      elseif (V1(2).ge.0.0D0.and.V1(1).lt.0.0D0) then
       phi1 = 180.0D0+scaler*datan(V1(2)/V1(1))
c       phi1 = 180.0D0+scaler*dasin(V1(2)/dsqrt(V1(1)**2+V1(2)**2))
c       phi1 = 180.0D0-scaler*dacos(V1(1)/dsqrt(V1(1)**2+V1(2)**2))
      elseif (V1(2).lt.0.0D0.and.V1(1).lt.0.0D0) then
       phi1 = 180.0D0+scaler*datan(V1(2)/V1(1))
c       phi1 = 180.0D0+scaler*dasin(V1(2)/dsqrt(V1(1)**2+V1(2)**2))
c       phi1 = 360.0D0-scaler*dacos(V1(1)/dsqrt(V1(1)**2+V1(2)**2))
      elseif (V1(2).lt.0.0D0.and.V1(1).ge.0.0D0) then
       phi1 = 360.0D0+scaler*datan(V1(2)/V1(1))
c       phi1 = 360.0D0+scaler*dasin(V1(2)/dsqrt(V1(1)**2+V1(2)**2))
c       phi1 = 360.0D0-scaler*dacos(V1(1)/dsqrt(V1(1)**2+V1(2)**2))
      endif 


      ! Calculate phi2:
      if     (V2(2).ge.0.0D0.and.V2(1).ge.0.0D0) then
       phi2 =         scaler*datan(V2(2)/V2(1))
      elseif (V2(2).ge.0.0D0.and.V2(1).lt.0.0D0) then
       phi2 = 180.0D0+scaler*datan(V2(2)/V2(1))
      elseif (V2(2).lt.0.0D0.and.V2(1).lt.0.0D0) then
       phi2 = 180.0D0+scaler*datan(V2(2)/V2(1))
      elseif (V2(2).lt.0.0D0.and.V2(1).ge.0.0D0) then
       phi2 = 360.0D0+scaler*datan(V2(2)/V2(1))
      endif 

      ! Calculate Phi between the two vectors:
      openphi = phi1 - phi2
      if (openphi.lt.0.0D0) openphi = phi2 - phi1
      if (openphi.gt.180.0D0) openphi = 360.0D0 - openphi

      return
      end
