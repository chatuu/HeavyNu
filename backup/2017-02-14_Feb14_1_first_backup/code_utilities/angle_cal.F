      subroutine angle_cal(angle,V1,V2)
      implicit none

      double precision angle
      double precision V1(0:4),V2(0:4)
      ! Indices of the input vectors:
      ! 0 = Energy (ignored)
      ! 1 = x-component
      ! 2 = y-component
      ! 3 = z-component
      ! 4 = Vector length (possibly ignored)

      integer ii
      double precision dotprod
      double precision ratio 

      double precision pi
      parameter (pi=3.14159265359D0)

      double precision len1,len2 !--> Vector lengths (possibly calculated)

      ! The ntuple version of the total momentum is often more than 0.1%
      ! different than the calculated value, so I had to increase the
      ! check limit to 1%
      double precision tolerance !--> Vector length check tolerance
      parameter (tolerance=0.001D0)


      !-----------------------------------------------------!
      ! This subroutine simply calculates the angle between
      ! the two given vectors (V1 and V2).  It calculates
      ! the angle using the formula for the dot product:
      !
      !             A*B = ABcos(angle)
      !
      ! where "*" represents the dot product operation
      ! on vectors A and B, "angle" is the angle between
      ! the two vectos and "AB" is the length of the
      ! two vectors multiplied together. So:
      !
      !          angle = acos( (A*B) / (AB)  )
      !
      ! The input vectors have the three components as the
      ! first three indices, and the last index gives the
      ! vector's length. The 0th index gives the particle's
      ! energy.
      !
      ! If the given length of the vectors (index 4) is very
      ! different from the calculated length then the given
      ! information is ignored.
      !-----------------------------------------------------!

      ! Initialize to an unrealistic value:
       angle = -8888.0D0

      ! Set vector lengths to given values before checking:
      len1 = V1(4)
      len2 = V2(4)

      !=================================================================!
      ! Check that the vector length given is correct
      ! (within the "tolerance" of the calculated value):
      if ( dabs(dsqrt(V1(1)**2+V1(2)**2+V1(3)**2)-V1(4)).gt.
     |                                    tolerance*V1(4) ) then 
       len1 = dsqrt(V1(1)**2+V1(2)**2+V1(3)**2)
       call printerror(2,388492144,'angle_cal.F         ')
       print*,'Vector length does not match!'
       print*,'New Calculation: ', len1
       print*,'Original: ',V1(4)
       print*,''
      endif
      if ( dabs(dsqrt(V2(1)**2+V2(2)**2+V2(3)**2)-V2(4)).gt.
     |                                    tolerance*V2(4) ) then 
       len2 = dsqrt(V2(1)**2+V2(2)**2+V2(3)**2)
       call printerror(2,998736612,'angle_cal.F         ')
       print*,'Vector length does not match!'
       print*,'New Calculation: ', len2
       print*,'Original: ',V2(4)
       print*,''
      endif
      !=================================================================!

      ! Check for zero-length vectors:
      if (len1.le.0.0D0.or.len2.le.0.0D0) then
        angle = -88888.0D0 !--> Return error value (unrealistic)
        return
      endif

      ! Dot product of the vectors:
      dotprod = V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)


      ! Check that the cosine of the angle is not out of range
      ! (the maximum value for the dot product should be the
      !  length of the vectors multiplied together).
      ratio = dotprod/(len1*len2)
      if (dabs(ratio).gt. 1.000000000000001D0) then
       if(dotprod.ge.0.0D0.and.ratio-1.0D0.lt. 1D-7) then
            angle=0.0D0
       elseif(dotprod.lt.0.0D0.and.ratio+1.0D0.gt. -1D-7) then
            angle=pi/2.0D0
       else
            angle=-888888.0D0 !--> Alternate unrealistic error output
       endif
       call printerror(2,339488271,'angle_cal.F         ')
       print *,'** Strange Angle ** '
       print*,' Vector information: '
       print *,'(A*B)/(AB): ', dabs(dotprod)/(len1*len2)
       print *,'(A*B), AB: ', dotprod,' ',(len1*len2)
       print *,'A: ',(V1(ii),ii=1,4)
       print *,'B: ',(V2(ii),ii=1,4)
       print *,''
       return
      endif
      
      angle = dacos(dotprod/(len1*len2))

      
      return
      end
