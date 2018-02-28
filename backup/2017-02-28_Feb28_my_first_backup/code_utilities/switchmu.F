      subroutine switchmu(imu,Pmu)
      implicit none

      ! This subroutine switches the index and
      ! total momentum values for a pair of
      ! muons.

      integer imu(2) !--> Muon index for each muon
      double precision Pmu(2)

      integer itmp
      double precision dtmp


      itmp=imu(1)
      imu(1)=imu(2)
      imu(2)=itmp

      dtmp=Pmu(1)
      Pmu(1)=Pmu(2)
      Pmu(2)=dtmp

      return
      end
