      subroutine muon_calculations( nttyp, nttypobg, one, nmu,
     |                               imu,Eclust,PcluAsym,
     |                               P1,P2,Pt1,Pt2,
     |                               Theta1,Theta2,Theta12,
     |                               Zeta1,Zeta2,
     |                               Phi1,Phi2,Phi12,
     |                               Pasym,
     |                               Charge1, Charge2,
     |                               Pparent,PtParent,Mparent,Mee,
     |                               ThetaParent,ZetaParent,PhiParent,
     |                               ptpos,ptneg,
     |                               thetaX1,thetaX2,thetaY1,thetaY2,
     |                               posx,posy,posz,muextrap,rDCA)
      implicit none

#include "ntuple.inc"

      ! This subroutine calculates various kinematic variables for the muons.

      
      integer nttyp !--> Ntuple type 
      integer nttypobg !--> Index of the OBG component
      integer one !--> Index of most upstream V0 for OBG
      integer nmu !--> Number of muons in the event

      ! Calculated kinematic variables:
      integer imu(2) !--> Track numbers of the two muons
      double precision Eclust !--> Neutral energy in ECAL clusters
      double precision PcluAsym !--> Asymmetry in parent momentum and Eclust
      double precision P1(0:4) !--> 1st Charged Track Momentum 
                                 !    (4th index is total momentum, 
                                 !     0th index is energy from ntuple)
      double precision P2(0:4) !--> Momentum for 2nd charged track
      double precision Pt1 !--> Transverse momentum of 1st track
      double precision Pt2 !--> Transverse momentum of 2nd track
      double precision Theta1 !--> Angle w.r.t. z-axis of 1st track
      double precision Theta2 !--> Angle w.r.t. z-axis of 2nd track
      double precision Theta12 !--> Opening angle between two tracks
      double precision Zeta1 !--> Zeta of 1st track
      double precision Zeta2 !--> Zeta of 2nd track
      double precision Phi1 !--> Angle in X-Y plane of 1st track
      double precision Phi2 !--> Angle in X-Y plane of 2nd track
      double precision Phi12 !--> Opening angle between two tracks in the X-Y 
      double precision Pasym !--> Momentum asymmetry between the two tracks
      integer Charge1 !--> Charge of 1st track
      integer Charge2 !--> Charge of 2nd track
       !Track pair "Parent" variables:
      double precision Pparent(0:4) !--> Momentum/Energy of the track pair's "parent" particle
      double precision PtParent !--> Transverse momentum of the "parent"
      double precision Mparent !--> Invariant mass of the "parent" with muon assumption
      double precision Mee !--> Mass of parent with electron assumption
      double precision ThetaParent !--> Angle w.r.t. z-axis of "parent"
      double precision ZetaParent !--> Zeta of "parent"
      double precision PhiParent !--> Angle in X-Y plane of "parent"
      double precision ptpos,ptneg
       ! Other variables:
      double precision thetaX1 !--> Angle of 1st track in X-Z plane 
      double precision thetaX2 !--> Angle of 2nd track in X-Z plane
      double precision thetaY1 !--> Angle of 1st track in Y-Z plane
                               ! (to check that our z-axis is along the beam) 
                               ! [The mean should be close to zero, but will be
                               !  ~0.042574rad if the momenta are given in
                               !  the lab frame, which is the beam angle]
      double precision thetaY2 !--> Angle of 2nd track in Y-Z plane
      double precision posx,posy,posz !--> Vertex position (modified only for extrapolation)
      logical muextrap !--> Use my 2-mu track extrapolation?
      double precision rDCA !--> Vertex Distance of closest approach for extrapolation

      !========================================================!
      ! Common particle masses:
      ! (From PDG in Feb. 2015)
      double precision Mpipm,Mpiz !--> Pion Masses (GeV)
      parameter (Mpipm=0.13957D0)
      parameter (Mpiz=0.134977D0)
      double precision Mrho !--> Rho Mass (GeV)
      parameter (Mrho=0.775D0)
      double precision Mp !--> Proton Mass (GeV)
      parameter (Mp=0.938272D0)
      double precision Me !--> Electron Mass (GeV)
      parameter (Me=0.0005109989D0)
      double precision Mmu !--> Muon Mass (GeV)
      parameter (Mmu=0.105658367D0)
      double precision Mk,Mk0 !--> Kaon Masses (GeV)
      parameter (Mk=0.493667D0)
      parameter (Mk0=0.497614D0)
      !========================================================!

      ! Subroutine variables:
      integer ii !--> Do loop variable
      logical ismu !--> Function to check if track is a muon
      double precision unitz(0:4) !--> Unit Vector along Z-axis
      double precision dotprod !--> Dot product of the two track momenta
      double precision Ptrkmax(2) !--> Track momentum for muons
      double precision Ptrktmp !--> Temporary track momentum
      integer phaseii,iimu,other

      ! My Extrapolation Variables:
      double precision Pext1(0:4),Pext2(0:4) !--> Momenta and energgy
      double precision VertExt(3) !--> Extrapolated vertex






      !==================================!
      ! Initialize variables: 

       ! Unit vector along Z-axis:
       unitz(0) = 0.0D0
       unitz(1) = 0.0D0
       unitz(2) = 0.0D0
       unitz(3) = 1.0D0
       unitz(4) = 1.0D0


       Charge1= -999
       Charge2= -999
       do ii=0,4
        P1(ii)      = -999999999.0D0
        P2(ii)      = -999999999.0D0
        Pparent(ii) = -999999999.0D0
       enddo
        Pt1        = -999999999.0D0
        Pt2        = -999999999.0D0
        Theta1     = -999999999.0D0
        Theta2     = -999999999.0D0
        Theta12    = -999999999.0D0
        Zeta1      = -999999999.0D0
        Zeta2      = -999999999.0D0
        Phi1       = -999999999.0D0
        Phi2       = -999999999.0D0
        Phi12      = -999999999.0D0
        PtParent   = -999999999.0D0
        ThetaParent= -999999999.0D0
        ZetaParent = -999999999.0D0
        PhiParent  = -999999999.0D0
        Mparent    = -999999999.0D0
        Mee        = -999999999.0D0
        thetaX1    = -999999999.0D0
        thetaX2    = -999999999.0D0
        thetaY1    = -999999999.0D0
        thetaY2    = -999999999.0D0
        Pasym      = -999999999.0D0
        ptpos      = -999999999.0D0
        ptneg      = -999999999.0D0

        ! Extrapolation variables:
        rDCA=0.0D0
        VertExt(1)=posx
        VertExt(2)=posy
        VertExt(3)=posz
        do ii=0,4
        Pext1(ii)= -999999999.0D0
        Pext2(ii)= -999999999.0D0
        enddo
      !==================================!



      !--------------------------------------------
      ! Set index for first and second muons.
      ! Here the first track has (-) charge and
      ! the second (+) charge for two-track, opposite
      ! signed events. For like-signed events the track
      ! with higher momentum is given the first index.
      imu(1)=0
      imu(2)=0
      if (nttyp.ne.nttypobg) then
        if(ncand.eq.1) then
          if(ismu(1)) imu(1)=1
        elseif (ncand.ge.2) then
         
          ! First get the two muons with hightest momentum:
          do ii=1,ncand !--> Loop over charged tracks
           ! Total momentum of this track:
           Ptrktmp = dsqrt( dble(pxlr(ii))**2 +
     |                      dble(pylr(ii))**2 +
     |                      dble(pzlr(ii))**2   )



           if (nmu.ge.2) then
             !==========================================================
             ! If no tracks are tagged as a muon:
             if(ismu(ii).and.imu(1).eq.0) then
                    imu(1)=ii
                    Ptrkmax(1)=Ptrktmp
             ! If only one track is tagged as a muon:
             elseif(ismu(ii).and.imu(2).eq.0) then
                    imu(2)=ii
                    Ptrkmax(2)=Ptrktmp
                    ! Keep the highest momentum as the 1st track:
                    if(Ptrkmax(2).gt.Ptrkmax(1)) 
     |                      call switchmu(imu,Ptrkmax)
             ! If two tracks are aready tagged as a muon:
             elseif(ismu(ii).and.imu(2).ne.0) then

                ! If already have two LS muons and new muon has different charge:
                if( lepq(imu(1)).eq.lepq(imu(2)).and.
     |              lepq(imu(1)).ne.lepq(ii) ) then
                      imu(2)=ii
                      Ptrkmax(2)=Ptrktmp
                ! All other cases:
                else
                   if(lepq(ii).eq.lepq(imu(2)).and.
     |                Ptrktmp.gt.Ptrkmax(2)) then
                       imu(2)=ii
                       Ptrkmax(2)=Ptrktmp
                   elseif(lepq(ii).eq.lepq(imu(1)).and.
     |                Ptrktmp.gt.Ptrkmax(1)) then
                       imu(1)=ii
                       Ptrkmax(1)=Ptrktmp
                   endif
                endif 

                ! Keep the highest momentum as the 1st track:
                if(Ptrkmax(2).gt.Ptrkmax(1)) call switchmu(imu,Ptrkmax)
             endif
             !==========================================================
           elseif(nmu.eq.1) then 
             !==========================================================
             call label_muid(phaseii,iimu,other)
             imu(1)=iimu
             Ptrkmax(1) = dsqrt( dble(pxlr(ii))**2 +
     |                           dble(pylr(ii))**2 +
     |                           dble(pzlr(ii))**2   )
             if(ii.ne.iimu) then
             ! If first OS, non-muon track is found:
             if(imu(2).eq.0.and.lepq(ii).ne.lepq(iimu)) then
                    imu(2)=ii
                    Ptrkmax(2)=Ptrktmp
             ! A new OS, non-muon track is found:
             elseif(lepq(ii).ne.lepq(iimu).and.
     |                                Ptrktmp.gt.Ptrkmax(2)) then
                    imu(2)=ii
                    Ptrkmax(2)=Ptrktmp
             endif
             endif
             !==========================================================
           endif


          enddo !--> Loop over charged tracks

          ! Now ensure that the first muon is negative for OS
          if(lepq(imu(2)).eq.-1.and.lepq(imu(1)).ne.-1) 
     |                    call switchmu(imu,Ptrkmax)

        endif !--> ncand.ge.2
      endif !--> nttyp.ne.nttypobg
      !--------------------------------------------

      !=========================================================!
      ! Perform my extrapolation of two muon tracks:
      if(muextrap) then 
      call muon_extrapolation(nmu,lepq,pxlr,pylr,pzlr, 
     |                        XStartLep,YStartLep,ZStartLep, 
     |                        imu(1),imu(2),
     |                        Pext1,Pext2,VertExt,rDCA)
       ! Set new vertex:
       posx=VertExt(1)
       posy=VertExt(2)
       posz=VertExt(3)
      endif
      !=========================================================!


      !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::!
      !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::!
      ! Calculations for the first charged track:

      if (nttyp.ne.nttypobg.and.imu(1).eq.0) return !--> Stop calculations if there are no muons 

      ! Stop calculations for OBG if the number of primary tracks is out
      ! of desired range, or if there are no V0s in the DC:
      if (nttyp.eq.nttypobg.and.ncand.eq.0) return 
      if (nttyp.eq.nttypobg.and.ncand.ge.4) return
      if (nttyp.eq.nttypobg.and.nvzero.le.0) return 


      !---------------------------------------------!
      ! Set the track momentum:
      if (nttyp.ne.nttypobg) then
       P1(1) = dble(pxlr(imu(1)))
       P1(2) = dble(pylr(imu(1)))
       P1(3) = dble(pzlr(imu(1)))
      elseif (nttyp.eq.nttypobg) then
       !--> Negative V0 track for OBG:
       P1(1) = dble(nv0px(one))
       P1(2) = dble(nv0py(one))
       P1(3) = dble(nv0pz(one))
      endif
      ! Set to extrapolated values instead:
      if(muextrap) then
       P1(1)=Pext1(1)
       P1(2)=Pext1(2)
       P1(3)=Pext1(3)
      endif
      ! Total Momentum:
      P1(4) = dsqrt(P1(1)**2 + P1(2)**2 + P1(3)**2)
      ! Rotate to the beam frame:
      call labtobeam(P1(1),P1(2),P1(3))
      !---------------------------------------------!

      ! Charge of the track:
      if(nttyp.ne.nttypobg) then
       Charge1= lepq(imu(1))
      else
       if(decay(one).eq. 0) Charge1= -1 !--> OS V0
       if(decay(one).eq.-1) Charge1= -1 !--> Two negative tracks
       if(decay(one).eq. 1) Charge1=  1 !--> Two positive tracks
      endif


      ! Transverse momentum:
      Pt1 = dsqrt(P1(1)**2+P1(2)**2)

      ! Calculate angle w.r.t. beam axis:
      call angle_cal(Theta1,P1,unitz) 
      ! Calculate angle in X-Y plane:
      call calcphi(Phi1,P1)
      ! Calculate angles in X-Z and Y-Z planes:
      thetaX1 = datan(P1(1)/P1(3))
      thetaY1 = datan(P1(2)/P1(3))

      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      !Below this line assumptions about the particle ID are made:

      ! Calculate the energy assuming the track is a muon:
      P1(0) = dsqrt(P1(4)**2 + Mmu**2) 
      ! If using extrapolation:
      if(muextrap) P1(0)=Pext1(0)

      ! Calculate zeta:
      Zeta1 = P1(0)*(1.0D0-dcos(Theta1))



      !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::!
      !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::!
      ! Calculations for the second charged track:

      if (nttyp.ne.nttypobg.and.imu(2).eq.0) return !--> Stop calculations if one or fewer charged tracks



      !---------------------------------------------!
      ! Set the track momentum:
      if (nttyp.ne.nttypobg) then
       P2(1) = dble(pxlr(imu(2)))
       P2(2) = dble(pylr(imu(2)))
       P2(3) = dble(pzlr(imu(2)))
      elseif (nttyp.eq.nttypobg) then
       !--> Positive V0 track for OBG:
       P2(1) = dble(pv0px(one)) 
       P2(2) = dble(pv0py(one))
       P2(3) = dble(pv0pz(one))
      endif
      ! Set to extrapolated values instead:
      if(muextrap) then
       P2(1)=Pext2(1)
       P2(2)=Pext2(2)
       P2(3)=Pext2(3)
      endif
      ! Total Momentum:
      P2(4) = dsqrt(P2(1)**2 + P2(2)**2 + P2(3)**2) 
      ! Rotate to the beam frame:
      call labtobeam(P2(1),P2(2),P2(3)) 
      !---------------------------------------------!

      ! Charge of the track:
      if(nttyp.ne.nttypobg) then
       Charge2= lepq(imu(2))
      else
       if(decay(one).eq. 0) Charge2=  1 !--> OS V0
       if(decay(one).eq.-1) Charge2= -1 !--> Two negative tracks
       if(decay(one).eq. 1) Charge2=  1 !--> Two positive tracks
      endif


      ! Transverse momentum:
      Pt2 = dsqrt(P2(1)**2+P2(2)**2)

      ! Calculate angle w.r.t. beam axis:
      call angle_cal(Theta2,P2,unitz) 
      ! Calculate angle in X-Y plane:
      call calcphi(Phi2,P2)
      ! Calculate angles in X-Z and Y-Z planes:
      thetaX2 = datan(P2(1)/P2(3))
      thetaY2 = datan(P2(2)/P2(3))



      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      !Below this line assumptions about the particle ID are made:

      ! Calculate the energy assuming the track is a muon:
      P2(0) = dsqrt(P2(4)**2 + Mmu**2) 
      ! If using extrapolation:
      if(muextrap) P2(0)=Pext2(0)


      ! Calculate zeta:
      Zeta2 = P2(0)*(1.0D0-dcos(Theta2))


      !-----------------------------------------------------!
      ! Variables relating the two tracks:

       ! Calculate opening angle between the two tracks:
       call angle_cal(Theta12,P1,P2)
       ! Calculate the opening phi between tracks:
       call open_phi(Phi12,P1,P2)
       ! Momentum asymmetry:
       Pasym = (P1(4)-P2(4)) / (P1(4)+P2(4))

       ptpos = dsin(Theta12)*P2(4)
       ptneg = dsin(Theta12)*P1(4)
      !-----------------------------------------------------!




      !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::!
      !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::!
      ! Calculations for the "parent" particle:




      !---------------------------------------------!
      ! Set the parent momentum:
       Pparent(0) = P1(0) + P2(0) !--> These energies have the muon assumption
       Pparent(1) = P1(1) + P2(1)
       Pparent(2) = P1(2) + P2(2)
       Pparent(3) = P1(3) + P2(3)
      ! Total Momentum:
       Pparent(4) = dsqrt(Pparent(1)**2 + 
     |                    Pparent(2)**2 + 
     |                    Pparent(3)**2)
      !---------------------------------------------!

      ! Transverse momentum:
      PtParent = dsqrt(Pparent(1)**2+Pparent(2)**2)

      ! Calculate angle w.r.t. beam axis:
      call angle_cal(ThetaParent,Pparent,unitz) 
      ! Calculate angle in X-Y plane:
      call calcphi(PhiParent,Pparent)

      ! Calculate zeta:
      ZetaParent = Pparent(0)*(1.0D0-dcos(ThetaParent))

      ! Calculate the mass of the parent particle (includes muon assumption):
      Mparent = dsqrt(Pparent(0)**2 - Pparent(4)**2)
      ! Calculate the parent mass assuming the 2 tracks are electrons:
      dotprod = P1(1)*P2(1) + P1(2)*P2(2) + P1(3)*P2(3)
      Mee = dsqrt(2*(Me**2 + dsqrt(P1(4)**2+Me**2)*dsqrt(P2(4)**2+Me**2)
     |                                            - dotprod))

      ! Calculate the asymmetry between parent momentum and neutral
      ! energy in the ECAL:
      PcluAsym=(Pparent(4)-Eclust)/(Pparent(4)+Eclust)

      return
      end
