      subroutine process_event(nmc,nttyp,extyp,nvar,ncuts,
     |                ndetsec,nncand,nevtyp,nextyp,
     |                cuts,norm,
     |                nttypobg,zetaS,cuthists,nbintyp,
     |                usecorr2D,nbin2Dmass,nbin2Dzeta,DScorr2D,
     |                binlim2Dmass,binlim2Dzeta,
     |                usecorrMass,nbinMass,DScorrMass,binlimMass,
     |                usecorrZeta,nbinZeta,DScorrZeta,binlimZeta,
     |                formRenorm,renorm,reCounts,iCorr,evtID,
     |                detsw,ncndsw,evtsw,extrapsw,evtpicsw)
      implicit none

#include "ntuple.inc"

      ! In this subroutine one performs desired actions on a single
      ! event.  This includes things like calculating variables,
      ! filling histograms and making cut-table counts.

      !----------------------------------------!
      ! Input/Output variables:
      integer nmc,nttyp,extyp,nvar,ncuts
      integer ndetsec,nncand,nevtyp,nextyp
      double precision cuts(3,ncuts,0:nmc,nevtyp,nextyp,nncand,ndetsec)
      double precision norm(0:1,0:nmc)
      integer nttypobg
      double precision zetaS
      integer cuthists(ncuts)
      integer nbintyp(nvar) 
       !For Correction functions:
      integer usecorr2D
      integer nbin2Dmass
      integer nbin2Dzeta
      double precision DScorr2D(nbin2Dmass,nbin2Dzeta)
      double precision binlim2Dmass(nbin2Dmass+1)
      double precision binlim2Dzeta(nbin2Dzeta+1)
      integer usecorrMass
      integer nbinMass
      double precision DScorrMass(nbinMass)
      double precision binlimMass(nbinMass+1)
      integer usecorrZeta
      integer nbinZeta
      double precision DScorrZeta(nbinZeta)
      double precision binlimZeta(nbinZeta+1)
      integer formRenorm 
      double precision renorm 
      double precision reCounts(2) 
      integer iCorr
      integer evtID(2)
      integer detsw(ndetsec)
      integer ncndsw(nncand)
      integer evtsw(nevtyp)
      integer extrapsw
      integer evtpicsw
      !----------------------------------------!

    
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


      ! Program variables:
      integer ii,jj,kk !--> Do loop variables
      integer one !--> Index of most upstream V0 for OBG
      integer check(extyp) !--> Switch for extra kinematic cuts (plotting)
      double precision wt !--> Event weight 
      double precision KinVar(nvar) !--> List of kinematic variables
      real minz
      integer idx !--> Temporary index
      integer idxdet
      integer idxncnd
      integer idxcut
      integer getcheckid !--> Function to get check index number
      double precision DScorr !--> Full DS correction
      double precision rnorm !--> Temporary renormalization value
      logical typpass(nevtyp) !--> To check if a certain event type passes its cuts
                           !--> 1 = All NuMuCC 
                           !--> 2 = OS Dimuon
                           !--> 3 = LS Dimuon
                           !--> 4 = OS Mu + <other> 
      ! For cuts:
      double precision zmin
      integer phaseii,iimu,other
      logical lveto,ltube
      integer imu(2)
      double precision Vec1(0:4),Vec2(0:4)
      double precision thprimord(22)
      logical ismu !--> Function to check if track is a muon
      integer nmu !--> number of muons

      !------------------------------------------------------------------------!
      !------------------------------------------------------------------------!
      ! For vectors with indices ranging from 0 to 4:
      !   0 = Measured Energy
      !   1 = X Momentum
      !   2 = Y Momentum
      !   3 = Z Momentum
      !   4 = Total Momentum (from ntuple)
      !----------------------------------------------------------------
      ! Kinematic variables related to the two charged tracks:
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
      double precision ptpos,ptneg
       !Track pair "Parent" variables:
      double precision Pparent(0:4) !--> Momentum/Energy of the track pair's "parent" particle
      double precision PtParent !--> Transverse momentum of the "parent"
      double precision Mparent !--> Invariant mass of the "parent" with muon assumption
      double precision Mee !--> Mass of parent with electron assumption
      double precision ThetaParent !--> Angle w.r.t. z-axis of "parent"
      double precision ZetaParent !--> Zeta of "parent"
      double precision PhiParent !--> Angle in X-Y plane of "parent"
      double precision PcluAsym !--> Asymmetry in parent momentum and Eclust
       ! Other variables:
      double precision thetaX1 !--> Angle of 1st track in X-Z plane 
      double precision thetaX2 !--> Angle of 2nd track in X-Z plane
      double precision thetaY1 !--> Angle of 1st track in Y-Z plane
                               ! (to check that our z-axis is along the beam) 
                               ! [The mean should be close to zero, but will be
                               !  ~0.042574rad if the momenta are given in
                               !  the lab frame, which is the beam angle]
      double precision thetaY2 !--> Angle of 2nd track in Y-Z plane
      !------------------------------------------------------------------------!







      !------------------------------------------------------------------------!
      double precision posx,posy,posz !--> Primary vertex position
      double precision Eclust !--> Neutral energy in ECAL clusters
      double precision PtNeutH !--> "Transverse" neutral hadron energy 
                               !     (the direction of a neutral cluster
                               !      is determined by pointing to the vertex)
      double precision PneutH !--> Total neutral hadron energy
      double precision Ehcal !--> Energy in the hadronic calorimeter
      logical mjpsi !--> True if in jpsi mass, false if not
      integer zsec !--> Gives the section of the detector in which the event
                   !    vertex lies.
                   !   1 = DC (zmin to 405)
                   !   2 = Other (-24 to zmin)
                   !   3 = Coil (-50 to -24)
                   !   4 = Upstream (-70 to -50)
      double precision delstart !--> Difference in 2-track first hit z-positions
      double precision delz !--> Difference between "posz" and "zvr"
      double precision rDCA !--> Distance of closest apporach of tracks from my extrapolation of >=2 muons
      logical muextrap !--> Use my muon extrapolation on the data?
      !------------------------------------------------------------------------!





      !-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-!
      !-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-!
      !-INIT-INIT-INIT-                        -INIT-INIT-INIT-INIT-!
      !-INIT-INIT-INIT-     Initialization     -INIT-INIT-INIT-INIT-!
      !-INIT-INIT-INIT-                        -INIT-INIT-INIT-INIT-!
      !-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-!
         ! Initialize Kinematic variable array:
         do ii=1,nvar
          KinVar(ii)= -9999999999.0D0
         enddo

         ! Initialize check variable: 
         do ii=1,extyp
          check(ii)=0
         enddo
      !-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-!
      !-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-!
      !-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-INIT-!



      !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
      ! MC Weighting Section
       wt=1.0D0 !--> Data

       ! MC z-reweight
       if(nttyp.ne.0.and.nttyp.ne.nttypobg) 
     |               call McWtDC_wts(1,6,zvs,1,wt) !--> MC

       ! Set the DS renormalization value:
       if(nttyp.ne.iCorr) rnorm=1.0D0
       if(nttyp.eq.iCorr) rnorm=renorm

      !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!


      
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! ////////////////////////////////////////////////////////////////////////////////////////////
! Calculation Section:


      ! Set index "one" of most upstream V0 (primary V0) for OBG:
      if (nvzero.ge.1) then
       minz = 9999.0
       do ii=1,nvzero
        if (v0posz(ii).lt.minz) then
          one = ii
          minz = v0posz(ii)
         endif
       enddo
      else
       one=0
      endif

      ! Set the primary vertex position
      if (nttyp.ne.nttypobg) then
       posx = dble(xvr)
       posy = dble(yvr)
       posz = dble(zvr)
      elseif (nttyp.eq.nttypobg.and.nvzero.ge.1) then
       posx = dble(v0posx(one))
       posy = dble(v0posy(one))
       posz = dble(v0posz(one))
      elseif (nttyp.eq.nttypobg.and.nvzero.le.0) then
       posx = dble(xvr)
       posy = dble(yvr)
       posz = dble(zvr)
      endif

      ! Set zmin value (minimum zvr cut for DC)
      if (nttyp.eq.0.or.nttyp.eq.nttypobg) then
        if (run.le.8375) then
              zmin=265.0D0
        elseif (run.le.9344) then
              zmin=115.0D0
        elseif (run.le.14164) then
              zmin=5.0D0
        else 
              zmin=35.0D0
        endif
      else
             zmin = 5.0D0
      endif


        ! Find the number of muons:
        nmu=0
        if(nttyp.ne.nttypobg) then !--> For normal components
         if(ncand.ge.1) then
         do ii=1,ncand
          if(ismu(ii)) nmu=nmu+1
         enddo
         endif
        else !--> For OBG
         if(nvzero.ge.1) then
          if(pphaseii(one).eq.1) nmu=nmu+1
          if(nphaseii(one).eq.1) nmu=nmu+1
         endif
        endif

        ! Find the neutral energy in ECAL clusters:
        Eclust = 0.0D0
        if(nclu.gt.0) then
         do ii=1,nclu
          Eclust=Eclust+dble(Ecluster(ii))
         enddo
        endif
        ! Find the "transverse" neutral hadron energy:
        PtNeutH=dsqrt( dble(PxHneuR)**2 + 
     |                dble(PyHneuR)**2   )
        ! Find the total neutral hadron momentum:
        PneutH =dsqrt( dble(PxHneuR)**2 + 
     |                dble(PyHneuR)**2 +
     |                dble(PzHneuR)**2   )
        Ehcal=dble(HCalNeu)


      ! Should the muon extrapolation be performed?
      muextrap=.false.
      if(extrapsw.eq.1.and.nttyp.eq.0.and.
     |   nmu.ge.2.and.posz.lt.zmin) muextrap=.true. 

      ! Perform variable calculations for the two tracks: 
      call muon_calculations( nttyp, nttypobg, one, nmu,
     |                          imu,Eclust,PcluAsym,
     |                          P1,P2,Pt1,Pt2,
     |                          Theta1,Theta2,Theta12,
     |                          Zeta1,Zeta2,
     |                          Phi1,Phi2,Phi12,
     |                          Pasym,
     |                          Charge1, Charge2,
     |                          Pparent,PtParent,Mparent,Mee,
     |                          ThetaParent,ZetaParent,PhiParent,
     |                          ptpos,ptneg,
     |                          thetaX1,thetaX2,thetaY1,thetaY2,
     |                          posx,posy,posz,muextrap,rDCA)

      ! Calculate the difference in first-hit Z-position:
      delstart=-999999999999.0D0
      if(nttyp.eq.0.and.imu(1).ne.0.and.imu(2).ne.0) 
     | delstart=dble(ZStartLep(imu(2))-ZStartLep(imu(1)))

      ! Find the difference between "posz" and "zvr" (for hand-extrapolated events)
      delz=posz-dble(zvr)

      ! Find the section of the detector in which the vertex lies:
      call findzsec(posz,zmin,zsec)


      ! Calculate primordial angle 
      ! (angle between the line connecting the primary
      !  vertex and the V0 with the net momentum of the V0):
      if(nvzero.ne.0) then
      do ii=1,nvzero
       if (nttyp.eq.nttypobg.and.ii.eq.one) goto 22 !--> Skip the OBG primary V0
        ! Vector between primary vertex and V0
        Vec1(0) = 0.0D0
        Vec1(1) = dble(v0posx(ii))-posx
        Vec1(2) = dble(v0posy(ii))-posy
        Vec1(3) = dble(v0posz(ii))-posz
        Vec1(4) = dsqrt(Vec1(1)**2+Vec1(2)**2+Vec1(3)**2)
        ! Net momentum of V0
        Vec2(0) = 0.0D0
        Vec2(1) = dble(pv0px(ii))+dble(nv0px(ii)) 
        Vec2(2) = dble(pv0py(ii))+dble(nv0py(ii)) 
        Vec2(3) = dble(pv0pz(ii))+dble(nv0pz(ii)) 
        Vec2(4) = dsqrt(Vec2(1)**2+Vec2(2)**2+Vec2(3)**2)
        if (ii.gt.22) print*,'Error 398892 (thprimord overfill)',nvzero
        if (ii.gt.22) goto 22
       call angle_cal(thprimord(ii),Vec1,Vec2)
22     continue
      enddo
      endif


      ![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]!
      ![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]!
      ! Calculate the DS correction:
       DScorr = 1.0D0

       if(nttyp.eq.iCorr) then
     
        ! 2D Correction:
        if(usecorr2D.eq.1)
     |       call usecorr_2D(   nbin2Dmass,  nbin2Dzeta,
     |                        binlim2Dmass,binlim2Dzeta,
     |                        DScorr2D,Mparent,zetaParent,DScorr)
        ! Mass Correction:
        if(usecorrMass.eq.1) 
     |       call usecorr_1D( nbinMass,binlimMass,
     |                        DScorrMass,Mparent,DScorr)
        ! Zeta Correction:
        if(usecorrZeta.eq.1) 
     |       call usecorr_1D( nbinZeta,binlimZeta,
     |                        DScorrZeta,ZetaParent,DScorr)

       endif
      ![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]!
      ![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]!


      !=============================================!
      ! Fill Kinematic Array:
      !-----------------------!
      ! First track:
      KinVar(1) = P1(1)
      KinVar(2) = P1(2) 
      KinVar(3) = P1(3)
      KinVar(4) = P1(4)
      KinVar(5) = P1(0)
      KinVar(6) = Pt1
      KinVar(7) = Theta1
      KinVar(8) = Zeta1
      KinVar(9) = Phi1
      KinVar(10)= dble(Charge1) 
      !-----------------------!
      ! Second track:
      KinVar(11)= P2(1) 
      KinVar(12)= P2(2)
      KinVar(13)= P2(3)
      KinVar(14)= P2(4)
      KinVar(15)= P2(0)
      KinVar(16)= Pt2 
      KinVar(17)= Theta2
      KinVar(18)= Zeta2
      KinVar(19)= Phi2
      KinVar(20)= dble(Charge2)
      !-----------------------!
      ! Parent Particle:
      KinVar(21)= Pparent(1) 
      KinVar(22)= Pparent(2)
      KinVar(23)= Pparent(3)
      KinVar(24)= Pparent(4)
      KinVar(25)= Pparent(0)
      KinVar(26)= PtParent
      KinVar(27)= ThetaParent
      KinVar(28)= ZetaParent
      KinVar(29)= PhiParent
      KinVar(30)= Mparent
      KinVar(31)= Eclust 
      !-----------------------!
      ! Variables relating the two tracks:
      KinVar(32)= Theta12
      KinVar(33)= Phi12
      KinVar(34)= Pasym
      !-----------------------!
      ! Extra:
      KinVar(35)= posx
      KinVar(36)= posy
      KinVar(37)= posz
      KinVar(38)= thetaX1 
      KinVar(39)= thetaX2
      KinVar(40)= thetaY1
      KinVar(41)= thetaY2
      KinVar(42)= Ehcal
      KinVar(43)= PtNeutH
      KinVar(44)= PneutH
      KinVar(45)= delstart 
      KinVar(46)= delz
      KinVar(47)= dble(zvr)
      !=============================================!


      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ! Get Checks for Special Region Histograms:

      ! 4 main regions (indexed by "zsec"):
      ! (1) DC:      zmin < zvr < 405
      ! (2) Other:    -24 < zvr < zmin
      ! (3) Coil:     -50 < zvr < -24
      ! (4) Upstream: -70 < zvr < -50

      ! Cuts for each:
      ! Zeta sig (zeta<zetaS)
      ! Zeta bkg (zeta>zetaS)
      ! Mass blind (Mparent blind)
      ! zetaSig and zetaBkg mass blind

      ! Is this event within the jpsi mass?
      if(Mparent.ge.2.7D0 .and. Mparent.le.3.5D0 ) then
       mjpsi=.true.
      else
       mjpsi=.false.
      endif

      ! Loop over the detector sections and set check values:
      idxdet=0
      do ii=1,ndetsec
      if(detsw(ii).eq.1) then !--> Only if the detector section is set on "on"
      idxdet=idxdet+1
      if(ii.eq.zsec) then !--> Only passes checks if we are looking at the correct detector section

      ! Loop over ncand values
      idxncnd=0
      do jj=1,nncand
       ! Is the switch for this ncand type on?:
       if(ncndsw(jj).eq.1) then
       idxncnd=idxncnd+1
       ! Skip obg for ncand.ne.2
       if( (nttyp.eq.nttypobg.and.jj.eq.1) .or. nttyp.ne.nttypobg) then
       ! Check ncand:
       if( nttyp.ne.nttypobg.and. 
     |    ( (jj.le.3.and.ncand.eq.jj+1) .or.
     |      (jj.eq.4.and.(ncand.eq.3.or.ncand.eq.4))) ) then
        
        !------
        ! All Events:
        idx=getcheckid(1,idxdet,idxncnd)
                                check(idx)=1 !--> All events in the section
        !------
        ! Mass blind (MB):
        idx=getcheckid(2,idxdet,idxncnd)
        if(.not. mjpsi)         check(idx)=1 ! Mass blind
        !------
        ! Mass signal (Msig):
        idx=getcheckid(3,idxdet,idxncnd)
        if(mjpsi)               check(idx)=1 ! Mass signal 
        !------
        ! Mass Norm Range (Mnorm):
        idx=getcheckid(4,idxdet,idxncnd)
        if(Mparent.ge.1.0D0)    check(idx)=1 ! Mass norm. region 
        !------
        
      
       endif
       endif
       endif
      enddo
      endif
      endif
      enddo
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


! End Calculation Section
!  ////////////////////////////////////////////////////////////////////////////////////////////
! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
!  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
      
      ! Initialize cut passing checkers:
      do ii=1,nevtyp
       typpass(ii)=.true.
      enddo

      ! Initialize the cut counting index (counting cuts actually plotted)
      idxcut=0
       
      !=====================================================!
      !=====================================================!
      !                       Cut 01                        !
      !              ---*** All Events ***---               !
      !_____________________________________________________!
      ! A count of all events in opened ntuple(s).          !
      !----------                                 ----------!     
       call countevent(1,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(1,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 02                        !
      !       ---***  OBG Primary Vertex FV ***---          !
      !       ---***  OBG Primary Vertex Mu ***---          !
      !      ---*** OBG 1 to 3 primary tracks ***---        !
      !        ---***  OBG V0 Trigger Cut  ***---           !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     


      if (nttyp.eq.nttypobg) then
       ! OBG Primary vertex in FV in X and Y
       if (abs(xvr).gt.140.0) call failall(nevtyp,typpass)
       if (abs(yvr-5.0).gt.140.0) call failall(nevtyp,typpass)
       ! OBG Primary vertex upstream of DC
       if (zvr.ge.zmin) call failall(nevtyp,typpass)
       ! OBG Primary vertex has a muon (CC event)
       call label_muid(phaseii,iimu,other)
       if(phaseii.eq.0) call failall(nevtyp,typpass)
       ! OBG primary vertex topology:
       if (ncand.le.0.or.ncand.ge.4) call failall(nevtyp,typpass)
       
       ! Simulated trigger check for OBG (One V0 track hits second trigger plane)
       if(nvzero.ge.1) then
        if(  (     pv0endz(one)     .lt.540.0.or.
     |         abs(pv0endx(one))    .gt.130.0.or.
     |         abs(pv0endy(one)-5.0).gt.130.0      )  .and.
     |       (     nv0endz(one)     .lt.540.0.or.
     |         abs(nv0endx(one))    .gt.130.0.or.
     |         abs(nv0endy(one)-5.0).gt.130.0      )  )
     |                   call failall(nevtyp,typpass)
       endif
      endif

       call countevent(2,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(2,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 03                        !
      !          ---*** Pfermi and W2 Cuts  ***---          !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (nttyp.ne.0.and.nttyp.ne.nttypobg) then
        if((pxfermi**2+pyfermi**2+pzfermi**2).gt.1.0) 
     |    call failall(nevtyp,typpass)
      endif
      if (nttyp.eq.1.or.nttyp.eq.2) then !--> For DIS
        if (w2s.lt.1.96) call failall(nevtyp,typpass)
      endif

       call countevent(3,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(3,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 04                        !
      !          ---*** Primary V0 Fid. - X ***---          !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (dabs(posx).gt.120.0D0) call failall(nevtyp,typpass)

       call countevent(4,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(4,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 05                        !
      !          ---*** Primary V0 Fid. - Y ***---          !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (dabs(posy-5.0D0).gt.120.0D0) call failall(nevtyp,typpass)

       call countevent(5,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(5,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 06                        !
      !          ---*** Primary V0 Fid. - Z ***---          !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (posz.lt.-70.0D0.or.posz.gt.405.0D0) 
     |                            call failall(nevtyp,typpass)

cc       if (posz.lt.-50.0D0.or.posz.gt.405.0D0) call failall(nevtyp,typpass)

       call countevent(6,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(6,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 07                        !
      !          ---*** At least one Muon ***---            !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if(nmu.lt.1) call failall(nevtyp,typpass)

       call countevent(7,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(7,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 08                        !
      !          ---***  2-Track Primary ***---             !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (nttyp.ne.nttypobg.and.ncand.lt.1) 
     |                            call failall(nevtyp,typpass)
      if (nttyp.ne.nttypobg.and.
     |         (ncand.le.1.or.ncand.gt.4) ) 
     |                            call failall(nevtyp,typpass)

       ! OBG must have a V0 
       if(nttyp.eq.nttypobg.and.nvzero.le.0) 
     |                            call failall(nevtyp,typpass)

       call countevent(8,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(8,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 09                        !
      !            ---*** 2-Track Cut #2 ***---             !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (nttyp.ne.nttypobg.and.ncand.eq.2.and.tnchgd.ne.2) 
     |                                   call faildimu(nevtyp,typpass)

       call countevent(9,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(9,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 10                        !
      !             ---***  Charge Cut ***---               !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if(Charge1.ne.-1) typpass(1)=.false.
      if(Charge1*Charge2.eq. 1) typpass(2)=.false.
      if(Charge1*Charge2.eq.-1) typpass(3)=.false.
      if(Charge1*Charge2.eq. 1) typpass(4)=.false.

       call countevent(10,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(10,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 11                        !
      !             ---***  Tube/Veto ***---                !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
c      call label_muid(phaseii,iimu,other)
c      call dcveto(ncand,totveto(iimu),tottube(iimu),lveto,ltube)
c      if (nttyp.ne.nttypobg) then
c       if (.not.ltube.or..not.lveto) call failall(nevtyp,typpass)
c      endif

       call countevent(11,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(11,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 12                        !
      !             ---***  Two Muons ***---                !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
       ! Check for Two Muons
       if(nmu.ne.2) call faildimu(nevtyp,typpass)
       if(nmu.ne.1) typpass(4)=.false.

       call countevent(12,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(12,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 13                        !
      !             ---*** Mom. Asym. ***---                !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
c       if(Pasym.gt.0.0D0) call failall(nevtyp,typpass)
        if((Pasym.lt.-1.0D0).or.(Pasym.gt.+0.0D0)) 
     |                     call failall(nevtyp,typpass)

       call countevent(13,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(13,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 14                        !
      !            ---***  Opening Angle ***---             !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (theta12.gt.2.62D0) call failall(nevtyp,typpass)

       call countevent(14,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(14,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 15                        !
      !                ---***  PtPos ***---                 !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (ptpos.lt.0.05D0) call failall(nevtyp,typpass)

       call countevent(15,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(15,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 16                        !
      !              ---*** No Photons ***---               !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
c      if (Mee.lt.2.0D0) call failall(nevtyp,typpass)

       call countevent(16,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(16,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 17                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (nttyp.ne.nttypobg) then
      do ii=1,nsecond
        if (secstartz(ii).lt.zvr  .and.
     |      abs(secpy(ii)/secpz(ii)).lt.0.4) 
     |                  call failall(nevtyp,typpass)
        if (zvr-secstartz(ii).gt.175.0) call failall(nevtyp,typpass)
      enddo
      endif

       call countevent(17,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(17,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 18                        !
      !             ---*** # of secondary ***---            !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (nttyp.ne.nttypobg.and.nsecond.gt.3) 
     |                   call failall(nevtyp,typpass)

       call countevent(18,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(18,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 19                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
       if (nttyp.ne.nttypobg) then
         do ii=1,nsecond
          if (abs(secstartx(ii)).gt.120.0.or.abs(secstarty(ii)-5)
     +      .gt.120.0.or.secstartz(ii).gt.405.0.or.secstartz(ii).lt.
     +        real(zmin)) call failall(nevtyp,typpass)
         enddo
       endif

       call countevent(19,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(19,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 20                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (nttyp.ne.nttypobg) then
       do ii=1,nsecond  
        if ((secstartz(ii)-zvr).lt.20.0) call failall(nevtyp,typpass)
       enddo
      endif

       call countevent(20,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(20,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 21                        !
      !         ---***  Positive Z-Momentum ***---          !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (P1(3).lt.0.0D0) call failall(nevtyp,typpass)
      if (P2(3).lt.0.0D0) call faildimu(nevtyp,typpass)

       call countevent(21,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(21,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 22                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if(nvzero.gt.0) then
      do ii=1,nvzero
       if (nttyp.eq.nttypobg.and.ii.eq.one) goto 33
       if (((v0posz(ii)-real(posz)).lt.10.0)) 
     |                            call failall(nevtyp,typpass)
        if (ii.gt.22) print*,'Error 368112 (thprimord overfill)',nvzero
        if (ii.gt.22) goto 33
       if (thprimord(ii).lt.0.4D0) call failall(nevtyp,typpass)
33     continue
      enddo
      endif

       call countevent(22,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(22,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 23                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (nttyp.ne.nttypobg.and.(nunh*fracunh).gt.200) 
     |                                   call failall(nevtyp,typpass)

       call countevent(23,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(23,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 24                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (Pparent(0).lt.2.0D0) call faildimu(nevtyp,typpass)

       call countevent(24,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(24,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 25                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (P1(4).lt.0.5D0) call failall(nevtyp,typpass)
      if (P2(4).lt.0.5D0) call faildimu(nevtyp,typpass)

       call countevent(25,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(25,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 26                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (P1(4).lt.1.0D0) call failall(nevtyp,typpass)
      if (P2(4).lt.1.0D0) call faildimu(nevtyp,typpass)

      if (P1(4).lt.2.5D0) typpass(4)=.false. 
      if (P2(4).lt.2.5D0) typpass(4)=.false. 

       call countevent(26,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(26,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 27                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
      if (Pparent(0).lt.5.0D0) call faildimu(nevtyp,typpass)
      if (Pparent(0).lt.8.0D0) typpass(4)=.false.

       call countevent(27,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(27,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 28                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
C      if (Phi12.lt.90.0D0) call failall(nevtyp,typpass)

       call countevent(28,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(28,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!
      !                       Cut 29                        !
      !_____________________________________________________!
      !                                                     !
      !----------                                 ----------!     
C      if (Pparent(4).lt.10.0D0) call faildimu(nevtyp,typpass)
C      if (Pparent(4).lt.10.0D0) typpass(4)=.false.

       call countevent(29,ncuts,nmc,nttyp,nevtyp,nextyp,nncand,ndetsec,
     |                cuts,typpass,wt,iCorr,rnorm,DScorr,
     |                             extyp,check,ncndsw,detsw)
       call fillhists(29,nmc,nvar,ncuts,extyp,nttyp,nevtyp,check,
     |              cuthists,nbintyp,typpass,KinVar,norm,wt,
     |                      iCorr,rnorm,DScorr,evtsw,idxcut)
      !=====================================================!




       !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!
       ! Get number of events after cuts for DS Reweighting:
       if(nttyp.eq.iCorr.and.formRenorm.eq.1) then
        if(typpass(2)) then !--> OS only
          reCounts(1) = reCounts(1)+wt*norm(1,nttyp)
          reCounts(2) = reCounts(2)+wt*norm(1,nttyp)*DScorr
        endif
       endif
       !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!


       !--------------------------------------------------!
       !--------------------------------------------------!
       ! Print event picture information:
         if(evtpicsw.eq.1) then
         if(nttyp.eq.0) then !--> Data only
         if(typpass(2)) then !--> OS only
         if(ncand.eq.2) then !--> 2-track only
c          if(Mparent.ge.2.7D0.and.Mparent.le.3.5D0) then
          if(dabs(delstart).gt.20.0D0) then
          write(evtID(1),301) run,event

          call write_evt_table(evtID,run,event,
     |          posx,posy,posz,
     |          Mparent,Pparent(0),
     |          ZetaParent,
     |          Pasym,Phi12,
     |          P1(4),P2(4),
     |          Eclust,PcluAsym,Ehcal,
     |          ZStartLep(imu(1)),ZstartLep(imu(2)))
          endif
         endif
         endif
         endif
         endif
301      format(2I10) 
       !--------------------------------------------------!
       !--------------------------------------------------!
       
       
      return 
      end
