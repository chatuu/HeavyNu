      subroutine muon_extrapolation(nmu,mucharge,pxmu,pymu,pzmu,
     |                              xftrk,yftrk,zftrk,
     |                              imu1,imu2,
     |                              Pout1,Pout2,VertOut,rDCA)
      implicit none

      ! This subroutine extrapolates the DC momentum vector
      ! for TWO muons (assumed two) through the coil and
      ! into the FCAL.
      
      !------------------------------------------------------------------------------------
      ! Input variables from Ntuple:
      integer nmu !--> Number of muons
      integer mucharge(25) !--> Muon charges
      real pxmu(25),pymu(25),pzmu(25) !--> DC muon momenta 
      real xftrk(25),yftrk(25),zftrk(25) !--> Position of first hit of track (TRACK BLOCK)

      ! Other input
      integer imu1,imu2 !--> Index number of the muons

      ! Output:
      double precision Pout1(0:4),Pout2(0:4) !--> Output momentum and energy
      double precision VertOut(3) !--> Output vertex position (in [cm])
      double precision rDCA !--> DCA length
      !------------------------------------------------------------------------------------

      !==================================================================================!
      !  USER INPUT
      double precision zcd !--> Z position (in [m]) of Coil's downstream face
      parameter (zcd = -0.27D0) !--> -27cm
      double precision zcu !--> Z position (in [m]) of Coil's upstream face
      parameter (zcu = -0.50D0) !--> -50cm
      double precision zFCAL !--> Most downstream z position (in [m]) of the FCAL
      parameter (zFCAL = -1.0D0) !--> -100cm
      !==================================================================================!

      ! Temporary variables from input:
      integer MuQ1,MuQ2 !--> Muon Charges
      double precision fPos1(3),fPos2(3) !--> First hit position (in [m]) of muons
      double precision PDC1(0:4),PDC2(0:4) !--> DC momentum vectors (LAB FRAME)

      ! Intermediate Values:
      double precision XYext1(2),XYext2(2) !--> X and Y coordinates at the upstream face of the Coil
      double precision VertCoil(3) !--> DCA vertex within coil
      double precision rDCAcoil !--> DCA length within coil
      ! Momentum and energy at minimum DCA within the DC/Coil:
      double precision Pcoil1(0:4),Pcoil2(0:4)

      ! Program Variables:
      double precision Mmu !--> Muon Mass (GeV)
      parameter (Mmu=0.105658367D0)
      integer ii !--> Do loop variable(s)


      !*******************************!
      ! Initialize output variables:
      do ii=0,4
       Pout1(ii)= -999999999.0D0
       Pout2(ii)= -999999999.0D0
      enddo
      do ii=1,3
       VertOut(ii)= -999999999.0D0
      enddo
      rDCA= -999999999.0D0 
      !*******************************!


      if(nmu.le.1) return !--> This subroutine is only for at least 2 muons

      !***************************************************! 
      ! Check muon index input:
c      if(.not.( (imu1.eq.1.and.imu2.eq.2) .or.
c     |          (imu1.eq.2.and.imu2.eq.1) ) ) then
c       call printerror(1,112706620,'muon_extrapolation.F')
c       print*,'Muon indices out of range!'
c       print*,'imu1: ',imu1
c       print*,'imu2: ',imu2
c       print*,''
c       stop
c      endif
      !***************************************************! 

      !===============================================================!
      ! Put input variables into subroutine definitions:

      ! Read the charges of the muons:
      MuQ1 = mucharge(imu1)
      MuQ2 = mucharge(imu2)

      ! Read in the first hit positions and change from [cm] to [m]:
      fPos1(1) = dble(xftrk(imu1))/100.0D0
      fPos1(2) = dble(yftrk(imu1))/100.0D0
      fPos1(3) = dble(zftrk(imu1))/100.0D0
      fPos2(1) = dble(xftrk(imu2))/100.0D0
      fPos2(2) = dble(yftrk(imu2))/100.0D0
      fPos2(3) = dble(zftrk(imu2))/100.0D0

      ! Read in the Drift Chamber momenta of the muons:
      PDC1(1) = dble(pxmu(imu1))
      PDC1(2) = dble(pymu(imu1))
      PDC1(3) = dble(pzmu(imu1))
      PDC2(1) = dble(pxmu(imu2))
      PDC2(2) = dble(pymu(imu2))
      PDC2(3) = dble(pzmu(imu2))
      ! Now calculate the total momentum and energy:
      PDC1(4) = dsqrt(PDC1(1)**2 + PDC1(2)**2 + PDC1(3)**2) 
      PDC2(4) = dsqrt(PDC2(1)**2 + PDC2(2)**2 + PDC2(3)**2) 
      PDC1(0) = dsqrt(PDC1(4)**2 + Mmu**2)
      PDC2(0) = dsqrt(PDC2(4)**2 + Mmu**2)
      !===============================================================!


      ! Extrapolate the muons through the DC and the Coil:
      call DC_Coil_extrap(zcd,zcu,
     |                    MuQ1,fPos1,PDC1,XYext1,Pout1, 
     |                    MuQ2,fPos2,PDC2,XYext2,Pout2,
     |                    VertCoil,rDCAcoil,Pcoil1,Pcoil2) 

      ! Find FCAL vertex (DCA) and add lost energy to the momentum vectors
      call FCAL_extrap(zcu,zFCAL,XYext1,XYext2,
     |                     Pout1,Pout2,VertOut,rDCA)

      ! Vertex in the coil rather than FCAL:
      if (rDCAcoil.lt.rDCA) then
       VertOut(1)=VertCoil(1)
       VertOut(2)=VertCoil(2)
       VertOut(3)=VertCoil(3)
       do ii=0,4
        Pout1(ii)=Pcoil1(ii)
        Pout2(ii)=Pcoil2(ii)
       enddo
       rDCA=rDCAcoil
      endif

      ! Change length output from [m] to [cm]:
      VertOut(1) = VertOut(1)*100.0D0
      VertOut(2) = VertOut(2)*100.0D0
      VertOut(3) = VertOut(3)*100.0D0
      rDCA = rDCA*100.0D0

      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine DC_Coil_extrap(zcd,zcu,
     |                       charge1,fPos1,PDC1,XYext1,Pext1,
     |                       charge2,fPos2,PDC2,XYext2,Pext2,
     |                       Vertex,rDCA,Pcoil1,Pcoil2)
      implicit none

      ! Extrapolate a single charged track through the DC to the downstream face of the Coil. 
      ! Then extrapolate through the coil to its upstream face.

      ! Input/Output:
      double precision zcd !--> Z-Position (in [m]) of the downstream face of the Coil
      double precision zcu !--> Z-Position (in [m]) of upstream face of the coil
      ! First muon variables
      integer charge1 !--> Charge sign of the muon (+/- 1)
      double precision fPos1(3) !--> First hit position of the muon
      double precision PDC1(0:4) !--> DC momentum of the muon
      double precision XYext1(2) !--> Extrapolated X-Y position at the upstream face of the Coil
      double precision Pext1(0:4) !--> Extrapolated momentum at the upstream Coil face
      ! Second muon variables
      integer charge2 !--> Charge sign of the muon (+/- 1)
      double precision fPos2(3) !--> First hit position of the muon
      double precision PDC2(0:4) !--> DC momentum of the muon
      double precision XYext2(2) !--> Extrapolated X-Y position at the upstream face of the Coil
      double precision Pext2(0:4) !--> Extrapolated momentum at the upstream Coil face
      ! Vertex position of smallest DCA within the DC/Coil:
      double precision Vertex(3)
      ! DCA length within the DC/coil:
      double precision rDCA
      ! Momentum/Energy at the minimum DCA within the DC/Coil:
      double precision Pcoil1(0:4),Pcoil2(0:4)

      !==================================================================================!
      !   USER INPUT
      double precision zstep !--> Length of z step size in [m] (SHOULD BE NEGATIVE!)
      parameter (zstep=-2.0D-4) ! 0.2 mm
      !==================================================================================!

      ! Program Variables:
      integer ii,istep !--> Do loop variables
      integer last !--> A tag to mark the final step
      integer nstep !--> Number of steps made in each section
      double precision lzstep !--> Size of final step in Z
      double precision izstep !--> Current step size (generally =zstep)
      double precision Z !--> Current Z value
      double precision Ztmp
      double precision rDCAtmp

      ! Check that the tracks are upstream of the coil!
      if(fPos1(3).le.zcu.or.fPos2(3).le.zcu) then
       print*,''
       print*,'Error in DC/Coil Extrapolator!'
       print*,'Event first-hit z-position within the coil!'
       print*,'Stopping Program!'
       print*,''
       print*,'Z-positions: ',fPos1(3),fPos2(3)
       print*,'Coil Face: ',zcu
       stop
      endif

      ! Initialize output vectors:
      XYext1(1)=fPos1(1)
      XYext1(2)=fPos1(2)
      Pext1(1)=PDC1(1)
      Pext1(2)=PDC1(2)
      Pext1(3)=PDC1(3)
      Pext1(4)=PDC1(4)
      Pext1(0)=PDC1(0)
      XYext2(1)=fPos2(1)
      XYext2(2)=fPos2(2)
      Pext2(1)=PDC2(1)
      Pext2(2)=PDC2(2)
      Pext2(3)=PDC2(3)
      Pext2(4)=PDC2(4)
      Pext2(0)=PDC2(0)

      !--------------------------------------------------------!
      ! Bring the tracks to the same Z-position
      ! (generally the first track of one of the
      !  muons will be ahead of the other, though
      !  both should be close to "zmin")
      nstep = int(dabs((fPos1(3)-fPos2(3))/zstep)) !--> Number of steps
      lzstep = dsign(1.0D0,zstep)*
     |         dmod(dabs(fPos1(3)-fPos2(3)),dabs(zstep)) !--> Size of last step 
      izstep= zstep !--> Current Z-step 
      last=0 !--> Initialize the last step tag

      ! Initialize the current Z value:
      if(fPos1(3).ge.fpos2(3)) Z = fPos1(3) 
      if(fPos2(3).gt.fpos1(3)) Z = fPos2(3) 

      if(fPos1(3).ne.fpos2(3)) then
        do istep=1,nstep !--> Loop over steps
11      continue !--> To perform the final step
        !================================================!
        ! Perform a single step along the z-axis:
        if(fPos1(3).ge.fpos2(3)) 
     |    call onestep(zcu,zcd,izstep,charge1,Pext1,XYext1,Z)
        if(fPos2(3).gt.fpos1(3)) 
     |    call onestep(zcu,zcd,izstep,charge2,Pext2,XYext2,Z)
        !================================================!
        if(istep.eq.nstep.and.last.ne.1) then !--> Perform the last step
         izstep=lzstep
         last=1
         goto 11
        endif
        enddo
      endif
      !--------------------------------------------------------!


      ! Now that both particles have the same Z-position:



      ! Initialize variables for DCA within the coil/DC section:
      rDCA=9999999999999.0D0
      do ii=0,4
       Pcoil1(ii)=-999999999.0D0
       Pcoil2(ii)=-999999999.0D0
      enddo
      do ii=1,3
       Vertex(ii)=-999999999.0D0
      enddo

      ! Set the number of steps and step sizes from user input:
      nstep = int((Z-zcu)/dabs(zstep)) !--> Number of steps
      lzstep = dsign(1.0D0,zstep)*dmod((Z-zcu),dabs(zstep)) !--> Size of last step 
      izstep= zstep !--> Current Z-step 
      last=0 !--> Initialize the last step tag


      do istep=1,nstep !--> Loop over steps

33    continue !--> To perform the final step

      !===========================================================!
      ! Perform a single step along the z-axis for both particles:
      Ztmp=Z
      call onestep(zcu,zcd,izstep,charge1,Pext1,XYext1,Z)
      call onestep(zcu,zcd,izstep,charge2,Pext2,XYext2,Ztmp)
      !===========================================================!

      ! Check if we have a minimum rDCA:
      rDCAtmp=dsqrt( (XYext1(1)-XYext2(1))**2 +
     |               (XYext1(2)-XYext2(2))**2  )
      if(rDCAtmp.lt.rDCA) then
       rDCA=rDCAtmp
       do ii=0,4
        Pcoil1(ii)=Pext1(ii)
        Pcoil2(ii)=Pext2(ii)
       enddo
       Vertex(1)=0.5D0*(XYext1(1)+XYext2(1))
       Vertex(2)=0.5D0*(XYext1(2)+XYext2(2))
       Vertex(3)=Z
      endif

      if(istep.eq.nstep.and.last.ne.1) then !--> Perform the last step
       izstep=lzstep
       last=1
       goto 33
      endif

      enddo
      
      ! Make sure that we have reached the face, as planned:
      if(dabs(Z-zcu).gt.0.00000001D0*dabs(zcu)) then
        print*,''
        print*,'Warning in DC/Coil extrapolation!'
        print*,'Have not reached the end of the coil.'
        print*,'Z: ',Z
        print*,'Coil face: ',zcu
        print*,''
      endif


      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine onestep(zcu,zcd,izstep,charge,Pext,XYext,Z)
      implicit none

      ! Make a single step alon the z-axis within the DC (constant) and Coil (linear) 
      ! magnetic fields for a single particle.
      ! The subroutine will return a new momentum/energy vector and a new position.

      ! Input/Output Variables:
      double precision zcu !--> Z-Position (in [m]) of upstream face of the coil
      double precision zcd !--> Z-Position (in [m]) of the downstream face of the Coil
      double precision izstep !--> Step size to be made
      integer charge !--> Charge of the particle
      ! Output variables (modified input):
      double precision Pext(0:4) !--> Particle's momentum and energy
      double precision XYext(2) !--> X and Y position of the particle
      double precision Z !--> Particle's Z-Position

      !==================================================================================!
      ! USER INPUT:
      double precision Bo !--> Magnetic field in x-direction in [T]
      parameter (Bo = 0.4D0) 
      double precision C0_DC !--> Stopping force in DC with density 0.1 g/cm^3 in [GeV/m]
      parameter (C0_DC=0.02D0)
      double precision C0_Coil !--> Stopping force in Coil with density 2.36 g/cm^3 in [GeV/m]
      parameter (C0_Coil=0.472D0)
      ! Information regarding the Steel Basket:
      double precision C0_Basket !--> Stopping force for steel basket with density 7.75 g/cm^3 (can range up to 8.05) in [GeV/m] (for basket)
      parameter (C0_Basket=1.55D0)
      double precision ZbasketMin !--> Minimum Z-Position of basket in [m]
      parameter (ZbasketMin=-0.05D0) ! -5cm
      double precision ZbasketMax !--> Maximum Z-Position of basket in [m]
      parameter (ZbasketMax=0.0D0)
      double precision Xbasket(2,3) !--> Min/Max X-Positions of 3 vertical basket bars in [m]
      save Xbasket
      data Xbasket/
     |    -1.0D0, -0.8D0,  ! Leftmost vertical bar (-100 to -80cm)
     |    -0.1D0,  0.1D0,  ! Central vertical bar (-10 to 10 cm)
     |     0.8D0,  1.0D0 / ! Rightmost vertical bar (80 to 100cm)
      double precision Ybasket(2,4) !--> Min/Max Y-Positions of 4 horizontal basket bars in [m]
      save Ybasket
      data Ybasket/
     |     1.15D0,  1.35D0,  ! Topmost horizontal bar (115 to 135cm)
     |     0.75D0,  0.95D0,  ! Upper horizontal bar (75 to 95cm)
     |    -0.10D0,  0.10D0,  ! Central horizontal bar (-10 to 10cm)
     |    -1.18D0, -0.98D0 / ! Bottomost horizontal bar (-118 to -98cm)
      !==================================================================================!

      ! Constants:
      double precision qe !--> Electon charge in [C]
      parameter (qe=1.60217657D-19) 
      double precision c !--> Speed of light in [m/s]
      parameter (c=2.99792458D8)
      double precision JGeV !--> Number of Joules per GeV
      parameter (JGeV = 1.60217657D-10)
      double precision Mmu !--> Muon Mass in [GeV]
      parameter (Mmu=0.105658367D0)

      ! Subroutine variables:
      double precision Ptmp(0:4) !--> Temporary momentum vector
      double precision qB !--> Charge X B-field term
      double precision C0 !--> Stopping power of material
      double precision delp !--> Change in momentum
      integer ii

      !TEST TEST TEST TMP TMP TMP TEST TEST TEST TMP TMP TMP
      !TEST TEST TEST TMP TMP TMP TEST TEST TEST TMP TMP TMP
      ! It seems that the ntuple extrapolation does not take
      ! the sign of the particle's charge into account.  To
      ! test this I may set all of my charge values to +1:
c      charge=1
      !TEST TEST TEST TMP TMP TMP TEST TEST TEST TMP TMP TMP
      !TEST TEST TEST TMP TMP TMP TEST TEST TEST TMP TMP TMP


      ! Save the initial values:
      Ptmp(1)=Pext(1)
      Ptmp(2)=Pext(2)
      Ptmp(3)=Pext(3)
      Ptmp(4)=Pext(4)
      Ptmp(0)=Pext(0)

      ! Step the current Z-position:
      Z = Z + izstep

      ! Set the qB value in units of [GeV/c * 1/m]:
      if(Z.gt.zcd) qB = charge*qe*Bo*c*(1.0D0/JGeV) !--> Constant in DC
      if(Z.ge.zcu.and.Z.le.zcd) 
     |             qB = charge*qe*Bo*c*(1.0D0/JGeV)*((Z-zcd)/(zcu-zcd)) !--> Linear in the Coil

      !------------------------------------------------------------------!
      ! Set the stopping power:
      if(Z.gt.zcd) C0=C0_DC !--> Within DC material
      if(Z.ge.zcu.and.Z.le.zcd) C0=C0_Coil !--> In the coil
      ! Inside basket material:
      if(Z.ge.ZbasketMin.and.Z.lt.ZbasketMax) then 
        ! Check if in one of 3 vertical slabs:
        do ii=1,3
         if( XYext(1).ge.Xbasket(1,ii).and.XYext(1).le.Xbasket(2,ii))
     |       C0=C0_Basket
        enddo
        ! Check if in one of 4 vertical slabs:
        do ii=1,4
         if( XYext(2).ge.Ybasket(1,ii).and.XYext(2).le.Ybasket(2,ii))
     |       C0=C0_Basket
        enddo
      endif
      !------------------------------------------------------------------!

      ! Find the change in momentum in X and add it:
      delp = -C0*(Ptmp(1)/Ptmp(3))*(Ptmp(0)/Ptmp(4))*izstep
      Pext(1) = Pext(1) + delp

      ! Find the change in momentum in Y and add it:
      delp = qB*izstep - C0*(Ptmp(2)/Ptmp(3))*(Ptmp(0)/Ptmp(4))*izstep
      Pext(2) = Pext(2) + delp

      ! Find the change in momentum in Z and add it:
      delp = -qB*izstep*(Ptmp(2)/Ptmp(3)) - C0*(Ptmp(0)/Ptmp(4))*izstep
      Pext(3) = Pext(3) + delp

      ! Set the new total momentum and energy:
      Pext(4) = dsqrt(Pext(1)**2+Pext(2)**2+Pext(3)**2)
      Pext(0) = dsqrt(Pext(4)**2 + Mmu**2)

      ! Find the new X and Y positions:
      XYext(1) = XYext(1) + (Ptmp(1)/Ptmp(3))*izstep
      XYext(2) = XYext(2) + (Ptmp(2)/Ptmp(3))*izstep


      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine FCAL_extrap(zcu,zFCAL,XYext1,XYext2,
     |                           P1,P2,Vertex,rDCA)
      implicit none

      ! Extrapolate two muons through the FCAL and find the DCA
      ! position. Also calculate the energy loss (gained) in the material.

      ! Input/Output:
      double precision zcu !--> Most upstream Z position of the Coil (in [m])
      double precision zFCAL !--> Most upstream Z-position of the FCAL (in [m])
                    !    (at edge of last iron plate +1 air gap)
      double precision XYext1(2) !--> X-Y position of first muon at coil
      double precision XYext2(2) !--> X-Y position of second muon at coil
      double precision P1(0:4) !--> First Muon Momentum (length will be modified here)
      double precision P2(0:4) !--> Second Muon Momentum (length will be modified here)
      double precision Vertex(3) !--> DCA vertex of two muons (n [m])
      double precision rDCA !--> Size of DCA (in [m])

      !==================================================================================!
      !==================================================================================!
      ! USER INPUT
      ! Constants giving energy loss in material (in GeV/m) [stopping force]:
      ! (These are based on the material density, and assuming
      !  a constant 2 MeV/(g/cm^2) )
      ! (A "block" is defined as an iron plate with its air
      !  gap, which may or may not contain scintillator)
      double precision CoIrn !--> For Iron plates (7.874 g/cm^3)
      parameter (CoIrn=1.5748D0)
      double precision CoSci !--> In Scintillator between iron plates in FCAL (1 g/cm^3)
      parameter (CoSci=0.2D0) !SAME MATERIAL AS VETO?
      double precision CoT1 !--> Average in Type 1 block with no scintillator in air gap
      parameter (CoT1=1.151D0)
      double precision CoT2 !--> Average in Type 2 block with scintillator
      parameter (CoT2=1.169D0)
      double precision CoVet !--> Veto Scintillator (1.03 g/cm^3)
      parameter (CoVet=0.206D0)

      !------------------------------------
      ! Veto Scintillator informatin:
      integer nVetoSec !--> Number of veto sections (aligned with XY plane)
      parameter (nVetoSec=6)
      double precision VetoLim(2,3,nVetoSec) !--> Min/Max X,Y,Z positions of the Veto Sections in [m]
      save VetoLim
      data VetoLim/
     |    0.298D0 ,  2.677D0 , ! 1) X min/max 29.8 to 267.7cm (Bank1 and Bank2a)
     |   -0.713D0 ,  2.287D0 , !    Y min/max -71.3 to 228.7 cm
     |   -0.842D0 , -0.821D0 , !    Z min/max -84.2 to -82.1 cm
     |   -2.677D0 , -0.298D0 , ! 2) X min/max -267.7 to -29.8 cm (Bank3 and Bank2b)
     |   -0.713D0 ,  2.287D0 , !    Y min/max -71.3 to 228.7 cm
     |   -0.842D0 , -0.821D0 , !    Z min/max -84.2 to -82.1 cm
     |   -1.5D0   ,  1.5D0   , ! 3) X min/max -150 to 150 cm (Bank6 counters 1-2) 
     |    0.0D0   ,  0.432D0 , !    Y min/max 0 to 43.2 cm
     |   -0.8795D0, -0.8585D0, !    Z min/max -87.95 to -85.85 cm
     |    0.097D0 ,  0.309D0 , ! 4) X min/max 9.7 to 30.9 cm (Bank9 counter 1) 
     |    0.687D0 ,  2.787D0 , !    Y min/max 68.7 to 278.7 cm
     |   -0.827D0 , -0.806D0 , !    Z min/max -82.7 to -80.6 cm
     |   -0.309D0 , -0.097D0 , ! 5) X min/max -30.9 to -9.7 cm (Bank9 counter 3) 
     |    0.687D0 ,  2.787D0 , !    Y min/max 68.7 to 278.7 cm
     |   -0.827D0 , -0.806D0 , !    Z min/max -82.7 to -80.6 cm
     |   -0.106D0 ,  0.106D0 , ! 6) X min/max -10.6 to -10.6 cm (Bank9 counter 2) 
     |    0.747D0 ,  2.847D0 , !    Y min/max 74.7 to 284.7 cm
     |   -0.8965D0, -0.8755D0 /!    Z min/max -89.65 to -87.55 cm

      !------------------------------------
      ! Veto info for angled Bank 6 (counters 3-8) and Bank 7.
      ! The angle is -10deg, and it is assumed this means that the top
      ! of the bank is leaning towards the FCAL, and away from the DC.
      double precision Vcent(3) !--> Center position of veto in [m]
      save Vcent
      data Vcent / 0.0D0, -1.9145D0, -0.9962D0 /
      double precision Vsiz(3) !--> Length/width/thickness in [m]
      save Vsiz
      data Vsiz / 3.0D0, 1.6622D0, 0.021D0 /
      double precision Vang !--> Angle relative to X-axis in [rad]:
      parameter (Vang=0.174532925D0) ! 10 deg


      !----------------------------------------
      ! FCAL Block z-measurements in [m]
      double precision Fl !--> Thickness of iron plates
      parameter (Fl=0.049D0)
      double precision Fw !--> Thickness of air gaps
      parameter (Fw=0.018D0)
      double precision Fb !--> Total block thickness (Fl+Fw)
      parameter (Fb=0.067D0)
      double precision Fs !--> Scintillator thickness
      parameter (Fs=0.006D0)
      double precision Fn !--> Separation between iron plate and scintillator
      parameter (Fn=0.0D0)
      !==================================================================================!
      !==================================================================================!

      !----------------------------------------------
      ! Factors often found in the DCA calculation:
      double precision Fx,Fy
      double precision delX,delY
      !----------------------------------------------
      ! For calculating energy loss:
      double precision DelE1,DelE2 !--> Energy loss upstream of Coil
      double precision dS1,dS2 !--> Factors used in energy loss calculation
      double precision tmpP !--> Temporary momentum value
      ! For Veto:
      double precision Zcent !--> Center position of veto blocks
      double precision X1,Y1,X2,Y2 !--> X-Y positions at center of veto blocks
      ! For FCAL
      integer Nblocks !--> Number of entire blocks traversed by the particles
      double precision Zlast !--> Length of the last penetrated block traversed by the particles
      integer nblk !-> Temporary block count integer
      double precision MatLenZ !--> Length along z of material traversed
      !----------------------------------------------

      ! Other Program Variables:
      integer ii !--> Do loop variable(s)
      double precision Mmu !--> Muon Mass (GeV)
      parameter (Mmu=0.105658367D0)

       

      !-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-!
      !-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-!
      !-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-!
      ! DCA Calculations:

      ! Calculate common factors in DCA calc.:
      Fx = P1(1)/P1(3) - P2(1)/P2(3)
      Fy = P1(2)/P1(3) - P2(2)/P2(3)
      delX = XYext1(1) - XYext2(1)
      delY = XYext1(2) - XYext2(2)

      ! Find length of DCA:
      rDCA=dsqrt(delX**2+delY**2-((delX*Fx + delY*Fy)**2)/(Fx**2+Fy**2))

      ! Find z-position of DCA (used to find X and Y):
      Vertex(3)=zcu- (delx*Fx + delY*Fy)/(Fx**2 + Fy**2)

      ! Now find X and Y position of DCA:
      Vertex(1) = 0.5D0*(XYext1(1)+XYext2(1)+
     |            (P1(1)/P1(3) + P2(1)/P2(3))*(Vertex(3)-zcu))
      Vertex(2) = 0.5D0*(XYext1(2)+XYext2(2)+
     |            (P1(2)/P1(3) + P2(2)/P2(3))*(Vertex(3)-zcu))
      !-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-!
      !-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-!
      !-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-DCA-!


      !=============================================================================!
      ! Initialize added energy:
      DelE1=0.0D0
      DelE2=0.0D0

      ! Calculate the constant factors of dS used in the energy loss:
      dS1 = dsqrt(1.0D0 + (P1(1)/P1(3))**2 + (P1(2)/P1(3))**2)
      dS2 = dsqrt(1.0D0 + (P2(1)/P2(3))**2 + (P2(2)/P2(3))**2)

      !=============================================================================!
      ! Sum Energy loss due to veto sections:

       ! First for veto sections that are in the X-Y plane:
       ! (Ignoring cases where particles enter/exit the sides)
       ! ( To include this one can check X-Y at the two faces
       !   of the block to see if the particle is always inside)
       do ii=1,nVetoSec
        ! Calculate center position of veto block:
        Zcent =(VetoLim(2,3,ii)+VetoLim(1,3,ii))/2.0D0
        ! Calculate the thickness of the veto block:
        MatLenZ = VetoLim(2,3,ii)-VetoLim(1,3,ii)
        ! Find X and Y-Positions at center of veto block:
        X1 = XYext1(1) + (P1(1)/P1(3))*(Zcent - zcu)
        Y1 = XYext1(2) + (P1(2)/P1(3))*(Zcent - zcu)
        X2 = XYext2(1) + (P2(1)/P2(3))*(Zcent - zcu)
        Y2 = XYext2(2) + (P2(2)/P2(3))*(Zcent - zcu)
        ! Add energy from the section:
        if(X1.ge.VetoLim(1,1,ii).and.X1.le.VetoLim(2,1,ii) .and.
     |    Y1.ge.VetoLim(1,2,ii).and.Y1.le.Vetolim(2,2,ii) )
     |    DelE1=DelE1 + CoVet*dS1*(MatLenZ)
        if(X2.ge.VetoLim(1,1,ii).and.X2.le.VetoLim(2,1,ii) .and.
     |    Y2.ge.VetoLim(1,2,ii).and.Y2.le.Vetolim(2,2,ii) )
     |    DelE2=DelE2 + CoVet*dS2*(MatLenZ)
       enddo

       !------------------------------------
       ! Now for the bank at a -10deg angle:
       ! (Bank6 counters 3-8 and Bank7)
       call AngVeto(P1,XYext1,DelE1, zcu,Vcent,Vsiz,Vang,CoVet)
       call AngVeto(P2,XYext2,DelE2, zcu,Vcent,Vsiz,Vang,CoVet)


      !=============================================================================!
      ! Calculate Energy loss due to FCAL:

      if(Vertex(3).gt.zFCAL) goto 99 !--> Dont change energy if FCAL is not penetrated

      ! Get the total number of full blocks traversed:
      Nblocks = int((zFCAL-Vertex(3))/Fb)
      if(Nblocks.gt.23) Nblocks=23 !--> Only 23 blocks in FCAL
      
      ! Get the length of the last block traversed:
      Zlast = dmod(zFCAL-Vertex(3),Fb)

      !-----------------------------------------
      ! Run over the first three (most downstream) full blocks with no scintillator:
      if(Nblocks.gt.3) nblk=3
      if(Nblocks.le.3) nblk=Nblocks
      DelE1=DelE1 + CoT1*dS1*(nblk*Fb)
      DelE2=DelE2 + CoT1*dS2*(nblk*Fb)
      ! Run over full blocks containing scintillator:
      if(Nblocks.gt.3) then
      nblk=Nblocks-3
      DelE1=DelE1 + CoT2*dS1*(nblk*Fb)
      DelE2=DelE2 + CoT2*dS2*(nblk*Fb)
      endif

      !-----------------------------------------
      ! Do final, partial block:

      ! For first type with no scintillator:
      if(Nblocks.lt.3) then

        if(Zlast.gt.Fw) then !--> Ignore air gap
           DelE1=DelE1 + CoIrn*dS1*(Zlast-Fw)
           DelE2=DelE2 + CoIrn*dS2*(Zlast-Fw)
        endif

      !-----------------------------------------
      ! For second type with scintillator:
      else

        if(Zlast.gt.(Fw-Fs-Fn)) then !--> Ignore first air gap

          ! Partly through Scintillator:
          if(Zlast.le.(Fw-Fn)) then
            DelE1=DelE1 + CoSci*dS1*(Zlast-(Fw-Fn-Fs))
            DelE2=DelE2 + CoSci*dS2*(Zlast-(Fw-Fn-Fs))

          ! Through entire Scintillator:
          else
            DelE1=DelE1 + CoSci*dS1*Fs
            DelE2=DelE2 + CoSci*dS2*Fs

            if(Zlast.gt.Fw) then !--> Ignore second air gap
            DelE1=DelE1 + CoIrn*dS1*(Zlast-Fw)
            DelE2=DelE2 + CoIrn*dS2*(Zlast-Fw)
            endif

          endif
        endif
      endif
      !-----------------------------------------
      !============================================================!

99    continue !--> Skip FCAL

      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      ! Add the lost energy to the particle:
      P1(0)=P1(0)+DelE1
      tmpP=P1(4)
      P1(4)=dsqrt(P1(0)**2-Mmu**2)
      do ii=1,3
       P1(ii)=P1(ii)*(P1(4)/tmpP)
      enddo
      P2(0)=P2(0)+DelE2
      tmpP=P2(4)
      P2(4)=dsqrt(P2(0)**2-Mmu**2)
      do ii=1,3
       P2(ii)=P2(ii)*(P2(4)/tmpP)
      enddo
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine AngVeto(Pmu,XYext,DelE, zcu,Vcent,Vsiz,Vang,CoVet)
      implicit none

      ! Calculates the energy change due to the angled veto block.

      ! Input/Output:
      double precision Pmu(0:4) !--> Muon Momentum 
      double precision XYext(2) !--> X-Y position of muon at coil
      double precision DelE !--> Energy loss 
      double precision zcu !--> Most upstream Z position of the Coil (in [m])
      double precision Vcent(3) !--> Center position of veto in [m]
      double precision Vsiz(3) !--> Length/width/thickness in [m]
      double precision Vang !--> Angle relative to X-axis in [rad]:
      double precision CoVet !--> Veto Scintillator (1.03 g/cm^3)


      ! Program Variables:
      double precision Ins(3) !--> Point of intersection
      double precision Lang !--> Length traversed through angled Veto


       ! Find the point of intersection of the block's central plane and
       ! the particle's momentum.
      Ins(2)=XYext(2)+
     |          (1.0D0/(1.0D0+(Pmu(3)/Pmu(2))*(1.0D0/dtan(Vang))))*
     |      (Vcent(2)-XYext(2)+(1.0D0/dtan(Vang))*(Vcent(3)-zcu))
      Ins(3)=(1.0D0/(Pmu(2)/Pmu(3)+1.0D0/dtan(Vang)))*
     |      (Vcent(2)-XYext(2)+Vcent(3)/dtan(Vang)+(Pmu(2)/Pmu(3))*zcu)
      Ins(1)=XYext(1) + (Pmu(1)/Pmu(3))*(Ins(3)-zcu)
      ! Ask if the particle is within the block:
      if(Ins(1).ge.Vcent(1)-Vsiz(1)/2.0D0 .and.
     |   Ins(1).le.Vcent(1)+Vsiz(1)/2.0D0 .and.
     |   Ins(2).ge.Vcent(2)-(Vsiz(2)/2.0D0)*dcos(Vang) .and.
     |   Ins(2).le.Vcent(2)+(Vsiz(2)/2.0D0)*dcos(Vang) .and.
     |   Ins(3).ge.Vcent(3)-(Vsiz(2)/2.0D0)*dsin(Vang) .and.
     |   Ins(3).le.Vcent(3)+(Vsiz(2)/2.0D0)*dsin(Vang) ) then
       ! Calculate length traversed:
       Lang=(Pmu(4)/dsqrt(Pmu(2)**2+Pmu(3)**2))*
     |     (Vsiz(3)/dsin(dacos( 
     |   (Pmu(2)/Pmu(4))*dcos(Vang)-(Pmu(3)/Pmu(3))*dsin(Vang) )))
       ! Calculate Energy Change
       DelE = DelE + CoVet*Lang
      endif

      return
      end
