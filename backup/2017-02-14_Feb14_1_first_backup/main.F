      program main
      implicit none     

#include "ntuple.inc"

      !---------------------------------------------------------------------!
      ! This program runs over NOMAD DC Data/MC ntuples in
      ! hbook format, and performs the desired analysis.
      !
      ! To run this code one simply does the following:
      !  >> sh make.sh
      !  >> main
      !
      !
      ! To make a backup of the current code folder:
      !  >> sh backup.sh <folder-tag>
      ! A backup directory will be created in the ./backup/ subdirectory
      ! with a name that includes the current date, and the given folder
      ! tag.
      !
      ! All program outputs are contained within the ./outputs/ folder.
      ! Cut tables (and programs to modify the tables) are found in:
      !   ./outputs/cuts/
      ! Output histrogram files and .kumacs are found in:
      !   ./outputs/hbook/
      ! In the ./outputs/ folder one can also find the settings.txt file
      ! and timestamps for the start and end of the program's execution.
      !---------------------------------------------------------------------!



      !---------------------------------------------------------------------!
      ! The main structure of the program:
      !
      !   main.F  --> Contains user settings
      !               Initializes variables
      !               Books histograms (bookhistos.F)
      !               Opens ntuples and output histogram files
      !               Performs actions on single events (process_event.F)
      !               Prints cut tables (cut_table.F)
      !
      !---------------------------------------------------------------------!
      ! The subroutines that will generally require editing for analysis
      ! modifications:
      !
      !   main.F --> * User settings located within the variable
      !                declarations (under "USER SET").
      !              * User settings located at the beginning of
      !                the executable code ("BEGIN USER INPUT" block)
      !
      !  check_print_settings.F --> This subroutine simply makes checks
      !                             on the user settings and prints the
      !                             current settings to file.  This must
      !                             be edited if any new user settings
      !                             are added to main.F
      !
      !  bookhistos.F --> * Settings for individual histograms
      !                   * Labels for extra cut types (histograms
      !                     with extra cuts applied)
      !
      !  process_event.F --> * This subroutine performs all actions
      !                        meant for a single event. This includes:
      !                        -> Event weighting
      !                        -> Kinematic variable calculations
      !                        -> Applying cuts
      !                        -> Filling histograms and cut array
      !
      !  muon_calculations.F --> A subroutine found in process_event.F
      !                          that contains calculations for muon
      !                          track kinematic variables.  A seperate
      !                          subroutine is used because these
      !                          calculations are made using both DC
      !                          ntuple values, and extrapolated values 
      !                          that include the FCAL information.
      !
      !  cut_table.F --> This subroutine must be edited when cut
      !                  definitions are changed.  The labels for each
      !                  cut can be found here.
      !
      !---------------------------------------------------------------------!
      ! Information on other subroutines included in the analysis:
      !
      ! ./code_utilities/ --> Contains utility subroutines for common
      !                       tasks.  Files here should generally not
      !                       need editing.
      !
      !---------------------------------------------------------------------!

      ! For PAW functionality
      integer nhbook
      parameter (nhbook=3e8)
      real paw
      common /pawc /paw(nhbook)
      integer istat,icycle,id,err 
    
      ! Do Loop Variables:
      integer ii,jj !--> General
      integer ievent !--> Loop over events
      integer ifile !--> Loop over ntuples
      integer nttyp !--> Loop over ntuple types

      ! Program Variables
      integer ntnum !--> Number of ntuples in current datacard
      integer nevents !--> Number of events in an ntuple
      integer UnitNum !--> FORTRAN unit number for datacards
      integer MaxNTS !--> Maximum possible number of ntuples
      parameter (MaxNTS=900)
      character*100 ntname(MaxNTS) !--> Ntuple locations
      integer length !--> Contains string lengths
      integer ldstr !--> Length of "dstr()" main datacard label
      integer sumintarr1d !--> Function to sum a 1d integer array
      integer extyp !--> Number of cuts used in the "extra cuts" section

      !-------------------------------------!
      !-----------    USER SET   -----------!
      ! Number of variables plotted:
      integer nvar 
      parameter (nvar = 47)
      ! Number of cuts made:
      integer ncuts 
      parameter (ncuts = 29)
      ! Number of MC types (non-data):
      integer nmc 
      parameter (nmc = 15)
      ! Number of MC generator types:
      integer ntyp
      parameter (ntyp=7)
      !-----
      ! Number of event class types:
      integer nevtyp
      parameter (nevtyp=4)
      ! Number of detector sections:
      integer ndetsec
      parameter (ndetsec=4)
      ! Number of ncand types:
      integer nncand
      parameter (nncand=4)
      ! Number of extra cuts:
      integer nextyp
      parameter (nextyp=4)
      !-----
      ! File Unit Numbers:
      ! (Fortran allows unit number range 
      !  1-99. Units 5 & 6 are reserved 
      !  for standard input/output.)
      integer hfileID !--> For histograms
      integer ntID !--> For ntuples
      integer evtID(2) !--> Event printout
      !-------------------------------------!
      !-------------------------------------!

      ! For User Customization
      ! (Explanation Below)
      integer maxevts
      integer usents
      integer fullnts
      integer runtype(0:nmc)
      integer cuthists(ncuts)
      integer nbintyp(nvar) 
      integer nttypobg 
      integer gtyp(0:nmc)
      double precision zetaS 
      character*20 datacard(0:nmc,0:1) !--> 2nd index gives full (1) or baby (0) ntuple datacards
      character*11 dname(0:nmc,ntyp) !--> 2nd index gives mc generator type (gtyp)
      character*11 bname(0:nmc,ntyp) !--> 2nd index gives mc generator type (gtyp)
      character*20 filetag(0:nmc)
      character*31 mctyp(0:nmc)
      character*10 ttag(0:nmc)
      character*11 dstr(0:nmc)
      character*3 dtyp(ntyp)
      integer drep(0:nmc)
      integer ndetsw
      integer detsw(ndetsec)
      integer detidx(ndetsec)
      integer nncndsw
      integer ncndsw(nncand)
      integer ncndidx(nncand)
      integer nevtsw
      integer evtsw(nevtyp)
      integer evtidx(nevtyp)
      integer ncuthists
      integer extrapsw
      integer evtpicsw
      

      ! Cut Counts & Normalizations
      double precision gennt(nmc,ntyp) ! Number of generated events in each ntuple
                                    ! Second index gives MC generator type (gtyp)
      double precision norm(0:1,0:nmc) ! In the 1st index: 0=zeroth, 1=final
      double precision cuts(3,ncuts,0:nmc,nevtyp,nextyp,nncand,ndetsec) ! In the 1st index:  
                                               !  1 = raw  (wt=1.0)
                                               !  2 = mc-z weighting 
                                               !  3 = Any extra corrections/weighting 
                                               ! 2nd index is for cut number
                                               ! 3rd index is for ntuple type
                                               ! In the 4th index:
                                               !  0 = All NuMuCC
                                               !  1 = OS DiMu
                                               !  2 = LS DiMu
                                               !  3 = OS  Mu + <other>

      !-------------------------------!
      ! Correction Functions:

      ! Index of the corrected MC:
      integer iCorr

      ! 2D Correction:
      integer usecorr2D
      integer nbin2Dmass
      integer nbin2Dzeta
      parameter (nbin2Dmass=10)
      parameter (nbin2Dzeta=10)
      double precision DScorr2D(nbin2Dmass,nbin2Dzeta)
      double precision binlim2Dmass(nbin2Dmass+1)
      double precision binlim2Dzeta(nbin2Dzeta+1)

      ! Parent mass correction:
      integer usecorrMass
      integer nbinMass
      parameter (nbinMass=20)
      double precision DScorrMass(nbinMass)
      double precision binlimMass(nbinMass+1)

      ! Parent zeta correction:
      integer usecorrZeta
      integer nbinZeta
      parameter (nbinZeta=20)
      double precision DScorrZeta(nbinZeta)
      double precision binlimZeta(nbinZeta+1)


      ! For renormalization factor:
      ! (so that correction functions change only the shape
      !  of the MC, and not the number of events)
      integer formRenorm !--> Form renormalization factor?
      double precision renorm !--> CCDIS renormalization factor
      double precision reCounts(2) ! 1=Raw number of CCDIS
                                   ! 2=Corrected number of CCDIS

      ! Misc.:
      character*80 filename
      !-------------------------------!




      !---------------------------------------------------------------------------!
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      !0o0o0o0o0o0o0o0o0o0o0---     BEGIN USER INPUT      ---0o0o0o0o0o0o0o0o0o0o0!
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      !                                                                           !
      !       *------=============================================------*         !
      !           *----=====   Set Above (or elsewhere):   =====----*             !
      !               *--=====================================--*                 !
      ! nvar = Number of Kinematic variables  {nvar<100 for histogram ID scheme}  !
      !                                                                           !
      ! ncuts = Number of cuts exacted                                            !
      !                                                                           !
      ! nmc = Number of MC components  <Data are 0th and 1st components>          !
      !       With 0th Data included the total number of components is (nmc+1)    !
      !                                                                           !
      ! ntyp = Number of possible MC generator types, or possible combinations    !
      !        of the basic generators                                            !
      !                                                                           !
      ! nextyp = Number of extra cut types to plot.  Extra cut types give plots    !
      !         which have additional kinematic cuts applied.                     !
      !         {should be no larger than 99 for current plotting scheme}         !
      !                                                                           !
      ! nbintyp(nvar) = Number of bin variations for each variable.               !
      !                 (Set in bookhistos.F.  Maximum Value=nbinvarMAX)          !
      !                                                                           !
      !               *--=====================================--*                 !
      !           *----=========================================----*             !
      !       *------=============================================------*         !
      !                                                                           !
      ! Histogram numbering scheme:                                               !
      !      Histograms are numbered internally using sequential                  !
      !      numbers.  At the user level histograms are numbered with a           !
      !      set of parameters, shown below.  The parameters are mapped           !
      !      to the sequential set using the getid.F subroutines.                 !
      !                      *****************************                        !
      !                      ***  ID = cciieeddnnbbvv  ***                        !
      !                      *****************************                        !
      !   cc = Event Type (0=All CC, 1=Opposite-sign DiMu, 2=Like-sign DiMu)      !
      !   ii = Cut number                                                         !
      !   ee = Extra Cut type (00=none)                                           !
      !   dd = Detector section                                                   !
      !   nn = Ncand type                                                         !
      !   bb = Bin Variation                                                      !
      !   vv = Variable Number                                                    !
      !                                                                           !
       zetaS = 0.075D0 !--> Gives the zeta value that differentiates              !
                       !    the signal and background regions.                    !
      !                                                                           !
       ! Maximum number of events to use per ntuple:                              !
       maxevts = 0 !--> 0 = Use all events                                        !
      !                                                                           !
       ! Number of ntuples to use for each type:                                  !
       usents = 0 !--> 0 = Use all NTS                                            !
      !           !--> 1 = Use only 1 Ntuple                                      !
      !           !--> 2 = Use only 2 Ntuples (or the maximum available)          !
      !           !--> 3 = Use only 3 Ntuples...                                  !
      !           !--> ...                                                        !
      !                                                                           !
       fullnts = 0  !--> 0 = Use Baby NTs                                         !
      !             !    1 = Use Full NTs                                         !
      !                                                                           !
       nttypobg = 4 !--> Index of the OBG (outside background) contribution       !
      !                                                                           !
      !                                                                           !
       evtpicsw = 0 !--> Print event picture output? (1 or 0)                     !
      !                                                                           !
      !                                                                           !
       extrapsw = 1 !--> Use my track extrapolation for 2 or more muons? (1 or 0) !
      !                                                                           !
      !                                                                           !
      !===========================================!                               !
      !===========================================!                               !
      ! DS Correction Function Settings           !                               !
      !                                           !                               !
      ! Index for the corrected MC:               !                               !
       iCorr = 1 !(CCDIS)                         !                               !
      !                                           !                               !
      ! Form CCDIS Renormalization factor?        !                               !
       formRenorm = 0 !                           !                               !
      !                                           !                               !
      ! Use 2D Correction?                        !                               ! 
       usecorr2D = 0 !                            !                               !
      ! Use Mass Correction?                      !                               ! 
       usecorrMass = 0 !                          !                               !
      ! Use Zeta Correction?                      !                               ! 
       usecorrZeta = 0 !                          !                               !
      !                                           !                               !
      !===========================================!                               !
      !===========================================!                               !
      !                                                                           !
      !                                                                           !
      !-------------------------------------------!                               !
      !-------------------------------------------!                               !
      ! Plotting switches:                        !                               !
      !                                           !                               !
      ! Set detector sections on/off              !                               !
       detsw(1) = 1 ! DC                          !                               !
       detsw(2) = 0 ! Other                       !                               !
       detsw(3) = 1 ! Coil                        !                               !
       detsw(4) = 0 ! Upstream                    !                               !
      !-------------------------------------------!                               !
      ! Set ncand types on/off                    !                               !
       ncndsw(1) = 1 ! ncand = 2                  !                               ! 
       ncndsw(2) = 0 ! ncand = 3                  !                               ! 
       ncndsw(3) = 0 ! ncand = 4                  !                               ! 
       ncndsw(4) = 1 ! ncand = {3,4}              !                               ! 
      !-------------------------------------------!                               !
      ! Set event topologies on/off               !                               !
       evtsw(1) = 0 ! CC-DIS                      !                               ! 
       evtsw(2) = 1 ! OS DiMu                     !                               ! 
       evtsw(3) = 0 ! LS DiMu                     !                               ! 
       evtsw(4) = 1 ! OS Mu+X                     !                               ! 
      !-------------------------------------------!                               !
      ! Initialize Post-cut histogram switches:   !                               !
       do ii=1,ncuts-1                            !                               !
        cuthists(ii) = 0                          !                               !
       enddo                                      !                               !
       cuthists(ncuts) = 1  ! Plot after last cut !                               !
      ! Turn on/off other Post-cut histograms:    !                               !
       cuthists(1) = 0                            !                               !
      !-------------------------------------------!                               !
      ! Calculate the number of "on" switches:    !                               ! 
       ndetsw=sumintarr1d(ndetsec,detsw)          !                               !
       nncndsw=sumintarr1d(nncand,ncndsw)         !                               !
       nevtsw=sumintarr1d(nevtyp,evtsw)           !                               !
       ncuthists=sumintarr1d(ncuts,cuthists)      !                               !
      !-------------------------------------------!                               !
      !                                                                           !
      !                                                                           !
       ! To run or not to run:                                                    !
       runtype(0) = 1  !--> Data                                                  !
       runtype(1) = 1  !--> CC-DIS                                                !
       runtype(2) = 1  !--> NC-DIS                                                !
       runtype(3) = 1  !--> J/Psi                                                 !
       runtype(4) = 1  !--> OBG                                                   !
       runtype(5) = 1  !--> CohPi+                                                !
       runtype(6) = 1  !--> CohRho+                                               !
       runtype(7) = 1  !--> aNumu-CC                                              !
       runtype(8) = 1  !--> QE       (<1 event)                                   !
       runtype(9) = 1  !--> CohPi0   (0 events)                                   !
       runtype(10)= 1  !-->  Nue-CC  (<1 event)                                   !
       runtype(11)= 1  !--> aNue-CC  (0 events)                                   !
       runtype(12)= 1  !--> aNumu-NC (<1 event)                                   !
       runtype(13)= 1  !--> CohRho0  (<10 events)                                 !
       runtype(14)= 1  !--> Res-CC   (<1 event)                                   !
       runtype(15)= 1  !--> CohPhi0  (<10 events)                                 !
      !                                                                           !
      !                                                                           !
       !====================================================!                     ! 
       ! Set MC generator types:                            !                     !
       !         1 = NUAGE                                  !                     !
       !         2 = NEGLIB                                 !                     !
       !         3 = GENIE                                  !                     !
       !         4 = NUAGE  + NEGLIB                        !                     !
       !         5 = NUAGE  + GENIE                         !                     !
       !         6 = NEGLIB + GENIE                         !                     !
       !         7 = NUAGE  + NEGLIB  + GENIE               !                     !
        gtyp(0) = 3  !--> Data (all choices identical)      !                     !
        gtyp(1) = 3  !--> CC-DIS                            !                     !
        gtyp(2) = 3  !--> NC-DIS                            !                     !
        gtyp(3) = 3  !--> J/Psi                             !                     !
        gtyp(4) = 3  !--> OBG                               !                     !
        gtyp(5) = 3  !--> CohPi+                            !                     !
        gtyp(6) = 3  !--> CohRho+                           !                     !
        gtyp(7) = 3  !--> aNumu-CC                          !                     !
        gtyp(8) = 3  !--> QE                                !                     !
        gtyp(9) = 3  !--> CohPi0                            !                     !
        gtyp(10)= 3  !-->  Nue-CC                           !                     !
        gtyp(11)= 3  !--> aNue-CC                           !                     !
        gtyp(12)= 3  !--> aNumu-NC                          !                     !
        gtyp(13)= 3  !--> CohRho0                           !                     !
        gtyp(14)= 3  !--> Res-CC                            !                     !
        gtyp(15)= 3  !--> CohPhi0                           !                     !
       !====================================================!                     ! 
      !                                                                           !
      !                                                                           !
      !   !===============================!                                       !
      !   !  Table Type Tags              !                                       !
            ttag(0) = ' Data     '        !                                       !
            ttag(1) = ' CCDIS    '        !                                       !
            ttag(2) = ' NCDIS    '        !                                       !
            ttag(3) = ' JPsi     '        !                                       !
            ttag(4) = ' OBG      '        !                                       !
            ttag(5) = ' CohPi+   '        !                                       !
            ttag(6) = ' CohRho+  '        !                                       !
            ttag(7) = ' aNuMu CC '        !                                       !
            ttag(8) = ' QE       '        !                                       !
            ttag(9) = ' CohPi0   '        !                                       !
            ttag(10)= '  Nue CC  '        !                                       !
            ttag(11)= ' aNue CC  '        !                                       !
            ttag(12)= ' aNuMu NC '        !                                       !
            ttag(13)= ' CohRho0  '        !                                       !
            ttag(14)= ' Res      '        !                                       !
            ttag(15)= ' CohPhi0  '        !                                       !
      !   !===============================!                                       !
      !                                                                           !
      !                                                                           !
       !******************************************************************!       !
       !*****************-----  Generated NT Events  -----****************!       !
       !*****************---------------------------------****************!       !
       !------------------------------------------------------------------!       !
       ! CCDIS:                                                           !       !
        gennt(1,1) = 1.67094362D6 !--> (NUAGE)                            !       !
        gennt(1,2) = 4.4777175D6 !---> (NEGLIB)                           !       !
        gennt(1,3) = 4.116629D6 !----> (GENIE)                            !       !
       !------------------------------------------------------------------!       !
       ! NCDIS:                                                           !       !
        gennt(2,1) = 1.34534462D6 !--> (NUAGE)                            !       !
        gennt(2,2) = 3.2419925D6 !---> (NEGLIB)                           !       !
        gennt(2,3) = 2.45185225D6 !--> (GENIE)                            !       !
       !------------------------------------------------------------------!       !
       ! CohJ/Psi (My NEGLIB/Nuage-reader):                               !       !
        gennt(3,1) = 337363.344D0 !                                       !       !
        gennt(3,2) = 337363.344D0 !                                       !       !
        gennt(3,3) = 337363.344D0 !                                       !       !
       !------------------------------------------------------------------!       !
       ! OBG (from data):                                                 !       !
        gennt(4,1) = 1.0D0 !                                              !       !
        gennt(4,2) = 1.0D0 !                                              !       !
        gennt(4,3) = 1.0D0 !                                              !       !
       !------------------------------------------------------------------!       !
       ! CohPi+:                                                          !       !
         ! -- RS Model: --                                                !       !
         gennt(5,1) = 362605.312D0 !----> (NUAGE)                         !       !
         gennt(5,2) = 412962.812D0 !----> (NEGLIB)                        !       !
         ! -- BK Model: --                                                !       !
cc         gennt(5,1) = 406748.031D0 !--> (NUAGE)                         !       !
         gennt(5,3) = 824186.438D0 !(---> GENIE) (BK?)                    !       !
         ! -- BS Model: --                                                !       !
cc         gennt(5,1) = 409103.781D0 !--> (NUAGE)                         !       !
       !------------------------------------------------------------------!       !
       ! CohRho+ (My NEGLIB/Nuage-reader):                                !       !
        gennt(6,1) = 147680.422D0 !                                       !       ! 
        gennt(6,2) = 147680.422D0 !                                       !       ! 
        gennt(6,3) = 147680.422D0 !                                       !       ! 
       !------------------------------------------------------------------!       !
       ! aNumu-CC:                                                        !       !
        gennt(7,1) = 0.234835203D6 !--> (NUAGE)                           !       !
        gennt(7,2) = 0.3733310D6 !----> (NEGLIB)                          !       !
        gennt(7,3) = 0.384881812D6 !--> (GENIE)                           !       !
       !------------------------------------------------------------------!       !
       ! QE-CC:                                                           !       !
        gennt(8,1) = 0.204318438D6 !--> (NUAGE)                           !       !
        gennt(8,2) = 9.8879750D6 !-----> (NEGLIB)                         !       !
        gennt(8,3) = 0.418257531D6 !---> (GENIE)                          !       !
       !------------------------------------------------------------------!       !
       ! CohPi0 (no GENIE!):                                              !       !
         ! -- RS Model: --                                                !       !
         gennt(9,1) = 529168.75D0  !----> (NUAGE)                         !       !
         gennt(9,2) = 38022.6055D0  !---> (NEGLIB)                        !       !
         ! -- BK Model: --                                                !       !
cc         gennt(9,1) = 179844.781D0 !--> (NUAGE)                         !       !
         ! -- BS Model: --                                                !       !
cc         gennt(9,1) = 23723.3262D0 !--> (NUAGE)                         !       !
        ! Use NUAGE BK for GENIE default:                                 !       !
        gennt(9,3) = 179844.781D0  !----> (NUAGE)                         !       !
       !------------------------------------------------------------------!       !
       ! Nue-CC:                                                          !       !
        gennt(10,1)= 0.173752453D6  !--> (NUAGE)                          !       !
        gennt(10,2)= 0.425471188D6  !--> (NEGLIB)                         !       !
        gennt(10,3)= 0.396271906D6  !--> (GENIE)                          !       !
       !------------------------------------------------------------------!       !
       ! aNue-CC:                                                         !       !
        gennt(11,1)= 0.104198117D6  !--> (NUAGE)                          !       !
        gennt(11,2)= 0.183138078D6  !--> (NEGLIB)                         !       !
        gennt(11,3)= 0.193616453D6  !--> (GENIE)                          !       !
       !------------------------------------------------------------------!       !
       ! aNumu-NC:                                                        !       !
        gennt(12,1)= 0.091272578D6 !--> (NUAGE)                           !       !
        gennt(12,2)= 0.184929312D6 !--> (NEGLIB)                          !       !
        gennt(12,3)= 0.189598922D6 !--> (GENIE)                           !       !
       !------------------------------------------------------------------!       !
       ! CohRho0 (My NEGLIB/Nuage-reader)                                 !       !
        gennt(13,1)= 25235.83D0 !                                         !       !
        gennt(13,2)= 25235.83D0 !                                         !       !
        gennt(13,3)= 25235.83D0 !                                         !       !
       !------------------------------------------------------------------!       !
       ! Res-CC:                                                          !       !
        gennt(14,1)= 0.0863934D6 !----> (NUAGE)                           !       !
        gennt(14,2)= 5.600115D6  !----> (NEGLIB)                          !       !
        gennt(14,3)= 0.934139812D6 !--> (GENIE)                           !       !
       !------------------------------------------------------------------!       !
       ! CohPhi0 (My NEGLIB/Nuage-reader)                                 !       !
        gennt(15,1)= 4075.93D0 !                                          !       !
        gennt(15,2)= 4075.93D0 !                                          !       !
        gennt(15,3)= 4075.93D0 !                                          !       !
       !------------------------------------------------------------------!       !
       ! Now add events for combined types.  Info for 2nd index:          !       !
       !         1 = NUAGE                                                !       !
       !         2 = NEGLIB                                               !       !
       !         3 = GENIE                                                !       !
       !         4 = NUAGE  + NEGLIB                                      !       !
       !         5 = NUAGE  + GENIE                                       !       !
       !         6 = NEGLIB + GENIE                                       !       !
       !         7 = NUAGE  + NEGLIB  + GENIE                             !       !
       do ii=1,nmc !                                                      !       !
        gennt(ii,4) = gennt(ii,1) + gennt(ii,2) ! Nuage+Neglib            !       !
        gennt(ii,5) = gennt(ii,1) + gennt(ii,3) ! Nuage+Genie             !       !
        gennt(ii,6) = gennt(ii,2) + gennt(ii,3) ! Neglib+Genie            !       !
        gennt(ii,7) = gennt(ii,1) + gennt(ii,2) + gennt(ii,3) ! All 3     !       !
       enddo !                                                            !       !
       !------------------------------------------------------------------!       !
       !*****************---------------------------------****************!       !
       !*****************---------------------------------****************!       !
       !******************************************************************!       !
      !                                                                           !
      !                                                                           !
       !******************************************************************!       !
       !*****************-----  Zeroth Level Norms  -----*****************!       !
       !*****************--------------------------------*****************!       !
        norm(0,0) = 1.0D0 !--------------------> Data                     !       !
        norm(0,1) = 1.44D6 !-------------------> CCDIS                    !       !
        norm(0,2) = (0.38D0*1.44D6) !----------> NC                       !       !
        norm(0,3) = (500.0D0*0.0593D0) !-------> CohJ/Psi-mumu            !       !
        norm(0,4) = 1.0D0 !--------------------> OBG (From Data)          !       !
        norm(0,5) = 10000.0D0 !----------------> CohPi+                   !       !
        norm(0,6) = (1000.0D0/0.1355D0) ! CohRho+ (sin2=0.2397-->0.1355)  !       ! 
        norm(0,7) = (1.44D6*0.025D0) !---------> aNumu-CC                 !       !
        norm(0,8) = 32000.0D0 !----------------> QE-CC                    !       !
        norm(0,9) = 5000.0D0  !----------------> CohPi0                   !       !
        norm(0,10)= (1.44D6*0.015D0) !----------> Nue-CC                  !       !
        norm(0,11)= (1.44D6*0.0015D0) !---------> aNue-CC                 !       !
        norm(0,12)= (0.38D0*1.44D6*0.025D0) !--> aNumu-NC                 !       !
        norm(0,13)= 1000.0D0 !----------------> CohRho0                   !       !
        norm(0,14)= (1.44D6*0.035D0) !--------> Res-CC                    !       !
        norm(0,15)= 200.0D0 !-----------------> CohPhi0                   !       !
        ! Print normalization settings to .tex file before scaling        !       ! 
        call print_zeroth_norms(nmc,nttypobg,ttag,gtyp,norm,ntyp,gennt) ! !       !
        ! Set zeroth normalization based on generated events in NT:       !       !
        do ii=1,nmc !                                                     !       !
         norm(0,ii) = norm(0,ii)/gennt(ii,gtyp(ii)) !                     !       !
        enddo  !                                                          !       !
       !*****************--------------------------------*****************!       !
       !******************************************************************!       !
      !                                                                           !
      !                                                                           !
       !*******************************************************!                  !
       !****************----  Final Norms  ----****************!                  !
       !****************-----------------------****************!                  !
        norm(1,0) = 1.0D0   ! Data                                                !
        norm(1,1) = 1.0D0   ! CC                                                  !
        norm(1,2) = 1.0D0   ! NC                                                  !
        norm(1,3) = 1.0D0   ! CohJ/Psi                                            ! 
        norm(1,4) = 0.22D0  ! From CohRho analysis ! OBG (non Ph2Mu)              !
        norm(1,5) = 0.985D0 ! CohPi+ (from 2v0 pi0 analysis)                      !
        norm(1,6) = 0.669D0 ! CohRho+ from CohRho0 measurement                    !
        norm(1,7) = 1.0D0   ! Anumu-CC                                            !
        norm(1,8) = 1.0D0   ! QE                                                  !
        norm(1,9) = 0.985D0 ! CohPi0 (from 2v0 analysis)                          !
        norm(1,10)= 1.0D0   ! Nue-CC                                              !
        norm(1,11)= 1.0D0   ! Anue-CC                                             !
        norm(1,12)= 1.0D0   ! Anumu-NC                                            !
        norm(1,13)= 0.669D0 ! CohRho0 (from measurement)                          !
        norm(1,14)= 1.0D0   ! Res-CC                                              !
        norm(1,15)= 1.0D0   ! CohPhi0                                             !
        ! Apply normalizations:                                                   !
        do ii=1,nmc  !                                                            !
         norm(1,ii)=norm(1,ii)*norm(0,ii) !                                       !
        enddo !                                                                   !
       !****************-----------------------****************!                  !
       !*******************************************************!                  !
      !                                                                           !
      !                                                                           !
      !                                                                           !
      !   |--------------------------------------------------|                    !
      !   |--------------------------------------------------|                    !
      !   | Set Datacard Locations (Full NTs):               |                    !
          !                                                  |                    !
          !  This section sets datacard filenames. If "drep" |                    !
          !  is 1, then the value of "dstr" is repeated      |                    !
          !  exaclty for all file variations, and the value  |                    !
          !  of "gtyp" will have no effect on the ntuples    |                    !
          !  used.  For "drep" of 0 the main name will have  |                    !
          !  additional tags referring to variations in MC   |                    !
          !  generator(s) used.                              |                    !
          !                                                  |                    !
          ! Set standardised datacard names:                 |                    !
          ! drep = 1  --> Repeats the same "dstr" name for   !                    !
          !               all MC generator types             !                    !
          !               (no variations)                    !                    !
          ! drep = 0  --> Puts MC generator type tag at end  !                    !
          !               of each "dstr" name                !                    !
          dstr(0) ='datafull   '  !                          |                    !
          drep(0) = 1             !                          !                    !
          dstr(1) ='ccfull     '  !                          |                    !
          drep(1) = 0             !                          !                    !
          dstr(2) ='ncfull     '  !                          |                    !
          drep(2) = 0             !                          !                    !
          dstr(3) ='jpsi-mumu  '  !                          |                    !
          drep(3) = 1             !                          !                    !
          dstr(4) ='obgfull    '  !                          |                    !
          drep(4) = 1             !                          !                    !
          dstr(5) ='cohpip     '  !  (Special, set below)    |                    !
          drep(5) = 1             !                          !                    !
          dstr(6) ='cohrhop    '  !                          |                    !
          drep(6) = 1             !                          !                    !
          dstr(7) ='anmcc      '  !                          |                    !
          drep(7) = 0             !                          !                    !
          dstr(8) ='qecc       '  !                          |                    !
          drep(8) = 0             !                          !                    !
          dstr(9) ='cohpi0     '  !  (Special, set below)    |                    !
          drep(9) = 1             !                          !                    !
          dstr(10)='nuecc      '  !                          |                    !
          drep(10)= 0             !                          !                    !
          dstr(11)='anecc      '  !                          |                    !
          drep(11)= 0             !                          !                    !
          dstr(12)='anmnc      '  !                          |                    !
          drep(12)= 0             !                          !                    !
          dstr(13)='cohrho0    '  !                          |                    !
          drep(13)= 1             !                          !                    !
          dstr(14)='rescc      '  !                          |                    !
          drep(14)= 0             !                          !                    !
          dstr(15)='cohphi0    '  !                          |                    !
          drep(15)= 1             !                          !                    !
          !                                                  |                    !
          ! Set MC type strings:                             !                    !
          dtyp(1)='nua' !--> NUAGE                           !                    !
          dtyp(2)='neg' !--> NEGLIB                          !                    !
          dtyp(3)='gen' !--> GENIE                           !                    !
          dtyp(4)='nn' !--> NUAGE + NEGLIB                   !                    !
          dtyp(5)='ng' !--> NUAGE + GENIE                    !                    !
          dtyp(6)='gn' !--> NEGLIB + GENIE                   !                    !
          dtyp(7)='nng' !--> NUAGE + NEGLIB + GENIE          !                    !
          !                                                  !                    !
          ! Make standardized set of names:                  |                    !
          do ii=0,nmc !                                      |                    !
           ldstr=LEN_TRIM(dstr(ii))!                         |                    !
           do jj=1,ntyp!                                     |                    !
            if(drep(ii).eq.0) then!                          |                    !
            dname(ii,jj)=dstr(ii)(1:ldstr)//dtyp(jj)!        |                    !
            elseif(drep(ii).eq.1) then!                      |                    !
            dname(ii,jj)=dstr(ii)(1:ldstr)!                  |                    !
            endif!                                           |                    !
           enddo!                                            |                    !
          enddo!                                             |                    !
          !                                                  |                    !
          ! Modify special cases:                            !                    !
          !..................................................|                    !
          ! CohPi+:                                          |                    !
            ! -- RS Model: --                                |                    !
            dname(5,1) ='cohpipnuars'  !(NUAGE)              |                    !
            dname(5,2) ='cohpipnegrs'  !(NEGLIB)             |                    !
            dname(5,3) ='cohpipgen  '  !(GENIE)              |                    !
            ! -- BK Model: --                                |                    !
cc            dname(5,1) ='cohpipnuabk'  !(NUAGE)            |                    !
            ! -- BS Model: --                                |                    !
cc            dname(5,1) ='cohpipnuabs'  !(NUAGE)            |                    !
            do ii=4,ntyp !                                   !                    !
            dname(5,ii)='NOT ALLOWED' !                      !                    !
            enddo!                                           !                    !
          !..................................................|                    !
          ! CohPi0 (no GENIE!):                              |                    !
            ! -- RS Model: --                                |                    !
            dname(9,1) ='cohpi0nuars'  !(NUAGE)              |                    !
            dname(9,2) ='cohpi0negrs'  !(NEGLIB)             |                    !
            ! -- BK Model: --                                |                    !
cc            dname(9,1) ='cohpi0nuabk'  !(NUAGE)            |                    !
            ! -- BS Model: --                                |                    !
cc            dname(9,1) ='cohpi0nuabs'  !(NUAGE)            |                    !
           ! Use NUAGE BK for GENIE default:                 |                    !
           dname(9,3) ='cohpi0nuabk'  !(NUAGE)               |                    !
            do ii=4,ntyp !                                   !                    !
            dname(9,ii)='NOT ALLOWED' !                      !                    !
            enddo!                                           !                    !
          !..................................................|                    !
          ! Set full NT datacard locations:                  |                    !
           do ii=0,nmc  !                                    |                    !
            datacard(ii,1)='datacard/'//dname(ii,gtyp(ii))   !                    !
           enddo  !                                          |                    !
          !..................................................|                    !
          !..................................................|                    !
      !                                                                           !
      !                                                                           !
      !   |-----------------------------------------------|                       !
      !   |-----------------------------------------------|                       !
      !   | Set Datacard Locations (Baby NTs):            |                       !
      !   |...............................................|                       !
      !   | Initialize locations to full ntuples:         |                       !
           do ii=0,nmc !                                  |                       !
            datacard(ii,0)=datacard(ii,1) !               |                       !
            do jj=1,ntyp !                                |                       !
             bname(ii,jj)=dname(ii,jj) !                  |                       !
            enddo !                                       |                       !
           enddo !                                        |                       !
          !...............................................|                       !
          !...............................................|                       !
          ! Set babynt name tags:                         !                       !
c          dstr(0) ='datababy   '  !                       |                       !
          dstr(0) ='datacoil   '  ! -70 < zvr < 405       |                       !
          dstr(1) ='ccbaby     '  !                       |                       !
          dstr(2) ='ncbaby     '  !                       |                       !
          dstr(4) ='obgbaby    '  !                       |                       !
          dstr(7) ='amccbaby   '  !                       |                       !
          dstr(8) ='qeccbaby   '  !                       |                       !
          dstr(10)='neccbaby   '  !                       |                       !
          dstr(14)='resbaby    '  !                       |                       !
          ! Make standardized set of names:               |                       !
          do ii=0,nmc !                                   |                       !
           ldstr=LEN_TRIM(dstr(ii))!                      |                       !
           do jj=1,ntyp!                                  |                       !
           if(dstr(ii)(1:ldstr) .ne.                      !                       !
     |        bname(ii,jj)(1:ldstr) ) then                !                       !
            if(drep(ii).eq.0) then!                       |                       !
            bname(ii,jj)=dstr(ii)(1:ldstr)//dtyp(jj)!     |                       !
            elseif(drep(ii).eq.1) then!                   |                       !
            bname(ii,jj)=dstr(ii)(1:ldstr)!               |                       !
            endif!                                        |                       !
           endif!                                         |                       !
           enddo!                                         |                       !
          enddo!                                          |                       !
          !...............................................|                       !
          !...............................................|                       !
          ! Set baby NT datacard locations:               |                       !
          do ii=0,nmc !                                   |                       !
           datacard(ii,0)='datacard/'//bname(ii,gtyp(ii))!|                       !
          enddo !                                         |                       !
          !...............................................|                       !
          !...............................................|                       !
      !                                                                           !
      !                                                                           !
      !   !************************************!                                  !
      !   ! Set output histogram filenames:    !                                  !
           filetag(0) = 'cohjpsi-mumu_data   ' !                                  !
           filetag(1) = 'cohjpsi-mumu_ccdis  ' !                                  !
           filetag(2) = 'cohjpsi-mumu_ncdis  ' !                                  !
           filetag(3) = 'cohjpsi-mumu_jpsi   ' !                                  !
           filetag(4) = 'cohjpsi-mumu_obg    ' !                                  !
           filetag(5) = 'cohjpsi-mumu_cohpip ' !                                  !
           filetag(6) = 'cohjpsi-mumu_cohrhop' !                                  !
           filetag(7) = 'cohjpsi-mumu_anumucc' !                                  !
           filetag(8) = 'cohjpsi-mumu_qe     ' !                                  !
           filetag(9) = 'cohjpsi-mumu_cohpi0 ' !                                  !
           filetag(10)= 'cohjpsi-mumu_nuecc  ' !                                  !
           filetag(11)= 'cohjpsi-mumu_anuecc ' !                                  !
           filetag(12)= 'cohjpsi-mumu_anumunc' !                                  !
           filetag(13)= 'cohjpsi-mumu_cohrho0' !                                  !
           filetag(14)= 'cohjpsi-mumu_res    ' !                                  !
           filetag(15)= 'cohjpsi-mumu_cohphi0' !                                  !
      !   !************************************!                                  !
      !                                                                           !
      !                                                                           !
      !   !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!                          !
      !   !  Screen Output Strings:                    !                          !
           mctyp(0)= '|===        Data           ===|' !                          !
           mctyp(1)= '|===        CCDIS          ===|' !                          !
           mctyp(2)= '|===        NCDIS          ===|' !                          !
           mctyp(3)= '|===         JPsi          ===|' !                          !
           mctyp(4)= '|===        OBG            ===|' !                          !
           mctyp(5)= '|===       CohPi+          ===|' !                          !
           mctyp(6)= '|===      CohRho+          ===|' !                          !
           mctyp(7)= '|===      aNuMu CC         ===|' !                          !
           mctyp(8)= '|===         QE            ===|' !                          !
           mctyp(9)= '|===       CohPi0          ===|' !                          !
           mctyp(10)='|===       Nue CC          ===|' !                          !
           mctyp(11)='|===      aNue CC          ===|' !                          !
           mctyp(12)='|===      aNuMu NC         ===|' !                          !
           mctyp(13)='|===      CohRho0          ===|' !                          !
           mctyp(14)='|===        Res            ===|' !                          !
           mctyp(15)='|===      CohPhi0          ===|' !                          !
      !   !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!                          !
      !                                                                           !
      !                                                                           !
      !   !-------------------------------------!                                 !
      !   ! Set user index values for plotting: !                                 !
      !   !-------------------------------------!                                 !
           detidx(1) = 0 ! DC                   !                                 !      
           detidx(2) = 1 ! Other                !                                 !         
           detidx(3) = 2 ! Coil                 !                                 !        
           detidx(4) = 3 ! Upstream             !                                 !            
      !   !-------------------------------------!                                 !
           ncndidx(1) = 2 ! ncand = 2           !                                 !
           ncndidx(2) = 3 ! ncand = 3           !                                 !
           ncndidx(3) = 4 ! ncand = 4           !                                 !
           ncndidx(4) = 5 ! ncand = {3,4}       !                                 !
      !   !-------------------------------------!                                 !
           evtidx(1) = 0 ! CC DIS               !                                 !
           evtidx(2) = 1 ! OS DiMu              !                                 !
           evtidx(3) = 2 ! LS DiMu              !                                 !
           evtidx(4) = 3 ! OS Mu+X              !                                 !
      !   !-------------------------------------!                                 !
      !                                                                           !
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      !0o0o0o0o0o0o0o0o0o0o0---       END USER INPUT      ---0o0o0o0o0o0o0o0o0o0o0!
      !0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0!
      !---------------------------------------------------------------------------!


      call timestamp('start') !--> Print timestamp for program start


      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
      ! Check user settings for errors, and print to file:
      call check_print_settings(nvar,ncuts,nmc,
     |                 maxevts,usents,fullnts,runtype,cuthists,
     |                 norm,datacard,filetag,gtyp,gennt,
     |                 usecorr2D,usecorrMass,usecorrZeta,
     |                 formRenorm,iCorr,
     |                 ndetsec,nncand,nevtyp,
     |                 detsw,ncndsw,evtsw,
     |                 ndetsw,nncndsw,nevtsw,ncuthists)
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!


      !--------------------------------------!
      ! Initialize Variables:
       call HLIMIT(nhbook)
       call initvar(nmc,ncuts,nevtyp,nextyp,nncand,ndetsec,cuts)
       extyp=nextyp*nncndsw*ndetsw
      !--------------------------------------!


      !......................................................!
      !......................................................!
      ! Initialize the renormalization factor:

      ! Read current renorm if not making a new one:
      if(formRenorm.eq.0) then
       open(22,file='outputs/renorm.txt',status='old')
        read(22,'(F12.8)') renorm
       close(22)
      endif
      ! If forming renorm, or if not using
      ! any correction functions, set the
      ! renormalization factor to unity.
      if(formRenorm.eq.1.or.
     | ( usecorr2D  .eq.0 .and.
     |   usecorrMass.eq.0 .and.
     |   usecorrZeta.eq.0  )) then
        renorm=1.0D0
      endif
      ! Initialize Counts
      do ii=1,2
        reCounts(ii) = 0.0
      enddo
      ! If forming normalizations run over
      ! only the MC component that has 
      ! correction functions applied, and
      ! requires renormalization:
      if(formRenorm.eq.1) then
      do ii=0,nmc
       runtype(ii) = 0
      enddo
      runtype(iCorr)=1 
      endif
      !......................................................!
      !......................................................!


      !OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO!
      !OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO!
      ! Load Correction Functions and Binnings:
      if(runtype(iCorr).eq.1) then

       ! 2D Correction:
       if(usecorr2D.eq.1) then
         ! Read Correction:
         filename='2ddscorr.txt'
         call read_2dcorr(filename,nbin2Dmass,nbin2Dzeta,DScorr2d)
         ! Read Equipopulated Binnings:
         filename='2d-mass_binning.txt'
         call load_bins(filename,nbin2Dmass,binlim2Dmass)
         filename='2d-zeta_binning.txt'
         call load_bins(filename,nbin2Dzeta,binlim2Dzeta)
       endif
       !--------------------
       ! Mass Correction:
       if(usecorrMass.eq.1) then
         ! Read Correction:
         filename='masscorr.txt'
         call read_1dcorr(filename,nbinMass,DScorrMass)
         ! Read Equipopulated Binnings:
         filename='mass_binning.txt'
         call load_bins(filename,nbinMass,binlimMass)
       endif
       !--------------------
       ! Zeta Correction:
       if(usecorrZeta.eq.1) then
         ! Read Correction:
         filename='zetacorr.txt'
         call read_1dcorr(filename,nbinZeta,DScorrZeta)
         ! Read Equipopulated Binnings:
         filename='zeta_binning.txt'
         call load_bins(filename,nbinZeta,binlimZeta)
       endif

      endif
      !OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO!
      !OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO!

      print *,''
      print *,''
      print *,''
      print *,'0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0'
      print *,'0o0o0o                             o0o0o0'
      print *,'0o0o0o      Performing Analysis    o0o0o0'
      print *,'0o0o0o    And Filling Histograms   o0o0o0'
      print *,'0o0o0o                             o0o0o0'
      print *,'0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0o0'
      print *,''
      print *,''



      do nttyp=0,nmc !--> Loop over ntuple types (DL-01)

         if (runtype(nttyp).eq.0) goto 88 !--> Skip unwanted NT types

     
             print*,''
             print*,''
             print'(a31)'       ,'|=============================|'
             print'(a20,i2,a9)','|===   Working type ',nttyp,':    ===|'
             print'(a31)'       ,      mctyp(nttyp)
             print'(a31)'       ,'|=============================|'
             print*,''

           !----------------------------------------------------------!
           ! Read Ntuple Location Datacards:
            call FindOpenUnit(UnitNum)
            open(UnitNum,file=datacard(nttyp,fullnts),status='old')
            read(UnitNum,*) ntnum
            if(ntnum.gt.MaxNTS) then
             call printerror(1,482773842,'main.F              ')
             print*,'Too many ntuples. Increse ntname size.'
             print*,'(MaxNTS is too small)'
             print*,'ntnum: ',ntnum
             print*,'nttyp: ',nttyp
             print*,'MaxNTS: ',MaxNTS
             print*,'Datacard: ',datacard(nttyp,fullnts)
             print*,''
             stop
            endif
            do ii=1,ntnum
              read(UnitNum,55) ntname(ii)
            enddo
            close(UnitNum)
55          format(a100)
           ! Change the number of ntuples to be read if desired:
            if(usents.gt.0 .and. usents.lt.ntnum) ntnum = usents
           !----------------------------------------------------------!
 

           !---------------------------------------------------------!
           ! Open output histogram file and book histograms:
             length=LEN_TRIM(filetag(nttyp))
             call FindOpenUnit(hfileID)
             call hropen(hfileID,'OUTPUT','outputs/hbook/'//
     |                  filetag(nttyp)(1:length)//'.h','N',1024,istat)
             call hcdir('//OUTPUT',' ')
             ! Histogram Booking:
             call bookhistos(nvar,extyp,ncuts,zetaS,nbintyp,cuthists,
     |                 ndetsec,nncand,nevtyp,nextyp,
     |                 detsw,ncndsw,evtsw,
     |                 ndetsw,nncndsw,nevtsw,ncuthists,
     |                 detidx,ncndidx,evtidx)
           !---------------------------------------------------------!


           !ooooooooooooooooooooooooooooooooooooooooo!
           ! Open event picture printout files:
             if (nttyp.eq.0.and.evtpicsw.eq.1) 
     |                      call openevtpic(evtID,1)
           !ooooooooooooooooooooooooooooooooooooooooo!



       do ifile=1,ntnum !--> Loop over Ntuples (DL-02)

      
            !------------------------!
            ! Set NT header ID
            ! 1000 for Full Ntuples
            ! 501 for Baby Ntuples
            id = 500
            
            if(fullnts.eq.0      .and.
     |         datacard(nttyp,0).ne.
     |         datacard(nttyp,1) ) 
     |      id=501
            !------------------------!

            !------------------------------------------------------!
            ! Open Ntuple:
            call FindOpenUnit(ntID)
            call openntuple(ntID,ntname(ifile),id) 
 67         format(1a1,' Working on file ',I3,' of ',I3,': ',a100,$)
            if(ifile.eq.1) print *,''
            !Refresh line:
            print 67, char(13),ifile,ntnum,ntname(ifile)
            if(ifile.eq.ntnum) print *,''
            call hnoent(id,nevents) !--> Get # of events in NT
            ! Change the number of event if desired:
            if(maxevts.gt.0.and.maxevts.lt.nevents) nevents=maxevts
            !------------------------------------------------------!


        do ievent = 1,nevents  ! Loop over Events of an Ntuple (DL-03)
      

          call checkeventerr(id,ievent,err) !--> Returns err=0 if OK
          if (err.ne.0) goto 77


         !============================================================!
         !````````````````````````````````````````````````````````````!
         ! Impose Cuts and Fill Histograms
          call process_event(nmc,nttyp,extyp,nvar,ncuts,
     |                ndetsec,nncand,nevtyp,nextyp,
     |                cuts,norm,
     |                nttypobg,zetaS,cuthists,nbintyp,
     |                usecorr2D,nbin2Dmass,nbin2Dzeta,DScorr2D,
     |                binlim2Dmass,binlim2Dzeta,
     |                usecorrMass,nbinMass,DScorrMass,binlimMass,
     |                usecorrZeta,nbinZeta,DScorrZeta,binlimZeta,
     |                formRenorm,renorm,reCounts,iCorr,evtID,
     |                detsw,ncndsw,evtsw,extrapsw,evtpicsw)
         !,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,!
         !============================================================!
    

77      continue !--> Skip bad events
        enddo !--> Loop over Events (DL-03)

            call closentuple(ntID) !--> Close ntuple

       enddo !--> Loop over Ntuples (DL-02)


      !-----------------------------
      ! Close output histogram file
       call hcdir('//OUTPUT',' ')
       call hrout(0,icycle,' ')
       call hrend('OUTPUT')
       close(hfileID)
       call hdelet(0)
      !-----------------------------

      !ooooooooooooooooooooooooooooooooooooooooo!
      ! Close event picture printout files:
        if (nttyp.eq.0.and.evtpicsw.eq.1) then
          ! Close run/evt file:
          close(evtID(1))
          call closeevtpic(evtID)
        endif
      !ooooooooooooooooooooooooooooooooooooooooo!

      ! Print Cut Table:
      call cut_table(nmc,filetag,ttag,ncuts,
     |           nevtyp,nextyp,nncand,ndetsec,
     |           detsw,ncndsw,evtsw,cuts,norm,nttyp)



88    continue !--> Skip unwanted event types
      enddo !--> Loop over ntuple types (DL-01)
    
      ! Print renormalization factor:
      if (formRenorm.eq.1) call print_renorms(reCounts)


      call timestamp('stop ') !--> Print timestamp for program end
      print *,''
      print *,''
      print *,'**********************'
      print *,'***    JOB DONE    ***'
      print *,'**********************'
      print *,''
      print *,''

      stop
      end
