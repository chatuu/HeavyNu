            subroutine MCWtDc_wts(iyr,fyr,VtxZ,GenMC,wt)
            implicit none

c
c Get the weights for MC includeing generated and reconstructed ones            
c 
c   iyr = beginning of the time 1-9504, 2-9508, 3-9511, 4-96,5-97, 6-98
c   fyr = end of the time 1-9504, 2-9508, 3-9511, 4-96,5-97, 6-9
c   VtxZ = Z-position of the Monte Carlo
c   GenMC:  If you are trying to get the weight for generated MC, 
c                     GenMC=0
c           elseif you are trying to get the weight for reconstructed MC,
c                    GenMC = 1
c
c  OUTPUT is wt (wt is a double precision number)
c

      integer iyr, fyr, GenMC
      real VtxZ
      double precision wt

      integer ii,jj 

      double precision Zadd
      parameter(Zadd = 5.0D0)

      double precision ModZ(5)
      save ModZ
      data ModZ / 5.0D0, 35.0D0, 115.0D0, 265.0D0, 405.0D0/

      logical*4  LFirst
      save LFirst
      data LFirst /.true./

      double precision  DtEvt(6), DtEvtZadd(6), DtStk(6)
      double precision  MCStk(6), MCEvtAdd(6),  MCEvt(6)
         
      double precision DT(6,5),DtZadd(6,5), MC(6,5),MCZadd(6,5)
      save DT,DtZadd, MC,MCZadd            
             
      ! To find open Unit Number:
      integer UnitNum
      logical UnitOpened
            
      IF ( LFIRST ) THEN
         LFIRST = .FALSE.

         !*********************************************************!
         ! Find a free file unit number:
          UnitNum=0
          do ii=1,99 !--> Loop over allowable file units 
           if(ii.ne.5.and.ii.ne.6) then  !--> (5&6 reserved by Fortran)
            INQUIRE (UNIT=ii,OPENED=UnitOpened) !--> Check if unit number is already being used
            if ((.not.UnitOpened) .and. UnitNum.eq.0) then
             UnitNum=ii
             goto 66 !--> Open unit found... stop checking
            endif
           endif
          enddo
66        continue
          if(UnitNum.eq.0) then
           print*,''
           print*,''
           print*,'**************************************'
           print*,'********        ERROR         ********'
           print*,'***        In McWtDc_wts.F         ***'
           print*,'*** No Available File Unit Numbers ***'
           print*,'***       Stopping program!        ***'
           print*,'**************************************'
           print*,''
           print*,''
           print*,'UnitNum: ',UnitNum
           stop
          endif
         !*********************************************************!
         open(UnitNum, file='wt-dis.dat',status = 'unknown')
         do ii = 1,6
         do jj = 1,5
          read(UnitNum,100)  MC(ii,jj), MCZAdd(ii,jj)
         enddo
         enddo
         close(UnitNum)

         open(UnitNum, file='wt-data.dat', status='unknown')
         do ii = 1,6
         do jj = 1,5
          read(UnitNum,100) DT(ii,jj), DtZadd(ii,jj)
         enddo
         enddo
         close(UnitNum)
               
      ENDIF     
100   format(f14.1, 3x, f14.1)
                

      do jj = 1,5
       DtEvt(jj)    = 0.0D0
       DtEvtZadd(jj)= 0.0D0
       DtStk(jj)    = 0.0D0
       MCStk(jj)    = 0.0D0
       MCEvtAdd(jj) = 0.0D0
       MCEvt(jj)    = 0.0D0
      enddo
                   

      do jj = 1,5
        do ii = iyr, fyr
           DtEvt(jj)     = DtEvt(jj) + Dt(ii,jj)
           DtEvtZadd(jj) = DtEvtZadd(jj) + DtZadd(ii,jj)
           DtStk(jj)     = DtEvt(jj) - DtEvtZadd(jj)
        enddo
        MCStk(jj)    = MC(1,jj) - MCZAdd(1,jj)    
        MCEvtAdd(jj) = MCZAdd(1,jj)
        MCEvt(jj)    = MC(1,jj)   
      enddo    

      do ii = iyr,fyr
        if(ii.eq.1) DtEvtZadd(4) = DtEvtZadd(4) - DtZadd(ii,4)
        if(ii.eq.2) DtEvtZadd(3) = DtEvtZadd(3) - DtZadd(ii,3)
        if(ii.eq.3.or.ii.eq.4) DtEvtZadd(1) = DtEvtZadd(1)-DtZadd(ii,1)
        if(ii.eq.5.or.ii.eq.6) DtEvtZadd(2) = DtEvtZadd(2)-DtZadd(ii,2)
      enddo

                

      ! Generated:
      if(GenMC.eq.0) then
        DtEvt(4) =  DtEvt(4) + DtEvt(5)
        MCEvt(4)  = MC(1,4) + MC(1,5)
        if(VtxZ.gt.265.0) then
               wt = 1.0D0
        elseif(VtxZ.gt.115.0) then
               wt = (MCEvt(4)/MCEvt(3)) * (DTEvt(3)/DtEvt(4))
        elseif(VtxZ.gt.35.0) then
               wt = (MCEvt(4)/MCEvt(2)) * (DTEvt(2)/DtEvt(4))
        else
               wt = (MCEvt(4)/MCEvt(1)) * (DTEvt(1)/DtEvt(4))
                          
        endif  

      ! Reconstructed:
      else 
c..................................380-405              
        if(dble(VtxZ).gt.modZ(5)) then
               wt = 0.0D0

c.................................265--380
        elseif(dble(VtxZ).gt.ModZ(4)) then 
          if(dble(VtxZ).ge.(ModZ(4)+Zadd)) then
               wt = 1.0D0
          else
               wt = (MCStk(4)/MCEvtadd(4)) *(DtEvtZadd(4)/DtStk(4))
          endif

c.................................115-.265
        elseif(dble(VtxZ).gt.modZ(3)) then
          if(dble(VtxZ).ge.(modZ(3)+Zadd)) then
               wt = (MCStk(4)/MCStk(3)) * (DTStk(3)/DtStk(4))
          else
               wt = (MCStk(4)/MCEvtadd(3)) * (DtEvtZadd(3)/DtStk(4))
          endif
                    
c.................................35-115                    
        elseif(dble(VtxZ).gt.modZ(2))  then
          if(dble(VtxZ).ge.(modZ(2)+Zadd)) then
               wt = (MCStk(4)/MCStk(2)) * (DTStk(2)/DtStk(4))
          else
               wt = (MCStk(4)/MCEvtadd(2)) * (DtEvtZadd(2)/DtStk(4))
          endif
c..................................5-35
        else
          if(dble(VtxZ).ge.(modZ(1)+Zadd)) then
               wt = (MCStk(4)/MCStk(1)) *(DTStk(1)/DtStk(4))
          else
               wt = (MCStk(4)/MCEvtadd(1)) * (DtEvtZadd(1)/DtStk(4))
          endif             
             
                  
        endif

      endif


      return
      end
              
            
        
            

           
            
