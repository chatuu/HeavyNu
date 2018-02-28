      subroutine timestamp(startstop)
      implicit none
      ! This subroutine prints the current time/date information
      
      character*5 startstop

      integer CYear,CDay,CurrentTime(3)
      character*10 CurrentMonth

      integer timedigits(6)
      
      integer CMonth

      integer UN
      integer ii

      call FindOpenUnit(UN) 

      ! Open output file:
      if(startstop.eq.'start') then
       open(UN,file='outputs/timestamp_start.txt',status='UNKNOWN')
      elseif(startstop.eq.'stop ') then
       open(UN,file='outputs/timestamp_stop.txt',status='UNKNOWN')
      else
       call printerror(2,487739021,'timestamp.F         ') 
       print*,'startstop value incorrect: ',startstop
       print*,'Aborting timestamp!'
       print*,''
       return
      endif

      call ITime(CurrentTime)
      call IDate(CMonth,CDay,CYear)
      CYear = CYear + 2000
      if (CMonth.eq.1)  CurrentMonth='January   '
      if (CMonth.eq.2)  CurrentMonth='February  '
      if (CMonth.eq.3)  CurrentMonth='March     '
      if (CMonth.eq.4)  CurrentMonth='April     '
      if (CMonth.eq.5)  CurrentMonth='May       '
      if (CMonth.eq.6)  CurrentMonth='June      '
      if (CMonth.eq.7)  CurrentMonth='July      '
      if (CMonth.eq.8)  CurrentMonth='August    '
      if (CMonth.eq.9)  CurrentMonth='September '
      if (CMonth.eq.10) CurrentMonth='October   '
      if (CMonth.eq.11) CurrentMonth='November  '
      if (CMonth.eq.12) CurrentMonth='December  '

      do ii=1,3
       timedigits(2*ii-1)=CurrentTime(ii)/10
       timedigits(2*ii  )=mod(CurrentTime(ii),10)
      enddo

100   format(A10,I2,' ',I4,' ',2I1,':',2I1,':',2I1)
      write(UN,100) CurrentMonth,CDay,CYear,(timedigits(ii),ii=1,6)

      close(UN)

      return
      end
