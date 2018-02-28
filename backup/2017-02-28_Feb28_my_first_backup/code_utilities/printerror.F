      subroutine printerror(typ,id,filename)
      implicit none

      ! typ -->  =1 for error (stops program)
      !          =2 for warning
      ! id --> Error/warning number
      !        (9 or fewer digits)
      ! filename --> name of subroutine calling printerror

      integer typ
      integer id
      character*20 filename

      !=======================================!
      ! Check input                           !
      if(typ.ne.1.and.typ.ne.2) then
       print*,''
       print*,''
       print*,'Error 3885399'
       print*,'Bad typ input into printerror.F'
       print*,'typ: ',typ
       print*,'id: ',id
       print*,'filename: ',filename
       print*,''
       stop       
      endif
      if(id.lt.1.or.id.gt.999999999) then
       print*,''
       print*,''
       print*,'Error 599488377'
       print*,'Bad id input into printerror.F'
       print*,'id: ',id
       print*,'typ: ',typ
       print*,'filename: ',filename
       print*,''
       stop       
      endif
      !=======================================!

      ! Error printing:
      if(typ.eq.1) then
       print*,''
       print*,''
       print '(A33)','|*******************************|'
       print '(A11,A20,A2)','| Error in ',filename,' |'
       print '(A33)','|                               |'
       print '(A12,I9,A12)','|          #',id,'           |'
       print '(A33)','|       STOPPING PROGRAM!       |'
       print '(A33)','|*******************************|'
       print*,''

      ! Warning printing:
      elseif (typ.eq.2) then
       print*,''
       print*,''
       print '(A35)','|*********************************|'
       print '(A13,A20,A2)','| Warning in ',filename,' |'
       print '(A35)','|                                 |'
       print '(A13,I9,A13)','|           #',id,'            |'
       print '(A35)','|*********************************|'
       print*,''

      else
       print*,''
       print*,''
       print*,'Error 39960299',typ
       print*,''
       stop
      endif

      return
      end
