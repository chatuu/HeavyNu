      subroutine initvar(nmc,ncuts,nevtyp,nextyp,nncand,ndetsec,cuts)
      implicit none

      ! This subroutine simply initializes arrays to desired values
      ! (generally zero).

      integer nmc
      integer ncuts
      integer nevtyp
      integer nextyp
      integer nncand
      integer ndetsec
      double precision cuts(3,ncuts,0:nmc,nevtyp,nextyp,nncand,ndetsec)

      integer ii,jj,kk,ll,mm,nn,oo

      do ii=1,3
       do jj=1,ncuts
        do kk=0,nmc
         do ll=1,nevtyp
          do mm=1,nextyp
          do nn=1,nncand
          do oo=1,ndetsec
          cuts(ii,jj,kk,ll,mm,nn,oo)=0.0D0
          enddo
          enddo
          enddo
         enddo
        enddo
       enddo
      enddo


      return
      end
