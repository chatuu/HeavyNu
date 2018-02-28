      subroutine label_muid(phaseii,iimu,other)

cc  output : phaseii=1 ---> phase ii events
c            phaseii=0 ---> stubs

      implicit none

#include "ntuple.inc"      

      integer phaseii,iimu,other
      integer ii
      real pmax,plep

      phaseii=0
      iimu=0
      other=0
      pmax = 0
    
      
      ! Loop over charged tracks and search for a phase1 Muon
      do ii=1,ncand 
        if (probmu(ii).eq.1) then
           plep = sqrt(pxlr(ii)**2+pylr(ii)**2+pzlr(ii)**2)
           if(plep.gt.pmax) then
               pmax = plep
               iimu = ii
               phaseii = 1
           endif
        ! Tag phase2 muon if needed later
        elseif((nx1(ii).gt.0.or.nx2(ii).gt.0)
     +    .and.(ny1(ii).gt.0.or.ny2(ii).gt.0)) then
           other = other +1
        ! Tag phase3 muon if needed later
        elseif(isstub(ii).eq.1) then
           other = other +1
        endif
      enddo
        
       
      if(phaseii.eq.1) goto 77 !--> Return if a phase1 muon was found
           
                 
      ! Continue checking for phase2 or phase3 muons:         
      if (other.gt.0)  then
              
        do ii=1,ncand
          if( (nx1(ii).gt.0.or.nx2(ii).gt.0) .and.
     +        (ny1(ii).gt.0.or.ny2(ii).gt.0) ) then
               
              plep=sqrt(pxlr(ii)**2+pylr(ii)**2+pzlr(ii)**2)
              if(plep.gt.pmax) then
                pmax=plep
                iimu=ii
                phaseii=2
              endif

          elseif (isstub(ii).eq.1) then
            
              plep=sqrt(pxlr(ii)**2+pylr(ii)**2+pzlr(ii)**2)
              if(plep.gt.pmax) then
                pmax=plep
                iimu=ii
                phaseii=3
              endif
          endif
        enddo  ! loop over all charged tracks (ncand)

               
      endif   

 77   continue

      return
      end
