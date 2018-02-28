      SUBROUTINE DCVETO(NPRIM,TOTVETO,TOTTUBE,LVETO,LTUBE)
      IMPLICIT NONE
*
*-----$ Impose cuts on the DC-Veto and the Tube-Veto Hits
*
*
*      NPrim--- Number of primary tracks == ncand
*      ToTVeTo--- ..............  TotVeto(ltrk) 
*      ToTTube--                  TotTube(ltrk)
*      Output:  LVeto, Ltube
*      ----Both have to be 'true'       
      INTEGER NPRIM,TOTVETO,TOTTUBE
      LOGICAL LVETO,LTUBE

*
      IF(NPRIM.GE.4)THEN
        LVETO=.TRUE.
        LTUBE=.TRUE.
        RETURN
      ENDIF
*
      LVETO=.FALSE.
      LTUBE=.FALSE.
*
*-----$ Cut on the DC-Veto
*       N(u/v/y)2BCK: Within 2.5cm of the extrapolated 
*                     Track at the Veto chamber: 1-TRACK
* 
      IF(NPRIM.EQ.1)THEN
         LVETO=TOTVETO.LE.0   !=Out=> MC:  33/3722 // Data: 4661/30551
      ENDIF
*
*-----$ 2-Track
      IF(NPRIM.EQ.2)THEN
         LVETO=TOTVETO.LE.1   !=Out=> MC: 35/12116 // Data: 1207/55234
      ENDIF
*
*-----$ 3-Track
      IF(NPRIM.EQ.3)THEN
         LVETO=TOTVETO.LE.3   !=Out=> MC:  6/10541 // Data:  124/53612 
      ENDIF
          
*
*
*-----$ Cut on the Tube-Veto
*       N(u/v/y)TBCK: Within 3.0cm wide tube extrapolated 50cm Back. 
*       1-TRACK
* 
      IF(NPRIM.EQ.1)THEN
         LTUBE=TOTTUBE.LE.10  !=Out=> MC:  17/3689 // Data:  804/25891
      ENDIF
*
*-----$ 2-Track
      IF(NPRIM.EQ.2)THEN
         LTUBE=TOTTUBE.LE.10  !=Out=> MC: 38/12081 // Data:  943/53084
      ENDIF
*
*-----$ 3-Track
      IF(NPRIM.EQ.3)THEN
         LTUBE=.TRUE. 
      ENDIF
*
      RETURN
      END          
