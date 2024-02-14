      SUBROUTINE REFER
C----------------------------------------
C     子領域の物理量を参照
C     (子領域の最大標高地点の物理量を参照する)
C     (流速は水面セルの値を参照する) ! 修正が必要
C----------------------------------------
      USE M_GRID
      USE M_GEOM
      USE M_FILEIN
      USE M_COM_STOC,ONLY: NC_STOC,IC_STOC
C
      IMPLICIT NONE
C
      REAL(8)::HTMX
      INTEGER::NAP,NAC
      INTEGER::IP,JP,KP,IPS,IPE,JPS,JPE
      INTEGER::IC,JC,KC,ICS,ICE,JCS,JCE
      INTEGER::ICT,JCT,KCT
C
      INTEGER::NNHP,NNHCT,NNUP,NNUCT
      INTEGER::NN
C
C
C
      DO NN=1,NC_STOC
         NAP=IC_STOC(1,NN)
         NAC=IC_STOC(2,NN)
CDEBUG         write(*,*) 'NAP=',NAP,' NAC=',NAC,'/',NN
C
         IPS = 1 + INT((XG(NAC,0) - XG(NAP,0)) / DX(NAP) + 1.0D-5)
         IPE = INT((XG(NAC,NI(NAC)) - XG(NAP,0)) / DX(NAP) + 1.0D-5)
         JPS = 1 + INT((YG(NAC,0) - YG(NAP,0)) / DY(NAP) + 1.0D-5)
         JPE = INT((YG(NAC,NJ(NAC)) - YG(NAP,0)) / DY(NAP) + 1.0D-5)
C
         DO IP=IPS,IPE
         DO JP=JPS,JPE
            ICS = 1 + INT((XG(NAP,IP-1) - XG(NAC,0)) / DX(NAC) + 1.0D-5)
            ICE = INT((XG(NAP,IP) - XG(NAC,0)) / DX(NAC) + 1.0D-5)
            JCS = 1 + INT((YG(NAP,JP-1) - YG(NAC,0)) / DY(NAC) + 1.0D-5)
            JCE = INT((YG(NAP,JP) - YG(NAC,0)) / DY(NAC) + 1.0D-5)
            HTMX = ZG(NAP,0)
C
            ICT=ICS
            JCT=JCS
            DO IC=ICS,ICE     !  最大標高地点の検索
            DO JC=JCS,JCE
               IF (HTMX.LT.HT(NAC,IC,JC)) THEN
                  HTMX = HT(NAC,IC,JC)
                  ICT  = IC
                  JCT  = JC
               ENDIF
            ENDDO
            ENDDO
C
            NNHCT = NI(NAC) * (JCT-1) + ICT
            DO KC=1,NK(NAC)   !  水面セルの検索(子領域)
               IF ( ZG(NAC,KC-1).LT.HHAR1(NAC,NNHCT) .AND.
     &              ZG(NAC,KC)  .GE.HHAR1(NAC,NNHCT) ) THEN
                  KCT = KC
                  EXIT
               ENDIF
            ENDDO
C
            NNHP  = NI(NAP) * (JP -1) + IP
            NNHCT = NI(NAC) * (JCT-1) + ICT
            HT(NAP,IP,JP) = HT(NAC,ICT,JCT)
            HHAR1(NAP,NNHP) = HHAR1(NAC,NNHCT)
            DO KP=1,NK(NAP)
               NNUP  = NI(NAP)*NJ(NAP) * (KP -1)
     &                 + (NI(NAP) * (JP -1) + IP )
               NNUCT = NI(NAC)*NJ(NAC) * (KCT-1)
     &                 + (NI(NAC) * (JCT-1) + ICT)
               IF ( ZG(NAP,KP-1).GE.HHAR1(NAP,NNHP) ) THEN
                  UUAR1(NAP,NNUP) = 0.0
                  VVAR1(NAP,NNUP) = 0.0
               ELSE
                  IF ( ZG(NAP,KP).LE.HT(NAP,IP,JP) ) THEN
                     UUAR1(NAP,NNUP) = 0.0
                     VVAR1(NAP,NNUP) = 0.0
                  ELSE
                     UUAR1(NAP,NNUP) = UUAR1(NAC,NNUCT)
                     VVAR1(NAP,NNUP) = VVAR1(NAC,NNUCT)
                  ENDIF
               ENDIF
            ENDDO
C
         ENDDO
         ENDDO
C
      ENDDO
C
C
      RETURN
      END
