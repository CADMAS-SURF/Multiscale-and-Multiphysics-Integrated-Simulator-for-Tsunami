      SUBROUTINE SPCDSETDU(ISID,DUG,KK,ISPD,NSPD,SPCD,F,ITYP,UG1,UG2
     &                    ,DT1,DT2)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),ISPD(2,*),NSPD(7,*),SPCD(6,*),UG1(6,*),UG2(6,*)
     &         ,DUG(6,*)
C-----------------------------------------------------------------------
      NISPD = KK(38)
C
      DO 410 J=1,NISPD
        IF(ISPD(1,J) .EQ. ISID) THEN
          CALL ADDSET(J,ISPD,IS,NN)
          DO 411 K=1,NN
            IAD=IS-1+K
            NODFIX=NSPD(1,IAD)
            DO 412 L=1,6
              IF(NSPD(L+1,IAD) .EQ. 1) 
     &          CALL ENFDISP(DUG(L,NODFIX),SPCD(L,IAD),F,ITYP
     &                      ,UG1(L,NODFIX),UG2(L,NODFIX),DT1,DT2)
  412       CONTINUE
  411     CONTINUE
          GOTO 500
        ENDIF
  410 CONTINUE
C
  500 RETURN
      END
