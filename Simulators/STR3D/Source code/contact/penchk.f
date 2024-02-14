      SUBROUTINE PENCHK(ISLV,RSLV,POSSO,POSSN,EN,IBDY,NBDY,ICBD,IELC
     &                 ,IELCB,IEDG,IELA,ICELA,ICEL,IBTE,GELC,ICTB,POSO
     &                 ,POSN,TOLA,IPFLG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IELC(3,*),GELC(*),POSO(3,*),POSN(3,*),XYZO(3,4)
     &         ,XYZN(3,4),RSLV(3),ISLV(2),RL(3),ICTB(NBDY,NBDY)
     &         ,ICBD(NBDY),RLO(3),RLN(3),RNO(3),RNN(3),GXN(3)
     &         ,ICELA(2,*),ICEL(*),IBTE(4,*),IEDG(6,*),IELA(3,*),EN(3)
     &         ,POSSO(3),POSSN(3),IELCB(*)
      LOGICAL AREAIN
C----&------------------------------------------------------------------
      CALL SHIFT1(XYZO(1,4),POSSO,3)
      CALL SHIFT1(XYZN(1,4),POSSN,3)
C
      DO 100 JBDY=1,NBDY
C
        IF(ICTB(IBDY,JBDY) .EQ. 0) GOTO 100
C
        IF(JBDY .EQ. 1) THEN
          IS=1
        ELSE
          IS=ICBD(JBDY-1)+1
        ENDIF
        IE=ICBD(JBDY)
C
        DO 110 I=IS,IE
C
          IF(IPFLG .EQ. 1) THEN
            IF(IELCB(I) .EQ. 0) GOTO 110
          ENDIF
C
          DO 111 J=1,3
            CALL SHIFT1(XYZO(1,J),POSO(1,IELC(J,I)),3)
            CALL SHIFT1(XYZN(1,J),POSN(1,IELC(J,I)),3)
  111     CONTINUE
C
          GXN(:) = ( XYZN(:,1) + XYZN(:,2) + XYZN(:,3) ) / 3.D0
          CALL LENGTH(DIST,XYZN(1,4),GXN,3)
          IF( DIST .GT. 2.D0*GELC(I) ) GOTO 110
C
          CALL AREACD(RLO,RNO,HO,XYZO)
          CALL AREACD(RLN,RNN,HN,XYZN)
C
          IF( HO .LE. 1.D-10 .AND. HN .GT. 1.D-10 ) THEN
C
            T = ( 1.D-10 - HO )/( HN - HO )
            CALL RMULT5(RL,1.D0-T,RLO,T,RLN,3)
C
            IF( AREAIN(RL,1.D-10) ) THEN
              CALL GRAZEEL(ICHK,POSSN,RL,IELC(1,I),ICELA,ICEL,IBTE,POSN)
              IF( ICHK .EQ. 1 ) THEN
                CALL PENST(ISLV,RSLV,IEDG,IELA(1,I),I,RL,XYZN,RNN,POSN
     &                    ,EN,TOLA)
                RETURN
              ENDIF
            ENDIF
C
          ENDIF
C
  110   CONTINUE
C
  100 CONTINUE
C
      END
