      SUBROUTINE ST_PLOAD( J_PL4, R_PL4, N_PL4, J_PL, R_PL, N_PL, NELM,
     &                     IELM, NM, INDG, ITO )
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION J_PL4(6,*), R_PL4(4,*), J_PL(5,N_PL), R_PL(N_PL),
     &          IELM(NM,NELM), INDG(*)
C
      DO 10 I = 1, N_PL
C
        IF( J_PL(5,I) == 0 ) THEN
          NND = 3
        ELSE
          NND = 4
        ENDIF
C
        IP = N_PL4 + I
C
        IFOUND = 0
C
        DO 20 IE = 1, NELM
C
          ITYP = IELM(2,IE)
          ND = IELM(3,IE)
C
          IF( NND == 3 ) THEN
            IF( ITYP == 1 .AND. ( ND == 3 .OR. ND == 6 ) ) THEN
              CALL TRCHK( IFOUND, R_PL(I), J_PL(2,I), IELM(8,IE), INDG )
            ELSEIF(ITYP == 2 .AND. ( ND == 4 .OR. ND == 10 ) )THEN
              CALL TECHK( IFOUND, IG1, IG4, R_PL(I), J_PL(2,I)
     &                  , IELM(8,IE), INDG )
            ELSEIF(ITYP == 2 .AND. ( ND == 6 .OR. ND == 15 ) )THEN
              CALL PNCHK1( IFOUND, IG1, IG4, R_PL(I), J_PL(2,I)
     &                   , IELM(8,IE), INDG )
            ENDIF
          ELSEIF( NND == 4 ) THEN
            IF( ITYP == 1 .AND. ( ND == 4 .OR. ND == 8 ) ) THEN
              CALL QUCHK( IFOUND, R_PL(I), J_PL(2,I), IELM(8,IE), INDG )
            ELSEIF(ITYP == 2 .AND. ( ND == 8 .OR. ND == 20 ) )THEN
              CALL HXCHK( IFOUND, IG1, IG3, R_PL(I), J_PL(2,I)
     &                  , IELM(8,IE), INDG )
            ELSEIF(ITYP == 2 .AND. ( ND == 6 .OR. ND == 15 ) )THEN
              CALL PNCHK2( IFOUND, IG1, IG3, R_PL(I), J_PL(2,I)
     &                   , IELM(8,IE), INDG )
            ENDIF
          ENDIF
C
          IF( IFOUND == 1 ) THEN
C
            J_PL4(1,IP) = J_PL(1,I)
            J_PL4(2,IP) = IELM(1,IE)
            J_PL4(3,IP) = IELM(1,IE)
            IF( ITYP == 2 ) THEN
              J_PL4(4,IP) = IG1
              IF( NND == 3 ) THEN
                J_PL4(5,IP) = IG4
              ELSEIF( NND == 4 ) THEN
                J_PL4(5,IP) = IG3
              ENDIF
            ENDIF
C
            R_PL4(1,IP) = R_PL(I)
C
            GOTO 10
C
          ENDIF
C
   20   CONTINUE
C
        WRITE(ITO,*) 'PLOAD DATA ERROR, STOP IN SUB. ST_PLOAD.'
        CALL ERRSTP(90,ITO)
C
   10 CONTINUE
C
      N_PL4 = N_PL4 + N_PL
C
      END
C
      SUBROUTINE TRCHK(IFOUND,P,NODE,KN,INDG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION II(3,2),JJ(3,3),NODE(3),KN(3),INDG(*)
      DATA II / 1,2,3, 3,2,1 / 
      DATA JJ / 1,2,3, 2,3,1, 3,1,2 /
C----&------------------------------------------------------------------
      DO I = 1, 2
        DO J = 1, 3
C
          J1 = JJ(1,J)
          J2 = JJ(2,J)
          J3 = JJ(3,J)
C
          I1 = II(J1,I)
          I2 = II(J2,I)
          I3 = II(J3,I)
C
          N1 = INDG( KN(I1) )
          N2 = INDG( KN(I2) )
          N3 = INDG( KN(I3) )
C
          IF( NODE(1) .EQ. N1 .AND. NODE(2) .EQ. N2 .AND.
     &        NODE(3) .EQ. N3 ) THEN
            IFOUND = 1
            IF( I .EQ. 2 ) P = -P
            RETURN
          ENDIF
C
        ENDDO
      ENDDO
C
      END
C
      SUBROUTINE TECHK(IFOUND,IG1,IG4,P,NODE,KN,INDG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION II(4,8),JJ(3,3),NODE(3),KN(4),INDG(*)
      DATA II / 2,4,3,1, 3,4,1,2, 4,2,1,3, 1,2,3,4,
     &          3,4,2,1, 1,4,3,2, 1,2,4,3, 3,2,1,4 / 
      DATA JJ / 1,2,3, 2,3,1, 3,1,2 /
C----&------------------------------------------------------------------
      DO I = 1, 8
        DO J = 1, 3
C
          J1 = JJ(1,J)
          J2 = JJ(2,J)
          J3 = JJ(3,J)
C
          I1 = II(J1,I)
          I2 = II(J2,I)
          I3 = II(J3,I)
          I4 = II(4 ,I)
C
          N1 = INDG( KN(I1) )
          N2 = INDG( KN(I2) )
          N3 = INDG( KN(I3) )
          N4 = INDG( KN(I4) )
C
          IF( NODE(1) .EQ. N1 .AND. NODE(2) .EQ. N2 .AND.
     &        NODE(3) .EQ. N3 ) THEN
            IFOUND = 1
            IG1 = N1
            IG4 = N4
            IF( I .GE. 5 ) P = -P
            RETURN
          ENDIF
C
        ENDDO
      ENDDO
C
      END
C
      SUBROUTINE QUCHK(IFOUND,P,NODE,KN,INDG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION II(4,2),JJ(4,4),NODE(4),KN(4),INDG(*)
      DATA II / 1,2,3,4, 4,3,2,1 / 
      DATA JJ / 1,2,3,4, 2,3,4,1, 3,4,1,2, 4,1,2,3 /
C----&------------------------------------------------------------------
      DO I = 1, 2
        DO J = 1, 4
C
          J1 = JJ(1,J)
          J2 = JJ(2,J)
          J3 = JJ(3,J)
          J4 = JJ(4,J)
C
          I1 = II(J1,I)
          I2 = II(J2,I)
          I3 = II(J3,I)
          I4 = II(J4,I)
C
          N1 = INDG( KN(I1) )
          N2 = INDG( KN(I2) )
          N3 = INDG( KN(I3) )
          N4 = INDG( KN(I4) )
C
          IF( NODE(1) .EQ. N1 .AND. NODE(2) .EQ. N2 .AND.
     &        NODE(3) .EQ. N3 .AND. NODE(4) .EQ. N4 ) THEN
            IFOUND = 1
            IF( I .EQ. 2 ) P = -P
            RETURN
          ENDIF
C
        ENDDO
      ENDDO
C
      END
C
      SUBROUTINE HXCHK(IFOUND,IG1,IG3,P,NODE,KN,INDG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION II(4,12),JJ(4,4),NODE(4),KN(8),INDG(*)
      DATA II / 1,2,3,4, 1,5,6,2, 2,6,7,3, 3,7,8,4, 4,8,5,1, 5,8,7,6,
     &          4,3,2,1, 2,6,5,1, 3,7,6,2, 4,8,7,3, 1,5,8,4, 6,7,8,5 / 
      DATA JJ / 1,2,3,4, 2,3,4,1, 3,4,1,2, 4,1,2,3 /
C----&------------------------------------------------------------------
      DO I = 1, 12
        DO J = 1, 4
C
          J1 = JJ(1,J)
          J2 = JJ(2,J)
          J3 = JJ(3,J)
          J4 = JJ(4,J)
C
          I1 = II(J1,I)
          I2 = II(J2,I)
          I3 = II(J3,I)
          I4 = II(J4,I)
C
          N1 = INDG( KN(I1) )
          N2 = INDG( KN(I2) )
          N3 = INDG( KN(I3) )
          N4 = INDG( KN(I4) )
C
          IF( NODE(1) .EQ. N1 .AND. NODE(2) .EQ. N2 .AND.
     &        NODE(3) .EQ. N3 .AND. NODE(4) .EQ. N4 ) THEN
            IFOUND = 1
            IG1 = N1
            IG3 = N3
            IF( I .GE. 7 ) P = -P
            RETURN
          ENDIF
C
        ENDDO
      ENDDO
C
      END
C
      SUBROUTINE PNCHK1(IFOUND,IG1,IG4,P,NODE,KN,INDG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION II(3,4),JJ(3,3),NODE(3),KN(6),INDG(*)
      DATA II / 1,2,3, 4,6,5,
     &          3,2,1, 5,6,4 /
      DATA JJ / 1,2,3, 2,3,1, 3,1,2 /
C----&------------------------------------------------------------------
      DO I = 1, 4
        DO J = 1, 3
C
          J1 = JJ(1,J)
          J2 = JJ(2,J)
          J3 = JJ(3,J)
C
          I1 = II(J1,I)
          I2 = II(J2,I)
          I3 = II(J3,I)
C
          N1 = INDG( KN(I1) )
          N2 = INDG( KN(I2) )
          N3 = INDG( KN(I3) )
C
          IF( NODE(1) .EQ. N1 .AND. NODE(2) .EQ. N2 .AND.
     &        NODE(3) .EQ. N3 ) THEN
            IFOUND = 1
            IG1 = N1
            IG4 = 0
            IF( I .GE. 3 ) P = -P
            RETURN
          ENDIF
C
        ENDDO
      ENDDO
C
      END
C
      SUBROUTINE PNCHK2(IFOUND,IG1,IG3,P,NODE,KN,INDG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION II(4,6),JJ(4,4),NODE(4),KN(6),INDG(*)
      DATA II / 1,4,5,2, 2,5,6,3, 3,6,4,1,
     &          2,5,4,1, 3,6,5,2, 1,4,6,3 / 
      DATA JJ / 1,2,3,4, 2,3,4,1, 3,4,1,2, 4,1,2,3 /
C----&------------------------------------------------------------------
      DO I = 1, 6
        DO J = 1, 4
C
          J1 = JJ(1,J)
          J2 = JJ(2,J)
          J3 = JJ(3,J)
          J4 = JJ(4,J)
C
          I1 = II(J1,I)
          I2 = II(J2,I)
          I3 = II(J3,I)
          I4 = II(J4,I)
C
          N1 = INDG( KN(I1) )
          N2 = INDG( KN(I2) )
          N3 = INDG( KN(I3) )
          N4 = INDG( KN(I4) )
C
          IF( NODE(1) .EQ. N1 .AND. NODE(2) .EQ. N2 .AND.
     &        NODE(3) .EQ. N3 .AND. NODE(4) .EQ. N4 ) THEN
            IFOUND = 1
            IG1 = N1
            IG3 = N3
            IF( I .GE. 4 ) P = -P
            RETURN
          ENDIF
C
        ENDDO
      ENDDO
C
      END
