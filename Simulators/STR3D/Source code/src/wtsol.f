      SUBROUTINE WTSOL( SIGG, EPSG, PRNSIGG, NNOD, IPS, IPE, NG_ENS,
     &                  IG_ENS, IFL, ICW )
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SIGG(6,NNOD), EPSG(6,NNOD), PRNSIGG(3,NNOD), NG_ENS(*),
     &          IG_ENS(NNOD,*), IFL(*)
C
      IF( ICW == 0 ) WRITE(IFL(20),'(A)') 'solid stress'
C
      DO IP = IPS, IPE
C
        WRITE(IFL(20),'(A)') 'part'
        WRITE(IFL(20),'(I10)') IP
        WRITE(IFL(20),'(A)') 'coordinates'
C
        DO J = 1, 6
C
          IF( J == 5 ) THEN
            JJ = 6
          ELSEIF( J == 6 ) THEN
            JJ = 5
          ELSE
            JJ = J
          ENDIF
C
          DO I = 1, NG_ENS(IP)
            IG = IG_ENS(I,IP)
            WRITE(IFL(20),'(1PE12.4)') SIGG(JJ,IG)
          ENDDO
C
        ENDDO
C
      ENDDO
C
      IF( ICW == 0 ) WRITE(IFL(21),'(A)') 'solid strain'
C
      DO IP = IPS, IPE
C
        WRITE(IFL(21),'(A)') 'part'
        WRITE(IFL(21),'(I10)') IP
        WRITE(IFL(21),'(A)') 'coordinates'
C
        DO J = 1, 6
C
          IF( J == 5 ) THEN
            JJ = 6
          ELSEIF( J == 6 ) THEN
            JJ = 5
          ELSE
            JJ = J
          ENDIF
C
          IF( JJ >= 4 ) THEN
            FAC = .5D0
          ELSE
            FAC = 1.D0
          ENDIF
C
          DO I = 1, NG_ENS(IP)
            IG = IG_ENS(I,IP)
            WRITE(IFL(21),'(1PE12.4)') EPSG(JJ,IG) * FAC
          ENDDO
C
        ENDDO
C
      ENDDO
C
      IF( ICW == 0 ) WRITE(IFL(22),'(A)') 'solid principal stress'
C
      DO IP = IPS, IPE
C
        WRITE(IFL(22),'(A)') 'part'
        WRITE(IFL(22),'(I10)') IP
        WRITE(IFL(22),'(A)') 'coordinates'
C
        DO J = 1, 3
C
          DO I = 1, NG_ENS(IP)
            IG = IG_ENS(I,IP)
            WRITE(IFL(22),'(1PE12.4)') PRNSIGG(J,IG)
          ENDDO
C
        ENDDO
C
      ENDDO
C
      ICW = 1
C
      END
