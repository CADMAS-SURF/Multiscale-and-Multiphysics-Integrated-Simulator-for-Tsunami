      SUBROUTINE SF_NF_RESET(NF,INDC,GGV,IVOID,IBUF)
 
      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION NF(NUMI,NUMJ,NUMK),INDC(NUMI,NUMJ,NUMK)
     &         ,GGV(NUMI,NUMJ,NUMK),IVOID(NUMI,NUMJ,NUMK)
     &         ,IBUF(*)

      IVOID(:,:,:) = 0

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE
            IF( GGV(I,J,K) < PLOWER ) IVOID(I,J,K) = 1
          ENDDO
        ENDDO
      ENDDO

      IF( MYMIS == 1 ) THEN
        IS = 1
      ELSE
        IS = 0
      ENDIF

      IF( MYMIE == 1 ) THEN
        IE = -1
      ELSE
        IE = 0
      ENDIF

      IF( MYMJS == 1 ) THEN
        JS = 1
      ELSE
        JS = 0
      ENDIF

      IF( MYMJE == 1 ) THEN
        JE = -1
      ELSE
        JE = 0
      ENDIF

      DO K = 2, NUMK - 1
        DO J = MYJS + JS, MYJE + JE
          DO I = MYIS + IS, MYIE + IE
            IF( GGV(I,J,K) < PLOWER2 .AND.
     &        ( ( ( NF(I-1,J,K) == -1 .OR. GGV(I-1,J,K) < PLOWER ).AND.
     &            ( NF(I+1,J,K) == -1 .OR. GGV(I+1,J,K) < PLOWER ).AND.
     &            ( NF(I-1,J,K) /= -1 .OR. NF(I+1,J,K) /= -1     ) ).OR.
     &          ( ( NF(I,J-1,K) == -1 .OR. GGV(I,J-1,K) < PLOWER ).AND.
     &            ( NF(I,J+1,K) == -1 .OR. GGV(I,J+1,K) < PLOWER ).AND.
     &            ( NF(I,J-1,K) /= -1 .OR. NF(I,J+1,K) /= -1     ) ).OR.
     &          ( ( NF(I,J,K-1) == -1 .OR. GGV(I,J,K-1) < PLOWER ).AND.
     &            ( NF(I,J,K+1) == -1 .OR. GGV(I,J,K+1) < PLOWER ).AND.
     &            ( NF(I,J,K-1) /= -1 .OR. NF(I,J,K+1) /= -1     ) ) ) )
     &      THEN
              IVOID(I,J,K) = 1
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      IF( MYMIS == 1 ) THEN
        IS = 2
      ELSE
        IS = 0
      ENDIF

      IF( MYMIE == 1 ) THEN
        IE = -1
      ELSE
        IE = 1
      ENDIF

      IF( MYMJS == 1 ) THEN
        JS = 2
      ELSE
        JS = 0
      ENDIF

      IF( MYMJE == 1 ) THEN
        JE = -1
      ELSE
        JE = 1
      ENDIF

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS + IS, MYIE + IE
            IF( ( GGV(I-1,J,K) < PLOWER2 .AND. GGV(I,J,K) < PLOWER2 )
     &          .AND. ( NF(I-2,J,K) == -1 .OR. GGV(I-2,J,K) < PLOWER )
     &          .AND. ( NF(I+1,J,K) == -1 .OR. GGV(I+1,J,K) < PLOWER )
     &          .AND. ( NF(I-2,J,K) /= -1 .OR. NF(I+1,J,K) /= -1 ) )
     &      THEN
              IVOID(I-1:I,J,K) = 1
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      DO K = 2, NUMK - 1
        DO J = MYJS + JS, MYJE + JE
          DO I = MYIS, MYIE
            IF( ( GGV(I,J-1,K) < PLOWER2 .AND. GGV(I,J,K) < PLOWER2 )
     &          .AND. ( NF(I,J-2,K) == -1 .OR. GGV(I,J-2,K) < PLOWER )
     &          .AND. ( NF(I,J+1,K) == -1 .OR. GGV(I,J+1,K) < PLOWER )
     &          .AND. ( NF(I,J-2,K) /= -1 .OR. NF(I,J+1,K) /= -1 ) )
     &      THEN
              IVOID(I,J-1:J,K) = 1
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      DO K = 3, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE
            IF( ( GGV(I,J,K-1) < PLOWER2 .AND. GGV(I,J,K) < PLOWER2 )
     &          .AND. ( NF(I,J,K-2) == -1 .OR. GGV(I,J,K-2) < PLOWER )
     &          .AND. ( NF(I,J,K+1) == -1 .OR. GGV(I,J,K+1) < PLOWER )
     &          .AND. ( NF(I,J,K-2) /= -1 .OR. NF(I,J,K+1) /= -1 ) )
     &      THEN
              IVOID(I,J,K-1:K) = 1
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      CALL VF_P3SRI2(IVOID,IBUF,0)

      DO K = 1, NUMK
        DO J = 1, NUMJ
          DO I = 1, NUMI
            IF( NF(I,J,K) >= 0 .AND. IVOID(I,J,K) == 1 ) THEN
              NF(I,J,K) = -2
              INDC(I,J,K) = -1
            ELSEIF( NF(I,J,K) == -2 .AND.  IVOID(I,J,K) == 0 ) THEN
              NF(I,J,K) = 0
              INDC(I,J,K) = 0
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      END