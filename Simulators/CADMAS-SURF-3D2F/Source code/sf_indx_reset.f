      SUBROUTINE SF_INDX_RESET(INDX,INDY,INDZ,NF,GGX,GGY,GGZ)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION INDX(NUMI,NUMJ,NUMK),INDY(NUMI,NUMJ,NUMK)
     &         ,INDZ(NUMI,NUMJ,NUMK),NF(NUMI,NUMJ,NUMK)
     &         ,GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
     &         ,GGZ(NUMI,NUMJ,NUMK)
!-----------------------------------------------------------------------
      INDX = 0
      INDY = 0
      INDZ = 0

      IF( MYMIS == 1 ) INDX(1,:,:) = -1
      IF( MYMJS == 1 ) INDX(:,1,:) = -1
      IF( MYMJE == 1 ) INDX(:,NUMJ,:) = -1
      INDX(:,:,1) = -1
      INDX(:,:,NUMK) = -1

      IF( MYMJS == 1 ) INDY(:,1,:) = -1
      IF( MYMIS == 1 ) INDY(1,:,:) = -1
      IF( MYMIE == 1 ) INDY(NUMI,:,:) = -1
      INDY(:,:,1) = -1
      INDY(:,:,NUMK) = -1

      INDZ(:,:,1) = -1
      IF( MYMIS == 1 ) INDZ(1,:,:) = -1
      IF( MYMIE == 1 ) INDZ(NUMI,:,:) = -1
      IF( MYMJS == 1 ) INDZ(:,1,:) = -1
      IF( MYMJE == 1 ) INDZ(:,NUMJ,:) = -1

      NUMB = 0

      DO K = 2, NUMK - 1
        DO J = 2, NUMJ - 1
          DO I = 2, NUMI
            N1 = NF(I-1,J,K)
            N2 = NF(I,J,K)
            IF( N1 < 0 .AND. N2 < 0 ) THEN
              INDX(I,J,K) = -1
            ELSEIF( N1 < 0 .OR. N2 < 0 ) THEN
              NUMB = NUMB + 1
              INDX(I,J,K) = NUMB
            ELSEIF( GGX(I,J,K) < PLOWER * 1.D-1 ) THEN
              INDX(I,J,K) = -1
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      DO K = 2, NUMK - 1
        DO J = 2, NUMJ
          DO I = 2, NUMI - 1
            N1 = NF(I,J-1,K)
            N2 = NF(I,J,K)
            IF( N1 < 0 .AND. N2 < 0 ) THEN
              INDY(I,J,K) = -1
            ELSEIF( N1 < 0 .OR. N2 < 0 ) THEN
              NUMB = NUMB + 1
              INDY(I,J,K) = NUMB
            ELSEIF( GGY(I,J,K) < PLOWER * 1.D-1 ) THEN
              INDY(I,J,K) = -1
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      DO K = 2, NUMK
        DO J = 2, NUMJ - 1
          DO I = 2, NUMI - 1
            N1 = NF(I,J,K-1)
            N2 = NF(I,J,K)
            IF( N1 < 0 .AND. N2 < 0 ) THEN
              INDZ(I,J,K) = -1
            ELSEIF( N1 < 0 .OR. N2 < 0 ) THEN
              NUMB = NUMB + 1
              INDZ(I,J,K) = NUMB
            ELSEIF( GGZ(I,J,K) < PLOWER * 1.D-1 ) THEN
              INDZ(I,J,K) = -1
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      END