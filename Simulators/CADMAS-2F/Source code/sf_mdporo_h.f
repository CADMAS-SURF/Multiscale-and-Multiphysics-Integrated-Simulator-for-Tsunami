      SUBROUTINE SF_MDPORO_H(GGV,GGX,GGY,GGZ,GGW,KST,NF,INDC,ZZ,SUMZ
     &                      ,MODE,ICHG,DBUF)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION ZZ(MAXG1,NUMK),NF(NUMI,NUMJ,NUMK),GGV(NUMI,NUMJ,NUMK)
     &         ,GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
     &         ,GGZ(NUMI,NUMJ,NUMK),GGW(NUMI,NUMJ,NUMK),KST(NUMI,NUMJ)
     &         ,SUMZ(NUMI,NUMJ),ICHG(NUMI,NUMJ,NUMK),DBUF(NUMBUF*MAXBUF)
     &         ,INDC(NUMI,NUMJ,NUMK)

      REAL(8), POINTER :: GGF(:,:,:,:),GGFW(:,:,:)
!-----------------------------------------------------------------------
      IF( MYMIE == 1 ) THEN
        IP = 1
      ELSE
        IP = 0
      ENDIF

      IF( MYMJE == 1 ) THEN
        JP = 1
      ELSE
        JP = 0
      ENDIF

      GGW(:,:,:) = 1.D0

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE
            IF( GGV(I,J,K) < 1.D0 ) GGW(I,J,K) = 0.D0
          ENDDO
        ENDDO
      ENDDO

      IF( MODE == 0 ) THEN

        DO I = MYIS, MYIE
          DO J = MYJS, MYJE
            DO K = NUMK - 1, 1, -1
              IF( NF(I,J,K) == -1 .OR. GGV(I,J,K) < 1.D0 ) THEN
                KST(I,J) = K + 1
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDDO

      ELSEIF( MODE == 2 ) THEN

        ALLOCATE( GGF(6,NUMI,NUMJ,NUMK) )

        GGF(:,:,:,:) = 1.D0

        DO K = 2, NUMK - 1
          DO J = MYJS, MYJE
            DO I = MYIS, MYIE
              GGF(:,I,J,K) = GGV(I,J,K)
            ENDDO
          ENDDO
        ENDDO

        ICHG(:,:,:) = 0

        DO I = MYIS, MYIE
          DO J = MYJS, MYJE
            H = SUMZ(I,J)
            IF( H >= 0.D0 ) THEN
              DO K = KST(I,J), NUMK - 1
                IF( H == 0.D0 ) EXIT
                CALL SF_FILL(H,GGW(I,J,K),GGV(I,J,K),GGF(1,I,J,K)
     &                      ,ZZ(2,K),PORS)
                ICHG(I,J,K) = 1
              ENDDO
            ELSE
              H = -H
              DO K = KST(I,J) - 1, 2, -1
                IF( H == 0.D0 ) EXIT
                CALL SF_REMV(H,GGW(I,J,K),GGV(I,J,K),GGF(1,I,J,K)
     &                      ,ZZ(2,K))
                ICHG(I,J,K) = 1
              ENDDO
            ENDIF
          ENDDO
        ENDDO

        ALLOCATE( GGFW(NUMI,NUMJ,NUMK) )

        DO I = 1, 6
          GGFW(:,:,:) = GGF(I,:,:,:)
          CALL VF_P3SRD1(GGFW,DBUF,0)
          GGF(I,:,:,:) = GGFW(:,:,:)
        ENDDO

        DEALLOCATE( GGFW )

        DO K = 2, NUMK - 1
          DO J = MYJS, MYJE
            DO I = MYIS, MYIE + IP
              IF( ICHG(I-1,J,K) == 1 .OR. ICHG(I,J,K) == 1 )
     &          GGX(I,J,K) = DMIN1(GGF(2,I-1,J,K),GGF(1,I,J,K))
            ENDDO
          ENDDO
        ENDDO

        DO K = 2, NUMK - 1
          DO J = MYJS, MYJE + JP
            DO I = MYIS, MYIE
              IF( ICHG(I,J-1,K) == 1 .OR. ICHG(I,J,K) == 1 )
     &          GGY(I,J,K) = DMIN1(GGF(4,I,J-1,K),GGF(3,I,J,K))
            ENDDO
          ENDDO
        ENDDO

        DO K = 2, NUMK
          DO J = MYJS, MYJE
            DO I = MYIS, MYIE
              IF( ICHG(I,J,K-1) == 1 .OR. ICHG(I,J,K) == 1 )
     &          GGZ(I,J,K) = DMIN1(GGF(6,I,J,K-1),GGF(5,I,J,K))
            ENDDO
          ENDDO
        ENDDO

        DEALLOCATE( GGF )

        DO K = 2, NUMK - 1
          DO J = MYJS, MYJE
            DO I = MYIS, MYIE
              IF( NF(I,J,K) >= 0 .AND. GGV(I,J,K) < PLOWER ) THEN
                NF(I,J,K) = -1
                INDC(I,J,K) = -1
              ELSEIF( NF(I,J,K) == -1 .AND. GGV(I,J,K) >= PLOWER ) THEN
                NF(I,J,K) = 0
                INDC(I,J,K) = 0
              ENDIF
            ENDDO
          ENDDO
        ENDDO

      ENDIF
     
      IF( MODE == 0 ) THEN
        CALL VF_P3SRD2(GGW,DBUF,0)
      ELSE
        CALL VF_P3SRD2(GGV,DBUF,0)
        CALL VF_P3SRD2(GGX,DBUF,1)
        CALL VF_P3SRD2(GGY,DBUF,2)
        CALL VF_P3SRD2(GGZ,DBUF,3)
        CALL VF_P3SRD2(GGW,DBUF,0)
        CALL VF_P3SRI2(NF,DBUF,0)
        CALL VF_P3SRI2(INDC,DBUF,0)
      ENDIF

      END