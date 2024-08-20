      SUBROUTINE SF_MDFXYZ(FX,FY,FZ,INDX,INDY,INDZ,GGX,GGY,GGZ,GGXP,GGYP
     &                    ,GGZP,DBUF)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION FX(NUMI,NUMJ,NUMK),FY(NUMI,NUMJ,NUMK),FZ(NUMI,NUMJ,NUMK)
     &         ,GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
     &         ,GGZ(NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
     &         ,INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK),DBUF(*)
     &         ,GGXP(NUMI,NUMJ,NUMK),GGYP(NUMI,NUMJ,NUMK)
     &         ,GGZP(NUMI,NUMJ,NUMK)

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE

            IF( INDX(I,J,K) == 0 .AND. GGX(I,J,K) /= GGXP(I,J,K) .AND.
     &          GGX(I,J,K) < GMIN .AND. FX(I,J,K) < FMIN )
     &        FX(I,J,K) = FMIN

            IF( INDY(I,J,K) == 0 .AND. GGY(I,J,K) /= GGYP(I,J,K) .AND.
     &          GGY(I,J,K) < GMIN .AND. FY(I,J,K) < FMIN )
     &        FY(I,J,K) = FMIN

            IF( INDZ(I,J,K) == 0 .AND. GGZ(I,J,K) /= GGZP(I,J,K) .AND.
     &          GGZ(I,J,K) < GMIN .AND. FZ(I,J,K) < FMIN )
     &        FZ(I,J,K) = FMIN

          ENDDO
        ENDDO
      ENDDO

      CALL VF_P3SRD2(FX,DBUF,1)
      CALL VF_P3SRD2(FY,DBUF,2)
      CALL VF_P3SRD2(FZ,DBUF,3)

      END
