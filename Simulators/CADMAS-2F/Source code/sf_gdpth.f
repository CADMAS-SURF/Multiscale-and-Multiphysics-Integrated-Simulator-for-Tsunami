      SUBROUTINE SF_GDPTH(DELZ0,DZ,LNDC,IRGRID,IGNO,POS,XX,YY)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION DELZ0(NNOD0),DZ(NUMI,NUMJ),LNDC(NUMI,NUMJ),IRGRID(NNOD)
     &         ,IGNO(NNOD),POS(3,NNOD),XX(MAXG1,NUMI),YY(MAXG1,NUMJ)

      REAL(8), POINTER :: DELZ(:)

      ALLOCATE( DELZ(NNOD) )

      DELZ(:) = 0.D0

      DO IG = 1, NNOD

        IJ = IRGRID(IG)

        IF( IJ <= 0 ) CYCLE

        CALL SF_IJ(I,J,IJ)

        XG = POS(1,IG)
        YG = POS(2,IG)

        IF( XG <= XX(1,I) + .5D0 * XX(2,I) ) THEN
          IM = I - 1
          IP = I
        ELSE
          IM = I
          IP = I + 1
        ENDIF

        XL = .5D0 * ( XX(2,IM) + XX(2,IP) )
        X  = XG - .5D0 * ( XX(1,IM) + XX(1,IP) )

        IF( YG <= YY(1,J) + .5D0 * YY(2,J) ) THEN
          JM = J - 1
          JP = J
        ELSE
          JM = J
          JP = J + 1
        ENDIF

        YL = .5D0 * ( YY(2,JM) + YY(2,JP) )
        Y  = YG - .5D0 * ( YY(1,JM) + YY(1,JP) )

        CALL SF_INTPL_DPTH(DELZ(IG),DZ,LNDC,I,IM,IP,J,JM,JP,XL,X,YL,Y)

      ENDDO

      IF( IPART == 0 ) THEN
        CALL SF_MPI_REDUCE_D(DELZ,DELZ0,NNOD,0)
      ELSEIF( IPART == 1 ) THEN
        CALL SF_REDUCE_D(DELZ0,NNOD0,DELZ,IGNO,NNOD)
      ENDIF

      DEALLOCATE( DELZ )

      END
