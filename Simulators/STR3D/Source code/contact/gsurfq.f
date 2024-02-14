      SUBROUTINE GSURFQ(XG,NDIM,NG,XX,IELQ,IVRQ)

      IMPLICIT REAL*8(A-H,O-Z)      
      DIMENSION XG(NDIM,NG),XX(NDIM,*),IELQ(4,*),IVRQ(NG),IG(4)

      DO I = 1, NG

        IF( IVRQ(I) == 0 ) CYCLE

        IG(:) = IELQ(:,IVRQ(I))

        CALL MEAN4(XG(1,I),XX,NDIM,IG,4)

      ENDDO

      END
