      SUBROUTINE SF_GIPND(IPND,IPGRID,NNOD)

      DIMENSION IPGRID(2,NNOD),IPND(NNOD)

      DO I = 1, NNOD
        IF( IPGRID(1,I) > 0 ) THEN
          IPND(I) = 1
        ELSE
          IPND(I) = IPGRID(1,I)
          IF( IPGRID(2,I) == 1 ) IPND(I) = 2
        ENDIF
      ENDDO

      END
