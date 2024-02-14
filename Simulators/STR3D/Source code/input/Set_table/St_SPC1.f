      SUBROUTINE ST_SPC1( ISP1, NSP1, NISP1, NNSP1, J_SPC1, N_SPC1,
     &                    INDGR )
C
      DIMENSION ISP1(2,NISP1), NSP1(7,NNSP1), J_SPC1(9,N_SPC1), INDGR(*)
C
      IP = 0
C
      DO I = 1, NISP1
C
        DO J = 1, N_SPC1
C
          IF( J_SPC1(1,J) == ISP1(1,I) ) THEN
C
            IS = J_SPC1(2,J)
            IE = J_SPC1(3,J)
C
            DO K = IS, IE
              IF( INDGR(K) > 0 ) THEN
                IP = IP + 1
                NSP1(1,IP) = K
                NSP1(2:7,IP) = J_SPC1(4:9,J)
              ENDIF
            ENDDO 
C
          ENDIF
C
        ENDDO
C
        ISP1(2,I) = IP
C
      ENDDO
C
      NSP1(1,:) = INDGR( NSP1(1,:) )
C
      END