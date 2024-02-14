      SUBROUTINE ETABM(E,S,T,RL,D1,D2,VST,VTT,BX,BY,BZ,UX,UY,UZ)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RI(3,3),UX(3),UY(3),UZ(3),UXD(3),UYD(3),UZD(3)
     &         ,CXX(12,12),CXY(12,12),CXZ(12,12),WK1(3,3),WK2(3,3)
     &         ,WK3(3,3),WK4(3,3),WK5(3,3),WK6(3,3),VST(3,2),VTT(3,2)
     &         ,BX(3,12),BY(3,12),BZ(3,12),BXTBX(12,12),BXTBY(12,12)
     &         ,BXTBZ(12,12),E(78,3)

      RI(:,:) = 0.

      RI(1,1) = 1.D0
      RI(2,2) = 1.D0
      RI(3,3) = 1.D0

      UXD(:) = UX(:)
      UYD(:) = UY(:)
      UZD(:) = UZ(:)

      UXD(1) = UXD(1) + 1.D0
      UYD(2) = UYD(2) + 1.D0
      UZD(3) = UZD(3) + 1.D0

      CXX(:,:) = 0.
      CXY(:,:) = 0.
      CXZ(:,:) = 0.

      DO I = 1, 2

        IS = 3 + 6 * ( I - 1 ) + 1
        IE = IS + 2

        SELECT CASE( I )
        CASE( 1 )
          P = -.5D0
        CASE( 2 ) 
          P = .5D0
        END SELECT
       
        CALL AXB(WK1,VST(1,I),UXD,3,1,3)
        CALL AXB(WK2,VTT(1,I),UXD,3,1,3)

        CALL VECML1(C1,VST(1,I),UXD,3)
        CALL VECML1(C2,VTT(1,I),UXD,3)

        CXX(IS:IE,IS:IE)
     &  = .5D0 / RL * P * ( S * D1 * ( WK1(:,:) - C1 * RI(:,:) )
     &                    + T * D2 * ( WK2(:,:) - C2 * RI(:,:) ) )

        CALL AXB(WK3,VST(1,I),UYD,3,1,3)
        CALL AXB(WK4,VTT(1,I),UYD,3,1,3)

        CALL VECML1(C3,VST(1,I),UYD,3)
        CALL VECML1(C4,VTT(1,I),UYD,3)

        CXY(IS:IE,IS:IE)
     &  = .125D0 * ( WK1(:,:) - C1 * RI(:,:) )
     &    + .25D0 / RL * P * ( S * D1 * ( WK3(:,:) - C3 * RI(:,:) )
     &                       + T * D2 * ( WK4(:,:) - C4 * RI(:,:) ) )

        CALL AXB(WK5,VST(1,I),UZD,3,1,3)
        CALL AXB(WK6,VTT(1,I),UZD,3,1,3)

        CALL VECML1(C5,VST(1,I),UZD,3)
        CALL VECML1(C6,VTT(1,I),UZD,3)

        CXZ(IS:IE,IS:IE)
     &  = .125D0 * ( WK2(:,:) - C2 * RI(:,:) )
     &    + .25D0 / RL * P * ( S * D1 * ( WK5(:,:) - C5 * RI(:,:) )
     &                       + T * D2 * ( WK6(:,:) - C6 * RI(:,:) ) )

      ENDDO

      CALL MATML(BXTBX,2,BX,3,BX,2,12,12,3)
      CALL MATML(BXTBY,2,BX,3,BY,2,12,12,3)
      CALL MATML(BXTBZ,2,BX,3,BZ,2,12,12,3)

      IP = 0

      DO I = 1, 12
        DO J = I, 12
          IP = IP + 1
          E(IP,1) = CXX(I,J) + CXX(J,I) + BXTBX(I,J)
          E(IP,2) = CXY(I,J) + CXY(J,I) + .5D0*(BXTBY(I,J)+BXTBY(J,I))
          E(IP,3) = CXZ(I,J) + CXZ(J,I) + .5D0*(BXTBZ(I,J)+BXTBZ(J,I))
        ENDDO
      ENDDO

      END

      