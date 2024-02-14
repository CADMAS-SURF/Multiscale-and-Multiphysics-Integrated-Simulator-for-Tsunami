      SUBROUTINE MATML(A,ITYPA,B,ITYPB,C,ITYPC,NI,NJ,NK)

!     A = B' * C'
!     
!     A(NI,NJ), B'(NI,NK), C'(NK,NJ)
!
!     ITYPA : 1  1D
!             2  2D
!
!     ITYPB : 1  B'=B  1D(SYM.)
!             2  B'=B  2D
!             3  B'=BT 2D
!
!     ITYPC : 1  C'=C  1D(SYM.) 
!             2  C'=C  2D
!             3  C'=CT 2D
!
!     1Dの場合のA,B,Cの配列要素の並びは,3×3の行列を例にとると
!
!       1 2 3
!         4 5
!           6

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION A(*),B(*),C(*)

      ITYP = 100 * ITYPA + 10 * ITYPB + ITYPC

      SELECT CASE( ITYP )
      CASE( 111 )

        IA = 0
        DO I = 1, NI
          DO J = I, NJ
            IA = IA + 1
            A(IA) = 0.
            IB = I - NK
            IC = J - NK
            DO K = 1, I
              IB = IB + NK + 1 - K
              IC = IC + NK + 1 - K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = I + 1, J
              IB = IB + 1
              IC = IC + NK + 1 - K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = J + 1, NK
              IB = IB + 1
              IC = IC + 1
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 112 )

        IA = 0
        DO I = 1, NI
          DO J = I, NJ
            IA = IA + 1
            A(IA) = 0.
            IB = I - NK
            DO K = 1, I
              IB = IB + NK + 1 - K
              IC = NK * ( J - 1 ) + K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = I + 1, NK
              IB = IB + 1
              IC = NK * ( J - 1 ) + K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 113 )

        IA = 0
        DO I = 1, NI
          DO J = I, NJ
            IA = IA + 1
            A(IA) = 0.
            IB = I - NK
            DO K = 1, I
              IB = IB + NK + 1 - K
              IC = NJ * ( K - 1 ) + J
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = I + 1, NK
              IB = IB + 1
              IC = NJ * ( K - 1 ) + J
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 121 )

        IA = 0
        DO I = 1, NI
          DO J = I, NJ
            IA = IA + 1
            A(IA) = 0.
            IC = J - NK
            DO K = 1, J
              IB = NI * ( K - 1 ) + I
              IC = IC + NK + 1 - K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = J + 1, NK
              IB = NI * ( K - 1 ) + I
              IC = IC + 1
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 122 )

        IA = 0
        DO I = 1, NI
          DO J = I, NJ
            IA = IA + 1
            A(IA) = 0.
            DO K = 1, NK
              IB = NI * ( K - 1 ) + I
              IC = NK * ( J - 1 ) + K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 123 )

        IA = 0
        DO I = 1, NI
          DO J = I, NJ
            IA = IA + 1
            A(IA) = 0.
            DO K = 1, NK
              IB = NI * ( K - 1 ) + I
              IC = NJ * ( K - 1 ) + J
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 131 )

        IA = 0
        DO I = 1, NI
          DO J = I, NJ
            IA = IA + 1
            A(IA) = 0.
            IC = J - NK
            DO K = 1, J
              IB = NK * ( I - 1 ) + K
              IC = IC + NK + 1 - K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = J + 1, NK
              IB = NK * ( I - 1 ) + K
              IC = IC + 1
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 132 )

        IA = 0
        DO I = 1, NI
          DO J = I, NJ
            IA = IA + 1
            A(IA) = 0.
            DO K = 1, NK
              IB = NK * ( I - 1 ) + K
              IC = NK * ( J - 1 ) + K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 133 )

        IA = 0
        DO I = 1, NI
          DO J = I, NJ
            IA = IA + 1
            A(IA) = 0.
            DO K = 1, NK
              IB = NK * ( I - 1 ) + K
              IC = NJ * ( K - 1 ) + J
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 211 )

        DO I = 1, NI

          DO J = 1, I - 1
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            IB = I - NK
            IC = J - NK
            DO K = 1, J
              IB = IB + NK + 1 - K
              IC = IC + NK + 1 - K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = J + 1, I
              IB = IB + NK + 1 - K
              IC = IC + 1
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = I + 1, NK
              IB = IB + 1
              IC = IC + 1
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO

          DO J = I, NJ
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            IB = I - NK
            IC = J - NK
            DO K = 1, I
              IB = IB + NK + 1 - K
              IC = IC + NK + 1 - K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = I + 1, J
              IB = IB + 1
              IC = IC + NK + 1 - K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = J + 1, NK
              IB = IB + 1
              IC = IC + 1
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO

        ENDDO

      CASE( 212 )

        DO I = 1, NI
          DO J = 1, NJ
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            IB = I - NK
            DO K = 1, I
              IB = IB + NK + 1 - K
              IC = NK * ( J - 1 ) + K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = I + 1, NK
              IB = IB + 1
              IC = NK * ( J - 1 ) + K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 213 )

        DO I = 1, NI
          DO J = 1, NJ
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            IB = I - NK
            DO K = 1, I
              IB = IB + NK + 1 - K
              IC = NJ * ( K - 1 ) + J
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = I + 1, NK
              IB = IB + 1
              IC = NJ * ( K - 1 ) + J
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 221 )

        DO I = 1, NI
          DO J = 1, NJ
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            IC = J - NK
            DO K = 1, J
              IB = NI * ( K - 1 ) + I
              IC = IC + NK + 1 - K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = J + 1, NK
              IB = NI * ( K - 1 ) + I
              IC = IC + 1
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 222 )

        DO I = 1, NI
          DO J = 1, NJ
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            DO K = 1, NK
              IB = NI * ( K - 1 ) + I
              IC = NK * ( J - 1 ) + K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 223 )

        DO I = 1, NI
          DO J = 1, NJ
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            DO K = 1, NK
              IB = NI * ( K - 1 ) + I
              IC = NJ * ( K - 1 ) + J
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 231 )

        DO I = 1, NI
          DO J = 1, NJ
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            IC = J - NK
            DO K = 1, J
              IB = NK * ( I - 1 ) + K
              IC = IC + NK + 1 - K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
            DO K = J + 1, NK
              IB = NK * ( I - 1 ) + K
              IC = IC + 1
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 232 )

        DO I = 1, NI
          DO J = 1, NJ
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            DO K = 1, NK
              IB = NK * ( I - 1 ) + K
              IC = NK * ( J - 1 ) + K
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      CASE( 233 )

        DO I = 1, NI
          DO J = 1, NJ
            IA = NI * ( J - 1 ) + I
            A(IA) = 0.
            DO K = 1, NK
              IB = NK * ( I - 1 ) + K
              IC = NJ * ( K - 1 ) + J
              A(IA) = A(IA) + B(IB) * C(IC)
            ENDDO
          ENDDO
        ENDDO

      END SELECT

      END