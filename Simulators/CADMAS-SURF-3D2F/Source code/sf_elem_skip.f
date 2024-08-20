      SUBROUTINE SF_ELEM_SKIP(ISKIP,XS,XE,YS,YE,N,NP,GRID)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(N),Y(N),NP(N),GRID(3,*)

      X(:) = GRID(1,NP(:))
      Y(:) = GRID(2,NP(:))

      XMIN = MINVAL( X )
      YMIN = MINVAL( Y )
      XMAX = MAXVAL( X )
      YMAX = MAXVAL( Y )

      IF( XMAX < XS .OR. XMIN > XE .OR. YMAX < YS .OR. YMIN > YE ) THEN
        ISKIP = 1
      ELSE
        ISKIP = 0
      ENDIF

      END
