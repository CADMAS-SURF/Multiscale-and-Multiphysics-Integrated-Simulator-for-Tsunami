      SUBROUTINE ORDER(N,IA,IB)
C
      DIMENSION IA(*)
C-----------------------------------------------------------------------
      DO I=1,N
        IF( IB .LT. IA(I) ) THEN
          CALL SHIFTB0(IA,I,N,1)
          IA(I)=IB
          GOTO 10
        ELSEIF( IB .EQ. IA(I) ) THEN
          RETURN
        ENDIF
      ENDDO
C
      IA(N+1)=IB
C
   10 N=N+1
C
      RETURN
      END
