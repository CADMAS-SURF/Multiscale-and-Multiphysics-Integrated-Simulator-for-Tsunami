      SUBROUTINE ORDER2(N,IS,IA,IB)
C
      DIMENSION IA(*)
C-----------------------------------------------------------------------
      DO I=IS,N
        IF( IB .LT. IA(I) ) THEN
          CALL SHIFTB0(IA,I,N,1)
          IA(I)=IB
          NEXTIS=I+1
          GOTO 10
        ELSEIF( IB .EQ. IA(I) ) THEN
          NEXTIS=I+1
          GOTO 20
        ENDIF
      ENDDO
C
      IA(N+1)=IB
      NEXTIS=N+2
C
   10 N=N+1
C
   20 IS=NEXTIS
C
      RETURN
      END
