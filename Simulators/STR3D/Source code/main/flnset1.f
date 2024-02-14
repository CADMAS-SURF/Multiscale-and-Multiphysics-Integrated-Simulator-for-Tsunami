      SUBROUTINE FLNSET1(FLNAME,NLEN0,NLEN)
C
      CHARACTER FLNAME*256
C----&------------------------------------------------------------------
      DO I=1,256
        IF(FLNAME(I:I) .EQ. '.') THEN
          NLEN0=I-1
          EXIT
        ENDIF
      ENDDO
C
      DO I=NLEN0+2,256
        IF(FLNAME(I:I) .EQ. ' ') THEN
          NLEN=I-1
          EXIT
        ENDIF
      ENDDO
C
      END
