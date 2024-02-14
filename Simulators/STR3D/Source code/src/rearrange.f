      SUBROUTINE REARRANGE(IDX,NSIZ,NIDX,N,IPREV,NEXT,LAST,IFR,IB,JB
     &                    ,JCOL)
C
      DIMENSION IDX(NSIZ,*),N(*),IPREV(*),NEXT(*),LAST(*)
C
      IP = LAST(IFR)
C
      DO
C
        IF( IP == IB ) THEN
          JP = JB
          JADD = JCOL
        ELSE
          JP = 1
          IPR = IPREV(IP)
          JADD = IDX(NSIZ,IPR)
        ENDIF
C
        IF( IP == LAST(IFR) ) THEN
          IF( N(IP) > 0 ) IDX(JP+1:N(IP)+1,IP) = IDX(JP:N(IP),IP)
          IDX(JP,IP) = JADD
          N(IP) = N(IP) + 1
          IF( N(IP) == NSIZ ) THEN
            NIDX = NIDX + 1
            NEXT(IP) = NIDX
            IPREV(NIDX) = IP
            LAST(IFR) = NIDX
          ENDIF
        ELSE
          IDX(JP+1:NSIZ,IP) = IDX(JP:NSIZ-1,IP)
          IDX(JP,IP) = JADD
        ENDIF
C
        IF( IP == IB ) THEN
          EXIT
        ELSE
          IP = IPREV(IP)
        ENDIF
C
      ENDDO
C
      END