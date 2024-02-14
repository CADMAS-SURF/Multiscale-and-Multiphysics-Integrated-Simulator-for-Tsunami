      SUBROUTINE CRH2EDGE(CRH,NS,J1,IG,IEDQ,POS)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,3),POS(3,*),IG(2),E(3,3),V12(3),V(3),CRH(*)
     &         ,E1(3),E2(3),E3(3),A2(12),A3(12)
      DATA E / 1.D0, 0.D0, 0.D0,
     &         0.D0, 1.D0, 0.D0,
     &         0.D0, 0.D0, 1.D0 /
C----&------------------------------------------------------------------
      CALL SHIFT1(XYZ(1,3),POS(1,NS),3)
      CALL SHIFT1(XYZ(1,1),POS(1,IG(1)),3)
      CALL SHIFT1(XYZ(1,2),POS(1,IG(2)),3)
C
C     --- E1,E2,E3 ---
C
      CALL SUBVEC(V12,XYZ(1,2),XYZ(1,1),3)
      CALL DIRCOS(E1,V12,3)
C
      J0=1
      DO 210 J=2,3
        IF( DABS(E1(J)) .LT. DABS(E1(J0)) ) J0=J
  210 CONTINUE
C
      CALL CROSS2(E1,E(1,J0),E3)
      CALL CROSS2(E3,E1,E2)
C
C     --- D1,D2,D3 ---
C
      D1=E2(2)*E3(3)-E2(3)*E3(2)
      D2=E2(3)*E3(1)-E2(1)*E3(3)
      D3=E2(1)*E3(2)-E2(2)*E3(1)
C
C     --- A2,A3 ---
C
      CALL LENCD(T,V,XYZ)
      TM=1.D0-T
C
      IF( IEDQ .EQ. 0 ) THEN
C
        N=6
C
        A2(1) = TM*E2(1)
        A2(2) = TM*E2(2)
        A2(3) = TM*E2(3)
        A2(4) = T *E2(1)
        A2(5) = T *E2(2)
        A2(6) = T *E2(3)
C
        A3(1) = TM*E3(1)
        A3(2) = TM*E3(2)
        A3(3) = TM*E3(3)
        A3(4) = T *E3(1)
        A3(5) = T *E3(2)
        A3(6) = T *E3(3)
C
      ELSE
C
        N=12
C
        A2(1) = (TM+.25D0*T)*E2(1)
        A2(2) = (TM+.25D0*T)*E2(2)
        A2(3) = (TM+.25D0*T)*E2(3)
        A2(4) = .25D0*T*E2(1)
        A2(5) = .25D0*T*E2(2)
        A2(6) = .25D0*T*E2(3)
        A2(7:9) = A2(4:6)
        A2(10:12) = A2(4:6)
C
        A3(1) = (TM+.25D0*T)*E3(1)
        A3(2) = (TM+.25D0*T)*E3(2)
        A3(3) = (TM+.25D0*T)*E3(3)
        A3(4) = .25D0*T*E3(1)
        A3(5) = .25D0*T*E3(2)
        A3(6) = .25D0*T*E3(3)
        A3(7:9) = A3(4:6)
        A3(10:12) = A3(4:6)
C
      ENDIF
C    
C     --- CRH ---
C
      IF( J1 .EQ. 1 ) THEN
        CRH(1)=D2
        CALL RMULT5(CRH(2),E3(3),A2,-E2(3),A3,N)
        CRH(N+2)=D3
        CALL RMULT5(CRH(N+3),-E3(2),A2,E2(2),A3,N)
        CALL RMULT1(CRH,CRH,1.D0/D1,2*N+2)
      ELSEIF( J1 .EQ. 2 ) THEN
        CRH(1)=D3
        CALL RMULT5(CRH(2),E3(1),A2,-E2(1),A3,N)
        CRH(N+2)=D1
        CALL RMULT5(CRH(N+3),-E3(3),A2,E2(3),A3,N)
        CALL RMULT1(CRH,CRH,1.D0/D2,2*N+2)
      ELSE
        CRH(1)=D1
        CALL RMULT5(CRH(2),E3(2),A2,-E2(2),A3,N)
        CRH(N+2)=D2
        CALL RMULT5(CRH(N+3),-E3(1),A2,E2(1),A3,N)
        CALL RMULT1(CRH,CRH,1.D0/D3,2*N+2)
      ENDIF
C
      RETURN
      END
