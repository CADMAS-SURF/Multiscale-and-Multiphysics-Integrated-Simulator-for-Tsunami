      SUBROUTINE CROSS (A,B,C)                                    
C                                                                       
      IMPLICIT REAL*8(A-H,O-Z)                                          
C                                                                       
C     C=A*B
C                                                                       
      DIMENSION A(3),B(3),C(3)                                          
C                                                                       
      C(1) = A(2) * B(3) - A(3) * B(2)                                     
      C(2) = A(3) * B(1) - A(1) * B(3)                                     
      C(3) = A(1) * B(2) - A(2) * B(1)                                     
C
      RETURN                                                            
      END                                                               
