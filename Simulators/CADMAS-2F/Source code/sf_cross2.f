      SUBROUTINE SF_CROSS2(A,B,C)                                    
C                                                                       
      IMPLICIT REAL*8(A-H,O-Z)                                          
C                                                                       
C     C=A*B/|A*B|
C                                                                       
      DIMENSION A(3),B(3),C(3)                                          
C                                                                       
      X = A(2) * B(3) - A(3) * B(2)                                     
      Y = A(3) * B(1) - A(1) * B(3)                                     
      Z = A(1) * B(2) - A(2) * B(1)                                     
      XLN =DSQRT(X*X+Y*Y+Z*Z)                                           
      XLN = 1.0 /XLN                                                    
      C(1) = X * XLN                                                    
      C(2) = Y * XLN                                                    
      C(3) = Z * XLN                                                    
      RETURN                                                            
      END                                                               
