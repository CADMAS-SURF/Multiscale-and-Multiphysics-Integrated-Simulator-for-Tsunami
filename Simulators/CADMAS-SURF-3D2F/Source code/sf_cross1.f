      SUBROUTINE SF_CROSS1 (A,B,S)                                    
C                                                                       
      IMPLICIT REAL*8(A-H,O-Z)                                          
C                                                                       
C                                                                       
      DIMENSION A(3),B(3)                                          
C                                                                       
      X = A(2) * B(3) - A(3) * B(2)                                     
      Y = A(3) * B(1) - A(1) * B(3)                                     
      Z = A(1) * B(2) - A(2) * B(1)                                     
      S =DSQRT(X*X+Y*Y+Z*Z)                                           
      RETURN                                                            
      END

