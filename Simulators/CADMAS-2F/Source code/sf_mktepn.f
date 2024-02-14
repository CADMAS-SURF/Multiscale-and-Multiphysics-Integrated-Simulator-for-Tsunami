      SUBROUTINE SF_MKTEPN(IG,JG)

      DIMENSION IG(4,11),JG(6),KG(3)

      DATA KG / -1, -2, -3 /

      IG(1,1) = KG(1)
      IG(2,1) = JG(5)
      IG(3,1) = JG(2)
      IG(4,1) = KG(2)

      IG(1,2) = KG(2)
      IG(2,2) = JG(6)
      IG(3,2) = JG(3)
      IG(4,2) = KG(3)

      IG(1,3) = KG(3)
      IG(2,3) = JG(4)
      IG(3,3) = JG(1)
      IG(4,3) = KG(1)

      IG(1,4) = KG(1)
      IG(2,4) = KG(2)
      IG(3,4) = KG(3)
      IG(4,4) = JG(6)

      IG(1,5) = JG(6)
      IG(2,5) = JG(5)
      IG(3,5) = JG(4)
      IG(4,5) = KG(1)

      IG(1,6) = KG(1)
      IG(2,6) = JG(5)
      IG(3,6) = KG(2)
      IG(4,6) = JG(6)

      IG(1,7) = KG(1)
      IG(2,7) = KG(3)
      IG(3,7) = JG(4)
      IG(4,7) = JG(6)

      IG(1,8) = KG(1)
      IG(2,8) = KG(3)
      IG(3,8) = KG(2)
      IG(4,8) = JG(3)

      IG(1,9) = JG(3)
      IG(2,9) = JG(1)
      IG(3,9) = JG(2)
      IG(4,9) = KG(1)

      IG(1,10) = KG(1)
      IG(2,10) = JG(1)
      IG(3,10) = KG(3)
      IG(4,10) = JG(3)

      IG(1,11) = KG(1)
      IG(2,11) = KG(2)
      IG(3,11) = JG(2)
      IG(4,11) = JG(3)

      END
