      SUBROUTINE SPINIT

      INCLUDE 'crndm.inc'

      REAL POL(4)


      IF(ISEED.NE.0) THEN

         PRINT *,'RM48 initialized: ',ISEED

         NRNDM1 = 0
         NRNDM2 = 0

         CALL RM48IN(ISEED,NRNDM1,NRNDM2)

      END IF

C     Initialize TAUOLA

C      CALL INITDK
C      CALL DEXAY(-1,POL)

      RETURN
      END
