      SUBROUTINE W3AI18(ITEM,I1,I2,LINE,L,K,N)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    W3AI18      LINE BUILDER SUBROUTINE
C   PRGMMR: ALLARD, R.       ORG: W/NMC42    DATE: 74-02-01
C
C ABSTRACT: BUILD A LINE OF INFORMATION COMPOSED OF USER SPECIFIED
C   CHARACTER STRINGS.
C
C PROGRAM HISTORY LOG:
C   74-02-02  ROBERT ALLARD
C   84-07-05  R.E.JONES     RECOMPILE
C   96-08-06  R.E.JONES     CONVERT FROM IBM370 ASSEMBLER TO FORTRAN
C                           FOR THE CRAY, WORKSTATIONS, AND PC'S.
C
C USAGE:    CALL W3AI18(ITEM, I1, I2, LINE, L, K, N)
C   INPUT ARGUMENT LIST:
C     ITEM   - CHARACTER STRING TO BE ADDED TO LINE ARRAY
C     I1     - NUMBER OF CHARACTER STRINGS TO BE ADDED TO LINE ARRAY
C     I2     - NUMBER OF CHARACTERS PER STRING TO ADD TO LINE
C     L      - CHARACTER LENGTH OF LINE TO BE BUILT (2.LE.L.LE.256)
C     K      - NUMBER OF BLKANK CHARACTERS TO PRECEDE A CHARACTER
C              STRING (0.LE.K.LE.256)
C     N      - POINTER SET EQUAL TO 0 WHEN BEGINNING A LINE
C
C   OUTPUT ARGUMENT LIST:     
C     LINE   - ARRAY IN WHICH CHARACTER STRING ARE PLACED WHILE
C              BUILDING ALINE; MUST BE OF TYPE INTEGER
C     N      - CHARACTER COUNT, ERROR INDICATOR
C
C
C   EXIT STATES:
C     N = -1    CHARACTER STRING WILL NOT FIT IN THE LINE ARRAY;
C               OTHERWISE, EACH TIME A CHACTER STRING IS ADDED
C               TO THE LINE, N IS INCREMENTED BY (I2 + K)
C
C NOTE 1. - EACH CHARACTER STRING INCLUDED IN THE ITEM ARRAY MUST 
C           START ON A FULL WORD BOUNDARY AND BE EQUAL IN LENGTH. 
C           EACH SUCCESSIVE STRING MUST START ON THE NEST FULLWORD
C           BOUNDARY FOLLOWING THE END OF THE PREVIOUS STRING.
C           ON A CRAY THIS 8.
C
C NOTE 2. - THE DIMENSIONS OF THE ITEM ARRAY SHOULD BE AT LEAST THE
C           VALUE OF (I1*(I2+J))/4, WHERE THE INTEGER J IS IN THE
C           RANGE 0.LE.J.LE.3 AND THE SUM (I2+J) IS 4 OR A MULTIPLE
C           OF 4. ON A CRAY THIS IS 8 OR A MULTIPLE OF 8. ON A CRAY
C           (I1*(I2+J))/8, RANGE IS 0.LE.J.LE.7
C
C NOTE 3. - THE MAXIMUM DIMENSION OF LINE IS 64 WORD OR 256 BYTES.
C           ON A CRAY IT IS 32 WORDS OR 256 BYTES.
C
C NOTE 4. - THE USER SHOULD SET N = 0 EACH TIME A LINE IS STATED TO
C           TELL W3AI18 TO FILL THE LINE ARRAY WITH BLANK CHARACTERS.
C           EACH TIME A CHARACTER STRING IS ADDED TO THE LINE, THE 
C           VARIABLE (N) IS INCREMENTED BY (I2 + K). IF A CHARACTER
C           STRING WILL NOT FIT IN THE LINE ARRAY, W3AI18 SETS N = -1
C           AND RETURNS TO THE USER. THE USER WILL NOT BE ABLE TO 
C           PROGRAM A RECOVERY PROCEDURE FOR THE LINE BEING FULL IF
C           MORE THAN ONE CHARACTER STRING IS IN THE ITEM ARRAY.
C           
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY C916/256, J916/2048.
C
C$$$
C
      CHARACTER * (*) LINE
      CHARACTER * (*) ITEM
C
      SAVE
C
C     TEST WORD LENGTH, LW WILL BE 4 OR 8 BYTES 
C
      CALL W3FI01(LW)
C
C     BAIL OUT IF NEGATIVE
C 
      IF (N.LT.0) RETURN
C
C     FILL LINE WITH BLANK CHAACTERS
C
      IF (N.EQ.0) THEN
        DO I = 1,L
          LINE(I:I) = ' '
        END DO
      END IF
      IF (I1.EQ.1) THEN 
        J = 0
        IF ((I2+K+N).GT.L) GO TO 200
          LINE(K+N+1:K+N+I2) = ITEM(1:I2)
          N = I2+K+N        
          RETURN
      ELSE
        JJ = MOD(I2, LW)
        IF (JJ.EQ.0) THEN
          J = 0
        ELSE       
          J = LW - JJ
        END IF
        IF ((I2+K+N).GT.L) GO TO 200
          LINE(K+N+1:K+N+I2) = ITEM(1:I2)
          N = I2+K+N
          DO I = 1,I1-1
            IF ((I2+K+N).GT.L) GO TO 200
            LINE(K+N+1:K+N+I2) = ITEM((I2+J)*I+1:(I2+J)*I+I2)
            N = I2+K+N
          END DO              
          RETURN    
      END IF
 200  CONTINUE
        N = -1
        RETURN         
      END
