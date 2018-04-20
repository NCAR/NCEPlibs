C$$$  SUBROUTINE DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBROUTINE:   ORDERS       A STABLE (RADIX) MULTIPURPOSE SORT ROUTINE
C   PRGMMR: WOOLLEN          ORG: NP22        DATE: 1999-06-03
C
C ABSTRACT:
C   ORDERS IS A FAST AND STABLE SORT ROUTINE SUITABLE FOR EFFICIENT,
C   MULTIPLE-PASS SORTING ON VARIABLE LENGTH CHARACTERS, INTEGERS, OR
C   REAL NUMBERS. THE ALGORITHM DERIVES FROM THE RADIX OR BUCKET SORT
C   PROCEDURE. THE FORM OF THE ORDERS SUBROUTINE IS DEFINED BY A CRAY
C   MAN PAGE. THE SORT WORKS BY COMPUTING FREQUENCY DISTRIBUTION OF THE
C   SET OF SORT KEYS AND USING THAT AS A MAP OF THE REORDERED DATA.
C   ORDERS REARRANGES INDEXES INSTEAD OF THE SORT KEYS, WHICH SIMPLIFIES
C   MULTI-PASS RECORD SORTING. THE RADIX OF THE SORT DETERMINES HOW MANY
C   "BUCKETS" THERE ARE IN THE FREQUENCY DISTRIBUTION ARRAY. THE LARGER
C   THE RADIX THE MORE BUCKETS. THE SIMPLEST IS A ONE BIT RADIX, WHICH
C   HAS TWO BUCKETS, AND REQUIRES AS MANY PASSES THROUGH THE KEYS AS
C   THE KEYS HAVE BITS. A ONE BYTE RADIX REQUIRES LESS PASSES THROUGH
C   THE DATA WITH MORE BUCKETS (256 TO BE EXACT). THE ONE BYTE RADIX
C   IS IMPLEMENTED HERE. AN ADDITIONAL COMPLICATION IS THE FACT THAT
C   RADIX SORT ONLY WORKS ON KEY SETS OF POSITIVE VALUES, SO THIS
C   IMPLEMENTATION INCLUDES A BIASING OF THE (NUMERIC) KEYS BEFORE
C   SORTING.  TO SAVE SPACE THE KEYS THEMSELVES ARE ADJUSTED AND THEN
C   READJUSTED BEFORE RETURNING. A SIMPLE EXAMPLE OF A ONE BIT RADIX
C   SORT ON A LIST OF FOUR, FOUR BIT, NUMBERS IS DIAGRAMED BELOW TO
C   ILLUSTRATE THE CONCEPT.
C
C-----------------------------------------------------------------------
C                 PASS1  >  PASS2  >  PASS3  >  PASS4  >   FINISHED
C-----------------------------------------------------------------------
C                     |        |        |        |
C    THE LIST      0011      0100      0100      1001      0011
C                  0101      0011      0101      0011      0100
C                  1001      0101      1001      0100      0101
C                  0100      1001      0011      0101      1001
C-----------------------------------------------------------------------
C    BUCKET 0      0100      0100      1001      0011
C                     |      0101      0011      0100
C                     |      1001       |        0101
C-----------------------------------------------------------------------
C    BUCKET 1      0011      0011      0100      1001
C                  0101        |       0101      |
C                  1001        |        |        |
C-----------------------------------------------------------------------
C
C PROGRAM HISTORY LOG:
C 1998-02-21  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 1998-04-11  B. VUONG    REPLACED OPERAND .AND. WITH INTRINSIC IAND
C 1999-06-03  D. KEYSER   MODIFIED TO PORT TO IBM SP AND RUN IN 4 OR
C                         8 BYTE STORAGE
C 1999-06-09  J. WOOLLEN  ADDED POTENTIAL FOR FOUR OR EIGHT BYTE KEYS
C                         IN EITHER A FOUR OR EIGHT BYTE ENVIRONMENT
C 2012-09-16  J. WOOLLEN  made sorting characters work on little endian 
C
C USAGE: CALL ORDERS(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
C
C INPUT ARGUMENTS:
C   IN    - INDICATOR OF KEY FORM AND INDEX STATE
C           IN =  0  INITIALIZE INDEXES AND SORT CHARACTERS
C           IN =  1  INITIALIZE INDEXES AND SORT INTEGERS
C           IN =  2  INITIALIZE INDEXES AND SORT REAL NUMBERS
C           IN = 10  SORT CHARACTERS WITH INDEXES AS IS
C           IN = 11  SORT INTEGERS WITH INDEXES AS IS
C           IN = 12  SORT REAL NUMBERS WITH INDEXES ASIS
C   ISORT - WORK ARRAY WITH THE SAME DIMENSION AS IDATA
C   IDATA - ARRAY OF SORT KEYS AS DESCRIBED BY IN
C   INDEX - ARRAY OF INDEXES REPRESENTING THE SORTED IDATA
C   N     - DIMENSION OF ISORT, IDATA, AND INDEX
C   M     - OFFSET (IN KEY-WORDS) BETWEEN SUCCESSIVE MEMBERS OF IDATA
C   I1    - BYTE LENGTH OF THE KEY-WORDS
C   I2    - NOT USED; INCLUDED FOR COMPATABILITY WITH ORIGINAL CRAY
C           ROUTINE
C
C OUTPUT ARGUMENTS:
C   INDEX - ARRAY OF INDEXES REPRESENTING THE SORTED 'IDATA'
C
C SUBPROGRAMS CALLED:
C   UNIQUE:    - NONE
C   LIBRARY:   - NONE
C
C REMARKS:
C   THE ONE BYTE RADIX METHOD WAS SELECTED FOR ORDERS BECAUSE IT
C   OFFERS A GOOD RATIO OF MEMORY REQUIREMENT TO OPERATION COUNT
C   FOR PRODUCING A SORT. BECAUSE OF RECURSIVE MANIPULATION OF INDEXES
C   IN ONE OF THE LOOPS, THIS MAY ACTUALLY TAKE SLIGHTLY LONGER ON SOME
C   VECTOR MACHINES THAN A (MORE WORK INTENSIVE) ONE BIT RADIX METHOD.
C   IN GENERAL, THOUGH, THE ONE BYTE METHOD IS FASTER. ANY LARGER RADIX
C   PRESENTS EXPONENTIALLY INCREASING MEMORY REQUIRED. NOTE THAT THE
C   IMPLEMENTATION USES VERY LITTLE LOCAL DATA SPACE, AND ONLY MODEST
C   USER-SUPPLIED MEMORY.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  INDEPENDENT
C
C$$$
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE ORDERS(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
 
      DIMENSION   ISORT(N),INDEX(N)
      INTEGER(8)  IDATA(M,N),ICHEK,IBYT
      REAL(8)     SMAL,RCHEK
      DIMENSION   INDX(0:255),KNDX(0:255)
      EQUIVALENCE (ICHEK,RCHEK)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  DISCERN THE VARIABLE TYPE OF THE INPUT ARRAY, AND MAYBE SET INDEXES
C  -------------------------------------------------------------------
 
      ITYPE = MOD(IN,10)
      IF(IN.LT.10) THEN
         DO I=1,N
         INDEX(I) = I
         ENDDO
      ENDIF
 
c  call different branches for different types of keys
c  ---------------------------------------------------

      IF(I1.EQ.4) THEN
         if(itype==0) CALL ORDEC4(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
         if(itype/=0) CALL ORDER4(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
         RETURN
      ELSEIF(I1.EQ.8) then
         IF(ITYPE==0) CALL ORDEC8(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
         IF(ITYPE==0) RETURN 
      ELSEIF(I1.NE.8) THEN
         PRINT*,'ORDERS argument i1 (keyword size) can be 4 or 8'
         PRINT*,'ORDERS argument i1 here=',i1 
         CALL ERREXIT(99_4) 
      ENDIF

C  COMPUTE A POSITIVE BIAS FOR INTEGER OR REAL NUMBERS
C  ---------------------------------------------------
 
      IF(ITYPE.GT.0) THEN
         SMAL = 1
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1 .AND. ICHEK.LT.SMAL) SMAL = ICHEK
         IF(ITYPE.EQ.2 .AND. RCHEK.LT.SMAL) SMAL = RCHEK
         ENDDO
         SMAL = 1-SMAL
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1) ICHEK = ICHEK+SMAL
         IF(ITYPE.EQ.2) RCHEK = RCHEK+SMAL
         IDATA(1,I) = ICHEK
         ENDDO
      ENDIF
 
C  SORT THE INPUT SET W/1BYTE RADIX - REARRANGE SORT LIST INDEXES ONLY
C  -------------------------------------------------------------------
 
      DO IBYT=0,I1-1
 
      KNDX(0) = 1
      DO I=0,255
      INDX(I) = 0
      ENDDO
 
      DO I=1,N
      JBYT = IAND(ISHFT(IDATA(1,INDEX(I)),-IBYT*8_8),255_8)
      INDX(JBYT) = INDX(JBYT)+1
      ISORT(I) = INDEX(I)
      ENDDO
 
      DO I=1,255
      KNDX(I) = KNDX(I-1)+INDX(I-1)
      ENDDO
 
      DO I=1,N
      JBYT = IAND(ISHFT(IDATA(1,ISORT(I)),-IBYT*8_8),255_8)
      INDEX(KNDX(JBYT)) = ISORT(I)
      KNDX(JBYT) = KNDX(JBYT)+1
      ENDDO
 
      ENDDO
 
C  UNBIAS THE INPUT ARRAY ON THE WAY OUT
C  -------------------------------------
 
      IF(ITYPE.GT.0) THEN
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1) ICHEK = ICHEK-SMAL
         IF(ITYPE.EQ.2) RCHEK = RCHEK-SMAL
         IDATA(1,I) = ICHEK
         ENDDO
      ENDIF
 
C  FINISHED!
C  ---------
 
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE ORDER4(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
 
      DIMENSION   ISORT(N),INDEX(N)
      INTEGER(4)  IDATA(M,N),ICHEK,IBYT
      REAL(4)     SMAL,RCHEK
      DIMENSION   INDX(0:255),KNDX(0:255)
      EQUIVALENCE (ICHEK,RCHEK)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  DISCERN THE VARIABLE TYPE OF THE INPUT ARRAY, AND MAYBE SET INDEXES
C  -------------------------------------------------------------------
 
      ITYPE = MOD(IN,10)
      IF(IN.LT.10) THEN
         DO I=1,N
         INDEX(I) = I
         ENDDO
      ENDIF
 
C  COMPUTE A POSITIVE BIAS FOR INTEGER OR REAL NUMBERS
C  ---------------------------------------------------
 
      IF(ITYPE.GT.0) THEN
         SMAL = 1
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1 .AND. ICHEK.LT.SMAL) SMAL = ICHEK
         IF(ITYPE.EQ.2 .AND. RCHEK.LT.SMAL) SMAL = RCHEK
         ENDDO
         SMAL = 1-SMAL
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1) ICHEK = ICHEK+SMAL
         IF(ITYPE.EQ.2) RCHEK = RCHEK+SMAL
         IDATA(1,I) = ICHEK
         ENDDO
      ENDIF
 
C  SORT THE INPUT SET W/1BYTE RADIX - REARRANGE SORT LIST INDEXES ONLY
C  -------------------------------------------------------------------
 
      DO IBYT=0,I1-1
 
      KNDX(0) = 1
      DO I=0,255
      INDX(I) = 0
      ENDDO
 
      DO I=1,N
      JBYT = IAND(ISHFT(IDATA(1,INDEX(I)),-IBYT*8_4),255_4)
      INDX(JBYT) = INDX(JBYT)+1
      ISORT(I) = INDEX(I)
      ENDDO
 
      DO I=1,255
      KNDX(I) = KNDX(I-1)+INDX(I-1)
      ENDDO
 
      DO I=1,N
      JBYT = IAND(ISHFT(IDATA(1,ISORT(I)),-IBYT*8_4),255_4)
      INDEX(KNDX(JBYT)) = ISORT(I)
      KNDX(JBYT) = KNDX(JBYT)+1
      ENDDO
 
      ENDDO
 
C  UNBIAS THE INPUT ARRAY ON THE WAY OUT
C  -------------------------------------
 
      IF(ITYPE.GT.0) THEN
         DO I=1,N
         ICHEK = IDATA(1,I)
         IF(ITYPE.EQ.1) ICHEK = ICHEK-SMAL
         IF(ITYPE.EQ.2) RCHEK = RCHEK-SMAL
         IDATA(1,I) = ICHEK
         ENDDO
      ENDIF
 
C  FINISHED!
C  ---------
 
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE ORDEC8(IN,ISORT,IDATA,INDEX,N,M,I1,I2)
 
      DIMENSION    ISORT(N),INDEX(N)
      character(8) IDATA(M,N)
      DIMENSION    INDX(0:255),KNDX(0:255)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  DISCERN THE VARIABLE TYPE OF THE INPUT ARRAY, AND MAYBE SET INDEXES
C  -------------------------------------------------------------------
 
      ITYPE = MOD(IN,10)
      IF(IN.LT.10) THEN
         DO I=1,N
         INDEX(I) = I
         ENDDO
      ENDIF
 
C  SORT THE INPUT SET W/1BYTE RADIX - REARRANGE SORT LIST INDEXES ONLY
C  -------------------------------------------------------------------
 
      DO IBYT=0,I1-1
 
      KNDX(0) = 1
      DO I=0,255
      INDX(I) = 0
      ENDDO

      II=I1-IBYT
 
      DO I=1,N
      JBYT = ICHAR(IDATA(1,INDEX(I))(II:II))
      INDX(JBYT) = INDX(JBYT)+1
      ISORT(I) = INDEX(I)
      ENDDO
 
      DO I=1,255
      KNDX(I) = KNDX(I-1)+INDX(I-1)
      ENDDO
 
      DO I=1,N
      JBYT = ICHAR(IDATA(1,isort(I))(II:II))
      INDEX(KNDX(JBYT)) = ISORT(I)
      KNDX(JBYT) = KNDX(JBYT)+1
      ENDDO
 
      ENDDO
 
C  FINISHED!
C  ---------
 
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE ORDEC4(IN,ISORT,IDATA,INDEX,N,M,I1,I2)

      DIMENSION    ISORT(N),INDEX(N)
      character(4) IDATA(M,N)
      DIMENSION    INDX(0:255),KNDX(0:255)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  DISCERN THE VARIABLE TYPE OF THE INPUT ARRAY, AND MAYBE SET INDEXES
C  -------------------------------------------------------------------

      ITYPE = MOD(IN,10)
      IF(IN.LT.10) THEN
         DO I=1,N
         INDEX(I) = I
         ENDDO
      ENDIF

C  SORT THE INPUT SET W/1BYTE RADIX - REARRANGE SORT LIST INDEXES ONLY
C  -------------------------------------------------------------------

      DO IBYT=0,I1-1

      KNDX(0) = 1
      DO I=0,255
      INDX(I) = 0
      ENDDO

      II=I1-IBYT

      DO I=1,N
      JBYT = ICHAR(IDATA(1,INDEX(I))(II:II))
      INDX(JBYT) = INDX(JBYT)+1
      ISORT(I) = INDEX(I)
      ENDDO

      DO I=1,255
      KNDX(I) = KNDX(I-1)+INDX(I-1)
      ENDDO

      DO I=1,N
      JBYT = ICHAR(IDATA(1,isort(I))(II:II))
      INDEX(KNDX(JBYT)) = ISORT(I)
      KNDX(JBYT) = KNDX(JBYT)+1
      ENDDO

      ENDDO

C  FINISHED!
C  ---------

      RETURN
      END

