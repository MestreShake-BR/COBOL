       IDENTIFICATION DIVISION.
           PROGRAM-ID. BHASKARAouQUADRATICA.
           AUTHOR. Alexandre S S Alves.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 A         PIC S9(5)V99.
       01 B         PIC S9(5)V99.
       01 C         PIC S9(5)V99.
       01 DELTA     PIC S9(7)V99.
       01 RAIZ-REAL PIC S9(7)V99.
       01 RAIZ-IMAG PIC S9(7)V99.

       PROCEDURE DIVISION.
       PRINCIPAL.

           DISPLAY "=================================".
           DISPLAY "       FORMULA DE BHASKARA".
           DISPLAY "       Forma: ax^2 + bx + c = 0".
           DISPLAY "=================================".

           DISPLAY "Digite o valor de a: ".
           ACCEPT A.

           IF A = 0
               DISPLAY "Nao e equacao de segundo grau (a = 0)."
               STOP RUN
           END-IF.

           DISPLAY "Digite o valor de b: ".
           ACCEPT B.

           DISPLAY "Digite o valor de c: ".
           ACCEPT C.

           COMPUTE DELTA = B * B - 4 * A * C.

           IF DELTA > 0
      * Duas raizes reais distintas
               COMPUTE RAIZ-REAL = (-B + FUNCTION SQRT(DELTA)) / (2 * A)
               DISPLAY "X1 = " RAIZ-REAL
               COMPUTE RAIZ-REAL = (-B - FUNCTION SQRT(DELTA)) / (2 * A)
               DISPLAY "X2 = " RAIZ-REAL
           ELSE
               IF DELTA = 0
      * Raiz real unica
                   COMPUTE RAIZ-REAL = -B / (2 * A)
                   DISPLAY "Raiz unica: X = " RAIZ-REAL
               ELSE
      * Raizes complexas
                   COMPUTE RAIZ-REAL = -B / (2 * A)
                   COMPUTE RAIZ-IMAG = FUNCTION SQRT(-DELTA) / (2 * A)
                   DISPLAY "Raizes complexas:"
                   DISPLAY "X1 = " RAIZ-REAL " + " RAIZ-IMAG "i"
                   DISPLAY "X2 = " RAIZ-REAL " - " RAIZ-IMAG "i"
               END-IF
           END-IF.

           STOP RUN.
