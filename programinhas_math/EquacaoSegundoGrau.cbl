       IDENTIFICATION DIVISION.
           PROGRAM-ID. EQUACAO2GRAU.
           AUTHOR. Alexandre S S Alves.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 A         PIC S9(5)V99.
       01 B         PIC S9(5)V99.
       01 C         PIC S9(5)V99.
       01 DELTA     PIC S9(7)V99.
       01 RAIZ1     PIC S9(7)V99.
       01 RAIZ2     PIC S9(7)V99.

       PROCEDURE DIVISION.
       PRINCIPAL.

           DISPLAY "=================================".
           DISPLAY "   RESOLVEDOR DE EQUACAO 2o GRAU".
           DISPLAY "   Forma: ax^2 + bx + c = 0".
           DISPLAY "=================================".

           DISPLAY "Digite o valor de a: ".
           ACCEPT A.

           IF A = 0
               DISPLAY "Nao eh equacao de segundo grau (a = 0)."
               STOP RUN
           END-IF.

           DISPLAY "Digite o valor de b: ".
           ACCEPT B.

           DISPLAY "Digite o valor de c: ".
           ACCEPT C.

           COMPUTE DELTA = B * B - 4 * A * C.

           IF DELTA < 0
               DISPLAY "Nao existem raizes reais."
           ELSE
               COMPUTE RAIZ1 = (-B + FUNCTION SQRT(DELTA)) / (2 * A)
               COMPUTE RAIZ2 = (-B - FUNCTION SQRT(DELTA)) / (2 * A)
               DISPLAY "As raizes da equacao sao: ".
               DISPLAY "X1 = " RAIZ1.
               DISPLAY "X2 = " RAIZ2.
           STOP RUN.
