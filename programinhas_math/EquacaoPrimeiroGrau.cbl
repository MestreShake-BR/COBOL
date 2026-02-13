       IDENTIFICATION DIVISION.
           PROGRAM-ID. EQUACAO1GRAU.
           AUTHOR. Alexandre S S Alves.
           
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 A        PIC S9(5)V99.
       01 B        PIC S9(5)V99.
       01 X        PIC S9(7)V99.

       PROCEDURE DIVISION.
       PRINCIPAL.

           DISPLAY "=================================".
           DISPLAY "   RESOLVEDOR DE EQUACAO 1o GRAU".
           DISPLAY "   Forma: ax + b = 0".
           DISPLAY "=================================".

           DISPLAY "Digite o valor de a: ".
           ACCEPT A.

           IF A = 0
               DISPLAY "Nao eh uma equacao de primeiro grau (a = 0)."
               STOP RUN
           END-IF.

           DISPLAY "Digite o valor de b: ".
           ACCEPT B.

           COMPUTE X = -B / A.

           DISPLAY "A solucao da equacao eh: X = " X.

           STOP RUN.
