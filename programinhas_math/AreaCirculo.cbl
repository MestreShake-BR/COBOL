       IDENTIFICATION DIVISION.
           PROGRAM-ID. SIRCUNFERENCIA.
           AUTHOR. Alexandre S S Alves.
           
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 RAIO           PIC 9(5)V99.
       01 CIRC           PIC 9(7)V99.
       01 PI             PIC 9V99 VALUE 3.14.

       PROCEDURE DIVISION.
       PRINCIPAL.

           DISPLAY "=================================".
           DISPLAY "  CALCULADORA DE CIRCUNFERENCIA".
           DISPLAY "=================================".
           DISPLAY "Digite o raio do circulo: ".
           ACCEPT RAIO.

           COMPUTE CIRC = 2 * PI * RAIO.

           DISPLAY "A circunferencia do circulo eh: " CIRC.

           STOP RUN.
