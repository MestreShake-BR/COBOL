       IDENTIFICATION DIVISION.
           PROGRAM-ID. NUMEROSPRIMOS.
           AUTHOR. Alexandre S S Alves.
           
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 LIMITE            PIC 9(5).
       01 NUM               PIC 9(5).
       01 DIVISOR           PIC 9(5).
       01 RESTO             PIC 9(5).
       01 RAIZ              PIC 9(5).
       01 EH-PRIMO          PIC X VALUE "S".

       PROCEDURE DIVISION.
       PRINCIPAL.

           DISPLAY "=================================".
           DISPLAY "     CALCULADORA DE PRIMOS".
           DISPLAY "=================================".
           DISPLAY "Digite o limite: ".
           ACCEPT LIMITE.

           IF LIMITE < 2
               DISPLAY "Nao existem numeros primos menores que 2."
               STOP RUN
           END-IF.

           PERFORM VARYING NUM FROM 2 BY 1 UNTIL NUM > LIMITE

               MOVE "S" TO EH-PRIMO

      * Testa divisores apenas até a raiz de NUM
               COMPUTE RAIZ = FUNCTION INTEGER(FUNCTION SQRT(NUM))

               PERFORM VARYING DIVISOR FROM 2 BY 1
                   UNTIL DIVISOR > RAIZ OR EH-PRIMO = "N"
                   COMPUTE RESTO = FUNCTION MOD(NUM DIVISOR)
                   IF RESTO = 0
                       MOVE "N" TO EH-PRIMO
                   END-IF
               END-PERFORM

      * Só exibe se for primo
               IF EH-PRIMO = "S"
                   DISPLAY NUM " E PRIMO"
               END-IF

           END-PERFORM.

           STOP RUN.
