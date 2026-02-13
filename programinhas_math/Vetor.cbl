       IDENTIFICATION DIVISION.
           PROGRAM-ID. VETOR.
           AUTHOR. Alexandre S S Alves.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 X         PIC S9(5)V99.
       01 Y         PIC S9(5)V99.
       01 MODULO    PIC S9(7)V99.
       01 ANGULO    PIC S9(7)V99.
       01 I         PIC 9(3).
       01 POS       PIC 9(3).
       01 ESPACOS   PIC X(80).  *> Linha de até 80 espaços
       01 LINHA     PIC X(100).

       PROCEDURE DIVISION.
       PRINCIPAL.

           DISPLAY "=================================".
           DISPLAY "      CALCULO DE VETOR 2D".
           DISPLAY "=================================".

           DISPLAY "Digite a coordenada X: ".
           ACCEPT X.

           DISPLAY "Digite a coordenada Y: ".
           ACCEPT Y.

      * Calcula modulo
           COMPUTE MODULO = FUNCTION SQRT(X * X + Y * Y).

      * Calcula angulo em graus (aprox)
           IF X = 0
               IF Y > 0
                   MOVE 90 TO ANGULO
               ELSE
                   MOVE 270 TO ANGULO
               END-IF
           ELSE
               COMPUTE ANGULO = FUNCTION ATAN(Y / X) * 180 / 3.1416
               IF X < 0
                   ADD 180 TO ANGULO
               END-IF
           END-IF.

           DISPLAY "Modulo do vetor: " MODULO.
           DISPLAY "Angulo do vetor (graus): " ANGULO.

      * Desenho ASCII do vetor
           DISPLAY "Curva do vetor aproximada:".

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MODULO
      * Calcula posicao horizontal do asterisco
               COMPUTE POS = FUNCTION INTEGER(I * X / MODULO)

      * Garante que POS nao ultrapasse 79
               IF POS > 79
                   MOVE 79 TO POS
               END-IF
               IF POS < 0
                   MOVE 0 TO POS
               END-IF

      * Gera linha com espaços
               MOVE SPACES TO ESPACOS
               MOVE "*" TO LINHA(POS + 1:1)  *> Coloca * na posicao correta
               DISPLAY LINHA
           END-PERFORM.

           STOP RUN.
