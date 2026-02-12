       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSV.
       AUTHOR. Alexandre S S Alves.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-CSV ASSIGN TO "entrada.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-CSV.
       01 REGISTRO-CSV PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-FIM-DO-ARQUIVO PIC X(01) VALUE 'N'.
       01 WS-REGISTRO        PIC 9(05) VALUE ZEROS.
       01 WS-DADO.
           05 WS-CAMPO PIC X(50) OCCURS 6 TIMES.
       01 WS-I PIC 9(01) VALUE 1.

       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN INPUT ARQUIVO-CSV
           PERFORM LER-CABECALHO
           PERFORM PROCESSAR-REGISTROS UNTIL WS-FIM-DO-ARQUIVO = 'S'
           CLOSE ARQUIVO-CSV
           STOP RUN.

       LER-CABECALHO.
           READ ARQUIVO-CSV INTO REGISTRO-CSV
               NOT AT END
                   PERFORM SEPARAR-LINHA
                   DISPLAY "=== Cabecalho ==="
                   PERFORM EXIBIR-CAMPOS
           END-READ.

       PROCESSAR-REGISTROS.
           READ ARQUIVO-CSV INTO REGISTRO-CSV
               AT END 
                   MOVE 'S' TO WS-FIM-DO-ARQUIVO
               NOT AT END 
                   ADD 1 TO WS-REGISTRO
                   PERFORM SEPARAR-LINHA
                   DISPLAY "=== Registro #" WS-REGISTRO " ==="
                   PERFORM EXIBIR-CAMPOS
               END-READ.

       SEPARAR-LINHA.
           MOVE SPACES TO WS-DADO  *> Limpa todos os campos antes de preencher
           UNSTRING REGISTRO-CSV DELIMITED BY ';'
               INTO WS-CAMPO(1)
                    WS-CAMPO(2)
                    WS-CAMPO(3)
                    WS-CAMPO(4)
                    WS-CAMPO(5)
                    WS-CAMPO(6)
           END-UNSTRING.

       EXIBIR-CAMPOS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               DISPLAY "Campo " WS-I ": " WS-CAMPO(WS-I)
           END-PERFORM.
