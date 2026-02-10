       IDENTIFICATION DIVISION.
           PROGRAM-ID. ALUNOS.
           AUTHOR. Alexandre S S Alves.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ALUNOS ASSIGN TO "ALUNOS.DATA"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ALUNOS.
       01 REGISTRO-ALUNO.
           05 NOME-ALUNO PIC X(20).
           05 NOTA-1     PIC 9(3)V9(2).
           05 NOTA-2     PIC 9(3)V9(2).
           05 MEDIA      PIC 9(3)V9(2).
           05 RESULTADO  PIC X(20).

       WORKING-STORAGE SECTION.
       01 WS-NOME PIC X(20).
       01 WS-NOTA1          PIC 9(3)V9(2).
       01 WS-NOTA2          PIC 9(3)V9(2).
       01 WS-MEDIA          PIC 9(3)V9(2).
       01 WS-RESULTADO      PIC X(20).
       01 OPCAO PIC X VALUE SPACE.

       PROCEDURE DIVISION.
       PRINCIPAL.
           DISPLAY "=============================".
           DISPLAY "     Calculo de Media".
           DISPLAY "=============================".

           OPEN OUTPUT ARQUIVO-ALUNOS.
           PERFORM ATE-FINALIZAR.
           CLOSE ARQUIVO-ALUNOS.

           DISPLAY "====================================".
           DISPLAY " Processo finalizado Arquivo criado".
           DISPLAY "====================================".
           
           STOP RUN.

       NOTA-ALUNO.
           DISPLAY "Entre com a sua Nota 1: ".
           ACCEPT WS-NOTA1.
           
           DISPLAY "Entre com a sua Nota 2: ".
           ACCEPT WS-NOTA2.
       
       NOME-ALUNO-WS.
           DISPLAY "Entre com seu Nome: ".
           ACCEPT WS-NOME.

       CALCULAR-MEDIA.
           COMPUTE WS-MEDIA =  (WS-NOTA1 + WS-NOTA2) / 2.
           IF WS-MEDIA >= 6
               DISPLAY "PASSOU"
               MOVE "PASSOU" TO WS-RESULTADO
           ELSE
               DISPLAY "REPROVOU"
               MOVE "REPROVOU" TO WS-RESULTADO
           END-IF.

       EXIBIR-RESULTADO.
           DISPLAY "=======================================".
           DISPLAY "  Nome Aluno: " WS-NOME.
           DISPLAY "  Resultado: "  PERFORM CALCULAR-MEDIA.
           DISPLAY "  Media Final " WS-MEDIA.
           DISPLAY "=======================================".
           
       ATE-FINALIZAR.
           PERFORM UNTIL OPCAO = "N" OR OPCAO = "n"    
               PERFORM SOLICITAR-DADOS                              
               PERFORM EXIBIR-RESULTADO
               PERFORM GRAVAR-ARQUIVO
               DISPLAY "Deseja cadastrar outro aluno? (S/N): " WITH NO
               ADVANCING ACCEPT OPCAO
           END-PERFORM.

       SOLICITAR-DADOS.
           PERFORM NOME-ALUNO-WS.
           PERFORM NOTA-ALUNO.
       
       GRAVAR-ARQUIVO.
           MOVE WS-NOME TO NOME-ALUNO.
           MOVE WS-NOTA1 TO NOTA-1.
           MOVE WS-NOTA2 TO NOTA-2.
           MOVE WS-MEDIA TO MEDIA.
           MOVE WS-RESULTADO TO RESULTADO.
           WRITE REGISTRO-ALUNO.
