       IDENTIFICATION DIVISION.
           PROGRAM-ID. ALUNOSRELATIVOS.
           AUTHOR. Alexandre S S Alves.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ALUNOS ASSIGN TO "alunos.dat"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS WS-CHAVE-RELATIVA.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ALUNOS.
       01 REGISTRO-ALUNO.
           05 WS-MATRICULA PIC 9(3).
           05 WS-NOME PIC X(30).
           05 WS-IDADE PIC 9(2).
           
       WORKING-STORAGE SECTION.
       01 OPCAO PIC 9.
       01 WS-CHAVE-RELATIVA PIC 9(3).
       01 WS-CHAVE-BUSCA PIC 9(3).

       PROCEDURE DIVISION.
       PRINCIPAL.
           DISPLAY "-------------------".
           DISPLAY " Sistema de Alunos ".
           DISPLAY "-------------------".
           DISPLAY "1 -  Inserir Aluno ".
           DISPLAY "2 -  Buscar Aluno  ".
           DISPLAY "3 -  Sair          ".
           ACCEPT OPCAO.

           EVALUATE OPCAO
               WHEN 1
                   PERFORM INSERIR-ALUNO
               WHEN 2 
                   PERFORM BUSCAR-ALUNO
               WHEN 3 
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opção Invalida!"
                   PERFORM PRINCIPAL
           END-EVALUATE.

       INSERIR-ALUNO.
           DISPLAY "-------------------".
           DISPLAY " Cadastro de Aluno ".
           DISPLAY "-------------------".
           DISPLAY "Matricula do Aluno ".
           ACCEPT WS-CHAVE-RELATIVA.
           MOVE WS-CHAVE-RELATIVA TO WS-MATRICULA .
           DISPLAY "Nome do Aluno".
           ACCEPT WS-NOME.
           DISPLAY "Idade do Aluno".
           ACCEPT WS-IDADE.

           OPEN I-O ARQUIVO-ALUNOS.
           WRITE REGISTRO-ALUNO INVALID KEY
               DISPLAY "Erro ao gravar registro!"
           CLOSE ARQUIVO-ALUNOS.

           DISPLAY "Aluno gravado com sucesso!"
           PERFORM PRINCIPAL.
           
       BUSCAR-ALUNO.
           DISPLAY "-------------------".
           DISPLAY "    Buscar Aluno   ".
           DISPLAY "-------------------".
           DISPLAY "Qual matricula do aluno?".
           ACCEPT WS-CHAVE-BUSCA.

           OPEN INPUT ARQUIVO-ALUNOS.
           MOVE WS-CHAVE-BUSCA TO WS-CHAVE-RELATIVA.
           READ ARQUIVO-ALUNOS INVALID KEY
               DISPLAY "Registro nao encontrado"
           NOT INVALID KEY
               DISPLAY "-------------------"
               DISPLAY "Aluno encontrado "
               DISPLAY "Matricula: " WS-MATRICULA
               DISPLAY "Nome: " WS-NOME
               DISPLAY "Idade: " WS-IDADE
               DISPLAY "-------------------"
           END-READ.
           CLOSE ARQUIVO-ALUNOS.
           PERFORM PRINCIPAL.
