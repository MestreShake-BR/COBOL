       IDENTIFICATION DIVISION.
           PROGRAM-ID. PROFINDEXADO.
           AUTHOR. Alexandre S S Alves.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-PROFESSOR ASSIGN TO "professores.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRO-MATRICULA
               FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-PROFESSOR.
       01 REG-PROFESSOR.
           05 PRO-MATRICULA PIC X(8).
           05 PRO-NOME PIC X(30).
           
       WORKING-STORAGE SECTION.
       01 WS-OPCAO PIC 9.
       01 WS-STATUS PIC X(2).

       01 WS-PROFESSOR.
           05 WS-MATRICULA PIC X(8).
           05 WS-NOME PIC X(30).

       PROCEDURE DIVISION.
       PRINCIPAL.
           DISPLAY "--------------------------".
           DISPLAY " Sistema de Professores   ".
           DISPLAY "--------------------------".
           DISPLAY "1 -  Inserir Professor    ".
           DISPLAY "2 -  Buscar Professor     ".
           DISPLAY "3 -  Modificar Professor  ".  
           DISPLAY "4 -  Excluir Professor    ".           
           DISPLAY "5 -  Sair                 ".
           DISPLAY "--------------------------".
           DISPLAY "Escolha uma opcao:        ".
           ACCEPT WS-OPCAO.

           EVALUATE WS-OPCAO
               WHEN 1
                   PERFORM CADASTRAR
               WHEN 2
                   PERFORM MOSTRAR
               WHEN 3 
                   PERFORM MODIFICAR
               WHEN 4 
                   PERFORM EXCLUIR
               WHEN 5 
                   STOP RUN
               WHEN OTHER
                DISPLAY "Opicao invalida!"
                PERFORM PRINCIPAL
           END-EVALUATE.

       ABRIR-ARQUIVO.
           OPEN I-O ARQUIVO-PROFESSOR.
           IF WS-STATUS = "35"
               OPEN OUTPUT ARQUIVO-PROFESSOR
               CLOSE ARQUIVO-PROFESSOR
               OPEN I-O ARQUIVO-PROFESSOR.

       FECHAR-ARQUIVO.
           CLOSE ARQUIVO-PROFESSOR.

       CADASTRAR.
           DISPLAY "----------------------------".
           DISPLAY "  Cadastrar de Professores  ".
           DISPLAY "----------------------------".
           DISPLAY "Matricula: ".
           ACCEPT WS-MATRICULA.
           DISPLAY "Nome:  ".
           ACCEPT WS-NOME.

           MOVE WS-MATRICULA TO PRO-MATRICULA.
           MOVE WS-NOME TO PRO-NOME.
           PERFORM ABRIR-ARQUIVO.
           WRITE REG-PROFESSOR
               INVALID KEY 
                   DISPLAY "Erro: Matricula ja existe!".
           PERFORM  FECHAR-ARQUIVO.
           PERFORM PRINCIPAL.

       MOSTRAR.
           DISPLAY "--------------------------".
           DISPLAY " Consulta de Professores  ".
           DISPLAY "--------------------------".
           PERFORM ABRIR-ARQUIVO.
           MOVE "00" TO WS-STATUS.

           PERFORM UNTIL WS-STATUS = "10"
               READ ARQUIVO-PROFESSOR NEXT RECORD 
                   AT END 
                       MOVE "10" TO  WS-STATUS
                   NOT AT END
                       DISPLAY "Matricula: " PRO-MATRICULA
                       DISPLAY "Nome: " PRO-NOME
                       DISPLAY "--------------------------"
               END-READ
           END-PERFORM

           PERFORM FECHAR-ARQUIVO.
           PERFORM PRINCIPAL.
       MODIFICAR.
           DISPLAY "--------------------------".
           DISPLAY "  Modificar Professores   ".
           DISPLAY "--------------------------".

           DISPLAY "Informe a Matricula: ".
           ACCEPT WS-MATRICULA.

           PERFORM ABRIR-ARQUIVO
           MOVE WS-MATRICULA TO PRO-MATRICULA.
           READ ARQUIVO-PROFESSOR KEY IS PRO-MATRICULA
               INVALID KEY 
                   DISPLAY "Erro: Matricula nao encontrada!"
               NOT INVALID KEY
                   DISPLAY "Novo nome"
                   ACCEPT WS-NOME
                   MOVE WS-NOME TO PRO-NOME
                   REWRITE  REG-PROFESSOR
                       DISPLAY "Registro Atualizado"

           PERFORM  FECHAR-ARQUIVO.
           PERFORM PRINCIPAL.
           

           
       EXCLUIR.
           DISPLAY "--------------------------".
           DISPLAY "    Excluir Professores   ".
           DISPLAY "--------------------------".
           
           DISPLAY "Informe a Matricula: ".
           ACCEPT WS-MATRICULA.

           PERFORM ABRIR-ARQUIVO
           MOVE WS-MATRICULA TO PRO-MATRICULA.
           READ ARQUIVO-PROFESSOR KEY IS PRO-MATRICULA
               INVALID KEY 
                   DISPLAY "Erro: Matricula nao encontrada!"
               NOT INVALID KEY                  
                   DELETE  ARQUIVO-PROFESSOR
                       DISPLAY "Registro Excluido"
           PERFORM  FECHAR-ARQUIVO.
           PERFORM PRINCIPAL.
           