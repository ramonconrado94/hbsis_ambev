       IDENTIFICATION                  DIVISION.

       PROGRAM-ID. CRUDVEND.
       AUTHOR.     RAMON CONRADO

       ENVIRONMENT                     DIVISION.

       CONFIGURATION                   SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

       INPUT-OUTPUT                    SECTION.

       FILE-CONTROL.

           SELECT ARQ-VENDEDOR         ASSIGN TO DISK "VENDEDOR.TXT"
                  ORGANIZATION         IS INDEXED
                  ACCESS MODE          IS DYNAMIC
                  RECORD KEY           IS FD-VEND-COD-VENDEDOR
                  ALTERNATE RECORD KEY IS FD-VEND-CPF
                  LOCK MODE            IS MANUAL
                  FILE STATUS          IS WS-FS-ARQ-VEND.

           SELECT ARQ-IMPORTA          ASSIGN TO DISK WS-ENDER
                  ORGANIZATION         IS LINE SEQUENTIAL
                  ACCESS MODE          IS SEQUENTIAL
                  LOCK MODE            IS MANUAL
                  FILE STATUS          IS WS-FS-ARQ-IMPORTA.

       DATA                            DIVISION.
       FILE                            SECTION.

       FD  ARQ-VENDEDOR
           LABEL RECORD                IS STANDARD.
       01  FD-VENDEDOR.
           05 FD-VEND-COD-VENDEDOR     PIC 9(003).
           05 FD-VEND-CPF              PIC 9(011).
           05 FD-VEND-NOME             PIC X(040).
           05 FD-VEND-LATITUDE         PIC S9(003)V9(008).
           05 FD-VEND-LONGITUDE        PIC S9(003)V9(008).

       FD  ARQ-IMPORTA
           LABEL RECORD                IS STANDARD.
       01  FD-IMPORTA.
           05 FD-IMP-COD-VENDEDOR      PIC 9(003).
           05 FD-IMP-CPF               PIC 9(011).
           05 FD-IMP-NOME              PIC X(040).
           05 FD-IMP-LATITUDE          PIC S9(003)V9(008).
           05 FD-IMP-LONGITUDE         PIC S9(003)V9(008).


       WORKING-STORAGE                 SECTION.

       77  WS-FS-ARQ-VEND              PIC X(002)          VALUE SPACES.
       77  WS-FS-ARQ-IMPORTA           PIC X(002)          VALUE SPACES.
       77  WS-VERIFICA                 PIC X(008)          VALUE
                                                           'VERIFICA'.

       01  WS-FIM-ARQ-IMPORTA          PIC X(001)          VALUE SPACES.
       01  WS-ENDER                    PIC X(040)          VALUE SPACES.

       01  WS-AREA-IMPORT-VEND.
           05 WS-COD-VENDEDOR          PIC 9(003)          VALUE ZEROS.
           05 WS-CPF                   PIC 9(011)          VALUE ZEROS.
           05 WS-NOME                  PIC X(040)          VALUE SPACES.
           05 WS-LATITUDE              PIC S9(003)V9(08)   VALUE ZEROS.
           05 WS-LONGITUDE             PIC S9(003)V9(08)   VALUE ZEROS.

       01  WS-LINK-VERIFICA.
           COPY 'BOOKVRIF.CPY'.

       LINKAGE                         SECTION.
       01  WS-LINKAGE-AREA.
           COPY 'BOOKVEND.CPY'.

       PROCEDURE                       DIVISION USING WS-LINKAGE-AREA.


       0000-PRINCIPAL                  SECTION.

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 3000-FINALIZAR
           .
       0000-99-FIM.                    EXIT.

       1000-INICIALIZAR                SECTION.

           PERFORM 1050-CONSISTIR-DADOS-ENTRADA

           PERFORM 1110-ABRIR-ARQ-VENDEDOR
           .
       1000-99-FIM.                    EXIT.

       1050-CONSISTIR-DADOS-ENTRADA    SECTION.

           PERFORM 2600-ACESSAR-VERIFICA
           IF BOOKVRIF-CTRL-COD    NOT EQUAL ZEROS
             MOVE 1                    TO BOOKVEND-CTRL-COD
             MOVE BOOKVRIF-CTRL-MSG
                                       TO BOOKVEND-CTRL-MSG
           END-IF
           .
       1050-99-FIM.                    EXIT.

       1110-ABRIR-ARQ-VENDEDOR         SECTION.

           OPEN OUTPUT ARQ-VENDEDOR
           IF WS-FS-ARQ-VEND           EQUAL ZEROS OR 05
             CONTINUE
           ELSE
             MOVE 1                    TO BOOKVEND-CTRL-COD
             MOVE 'ERRO NA ABERTURA DO ARQUIVO'
                                       TO BOOKVEND-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF
           .
       1110-99-FIM.                    EXIT.

       1120-GRAVAR-ARQ-VENDEDOR        SECTION.

           WRITE FD-VENDEDOR
           IF WS-FS-ARQ-VEND           EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKVEND-CTRL-COD
             MOVE "ERRO NA GRAVACAO DO ARQUIVO"
                                       TO BOOKVEND-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF
           .
       1120-99-FIM.                    EXIT.

       1130-REGRAVAR-ARQ-VENDEDOR      SECTION.

           REWRITE FD-VENDEDOR

           IF WS-FS-ARQ-VEND           EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKVEND-CTRL-COD
             MOVE "ERRO NA ATUALIZACAO DO ARQUIVO"
                                       TO BOOKVEND-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF
           .
       1130-99-FIM.                    EXIT.

       1140-EXCLUIR-ARQ-VENDEDOR       SECTION.

           DELETE ARQ-VENDEDOR

           IF WS-FS-ARQ-VEND           EQUAL ZEROS
              CONTINUE
           ELSE
             MOVE 2                    TO BOOKVEND-CTRL-COD
             MOVE "ERRO NA EXCLUSAO DO ARQUIVO"
                                       TO BOOKVEND-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF
           .
       1140-99-FIM.                    EXIT.

       1150-FECHAR-ARQ-VENDEDOR        SECTION.

           CLOSE ARQ-VENDEDOR

           IF WS-FS-ARQ-VEND           EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKVEND-CTRL-COD
             MOVE "ERRO NO FECHAMENTO DO ARQUIVO"
                                       TO BOOKVEND-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF
           .
       1150-99-FIM.                    EXIT.


       1160-ABRIR-ARQ-IMPORT           SECTION.

           OPEN INPUT ARQ-IMPORTA

           IF WS-FS-ARQ-IMPORTA        EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKVEND-CTRL-COD
             MOVE "ERRO NO ABERTURA DO ARQUIVO"
                                       TO BOOKVEND-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1160-99-FIM.

       1170-LER-ARQ-IMPORT             SECTION.

           READ ARQ-VENDEDOR           INTO WS-AREA-IMPORT-VEND

           IF WS-FS-ARQ-VEND           EQUAL ZEROS
             CONTINUE
           ELSE IF WS-FS-ARQ-VEND      EQUAL '10'
             MOVE 'S'                  TO WS-FIM-ARQ-IMPORTA
           ELSE
             MOVE 2                    TO BOOKVEND-CTRL-COD
             MOVE "ERRO NA LEITURA DO ARQUIVO"
                                       TO BOOKVEND-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1170-99-FIM.

       1180-FECHAR-ARQ-IMPORT          SECTION.

           CLOSE ARQ-IMPORTA

           IF WS-FS-ARQ-IMPORTA        EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 2                   TO BOOKVEND-CTRL-COD
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO"
                                       TO BOOKVEND-CTRL-MSG
              PERFORM 3000-FINALIZAR
           END-IF

           .
       1180-99-FIM.

       2000-PROCESSAR                  SECTION.

           EVALUATE BOOKVEND-REG-SERVICO
             WHEN 'I'
               PERFORM 2100-INCLUIR
             WHEN 'A'
               PERFORM 2200-ALTERAR
             WHEN 'E'
               PERFORM 2300-EXCLUIR
             WHEN 'R'
               PERFORM 1160-ABRIR-ARQ-IMPORT
               PERFORM 2400-IMPORTAR
               PERFORM 1180-FECHAR-ARQ-IMPORT
             WHEN 'P'
               PERFORM 2500-PESQUISAR
             WHEN OTHER
               MOVE 1                  TO BOOKVEND-CTRL-COD
               MOVE "OPCAO INVALIDA"
                                       TO BOOKVEND-CTRL-MSG
               PERFORM 3000-FINALIZAR
           END-EVALUATE

           .
       2000-99-FIM.                    EXIT.

       2100-INCLUIR                  SECTION.

           MOVE BOOKVEND-REG-COD-VEND  TO FD-VEND-COD-VENDEDOR
           READ ARQ-VENDEDOR           RECORD INTO FD-VENDEDOR
                                       KEY IS FD-VEND-COD-VENDEDOR
           IF WS-FS-ARQ-VEND           EQUAL ZEROS
             MOVE 1                    TO BOOKVEND-CTRL-COD
             MOVE "VENDEDOR JA CADASTRADO"
                                       TO BOOKVEND-CTRL-MSG
           ELSE
             MOVE BOOKVEND-REG-CPF     TO FD-VEND-CPF
             READ ARQ-VENDEDOR         RECORD INTO FD-VENDEDOR
                                       KEY IS FD-VEND-CPF
             IF WS-FS-ARQ-VEND         EQUAL ZEROS
               MOVE 1                  TO BOOKVEND-CTRL-COD
               MOVE "VENDEDOR JA CADASTRADO"
                                       TO BOOKVEND-CTRL-MSG
             ELSE
               MOVE BOOKVEND-REG-COD-VEND
                                       TO FD-VEND-COD-VENDEDOR
               MOVE BOOKVEND-REG-CPF   TO FD-VEND-CPF
               MOVE BOOKVEND-REG-NOME  TO FD-VEND-NOME
               MOVE BOOKVEND-REG-LATITUDE
                                       TO FD-VEND-LATITUDE
               MOVE BOOKVEND-REG-LONGITUDE
                                       TO FD-VEND-LONGITUDE
               PERFORM 1120-GRAVAR-ARQ-VENDEDOR
               MOVE ZEROS              TO BOOKVEND-CTRL-COD
               MOVE "VENDEDOR CADASTRADO COM SUCESSO"
                                       TO BOOKVEND-CTRL-MSG
             END-IF
           END-IF

           .
       2100-99-FIM.                    EXIT.

       2200-ALTERAR                    SECTION.
           MOVE BOOKVEND-REG-COD-VEND  TO FD-IMP-COD-VENDEDOR
           READ ARQ-VENDEDOR           RECORD INTO FD-VENDEDOR
                                       KEY IS FD-VEND-COD-VENDEDOR

           DISPLAY WS-FS-ARQ-VEND AT 2310
           IF WS-FS-ARQ-VEND           EQUAL ZEROS
             MOVE BOOKVEND-REG-COD-VEND
                                       TO FD-VEND-COD-VENDEDOR
             MOVE BOOKVEND-REG-CPF     TO FD-VEND-CPF
             MOVE BOOKVEND-REG-NOME    TO FD-VEND-NOME
             MOVE BOOKVEND-REG-LATITUDE
                                       TO FD-VEND-LATITUDE
             MOVE BOOKVEND-REG-LONGITUDE
                                       TO FD-VEND-LONGITUDE
             PERFORM 1130-REGRAVAR-ARQ-VENDEDOR
             MOVE ZEROS                TO BOOKVEND-CTRL-COD
             MOVE "VENDEDOR ALTERADO COM SUCESSO"
                                       TO BOOKVEND-CTRL-MSG
           END-IF

           .
       2200-99-FIM.                    EXIT.

       2300-EXCLUIR                    SECTION.
           MOVE BOOKVEND-REG-COD-VEND  TO FD-IMP-COD-VENDEDOR
           READ ARQ-VENDEDOR           RECORD INTO FD-VENDEDOR
                                       KEY IS FD-VEND-COD-VENDEDOR
           IF WS-FS-ARQ-VEND           EQUAL ZEROS
             PERFORM 1140-EXCLUIR-ARQ-VENDEDOR
             MOVE ZEROS                TO BOOKVEND-CTRL-COD
             MOVE "VENDEDOR EXCLUIDO COM SUCESSO"
                                       TO BOOKVEND-CTRL-MSG
           END-IF
           PERFORM 3000-FINALIZAR

           .
       2300-99-FIM.                    EXIT.

       2400-IMPORTAR                   SECTION.

           MOVE BOOKVEND-REG-ARQUVO    TO WS-ENDER

           PERFORM                     UNTIL WS-FIM-ARQ-IMPORTA
                                       EQUAL 'S'
             INITIALIZE                WS-AREA-IMPORT-VEND
             PERFORM 1170-LER-ARQ-IMPORT

             MOVE WS-COD-VENDEDOR      TO FD-VEND-COD-VENDEDOR
             MOVE WS-CPF               TO FD-VEND-CPF
             MOVE WS-NOME              TO FD-VEND-NOME
             MOVE WS-LATITUDE          TO FD-VEND-LATITUDE
             MOVE WS-LONGITUDE         TO FD-VEND-LONGITUDE

             PERFORM 1120-GRAVAR-ARQ-VENDEDOR
           END-PERFORM

           MOVE ZEROS                  TO BOOKVEND-CTRL-COD
           MOVE "VENDEDORES IMPORTADOS COM SUCESSO"
                                       TO BOOKVEND-CTRL-MSG
           .
       2400-99-FIM.                    EXIT.


       2500-PESQUISAR                  SECTION.

           MOVE BOOKVEND-REG-COD-VEND  TO FD-IMP-COD-VENDEDOR
           READ ARQ-VENDEDOR           RECORD INTO FD-VENDEDOR
                                       KEY IS FD-VEND-COD-VENDEDOR

           IF WS-FS-ARQ-VEND           EQUAL ZEROS
             MOVE ZEROS                TO BOOKVEND-CTRL-COD
             MOVE FD-VEND-COD-VENDEDOR TO BOOKVEND-REG-COD-VEND
             MOVE FD-VEND-CPF          TO BOOKVEND-REG-CPF
             MOVE FD-VEND-NOME         TO BOOKVEND-REG-NOME
             MOVE FD-VEND-LATITUDE     TO BOOKVEND-REG-LATITUDE
             MOVE FD-VEND-LONGITUDE    TO BOOKVEND-REG-LONGITUDE
           ELSE
             MOVE 3                   TO BOOKVEND-CTRL-COD
             MOVE 'VENDEDOR NAO ENCONTRADO'
                                       TO BOOKVEND-CTRL-MSG
           END-IF

           .
       2500-99-FIM.                    EXIT.

       2600-ACESSAR-VERIFICA           SECTION.

           INITIALIZE                  WS-LINK-VERIFICA

           DISPLAY BOOKVEND-REG-CPF
           MOVE BOOKVEND-REG-CPF       TO BOOKVRIF-REG-CPF

           CALL WS-VERIFICA            USING WS-LINK-VERIFICA

           .
       2600-99-FIM.

       3000-FINALIZAR                  SECTION.
           PERFORM 1150-FECHAR-ARQ-VENDEDOR
           GOBACK
           .
       3000-99-FIM.                    EXIT.
