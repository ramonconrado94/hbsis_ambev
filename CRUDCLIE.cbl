       IDENTIFICATION                  DIVISION.

       PROGRAM-ID. CRUDCLIE.
       AUTHOR.     RAMON CONRADO.

       ENVIRONMENT                     DIVISION.


       CONFIGURATION                   SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

       INPUT-OUTPUT                    SECTION.

       FILE-CONTROL.

           SELECT ARQ-CLIENTE          ASSIGN TO "CLIENTE.TXT"
                  ORGANIZATION         IS INDEXED
                  ACCESS MODE          IS DYNAMIC
                  RECORD KEY           IS FD-CLI-COD-CLIENTE
                  ALTERNATE RECORD KEY IS FD-CLI-CNPJ
                  LOCK MODE            IS MANUAL
                  FILE STATUS          IS WS-FS-ARQ-CLIE.

           SELECT ARQ-IMPORTA          ASSIGN TO DISK WS-ENDER
                  ORGANIZATION         IS LINE SEQUENTIAL
                  ACCESS MODE          IS SEQUENTIAL
                  LOCK MODE            IS MANUAL
                  FILE STATUS          IS WS-FS-ARQ-IMPORTA.

       DATA                            DIVISION.
       FILE                            SECTION.

       FD  ARQ-CLIENTE
           LABEL RECORD                IS STANDARD.
       01  FD-CLIENTE.
           05 FD-CLI-COD-CLIENTE       PIC 9(007).
           05 FD-CLI-CNPJ              PIC 9(014).
           05 FD-CLI-RZ-SOCIAL         PIC X(040).
           05 FD-CLI-LATITUDE          PIC S9(003)V9(008).
           05 FD-CLI-LONGITUDE         PIC S9(003)V9(008).
           05 FD-CLI-COD-VENDEDOR      PIC 9(003).

       FD  ARQ-IMPORTA
           LABEL RECORD                IS STANDARD.
       01  FD-IMPORTA.
           05 FD-IMP-COD-CLIENTE       PIC 9(007).
           05 FD-IMP-CNPJ              PIC 9(014).
           05 FD-IMP-RAZAO-SOCIAL      PIC X(040).
           05 FD-IMP-LATITUDE          PIC S9(003)V9(008).
           05 FD-IMP-LONGITUDE         PIC S9(003)V9(008).

       WORKING-STORAGE                 SECTION.

       77  WS-FS-ARQ-CLIE              PIC X(002)          VALUE SPACES.
       77  WS-FS-ARQ-IMPORTA           PIC X(002)          VALUE SPACES.
       77  WS-VERIFICA                 PIC X(008)          VALUE
                                                           'VERIFICA'.

       01  WS-FIM-ARQ-IMPORTA          PIC X(001)          VALUE SPACES.
       01  WS-ENDER                    PIC X(040)          VALUE SPACES.

       01  WS-IMPORTACAO.
           05 WS-COD-CLIENTE           PIC 9(007)          VALUE ZEROS.
           05 WS-CNPJ                  PIC 9(014)          VALUE ZEROS.
           05 WS-RAZAO-SOCIAL          PIC X(040)          VALUE SPACES.
           05 WS-LAT                   PIC S9(003)V9(08)   VALUE ZEROS.
           05 WS-LONG                  PIC S9(003)V9(08)   VALUE ZEROS.


       01  WS-LINK-VERIFICA.
           COPY 'BOOKVRIF.CPY'.


       LINKAGE                         SECTION.
       01  WS-LINKAGE-AREA.
           COPY 'BOOKCLIE.CPY'.

       PROCEDURE                       DIVISION USING WS-LINKAGE-AREA.


       0000-PRINCIPAL                  SECTION.

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 3000-FINALIZAR
           .
       0000-99-FIM.                    EXIT.

       1000-INICIALIZAR                SECTION.

           PERFORM 1100-CONSISTIR-DADOS-ENTRADA
           PERFORM 1110-ABRIR-ARQ-CLIENTE
           .
       1000-99-FIM.                    EXIT.

       1100-CONSISTIR-DADOS-ENTRADA    SECTION.

           PERFORM 2600-ACESSAR-VERIFICA
           IF BOOKVRIF-CONTROLE-COD    NOT EQUAL ZEROS
             MOVE 1                    TO BOOKCLIE-CONTROLE-COD
             MOVE BOOKVRIF-CONTROLE-MSG
                                       TO BOOKCLIE-CONTROLE-MSG
           END-IF
           .
       1100-99-FIM.                    EXIT.

       1110-ABRIR-ARQ-CLIENTE          SECTION.

           OPEN OUTPUT ARQ-CLIENTE
           IF WS-FS-ARQ-CLIE           EQUAL ZEROS OR 05
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKCLIE-CTRL-COD
             MOVE 'ERRO NA ABERTURA DO ARQUIVO'
                                       TO BOOKCLIE-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF
           .
       1110-99-FIM.                    EXIT.

       1120-GRAVAR-ARQ-CLIENTE         SECTION.

           WRITE FD-CLIENTE
           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKCLIE-CTRL-COD
             MOVE "ERRO NA GRAVACAO DO ARQUIVO"
                                       TO BOOKCLIE-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF
           .
       1120-99-FIM.                    EXIT.

       1130-REGRAVAR-ARQ-CLIENTE       SECTION.

           REWRITE FD-CLIENTE

           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKCLIE-CTRL-COD
             MOVE "ERRO NA ATUALIZACAO DO ARQUIVO"
                                       TO BOOKCLIE-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1130-99-FIM.                    EXIT.

       1140-EXCLUIR-ARQ-CLIENTE        SECTION.

           DELETE ARQ-CLIENTE

           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
              CONTINUE
           ELSE
             MOVE 2                    TO BOOKCLIE-CTRL-COD
             MOVE "ERRO NA EXCLUSAO DO ARQUIVO"
                                       TO BOOKCLIE-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1140-99-FIM.                    EXIT.

       1150-FECHAR-ARQ-CLIENTE         SECTION.

           CLOSE ARQ-CLIENTE

           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKCLIE-CTRL-COD
             MOVE "ERRO NO FECHAMENTO DO ARQUIVO"
                                       TO BOOKCLIE-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1150-99-FIM.                    EXIT.

       1160-ABRIR-ARQ-IMPORT           SECTION.

           OPEN INPUT ARQ-IMPORTA

           IF WS-FS-ARQ-IMPORTA        EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKCLIE-CTRL-COD
             MOVE "ERRO NO ABERTURA DO ARQUIVO"
                                       TO BOOKCLIE-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1160-99-FIM.

       1170-LER-ARQ-IMPORT             SECTION.

           READ ARQ-CLIENTE            INTO WS-IMPORTACAO

           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
             CONTINUE
           ELSE IF WS-FS-ARQ-CLIE      EQUAL '10'
             MOVE 'S'                  TO WS-FIM-ARQ-IMPORTA
           ELSE
             MOVE 2                    TO BOOKCLIE-CTRL-COD
             MOVE "ERRO NA LEITURA DO ARQUIVO"
                                       TO BOOKCLIE-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF
           .
       1170-99-FIM.

       1180-FECHAR-ARQ-IMPORT          SECTION.

           CLOSE ARQ-IMPORTA

           IF WS-FS-ARQ-IMPORTA        EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 2                   TO BOOKCLIE-CTRL-COD
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO"
                                       TO BOOKCLIE-CTRL-MSG
              PERFORM 3000-FINALIZAR
           END-IF
           .
       1180-99-FIM.

       2000-PROCESSAR                  SECTION.

           EVALUATE BOOKCLIE-REG-SERVICO
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
               MOVE 1                  TO BOOKCLIE-CTRL-COD
               MOVE "OPCAO INVALIDA"
                                       TO BOOKCLIE-CTRL-MSG
               PERFORM 3000-FINALIZAR
           END-EVALUATE
           .
       2000-99-FIM.                    EXIT.

       2100-INCLUIR                  SECTION.

           MOVE BOOKCLIE-REG-COD-CLIENTE
                                       TO FD-CLI-COD-CLIENTE
           READ ARQ-CLIENTE            RECORD INTO FD-CLIENTE
                                       KEY IS FD-CLI-COD-CLIENTE
           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
             MOVE 1                    TO BOOKCLIE-CTRL-COD
             MOVE "CLIENTE JA CADASTRADO"
                                       TO BOOKCLIE-CTRL-MSG
           ELSE
             MOVE BOOKCLIE-REG-CNPJ    TO FD-CLI-CNPJ
             READ ARQ-CLIENTE          RECORD INTO FD-CLIENTE
                                       KEY IS FD-CLI-CNPJ
             IF WS-FS-ARQ-CLIE         EQUAL ZEROS
               MOVE 1                  TO BOOKCLIE-CTRL-COD
               MOVE "CLIENTE JA CADASTRADO"
                                       TO BOOKCLIE-CTRL-MSG
             ELSE
               MOVE BOOKCLIE-REG-COD-CLIENTE
                                       TO FD-CLI-COD-CLIENTE
               MOVE BOOKCLIE-REG-CNPJ
                                       TO FD-CLI-CNPJ
               MOVE BOOKCLIE-REG-RZ-SOCIAL
                                       TO FD-CLI-RZ-SOCIAL
               MOVE BOOKCLIE-REG-LATITUDE
                                       TO FD-CLI-LATITUDE
               MOVE BOOKCLIE-REG-LONGITUDE
                                       TO FD-CLI-LONGITUDE
               PERFORM 1120-GRAVAR-ARQ-CLIENTE
               MOVE ZEROS              TO BOOKCLIE-CTRL-COD
               MOVE "CLIENTE CADASTRADO COM SUCESSO"
                                       TO BOOKCLIE-CTRL-MSG
             END-IF
           END-IF
           .
       2100-99-FIM.                    EXIT.

       2200-ALTERAR                  SECTION.

           MOVE BOOKCLIE-REG-COD-CLIENTE
                                       TO FD-IMP-COD-CLIENTE
           READ ARQ-CLIENTE            RECORD INTO FD-CLIENTE
                                       KEY IS FD-CLI-COD-CLIENTE
           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
             MOVE BOOKCLIE-REG-COD-CLIENTE
                                       TO FD-CLI-COD-CLIENTE
             MOVE BOOKCLIE-REG-CNPJ
                                       TO FD-CLI-CNPJ
             MOVE BOOKCLIE-REG-RZ-SOCIAL
                                       TO FD-CLI-RZ-SOCIAL
             MOVE BOOKCLIE-REG-LATITUDE
                                       TO FD-CLI-LATITUDE
             MOVE BOOKCLIE-REG-LONGITUDE
                                       TO FD-CLI-LONGITUDE
             PERFORM 1130-REGRAVAR-ARQ-CLIENTE
             MOVE ZEROS                TO BOOKCLIE-CTRL-COD
             MOVE "CLIENTE CADASTRADO COM SUCESSO"
                                       TO BOOKCLIE-CTRL-MSG
           END-IF
           .
       2200-99-FIM.                    EXIT.

       2300-EXCLUIR                  SECTION.

           MOVE BOOKCLIE-REG-COD-CLIENTE
                                       TO FD-IMP-COD-CLIENTE
           READ ARQ-CLIENTE            RECORD INTO FD-CLIENTE
                                       KEY IS FD-CLI-COD-CLIENTE
           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
             PERFORM 1140-EXCLUIR-ARQ-CLIENTE
             MOVE ZEROS                TO BOOKCLIE-CTRL-COD
             MOVE "CLIENTE EXCLUIDO COM SUCESSO"
                                       TO BOOKCLIE-CTRL-MSG
           END-IF
           .
       2300-99-FIM.                    EXIT.

       2400-IMPORTAR                   SECTION.

           MOVE BOOKCLIE-REG-ARQUVO    TO WS-ENDER

           PERFORM                     UNTIL WS-FIM-ARQ-IMPORTA
                                       EQUAL 'S'
             INITIALIZE                WS-IMPORTACAO
             PERFORM 1170-LER-ARQ-IMPORT

             MOVE WS-COD-CLIENTE       TO FD-CLI-COD-CLIENTE
             MOVE WS-CNPJ              TO FD-CLI-CNPJ
             MOVE WS-RAZAO-SOCIAL      TO FD-CLI-RZ-SOCIAL
             MOVE WS-LAT          TO FD-CLI-LATITUDE
             MOVE WS-LONG         TO FD-CLI-LONGITUDE

             PERFORM 1120-GRAVAR-ARQ-CLIENTE
           END-PERFORM

           MOVE ZEROS                  TO BOOKCLIE-CTRL-COD
           MOVE "ARQUIVO IMPORTADO COM SUCESSO"
                                       TO BOOKCLIE-CTRL-MSG
           .
       2400-99-FIM.                    EXIT.

       2500-PESQUISAR                     SECTION.

           MOVE BOOKCLIE-REG-COD-CLIENTE
                                       TO FD-IMP-COD-CLIENTE
           READ ARQ-CLIENTE            RECORD INTO FD-CLIENTE
                                       KEY IS FD-CLI-COD-CLIENTE

           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
             MOVE ZEROS                TO BOOKCLIE-CTRL-COD
             MOVE FD-CLI-COD-CLIENTE   TO BOOKCLIE-REG-COD-CLIENTE
             MOVE FD-CLI-CNPJ          TO BOOKCLIE-REG-CNPJ
             MOVE FD-CLI-RZ-SOCIAL     TO BOOKCLIE-REG-RZ-SOCIAL
             MOVE FD-CLI-LATITUDE      TO BOOKCLIE-REG-LATITUDE
             MOVE FD-CLI-LONGITUDE     TO BOOKCLIE-REG-LONGITUDE
           ELSE
             MOVE 3                    TO BOOKCLIE-CTRL-COD
             MOVE 'CLIENTE NAO ENCONTRADO'
                                       TO BOOKCLIE-CTRL-MSG
           END-IF
           .
       2500-99-FIM.                    EXIT.

       2600-ACESSAR-VERIFICA           SECTION.
           INITIALIZE                  WS-LINK-VERIFICA

           MOVE BOOKCLIE-REG-CNPJ      TO BOOKVRIF-REG-CNPJ

           CALL WS-VERIFICA            USING WS-LINK-VERIFICA
           END-CALL
           .
       2600-99-FIM.

       3000-FINALIZAR                  SECTION.

           PERFORM 1150-FECHAR-ARQ-CLIENTE
           GOBACK

           .
       3000-99-FIM.                    EXIT.
