
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID. PROGMENU.
       AUTHOR.     RAMON CONRADO.


       ENVIRONMENT                     DIVISION.

       CONFIGURATION                   SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

       DATA                            DIVISION.

       WORKING-STORAGE                 SECTION.

       01  WS-DADOS-ENTRADA.
           05 WS-OPCAO                 PIC X(001)          VALUE ZEROS.
           05 WS-TIPO-CADASTRO         PIC X(001)          VALUE ZEROS.
           05 WS-OPCAO-CRUD            PIC X(001)          VALUE SPACES.
              88 WS-INCLUIR                                VALUE 'I'.
              88 WS-ALTERAR                                VALUE 'A'.
              88 WS-EXCLUIR                                VALUE 'E'.
              88 WS-IMPORTACAO                             VALUE 'R'.
              88 WS-PESQUISAR                              VALUE 'P'.
           05 WS-ARQ-CLI               PIC X(040)          VALUE SPACES.

           05 WS-CLIENTE.
              10 WS-CODIGO             PIC 9(007)          VALUE ZEROS.
              10 WS-CNPJ               PIC 9(014)          VALUE ZEROS.
              10 WS-RAZAO-SOCIAL       PIC X(040)          VALUE SPACES.
              10 WS-LAT                PIC S9(003)V9(008)  VALUE ZEROS.
              10 WS-LONG               PIC S9(003)V9(008)  VALUE ZEROS.

           05 WS-VENDEDOR.
              10 WS-CODIGO             PIC 9(003)          VALUE ZEROS.
              10 WS-CPF                PIC 9(011)          VALUE ZEROS.
              10 WS-NOME-VENDEDOR      PIC X(040)          VALUE SPACES.
              10 WS-LAT                PIC S9(003)V9(008)  VALUE ZEROS.
              10 WS-LONG               PIC S9(003)V9(008)  VALUE ZEROS.

           05 WS-RELATORIO.
              10 WS-TIPO-ORDENACAO     PIC X(001)          VALUE SPACES.
              10 WS-TIPO-CLASS         PIC X(001)          VALUE SPACES.
              10 WS-COD-CLIE-RELAT     PIC 9(007)          VALUE ZEROS.
              10 WS-COD-VEND-RELAT     PIC 9(003)          VALUE ZEROS.
              10 WS-NOME-RAZAO-RELAT   PIC X(040)          VALUE ZEROS.

       01 WS-AUXILIARES.
              05 WS-TIPO-OPER          PIC X(020)          VALUE SPACES.

       01  WS-LINK-CRUDCLIE.
           COPY 'BOOKCLIE.CPY'.

       01  WS-LINK-CRUDVEND.
           COPY 'BOOKVEND.CPY'.

       77  WS-CRUDCLIE                 PIC  X(008)         VALUE
                                                           'CRUDCLIE'.
       77  WS-CRUDVEND                 PIC  X(008)         VALUE
                                                           'CRUDVEND'.
       SCREEN                          SECTION.

       01  TELA-MENU.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE
           "---------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 20          VALUE "AMBEV - MENU PRINCIPAL".
           05  LINE 02 COL 57          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "---------------------------------------------------------".
           05  LINE 05 COL 02          VALUE
                                          "ESCOLHA UMA OPCAO:".
           05  LINE 05 COL 21          PIC X(001)
                                       TO WS-OPCAO.
           05  LINE 07 COL 02          VALUE "1 - CADASTROS".
           05  LINE 08 COL 02          VALUE "2 - RELATORIOS".
           05  LINE 09 COL 02          VALUE "3 - EXECUTAR".
           05  LINE 11 COL 02          VALUE "4 - SAIR".

       01  TELA-SUBMENU.
              05 BLANK SCREEN.
              05 LINE 1 COL 1             VALUE
              "-------------------------------------------------------".
              05  LINE 02 COL 01          VALUE "*".
              05  LINE 02 COL 19          VALUE
                                            "AMBEV - MENU DE OPERACOES".
              05  LINE 02 COL 55          VALUE "*".
              05  LINE 03 COL 01          VALUE
              "-------------------------------------------------------".
              05  LINE 05 COL 02          VALUE
                                          "ESCOLHA UMA OPCAO:".
              05  LINE 05 COL 21          PIC X(001)
                                          TO WS-OPCAO.
              05  LINE 07 COL 02          VALUE "1 - INCLUSAO".
              05  LINE 08 COL 02          VALUE "2 - ALTERACAO".
              05  LINE 09 COL 02          VALUE "3 - EXCLUSAO".
              05  LINE 10 COL 02          VALUE "4 - PESQUISAR REGISTRO".
              05  LINE 12 COL 02          VALUE "5 - VOLTAR".


       01  TELA-CADASTROS.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 19          VALUE "AMBEV - CADASTRO".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 05 COL 02          VALUE
                                          "ESCOLHA O TIPO DE CADASTRO:".
           05  LINE 05 COL 30         PIC X(001)
                                       TO WS-OPCAO.
           05  LINE 07 COL 02          VALUE "1 - CLIENTE".
           05  LINE 08 COL 02          VALUE "2 - VENDEDOR".
           05  LINE 10 COL 02          VALUE "3 - VOLTAR".

       01  TELA-INCLUIR-CLIENTE.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 19          VALUE "INCLUIR CLIENTE".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 02          VALUE "CODIGO CLIENTE:".
           05  LINE 06 COL 26          PIC ZZZZZZ9
                                       TO WS-CODIGO
                                       OF WS-CLIENTE.
           05  LINE 07 COL 02          VALUE "CNPJ..........:".
           05  LINE 07 COL 26          PIC 9(014)
                                       TO WS-CNPJ.
           05  LINE 08 COL 02          VALUE "RAZAO SOCIAL..:".
           05  LINE 08 COL 26          PIC  X(040)
                                       TO WS-RAZAO-SOCIAL.
           05  LINE 09 COL 02          VALUE "LATITUDE......:".
           05  LINE 09 COL 26          PIC -ZZ9,99999999
                                       TO WS-LAT
                                       OF WS-CLIENTE.
           05  LINE 10 COL 02          VALUE "LONGITUDE.....:".
           05  LINE 10 COL 26          PIC -ZZ9,99999999
                                       TO WS-LONG
                                       OF WS-CLIENTE.
           05  LINE 15 COL 02          VALUE
              "CONFIRMA INCLUIR DO CLIENTE?".
           05  LINE 13 COL 02          VALUE
              "1 - SIM  2 - NAO:".
           05  LINE 13 COL 28          PIC X(001)
                                       TO WS-OPCAO.

       01  TELA-ALTERAR-CLIENTE.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 19          VALUE "ALTERAR CLIENTE".
           05  LINE 02 COL 58         VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 02          VALUE "CODIGO CLIENTE:".
           05  LINE 06 COL 26          PIC ZZZZZZ9
                                       FROM WS-CODIGO
                                       OF WS-CLIENTE.
           05  LINE 07 COL 02          VALUE "CNPJ..........:".
           05  LINE 07 COL 26          PIC 9(014)
                                       FROM WS-CNPJ.
           05  LINE 08 COL 02          VALUE "RAZAO SOCIAL..:".
           05  LINE 08 COL 26          PIC X(040)
                                       USING WS-RAZAO-SOCIAL AUTO.
           05  LINE 10 COL 02          VALUE "LATITUDE......:".
           05  LINE 10 COL 26          PIC -ZZ9,99999999
                                       USING WS-LAT
                                       OF WS-CLIENTE AUTO.
           05  LINE 09 COL 02          VALUE "LONGITUDE.....:".
           05  LINE 09 COL 26          PIC -ZZ9,99999999
                                       USING WS-LONG
                                       OF WS-CLIENTE AUTO.
           05  LINE 15 COL 02          VALUE
              "CONFIRMA ALTERAR DO CLIENTE?".
           05  LINE 13 COL 02          VALUE
              "1 - SIM  2 - NAO".
           05  LINE 15 COL 28          PIC X(001)
                                       TO WS-OPCAO.

       01  TELA-EXCLUIR-CLIENTE.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 19          VALUE "EXCLUIR CLIENTE".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 02          VALUE "CODIGO CLIENTE:".
           05  LINE 06 COL 26          PIC ZZZZZZ9
                                       FROM WS-CODIGO
                                       OF WS-CLIENTE.
           05  LINE 07 COL 02          VALUE "CNPJ..........:".
           05  LINE 07 COL 26          PIC 9(014)
                                       FROM WS-CNPJ.
           05  LINE 08 COL 02          VALUE "RAZAO SOCIAL..:".
           05  LINE 08 COL 26          PIC X(040)
                                       USING WS-RAZAO-SOCIAL AUTO.
           05  LINE 09 COL 02          VALUE "LATITUDE......:".
           05  LINE 09 COL 26          PIC -ZZ9,99999999
                                       USING WS-LAT
                                       OF WS-CLIENTE AUTO.
           05  LINE 09 COL 02          VALUE "LONGITUDE.....:".
           05  LINE 09 COL 26          PIC -ZZ9,99999999
                                       USING WS-LONG
                                       OF WS-CLIENTE AUTO.

           05  LINE 11 COL 02          VALUE
              "CONFIRMA EXCLUIR CLIENTE?".
           05  LINE 15 COL 02          VALUE
              "1 - SIM  2 - NAO".
           05  LINE 15 COL 28          PIC X(001)
                                       TO WS-OPCAO.

       01  TELA-PESQUISAR-CLIENTE.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 19          VALUE
                                            "AMBEV - PESQUISAR CLIENTE".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 02          VALUE "CODIGO CLIENTE:".
           05  LINE 06 COL 26          PIC ZZZZZZ9
                                       TO WS-CODIGO
                                       OF WS-CLIENTE.
           05  LINE 08 COL 02          VALUE
              "CONFIRMA BUSCA DO CLIENTE?".
           05  LINE 09 COL 02          VALUE
              "1 - SIM  2 - NAO".
           05  LINE 09 COL 28          PIC X(001)
                                       TO WS-OPCAO.

       01  TELA-INCLUIR-VENDEDOR.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 19          VALUE "INCLUIR VENDEDOR".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 02          VALUE "CODIGO VENDEDOR:".
           05  LINE 06 COL 26          PIC ZZ9
                                       TO WS-CODIGO
                                       OF WS-VENDEDOR.
           05  LINE 07 COL 02          VALUE "CPF...........:".
           05  LINE 07 COL 26          PIC 9(011)
                                       TO WS-CPF.
           05  LINE 08 COL 02          VALUE "NOME..........:".
           05  LINE 08 COL 26          PIC X(040)
                                       TO WS-NOME-VENDEDOR.
           05  LINE 09 COL 02          VALUE "LATITUDE......:".
           05  LINE 09 COL 26          PIC -ZZ9,99999999
                                       TO WS-LAT
                                       OF WS-VENDEDOR.
           05  LINE 10 COL 02          VALUE "LONGITUDE.....:".
           05  LINE 10 COL 26          PIC -ZZ9,99999999
                                       TO WS-LONG
                                       OF WS-VENDEDOR.

           05  LINE 14 COL 02          VALUE
              "CONFIRMA INCLUIR DO VENDEDOR?".
           05  LINE 14 COL 32          PIC X(001)
                                       TO WS-OPCAO.
           05  LINE 15 COL 02          VALUE
              "1 - SIM ".
           05  LINE 16 COL 02          VALUE
              "2 - NAO ".

       01  TELA-ALTERAR-VENDEDOR.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 19          VALUE "ALTERAR VENDEDOR".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 02          VALUE "CODIGO VENDEDOR:".
           05  LINE 06 COL 26          PIC 999
                                       FROM WS-CODIGO
                                       OF WS-VENDEDOR.
           05  LINE 07 COL 02          VALUE "CPF...........:".
           05  LINE 07 COL 26          PIC  9(011)
                                       FROM WS-CPF.
           05  LINE 08 COL 02          VALUE "NOME..........:".
           05  LINE 08 COL 26          PIC  X(040)
                                       USING WS-NOME-VENDEDOR AUTO.
           05  LINE 09 COL 02          VALUE "LATITUDE......:".
           05  LINE 09 COL 26          PIC -ZZ9,99999999
                                       USING WS-LAT
                                       OF WS-VENDEDOR AUTO.
           05  LINE 10 COL 02          VALUE "LONGITUDE.....:".
           05  LINE 10 COL 26          PIC -ZZ9,99999999
                                       USING WS-LONG
                                       OF WS-VENDEDOR AUTO.

           05  LINE 14 COL 02          VALUE
              "CONFIRMA ALTERAR DO VENDEDOR?".
           05  LINE 14 COL 31          PIC X(001)
                                       TO WS-OPCAO.
           05  LINE 15 COL 02          VALUE
              "1 - SIM ".
           05  LINE 16 COL 02          VALUE
              "2 - NAO ".

       01  TELA-IMPORTACAO.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 24          VALUE "IMPORTAR".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 02          VALUE
           "DIGITE O NOME DO ARQUIVO:".
           05  LINE 07 COL 02          PIC X(040)
                                       TO WS-ARQ-CLI.
           05  LINE 14 COL 02          VALUE
              "CONFIRMA A IMPORTACAO?".
           05  LINE 14 COL 31          PIC X(001)
                                       TO WS-OPCAO.
           05  LINE 15 COL 02          VALUE
              "1 - SIM ".
           05  LINE 16 COL 02          VALUE
              "2 - NAO ".

       01  TELA-PESQUISAR-VENDEDOR.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "-----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 19          VALUE
                                       "AMBEV - PESQUISAR VENDEDOR".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "-----------------------------------------------------------".
           05  LINE 06 COL 02          VALUE "CODIGO VENDEDOR:".
           05  LINE 06 COL 26          PIC ZZ9
                                       TO WS-CODIGO
                                       OF WS-VENDEDOR.
           05  LINE 14 COL 02          VALUE
              "CONFIRMA BUSCA DO CLIENTE?".
           05  LINE 14 COL 31          PIC X(001)
                                       TO WS-OPCAO.
           05  LINE 15 COL 02          VALUE
              "1 - SIM ".
           05  LINE 16 COL 02          VALUE
              "2 - NAO ".

       01  TELA-EXCLUIR-VENDEDOR.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 02          VALUE "CODIGO VENDEDOR:".
           05  LINE 06 COL 26          PIC ZZ9
                                       FROM WS-CODIGO
                                       OF WS-VENDEDOR.
           05  LINE 07 COL 02          VALUE "CPF...........:".
           05  LINE 07 COL 26          PIC  9(011)
                                       FROM WS-CPF.
           05  LINE 08 COL 02          VALUE "NOME..........:".
           05  LINE 08 COL 26          PIC  X(040)
                                       USING WS-NOME-VENDEDOR AUTO.
           05  LINE 09 COL 02          VALUE "LATITUDE......:".
           05  LINE 09 COL 26          PIC -ZZ9,99999999
                                       USING WS-LAT
                                       OF WS-VENDEDOR AUTO.
           05  LINE 10 COL 02          VALUE "LONGITUDE.....:".
           05  LINE 10 COL 26          PIC -ZZ9,99999999
                                       USING WS-LONG
                                       OF WS-VENDEDOR AUTO.
           05  LINE 14 COL 02          VALUE
              "CONFIRMA EXCLUIR VENDEDOR?".
           05  LINE 14 COL 31          PIC X(001)
                                       TO WS-OPCAO.
           05  LINE 15 COL 02          VALUE
              "1 - SIM ".
           05  LINE 16 COL 02          VALUE
              "2 - NAO ".

       01  TELA-RELATORIOS.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 24          VALUE "MENU RELATORIOS".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 10          VALUE "1 - CLIENTE".
           05  LINE 07 COL 10          VALUE "2 - VENDEDOR".
           05  LINE 09 COL 10          VALUE
           "DIGITE A OPCAO DESEJADA E TECLE ENTER: ".
           05  LINE 09 COL 49          PIC X(001)
                                       TO WS-OPCAO.

       01  TELA-RELATORIOS-CLIENTE.
           05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 24          VALUE "MENU RELATORIO CLIENTE".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 10          VALUE "TIPO DE ORDENACAO:".
           05  LINE 07 COL 10          VALUE
           "1 - ACESCENDENTE 2 - DECRESCENTE:".
           05  LINE 07 COL 55           PIC X(001)
                                       TO WS-TIPO-ORDENACAO.
           05  LINE 08 COL 10          VALUE "TIPO DE CLASSIFICACAO:".
           05  LINE 09 COL 10          VALUE
           "1 - CODIGO CLIENTE 2 - RAZAO SOCIAL:".
           05  LINE 09 COL 48          PIC X(001)
                                       TO WS-TIPO-CLASS.
           05  LINE 10 COL 10          VALUE "CODIGO CLIENTE:".
           05  LINE 10 COL 27          PIC ZZZZZZ9
                                       TO WS-COD-CLIE-RELAT.
           05  LINE 11 COL 10          VALUE "RAZAO SOCIAL:".
           05  LINE 11 COL 24          PIC X(040)
                                       TO WS-NOME-RAZAO-RELAT.
           05  LINE 12 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 12 COL 28          PIC ZZ9
                                       TO WS-COD-VEND-RELAT.
           05  LINE 14 COL 10          VALUE
           "DIGITE A OPCAO DESEJADA E TECLE ENTER: ".
           05  LINE 14 COL 49          PIC X(001)
                                       TO WS-OPCAO.

       01  TELA-RELATORIOS-VENDEDOR.
          05 BLANK SCREEN.
           05 LINE 1 COL 1             VALUE
           "----------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 24          VALUE "MENU RELATORIO VENDEDOR".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "----------------------------------------------------------".
           05  LINE 06 COL 10          VALUE "TIPO DE ORDENACAO:".
           05  LINE 07 COL 10          VALUE
           "1 - ACESCENDENTE 2 - DECRESCENTE:".
           05  LINE 07 COL 55           PIC X(001)
                                       TO WS-TIPO-ORDENACAO.
           05  LINE 08 COL 10          VALUE "TIPO DE CLASSIFICACAO:".
           05  LINE 09 COL 10          VALUE
           "1 - CODIGO VENDEDOR 2 - NOME VENDEDOR:".
           05  LINE 09 COL 60          PIC X(001)
                                       TO WS-TIPO-CLASS.
           05  LINE 10 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 10 COL 28          PIC ZZ9
                                       TO WS-COD-CLIE-RELAT.
           05  LINE 11 COL 10          VALUE "NOME VENDEDOR".
           05  LINE 11 COL 24          PIC X(040)
                                       TO WS-NOME-RAZAO-RELAT.
           05  LINE 13 COL 10          VALUE
           "DIGITE A OPCAO DESEJADA E TECLE ENTER: ".
           05  LINE 13 COL 49          PIC X(001)
                                       TO WS-OPCAO.

       PROCEDURE                       DIVISION.

       0000-PRINCIPAL                  SECTION.

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 3000-FINALIZAR
           .
       0000-99-FIM.                    EXIT.

       1000-INICIALIZAR                SECTION.

           PERFORM 1100-CONSISTIR-DADOS-ENTRADA
           .
       1000-99-FIM.                    EXIT.

       1100-CONSISTIR-DADOS-ENTRADA    SECTION.
           INITIALIZE                  WS-DADOS-ENTRADA
           .
       1100-99-FIM.                    EXIT.

       2000-PROCESSAR                  SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-MENU
           ACCEPT  TELA-MENU

           EVALUATE WS-OPCAO
             WHEN 1
               PERFORM 2100-MENU-CADASTRO
             WHEN 2
               PERFORM 2400-MENU-RELATORIOS
             WHEN 4
               PERFORM 3000-FINALIZAR
             WHEN OTHER
               DISPLAY "OPCAO INVALIDA"
                                       AT 2310
               PERFORM 2000-PROCESSAR
           END-EVALUATE
           .
       2000-99-FIM.                    EXIT.

       2100-MENU-CADASTRO              SECTION.
           INITIALIZE                  WS-OPCAO
                                       WS-TIPO-CADASTRO

           DISPLAY TELA-CADASTROS
           ACCEPT  TELA-CADASTROS

           EVALUATE WS-OPCAO
             WHEN 1
               PERFORM 2200-CRUD-CLIENTE
               MOVE 'C'                TO WS-TIPO-CADASTRO
             WHEN 2
               PERFORM 2300-CRUD-VENDEDOR
               MOVE 'V'                TO WS-TIPO-CADASTRO
             WHEN 3
               PERFORM 2000-PROCESSAR
             WHEN OTHER
               DISPLAY "OPCAO INVALIDA"
                                       AT 2310
               PERFORM 2100-MENU-CADASTRO
           END-EVALUATE
           .
       2100-99-FIM.                    EXIT.

       2200-CRUD-CLIENTE               SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-SUBMENU
           ACCEPT  TELA-SUBMENU

           EVALUATE WS-OPCAO
             WHEN 1
               PERFORM 2210-INCLUIR-CLIENTE
             WHEN 2
               PERFORM 2220-ALTERAR-CLIENTE
             WHEN 3
               PERFORM 2230-EXCLUIR-CLIENTE
             WHEN 4
               PERFORM 2240-IMPORTAR-CLIENTE
             WHEN 5
               PERFORM 2100-MENU-CADASTRO
             WHEN OTHER
               DISPLAY "OPCAO INVALIDA"
                                       AT 2310
               PERFORM 2200-CRUD-CLIENTE
           END-EVALUATE
           .
       2200-99-FIM.                    EXIT.

       2210-INCLUIR-CLIENTE            SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-INCLUIR-CLIENTE
           ACCEPT  TELA-INCLUIR-CLIENTE
           IF WS-OPCAO                 EQUAL 1
             SET WS-INCLUIR            TO TRUE
             PERFORM 2250-ACESSAR-CRUDCLIE
             DISPLAY BOOKCLIE-CTRL-MSG
                                       AT 2310
             STOP ' '
             INITIALIZE                WS-CLIENTE
             PERFORM 2210-INCLUIR-CLIENTE
           ELSE IF WS-OPCAO            EQUAL 2
             PERFORM 2200-CRUD-CLIENTE
           ELSE
             DISPLAY "OPCAO INVALIDA"  AT 2310
             PERFORM 2210-INCLUIR-CLIENTE
           END-IF
           .
           .
       2210-99-FIM.                    EXIT.

       2220-ALTERAR-CLIENTE            SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-PESQUISAR-CLIENTE
           ACCEPT  TELA-PESQUISAR-CLIENTE

            IF WS-OPCAO                 EQUAL 1
             SET WS-PESQUISAR           TO TRUE
             PERFORM 2250-ACESSAR-CRUDCLIE
             IF BOOKCLIE-CTRL-COD  ZEROS
               DISPLAY TELA-INCLUIR-CLIENTE
               ACCEPT  TELA-INCLUIR-CLIENTE
               SET WS-ALTERAR        TO TRUE
               PERFORM 2250-ACESSAR-CRUDCLIE
             END-IF
             DISPLAY BOOKCLIE-CTRL-MSG
                                       AT 2310
             INITIALIZE                WS-CLIENTE
             PERFORM 2220-ALTERAR-CLIENTE
             ELSE IF WS-OPCAO            EQUAL 2
               PERFORM 2200-CRUD-CLIENTE
             ELSE
               DISPLAY "OPCAO INVALIDA"  AT 2310
               PERFORM 2220-ALTERAR-CLIENTE
           END-IF
           .

           .
       2220-99-FIM.                    EXIT.

       2230-EXCLUIR-CLIENTE           SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-EXCLUIR-CLIENTE
           ACCEPT  TELA-EXCLUIR-CLIENTE

           IF WS-OPCAO                 EQUAL 1
             SET WS-EXCLUIR            TO TRUE
             PERFORM 2250-ACESSAR-CRUDCLIE
             DISPLAY BOOKCLIE-CTRL-MSG
                                       AT 2310
             STOP ' '
             INITIALIZE                WS-CLIENTE
             PERFORM 2230-EXCLUIR-CLIENTE
           ELSE IF WS-OPCAO            EQUAL 2
             PERFORM 2200-CRUD-CLIENTE
           ELSE
             DISPLAY "OPCAO INVALIDA"  AT 2310
             PERFORM 2230-EXCLUIR-CLIENTE
           END-IF
           .
       2230-99-FIM.                    EXIT.

       2240-IMPORTAR-CLIENTE         SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-IMPORTACAO
           ACCEPT  TELA-IMPORTACAO

           IF WS-OPCAO                 EQUAL 1
             SET WS-IMPORTACAO           TO TRUE
             PERFORM 2250-ACESSAR-CRUDCLIE
             DISPLAY BOOKCLIE-CTRL-MSG
                                       AT 2310
             STOP ' '
             INITIALIZE                WS-CLIENTE
             PERFORM 2240-IMPORTAR-CLIENTE
           ELSE IF WS-OPCAO            EQUAL 2
             PERFORM 2200-CRUD-CLIENTE
           ELSE
             DISPLAY "VALOR INVALIDO"  AT 2310
             STOP ' '
             PERFORM 2240-IMPORTAR-CLIENTE
           END-IF

           .
       2240-99-FIM.                    EXIT.

       2250-ACESSAR-CRUDCLIE           SECTION.

           INITIALIZE                  WS-LINK-CRUDCLIE

           MOVE WS-OPCAO-CRUD          TO BOOKCLIE-REG-SERVICO

           MOVE WS-CODIGO              OF WS-CLIENTE
                                       TO BOOKCLIE-REG-COD-CLIENTE
           MOVE WS-CNPJ                TO BOOKCLIE-REG-CNPJ
           MOVE WS-RAZAO-SOCIAL        TO BOOKCLIE-REG-RZ-SOCIAL
           MOVE WS-LAT                 OF WS-CLIENTE
                                       TO BOOKCLIE-REG-LATITUDE
           MOVE WS-LONG                OF WS-CLIENTE
                                       TO BOOKCLIE-REG-LONGITUDE
           CALL WS-CRUDCLIE            USING WS-LINK-CRUDCLIE
           .
       2250-99-FIM.

       2300-CRUD-VENDEDOR              SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-SUBMENU
           ACCEPT  TELA-SUBMENU

           EVALUATE WS-OPCAO
             WHEN 1
               PERFORM 2310-INCLUIR-VENDEDOR
             WHEN 2
               PERFORM 2320-ALTERAR-VENDEDOR
             WHEN 3
               PERFORM 2330-EXCLUIR-VENDEDOR
             WHEN 4
               PERFORM 2340-IMPORTAR-VENDEDOR
             WHEN 5
               PERFORM 2100-MENU-CADASTRO
             WHEN OTHER
               DISPLAY  "OPCAO INVALIDA"
                                       AT 2310
               PERFORM 2300-CRUD-VENDEDOR
           END-EVALUATE
           .
       2300-99-FIM.                    EXIT.

       2310-INCLUIR-VENDEDOR           SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-INCLUIR-VENDEDOR
           ACCEPT  TELA-INCLUIR-VENDEDOR

           IF WS-OPCAO                 EQUAL 1
             SET WS-INCLUIR            TO TRUE
             PERFORM 2350-ACESSAR-CRUDVEND
             DISPLAY BOOKVEND-CTRL-MSG
                                       AT 2310
             STOP ' '
             INITIALIZE                WS-VENDEDOR
             PERFORM 2310-INCLUIR-VENDEDOR
           ELSE IF WS-OPCAO            EQUAL 2
             PERFORM 2300-CRUD-VENDEDOR
           ELSE
             DISPLAY "OPCAO INVALIDA"  AT 2310
             PERFORM 2310-INCLUIR-VENDEDOR
           END-IF
           .
       2310-99-FIM.                    EXIT.

       2320-ALTERAR-VENDEDOR           SECTION.

           DISPLAY TELA-PESQUISAR-VENDEDOR
           ACCEPT  TELA-PESQUISAR-VENDEDOR

           IF WS-OPCAO                 EQUAL 1
             SET WS-PESQUISAR          TO TRUE
             PERFORM 2350-ACESSAR-CRUDVEND
             IF BOOKVEND-CTRL-COD  ZEROS
               DISPLAY TELA-INCLUIR-VENDEDOR
               ACCEPT  TELA-INCLUIR-VENDEDOR
               SET WS-ALTERAR        TO TRUE
               PERFORM 2350-ACESSAR-CRUDVEND
             END-IF
             DISPLAY BOOKVEND-CTRL-MSG
                                       AT 2310
             INITIALIZE                WS-CLIENTE
             PERFORM 2320-ALTERAR-VENDEDOR
           ELSE IF WS-OPCAO            EQUAL 2
             PERFORM 2300-CRUD-VENDEDOR
           ELSE
             DISPLAY "OPCAO INVALIDA"  AT 2310
             STOP ' '
             PERFORM 2320-ALTERAR-VENDEDOR
           END-IF
           .
       2320-99-FIM.                    EXIT.

       2330-EXCLUIR-VENDEDOR           SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-EXCLUIR-VENDEDOR
           ACCEPT  TELA-EXCLUIR-VENDEDOR
           IF WS-OPCAO                 EQUAL 1
             SET WS-EXCLUIR           TO TRUE
             PERFORM 2350-ACESSAR-CRUDVEND
             DISPLAY BOOKVEND-CTRL-MSG
                                       AT 2310
             INITIALIZE                WS-VENDEDOR
             PERFORM 2330-EXCLUIR-VENDEDOR
           ELSE IF WS-OPCAO            EQUAL 2
             PERFORM 2300-CRUD-VENDEDOR
           ELSE
             DISPLAY "OPCAO INVALIDA"  AT 2310
             PERFORM 2330-EXCLUIR-VENDEDOR
           END-IF
           .
       2330-99-FIM.                    EXIT.

       2340-IMPORTAR-VENDEDOR          SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-IMPORTACAO
           ACCEPT  TELA-IMPORTACAO
           IF WS-OPCAO                 EQUAL 1
             SET WS-IMPORTACAO         TO TRUE
             PERFORM 2350-ACESSAR-CRUDVEND
             DISPLAY BOOKVEND-CTRL-MSG AT 2310

             INITIALIZE                WS-VENDEDOR
             PERFORM 2340-IMPORTAR-VENDEDOR
           ELSE IF WS-OPCAO            EQUAL 2
             PERFORM 2300-CRUD-VENDEDOR
           ELSE
             DISPLAY "VALOR INVALIDO"  AT 2310
             PERFORM 2340-IMPORTAR-VENDEDOR
           END-IF
           .
       2340-99-FIM.                    EXIT.


       2350-ACESSAR-CRUDVEND           SECTION.
           INITIALIZE                  WS-LINK-CRUDVEND

           MOVE WS-OPCAO-CRUD          TO BOOKVEND-REG-SERVICO

           MOVE WS-CODIGO              OF WS-VENDEDOR
                                       TO BOOKVEND-REG-COD-VEND
           MOVE WS-CPF                 TO BOOKVEND-REG-CPF
           MOVE WS-RAZAO-SOCIAL        TO BOOKVEND-REG-NOME
           MOVE WS-LAT                 OF WS-VENDEDOR
                                       TO BOOKVEND-REG-LATITUDE
           MOVE WS-LONG                OF WS-VENDEDOR
                                       TO BOOKVEND-REG-LONGITUDE

           CALL WS-CRUDVEND            USING WS-LINK-CRUDVEND
           .
       2360-99-FIM.

       2400-MENU-RELATORIOS            SECTION.

           DISPLAY TELA-RELATORIOS
           ACCEPT  TELA-RELATORIOS

           IF WS-OPCAO                 EQUAL 1
             PERFORM 2410-RELATORIO-CLIENTE

           ELSE IF WS-OPCAO            EQUAL 2
             PERFORM 2420-RELATORIOS-VENDEDOR
           ELSE
             DISPLAY "VALOR INVALIDO"  AT 2310
             PERFORM 2400-MENU-RELATORIOS
           END-IF


           .
       2400-99-FIM.                    EXIT.

       2410-RELATORIO-CLIENTE          SECTION.

           DISPLAY TELA-RELATORIOS
           ACCEPT  TELA-RELATORIOS

           .
       2410-99-FIM.                    EXIT.

       2420-RELATORIOS-VENDEDOR        SECTION.

           DISPLAY TELA-RELATORIOS
           ACCEPT  TELA-RELATORIOS

           .
       2420-99-FIM.                    EXIT.


       3000-FINALIZAR                  SECTION.

             GOBACK
             .
       3000-99-FIM.                    EXIT.
