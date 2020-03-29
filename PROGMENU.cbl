
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID. PROGMENU.
       AUTHOR.     RAMON CONRADO.


       ENVIRONMENT                     DIVISION.

       CONFIGURATION                   SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
           CONSOLE                     IS CRT.

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
           05 WS-ARQ-CLI      PIC X(040)          VALUE SPACES.

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

       01 WS-AUXILIARES.
              05 WS-TIPO-OPER          PIC X(020)          VALUE SPACES.

       SCREEN                          SECTION.

       01  TELA-MENU.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE
           "---------------------------------------------------------".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 20          VALUE "AMBEV - MENU PRINCIPAL".
           05  LINE 02 COL 58          VALUE "*".
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

       01  TELA-CADASTRO-CLIENTE.
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
              "CONFIRMA EXCLUIR DO CLIENTE?".
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
           05  LINE 14 COL 31          PIC X(001)
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
           "-----------------------------------------------------------".
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
              "CONFIRMA EXCLUIR DO VENDEDOR?".
           05  LINE 14 COL 31          PIC X(001)
                                       TO WS-OPCAO.
           05  LINE 15 COL 02          VALUE
              "1 - SIM ".
           05  LINE 16 COL 02          VALUE
              "2 - NAO ".

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
             WHEN 5
               PERFORM 2100-MENU-CADASTRO
             WHEN OTHER
               DISPLAY "OPCAO INVALIDA"
                                       AT 2310 
               PERFORM 2200-CRUD-CLIENTE
           END-EVALUATE
           .
       2110-99-FIM.                    EXIT.

       2210-INCLUIR-CLIENTE            SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-CADASTRO-CLIENTE
           ACCEPT  TELA-CADASTRO-CLIENTE
           .
       2112-99-FIM.                    EXIT.

       2220-ALTERAR-CLIENTE            SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-PESQUISAR-CLIENTE
           ACCEPT  TELA-PESQUISAR-CLIENTE

           .
       2114-99-FIM.                    EXIT.

       2230-EXCLUIR-CLIENTE           SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-EXCLUIR-CLIENTE
           ACCEPT  TELA-EXCLUIR-CLIENTE

           .
       2116-99-FIM.                    EXIT.

       2300-CRUD-VENDEDOR              SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-SUBMENU
           ACCEPT  TELA-SUBMENU

           .
       2120-99-FIM.                    EXIT.

       2310-INCLUIR-VENDEDOR           SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-INCLUIR-VENDEDOR
           ACCEPT  TELA-INCLUIR-VENDEDOR

           .
       2122-99-FIM.                    EXIT.

       2320-ALTERAR-VENDEDOR           SECTION.

           DISPLAY TELA-PESQUISAR-VENDEDOR
           ACCEPT  TELA-PESQUISAR-VENDEDOR

           .
       2124-99-FIM.                    EXIT.

       2330-EXCLUIR-VENDEDOR           SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-EXCLUIR-VENDEDOR
           ACCEPT  TELA-EXCLUIR-VENDEDOR
           .
       2126-99-FIM.                    EXIT.

       2340-IMPORTAR-VENDEDOR          SECTION.
           INITIALIZE                  WS-OPCAO

           DISPLAY TELA-IMPORTACAO
           ACCEPT  TELA-IMPORTACAO
           .
       2128-99-FIM.                    EXIT.

       3000-FINALIZAR                  SECTION.

             GOBACK
             .
       3000-99-FIM.                    EXIT.
