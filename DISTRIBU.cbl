       IDENTIFICATION                  DIVISION.

       PROGRAM-ID. DISTRIBU.
       AUTHOR.     RAMON CONRADO


       ENVIRONMENT                     DIVISION.

       CONFIGURATION                   SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

       INPUT-OUTPUT                    SECTION.

       FILE-CONTROL.

           SELECT ARQ-CLIENTE          ASSIGN TO DISK "CLIENTE.TXT"
                  ORGANIZATION         IS INDEXED
                  ACCESS MODE          IS DYNAMIC
                  RECORD KEY           IS FD-CLI-COD-CLIENTE
                  ALTERNATE RECORD KEY IS FD-CLI-CNPJ
                  LOCK MODE            IS MANUAL
                  FILE STATUS          IS WS-FS-ARQ-CLIE.

           SELECT ARQ-VENDEDOR         ASSIGN TO DISK "VENDEDOR.TXT"
                  ORGANIZATION         IS INDEXED
                  ACCESS MODE          IS DYNAMIC
                  RECORD KEY           IS FD-VEND-COD-VENDEDOR
                  ALTERNATE RECORD KEY IS FD-VEND-CPF
                  LOCK MODE            IS MANUAL
                  FILE STATUS          IS WS-FS-ARQ-VEND.

           SELECT ARQ-RESULT           ASSIGN TO DISK "RESULT.CSV"
                  ORGANIZATION         IS LINE SEQUENTIAL
                  ACCESS MODE          IS SEQUENTIAL
                  LOCK MODE            IS MANUAL
                  FILE STATUS          IS WS-FS-ARQ-RESULT.


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

       FD  ARQ-VENDEDOR
           LABEL RECORD                IS STANDARD.
       01  FD-VENDEDOR.
           05 FD-VEND-COD-VENDEDOR     PIC 9(003).
           05 FD-VEND-CPF              PIC 9(011).
           05 FD-VEND-NOME             PIC X(040).
           05 FD-VEND-LATITUDE         PIC S9(003)V9(008).
           05 FD-VEND-LONGITUDE        PIC S9(003)V9(008).

       FD  ARQ-RESULT
           LABEL RECORD                IS STANDARD.
       01  FD-RESULT.
           05 FD-RESULT-COD-CLIE       PIC 9(007).
           05 FILLER                   PIC X(001).
           05 FD-RESULT-RZ-SOCIAL      PIC X(040).
           05 FILLER                   PIC X(001).
           05 FD-RESULT-COD-VEND       PIC 9(003).
           05 FILLER                   PIC X(001).
           05 FD-RESULT-NOME           PIC X(040).
           05 FILLER                   PIC X(001).
           05 FD-RESULT-DISTANCIA      PIC 9(010)V9(008).

       WORKING-STORAGE                 SECTION.

       77  WS-FS-ARQ-CLIE              PIC X(002)          VALUE SPACES.
       77  WS-FS-ARQ-VEND              PIC X(002)          VALUE SPACES.
       77  WS-FS-ARQ-RESULT            PIC X(002)          VALUE SPACES.

       01  WS-LAT-CLI                  PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-LAT-VEN                  PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-LON-CLI                  PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-LON-VEN                  PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-DLA                      PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-DLO                      PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-A                        PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-C                        PIC S9(003)V9(008)  VALUE ZEROS.

       01  WS-RESULT.
           05 WS-RESULT-COD-CLIE       PIC 9(007)          VALUE ZEROS.
           05 FILLER                   PIC X(001)          VALUE ';'.
           05 WS-RESULT-RZ-SOCIAL      PIC X(040)          VALUE SPACES.
           05 FILLER                   PIC X(001)          VALUE ';'.
           05 WS-RESULT-COD-VEND       PIC 9(003)          VALUE ZEROS.
           05 FILLER                   PIC X(001)          VALUE ';'.
           05 WS-RESULT-NOME           PIC X(040)          VALUE SPACES.
           05 FILLER                   PIC X(001)          VALUE ';'.
           05 WS-RESULT-DISTANCIA      PIC 9(010)V9(008)   VALUE ZEROS.


       01  WS-CALC-DISTANCIA           PIC 9(010)V9(008)   VALUE ZEROS.
       01  WS-FIM-ARQ-IMPORTA          PIC X(001)          VALUE SPACES.
       01  WS-ENDER                    PIC X(040)          VALUE SPACES.


       LINKAGE                         SECTION.
       01  WS-LINKAGE-AREA.
           COPY 'BOOKDSTR.CPY'.

       PROCEDURE                       DIVISION USING WS-LINKAGE-AREA.


       0000-PRINCIPAL                  SECTION.

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 3000-FINALIZAR

           .
       0000-99-FIM.                    EXIT.

       1000-INICIALIZAR                SECTION.

           PERFORM 1110-ABRIR-ARQ-CLIENTE
           PERFORM 1140-ABRIR-ARQ-VENDEDOR
           PERFORM 1170-ABRIR-ARQ-RESULT
           .
       1000-99-FIM.                    EXIT.

       1110-ABRIR-ARQ-CLIENTE          SECTION.

           OPEN INPUT ARQ-CLIENTE

           IF WS-FS-ARQ-CLIE           EQUAL ZEROS OR 05
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKDSTR-CTRL-COD
             MOVE 'ERRO NA ABERTURA DO ARQUIVO CLIENTE'
                                       TO BOOKDSTR-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1110-99-FIM.                    EXIT.

       1120-LER-ARQ-CLIENTE            SECTION.

           READ ARQ-CLIENTE            INTO FD-CLIENTE

           IF WS-FS-ARQ-CLIE           EQUAL ZEROS OR
                                       EQUAL '10'
              CONTINUE
           ELSE
              MOVE 2                   TO BOOKDSTR-CTRL-COD
              MOVE "ERRO NA LEITURA DO ARQUIVO CLIENTE"
                                       TO BOOKDSTR-CTRL-MSG
              PERFORM 3000-FINALIZAR
           END-IF

           .
       1120-99-FIM.                    EXIT.


       1130-FECHAR-ARQ-CLIENTE         SECTION.

           CLOSE ARQ-CLIENTE

           IF WS-FS-ARQ-CLIE           EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKDSTR-CTRL-COD
             MOVE "ERRO NO FECHAMENTO DO ARQUIVO CLIENTE"
                                       TO BOOKDSTR-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1130-99-FIM.                    EXIT.

       1140-ABRIR-ARQ-VENDEDOR         SECTION.

           OPEN INPUT ARQ-VENDEDOR

           IF WS-FS-ARQ-VEND           EQUAL ZEROS OR 05
             CONTINUE
           ELSE
             MOVE 1                    TO BOOKDSTR-CTRL-COD
             MOVE 'ERRO NA ABERTURA DO ARQUIVO VENDEDOR'
                                       TO BOOKDSTR-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1140-99-FIM.                    EXIT.

       1150-LER-ARQ-VENDEDOR           SECTION.

           READ ARQ-VENDEDOR           INTO FD-VENDEDOR

           IF WS-FS-ARQ-VEND           EQUAL ZEROS OR
                                       EQUAL '10'
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKDSTR-CTRL-COD
             MOVE "ERRO NA LEITURA DO ARQUIVO VENDEDOR"
                                       TO BOOKDSTR-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1150-99-FIM.                    EXIT.

       1160-FECHAR-ARQ-VENDEDOR        SECTION.

           CLOSE ARQ-VENDEDOR

           IF WS-FS-ARQ-VEND           EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKDSTR-CTRL-COD
             MOVE "ERRO NO FECHAMENTO DO ARQUIVO VENDEDOR"
                                       TO BOOKDSTR-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1160-99-FIM.                    EXIT.

       1170-ABRIR-ARQ-RESULT           SECTION.

           OPEN OUTPUT ARQ-RESULT

           IF WS-FS-ARQ-RESULT         EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKDSTR-CTRL-COD
             MOVE "ERRO NO ABERTURA DO ARQUIVO RESULT"
                                       TO BOOKDSTR-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1170-99-FIM.

       1180-GRAVAR-ARQ-RESULT          SECTION.

           WRITE FD-RESULT

           IF WS-FS-ARQ-RESULT         EQUAL ZEROS
             CONTINUE
           ELSE
             MOVE 2                    TO BOOKDSTR-CTRL-COD
             MOVE "ERRO NA GRAVACAO DO ARQUIVO RESULT"
                                       TO BOOKDSTR-CTRL-MSG
             PERFORM 3000-FINALIZAR
           END-IF

           .
       1180-99-FIM.

       1190-FECHAR-ARQ-RESULT          SECTION.

           CLOSE ARQ-RESULT

           IF WS-FS-ARQ-RESULT         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 2                   TO BOOKDSTR-CTRL-COD
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO RESULT"
                                       TO BOOKDSTR-CTRL-MSG
              PERFORM 3000-FINALIZAR
           END-IF

           .
       1190-99-FIM.


       2000-PROCESSAR                  SECTION.

           PERFORM 2100-PROCESSAR-CLIENTE
                                       UNTIL WS-FS-ARQ-CLIE
                                       EQUAL '10'

           MOVE ZEROS                  TO BOOKDSTR-CTRL-COD
           MOVE "CARTEIRA GERADA COM SUCESSO"
                                       TO BOOKDSTR-CTRL-MSG

           .
       2000-99-FIM.                    EXIT.

       2100-PROCESSAR-CLIENTE          SECTION.

           PERFORM 1120-LER-ARQ-CLIENTE

           MOVE 9999999999             TO WS-RESULT-DISTANCIA

           PERFORM 2200-PROCESSAR-VENDEDOR
                                       UNTIL WS-FS-ARQ-VEND
                                       EQUAL '10'

           PERFORM 1180-GRAVAR-ARQ-RESULT

           .
       2100-99-FIM.                    EXIT.

       2200-PROCESSAR-VENDEDOR         SECTION.

           PERFORM 1150-LER-ARQ-VENDEDOR

           PERFORM 2300-CALCULA-DISTANCIA

           IF WS-RESULT-DISTANCIA      GREATER WS-CALC-DISTANCIA

             MOVE WS-RESULT            TO FD-RESULT

           END-IF

           .
       2200-99-FIM.                    EXIT.

       2300-CALCULA-DISTANCIA          SECTION.

           MOVE ZEROS                  TO WS-CALC-DISTANCIA

           COMPUTE WS-LAT-CLI          = FD-CLI-LATITUDE
                                       * FUNCTION PI
                                       / 180

           COMPUTE WS-LAT-VEN          = FD-VEND-LATITUDE
                                       * FUNCTION PI
                                       / 180
           COMPUTE WS-LON-CLI          = FD-CLI-LONGITUDE
                                       * FUNCTION PI
                                       / 180

           COMPUTE WS-LON-VEN          = FD-VEND-LONGITUDE
                                       * FUNCTION PI
                                       / 180

           COMPUTE WS-DLA              = WS-LAT-VEN - (WS-LAT-CLI)
           COMPUTE WS-DLO              = WS-LON-VEN - (WS-LON-CLI)
           COMPUTE WS-A                = FUNCTION SIN(WS-DLA / 2)
                                       * FUNCTION SIN(WS-DLA / 2)
                                       + FUNCTION COS(WS-LAT-CLI)
                                       * FUNCTION COS(WS-LAT-VEN)
                                       * FUNCTION SIN(WS-DLO / 2)
                                       * FUNCTION SIN(WS-DLO / 2)

           COMPUTE WS-C                = 2 * FUNCTION ATAN(
                                             FUNCTION SQRT(WS-A) /
                                             FUNCTION SQRT(1 - WS-A))

           COMPUTE WS-CALC-DISTANCIA   = 6731 * WS-C * 1000

           .
       2300-99-FIM.                    EXIT.

       3000-FINALIZAR                  SECTION.

           PERFORM 1130-FECHAR-ARQ-CLIENTE
           PERFORM 1160-FECHAR-ARQ-VENDEDOR
           PERFORM 1190-FECHAR-ARQ-RESULT

           GOBACK

           .
       3000-99-FIM.                    EXIT.
