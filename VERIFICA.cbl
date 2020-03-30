       IDENTIFICATION                  DIVISION.

       PROGRAM-ID. VERIFICA.
       AUTHOR.     RAMON CONRADO


       ENVIRONMENT                     DIVISION.


       CONFIGURATION                   SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

       DATA                            DIVISION.


       WORKING-STORAGE                 SECTION.

       01  WS-TIPO-VALIDACAO           PIC X(001)          VALUE SPACES.
           88  WS-VALIDA-CPF                               VALUE '1'.
           88  WS-VALIDA-CNPJ                              VALUE '2'.

       01  WS-CPF                      PIC 9(011)          VALUE ZEROES.
       01  WS-CPF-R                    REDEFINES WS-CPF.
           05 WS-NUM-CPF               PIC 9(001)       OCCURS 11 TIMES.

       01  WS-CNPJ                     PIC 9(014)          VALUE ZEROES.
       01  WS-CNPJ-R                   REDEFINES WS-CNPJ.
           05 WS-NUM-CNPJ              PIC 9(001)       OCCURS 14 TIMES.

       01  WS-PESOS-CNPJ               PIC 9(012)
                                                     VALUE 543298765432.
       01  WS-PESOS-CNPJ-R             REDEFINES WS-PESOS-CNPJ.
           05 WS-NUM-PESOS             PIC 9(001)       OCCURS 12 TIMES.

       01  WS-IND-1                    PIC 9(002)          VALUE ZEROS.
       01  WS-IND-2                    PIC 9(002)          VALUE ZEROS.
       01  WS-SOMA                     PIC 9(003)          VALUE ZEROS.
       01  WS-QUOCIENTE                PIC 9(008)          VALUE ZEROS.
       01  WS-RESTO                    PIC 9(008)          VALUE ZEROS.
       01  WS-DIGITO1                  PIC 9(001)          VALUE ZEROS.
       01  WS-DIGITO2                  PIC 9(001)          VALUE ZEROS.

       LINKAGE                         SECTION.
       01  WS-LINKAGE-AREA.
           COPY 'BOOKVRIF.CPY'.

       PROCEDURE                       DIVISION USING WS-LINKAGE-AREA.

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

           DISPLAY  BOOKVRIF-REG-CPF
           IF BOOKVRIF-REG-CPF         NOT EQUAL SPACES
             MOVE '1'                  TO WS-TIPO-VALIDACAO
           END-IF

           IF BOOKVRIF-REG-CNPJ        NOT EQUAL SPACES
             MOVE '2'                  TO WS-TIPO-VALIDACAO
           END-IF
           .
       1100-99-FIM.                    EXIT.

       2000-PROCESSAR                  SECTION.

           IF WS-TIPO-VALIDACAO        EQUAL '1'
             PERFORM 2100-VALIDAR-CPF
           ELSE IF WS-TIPO-VALIDACAO   EQUAL '2'
             PERFORM 2200-VALIDAR-CNPJ
           ELSE
             MOVE 9                    TO BOOKVRIF-CTRL-COD
             MOVE 'ENTRADA INVALIDA'   TO BOOKVRIF-CTRL-MSG
           END-IF
           .
       2000-99-FIM.                    EXIT.

       2100-VALIDAR-CPF                SECTION.

           MOVE ZEROS                  TO WS-SOMA
           MOVE 1                      TO WS-IND-1
           MOVE 10                     TO WS-IND-2
           MOVE BOOKVRIF-REG-CPF       TO WS-CPF

           PERFORM                     UNTIL WS-IND-1
                                       GREATER 9
             COMPUTE WS-SOMA = WS-SOMA + (
                               WS-NUM-CPF(WS-IND-1) * WS-IND-2
                               )
             SUBTRACT 1                FROM WS-IND-2
             ADD 1                     TO WS-IND-1
           END-PERFORM

           DIVIDE WS-SOMA BY 11        GIVING WS-QUOCIENTE
                                       REMAINDER WS-RESTO

           COMPUTE WS-RESTO = 11 - WS-RESTO

           IF WS-RESTO                 GREATER 9
             MOVE ZEROS                TO WS-DIGITO1
           ELSE
             MOVE WS-RESTO             TO WS-DIGITO1
           END-IF

           MOVE ZEROS                  TO WS-SOMA
           MOVE 1                      TO WS-IND-1
           MOVE 11                     TO WS-IND-2
           MOVE WS-DIGITO1             TO WS-NUM-CPF(10)

           PERFORM                     UNTIL WS-IND-1
                                       GREATER 10
             COMPUTE WS-SOMA = WS-SOMA + (
                               WS-NUM-CPF(WS-IND-1) * WS-IND-2
                               )
             SUBTRACT 1                FROM WS-IND-2
             ADD 1                     TO WS-IND-1
           END-PERFORM

           DIVIDE WS-SOMA BY 11        GIVING WS-QUOCIENTE
                                       REMAINDER WS-RESTO

           COMPUTE WS-RESTO = 11 - WS-RESTO

           IF WS-RESTO                 GREATER 9
             MOVE ZEROS                TO WS-DIGITO2
           ELSE
             MOVE WS-RESTO             TO WS-DIGITO2
           END-IF

           MOVE WS-DIGITO2             TO WS-NUM-CPF(11)

           IF WS-CPF                   EQUAL BOOKVRIF-REG-CPF
             MOVE ZEROS                TO BOOKVRIF-CTRL-COD
             MOVE 'CPF VALIDO'         TO BOOKVRIF-CTRL-MSG
           ELSE
             MOVE 1                    TO BOOKVRIF-CTRL-COD
             MOVE 'CPF INVALIDO'       TO BOOKVRIF-CTRL-MSG
           END-IF

           .
       2100-99-FIM.

       2200-VALIDAR-CNPJ               SECTION.

           MOVE ZEROS                  TO WS-SOMA
           MOVE 1                      TO WS-IND-1
           MOVE BOOKVRIF-REG-CPF       TO WS-CPF

           PERFORM                     UNTIL WS-IND-1
                                       GREATER 12
             COMPUTE WS-SOMA = WS-SOMA + (
                               WS-NUM-CNPJ(WS-IND-1) *
                               WS-NUM-PESOS(WS-IND-1))
             ADD 1                     TO WS-IND-1
           END-PERFORM

           DIVIDE WS-SOMA BY 11        GIVING WS-QUOCIENTE
                                       REMAINDER WS-RESTO

           COMPUTE WS-RESTO = 11 - WS-RESTO

           IF WS-RESTO                 GREATER 9
             MOVE ZEROS                TO WS-DIGITO1
           ELSE
             MOVE WS-RESTO             TO WS-DIGITO1
           END-IF

           MOVE ZEROS                  TO WS-SOMA
           MOVE 1                      TO WS-IND-1
           MOVE WS-DIGITO1             TO WS-NUM-CNPJ(13)

           PERFORM                     UNTIL WS-IND-1
                                       GREATER 13
             COMPUTE WS-SOMA = WS-SOMA + (
                               WS-NUM-CNPJ(WS-IND-1) *
                               WS-NUM-PESOS(WS-IND-1))
             ADD 1                     TO WS-IND-1
           END-PERFORM

           DIVIDE WS-SOMA BY 11        GIVING WS-QUOCIENTE
                                       REMAINDER WS-RESTO

           COMPUTE WS-RESTO = 11 - WS-RESTO

           IF WS-RESTO                 GREATER 9
             MOVE ZEROS                TO WS-DIGITO2
           ELSE
             MOVE WS-RESTO             TO WS-DIGITO2
           END-IF

           MOVE WS-DIGITO2             TO WS-NUM-CNPJ(14)

           IF WS-CPF                   EQUAL BOOKVRIF-REG-CPF
             MOVE ZEROS                TO BOOKVRIF-CTRL-COD
             MOVE 'CNPJ VALIDO'        TO BOOKVRIF-CTRL-MSG
           ELSE
             MOVE 1                    TO BOOKVRIF-CTRL-COD
             MOVE 'CNPJ INVALIDO'      TO BOOKVRIF-CTRL-MSG
           END-IF
           .
       2200-99-FIM.

       3000-FINALIZAR                  SECTION.

           GOBACK
           .
       3000-99-FIM.                    EXIT.
