       05  BOOKVEND-CTRL.
           10 BOOKVEND-CTRL-COD        PIC 9(001)          VALUE ZEROS.
           10 BOOKVEND-CTRL-MSG        PIC X(040)          VALUE SPACES.
       05  BOOKVEND-REG.
           10 BOOKVEND-REG-SERVICO     PIC X(001)          VALUE SPACES.
           10 BOOKVEND-REG-COD-VEND    PIC 9(003)          VALUE ZEROS.
           10 BOOKVEND-REG-CPF         PIC 9(011)          VALUE ZEROS.
           10 BOOKVEND-REG-NOME        PIC X(040)          VALUE SPACES.
           10 BOOKVEND-REG-LATITUDE    PIC S9(003)V9(008)  VALUE ZEROS.
           10 BOOKVEND-REG-LONGITUDE   PIC S9(003)V9(008)  VALUE ZEROS.
           10 BOOKVEND-REG-ARQUVO      PIC X(040)          VALUE SPACES.