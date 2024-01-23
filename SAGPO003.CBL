      *===============================================================*
       IDENTIFICATION              DIVISION.
      *---------------------------------------------------------------*
      *
       PROGRAM-ID.                 SAGPO003.
       AUTHOR.                     ULISSES & MORAES (TI).
       DATE-WRITTEN.               11/03/2012.
       SECURITY.
      *
      *===============================================================*
      *                                                               *
      *   SISTEMA       : SAG - SISTEMA DE GESTAO DE ALUNOS.          *
      *   PROGRAMA      : SAGPO003                                    *
      *   TRANSACAO     : GA03                                        *
      *   LINGUAGEM     : COBOL / CICS / DB2                          *
      *   PROGRAMADOR   : ULISSES & MORAES (TI)                       *
      *   ANALISTA      : ULISSES & MORAES (TI)                       *
      *   DATA          : 11/03/2012                                  *
      *                                                               *
      *   OBJETIVO      : REALIZAR INCLUSAO NO CADASTRO DE ALUNOS     *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *   ROTINAS                                                     *
      *                                                               *
      *   NOME             BOOK     DESCRICAO                         *
      *   ---------------- -------- ----------------------------------*
      *   XXXXXXXX         XXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *   DB2                                                         *
      *                                                               *
      *   NOME               BOOK     DESCRICAO                       *
      *   -----------------  -------- --------------------------------*
      *   XXXXXXXXXXXXXXXXX  XXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *---------------------------------------------------------------*
      *   HISTORICO DE ALTERACOES                                     *
      *---------------------------------------------------------------*
      *                                                               *
      *   PROGRAMADOR    : XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *   ANALISTA       : XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *   DATA           : 99/99/9999                                 *
      *                                                               *
      *   OBJETIVO       : XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *===============================================================*

      *===============================================================*
       ENVIRONMENT                 DIVISION.
      *---------------------------------------------------------------*
       CONFIGURATION               SECTION.
      *---------------------------------------------------------------*
       SPECIAL-NAMES.              DECIMAL-POINT   IS   COMMA.
      *---------------------------------------------------------------*

      *===============================================================*
       DATA                        DIVISION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       WORKING-STORAGE             SECTION.
      *---------------------------------------------------------------*
      *
       01      FILLER              PIC     X(040)  VALUE
              '** INICIO WORKING SAGPO003 **'.
      *
      /**-----------------------------------------------------------***
      ***      AREAS AUXILIARES                                     ***
      ***-----------------------------------------------------------***
      *
       77      WS-ANO-CICS         PIC    S9(008) COMP  VALUE +0.
       77      WS-ABSTIME          PIC    S9(015) COMP  VALUE +0.
       77      WS-HRS-EDIT         PIC     X(008) VALUE SPACES.
      *
       77      WS-ABCODE           PIC     X(004) VALUE SPACES.
       77      WS-EIBRESP          PIC     9(003) VALUE ZEROS.
      *
       77      WS-COMANDO-DB2      PIC     X(006) VALUE SPACES.
       77      WS-TABELAS-DB2      PIC     X(008) VALUE SPACES.
       77      WS-SQLCODE-DB2      PIC     +++9.
      *
       77      WS-MSG              PIC     X(079) VALUE SPACES.
      *
       77      WS-TRANS            PIC     X(004) VALUE SPACES.
       77      WS-COMM-TRANS       PIC     X(004) VALUE SPACES.
      *
      /**-----------------------------------------------------------***
      ***      TRATAMENTO DE CHAMADA DE SUBROTINAS                  ***
      ***-----------------------------------------------------------***
      *
       01      WS-SAGBBDIG         PIC     X(008) VALUE 'SAGBBDIG'.
       01      WS-SAGBB006         PIC     X(008) VALUE 'SAGBB006'.
       01      WS-SAGBB028         PIC     X(008) VALUE 'SAGBB028'.
      *
      /**-----------------------------------------------------------***
      ***      AREA DE TRATAMENTO DE DATA                           ***
      ***-----------------------------------------------------------***
      *
       01      WS-DT-EDIT.
         03    WS-DAT-EDIT.
           05  WS-DD-EDIT          PIC     9(002).
           05  FILLER              PIC     X(001).
           05  WS-MM-EDIT          PIC     9(002).
           05  FILLER              PIC     X(001).
           05  WS-AA-EDIT          PIC     X(002).
         03    FILLER              PIC     X(002).
       01      FILLER              REDEFINES      WS-DT-EDIT.
         03    FILLER              PIC     X(006).
         03    WS-SA-EDIT          PIC     9(004).
      *
       01      WS-DAT-DB2          PIC     X(010) VALUE '99.99.9999'.
       01      FILLER              REDEFINES      WS-DAT-DB2.
         03    WS-DIA-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-MES-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-ANO-DB2          PIC     9(004).
      *
      /**-----------------------------------------------------------***
      ***      TABELA INTERNA PARA TRATAMENTO DE ALF X NUM          ***
      ***-----------------------------------------------------------***
      *
       01      TB-ALF-IND          PIC     9(003) VALUE ZEROS.
       01      TB-NUM-IND          PIC     9(003) VALUE ZEROS.
      *
       01      TB-ALF-018          PIC     X(018) VALUE ZEROS.
       01      FILLER              REDEFINES      TB-ALF-018.
         03    TB-ALF-OCCURS       OCCURS  18  TIMES.
           05  TB-ALF-BYTE         PIC     X(001).
      *
       01      TB-NUM-018          PIC     X(018) VALUE ZEROS.
       01      FILLER              REDEFINES      TB-NUM-018.
         03    TB-NUM-OCCURS       OCCURS  18  TIMES.
           05  TB-NUM-BYTE         PIC     X(001).
      *
       01      WS-CON-NUM          PIC     X(018).
       01      FILLER              REDEFINES      WS-CON-NUM.
         03    FILLER              PIC     X(016).
         03    WS-CON-002          PIC     X(002).
      *
       01      FILLER              REDEFINES      WS-CON-NUM.
         03    FILLER              PIC     X(015).
         03    WS-CON-003          PIC     X(003).
      *
       01      FILLER              REDEFINES      WS-CON-NUM.
         03    FILLER              PIC     X(014).
         03    WS-CON-004          PIC     X(004).
      *
      /**-----------------------------------------------------------***
      ***      AREA DE CRITICA CPF ZERADO                           ***
      ***-----------------------------------------------------------***
      *
       01      WS-CPF-00           PIC     9(011) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-CPF-00.
         03    WS-CPF-01           PIC     9(003).
         03    WS-CPF-02           PIC     9(003).
         03    WS-CPF-03           PIC     9(003).
         03    WS-CPF-DV           PIC     9(002).
      *
       01      WS-CPF-11           PIC     9(011) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-CPF-11.
         03    WS-CPF-09           PIC     9(009).
         03    WS-CPF-DG           PIC     9(002).
      *
      /**-----------------------------------------------------------***
      ***      AREA DE CRITICA CPF INVALIDO                         ***
      ***-----------------------------------------------------------***
      *
       01      WS-CPF-VAL          PIC     9(011) VALUE ZEROS.
         88    WS-CPF-88           VALUE   11111111111
                                           22222222222
                                           33333333333
                                           44444444444
                                           55555555555
                                           66666666666
                                           77777777777
                                           88888888888
                                           99999999999
                                           12345678909.
      *
      /**-----------------------------------------------------------***
      ***      TRATAMENTO DO CAMPO TELEFONE                         ***
      ***-----------------------------------------------------------***
      *
       01      WS-TEL-00           PIC     9(008) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-TEL-00.
         03    WS-TEL-01           PIC     9(004).
         03    WS-TEL-02           PIC     9(002).
         03    WS-TEL-03           PIC     9(002).
      *
      /**-----------------------------------------------------------***
      ***      TRATAMENTO DE NULIDADE DE CAMPOS                     ***
      ***-----------------------------------------------------------***
      *
       01     WS-CO-LOCAL-CEL-NULL PIC    S9(004) COMP.
       01     WS-NU-TELEF-CEL-NULL PIC    S9(004) COMP.
       01     WS-NO-E-MAIL-NULL    PIC    S9(004) COMP.
       01     WS-NO-OBS-NULL       PIC    S9(004) COMP.
      *
      /**-----------------------------------------------------------***
      ***      AREA DE MENSAGENS                                    ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGWSMSG.
      *
      /**-----------------------------------------------------------***
      ***      AREA DE COMMAREA                                     ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGWSCOM.
      *
      /**-----------------------------------------------------------***
      ***          AREA DE DEFINICAO DO MAPA - MAPSET (SAGM003)     ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGM003.
      *
      /**-----------------------------------------------------------***
      ***          SAGBB006 - CONSISTENCIA DE DATAS                 ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGWS006.
      *
      /**-----------------------------------------------------------***
      ***          SAGBBDIG - CALCULA DIGITO VERIFICADOR            ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGWSDIG    REPLACING  ==:RUCWS:==  BY  ==RUCWS==.
      *
      /**-----------------------------------------------------------***
      ***          SAGBB028 - INCLUI REGISTRO ALUNO                 ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGWS028.
      *
      /**-----------------------------------------------------------***
      ***          AREA DO BOOK DE AREAS DO CICS                    ***
      ***-----------------------------------------------------------***
      *
           COPY    DFHAID.
      *
      /**-----------------------------------------------------------***
      ***          AREA DO BOOK DE ATRIBUTOS                        ***
      ***-----------------------------------------------------------***
      *
           COPY    DFHBMSCA.
      *
      /**-----------------------------------------------------------***
      ***          AREA DE TRATAMENTO DB2                           ***
      ***-----------------------------------------------------------***
      *
           EXEC    SQL
                   INCLUDE SQLCA
           END-EXEC.
      /
           EXEC    SQL
                   INCLUDE SAGTBS01
           END-EXEC.
      *
      *---------------------------------------------------------------*
       01          FILLER          PIC     X(040)  VALUE
                  '** FINAL WORKING SAGPO003 **'.
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       LINKAGE                     SECTION.
      *---------------------------------------------------------------*
      *
       01          DFHCOMMAREA.
         03        FILLER          PIC     X(001)
                   OCCURS  1000  DEPENDING  ON  EIBCALEN.
      *
      *===============================================================*
       PROCEDURE                   DIVISION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    HANDLE  ABEND
                                   LABEL   (997-99-ABCODE)
           END-EXEC.

           EXEC    CICS    HANDLE  CONDITION
                                   ERROR   (998-99-ERROR)
           END-EXEC.

           IF      EIBAID          EQUAL   DFHPF12
                   PERFORM         030-00-DISCONNECT
           END-IF.

           IF      EIBAID          EQUAL   DFHPF4
                   PERFORM         050-00-RETORNA-MENU
           END-IF.

           IF      EIBAID          EQUAL   DFHCLEAR OR
                   EIBAID          EQUAL   DFHPA1   OR
                   EIBAID          EQUAL   DFHPA2
                   MOVE WS-MSG-094 TO      WS-MSG
                   PERFORM         020-00-ENCERRA-TRANSACAO
           END-IF.

           IF      EIBCALEN        EQUAL   ZEROS
                   PERFORM         005-00-RETRIEVE
                   PERFORM         100-00-INICIALIZA-TELA
           END-IF.

           MOVE    DFHCOMMAREA     TO      COMM-COMMAREA.

           IF      COMM-FASE       EQUAL   9
                   MOVE WS-MSG-094 TO      WS-MSG
                   PERFORM         020-00-ENCERRA-TRANSACAO
           END-IF.

           IF      COMM-FASE       EQUAL   2 AND
                   EIBAID          EQUAL   DFHPF2
                   PERFORM         300-00-TRATA-PF2
           END-IF.

           IF      EIBAID          EQUAL   DFHPF3
                   PERFORM         040-00-RETORNA-CHAMADOR
           END-IF.

           IF      EIBAID          EQUAL   DFHPF5 OR
                   COMM-FASE       EQUAL   3
                   PERFORM         100-00-INICIALIZA-TELA
           END-IF.

           IF      COMM-FASE       EQUAL   1 AND
                   EIBAID          EQUAL   DFHENTER
                   PERFORM         110-00-TRATA-FASE-1
           END-IF.

           IF      COMM-FASE       EQUAL   2 AND
                   EIBAID          EQUAL   DFHENTER
                   PERFORM         160-00-TRATA-FASE-2
           END-IF.

           IF      COMM-FASE       EQUAL   2 AND
                   EIBAID          EQUAL   DFHPF6
                   PERFORM         200-00-TRATA-PF6
           END-IF.

           PERFORM 690-00-TECLA-INVALIDA.
      *
      /===============================================================*
       005-00-RETRIEVE             SECTION.
      *---------------------------------------------------------------*
      *
           EXEC   CICS   RETRIEVE  INTO   (COMM-COMMAREA)
                                   LENGTH (LENGTH OF COMM-COMMAREA)
                                   NOHANDLE
           END-EXEC.

           IF      EIBRESP     NOT EQUAL   DFHRESP (NORMAL)
                   MOVE WS-MSG-095 TO      WS-MSG
                   PERFORM         020-00-ENCERRA-TRANSACAO
           END-IF.
      *
      *---------------------------------------------------------------*
       005-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       020-00-ENCERRA-TRANSACAO    SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    SEND    FROM    (WS-MSG)
                                   LENGTH  (+79)
                                   ERASE
           END-EXEC.

           EXEC    CICS    RETURN

           END-EXEC.
      *
      *---------------------------------------------------------------*
       020-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       030-00-DISCONNECT           SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    ISSUE   DISCONNECT

           END-EXEC.

           EXEC    CICS    RETURN

           END-EXEC.
      *
      *---------------------------------------------------------------*
       030-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       040-00-RETORNA-CHAMADOR     SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    COMM-TRANS(COMM-IND)
                                   TO      WS-COMM-TRANS.

           MOVE    SPACES          TO      COMM-TRANS(COMM-IND).

           SUBTRACT 1              FROM    COMM-IND.

           EXEC    CICS    START   TRANSID (WS-COMM-TRANS)
                                   TERMID  (EIBTRMID)
                                   FROM    (COMM-COMMAREA)
                                   LENGTH  (LENGTH OF COMM-COMMAREA)
           END-EXEC.

           EXEC    CICS    RETURN

           END-EXEC.
      *
      *---------------------------------------------------------------*
       040-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       050-00-RETORNA-MENU         SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    START   TRANSID ('GA01')
                                   TERMID  (EIBTRMID)
           END-EXEC.

           EXEC    CICS    RETURN

           END-EXEC.
      *
      *---------------------------------------------------------------*
       050-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       100-00-INICIALIZA-TELA      SECTION.
      *---------------------------------------------------------------*
      *
           MOVE   'A'              TO      M003-NO-ALUNOA
                                           M003-SEXOA
                                           M003-E-MAILA
                                           M003-OBSA.

           MOVE   'J'              TO      M003-DIA-NASCA
                                           M003-MES-NASCA
                                           M003-ANO-NASCA
                                           M003-NUM-CPF1A
                                           M003-NUM-CPF2A
                                           M003-NUM-CPF3A
                                           M003-DIG-CPFA
                                           M003-LCL-CELA
                                           M003-TEL-CEL1A
                                           M003-TEL-CEL2A
                                           M003-TEL-CEL3A.

           MOVE    ALL '_'         TO      M003-NO-ALUNOI
                                           M003-DIA-NASCI
                                           M003-MES-NASCI
                                           M003-ANO-NASCI
                                           M003-SEXOI
                                           M003-NUM-CPF1I
                                           M003-NUM-CPF2I
                                           M003-NUM-CPF3I
                                           M003-DIG-CPFI
                                           M003-LCL-CELI
                                           M003-TEL-CEL1I
                                           M003-TEL-CEL2I
                                           M003-TEL-CEL3I
                                           M003-E-MAILI
                                           M003-OBSI.

           MOVE    1               TO      COMM-FASE.

           MOVE   -1               TO      M003-NO-ALUNOL.

           MOVE    WS-MSG-005      TO      M003-MSGO.

           PERFORM 810-00-SEND-ERASE-SAGM003.
      *
      *---------------------------------------------------------------*
       100-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       110-00-TRATA-FASE-1         SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 800-00-RECEIVE-MAP.

           MOVE   'A'              TO      M003-NO-ALUNOA
                                           M003-SEXOA
                                           M003-E-MAILA
                                           M003-OBSA.

           MOVE   'J'              TO      M003-DIA-NASCA
                                           M003-MES-NASCA
                                           M003-ANO-NASCA
                                           M003-NUM-CPF1A
                                           M003-NUM-CPF2A
                                           M003-NUM-CPF3A
                                           M003-DIG-CPFA
                                           M003-LCL-CELA
                                           M003-TEL-CEL1A
                                           M003-TEL-CEL2A
                                           M003-TEL-CEL3A.

           INSPECT M003-NO-ALUNOI  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-NO-ALUNOI  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-DIA-NASCI  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-DIA-NASCI  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-MES-NASCI  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-MES-NASCI  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-ANO-NASCI  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-ANO-NASCI  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-SEXOI      REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-SEXOI      REPLACING ALL '_'        BY SPACES.

           INSPECT M003-NUM-CPF1I  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-NUM-CPF1I  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-NUM-CPF2I  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-NUM-CPF2I  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-NUM-CPF3I  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-NUM-CPF3I  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-DIG-CPFI   REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-DIG-CPFI   REPLACING ALL '_'        BY SPACES.

           INSPECT M003-LCL-CELI   REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-LCL-CELI   REPLACING ALL '_'        BY SPACES.

           INSPECT M003-TEL-CEL1I  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-TEL-CEL1I  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-TEL-CEL2I  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-TEL-CEL2I  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-TEL-CEL3I  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-TEL-CEL3I  REPLACING ALL '_'        BY SPACES.

           INSPECT M003-E-MAILI    REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-E-MAILI    REPLACING ALL '_'        BY SPACES.

           INSPECT M003-OBSI       REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M003-OBSI       REPLACING ALL '_'        BY SPACES.

           IF      M003-NO-ALUNOI  EQUAL   SPACES
                   MOVE 'I'        TO      M003-NO-ALUNOA
                   MOVE -1         TO      M003-NO-ALUNOL
                   MOVE WS-MSG-032 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-NO-ALUNOI  NOT     ALPHABETIC
                   MOVE 'I'        TO      M003-NO-ALUNOA
                   MOVE -1         TO      M003-NO-ALUNOL
                   MOVE WS-MSG-052 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-DIA-NASCI  EQUAL   SPACES
                   MOVE 'R'        TO      M003-DIA-NASCA
                   MOVE -1         TO      M003-DIA-NASCL
                   MOVE WS-MSG-034 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-DIA-NASCI  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-002      TO      M003-DIA-NASCI.

           IF      M003-DIA-NASCO  NOT     NUMERIC OR
                   M003-DIA-NASCO  EQUAL   ZEROS   OR
                   M003-DIA-NASCO  GREATER 31
                   MOVE 'R'        TO      M003-DIA-NASCA
                   MOVE -1         TO      M003-DIA-NASCL
                   MOVE WS-MSG-035 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-MES-NASCI  EQUAL   SPACES
                   MOVE 'R'        TO      M003-MES-NASCA
                   MOVE -1         TO      M003-MES-NASCL
                   MOVE WS-MSG-034 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-MES-NASCI  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-002      TO      M003-MES-NASCI.

           IF      M003-MES-NASCO  NOT     NUMERIC OR
                   M003-MES-NASCO  EQUAL   ZEROS   OR
                   M003-MES-NASCO  GREATER 12
                   MOVE 'R'        TO      M003-MES-NASCA
                   MOVE -1         TO      M003-MES-NASCL
                   MOVE WS-MSG-035 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-ANO-NASCI  EQUAL   SPACES
                   MOVE 'R'        TO      M003-ANO-NASCA
                   MOVE -1         TO      M003-ANO-NASCL
                   MOVE WS-MSG-034 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-ANO-NASCI  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-004      TO      M003-ANO-NASCI.

           IF      M003-ANO-NASCO  NOT     NUMERIC OR
                   M003-ANO-NASCO  EQUAL   ZEROS   OR
                   M003-ANO-NASCO  LESS    1900
                   MOVE 'R'        TO      M003-ANO-NASCA
                   MOVE -1         TO      M003-ANO-NASCL
                   MOVE WS-MSG-035 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           PERFORM 120-00-LINK-SAGBB006.

           IF      M003-SEXOI  NOT EQUAL  'M' AND 'F'
                   MOVE 'I'        TO      M003-SEXOA
                   MOVE -1         TO      M003-SEXOL
                   MOVE WS-MSG-036 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-NUM-CPF1I  EQUAL   SPACES
                   MOVE 'R'        TO      M003-NUM-CPF1A
                   MOVE -1         TO      M003-NUM-CPF1L
                   MOVE WS-MSG-034 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-NUM-CPF1I  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-003      TO      M003-NUM-CPF1I.

           IF      M003-NUM-CPF1O  NOT     NUMERIC
                   MOVE 'R'        TO      M003-NUM-CPF1A
                   MOVE -1         TO      M003-NUM-CPF1L
                   MOVE WS-MSG-035 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-NUM-CPF2I  EQUAL   SPACES
                   MOVE 'R'        TO      M003-NUM-CPF2A
                   MOVE -1         TO      M003-NUM-CPF2L
                   MOVE WS-MSG-034 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-NUM-CPF2I  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-003      TO      M003-NUM-CPF2I.

           IF      M003-NUM-CPF2O  NOT     NUMERIC
                   MOVE 'R'        TO      M003-NUM-CPF2A
                   MOVE -1         TO      M003-NUM-CPF2L
                   MOVE WS-MSG-035 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-NUM-CPF3I  EQUAL   SPACES
                   MOVE 'R'        TO      M003-NUM-CPF3A
                   MOVE -1         TO      M003-NUM-CPF3L
                   MOVE WS-MSG-034 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-NUM-CPF3I  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-003      TO      M003-NUM-CPF3I.

           IF      M003-NUM-CPF3O  NOT     NUMERIC
                   MOVE 'R'        TO      M003-NUM-CPF3A
                   MOVE -1         TO      M003-NUM-CPF3L
                   MOVE WS-MSG-035 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-DIG-CPFI   EQUAL   SPACES
                   MOVE 'R'        TO      M003-DIG-CPFA
                   MOVE -1         TO      M003-DIG-CPFL
                   MOVE WS-MSG-034 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-DIG-CPFI   TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-002      TO      M003-DIG-CPFI.

           IF      M003-DIG-CPFO   NOT     NUMERIC
                   MOVE 'R'        TO      M003-NUM-CPF3A
                   MOVE -1         TO      M003-NUM-CPF3L
                   MOVE WS-MSG-035 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-NUM-CPF1O  TO      WS-CPF-01.
           MOVE    M003-NUM-CPF2O  TO      WS-CPF-02.
           MOVE    M003-NUM-CPF3O  TO      WS-CPF-03.
           MOVE    M003-DIG-CPFO   TO      WS-CPF-DV.

           IF      WS-CPF-00       EQUAL   ZEROS
                   MOVE 'R'        TO      M003-NUM-CPF1A
                                           M003-NUM-CPF2A
                                           M003-NUM-CPF3A
                                           M003-DIG-CPFA
                   MOVE -1         TO      M003-NUM-CPF1L
                   MOVE WS-MSG-061 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    WS-CPF-00       TO      WS-CPF-VAL.

           IF      WS-CPF-88
                   MOVE 'R'        TO      M003-NUM-CPF1A
                                           M003-NUM-CPF2A
                                           M003-NUM-CPF3A
                                           M003-DIG-CPFA
                   MOVE -1         TO      M003-NUM-CPF1L
                   MOVE WS-MSG-061 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           PERFORM 140-00-LINK-SAGBBDIG.

           IF     M003-LCL-CELI  NOT EQUAL SPACES OR
                  M003-TEL-CEL1I NOT EQUAL SPACES OR
                  M003-TEL-CEL2I NOT EQUAL SPACES OR
                  M003-TEL-CEL3I NOT EQUAL SPACES
                  PERFORM      150-00-CRITICA-TELEFONE
           END-IF.

           MOVE    2               TO      COMM-FASE.

           MOVE   -1               TO      M003-MSGL.

           MOVE    WS-MSG-006      TO      M003-MSGO.

           PERFORM 820-00-SEND-DTONLY-SAGM003.
      *
      *---------------------------------------------------------------*
       110-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       120-00-LINK-SAGBB006        SECTION.
      *---------------------------------------------------------------*
      *
           MOVE   'C'              TO      WRD-CODOPE.

           MOVE    M003-DIA-NASCO  TO      WRD-DATA01-DD.
           MOVE    M003-MES-NASCO  TO      WRD-DATA01-MM.
           MOVE    M003-ANO-NASCO  TO      WRD-DATA01-AA.

           MOVE    00              TO      WRD-CODRET.

           EXEC    CICS    LINK    PROGRAM (WS-SAGBB006)
                                   COMMAREA(WRD-GRUPO)
                                   LENGTH  (LENGTH OF WRD-GRUPO)
           END-EXEC.

           IF      WRD-CODRET  NOT EQUAL   00 AND 91 AND 92
                   MOVE WS-SAGBB006 TO     WS-MSG-096 (33:08)
                   MOVE WRD-CODRET  TO     WS-MSG-096 (59:02)
                   PERFORM         996-00-ABEND-SUB
           END-IF.

           IF      WRD-CODRET  NOT EQUAL   00
                   MOVE 'R'        TO      M003-DIA-NASCA
                                           M003-MES-NASCA
                                           M003-ANO-NASCA
                   MOVE -1         TO      M003-DIA-NASCL
                   MOVE WS-MSG-033 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.
      *
      *---------------------------------------------------------------*
       120-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       0130-00-CONVERTE-ALF-NUM       SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    018             TO      TB-ALF-IND.
           MOVE    018             TO      TB-NUM-IND.

           MOVE    ZEROS           TO      TB-NUM-018.

           PERFORM VARYING TB-ALF-IND FROM  018 BY -1
                   UNTIL   TB-ALF-IND EQUAL ZEROS

            IF     TB-ALF-BYTE (TB-ALF-IND)
                               NOT EQUAL    SPACES
                   MOVE TB-ALF-BYTE (TB-ALF-IND)
                                   TO      TB-NUM-BYTE (TB-NUM-IND)
                   SUBTRACT 001    FROM    TB-NUM-IND
            END-IF

           END-PERFORM.

           MOVE    TB-NUM-018      TO      WS-CON-NUM.
      *
      *---------------------------------------------------------------*
       130-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       140-00-LINK-SAGBBDIG        SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    02              TO      RUCWS-ACAO.

           MOVE    WS-CPF-00       TO      WS-CPF-11.

           MOVE    WS-CPF-09       TO      RUCWS-NU-CPF.
           MOVE    00              TO      RUCWS-DV-CPF.

           MOVE    00              TO      RUCWS-CODIGO-RETORNO.

           EXEC    CICS    LINK    PROGRAM (WS-SAGBBDIG)
                                   COMMAREA(RUCWSDIG)
                                   LENGTH  (LENGTH OF RUCWSDIG)
           END-EXEC.

           IF      RUCWS-CODIGO-RETORNO
                               NOT EQUAL   00
                   MOVE WS-SAGBBDIG
                                   TO      WS-MSG-096 (33:08)
                   MOVE RUCWS-CODIGO-RETORNO
                                   TO      WS-MSG-096 (59:02)
                   PERFORM         996-00-ABEND-SUB
           END-IF.

           IF      WS-CPF-DG   NOT EQUAL   RUCWS-DV-CPF
                   MOVE 'R'        TO      M003-DIG-CPFA
                   MOVE -1         TO      M003-DIG-CPFL
                   MOVE WS-MSG-037 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.
      *
      *---------------------------------------------------------------*
       140-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       150-00-CRITICA-TELEFONE     SECTION.
      *---------------------------------------------------------------*
      *
           IF      M003-LCL-CELI   EQUAL   SPACES
                   MOVE 'R'        TO      M003-LCL-CELA
                   MOVE -1         TO      M003-LCL-CELL
                   MOVE WS-MSG-063 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-LCL-CELI   TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-003      TO      M003-LCL-CELI.

           IF      M003-LCL-CELO   NOT     NUMERIC OR
                   M003-LCL-CELO   EQUAL   ZEROS
                   MOVE 'R'        TO      M003-LCL-CELA
                   MOVE -1         TO      M003-LCL-CELL
                   MOVE WS-MSG-063 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-TEL-CEL1I  EQUAL   SPACES
                   MOVE 'R'        TO      M003-TEL-CEL1A
                   MOVE -1         TO      M003-TEL-CEL1L
                   MOVE WS-MSG-053 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-TEL-CEL1I  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-004      TO      M003-TEL-CEL1I.

           IF      M003-TEL-CEL1O  NOT     NUMERIC OR
                   M003-TEL-CEL1O  EQUAL   ZEROS
                   MOVE 'R'        TO      M003-TEL-CEL1A
                   MOVE -1         TO      M003-TEL-CEL1L
                   MOVE WS-MSG-054 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-TEL-CEL2I  EQUAL   SPACES
                   MOVE 'R'        TO      M003-TEL-CEL2A
                   MOVE -1         TO      M003-TEL-CEL2L
                   MOVE WS-MSG-053 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-TEL-CEL2I  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-002      TO      M003-TEL-CEL2I.

           IF      M003-TEL-CEL2O  NOT     NUMERIC
                   MOVE 'R'        TO      M003-TEL-CEL2A
                   MOVE -1         TO      M003-TEL-CEL2L
                   MOVE WS-MSG-054 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           IF      M003-TEL-CEL3I  EQUAL   SPACES
                   MOVE 'R'        TO      M003-TEL-CEL3A
                   MOVE -1         TO      M003-TEL-CEL3L
                   MOVE WS-MSG-053 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.

           MOVE    M003-TEL-CEL3I  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-002      TO      M003-TEL-CEL3I.

           IF      M003-TEL-CEL3O  NOT     NUMERIC
                   MOVE 'R'        TO      M003-TEL-CEL3A
                   MOVE -1         TO      M003-TEL-CEL3L
                   MOVE WS-MSG-054 TO      M003-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM003
           END-IF.
      *
      *---------------------------------------------------------------*
       150-99-EXIT.
      *-=-=-=-=-=-*
           EXIT.
      /===============================================================*
       160-00-TRATA-FASE-2         SECTION.
      *---------------------------------------------------------------*
      *
           MOVE   -1               TO      M003-MSGL.

           MOVE    WS-MSG-006      TO      M003-MSGO.

           PERFORM 820-00-SEND-DTONLY-SAGM003.
      *
      *---------------------------------------------------------------*
       160-99-EXIT.
      *-=-=-=-=-=-*
           EXIT.
      /===============================================================*
       200-00-TRATA-PF6            SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 800-00-RECEIVE-MAP.

           MOVE   'A'              TO      M003-NO-ALUNOA
                                           M003-SEXOA
                                           M003-E-MAILA
                                           M003-OBSA.

           MOVE   'J'              TO      M003-DIA-NASCA
                                           M003-MES-NASCA
                                           M003-ANO-NASCA
                                           M003-NUM-CPF1A
                                           M003-NUM-CPF2A
                                           M003-NUM-CPF3A
                                           M003-DIG-CPFA
                                           M003-LCL-CELA
                                           M003-TEL-CEL1A
                                           M003-TEL-CEL2A
                                           M003-TEL-CEL3A.

           MOVE    1               TO      COMM-FASE.

           MOVE    WS-MSG-007      TO      M003-MSGI.

           MOVE   -1               TO      M003-NO-ALUNOL.

           PERFORM 820-00-SEND-DTONLY-SAGM003.
      *
      *---------------------------------------------------------------*
       200-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       300-00-TRATA-PF2            SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 800-00-RECEIVE-MAP.

           PERFORM 310-00-SELMAX-SAGTBS01.

           PERFORM 320-00-INSERT-SAGTBS01.

           MOVE    3               TO      COMM-FASE.

           MOVE   -1               TO      M003-MSGL.

           MOVE    WS-MSG-028      TO      M003-MSGI.

           PERFORM 820-00-SEND-DTONLY-SAGM003.
      *
      *---------------------------------------------------------------*
       300-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       310-00-SELMAX-SAGTBS01      SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    SQL

                   SELECT  MAX(CO_ALUNO)

                   INTO   :CO-ALUNO

                   FROM    DB2.SAGTBS01_ALUNOS

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100 AND -305
                   MOVE 'SELMAX'   TO      WS-COMANDO-DB2
                   MOVE 'SAGTBS01' TO      WS-TABELAS-DB2
                   PERFORM         995-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +100 OR  -305
                   MOVE +0         TO      CO-ALUNO
           END-IF.

           ADD     001             TO      CO-ALUNO.
      *
      *---------------------------------------------------------------*
       310-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       320-00-INSERT-SAGTBS01      SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    CO-ALUNO        TO      M003-CO-ALUNOO.

           INSPECT M003-NO-ALUNOI  REPLACING ALL '_' BY SPACES.
           INSPECT M003-E-MAILI    REPLACING ALL '_' BY SPACES.
           INSPECT M003-OBSI       REPLACING ALL '_' BY SPACES.

           MOVE    M003-NO-ALUNOO  TO      NO-ALUNO.

           MOVE    M003-DIA-NASCO  TO      WS-DIA-DB2.
           MOVE    M003-MES-NASCO  TO      WS-MES-DB2.
           MOVE    M003-ANO-NASCO  TO      WS-ANO-DB2.

           MOVE    WS-DAT-DB2      TO      DT-NASCIMENTO.

           MOVE    M003-SEXOO      TO      IC-SEXO.

           MOVE    M003-NUM-CPF1O  TO      WS-CPF-01.
           MOVE    M003-NUM-CPF2O  TO      WS-CPF-02.
           MOVE    M003-NUM-CPF3O  TO      WS-CPF-03.
           MOVE    M003-DIG-CPFO   TO      WS-CPF-DV.

           MOVE    WS-CPF-00       TO      NU-CPF.

           MOVE    -1              TO      WS-CO-LOCAL-CEL-NULL.
           MOVE    -1              TO      WS-NU-TELEF-CEL-NULL.
           MOVE    -1              TO      WS-NO-E-MAIL-NULL.
           MOVE    -1              TO      WS-NO-OBS-NULL.

           IF      M003-LCL-CELI NOT EQUAL '___'
                   MOVE +0         TO      WS-CO-LOCAL-CEL-NULL
                   MOVE +0         TO      WS-NU-TELEF-CEL-NULL
                   MOVE M003-LCL-CELO
                                   TO      CO-LOCAL-CEL
                   MOVE M003-TEL-CEL1O
                                   TO      WS-TEL-01
                   MOVE M003-TEL-CEL2O
                                   TO      WS-TEL-02
                   MOVE M003-TEL-CEL3O
                                   TO      WS-TEL-03
                   MOVE WS-TEL-00  TO      NU-TELEF-CEL
           END-IF.

           IF      M003-E-MAILO NOT EQUAL  SPACES
                   MOVE +0          TO      WS-NO-E-MAIL-NULL
           END-IF.

           IF      M003-OBSO    NOT EQUAL  SPACES
                   MOVE +0          TO     WS-NO-OBS-NULL
           END-IF.

           MOVE    M003-E-MAILO    TO      NO-E-MAIL.
           MOVE    M003-OBSO       TO      NO-OBS.

           EXEC    SQL     INSERT

                   INTO    DB2.SAGTBS01_ALUNOS

                          ( CO_ALUNO     ,
                            DT_INCLUSAO  ,
                            DT_ALTERACAO ,
                            NO_ALUNO     ,
                            DT_NASCIMENTO,
                            IC_SEXO      ,
                            NU_CPF       ,
                            CO_LOCAL_CEL ,
                            NU_TELEF_CEL ,
                            NO_E_MAIL    ,
                            NO_OBS       )

                   VALUES (:CO-ALUNO     ,
                           CURRENT DATE  ,
                           NULL          ,
                           :NO-ALUNO     ,
                           :DT-NASCIMENTO,
                           :IC-SEXO      ,
                           :NU-CPF       ,
                           :CO-LOCAL-CEL
                              :WS-CO-LOCAL-CEL-NULL,
                           :NU-TELEF-CEL
                              :WS-NU-TELEF-CEL-NULL,
                           :NO-E-MAIL
                              :WS-NO-E-MAIL-NULL,
                           :NO-OBS
                              :WS-NO-OBS-NULL )

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE 'INSERT'   TO      WS-COMANDO-DB2
                   MOVE 'SAGTBS01' TO      WS-TABELAS-DB2
                   PERFORM         995-00-ABEND-DB2
           END-IF.
      *
      *---------------------------------------------------------------*
       320-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       690-00-TECLA-INVALIDA       SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS-MSG-001      TO      M003-MSGI.

           IF      COMM-FASE       EQUAL   1
                   MOVE -1         TO      M003-NO-ALUNOL
           ELSE
                   MOVE -1         TO      M003-MSGL
           END-IF.

           PERFORM 820-00-SEND-DTONLY-SAGM003.
      *
      *---------------------------------------------------------------*
       690-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       800-00-RECEIVE-MAP          SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    RECEIVE MAP   ('SAGM003')
                           MAPSET        ('SAGM003')
                           INTO          (SAGM003O)
           END-EXEC.
      *
      *---------------------------------------------------------------*
       800-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       810-00-SEND-ERASE-SAGM003   SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 830-00-FORMATTIME.

           EXEC    CICS    SEND    MAP      ('SAGM003')
                                   MAPSET   ('SAGM003')
                                   FROM     (SAGM003O)
                                   ERASE
                                   CURSOR
                                   FREEKB
                                   ALARM
           END-EXEC.

           EXEC    CICS    RETURN  TRANSID ('GA03')
                                   COMMAREA(COMM-COMMAREA)
                                   LENGTH  (LENGTH OF COMM-COMMAREA)
           END-EXEC.


      *
      *---------------------------------------------------------------*
       810-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       820-00-SEND-DTONLY-SAGM003  SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 830-00-FORMATTIME.

           EXEC    CICS    SEND    MAP      ('SAGM003')
                                   MAPSET   ('SAGM003')
                                   FROM     (SAGM003O)
                                   DATAONLY
                                   CURSOR
                                   FREEKB
                                   ALARM
           END-EXEC.

           EXEC    CICS    RETURN  TRANSID ('GA03')
                                   COMMAREA(COMM-COMMAREA)
                                   LENGTH  (LENGTH OF COMM-COMMAREA)
           END-EXEC.
      *
      *---------------------------------------------------------------*
       820-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       830-00-FORMATTIME           SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    ASKTIME      ABSTIME    (WS-ABSTIME)

           END-EXEC.

           EXEC    CICS    FORMATTIME   ABSTIME    (WS-ABSTIME)
                                        DDMMYY     (WS-DAT-EDIT)
                                        DATESEP    ('/')
                                        YEAR       (WS-ANO-CICS)
                                        TIME       (WS-HRS-EDIT)
                                        TIMESEP    (':')
           END-EXEC.

           MOVE    COMM-USERID     TO      M003-USERIDO.
           MOVE    WS-ANO-CICS     TO      WS-SA-EDIT.
           MOVE    WS-DT-EDIT      TO      M003-DATAO.
           MOVE    WS-HRS-EDIT     TO      M003-HORAO.

           IF      COMM-FASE   NOT EQUAL   1
                   MOVE '/'        TO      M003-NO-ALUNOA
                                           M003-DIA-NASCA
                                           M003-MES-NASCA
                                           M003-ANO-NASCA
                                           M003-SEXOA
                                           M003-NUM-CPF1A
                                           M003-NUM-CPF2A
                                           M003-NUM-CPF3A
                                           M003-DIG-CPFA
                                           M003-LCL-CELA
                                           M003-TEL-CEL1A
                                           M003-TEL-CEL2A
                                           M003-TEL-CEL3A
                                           M003-E-MAILA
                                           M003-OBSA
           END-IF.

           INSPECT M003-NO-ALUNOI  REPLACING ALL SPACES BY '_'.
           INSPECT M003-DIA-NASCI  REPLACING ALL SPACES BY '_'.
           INSPECT M003-MES-NASCI  REPLACING ALL SPACES BY '_'.
           INSPECT M003-ANO-NASCI  REPLACING ALL SPACES BY '_'.
           INSPECT M003-SEXOI      REPLACING ALL SPACES BY '_'.
           INSPECT M003-NUM-CPF1I  REPLACING ALL SPACES BY '_'.
           INSPECT M003-NUM-CPF2I  REPLACING ALL SPACES BY '_'.
           INSPECT M003-NUM-CPF3I  REPLACING ALL SPACES BY '_'.
           INSPECT M003-DIG-CPFI   REPLACING ALL SPACES BY '_'.
           INSPECT M003-LCL-CELI   REPLACING ALL SPACES BY '_'.
           INSPECT M003-TEL-CEL1I  REPLACING ALL SPACES BY '_'.
           INSPECT M003-TEL-CEL2I  REPLACING ALL SPACES BY '_'.
           INSPECT M003-TEL-CEL3I  REPLACING ALL SPACES BY '_'.
           INSPECT M003-E-MAILI    REPLACING ALL SPACES BY '_'.
           INSPECT M003-OBSI       REPLACING ALL SPACES BY '_'.
      *
      *---------------------------------------------------------------*
       830-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       995-00-ABEND-DB2            SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    SQLCODE         TO      WS-SQLCODE-DB2.

           MOVE    WS-COMANDO-DB2  TO      WS-MSG-099 (14:06).
           MOVE    WS-TABELAS-DB2  TO      WS-MSG-099 (31:08).
           MOVE    WS-SQLCODE-DB2  TO      WS-MSG-099 (53:04).

           MOVE    WS-MSG-099      TO      M003-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM003.
      *
      *---------------------------------------------------------------*
       995-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       996-00-ABEND-SUB            SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS-MSG-096      TO      M003-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM003.
      *
      *---------------------------------------------------------------*
       996-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       997-99-ABCODE               SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    ASSIGN    ABCODE    (WS-ABCODE)

           END-EXEC.

           MOVE    WS-ABCODE       TO      WS-MSG-097 (51:04).

           MOVE    WS-MSG-097      TO      M003-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM003.
      *
      *---------------------------------------------------------------*
       997-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       998-99-ERROR                SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    EIBRESP         TO      WS-EIBRESP.

           MOVE    WS-EIBRESP      TO      WS-MSG-098 (48:03).

           MOVE    WS-MSG-098      TO      M003-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM003.
      *
      *---------------------------------------------------------------*
       998-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      *
      /**-----------------------------------------------------------***
      ***                FIM DO PROGRAMA - SAGPO003                 ***
      ***-----------------------------------------------------------***
