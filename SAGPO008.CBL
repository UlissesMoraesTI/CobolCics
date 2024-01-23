      *===============================================================*
       IDENTIFICATION              DIVISION.
      *---------------------------------------------------------------*
      *
       PROGRAM-ID.                 SAGPO008.
       AUTHOR.                     ULISSES & MORAES (TI).
       DATE-WRITTEN.               15/04/2012.
       SECURITY.
      *
      *===============================================================*
      *                                                               *
      *   SISTEMA       : SAG - SISTEMA DE GESTAO DE ALUNOS.          *
      *   PROGRAMA      : SAGPO008                                    *
      *   TRANSACAO     : GA07                                        *
      *   LINGUAGEM     : COBOL / CICS / DB2                          *
      *   PROGRAMADOR   : ULISSES & MORAES (TI)                       *
      *   ANALISTA      : ULISSES & MORAES (TI)                       *
      *   DATA          : 15/04/2012                                  *
      *                                                               *
      *   OBJETIVO      : LISTAR - ALUNO                              *
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
              '** INICIO WORKING SAGPO008 **'.
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
       77      WS-COMM-TRANS       PIC     X(004) VALUE SPACES.
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
      /**-----------------------------------------------------------***
      ***      AREA DA TS - MONTAGEM DAS OCORRENCIAS                ***
      ***-----------------------------------------------------------***
      *
       01      WS-IND1             PIC     9(005) VALUE ZEROS.
      *
       01      WS-TS-ITEM          PIC    S9(004) COMP VALUE +0.
       01      WS-TS-LENGTH        PIC    S9(004) COMP VALUE +1140.
      *
       01      WS-NOME-TS.
         03    WS-TRANID-TS        PIC     X(004) VALUE 'GA07'.
         03    WS-TERMID-TS        PIC     X(004) VALUE SPACES.
      *
       01      TS-AREA.
         03    TS-OCOR          OCCURS     15  TIMES.
           05  TS-CO-ALUNO         PIC     9(007).
           05  TS-NO-ALUNO         PIC     X(069).
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
      ***          AREA DE DEFINICAO DO MAPA - MAPSET (SAGM008)     ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGM008.
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
      /**-----------------------------------------------------------***
      ***          AREA DE TRATAMENTO DE CURSORES                   ***
      ***-----------------------------------------------------------***
      *
           EXEC    SQL     DECLARE     C-SAGTBS01      CURSOR  FOR

                   SELECT  CO_ALUNO,
                           NO_ALUNO

                   FROM    DB2.SAGTBS01_ALUNOS

                  ORDER BY CO_ALUNO

                  FOR      FETCH       ONLY

           END-EXEC.
      *
      *---------------------------------------------------------------*
       01          FILLER          PIC     X(040)  VALUE
                  '** FINAL WORKING SAGPO008 **'.
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

           IF      EIBAID          EQUAL   DFHPF3
                   PERFORM         040-00-RETORNA-CHAMADOR
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
                   EIBAID          EQUAL   DFHPF7
                   PERFORM         210-00-VOLTA-PAGINA
           END-IF.

           IF      COMM-FASE       EQUAL   2 AND
                   EIBAID          EQUAL   DFHPF8
                   PERFORM         220-00-AVANCA-PAGINA
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
           MOVE    1               TO      COMM-FASE.

           PERFORM 110-00-BUSCA-LISTA-ALUNO.

           MOVE    001             TO      COMM-PAG-INI.

           PERFORM 200-00-MONTA-PAGINA.
      *
      *---------------------------------------------------------------*
       100-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       110-00-BUSCA-LISTA-ALUNO    SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 160-00-DELETAR-TS.

           PERFORM 170-00-LIMPAR-TS.

           PERFORM 120-00-OPEN-SAGTBS01.

           PERFORM 130-00-FETCH-SAGTBS01.

           IF      SQLCODE         EQUAL   +100
                   PERFORM         140-00-CLOSE-SAGTBS01
                   MOVE WS-MSG-010 TO      M008-MSGO
                   PERFORM         810-00-SEND-ERASE-SAGM007
           END-IF.

           PERFORM 150-00-PROCESSA-SAGTBS01
             UNTIL SQLCODE EQUAL +100.

           PERFORM 140-00-CLOSE-SAGTBS01.

           IF      WS-IND1         GREATER ZEROS
                   PERFORM         190-00-GRAVA-TS
           END-IF.
      *
      *---------------------------------------------------------------*
       110-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       120-00-OPEN-SAGTBS01        SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    SQL
                   OPEN    C-SAGTBS01
           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE ' OPEN '   TO      WS-COMANDO-DB2
                   MOVE 'SAGTBS01' TO      WS-TABELAS-DB2
                   PERFORM         995-00-ABEND-DB2
           END-IF.
      *
      *---------------------------------------------------------------*
       120-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       130-00-FETCH-SAGTBS01       SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    SQL

                   FETCH   C-SAGTBS01

                   INTO   :CO-ALUNO,
                          :NO-ALUNO

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100
                   MOVE 'FETCH '   TO      WS-COMANDO-DB2
                   MOVE 'SAGTBS01' TO      WS-TABELAS-DB2
                   PERFORM         995-00-ABEND-DB2
           END-IF.
      *
      *---------------------------------------------------------------*
       130-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       140-00-CLOSE-SAGTBS01       SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    SQL
                   CLOSE   C-SAGTBS01
           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE 'CLOSE '   TO      WS-COMANDO-DB2
                   MOVE 'SAGTBS01' TO      WS-TABELAS-DB2
                   PERFORM         995-00-ABEND-DB2
           END-IF.
      *
      *---------------------------------------------------------------*
       140-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       150-00-PROCESSA-SAGTBS01    SECTION.
      *---------------------------------------------------------------*
      *
           ADD     001             TO      WS-IND1.

           MOVE    CO-ALUNO        TO      TS-CO-ALUNO (WS-IND1).
           MOVE    NO-ALUNO        TO      TS-NO-ALUNO (WS-IND1).

           IF      WS-IND1         EQUAL   15
                   PERFORM         190-00-GRAVA-TS
                   PERFORM         170-00-LIMPAR-TS
           END-IF.

           PERFORM 130-00-FETCH-SAGTBS01.
      *
      *---------------------------------------------------------------*
       150-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       160-00-DELETAR-TS           SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    'GA07'          TO      WS-TRANID-TS.
           MOVE    EIBTRMID        TO      WS-TERMID-TS.

           EXEC    CICS DELETEQ TS QUEUE  (WS-NOME-TS)
                                   NOHANDLE
           END-EXEC.
      *
      *---------------------------------------------------------------*
       160-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       170-00-LIMPAR-TS            SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM VARYING WS-IND1 FROM 01 BY 01
                   UNTIL   WS-IND1 GREATER 15

             MOVE  ZEROS           TO      TS-CO-ALUNO (WS-IND1)
             MOVE  SPACES          TO      TS-NO-ALUNO (WS-IND1)

           END-PERFORM.

           MOVE    ZEROS           TO      WS-IND1.
      *
      *---------------------------------------------------------------*
       170-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       180-00-LER-TS               SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    'GA07'          TO      WS-TRANID-TS.
           MOVE    EIBTRMID        TO      WS-TERMID-TS.

           MOVE    COMM-PAG-INI    TO      WS-TS-ITEM.

           EXEC    CICS  READQ  TS QUEUE   (WS-NOME-TS)
                                   INTO    (TS-AREA)
                                   LENGTH  (WS-TS-LENGTH)
                                   ITEM    (WS-TS-ITEM)
           END-EXEC.
      *
      *---------------------------------------------------------------*
       180-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       190-00-GRAVA-TS             SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    'GA07'          TO      WS-TRANID-TS.
           MOVE    EIBTRMID        TO      WS-TERMID-TS.

           ADD     001             TO      WS-TS-ITEM.

           EXEC    CICS WRITEQ  TS QUEUE  (WS-NOME-TS)
                                   FROM   (TS-AREA)
                                   LENGTH (WS-TS-LENGTH)
                                   ITEM   (WS-TS-ITEM)
           END-EXEC.

           MOVE    WS-TS-ITEM      TO      COMM-PAG-FNL.
      *
      *---------------------------------------------------------------*
       190-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       200-00-MONTA-PAGINA         SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 180-00-LER-TS.

           PERFORM VARYING WS-IND1 FROM 01 BY 01
                   UNTIL   WS-IND1 GREATER 15

             IF    TS-CO-ALUNO (WS-IND1)
                               NOT EQUAL   ZEROS

                   MOVE TS-CO-ALUNO (WS-IND1)
                                   TO      M008-CO-ALUNOO (WS-IND1)
                   MOVE TS-NO-ALUNO (WS-IND1)
                                   TO      M008-NO-ALUNOO (WS-IND1)
             ELSE

                   MOVE SPACES     TO      M008-CO-ALUNOI (WS-IND1)
                   MOVE SPACES     TO      M008-NO-ALUNOI (WS-IND1)
             END-IF

           END-PERFORM.

           MOVE    WS-MSG-029      TO      M008-MSGO.

           IF      COMM-FASE       EQUAL   1
                   MOVE 2          TO      COMM-FASE
                   PERFORM         810-00-SEND-ERASE-SAGM007
           END-IF.

           MOVE    2               TO      COMM-FASE.

           PERFORM 820-00-SEND-DTONLY-SAGM007.
      *
      *---------------------------------------------------------------*
       200-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       210-00-VOLTA-PAGINA         SECTION.
      *---------------------------------------------------------------*
      *
           IF      COMM-PAG-INI    EQUAL   1
                   MOVE WS-MSG-008 TO      M008-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM007
           END-IF.

           SUBTRACT 001            FROM    COMM-PAG-INI.

           PERFORM 200-00-MONTA-PAGINA.
      *
      *---------------------------------------------------------------*
       210-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       220-00-AVANCA-PAGINA        SECTION.
      *---------------------------------------------------------------*
      *
           IF      COMM-PAG-INI    EQUAL   COMM-PAG-FNL
                   MOVE WS-MSG-009 TO      M008-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM007
           END-IF.

           ADD     001             TO      COMM-PAG-INI.

           PERFORM 200-00-MONTA-PAGINA.
      *
      *---------------------------------------------------------------*
       220-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       110-00-TRATA-FASE-1         SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS-MSG-010       TO      M008-MSGO.

           PERFORM 820-00-SEND-DTONLY-SAGM007.
      *
      *---------------------------------------------------------------*
       110-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       160-00-TRATA-FASE-2         SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS-MSG-029      TO      M008-MSGO.

           PERFORM 820-00-SEND-DTONLY-SAGM007.
      *
      *---------------------------------------------------------------*
       160-99-EXIT.
      *-=-=-=-=-=-*
           EXIT.
      /===============================================================*
       690-00-TECLA-INVALIDA       SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS-MSG-001      TO      M008-MSGI.

           PERFORM 820-00-SEND-DTONLY-SAGM007.
      *
      *---------------------------------------------------------------*
       690-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       800-00-RECEIVE-MAP          SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    RECEIVE MAP   ('SAGM008')
                           MAPSET        ('SAGM008')
                           INTO          (SAGM008O)
           END-EXEC.
      *
      *---------------------------------------------------------------*
       800-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       810-00-SEND-ERASE-SAGM007   SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 830-00-FORMATTIME.

           EXEC    CICS    SEND    MAP      ('SAGM008')
                                   MAPSET   ('SAGM008')
                                   FROM     (SAGM008O)
                                   ERASE
                                   CURSOR
                                   FREEKB
                                   ALARM
           END-EXEC.

           EXEC    CICS    RETURN  TRANSID ('GA07')
                                   COMMAREA(COMM-COMMAREA)
                                   LENGTH  (LENGTH OF COMM-COMMAREA)
           END-EXEC.


      *
      *---------------------------------------------------------------*
       810-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       820-00-SEND-DTONLY-SAGM007  SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 830-00-FORMATTIME.

           EXEC    CICS    SEND    MAP      ('SAGM008')
                                   MAPSET   ('SAGM008')
                                   FROM     (SAGM008O)
                                   DATAONLY
                                   CURSOR
                                   FREEKB
                                   ALARM
           END-EXEC.

           EXEC    CICS    RETURN  TRANSID ('GA07')
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

           MOVE    COMM-USERID     TO      M008-USERIDO.
      *    MOVE    COMM-PAG-INI    TO      M008-PAGNO.

           MOVE    WS-ANO-CICS     TO      WS-SA-EDIT.
           MOVE    WS-DT-EDIT      TO      M008-DATAO.
           MOVE    WS-HRS-EDIT     TO      M008-HORAO.

      *    MOVE    COMM-PAG-INI    TO      M008-PAGNO.
           MOVE    '/'             TO      M008-PAGTA.

           IF      COMM-FASE   NOT EQUAL   2
                   MOVE '@'        TO      M008-PAGTA
           END-IF.
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

           MOVE    WS-MSG-099      TO      M008-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM007.
      *
      *---------------------------------------------------------------*
       995-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       996-00-ABEND-SUB            SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS-MSG-096      TO      M008-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM007.
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

           MOVE    WS-MSG-097      TO      M008-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM007.
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

           MOVE    WS-MSG-098      TO      M008-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM007.
      *
      *---------------------------------------------------------------*
       998-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      *
      /**-----------------------------------------------------------***
      ***                FIM DO PROGRAMA - SAGPO008                 ***
      ***-----------------------------------------------------------***
