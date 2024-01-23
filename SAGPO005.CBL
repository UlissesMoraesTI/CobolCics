      *===============================================================*
       IDENTIFICATION              DIVISION.
      *---------------------------------------------------------------*
      *
       PROGRAM-ID.                 SAGPO005.
       AUTHOR.                     ULISSES & MORAES (TI).
       DATE-WRITTEN.               25/03/2012.
       SECURITY.
      *
      *===============================================================*
      *                                                               *
      *   SISTEMA       : SAG - SISTEMA DE GESTAO DE ALUNOS.          *
      *   PROGRAMA      : SAGPO005                                    *
      *   TRANSACAO     : GA05                                        *
      *   LINGUAGEM     : COBOL / CICS / DB2                          *
      *   PROGRAMADOR   : ULISSES & MORAES (TI)                       *
      *   ANALISTA      : ULISSES & MORAES (TI)                       *
      *   DATA          : 25/03/2012                                  *
      *                                                               *
      *   OBJETIVO      : REALIZAR EXCLUSAO NO CADASTRO DE ALUNOS     *
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
              '** INICIO WORKING SAGPO005 **'.
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
       01      FILLER              REDEFINES      WS-CON-NUM.
         03    FILLER              PIC     X(011).
         03    WS-CON-007          PIC     X(007).
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
      ***          AREA DE DEFINICAO DO MAPA - MAPSET (SAGM005)     ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGM005.
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
                  '** FINAL WORKING SAGPO005 **'.
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
           MOVE   'J'              TO      M005-CO-ALUNOA.

           MOVE    ALL '_'         TO      M005-CO-ALUNOI.

           MOVE    1               TO      COMM-FASE.

           MOVE   -1               TO      M005-CO-ALUNOL.

           MOVE    WS-MSG-039      TO      M005-MSGO.

           PERFORM 810-00-SEND-ERASE-SAGM005.
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

           MOVE   'J'              TO      M005-CO-ALUNOA.

           INSPECT M005-CO-ALUNOI  REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT M005-CO-ALUNOI  REPLACING ALL '_'        BY SPACES.

           IF      M005-CO-ALUNOI  EQUAL   SPACES
                   MOVE 'R'        TO      M005-CO-ALUNOA
                   MOVE -1         TO      M005-CO-ALUNOL
                   MOVE WS-MSG-039 TO      M005-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM005
           END-IF.

           MOVE    M005-CO-ALUNOI  TO      TB-ALF-018.
           PERFORM 0130-00-CONVERTE-ALF-NUM.
           MOVE    WS-CON-007      TO      M005-CO-ALUNOI.

           IF      M005-CO-ALUNOO  NOT     NUMERIC OR
                   M005-CO-ALUNOO  EQUAL   ZEROS
                   MOVE 'R'        TO      M005-CO-ALUNOA
                   MOVE -1         TO      M005-CO-ALUNOL
                   MOVE WS-MSG-030 TO      M005-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM005
           END-IF.

           PERFORM 120-00-SELECT-SAGTBS01.

           MOVE    2               TO      COMM-FASE.

           MOVE   -1               TO      M005-MSGL.

           MOVE    WS-MSG-051      TO      M005-MSGO.

           PERFORM 820-00-SEND-DTONLY-SAGM005.
      *
      *---------------------------------------------------------------*
       110-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       120-00-SELECT-SAGTBS01      SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    M005-CO-ALUNOO  TO      CO-ALUNO.

           EXEC    SQL

                   SELECT   DT_INCLUSAO  ,
                            VALUE(CHAR(DT_ALTERACAO), '01.01.0001'),
                            NO_ALUNO     ,
                            DT_NASCIMENTO,
                            IC_SEXO      ,
                            NU_CPF       ,
                            VALUE(CO_LOCAL_CEL, 0),
                            VALUE(NU_TELEF_CEL, 0),
                            VALUE(NO_E_MAIL, ' '),
                            VALUE(NO_OBS, ' ')

                   INTO    :DT-INCLUSAO  ,
                           :DT-ALTERACAO ,
                           :NO-ALUNO     ,
                           :DT-NASCIMENTO,
                           :IC-SEXO      ,
                           :NU-CPF       ,
                           :CO-LOCAL-CEL ,
                           :NU-TELEF-CEL ,
                           :NO-E-MAIL    ,
                           :NO-OBS

                   FROM    DB2.SAGTBS01_ALUNOS

                   WHERE   CO_ALUNO = :CO-ALUNO

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100
                   MOVE 'SELECT'   TO      WS-COMANDO-DB2
                   MOVE 'SAGTBS01' TO      WS-TABELAS-DB2
                   PERFORM         995-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +100
                   MOVE 'R'        TO      M005-CO-ALUNOA
                   MOVE -1         TO      M005-CO-ALUNOL
                   MOVE WS-MSG-031 TO      M005-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM005
           END-IF.

           MOVE    DT-INCLUSAO     TO      M005-DT-INCLUSAOO
           MOVE    DT-ALTERACAO    TO      M005-DT-ALTERACAOO.

           IF      M005-DT-ALTERACAOO
                                   EQUAL  '01.01.0001'
                   MOVE SPACES     TO      M005-DT-ALTERACAOO
           END-IF.

           INSPECT M005-DT-INCLUSAOO   REPLACING ALL '.' BY '/'.
           INSPECT M005-DT-ALTERACAOO  REPLACING ALL '.' BY '/'.

           MOVE    NO-ALUNO        TO      M005-NO-ALUNOO.

           MOVE    DT-NASCIMENTO   TO      WS-DAT-DB2.

           MOVE    WS-DIA-DB2      TO      M005-DIA-NASCO.
           MOVE    WS-MES-DB2      TO      M005-MES-NASCO.
           MOVE    WS-ANO-DB2      TO      M005-ANO-NASCO.

           IF      IC-SEXO         EQUAL  'M'
                   MOVE 'Masculino'   TO   M005-SEXOO
           ELSE
                   MOVE 'Feminino '   TO   M005-SEXOO
           END-IF.

           MOVE    NU-CPF          TO      WS-CPF-00.

           MOVE    WS-CPF-01       TO      M005-NUM-CPF1O.
           MOVE    WS-CPF-02       TO      M005-NUM-CPF2O.
           MOVE    WS-CPF-03       TO      M005-NUM-CPF3O.
           MOVE    WS-CPF-DV       TO      M005-DIG-CPFO.

           IF      NU-TELEF-CEL NOT EQUAL  ZEROS
                   MOVE CO-LOCAL-CEL
                                   TO
                                           M005-LCL-CELO
                   MOVE NU-TELEF-CEL
                                   TO      WS-TEL-00
                   MOVE WS-TEL-01  TO      M005-TEL-CEL1O
                   MOVE WS-TEL-02  TO      M005-TEL-CEL2O
                   MOVE WS-TEL-03  TO      M005-TEL-CEL3O
           END-IF.

           MOVE    NO-E-MAIL       TO      M005-E-MAILO.
           MOVE    NO-OBS          TO      M005-OBSO.
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
       160-00-TRATA-FASE-2         SECTION.
      *---------------------------------------------------------------*
      *
           MOVE   -1               TO      M005-MSGL.

           MOVE    WS-MSG-051      TO      M005-MSGO.

           PERFORM 820-00-SEND-DTONLY-SAGM005.
      *
      *---------------------------------------------------------------*
       160-99-EXIT.
      *-=-=-=-=-=-*
           EXIT.
      /===============================================================*
       300-00-TRATA-PF2            SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 800-00-RECEIVE-MAP.

           PERFORM 320-00-DELETE-SAGTBS01.

           MOVE    3               TO      COMM-FASE.

           MOVE   -1               TO      M005-MSGL.

           MOVE    WS-MSG-026      TO      M005-MSGI.

           PERFORM 820-00-SEND-DTONLY-SAGM005.
      *
      *---------------------------------------------------------------*
       300-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       320-00-DELETE-SAGTBS01      SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    M005-CO-ALUNOO  TO      CO-ALUNO.

           EXEC    SQL     DELETE

                   FROM    DB2.SAGTBS01_ALUNOS

                   WHERE   CO_ALUNO = :CO-ALUNO

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE 'DELETE'   TO      WS-COMANDO-DB2
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
           MOVE    WS-MSG-001      TO      M005-MSGI.

           IF      COMM-FASE       EQUAL   1
                   MOVE -1         TO      M005-CO-ALUNOL
           ELSE
                   MOVE -1         TO      M005-MSGL
           END-IF.

           PERFORM 820-00-SEND-DTONLY-SAGM005.
      *
      *---------------------------------------------------------------*
       690-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       800-00-RECEIVE-MAP          SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    RECEIVE MAP   ('SAGM005')
                           MAPSET        ('SAGM005')
                           INTO          (SAGM005O)
           END-EXEC.
      *
      *---------------------------------------------------------------*
       800-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       810-00-SEND-ERASE-SAGM005   SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 830-00-FORMATTIME.

           EXEC    CICS    SEND    MAP      ('SAGM005')
                                   MAPSET   ('SAGM005')
                                   FROM     (SAGM005O)
                                   ERASE
                                   CURSOR
                                   FREEKB
                                   ALARM
           END-EXEC.

           EXEC    CICS    RETURN  TRANSID ('GA05')
                                   COMMAREA(COMM-COMMAREA)
                                   LENGTH  (LENGTH OF COMM-COMMAREA)
           END-EXEC.


      *
      *---------------------------------------------------------------*
       810-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       820-00-SEND-DTONLY-SAGM005  SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 830-00-FORMATTIME.

           EXEC    CICS    SEND    MAP      ('SAGM005')
                                   MAPSET   ('SAGM005')
                                   FROM     (SAGM005O)
                                   DATAONLY
                                   CURSOR
                                   FREEKB
                                   ALARM
           END-EXEC.

           EXEC    CICS    RETURN  TRANSID ('GA05')
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

           MOVE    COMM-USERID     TO      M005-USERIDO.
           MOVE    WS-ANO-CICS     TO      WS-SA-EDIT.
           MOVE    WS-DT-EDIT      TO      M005-DATAO.
           MOVE    WS-HRS-EDIT     TO      M005-HORAO.

           IF      COMM-FASE   NOT EQUAL   1
                   MOVE '/'        TO      M005-CO-ALUNOA
           END-IF.

           INSPECT M005-CO-ALUNOI  REPLACING ALL SPACES BY '_'.
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

           MOVE    WS-MSG-099      TO      M005-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM005.
      *
      *---------------------------------------------------------------*
       995-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       996-00-ABEND-SUB            SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS-MSG-096      TO      M005-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM005.
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

           MOVE    WS-MSG-097      TO      M005-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM005.
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

           MOVE    WS-MSG-098      TO      M005-MSGO.

           MOVE    9               TO      COMM-FASE.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 810-00-SEND-ERASE-SAGM005.
      *
      *---------------------------------------------------------------*
       998-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      *
      /**-----------------------------------------------------------***
      ***                FIM DO PROGRAMA - SAGPO005                 ***
      ***-----------------------------------------------------------***
