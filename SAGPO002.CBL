      *===============================================================*
       IDENTIFICATION              DIVISION.
      *---------------------------------------------------------------*
      *
       PROGRAM-ID.                 SAGPO002.
       AUTHOR.                     ULISSES & MORAES (TI).
       DATE-WRITTEN.               04/03/2012.
       SECURITY.
      *
      *===============================================================*
      *                                                               *
      *   SISTEMA       : SAG - SISTEMA DE GESTAO DE ALUNOS.          *
      *   PROGRAMA      : SAGPO002                                    *
      *   TRANSACAO     : GA01                                        *
      *   LINGUAGEM     : COBOL / CICS / DB2                          *
      *   PROGRAMADOR   : ULISSES & MORAES (TI)                       *
      *   ANALISTA      : ULISSES & MORAES (TI)                       *
      *   DATA          : 04/03/2012                                  *
      *                                                               *
      *   OBJETIVO      : TELA DE APRESENTACAO DO SISTEMA             *
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
              '** INICIO WORKING SAGPO002 **'.
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
       77      WS-MSG              PIC     X(079) VALUE SPACES.
      *
       77      WS-TRANS            PIC     X(004) VALUE SPACES.
       77      WS-COMM-TRANS       PIC     X(004) VALUE SPACES.
      *
      /**-----------------------------------------------------------***
      ***      TRATAMENTO DE OPCAO                                  ***
      ***-----------------------------------------------------------***
      *
       01      WS-OPC              PIC     9(001) VALUE ZEROS.
         88    WS-OPC-88           VALUE   1 2 3 4 5 6 7 8 9.
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
      ***          AREA DE DEFINICAO DO MAPA - MAPSET (SAGM002)     ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGM002.
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
      *---------------------------------------------------------------*
       01          FILLER          PIC     X(040)  VALUE
                  '** FINAL WORKING SAGPO002 **'.
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
       100-00-INICIALIZA-TELA      SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    001             TO      COMM-FASE.

           MOVE   '_'              TO      M002-OPCI.

           MOVE   'A'              TO      M002-OPCA.

           MOVE   -1               TO      M002-OPCL.

           MOVE    WS-MSG-003      TO      M002-MSGO.

           PERFORM 810-00-SEND-ERASE-SAGM002.
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

           MOVE   'A'              TO      M002-OPCA.

           INSPECT M002-OPCI       REPLACING ALL SPACES     BY '_'.
           INSPECT M002-OPCI       REPLACING ALL LOW-VALUES BY '_'.

           IF      M002-OPCI       EQUAL  '_'
                   MOVE 'I'        TO      M002-OPCA
                   MOVE -1         TO      M002-OPCL
                   MOVE WS-MSG-003 TO      M002-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM002
           END-IF.

           IF      M002-OPCO       NOT     NUMERIC OR
                   M002-OPCO       EQUAL   ZEROS
                   MOVE 'I'        TO      M002-OPCA
                   MOVE -1         TO      M002-OPCL
                   MOVE WS-MSG-004 TO      M002-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM002
           END-IF.

           MOVE    M002-OPCO       TO      WS-OPC.

           IF      NOT     WS-OPC-88
                   MOVE 'I'        TO      M002-OPCA
                   MOVE -1         TO      M002-OPCL
                   MOVE WS-MSG-004 TO      M002-MSGO
                   PERFORM         820-00-SEND-DTONLY-SAGM002
           END-IF.

           EVALUATE    M002-OPCO

               WHEN    1
                 MOVE 'GA03'       TO      WS-TRANS

               WHEN    2
                 MOVE 'GA04'       TO      WS-TRANS

               WHEN    3
                 MOVE 'GA05'       TO      WS-TRANS

               WHEN    4
                 MOVE 'GA06'       TO      WS-TRANS

               WHEN    5
                 MOVE 'GA07'       TO      WS-TRANS

               WHEN    6
                 MOVE 'GA08'       TO      WS-TRANS

               WHEN    7
                 MOVE 'GA09'       TO      WS-TRANS

               WHEN    8
                 MOVE 'GA10'       TO      WS-TRANS

               WHEN    9
                 MOVE 'GA11'       TO      WS-TRANS

           END-EVALUATE.

           ADD     001             TO      COMM-IND

           MOVE   'GA02'           TO      COMM-TRANS (COMM-IND).

           EXEC    CICS    START   TRANSID (WS-TRANS)
                                   TERMID  (EIBTRMID)
                                   FROM    (COMM-COMMAREA)
                                   LENGTH  (LENGTH OF COMM-COMMAREA)
           END-EXEC.

           EXEC    CICS    RETURN

           END-EXEC.
      *
      *---------------------------------------------------------------*
       110-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       690-00-TECLA-INVALIDA       SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS-MSG-001      TO      M002-MSGI.

           MOVE   -1               TO      M002-OPCL.

           PERFORM 820-00-SEND-DTONLY-SAGM002.
      *
      *---------------------------------------------------------------*
       690-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       800-00-RECEIVE-MAP          SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    RECEIVE MAP   ('SAGM002')
                           MAPSET        ('SAGM002')
                           INTO          (SAGM002O)
           END-EXEC.
      *
      *---------------------------------------------------------------*
       800-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       810-00-SEND-ERASE-SAGM002   SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 830-00-FORMATTIME.

           EXEC    CICS    SEND    MAP      ('SAGM002')
                                   MAPSET   ('SAGM002')
                                   FROM     (SAGM002O)
                                   ERASE
                                   CURSOR
                                   FREEKB
                                   ALARM
           END-EXEC.

           EXEC    CICS    RETURN  TRANSID ('GA02')
                                   COMMAREA(COMM-COMMAREA)
                                   LENGTH  (LENGTH OF COMM-COMMAREA)
           END-EXEC.


      *
      *---------------------------------------------------------------*
       810-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       820-00-SEND-DTONLY-SAGM002  SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 830-00-FORMATTIME.

           EXEC    CICS    SEND    MAP      ('SAGM002')
                                   MAPSET   ('SAGM002')
                                   FROM     (SAGM002O)
                                   DATAONLY
                                   CURSOR
                                   FREEKB
                                   ALARM
           END-EXEC.

           EXEC    CICS    RETURN  TRANSID ('GA02')
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

           MOVE    COMM-USERID     TO      M002-USERIDO.
           MOVE    WS-ANO-CICS     TO      WS-SA-EDIT.
           MOVE    WS-DT-EDIT      TO      M002-DATAO.
           MOVE    WS-HRS-EDIT     TO      M002-HORAO.

           IF      COMM-FASE       EQUAL   9
                   MOVE '/'        TO      M002-OPCA
           END-IF.
      *
      *---------------------------------------------------------------*
       830-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       997-99-ABCODE               SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    ASSIGN    ABCODE    (WS-ABCODE)

           END-EXEC.

           MOVE    WS-ABCODE       TO      WS-MSG-097 (51:04).

           MOVE    WS-MSG-097      TO      M002-MSGO.

           MOVE    9               TO      COMM-FASE.

           PERFORM 810-00-SEND-ERASE-SAGM002.
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

           MOVE    WS-MSG-098      TO      M002-MSGO.

           MOVE    9               TO      COMM-FASE.

           PERFORM 810-00-SEND-ERASE-SAGM002.
      *
      *---------------------------------------------------------------*
       998-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      *
      /**-----------------------------------------------------------***
      ***                FIM DO PROGRAMA - SAGPO002                 ***
      ***-----------------------------------------------------------***
