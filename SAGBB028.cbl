      *===============================================================*
       IDENTIFICATION              DIVISION.
      *---------------------------------------------------------------*
      *
       PROGRAM-ID.                 SAGBB028.
       AUTHOR.                     ULISSES & MORAES (TI).
       DATE-WRITTEN.               16/02/2014.
       SECURITY.
      *
      *===============================================================*
      *              ULISSES & MORAES INFORMATICA S/C LTDA            *
      *---------------------------------------------------------------*
      *                                                               *
      *   SISTEMA       : SISAG                                       *
      *   PROJETO       : SISTEMA DE GESTAO DE ALUNOS/CURSOS          *
      *   PROGRAMA      : SAGBB028                                    *
      *   LINGUAGEM     : COBOL / CICS                                *
      *   PROGRAMADOR   : ULISSES & MORAES                            *
      *   ANALISTA      : ULISSES & MORAES                            *
      *   DATA          : 16/02/2014                                  *
      *                                                               *
      *   OBJETIVO      : INCLUIR ALUNO                               *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *   ROTINAS                                                     *
      *                                                               *
      *   NOME             BOOK     DESCRICAO                         *
      *   ---------------- -------- ----------------------------------*
      *   XXXXXXXXXXXXXXXX XXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *   DB2                                                         *
      *                                                               *
      *   NOME               BOOK     DESCRICAO                       *
      *   -----------------  -------- ------------------------------- *
      *   XXXXXXXXXXXXXXXXX  XXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *---------------------------------------------------------------*
      *   HISTORICO DE ALTERACOES                                     *
      *---------------------------------------------------------------*
      *                                                               *
      *   PROGRAMADOR    : ULISSES & MORAES                           *
      *   ANALISTA       : ULISSES & MORAES                           *
      *   DATA           : 21/10/2004                                 *
      *                                                               *
      *   OBJETIVO       : CRIACAO - DESENVOLVIMENTO                  *
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
              '** INICIO WORKING SAGBB028 **'.
      *
      /**-----------------------------------------------------------***
      ***      AREA DE TRATAMENTO DE VARIAVEIS                      ***
      ***-----------------------------------------------------------***
      *
       01          WS-EIBRESP      PIC     9(003) VALUE ZEROS.
       01          WS-ABCODE       PIC     X(004) VALUE SPACES.
      *
       01          WS-COMANDO-DB2  PIC     X(006) VALUE SPACES.
       01          WS-TABELAS-DB2  PIC     X(008) VALUE SPACES.
       01          WS-SQLCODE-DB2  PIC     +++9.
      *
      /**-----------------------------------------------------------***
      ***          AREA DE TRATAMENTO DE DATA/HORA/TIMESTAMP        ***
      ***-----------------------------------------------------------***
      *
       01          WS-DAT-DB2      PIC     X(010) VALUE '99.99.9999'.
       01          FILLER          REDEFINES      WS-DAT-DB2.
         03        WS-DIA-DB2      PIC     9(002).
         03        WS-PT1-DB2      PIC     X(001).
         03        WS-MES-DB2      PIC     9(002).
         03        WS-PT2-DB2      PIC     X(001).
         03        WS-ANO-DB2      PIC     9(004).
      *
       01          WS-DAT-ATU      PIC     X(010) VALUE '99.99.9999'.
       01          FILLER          REDEFINES      WS-DAT-ATU.
         03        WS-DIA-ATU      PIC     9(002).
         03        FILLER          PIC     X(001).
         03        WS-MES-ATU      PIC     9(002).
         03        FILLER          PIC     X(001).
         03        WS-ANO-ATU      PIC     9(004).
      *
      /**-----------------------------------------------------------***
      ***          AREA DE TRATAMENTO DE CPF                        ***
      ***-----------------------------------------------------------***
      *
       01      WS-CPF-11           PIC     9(011) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-CPF-11.
         03    WS-CPF-09           PIC     9(009).
         03    WS-CPF-DG           PIC     9(002).
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
           COPY    SAGWS028.
      *
      /**-----------------------------------------------------------***
      ***          SAGBB006 - CRITICA DE DATAS                      ***
      ***-----------------------------------------------------------***
      *
       01          WS-SAGBB006     PIC     X(008) VALUE 'SAGBB006'.
      *
           COPY    SAGWS006.
      *
      /**-----------------------------------------------------------***
      ***          SAGBBDIG - CALCULA DIGITO VERIFICADOR            ***
      ***-----------------------------------------------------------***
      *
       01      WS-SAGBBDIG         PIC     X(008) VALUE 'SAGBBDIG'.
      *
           COPY    SAGWSDIG    REPLACING  ==:RUCWS:==  BY  ==RUCWS==.
      *
      /**-----------------------------------------------------------***
      ***      AREA DO BOOK DE AREAS DO CICS                        ***
      ***-----------------------------------------------------------***
      *
           COPY    DFHAID.
      *
      /**-----------------------------------------------------------***
      ***      AREA DO BOOK DE ATRIBUTOS                            ***
      ***-----------------------------------------------------------***
      *
           COPY    DFHBMSCA.
      *
      /**-----------------------------------------------------------***
      ***      AREA DE TRATAMENTO DAS BOOKS DB2                     ***
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
                  '** FINAL WORKING SAGBB028 **'.
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       LINKAGE                     SECTION.
      *---------------------------------------------------------------*
      *
       01          DFHCOMMAREA.
         03        FILLER          PIC     X(001)
                   OCCURS   500  DEPENDING  ON  EIBCALEN.
      *
      *===============================================================*
       PROCEDURE                   DIVISION.
      *---------------------------------------------------------------*
      *
           PERFORM 100-00-PROCED-INICIAIS.
 
           PERFORM 200-00-PROCED-PRINCIPAIS.
 
           PERFORM 999-00-PROCED-FINAIS.
      *
      /===============================================================*
       100-00-PROCED-INICIAIS      SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    HANDLE  ABEND
                                   LABEL   (998-00-ABEND)
           END-EXEC.

           EXEC    CICS    HANDLE  CONDITION
                                   ERROR   (997-00-ERROR)
           END-EXEC.

           MOVE    DFHCOMMAREA     TO      WS028-COMMAREA.

           MOVE    00              TO      WS028-CD-RETORNO.

           MOVE    WS-MSG-028      TO      WS028-MENSAGEM.

           MOVE    ZEROS           TO      WS028-CO-ALUNO.

           PERFORM 110-00-CONS-FIS-LOG.
      *
      *---------------------------------------------------------------*
       100-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       110-00-CONS-FIS-LOG         SECTION.
      *---------------------------------------------------------------*
      *
           INSPECT WS028-NO-ALUNO      REPLACING ALL LOW-VALUES BY SPACE
           INSPECT WS028-DT-NASCIMENTO REPLACING ALL LOW-VALUES BY SPACE

           IF      WS028-NO-ALUNO  EQUAL   SPACES
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-057 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           IF      WS028-DT-NASCIMENTO
                                   EQUAL   SPACES
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-058 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           MOVE    WS028-DT-NASCIMENTO
                                   TO      WS-DAT-DB2.

           MOVE   '.'              TO      WS-PT1-DB2.
           MOVE   '.'              TO      WS-PT2-DB2.

           IF      WS-DIA-DB2      NOT     NUMERIC OR
                   WS-DIA-DB2      EQUAL   00      OR
                   WS-DIA-DB2      GREATER 31
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-059 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           IF      WS-MES-DB2      NOT     NUMERIC OR
                   WS-MES-DB2      EQUAL   00      OR
                   WS-MES-DB2      GREATER 12
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-059 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           IF      WS-ANO-DB2      NOT     NUMERIC OR
                   WS-ANO-DB2      EQUAL   00
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-059 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           EXEC    SQL
                   SET :WS-DAT-ATU = CURRENT DATE
           END-EXEC.

           IF      WS-ANO-DB2      GREATER WS-ANO-ATU OR
                   WS-ANO-DB2      LESS    1900
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-059 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           PERFORM 120-00-LINK-SAGBB006.

           MOVE    WS-DAT-DB2      TO      WS028-DT-NASCIMENTO.

           IF      WS028-IC-SEXO
                               NOT EQUAL   'M' AND 'F'
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-060 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           IF      WS028-NU-CPF    NOT     NUMERIC OR
                   WS028-NU-CPF    EQUAL   ZEROS
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-061 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           PERFORM 130-00-LINK-SAGBBDIG.

           IF      WS028-CO-LOCAL-CEL
                                   NOT     NUMERIC
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-063 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           IF      WS028-NU-TELEF-CEL
                                   NOT     NUMERIC
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-064 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.

           INSPECT WS028-NO-E-MAIL REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT WS028-NO-OBS    REPLACING ALL LOW-VALUES BY SPACES.
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

           MOVE    WS-DIA-DB2      TO      WRD-DATA01-DD.
           MOVE    WS-MES-DB2      TO      WRD-DATA01-MM.
           MOVE    WS-ANO-DB2      TO      WRD-DATA01-AA.

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
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-059 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.
      *
      *---------------------------------------------------------------*
       120-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       130-00-LINK-SAGBBDIG        SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    02              TO      RUCWS-ACAO.

           MOVE    WS028-NU-CPF    TO      WS-CPF-11.

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
                   MOVE 01         TO      WS028-CD-RETORNO
                   MOVE WS-MSG-061 TO      WS028-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.
      *
      *---------------------------------------------------------------*
       130-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       200-00-PROCED-PRINCIPAIS    SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 210-00-SELMAX-SAGTBS01.

           PERFORM 220-00-INSERT-SAGTBS01.
      *
      *---------------------------------------------------------------*
       200-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       210-00-SELMAX-SAGTBS01      SECTION.
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
       210-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       220-00-INSERT-SAGTBS01      SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    CO-ALUNO        TO      WS028-CO-ALUNO.

           MOVE    WS028-NO-ALUNO  TO      NO-ALUNO.

           MOVE    WS028-DT-NASCIMENTO
                                   TO      DT-NASCIMENTO.

           MOVE    WS028-IC-SEXO   TO      IC-SEXO.

           MOVE    WS028-NU-CPF    TO      NU-CPF.

           MOVE    -1              TO      WS-CO-LOCAL-CEL-NULL.
           MOVE    -1              TO      WS-NU-TELEF-CEL-NULL.
           MOVE    -1              TO      WS-NO-E-MAIL-NULL.
           MOVE    -1              TO      WS-NO-OBS-NULL.

           IF      WS028-NU-TELEF-CEL
                               NOT EQUAL   ZEROS
                   MOVE +0         TO      WS-CO-LOCAL-CEL-NULL
                   MOVE +0         TO      WS-NU-TELEF-CEL-NULL
                   MOVE WS028-CO-LOCAL-CEL
                                   TO      CO-LOCAL-CEL
                   MOVE WS028-NU-TELEF-CEL
                                   TO      NU-TELEF-CEL
           END-IF.

           IF      WS028-NO-E-MAIL
                                NOT EQUAL  SPACES
                   MOVE +0          TO      WS-NO-E-MAIL-NULL
           END-IF.

           IF      WS028-NO-OBS NOT EQUAL  SPACES
                   MOVE +0          TO     WS-NO-OBS-NULL
           END-IF.

           MOVE    WS028-NO-E-MAIL TO      NO-E-MAIL.
           MOVE    WS028-NO-OBS    TO      NO-OBS.

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
       220-99-EXIT.
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

           MOVE    WS-MSG-099      TO      WS028-MENSAGEM.

           MOVE    97              TO      WS028-CD-RETORNO.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           PERFORM 999-00-PROCED-FINAIS.
      *
      *---------------------------------------------------------------*
       995-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       996-00-ABEND-SUB            SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           MOVE    WS-MSG-096      TO      WS028-MENSAGEM.

           MOVE    96              TO      WS028-CD-RETORNO.

           PERFORM 999-00-PROCED-FINAIS.
      *
      *---------------------------------------------------------------*
       996-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       998-00-ABEND                SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS   ASSIGN   ABCODE    (WS-ABCODE)

           END-EXEC.

           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           MOVE    WS-ABCODE       TO      WS-MSG-099(51:03).

           MOVE    WS-MSG-097      TO      WS028-MENSAGEM.

           MOVE    99              TO      WS028-CD-RETORNO.

           PERFORM 999-00-PROCED-FINAIS.
      *
      *---------------------------------------------------------------*
       998-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       997-00-ERROR                SECTION.
      *---------------------------------------------------------------*
      *
           EXEC    CICS    SYNCPOINT   ROLLBACK

           END-EXEC.

           MOVE    EIBRESP         TO      WS-EIBRESP.

           MOVE    WS-EIBRESP      TO      WS-MSG-098(48:03).

           MOVE    WS-MSG-098      TO      WS028-MENSAGEM.

           MOVE    98              TO      WS028-CD-RETORNO.

           PERFORM 999-00-PROCED-FINAIS.
      *
      *---------------------------------------------------------------*
       997-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       999-00-PROCED-FINAIS        SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS028-COMMAREA  TO      DFHCOMMAREA.

           EXEC    CICS    RETURN

           END-EXEC.
      *
      *---------------------------------------------------------------*
       999-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      *---------------------------------------------------------------*
      *                  FIM DO PROGRAMA - SAGBB028                   *
      *---------------------------------------------------------------*
