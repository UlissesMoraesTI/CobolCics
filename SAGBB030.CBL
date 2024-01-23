      *===============================================================*
       IDENTIFICATION              DIVISION.
      *---------------------------------------------------------------*
      *
       PROGRAM-ID.                 SAGBB030.
       AUTHOR.                     ULISSES & MORAES (TI).
       DATE-WRITTEN.               09/03/2014.
       SECURITY.
      *
      *===============================================================*
      *              ULISSES & MORAES INFORMATICA S/C LTDA            *
      *---------------------------------------------------------------*
      *                                                               *
      *   SISTEMA       : SISAG                                       *
      *   PROJETO       : SISTEMA DE GESTAO DE ALUNOS/CURSOS          *
      *   PROGRAMA      : SAGBB030                                    *
      *   LINGUAGEM     : COBOL / CICS                                *
      *   PROGRAMADOR   : ULISSES & MORAES                            *
      *   ANALISTA      : ULISSES & MORAES                            *
      *   DATA          : 09/03/2014                                  *
      *                                                               *
      *   OBJETIVO      : EXCLUIR ALUNO                               *
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
      *   DATA           : 09/03/2014                                 *
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
              '** INICIO WORKING SAGBB030 **'.
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
      ***      AREA DE MENSAGENS                                    ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGWSMSG.
      *
      /**-----------------------------------------------------------***
      ***      AREA DE COMMAREA                                     ***
      ***-----------------------------------------------------------***
      *
           COPY    SAGWS030.
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
                  '** FINAL WORKING SAGBB030 **'.
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

           MOVE    DFHCOMMAREA     TO      WS030-COMMAREA.

           MOVE    00              TO      WS030-CD-RETORNO.

           MOVE    WS-MSG-026      TO      WS030-MENSAGEM.

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
           IF      WS030-CO-ALUNO  NOT     NUMERIC OR
                   WS030-CO-ALUNO  EQUAL   ZEROS
                   MOVE 01         TO      WS030-CD-RETORNO
                   MOVE WS-MSG-035 TO      WS030-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.
      *
      *---------------------------------------------------------------*
       110-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       200-00-PROCED-PRINCIPAIS    SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 210-00-SELECT-SAGTBS01.

           PERFORM 220-00-DELETE-SAGTBS01.
      *
      *---------------------------------------------------------------*
       200-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       210-00-SELECT-SAGTBS01      SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS030-CO-ALUNO  TO      CO-ALUNO.

           EXEC    SQL

                   SELECT  CO_ALUNO

                   INTO   :SAGTBS01.CO-ALUNO

                   FROM    DB2.SAGTBS01_ALUNOS

                   WHERE   CO_ALUNO = :SAGTBS01.CO-ALUNO

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100
                   MOVE 'SELECT'   TO      WS-COMANDO-DB2
                   MOVE 'SAGTBS01' TO      WS-TABELAS-DB2
                   PERFORM         995-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +100
                   MOVE 99         TO      WS030-CD-RETORNO
                   MOVE WS-MSG-031 TO      WS030-MENSAGEM
                   PERFORM         999-00-PROCED-FINAIS
           END-IF.
      *
      *---------------------------------------------------------------*
       210-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      /===============================================================*
       220-00-DELETE-SAGTBS01      SECTION.
      *---------------------------------------------------------------*
      *
           MOVE    WS030-CO-ALUNO  TO      CO-ALUNO.

           EXEC    SQL     DELETE

                   FROM    DB2.SAGTBS01_ALUNOS

                   WHERE   CO_ALUNO = :SAGTBS01.CO-ALUNO

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE 'DELETE'   TO      WS-COMANDO-DB2
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

           MOVE    WS-MSG-099      TO      WS030-MENSAGEM.

           MOVE    97              TO      WS030-CD-RETORNO.

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

           MOVE    WS-MSG-096      TO      WS030-MENSAGEM.

           MOVE    96              TO      WS030-CD-RETORNO.

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

           MOVE    WS-MSG-097      TO      WS030-MENSAGEM.

           MOVE    99              TO      WS030-CD-RETORNO.

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

           MOVE    WS-MSG-098      TO      WS030-MENSAGEM.

           MOVE    98              TO      WS030-CD-RETORNO.

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
           MOVE    WS030-COMMAREA  TO      DFHCOMMAREA.

           EXEC    CICS    RETURN

           END-EXEC.
      *
      *---------------------------------------------------------------*
       999-99-EXIT.
      *-=-=-=-=-=-*
              EXIT.
      *---------------------------------------------------------------*
      *                  FIM DO PROGRAMA - SAGBB030                   *
      *---------------------------------------------------------------*
