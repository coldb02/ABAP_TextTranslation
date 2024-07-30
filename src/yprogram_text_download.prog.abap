*&---------------------------------------------------------------------*
*& Report YPROGRAM_TEXT_DOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& https://github.com/coldb02/ABAP_TextTranslation ( Download the code
*& from this GITHUB- Abhinandan Dutt)
*&---------------------------------------------------------------------*

REPORT yprogram_text_download.

*--- download table data form table 'DD03M'

TABLES: figekwc_repform, rsrd1, dd03m.

TYPES: BEGIN OF ty_program,
         mdul  TYPE p01_ui_baust,
         pname TYPE programm,
         dynr  TYPE /plmb/rcp_field_type,
         key   TYPE /pra/pn_key_str,
         leng  TYPE sqllength,
         entry TYPE textpooltx,
       END OF ty_program.

DATA: lt_program  TYPE TABLE OF ty_program,
      ls_program  TYPE ty_program,
      lt_textpool TYPE TABLE OF textpool,
      lv_dec      TYPE i,
      lv_dnum     TYPE d020s-dnum,
      lv_prog     TYPE d020s-prog,
      lv_langu    TYPE sy-langu,
      lv_data     TYPE programm,
      lt_fld      TYPE STANDARD TABLE OF d021s WITH HEADER LINE,
      lrs_naming  TYPE REF TO if_oo_clif_incl_naming.

SELECTION-SCREEN BEGIN OF BLOCK rad1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: r1 RADIOBUTTON GROUP rad1 USER-COMMAND opt DEFAULT 'X',
              r2 RADIOBUTTON GROUP rad1,
              r3 RADIOBUTTON GROUP rad1,
              r5 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK rad1.

SELECTION-SCREEN BEGIN OF BLOCK rad2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_mdul  TYPE p01_ui_baust,
              p_slang TYPE spras DEFAULT sy-langu OBLIGATORY.

*--               From Report/Module pool/ Class
  SELECT-OPTIONS: s_prgna  FOR figekwc_repform-repid MODIF ID id1 NO INTERVALS,

*--               DDIC table and Data Element  " Always look form data emelent with Z* because Y* can be standard
                  s_tname1  FOR dd03m-tabname       MODIF ID id2 NO INTERVALS,
                  s_datael  FOR dd03m-rollname        MODIF ID id2 NO INTERVALS,

                  s_struct FOR rsrd1-ddtype_val      MODIF ID id3 NO INTERVALS,

*--               DDIC table and Domain  " Always look for domain with Z* because Y* can be standard
                  s_tname2  FOR rsrd1-tbma_val       MODIF ID id4 NO INTERVALS,
                  s_domnam  FOR dd03m-domname        MODIF ID id4 NO INTERVALS.

SELECTION-SCREEN END OF BLOCK rad2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM: screen_optput.

START-OF-SELECTION.

  CASE abap_true.
    WHEN r1.
      PERFORM: fetch_data_program.
    WHEN r2.
      PERFORM: fetch_data_class.
    WHEN r3.
      PERFORM: fetch_data_ddic_table. "Both for DDIC and structure type
    WHEN r5.
      PERFORM: fetch_data_ddic_domain.

  ENDCASE.
  PERFORM: display_alv.

*&---------------------------------------------------------------------*
*& Form screen_optput
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM screen_optput .

  CASE abap_true.
    WHEN r1 OR r2.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'ID2' OR 'ID3' OR 'ID4'.
            screen-active    = 0.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
    WHEN r3.
      IF s_datael[] IS INITIAL.
        s_datael-sign   = 'I'.
        s_datael-option = 'CP'.
        s_datael-low    = 'Z*'.
        APPEND s_datael.
      ENDIF.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'ID1' OR 'ID3' OR 'ID4'.
            screen-active    = 0.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.

    WHEN r5.
      IF s_domnam[] IS INITIAL.
        s_domnam-sign   = 'I'.
        s_domnam-option = 'CP'.
        s_domnam-low    = 'Z*'.
        APPEND s_domnam.
      ENDIF.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'ID1' OR 'ID2' OR 'ID3'.
            screen-active    = 0.
            MODIFY SCREEN.

        ENDCASE.
      ENDLOOP.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fetch_data_program
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_data_program .

*-- To download/extract screen related data
  CHECK s_prgna IS NOT INITIAL.

  SELECT *
    FROM d020t
    INTO TABLE @DATA(lt_d020t)
   WHERE prog IN @s_prgna
     AND lang EQ @p_slang
    ORDER BY prog ASCENDING.
  IF sy-subrc = 0.

    LOOP AT lt_d020t INTO DATA(ls_d020t).

      CLEAR: lv_dnum, lv_prog, lv_langu, lt_fld.
      FREE: lt_fld.

      lv_dnum  = ls_d020t-dynr.
      lv_prog  = ls_d020t-prog.
      lv_langu = ls_d020t-lang.

      DATA(lv_strlen) = strlen( ls_d020t-dtxt ).

      IF lv_strlen GE 10.
        IF ls_d020t-dtxt+0(10) = 'SEL_SCREEN'.
          DATA(lv_skip_disc) = abap_true.
        ENDIF.
      ELSE.

      ENDIF.

      IF lv_skip_disc = abap_false.
        ls_program-mdul  = p_mdul.
        ls_program-pname = ls_d020t-prog.
        ls_program-dynr  = |Screen DTXT-| & |{ ls_d020t-dynr }|.
        ls_program-key   = ls_d020t-dynr.
        ls_program-leng  = 60.
        ls_program-entry = ls_d020t-dtxt.

        APPEND ls_program TO lt_program.
        CLEAR: ls_program.
      ENDIF.

      CLEAR: lv_skip_disc.

      CALL FUNCTION 'RS_SCRP_GET_SCREEN_INFOS'
        EXPORTING
          dynnr                 = lv_dnum
          progname              = lv_prog
*         WITH_FIELDLIST        = ' '
*         WITH_TEMPLATELIST     = ' '
          text_language         = lv_langu
*       IMPORTING
*         LINES                 =
*         COLUMNS               =
        TABLES
          fieldlist             = lt_fld
*         TEMPLATELIST          =
        EXCEPTIONS
          dynpro_does_not_exist = 1
          no_field_list         = 2
          cancelled             = 3
          OTHERS                = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.

      LOOP AT lt_fld INTO DATA(ls_fld) WHERE fnam IS NOT INITIAL.
        ls_program-mdul  = p_mdul.
        ls_program-pname = ls_d020t-prog.
        ls_program-dynr  = |Screen-| & |{ ls_d020t-dynr }|.
        ls_program-key   = ls_fld-fnam.

        REPLACE ALL OCCURRENCES OF '_' IN ls_fld-stxt WITH ' '.
        ls_program-entry = ls_fld-stxt.

        CLEAR: lv_dec .
        lv_dec = ls_fld-leng.
        ls_program-leng  = lv_dec.

        APPEND ls_program TO lt_program.
        CLEAR: ls_program, ls_program.
      ENDLOOP.
      CLEAR: ls_d020t.

    ENDLOOP.
  ENDIF.

*-- To extract text data and title for the screen dynnr.
  DELETE ADJACENT DUPLICATES FROM lt_d020t COMPARING prog.
  LOOP AT s_prgna INTO DATA(ls_prgna).
    FREE: lt_textpool.
    CLEAR: lv_data.
    lv_data = ls_prgna-low.

    READ TEXTPOOL lv_data INTO lt_textpool LANGUAGE p_slang.
    IF sy-subrc = 0.
      LOOP AT lt_textpool INTO DATA(ls_textpool).
        ls_program-mdul   = p_mdul.
        ls_program-pname  = lv_data.
        ls_program-dynr   = ls_textpool-id.
        CASE ls_textpool-id.
          WHEN 'I'.
            ls_program-dynr    = 'Text Element'.
          WHEN 'R'.
            ls_program-dynr    = 'Report Title'.
          WHEN 'S'.
            ls_program-dynr    = 'Screen Input'.
          WHEN OTHERS.
            ls_program-key    = ls_textpool-key.
        ENDCASE.

        ls_program-key    = ls_textpool-key.
        ls_program-entry  = ls_textpool-entry .
        ls_program-leng   = ls_textpool-length .

        APPEND  ls_program TO lt_program.
        CLEAR: ls_program, ls_textpool.
      ENDLOOP.
    ELSE.
      ls_program-mdul   = p_mdul.
      ls_program-pname = lv_data.
      APPEND  ls_program TO lt_program.
      CLEAR: ls_program, ls_textpool.
    ENDIF.

    SELECT DISTINCT *
      FROM rsmptexts
      INTO TABLE @DATA(lt_texts)
     WHERE progname   EQ @lv_data
       AND   sprsl    EQ @p_slang
       AND   text     NE @space
       AND   obj_type EQ 'T'
       ORDER BY PRIMARY KEY.

    LOOP AT lt_texts INTO DATA(ls_text).
      ls_program-mdul   = p_mdul.
      ls_program-pname  = lv_data.
      ls_program-dynr   = 'Module Pool Title'.
      ls_program-key    = ls_text-obj_code.
      ls_program-entry  = ls_text-text.
      ls_program-leng   = '60'.
      APPEND  ls_program TO lt_program.
      CLEAR: ls_program, ls_text.
    ENDLOOP.
    FREE: lt_texts.

  ENDLOOP.

  DELETE lt_program WHERE entry IS INITIAL
                       OR entry = '^'
                       OR entry = 'v'
                       OR entry = 'V'
                       OR entry = 'D       .'
                       OR entry = '-'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fetch_data_class
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_data_class .

  TYPES: BEGIN OF ty_prg_data,
           progname TYPE programm,
           poolname TYPE programm,
         END OF ty_prg_data,
         ltt_prg_data TYPE STANDARD TABLE OF ty_prg_data WITH DEFAULT KEY.

  DATA: lt_prog_data TYPE TABLE OF ty_prg_data.

  CHECK s_prgna IS NOT INITIAL.

  LOOP AT s_prgna INTO DATA(ls_prgna).

    CLEAR: lrs_naming.
    cl_oo_include_naming=>get_instance_by_name(
      EXPORTING
        name   = ls_prgna-low
      RECEIVING
        cifref = lrs_naming
      EXCEPTIONS
        OTHERS = 1 ).
    IF lrs_naming IS BOUND.
      lt_prog_data = VALUE ltt_prg_data( BASE lt_prog_data ( progname = ls_prgna-low
                                                             poolname = lrs_naming->pool ) ).
    ENDIF.

  ENDLOOP.

  IF lt_prog_data IS NOT INITIAL.
    SELECT b~progname,
           a~progname AS poolname
      FROM reposrc AS a INNER JOIN @lt_prog_data AS b
        ON a~progname = b~poolname
     ORDER BY a~progname ASCENDING
      INTO TABLE @DATA(lt_reposrc).
    IF sy-subrc = 0.

      LOOP AT lt_reposrc INTO DATA(ls_reposrc).
        CLEAR: lv_data.
        FREE: lt_textpool.
        lv_data = ls_reposrc-poolname.
        READ TEXTPOOL lv_data INTO lt_textpool LANGUAGE p_slang.
        LOOP AT lt_textpool INTO DATA(ls_textpool).
          ls_program-mdul   = p_mdul.
          ls_program-pname  = ls_reposrc-poolname.
          ls_program-dynr   = ls_textpool-id.
          CASE ls_textpool-id.
            WHEN 'I'.
              ls_program-dynr = 'Text Element'.
            WHEN 'R'.
              ls_program-dynr = 'Report Title'.
            WHEN OTHERS.
              ls_program-key  = ls_textpool-key.
          ENDCASE.

          ls_program-key    = ls_textpool-key.
          ls_program-entry  = ls_textpool-entry .
          ls_program-leng   = ls_textpool-length .

          APPEND ls_program TO lt_program.
          CLEAR: ls_program, ls_textpool.
        ENDLOOP.

      ENDLOOP.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fetch_data_ddic_table
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_data_ddic_table .

  DATA: lv_run TYPE i.

  CHECK s_tname1 IS NOT INITIAL OR s_datael IS NOT INITIAL.

  IF s_datael IS INITIAL.
    s_datael-sign   = 'I'.
    s_datael-option = 'CP'.
    s_datael-low    = 'Z*'.
    APPEND s_datael.

  ENDIF.

  SELECT *
    FROM dd03m
    INTO TABLE @DATA(lt_dd03m)
   WHERE tabname    IN @s_tname1
     AND rollname   IN @s_datael
     AND ddlanguage EQ @p_slang.
  IF sy-subrc = 0.
    LOOP AT lt_dd03m INTO DATA(ls_dd03m).
      CLEAR: ls_program, lv_run.

*-- Doing this 5 times because a data element can have 5 different translatable texts.
      DO 5 TIMES.
        lv_run += 1.
        ls_program-mdul   = p_mdul.
        ls_program-pname  = ls_dd03m-tabname.
        ls_program-key    = ls_dd03m-rollname.
        CASE lv_run.
          WHEN 1.
            ls_program-dynr  = 'DDTEXT'.
            ls_program-leng  = 60.
            ls_program-entry = ls_dd03m-ddtext.
          WHEN 2.
            ls_program-dynr = 'REPTEXT'.
            ls_program-leng  = ls_dd03m-headlen.
            ls_program-entry = ls_dd03m-reptext.
          WHEN 3.
            ls_program-dynr = 'SCRTEXT_S'.
            ls_program-leng  = ls_dd03m-scrlen1.
            ls_program-entry = ls_dd03m-scrtext_s.
          WHEN 4.
            ls_program-dynr = 'SCRTEXT_M'.
            ls_program-leng  = ls_dd03m-scrlen2.
            ls_program-entry = ls_dd03m-scrtext_m.
          WHEN 5.
            ls_program-dynr = 'SCRTEXT_L'.
            ls_program-leng  = ls_dd03m-scrlen3.
            ls_program-entry = ls_dd03m-scrtext_l.
        ENDCASE.
        APPEND ls_program TO lt_program.
      ENDDO.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fetch_data_ddic_domain
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_data_ddic_domain .

  DATA: lv_trobj_name TYPE trobj_name,
        lv_t_lang     TYPE lxeisolang,
        lv_s_lang     TYPE lxeisolang,
        lv_objtype    TYPE lxeobjtype,
        lv_objname    TYPE lxeobjname,
        lv_domanam    TYPE lxedomanam,
        lv_pstatus    TYPE lxestatprc,
        lv_err_msg    TYPE lxestring,
        lv_flag       TYPE char1,
        lt_colob      TYPE TABLE OF lxe_colob,
        lt_pcx_s1     TYPE TABLE OF lxe_pcx_s1.

  IF s_tname2 IS NOT INITIAL AND s_domnam IS NOT INITIAL.
    SELECT DISTINCT
           b~*
      FROM dd03m AS a INNER JOIN dd01l AS b
        ON a~domname = b~domname
      INTO TABLE @DATA(lt_dd01l)
     WHERE a~tabname    IN @s_tname2
       AND a~domname    IN @s_domnam
       AND a~ddlanguage EQ @p_slang.

  ELSEIF s_tname2 IS INITIAL AND s_domnam IS NOT INITIAL.
    SELECT *
      FROM dd01l
      INTO TABLE @lt_dd01l
     WHERE domname    IN @s_domnam.
  ENDIF.

  IF lt_dd01l IS NOT INITIAL.

    SELECT *
      FROM t002t
      INTO TABLE @DATA(lt_t002t)
     WHERE spras = @p_slang
       AND sprsl IN ( '1', 'C', 'D', 'E', 'F', 'H', 'P', 'Q', 'S', 'd'  ).
    IF sy-subrc = 0.
      LOOP AT lt_t002t INTO DATA(ls_t002t) WHERE sprsl <> p_slang.

        EXIT.
      ENDLOOP.
    ENDIF.

    PERFORM: fetch_source_traget_language USING    p_slang
                                          CHANGING lv_s_lang
                                                   lv_flag.
    IF lv_flag IS NOT INITIAL.
      EXIT.
    ENDIF.

    PERFORM: fetch_source_traget_language USING    ls_t002t-sprsl
                                          CHANGING lv_t_lang
                                                   lv_flag.
    IF lv_flag IS NOT INITIAL.
      EXIT.
    ENDIF.

    LOOP AT lt_dd01l INTO DATA(ls_dd01l).

      FREE: lt_colob, lt_pcx_s1.
      CLEAR: lv_trobj_name.

      lv_trobj_name = ls_dd01l-domname.
      CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
        EXPORTING
          pgmid           = 'LIMU'
          object          = 'DOMD'
          obj_name        = lv_trobj_name
        TABLES
*         IN_E071K        =
          ex_colob        = lt_colob
        EXCEPTIONS
          unknown_object  = 1
          unknown_ta_type = 2
          OTHERS          = 3.
      IF lt_colob IS NOT INITIAL.

        LOOP AT lt_colob INTO DATA(ls_colob).

          FREE: lt_pcx_s1.
          CLEAR: lv_objtype, lv_objname, lv_domanam, lv_pstatus, lv_err_msg.

          lv_objtype = ls_colob-objtype.
          lv_objname = ls_colob-objname.

          CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
            EXPORTING
              t_lang    = lv_t_lang
              s_lang    = lv_s_lang
              custmnr   = '999999'
              objtype   = lv_objtype
              objname   = lv_objname
*             READ_ONLY = 'X'
*             BYPASS_ATTR_BUFFER       = 'X'
*             KEEP_ATTR_BUFFER         = ''
            IMPORTING
*             COLLTYP   =
*             COLLNAM   =
*             DOMATYP   =
              domanam   = lv_domanam
              pstatus   = lv_pstatus
*             O_LANG    =
              err_msg   = lv_err_msg
            TABLES
              lt_pcx_s1 = lt_pcx_s1.

          IF lt_pcx_s1 IS NOT INITIAL.
            CLEAR: ls_program.
            LOOP AT lt_pcx_s1 INTO DATA(ls_pcx_s1).
              ls_program-mdul  = p_mdul.
              ls_program-pname = ls_dd01l-domname.
              ls_program-dynr  = ls_colob-objtype.
              ls_program-key   = ls_pcx_s1-textkey.
              ls_program-leng  = ls_pcx_s1-unitmlt.
              ls_program-entry = ls_pcx_s1-s_text.

              APPEND ls_program TO lt_program.
              CLEAR: ls_program.
            ENDLOOP.

          ENDIF.

        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FETCH_SOURCE_TRAGET_LANGUAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_SLANG
*&      <-- LV_S_LANG
*&      <-- LV_FLAG
*&---------------------------------------------------------------------*
FORM fetch_source_traget_language  USING    p_p_slang   TYPE syst-langu
                                   CHANGING p_lv_s_lang TYPE lxeisolang
                                            p_lv_flag   TYPE char1.

  CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
    EXPORTING
      r3_lang            = p_p_slang
    IMPORTING
      o_language         = p_lv_s_lang
    EXCEPTIONS
      language_not_in_cp = 1
      unknown            = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE 'Language not supported:'(002) && ` ` && p_p_slang TYPE 'I'.
    p_lv_flag = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .

  DATA: layout_settings TYPE REF TO cl_salv_layout,
        layout_key      TYPE salv_s_layout_key.

  IF lt_program IS NOT INITIAL.

    SORT lt_program BY mdul  ASCENDING pname ASCENDING
                       dynr  ASCENDING key   ASCENDING
                       entry ASCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_program COMPARING mdul pname dynr key entry.

    DATA(lv_lines) = lines( lt_program ).
    DATA(lv_message) = |{ TEXT-003 }| & | - | & |{ lv_lines }|.

    MESSAGE: lv_message TYPE 'S'.

    TRY.
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            list_display = if_salv_c_bool_sap=>false
*           r_container  =
*           container_name =
          IMPORTING
            r_salv_table = DATA(gr_alv)
          CHANGING
            t_table      = lt_program.

        DATA(gr_functions) = gr_alv->get_functions( ).
        gr_functions->set_all( abap_true ).

        layout_settings = gr_alv->get_layout( ).

        layout_key-report = sy-repid.
        layout_settings->set_key( layout_key ).
        layout_settings->set_default( value = 'X' ).

        layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

        DATA(gr_columns) = gr_alv->get_columns( ).
        gr_columns->set_optimize( ).

      CATCH cx_salv_msg .
    ENDTRY.

    gr_alv->display( ). "display grid

  ELSE.
    MESSAGE: s001(/accgo/acm_uis_corrc) DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
