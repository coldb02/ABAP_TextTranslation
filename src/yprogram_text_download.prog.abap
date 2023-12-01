*&---------------------------------------------------------------------*
*& Report YPROGRAM_TEXT_DOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yprogram_text_download.

*--- download table data form table 'DD03M'

TABLES: figekwc_repform.
TYPES: BEGIN OF ty_program,
         mdul  TYPE p01_ui_baust,
         pname TYPE programm,
         dynr  TYPE /plmb/rcp_field_type,
         leng  TYPE sqllength,
         key   TYPE /pra/pn_key_str,
         entry TYPE textpooltx,
       END OF ty_program.

DATA: lt_program  TYPE TABLE OF ty_program,
      ls_program  TYPE ty_program,
      lt_textpool TYPE TABLE OF textpool,
      lv_dec      TYPE i,
      lv_dnum     TYPE d020s-dnum,
      lv_prog     TYPE d020s-prog,
      lv_langu    TYPE sy-langu,
      lv_data     TYPE figekwc_repform-repid,
      lt_fld      TYPE STANDARD TABLE OF d021s WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK rad1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_mdul  TYPE p01_ui_baust,
            p_slang TYPE spras DEFAULT sy-langu OBLIGATORY.
SELECT-OPTIONS s_prgna FOR figekwc_repform-repid NO INTERVALS.

SELECTION-SCREEN END OF BLOCK rad1.

START-OF-SELECTION.

*-- To download/extract screen related data
  SELECT *
    FROM d020t
    INTO TABLE @DATA(lt_d020t)
   WHERE prog IN @s_prgna
     AND lang EQ @p_slang
    ORDER BY prog ASCENDING.
  IF sy-subrc = 0.

    LOOP AT lt_d020t INTO DATA(ls_d020t).

      CLEAR: lv_dnum, lv_prog, lv_langu, lt_fld.
      REFRESH: lt_fld.

      lv_dnum = ls_d020t-dynr.
      lv_prog = ls_d020t-prog.
      lv_langu = ls_d020t-lang.

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
    REFRESH: lt_textpool.
    CLEAR: lv_data.
*    lv_data = ls_prgna-prog.
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
      ls_program-dynr   = 'Module Pool Title'    .
      ls_program-key    = ls_text-obj_code .
      ls_program-entry  = ls_text-text .
      ls_program-leng   = '20' .
      APPEND  ls_program TO lt_program.
      CLEAR: ls_program, ls_text.
    ENDLOOP.
    REFRESH: lt_texts.

  ENDLOOP.

  SORT lt_program BY mdul  ASCENDING pname ASCENDING
                     dynr  ASCENDING key   ASCENDING
                     entry ASCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_program COMPARING mdul pname dynr key entry.

  DELETE lt_program WHERE entry IS INITIAL
                       OR entry = '^'
                       OR entry = 'v'
                       OR entry = 'V'
                       OR entry = 'D       .'
                       OR entry = '-'.

  DATA(lv_line) = lines( lt_program ).
  DATA: lv_char TYPE char5.
  lv_char = lv_line.
  MESSAGE: lv_char TYPE 'S'.

  IF lt_program IS NOT INITIAL.

    DATA: layout_settings TYPE REF TO cl_salv_layout,
          layout_key      TYPE salv_s_layout_key.
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

  ENDIF.
