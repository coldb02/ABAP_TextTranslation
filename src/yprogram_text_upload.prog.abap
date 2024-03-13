*&---------------------------------------------------------------------*
*& Report YPROGRAM_TEXT_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yprogram_text_upload.

TABLES: figekwc_repform, t002.

TYPES: BEGIN OF ty_program,
         mdul     TYPE p01_ui_baust,
         pname    TYPE programm,
         dynr     TYPE /plmb/rcp_field_type,
         key      TYPE /pra/pn_key_str,
         leng     TYPE sqllength,
         entry_en TYPE textpooltx,
         entry_hu TYPE textpooltx,
       END OF ty_program.

TYPES: BEGIN OF ty_output,
         mdul          TYPE p01_ui_baust,
         pname         TYPE programm,
         dynr          TYPE /plmb/rcp_field_type,
         key           TYPE /pra/pn_key_str,
         leng_sys      TYPE sqllength,
         leng_prop     TYPE sqllength,
         text_key      TYPE lxetextkey,
         entry_en_sys  TYPE textpooltx,
         entry_en_prop TYPE textpooltx,
         entry_hu_sys  TYPE textpooltx,
         entry_hu_prop TYPE textpooltx,
       END OF ty_output.

DATA: gt_program TYPE TABLE OF ty_program,
      gs_program TYPE ty_program,
      gt_dislay  TYPE TABLE OF ty_program,
      gs_output  TYPE ty_output,
      gt_output  TYPE TABLE OF ty_output.

SELECTION-SCREEN BEGIN OF BLOCK rad1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: r1 RADIOBUTTON GROUP rad1 DEFAULT 'X',
              r2 RADIOBUTTON GROUP rad1,
              r3 RADIOBUTTON GROUP rad1.
  PARAMETERS: p_upload TYPE localfile OBLIGATORY,
              p_slang  TYPE spras DEFAULT sy-langu OBLIGATORY,
              p_tlang  TYPE spras OBLIGATORY.
  SELECT-OPTIONS s_prgna FOR figekwc_repform-repid NO INTERVALS NO-EXTENSION OBLIGATORY.
  PARAMETERS: p_save AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN END OF BLOCK rad1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR  p_upload .
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
*     FIELD_NAME    = ' '
    IMPORTING
      file_name     = p_upload.

START-OF-SELECTION.


  PERFORM: validate_labguage,
           fetch_excel_data.
*-- determin the type of translation.

  IF r1 = 'X'.
*--  CASE Module Pool/ Module pool Title, Text Elements and Report Titles.
    PERFORM: process_data_mp.
  ELSEIF r2 = 'X'.
*-- Case Message class .
    PERFORM: process_data_mc.

  ELSEIF r3 = 'X'.
*-- CASE Data Type.
    PERFORM: process_data_dt.

  ENDIF.

END-OF-SELECTION.
  PERFORM: display_alv.

*&---------------------------------------------------------------------*
*& Form FETCH_EXCEL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_excel_data .

*-- Had to create the copy of the existing FM and the data type was modifed as well
*-- Structure       -->Field  -->Component Type
*-- YALSMEX_TABLINE -->VALUE  -->TEXTPOOLTX

  DATA: lv_upload TYPE rlgrap-filename,
        lt_excl   TYPE STANDARD TABLE OF yalsmex_tabline.

  FIELD-SYMBOLS: <lv_field> TYPE any.

  lv_upload = p_upload.

*-- Copy of the original FM ALSM_EXCEL_TO_INTERNAL_TABLE
  CALL FUNCTION 'YALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lv_upload
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 8
      i_end_row               = 99999
    TABLES
      intern                  = lt_excl
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE TEXT-e20 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSE.
    IF lt_excl[] IS INITIAL.
      MESSAGE TEXT-e20 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.
  REFRESH: gt_program.
  LOOP AT lt_excl ASSIGNING FIELD-SYMBOL(<ls_data>).
    ASSIGN <ls_data>-value TO <lv_field>.
    CASE <ls_data>-col.
      WHEN 1 .
        gs_program-mdul    = <lv_field>.
      WHEN 2 .
        gs_program-pname    = <lv_field>.
      WHEN 3 .
        gs_program-dynr     = <lv_field>.
      WHEN 4 .
        gs_program-key      = <lv_field>.
      WHEN 5 .
        gs_program-leng     = <lv_field>.
      WHEN 6.
        gs_program-entry_en = <lv_field>.
      WHEN 7.
        gs_program-entry_hu = <lv_field>.
    ENDCASE .
    AT END OF row.
      APPEND gs_program TO gt_program.
      CLEAR gs_program.
    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA_MP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data_mp .

  DATA: lt_colob_tmp  TYPE TABLE OF lxe_colob,
        lt_colob      TYPE TABLE OF lxe_colob,
        lt_lxe_pcx    TYPE TABLE OF lxe_pcx_s1,
        lv_pname      TYPE programm,
        lv_objname    TYPE trobj_name,
        lv_trobj_name TYPE trobjtype,
        lv_dynnr      TYPE syst-dynnr,
        lv_lxestatprc TYPE lxestatprc,
        lv_lxestring  TYPE lxestring,
        lv_t_lang     TYPE lxeisolang,
        lv_s_lang     TYPE lxeisolang,
        lv_flag       TYPE char1.

  CLEAR: lv_pname, lv_objname, lv_trobj_name, lv_dynnr, lv_lxestatprc,
         lv_lxestring, lv_t_lang, lv_s_lang, lv_flag.
  REFRESH: lt_colob_tmp, lt_colob, lt_lxe_pcx.

  SORT gt_program BY pname ASCENDING
                     dynr  ASCENDING.
  DELETE gt_program WHERE pname NOT IN s_prgna.

  DATA(lt_program_tmp) = gt_program.
  IF line_exists( lt_program_tmp[ dynr = 'Text Element'] ).
    DELETE lt_program_tmp WHERE dynr = 'Report Title' OR dynr = 'Screen Input'.
  ELSE.
    DELETE lt_program_tmp WHERE dynr = 'Screen Input'.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM lt_program_tmp COMPARING pname dynr.

*-- Fetech Source and target language
*--  Change Source and target language to readable format of the read FM
  PERFORM: fetch_source_traget_language USING    p_slang
                                        CHANGING lv_s_lang
                                                 lv_flag.
  IF lv_flag IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM: fetch_source_traget_language USING    p_tlang
                                        CHANGING lv_t_lang
                                                 lv_flag.
  IF lv_flag IS NOT INITIAL.
    EXIT.
  ENDIF.

  REFRESH: gt_output.
  LOOP AT lt_program_tmp INTO DATA(ls_program_tmp).
    CLEAR: lv_pname, lv_dynnr, lv_objname, lv_trobj_name.
    REFRESH: lt_colob_tmp.

    CASE ls_program_tmp-dynr.
      WHEN 'Text Element' OR 'Report Title' OR 'H'.  "H -> List Heading
        lv_trobj_name = 'REPT'.
        lv_objname    = ls_program_tmp-pname.
      WHEN 'Module Pool Title'.
        lv_trobj_name = 'CUAD'.
        lv_objname    = ls_program_tmp-pname.
      WHEN OTHERS.
        lv_pname = ls_program_tmp-pname.
        SPLIT ls_program_tmp-dynr AT '-' INTO DATA(lv_1) lv_dynnr.
        CONCATENATE lv_pname lv_dynnr INTO lv_objname RESPECTING BLANKS.
        lv_trobj_name = 'DYNP'.
    ENDCASE.

    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid           = 'LIMU'
        object          = lv_trobj_name
        obj_name        = lv_objname
      TABLES
*       IN_E071K        =
        ex_colob        = lt_colob_tmp
      EXCEPTIONS
        unknown_object  = 1
        unknown_ta_type = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.

* Implement suitable error handling here
    ENDIF.

    APPEND LINES OF lt_colob_tmp TO lt_colob.
    CLEAR: ls_program_tmp.
  ENDLOOP.

  SORT lt_colob BY objtype ASCENDING
                   objname ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_colob COMPARING objtype objname.

*-- Fetch possibe texts to be translated.
  CLEAR: gs_output.
  REFRESH: lt_lxe_pcx.

  LOOP AT lt_colob INTO DATA(ls_colob).

    REFRESH: lt_lxe_pcx.
    CLEAR: lv_lxestatprc, lv_lxestring.
    CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
      EXPORTING
        t_lang    = lv_t_lang
        s_lang    = lv_s_lang
        custmnr   = '999999'
        objtype   = ls_colob-objtype
        objname   = ls_colob-objname
        read_only = ' '
*       BYPASS_ATTR_BUFFER       = 'X'
*       KEEP_ATTR_BUFFER         = ''
      IMPORTING
*       COLLTYP   =
*       COLLNAM   =
*       DOMATYP   =
*       DOMANAM   =
        pstatus   = lv_lxestatprc
*       O_LANG    =
        err_msg   = lv_lxestring
      TABLES
        lt_pcx_s1 = lt_lxe_pcx
*       LT_WHERECLAUS            =
      .
    IF lv_lxestatprc <> 'S'.
      IF lv_lxestring IS NOT INITIAL.
        MESSAGE lv_lxestring TYPE 'E'.
        EXIT.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.
    ENDIF.
*-- Update the traget translation if there are any in the excel.
    DATA(lv_dynnr_txt) = |{ 'Screen' }| & |-| & |{ ls_colob-objname+40(4) }|.
    LOOP AT lt_lxe_pcx ASSIGNING FIELD-SYMBOL(<lfs_les_pcx_tmp>).
      CASE ls_colob-objtype.
        WHEN 'SRH1' OR 'CAD1'.

        WHEN OTHERS.
          CLEAR: gs_program.
          gs_output-leng_sys      = <lfs_les_pcx_tmp>-unitmlt.
          gs_output-entry_en_sys  = <lfs_les_pcx_tmp>-s_text.
          gs_output-entry_hu_sys  =  <lfs_les_pcx_tmp>-t_text.
*-- Module pool Screen
          LOOP AT gt_program INTO gs_program WHERE dynr     = lv_dynnr_txt
                                               AND entry_en = <lfs_les_pcx_tmp>-s_text.
            <lfs_les_pcx_tmp>-t_text = gs_program-entry_hu.
            EXIT.
          ENDLOOP.
*-- With some text
          IF sy-subrc <> 0.
            SPLIT <LFS_LES_PCX_TMP>-TEXTKEY AT ' ' INTO DATA(lv_sample_text) DATA(lv_b).
            LOOP AT gt_program INTO gs_program WHERE dynr     CS lv_sample_text
                                                 AND entry_en EQ <lfs_les_pcx_tmp>-s_text.
              <lfs_les_pcx_tmp>-t_text = gs_program-entry_hu.
              EXIT.
            ENDLOOP.
          ENDIF.
*-- Search whole
          IF sy-subrc <> 0.
            LOOP AT gt_program INTO gs_program WHERE entry_en = <lfs_les_pcx_tmp>-s_text.
              <lfs_les_pcx_tmp>-t_text = gs_program-entry_hu.
              EXIT.
            ENDLOOP.
          ENDIF.
          gs_output-mdul          = gs_program-mdul.
          gs_output-pname         = gs_program-pname.
          gs_output-dynr          = gs_program-dynr.
          gs_output-key           = gs_program-key.
          gs_output-leng_prop     = gs_program-leng.
          gs_output-text_key      = <lfs_les_pcx_tmp>-textkey.
          gs_output-entry_en_prop = gs_program-entry_en.
          gs_output-entry_hu_prop = gs_program-entry_hu.
          APPEND gs_output TO gt_output.
          CLEAR: gs_output, gs_program.
      ENDCASE.
    ENDLOOP.

*-- save the translation as per the excel.
    IF p_save = 'X'.
      CLEAR: lv_lxestatprc, lv_lxestring.
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
        EXPORTING
          t_lang    = lv_t_lang
          s_lang    = lv_s_lang
          custmnr   = '999999'
          objtype   = ls_colob-objtype
          objname   = ls_colob-objname
*         AUTODIST  =
*         RFC_COPY  =
        IMPORTING
          pstatus   = lv_lxestatprc
          err_msg   = lv_lxestring
        TABLES
          lt_pcx_s1 = lt_lxe_pcx.
      IF lv_lxestatprc <> 'S'.
        IF lv_lxestring IS NOT INITIAL.
          MESSAGE lv_lxestring TYPE 'E'.
          EXIT.
        ELSE.
          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

      ENDIF.
      MESSAGE: '\(^_^)/ Translation Completed' TYPE 'S'.
    ENDIF.

    CLEAR: ls_colob, lv_dynnr_txt.
    REFRESH: lt_lxe_pcx.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA_MC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data_mc .

  DATA: lt_colob_tmp  TYPE TABLE OF lxe_colob,
        lt_colob      TYPE TABLE OF lxe_colob,
        lt_lxe_pcx    TYPE TABLE OF lxe_pcx_s1,
        lv_pname      TYPE programm,
        lv_objname    TYPE trobj_name,
        lv_trobj_name TYPE trobjtype,
        lv_dynnr      TYPE syst-dynnr,
        lv_lxestatprc TYPE lxestatprc,
        lv_lxestring  TYPE lxestring,
        lv_t_lang     TYPE lxeisolang,
        lv_s_lang     TYPE lxeisolang,
        lv_flag       TYPE char1.

  CLEAR: lv_pname, lv_objname, lv_trobj_name, lv_dynnr, lv_lxestatprc, lv_lxestring, lv_t_lang, lv_s_lang, lv_flag.
  REFRESH: lt_colob_tmp, lt_colob, lt_lxe_pcx.

  SORT gt_program BY pname ASCENDING
                     dynr  ASCENDING
                     key   ASCENDING.

  DATA(lt_program_tmp) = gt_program.
  DELETE ADJACENT DUPLICATES FROM lt_program_tmp COMPARING pname dynr.

*-- Fetech Source and target language
*--  Change Source and target language to readable format of the read FM
  PERFORM: fetch_source_traget_language USING    p_slang
                                        CHANGING lv_s_lang
                                                 lv_flag.
  IF lv_flag IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM: fetch_source_traget_language USING    p_tlang
                                        CHANGING lv_t_lang
                                                 lv_flag.
  IF lv_flag IS NOT INITIAL.
    EXIT.
  ENDIF.

  REFRESH: gt_output.
  LOOP AT lt_program_tmp INTO DATA(ls_program_tmp).
    CLEAR: lv_objname.
    REFRESH: lt_colob_tmp.

    lv_objname = ls_program_tmp-pname.

    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid           = 'R3TR'
        object          = 'MSAG'
        obj_name        = lv_objname
      TABLES
*       IN_E071K        =
        ex_colob        = lt_colob_tmp
      EXCEPTIONS
        unknown_object  = 1
        unknown_ta_type = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.

* Implement suitable error handling here
    ENDIF.

    APPEND LINES OF lt_colob_tmp TO lt_colob.
    CLEAR: ls_program_tmp.
  ENDLOOP.

  SORT lt_colob BY objtype ASCENDING
                   objname ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_colob COMPARING objtype objname.

*-- Fetch possibe texts to be translated.

  CLEAR: gs_output.
  REFRESH: lt_lxe_pcx.

  LOOP AT lt_colob INTO DATA(ls_colob).
    DATA(lv_dynnr_txt) = ls_colob-objname+20(3).
    DATA(ls_program) = VALUE #( gt_program[ key = lv_dynnr_txt ] OPTIONAL ).
    IF ls_program IS INITIAL.
      CONTINUE.
    ELSE.
      CLEAR: ls_program.
    ENDIF.

    REFRESH: lt_lxe_pcx.
    CLEAR: lv_lxestatprc, lv_lxestring.
    CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
      EXPORTING
        t_lang    = lv_t_lang
        s_lang    = lv_s_lang
        custmnr   = '999999'
        objtype   = ls_colob-objtype
        objname   = ls_colob-objname
        read_only = ' '
*       BYPASS_ATTR_BUFFER       = 'X'
*       KEEP_ATTR_BUFFER         = ''
      IMPORTING
*       COLLTYP   =
*       COLLNAM   =
*       DOMATYP   =
*       DOMANAM   =
        pstatus   = lv_lxestatprc
*       O_LANG    =
        err_msg   = lv_lxestring
      TABLES
        lt_pcx_s1 = lt_lxe_pcx
*       LT_WHERECLAUS            =
      .
    IF lv_lxestatprc <> 'S'.
      IF lv_lxestring IS NOT INITIAL.
        MESSAGE lv_lxestring TYPE 'E'.
        EXIT.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.
    ENDIF.
*-- Update the traget translation if there are any in the excel.
*    DATA(lv_dynnr_txt) = ls_colob-objname+20(3).
    LOOP AT lt_lxe_pcx ASSIGNING FIELD-SYMBOL(<lfs_les_pcx_tmp>).

      CASE ls_colob-objtype.
        WHEN 'MSAG'.

        WHEN OTHERS.
          CLEAR: gs_program.
          gs_output-leng_sys      = <lfs_les_pcx_tmp>-unitmlt.
          gs_output-entry_en_sys  = <lfs_les_pcx_tmp>-s_text.
          gs_output-entry_hu_sys  =  <lfs_les_pcx_tmp>-t_text.
          LOOP AT gt_program INTO gs_program WHERE key      = lv_dynnr_txt
                                               AND entry_en = <lfs_les_pcx_tmp>-s_text.
            <lfs_les_pcx_tmp>-t_text = gs_program-entry_hu.
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            LOOP AT gt_program INTO gs_program WHERE entry_en = <lfs_les_pcx_tmp>-s_text.
              <lfs_les_pcx_tmp>-t_text = gs_program-entry_hu.
              EXIT.
            ENDLOOP.
          ENDIF.
          gs_output-mdul          = gs_program-mdul.
          gs_output-pname         = gs_program-pname.
          gs_output-dynr          = gs_program-dynr.
          gs_output-key           = gs_program-key.
          gs_output-leng_prop     = gs_program-leng.
          gs_output-text_key      = <lfs_les_pcx_tmp>-textkey.
          gs_output-entry_en_prop = gs_program-entry_en.
          gs_output-entry_hu_prop = gs_program-entry_hu.
          APPEND gs_output TO gt_output.
          CLEAR: gs_output, gs_program.
      ENDCASE.

    ENDLOOP.

*-- save the translation as per the excel.
    IF p_save = 'X'.
      CLEAR: lv_lxestatprc, lv_lxestring.
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
        EXPORTING
          t_lang    = lv_t_lang
          s_lang    = lv_s_lang
          custmnr   = '999999'
          objtype   = ls_colob-objtype
          objname   = ls_colob-objname
*         AUTODIST  =
*         RFC_COPY  =
        IMPORTING
          pstatus   = lv_lxestatprc
          err_msg   = lv_lxestring
        TABLES
          lt_pcx_s1 = lt_lxe_pcx.
      IF lv_lxestatprc <> 'S'.
        IF lv_lxestring IS NOT INITIAL.
          MESSAGE lv_lxestring TYPE 'E'.
          EXIT.
        ELSE.
          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

      ENDIF.
      MESSAGE: '\(^_^)/ Translation Completed' TYPE 'S'.
    ENDIF.

    CLEAR: ls_colob, lv_dynnr_txt.
    REFRESH: lt_lxe_pcx.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA_DT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data_dt .

  DATA: lt_colob_tmp  TYPE TABLE OF lxe_colob,
        lt_colob      TYPE TABLE OF lxe_colob,
        lt_lxe_pcx    TYPE TABLE OF lxe_pcx_s1,
        lv_pname      TYPE programm,
        lv_objname    TYPE trobj_name,
        lv_trobj_name TYPE trobjtype,
        lv_dynnr      TYPE syst-dynnr,
        lv_lxestatprc TYPE lxestatprc,
        lv_lxestring  TYPE lxestring,
        lv_t_lang     TYPE lxeisolang,
        lv_s_lang     TYPE lxeisolang,
        lv_flag       TYPE char1.

  CLEAR: lv_pname, lv_objname, lv_trobj_name, lv_dynnr, lv_lxestatprc, lv_lxestring, lv_t_lang, lv_s_lang, lv_flag.
  REFRESH: lt_colob_tmp, lt_colob, lt_lxe_pcx.

  SORT gt_program BY pname ASCENDING
                     dynr  ASCENDING
                     key   ASCENDING.

  DATA(lt_program_tmp) = gt_program.
  DELETE ADJACENT DUPLICATES FROM lt_program_tmp COMPARING pname dynr key.

*-- Fetech Source and target language
*--  Change Source and target language to readable format of the read FM
  PERFORM: fetch_source_traget_language USING    p_slang
                                        CHANGING lv_s_lang
                                                 lv_flag.
  IF lv_flag IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM: fetch_source_traget_language USING    p_tlang
                                        CHANGING lv_t_lang
                                                 lv_flag.
  IF lv_flag IS NOT INITIAL.
    EXIT.
  ENDIF.

  REFRESH: gt_output.
  LOOP AT lt_program_tmp INTO DATA(ls_program_tmp).
    CLEAR: lv_objname.
    REFRESH: lt_colob_tmp.

    lv_objname = ls_program_tmp-key.

    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid           = 'LIMU'
        object          = 'DTED'
        obj_name        = lv_objname
      TABLES
*       IN_E071K        =
        ex_colob        = lt_colob_tmp
      EXCEPTIONS
        unknown_object  = 1
        unknown_ta_type = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.

* Implement suitable error handling here
    ENDIF.

    APPEND LINES OF lt_colob_tmp TO lt_colob.
    CLEAR: ls_program_tmp.
  ENDLOOP.

  SORT lt_colob BY objtype ASCENDING
                   objname ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_colob COMPARING objtype objname.

*-- Fetch possibe texts to be translated.

  CLEAR: gs_output.
  REFRESH: lt_lxe_pcx.

  LOOP AT lt_colob INTO DATA(ls_colob).

    REFRESH: lt_lxe_pcx.
    CLEAR: lv_lxestatprc, lv_lxestring.
    CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
      EXPORTING
        t_lang    = lv_t_lang
        s_lang    = lv_s_lang
        custmnr   = '999999'
        objtype   = ls_colob-objtype
        objname   = ls_colob-objname
        read_only = ' '
*       BYPASS_ATTR_BUFFER       = 'X'
*       KEEP_ATTR_BUFFER         = ''
      IMPORTING
*       COLLTYP   =
*       COLLNAM   =
*       DOMATYP   =
*       DOMANAM   =
        pstatus   = lv_lxestatprc
*       O_LANG    =
        err_msg   = lv_lxestring
      TABLES
        lt_pcx_s1 = lt_lxe_pcx
*       LT_WHERECLAUS            =
      .
    IF lv_lxestatprc <> 'S'.
      IF lv_lxestring IS NOT INITIAL.
        MESSAGE lv_lxestring TYPE 'E'.
        EXIT.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.
    ENDIF.
*-- Update the traget translation if there are any in the excel.
    LOOP AT lt_lxe_pcx ASSIGNING FIELD-SYMBOL(<lfs_les_pcx_tmp>).

      CLEAR: gs_program.
      gs_output-leng_sys      = <lfs_les_pcx_tmp>-unitmlt.
      gs_output-entry_en_sys  = <lfs_les_pcx_tmp>-s_text.
      gs_output-entry_hu_sys  =  <lfs_les_pcx_tmp>-t_text.

      LOOP AT gt_program INTO gs_program WHERE dynr     = <lfs_les_pcx_tmp>-textkey
                                           AND key      = ls_colob-objname
                                           AND entry_en = <lfs_les_pcx_tmp>-s_text.
        <lfs_les_pcx_tmp>-t_text = gs_program-entry_hu.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0 .
        LOOP AT gt_program INTO gs_program WHERE dynr     = <lfs_les_pcx_tmp>-textkey
                                             AND entry_en = <lfs_les_pcx_tmp>-s_text.
          <lfs_les_pcx_tmp>-t_text = gs_program-entry_hu.
          EXIT.
        ENDLOOP.
      ENDIF.
      gs_output-mdul          = gs_program-mdul.
      gs_output-pname         = gs_program-pname.
      gs_output-dynr          = gs_program-dynr.
      gs_output-key           = gs_program-key.
      gs_output-leng_prop     = gs_program-leng.
      gs_output-text_key      = <lfs_les_pcx_tmp>-textkey.
      gs_output-entry_en_prop = gs_program-entry_en.
      gs_output-entry_hu_prop = gs_program-entry_hu.
      APPEND gs_output TO gt_output.
      CLEAR: gs_output, gs_program.

    ENDLOOP.

*-- save the translation as per the excel.
    IF p_save = 'X'.
      CLEAR: lv_lxestatprc, lv_lxestring.
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
        EXPORTING
          t_lang    = lv_t_lang
          s_lang    = lv_s_lang
          custmnr   = '999999'
          objtype   = ls_colob-objtype
          objname   = ls_colob-objname
*         AUTODIST  =
*         RFC_COPY  =
        IMPORTING
          pstatus   = lv_lxestatprc
          err_msg   = lv_lxestring
        TABLES
          lt_pcx_s1 = lt_lxe_pcx.
      IF lv_lxestatprc <> 'S'.
        IF lv_lxestring IS NOT INITIAL.
          MESSAGE lv_lxestring TYPE 'E'.
          EXIT.
        ELSE.
          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

      ENDIF.
      MESSAGE: '\(^_^)/ Translation Completed' TYPE 'S'.
    ENDIF.

    CLEAR: ls_colob.
    REFRESH: lt_lxe_pcx.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .

  IF gt_output IS NOT INITIAL.
    DELETE gt_output WHERE pname IS INITIAL.
    IF p_save <> 'X'.
      DATA(lv_lines1) = lines( gt_program ).
      DATA(lv_lines2) = lines( gt_output ).
      DATA(lv_message) = |{ 'Upload Lines ' }| &
                         | | &
                         | 'From Excel-'| &
                         |{ lv_lines1 }| &
                         | | &
                         | 'To upload-' | &
                         |{ lv_lines2 }|.
      MESSAGE: s185(zwm_msg) WITH lv_message.
    ENDIF.


    DATA: layout_settings TYPE REF TO cl_salv_layout,
          layout_key      TYPE salv_s_layout_key,
          gr_column       TYPE REF TO cl_salv_column,
          lv_not_found    TYPE REF TO cx_salv_not_found.
    TRY.
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            list_display = if_salv_c_bool_sap=>false
*           r_container  =
*           container_name =
          IMPORTING
            r_salv_table = DATA(gr_alv)
          CHANGING
            t_table      = gt_output.

        DATA(gr_functions) = gr_alv->get_functions( ).
        gr_functions->set_all( abap_true ).

        layout_settings = gr_alv->get_layout( ).

        layout_key-report = sy-repid.
        layout_settings->set_key( layout_key ).
        layout_settings->set_default( value = 'X' ).

        layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

        DATA(gr_columns) = gr_alv->get_columns( ).
        gr_columns->set_optimize( ).

        TRY.
            gr_column ?= gr_columns->get_column( 'LENG_SYS' )      ##NO_TEXT.
            gr_column->set_short_text( ' ' )                       ##NO_TEXT.
            gr_column->set_medium_text( 'Sys Len ' )                      ##NO_TEXT.
            gr_column->set_long_text( 'System Length' )            ##NO_TEXT.

          CATCH cx_salv_not_found INTO lv_not_found.
            " error handling
        ENDTRY.
        CLEAR: lv_not_found.

        TRY.
            gr_column ?= gr_columns->get_column( 'LENG_PROP' )     ##NO_TEXT.
            gr_column->set_short_text( ' ' )                       ##NO_TEXT.
            gr_column->set_medium_text( 'Prop Len ' )                      ##NO_TEXT.
            gr_column->set_long_text( 'Proposed Length' )          ##NO_TEXT.

          CATCH cx_salv_not_found INTO lv_not_found.
            " error handling
        ENDTRY.
        CLEAR: lv_not_found.

        TRY.
            gr_column ?= gr_columns->get_column( 'ENTRY_EN_SYS' )  ##NO_TEXT.
            gr_column->set_short_text( ' ' )                       ##NO_TEXT.
            gr_column->set_medium_text( ' ' )                      ##NO_TEXT.
            gr_column->set_long_text( 'System Source Text' )       ##NO_TEXT.

          CATCH cx_salv_not_found INTO lv_not_found.
            " error handling
        ENDTRY.
        CLEAR: lv_not_found.

        TRY.
            gr_column ?= gr_columns->get_column( 'ENTRY_EN_PROP' ) ##NO_TEXT.
            gr_column->set_short_text( ' ' )                       ##NO_TEXT.
            gr_column->set_medium_text( ' ' )                      ##NO_TEXT.
            gr_column->set_long_text( 'Porposed Source Text' )     ##NO_TEXT.

          CATCH cx_salv_not_found INTO lv_not_found.
            " error handling
        ENDTRY.
        CLEAR: lv_not_found.

        TRY.
            gr_column ?= gr_columns->get_column( 'ENTRY_HU_SYS ' ) ##NO_TEXT.
            gr_column->set_short_text( ' ' )                       ##NO_TEXT.
            gr_column->set_medium_text( ' ' )                      ##NO_TEXT.
            gr_column->set_long_text( ' System Target Text' )      ##NO_TEXT.

          CATCH cx_salv_not_found INTO lv_not_found.
            " error handling
        ENDTRY.
        CLEAR: lv_not_found.

        TRY.
            gr_column ?= gr_columns->get_column( 'ENTRY_HU_PROP' ) ##NO_TEXT.
            gr_column->set_short_text( ' ' )                       ##NO_TEXT.
            gr_column->set_medium_text( ' ' )                      ##NO_TEXT.
            gr_column->set_long_text( 'Porposed Target Text' )     ##NO_TEXT.

          CATCH cx_salv_not_found INTO lv_not_found.
            " error handling
        ENDTRY.
        CLEAR: lv_not_found.

      CATCH cx_salv_msg .
    ENDTRY.

    gr_alv->display( ). "display grid

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
    MESSAGE 'Language not supported:'(002) && ` ` && p_tlang TYPE 'I'.
    p_lv_flag = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_labguage
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_labguage .

  DATA: lv_t_lang TYPE lxeisolang,
        lv_s_lang TYPE lxeisolang,
        lv_flag   TYPE char1.

  PERFORM: fetch_source_traget_language USING    p_slang
                                        CHANGING lv_s_lang
                                                 lv_flag.
  IF lv_flag IS NOT INITIAL.
    MESSAGE: 'Langauge error' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM: fetch_source_traget_language USING    p_tlang
                                        CHANGING lv_t_lang
                                                 lv_flag.
  IF lv_flag IS NOT INITIAL.
    MESSAGE: 'Langauge error' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF lv_t_lang = lv_s_lang.
    MESSAGE: 'Source and target language can not be same' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
