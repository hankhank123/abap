
CLASS zcl_rap_entity_vh_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .

    TYPES: BEGIN OF ty_page,
             top  TYPE p LENGTH 10 DECIMALS 0,
             skip TYPE p LENGTH 10 DECIMALS 0,
           END OF ty_page.
    TYPES: ty_big5_t TYPE TABLE OF zbig5.


    CLASS-METHODS paging
      IMPORTING
        !is_paging TYPE ty_page
      CHANGING
        !ct_data   TYPE STANDARD TABLE .


          types:
    TY_BIG5 type table of zbig5 .

  methods CONSTRUCTOR .
  class-methods CONV_TO_TABLE
    importing
      !PI_TRDATA type STRING .
  methods GET_CHAR_BIG5
    importing
      value(PI_CHAR) type CHAR1 optional
    exporting
      value(PE_BIG5) type XSTRING
      value(PE_ASCII) type FLAG .
  methods CONV_STR_TO_BIG5
    importing
      !PI_STR type CHAR128 optional
      !PI_BIG5_LEN type INT4 optional
    exporting
      !PE_BIG5 type XSTRING .

  PROTECTED SECTION.
  PRIVATE SECTION.
  class-data GT_BIG5 type TY_BIG5 .
    TYPES ty_response_tab TYPE STANDARD TABLE OF zvtest_vh2 WITH EMPTY KEY.

    METHODS upd
      IMPORTING pi_big5 TYPE  ty_big5_t.

ENDCLASS.

CLASS zcl_rap_entity_vh_query IMPLEMENTATION.
  METHOD if_rap_query_provider~select.
    DATA response TYPE ty_response_tab.

    IF NOT io_request->is_data_requested( ).
      RETURN.
    ENDIF.
*
    TRY.
        DATA(filters) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range INTO DATA(ex_ranges).
    ENDTRY.

    DATA(pa) = io_request->get_parameters( ).
    DO 100 TIMES.
      APPEND INITIAL LINE TO response ASSIGNING FIELD-SYMBOL(<res>).
      <res>-rootName = lines(  response ).
      <res>-aliasName = lines(  response ).
      <res>-testname = lines(  response ).
      CONDENSE: <res>-testname, <res>-aliasName.
    ENDDO.

    DATA: l_action(10) TYPE c.
    DATA(l_sel_json) = VALUE #( filters[ name = 'TESTNAME' ]-range OPTIONAL ).
    DATA(l_xaction) = VALUE #( filters[ name = 'ACTION' ]-range OPTIONAL ).
    DATA:l_json TYPE string.
    l_json = l_sel_json[ 1 ]-low.
    IF l_xaction[] IS NOT INITIAL.
      l_action = l_xaction[ 1 ]-low.
    ENDIF.

    CASE l_action.
      WHEN 'BIG5'.


        DATA: lt_big5 TYPE ty_big5_t.
        /ui2/cl_json=>deserialize(
        EXPORTING json = l_json
        CHANGING
           data = lt_big5
        ).
        upd( EXPORTING pi_big5 = lt_big5 ).


      WHEN 'EXCEL'.
        DATA: l_xexcel TYPE xstring.
        l_xexcel = cl_web_http_utility=>decode_x_base64(
             EXPORTING encoded = l_json
         ).

        TYPES:
          BEGIN OF ts_row,
            index      TYPE string,
            j          TYPE string,
            jstr       TYPE string,
            xstr       TYPE string,
            rev        TYPE string,
            base64     TYPE string,
            base64_rev TYPE string,
            typ        TYPE string,
          END OF ts_row.


        DATA :   tt_row TYPE STANDARD TABLE OF ts_row WITH DEFAULT KEY.
        DATA lt_rows LIKE tt_row.

        DATA(lo_xlsx) = xco_cp_xlsx=>document->for_file_content( iv_file_content = l_xexcel )->read_access( ).
        DATA(lo_worksheet) = lo_xlsx->get_workbook( )->worksheet->for_name( iv_name = 'Sheet1' ).

        DATA(lo_selection_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to( )->get_pattern( ).

        lo_worksheet->select( lo_selection_pattern
           )->row_stream(
           )->operation->write_to( REF #( lt_rows )
           )->if_xco_xlsx_ra_operation~execute( ).

    ENDCASE.
    " PAGING  "
    DATA(top) = io_request->get_paging( )->get_page_size( ).
    IF top < 0.
      top = 50.
    ENDIF.

    DATA(skip) = io_request->get_paging( )->get_offset( ).

    IF top IS NOT INITIAL OR skip IS NOT INITIAL.
      paging( EXPORTING is_paging = VALUE #( top  = top
                                             skip = skip )
                                       CHANGING  ct_data   = response ).
    ENDIF.

    IF io_request->is_total_numb_of_rec_requested( ).
      io_response->set_total_number_of_records( lines( response ) ).
    ENDIF.

    io_response->set_data( response ).
  ENDMETHOD.

  METHOD upd.
    DATA: lt_big5 TYPE ty_big5_t.
    lt_big5 = pi_big5.
    LOOP AT lt_big5 ASSIGNING FIELD-SYMBOL(<big5>).
      <big5>-tabix = sy-tabix.
    ENDLOOP.

    DELETE FROM zbig5.
    MODIFY zbig5 FROM TABLE @lt_big5.

  ENDMETHOD.

  METHOD paging.
    DATA: lv_from TYPE i,
          lv_to   TYPE i.
    DATA: lo_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_result> TYPE STANDARD TABLE,
                   <fs_rec>    TYPE any.

    CREATE DATA lo_data LIKE ct_data.
    ASSIGN lo_data->* TO <fs_result>.

    IF is_paging-skip IS NOT INITIAL.
      lv_from = is_paging-skip + 1. "start from record
    ELSE.
      lv_from = 1.
    ENDIF.
    IF is_paging-top IS NOT INITIAL.
      lv_to   = lv_from + is_paging-top - 1.
    ELSE.
      lv_to = lines( ct_data ).
    ENDIF.

    LOOP AT ct_data ASSIGNING <fs_rec> FROM lv_from TO lv_to.
      APPEND <fs_rec> TO <fs_result>.
    ENDLOOP.

    ct_data = <fs_result>.

*    data: l_xstr type xstring,
*           l_xstr1 type xstring.
*           data: l_base64 type string.
*           concatenate l_xstr l_xstr1 into l_xstr in BYTE MODE.
**********************************************************************
*         l_base64 =
*CL_WEB_HTTP_UTILITY=>encode_x_base64( l_xstr ).

*data(zz1) = CL_ABAP_TYPEDESCR=>TYPEKIND_NUM.
*data(rr) = cl_rsda_csv_converter=>create( ).

*data: iv_xstring type xstring.
*DATA(lo_xlsx) = xco_cp_xlsx=>document->for_file_content( iv_file_content = iv_xstring )->read_access( ).

*DATA(lo_xlsx) = xco_cp_xlsx=>document->for_file_content( iv_file_content = iv_xstring )->read_access( ).
*DATA(lo_worksheet) = lo_xlsx->get_workbook( )->worksheet->for_name( iv_name = 'Sheet1' ).
*
*DATA(lo_selection_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to( )->get_pattern( ).
*
*lo_worksheet->select( lo_selection_pattern
*)->row_stream(
*)->operation->write_to( REF #( rt_records )
*)->if_xco_xlsx_ra_operation~execute( ).
*
*Where iv_xstring is an excel file converted into string and rt_records is a DDIC-table type which represents structure of your excel table.

  ENDMETHOD.

    METHOD constructor.
  check gt_big5[] is initial.
    SELECT *
    FROM zbig5
    ORDER BY jstr
    INTO TABLE @gt_big5.
  ENDMETHOD.

    METHOD conv_str_to_big5.
    CLEAR pe_big5.

    DATA: l_time TYPE i.
    DATA: l_char TYPE char1.
    DATA: l_pos TYPE i.
    DATA: l_len_cnt TYPE i.
    DATA: l_ascii TYPE flag.
    DATA: l_xstr TYPE xstring.
    l_time = strlen( pi_str ) .

    l_pos = 0.
    DO l_time TIMES.
      l_char = pi_str+l_pos(1).
      CLEAR l_xstr.
      get_char_big5( EXPORTING  pi_char = l_char
                     IMPORTING  pe_big5 = l_xstr
                                pe_ascii = l_ascii ).
      IF l_ascii = 'X'.
        ADD 1 TO l_len_cnt.
      ELSE.
        ADD 2 TO l_len_cnt.
      ENDIF.
      IF l_len_cnt > pi_big5_len.
        EXIT.
      ELSE.
        CONCATENATE pe_big5 l_xstr INTO pe_big5 IN BYTE MODE.
      ENDIF.

      ADD 1 TO l_pos.
    ENDDO.

  ENDMETHOD.
    METHOD conv_to_table.
    DATA: lt_big5_tab TYPE TABLE OF zbig5.
    /ui2/cl_json=>deserialize(
    EXPORTING
      json             = pi_trdata
    CHANGING
      data             = lt_big5_tab
         ).

    LOOP AT lt_big5_tab ASSIGNING FIELD-SYMBOL(<big5>).
      <big5>-tabix = sy-tabix.
    ENDLOOP.
    DELETE FROM  zbig5.
    MODIFY  zbig5 FROM TABLE @lt_big5_tab.
  ENDMETHOD.

   METHOD get_char_big5.
    DATA: l_encoded TYPE string.
    CLEAR: pe_big5, pe_ascii.
    READ TABLE gt_big5 INTO DATA(lw_big5) WITH KEY jstr = pi_char BINARY SEARCH.
    IF sy-subrc = 0.
      l_encoded = lw_big5-base64.
      pe_big5 = CL_WEB_HTTP_UTILITY=>decode_x_base64( encoded = l_encoded ).
      IF lw_big5-typ = 'ASCII'.
        pe_ascii = 'X'.
      ENDIF.

    ELSE.
    clear pe_big5 .
      READ TABLE gt_big5 into lw_big5 WITH KEY jstr = '#' BINARY SEARCH.
    IF sy-subrc = 0.
    l_encoded = lw_big5-base64.
      pe_big5 = CL_WEB_HTTP_UTILITY=>decode_x_base64( encoded = l_encoded ).
      IF lw_big5-typ = 'ASCII'.
        pe_ascii = 'X'.
      ENDIF.
     endif.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
