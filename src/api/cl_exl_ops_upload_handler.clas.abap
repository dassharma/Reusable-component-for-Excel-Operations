CLASS cl_exl_ops_upload_handler DEFINITION PUBLIC ABSTRACT CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      create IMPORTING iv_class_name      TYPE seoclsname
             RETURNING VALUE(ro_instance) TYPE REF TO object .

    METHODS:
      process_excel IMPORTING iv_datastream_guid TYPE guid
                              iv_business_object TYPE swo_objtyp .

  PROTECTED SECTION.

    DATA:
      mt_configuration_data TYPE cl_exl_ops_utilities=>tt_configuration_data,
      mt_field_notation     TYPE cl_exl_ops_utilities=>tt_field_notation,
      mt_excel_data         TYPE cl_exl_ops_utilities=>tt_sheets_data,
      mt_invalid_documents  TYPE REF TO data.

    METHODS:
      _process_data IMPORTING it_excel_data      TYPE cl_exl_ops_utilities=>tt_sheets_data
                    RETURNING VALUE(rt_messages) TYPE bal_t_msg
                    RAISING   cx_exl_ops .
  PRIVATE SECTION.

    DATA:
      ms_mass_operation_stream_data TYPE mass_op_strm_tmp,
      mv_log_handle                 TYPE balloghndl,
      mt_messages                   TYPE bal_t_msg.

    METHODS:
      _extract_datastream_for_upload IMPORTING iv_guid TYPE guid
                                     RAISING   cx_exl_ops,
      _convert_stream_to_data        RAISING   cx_exl_ops,
      _apply_system_locale           IMPORTING iv_sheet_id      TYPE exl_ws_id OPTIONAL
                                               iv_fieldname     TYPE exl_gfn OPTIONAL
                                               iv_fieldvalue    TYPE any OPTIONAL
                                               iv_currentrow    TYPE any OPTIONAL
                                               iv_rowheader     TYPE any OPTIONAL
                                     EXPORTING ev_fieldvalue    TYPE any
                                     RETURNING VALUE(rv_status) TYPE boolean,
      _determine_excel_language      IMPORTING iv_doc_id         TYPE exl_doc_id
                                               iv_sheet_name     TYPE exl_ws_name
                                     RETURNING VALUE(rv_languge) TYPE langu
                                     RAISING   cx_exl_ops,
      _create_job_log                IMPORTING iv_business_object TYPE swo_objtyp
                                     RETURNING VALUE(rv_message)  TYPE cl_exl_ops_utilities=>ty_messages,
      _add_message_to_log            IMPORTING it_messages       TYPE bal_t_msg
                                     RETURNING VALUE(rv_message) TYPE cl_exl_ops_utilities=>ty_messages,
      _save_log                      RETURNING VALUE(rv_message) TYPE cl_exl_ops_utilities=>ty_messages .
ENDCLASS.



CLASS CL_EXL_OPS_UPLOAD_HANDLER IMPLEMENTATION.


  METHOD create.
    CREATE OBJECT ro_instance TYPE (iv_class_name).
  ENDMETHOD.


  METHOD process_excel.
    TRY.
        "   Instantiate the JOB log
        DATA(lv_messages) = _create_job_log( iv_business_object ).

        "   Extract the saved data-stream in the DB
        _extract_datastream_for_upload( iv_datastream_guid ).

        "   Convert the data-stream in proper tables
        _convert_stream_to_data( ).

        "   Processing the actual data
        DATA(lt_messages) = _process_data( it_excel_data = mt_excel_data ).
      CATCH cx_exl_ops INTO DATA(lx_exl_ops).
        " Handle messages
        APPEND VALUE bal_s_msg( msgty     = 'E'
                                msgid     = lx_exl_ops->if_t100_message~t100key-msgid
                                msgno     = lx_exl_ops->if_t100_message~t100key-msgno
*                                msgv1     = lx_exl_ops->if_t100_message~t100key-attr1
                                msgv1     = COND #( WHEN lx_exl_ops->get_longtext( ) IS INITIAL THEN lx_exl_ops->get_text( )
                                                                                                ELSE lx_exl_ops->get_longtext( ) )
*                                msgv2     = lx_exl_ops->if_t100_message~t100key-attr2
*                                msgv3     = lx_exl_ops->if_t100_message~t100key-attr3
*                                msgv4     = lx_exl_ops->if_t100_message~t100key-attr4
                                 ) TO lt_messages.
    ENDTRY.
    "  Inject the messages to the APPLICATION JOB LOG
    APPEND LINES OF lt_messages TO mt_messages.
    _add_message_to_log( mt_messages ).

    _save_log( ).
  ENDMETHOD.


  METHOD _add_message_to_log.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "       Inject messages into the log
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT it_messages ASSIGNING FIELD-SYMBOL(<fs_message>).
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = mv_log_handle
          i_s_msg          = VALUE bal_s_msg( msgty = <fs_message>-msgty
                                              msgid = <fs_message>-msgid
                                              msgno = <fs_message>-msgno
                                              msgv1 = <fs_message>-msgv1
                                              msgv2 = <fs_message>-msgv2
                                              msgv3 = <fs_message>-msgv3
                                              msgv4 = <fs_message>-msgv4 )
        EXCEPTIONS
          log_not_found    = 1                " Log not found
          msg_inconsistent = 2                " Message inconsistent
          log_is_full      = 3                " Message number 999999 reached. Log is full
          OTHERS           = 4.
      IF sy-subrc <> 0.
        DATA(lv_message) = VALUE cl_exl_ops_utilities=>ty_messages( msg_id     = sy-msgid
                                                                    msg_type   = sy-msgty
                                                                    msg_number = sy-msgno
                                                                    msg_value1 = sy-msgv1
                                                                    msg_value2 = sy-msgv2
                                                                    msg_value3 = sy-msgv3
                                                                    msg_value4 = sy-msgv4 ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _apply_system_locale.

    DATA: lv_thousand_separator TYPE c,
          lv_decimal_separator  TYPE c,
          lv_integer            TYPE char060,
          lv_decimal            TYPE char060,
          lv_amt_dec            TYPE bapicurr_d,
          ls_err_message        TYPE bapireturn,
          lv_invalid_number     TYPE REF TO cx_root,
          lv_curr_unit          TYPE waers,
          l_datfm               TYPE xudatfm,
          l_date                TYPE d.

    DATA(lv_cds_name) = VALUE #( mt_configuration_data[ exl_ws_id = iv_sheet_id ]-cds OPTIONAL ).                                                 "fetch CDS view corresponding to the excel sheet

    "fetch details of the input field iv_fieldname whose data is to be formatted, empty if field is not to be formatted.
    DATA(lt_field_notation) = VALUE cl_exl_ops_utilities=>tt_field_notation( FOR <fs_field_notation> IN mt_field_notation WHERE ( cds_name = lv_cds_name AND field_name = iv_fieldname ) ( <fs_field_notation> ) ).

    ev_fieldvalue = iv_fieldvalue.
    rv_status = abap_true.

    LOOP AT lt_field_notation ASSIGNING FIELD-SYMBOL(<fs_column>).
      CLEAR: lv_thousand_separator,lv_decimal_separator,lv_integer,lv_decimal,lv_amt_dec,ls_err_message,lv_invalid_number,lv_curr_unit.

      CASE <fs_column>-field_type.
        WHEN 'DATS'.
          IF iv_fieldvalue IS NOT INITIAL.
            CONDENSE ev_fieldvalue.
            l_datfm = cl_abap_datfm=>get_datfm( ) .                                           " get user profile format
            TRY.
                CALL METHOD cl_abap_datfm=>conv_date_ext_to_int
                  EXPORTING
                    im_datext   = ev_fieldvalue                                               " external representation of date
                    im_datfmdes = l_datfm                                                     " date format wanted for conversion
                  IMPORTING
                    ex_datint   = l_date                                                      " internal representation of date
*                   ex_datfmused =                                                            " date format used for conversion
                  .
              CATCH cx_abap_datfm_no_date INTO lv_invalid_number.                             " Exception in Class CL_ABAP_DATFM - Input is not a date
              CATCH cx_abap_datfm_invalid_date INTO lv_invalid_number.                        " Exception in Class CL_ABAP_DATFM - Invalid date
              CATCH cx_abap_datfm_format_unknown INTO lv_invalid_number.                      " Exception in Class CL_ABAP_DATFM - Format unknown
              CATCH cx_abap_datfm_ambiguous INTO lv_invalid_number.                           " Exception in Class CL_ABAP_DATFM - Ambiguous
            ENDTRY.

            IF lv_invalid_number IS INITIAL.
              ev_fieldvalue = l_date.
            ELSE.
              rv_status = abap_false.
              RETURN.
            ENDIF.

          ELSE.
            ev_fieldvalue = '00000000'.
          ENDIF.

        WHEN 'CURR'.

          SELECT SINGLE dcpfm INTO @DATA(lv_dcpfm1) FROM usr01 WHERE bname = @sy-uname.                                                     "get locale data
          IF sy-subrc <> 0.
            rv_status = abap_false.
            RETURN.
          ENDIF.

          CONDENSE ev_fieldvalue.
          IF iv_fieldvalue IS INITIAL.
            rv_status = abap_false.
            RETURN.
          ELSEIF lv_dcpfm1 = 'X'.                                "__,___,___.___
            lv_thousand_separator = ','.
            lv_decimal_separator = '.'.
          ELSEIF lv_dcpfm1 = 'Y'.                                "__ ___ ___,___
            lv_thousand_separator = ' '.
            lv_decimal_separator = ','.
          ELSEIF lv_dcpfm1 = ' '.                                "__.___.___,___
            lv_thousand_separator = '.'.
            lv_decimal_separator = ','.
          ELSE.
          ENDIF.

          SPLIT ev_fieldvalue AT lv_decimal_separator INTO lv_integer lv_decimal.
          REPLACE ALL OCCURRENCES OF lv_thousand_separator IN lv_integer WITH '' .
          IF lv_integer IS INITIAL.
            lv_integer = '0'.
          ENDIF.
          IF lv_decimal IS INITIAL.
            lv_decimal = '0'.
          ENDIF.

          CONCATENATE lv_integer '.' lv_decimal INTO ev_fieldvalue.

          TRY.                                                                                                                              "check for more than 1 decimal separator
              MOVE ev_fieldvalue TO lv_amt_dec.
            CATCH cx_sy_conversion_no_number INTO lv_invalid_number.
          ENDTRY.

          IF lv_invalid_number IS INITIAL.                                                                                                   "Decimal shifting of data

            DATA(lv_unit_label) = VALUE #( mt_configuration_data[ exl_ws_id = iv_sheet_id exl_gfn = <fs_column>-field_exit ]-enduser_label_text OPTIONAL ).

            IF lv_unit_label IS NOT INITIAL.
              DO.
                ASSIGN COMPONENT sy-index OF STRUCTURE iv_rowheader TO FIELD-SYMBOL(<fs_column_header>).
                IF sy-subrc NE 0.
                  rv_status = abap_false.
                  RETURN.
                ENDIF.

                IF lv_unit_label EQ <fs_column_header>.
                  ASSIGN COMPONENT sy-index OF STRUCTURE iv_currentrow TO FIELD-SYMBOL(<fs_unit>).
                  IF sy-subrc NE 0.
                    rv_status = abap_false.
                    RETURN.
                  ENDIF.

                  lv_curr_unit = <fs_unit>.
                  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
                    EXPORTING
                      currency             = lv_curr_unit                                   " Currency Description
                      amount_external      = lv_amt_dec                                     " External Currency Amount
                      max_number_of_digits = '23'                                           " Maximum Field Length of Internal Domains
                    IMPORTING
                      amount_internal      = ev_fieldvalue                                  " Converted Internal Currency Amount
                      return               = ls_err_message.                                " Return Messages

                  IF ls_err_message IS NOT INITIAL.
                       rv_status = abap_false.
                  ENDIF.
                  RETURN.
                ENDIF.
              ENDDO.
            ELSE.
              rv_status = abap_false.
              RETURN.
            ENDIF.
          ELSE.
            rv_status = abap_false.
            RETURN.
          ENDIF.

        WHEN 'QUAN' OR 'DEC'.

          SELECT SINGLE dcpfm INTO @DATA(lv_dcpfm) FROM usr01 WHERE bname = @sy-uname.
          IF sy-subrc <> 0.
            rv_status = abap_false.
            RETURN.
          ENDIF.

          "remove only thousands separator
          CONDENSE ev_fieldvalue.

          IF iv_fieldvalue IS INITIAL.
            rv_status = abap_false.
            RETURN.
          ELSEIF lv_dcpfm = 'X'.                                "__,___,___.___
            lv_thousand_separator = ','.
            lv_decimal_separator = '.'.
          ELSEIF lv_dcpfm = 'Y'.                                "__ ___ ___,___
            lv_thousand_separator = ' '.
            lv_decimal_separator = ','.
          ELSEIF lv_dcpfm1 = ' '.                                "__.___.___,___
            lv_thousand_separator = '.'.
            lv_decimal_separator = ','.
          ELSE.
          ENDIF.

          SPLIT ev_fieldvalue AT lv_decimal_separator INTO lv_integer lv_decimal.
          REPLACE ALL OCCURRENCES OF lv_thousand_separator IN lv_integer WITH '' .
          IF lv_integer IS INITIAL.
            lv_integer = '0'.
          ENDIF.
          IF lv_decimal IS INITIAL.
            lv_decimal = '0'.
          ENDIF.

          CONCATENATE lv_integer '.' lv_decimal INTO ev_fieldvalue.

          TRY.
              MOVE ev_fieldvalue TO lv_amt_dec.
            CATCH cx_sy_conversion_no_number INTO lv_invalid_number.
          ENDTRY.
          IF lv_invalid_number IS NOT INITIAL.
            rv_status = abap_false.
            RETURN.
          ENDIF.

      ENDCASE.

    ENDLOOP.


  ENDMETHOD.


  METHOD _convert_stream_to_data.
    CONSTANTS:
      BEGIN OF lc_messages,
        class           TYPE bal_s_msg-msgid VALUE 'EXL_OPS',
        free_text       TYPE bal_s_msg-msgno VALUE 004,
        mandatory_error TYPE bal_s_msg-msgno VALUE 016,
        format_error    TYPE bal_s_msg-msgno VALUE 017,
        message_header  TYPE bal_s_msg-msgno VALUE 018,
      END OF lc_messages.

    FIELD-SYMBOLS:
      <ft_ws_data>           TYPE STANDARD TABLE,
      <ft_dyn_table>         TYPE STANDARD TABLE,
      <ft_invalid_documents> TYPE STANDARD TABLE.

    TRY.
        TRY.
            "   Generate spreadsheet handler for excel operations
            DATA(lo_spreadsheet_handler) = NEW cl_mm_spreadsheet_handler( iv_document_name = |{ ms_mass_operation_stream_data-exl_doc_id }_FILE|
                                                                          iv_xdocument     = ms_mass_operation_stream_data-media_content ).
          CATCH cx_ofi_doc INTO DATA(lx_excp).
        ENDTRY.

        "   Get the sheet details from the excel uploaded
        lo_spreadsheet_handler->get_worksheet_names( IMPORTING et_worksheets = DATA(lt_worksheets) ).

        "   Check if excel contains sheets
        IF lines( lt_worksheets ) EQ 0.
          RAISE EXCEPTION TYPE cx_exl_ops EXPORTING textid = cx_exl_ops=>no_sheets.
        ENDIF.

        "   Determine the language of the excel uploaded
        IF _determine_excel_language( iv_doc_id     = ms_mass_operation_stream_data-exl_doc_id
                                      iv_sheet_name = CONV #( lt_worksheets[ 1 ] ) ) NE sy-langu.
          RAISE EXCEPTION TYPE cx_exl_ops EXPORTING textid = cx_exl_ops=>language_err.
        ENDIF.

        "   Read configuration details
        mt_configuration_data = cl_exl_ops_utilities=>get_exl_ops_config_details( iv_document_id  = ms_mass_operation_stream_data-exl_doc_id
                                                                                  iv_language_key = sy-langu ).
        "   Read field notation
        mt_field_notation = cl_exl_ops_utilities=>get_exl_ops_field_notation( CHANGING ct_exl_ops_config = mt_configuration_data ).     " Table: Excel Operation configuration details

        "   Check on the no of sheets
        SELECT COUNT( DISTINCT exl_ws_id ) FROM @mt_configuration_data AS excel_configuration INTO @DATA(lv_no_of_sheets).
        IF lines( lt_worksheets ) NE lv_no_of_sheets.
          RAISE EXCEPTION TYPE cx_exl_ops EXPORTING textid = cx_exl_ops=>wrong_no_of_sheets.
        ENDIF.

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "       DEFINE INVALID DOCUMENTS TABLE SCHEMA
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        TRY.
            SELECT DISTINCT exl_gfn AS key_field FROM @mt_configuration_data AS configuration_data WHERE is_key = @abap_true INTO TABLE @DATA(lt_key_list).
            IF sy-subrc = 0.
              DATA(lt_key_configuration) = VALUE cl_exl_ops_utilities=>tt_configuration_data( ).
              LOOP AT lt_key_list ASSIGNING FIELD-SYMBOL(<fs_key>).
                SELECT COUNT( * ) FROM @mt_configuration_data AS config_data WHERE exl_gfn = @<fs_key>-key_field INTO @DATA(lv_key_occurence).
                IF lv_key_occurence = lv_no_of_sheets.
                  APPEND VALUE #( mt_configuration_data[ exl_gfn = <fs_key>-key_field ] OPTIONAL ) TO lt_key_configuration.
                ENDIF.
              ENDLOOP.

              DATA(lt_components) = VALUE cl_abap_structdescr=>component_table( FOR <fs_config> IN lt_key_configuration ( name = <fs_config>-exl_gfn
                                                                                                                          type = CAST #( cl_abap_structdescr=>describe_by_name( <fs_config>-exl_data_element ) ) ) ).

              IF lines( lt_components ) > 0.
                "   Get the descriptor of the table
                DATA(lr_table_descriptor) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_components ) ).
                "   Transform the data object to the table
                CREATE DATA mt_invalid_documents TYPE HANDLE lr_table_descriptor.
              ENDIF.
            ENDIF.
          CATCH cx_sy_table_creation INTO DATA(lx_exeception) ."type_not_found.
            RAISE EXCEPTION TYPE cx_exl_ops
              EXPORTING
                previous = lx_exeception.
        ENDTRY.

        DATA(lt_group_messages) = VALUE bal_t_msg(  ).
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "       CONVERSION OF DATA FROM EXCEL TO TABLE
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        LOOP AT lt_worksheets ASSIGNING FIELD-SYMBOL(<fs_worksheet>).
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "             SHEET NAME CHECK
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          IF NOT line_exists( mt_configuration_data[ exl_ws_name = <fs_worksheet> ] ).
            RAISE EXCEPTION TYPE cx_exl_ops
              EXPORTING
                textid        = cx_exl_ops=>missing_sheet
                mv_sheet_name = <fs_worksheet>.
          ENDIF.

          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "         EXTRACT WORK-SHEET DATA FROM EXCEL
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          DATA(lo_ws_data) = lo_spreadsheet_handler->get_itab_from_worksheet( <fs_worksheet> ).
          ASSIGN lo_ws_data->* TO <ft_ws_data>.

          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "             EXTRACT WORK-SHEET ID
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          DATA(lv_ws_id) = mt_configuration_data[ exl_ws_name = <fs_worksheet> ]-exl_ws_id.

          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "             CREATE DYNAMIC TABLE
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          DATA(lr_table) = cl_dynamic_table_generator=>generate_dynamic_table( iv_sheet_id           = lv_ws_id
                                                                               it_configuration_data = mt_configuration_data ).
          ASSIGN lr_table->* TO <ft_dyn_table>.

          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "             EXTRACT WORK-SHEET COLUMN NAMES
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          READ TABLE <ft_ws_data> ASSIGNING FIELD-SYMBOL(<fs_header>) INDEX 1.

          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "         FILL DYNAMIC TABLE WITH DATA
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "     For every single record
          LOOP AT <ft_ws_data> ASSIGNING FIELD-SYMBOL(<fs_ws_data>).
            DATA(lt_messages) = VALUE bal_t_msg(  ).
            IF sy-tabix EQ 1.
              CONTINUE.
            ENDIF.
            APPEND INITIAL LINE TO <ft_dyn_table> ASSIGNING FIELD-SYMBOL(<fs_dyn_table>).

            "   For every single column
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE <fs_header> TO FIELD-SYMBOL(<fs_header_value>).
              IF sy-subrc NE 0.
                EXIT.
              ENDIF.

              "     Get configuration detail for current field
              DATA(ls_configuration) = VALUE #( mt_configuration_data[ exl_ws_id = lv_ws_id enduser_label_text = <fs_header_value> ] OPTIONAL ).
              IF ls_configuration IS INITIAL.
                CONTINUE.
              ENDIF.

              "     Field is marked required only when read-only is unchecked and mandatory is checked
              DATA(lv_is_required) = COND boolean( WHEN ls_configuration-is_readonly = abap_true THEN abap_false
                                                                                                 ELSE COND #( WHEN ls_configuration-is_mandatory = abap_true THEN abap_true ELSE abap_false ) ).

              ASSIGN COMPONENT sy-index OF STRUCTURE <fs_ws_data> TO FIELD-SYMBOL(<fs_ws_field_value>).
              ASSIGN COMPONENT ls_configuration-exl_gfn OF STRUCTURE <fs_dyn_table> TO FIELD-SYMBOL(<fs_dyn_table_field_value>).
              IF <fs_ws_field_value> IS NOT ASSIGNED OR <fs_dyn_table_field_value> IS NOT ASSIGNED.
                CONTINUE.
              ENDIF.

              """"""""""""""""""""""""""""""""""""""""""""""""""""""""
              "         MANDATORY CHECK
              """"""""""""""""""""""""""""""""""""""""""""""""""""""""
              IF lv_is_required = abap_true AND <fs_ws_field_value> IS INITIAL.
                "       Mark the document invalid
                DATA(lv_invalid_document) = abap_true.

                "       Raise the message -> A value for the mandatory field & of the & & is missing.
                APPEND VALUE #( msgty = sy-abcde+4(1) "E
                                msgid = lc_messages-class
                                msgno = lc_messages-mandatory_error
                                msgv1 = ls_configuration-enduser_label_text
                                msgv2 = ls_configuration-exl_ws_name ) TO lt_messages.
                """"""""""""""""""""""""""""""""""""""""""""""""""""""""
                "         FORMATTING CHECK
                """"""""""""""""""""""""""""""""""""""""""""""""""""""""
              ELSEIF _apply_system_locale( EXPORTING iv_sheet_id   =  lv_ws_id                    " Excel Operations Work-sheet ID
                                                   iv_fieldname  =  ls_configuration-exl_gfn    " Global Field Name
                                                   iv_fieldvalue =  <fs_ws_field_value>         " Current Cell Raw Data
                                                   iv_currentrow =  <fs_ws_data>                " Current Row Raw Data
                                                   iv_rowheader  =  <fs_header>                 " Current Row Header Information
                                         IMPORTING ev_fieldvalue =  <fs_ws_field_value> ).      " Current Cell Formatted Data
                " No issue in formatting, data can be copied over
                TRY.
                    <fs_dyn_table_field_value> = <fs_ws_field_value>.
                  CATCH cx_sy_conversion_overflow INTO DATA(lx_invalid_data).
                    "       Mark the document invalid
                    lv_invalid_document = abap_true.

                    "       Raise the message -> The value & for & in the sheet & is not formatted correctly.
                    APPEND VALUE #( msgty = sy-abcde+4(1) "E
                                    msgid = lc_messages-class
                                    msgno = lc_messages-format_error
                                    msgv1 = <fs_ws_field_value>
                                    msgv2 = ls_configuration-enduser_label_text
                                    msgv3 = ls_configuration-exl_ws_name ) TO lt_messages.
                ENDTRY.
              ELSE.
                "       Mark the document invalid
                lv_invalid_document = abap_true.

                "       Raise the message -> The value & for & in the sheet & is not formatted correctly.
                APPEND VALUE #( msgty = sy-abcde+4(1) "E
                                msgid = lc_messages-class
                                msgno = lc_messages-format_error
                                msgv1 = <fs_ws_field_value>
                                msgv2 = ls_configuration-enduser_label_text
                                msgv3 = ls_configuration-exl_ws_name ) TO lt_messages.

              ENDIF.
              UNASSIGN: <fs_header_value>,
                        <fs_ws_field_value>,
                        <fs_dyn_table_field_value>.

            ENDDO.

            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
            "       INVALID DOCUMENT PROCESSING
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
            IF lv_invalid_document = abap_true.
              DATA(lv_fieldvalue) = VALUE string(  ).
              ASSIGN mt_invalid_documents->* TO <ft_invalid_documents>.

              APPEND INITIAL LINE TO <ft_invalid_documents> ASSIGNING FIELD-SYMBOL(<fs_invalid_document>).

              "     Fill invalid document number
              LOOP AT lt_key_configuration ASSIGNING FIELD-SYMBOL(<fs_key_configuration>).
                ASSIGN COMPONENT <fs_key_configuration>-exl_gfn OF STRUCTURE: <fs_dyn_table> TO FIELD-SYMBOL(<fs_col_value>),
                                                                              <fs_invalid_document> TO FIELD-SYMBOL(<fs_invalid_doc_key>).
                IF  <fs_col_value> IS ASSIGNED.
                  <fs_invalid_doc_key> = <fs_col_value>.
                  lv_fieldvalue = COND #( WHEN lv_fieldvalue IS INITIAL THEN |{ <fs_col_value> }| ELSE |{ lv_fieldvalue }-{ <fs_col_value> }| ).
                ENDIF.
              ENDLOOP.
              "     Tag the all the messages raised with the document number
              APPEND LINES OF VALUE bal_t_msg( FOR <fs_msg> IN lt_messages ( msgty = <fs_msg>-msgty
                                                                             msgid = <fs_msg>-msgid
                                                                             msgno = <fs_msg>-msgno
                                                                             msgv1 = <fs_msg>-msgv1
                                                                             msgv2 = <fs_msg>-msgv2
                                                                             msgv3 = <fs_msg>-msgv3
                                                                             msgv4 = lv_fieldvalue ) ) TO lt_group_messages.
              "       Remove the faulty record
              DELETE TABLE <ft_dyn_table> FROM <fs_dyn_table>.

              CLEAR: lv_invalid_document,
                     lv_fieldvalue.

              UNASSIGN: <fs_col_value>,
                        <fs_invalid_doc_key>,
                        <fs_invalid_document>.
            ENDIF.
          ENDLOOP.

          "     Add actual data object to the list of work-sheet
          APPEND VALUE #( sheet_id   = lv_ws_id
                          sheet_name = <fs_worksheet>
                          sheet_data = lr_table ) TO mt_excel_data.
        ENDLOOP.

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "           COLLATE ERROR MESSAGES PER DOCUMENT
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF <ft_invalid_documents> IS NOT ASSIGNED.
          ASSIGN mt_invalid_documents->* TO <ft_invalid_documents>.
        ENDIF.
        SORT <ft_invalid_documents>.
        DELETE ADJACENT DUPLICATES FROM <ft_invalid_documents>.
        LOOP AT <ft_invalid_documents> ASSIGNING <fs_invalid_document>.
          CLEAR: lv_fieldvalue.
          DO.
            ASSIGN COMPONENT sy-index OF STRUCTURE <fs_invalid_document> TO <fs_invalid_doc_key>.
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.
            lv_fieldvalue = COND #( WHEN lv_fieldvalue IS INITIAL THEN |{ <fs_invalid_doc_key> }| ELSE |{ lv_fieldvalue }-{ <fs_invalid_doc_key> }| ).
          ENDDO.
          "     Add header message
          APPEND VALUE bal_s_msg( msgty = sy-abcde+8(1)  " I
                                  msgid = lc_messages-class
                                  msgno = lc_messages-message_header
                                  msgv1 = to_lower( mt_configuration_data[ 1 ]-exl_doc_name )
                                  msgv2 = lv_fieldvalue ) TO mt_messages.
          "   Collating other messages under the header.
          APPEND LINES OF VALUE bal_t_msg( FOR <fs_msg> IN lt_group_messages WHERE ( msgv4 = lv_fieldvalue ) ( msgty = <fs_msg>-msgty
                                                                                                               msgid = <fs_msg>-msgid
                                                                                                               msgno = <fs_msg>-msgno
                                                                                                               msgv1 = <fs_msg>-msgv1
                                                                                                               msgv2 = <fs_msg>-msgv2
                                                                                                               msgv3 = <fs_msg>-msgv3 ) ) TO mt_messages.

        ENDLOOP.

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "               DELETE INVALID DOCUMENTS
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "   The logic to delete the invalid documents is written in the
        "   PROCESS_DATA method of the respective implementation class.
        "   This is done because in the implementation class static
        "   check is possible.

      CATCH cx_exl_ops INTO DATA(lx_exception). "cx_ofi_doc

        RAISE EXCEPTION TYPE cx_exl_ops
          EXPORTING
            textid = lx_exception->if_t100_message~t100key.
    ENDTRY.
  ENDMETHOD.


  METHOD _create_job_log.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "     Create the instance of the application log and add to application
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "       Create the log handler
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = VALUE bal_s_log( object = 'MASS' subobject = iv_business_object aldate_del = sy-datum + 15 )
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc EQ 0.
      "     Register the log handler against the current running application
      CALL FUNCTION 'BP_ADD_APPL_LOG_HANDLE'
        EXPORTING
          loghandle                  = mv_log_handle
        EXCEPTIONS
          could_not_set_handle       = 1
          not_running_in_batch       = 2
          could_not_get_runtime_info = 3
          handle_already_exists      = 4
          locking_error              = 5
          OTHERS                     = 6.
      IF sy-subrc NE 0.
        rv_message = VALUE #( msg_id     = sy-msgid
                      msg_type   = sy-msgty
                      msg_number = sy-msgno
                      msg_value1 = sy-msgv1
                      msg_value2 = sy-msgv2
                      msg_value3 = sy-msgv3
                      msg_value4 = sy-msgv4 ).
      ENDIF.
    ELSE.
      rv_message = VALUE #( msg_id     = sy-msgid
                            msg_type   = sy-msgty
                            msg_number = sy-msgno
                            msg_value1 = sy-msgv1
                            msg_value2 = sy-msgv2
                            msg_value3 = sy-msgv3
                            msg_value4 = sy-msgv4 ).
    ENDIF.
  ENDMETHOD.


  METHOD _determine_excel_language.
    "   Check the language of the excel, i.e., language in which the excel is uploaded
    SELECT langu FROM exl_ops_ws_txt INTO @rv_languge WHERE exl_doc_id = @iv_doc_id AND exl_ws_name = @iv_sheet_name.
    ENDSELECT.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE cx_exl_ops EXPORTING textid = cx_exl_ops=>language_error.
    ENDIF.
  ENDMETHOD.


  METHOD _extract_datastream_for_upload.
    "   Extract the data-stream saved to DB for processing using the Data-stream GUID
    SELECT  * FROM mass_op_strm_tmp INTO @ms_mass_operation_stream_data WHERE guid = @iv_guid.
    ENDSELECT.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE cx_exl_ops EXPORTING textid = cx_exl_ops=>missing_data_stream.
    ENDIF.
  ENDMETHOD.


  METHOD _process_data.

  ENDMETHOD.


  METHOD _save_log.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "     Save the messages added to log
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_in_update_task = abap_false
        i_t_log_handle   = VALUE bal_t_logh( ( mv_log_handle ) )
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      rv_message = VALUE #( msg_id     = sy-msgid
                            msg_type   = sy-msgty
                            msg_number = sy-msgno
                            msg_value1 = sy-msgv1
                            msg_value2 = sy-msgv2
                            msg_value3 = sy-msgv3
                            msg_value4 = sy-msgv4 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
