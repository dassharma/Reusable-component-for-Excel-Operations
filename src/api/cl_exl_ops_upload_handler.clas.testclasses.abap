*"* use this source file for your ABAP unit test classes
CLASS lcl_exl_ops_upload_handler DEFINITION INHERITING FROM cl_exl_ops_upload_handler.
ENDCLASS.

CLASS lcl_exl_ops_upload_handler IMPLEMENTATION.
ENDCLASS.

CLASS ltc_upload_handler DEFINITION DEFERRED.
CLASS cl_exl_ops_upload_handler DEFINITION LOCAL FRIENDS ltc_upload_handler.

CLASS ltc_upload_handler DEFINITION
FOR TESTING DURATION LONG RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA:
      ms_mass_ops_strm   TYPE mass_op_strm_tmp,
      mo_sql_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    DATA:
      t_temp_db         TYPE STANDARD TABLE OF mass_op_strm_tmp WITH EMPTY KEY,
      mo_upload_handler TYPE REF TO cl_exl_ops_upload_handler.

    METHODS:
      setup,
      create_sql_double,

      "!  TEST --> Process excel
      fxublr02_1737_ac1 FOR TESTING,
      "! TEST --> Convert Stream to Data
      fxublr02_1740_ac1 FOR TESTING,
      "! TEST --> Get Instance
      fxublr02_1742_ac1 FOR TESTING,
      "! Test --> Extraction Successful
      fxublr02_1743_ac1 FOR TESTING,
      "! Test --> Extraction Failed
      fxublr02_1743_ac2 FOR TESTING,
      "! Test --> Create job log Successful
      fxublr02_1745_ac1 FOR TESTING,
      "! Test --> Create Job Log Failed
      fxublr02_1745_ac2 FOR TESTING,
      "! Test --> Add Message To Log.
      fxublr02_1746_ac1 FOR TESTING,
      "! Test --> Save log success
      fxublr02_1747_ac1 FOR TESTING,
      "! Test --> Save log failed
      fxublr02_1747_ac2 FOR TESTING,

      determine_excel_language FOR TESTING,
      generate_dynamic_table   FOR TESTING,
      apply_sys_loacle         FOR TESTING,
      process_data             FOR TESTING.

ENDCLASS.

CLASS ltc_upload_handler IMPLEMENTATION.

  METHOD class_setup.
    SELECT * FROM mass_op_strm_tmp ORDER BY created_on DESCENDING, guid DESCENDING INTO @ms_mass_ops_strm UP TO 1 ROWS.
    ENDSELECT.
    "   Create OSQL environment for testing.
    mo_sql_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'MASS_OP_STRM_TMP' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    "   Destroy the environment for testing
    mo_sql_environment->destroy(  ).
  ENDMETHOD.

  METHOD setup.
    "   Generate instance of CLASS Under Test
    mo_upload_handler = CAST #( cl_exl_ops_upload_handler=>create( 'LCL_EXL_OPS_UPLOAD_HANDLER' ) ).
    "   Clear the double
    mo_sql_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD create_sql_double.
    "   Create data for double
    t_temp_db = VALUE #( ( guid          = '123'
                           exl_doc_id    = |EXL_OPS_CCTR| ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_temp_db ).
  ENDMETHOD.

  METHOD fxublr02_1737_ac1.
    TRY.
        mo_upload_handler->process_excel( iv_datastream_guid = ms_mass_ops_strm-guid
                                          iv_business_object = ' ' ).
      CATCH cx_aunit_uncaught_message.
    ENDTRY.
  ENDMETHOD.

  METHOD fxublr02_1740_ac1.
    mo_upload_handler->ms_mass_operation_stream_data = ms_mass_ops_strm.
    TRY.
        mo_upload_handler->_convert_stream_to_data( ).

        CLEAR: mo_upload_handler->ms_mass_operation_stream_data.
        mo_upload_handler->_convert_stream_to_data( ).

      CATCH cx_exl_ops cx_ofi_doc cx_root.
        "handle exception
        DATA(lv_exception_occured) = abap_true.
    ENDTRY.
  ENDMETHOD.

  METHOD fxublr02_1742_ac1.
    cl_abap_unit_assert=>assert_bound( act = mo_upload_handler
                                       msg = |Failed: Instance of Upload Handler could not be generated| ).

    FREE mo_upload_handler.
    cl_abap_unit_assert=>assert_initial( act = mo_upload_handler
                                         msg = |Failed: Instance of Upload Handler was generated| ).
  ENDMETHOD.

  METHOD fxublr02_1743_ac1.
    create_sql_double( ).
    TRY.
        mo_upload_handler->_extract_datastream_for_upload( '123' ).
      CATCH cx_exl_ops.
        "handle exception
    ENDTRY.
    "  Check private attribute MS_MASS_OPERATION_STREAM_DATA is filled
    cl_abap_unit_assert=>assert_not_initial( act = mo_upload_handler->ms_mass_operation_stream_data
                                             msg = |Failed: Extraction of Data-Stream| ).
  ENDMETHOD.

  METHOD fxublr02_1743_ac2.
    mo_sql_environment->clear_doubles( ).
    CLEAR: mo_upload_handler->ms_mass_operation_stream_data.
    TRY.
        mo_upload_handler->_extract_datastream_for_upload( '123' ).
      CATCH cx_exl_ops.
        "handle exception
    ENDTRY.
    "  Check private attribute MS_MASS_OPERATION_STREAM_DATA is filled
    cl_abap_unit_assert=>assert_initial( act = mo_upload_handler->ms_mass_operation_stream_data
                                         msg = |Failed: Extraction of Data-Stream successful.| ).
  ENDMETHOD.

  METHOD fxublr02_1745_ac1.
    mo_upload_handler->_create_job_log( iv_business_object = 'BUS2014' ).
    cl_abap_unit_assert=>assert_not_initial( act = mo_upload_handler->mv_log_handle
                                             msg = |Application Message Log handler instance generation failed.| ).
  ENDMETHOD.

  METHOD fxublr02_1745_ac2.
    cl_abap_unit_assert=>assert_not_initial( act = mo_upload_handler->_create_job_log( iv_business_object = '' )
                                             msg = |Application Message Log handler instance generation successful.| ).
  ENDMETHOD.

  METHOD fxublr02_1746_ac1.
    DATA: lt_message TYPE bal_t_msg.
    mo_upload_handler->_add_message_to_log( it_messages = VALUE #( ( msgty = 'I'
                                                                     msgid = |EXL_OPS|
                                                                     msgno = 000 ) ) ).
    mo_upload_handler->_add_message_to_log( lt_message ).
  ENDMETHOD.

  METHOD fxublr02_1747_ac1.
    "   Test Save log success
    mo_upload_handler->_create_job_log( iv_business_object = 'BUS2014' ).
    cl_abap_unit_assert=>assert_initial( act = mo_upload_handler->_save_log( )
                                         msg = |Log save failure.| ).
  ENDMETHOD.

  METHOD fxublr02_1747_ac2.
    "   Test Save log failure
    cl_abap_unit_assert=>assert_not_initial( act = mo_upload_handler->_save_log( )
                                             msg = |Log save success.| ).
  ENDMETHOD.

  METHOD apply_sys_loacle.
    DATA(lv_value) = VALUE string( ).
    mo_upload_handler->_apply_system_locale(
      EXPORTING
        iv_sheet_id   = 'WS_1'                 " Excel Operations Worksheet ID
        iv_fieldname  = 'CENTRALPURCHASECONTRACT'                 " Global Field Name
        iv_fieldvalue = '12345'                 " Current Cell Raw Data
*        iv_currentrow =                  " Current Row Raw Data
*        iv_rowheader  =                  " Current Row Header Information
      IMPORTING
        ev_fieldvalue = lv_value                 " Current Cell Formatted Data
*      RECEIVING
*        rv_status     =                  " Boolean Variable (X = True, - = False, Space = Unknown)
    ).
  ENDMETHOD.


  METHOD determine_excel_language.
    TRY.
        cl_abap_unit_assert=>assert_not_initial( act = mo_upload_handler->_determine_excel_language( iv_doc_id     = |EXL_OPS_CCTR|
                                                                                                     iv_sheet_name = 'Central Contract Header' ) " Actual Data Object
                                                 msg = |Language determination failed| ).                                                        " Message in Case of Error
        DATA(lv_language) = mo_upload_handler->_determine_excel_language( iv_doc_id     = |EXL_OPS_CCTR|
                                                                          iv_sheet_name = ' ' ).
      CATCH cx_exl_ops. " Exception class for Excel Operations
        DATA(lv_exception_occurred) = abap_true.
    ENDTRY.
  ENDMETHOD.

  METHOD generate_dynamic_table.
    TRY.
        DATA(lt_configuration_data) = cl_exl_ops_utilities=>get_exl_ops_config_details( iv_document_id  = |EXL_OPS_CCTR|
                                                                                        iv_language_key = sy-langu ).
        DATA(lt_fiedl_notation) = cl_exl_ops_utilities=>get_exl_ops_field_notation( CHANGING ct_exl_ops_config = lt_configuration_data ).                  " Table: Excel Operation configuration details
        cl_abap_unit_assert=>assert_not_initial( act = cl_dynamic_table_generator=>generate_dynamic_table( iv_sheet_id           = |WS_1|
                                                                                                           it_configuration_data = lt_configuration_data ) " Actual data object
                                                 msg = |Failed: Dynamic table generation| ).                                                               " Message in Case of Error
        CLEAR lt_configuration_data.
        cl_abap_unit_assert=>assert_initial( act = cl_dynamic_table_generator=>generate_dynamic_table( iv_sheet_id           = |WS_1|
                                                                                                       it_configuration_data = lt_configuration_data )
                                             msg = |Failed: Test dynamic table generation.| ).
      CATCH cx_exl_ops. " Exception class for Excel Operations
    ENDTRY.
  ENDMETHOD.

  METHOD process_data.
    TRY.
        mo_upload_handler->_process_data( it_excel_data = VALUE #( ( ) ) ).
      CATCH cx_exl_ops.
    ENDTRY.
  ENDMETHOD.



ENDCLASS.
