*"* use this source file for your ABAP unit test classes

"!  <p class="shorttext synchronized" lang="en">Test Class for Excel Operation Utility class</p>
CLASS ltc_exl_ops_utilities DEFINITION FOR TESTING
    DURATION LONG RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
                  mo_sql_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    DATA:
      t_excel_bo             TYPE STANDARD TABLE OF exl_ops_bo,
      t_excel_worksheet      TYPE STANDARD TABLE OF exl_ops_ws,
      t_excel_worksheet_cols TYPE STANDARD TABLE OF exl_ops_ws_col,
      t_excel_worksheet_txt  TYPE STANDARD TABLE OF exl_ops_ws_txt,
      t_excel_appl_config    TYPE STANDARD TABLE OF exl_ops_appl_cfg.

    METHODS:
      "!  <p class="shorttext synchronized" lang="en">Create the double for mocking data</p>
      create_double,
      "!  <p class="shorttext synchronized" lang="en">Test for existing configuration data pass</p>
      fxublr02_1692_ac1 FOR TESTING,    "   Test configuration data exists
      "!  <p class="shorttext synchronized" lang="en">Test for existing configuration data failed</p>
      fxublr02_1692_ac2 FOR TESTING,    "   Test configuration data do not exist
      "!  <p class="shorttext synchronized" lang="en">Test for Field Notation data successful</p>
      fxublr02_1693_ac1 FOR TESTING,    "   Test field notation data exists
      "!  <p class="shorttext synchronized" lang="en">Test for Field Notation data failed</p>
      fxublr02_1693_ac2 FOR TESTING,    "   Test field notation data do not exist
      test_extract_itab_components FOR TESTING.
ENDCLASS.

CLASS ltc_exl_ops_utilities IMPLEMENTATION.

  METHOD class_setup.
    "   Create OSQL environment for testing.
    mo_sql_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'EXL_OPS_BO' )
                                                                                        ( 'EXL_OPS_WS' )
                                                                                        ( 'EXL_OPS_WS_COL' )
                                                                                        ( 'EXL_OPS_WS_TXT' )
                                                                                        ( 'EXL_OPS_APPL_CFG' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_sql_environment->destroy(  ).
  ENDMETHOD.

  METHOD create_double.
    "   Create data for  --> EXL_OPS_BO
    t_excel_bo = VALUE #( ( exl_doc_id   = |EXL_OPS_CCTR|
                             exl_doc_name = |Central Purchase Contract| ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_bo ).

    "   Create data for --> EXL_OPS_WS
    t_excel_worksheet = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1| ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_worksheet ).

    "   Create data for -->EXL_OPS_WS_TXT
    t_excel_worksheet_txt = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1| langu = |E| exl_ws_name = |Central Contract Header| ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_worksheet_txt ).

    "   Create data for -->EXL_OPS_WS_COL
    t_excel_worksheet_cols = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1|  exl_field_name = |EBELN| exl_gfn = |CENTRALPURCHASECONTRACT| is_downloadable = abap_true ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_worksheet_cols ).

    "   Create data for -->EXL_OPS_APPL_CFG
    t_excel_appl_config = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1| cds = |C_PURCNTRLCONTRMASSUPDT|    get_data_class = |CL_MM_MASS_OPS_CCTR| get_data_method = |CCTR_HEADER_GETDATA|           hierarchy_notation = |1.0| )  ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_appl_config ).
  ENDMETHOD.

  METHOD fxublr02_1692_ac1.
    "   Test for existing data in the double for EXL_OPS_CCTR
    TRY.
        create_double( ).
        DATA(lt_data) = cl_exl_ops_utilities=>get_exl_ops_config_details( iv_document_id  = |EXL_OPS_CCTR|
                                                                          iv_language_key = sy-langu ).
      CATCH cx_exl_ops.
        "handle exception
    ENDTRY.
    cl_abap_unit_assert=>assert_not_initial(  act = lt_data
                                              msg = |Failed: Excel operation configuration data extraction.| ).
  ENDMETHOD.

  METHOD fxublr02_1692_ac2.
    "   Test for NULL data
    TRY.
        mo_sql_environment->clear_doubles( ).
        DATA(lt_data) = cl_exl_ops_utilities=>get_exl_ops_config_details( iv_document_id  = | |
                                                                          iv_language_key = sy-langu ).
      CATCH cx_exl_ops.
        "handle exception
        DATA(lv_exception_occurred) = abap_true.
    ENDTRY.
    cl_abap_unit_assert=>assert_true( act = lv_exception_occurred
                                      msg = |Failed: Exception did not occur.| ).
  ENDMETHOD.

  METHOD fxublr02_1693_ac1.
    "   Test for Field notation data
    mo_sql_environment->clear_doubles( ).
    DATA(lt_config_details) =  VALUE cl_exl_ops_utilities=>tt_configuration_data( ( exl_field_name     = |EBELN|
                                                                                    exl_gfn            = |CENTRALPURCHASECONTRACT|
                                                                                    is_downloadable    = abap_true
                                                                                    cds                = |C_PURCNTRLCONTRMASSUPDT| )
                                                                                (   exl_field_name     = |KDATE|
                                                                                    exl_gfn            = |VALIDITYENDDATE|
                                                                                    is_downloadable    = abap_true
                                                                                    cds                = |C_PURCNTRLCONTRMASSUPDT| ) ).
    DATA(lt_field_data) = cl_exl_ops_utilities=>get_exl_ops_field_notation( CHANGING ct_exl_ops_config = lt_config_details ).
    cl_abap_unit_assert=>assert_not_initial(  act = lt_field_data
                                              msg = |Failed: Field Notation extraction.| ).
  ENDMETHOD.

  METHOD fxublr02_1693_ac2.
    "   Test for Field notation data = NULL
    mo_sql_environment->clear_doubles( ).
    DATA(lt_config_details) =  VALUE cl_exl_ops_utilities=>tt_configuration_data( ( ) ).
    cl_abap_unit_assert=>assert_initial(  act = cl_exl_ops_utilities=>get_exl_ops_field_notation( CHANGING ct_exl_ops_config = lt_config_details )
                                          msg = |Failed: Field Notation extraction data present.| ).

    lt_config_details = VALUE cl_exl_ops_utilities=>tt_configuration_data( ( exl_field_name     = |EBELN|
                                                                             exl_gfn            = |CENTRALPURCHASECONTRACT|
                                                                             is_downloadable    = abap_true
                                                                             cds                = |C_PURCNTRLCONTRMASSUPDT| ) ).
    cl_abap_unit_assert=>assert_initial(  act = cl_exl_ops_utilities=>get_exl_ops_field_notation( CHANGING ct_exl_ops_config = lt_config_details )
                                          msg = |Failed: Field Notation extraction data present.| ).

    CLEAR: lt_config_details.
    lt_config_details = VALUE cl_exl_ops_utilities=>tt_configuration_data( ( exl_field_name     = |EBELN|
                                                                             exl_gfn            = |CENTRALPURCHASECONTRACT1|
                                                                             is_downloadable    = abap_true
                                                                             cds                = |C_PURCNTRLCONTRMASSUPDT| ) ).
    cl_abap_unit_assert=>assert_initial(  act = cl_exl_ops_utilities=>get_exl_ops_field_notation( CHANGING ct_exl_ops_config = lt_config_details )
                                          msg = |Failed: Field Notation extraction data present.| ).

  ENDMETHOD.

  METHOD test_extract_itab_components.
    DATA: lr_data TYPE REF TO data.

    CREATE DATA lr_data TYPE cl_exl_ops_utilities=>tt_sheets_data.

    cl_abap_unit_assert=>assert_not_initial( act = cl_exl_ops_utilities=>extract_itab_components( lr_data  )
                                             msg = |Failed: Extraction of components.| ).
  ENDMETHOD.

ENDCLASS.
