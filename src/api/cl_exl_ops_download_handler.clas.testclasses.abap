*"* use this source file for your ABAP unit test classes

CLASS ltc_private_member DEFINITION DEFERRED.
CLASS cl_exl_ops_download_handler DEFINITION LOCAL FRIENDS ltc_private_member.

CLASS ltc_private_member DEFINITION FINAL
FOR TESTING DURATION LONG RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-METHODS:
      class_setup,
      class_teardown.

    CLASS-DATA:
                  mo_sql_environment TYPE REF TO if_osql_test_environment.
    METHODS:
      setup,
      teardown,
      create_sql_double,
      create_sql_double_with_error,
      handler_instantiated           FOR TESTING,
      test_load_table_with_data      FOR TESTING,
      test_generate_dynamic_table    FOR TESTING,
      test_dynamic_table_fail        FOR TESTING,
      fxublr02_1713_ac1              FOR TESTING,
      read_apply_meta_config_success FOR TESTING,
      read_apply_meta_config_no_data FOR TESTING,
      read_apply_meta_cfg_no_modify FOR TESTING.
    DATA:
      mo_handler             TYPE REF TO cl_exl_ops_download_handler,
      t_excel_bo             TYPE STANDARD TABLE OF exl_ops_bo,
      t_excel_worksheet      TYPE STANDARD TABLE OF exl_ops_ws,
      t_excel_worksheet_cols TYPE STANDARD TABLE OF exl_ops_ws_col,
      t_excel_worksheet_txt  TYPE STANDARD TABLE OF exl_ops_ws_txt,
      t_excel_appl_config    TYPE STANDARD TABLE OF exl_ops_appl_cfg.
ENDCLASS.

CLASS ltc_private_member IMPLEMENTATION.

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

  METHOD create_sql_double.
    "   --> EXL_OPS_BO
    "   Create data for double
    t_excel_bo = VALUE #( ( exl_doc_id   = |EXL_OPS_CCTR|
                             exl_doc_name = |Central Purchase Contract| ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_bo ).

    "   --> EXL_OPS_WS
    "   Create data for double
    t_excel_worksheet = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1| )
                                 ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_2| )
                                 ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_3| )
                                 ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_4| ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_worksheet ).

    "   -->EXL_OPS_WS_TXT
    "   Create data for double
    t_excel_worksheet_txt = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1| langu = |E| exl_ws_name = |Central Contract Header| )
                                     ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_2| langu = |E| exl_ws_name = |Central Contract Item| )
                                     ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_3| langu = |E| exl_ws_name = |Contract Header Distribution| )
                                     ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_4| langu = |E| exl_ws_name = |Contract Item Distribution| )  ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_worksheet_txt ).

    "   -->EXL_OPS_WS_COL
    "   Create data for double
    t_excel_worksheet_cols = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1|  exl_field_name = |EBELN|                exl_gfn = |CENTRALPURCHASECONTRACT|       is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1|  exl_field_name = |BUKRS|                exl_gfn = |COMPANYCODE|                   is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1|  exl_field_name = |KDATE|                exl_gfn = |VALIDITYENDDATE|               is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1|  exl_field_name = |KTWRT|                exl_gfn = |PURCHASECONTRACTTARGETAMOUNT|  is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1|  exl_field_name = |WAERS|                exl_gfn = |CURRENCY|                      is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_2|  exl_field_name = |EBELN|                exl_gfn = |CENTRALPURCHASECONTRACT|       is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_2|  exl_field_name = |EBELP|                exl_gfn = |CENTRALPURCHASECONTRACTITEM|   is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_2|  exl_field_name = |KTMNG|                exl_gfn = |TARGETQUANTITY|                is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_2|  exl_field_name = |PRSDR|                exl_gfn = |PRICEISTOBEPRINTED|            is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_3|  exl_field_name = |EBELN|                exl_gfn = |CENTRALPURCHASECONTRACT|       is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_3|  exl_field_name = |MM_PURGDOC_DISTR_NUM| exl_gfn = |DISTRIBUTIONKEY|               is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_3|  exl_field_name = |BKGRP|                exl_gfn = |PROCMTHUBPURCHASINGGROUP|      is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_4|  exl_field_name = |EBELN|                exl_gfn = |CENTRALPURCHASECONTRACT|       is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_4|  exl_field_name = |EBELP|                exl_gfn = |CENTRALPURCHASECONTRACTITEM|   is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_4|  exl_field_name = |MM_PURGDOC_DISTR_NUM| exl_gfn = |DISTRIBUTIONKEY|               is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_3|  exl_field_name = |FARP_DZTERM|          exl_gfn = |PAYMENTTERMS|                  is_downloadable = abap_true ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_worksheet_cols ).

    "   -->EXL_OPS_APPL_CFG
    "   Create data for double
    t_excel_appl_config = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1| cds = |C_PURCNTRLCONTRMASSUPDT|    get_data_class = |CL_MM_MASS_OPS_CCTR| get_data_method = |CCTR_HEADER_GETDATA|           hierarchy_notation = |1.0| )
                                   ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_2| cds = |C_PURCNTRLCONTRITMMASSUPDT| get_data_class = |CL_MM_MASS_OPS_CCTR| get_data_method = |CCTR_ITEM_GETDATA|             hierarchy_notation = |1.1| )
                                   ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_3| cds = |C_CNTRLPURCONTRHDRDISTR|    get_data_class = |CL_MM_MASS_OPS_CCTR| get_data_method = |CCTR_HDRDISTRIBUTION_GETDATA|  hierarchy_notation = |1.2| )
                                   ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_4| cds = |C_CNTRLPURCONTRITMDISTR|    get_data_class = |CL_MM_MASS_OPS_CCTR| get_data_method = |CCTR_ITEMDISTRIBUTION_GETDATA| hierarchy_notation = |1.1.1| )  ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_appl_config ).
  ENDMETHOD.

  METHOD create_sql_double_with_error.
    "   Create data for double --> EXL_OPS_BO
    t_excel_bo = VALUE #( ( exl_doc_id   = |EXL_OPS_CCTR|
                            exl_doc_name = |Central Purchase Contract| ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_bo ).
    "   Create data for double --> EXL_OPS_WS
    t_excel_worksheet = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1| ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_worksheet ).
    "   Create data for double -->EXL_OPS_WS_TXT
    t_excel_worksheet_txt = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1| langu = |E| exl_ws_name = |Central Contract Header| )  ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_worksheet_txt ).
    "   Create data for double -->EXL_OPS_WS_COL
    t_excel_worksheet_cols = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1|  exl_field_name = |EBELN|                exl_gfn = |CENTRALPURCHASECONTRACT|     is_downloadable = abap_true )
                                      ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1|  exl_field_name = |ABC  |                exl_gfn = |CENTRALPURCHASECONTRACT1|     is_downloadable = abap_true ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_worksheet_cols ).
    "   Create data for double -->EXL_OPS_APPL_CFG
    t_excel_appl_config = VALUE #( ( exl_doc_id = |EXL_OPS_CCTR| exl_ws_id = |WS_1| cds = |C_PURCNTRLCONTRMASSUPDT| get_data_class = |CL_MM_MASS_OPS_CCTR| get_data_method = |CCTR_HEADER_GETDATA| hierarchy_notation = |1.0| ) ).
    "   Inject data into double
    mo_sql_environment->insert_test_data( t_excel_appl_config ).
  ENDMETHOD.

  METHOD setup.
    mo_handler = cl_exl_ops_download_handler=>create( ).
    mo_sql_environment->clear_doubles( ).
*    create_sql_double( ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_handler.
  ENDMETHOD.

  METHOD handler_instantiated.
    cl_abap_unit_assert=>assert_bound( act = mo_handler
                                       msg = |Handler instance creation error| ).
  ENDMETHOD.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "                     FXUBLR02-1712
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD test_load_table_with_data.
    " FXUBLR02-1712-AC1: It loads the structure generated with the data which will be downloaded to the excel.
    DATA: lo_data TYPE REF TO data.
    CREATE DATA lo_data TYPE STANDARD TABLE OF string.
    mo_handler->_load_table_with_data(
      EXPORTING
        io_sheet_data    = lo_data
      CHANGING
        ct_dynamic_table = lo_data
    ).

    FIELD-SYMBOLS: <ft_table> TYPE STANDARD TABLE.
    ASSIGN lo_data->* TO <ft_table>.

    cl_abap_unit_assert=>assert_initial( act = <ft_table>                                       " Actual Data Object
                                         msg = |Unable to load data to the table.| ).           " Message in Case of Error

  ENDMETHOD.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "                     FXUBLR02-1695
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD test_generate_dynamic_table.
    " FXUBLR02-1695-AC1: It returns a structure of the internal table corresponding to the sheet and the columns.

    TRY.
*        create_sql_double( ).
        "   Populate MT_CONFIGURATION_DATA table
        mo_handler->mt_configuration_data = VALUE #( ( exl_ws_id          = |WS_1|
                                                       langu              = |E|
                                                       exl_ws_name        = |Central Contract Header|
                                                       exl_field_name     = |EBELN|
                                                       exl_gfn            = |CENTRALPURCHASECONTRACT|
                                                       exl_data_element   = |EBELN|
                                                       enduser_label_text = |Central Purchase Contract|
                                                       cds                = |C_PURCNTRLCONTRMASSUPDT| ) ).

        mo_handler->_generate_dynamic_table( EXPORTING iv_sheet_id     = 'WS_1'                     " Excel Operations Worksheet ID
                                             IMPORTING et_table        = DATA(lt_dynamic_table)     " Generated Table Schema
                                                       et_sheet_fields = DATA(lt_columns) ).        " Generated Column List
      CATCH cx_exl_ops INTO DATA(lx_exl_ops).
        "handle exception
        DATA(lv_exception_occurred) = abap_true.
    ENDTRY.
    "   Check Dynamic Table Schema is generated
    cl_abap_unit_assert=>assert_bound( act = lt_dynamic_table                                       " Reference variable to be checked
                                       msg = |Dynamic table schema generation failed.| ).           " Description

    "   Check column names of dynamic table returned
    cl_abap_unit_assert=>assert_not_initial( act = lt_columns                                       " Actual Data Object
                                             msg = |Dynamic table columns details not found.| ).    " Message in Case of Error
  ENDMETHOD.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "                     FXUBLR02-1695
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD test_dynamic_table_fail.
    " FXUBLR02-1695: It should not returns a structure of the internal table corresponding to the sheet and the columns.

*    create_sql_double( ).
    mo_sql_environment->clear_doubles( ).

    mo_handler->_generate_dynamic_table( EXPORTING iv_sheet_id     = 'WS_6'                     " Excel Operations Worksheet ID
                                         IMPORTING et_table        = DATA(lt_dynamic_table)     " Generated Table Schema
                                                   et_sheet_fields = DATA(lt_columns) ).        " Generated Column List

    cl_abap_unit_assert=>assert_initial( act = lt_dynamic_table
                                         msg = |Dynamic table schema generated.| ).

    cl_abap_unit_assert=>assert_initial( act = lt_columns                                       " Actual Data Object
                                        msg = |Dynamic table columns found.| ).


  ENDMETHOD.
**********************************************************************
*                       FXUBLR02-1713
**********************************************************************
  METHOD fxublr02_1713_ac1.
    " FXUBLR02-1713-AC1: Should return the data-stream generated.
    create_sql_double( ).
    DATA(lv_stream) = mo_handler->generate_excel( 'EXL_OPS_CCTR' ).

    cl_abap_unit_assert=>assert_not_initial( act = lv_stream                                         " Actual Data Object
                                             msg = |Stream generation for excel download failed.| ). " Message in Case of Error

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Test for table schema generation fail
    mo_sql_environment->clear_doubles( ).
    create_sql_double_with_error( ).
    mo_handler->generate_excel( EXPORTING iv_document = 'EXL_OPS_CCTR'
                                IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_not_initial( act = lt_messages
                                             msg = |Exception block not covered.| ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   TEST exception block covered.
    mo_sql_environment->clear_doubles( ).
    CLEAR: lt_messages.

    mo_handler->generate_excel( EXPORTING iv_document = 'EXL_OPS_CCTR'
                                IMPORTING et_messages = lt_messages ).

    cl_abap_unit_assert=>assert_not_initial( act = lt_messages
                                             msg = |Exception block not covered.| ).
  ENDMETHOD.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "                     FXUBLR02-1711
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  METHOD read_apply_meta_config_no_data.
    " FXUBLR02-1711-AC2: change the data retrieved from RETRIEVED_DATA with the configuration details and meta information.
    TRY.
        DATA: lt_sheet_data TYPE REF TO data.
        mo_handler->_apply_meta_config(
          EXPORTING
            iv_sheet_id   =  'WS_1'                " Excel Operations Worksheet ID
          CHANGING
            ct_sheet_data =  lt_sheet_data
        ).
      CATCH cx_exl_ops INTO DATA(lx_exl_ops).
        "handle exception
        DATA(lv_exception_occurred) = abap_true.
    ENDTRY.
    cl_abap_unit_assert=>assert_not_initial( act = mo_handler
                                             msg = |Exception due to empty dataset| ).
  ENDMETHOD.

  METHOD read_apply_meta_config_success.
    create_sql_double( ).
    " FXUBLR02-1711-AC1: change the data retrieved from RETRIEVED_DATA with the configuration details and meta information.
    DATA(lv_stream) = mo_handler->generate_excel( 'EXL_OPS_CCTR' ).

    cl_abap_unit_assert=>assert_not_initial( act = lv_stream                                         " Actual Data Object
                                             msg = |Stream generation for excel download failed.| ). " Message in Case of Error
  ENDMETHOD.

  METHOD read_apply_meta_cfg_no_modify.
    " FXUBLR02-1711-AC3: change the data retrieved from RETRIEVED_DATA with the configuration details and meta information.

    TRY.
        DATA: lt_sheet_data TYPE REF TO data.
        CREATE DATA lt_sheet_data TYPE STANDARD TABLE OF string.
        CLEAR: mo_handler->mt_field_notation.
        mo_handler->_apply_meta_config(
          EXPORTING
            iv_sheet_id   =  'WS_1'                " Excel Operations Worksheet ID
          CHANGING
            ct_sheet_data =  lt_sheet_data
        ).
      CATCH cx_exl_ops INTO DATA(lx_exl_ops).
        "handle exception
        DATA(lv_exception_occurred) = abap_true.
    ENDTRY.
    cl_abap_unit_assert=>assert_not_initial( act = mo_handler
                                             msg = |Exception due to no modifiable columns| ).
  ENDMETHOD.

ENDCLASS.
