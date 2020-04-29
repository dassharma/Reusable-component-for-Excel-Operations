*"* use this source file for your ABAP unit test classes
**********************************************************************
CLASS ltd_guid_generator DEFINITION FINAL CREATE PRIVATE FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_double RETURNING VALUE(ro_instance) TYPE REF TO ltd_guid_generator.

    INTERFACES if_exl_ops_job_scheduler.

    CLASS-DATA: mv_exception TYPE boolean.

ENDCLASS.

CLASS ltd_guid_generator IMPLEMENTATION.

  METHOD create_double.
    ro_instance = NEW ltd_guid_generator( ).
  ENDMETHOD.

  METHOD if_exl_ops_job_scheduler~generate_guid.
    IF mv_exception = abap_true.
      RAISE EXCEPTION TYPE cx_uuid_error.
    ELSE.
      rv_uuid = '1234567890123456'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS ltc_controller DEFINITION DEFERRED.
CLASS cl_exl_ops_controller DEFINITION LOCAL FRIENDS ltc_controller.

CLASS ltc_controller DEFINITION FINAL
FOR TESTING DURATION LONG RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
                mo_sql_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    DATA:
            mo_controller TYPE REF TO cl_exl_ops_controller.

    METHODS:
      setup,
      test_cut_instance FOR TESTING,
      create_downld_handler_instance FOR TESTING,
      create_upload_handler_instance FOR TESTING,
      test_download_excel FOR TESTING,
      "!  TEST --> Upload Excel
      fxublr02_1737_ac1 FOR TESTING,
      "! Test stream information stored in DB table with guid.
      fxublr02_1723_ac1 FOR TESTING,
      "! Test for job being scheduled properly.
      fxublr02_1723_ac2 FOR TESTING,
      fxublr02_1723_ac3 FOR TESTING.
ENDCLASS.

CLASS ltc_controller IMPLEMENTATION.

  METHOD class_setup.
    mo_sql_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'MASS_OP_STRM_TMP' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_sql_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mo_controller = cl_exl_ops_controller=>create( iv_schedule_flag = abap_true ).
  ENDMETHOD.

  METHOD test_cut_instance.
    cl_abap_unit_assert=>assert_bound( act = mo_controller                                         " Reference variable to be checked
                                       msg = |CUT object instantiation failed.| ).                 " Description
  ENDMETHOD.

  METHOD create_downld_handler_instance.
    mo_controller->create_handler_instance( ).

    cl_abap_unit_assert=>assert_bound( act = mo_controller->mo_download_handler                    " Reference variable to be checked
                                       msg = |Creation of handler instance failed.| ).             " Description
  ENDMETHOD.

  METHOD create_upload_handler_instance.
    mo_controller->create_handler_instance( |CL_EXL_OPS_MM_CCTR_UPLD_HANDLR| ).

    cl_abap_unit_assert=>assert_bound( act = mo_controller->mo_upload_handler                      " Reference variable to be checked
                                       msg = |Creation of handler instance failed.| ).             " Description
  ENDMETHOD.

  METHOD test_download_excel.
    cl_abap_unit_assert=>assert_not_initial( act = mo_controller->download_excel( 'EXL_OPS_CCTR' ) " Actual Data Object
                                             msg = |Unable to generate excel stream.| ).           " Message in Case of Error
  ENDMETHOD.

  METHOD fxublr02_1737_ac1.
    mo_controller->upload_excel( iv_guid            = '01234567890123456'
                                 iv_business_object = |BUS2014|
                                 iv_class_name      = |CL_EXL_OPS_MM_CCTR_UPLD_HANDLR| ).
  ENDMETHOD.

  METHOD fxublr02_1723_ac1.
    DATA(lo_double) = ltd_guid_generator=>create_double( ).
    mo_controller->mo_guid_generator = lo_double.
    mo_controller->schedule_background_job( EXPORTING is_job_details         = VALUE #( template_name   = |SAP_MM_PUR_MASSCCTRBG_T|
                                                                                        text            = |TEST_RUN|
                                                                                         )
                                                      is_job_scheduling_info = VALUE #( start_info = VALUE #( start_immediately = abap_true ) )
                                                      is_excel_information   = VALUE #( excel_document_id = |EXL_OPS_CCTR|
                                                                                        stream_type       = |EXCEL| )
*                                                      iv_data_stream         = conv #(  'adafa' )
                                                      iv_test_mode           = abap_true
                                            IMPORTING et_message             = DATA(lt_messages)
                                                      es_job_details         = DATA(ls_job_details) ).
    TRY.
        DATA(lv_guid) = lo_double->if_exl_ops_job_scheduler~generate_guid( ).
      CATCH cx_uuid_error cx_exl_ops.
    ENDTRY.
    SELECT SINGLE * FROM mass_op_strm_tmp INTO @DATA(ls_mass_op_strm_tmp) WHERE guid = @lv_guid.
    cl_abap_unit_assert=>assert_subrc( act = sy-subrc
                                       msg = |Data not saved to DB.| ).
  ENDMETHOD.

  METHOD fxublr02_1723_ac2.
    mo_sql_environment->clear_doubles( ).

    DATA(lo_scheduler_double) = ltd_guid_generator=>create_double( ).

    mo_controller->schedule_background_job( EXPORTING is_job_details         = VALUE #( template_name   = |SAP_MM_PUR_MASSCCTRBG_T|
                                                                                        text            = |TEST_RUN|
                                                                                        )
                                                      is_job_scheduling_info = VALUE #( start_info = VALUE #( start_immediately = abap_true ) )
                                                      is_excel_information   = VALUE #( excel_document_id = |EXL_OPS_CCTR|
                                                                                        stream_type       = |EXCEL| )
*                                                      iv_data_stream         = conv #(  'adafa' )
                                                      iv_test_mode           = abap_true
                                            IMPORTING et_message             = DATA(lt_messages)
                                                      es_job_details         = DATA(ls_job_details) ).
    cl_abap_unit_assert=>assert_not_initial( act = ls_job_details-job_name
                                             msg = |Data not saved to DB.| ).

  ENDMETHOD.

  METHOD fxublr02_1723_ac3.
    mo_sql_environment->clear_doubles( ).

    DATA(lo_double) = ltd_guid_generator=>create_double( ).

    lo_double->mv_exception = abap_true.

    mo_controller->mo_guid_generator = lo_double.

    mo_controller->schedule_background_job( EXPORTING is_job_details         = VALUE #( template_name   = |SAP_MM_PUR_MASSCCTRBG_T|
                                                                                        text            = |TEST_RUN|
                                                                                        )
                                                      is_job_scheduling_info = VALUE #( start_info = VALUE #( start_immediately = abap_true ) )
                                                      is_excel_information   = VALUE #( excel_document_id = |EXL_OPS_CCTR|
                                                                                        stream_type       = |EXCEL| )
*                                                      iv_data_stream         = conv #(  'adafa' )
                                                      iv_test_mode           = abap_true
                                            IMPORTING et_message             = DATA(lt_messages)
                                                      es_job_details         = DATA(ls_job_details) ).

    cl_abap_unit_assert=>assert_initial( act = ls_job_details-job_name
                                         msg = |Data not saved to DB.| ).
  ENDMETHOD.

ENDCLASS.
**********************************************************************

CLASS ltc_scheduler DEFINITION FINAL
*INHERITING FROM cl_exl_ops_job_scheduler
FOR TESTING DURATION LONG RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS:
      test_instance_generation FOR TESTING.

ENDCLASS.

CLASS ltc_scheduler IMPLEMENTATION.

  METHOD test_instance_generation.

    cl_abap_unit_assert=>assert_bound( act = cl_exl_ops_guid_generator=>create( )
                                       msg = |Helper Class instantiation failed.| ).
  ENDMETHOD.

ENDCLASS.
