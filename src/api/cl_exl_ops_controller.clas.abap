"! <p class="shorttext synchronized" lang="en">Mass excel download controller API</p>
class CL_EXL_OPS_CONTROLLER definition
  public
  final
  create private .

public section.

  types:
      "! Structure: Information pertaining to job
    BEGIN OF ty_job_details,
        "! Job template name, i.e., the job that will be scheduled
        template_name TYPE apj_job_template_name,
        "!  Job description
        text          TYPE apj_job_text,
*        parameter_value TYPE if_apj_rt_types=>tt_job_parameter_value,
      END OF ty_job_details .
  types:
      "! Structure: Information pertaining to job scheduling
    BEGIN OF ty_job_information,
        "! Scheduling information to start the job
        start_info        TYPE cl_apj_rt_job_scheduling_api=>ty_start_info,
        "! Scheduling information to end the job
        end_info          TYPE cl_apj_rt_job_scheduling_api=>ty_end_info,
        "! Information pertaining to the job scheduled
        scheduling_info   TYPE cl_apj_rt_job_scheduling_api=>ty_scheduling_info,
        "! Information for job start adjustment
        adjust_start_info TYPE boolean,
      END OF ty_job_information .
  types:
      "! Structure: Excel information
    BEGIN OF ty_excel_information,
        "! Type of stream: Excel/JSON
        stream_type       TYPE stream_type,
        "! Excel related business document ID
        excel_document_id TYPE exl_doc_id,
        "! Excel data converted as data-stream
        excel_stream      TYPE xstring,
      END OF ty_excel_information .

*    "! <p class="shorttext synchronized" lang="en">Create the instance of the controller to schedule job.</p>
*    "!
*    "! @parameter ro_instance | <p class="shorttext synchronized" lang="en">Generated Instance</p>
*    CLASS-METHODS create_job_schdeuler RETURNING VALUE(ro_instance) TYPE REF TO cl_exl_ops_controller.
    "! <p class="shorttext synchronized" lang="en">Create the instance of the controller to schedule job.</p>
    "! @parameter ro_instance | <p class="shorttext synchronized" lang="en">Generated Instance</p>
  class-methods CREATE
    importing
      !IV_SCHEDULE_FLAG type BOOLEAN optional
    returning
      value(RO_INSTANCE) type ref to CL_EXL_OPS_CONTROLLER .
      "! <p class="shorttext synchronized" lang="en">Execute the functionality of download excel and get the data</p>
      "! @parameter iv_document          | <p class="shorttext synchronized" lang="en">Excel Operations Document ID</p>
      "! @parameter it_filter_conditions | <p class="shorttext synchronized" lang="en">Table: Filter conditions from UI</p>
      "! @parameter it_inclusion_array   | <p class="shorttext synchronized" lang="en">Table: Inclusion parameters</p>
      "! @parameter it_exclusion_array   | <p class="shorttext synchronized" lang="en">Table: Exclusion parameters</p>
      "! @parameter et_messages          | <p class="shorttext synchronized" lang="en">Table: Messages as a result of processing</p>
      "! @parameter rv_data_stream       | <p class="shorttext synchronized" lang="en">Generated data-stream representing the excel</p>
  methods DOWNLOAD_EXCEL
    importing
      !IV_DOCUMENT type EXL_DOC_ID
      !IT_FILTER_CONDITIONS type /IWBEP/T_MGW_SELECT_OPTION optional
      !IT_SELECTED_DOCUMENTS type /IWBEP/T_COD_SELECT_OPTIONS optional
    exporting
      !ET_MESSAGES type BAL_T_MSG
    returning
      value(RV_DATA_STREAM) type XSTRING .
      "! <p class="shorttext synchronized" lang="en">Execute the action of upload of the excel document</p>
      "!
      "! @parameter iv_guid            | <p class="shorttext synchronized" lang="en">Unique ID of data-stream stored in DB</p>
      "! @parameter iv_business_object | <p class="shorttext synchronized" lang="en">Business Object for log creation</p>
      "! @parameter iv_class_name      | <p class="shorttext synchronized" lang="en">Extended upload handler class name</p>
  methods UPLOAD_EXCEL
    importing
      !IV_GUID type GUID
      !IV_BUSINESS_OBJECT type SWO_OBJTYP
      !IV_CLASS_NAME type SEOCLSNAME .
      "! <p class="shorttext synchronized" lang="en">API to schedule the job by saving relavant information in DB</p>
      "!
      "! @parameter is_job_details         | <p class="shorttext synchronized" lang="en">Application Job - Template name &amp; Job Description</p>
      "! @parameter is_job_scheduling_info | <p class="shorttext synchronized" lang="en">Application Job Scheduling Information</p>
      "! @parameter is_excel_information   | <p class="shorttext synchronized" lang="en">Information to start the application job</p>
      "! @parameter iv_test_mode           | <p class="shorttext synchronized" lang="en">Boolean Variable (X = True, - = False, Space = Unknown)</p>
      "! @parameter et_message             | <p class="shorttext synchronized" lang="en">Return parameter table</p>
      "! @parameter es_job_details         | <p class="shorttext synchronized" lang="en">JOB details</p>
  methods SCHEDULE_BACKGROUND_JOB
    importing
      !IS_JOB_DETAILS type TY_JOB_DETAILS
      !IS_JOB_SCHEDULING_INFO type TY_JOB_INFORMATION
      !IS_EXCEL_INFORMATION type TY_EXCEL_INFORMATION
      !IV_TEST_MODE type BOOLEAN optional
    exporting
      !ET_MESSAGE type BAPIRET2_T
      !ES_JOB_DETAILS type CL_APJ_RT_JOB_SCHEDULING_API=>TY_JOB_DETAILS .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      "! Instance of download handler
      mo_download_handler TYPE REF TO cl_exl_ops_download_handler,
      "! Instance of upload handler
      mo_upload_handler   TYPE REF TO cl_exl_ops_upload_handler,
      "! Instance of GUID generator
      mo_guid_generator   TYPE REF TO if_exl_ops_job_scheduler.


    METHODS:
      "! <p class="shorttext synchronized" lang="en">Instantiate the handler class instance</p>
      "! @parameter iv_class_name | <p class="shorttext synchronized" lang="en">Name of the extended handler</p>
      create_handler_instance IMPORTING iv_class_name TYPE seoclsname OPTIONAL.
ENDCLASS.



CLASS CL_EXL_OPS_CONTROLLER IMPLEMENTATION.


  METHOD create.
    ro_instance = NEW cl_exl_ops_controller( ).
    "   Check if schedule background job scenario
    IF iv_schedule_flag IS SUPPLIED AND iv_schedule_flag EQ abap_true.
      "   Populate private member: GUID Generator
      ro_instance->mo_guid_generator ?= cl_exl_ops_guid_generator=>create( ).
    ENDIF.
  ENDMETHOD.


  METHOD create_handler_instance.
    DATA: lo_handler TYPE REF TO object.

    "   Check if Implemented Class Name provided
    IF iv_class_name IS SUPPLIED.
*      *--------------------------------------------------------------------*
*      *                 UPLOAD SCENARIO
*      *--------------------------------------------------------------------*
      "   Generate the instance for the Implemented Class Name provided
      TRY.
          CALL METHOD (iv_class_name)=>create
            EXPORTING
              iv_class_name = iv_class_name
            RECEIVING
              ro_instance   = lo_handler.
          "   Set private memeber: UPLOAD HANDLER
          mo_upload_handler ?= lo_handler.
        CATCH cx_sy_dyn_call_error.
      ENDTRY.
    ELSE.
*      *--------------------------------------------------------------------*
*      *                   DOWNLOAD SCENARIO
*      *--------------------------------------------------------------------*
      mo_download_handler = cl_exl_ops_download_handler=>create( ).
    ENDIF.
  ENDMETHOD.


  METHOD download_excel.
    "   Generate the Download Handler Instance
    create_handler_instance( ).
    "   Check if Download Handler Instance is generated
    IF mo_download_handler IS NOT BOUND.
      RETURN.
    ENDIF.
    "   Generate the data-stream for download
    rv_data_stream = mo_download_handler->generate_excel( EXPORTING iv_document           = iv_document
                                                                    it_filter_conditions  = it_filter_conditions
                                                                    it_selected_documents = it_selected_documents
                                                          IMPORTING et_messages           = et_messages ).
  ENDMETHOD.


  METHOD schedule_background_job.
    TRY.
        "   Generate the unique ID to save the data-stream
        DATA(lv_guid) = mo_guid_generator->generate_guid( ).

        "   Create the DB entry of the data-stream
        INSERT mass_op_strm_tmp FROM @( VALUE #( mandt         = sy-mandt
                                                 guid          = lv_guid
                                                 exl_doc_id    = is_excel_information-excel_document_id
                                                 jobdesc       = is_job_details-text
                                                 media_content = is_excel_information-excel_stream
                                                 stream_type   = is_excel_information-stream_type
                                                 language      = sy-langu
                                                 created_on    = sy-datum
                                                 created_by    = sy-uname ) ).
        IF sy-subrc NE 0.
          "   Return appropriate message
          APPEND VALUE #( type   = 'E'
                          id     = 'EXL_OPS'
                          number = 008 ) TO et_message.
          RETURN.
        ENDIF.

        "   API Call: Schedule the job
        cl_apj_rt_job_scheduling_api=>schedule_job( EXPORTING iv_job_template_name   = is_job_details-template_name
                                                              iv_job_text            = is_job_details-text
                                                              is_start_info          = is_job_scheduling_info-start_info
                                                              is_end_info            = is_job_scheduling_info-end_info
                                                              is_scheduling_info     = is_job_scheduling_info-scheduling_info
                                                              it_job_parameter_value = VALUE #( ( name    = |P_GUID|
                                                                                                  t_value = VALUE #( ( sign   = 'I'
                                                                                                                       option = 'EQ'
                                                                                                                       low    = lv_guid ) ) ) )
                                                              iv_adjust_start_info   = is_job_scheduling_info-adjust_start_info
                                                              iv_username            = sy-uname
                                                              iv_test_mode           = iv_test_mode
                                                    IMPORTING et_message             = et_message
                                                              es_job_details         = es_job_details ).
      CATCH cx_exl_ops cx_uuid_error cm_apj_base INTO DATA(lx_exception). " Error Class for UUID Processing Errors
        APPEND VALUE #( type       = 'E'
                        id         = 'EXL_OPS'
                        number     = 004
                        message_v1 = lx_exception->get_text( ) ) TO et_message.
    ENDTRY.
  ENDMETHOD.


  METHOD upload_excel.
    "   Check value of mandatory parameters
    IF iv_class_name IS INITIAL OR
       iv_business_object IS INITIAL OR
       iv_guid IS INITIAL.
      RETURN.
    ENDIF.
    "   Create instance for the upload handler
    create_handler_instance( iv_class_name ).
    "   Start processing the upload scenario
    mo_upload_handler->process_excel( iv_datastream_guid = iv_guid
                                      iv_business_object = iv_business_object ).
  ENDMETHOD.
ENDCLASS.
