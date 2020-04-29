"! <p class="shorttext synchronized" lang="en">Mass excel download handler API</p>
class CL_EXL_OPS_DOWNLOAD_HANDLER definition
  public
  final
  create private .

public section.

    "! <p class="shorttext synchronized" lang="en">Generate the instance of the class</p>
    "!
    "! @parameter ro_instance | <p class="shorttext synchronized" lang="en">Download Handler instance</p>
  class-methods CREATE
    returning
      value(RO_INSTANCE) type ref to CL_EXL_OPS_DOWNLOAD_HANDLER .
    "! <p class="shorttext synchronized" lang="en">Generate the excel to be downloaded</p>
    "!
    "! @parameter iv_document          | <p class="shorttext synchronized" lang="en">Excel operations Worksheet ID</p>
    "! @parameter it_filter_conditions | <p class="shorttext synchronized" lang="en">Table: Filter conditions passed from UI</p>
    "! @parameter it_inclusion_array   | <p class="shorttext synchronized" lang="en">Table: Inclusion value passed from UI</p>
    "! @parameter it_exclusion_array   | <p class="shorttext synchronized" lang="en">Table: Exclusion value passed from UI</p>
  methods GENERATE_EXCEL
    importing
      !IV_DOCUMENT type EXL_DOC_ID
      !IT_FILTER_CONDITIONS type /IWBEP/T_MGW_SELECT_OPTION optional
      !IT_SELECTED_DOCUMENTS type /IWBEP/T_COD_SELECT_OPTIONS optional
    exporting
      !ET_MESSAGES type BAL_T_MSG
    returning
      value(RV_DATA_STREAM) type XSTRING .
  PROTECTED SECTION.

private section.

  types:
      "! Structure: List of Keys
    BEGIN OF ty_key_list,
        "!  Sequence of field. Preference to high value
        sequence   TYPE i,
        "!  Field name
        field_name TYPE exl_gfn,
      END OF ty_key_list .
  types:
           "!   Table Structure of Key fields with their weightage
    tt_key_list TYPE STANDARD TABLE OF ty_key_list WITH KEY sequence .

      "!  Table storing information about currency/quantity/decimal/date field against each CDS
  data MT_FIELD_NOTATION type CL_EXL_OPS_UTILITIES=>TT_FIELD_NOTATION .
      "!  Table for the configuration details for Excel Operations Object ID
  data MT_CONFIGURATION_DATA type CL_EXL_OPS_UTILITIES=>TT_CONFIGURATION_DATA .
      "!  Table to store all the sheets data with respect to the names and ID for a given excel
  data MT_EXCEL_DATA type CL_EXL_OPS_UTILITIES=>TT_SHEETS_DATA .

    "! <p class="shorttext synchronized" lang="en">Retrieve the data of all the sheets of the excel</p>
    "!
    "! @raising   cx_exl_ops | <p class="shorttext synchronized" lang="en">Exception raised in case of issue in retrieving data</p>
  methods _RETRIEVE_DATA
    importing
      !IV_DOCUMENT type EXL_DOC_ID
      !IT_FILTER_CONDITIONS type /IWBEP/T_MGW_SELECT_OPTION optional
      !IT_SELECTED_DOCUMENTS type /IWBEP/T_COD_SELECT_OPTIONS optional
      !ET_KEYS type TT_KEY_LIST optional
    returning
      value(RT_SHEETS_DATA) type CL_EXL_OPS_UTILITIES=>TT_SHEETS_DATA
    raising
      CX_EXL_OPS .
    "! <p class="shorttext synchronized" lang="en">Apply the user locale information on the raw data</p>
    "!
    "! @parameter iv_sheet_id   | <p class="shorttext synchronized" lang="en">Excel Operations Worksheet ID</p>
    "! @parameter ct_sheet_data | <p class="shorttext synchronized" lang="en">Excel Sheet Raw Data</p>
  methods _APPLY_META_CONFIG
    importing
      !IV_SHEET_ID type EXL_WS_ID
    changing
      !CT_SHEET_DATA type ref to DATA .
    "! <p class="shorttext synchronized" lang="en">Generate the table schema to hold the data</p>
    "!
    "! @parameter iv_sheet_id     | <p class="shorttext synchronized" lang="en">Excel Operations Worksheet ID</p>
    "! @parameter et_table        | <p class="shorttext synchronized" lang="en">Generated Table Schema</p>
    "! @parameter et_sheet_fields | <p class="shorttext synchronized" lang="en">Generated Column List</p>
  methods _GENERATE_DYNAMIC_TABLE
    importing
      !IV_SHEET_ID type EXL_WS_ID
    exporting
      !ET_TABLE type ref to DATA
      !ET_SHEET_FIELDS type CL_MM_SPREADSHEET_HANDLER=>T_COLUMN .
    "! <p class="shorttext synchronized" lang="en">Load the generated table schema with actual data</p>
    "!
    "! @parameter io_sheet_data    | <p class="shorttext synchronized" lang="en"> Excel Sheet data in RAW format</p>
    "! @parameter ct_dynamic_table | <p class="shorttext synchronized" lang="en"> Excel Sheet data in actual format</p>
  methods _LOAD_TABLE_WITH_DATA
    importing
      !IO_SHEET_DATA type ref to DATA
    changing
      !CT_DYNAMIC_TABLE type ref to DATA .
    "! <p class="shorttext synchronized" lang="en">Build the string containing the selection fields of the CDS</p>
    "!
    "! @parameter iv_cds_name      | <p class="shorttext synchronized" lang="en"> CDS name</p>
    "! @parameter rv_select_string | <p class="shorttext synchronized" lang="en"> Select statement string of fields</p>
  methods _BUILD_SELECT_STRING
    importing
      !IV_CDS_NAME type CDS_NAME
    returning
      value(RV_SELECT_STRING) type STRING .
ENDCLASS.



CLASS CL_EXL_OPS_DOWNLOAD_HANDLER IMPLEMENTATION.


  METHOD create.
      ro_instance = NEW cl_exl_ops_download_handler( ).
  ENDMETHOD.


  METHOD generate_excel.
    TRY.
        "   Get Excel Framework configuration data
        mt_configuration_data = cl_exl_ops_utilities=>get_exl_ops_config_details( iv_document_id  = iv_document
                                                                                  iv_language_key = sy-langu ).

        "   Get excel sheet fields meta data
        mt_field_notation = cl_exl_ops_utilities=>get_exl_ops_field_notation( CHANGING ct_exl_ops_config = mt_configuration_data ).

        "   Get the data of the corresponding business object --> IV_DOCUMENT
        mt_excel_data = _retrieve_data( iv_document          = iv_document
                                        it_filter_conditions = it_filter_conditions
                                        it_selected_documents = it_selected_documents ).

        "   Generate Excel Generator Handler
        DATA(lo_document_xslx) = NEW cl_mm_spreadsheet_handler( iv_document_name = |{ iv_document }_FILE|
                                                                iv_xdocument     = cl_mm_spreadsheet_handler=>create_document( it_sheet_names = VALUE #( FOR <fs_sheet_name> IN mt_excel_data ( <fs_sheet_name>-sheet_name ) ) )
                                                                iv_mime_type     = |XLSX| ).

        LOOP AT mt_excel_data ASSIGNING FIELD-SYMBOL(<fs_sheet_data>).
          "   Generate internal table schema to hold the data of the sheet.
          _generate_dynamic_table( EXPORTING iv_sheet_id     = <fs_sheet_data>-sheet_id
                                   IMPORTING et_table        = DATA(lt_dyn_table)
                                             et_sheet_fields = DATA(lt_sheet_fields) ).
          IF lt_dyn_table IS NOT BOUND.
            "   Error Message: Genration of sheet schema table failed.
            APPEND VALUE bal_s_msg( msgty     = 'E'
                                    msgid     = 'EXL_OPS'
                                    msgno     = '007' ) TO et_messages.
            EXIT.
          ENDIF.

          _load_table_with_data( EXPORTING io_sheet_data    = <fs_sheet_data>-sheet_data
                                 CHANGING  ct_dynamic_table = lt_dyn_table ).

          _apply_meta_config( EXPORTING iv_sheet_id   = <fs_sheet_data>-sheet_id                 " Excel Operations Worksheet ID
                              CHANGING  ct_sheet_data = lt_dyn_table ).

          lo_document_xslx->set_worksheet_from_itab( iv_worksheet_name = <fs_sheet_data>-sheet_name
                                                     io_itab           = lt_dyn_table
                                                     it_columns        = lt_sheet_fields ).
          CLEAR: lt_sheet_fields,
                 lt_dyn_table.
        ENDLOOP.

        rv_data_stream = lo_document_xslx->get_document( ).

      CATCH cx_exl_ops INTO DATA(lx_exl_ops). " Exception class for Excel Operations
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
                                 ) TO et_messages.
    ENDTRY.
  ENDMETHOD.


  METHOD _apply_meta_config.

    FIELD-SYMBOLS: <ft_table> TYPE STANDARD TABLE.
    "check if the dynamic table is initial
    IF ct_sheet_data IS INITIAL.
      RETURN.
    ELSE.
      ASSIGN ct_sheet_data->* TO <ft_table>.
    ENDIF.

    "fetch CDS view corresponding to the excel sheet
    DATA(lv_cds_name) = VALUE #( mt_configuration_data[ exl_ws_id = iv_sheet_id ]-cds OPTIONAL ).

    "fetch the list of excel sheet fields whose data is to be formatted
    DATA(lt_field_notation) = VALUE cl_exl_ops_utilities=>tt_field_notation( FOR <fs_field_notation> IN mt_field_notation WHERE ( cds_name = lv_cds_name ) ( <fs_field_notation> ) ).

    DATA(lv_formatter) = VALUE decfloat34( ).
    DATA: lv_amt_dec      TYPE bapicurr_d,
          ls_curr_decimal TYPE bapi1090_1,
          ls_err_message  TYPE bapireturn.
    DATA: lv_integer         TYPE char060,
          lv_decimal         TYPE char060,
          lv_decimal_counter TYPE i.

    LOOP AT lt_field_notation ASSIGNING FIELD-SYMBOL(<fs_column>).
      LOOP AT <ft_table> ASSIGNING FIELD-SYMBOL(<fs_row>).
        "fetch the data present in a single table row cell.
        ASSIGN COMPONENT  <fs_column>-field_name OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_field>).
        IF sy-subrc <> 0 .
          CONTINUE.
        ENDIF.

        CLEAR: lv_formatter, lv_integer, lv_decimal, lv_decimal_counter,lv_amt_dec,ls_curr_decimal,ls_err_message.

        "format the data of type DATS/ CURR/ DEC/ QUAN into user profile format.
        CASE <fs_column>-field_type.
          WHEN 'DATS'.
            IF <fs_field> NE '00000000'.
              CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
                EXPORTING
                  input  = <fs_field>
                IMPORTING
                  output = <fs_field>.
            ELSE.
              CLEAR <fs_field>.
            ENDIF.

          WHEN 'CURR'.

            CONDENSE  <fs_field>.
            ASSIGN COMPONENT  <fs_column>-field_exit OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_unit>).          " Get currency unit data
            IF <fs_unit> IS ASSIGNED .

              CALL FUNCTION 'BAPI_CURRENCY_GETDECIMALS'                                                         " Get the number of decimal digits
                EXPORTING
                  currency          = <fs_unit>                                                                 " Currency Key
                IMPORTING
                  currency_decimals = ls_curr_decimal                                                           " Structure of Currency Key and Decimals
                  return            = ls_err_message.                                                           " Message Structure

              IF ls_err_message IS INITIAL AND ls_curr_decimal IS NOT INITIAL.
                CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
                  EXPORTING
                    currency        = <fs_unit>                                                                 " Currency
                    amount_internal = <fs_field>                                                                " Currency Amount (SAP): Internal Data Format
                  IMPORTING
                    amount_external = lv_amt_dec.                                                               " Currency Amount: External Data Format

                lv_formatter = lv_amt_dec.
                WRITE lv_formatter TO <fs_field> DECIMALS ls_curr_decimal-curdecimals.
                CONTINUE.
              ENDIF.

            ENDIF.
            " If currency unit column not found, then no decimal shift but add system locale
            lv_formatter = <fs_field>.
            REPLACE '-' IN <fs_field> WITH ''.
            SPLIT <fs_field> AT '.' INTO lv_integer lv_decimal.
            lv_decimal_counter = strlen( lv_decimal ).
            WRITE lv_formatter TO <fs_field> DECIMALS lv_decimal_counter.

          WHEN 'QUAN' OR 'DEC'.
            CONDENSE  <fs_field>.
            lv_formatter = <fs_field>.
            REPLACE '-' IN <fs_field> WITH ''.
            SPLIT <fs_field> AT '.' INTO lv_integer lv_decimal.
            lv_decimal_counter = strlen( lv_decimal ).
            WRITE lv_formatter TO <fs_field> DECIMALS lv_decimal_counter.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD _build_select_string.
*    Based on the document ID and CDS name, generate the select clause string for selecting the data from the CDS.
    LOOP AT mt_configuration_data ASSIGNING FIELD-SYMBOL(<fs_configuration>) WHERE cds = iv_cds_name
                                                                               AND is_downloadable = abap_true.
      rv_select_string = COND #( WHEN rv_select_string IS INITIAL THEN |{ <fs_configuration>-exl_gfn }|
                                                                  ELSE |{ rv_select_string }, { <fs_configuration>-exl_gfn }| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD _generate_dynamic_table.

    DATA: lr_table TYPE REF TO data.
    DATA(lt_component) = VALUE cl_abap_structdescr=>component_table( ).
    CLEAR: et_sheet_fields.
    TRY.
        LOOP AT mt_configuration_data ASSIGNING FIELD-SYMBOL(<fs_configuration_data>) WHERE exl_ws_id = iv_sheet_id.
          "   Check if current field is present in MT_FIELD_NOTATION
          READ TABLE mt_field_notation  WITH KEY cds_name = <fs_configuration_data>-cds field_name = <fs_configuration_data>-exl_gfn ASSIGNING FIELD-SYMBOL(<fs_field_notation>).
          IF sy-subrc = 0 .
            "   Hence changing the data type --> STRING to support locale formatting of this column
            <fs_configuration_data>-exl_data_element = |EXL_OPS_DTEL_STRING|.
          ENDIF.

          cl_abap_structdescr=>describe_by_name( EXPORTING  p_name         = <fs_configuration_data>-exl_data_element                  " Type name
                                                 RECEIVING  p_descr_ref    = DATA(lo_description)                                      " Reference to description object
                                                 EXCEPTIONS type_not_found = 1                                                         " Type with name p_name could not be found
                                                            OTHERS         = 2 ).
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.

          "   Forming the components table to generate the dynamic table
          APPEND VALUE #( name         = <fs_configuration_data>-exl_gfn
                          type         = CAST #( lo_description ) ) TO lt_component.

          "   Forming the cosmetic inputs for the excel sheet, i.e., Labels, Order of Columns, etc.
          APPEND VALUE #( id           = sy-tabix
                          name         = <fs_configuration_data>-exl_gfn
                          display_name = <fs_configuration_data>-enduser_label_text
                          is_result    = abap_true
                          type         = CAST #( lo_description ) ) TO et_sheet_fields.
        ENDLOOP.

        IF lines( lt_component ) > 0.
          "   Get the descriptor of the table
          DATA(lr_table_descriptor) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_component ) ).
          "   Transform the data object to the table
          CREATE DATA et_table TYPE HANDLE lr_table_descriptor.
        ENDIF.
      CATCH cx_sy_table_creation ."type_not_found.
    ENDTRY.

  ENDMETHOD.


  METHOD _load_table_with_data.

    FIELD-SYMBOLS: <fs_sheet_data> TYPE STANDARD TABLE,
                   <ft_dyn_table>  TYPE STANDARD TABLE.

    ASSIGN io_sheet_data->* TO <fs_sheet_data>.
    ASSIGN ct_dynamic_table->* TO <ft_dyn_table>.

    <ft_dyn_table> = CORRESPONDING #( <fs_sheet_data> ).

  ENDMETHOD.


  METHOD _retrieve_data.
    DATA: lt_data          TYPE REF TO data,
          lt_keys          TYPE tt_key_list,
          lo_mass_instance TYPE REF TO object.

    "   Extract class name from the configuration
    DATA(lv_class_name) = VALUE #( mt_configuration_data[ exl_doc_id = iv_document ]-get_data_class OPTIONAL ).

    TRY.
        "   Dynamically create the instance of the class extracted above
        CALL METHOD (lv_class_name)=>get_instance
          EXPORTING
            it_filter_conditions  = it_filter_conditions
            it_selected_documents = it_selected_documents
          RECEIVING
            ro_instance          = lo_mass_instance.


        "   Extract class configuration details from main configuration
        DATA(lt_class_configuration) = mt_configuration_data.
        SORT lt_class_configuration BY exl_ws_id.
        DELETE ADJACENT DUPLICATES FROM lt_class_configuration COMPARING exl_ws_id.

        "   Dynamically call the methods of the extracted to fetch the data of the correspodnding worksheet
        LOOP AT lt_class_configuration ASSIGNING FIELD-SYMBOL(<fs_configuration>).

          CALL METHOD lo_mass_instance->(<fs_configuration>-get_data_method)
            EXPORTING
              iv_select_string = _build_select_string( <fs_configuration>-cds )
            IMPORTING
              et_keys          = lt_keys
            RECEIVING
              rt_sheets_data   = lt_data.

          " Store the data retrieved in the data container
          APPEND VALUE cl_exl_ops_utilities=>ty_sheet_data( sheet_id   = <fs_configuration>-exl_ws_id   " Sheet ID: 'WS_1'
                                                            sheet_name = <fs_configuration>-exl_ws_name " Sheet Name: 'Central Contract Header'
                                                            sheet_data = lt_data ) TO rt_sheets_data.
        ENDLOOP.

      CATCH cx_sy_dyn_call_error INTO DATA(lx_dynamic_call_error).
        RAISE EXCEPTION TYPE cx_exl_ops EXPORTING textid = cx_exl_ops=>missing_class_configuration.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
