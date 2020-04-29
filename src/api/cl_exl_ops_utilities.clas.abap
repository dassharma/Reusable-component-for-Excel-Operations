"! <p class="shorttext synchronized" lang="en">Excel Operation Utility class</p>
"! This is a helper class to assist in the functionality of upload and download of excel.
CLASS cl_exl_ops_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      "!  Structure for containing the configuration details from both SCCUI &amp; IMG Activity
      BEGIN OF ty_configuration_data,
        exl_doc_id         TYPE exl_doc_id,
        exl_doc_name       TYPE exl_doc_name,
        exl_ws_id          TYPE exl_ws_id,
        langu              TYPE spras,
        exl_ws_name        TYPE exl_ws_name,
        sequence_number    TYPE i,
        exl_field_name     TYPE exl_field_name,
        exl_gfn            TYPE exl_gfn, "    Should create a new Data Element
        exl_data_element   TYPE rollname,
        enduser_label_text TYPE string,
        is_key             TYPE boolean,
        is_downloadable    TYPE is_downloadable,
        is_mandatory       TYPE ismandatory,
        is_filterable      TYPE is_filterable,
        is_readonly        TYPE is_readonly,
        cds                TYPE cds_name,
        get_data_class     TYPE mass_ops_class_name,
        get_data_method    TYPE method_name, "       Might change
        hierarchy_notation TYPE exl_bo_hierarchy,
      END OF ty_configuration_data .
    TYPES:
      tt_configuration_data TYPE STANDARD TABLE OF ty_configuration_data WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_messages,
        msg_id     TYPE syst_msgid,
        msg_type   TYPE syst_msgty,
        msg_number TYPE syst_msgno,
        msg_value1 TYPE syst_msgv,
        msg_value2 TYPE syst_msgv,
        msg_value3 TYPE syst_msgv,
        msg_value4 TYPE syst_msgv,
      END OF ty_messages .
    TYPES:
      BEGIN OF ty_field_notation,
        cds_name   TYPE string,
        field_name TYPE string,
        field_type TYPE string,
        field_exit TYPE string,
      END OF ty_field_notation .
    TYPES:
      tt_field_notation TYPE STANDARD TABLE OF ty_field_notation WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_sheet_data,
        sheet_id   TYPE exl_ws_id,
        sheet_name TYPE string,
        sheet_data TYPE REF TO data,
      END OF ty_sheet_data .
    TYPES:
      tt_sheets_data        TYPE SORTED TABLE OF ty_sheet_data WITH UNIQUE KEY sheet_id .


    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Extract excel configuration details for a given BO</p>
      "! The purpose is to extract configuration details for a given document ID from both the External SSCUI and the Internal IMG Activity
      "! @parameter iv_document_id    | <p class="shorttext synchronized" lang="en">Excel Operations Document ID</p>
      "! @parameter rt_exl_ops_config | <p class="shorttext synchronized" lang="en">Table: Excel Operation configuration details</p>
      "! @raising   cx_exl_ops        | <p class="shorttext synchronized" lang="en">This exception is raised when configuration is not present</p>
      get_exl_ops_config_details IMPORTING iv_document_id           TYPE exl_doc_id
                                           iv_language_key          TYPE langu
                                 RETURNING VALUE(rt_exl_ops_config) TYPE tt_configuration_data
                                 RAISING   cx_exl_ops,

      "! <p class="shorttext synchronized" lang="en">Get the certain type of fields information</p>
      "! For every CDS the get the decimal/quantity/date/currency fields names, here GFNs
      "! @parameter ct_exl_ops_config         | <p class="shorttext synchronized" lang="en">Table: Excel Operation configuration details</p>
      "! @parameter rt_exl_ops_field_notation | <p class="shorttext synchronized" lang="en">Table: Excel Operation special field details</p>
      get_exl_ops_field_notation CHANGING  ct_exl_ops_config                TYPE tt_configuration_data
                                 RETURNING VALUE(rt_exl_ops_field_notation) TYPE tt_field_notation,

      "! <p class="shorttext synchronized" lang="en">Extract the component detail of the dynamic table</p>
      "!
      "! @parameter it_dynamic_table | <p class="shorttext synchronized" lang="en">Table: Dynamic internal table</p>
      "! @parameter rt_components    | <p class="shorttext synchronized" lang="en">Table: Components of the internal table</p>
      extract_itab_components IMPORTING it_dynamic_table     TYPE REF TO data
                              RETURNING VALUE(rt_components) TYPE cl_abap_structdescr=>component_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS CL_EXL_OPS_UTILITIES IMPLEMENTATION.


  METHOD extract_itab_components.
    "   Get the component details of the structure object
    rt_components = CAST cl_abap_structdescr( CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data_ref( it_dynamic_table ) )->get_table_line_type( ) )->get_components( ).
  ENDMETHOD.


  METHOD get_exl_ops_config_details.
    SELECT document~exl_doc_id,
           document~exl_doc_name,
           worksheet~exl_ws_id,
           sheet_txt~langu,
           sheet_txt~exl_ws_name,
           column~exl_field_name,
           column~exl_gfn,
           column~sequence_number,
           column~is_key,
           column~is_downloadable,
           column~is_mandatory,
           column~is_filterable,
           column~is_readonly,
           config~cds,
           config~get_data_class,
           config~get_data_method,
           config~hierarchy_notation
       INTO CORRESPONDING FIELDS OF TABLE @rt_exl_ops_config
       FROM exl_ops_bo             AS document
       INNER JOIN exl_ops_ws       AS worksheet ON worksheet~exl_doc_id       = document~exl_doc_id
       INNER JOIN exl_ops_ws_txt   AS sheet_txt ON sheet_txt~exl_doc_id       = worksheet~exl_doc_id AND sheet_txt~exl_ws_id = worksheet~exl_ws_id
       INNER JOIN exl_ops_ws_col   AS column    ON column~exl_doc_id          = worksheet~exl_doc_id AND column~exl_ws_id = worksheet~exl_ws_id
       INNER JOIN exl_ops_appl_cfg AS config    ON config~exl_doc_id          = worksheet~exl_doc_id AND config~exl_ws_id = worksheet~exl_ws_id
       WHERE document~exl_doc_id = @iv_document_id AND sheet_txt~langu        = @iv_language_key
                                                   AND column~is_downloadable = @abap_true.
    IF sy-subrc NE 0.
      "   Raise appropriate error message
      RAISE EXCEPTION TYPE cx_exl_ops
        EXPORTING
          textid        = cx_exl_ops=>missing_config_data
          mv_exl_doc_id = iv_document_id.
    ELSE.
      SORT rt_exl_ops_config BY exl_ws_id sequence_number exl_gfn.
    ENDIF.
  ENDMETHOD.


  METHOD get_exl_ops_field_notation.
    "   Get the annotation details of all the CDS participating in the process.
    cl_dd_ddl_annotation_service=>get_annos_mass( EXPORTING entity_variants = VALUE #( FOR <fs_configuration> IN ct_exl_ops_config ( entityname = <fs_configuration>-cds ) )
                                                  IMPORTING entity_annos    = DATA(lt_entity_annotation)
                                                            element_annos   = DATA(lt_element_annotation) ).

    "   Filter out the SQL View Name
    DELETE lt_entity_annotation WHERE annoname NE 'ABAPCATALOG.SQLVIEWNAME'.
    "   Filter out the End User Label Texts
    DELETE lt_element_annotation WHERE annoname NE 'ENDUSERTEXT.LABEL' AND annoname NE 'SEMANTICS.AMOUNT.CURRENCYCODE'.

    "   Get the data type information for all the views participating.
    LOOP AT lt_entity_annotation ASSIGNING FIELD-SYMBOL(<fs_annotation>).
      "   Extracting the SQL View Name
      REPLACE ALL OCCURRENCES OF `'` IN <fs_annotation>-value WITH ``.

      "   Get the technical information of the fields of the CDS
      DATA(lt_field_information) = VALUE  dfies_tab( ).
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = CONV ddobjname( <fs_annotation>-value )
        TABLES
          dfies_tab      = lt_field_information
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      "   Fill the data element & End User Label Text for each field
      LOOP AT ct_exl_ops_config ASSIGNING FIELD-SYMBOL(<fs_configuration_data>) WHERE cds = <fs_annotation>-entityname.
        "   Fill data element
        READ TABLE lt_field_information ASSIGNING FIELD-SYMBOL(<fs_field_information>) WITH KEY fieldname = <fs_configuration_data>-exl_gfn.
        IF sy-subrc EQ 0.
          <fs_configuration_data>-exl_data_element = <fs_field_information>-rollname.       "   Data Element
          <fs_configuration_data>-enduser_label_text = COND #( WHEN <fs_field_information>-scrtext_l IS NOT INITIAL THEN <fs_field_information>-scrtext_l
                                                               WHEN <fs_field_information>-scrtext_m IS NOT INITIAL THEN <fs_field_information>-scrtext_m
                                                               WHEN <fs_field_information>-scrtext_s IS NOT INITIAL THEN <fs_field_information>-scrtext_s ).    "   Field Label
          "     Filter out Date, Currency and Quantity type fields --> MT_FIELD_NOTATION
          IF <fs_field_information>-datatype EQ 'DATS' OR
             <fs_field_information>-datatype EQ 'CURR' OR
             <fs_field_information>-datatype EQ 'QUAN' OR
             <fs_field_information>-datatype EQ 'NUMC' OR
             <fs_field_information>-datatype EQ 'DEC'.
            DATA(lv_currency_field) = to_upper( VALUE #( lt_element_annotation[ entityname  = <fs_configuration_data>-cds
                                                                         elementname = <fs_configuration_data>-exl_gfn
                                                                         annoname    = 'SEMANTICS.AMOUNT.CURRENCYCODE' ]-value OPTIONAL ) ).
          REPLACE ALL OCCURRENCES OF `'` IN lv_currency_field WITH ``.

            APPEND VALUE #( cds_name   = <fs_annotation>-entityname
                            field_name = <fs_field_information>-fieldname
                            field_type = <fs_field_information>-datatype
                            field_exit = lv_currency_field ) TO rt_exl_ops_field_notation.
          ENDIF.
        ENDIF.

        "   Fill end user label text
        READ TABLE lt_element_annotation ASSIGNING FIELD-SYMBOL(<fs_element_annotation>) WITH KEY entityname  = <fs_configuration_data>-cds
                                                                                                  elementname = <fs_configuration_data>-exl_gfn
                                                                                                  annoname    = 'ENDUSERTEXT.LABEL'.
        IF sy-subrc EQ 0.
          REPLACE ALL OCCURRENCES OF `'` IN <fs_element_annotation>-value WITH ``.
          "   Replace Field Label with End User Label text if one exists
          <fs_configuration_data>-enduser_label_text = COND #( WHEN <fs_element_annotation>-value IS NOT INITIAL THEN <fs_element_annotation>-value ELSE <fs_configuration_data>-enduser_label_text ).
        ENDIF.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
