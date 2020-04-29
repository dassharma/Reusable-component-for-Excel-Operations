*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS cl_dynamic_table_generator DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      generate_dynamic_table IMPORTING iv_sheet_id       TYPE exl_ws_id
                                       it_configuration_data TYPE cl_exl_ops_utilities=>tt_configuration_data
                             RETURNING VALUE(rt_data)    TYPE REF TO data
                             RAISING cx_exl_ops.
ENDCLASS.

CLASS cl_dynamic_table_generator IMPLEMENTATION.

  METHOD generate_dynamic_table.
    TRY.
        DATA(lt_component) = VALUE cl_abap_structdescr=>component_table( ).
        LOOP AT it_configuration_data ASSIGNING FIELD-SYMBOL(<fs_configuration_data>) WHERE exl_ws_id = iv_sheet_id.
          "   Forming the components table to generate the dynamic table
          APPEND VALUE #( name         = <fs_configuration_data>-exl_gfn
                          type         = CAST #( cl_abap_structdescr=>describe_by_name( <fs_configuration_data>-exl_data_element ) ) ) TO lt_component.
        ENDLOOP.
        IF lines( lt_component ) > 0.
          "   Get the descriptor of the table
          DATA(lr_table_descriptor) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_component ) ).
          "   Transform the data object to the table
          CREATE DATA rt_data TYPE HANDLE lr_table_descriptor.
        ENDIF.
      CATCH cx_sy_table_creation INTO DATA(lx_exeception) ."type_not_found.
        RAISE EXCEPTION TYPE cx_exl_ops
          EXPORTING
            previous = lx_exeception.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
