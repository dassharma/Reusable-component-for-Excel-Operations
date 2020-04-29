*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS cl_exl_ops_guid_generator DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES if_exl_ops_job_scheduler.

    CLASS-METHODS:
      create RETURNING VALUE(ro_job_scheduler) TYPE REF TO if_exl_ops_job_scheduler.
ENDCLASS.

CLASS cl_exl_ops_guid_generator IMPLEMENTATION.
  METHOD create.
    ro_job_scheduler = NEW cl_exl_ops_guid_generator( ).
  ENDMETHOD.

  METHOD if_exl_ops_job_scheduler~generate_guid.
    TRY.
        "   Fetch generated SYSTEM GUID. If system GUID not available, random GUID is returned.
        rv_uuid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lx_error). " Error Class for UUID Processing Errors
        RAISE EXCEPTION TYPE cx_exl_ops EXPORTING previous = lx_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
