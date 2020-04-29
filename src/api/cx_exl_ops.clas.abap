class CX_EXL_OPS definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF missing_config_data,
        msgid TYPE symsgid VALUE 'EXL_OPS',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MV_EXL_DOC_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF missing_config_data .
  constants:
    begin of MISSING_USER,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_USER .
  constants:
    begin of MISSING_CLASS_CONFIGURATION,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MV_EXL_DOC_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_CLASS_CONFIGURATION .
  constants:
    begin of MISSING_KEY_FIELDS,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MV_USER_NAME',
      attr2 type scx_attrname value 'MV_SHEET_NAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_KEY_FIELDS .
  constants:
    begin of WS_SCHEMA_GEN_FAILED,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of WS_SCHEMA_GEN_FAILED .
  constants:
    begin of MISSING_DATA_STREAM,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MV_STREAM_KEY',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_DATA_STREAM .
  constants:
    begin of FAILED_DB_INSERT,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FAILED_DB_INSERT .
  constants:
    begin of LANGUAGE_ERROR,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of LANGUAGE_ERROR .
  constants:
    begin of NO_SHEETS,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_SHEETS .
  constants:
    begin of WRONG_NO_OF_SHEETS,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of WRONG_NO_OF_SHEETS .
  constants:
    begin of MISSING_SHEET,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'MV_SHEET_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_SHEET .
  constants:
    begin of LANGUAGE_ERR,
      msgid type symsgid value 'EXL_OPS',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of LANGUAGE_ERR .
  data MV_EXL_DOC_ID type EXL_DOC_ID .
  data MV_USER_NAME type SYUNAME .
  data MV_STREAM_KEY type GUID .
  data MV_SHEET_NAME type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_EXL_DOC_ID type EXL_DOC_ID optional
      !MV_USER_NAME type SYUNAME optional
      !MV_STREAM_KEY type GUID optional
      !MV_SHEET_NAME type STRING optional .
*  methods CONSTRUCTOR
*    importing
*      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
*      !PREVIOUS like PREVIOUS optional
*      !MV_EXL_DOC_ID type EXL_DOC_ID optional .
*        !mv_user_name  TYPE syuname OPTIONAL.
protected section.
private section.
ENDCLASS.



CLASS CX_EXL_OPS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_EXL_DOC_ID = MV_EXL_DOC_ID .
me->MV_USER_NAME = MV_USER_NAME .
me->MV_STREAM_KEY = MV_STREAM_KEY .
me->MV_SHEET_NAME = MV_SHEET_NAME .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
