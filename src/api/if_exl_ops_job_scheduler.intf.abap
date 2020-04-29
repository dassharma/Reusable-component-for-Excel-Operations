interface IF_EXL_OPS_JOB_SCHEDULER
  public .


  class-methods GENERATE_GUID
    returning
      value(RV_UUID) type SYSUUID_X16
    raising
      CX_UUID_ERROR
      CX_EXL_OPS .
endinterface.
