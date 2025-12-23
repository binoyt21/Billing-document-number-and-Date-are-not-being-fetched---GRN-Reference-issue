Requirement 1: Delivery number to be passed as reference number.

Changes to be done in function Module Z_SCM_SHIPMENT_DETAIL



Comment below code at line 309 to get delivery number (VTTP- VBELN) as GRN reference number.

Code to be commented “IF lw_zlog_exec_var IS NOT INITIAL.” and its corresponding ENDIF. The statements within the IF ENDIF is required and it remains as-is.

Requirement 2: Delivery number to be passed as invoice number.

Changes to be done in existing function Module Z_SCM_SHIPMENT_DETAIL

At line 431 e_invoice  to be captured from lw_zptc_lrpo- VBELN_D instead of lw_zptc_lrpo-exnum.