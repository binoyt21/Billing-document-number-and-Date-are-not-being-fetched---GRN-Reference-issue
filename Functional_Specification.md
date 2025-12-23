# Functional Specification

## Document Information

| Field | Value |
|-------|-------|
| **Document Type** | Functional Specification |
| **Document ID** | FS-RD2-211-001 |
| **Project** | RD2-211 ABAP Workspace |
| **Module** | SCM - Shipment Management |
| **Function Module** | Z_SCM_SHIPMENT_DETAIL |
| **Creation Date** | December 23, 2025 |
| **Version** | 1.0 |
| **Status** | Draft |

---

## 1. Overview

### 1.1 Purpose
This document describes the functional requirements for modifications to the function module `Z_SCM_SHIPMENT_DETAIL` to correctly pass delivery number as GRN reference number and invoice number in the shipment details retrieval process.

### 1.2 Background
Currently, the function module has logic that conditionally determines the GRN reference number and invoice number. There are issues where:
- The delivery number is not being properly passed as the GRN reference number due to an unnecessary conditional check
- The invoice number is being retrieved from the wrong field (external number instead of delivery number)

### 1.3 Business Impact
- **Current Issue**: Incorrect GRN reference numbers and invoice numbers are causing discrepancies in shipment tracking and goods receipt processing
- **Business Benefit**: Accurate shipment reference data will improve goods receipt processing, invoice matching, and supply chain visibility
- **Affected Processes**: Inbound shipment processing, goods receipt, invoice verification

---

## 2. Scope

### 2.1 In Scope
- Modification to GRN reference number logic in function module `Z_SCM_SHIPMENT_DETAIL`
- Modification to invoice number field mapping in function module `Z_SCM_SHIPMENT_DETAIL`

### 2.2 Out of Scope
- Changes to database tables
- Changes to calling programs
- Changes to user interface
- New functionality additions

---

## 3. Functional Requirements

### 3.1 Requirement 1: Delivery Number as GRN Reference Number

**Requirement ID**: REQ-001

**Description**: The delivery number (VTTP-VBELN) should always be passed as the GRN (Goods Receipt Note) reference number without any conditional logic that might prevent this assignment.

**Current Behavior**:
- The delivery number assignment to `lw_shipment_details-ibd_no` is wrapped in a conditional check: `IF lw_zlog_exec_var IS NOT INITIAL`
- This condition prevents the delivery number from being assigned when `lw_zlog_exec_var` is initial/empty
- The reference number is not populated correctly in certain scenarios

**Expected Behavior**:
- The delivery number from VTTP-VBELN should be directly assigned to `lw_shipment_details-ibd_no` (GRN reference)
- This assignment should occur unconditionally for all shipments
- The subsequent sales and distribution document logic (from VBFA table) should override this value when applicable

**Business Rule**:
- Primary source: VTTP-VBELN (Delivery number from shipment item)
- Override condition: If subsequent document exists in VBFA (document flow), use VBFA-VBELN instead

**Affected Code Section**: Lines 309-311 in function module

### 3.2 Requirement 2: Delivery Number as Invoice Number

**Requirement ID**: REQ-002

**Description**: The invoice number export parameter should be populated with the delivery number (VBELN_D) instead of the external number (EXNUM) from the ZPTC_LRPO table.

**Current Behavior**:
- Export parameter `e_invoice` is populated from `lw_zptc_lrpo-exnum` (external invoice number)
- This external number may not match the actual delivery document number
- Invoice matching and verification processes receive incorrect document references

**Expected Behavior**:
- Export parameter `e_invoice` should be populated from `lw_zptc_lrpo-vbeln_d` (delivery number)
- This ensures consistency with the delivery document used in shipment processing
- Invoice verification processes will have accurate document references

**Business Rule**:
- Source field: ZPTC_LRPO-VBELN_D (Delivery number)
- Target field: E_INVOICE (Export parameter for invoice number)

**Affected Code Section**: Line 431 in function module

---

## 4. Data Flow

### 4.1 Current Data Flow - GRN Reference

```
Shipment Number (I_SHNUMBER)
    ↓
Query VTTP table
    ↓
Get Delivery Number (VTTP-VBELN)
    ↓
[Conditional Check: IF lw_zlog_exec_var IS NOT INITIAL] ← PROBLEM
    ↓
Assign to lw_shipment_details-ibd_no
```

### 4.2 Proposed Data Flow - GRN Reference

```
Shipment Number (I_SHNUMBER)
    ↓
Query VTTP table
    ↓
Get Delivery Number (VTTP-VBELN)
    ↓
Assign to lw_shipment_details-ibd_no (UNCONDITIONAL)
    ↓
Check VBFA for subsequent document
    ↓
If exists: Override with VBFA-VBELN
```

### 4.3 Current Data Flow - Invoice Number

```
Query ZPTC_LRPO table
    ↓
Read lw_zptc_lrpo-exnum (External Invoice Number) ← PROBLEM
    ↓
Assign to e_invoice export parameter
```

### 4.4 Proposed Data Flow - Invoice Number

```
Query ZPTC_LRPO table
    ↓
Read lw_zptc_lrpo-vbeln_d (Delivery Number)
    ↓
Assign to e_invoice export parameter
```

---

## 5. Business Process Impact

### 5.1 Affected Business Processes
- **Inbound Shipment Processing**: Accurate shipment reference data
- **Goods Receipt Processing**: Correct GRN reference for matching
- **Invoice Verification**: Proper invoice-to-delivery matching
- **Supply Chain Reporting**: Accurate shipment tracking data

### 5.2 User Impact
- **Warehouse Users**: Will see correct GRN reference numbers in goods receipt transactions
- **Finance Users**: Will have accurate invoice-to-delivery references for verification
- **Supply Chain Analysts**: Will have consistent shipment tracking data in reports

---

## 6. Acceptance Criteria

### 6.1 Requirement 1 Acceptance Criteria

**Test Scenario 1: Standard Shipment Processing**
- **Given**: A shipment exists with delivery number in VTTP table
- **When**: Function module is called with shipment number
- **Then**: The delivery number should be populated in the GRN reference field (ibd_no)

**Test Scenario 2: Shipment with Subsequent Document**
- **Given**: A shipment has a subsequent sales document in VBFA table
- **When**: Function module is called with shipment number
- **Then**: The subsequent document number from VBFA should be populated in the GRN reference field

### 6.2 Requirement 2 Acceptance Criteria

**Test Scenario 3: Invoice Number Retrieval**
- **Given**: Shipment data exists in ZPTC_LRPO table with delivery number
- **When**: Function module is called and retrieves data from ZPTC_LRPO
- **Then**: The e_invoice export parameter should contain the delivery number (vbeln_d) not the external number (exnum)

---

## 7. Dependencies

### 7.1 System Dependencies
- SAP ECC 6.0 / NetWeaver 7.31
- Function module: Z_SCM_SHIPMENT_DETAIL

### 7.2 Data Dependencies
- Table: VTTP (Shipment item data)
- Table: VBFA (Document flow)
- Table: ZPTC_LRPO (Custom logistics table)
- Table: ZLOG_EXEC_VAR (Configuration table)

### 7.3 Process Dependencies
- Shipment creation and confirmation processes
- Goods receipt processing
- Invoice verification processes

---

## 8. Risks and Assumptions

### 8.1 Assumptions
- The delivery number (VBELN_D) in ZPTC_LRPO table is always populated correctly
- The VTTP table contains valid delivery numbers for all shipments
- Downstream processes expecting invoice data can handle delivery numbers

### 8.2 Risks
- **Low Risk**: Existing integrations expecting external invoice number may need adjustment
- **Mitigation**: Conduct thorough testing with all calling programs

---

## 9. Testing Requirements

### 9.1 Unit Testing
- Test function module with various shipment scenarios
- Verify correct field mappings
- Validate data consistency

### 9.2 Integration Testing
- Test with calling programs and transactions
- Verify goods receipt processing with new reference numbers
- Validate invoice verification processes

### 9.3 Regression Testing
- Test existing functionality not affected by changes
- Verify no side effects on other shipment types
- Validate configuration-driven scenarios

---

## 10. Sign-Off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| **Business Owner** | | | |
| **Functional Analyst** | | | |
| **Technical Lead** | | | |
| **QA Lead** | | | |

---

## 11. Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | December 23, 2025 | System | Initial document creation |

---

## 12. References

- Function Module: Z_SCM_SHIPMENT_DETAIL
- Original Requirements Document: FS.md
- ABAP Coding Guidelines: ABAP coding guidelines folder
- SAP Tables: VTTP, VBFA, ZPTC_LRPO, ZLOG_EXEC_VAR

