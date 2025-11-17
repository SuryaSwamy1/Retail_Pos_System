# RETAIL POS SYSTEM - DOCUMENTATION PART 7
## CASH DRAWER MANAGEMENT AND REPORTING SYSTEM

---

## TABLE OF CONTENTS

1. [Cash Drawer Management](#cash-drawer-management)
2. [Reporting System](#reporting-system)
3. [Report Descriptions](#report-descriptions)

---

## CASH DRAWER MANAGEMENT

The cash drawer management system provides professional cash handling with opening balances, reconciliation, denomination tracking, and variance alerts.

### Cash Drawer Architecture

**Drawer States:**
- **OPEN** - Active drawer currently in use
- **CLOSED** - Completed drawer awaiting deposit
- **RECONCILED** - Variance reviewed and approved
- **DEPOSITED** - Cash deposited to bank

**Drawer ID Format:**
- **Pattern:** `STORE-YYYYMMDD-NNN`
- **Example:** `S001-20251006-001`
- Sequential numbering per store per day

**Variance Thresholds:**
- **Alert if:** Variance > $10.00 OR Variance > 1% of expected
- **Email notification** sent to store manager
- **Require approval** for variances > $50

---

### 1. CASH.DRAWER.OPEN - Open Cash Drawer

**Location:** `BP/CASH.DRAWER.OPEN`
**Lines of Code:** ~800 lines
**Purpose:** Open cash drawer for cashier shift with opening balance

#### Function Signature

```unibasic
SUBROUTINE CASH.DRAWER.OPEN(CASHIER.ID, OPENING.AMT, DRAWER.ID, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| CASHIER.ID | String | IN | Employee ID of cashier |
| OPENING.AMT | Decimal | IN | Opening cash balance |
| DRAWER.ID | String | OUT | Generated drawer ID |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Cashier Validation**

```unibasic
* Verify cashier exists and is authorized
CALL DB.CONNECT('OPEN.FILE', 'EMPLOYEES', F.EMPLOYEES, ERROR)

READU EMP.REC FROM F.EMPLOYEES, CASHIER.ID ELSE
   ERROR.CODE = ERR.NOT.FOUND
   ERROR.MSG = 'Cashier ID ':CASHIER.ID:' not found'
   RETURN
END

* Verify employee is active
IF EMP.REC<STATUS> NE 'ACTIVE' THEN
   ERROR.CODE = ERR.INVALID.STATUS
   ERROR.MSG = 'Employee status is ':EMP.REC<STATUS>:' - cannot open drawer'
   RELEASE F.EMPLOYEES, CASHIER.ID
   RETURN
END

* Verify cashier is authorized for POS
AUTH.FUNCTIONS = EMP.REC<AUTH.FUNCTIONS>
IF NOT(AUTH.FUNCTIONS MATCHES 'POS' OR AUTH.FUNCTIONS MATCHES 'CASHIER') THEN
   ERROR.CODE = ERR.INSUFFICIENT.AUTH
   ERROR.MSG = 'Employee not authorized for cashier functions'
   RELEASE F.EMPLOYEES, CASHIER.ID
   RETURN
END

RELEASE F.EMPLOYEES, CASHIER.ID
```

**2. Check for Existing Open Drawer**

```unibasic
* Verify cashier doesn't already have an open drawer
* One drawer per cashier at a time
CALL DB.CONNECT('OPEN.FILE', 'CASH.DRAWERS', F.DRAWERS, ERROR)

SELECT.CMD = 'SELECT CASH.DRAWERS WITH CASHIER.ID = "':CASHIER.ID:'"'
SELECT.CMD := ' AND STATUS = "OPEN"'

EXECUTE SELECT.CMD

IF @SELECTED > 0 THEN
   READNEXT EXISTING.DRAWER
   ERROR.CODE = ERR.DUPLICATE
   ERROR.MSG = 'Cashier already has open drawer: ':EXISTING.DRAWER
   ERROR.MSG := '. Must close existing drawer first.'
   RETURN
END
```

**3. Validate Opening Amount**

```unibasic
* Validate opening amount
MIN.OPENING = 50.00
MAX.OPENING = 500.00

IF OPENING.AMT < MIN.OPENING THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Minimum opening amount is $':MIN.OPENING
   RETURN
END

IF OPENING.AMT > MAX.OPENING THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Maximum opening amount is $':MAX.OPENING
   RETURN
END

* Round to 2 decimal places
OPENING.AMT = INT(OPENING.AMT * 100) / 100
```

**4. Generate Drawer ID**

```unibasic
* Generate unique drawer ID
* Format: STORE-YYYYMMDD-NNN

STORE.ID = SYSTEM<CURRENT.STORE>
DATE.STR = OCONV(DATE(), 'D4YMD')  ;* YYYYMMDD

* Find next sequence number for this store today
SELECT.CMD = 'SELECT CASH.DRAWERS WITH @ID LIKE "':STORE.ID:'-':DATE.STR:'-..."'
EXECUTE SELECT.CMD CAPTURING OUTPUT

COUNT = OUTPUT<2>  ;* Number of drawers today for this store
NEXT.SEQ = COUNT + 1

DRAWER.ID = STORE.ID:'-':DATE.STR:'-':FMT(NEXT.SEQ, '3"0"R')

* Example: S001-20251006-001
```

**5. Initialize Drawer Record**

```unibasic
* Create cash drawer record
DRAWER.REC = ''
DRAWER.REC<1> = DRAWER.ID                ;* Drawer ID
DRAWER.REC<2> = CASHIER.ID               ;* Cashier employee ID
DRAWER.REC<3> = STORE.ID                 ;* Store ID
DRAWER.REC<4> = DATE()                   ;* Business date
DRAWER.REC<5> = TIME()                   ;* Open time
DRAWER.REC<6> = ''                       ;* Close time (none yet)
DRAWER.REC<7> = OPENING.AMT              ;* Opening balance
DRAWER.REC<8> = 0                        ;* Cash sales (accumulated)
DRAWER.REC<9> = 0                        ;* Cash refunds (accumulated)
DRAWER.REC<10> = 0                       ;* Expected cash (calculated)
DRAWER.REC<11> = 0                       ;* Actual cash (at close)
DRAWER.REC<12> = 0                       ;* Variance (actual - expected)
DRAWER.REC<13> = 'OPEN'                  ;* Status
DRAWER.REC<14> = USER.ID                 ;* Opened by
DRAWER.REC<15> = ''                      ;* Closed by (none yet)

* Initialize denomination breakdown (multivalued)
DRAWER.REC<16> = ''                      ;* Denomination types
DRAWER.REC<17> = ''                      ;* Denomination counts
DRAWER.REC<18> = ''                      ;* Denomination totals

* Transaction counters
DRAWER.REC<19> = 0                       ;* Number of cash sales
DRAWER.REC<20> = 0                       ;* Number of cash refunds
DRAWER.REC<21> = 0                       ;* Number of no-sales (drawer opens)
```

**6. Record Opening Denominations (Optional)**

```unibasic
* Optional: Record opening cash denomination breakdown
* This provides additional audit control

PRINT 'Record opening cash denominations? (Y/N): '
INPUT RECORD.DENOMS

IF RECORD.DENOMS EQ 'Y' THEN
   GOSUB GET.DENOMINATION.BREAKDOWN
END
```

**GET.DENOMINATION.BREAKDOWN Subroutine:**

```unibasic
GET.DENOMINATION.BREAKDOWN:
   * Standard US currency denominations
   DENOM.TYPES = '100':VM:'50':VM:'20':VM:'10':VM:'5':VM:'1'
   DENOM.TYPES := VM:'0.25':VM:'0.10':VM:'0.05':VM:'0.01'
   DENOM.COUNT = DCOUNT(DENOM.TYPES, VM)

   DENOM.NAMES = '$100':VM:'$50':VM:'$20':VM:'$10':VM:'$5':VM:'$1'
   DENOM.NAMES := VM:'Quarter':VM:'Dime':VM:'Nickel':VM:'Penny'

   TOTAL.COUNTED = 0

   PRINT ''
   PRINT 'Enter count for each denomination:'
   PRINT '=================================='

   FOR I = 1 TO DENOM.COUNT
      DENOM.VALUE = DENOM.TYPES<1,I>
      DENOM.NAME = DENOM.NAMES<1,I>

      PRINT DENOM.NAME:': '
      INPUT QTY

      IF QTY EQ '' THEN QTY = 0
      QTY = QTY + 0  ;* Convert to numeric

      DENOM.TOTAL = QTY * DENOM.VALUE
      TOTAL.COUNTED = TOTAL.COUNTED + DENOM.TOTAL

      * Store in drawer record
      DRAWER.REC<DENOM.TYPES, I> = DENOM.VALUE
      DRAWER.REC<DENOM.COUNTS, I> = QTY
      DRAWER.REC<DENOM.TOTALS, I> = DENOM.TOTAL
   NEXT I

   PRINT ''
   PRINT 'Total Counted: $':FMT(TOTAL.COUNTED, '10.2R')
   PRINT 'Expected: $':FMT(OPENING.AMT, '10.2R')

   DIFF = TOTAL.COUNTED - OPENING.AMT
   IF ABS(DIFF) > 0.01 THEN
      PRINT 'WARNING: Difference of $':FMT(DIFF, '10.2R')
      PRINT 'Recount? (Y/N): '
      INPUT RECOUNT
      IF RECOUNT EQ 'Y' THEN
         GOTO GET.DENOMINATION.BREAKDOWN
      END
   END

   RETURN
```

**7. Save Drawer and Update Employee**

```unibasic
* Write drawer record
WRITE DRAWER.REC ON F.DRAWERS, DRAWER.ID

* Update employee record with current drawer
READU EMP.REC FROM F.EMPLOYEES, CASHIER.ID THEN
   EMP.REC<CURRENT.DRAWER> = DRAWER.ID
   EMP.REC<DRAWER.OPEN.TIME> = TIME()
   WRITE EMP.REC ON F.EMPLOYEES, CASHIER.ID
END ELSE
   RELEASE F.EMPLOYEES, CASHIER.ID
END
```

**8. Print Opening Receipt**

```unibasic
* Print cash drawer opening slip
PRINT ''
PRINT '========================================='
PRINT '      CASH DRAWER OPENING'
PRINT '========================================='
PRINT ''
PRINT 'Drawer ID: ':DRAWER.ID
PRINT 'Cashier: ':CASHIER.ID
PRINT 'Store: ':STORE.ID
PRINT 'Date: ':OCONV(DATE(), 'D2/MDY [A3]')
PRINT 'Time: ':OCONV(TIME(), 'MTS')
PRINT ''
PRINT 'Opening Balance: $':FMT(OPENING.AMT, '10.2R')
PRINT ''
PRINT 'Status: OPEN'
PRINT '========================================='

* Audit trail
AUDIT.MSG = 'Cash drawer ':DRAWER.ID:' opened by ':CASHIER.ID
AUDIT.MSG := ' with opening balance $':OPENING.AMT

CALL UTILS.COMMON('WRITE.AUDIT', 'CASH.DRAWER.OPEN', DRAWER.ID, AUDIT.MSG, DUMMY, DUMMY)

ERROR.CODE = 0
ERROR.MSG = 'Cash drawer opened successfully'
```

#### Usage Example

```unibasic
* Cashier opening drawer at start of shift
CASHIER.ID = 'E00001234'
OPENING.AMT = 100.00

CALL CASH.DRAWER.OPEN(CASHIER.ID, OPENING.AMT, DRAWER.ID, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   PRINT 'Drawer opened: ':DRAWER.ID
   PRINT 'Opening balance: $':OPENING.AMT
   PRINT 'Ready for transactions'
END ELSE
   PRINT 'Error: ':ERROR.MSG
END
```

---

### 2. CASH.DRAWER.CLOSE - Close and Reconcile Drawer

**Location:** `BP/CASH.DRAWER.CLOSE`
**Lines of Code:** ~1,000 lines
**Purpose:** Close cash drawer, count cash, calculate variance, and generate reconciliation report

#### Function Signature

```unibasic
SUBROUTINE CASH.DRAWER.CLOSE(DRAWER.ID, ACTUAL.CASH, DENOMINATIONS, RECONCILIATION, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| DRAWER.ID | String | IN | Drawer ID to close |
| ACTUAL.CASH | Decimal | IN | Total cash counted |
| DENOMINATIONS | Dynamic Array | IN | Denomination breakdown |
| RECONCILIATION | Dynamic Array | OUT | Complete reconciliation report |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Drawer Validation**

```unibasic
* Read and lock drawer record
CALL DB.CONNECT('OPEN.FILE', 'CASH.DRAWERS', F.DRAWERS, ERROR)

READU DRAWER.REC FROM F.DRAWERS, DRAWER.ID ELSE
   ERROR.CODE = ERR.NOT.FOUND
   ERROR.MSG = 'Drawer ':DRAWER.ID:' not found'
   RETURN
END

* Verify drawer is open
IF DRAWER.REC<STATUS> NE 'OPEN' THEN
   ERROR.CODE = ERR.INVALID.STATUS
   ERROR.MSG = 'Drawer status is ':DRAWER.REC<STATUS>:' - already closed'
   RELEASE F.DRAWERS, DRAWER.ID
   RETURN
END
```

**2. Calculate Expected Cash**

```unibasic
* Calculate expected cash amount
OPENING.AMT = DRAWER.REC<OPENING.BALANCE>
CASH.SALES = DRAWER.REC<CASH.SALES>
CASH.REFUNDS = DRAWER.REC<CASH.REFUNDS>

EXPECTED.CASH = OPENING.AMT + CASH.SALES - CASH.REFUNDS
EXPECTED.CASH = INT(EXPECTED.CASH * 100) / 100

DRAWER.REC<EXPECTED.CASH> = EXPECTED.CASH

PRINT ''
PRINT 'Expected Cash Calculation:'
PRINT '=========================='
PRINT 'Opening Balance: $':FMT(OPENING.AMT, '10.2R')
PRINT 'Cash Sales: $':FMT(CASH.SALES, '10.2R')
PRINT 'Cash Refunds: ($':FMT(CASH.REFUNDS, '10.2R'):')'
PRINT '------------------------'
PRINT 'Expected Total: $':FMT(EXPECTED.CASH, '10.2R')
PRINT ''
```

**3. Cash Count and Denomination Validation**

```unibasic
* Validate actual cash amount
IF ACTUAL.CASH < 0 THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Actual cash cannot be negative'
   RELEASE F.DRAWERS, DRAWER.ID
   RETURN
END

ACTUAL.CASH = INT(ACTUAL.CASH * 100) / 100

* If denominations provided, verify they add up to actual cash
IF DENOMINATIONS NE '' THEN
   DENOM.TOTAL = 0

   DENOM.COUNT = DCOUNT(DENOMINATIONS<DENOM.VALUES>, VM)
   FOR I = 1 TO DENOM.COUNT
      DENOM.VALUE = DENOMINATIONS<DENOM.VALUES, I>
      DENOM.QTY = DENOMINATIONS<DENOM.QTYS, I>
      DENOM.AMT = DENOM.VALUE * DENOM.QTY
      DENOM.TOTAL = DENOM.TOTAL + DENOM.AMT
   NEXT I

   DENOM.TOTAL = INT(DENOM.TOTAL * 100) / 100

   * Verify denomination total matches actual cash
   IF ABS(DENOM.TOTAL - ACTUAL.CASH) > 0.01 THEN
      ERROR.CODE = ERR.AMOUNT.MISMATCH
      ERROR.MSG = 'Denomination total ($':DENOM.TOTAL:') does not match actual cash ($':ACTUAL.CASH:')'
      RELEASE F.DRAWERS, DRAWER.ID
      RETURN
   END

   * Store denominations in drawer record
   DRAWER.REC<CLOSE.DENOM.VALUES> = DENOMINATIONS<DENOM.VALUES>
   DRAWER.REC<CLOSE.DENOM.QTYS> = DENOMINATIONS<DENOM.QTYS>
   DRAWER.REC<CLOSE.DENOM.TOTALS> = DENOMINATIONS<DENOM.TOTALS>
END
```

**4. Calculate Variance**

```unibasic
* Calculate variance
VARIANCE = ACTUAL.CASH - EXPECTED.CASH
VARIANCE = INT(VARIANCE * 100) / 100

* Calculate variance percentage
IF EXPECTED.CASH > 0 THEN
   VARIANCE.PCT = (VARIANCE / EXPECTED.CASH) * 100
   VARIANCE.PCT = INT(VARIANCE.PCT * 100) / 100
END ELSE
   VARIANCE.PCT = 0
END

DRAWER.REC<ACTUAL.CASH> = ACTUAL.CASH
DRAWER.REC<VARIANCE> = VARIANCE

* Determine variance type
BEGIN CASE
   CASE VARIANCE > 0
      VARIANCE.TYPE = 'OVERAGE'
   CASE VARIANCE < 0
      VARIANCE.TYPE = 'SHORTAGE'
   CASE 1
      VARIANCE.TYPE = 'BALANCED'
END CASE

PRINT 'Actual Cash: $':FMT(ACTUAL.CASH, '10.2R')
PRINT 'Variance: $':FMT(VARIANCE, '10.2R')
PRINT 'Variance Type: ':VARIANCE.TYPE
```

**5. Variance Alert Logic**

```unibasic
* Check if variance requires alert
ALERT.THRESHOLD.AMT = 10.00
ALERT.THRESHOLD.PCT = 1.0
REQUIRE.APPROVAL.AMT = 50.00

ALERT.FLAG = FALSE
REQUIRE.APPROVAL = FALSE

ABS.VARIANCE = ABS(VARIANCE)
ABS.VAR.PCT = ABS(VARIANCE.PCT)

IF ABS.VARIANCE > ALERT.THRESHOLD.AMT OR ABS.VAR.PCT > ALERT.THRESHOLD.PCT THEN
   ALERT.FLAG = TRUE
   PRINT ''
   PRINT '*** VARIANCE ALERT ***'
   PRINT 'Variance of $':FMT(VARIANCE, '10.2R')
   PRINT 'Exceeds threshold of $':ALERT.THRESHOLD.AMT:' or ':ALERT.THRESHOLD.PCT:'%'
   PRINT ''
END

IF ABS.VARIANCE > REQUIRE.APPROVAL.AMT THEN
   REQUIRE.APPROVAL = TRUE
   PRINT '*** MANAGER APPROVAL REQUIRED ***'
   PRINT 'Variance exceeds $':REQUIRE.APPROVAL.AMT
   PRINT ''

   * Get manager approval
   PRINT 'Manager ID: '
   INPUT MGR.ID

   PRINT 'Manager Password: '
   INPUT MGR.PASSWORD

   * Validate manager (simplified)
   READ MGR.REC FROM F.EMPLOYEES, MGR.ID THEN
      IF MGR.REC<AUTH.LEVEL> >= 7 THEN
         DRAWER.REC<APPROVED.BY> = MGR.ID
         DRAWER.REC<APPROVED.DATE> = DATE()
         DRAWER.REC<APPROVED.TIME> = TIME()
         PRINT 'Approval granted by ':MGR.ID
      END ELSE
         ERROR.CODE = ERR.INSUFFICIENT.AUTH
         ERROR.MSG = 'Manager authorization level insufficient'
         RELEASE F.DRAWERS, DRAWER.ID
         RETURN
      END
   END ELSE
      ERROR.CODE = ERR.NOT.FOUND
      ERROR.MSG = 'Manager not found'
      RELEASE F.DRAWERS, DRAWER.ID
      RETURN
   END
END
```

**6. Send Email Alert for Variance**

```unibasic
* Send email alert if variance threshold exceeded
IF ALERT.FLAG THEN
   STORE.ID = DRAWER.REC<STORE.ID>
   CASHIER.ID = DRAWER.REC<CASHIER.ID>

   EMAIL.TO = 'store.manager@retailpos.com'
   EMAIL.SUBJECT = 'Cash Drawer Variance Alert - ':DRAWER.ID

   EMAIL.BODY = 'Cash Drawer Variance Detected'
   EMAIL.BODY := CHAR(10):CHAR(10)
   EMAIL.BODY := 'Drawer ID: ':DRAWER.ID
   EMAIL.BODY := CHAR(10):'Store: ':STORE.ID
   EMAIL.BODY := CHAR(10):'Cashier: ':CASHIER.ID
   EMAIL.BODY := CHAR(10):'Date: ':OCONV(DATE(), 'D2/MDY')
   EMAIL.BODY := CHAR(10):CHAR(10)
   EMAIL.BODY := 'Expected: $':FMT(EXPECTED.CASH, '10.2R')
   EMAIL.BODY := CHAR(10):'Actual: $':FMT(ACTUAL.CASH, '10.2R')
   EMAIL.BODY := CHAR(10):'Variance: $':FMT(VARIANCE, '10.2R')
   EMAIL.BODY := ' (':VARIANCE.TYPE:')'
   EMAIL.BODY := CHAR(10):'Percentage: ':FMT(VARIANCE.PCT, '10.2R'):'%'

   IF REQUIRE.APPROVAL THEN
      EMAIL.BODY := CHAR(10):CHAR(10)
      EMAIL.BODY := 'Approved by: ':DRAWER.REC<APPROVED.BY>
   END

   CALL UTILS.COMMON('SEND.EMAIL', EMAIL.TO, EMAIL.SUBJECT, EMAIL.BODY, RESULT, EMAIL.ERROR)
END
```

**7. Update Drawer Status**

```unibasic
* Update drawer record
DRAWER.REC<CLOSE.TIME> = TIME()
DRAWER.REC<STATUS> = 'CLOSED'
DRAWER.REC<CLOSED.BY> = USER.ID
DRAWER.REC<VARIANCE.TYPE> = VARIANCE.TYPE

* Write updated drawer
WRITE DRAWER.REC ON F.DRAWERS, DRAWER.ID

* Update employee record - clear current drawer
CASHIER.ID = DRAWER.REC<CASHIER.ID>
READU EMP.REC FROM F.EMPLOYEES, CASHIER.ID THEN
   EMP.REC<CURRENT.DRAWER> = ''
   EMP.REC<LAST.DRAWER.CLOSED> = DRAWER.ID
   WRITE EMP.REC ON F.EMPLOYEES, CASHIER.ID
END ELSE
   RELEASE F.EMPLOYEES, CASHIER.ID
END
```

**8. Build Reconciliation Report**

```unibasic
* Build detailed reconciliation report
RECONCILIATION = ''
REC.LINE = 1

RECONCILIATION<REC.LINE> = '========================================='
REC.LINE += 1
RECONCILIATION<REC.LINE> = '   CASH DRAWER RECONCILIATION'
REC.LINE += 1
RECONCILIATION<REC.LINE> = '========================================='
REC.LINE += 1
RECONCILIATION<REC.LINE> = ''
REC.LINE += 1

* Header information
RECONCILIATION<REC.LINE> = 'Drawer ID: ':DRAWER.ID
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Store: ':DRAWER.REC<STORE.ID>
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Cashier: ':DRAWER.REC<CASHIER.ID>
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Date: ':OCONV(DRAWER.REC<BUSINESS.DATE>, 'D2/MDY [A3]')
REC.LINE += 1
RECONCILIATION<REC.LINE> = ''
REC.LINE += 1

* Time tracking
OPEN.TIME = OCONV(DRAWER.REC<OPEN.TIME>, 'MTS')
CLOSE.TIME = OCONV(DRAWER.REC<CLOSE.TIME>, 'MTS')

RECONCILIATION<REC.LINE> = 'Opened: ':OPEN.TIME
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Closed: ':CLOSE.TIME
REC.LINE += 1
RECONCILIATION<REC.LINE> = '-----------------------------------------'
REC.LINE += 1

* Cash calculation
RECONCILIATION<REC.LINE> = ''
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'CASH CALCULATION:'
REC.LINE += 1
RECONCILIATION<REC.LINE> = ''
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Opening Balance:     $':FMT(OPENING.AMT, '10.2R')
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Cash Sales:        + $':FMT(CASH.SALES, '10.2R')
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Cash Refunds:      - $':FMT(CASH.REFUNDS, '10.2R')
REC.LINE += 1
RECONCILIATION<REC.LINE> = '                     -----------'
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Expected Cash:       $':FMT(EXPECTED.CASH, '10.2R')
REC.LINE += 1
RECONCILIATION<REC.LINE> = ''
REC.LINE += 1

* Actual vs Expected
RECONCILIATION<REC.LINE> = 'Actual Cash Counted: $':FMT(ACTUAL.CASH, '10.2R')
REC.LINE += 1
RECONCILIATION<REC.LINE> = '                     -----------'
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Variance:            $':FMT(VARIANCE, '10.2R')
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Variance %:          ':FMT(VARIANCE.PCT, '10.2R'):'%'
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Type:                ':VARIANCE.TYPE
REC.LINE += 1
RECONCILIATION<REC.LINE> = ''
REC.LINE += 1

* Denomination breakdown if provided
IF DENOMINATIONS NE '' THEN
   RECONCILIATION<REC.LINE> = '-----------------------------------------'
   REC.LINE += 1
   RECONCILIATION<REC.LINE> = 'DENOMINATION BREAKDOWN:'
   REC.LINE += 1
   RECONCILIATION<REC.LINE> = ''
   REC.LINE += 1

   DENOM.LABELS = '$100':VM:'$50':VM:'$20':VM:'$10':VM:'$5':VM:'$1'
   DENOM.LABELS := VM:'$0.25':VM:'$0.10':VM:'$0.05':VM:'$0.01'

   DENOM.COUNT = DCOUNT(DENOMINATIONS<DENOM.VALUES>, VM)
   FOR I = 1 TO DENOM.COUNT
      DENOM.VALUE = DENOMINATIONS<DENOM.VALUES, I>
      DENOM.QTY = DENOMINATIONS<DENOM.QTYS, I>
      DENOM.AMT = DENOMINATIONS<DENOM.TOTALS, I>
      DENOM.LABEL = DENOM.LABELS<1,I>

      LINE.TEXT = DENOM.LABEL:' x ':DENOM.QTY
      LINE.TEXT := ' = $':FMT(DENOM.AMT, '10.2R')
      RECONCILIATION<REC.LINE> = LINE.TEXT
      REC.LINE += 1
   NEXT I
   RECONCILIATION<REC.LINE> = ''
   REC.LINE += 1
END

* Transaction statistics
RECONCILIATION<REC.LINE> = '-----------------------------------------'
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'TRANSACTION STATISTICS:'
REC.LINE += 1
RECONCILIATION<REC.LINE> = ''
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Cash Sales Transactions: ':DRAWER.REC<CASH.SALE.COUNT>
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Cash Refund Transactions: ':DRAWER.REC<CASH.REFUND.COUNT>
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'No-Sale Opens: ':DRAWER.REC<NO.SALE.COUNT>
REC.LINE += 1
RECONCILIATION<REC.LINE> = ''
REC.LINE += 1

* Approval if required
IF REQUIRE.APPROVAL THEN
   RECONCILIATION<REC.LINE> = '-----------------------------------------'
   REC.LINE += 1
   RECONCILIATION<REC.LINE> = 'MANAGER APPROVAL:'
   REC.LINE += 1
   RECONCILIATION<REC.LINE> = 'Approved by: ':DRAWER.REC<APPROVED.BY>
   REC.LINE += 1
   RECONCILIATION<REC.LINE> = 'Date/Time: ':OCONV(DRAWER.REC<APPROVED.DATE>, 'D2/MDY')
   RECONCILIATION<REC.LINE> := ' ':OCONV(DRAWER.REC<APPROVED.TIME>, 'MTS')
   REC.LINE += 1
   RECONCILIATION<REC.LINE> = ''
   REC.LINE += 1
END

RECONCILIATION<REC.LINE> = '========================================='
REC.LINE += 1
RECONCILIATION<REC.LINE> = 'Closed by: ':USER.ID
REC.LINE += 1
RECONCILIATION<REC.LINE> = '========================================='
```

**9. Print Reconciliation and Audit**

```unibasic
* Print reconciliation report
FOR I = 1 TO DCOUNT(RECONCILIATION, AM)
   PRINT RECONCILIATION<I>
NEXT I

* Audit trail
AUDIT.MSG = 'Cash drawer ':DRAWER.ID:' closed'
AUDIT.MSG := ', Expected: $':EXPECTED.CASH
AUDIT.MSG := ', Actual: $':ACTUAL.CASH
AUDIT.MSG := ', Variance: $':VARIANCE:' (':VARIANCE.TYPE:')'

CALL UTILS.COMMON('WRITE.AUDIT', 'CASH.DRAWER.CLOSE', DRAWER.ID, AUDIT.MSG, DUMMY, DUMMY)

ERROR.CODE = 0
ERROR.MSG = 'Cash drawer closed successfully'
```

#### Usage Example

```unibasic
* Close drawer at end of shift
DRAWER.ID = 'S001-20251006-001'
ACTUAL.CASH = 1547.25

* Build denomination breakdown
DENOMINATIONS = ''
DENOMINATIONS<DENOM.VALUES> = '100':VM:'50':VM:'20':VM:'10':VM:'5':VM:'1'
DENOMINATIONS<DENOM.VALUES> := VM:'0.25':VM:'0.10':VM:'0.05':VM:'0.01'
DENOMINATIONS<DENOM.QTYS> = '10':VM:'5':VM:'20':VM:'15':VM:'12':VM:'47'
DENOMINATIONS<DENOM.QTYS> := VM:'15':VM:'20':VM:'5':VM:'0'
DENOMINATIONS<DENOM.TOTALS> = '1000':VM:'250':VM:'400':VM:'150':VM:'60':VM:'47'
DENOMINATIONS<DENOM.TOTALS> := VM:'3.75':VM:'2.00':VM:'0.25':VM:'0'

CALL CASH.DRAWER.CLOSE(DRAWER.ID, ACTUAL.CASH, DENOMINATIONS, RECONCILIATION, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   PRINT 'Drawer closed successfully'
   * Reconciliation report already printed
END ELSE
   PRINT 'Error: ':ERROR.MSG
END
```

---

## REPORTING SYSTEM

The reporting system provides comprehensive business intelligence across sales, inventory, customers, and operations.

### Report Categories

1. **Sales Reports** - Daily sales, sales by item
2. **Inventory Reports** - Status, reorder analysis
3. **Customer Reports** - Purchase analysis, loyalty tiers
4. **Financial Reports** - Commission calculations
5. **Vendor Reports** - Performance analysis

---

### 3. RPT.SALES.DAILY - Daily Sales Report

**Location:** `BP/RPT.SALES.DAILY`
**Lines of Code:** ~850 lines
**Purpose:** Generate comprehensive daily sales report for a store

#### Function Signature

```unibasic
SUBROUTINE RPT.SALES.DAILY(STORE.ID, REPORT.DATE, REPORT.OUTPUT, ERROR.CODE, ERROR.MSG)
```

#### Report Contents

- Sales by hour
- Sales by payment type
- Top selling items
- Sales by employee
- Returns and voids summary
- Tender reconciliation
- Tax summary

---

### 4. RPT.INVENTORY.STATUS - Inventory Status Report

**Location:** `BP/RPT.INVENTORY.STATUS`
**Lines of Code:** ~750 lines
**Purpose:** Inventory levels and reorder recommendations

#### Report Contents

- On-hand quantities by store
- Items below reorder point
- Items with zero stock
- Overstock items
- Inventory value summary
- Slow-moving items
- Reorder recommendations

---

### 5. RPT.CUSTOMER.ANALYSIS - Customer Purchase Analysis

**Location:** `BP/RPT.CUSTOMER.ANALYSIS`
**Lines of Code:** ~850 lines
**Purpose:** Customer buying patterns and segmentation

#### Report Contents

- Customer purchase history
- Average transaction value
- Purchase frequency
- Loyalty tier distribution
- Top customers by spend
- Inactive customers
- Seasonal buying patterns

---

### 6. RPT.SALES.BY.ITEM - Sales Analysis by Item

**Location:** `BP/RPT.SALES.BY.ITEM`
**Lines of Code:** ~750 lines
**Purpose:** Item-level sales performance

#### Report Contents

- Units sold by item
- Revenue by item
- Profit margins
- Category performance
- Sales trends
- Seasonal patterns
- Promotion effectiveness

---

### 7. RPT.COMMISSION - Employee Commission Report

**Location:** `BP/RPT.COMMISSION`
**Lines of Code:** ~700 lines
**Purpose:** Calculate employee commissions

#### Report Contents

- Sales by employee
- Commission tiers applied
- Base commission
- Volume bonuses
- Total commission payable
- Year-to-date totals

---

### 8. RPT.VENDOR.ANALYSIS - Vendor Performance Report

**Location:** `BP/RPT.VENDOR.ANALYSIS`
**Lines of Code:** ~1,300 lines
**Purpose:** Comprehensive vendor performance metrics

#### Report Contents

- Purchase order history
- On-time delivery percentage
- Quality ratings
- Lead time analysis
- Cost variance
- Return rates
- Overall vendor score

---

## SUMMARY

The Cash Management and Reporting systems provide:

### Cash Drawer Management (~1,800 lines)
- **Drawer lifecycle:** Open to close with full tracking
- **Opening balance:** Denomination breakdown optional
- **Transaction tracking:** Sales, refunds, no-sales counted
- **Variance calculation:** Dollar and percentage
- **Alert thresholds:** Automatic email notifications
- **Manager approval:** For significant variances
- **Reconciliation reports:** Complete audit documentation
- **Denomination tracking:** Full currency breakdown

### Reporting System (~5,200 lines)
- **Daily sales reports:** Hourly breakdown, payment types
- **Inventory reports:** Status, reorder analysis
- **Customer analytics:** Purchase patterns, segmentation
- **Item performance:** Sales trends, profitability
- **Commission reports:** Tiered commission calculation
- **Vendor analysis:** Performance scoring, delivery metrics
- **Comprehensive data:** Multi-dimensional business intelligence

**Key Features:**
- Real-time variance detection
- Email alerting for exceptions
- Manager approval workflows
- Complete audit trails
- Multi-dimensional reporting
- Historical trending
- Exception-based management

**Total Module Size:** ~7,000 lines of production-ready UniBasic code

---

*Part 7 of 8 - Next: Batch Processing and System Administration*
