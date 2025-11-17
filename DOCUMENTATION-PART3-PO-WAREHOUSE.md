# RETAIL POS SYSTEM - COMPLETE FUNCTIONALITY GUIDE

## PART 3: PURCHASE ORDERS AND WAREHOUSE OPERATIONS

---

## TABLE OF CONTENTS - PART 3

1. [Purchase Order Management](#1-purchase-order-management)
   - 1.1 Create Purchase Order
   - 1.2 Approve Purchase Order
   - 1.3 Receive Purchase Order
   - 1.4 Cancel Purchase Order

2. [Warehouse Operations](#2-warehouse-operations)
   - 2.1 Warehouse Receiving
   - 2.2 Quality Inspection
   - 2.3 Putaway Processing

3. [Vendor Management](#3-vendor-management)
   - 3.1 Create Vendor
   - 3.2 Vendor Performance Analysis
   - 3.3 Vendor Rating System

---

## 1. PURCHASE ORDER MANAGEMENT

The Purchase Order module manages the complete procurement lifecycle from requisition through receipt, with multi-level approval workflows and budget controls.

### 1.1 Create Purchase Order (PO.CREATE)

**Purpose:** Create new purchase orders with item details and approval routing

**Program:** `BP/PO.CREATE`

**Input Parameters:**
```unibasic
PO.REC         ; Purchase order record
PO.ID.OUT      ; Generated PO ID (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Purchase Order Fields:**

| Field | Description | Required | Validation |
|-------|-------------|----------|------------|
| VENDOR.ID | Vendor identifier | Yes | Must exist in VENDORS |
| PO.DATE | Order date | Yes | Cannot be future date |
| DELIVERY.DATE | Requested delivery | No | Must be future date |
| SHIP.TO.STORE | Destination store | Yes | Must exist |
| DEPARTMENT | Ordering department | No | For budget tracking |
| ITEM.IDS | List of item IDs | Yes | VM-separated list |
| ITEM.QTYS | Quantities ordered | Yes | VM-separated, must be > 0 |
| ITEM.COSTS | Unit costs | Yes | VM-separated, must be >= 0 |
| NOTES | Special instructions | No | Free text |

**Process Flow:**
```
1. Validate vendor exists and is active
2. Validate all items exist
3. Calculate PO total
4. Check vendor minimum order amount
5. Generate PO ID (PO-YYYYMMDD-XXX)
6. Set initial status to 'PENDING'
7. Create PO record
8. Determine approval routing
9. Send approval notification
10. Update vendor statistics
```

**PO Total Calculation:**
```unibasic
PO.TOTAL = 0
ITEM.COUNT = DCOUNT(ITEM.IDS, VM)

FOR I = 1 TO ITEM.COUNT
   ITEM.ID = ITEM.IDS<1, I>
   ITEM.QTY = ITEM.QTYS<1, I>
   ITEM.COST = ITEM.COSTS<1, I>

   LINE.TOTAL = ITEM.QTY * ITEM.COST
   PO.TOTAL += LINE.TOTAL
NEXT I
```

**Approval Routing:**
```
PO Amount          Required Approval Level
---------          -----------------------
$0 - $1,000       Level 5+ (Supervisor)
$1,001 - $10,000  Level 7+ (Manager)
$10,001 - $50,000 Level 9+ (Director)
> $50,000         Level 10 (Executive)
```

**Example Usage:**
```unibasic
PO.REC = ''
PO.REC<VENDOR.ID> = 'VEND-001'
PO.REC<PO.DATE> = SYSTEM.DATE
PO.REC<DELIVERY.DATE> = SYSTEM.DATE + 7
PO.REC<SHIP.TO.STORE> = 'STORE-001'
PO.REC<DEPARTMENT> = 'MERCHANDISE'

* Add items (VM-separated)
PO.REC<ITEM.IDS> = 'ITEM-001':VM:'ITEM-002':VM:'ITEM-003'
PO.REC<ITEM.QTYS> = '100':VM:'50':VM:'75'
PO.REC<ITEM.COSTS> = '10.50':VM:'25.00':VM:'15.75'
PO.REC<NOTES> = 'Rush order - needed for sale'

CALL PO.CREATE(PO.REC, PO.ID, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Purchase order created: ' : PO.ID
   CRT 'Status: PENDING APPROVAL'
END
```

---

### 1.2 Approve Purchase Order (PO.APPROVE)

**Purpose:** Approve purchase orders with budget validation

**Program:** `BP/PO.APPROVE`

**Input Parameters:**
```unibasic
PO.ID          ; Purchase order ID
APPROVER.ID    ; User approving
APPROVAL.NOTES ; Optional notes
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Validation Checks:**

#### 1. PO Status Validation
- Must be in 'PENDING' status
- Cannot approve already approved POs
- Cannot approve rejected/cancelled POs

#### 2. Approver Authorization
```unibasic
* Based on PO total amount
PO.TOTAL = PO.REC<PO.TOTAL>

READ APPROVER.REC FROM F.EMPLOYEES, APPROVER.ID
APPROVER.LEVEL = APPROVER.REC<SECURITY.LEVEL>

* Check authorization
IF PO.TOTAL > 50000 AND APPROVER.LEVEL < 10 THEN
   ERROR.MSG = 'Insufficient approval authority'
   RETURN
END
```

#### 3. Budget Availability
```unibasic
* Check department budget
DEPARTMENT = PO.REC<DEPARTMENT>
BUDGET.KEY = DEPARTMENT : '*' : OCONV(SYSTEM.DATE, 'D4-YM')

READ BUDGET.REC FROM F.BUDGETS, BUDGET.KEY

BUDGET.AMOUNT = BUDGET.REC<1>
COMMITTED.AMOUNT = BUDGET.REC<2>
SPENT.AMOUNT = BUDGET.REC<3>

AVAILABLE = BUDGET.AMOUNT - COMMITTED.AMOUNT - SPENT.AMOUNT

IF PO.TOTAL > AVAILABLE THEN
   ERROR.MSG = 'Insufficient budget: Available $' : AVAILABLE
   RETURN
END
```

#### 4. Vendor Status
- Vendor must be ACTIVE
- Vendor must not be on HOLD
- Vendor minimum order must be met

**Process Flow:**
```
1. Lock PO record
2. Validate PO status
3. Check approver authorization
4. Validate budget availability
5. Verify vendor status
6. Update PO status to 'APPROVED'
7. Record approval details
8. Allocate budget
9. Calculate expected delivery date
10. Send notifications (requester, vendor, receiving)
11. Update vendor statistics
12. Generate PO confirmation
```

**Expected Delivery Calculation:**
```unibasic
* Use vendor lead time
LEAD.TIME = VENDOR.REC<LEAD.TIME.DAYS>
IF LEAD.TIME = '' THEN LEAD.TIME = 7  ; Default

EXPECTED.DATE = SYSTEM.DATE + LEAD.TIME
PO.REC<EXPECTED.DATE> = EXPECTED.DATE
```

**Notifications Sent:**
1. **Requester:** PO approved, expected delivery date
2. **Vendor:** New order, PO details, delivery requirements
3. **Receiving Dept:** Prepare for incoming shipment

**Example Usage:**
```unibasic
* Manager approves $5,000 PO
CALL PO.APPROVE('PO-20251006-001', 'MGR-001', 'Approved for Q4 promotion', ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Purchase order approved'
   CRT 'Expected delivery: ' : OCONV(PO.REC<EXPECTED.DATE>, 'D4/')
END
```

---

### 1.3 Receive Purchase Order (PO.RECEIVE)

**Purpose:** Receive ordered items with quality inspection and cost update

**Program:** `BP/PO.RECEIVE`

**Input Parameters:**
```unibasic
PO.ID          ; Purchase order ID
RECEIPT.INFO   ; Receiving information
RECEIPT.ID     ; Generated receipt ID (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Receipt Information:**
```unibasic
RECEIPT.INFO<1> = received_date
RECEIPT.INFO<2> = received_by (user ID)
RECEIPT.INFO<3> = item_IDs (VM-separated)
RECEIPT.INFO<4> = quantities_received (VM-separated)
RECEIPT.INFO<5> = quantities_rejected (VM-separated, optional)
RECEIPT.INFO<6> = rejection_reasons (VM-separated, optional)
RECEIPT.INFO<7> = notes
```

**Process Flow:**
```
1. Validate PO exists and is approved
2. Lock PO record
3. Validate receiving information
4. Create receipt record
5. For each item received:
   a. Update inventory quantity
   b. Calculate new weighted average cost
   c. Create cost history
6. Update PO status (RECEIVING or RECEIVED)
7. Handle rejected items
8. Update vendor performance
9. Create receiving audit
10. Send notifications
```

**Weighted Average Cost Calculation:**
```unibasic
* Formula: (Current Value + New Value) / (Current Qty + New Qty)

* Current inventory
CURRENT.QTY = ITEM.REC<QTY.ON.HAND, STORE.INDEX>
CURRENT.COST = ITEM.REC<AVG.COST>
CURRENT.VALUE = CURRENT.QTY * CURRENT.COST

* New receipt
NEW.QTY = quantity_received
NEW.COST = unit_cost_from_PO
NEW.VALUE = NEW.QTY * NEW.COST

* Calculate new average
TOTAL.VALUE = CURRENT.VALUE + NEW.VALUE
TOTAL.QTY = CURRENT.QTY + NEW.QTY

NEW.AVG.COST = TOTAL.VALUE / TOTAL.QTY

* Update item
ITEM.REC<AVG.COST> = NEW.AVG.COST
ITEM.REC<QTY.ON.HAND, STORE.INDEX> = TOTAL.QTY
```

**Partial Receipt Handling:**
```unibasic
* Track received vs ordered
ITEM.ORDERED = ITEM.QTYS<1, I>  ; From PO
ITEM.RECEIVED = RECEIPT.QTY<1, I>  ; From receipt

* Update PO item tracking
PO.REC<RECEIVED.QTYS, I> = ITEM.RECEIVED

* Determine PO status
IF all_items_fully_received THEN
   PO.REC<PO.STATUS> = 'RECEIVED'
END ELSE
   PO.REC<PO.STATUS> = 'RECEIVING'
END
```

**Rejected Items:**
```unibasic
* If items rejected (quality issues):
REJECTED.QTY = RECEIPT.INFO<5, I>
REJECT.REASON = RECEIPT.INFO<6, I>

* Do not add to inventory
* Record rejection
* Update vendor quality metrics
* May trigger vendor follow-up
```

**Example Usage:**
```unibasic
* Receive full shipment
RECEIPT.INFO = ''
RECEIPT.INFO<1> = SYSTEM.DATE
RECEIPT.INFO<2> = 'RECV-USER-001'
RECEIPT.INFO<3> = 'ITEM-001':VM:'ITEM-002':VM:'ITEM-003'
RECEIPT.INFO<4> = '100':VM:'50':VM:'75'  ; Full quantities
RECEIPT.INFO<5> = '0':VM:'0':VM:'5'      ; 5 units of ITEM-003 rejected
RECEIPT.INFO<6> = '':VM:'':VM:'DAMAGED'  ; Rejection reason
RECEIPT.INFO<7> = 'Box 3 arrived damaged'

CALL PO.RECEIVE('PO-20251006-001', RECEIPT.INFO, RECEIPT.ID, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Receipt processed: ' : RECEIPT.ID
   CRT '5 units rejected due to damage'
END
```

---

### 1.4 Cancel Purchase Order (PO.CANCEL)

**Purpose:** Cancel purchase order with vendor notification

**Program:** `BP/PO.CANCEL`

**Input Parameters:**
```unibasic
PO.ID          ; Purchase order ID
CANCEL.REASON  ; Reason for cancellation
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Cancellation Reasons:**
- **VENDOR.REQUEST** - Vendor cannot fulfill
- **BUDGET.CUTS** - Budget no longer available
- **DUPLICATE.ORDER** - Order was duplicated
- **VENDOR.UNAVAILABLE** - Vendor out of business
- **PRICING.CHANGE** - Price increased significantly
- **ITEMS.DISCONTINUED** - Items no longer available
- **NO.LONGER.NEEDED** - Business need changed
- **VENDOR.POOR.PERFORMANCE** - Vendor issues
- **OTHER** - Other reasons

**Authorization Matrix:**
| PO Status | PO Amount | Required Level |
|-----------|-----------|----------------|
| DRAFT/PENDING | Any | Creator or Level 5+ |
| APPROVED | < $5,000 | Level 7+ |
| APPROVED | $5,001 - $25,000 | Level 9+ |
| APPROVED | > $25,000 | Level 10 only |
| RECEIVING | Any | Cannot cancel |
| RECEIVED | Any | Cannot cancel |

**Process Flow:**
```
1. Validate PO exists
2. Check PO status
3. Check for partial receipts
4. Validate cancellation authorization
5. Release budget allocation
6. Update PO status to 'CANCELLED'
7. Record cancellation details
8. Update vendor statistics
9. Create cancellation audit
10. Send notifications
```

**Budget Release:**
```unibasic
* Only if PO was approved
IF PO.REC<APPROVED.DATE> # '' THEN
   DEPARTMENT = PO.REC<DEPARTMENT>
   APPROVAL.MONTH = OCONV(PO.REC<APPROVED.DATE>, 'D4-YM')
   BUDGET.KEY = DEPARTMENT : '*' : APPROVAL.MONTH

   READU BUDGET.REC FROM F.BUDGETS, BUDGET.KEY

   * Release committed amount
   COMMITTED = BUDGET.REC<2>
   BUDGET.REC<2> = COMMITTED - PO.REC<PO.TOTAL>

   WRITE BUDGET.REC TO F.BUDGETS, BUDGET.KEY
END
```

**Example Usage:**
```unibasic
* Cancel due to vendor issues
CALL PO.CANCEL('PO-20251006-001', 'VENDOR.POOR.PERFORMANCE', ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Purchase order cancelled'
   CRT 'Budget released: $' : PO.REC<PO.TOTAL>
END
```

---

## 2. WAREHOUSE OPERATIONS

Warehouse operations manage incoming shipments, quality control, and inventory putaway.

### 2.1 Warehouse Receiving (WH.RECEIVE)

**Purpose:** Process warehouse receipts with detailed tracking

**Program:** `BP/WH.RECEIVE`

**Input Parameters:**
```unibasic
RECEIPT.REC    ; Receipt information
RECEIPT.ID     ; Generated receipt ID (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Receipt Record Fields:**

| Field | Description |
|-------|-------------|
| VENDOR.ID | Shipping vendor |
| PO.NUM | Related PO number |
| CARRIER | Shipping carrier |
| TRACKING.NUM | Tracking number |
| RECEIVED.DATE | Receipt date |
| RECEIVED.BY | Receiving clerk |
| PALLET.COUNT | Number of pallets |
| BOX.COUNT | Number of boxes |
| ITEM.IDS | Items in shipment (VM) |
| ITEM.QTYS | Quantities (VM) |
| CONDITION | Overall condition |
| NOTES | Special notes |

**Process Flow:**
```
1. Generate receipt ID
2. Validate PO if provided
3. Create receipt record
4. For each item:
   a. Verify against PO
   b. Perform quality inspection
   c. Record condition
   d. Generate putaway task
5. Update PO receiving status
6. Create receiving audit
7. Send notifications
```

**Quality Inspection:**
```unibasic
* For each item received:
INSPECTION.RESULT = ''
INSPECTION.RESULT<1> = item_id
INSPECTION.RESULT<2> = quantity_inspected
INSPECTION.RESULT<3> = quantity_accepted
INSPECTION.RESULT<4> = quantity_rejected
INSPECTION.RESULT<5> = defect_codes (VM-separated)
INSPECTION.RESULT<6> = inspector_id
INSPECTION.RESULT<7> = inspection_notes
```

**Defect Codes:**
- **DMG** - Physical damage
- **EXP** - Expired or near expiration
- **WRONG** - Wrong item received
- **SHORT** - Short count
- **POOR** - Poor quality
- **OPEN** - Package opened/tampered

### 2.2 Quality Inspection

**Inspection Levels:**

| Level | Description | Sample Size |
|-------|-------------|-------------|
| **1** | Visual only | 100% visual |
| **2** | Sample inspection | 10% of shipment |
| **3** | Full inspection | 100% detailed |
| **4** | Lab testing | Per protocol |

**Inspection Process:**
```
1. Determine inspection level (by vendor, item type)
2. Select sample items
3. Perform inspection
4. Record results
5. Accept or reject items
6. Update vendor quality metrics
7. Generate inspection report
```

**Acceptance Criteria:**
```unibasic
* Defect rate thresholds:
CRITICAL.DEFECTS = 0%  ; Zero tolerance
MAJOR.DEFECTS = 2.5%   ; 2.5% allowable
MINOR.DEFECTS = 10%    ; 10% allowable

* Lot rejection:
IF critical_defects > 0 OR
   major_defects > 2.5% OR
   minor_defects > 10% THEN
   LOT.STATUS = 'REJECTED'
END
```

### 2.3 Putaway Processing

**Purpose:** Assign storage locations and create putaway tasks

**Process:**
```
1. Determine optimal location:
   - Item velocity (fast/medium/slow)
   - Available space
   - Zone assignment
   - Proximity to picking area

2. Generate putaway tasks:
   - Priority (rush, normal, backlog)
   - Item ID and quantity
   - From location (receiving dock)
   - To location (storage bin)
   - Assigned worker

3. Track putaway:
   - Task status (pending, in progress, complete)
   - Time stamps
   - Worker ID
   - Exceptions/issues
```

**Putaway Task:**
```
═══════════════════════════════════
       PUTAWAY TASK
═══════════════════════════════════
Task ID: PUT-20251006-001
Priority: NORMAL
Status: PENDING

Item: Blue Widget (ITEM-001)
Quantity: 100 units
From: DOCK-A
To: AISLE-12-BIN-C

Assigned: WORKER-003
Created: 10/06/2025 10:30 AM
═══════════════════════════════════
```

---

## 3. VENDOR MANAGEMENT

Vendor management tracks supplier information and performance metrics.

### 3.1 Create Vendor (VENDOR.CREATE)

**Purpose:** Create new vendor records with validation

**Program:** `BP/VENDOR.CREATE`

**Input Parameters:**
```unibasic
VENDOR.REC     ; Vendor record
VENDOR.ID.OUT  ; Generated vendor ID (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Vendor Record Fields:**

| Field | Required | Validation |
|-------|----------|------------|
| VENDOR.NAME | Yes | 2-100 characters, unique |
| TAX.ID | No | XXX-XX-XXXX format |
| VENDOR.EMAIL | No | Valid email format |
| VENDOR.PHONE | No | US phone format |
| ADDRESS | No | |
| CITY | No | |
| STATE | No | 2 characters |
| ZIP | No | 5 or 9 digits |
| PAYMENT.TERMS | No | NET30, NET60, NET90, COD, PREPAID |
| LEAD.TIME.DAYS | No | Numeric, default 7 |
| MIN.ORDER.AMT | No | Minimum order amount |
| PRIMARY.CONTACT | No | Contact person name |
| CONTACT.PHONE | No | Contact phone |
| CONTACT.EMAIL | No | Contact email |

**Validation Rules:**

#### Tax ID Format:
```unibasic
* Must be 9 digits (XXX-XX-XXXX)
TAX.ID.CLEAN = CONVERT('-', '', TAX.ID)
IF LEN(TAX.ID.CLEAN) # 9 OR NOT(NUM(TAX.ID.CLEAN)) THEN
   ERROR.MSG = 'Invalid tax ID format'
   RETURN
END
```

#### Duplicate Check:
```unibasic
* Check name uniqueness
* Check tax ID uniqueness (if provided)
```

**Default Values:**
- Status: ACTIVE
- Payment Terms: NET30
- Lead Time: 7 days
- All metrics: 0

**Example Usage:**
```unibasic
VENDOR.REC = ''
VENDOR.REC<VENDOR.NAME> = 'Acme Widgets Inc'
VENDOR.REC<TAX.ID> = '12-3456789'
VENDOR.REC<VENDOR.EMAIL> = 'orders@acmewidgets.com'
VENDOR.REC<VENDOR.PHONE> = '555-123-4567'
VENDOR.REC<PAYMENT.TERMS> = 'NET30'
VENDOR.REC<LEAD.TIME.DAYS> = 5
VENDOR.REC<MIN.ORDER.AMT> = 500.00

CALL VENDOR.CREATE(VENDOR.REC, VENDOR.ID, ERROR.CODE, ERROR.MSG)
```

---

### 3.2 Vendor Performance Analysis (VENDOR.PERFORMANCE)

**Purpose:** Calculate vendor performance metrics

**Program:** `BP/VENDOR.PERFORMANCE`

**Input Parameters:**
```unibasic
VENDOR.ID      ; Vendor to analyze
START.DATE     ; Analysis start date
END.DATE       ; Analysis end date
PERFORMANCE.REC ; Performance metrics (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Metrics Calculated:**

#### Purchase Metrics:
- Total purchase orders
- Total PO amount
- Average PO amount
- Total items ordered
- Total items received

#### Delivery Performance:
- Total deliveries
- On-time deliveries
- Late deliveries
- Early deliveries
- On-time percentage
- Average lead time

#### Quality Metrics:
- Total items inspected
- Items accepted
- Items rejected
- Acceptance rate
- Defect rate

#### Cost Metrics:
- Average cost variance
- Price competitiveness

**Overall Rating Calculation:**
```unibasic
* Weighted scoring (0-100):
* 40% - On-time delivery
* 30% - Quality (acceptance rate)
* 20% - Cost competitiveness
* 10% - Responsiveness

DELIVERY.SCORE = ON.TIME.PCT * 0.4
QUALITY.SCORE = ACCEPTANCE.RATE * 0.3
COST.SCORE = cost_competitiveness_score * 0.2
RESPONSIVE.SCORE = responsiveness_score * 0.1

OVERALL.RATING = DELIVERY.SCORE + QUALITY.SCORE + COST.SCORE + RESPONSIVE.SCORE
```

**Rating Categories:**
```
90-100: EXCELLENT
80-89:  GOOD
70-79:  FAIR
60-69:  POOR
<60:    UNACCEPTABLE
```

---

### 3.3 Vendor Rating System

**Rating Components:**

#### 1. Delivery Rating (40%)
```
On-Time %     Score
---------     -----
95-100%       40 points
90-94%        36 points
85-89%        32 points
80-84%        28 points
<80%          20 points
```

#### 2. Quality Rating (30%)
```
Acceptance %  Score
------------  -----
98-100%       30 points
95-97%        27 points
90-94%        24 points
85-89%        21 points
<85%          15 points
```

#### 3. Cost Rating (20%)
```
Variance      Score
--------      -----
Below market  20 points
At market     15 points
Above market  10 points
```

#### 4. Responsiveness (10%)
```
Lead Time     Score
---------     -----
Better than expected  10 points
As expected          7 points
Slower than expected 5 points
```

**Usage in Vendor Selection:**
```unibasic
* Automatically prefer higher-rated vendors
* Alert when rating drops below threshold
* Trigger re-evaluation for poor performers
* Consider rating in auto-ordering
```

---

*Document Version: 1.0*
*Last Updated: 2025-10-06*
*Part: 3 of 8*
