# RETAIL POS SYSTEM - COMPLETE FUNCTIONALITY GUIDE

## PART 2: CORE MODULES (Customer, Inventory, Point of Sale)

---

## TABLE OF CONTENTS - PART 2

1. [Customer Management](#1-customer-management)
   - 1.1 Create Customer
   - 1.2 Read Customer
   - 1.3 Update Customer
   - 1.4 Delete Customer
   - 1.5 Search Customers

2. [Inventory Management](#2-inventory-management)
   - 2.1 Create Inventory Item
   - 2.2 Read Inventory Item
   - 2.3 Update Inventory Item
   - 2.4 Adjust Inventory
   - 2.5 Transfer Inventory

3. [Point of Sale Operations](#3-point-of-sale-operations)
   - 3.1 Start Transaction
   - 3.2 Add Items
   - 3.3 Calculate Totals
   - 3.4 Process Payment
   - 3.5 Complete Transaction
   - 3.6 Process Returns
   - 3.7 Void Transactions
   - 3.8 Process Exchanges

---

## 1. CUSTOMER MANAGEMENT

The Customer Management module provides comprehensive functionality for managing customer relationships, including profile management, loyalty program integration, and purchase history tracking.

### 1.1 Create Customer (CUST.CREATE)

**Purpose:** Create new customer records with automatic loyalty ID generation

**Program:** `BP/CUST.CREATE`

**Input Parameters:**
```unibasic
CUST.REC       ; Customer record with populated fields
CUST.ID.OUT    ; Generated customer ID (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Functionality:**

#### Customer Record Fields:
| Field | Description | Validation |
|-------|-------------|------------|
| FIRST.NAME | First name | Required, 2-50 characters |
| LAST.NAME | Last name | Required, 2-50 characters |
| EMAIL | Email address | Required, RFC-compliant format |
| PHONE | Phone number | Required, US format (XXX-XXX-XXXX) |
| ADDRESS | Street address | Optional |
| CITY | City | Optional |
| STATE | State code | Optional, 2 characters |
| ZIP | ZIP code | Optional, 5 or 9 digits |
| DATE.OF.BIRTH | Birth date | Optional, for age verification |

#### Process Flow:
```
1. Validate all input fields
2. Check for duplicate email/phone
3. Generate unique Customer ID (CUST-YYYYMMDD-XXX)
4. Generate Loyalty ID with Luhn check digit
5. Assign initial loyalty tier (SILVER)
6. Set default values
7. Create audit trail
8. Write customer record
9. Send welcome email
```

#### Loyalty ID Generation:
```unibasic
* Format: 9-digit number with Luhn check digit
* Example: 123456789 (last digit is check digit)

BASE.NUMBER = '12345678'
CALL UTILS.COMMON('LUHN.CHECK.DIGIT', BASE.NUMBER, CHECK.DIGIT)
LOYALTY.ID = BASE.NUMBER : CHECK.DIGIT  ; Result: 123456789
```

#### Default Values Set:
- **Status:** ACTIVE
- **Loyalty Tier:** SILVER
- **Loyalty Points:** 0
- **Total Purchases:** 0
- **Total Amount:** $0.00
- **Account Balance:** $0.00
- **Credit Limit:** $0.00

#### Example Usage:
```unibasic
CUST.REC = ''
CUST.REC<FIRST.NAME> = 'John'
CUST.REC<LAST.NAME> = 'Smith'
CUST.REC<EMAIL> = 'john.smith@email.com'
CUST.REC<PHONE> = '555-123-4567'
CUST.REC<ADDRESS> = '123 Main St'
CUST.REC<CITY> = 'Anytown'
CUST.REC<STATE> = 'CA'
CUST.REC<ZIP> = '90210'

CALL CUST.CREATE(CUST.REC, CUST.ID, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Customer created: ' : CUST.ID
   CRT 'Loyalty ID: ' : CUST.REC<LOYALTY.ID>
END ELSE
   CRT 'Error: ' : ERROR.MSG
END
```

---

### 1.2 Read Customer (CUST.READ)

**Purpose:** Retrieve customer record with calculated fields

**Program:** `BP/CUST.READ`

**Input Parameters:**
```unibasic
CUST.ID        ; Customer ID to read
CUST.REC       ; Customer record (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Calculated Fields Added:**
- **Days Since Last Purchase** - Calculated from LAST.PURCHASE.DATE
- **Customer Lifetime Value** - Total purchases amount
- **Average Transaction** - Total amount / Total purchases
- **Loyalty Status** - Active/Inactive based on recent activity
- **Tier Progress** - Percentage to next loyalty tier

**Example Usage:**
```unibasic
CALL CUST.READ('CUST-20251006-001', CUST.REC, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Customer: ' : CUST.REC<FIRST.NAME> : ' ' : CUST.REC<LAST.NAME>
   CRT 'Tier: ' : CUST.REC<LOYALTY.TIER>
   CRT 'Points: ' : CUST.REC<LOYALTY.POINTS>
   CRT 'Lifetime Value: $' : CUST.REC<TOTAL.PURCHASES.AMT>
END
```

---

### 1.3 Update Customer (CUST.UPDATE)

**Purpose:** Update customer information with tier change processing

**Program:** `BP/CUST.UPDATE`

**Input Parameters:**
```unibasic
CUST.ID        ; Customer ID to update
CUST.REC       ; Updated customer record
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Functionality:**

#### Updateable Fields:
- Contact information (email, phone, address)
- Loyalty tier (with authorization)
- Account status
- Credit limit (with authorization)
- Preferences and notes

#### Automatic Processing:
- **Tier Changes:** If tier is upgraded, bonus points awarded
- **Status Changes:** If deactivated, clear loyalty points
- **Email Changes:** Verification email sent
- **Phone Changes:** SMS verification (if enabled)

#### Tier Change Logic:
```
SILVER → GOLD: 500 bonus points
GOLD → PLATINUM: 1000 bonus points
Downgrade: No bonus, points retained
```

#### Audit Trail:
All changes logged with:
- Field name
- Old value
- New value
- Changed by (USER.ID)
- Change date/time

**Example Usage:**
```unibasic
* Read current record
CALL CUST.READ(CUST.ID, CUST.REC, ERROR.CODE, ERROR.MSG)

* Update email
CUST.REC<EMAIL> = 'newemail@example.com'

* Update tier (requires Level 7+)
CUST.REC<LOYALTY.TIER> = 'GOLD'

* Save changes
CALL CUST.UPDATE(CUST.ID, CUST.REC, ERROR.CODE, ERROR.MSG)
```

---

### 1.4 Delete Customer (CUST.DELETE)

**Purpose:** Soft or hard delete customer records

**Program:** `BP/CUST.DELETE`

**Input Parameters:**
```unibasic
CUST.ID        ; Customer ID to delete
DELETE.TYPE    ; 'SOFT' or 'HARD'
DELETE.REASON  ; Reason for deletion
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Delete Types:**

#### Soft Delete (Default):
- Sets status to 'INACTIVE'
- Retains all data for reporting
- Can be reactivated
- Recommended for normal operations

#### Hard Delete (Permanent):
- Archives to CUSTOMERS.ARCHIVE
- Removes from active file
- Requires Level 10 authorization
- Cannot be undone

**Validation Checks:**
- Cannot delete customers with pending orders
- Cannot delete customers with account balance
- Cannot delete customers with active disputes

**Example Usage:**
```unibasic
* Soft delete
CALL CUST.DELETE(CUST.ID, 'SOFT', 'Customer request', ERROR.CODE, ERROR.MSG)

* Hard delete (admin only)
CALL CUST.DELETE(CUST.ID, 'HARD', 'Duplicate record', ERROR.CODE, ERROR.MSG)
```

---

### 1.5 Search Customers (CUST.SEARCH)

**Purpose:** Advanced customer search with filtering

**Program:** `BP/CUST.SEARCH`

**Input Parameters:**
```unibasic
SEARCH.CRITERIA    ; Search criteria record
RESULT.LIST        ; List of matching customer IDs (output)
ERROR.CODE         ; Error code (output)
ERROR.MSG          ; Error message (output)
```

**Search Criteria:**

| Field | Search Type | Example |
|-------|-------------|---------|
| NAME | Partial match | "Smith" finds all Smiths |
| EMAIL | Exact or partial | "john@" finds all johns |
| PHONE | Formatted or digits | "555-1234" or "5551234" |
| LOYALTY.TIER | Exact | "GOLD" |
| CITY | Exact | "Los Angeles" |
| STATE | Exact | "CA" |
| ZIP | Exact or range | "90210" or "90200-90299" |
| STATUS | Exact | "ACTIVE" |
| CREATED.DATE.FROM | Date range | "20250101" |
| CREATED.DATE.TO | Date range | "20251231" |
| TOTAL.PURCHASES.MIN | Amount range | 1000 |
| TOTAL.PURCHASES.MAX | Amount range | 5000 |

**Example Usage:**
```unibasic
* Search for all GOLD tier customers in California
SEARCH.CRITERIA = ''
SEARCH.CRITERIA<1> = 'GOLD'      ; Tier
SEARCH.CRITERIA<2> = 'CA'        ; State
SEARCH.CRITERIA<3> = 'ACTIVE'    ; Status

CALL CUST.SEARCH(SEARCH.CRITERIA, RESULT.LIST, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CUST.COUNT = DCOUNT(RESULT.LIST, VM)
   CRT 'Found ' : CUST.COUNT : ' customers'

   FOR I = 1 TO CUST.COUNT
      CUST.ID = RESULT.LIST<1, I>
      * Process each customer
   NEXT I
END
```

---

## 2. INVENTORY MANAGEMENT

The Inventory Management module handles all aspects of product tracking, including multi-store quantities, cost management, and stock movements.

### 2.1 Create Inventory Item (INV.CREATE)

**Purpose:** Create new inventory items with multi-store initialization

**Program:** `BP/INV.CREATE`

**Input Parameters:**
```unibasic
ITEM.REC       ; Item record with populated fields
INITIAL.QTY    ; Starting quantity
STORE.ID       ; Store for initial quantity
ITEM.ID.OUT    ; Generated item ID (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Item Record Fields:**

| Field | Description | Required | Validation |
|-------|-------------|----------|------------|
| ITEM.NAME | Product name | Yes | 2-100 characters |
| ITEM.DESC | Description | No | Up to 500 characters |
| SKU | Stock keeping unit | Yes | Unique, alphanumeric |
| UPC | Universal product code | No | 12 digits |
| CATEGORY | Product category | Yes | Predefined categories |
| AVG.COST | Average cost | Yes | > 0 |
| SELLING.PRICE | Retail price | Yes | >= Cost |
| MSRP | Manufacturer suggested | No | Informational |
| REORDER.POINT | Minimum quantity | No | Default: 10 |
| REORDER.QTY | Reorder amount | No | Default: 50 |
| VENDOR.ID | Primary vendor | No | Must exist |

**Process Flow:**
```
1. Validate all required fields
2. Check SKU/UPC duplicates
3. Generate Item ID (ITEM-YYYYMMDD-XXX)
4. Initialize multi-store quantities
5. Set initial quantity for specified store
6. Create cost history record
7. Create audit trail
8. Write item record
```

**Multi-Store Initialization:**
```unibasic
* Initialize quantities for all stores
SELECTGET STORE.LIST FROM STORES

STORE.COUNT = DCOUNT(STORE.LIST, VM)

FOR I = 1 TO STORE.COUNT
   STORE.INDEX = I

   IF STORE.LIST<1, I> = STORE.ID THEN
      ITEM.REC<QTY.ON.HAND, STORE.INDEX> = INITIAL.QTY
   END ELSE
      ITEM.REC<QTY.ON.HAND, STORE.INDEX> = 0
   END

   ITEM.REC<QTY.COMMITTED, STORE.INDEX> = 0
   ITEM.REC<QTY.ON.ORDER, STORE.INDEX> = 0
NEXT I
```

**Example Usage:**
```unibasic
ITEM.REC = ''
ITEM.REC<ITEM.NAME> = 'Blue Widget'
ITEM.REC<ITEM.DESC> = 'Premium quality widget'
ITEM.REC<SKU> = 'WDG-BLU-001'
ITEM.REC<UPC> = '123456789012'
ITEM.REC<CATEGORY> = 'WIDGETS'
ITEM.REC<AVG.COST> = 10.00
ITEM.REC<SELLING.PRICE> = 19.99
ITEM.REC<REORDER.POINT> = 25
ITEM.REC<REORDER.QTY> = 100

CALL INV.CREATE(ITEM.REC, 50, 'STORE-001', ITEM.ID, ERROR.CODE, ERROR.MSG)
```

---

### 2.2 Read Inventory Item (INV.READ)

**Purpose:** Retrieve inventory item with analytics

**Program:** `BP/INV.READ`

**Input Parameters:**
```unibasic
ITEM.ID        ; Item ID to read
ITEM.REC       ; Item record (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Calculated Analytics:**
- **Total System Quantity** - Sum across all stores
- **Total Available** - On-hand minus committed
- **Reorder Needed** - If below reorder point
- **Margin Percentage** - (Price - Cost) / Price * 100
- **Turnover Rate** - Sales / Average inventory
- **Days Supply** - Quantity / Daily sales velocity

**Store-Specific Data:**
```unibasic
* Access by store index
STORE.INDEX = 1  ; First store
QTY.ON.HAND = ITEM.REC<QTY.ON.HAND, STORE.INDEX>
QTY.AVAILABLE = QTY.ON.HAND - ITEM.REC<QTY.COMMITTED, STORE.INDEX>
```

---

### 2.3 Update Inventory Item (INV.UPDATE)

**Purpose:** Update item master data with history tracking

**Program:** `BP/INV.UPDATE`

**Input Parameters:**
```unibasic
ITEM.ID        ; Item ID to update
ITEM.REC       ; Updated item record
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Tracked Changes:**
- **Price Changes** - Saved to PRICE.HISTORY
- **Cost Changes** - Saved to COST.HISTORY
- **Description Changes** - Audit log only
- **Status Changes** - Audit log with reason

**Price Change Validation:**
- Cannot reduce price > 75% without Level 9+
- Cannot price below cost without Level 7+
- Significant changes trigger email alerts

---

### 2.4 Adjust Inventory (INV.ADJUST)

**Purpose:** Manual inventory adjustments with COGS impact

**Program:** `BP/INV.ADJUST`

**Input Parameters:**
```unibasic
ITEM.ID        ; Item ID to adjust
STORE.ID       ; Store location
ADJ.QTY        ; Adjustment quantity (+ or -)
ADJ.REASON     ; Reason code
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Reason Codes:**
- **PHYSICAL.COUNT** - Cycle count adjustment
- **DAMAGE** - Damaged goods write-off
- **THEFT** - Shrinkage/loss
- **EXPIRED** - Expired products
- **RETURN.TO.VENDOR** - Vendor return
- **OTHER** - Other reasons (requires notes)

**COGS Impact:**
```
Negative Adjustment (decrease):
   COGS = Quantity * Average Cost
   Reduces asset value

Positive Adjustment (increase):
   Must specify reason
   May require PO reference
```

**Authorization:**
- Adjustments < 10 units: Level 5+
- Adjustments 10-100 units: Level 7+
- Adjustments > 100 units: Level 9+

**Example Usage:**
```unibasic
* Write off damaged inventory
CALL INV.ADJUST('ITEM-001', 'STORE-001', -5, 'DAMAGE', ERROR.CODE, ERROR.MSG)

* Add found inventory
CALL INV.ADJUST('ITEM-001', 'STORE-001', 3, 'PHYSICAL.COUNT', ERROR.CODE, ERROR.MSG)
```

---

### 2.5 Transfer Inventory (INV.TRANSFER)

**Purpose:** Move inventory between stores

**Program:** `BP/INV.TRANSFER`

**Input Parameters:**
```unibasic
ITEM.ID        ; Item ID to transfer
FROM.STORE     ; Source store
TO.STORE       ; Destination store
TRANSFER.QTY   ; Quantity to transfer
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Process Flow:**
```
1. Validate item exists
2. Validate both stores exist
3. Check sufficient quantity at source
4. Lock item record
5. Decrement source store quantity
6. Increment destination store quantity
7. Create transfer audit record
8. Update transfer statistics
9. Generate transfer document
10. Send notification to both stores
```

**Transfer Document:**
```
═══════════════════════════════════
      INVENTORY TRANSFER
═══════════════════════════════════
Transfer ID: TRF-20251006-001
Date: 10/06/2025

From Store: STORE-001
To Store: STORE-002

Item: Blue Widget (ITEM-001)
SKU: WDG-BLU-001
Quantity: 25 units

Initiated By: USER123
═══════════════════════════════════
```

**Example Usage:**
```unibasic
* Transfer 25 units from Store 1 to Store 2
CALL INV.TRANSFER('ITEM-001', 'STORE-001', 'STORE-002', 25, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Transfer completed successfully'
END
```

---

## 3. POINT OF SALE OPERATIONS

The Point of Sale module handles complete transaction lifecycle from initiation to completion, including returns, voids, and exchanges.

### 3.1 Start Transaction (POS.START)

**Purpose:** Initialize new POS transaction

**Program:** `BP/POS.START`

**Input Parameters:**
```unibasic
CUSTOMER.ID    ; Optional customer ID
TRANS.ID       ; Generated transaction ID (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Process Flow:**
```
1. Generate unique Transaction ID
2. Create transaction record
3. Link customer if provided
4. Set transaction date/time
5. Set store and cashier
6. Initialize totals to zero
7. Set status to 'OPEN'
8. Write initial record
```

**Transaction ID Format:**
```
TRANS-YYYYMMDD-XXX
Example: TRANS-20251006-001
```

**Initial Transaction State:**
```unibasic
TRANS.REC<TRANS.ID> = generated_id
TRANS.REC<TRANS.TYPE> = 'SALE'
TRANS.REC<TRANS.DATE> = SYSTEM.DATE
TRANS.REC<TRANS.TIME> = SYSTEM.TIME
TRANS.REC<STORE.ID> = STORE.ID
TRANS.REC<CASHIER.ID> = USER.ID
TRANS.REC<CUSTOMER.ID> = customer_id (if provided)
TRANS.REC<STATUS> = 'OPEN'
TRANS.REC<SUBTOTAL> = 0
TRANS.REC<TAX> = 0
TRANS.REC<TOTAL> = 0
```

**Example Usage:**
```unibasic
* Start transaction without customer
CALL POS.START('', TRANS.ID, ERROR.CODE, ERROR.MSG)

* Start transaction with customer
CALL POS.START('CUST-001', TRANS.ID, ERROR.CODE, ERROR.MSG)

CRT 'Transaction started: ' : TRANS.ID
```

---

### 3.2 Add Items (POS.ADD.ITEM)

**Purpose:** Add items to transaction with promotion application

**Program:** `BP/POS.ADD.ITEM`

**Input Parameters:**
```unibasic
TRANS.ID       ; Transaction ID
ITEM.ID        ; Item to add
QUANTITY       ; Quantity to add
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Process Flow:**
```
1. Lock transaction record
2. Validate item exists and is active
3. Check inventory availability
4. Get current selling price
5. Check for applicable promotions
6. Apply best discount
7. Calculate line total
8. Add to transaction item lists
9. Reserve inventory (commit quantity)
10. Update transaction totals
11. Write updated transaction
```

**Promotion Application:**
```unibasic
* Check all active promotions
* Evaluate eligibility:
  - Date range valid
  - Store applicable
  - Item applicable
  - Quantity requirements met

* Calculate discounts:
  - PERCENT: Price * (Discount% / 100)
  - DOLLAR: Fixed dollar amount off
  - BOGO: Buy X, Get Y free/discounted
  - BUY.AMOUNT.GET.DISCOUNT: Spend $X, save Y%

* Apply best discount (highest savings)
```

**Inventory Reservation:**
```unibasic
* When item added:
QTY.ON.HAND remains unchanged
QTY.COMMITTED increases by quantity

* Available = On Hand - Committed
```

**Example Usage:**
```unibasic
* Add 2 units of item to transaction
CALL POS.ADD.ITEM(TRANS.ID, 'ITEM-001', 2, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT '2 units added to transaction'
END ELSE
   CRT 'Error: ' : ERROR.MSG
END
```

---

### 3.3 Calculate Totals (POS.CALCULATE)

**Purpose:** Calculate transaction subtotal, tax, and total

**Program:** `BP/POS.CALCULATE`

**Input Parameters:**
```unibasic
TRANS.ID       ; Transaction ID
TOTALS.REC     ; Calculated totals (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Calculation Logic:**
```
1. Sum all item line totals = SUBTOTAL
2. Calculate tax:
   TAX = SUBTOTAL * TAX.RATE (typically 8%)
3. Calculate loyalty points earned:
   POINTS = SUBTOTAL * POINTS.RATE (tier-based)
4. TOTAL = SUBTOTAL + TAX
```

**Tax Rate Determination:**
```unibasic
* By store location
READ STORE.REC FROM F.STORES, STORE.ID
TAX.RATE = STORE.REC<TAX.RATE>

* Default if not set
IF TAX.RATE = '' THEN TAX.RATE = 0.08  ; 8%
```

**Loyalty Points Calculation:**
```unibasic
* Points per dollar spent varies by tier:
SILVER:   1 point per $1 (1%)
GOLD:     1.5 points per $1 (1.5%)
PLATINUM: 2 points per $1 (2%)

* Example: $100 purchase
SILVER customer:   100 points
GOLD customer:     150 points
PLATINUM customer: 200 points
```

**Output Format:**
```unibasic
TOTALS.REC<1> = SUBTOTAL
TOTALS.REC<2> = TAX
TOTALS.REC<3> = TOTAL
TOTALS.REC<4> = LOYALTY.POINTS.EARNED
TOTALS.REC<5> = TOTAL.DISCOUNT.AMT
```

---

### 3.4 Process Payment (POS.PAYMENT)

**Purpose:** Process payment for transaction

**Program:** `BP/POS.PAYMENT`

**Input Parameters:**
```unibasic
TRANS.ID       ; Transaction ID
PAYMENT.TYPE   ; Payment method
PAYMENT.AMT    ; Payment amount
PAYMENT.INFO   ; Payment details
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Supported Payment Types:**

#### 1. CASH
```unibasic
* No additional validation
* Calculate change due
CHANGE.DUE = PAYMENT.AMT - TOTAL
```

#### 2. CREDIT / DEBIT
```unibasic
* Validate card number (Luhn algorithm)
* Simulate authorization (in production, integrate with processor)
* Store last 4 digits only
* Generate authorization code
```

#### 3. GIFT CARD
```unibasic
* Validate card number (Luhn algorithm)
* Check card exists and is active
* Verify sufficient balance
* Deduct payment amount
* Update gift card balance
```

#### 4. LOYALTY POINTS
```unibasic
* Validate customer has sufficient points
* Calculate redemption (100 points = $1)
* Deduct points from customer account
* Record loyalty transaction
```

#### 5. SPLIT TENDER
```unibasic
* Accept multiple payments
* Track each payment separately
* Validate total payments = transaction total
```

**Payment Processing:**
```unibasic
CALL POS.PAYMENT(TRANS.ID, 'CREDIT', 50.00, CARD.INFO, ERROR.CODE, ERROR.MSG)

* CARD.INFO format:
CARD.INFO<1> = card_number (Luhn validated)
CARD.INFO<2> = expiration_date
CARD.INFO<3> = cvv (not stored)
CARD.INFO<4> = cardholder_name
```

---

*Document continues in next response due to length...*

---

*Document Version: 1.0*
*Last Updated: 2025-10-06*
*Part: 2 of 8*
