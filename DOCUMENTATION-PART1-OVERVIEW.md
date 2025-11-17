# RETAIL POS SYSTEM - COMPLETE FUNCTIONALITY GUIDE

## PART 1: SYSTEM OVERVIEW AND ARCHITECTURE

---

## TABLE OF CONTENTS (ALL PARTS)

**PART 1: System Overview and Architecture** (This Document)
- Introduction
- System Architecture
- Database Schema Overview
- Security Model
- Common Components

**PART 2: Core Modules**
- Customer Management
- Inventory Management
- Point of Sale Operations

**PART 3: Purchase Orders and Warehouse**
- Purchase Order Management
- Warehouse Operations
- Vendor Management

**PART 4: Employee and HR Management**
- Employee Records
- Time and Attendance
- Commission Tracking

**PART 5: Loyalty and Promotions**
- Loyalty Program
- Promotional Engine

**PART 6: Gift Cards and Pricing**
- Gift Card Management
- Price Management and Markdowns

**PART 7: Cash Management and Reporting**
- Cash Drawer Operations
- Comprehensive Reporting Suite

**PART 8: Batch Processing and System Administration**
- Batch Processing Jobs
- System Administration
- Menu System

---

## 1. INTRODUCTION

### 1.1 System Purpose

The Retail Point-of-Sale & Distribution System is a comprehensive, enterprise-grade solution designed to manage all aspects of retail operations for multi-store environments. Built entirely in UniBasic for MultiValue database platforms (D3, Universe, UniData), the system provides:

- **Complete Transaction Processing** - From sale initiation to receipt generation
- **Multi-Store Management** - Centralized control with distributed operations
- **Inventory Control** - Real-time tracking across multiple locations
- **Financial Management** - Cash reconciliation, commission tracking, pricing control
- **Customer Relationship** - Loyalty programs, purchase history, targeted marketing
- **Supply Chain** - Purchase orders, vendor management, warehouse operations
- **Staff Management** - Time tracking, scheduling, performance monitoring
- **Business Intelligence** - Comprehensive reporting and analytics

### 1.2 Target Users

| User Role | Access Level | Primary Functions |
|-----------|-------------|-------------------|
| **Cashier** | Level 1-4 | POS transactions, basic lookups |
| **Supervisor** | Level 5-6 | Price changes, inventory adjustments |
| **Manager** | Level 7-8 | Approvals, void authorizations, staff management |
| **Senior Management** | Level 9 | Large price changes, batch operations |
| **System Administrator** | Level 10 | System configuration, mass operations |

### 1.3 Key Features

#### Transaction Processing
- Multiple payment types (cash, credit, debit, gift card, loyalty points)
- Returns and exchanges with policy enforcement
- Void processing with authorization
- Split tender support
- Receipt generation and reprinting

#### Inventory Management
- Multi-store quantity tracking
- Inter-store transfers
- Automatic reordering based on sales velocity
- Cost tracking (weighted average)
- Cycle counting and adjustments

#### Customer Management
- Complete profile management
- Three-tier loyalty program (Silver, Gold, Platinum)
- Purchase history tracking
- Gift card association
- Targeted promotions

#### Financial Controls
- Cash drawer management and reconciliation
- Commission calculation and tracking
- Price change authorization matrix
- Mass markdown capabilities
- Budget tracking and alerts

#### Operational Excellence
- Purchase order workflow with approvals
- Vendor performance tracking
- Employee scheduling and time tracking
- Comprehensive audit trails
- Multi-store data synchronization

---

## 2. SYSTEM ARCHITECTURE

### 2.1 Platform and Technology

**Programming Language:** UniBasic
**Database Platform:** MultiValue Database (D3, Universe, or UniData)
**Architecture Type:** Client-Server with Terminal Interface
**Data Model:** Hierarchical MultiValue

### 2.2 System Components

```
┌─────────────────────────────────────────────────────┐
│                  USER INTERFACE                      │
│              (Menu-Driven Terminal)                  │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│              APPLICATION LAYER                       │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐          │
│  │   POS    │  │ Inventory│  │ Customer │          │
│  │  Module  │  │  Module  │  │  Module  │          │
│  └──────────┘  └──────────┘  └──────────┘          │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐          │
│  │Purchase  │  │ Employee │  │ Loyalty  │          │
│  │  Orders  │  │   HR     │  │  Promo   │          │
│  └──────────┘  └──────────┘  └──────────┘          │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐          │
│  │Gift Card │  │  Pricing │  │   Cash   │          │
│  └──────────┘  └──────────┘  └──────────┘          │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│            COMMON SERVICES LAYER                     │
│  - Error Handling   - Audit Logging                 │
│  - Validation       - Email Notifications            │
│  - Encryption       - Luhn Algorithm                 │
│  - Utilities        - Date/Time Functions            │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│              DATABASE LAYER                          │
│         MultiValue Database Engine                   │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐          │
│  │Transaction│  │ Inventory│  │ Customer │          │
│  │   Data    │  │   Data   │  │   Data   │          │
│  └──────────┘  └──────────┘  └──────────┘          │
└─────────────────────────────────────────────────────┘
```

### 2.3 Data Flow Architecture

#### Transaction Processing Flow
```
Customer → Cashier → POS.START → POS.ADD.ITEM → POS.CALCULATE
                                                      ↓
                                                 POS.PAYMENT
                                                      ↓
                                                 POS.COMPLETE
                                                      ↓
                                    ┌─────────────────┴─────────────────┐
                                    ↓                 ↓                 ↓
                            Update Inventory   Update Customer    Update Cash
                                                                    Drawer
```

#### Purchase Order Flow
```
Need → PO.CREATE → PO.APPROVE → Vendor → WH.RECEIVE → INV.UPDATE
         ↓             ↓                      ↓            ↓
    Budget Check  Authorization        Quality Check   Cost Update
```

### 2.4 Multi-Store Architecture

The system uses **multivalued fields** to track store-specific data:

```unibasic
* Store index determines position in multivalued fields
ITEM.REC<QTY.ON.HAND, STORE.INDEX> = quantity
ITEM.REC<QTY.COMMITTED, STORE.INDEX> = committed_qty
ITEM.REC<QTY.ON.ORDER, STORE.INDEX> = on_order_qty
```

**Store Index Calculation:**
- Stores are indexed dynamically via SELECT loop
- Flexible addition/removal of stores
- Consistent indexing across all inventory records

**Benefits:**
- Single item record for all stores
- Efficient data access
- Centralized inventory management
- Store-specific quantity tracking

---

## 3. DATABASE SCHEMA OVERVIEW

### 3.1 Core Database Files

| File Name | Purpose | Key Fields | Record Count (Typical) |
|-----------|---------|-----------|------------------------|
| **CUSTOMERS** | Customer master data | Customer ID, Name, Loyalty tier | 10,000+ |
| **INVENTORY** | Item master and quantities | Item ID, Prices, Multi-store quantities | 5,000+ |
| **POS.TRANS** | Transaction records | Trans ID, Items, Payments, Totals | 50,000+ |
| **PURCHASE.ORDERS** | Purchase orders | PO ID, Vendor, Items, Status | 5,000+ |
| **EMPLOYEES** | Employee master | Employee ID, Position, Pay rate | 500+ |
| **STORES** | Store locations | Store ID, Address, Operating hours | 10-50 |
| **VENDORS** | Vendor master | Vendor ID, Contact, Performance | 200+ |
| **PROMOTIONS** | Active promotions | Promo ID, Rules, Date range | 100+ |
| **LOYALTY.TRANS** | Loyalty transactions | Points earned/redeemed | 100,000+ |

### 3.2 Supporting Files

| File Name | Purpose | Retention |
|-----------|---------|-----------|
| **GIFT.CARDS** | Gift card balances | Permanent |
| **GIFTCARD.TRANS** | Gift card transactions | Permanent |
| **CASH.DRAWERS** | Drawer sessions | 2 years |
| **CASH.TRANS** | Cash movements | 2 years |
| **RECEIPTS** | Receiving records | 7 years |
| **VOID.AUDIT** | Void transactions | 7 years |
| **RETURN.AUDIT** | Return transactions | 7 years |
| **EXCHANGE.AUDIT** | Exchange transactions | 7 years |
| **PRICE.HISTORY** | Price changes | Permanent |
| **PRICE.SCHEDULE** | Scheduled prices | Until applied |
| **MARKDOWN.BATCHES** | Markdown operations | Permanent |

### 3.3 Sequence Files

Used for generating unique IDs:

| File | ID Format | Example |
|------|-----------|---------|
| **TRANS.SEQUENCES** | TRANS-YYYYMMDD-XXX | TRANS-20251006-001 |
| **PO.SEQUENCES** | PO-YYYYMMDD-XXX | PO-20251006-001 |
| **VENDOR.SEQUENCES** | VEND-YYYYMMDD-XXX | VEND-20251006-001 |
| **DRAWER.SEQUENCES** | DRW-YYYYMMDD-STORE-XXX | DRW-20251006-S01-001 |

### 3.4 DICT File Structure

Each data file has a corresponding DICT file defining:

- **Field positions** (e.g., CUSTOMER.ID = Field 1)
- **Field names** (descriptive labels)
- **Data types** (text, numeric, date)
- **Display formats** (date format, decimal places)
- **Multivalued associations** (for related fields)

**Example: CUSTOMERS.DICT**
```
Field 1: CUSTOMER.ID (Key field)
Field 2: FIRST.NAME
Field 3: LAST.NAME
Field 4: EMAIL
Field 5: PHONE
...
Field 15: LOYALTY.ID
Field 16: LOYALTY.TIER (SILVER/GOLD/PLATINUM)
Field 17: LOYALTY.POINTS
```

---

## 4. SECURITY MODEL

### 4.1 Security Levels

The system implements a **10-level security hierarchy**:

| Level | Role | Capabilities |
|-------|------|-------------|
| **1-2** | Cashier | Basic POS transactions, item lookups |
| **3-4** | Senior Cashier | Returns (< $100), basic adjustments |
| **5-6** | Supervisor | Price changes (< 10%), inventory adjustments, vendor access |
| **7-8** | Store Manager | Voids, returns (any amount), price changes (< 25%), approvals (< $10k) |
| **9** | District Manager | Mass markdowns (< 40%), approvals (< $50k), batch operations |
| **10** | System Admin | Unlimited access, system configuration, mass operations |

### 4.2 Authorization Matrix

#### Price Changes
| Change Percentage | Required Level |
|-------------------|----------------|
| 0% - 10% | Level 5+ |
| 10% - 25% | Level 7+ |
| 25% - 50% | Level 9+ |
| > 50% | Level 10 only |

#### Purchase Order Approvals
| PO Amount | Required Level |
|-----------|----------------|
| $0 - $1,000 | Level 5+ |
| $1,001 - $10,000 | Level 7+ |
| $10,001 - $50,000 | Level 9+ |
| > $50,000 | Level 10 only |

#### Transaction Operations
| Operation | Required Level |
|-----------|----------------|
| Sale | Level 1+ |
| Return (same day) | Level 3+ |
| Return (any time) | Level 7+ |
| Void | Level 7+ |
| Exchange | Level 3+ |

### 4.3 Data Encryption

**Encrypted Fields:**
- Social Security Numbers (SSN)
- Bank Account Numbers
- Credit Card PINs
- Employee Salary Information

**Encryption Method:**
```unibasic
CALL UTILS.COMMON('ENCRYPT.STRING', PLAIN.TEXT, ENCRYPTED.TEXT)
CALL UTILS.COMMON('DECRYPT.STRING', ENCRYPTED.TEXT, PLAIN.TEXT)
```

### 4.4 Audit Trail

**All operations are logged with:**
- User ID (who performed the action)
- Date and Time (when it occurred)
- Action Type (what was done)
- Before/After Values (what changed)
- Store Location (where it happened)
- Authorization Level (permission used)

**Audit Files:**
- Transaction audits (sales, returns, voids)
- Price change audits
- Inventory adjustment audits
- Cash drawer audits
- Gift card audits
- Employee change audits

---

## 5. COMMON COMPONENTS

### 5.1 COMMON.INCLUDES

**Purpose:** Central repository for system-wide constants and equates

**Contents:**
```unibasic
* System Constants
EQU TRUE TO 1
EQU FALSE TO 0
EQU AM TO CHAR(254)    ; Attribute Mark
EQU VM TO CHAR(253)    ; Value Mark
EQU SVM TO CHAR(252)   ; Sub-Value Mark

* Error Codes
EQU ERR.SUCCESS TO 0
EQU ERR.INVALID.DATA TO 1
EQU ERR.RECORD.NOT.FOUND TO 2
EQU ERR.DUPLICATE.KEY TO 3
EQU ERR.DATABASE.ERROR TO 4

* Field Positions - CUSTOMERS
EQU CUSTOMER.ID TO 1
EQU FIRST.NAME TO 2
EQU LAST.NAME TO 3
EQU EMAIL TO 4
...

* Field Positions - INVENTORY
EQU ITEM.ID TO 1
EQU ITEM.NAME TO 2
EQU SKU TO 3
EQU UPC TO 4
...

* Common Variables
COMMON /SYSTEM/ SYSTEM.DATE, SYSTEM.TIME
COMMON /USER/ USER.ID, USER.LEVEL, STORE.ID
COMMON /FILES/ F.CUSTOMERS, F.INVENTORY, F.POS.TRANS, ...
```

### 5.2 DB.CONNECT

**Purpose:** Database connection management

**Functions:**
- Open all required data files
- Verify file existence
- Create missing files if needed
- Return connection status

**Usage:**
```unibasic
CALL DB.CONNECT(ERROR.CODE, ERROR.MSG)
IF ERROR.CODE # ERR.SUCCESS THEN
   CRT 'Database connection failed: ' : ERROR.MSG
   STOP
END
```

### 5.3 UTILS.COMMON

**Purpose:** Shared utility functions

**Available Functions:**

| Function | Purpose | Parameters |
|----------|---------|-----------|
| **VALIDATE.EMAIL** | Email format validation | Email, Valid.Flag |
| **VALIDATE.PHONE** | Phone format validation | Phone, Formatted.Phone, Valid.Flag |
| **LUHN.CHECK.DIGIT** | Calculate Luhn check digit | Number, Check.Digit |
| **LUHN.VALIDATE** | Validate Luhn checksum | Number, Valid.Flag |
| **ENCRYPT.STRING** | Encrypt sensitive data | Plain.Text, Encrypted.Text |
| **DECRYPT.STRING** | Decrypt sensitive data | Encrypted.Text, Plain.Text |
| **FORMAT.CURRENCY** | Format dollar amounts | Amount, Formatted.Amount |
| **FORMAT.DATE** | Format dates | Date, Format, Formatted.Date |
| **FORMAT.PHONE** | Format phone numbers | Phone, Formatted.Phone |
| **LOG.ERROR** | Write to error log | Message, Severity |
| **SEND.EMAIL** | Queue email notification | To, Subject, Body |

**Example Usage:**
```unibasic
* Validate email
CALL UTILS.COMMON('VALIDATE.EMAIL', EMAIL.ADDR, VALID.FLAG)
IF NOT(VALID.FLAG) THEN
   ERROR.MSG = 'Invalid email address'
   RETURN
END

* Generate Luhn check digit
CARD.NUMBER = '600012345678901'
CALL UTILS.COMMON('LUHN.CHECK.DIGIT', CARD.NUMBER, CHECK.DIGIT)
CARD.NUMBER := CHECK.DIGIT  ; Now: 6000123456789019

* Validate credit card
CALL UTILS.COMMON('LUHN.VALIDATE', CARD.NUMBER, VALID.FLAG)
IF NOT(VALID.FLAG) THEN
   ERROR.MSG = 'Invalid card number'
   RETURN
END
```

### 5.4 Error Handling Pattern

**Standard Pattern Used Throughout System:**

```unibasic
SUBROUTINE EXAMPLE.FUNCTION(INPUT.PARAM, OUTPUT.RESULT, ERROR.CODE, ERROR.MSG)
   $INCLUDE COMMON.INCLUDES

   ERROR.CODE = ERR.SUCCESS
   ERROR.MSG = ''
   OUTPUT.RESULT = ''

   * Validate input
   IF INPUT.PARAM = '' THEN
      ERROR.CODE = ERR.INVALID.DATA
      ERROR.MSG = 'Input parameter is required'
      RETURN
   END

   * Perform operation
   GOSUB DO.OPERATION
   IF ERROR.CODE # ERR.SUCCESS THEN RETURN

   * Success
   RETURN

DO.OPERATION:
   * Implementation here
   RETURN
END
```

**Benefits:**
- Consistent error handling across all modules
- Clear error messages for users
- Easy debugging with error codes
- Graceful failure handling

---

## 6. SYSTEM INITIALIZATION

### 6.1 Startup Sequence

```
1. Load COMMON.INCLUDES
2. Connect to database (DB.CONNECT)
3. Initialize system variables
4. Authenticate user
5. Load user permissions
6. Display main menu
```

### 6.2 Session Variables

**Automatically Set:**
- `SYSTEM.DATE` - Current business date
- `SYSTEM.TIME` - Current time
- `USER.ID` - Logged in user
- `USER.LEVEL` - User security level
- `STORE.ID` - Current store location

### 6.3 File Handles

**Common File Variables:**
```unibasic
F.CUSTOMERS          ; Customer file handle
F.INVENTORY          ; Inventory file handle
F.POS.TRANS          ; Transaction file handle
F.PURCHASE.ORDERS    ; Purchase order file handle
F.EMPLOYEES          ; Employee file handle
F.STORES             ; Store file handle
F.VENDORS            ; Vendor file handle
F.PROMOTIONS         ; Promotion file handle
F.LOYALTY.TRANS      ; Loyalty transaction file handle
F.GIFT.CARDS         ; Gift card file handle
```

---

## NEXT STEPS

Continue to **PART 2: Core Modules** for detailed information on:
- Customer Management functionality
- Inventory Management operations
- Point of Sale transaction processing

---

*Document Version: 1.0*
*Last Updated: 2025-10-06*
*Part: 1 of 8*
