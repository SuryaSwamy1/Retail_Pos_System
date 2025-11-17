# RETAIL POS SYSTEM - DOCUMENTATION PART 6
## GIFT CARD SYSTEM AND PRICE MANAGEMENT

---

## TABLE OF CONTENTS

1. [Gift Card Management](#gift-card-management)
2. [Price Management System](#price-management-system)
3. [Authorization and Security](#authorization-and-security)

---

## GIFT CARD MANAGEMENT

The gift card system provides complete lifecycle management for store-branded gift cards, from activation through redemption, with robust security features.

### Gift Card Architecture

**Card Number Format:**
- **16-digit number** with Luhn check digit validation
- **Format:** 6000-XXXX-XXXX-XXXX
- **Prefix:** 6000 (identifies gift cards)
- **Check digit:** Last digit calculated using Luhn algorithm

**Balance Limits:**
- **Minimum initial load:** $5.00
- **Maximum per load:** $500.00
- **Maximum balance:** $2,000.00
- **Expiration:** 3 years from activation

**Card States:**
- **ACTIVE** - Can be used for purchases
- **SUSPENDED** - Temporarily blocked (fraud, dispute)
- **EXPIRED** - Past expiration date
- **DEPLETED** - Zero balance (can be reloaded)

---

### 1. GIFTCARD.CREATE - Create and Activate Gift Card

**Location:** `BP/GIFTCARD.CREATE`
**Lines of Code:** ~600 lines
**Purpose:** Create new gift cards with initial balance and Luhn validation

#### Function Signature

```unibasic
SUBROUTINE GIFTCARD.CREATE(INITIAL.AMT, CUSTOMER.ID, GC.REC, GC.NUMBER, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| INITIAL.AMT | Decimal | IN | Initial balance to load ($5-$500) |
| CUSTOMER.ID | String | IN | Optional customer ID to link |
| GC.REC | Dynamic Array | OUT | Complete gift card record |
| GC.NUMBER | String | OUT | Generated card number |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Amount Validation**

```unibasic
* Validate initial amount
MIN.INITIAL = 5.00
MAX.LOAD = 500.00

IF INITIAL.AMT < MIN.INITIAL THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Minimum initial balance is $':MIN.INITIAL
   RETURN
END

IF INITIAL.AMT > MAX.LOAD THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Maximum load amount is $':MAX.LOAD
   RETURN
END

* Round to 2 decimal places
INITIAL.AMT = INT(INITIAL.AMT * 100) / 100
```

**2. Gift Card Number Generation with Luhn**

The system generates a unique 16-digit card number with Luhn check digit:

```unibasic
GENERATE.CARD.NUMBER:
   * Start with 6000 prefix (gift card identifier)
   BASE.NUMBER = '6000'

   * Generate 11 random digits
   FOR I = 1 TO 11
      DIGIT = RND(10)
      BASE.NUMBER := DIGIT
   NEXT I

   * Calculate Luhn check digit
   CALL UTILS.COMMON('LUHN.CHECK.DIGIT', BASE.NUMBER, CHECK.DIGIT)
   GC.NUMBER = BASE.NUMBER : CHECK.DIGIT

   * Format: 6000-XXXX-XXXX-XXXX
   FORMATTED.NUMBER = GC.NUMBER[1,4]:'-':GC.NUMBER[5,4]:'-'
   FORMATTED.NUMBER := GC.NUMBER[9,4]:'-':GC.NUMBER[13,4]

   * Verify uniqueness
   READU EXISTING FROM F.GIFTCARDS, GC.NUMBER THEN
      RELEASE F.GIFTCARDS, GC.NUMBER
      GOTO GENERATE.CARD.NUMBER  ;* Try again
   END
```

**Luhn Algorithm for Gift Cards:**

```unibasic
* Luhn check digit calculation
* Input: 6000123456789 (15 digits)
* Output: Check digit to make valid 16-digit number

SUM = 0
DOUBLE.FLAG = TRUE

* Process from right to left
FOR POS = LEN(BASE.NUMBER) TO 1 STEP -1
   DIGIT = BASE.NUMBER[POS,1]

   IF DOUBLE.FLAG THEN
      DIGIT = DIGIT * 2
      IF DIGIT > 9 THEN
         DIGIT = DIGIT - 9  ;* Sum digits: 14 becomes 1+4=5
      END
   END

   SUM = SUM + DIGIT
   DOUBLE.FLAG = NOT(DOUBLE.FLAG)
NEXT POS

* Calculate check digit
CHECK.DIGIT = (10 - (SUM MOD 10)) MOD 10

* Example: 6000123456789012
*   Checksum ensures last digit makes total divisible by 10
```

**Luhn Validation Function:**

```unibasic
* Validate existing card number
SUBROUTINE VALIDATE.LUHN(CARD.NUMBER, VALID.FLAG)

   * Remove any dashes or spaces
   CARD.NUMBER = CONVERT('-', '', CARD.NUMBER)
   CARD.NUMBER = CONVERT(' ', '', CARD.NUMBER)

   IF LEN(CARD.NUMBER) NE 16 THEN
      VALID.FLAG = FALSE
      RETURN
   END

   * Calculate checksum
   SUM = 0
   DOUBLE.FLAG = FALSE

   FOR POS = LEN(CARD.NUMBER) TO 1 STEP -1
      DIGIT = CARD.NUMBER[POS,1]

      IF DOUBLE.FLAG THEN
         DIGIT = DIGIT * 2
         IF DIGIT > 9 THEN DIGIT = DIGIT - 9
      END

      SUM = SUM + DIGIT
      DOUBLE.FLAG = NOT(DOUBLE.FLAG)
   NEXT POS

   * Valid if divisible by 10
   VALID.FLAG = (MOD(SUM, 10) EQ 0)

   RETURN
```

**3. Initialize Gift Card Record**

```unibasic
* Create gift card record
GC.REC = ''
GC.REC<1> = GC.NUMBER                    ;* Card number
GC.REC<2> = INITIAL.AMT                  ;* Current balance
GC.REC<3> = INITIAL.AMT                  ;* Original balance
GC.REC<4> = 0                            ;* Total loaded (lifetime)
GC.REC<5> = 0                            ;* Total redeemed (lifetime)
GC.REC<6> = 'ACTIVE'                     ;* Status
GC.REC<7> = DATE()                       ;* Activation date
GC.REC<8> = DATE() + (365 * 3)          ;* Expiration (3 years)
GC.REC<9> = CUSTOMER.ID                  ;* Linked customer (optional)
GC.REC<10> = USER.ID                     ;* Created by
GC.REC<11> = DATE()                      ;* Created date
GC.REC<12> = TIME()                      ;* Created time

* Initialize transaction history (multivalued)
GC.REC<13> = DATE()                      ;* Transaction dates
GC.REC<14> = 'ACTIVATE'                  ;* Transaction types
GC.REC<15> = INITIAL.AMT                 ;* Transaction amounts
GC.REC<16> = 'Initial activation'        ;* Transaction notes
GC.REC<17> = USER.ID                     ;* Transaction users
GC.REC<18> = ''                          ;* POS transaction references

* Statistics by month (for reporting)
CURRENT.MONTH = OCONV(DATE(), 'D4YM')    ;* YYYYMM format
GC.REC<19> = CURRENT.MONTH               ;* Stat months
GC.REC<20> = INITIAL.AMT                 ;* Loaded amounts by month
GC.REC<21> = 0                           ;* Redeemed amounts by month
```

**4. Payment Processing**

```unibasic
* Process payment for gift card purchase
PAYMENT.REC = ''
PAYMENT.REC<AMOUNT> = INITIAL.AMT
PAYMENT.REC<PAYMENT.TYPE> = 'GIFTCARD.PURCHASE'
PAYMENT.REC<REFERENCE> = GC.NUMBER

* This would integrate with payment processor
* For now, assume payment successful
PAYMENT.SUCCESS = TRUE

IF NOT(PAYMENT.SUCCESS) THEN
   ERROR.CODE = ERR.PAYMENT.FAILED
   ERROR.MSG = 'Payment processing failed'
   RETURN
END
```

**5. Customer Linkage**

```unibasic
* If customer ID provided, link to customer record
IF CUSTOMER.ID NE '' THEN
   CALL DB.CONNECT('OPEN.FILE', 'CUSTOMERS', F.CUSTOMERS, ERROR)

   READU CUST.REC FROM F.CUSTOMERS, CUSTOMER.ID THEN
      * Add to customer's gift card list
      GIFT.CARDS = CUST.REC<GIFT.CARDS>

      IF GIFT.CARDS EQ '' THEN
         CUST.REC<GIFT.CARDS> = GC.NUMBER
      END ELSE
         CUST.REC<GIFT.CARDS> := VM : GC.NUMBER
      END

      WRITE CUST.REC ON F.CUSTOMERS, CUSTOMER.ID
   END ELSE
      * Customer not found - warning but continue
      PRINT 'WARNING: Customer ':CUSTOMER.ID:' not found'
      GC.REC<CUSTOMER.ID> = ''
      RELEASE F.CUSTOMERS, CUSTOMER.ID
   END
END
```

**6. Save and Print Card**

```unibasic
* Write gift card record
WRITE GC.REC ON F.GIFTCARDS, GC.NUMBER

* Print gift card receipt/activation slip
PRINT ''
PRINT '========================================='
PRINT '      GIFT CARD ACTIVATION'
PRINT '========================================='
PRINT ''
PRINT 'Card Number: ':FORMATTED.NUMBER
PRINT 'Balance: $':FMT(INITIAL.AMT, '10.2R')
PRINT 'Activated: ':OCONV(DATE(), 'D2/MDY')
PRINT 'Expires: ':OCONV(GC.REC<EXPIRATION.DATE>, 'D2/MDY')
PRINT ''
PRINT 'Keep this card in a safe place'
PRINT 'Treat this card like cash'
PRINT '========================================='

* Audit trail
AUDIT.MSG = 'Gift card ':GC.NUMBER:' created with balance $':INITIAL.AMT
IF CUSTOMER.ID NE '' THEN
   AUDIT.MSG := ' for customer ':CUSTOMER.ID
END

CALL UTILS.COMMON('WRITE.AUDIT', 'GIFTCARD.CREATE', GC.NUMBER, AUDIT.MSG, DUMMY, DUMMY)

ERROR.CODE = 0
ERROR.MSG = 'Gift card created successfully'
```

#### Usage Example

```unibasic
* Create gift card with $100 balance
INITIAL.AMT = 100.00
CUSTOMER.ID = 'C00001234'  ;* Optional

CALL GIFTCARD.CREATE(INITIAL.AMT, CUSTOMER.ID, GC.REC, GC.NUMBER, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   PRINT 'Gift card created: ':GC.NUMBER
   PRINT 'Balance: $':GC.REC<BALANCE>
   PRINT 'Expires: ':OCONV(GC.REC<EXPIRATION.DATE>, 'D2/MDY')
END ELSE
   PRINT 'Error: ':ERROR.MSG
END
```

---

### 2. GIFTCARD.LOAD - Add Funds to Gift Card

**Location:** `BP/GIFTCARD.LOAD`
**Lines of Code:** ~600 lines
**Purpose:** Add additional funds to existing gift cards

#### Function Signature

```unibasic
SUBROUTINE GIFTCARD.LOAD(GC.NUMBER, LOAD.AMT, PAYMENT.INFO, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| GC.NUMBER | String | IN | Gift card number to load |
| LOAD.AMT | Decimal | IN | Amount to add ($1-$500) |
| PAYMENT.INFO | Dynamic Array | IN | Payment method details |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Card Validation**

```unibasic
* Validate card number format and Luhn check
CALL UTILS.COMMON('LUHN.VALIDATE', GC.NUMBER, VALID.FLAG)

IF NOT(VALID.FLAG) THEN
   ERROR.CODE = ERR.INVALID.CARD
   ERROR.MSG = 'Invalid gift card number'
   RETURN
END

* Read and lock card record
CALL DB.CONNECT('OPEN.FILE', 'GIFTCARDS', F.GIFTCARDS, ERROR)

READU GC.REC FROM F.GIFTCARDS, GC.NUMBER ELSE
   ERROR.CODE = ERR.NOT.FOUND
   ERROR.MSG = 'Gift card not found'
   RETURN
END
```

**2. Status and Expiration Check**

```unibasic
* Check card status
CARD.STATUS = GC.REC<STATUS>
EXPIRATION.DATE = GC.REC<EXPIRATION.DATE>

BEGIN CASE
   CASE CARD.STATUS EQ 'SUSPENDED'
      ERROR.CODE = ERR.CARD.SUSPENDED
      ERROR.MSG = 'Gift card is suspended - contact customer service'
      RELEASE F.GIFTCARDS, GC.NUMBER
      RETURN

   CASE CARD.STATUS EQ 'EXPIRED'
      ERROR.CODE = ERR.CARD.EXPIRED
      ERROR.MSG = 'Gift card has expired on ':OCONV(EXPIRATION.DATE, 'D2/MDY')
      RELEASE F.GIFTCARDS, GC.NUMBER
      RETURN

   CASE DATE() > EXPIRATION.DATE
      * Auto-expire if past date
      GC.REC<STATUS> = 'EXPIRED'
      WRITE GC.REC ON F.GIFTCARDS, GC.NUMBER
      ERROR.CODE = ERR.CARD.EXPIRED
      ERROR.MSG = 'Gift card expired on ':OCONV(EXPIRATION.DATE, 'D2/MDY')
      RETURN
END CASE
```

**3. Load Amount Validation**

```unibasic
* Validate load amount
MIN.LOAD = 1.00
MAX.LOAD = 500.00
MAX.BALANCE = 2000.00

IF LOAD.AMT < MIN.LOAD THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Minimum load amount is $':MIN.LOAD
   RELEASE F.GIFTCARDS, GC.NUMBER
   RETURN
END

IF LOAD.AMT > MAX.LOAD THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Maximum load amount is $':MAX.LOAD:' per transaction'
   RELEASE F.GIFTCARDS, GC.NUMBER
   RETURN
END

* Check maximum balance limit
CURRENT.BALANCE = GC.REC<BALANCE>
NEW.BALANCE = CURRENT.BALANCE + LOAD.AMT

IF NEW.BALANCE > MAX.BALANCE THEN
   ALLOWED.AMT = MAX.BALANCE - CURRENT.BALANCE
   ERROR.CODE = ERR.BALANCE.LIMIT
   ERROR.MSG = 'Would exceed maximum balance of $':MAX.BALANCE
   ERROR.MSG := '. Maximum you can load: $':FMT(ALLOWED.AMT, '10.2R')
   RELEASE F.GIFTCARDS, GC.NUMBER
   RETURN
END

* Round to 2 decimals
LOAD.AMT = INT(LOAD.AMT * 100) / 100
NEW.BALANCE = INT(NEW.BALANCE * 100) / 100
```

**4. Process Payment**

```unibasic
* Process payment for load
PAYMENT.TYPE = PAYMENT.INFO<PAYMENT.TYPE>
PAYMENT.REF = PAYMENT.INFO<REFERENCE>

* Payment processing (simplified)
PAYMENT.SUCCESS = TRUE  ;* Would call payment processor

IF NOT(PAYMENT.SUCCESS) THEN
   ERROR.CODE = ERR.PAYMENT.FAILED
   ERROR.MSG = 'Payment processing failed'
   RELEASE F.GIFTCARDS, GC.NUMBER
   RETURN
END
```

**5. Update Gift Card Balances**

```unibasic
* Update balances
TOTAL.LOADED = GC.REC<TOTAL.LOADED>
TOTAL.LOADED = TOTAL.LOADED + LOAD.AMT

GC.REC<BALANCE> = NEW.BALANCE
GC.REC<TOTAL.LOADED> = TOTAL.LOADED

* If card was DEPLETED, reactivate
IF CARD.STATUS EQ 'DEPLETED' THEN
   GC.REC<STATUS> = 'ACTIVE'
   PRINT 'Gift card reactivated'
END

* Update modified tracking
GC.REC<MODIFIED.BY> = USER.ID
GC.REC<MODIFIED.DATE> = DATE()
GC.REC<MODIFIED.TIME> = TIME()
```

**6. Record Transaction History**

```unibasic
* Add to transaction history
TRANS.COUNT = DCOUNT(GC.REC<TRANS.DATES>, VM) + 1

GC.REC<TRANS.DATES, TRANS.COUNT> = DATE()
GC.REC<TRANS.TYPES, TRANS.COUNT> = 'LOAD'
GC.REC<TRANS.AMOUNTS, TRANS.COUNT> = LOAD.AMT
GC.REC<TRANS.NOTES, TRANS.COUNT> = 'Balance reload'
GC.REC<TRANS.USERS, TRANS.COUNT> = USER.ID
GC.REC<TRANS.REFS, TRANS.COUNT> = PAYMENT.REF

* Update monthly statistics
CURRENT.MONTH = OCONV(DATE(), 'D4YM')
LOCATE CURRENT.MONTH IN GC.REC<STAT.MONTHS, 1> SETTING POS THEN
   * Month exists - add to total
   MONTH.LOADED = GC.REC<STAT.LOADED, POS>
   GC.REC<STAT.LOADED, POS> = MONTH.LOADED + LOAD.AMT
END ELSE
   * New month - add entry
   GC.REC<STAT.MONTHS, -1> = CURRENT.MONTH
   GC.REC<STAT.LOADED, -1> = LOAD.AMT
   GC.REC<STAT.REDEEMED, -1> = 0
END
```

**7. Save and Print Receipt**

```unibasic
* Write updated record
WRITE GC.REC ON F.GIFTCARDS, GC.NUMBER

* Print receipt
FORMATTED.NUMBER = GC.NUMBER[1,4]:'-':GC.NUMBER[5,4]:'-'
FORMATTED.NUMBER := GC.NUMBER[9,4]:'-':GC.NUMBER[13,4]

PRINT ''
PRINT '========================================='
PRINT '      GIFT CARD RELOAD'
PRINT '========================================='
PRINT ''
PRINT 'Card Number: ':FORMATTED.NUMBER
PRINT 'Amount Loaded: $':FMT(LOAD.AMT, '10.2R')
PRINT 'Previous Balance: $':FMT(CURRENT.BALANCE, '10.2R')
PRINT 'New Balance: $':FMT(NEW.BALANCE, '10.2R')
PRINT 'Date: ':OCONV(DATE(), 'D2/MDY [A3]')
PRINT 'Expires: ':OCONV(GC.REC<EXPIRATION.DATE>, 'D2/MDY')
PRINT ''
PRINT 'Thank you!'
PRINT '========================================='

* Audit trail
AUDIT.MSG = 'Loaded $':LOAD.AMT:' onto gift card ':GC.NUMBER
AUDIT.MSG := ', New balance: $':NEW.BALANCE

CALL UTILS.COMMON('WRITE.AUDIT', 'GIFTCARD.LOAD', GC.NUMBER, AUDIT.MSG, DUMMY, DUMMY)

ERROR.CODE = 0
ERROR.MSG = 'Gift card loaded successfully'
```

#### Usage Example

```unibasic
* Reload gift card
GC.NUMBER = '6000123456789012'
LOAD.AMT = 50.00

PAYMENT.INFO = ''
PAYMENT.INFO<PAYMENT.TYPE> = 'CASH'
PAYMENT.INFO<REFERENCE> = 'RELOAD-':DATE():'-':TIME()

CALL GIFTCARD.LOAD(GC.NUMBER, LOAD.AMT, PAYMENT.INFO, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   PRINT 'Successfully loaded $':LOAD.AMT
   PRINT ERROR.MSG
END ELSE
   PRINT 'Load failed: ':ERROR.MSG
END
```

---

### 3. GIFTCARD.REDEEM - Redeem Gift Card for Purchase

**Location:** `BP/GIFTCARD.REDEEM`
**Lines of Code:** ~650 lines
**Purpose:** Redeem gift card balance during POS transactions

#### Function Signature

```unibasic
SUBROUTINE GIFTCARD.REDEEM(GC.NUMBER, REDEEM.AMT, POS.TRANS.ID, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| GC.NUMBER | String | IN | Gift card number |
| REDEEM.AMT | Decimal | IN | Amount to redeem |
| POS.TRANS.ID | String | IN | POS transaction ID reference |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Card Validation and Balance Check**

```unibasic
* Validate Luhn check
CALL UTILS.COMMON('LUHN.VALIDATE', GC.NUMBER, VALID.FLAG)

IF NOT(VALID.FLAG) THEN
   ERROR.CODE = ERR.INVALID.CARD
   ERROR.MSG = 'Invalid gift card number'
   RETURN
END

* Read and lock card
READU GC.REC FROM F.GIFTCARDS, GC.NUMBER ELSE
   ERROR.CODE = ERR.NOT.FOUND
   ERROR.MSG = 'Gift card not found'
   RETURN
END

* Validate status
CARD.STATUS = GC.REC<STATUS>

IF CARD.STATUS NE 'ACTIVE' THEN
   ERROR.CODE = ERR.CARD.INACTIVE
   ERROR.MSG = 'Gift card status is ':CARD.STATUS:' - cannot use'
   RELEASE F.GIFTCARDS, GC.NUMBER
   RETURN
END

* Check expiration
IF DATE() > GC.REC<EXPIRATION.DATE> THEN
   GC.REC<STATUS> = 'EXPIRED'
   WRITE GC.REC ON F.GIFTCARDS, GC.NUMBER
   ERROR.CODE = ERR.CARD.EXPIRED
   ERROR.MSG = 'Gift card expired on ':OCONV(GC.REC<EXPIRATION.DATE>, 'D2/MDY')
   RETURN
END

* Check balance
CURRENT.BALANCE = GC.REC<BALANCE>

IF CURRENT.BALANCE <= 0 THEN
   GC.REC<STATUS> = 'DEPLETED'
   WRITE GC.REC ON F.GIFTCARDS, GC.NUMBER
   ERROR.CODE = ERR.INSUFFICIENT.BALANCE
   ERROR.MSG = 'Gift card has zero balance'
   RETURN
END

IF REDEEM.AMT > CURRENT.BALANCE THEN
   ERROR.CODE = ERR.INSUFFICIENT.BALANCE
   ERROR.MSG = 'Insufficient balance. Available: $':FMT(CURRENT.BALANCE, '10.2R')
   RELEASE F.GIFTCARDS, GC.NUMBER
   RETURN
END
```

**2. Minimum Redemption Check**

```unibasic
* Minimum redemption amount (usually $0.01, but can be configured)
MIN.REDEEM = 0.01

IF REDEEM.AMT < MIN.REDEEM THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Minimum redemption is $':MIN.REDEEM
   RELEASE F.GIFTCARDS, GC.NUMBER
   RETURN
END

* Round to 2 decimals
REDEEM.AMT = INT(REDEEM.AMT * 100) / 100
```

**3. Update Balances**

```unibasic
* Calculate new balance
NEW.BALANCE = CURRENT.BALANCE - REDEEM.AMT
NEW.BALANCE = INT(NEW.BALANCE * 100) / 100  ;* Round to cents

* Update totals
TOTAL.REDEEMED = GC.REC<TOTAL.REDEEMED>
TOTAL.REDEEMED = TOTAL.REDEEMED + REDEEM.AMT

GC.REC<BALANCE> = NEW.BALANCE
GC.REC<TOTAL.REDEEMED> = TOTAL.REDEEMED

* Check if depleted
IF NEW.BALANCE <= 0 THEN
   GC.REC<STATUS> = 'DEPLETED'
   PRINT 'Gift card fully redeemed - balance is now $0.00'
   PRINT 'Card can be reloaded for future use'
END

* Update modified tracking
GC.REC<MODIFIED.BY> = USER.ID
GC.REC<MODIFIED.DATE> = DATE()
GC.REC<MODIFIED.TIME> = TIME()
GC.REC<LAST.USED.DATE> = DATE()
```

**4. Record Redemption Transaction**

```unibasic
* Add to transaction history
TRANS.COUNT = DCOUNT(GC.REC<TRANS.DATES>, VM) + 1

GC.REC<TRANS.DATES, TRANS.COUNT> = DATE()
GC.REC<TRANS.TYPES, TRANS.COUNT> = 'REDEEM'
GC.REC<TRANS.AMOUNTS, TRANS.COUNT> = REDEEM.AMT
GC.REC<TRANS.NOTES, TRANS.COUNT> = 'Purchase redemption'
GC.REC<TRANS.USERS, TRANS.COUNT> = USER.ID
GC.REC<TRANS.REFS, TRANS.COUNT> = POS.TRANS.ID

* Update monthly statistics
CURRENT.MONTH = OCONV(DATE(), 'D4YM')
LOCATE CURRENT.MONTH IN GC.REC<STAT.MONTHS, 1> SETTING POS THEN
   * Month exists - add to redeemed total
   MONTH.REDEEMED = GC.REC<STAT.REDEEMED, POS>
   GC.REC<STAT.REDEEMED, POS> = MONTH.REDEEMED + REDEEM.AMT
END ELSE
   * New month
   GC.REC<STAT.MONTHS, -1> = CURRENT.MONTH
   GC.REC<STAT.LOADED, -1> = 0
   GC.REC<STAT.REDEEMED, -1> = REDEEM.AMT
END
```

**5. Link to POS Transaction**

```unibasic
* Update POS transaction with gift card payment
IF POS.TRANS.ID NE '' THEN
   CALL DB.CONNECT('OPEN.FILE', 'POS.TRANS', F.POS.TRANS, ERROR)

   READU TRANS.REC FROM F.POS.TRANS, POS.TRANS.ID THEN
      * Add gift card payment to transaction
      PAYMENT.TYPES = TRANS.REC<PAYMENT.TYPES>
      PAYMENT.AMTS = TRANS.REC<PAYMENT.AMOUNTS>
      PAYMENT.REFS = TRANS.REC<PAYMENT.REFS>

      PAY.COUNT = DCOUNT(PAYMENT.TYPES, VM) + 1
      TRANS.REC<PAYMENT.TYPES, PAY.COUNT> = 'GIFTCARD'
      TRANS.REC<PAYMENT.AMOUNTS, PAY.COUNT> = REDEEM.AMT
      TRANS.REC<PAYMENT.REFS, PAY.COUNT> = GC.NUMBER

      WRITE TRANS.REC ON F.POS.TRANS, POS.TRANS.ID
   END ELSE
      RELEASE F.POS.TRANS, POS.TRANS.ID
   END
END
```

**6. Save and Generate Receipt**

```unibasic
* Write updated gift card record
WRITE GC.REC ON F.GIFTCARDS, GC.NUMBER

* Format card number for display
FORMATTED.NUMBER = GC.NUMBER[1,4]:'-':GC.NUMBER[5,4]:'-'
FORMATTED.NUMBER := GC.NUMBER[9,4]:'-':GC.NUMBER[13,4]

* Print gift card portion of receipt
PRINT ''
PRINT '-----------------------------------------'
PRINT 'GIFT CARD PAYMENT'
PRINT '-----------------------------------------'
PRINT 'Card: ****-****-****-':GC.NUMBER[13,4]
PRINT 'Amount Paid: $':FMT(REDEEM.AMT, '10.2R')
PRINT 'Remaining Balance: $':FMT(NEW.BALANCE, '10.2R')

IF NEW.BALANCE > 0 THEN
   PRINT 'Keep this card for future purchases'
END ELSE
   PRINT 'Card balance: $0.00'
   PRINT 'Card can be reloaded at any time'
END

PRINT '-----------------------------------------'

* Audit trail
AUDIT.MSG = 'Redeemed $':REDEEM.AMT:' from gift card ':GC.NUMBER
AUDIT.MSG := ' for transaction ':POS.TRANS.ID
AUDIT.MSG := ', Remaining balance: $':NEW.BALANCE

CALL UTILS.COMMON('WRITE.AUDIT', 'GIFTCARD.REDEEM', GC.NUMBER, AUDIT.MSG, DUMMY, DUMMY)

ERROR.CODE = 0
ERROR.MSG = 'Gift card redeemed successfully'
```

#### Usage Example in POS Transaction

```unibasic
* During POS checkout, customer wants to pay with gift card
TRANS.ID = 'POS20251006-000123'
TRANSACTION.TOTAL = 75.50
GC.NUMBER = '6000123456789012'

* Customer wants to use gift card
PRINT 'Enter gift card number: '
INPUT GC.NUMBER

* Check balance first
READ GC.REC FROM F.GIFTCARDS, GC.NUMBER THEN
   AVAILABLE = GC.REC<BALANCE>
   PRINT 'Available balance: $':FMT(AVAILABLE, '10.2R')

   * Ask how much to use
   IF AVAILABLE >= TRANSACTION.TOTAL THEN
      PRINT 'Use full amount? (Y/N): '
      INPUT RESPONSE
      IF RESPONSE EQ 'Y' THEN
         REDEEM.AMT = TRANSACTION.TOTAL
      END ELSE
         PRINT 'Amount to use: '
         INPUT REDEEM.AMT
      END
   END ELSE
      REDEEM.AMT = AVAILABLE
      PRINT 'Using full card balance: $':REDEEM.AMT
      REMAINING = TRANSACTION.TOTAL - REDEEM.AMT
      PRINT 'Remaining amount due: $':FMT(REMAINING, '10.2R')
   END
END ELSE
   PRINT 'Gift card not found'
   STOP
END

* Redeem the gift card
CALL GIFTCARD.REDEEM(GC.NUMBER, REDEEM.AMT, TRANS.ID, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   PRINT 'Gift card applied: $':REDEEM.AMT
   TRANSACTION.TOTAL = TRANSACTION.TOTAL - REDEEM.AMT

   IF TRANSACTION.TOTAL > 0 THEN
      PRINT 'Remaining balance due: $':FMT(TRANSACTION.TOTAL, '10.2R')
      * Process additional payment
   END
END ELSE
   PRINT 'Gift card error: ':ERROR.MSG
END
```

#### Complete Gift Card Lifecycle Example

```unibasic
* Example: Complete gift card lifecycle

* 1. Customer purchases $100 gift card for friend
CALL GIFTCARD.CREATE(100.00, '', GC.REC, GC.NUMBER, ERR, MSG)
   Result: Card 6000-1234-5678-9012 created
           Balance: $100.00
           Expires: 3 years from today

* 2. Friend receives card and checks balance
READ GC.REC FROM F.GIFTCARDS, '6000123456789012' THEN
   PRINT 'Balance: $':GC.REC<BALANCE>
   Result: $100.00
END

* 3. Friend uses $35.50 for first purchase
CALL GIFTCARD.REDEEM('6000123456789012', 35.50, 'POS-001', ERR, MSG)
   Result: Redeemed $35.50
           Remaining: $64.50

* 4. Friend uses $64.50 for second purchase (full balance)
CALL GIFTCARD.REDEEM('6000123456789012', 64.50, 'POS-002', ERR, MSG)
   Result: Redeemed $64.50
           Remaining: $0.00
           Status changed to DEPLETED

* 5. Customer reloads card with $75
CALL GIFTCARD.LOAD('6000123456789012', 75.00, PAYMENT.INFO, ERR, MSG)
   Result: Loaded $75.00
           Status changed back to ACTIVE
           Balance: $75.00

* 6. Summary statistics
Lifetime Stats:
   Total Loaded: $175.00 ($100 initial + $75 reload)
   Total Redeemed: $100.00 ($35.50 + $64.50)
   Current Balance: $75.00
   Status: ACTIVE
   Transactions: 4 (1 activate, 2 redeem, 1 load)
```

---

## PRICE MANAGEMENT SYSTEM

The price management system provides authorized price changes and mass markdown capabilities with complete audit trails and approval workflows.

### Authorization Levels for Pricing

**Single Item Price Changes:**

| Price Change % | Required Auth Level | Typical Role |
|----------------|---------------------|--------------|
| < 10% | Level 5+ | Department Manager |
| 10-25% | Level 7+ | Store Manager |
| 25-50% | Level 9+ | Regional Manager |
| > 50% | Level 10+ | VP/Executive |

**Mass Markdowns:**

| Markdown % | Required Auth Level | Typical Role |
|------------|---------------------|--------------|
| < 20% | Level 7+ | Store Manager |
| 20-40% | Level 9+ | Regional Manager |
| > 40% | Level 10+ | VP Merchandising |

---

### 4. PRICE.CHANGE - Individual Price Change

**Location:** `BP/PRICE.CHANGE`
**Lines of Code:** ~850 lines
**Purpose:** Change individual item prices with authorization and history tracking

#### Function Signature

```unibasic
SUBROUTINE PRICE.CHANGE(ITEM.ID, NEW.PRICE, REASON, EFFECTIVE.DATE, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| ITEM.ID | String | IN | Item ID to change price |
| NEW.PRICE | Decimal | IN | New selling price |
| REASON | String | IN | Reason code (MARKDOWN/MARKUP/SEASONAL/etc) |
| EFFECTIVE.DATE | Date | IN | When price becomes effective |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Item Validation**

```unibasic
* Read and lock inventory record
CALL DB.CONNECT('OPEN.FILE', 'INVENTORY', F.INVENTORY, ERROR)

READU ITEM.REC FROM F.INVENTORY, ITEM.ID ELSE
   ERROR.CODE = ERR.NOT.FOUND
   ERROR.MSG = 'Item ':ITEM.ID:' not found'
   RETURN
END

* Get current price information
CURRENT.PRICE = ITEM.REC<SELL.PRICE>
ITEM.COST = ITEM.REC<AVG.COST>
ITEM.DESC = ITEM.REC<DESCRIPTION>

* Validate new price is positive
IF NEW.PRICE <= 0 THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Price must be greater than zero'
   RELEASE F.INVENTORY, ITEM.ID
   RETURN
END

* Round to 2 decimals
NEW.PRICE = INT(NEW.PRICE * 100) / 100
```

**2. Calculate Price Change Percentage**

```unibasic
* Calculate percentage change
IF CURRENT.PRICE > 0 THEN
   PRICE.DIFF = NEW.PRICE - CURRENT.PRICE
   CHANGE.PCT = (PRICE.DIFF / CURRENT.PRICE) * 100
   CHANGE.PCT = INT(CHANGE.PCT * 100) / 100
END ELSE
   * First time pricing
   CHANGE.PCT = 100
END

* Determine if increase or decrease
IF NEW.PRICE > CURRENT.PRICE THEN
   CHANGE.TYPE = 'INCREASE'
END ELSE IF NEW.PRICE < CURRENT.PRICE THEN
   CHANGE.TYPE = 'DECREASE'
END ELSE
   ERROR.CODE = ERR.INVALID.DATA
   ERROR.MSG = 'New price is same as current price'
   RELEASE F.INVENTORY, ITEM.ID
   RETURN
END
```

**3. Cost Validation (Minimum Markup)**

```unibasic
* Validate minimum markup (prevent selling below cost)
MIN.MARKUP.PCT = 5  ;* Minimum 5% markup

IF ITEM.COST > 0 THEN
   MARKUP.AMT = NEW.PRICE - ITEM.COST
   MARKUP.PCT = (MARKUP.AMT / ITEM.COST) * 100

   IF MARKUP.PCT < MIN.MARKUP.PCT THEN
      ERROR.CODE = ERR.PRICE.TOO.LOW
      ERROR.MSG = 'Price below minimum markup of ':MIN.MARKUP.PCT:'%'
      ERROR.MSG := '. Cost: $':ITEM.COST:', Minimum price: $'
      MIN.PRICE = ITEM.COST * (1 + (MIN.MARKUP.PCT / 100))
      ERROR.MSG := FMT(MIN.PRICE, '10.2R')

      RELEASE F.INVENTORY, ITEM.ID
      RETURN
   END
END
```

**4. Authorization Level Check**

```unibasic
* Determine required authorization level based on change percentage
ABS.CHANGE.PCT = ABS(CHANGE.PCT)

BEGIN CASE
   CASE ABS.CHANGE.PCT >= 50
      REQUIRED.LEVEL = 10
      LEVEL.DESC = 'Executive approval'

   CASE ABS.CHANGE.PCT >= 25
      REQUIRED.LEVEL = 9
      LEVEL.DESC = 'Regional Manager approval'

   CASE ABS.CHANGE.PCT >= 10
      REQUIRED.LEVEL = 7
      LEVEL.DESC = 'Store Manager approval'

   CASE 1
      REQUIRED.LEVEL = 5
      LEVEL.DESC = 'Department Manager approval'
END CASE

* Check user's authorization level
USER.LEVEL = SYSTEM<USER.AUTH.LEVEL>

IF USER.LEVEL < REQUIRED.LEVEL THEN
   ERROR.CODE = ERR.INSUFFICIENT.AUTH
   ERROR.MSG = 'Price change of ':FMT(ABS.CHANGE.PCT, '10.2R'):'%'
   ERROR.MSG := ' requires ':LEVEL.DESC:' (Level ':REQUIRED.LEVEL:')'
   ERROR.MSG := '. Your level: ':USER.LEVEL

   RELEASE F.INVENTORY, ITEM.ID
   RETURN
END
```

**5. Reason Code Validation**

```unibasic
* Validate reason code
VALID.REASONS = 'MARKDOWN':VM:'MARKUP':VM:'CLEARANCE':VM:'SEASONAL'
VALID.REASONS := VM:'PROMOTIONAL':VM:'COST.CHANGE':VM:'COMPETITIVE'
VALID.REASONS := VM:'CORRECTION':VM:'OTHER'

IF NOT(REASON MATCHES VALID.REASONS) THEN
   ERROR.CODE = ERR.INVALID.DATA
   ERROR.MSG = 'Invalid reason code. Valid: MARKDOWN, MARKUP, CLEARANCE, etc.'
   RELEASE F.INVENTORY, ITEM.ID
   RETURN
END
```

**6. Effective Date Validation**

```unibasic
* Validate effective date
IF EFFECTIVE.DATE EQ '' THEN
   EFFECTIVE.DATE = DATE()  ;* Default to today
END

IF EFFECTIVE.DATE < DATE() THEN
   ERROR.CODE = ERR.INVALID.DATE
   ERROR.MSG = 'Effective date cannot be in the past'
   RELEASE F.INVENTORY, ITEM.ID
   RETURN
END

* Future-dated price changes are allowed (for scheduled changes)
IF EFFECTIVE.DATE > DATE() THEN
   SCHEDULED.FLAG = TRUE
   PRINT 'Price change scheduled for ':OCONV(EFFECTIVE.DATE, 'D2/MDY')
END ELSE
   SCHEDULED.FLAG = FALSE
END
```

**7. Record Price History**

```unibasic
* Add to price history (multivalued fields)
HIST.COUNT = DCOUNT(ITEM.REC<PRICE.HIST.DATES>, VM)

IF HIST.COUNT EQ 0 THEN
   HIST.COUNT = 1
ELSE
   HIST.COUNT = HIST.COUNT + 1
END

ITEM.REC<PRICE.HIST.DATES, HIST.COUNT> = DATE()
ITEM.REC<PRICE.HIST.PRICES, HIST.COUNT> = CURRENT.PRICE
ITEM.REC<PRICE.HIST.NEW.PRICES, HIST.COUNT> = NEW.PRICE
ITEM.REC<PRICE.HIST.REASONS, HIST.COUNT> = REASON
ITEM.REC<PRICE.HIST.USERS, HIST.COUNT> = USER.ID
ITEM.REC<PRICE.HIST.AUTH.LEVELS, HIST.COUNT> = USER.LEVEL
ITEM.REC<PRICE.HIST.EFFECTIVE, HIST.COUNT> = EFFECTIVE.DATE
```

**8. Update Item Price**

```unibasic
* Update price (if effective today)
IF SCHEDULED.FLAG THEN
   * Don't update price yet - scheduled for future
   ITEM.REC<SCHEDULED.PRICE> = NEW.PRICE
   ITEM.REC<SCHEDULED.PRICE.DATE> = EFFECTIVE.DATE
END ELSE
   * Update price immediately
   ITEM.REC<SELL.PRICE> = NEW.PRICE
   ITEM.REC<LAST.PRICE.CHANGE> = DATE()
END

* Update modified tracking
ITEM.REC<MODIFIED.BY> = USER.ID
ITEM.REC<MODIFIED.DATE> = DATE()
ITEM.REC<MODIFIED.TIME> = TIME()

* Write updated record
WRITE ITEM.REC ON F.INVENTORY, ITEM.ID
```

**9. Email Notification for Significant Changes**

```unibasic
* Send email notification for significant price changes
IF ABS.CHANGE.PCT >= 25 THEN
   EMAIL.TO = 'merchandising@retailpos.com'
   EMAIL.SUBJECT = 'Significant Price Change Alert'

   EMAIL.BODY = 'Item: ':ITEM.ID:' - ':ITEM.DESC
   EMAIL.BODY := CHAR(10):'Old Price: $':FMT(CURRENT.PRICE, '10.2R')
   EMAIL.BODY := CHAR(10):'New Price: $':FMT(NEW.PRICE, '10.2R')
   EMAIL.BODY := CHAR(10):'Change: ':FMT(CHANGE.PCT, '10.2R'):'%'
   EMAIL.BODY := CHAR(10):'Reason: ':REASON
   EMAIL.BODY := CHAR(10):'Changed by: ':USER.ID:' (Level ':USER.LEVEL:')'
   EMAIL.BODY := CHAR(10):'Effective: ':OCONV(EFFECTIVE.DATE, 'D2/MDY')

   CALL UTILS.COMMON('SEND.EMAIL', EMAIL.TO, EMAIL.SUBJECT, EMAIL.BODY, RESULT, EMAIL.ERROR)
END
```

**10. Audit Trail**

```unibasic
* Create audit trail entry
AUDIT.MSG = 'Price change for ':ITEM.ID:' (':ITEM.DESC:')'
AUDIT.MSG := ' from $':CURRENT.PRICE:' to $':NEW.PRICE
AUDIT.MSG := ' (':CHANGE.TYPE:' ':ABS.CHANGE.PCT:'%)'
AUDIT.MSG := ' Reason: ':REASON
AUDIT.MSG := ', Effective: ':OCONV(EFFECTIVE.DATE, 'D2/MDY')

CALL UTILS.COMMON('WRITE.AUDIT', 'PRICE.CHANGE', ITEM.ID, AUDIT.MSG, DUMMY, DUMMY)

ERROR.CODE = 0
IF SCHEDULED.FLAG THEN
   ERROR.MSG = 'Price change scheduled successfully'
END ELSE
   ERROR.MSG = 'Price changed successfully'
END
```

#### Usage Example

```unibasic
* Change item price with markdown
ITEM.ID = 'ITEM123456'
NEW.PRICE = 49.99
REASON = 'CLEARANCE'
EFFECTIVE.DATE = DATE()  ;* Immediate

CALL PRICE.CHANGE(ITEM.ID, NEW.PRICE, REASON, EFFECTIVE.DATE, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   PRINT 'Price updated successfully'
   PRINT ERROR.MSG
END ELSE
   PRINT 'Error: ':ERROR.MSG
END

* Schedule future price change
ITEM.ID = 'ITEM789012'
NEW.PRICE = 39.99
REASON = 'SEASONAL'
EFFECTIVE.DATE = DATE() + 30  ;* 30 days from now

CALL PRICE.CHANGE(ITEM.ID, NEW.PRICE, REASON, EFFECTIVE.DATE, ERROR.CODE, ERROR.MSG)
```

---

### 5. PRICE.MARKDOWN - Mass Markdown by Category

**Location:** `BP/PRICE.MARKDOWN`
**Lines of Code:** ~900 lines
**Purpose:** Apply percentage markdown to entire product categories

#### Function Signature

```unibasic
SUBROUTINE PRICE.MARKDOWN(CATEGORY, MARKDOWN.PCT, REASON, EFFECTIVE.DATE, SUMMARY, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| CATEGORY | String | IN | Category code to markdown |
| MARKDOWN.PCT | Decimal | IN | Markdown percentage (positive number) |
| REASON | String | IN | Reason code |
| EFFECTIVE.DATE | Date | IN | Effective date |
| SUMMARY | Dynamic Array | OUT | Summary of changes made |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Input Validation**

```unibasic
* Validate markdown percentage
IF MARKDOWN.PCT <= 0 OR MARKDOWN.PCT > 90 THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Markdown percentage must be between 1 and 90'
   RETURN
END

* Round to 2 decimals
MARKDOWN.PCT = INT(MARKDOWN.PCT * 100) / 100
```

**2. Authorization Check for Mass Markdown**

```unibasic
* Determine required authorization level for mass markdown
BEGIN CASE
   CASE MARKDOWN.PCT >= 40
      REQUIRED.LEVEL = 10
      LEVEL.DESC = 'Executive approval required'

   CASE MARKDOWN.PCT >= 20
      REQUIRED.LEVEL = 9
      LEVEL.DESC = 'Regional Manager approval required'

   CASE 1
      REQUIRED.LEVEL = 7
      LEVEL.DESC = 'Store Manager approval required'
END CASE

* Check user authorization
USER.LEVEL = SYSTEM<USER.AUTH.LEVEL>

IF USER.LEVEL < REQUIRED.LEVEL THEN
   ERROR.CODE = ERR.INSUFFICIENT.AUTH
   ERROR.MSG = 'Mass markdown of ':MARKDOWN.PCT:'% requires ':LEVEL.DESC
   ERROR.MSG := ' (Level ':REQUIRED.LEVEL:'). Your level: ':USER.LEVEL
   RETURN
END
```

**3. Select Items in Category**

```unibasic
* Select all active items in category
SELECT.CMD = 'SELECT INVENTORY WITH CATEGORY = "':CATEGORY:'"'
SELECT.CMD := ' AND STATUS = "ACTIVE"'

EXECUTE SELECT.CMD

ITEM.COUNT = @SELECTED

IF ITEM.COUNT EQ 0 THEN
   ERROR.CODE = ERR.NOT.FOUND
   ERROR.MSG = 'No active items found in category ':CATEGORY
   RETURN
END

PRINT 'Found ':ITEM.COUNT:' items in category ':CATEGORY
PRINT 'Applying ':MARKDOWN.PCT:'% markdown...'
```

**4. Process Each Item**

```unibasic
* Initialize summary counters
ITEMS.PROCESSED = 0
ITEMS.CHANGED = 0
ITEMS.SKIPPED = 0
TOTAL.OLD.VALUE = 0
TOTAL.NEW.VALUE = 0
SKIPPED.REASONS = ''

LOOP
   READNEXT ITEM.ID ELSE EXIT

   READU ITEM.REC FROM F.INVENTORY, ITEM.ID THEN
      * Get current price and cost
      CURRENT.PRICE = ITEM.REC<SELL.PRICE>
      ITEM.COST = ITEM.REC<AVG.COST>

      * Calculate new price
      MARKDOWN.MULTIPLIER = 1 - (MARKDOWN.PCT / 100)
      NEW.PRICE = CURRENT.PRICE * MARKDOWN.MULTIPLIER
      NEW.PRICE = INT(NEW.PRICE * 100) / 100  ;* Round to cents

      * Validate minimum markup
      MIN.MARKUP.PCT = 5
      SKIP.ITEM = FALSE

      IF ITEM.COST > 0 THEN
         MIN.PRICE = ITEM.COST * (1 + (MIN.MARKUP.PCT / 100))

         IF NEW.PRICE < MIN.PRICE THEN
            * Would go below cost - skip this item
            SKIP.ITEM = TRUE
            SKIP.REASON = 'Below minimum markup'
            ITEMS.SKIPPED = ITEMS.SKIPPED + 1
            SKIPPED.REASONS<ITEMS.SKIPPED> = ITEM.ID:': ':SKIP.REASON
         END
      END

      IF NOT(SKIP.ITEM) THEN
         * Update price history
         HIST.COUNT = DCOUNT(ITEM.REC<PRICE.HIST.DATES>, VM) + 1
         ITEM.REC<PRICE.HIST.DATES, HIST.COUNT> = DATE()
         ITEM.REC<PRICE.HIST.PRICES, HIST.COUNT> = CURRENT.PRICE
         ITEM.REC<PRICE.HIST.NEW.PRICES, HIST.COUNT> = NEW.PRICE
         ITEM.REC<PRICE.HIST.REASONS, HIST.COUNT> = REASON
         ITEM.REC<PRICE.HIST.USERS, HIST.COUNT> = USER.ID
         ITEM.REC<PRICE.HIST.AUTH.LEVELS, HIST.COUNT> = USER.LEVEL
         ITEM.REC<PRICE.HIST.EFFECTIVE, HIST.COUNT> = EFFECTIVE.DATE

         * Update price
         IF EFFECTIVE.DATE > DATE() THEN
            * Scheduled
            ITEM.REC<SCHEDULED.PRICE> = NEW.PRICE
            ITEM.REC<SCHEDULED.PRICE.DATE> = EFFECTIVE.DATE
         END ELSE
            * Immediate
            ITEM.REC<SELL.PRICE> = NEW.PRICE
            ITEM.REC<LAST.PRICE.CHANGE> = DATE()
         END

         * Update modified tracking
         ITEM.REC<MODIFIED.BY> = USER.ID
         ITEM.REC<MODIFIED.DATE> = DATE()

         WRITE ITEM.REC ON F.INVENTORY, ITEM.ID

         * Update summary
         ITEMS.CHANGED = ITEMS.CHANGED + 1
         TOTAL.OLD.VALUE = TOTAL.OLD.VALUE + CURRENT.PRICE
         TOTAL.NEW.VALUE = TOTAL.NEW.VALUE + NEW.PRICE
      END ELSE
         RELEASE F.INVENTORY, ITEM.ID
      END

      ITEMS.PROCESSED = ITEMS.PROCESSED + 1
   END ELSE
      * Couldn't read/lock item
      ITEMS.SKIPPED = ITEMS.SKIPPED + 1
   END
REPEAT
```

**5. Build Summary Report**

```unibasic
* Build summary report
SUMMARY = ''
SUMMARY<1> = 'MASS MARKDOWN SUMMARY'
SUMMARY<2> = '====================='
SUMMARY<3> = 'Category: ':CATEGORY
SUMMARY<4> = 'Markdown: ':MARKDOWN.PCT:'%'
SUMMARY<5> = 'Reason: ':REASON
SUMMARY<6> = 'Effective: ':OCONV(EFFECTIVE.DATE, 'D2/MDY')
SUMMARY<7> = ''
SUMMARY<8> = 'Items Processed: ':ITEMS.PROCESSED
SUMMARY<9> = 'Items Changed: ':ITEMS.CHANGED
SUMMARY<10> = 'Items Skipped: ':ITEMS.SKIPPED
SUMMARY<11> = ''

IF ITEMS.CHANGED > 0 THEN
   AVG.OLD = TOTAL.OLD.VALUE / ITEMS.CHANGED
   AVG.NEW = TOTAL.NEW.VALUE / ITEMS.CHANGED
   TOTAL.REDUCTION = TOTAL.OLD.VALUE - TOTAL.NEW.VALUE

   SUMMARY<12> = 'Average Old Price: $':FMT(AVG.OLD, '10.2R')
   SUMMARY<13> = 'Average New Price: $':FMT(AVG.NEW, '10.2R')
   SUMMARY<14> = 'Total Price Reduction: $':FMT(TOTAL.REDUCTION, '10.2R')
END

IF ITEMS.SKIPPED > 0 THEN
   SUMMARY<15> = ''
   SUMMARY<16> = 'Skipped Items:'
   FOR I = 1 TO ITEMS.SKIPPED
      SUMMARY<16+I> = '  ':SKIPPED.REASONS<I>
   NEXT I
END
```

**6. Email Notification**

```unibasic
* Send email notification
EMAIL.TO = 'merchandising@retailpos.com'
EMAIL.SUBJECT = 'Mass Markdown Applied: ':CATEGORY

EMAIL.BODY = ''
FOR I = 1 TO DCOUNT(SUMMARY, AM)
   EMAIL.BODY := SUMMARY<I>:CHAR(10)
NEXT I

CALL UTILS.COMMON('SEND.EMAIL', EMAIL.TO, EMAIL.SUBJECT, EMAIL.BODY, RESULT, EMAIL.ERROR)
```

**7. Audit Trail**

```unibasic
* Create audit trail
AUDIT.MSG = 'Mass markdown of ':MARKDOWN.PCT:'% applied to category ':CATEGORY
AUDIT.MSG := ', ':ITEMS.CHANGED:' items changed'
AUDIT.MSG := ', Effective: ':OCONV(EFFECTIVE.DATE, 'D2/MDY')

CALL UTILS.COMMON('WRITE.AUDIT', 'PRICE.MARKDOWN', CATEGORY, AUDIT.MSG, DUMMY, DUMMY)

ERROR.CODE = 0
ERROR.MSG = 'Mass markdown completed successfully'
```

#### Usage Example

```unibasic
* Apply 30% markdown to entire category
CATEGORY = 'WINTER.CLOTHING'
MARKDOWN.PCT = 30
REASON = 'SEASONAL.CLEARANCE'
EFFECTIVE.DATE = DATE()  ;* Immediate

CALL PRICE.MARKDOWN(CATEGORY, MARKDOWN.PCT, REASON, EFFECTIVE.DATE, SUMMARY, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   * Print summary
   FOR I = 1 TO DCOUNT(SUMMARY, AM)
      PRINT SUMMARY<I>
   NEXT I
END ELSE
   PRINT 'Error: ':ERROR.MSG
END

* Example output:
* MASS MARKDOWN SUMMARY
* =====================
* Category: WINTER.CLOTHING
* Markdown: 30%
* Reason: SEASONAL.CLEARANCE
* Effective: 10/06/2025
*
* Items Processed: 127
* Items Changed: 121
* Items Skipped: 6
*
* Average Old Price: $45.67
* Average New Price: $31.97
* Total Price Reduction: $1,657.47
*
* Skipped Items:
*   ITEM98765: Below minimum markup
*   ITEM87654: Below minimum markup
*   ...
```

---

## AUTHORIZATION AND SECURITY

### Security Features

**1. Luhn Algorithm Validation**
- Gift cards protected with mathematical check digit
- Prevents manual entry errors
- Detects transposition and single-digit errors

**2. Authorization Levels**
- Price changes require appropriate management approval
- Mass markdowns require higher authorization
- Email alerts for significant changes

**3. Audit Trails**
- Complete price change history
- All gift card transactions logged
- User and timestamp tracking

**4. Balance Limits**
- Gift card maximum balance: $2,000
- Maximum load per transaction: $500
- Minimum initial load: $5

**5. Expiration Management**
- 3-year expiration from activation
- Automatic status changes
- Reload capability after depletion

---

## SUMMARY

The Gift Card and Price Management systems provide:

### Gift Card System (~1,850 lines)
- **Complete lifecycle:** Creation, loading, redemption
- **Luhn validation:** Secure 16-digit card numbers
- **Balance management:** Load limits and balance tracking
- **Status management:** ACTIVE, SUSPENDED, EXPIRED, DEPLETED
- **Transaction history:** Complete audit trail
- **Customer linkage:** Optional customer association
- **Monthly statistics:** Reporting and analytics

### Price Management System (~1,750 lines)
- **Individual price changes:** With authorization workflows
- **Mass markdowns:** Category-based batch processing
- **Future-dated pricing:** Scheduled price changes
- **Cost validation:** Minimum markup enforcement
- **Price history:** Complete change tracking
- **Authorization matrix:** Percentage-based approval levels
- **Email notifications:** Alerts for significant changes

**Total Module Size:** ~3,600 lines of production-ready UniBasic code

---

*Part 6 of 8 - Next: Cash Management and Reporting*
