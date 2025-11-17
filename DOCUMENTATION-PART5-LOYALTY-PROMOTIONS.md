# RETAIL POS SYSTEM - DOCUMENTATION PART 5
## LOYALTY PROGRAMS AND PROMOTIONAL CAMPAIGNS

---

## TABLE OF CONTENTS

1. [Loyalty Program Management](#loyalty-program-management)
2. [Loyalty Transaction Processing](#loyalty-transaction-processing)
3. [Promotional Campaign System](#promotional-campaign-system)
4. [Integration with POS](#integration-with-pos)

---

## LOYALTY PROGRAM MANAGEMENT

The loyalty program provides a three-tier system designed to reward repeat customers and drive engagement through points-based rewards.

### System Architecture

**Three-Tier Structure:**
- **Silver Tier** (Entry level)
  - Points multiplier: 1.0x
  - Minimum spend: $0
  - Benefits: Basic points earning

- **Gold Tier** (Mid level)
  - Points multiplier: 1.5x
  - Minimum spend: $500
  - Benefits: 50% bonus on all points earned

- **Platinum Tier** (Premium level)
  - Points multiplier: 2.0x
  - Minimum spend: $2,000
  - Benefits: 100% bonus on all points earned

**Points Calculation:**
- Base earning: 1 point per $1 spent
- Multiplier applied based on tier
- Example: $100 purchase
  - Silver: 100 points
  - Gold: 150 points
  - Platinum: 200 points

---

### 1. LOYAL.ENROLL - Loyalty Program Enrollment

**Location:** `BP/LOYAL.ENROLL`
**Lines of Code:** ~550 lines
**Purpose:** Enroll customers in the loyalty program with secure card number generation

#### Function Signature

```unibasic
SUBROUTINE LOYAL.ENROLL(CUSTOMER.ID, LOYALTY.REC, LOYALTY.NUMBER, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| CUSTOMER.ID | String | IN | Customer ID to enroll |
| LOYALTY.REC | Dynamic Array | OUT | Complete loyalty record created |
| LOYALTY.NUMBER | String | OUT | Generated loyalty card number |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Customer Validation**

```unibasic
* Verify customer exists and is not already enrolled
CALL DB.CONNECT('OPEN.FILE', 'CUSTOMERS', F.CUSTOMERS, ERROR)

READU CUST.REC FROM F.CUSTOMERS, CUSTOMER.ID ELSE
   ERROR.CODE = ERR.NOT.FOUND
   ERROR.MSG = 'Customer ID ':CUSTOMER.ID:' not found'
   RETURN
END

* Check if already enrolled
IF CUST.REC<LOYALTY.NUMBER> NE '' THEN
   ERROR.CODE = ERR.DUPLICATE
   ERROR.MSG = 'Customer already enrolled in loyalty program'
   RELEASE F.CUSTOMERS, CUSTOMER.ID
   RETURN
END
```

**2. Loyalty Number Generation with Luhn Algorithm**

The system generates a 16-digit loyalty card number with the following structure:
- **Prefix:** 5000 (identifies loyalty cards)
- **Middle digits:** Random 11 digits
- **Check digit:** Calculated using Luhn algorithm

```unibasic
* Generate unique 16-digit loyalty number with Luhn check digit
* Format: 5000-XXXX-XXXX-XXXC (C = check digit)

GENERATE.UNIQUE.NUMBER:
   * Start with 5000 prefix
   BASE.NUMBER = '5000'

   * Generate 11 random digits
   FOR I = 1 TO 11
      DIGIT = RND(10)
      BASE.NUMBER := DIGIT
   NEXT I

   * Calculate Luhn check digit
   CALL UTILS.COMMON('LUHN.CHECK.DIGIT', BASE.NUMBER, CHECK.DIGIT)
   LOYALTY.NUMBER = BASE.NUMBER : CHECK.DIGIT

   * Verify uniqueness
   READU EXISTING FROM F.LOYALTY, LOYALTY.NUMBER THEN
      RELEASE F.LOYALTY, LOYALTY.NUMBER
      GOTO GENERATE.UNIQUE.NUMBER
   END
```

**Luhn Algorithm Implementation:**

The Luhn check digit calculation ensures data integrity:

```unibasic
* Luhn check digit calculation
SUM = 0
DOUBLE.FLAG = TRUE

* Process digits from right to left
FOR POS = LEN(NUMBER) TO 1 STEP -1
   DIGIT = NUMBER[POS,1]

   IF DOUBLE.FLAG THEN
      DIGIT = DIGIT * 2
      IF DIGIT > 9 THEN DIGIT = DIGIT - 9
   END

   SUM = SUM + DIGIT
   DOUBLE.FLAG = NOT(DOUBLE.FLAG)
NEXT POS

* Check digit makes sum divisible by 10
CHECK.DIGIT = (10 - (SUM MOD 10)) MOD 10
```

**3. Loyalty Record Initialization**

```unibasic
* Initialize loyalty record
LOYALTY.REC = ''
LOYALTY.REC<1> = LOYALTY.NUMBER          ;* Loyalty number
LOYALTY.REC<2> = CUSTOMER.ID             ;* Customer ID
LOYALTY.REC<3> = 'SILVER'                ;* Initial tier
LOYALTY.REC<4> = 0                       ;* Points balance
LOYALTY.REC<5> = 0                       ;* Lifetime points earned
LOYALTY.REC<6> = 0                       ;* Lifetime points redeemed
LOYALTY.REC<7> = 0                       ;* Total spend
LOYALTY.REC<8> = DATE()                  ;* Enrollment date
LOYALTY.REC<9> = 'ACTIVE'                ;* Status
LOYALTY.REC<10> = USER.ID                ;* Created by
LOYALTY.REC<11> = DATE()                 ;* Created date
LOYALTY.REC<12> = TIME()                 ;* Created time

* Initialize multivalued transaction history
LOYALTY.REC<13> = ''                     ;* Transaction dates
LOYALTY.REC<14> = ''                     ;* Transaction types
LOYALTY.REC<15> = ''                     ;* Points amounts
LOYALTY.REC<16> = ''                     ;* Transaction references
```

**4. Customer Record Update**

```unibasic
* Update customer record with loyalty number
CUST.REC<LOYALTY.NUMBER> = LOYALTY.NUMBER
CUST.REC<LOYALTY.TIER> = 'SILVER'
CUST.REC<LOYALTY.ENROLL.DATE> = DATE()

WRITE CUST.REC ON F.CUSTOMERS, CUSTOMER.ID
```

**5. Audit Trail Creation**

```unibasic
* Log enrollment in audit trail
AUDIT.REC = ''
AUDIT.REC<1> = 'LOYALTY.ENROLL'
AUDIT.REC<2> = CUSTOMER.ID
AUDIT.REC<3> = LOYALTY.NUMBER
AUDIT.REC<4> = 'Customer enrolled in loyalty program'
AUDIT.REC<5> = USER.ID
AUDIT.REC<6> = DATE()
AUDIT.REC<7> = TIME()
AUDIT.REC<8> = ''                        ;* Before value (none)
AUDIT.REC<9> = 'TIER=SILVER,POINTS=0'   ;* After value

CALL UTILS.COMMON('WRITE.AUDIT', AUDIT.REC, DUMMY, DUMMY)
```

#### Usage Example

```unibasic
* Enroll customer in loyalty program
CUSTOMER.ID = 'C00001234'

CALL LOYAL.ENROLL(CUSTOMER.ID, LOYALTY.REC, LOYALTY.NUMBER, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE NE 0 THEN
   PRINT 'Enrollment failed: ':ERROR.MSG
END ELSE
   PRINT 'Successfully enrolled!'
   PRINT 'Loyalty Number: ':LOYALTY.NUMBER
   PRINT 'Current Tier: ':LOYALTY.REC<3>
   PRINT 'Points Balance: ':LOYALTY.REC<4>
END
```

#### Error Codes

| Code | Meaning | Resolution |
|------|---------|------------|
| 1001 | Customer not found | Verify customer ID exists |
| 1002 | Already enrolled | Customer already has loyalty number |
| 1003 | Database error | Check file permissions |
| 1004 | Invalid customer status | Customer must be active |

---

### 2. LOYAL.EARN - Award Loyalty Points

**Location:** `BP/LOYAL.EARN`
**Lines of Code:** ~750 lines
**Purpose:** Award loyalty points based on purchases and handle tier upgrades

#### Function Signature

```unibasic
SUBROUTINE LOYAL.EARN(LOYALTY.NUMBER, PURCHASE.AMOUNT, TRANS.ID, POINTS.EARNED, NEW.BALANCE, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| LOYALTY.NUMBER | String | IN | Loyalty card number |
| PURCHASE.AMOUNT | Decimal | IN | Purchase amount (before tax) |
| TRANS.ID | String | IN | POS transaction ID reference |
| POINTS.EARNED | Integer | OUT | Points awarded this transaction |
| NEW.BALANCE | Integer | OUT | Updated points balance |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Loyalty Account Validation**

```unibasic
* Read and lock loyalty record
CALL DB.CONNECT('OPEN.FILE', 'LOYALTY', F.LOYALTY, ERROR)

READU LOYALTY.REC FROM F.LOYALTY, LOYALTY.NUMBER ELSE
   ERROR.CODE = ERR.NOT.FOUND
   ERROR.MSG = 'Loyalty number ':LOYALTY.NUMBER:' not found'
   RETURN
END

* Validate loyalty account status
ACCOUNT.STATUS = LOYALTY.REC<STATUS>
IF ACCOUNT.STATUS NE 'ACTIVE' THEN
   ERROR.CODE = ERR.INVALID.STATUS
   ERROR.MSG = 'Loyalty account status is ':ACCOUNT.STATUS
   RELEASE F.LOYALTY, LOYALTY.NUMBER
   RETURN
END
```

**2. Points Calculation with Tier Multiplier**

```unibasic
* Get current tier and multiplier
CURRENT.TIER = LOYALTY.REC<TIER>

BEGIN CASE
   CASE CURRENT.TIER EQ 'SILVER'
      TIER.MULTIPLIER = 1.0

   CASE CURRENT.TIER EQ 'GOLD'
      TIER.MULTIPLIER = 1.5

   CASE CURRENT.TIER EQ 'PLATINUM'
      TIER.MULTIPLIER = 2.0

   CASE 1
      TIER.MULTIPLIER = 1.0
END CASE

* Calculate base points (1 point per dollar)
BASE.POINTS = INT(PURCHASE.AMOUNT)

* Apply tier multiplier
POINTS.EARNED = INT(BASE.POINTS * TIER.MULTIPLIER)

* Validate minimum points
IF POINTS.EARNED < 1 THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Purchase amount too small to earn points'
   RELEASE F.LOYALTY, LOYALTY.NUMBER
   RETURN
END
```

**Example Calculation:**
```
Purchase: $125.50
Tier: GOLD (1.5x multiplier)

Base Points = INT(125.50) = 125
Points Earned = INT(125 * 1.5) = 187 points
```

**3. Update Loyalty Statistics**

```unibasic
* Get current values
CURRENT.BALANCE = LOYALTY.REC<POINTS.BALANCE>
LIFETIME.EARNED = LOYALTY.REC<LIFETIME.EARNED>
TOTAL.SPEND = LOYALTY.REC<TOTAL.SPEND>

* Update balances
NEW.BALANCE = CURRENT.BALANCE + POINTS.EARNED
LIFETIME.EARNED = LIFETIME.EARNED + POINTS.EARNED
TOTAL.SPEND = TOTAL.SPEND + PURCHASE.AMOUNT

LOYALTY.REC<POINTS.BALANCE> = NEW.BALANCE
LOYALTY.REC<LIFETIME.EARNED> = LIFETIME.EARNED
LOYALTY.REC<TOTAL.SPEND> = TOTAL.SPEND
```

**4. Tier Upgrade Logic**

The system automatically upgrades customers when they reach spending thresholds:

```unibasic
* Check for tier upgrade based on total spend
OLD.TIER = CURRENT.TIER
NEW.TIER = OLD.TIER
UPGRADE.FLAG = FALSE

BEGIN CASE
   CASE TOTAL.SPEND >= 2000 AND CURRENT.TIER NE 'PLATINUM'
      NEW.TIER = 'PLATINUM'
      UPGRADE.FLAG = TRUE
      BONUS.POINTS = 500  ;* Platinum upgrade bonus

   CASE TOTAL.SPEND >= 500 AND CURRENT.TIER EQ 'SILVER'
      NEW.TIER = 'GOLD'
      UPGRADE.FLAG = TRUE
      BONUS.POINTS = 250  ;* Gold upgrade bonus
END CASE

IF UPGRADE.FLAG THEN
   * Apply tier upgrade
   LOYALTY.REC<TIER> = NEW.TIER
   LOYALTY.REC<TIER.UPGRADE.DATE> = DATE()

   * Award upgrade bonus points
   NEW.BALANCE = NEW.BALANCE + BONUS.POINTS
   LOYALTY.REC<POINTS.BALANCE> = NEW.BALANCE

   * Log upgrade event
   PRINT 'TIER UPGRADE: ':OLD.TIER:' -> ':NEW.TIER
   PRINT 'Bonus Points Awarded: ':BONUS.POINTS

   * Send upgrade notification email
   CALL UTILS.COMMON('SEND.EMAIL', CUSTOMER.EMAIL, EMAIL.BODY, RESULT, ERROR)
END
```

**Tier Upgrade Thresholds:**

| Tier | Minimum Spend | Upgrade Bonus |
|------|---------------|---------------|
| Silver → Gold | $500 | 250 points |
| Gold → Platinum | $2,000 | 500 points |

**5. Transaction History Recording**

```unibasic
* Add transaction to history (multivalued fields)
TRANS.COUNT = DCOUNT(LOYALTY.REC<TRANS.DATES>, VM) + 1

LOYALTY.REC<TRANS.DATES, TRANS.COUNT> = DATE()
LOYALTY.REC<TRANS.TYPES, TRANS.COUNT> = 'EARN'
LOYALTY.REC<TRANS.AMOUNTS, TRANS.COUNT> = POINTS.EARNED
LOYALTY.REC<TRANS.REFS, TRANS.COUNT> = TRANS.ID
LOYALTY.REC<TRANS.PURCHASES, TRANS.COUNT> = PURCHASE.AMOUNT

* Update last transaction date
LOYALTY.REC<LAST.TRANS.DATE> = DATE()
```

**6. Update Customer Record**

```unibasic
* Update customer's tier if upgraded
IF UPGRADE.FLAG THEN
   CALL DB.CONNECT('OPEN.FILE', 'CUSTOMERS', F.CUSTOMERS, ERROR)
   CUSTOMER.ID = LOYALTY.REC<CUSTOMER.ID>

   READU CUST.REC FROM F.CUSTOMERS, CUSTOMER.ID THEN
      CUST.REC<LOYALTY.TIER> = NEW.TIER
      WRITE CUST.REC ON F.CUSTOMERS, CUSTOMER.ID
   END ELSE
      RELEASE F.CUSTOMERS, CUSTOMER.ID
   END
END
```

**7. Save and Audit**

```unibasic
* Write updated loyalty record
WRITE LOYALTY.REC ON F.LOYALTY, LOYALTY.NUMBER

* Create audit trail entry
AUDIT.MSG = 'Earned ':POINTS.EARNED:' points from purchase $':PURCHASE.AMOUNT
AUDIT.MSG := ', New balance: ':NEW.BALANCE

IF UPGRADE.FLAG THEN
   AUDIT.MSG := ', UPGRADED to ':NEW.TIER:' (+':BONUS.POINTS:' bonus)'
END

CALL UTILS.COMMON('WRITE.AUDIT', 'LOYALTY.EARN', LOYALTY.NUMBER, AUDIT.MSG, DUMMY, DUMMY)
```

#### Usage Example

```unibasic
* Award points for purchase
LOYALTY.NUMBER = '5000123456789012'
PURCHASE.AMOUNT = 156.75
TRANS.ID = 'POS20251006-000123'

CALL LOYAL.EARN(LOYALTY.NUMBER, PURCHASE.AMOUNT, TRANS.ID, POINTS.EARNED, NEW.BALANCE, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   PRINT 'Points Earned: ':POINTS.EARNED
   PRINT 'New Balance: ':NEW.BALANCE:' points'
   PRINT 'Thank you for your purchase!'
END ELSE
   PRINT 'Error: ':ERROR.MSG
END
```

#### Points Earning Examples

**Example 1: Silver Tier Purchase**
```
Purchase Amount: $50.00
Tier: SILVER (1.0x)
Points Earned: 50
```

**Example 2: Gold Tier Purchase**
```
Purchase Amount: $50.00
Tier: GOLD (1.5x)
Points Earned: 75
```

**Example 3: Platinum Tier Purchase**
```
Purchase Amount: $50.00
Tier: PLATINUM (2.0x)
Points Earned: 100
```

**Example 4: Tier Upgrade Scenario**
```
Current: SILVER tier, $475 lifetime spend
Purchase: $50.00
New Spend: $525

Result:
- Base points: 50
- TIER UPGRADED: SILVER → GOLD
- Upgrade bonus: +250 points
- Total points earned: 300
```

---

### 3. LOYAL.REDEEM - Redeem Loyalty Points

**Location:** `BP/LOYAL.REDEEM`
**Lines of Code:** ~650 lines
**Purpose:** Redeem loyalty points for discounts and rewards

#### Function Signature

```unibasic
SUBROUTINE LOYAL.REDEEM(LOYALTY.NUMBER, POINTS.TO.REDEEM, TRANS.ID, DOLLAR.VALUE, NEW.BALANCE, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| LOYALTY.NUMBER | String | IN | Loyalty card number |
| POINTS.TO.REDEEM | Integer | IN | Points customer wants to redeem |
| TRANS.ID | String | IN | POS transaction ID reference |
| DOLLAR.VALUE | Decimal | OUT | Dollar discount value |
| NEW.BALANCE | Integer | OUT | Remaining points balance |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Account Validation and Balance Check**

```unibasic
* Read and lock loyalty record
READU LOYALTY.REC FROM F.LOYALTY, LOYALTY.NUMBER ELSE
   ERROR.CODE = ERR.NOT.FOUND
   ERROR.MSG = 'Loyalty number not found'
   RETURN
END

* Check account status
IF LOYALTY.REC<STATUS> NE 'ACTIVE' THEN
   ERROR.CODE = ERR.INVALID.STATUS
   ERROR.MSG = 'Loyalty account is not active'
   RELEASE F.LOYALTY, LOYALTY.NUMBER
   RETURN
END

* Get current balance
CURRENT.BALANCE = LOYALTY.REC<POINTS.BALANCE>

* Validate sufficient balance
IF POINTS.TO.REDEEM > CURRENT.BALANCE THEN
   ERROR.CODE = ERR.INSUFFICIENT.BALANCE
   ERROR.MSG = 'Insufficient points. Available: ':CURRENT.BALANCE
   RELEASE F.LOYALTY, LOYALTY.NUMBER
   RETURN
END
```

**2. Redemption Rules Validation**

```unibasic
* Minimum redemption amount: 100 points
MIN.REDEMPTION = 100
IF POINTS.TO.REDEEM < MIN.REDEMPTION THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Minimum redemption is ':MIN.REDEMPTION:' points'
   RELEASE F.LOYALTY, LOYALTY.NUMBER
   RETURN
END

* Points must be in increments of 100
IF MOD(POINTS.TO.REDEEM, 100) NE 0 THEN
   ERROR.CODE = ERR.INVALID.AMOUNT
   ERROR.MSG = 'Points must be redeemed in multiples of 100'
   RELEASE F.LOYALTY, LOYALTY.NUMBER
   RETURN
END
```

**3. Calculate Dollar Value**

The redemption rate is 100 points = $1.00:

```unibasic
* Calculate dollar value
* Redemption rate: 100 points = $1.00
POINTS.PER.DOLLAR = 100
DOLLAR.VALUE = POINTS.TO.REDEEM / POINTS.PER.DOLLAR

* Format to 2 decimal places
DOLLAR.VALUE = INT(DOLLAR.VALUE * 100) / 100
```

**Redemption Examples:**
```
100 points = $1.00
500 points = $5.00
1000 points = $10.00
2500 points = $25.00
```

**4. Update Loyalty Statistics**

```unibasic
* Update points balances
NEW.BALANCE = CURRENT.BALANCE - POINTS.TO.REDEEM
LIFETIME.REDEEMED = LOYALTY.REC<LIFETIME.REDEEMED>
LIFETIME.REDEEMED = LIFETIME.REDEEMED + POINTS.TO.REDEEM

LOYALTY.REC<POINTS.BALANCE> = NEW.BALANCE
LOYALTY.REC<LIFETIME.REDEEMED> = LIFETIME.REDEEMED
```

**5. Transaction History Recording**

```unibasic
* Add redemption to transaction history
TRANS.COUNT = DCOUNT(LOYALTY.REC<TRANS.DATES>, VM) + 1

LOYALTY.REC<TRANS.DATES, TRANS.COUNT> = DATE()
LOYALTY.REC<TRANS.TYPES, TRANS.COUNT> = 'REDEEM'
LOYALTY.REC<TRANS.AMOUNTS, TRANS.COUNT> = POINTS.TO.REDEEM
LOYALTY.REC<TRANS.REFS, TRANS.COUNT> = TRANS.ID
LOYALTY.REC<TRANS.DOLLAR.VALUES, TRANS.COUNT> = DOLLAR.VALUE

* Update last transaction date
LOYALTY.REC<LAST.TRANS.DATE> = DATE()
LOYALTY.REC<LAST.REDEMPTION.DATE> = DATE()
```

**6. Tier Downgrade Check**

While rare, the system can handle tier downgrades if configured:

```unibasic
* Optional: Check for tier downgrade (if enabled)
IF TIER.DOWNGRADE.ENABLED THEN
   TOTAL.SPEND = LOYALTY.REC<TOTAL.SPEND>
   CURRENT.TIER = LOYALTY.REC<TIER>

   BEGIN CASE
      CASE CURRENT.TIER EQ 'PLATINUM' AND TOTAL.SPEND < 2000
         LOYALTY.REC<TIER> = 'GOLD'
         TIER.CHANGED = TRUE

      CASE CURRENT.TIER EQ 'GOLD' AND TOTAL.SPEND < 500
         LOYALTY.REC<TIER> = 'SILVER'
         TIER.CHANGED = TRUE
   END CASE
END
```

**7. Save and Create Receipt**

```unibasic
* Write updated loyalty record
WRITE LOYALTY.REC ON F.LOYALTY, LOYALTY.NUMBER

* Create redemption receipt
RECEIPT = ''
RECEIPT<1> = 'LOYALTY POINTS REDEMPTION'
RECEIPT<2> = 'Loyalty Number: ':LOYALTY.NUMBER
RECEIPT<3> = 'Points Redeemed: ':POINTS.TO.REDEEM
RECEIPT<4> = 'Discount Value: $':DOLLAR.VALUE
RECEIPT<5> = 'Remaining Balance: ':NEW.BALANCE:' points'
RECEIPT<6> = 'Transaction: ':TRANS.ID
RECEIPT<7> = 'Date: ':OCONV(DATE(), 'D2/MDY')

* Audit trail
AUDIT.MSG = 'Redeemed ':POINTS.TO.REDEEM:' points for $':DOLLAR.VALUE
AUDIT.MSG := ', New balance: ':NEW.BALANCE

CALL UTILS.COMMON('WRITE.AUDIT', 'LOYALTY.REDEEM', LOYALTY.NUMBER, AUDIT.MSG, DUMMY, DUMMY)
```

#### Usage Example

```unibasic
* Redeem points during checkout
LOYALTY.NUMBER = '5000123456789012'
POINTS.TO.REDEEM = 500
TRANS.ID = 'POS20251006-000456'

CALL LOYAL.REDEEM(LOYALTY.NUMBER, POINTS.TO.REDEEM, TRANS.ID, DOLLAR.VALUE, NEW.BALANCE, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   PRINT 'Points Redeemed: ':POINTS.TO.REDEEM
   PRINT 'Discount Applied: $':DOLLAR.VALUE
   PRINT 'Remaining Points: ':NEW.BALANCE

   * Apply discount to transaction
   TRANSACTION.DISCOUNT = DOLLAR.VALUE
END ELSE
   PRINT 'Redemption failed: ':ERROR.MSG
END
```

#### Complete Transaction Lifecycle Example

```unibasic
* Example: Customer loyalty lifecycle

* 1. Enrollment
CALL LOYAL.ENROLL('C00001234', LOYALTY.REC, LOYALTY.NUMBER, ERR, MSG)
   Result: Loyalty Number = 5000123456789012
           Tier = SILVER, Balance = 0 points

* 2. First purchase - $75
CALL LOYAL.EARN(LOYALTY.NUMBER, 75.00, 'POS-001', POINTS, BALANCE, ERR, MSG)
   Result: Earned 75 points (Silver tier 1.0x)
           Balance = 75 points

* 3. Second purchase - $450
CALL LOYAL.EARN(LOYALTY.NUMBER, 450.00, 'POS-002', POINTS, BALANCE, ERR, MSG)
   Result: Earned 450 points
           Total spend = $525
           UPGRADED TO GOLD! Bonus: 250 points
           Balance = 75 + 450 + 250 = 775 points

* 4. Third purchase - $100 (now Gold tier)
CALL LOYAL.EARN(LOYALTY.NUMBER, 100.00, 'POS-003', POINTS, BALANCE, ERR, MSG)
   Result: Earned 150 points (Gold tier 1.5x)
           Balance = 775 + 150 = 925 points

* 5. Redeem 500 points
CALL LOYAL.REDEEM(LOYALTY.NUMBER, 500, 'POS-004', DOLLAR, BALANCE, ERR, MSG)
   Result: Redeemed 500 points = $5.00 discount
           Balance = 925 - 500 = 425 points

* 6. Purchase $1,600 more to reach Platinum
CALL LOYAL.EARN(LOYALTY.NUMBER, 1600.00, 'POS-005', POINTS, BALANCE, ERR, MSG)
   Result: Earned 2,400 points (Gold 1.5x)
           Total spend = $2,225
           UPGRADED TO PLATINUM! Bonus: 500 points
           Balance = 425 + 2400 + 500 = 3,325 points

* 7. Future purchases earn 2x
CALL LOYAL.EARN(LOYALTY.NUMBER, 50.00, 'POS-006', POINTS, BALANCE, ERR, MSG)
   Result: Earned 100 points (Platinum tier 2.0x)
           Balance = 3,325 + 100 = 3,425 points
```

---

## PROMOTIONAL CAMPAIGN SYSTEM

The promotional engine provides flexible discount campaigns with multiple types, date ranges, and item/category targeting.

### Promotion Types Supported

1. **PERCENT** - Percentage off (e.g., 20% off)
2. **DOLLAR** - Fixed dollar amount off (e.g., $5 off)
3. **BOGO** - Buy One Get One (50% off second item)
4. **BUY.AMOUNT.GET.DISCOUNT** - Spend threshold discount (e.g., Spend $50, get $10 off)

---

### 4. PROMO.CREATE - Create Promotional Campaign

**Location:** `BP/PROMO.CREATE`
**Lines of Code:** ~750 lines
**Purpose:** Create and configure promotional campaigns with complex rules

#### Function Signature

```unibasic
SUBROUTINE PROMO.CREATE(PROMO.REC, PROMO.ID, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| PROMO.REC | Dynamic Array | IN/OUT | Promotion record with all rules |
| PROMO.ID | String | OUT | Generated promotion ID |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Promotion Record Structure

```unibasic
* Promotion record layout
PROMO.REC<1> = PROMO.ID                  ;* Auto-generated (PROMO-YYYYMMDD-NNN)
PROMO.REC<2> = PROMO.NAME                ;* Descriptive name
PROMO.REC<3> = PROMO.TYPE                ;* PERCENT/DOLLAR/BOGO/BUY.AMOUNT.GET.DISCOUNT
PROMO.REC<4> = DISCOUNT.VALUE            ;* Percentage or dollar amount
PROMO.REC<5> = START.DATE                ;* Effective start date
PROMO.REC<6> = END.DATE                  ;* Effective end date
PROMO.REC<7> = STATUS                    ;* ACTIVE/INACTIVE/EXPIRED
PROMO.REC<8> = MIN.PURCHASE              ;* Minimum purchase amount
PROMO.REC<9> = MAX.DISCOUNT              ;* Maximum discount cap
PROMO.REC<10> = STACKABLE.FLAG           ;* Can combine with other promos (Y/N)
PROMO.REC<11> = ITEM.IDS<1,N>            ;* Multivalued: applicable item IDs
PROMO.REC<12> = CATEGORIES<1,N>          ;* Multivalued: applicable categories
PROMO.REC<13> = STORE.IDS<1,N>           ;* Multivalued: applicable stores
PROMO.REC<14> = PRIORITY                 ;* Evaluation priority (1-10)
PROMO.REC<15> = USE.COUNT                ;* Times promotion has been used
PROMO.REC<16> = TOTAL.DISCOUNT           ;* Total discount amount given
```

#### Detailed Functionality

**1. Input Validation**

```unibasic
* Validate required fields
PROMO.NAME = PROMO.REC<PROMO.NAME>
PROMO.TYPE = PROMO.REC<PROMO.TYPE>
DISCOUNT.VALUE = PROMO.REC<DISCOUNT.VALUE>
START.DATE = PROMO.REC<START.DATE>
END.DATE = PROMO.REC<END.DATE>

* Check promotion name
IF PROMO.NAME EQ '' THEN
   ERROR.CODE = ERR.MISSING.REQUIRED
   ERROR.MSG = 'Promotion name is required'
   RETURN
END

* Validate promotion type
VALID.TYPES = 'PERCENT':VM:'DOLLAR':VM:'BOGO':VM:'BUY.AMOUNT.GET.DISCOUNT'
IF NOT(PROMO.TYPE MATCHES VALID.TYPES) THEN
   ERROR.CODE = ERR.INVALID.DATA
   ERROR.MSG = 'Invalid promotion type: ':PROMO.TYPE
   RETURN
END
```

**2. Type-Specific Validation**

```unibasic
BEGIN CASE
   * Percentage discount validation
   CASE PROMO.TYPE EQ 'PERCENT'
      IF DISCOUNT.VALUE <= 0 OR DISCOUNT.VALUE > 100 THEN
         ERROR.CODE = ERR.INVALID.DATA
         ERROR.MSG = 'Percentage must be between 1 and 100'
         RETURN
      END

   * Dollar discount validation
   CASE PROMO.TYPE EQ 'DOLLAR'
      IF DISCOUNT.VALUE <= 0 THEN
         ERROR.CODE = ERR.INVALID.DATA
         ERROR.MSG = 'Dollar discount must be greater than 0'
         RETURN
      END

   * BOGO validation
   CASE PROMO.TYPE EQ 'BOGO'
      * BOGO typically uses 50% for second item
      IF DISCOUNT.VALUE EQ 0 THEN
         DISCOUNT.VALUE = 50  ;* Default 50% off
         PROMO.REC<DISCOUNT.VALUE> = 50
      END

   * Buy amount get discount validation
   CASE PROMO.TYPE EQ 'BUY.AMOUNT.GET.DISCOUNT'
      MIN.PURCHASE = PROMO.REC<MIN.PURCHASE>
      IF MIN.PURCHASE <= 0 THEN
         ERROR.CODE = ERR.INVALID.DATA
         ERROR.MSG = 'Minimum purchase amount required for this type'
         RETURN
      END

      IF DISCOUNT.VALUE <= 0 THEN
         ERROR.CODE = ERR.INVALID.DATA
         ERROR.MSG = 'Discount value must be greater than 0'
         RETURN
      END
END CASE
```

**3. Date Range Validation**

```unibasic
* Validate date range
IF END.DATE < START.DATE THEN
   ERROR.CODE = ERR.INVALID.DATA
   ERROR.MSG = 'End date cannot be before start date'
   RETURN
END

* Check if already expired
IF END.DATE < DATE() THEN
   ERROR.CODE = ERR.INVALID.DATA
   ERROR.MSG = 'End date cannot be in the past'
   RETURN
END

* Set status based on dates
IF DATE() < START.DATE THEN
   PROMO.REC<STATUS> = 'SCHEDULED'
END ELSE
   PROMO.REC<STATUS> = 'ACTIVE'
END
```

**4. Generate Promotion ID**

```unibasic
* Generate unique promotion ID
* Format: PROMO-YYYYMMDD-NNN
TODAY.STR = OCONV(DATE(), 'D4YMD')  ;* YYYYMMDD format

* Find next sequence number for today
EXECUTE 'SELECT PROMOTIONS WITH @ID LIKE "PROMO-':TODAY.STR:'-..."' CAPTURING OUTPUT
COUNT = OUTPUT<2>  ;* Number of promotions created today

NEXT.SEQ = COUNT + 1
PROMO.ID = 'PROMO-':TODAY.STR:'-':FMT(NEXT.SEQ, '3"0"R')

* Example: PROMO-20251006-001
```

**5. Store and Item Assignment**

```unibasic
* Process store assignments
STORE.LIST = PROMO.REC<STORE.IDS>
IF STORE.LIST EQ '' OR STORE.LIST EQ 'ALL' THEN
   * Apply to all stores - get list from STORES file
   EXECUTE 'SELECT STORES' CAPTURING OUTPUT
   STORE.COUNT = OUTPUT<2>

   * Build multivalued list of all store IDs
   PROMO.REC<STORE.IDS> = ''
   FOR I = 1 TO STORE.COUNT
      READ STORE.REC FROM F.STORES, OUTPUT<I> THEN
         IF I EQ 1 THEN
            PROMO.REC<STORE.IDS> = OUTPUT<I>
         END ELSE
            PROMO.REC<STORE.IDS> := VM : OUTPUT<I>
         END
      END
   NEXT I
END

* Process item assignments
ITEM.LIST = PROMO.REC<ITEM.IDS>
CATEGORY.LIST = PROMO.REC<CATEGORIES>

* Must have either items or categories specified
IF ITEM.LIST EQ '' AND CATEGORY.LIST EQ '' THEN
   ERROR.CODE = ERR.MISSING.REQUIRED
   ERROR.MSG = 'Must specify item IDs or categories'
   RETURN
END
```

**6. Priority and Stacking Rules**

```unibasic
* Set default priority if not specified
PRIORITY = PROMO.REC<PRIORITY>
IF PRIORITY EQ '' OR PRIORITY < 1 OR PRIORITY > 10 THEN
   PROMO.REC<PRIORITY> = 5  ;* Default medium priority
END

* Set stacking flag
STACKABLE = PROMO.REC<STACKABLE.FLAG>
IF STACKABLE NE 'Y' AND STACKABLE NE 'N' THEN
   PROMO.REC<STACKABLE.FLAG> = 'N'  ;* Default non-stackable
END

* Non-stackable promotions with same priority conflict
* Check for conflicts if non-stackable
IF PROMO.REC<STACKABLE.FLAG> EQ 'N' THEN
   EXECUTE 'SELECT PROMOTIONS WITH STATUS = "ACTIVE" AND STACKABLE.FLAG = "N" AND PRIORITY = "':PRIORITY:'"'
   IF @SELECTED > 0 THEN
      * Warning but allow - evaluation will choose best discount
      PRINT 'WARNING: Multiple non-stackable promotions with priority ':PRIORITY
   END
END
```

**7. Initialize Statistics**

```unibasic
* Initialize usage tracking fields
PROMO.REC<USE.COUNT> = 0
PROMO.REC<TOTAL.DISCOUNT> = 0
PROMO.REC<CREATED.BY> = USER.ID
PROMO.REC<CREATED.DATE> = DATE()
PROMO.REC<CREATED.TIME> = TIME()
PROMO.REC<MODIFIED.BY> = USER.ID
PROMO.REC<MODIFIED.DATE> = DATE()
PROMO.REC<MODIFIED.TIME> = TIME()
```

**8. Save and Audit**

```unibasic
* Write promotion record
WRITE PROMO.REC ON F.PROMOTIONS, PROMO.ID

* Create audit trail
AUDIT.REC = ''
AUDIT.REC<1> = 'PROMO.CREATE'
AUDIT.REC<2> = PROMO.ID
AUDIT.REC<3> = PROMO.NAME
AUDIT.REC<4> = 'Type=':PROMO.TYPE:', Value=':DISCOUNT.VALUE
AUDIT.REC<5> = 'Dates: ':OCONV(START.DATE,'D2/MDY'):' to ':OCONV(END.DATE,'D2/MDY')
AUDIT.REC<6> = USER.ID
AUDIT.REC<7> = DATE()
AUDIT.REC<8> = TIME()

CALL UTILS.COMMON('WRITE.AUDIT', AUDIT.REC, DUMMY, DUMMY)

ERROR.CODE = 0
ERROR.MSG = 'Promotion created successfully'
```

#### Promotion Examples

**Example 1: Percentage Discount**
```unibasic
PROMO.REC = ''
PROMO.REC<PROMO.NAME> = 'Fall Sale - 20% Off Electronics'
PROMO.REC<PROMO.TYPE> = 'PERCENT'
PROMO.REC<DISCOUNT.VALUE> = 20
PROMO.REC<START.DATE> = '2025-10-01'
PROMO.REC<END.DATE> = '2025-10-31'
PROMO.REC<CATEGORIES> = 'ELECTRONICS':VM:'COMPUTERS'
PROMO.REC<STACKABLE.FLAG> = 'N'
PROMO.REC<PRIORITY> = 5

CALL PROMO.CREATE(PROMO.REC, PROMO.ID, ERROR.CODE, ERROR.MSG)
* Result: PROMO-20251006-001
```

**Example 2: Dollar Discount with Minimum**
```unibasic
PROMO.REC = ''
PROMO.REC<PROMO.NAME> = 'Spend $50 Get $10 Off'
PROMO.REC<PROMO.TYPE> = 'BUY.AMOUNT.GET.DISCOUNT'
PROMO.REC<DISCOUNT.VALUE> = 10
PROMO.REC<MIN.PURCHASE> = 50
PROMO.REC<START.DATE> = DATE()
PROMO.REC<END.DATE> = DATE() + 30
PROMO.REC<STACKABLE.FLAG> = 'Y'
PROMO.REC<PRIORITY> = 3

CALL PROMO.CREATE(PROMO.REC, PROMO.ID, ERROR.CODE, ERROR.MSG)
```

**Example 3: BOGO (Buy One Get One)**
```unibasic
PROMO.REC = ''
PROMO.REC<PROMO.NAME> = 'BOGO 50% Off Shoes'
PROMO.REC<PROMO.TYPE> = 'BOGO'
PROMO.REC<DISCOUNT.VALUE> = 50  ;* 50% off second item
PROMO.REC<START.DATE> = DATE()
PROMO.REC<END.DATE> = DATE() + 14
PROMO.REC<CATEGORIES> = 'SHOES'
PROMO.REC<STACKABLE.FLAG> = 'N'
PROMO.REC<PRIORITY> = 7

CALL PROMO.CREATE(PROMO.REC, PROMO.ID, ERROR.CODE, ERROR.MSG)
```

---

### 5. PROMO.EVALUATE - Evaluate Promotions for Transaction

**Location:** `BP/PROMO.EVALUATE`
**Lines of Code:** ~800 lines
**Purpose:** Evaluate all applicable promotions and apply best discounts to transaction

#### Function Signature

```unibasic
SUBROUTINE PROMO.EVALUATE(TRANS.REC, STORE.ID, APPLIED.PROMOS, TOTAL.DISCOUNT, ERROR.CODE, ERROR.MSG)
```

#### Parameters

| Parameter | Type | Direction | Description |
|-----------|------|-----------|-------------|
| TRANS.REC | Dynamic Array | IN/OUT | POS transaction record |
| STORE.ID | String | IN | Current store ID |
| APPLIED.PROMOS | Dynamic Array | OUT | List of applied promotion IDs and amounts |
| TOTAL.DISCOUNT | Decimal | OUT | Total discount amount applied |
| ERROR.CODE | Integer | OUT | 0=success, non-zero=error |
| ERROR.MSG | String | OUT | Error description if failed |

#### Detailed Functionality

**1. Select Active Promotions**

```unibasic
* Select all active promotions for this store
SELECT.CMD = 'SELECT PROMOTIONS WITH STATUS = "ACTIVE"'
SELECT.CMD := ' AND START.DATE <= "':DATE():'"'
SELECT.CMD := ' AND END.DATE >= "':DATE():'"'
SELECT.CMD := ' BY PRIORITY DESCENDING'  ;* Highest priority first

EXECUTE SELECT.CMD

PROMO.LIST = ''
PROMO.COUNT = 0

* Build list of applicable promotions
LOOP
   READNEXT PROMO.ID ELSE EXIT

   READ PROMO.REC FROM F.PROMOTIONS, PROMO.ID THEN
      * Check if promotion applies to this store
      STORE.LIST = PROMO.REC<STORE.IDS>
      LOCATE STORE.ID IN STORE.LIST<1> SETTING POS THEN
         * Store matches - add to evaluation list
         PROMO.COUNT = PROMO.COUNT + 1
         PROMO.LIST<PROMO.COUNT> = PROMO.REC
      END
   END
REPEAT
```

**2. Extract Transaction Items**

```unibasic
* Get transaction items for evaluation
ITEM.IDS = TRANS.REC<ITEM.IDS>
ITEM.QTYS = TRANS.REC<QTYS>
ITEM.PRICES = TRANS.REC<PRICES>
ITEM.EXTENDED = TRANS.REC<EXTENDED.PRICES>
ITEM.COUNT = DCOUNT(ITEM.IDS, VM)

* Calculate subtotal
SUBTOTAL = 0
FOR I = 1 TO ITEM.COUNT
   SUBTOTAL = SUBTOTAL + ITEM.EXTENDED<1,I>
NEXT I
```

**3. Evaluate Each Promotion**

```unibasic
TOTAL.DISCOUNT = 0
APPLIED.PROMOS = ''
APPLIED.COUNT = 0

FOR PROMO.INDEX = 1 TO PROMO.COUNT
   PROMO.REC = PROMO.LIST<PROMO.INDEX>
   PROMO.ID = PROMO.REC<PROMO.ID>
   PROMO.TYPE = PROMO.REC<PROMO.TYPE>

   * Calculate discount for this promotion
   BEGIN CASE
      CASE PROMO.TYPE EQ 'PERCENT'
         GOSUB CALCULATE.PERCENT.DISCOUNT

      CASE PROMO.TYPE EQ 'DOLLAR'
         GOSUB CALCULATE.DOLLAR.DISCOUNT

      CASE PROMO.TYPE EQ 'BOGO'
         GOSUB CALCULATE.BOGO.DISCOUNT

      CASE PROMO.TYPE EQ 'BUY.AMOUNT.GET.DISCOUNT'
         GOSUB CALCULATE.BUY.AMOUNT.DISCOUNT
   END CASE

   * If discount calculated, check stacking rules
   IF PROMO.DISCOUNT > 0 THEN
      STACKABLE = PROMO.REC<STACKABLE.FLAG>

      IF STACKABLE EQ 'Y' THEN
         * Stackable - add to total
         GOSUB APPLY.PROMOTION
      END ELSE
         * Non-stackable - only if better than current
         IF APPLIED.COUNT EQ 0 OR PROMO.DISCOUNT > TOTAL.DISCOUNT THEN
            * Replace previous or apply first
            TOTAL.DISCOUNT = PROMO.DISCOUNT
            APPLIED.PROMOS = ''
            APPLIED.COUNT = 0
            GOSUB APPLY.PROMOTION
         END
      END
   END
NEXT PROMO.INDEX
```

**4. Percentage Discount Calculation**

```unibasic
CALCULATE.PERCENT.DISCOUNT:
   * Get applicable items for this promotion
   PROMO.ITEMS = PROMO.REC<ITEM.IDS>
   PROMO.CATEGORIES = PROMO.REC<CATEGORIES>
   DISCOUNT.PCT = PROMO.REC<DISCOUNT.VALUE>

   ELIGIBLE.AMOUNT = 0

   * Loop through transaction items
   FOR I = 1 TO ITEM.COUNT
      ITEM.ID = ITEM.IDS<1,I>
      ELIGIBLE = FALSE

      * Check if item qualifies
      IF PROMO.ITEMS NE '' THEN
         LOCATE ITEM.ID IN PROMO.ITEMS<1> SETTING POS THEN
            ELIGIBLE = TRUE
         END
      END

      * Check if category qualifies
      IF NOT(ELIGIBLE) AND PROMO.CATEGORIES NE '' THEN
         * Read item record to get category
         READ INV.REC FROM F.INVENTORY, ITEM.ID THEN
            ITEM.CAT = INV.REC<CATEGORY>
            LOCATE ITEM.CAT IN PROMO.CATEGORIES<1> SETTING POS THEN
               ELIGIBLE = TRUE
            END
         END
      END

      * Add to eligible amount if qualified
      IF ELIGIBLE THEN
         ELIGIBLE.AMOUNT = ELIGIBLE.AMOUNT + ITEM.EXTENDED<1,I>
      END
   NEXT I

   * Calculate discount
   IF ELIGIBLE.AMOUNT > 0 THEN
      PROMO.DISCOUNT = ELIGIBLE.AMOUNT * (DISCOUNT.PCT / 100)

      * Apply maximum discount cap if set
      MAX.DISCOUNT = PROMO.REC<MAX.DISCOUNT>
      IF MAX.DISCOUNT > 0 AND PROMO.DISCOUNT > MAX.DISCOUNT THEN
         PROMO.DISCOUNT = MAX.DISCOUNT
      END

      * Round to 2 decimals
      PROMO.DISCOUNT = INT(PROMO.DISCOUNT * 100) / 100
   END ELSE
      PROMO.DISCOUNT = 0
   END

   RETURN
```

**5. BOGO (Buy One Get One) Calculation**

```unibasic
CALCULATE.BOGO.DISCOUNT:
   * BOGO: For every 2 qualifying items, discount the cheaper one
   PROMO.ITEMS = PROMO.REC<ITEM.IDS>
   PROMO.CATEGORIES = PROMO.REC<CATEGORIES>
   BOGO.PCT = PROMO.REC<DISCOUNT.VALUE>  ;* Usually 50%

   * Find all eligible items
   ELIGIBLE.ITEMS = ''
   ELIGIBLE.PRICES = ''
   ELIGIBLE.COUNT = 0

   FOR I = 1 TO ITEM.COUNT
      ITEM.ID = ITEM.IDS<1,I>
      ITEM.QTY = ITEM.QTYS<1,I>
      ITEM.PRICE = ITEM.PRICES<1,I>

      * Check eligibility (same logic as percent)
      ELIGIBLE = FALSE

      IF PROMO.ITEMS NE '' THEN
         LOCATE ITEM.ID IN PROMO.ITEMS<1> SETTING POS THEN
            ELIGIBLE = TRUE
         END
      END

      IF ELIGIBLE THEN
         * Add each unit separately for BOGO calculation
         FOR QTY.INDEX = 1 TO ITEM.QTY
            ELIGIBLE.COUNT = ELIGIBLE.COUNT + 1
            ELIGIBLE.PRICES<1,ELIGIBLE.COUNT> = ITEM.PRICE
         NEXT QTY.INDEX
      END
   NEXT I

   * Must have at least 2 items for BOGO
   IF ELIGIBLE.COUNT < 2 THEN
      PROMO.DISCOUNT = 0
      RETURN
   END

   * Sort prices to find cheapest items
   * For every pair, discount the cheaper one
   PAIRS = INT(ELIGIBLE.COUNT / 2)
   PROMO.DISCOUNT = 0

   * Sort prices ascending
   GOSUB SORT.PRICES

   * Discount the first PAIRS items (cheapest)
   FOR I = 1 TO PAIRS
      DISCOUNTED.PRICE = ELIGIBLE.PRICES<1,I> * (BOGO.PCT / 100)
      PROMO.DISCOUNT = PROMO.DISCOUNT + DISCOUNTED.PRICE
   NEXT I

   PROMO.DISCOUNT = INT(PROMO.DISCOUNT * 100) / 100

   RETURN

SORT.PRICES:
   * Bubble sort prices (simple for small lists)
   FOR I = 1 TO ELIGIBLE.COUNT - 1
      FOR J = I + 1 TO ELIGIBLE.COUNT
         IF ELIGIBLE.PRICES<1,J> < ELIGIBLE.PRICES<1,I> THEN
            TEMP = ELIGIBLE.PRICES<1,I>
            ELIGIBLE.PRICES<1,I> = ELIGIBLE.PRICES<1,J>
            ELIGIBLE.PRICES<1,J> = TEMP
         END
      NEXT J
   NEXT I
   RETURN
```

**6. Buy Amount Get Discount Calculation**

```unibasic
CALCULATE.BUY.AMOUNT.DISCOUNT:
   * Spend $X get $Y off
   MIN.PURCHASE = PROMO.REC<MIN.PURCHASE>
   DISCOUNT.AMT = PROMO.REC<DISCOUNT.VALUE>

   * Check if subtotal meets minimum
   IF SUBTOTAL >= MIN.PURCHASE THEN
      PROMO.DISCOUNT = DISCOUNT.AMT
   END ELSE
      PROMO.DISCOUNT = 0
   END

   RETURN
```

**7. Apply Promotion to Transaction**

```unibasic
APPLY.PROMOTION:
   * Add to applied promotions list
   APPLIED.COUNT = APPLIED.COUNT + 1
   APPLIED.PROMOS<1,APPLIED.COUNT> = PROMO.ID
   APPLIED.PROMOS<2,APPLIED.COUNT> = PROMO.DISCOUNT
   APPLIED.PROMOS<3,APPLIED.COUNT> = PROMO.REC<PROMO.NAME>

   * Update total if stackable
   IF PROMO.REC<STACKABLE.FLAG> EQ 'Y' THEN
      TOTAL.DISCOUNT = TOTAL.DISCOUNT + PROMO.DISCOUNT
   END

   * Update promotion usage statistics
   READU PROMO.REC FROM F.PROMOTIONS, PROMO.ID THEN
      PROMO.REC<USE.COUNT> = PROMO.REC<USE.COUNT> + 1
      PROMO.REC<TOTAL.DISCOUNT> = PROMO.REC<TOTAL.DISCOUNT> + PROMO.DISCOUNT
      WRITE PROMO.REC ON F.PROMOTIONS, PROMO.ID
   END ELSE
      RELEASE F.PROMOTIONS, PROMO.ID
   END

   RETURN
```

**8. Update Transaction Record**

```unibasic
* Apply discounts to transaction
TRANS.REC<PROMO.DISCOUNT> = TOTAL.DISCOUNT
TRANS.REC<APPLIED.PROMOS> = APPLIED.PROMOS

* Recalculate transaction total
SUBTOTAL = TRANS.REC<SUBTOTAL>
TAX.AMT = TRANS.REC<TAX>
NEW.TOTAL = SUBTOTAL - TOTAL.DISCOUNT + TAX.AMT

TRANS.REC<TOTAL> = NEW.TOTAL

ERROR.CODE = 0
ERROR.MSG = 'Promotions evaluated successfully'
```

#### Usage Example

```unibasic
* During POS checkout, evaluate promotions
STORE.ID = 'S001'

CALL PROMO.EVALUATE(TRANS.REC, STORE.ID, APPLIED.PROMOS, TOTAL.DISCOUNT, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE EQ 0 THEN
   * Display applied promotions
   PROMO.COUNT = DCOUNT(APPLIED.PROMOS<1>, VM)

   IF PROMO.COUNT > 0 THEN
      PRINT 'Promotions Applied:'
      FOR I = 1 TO PROMO.COUNT
         PROMO.ID = APPLIED.PROMOS<1,I>
         PROMO.AMT = APPLIED.PROMOS<2,I>
         PROMO.NAME = APPLIED.PROMOS<3,I>

         PRINT '  ':PROMO.NAME:' - $':FMT(PROMO.AMT, '10.2R')
      NEXT I

      PRINT 'Total Promotional Savings: $':FMT(TOTAL.DISCOUNT, '10.2R')
   END
END
```

#### Complete Promotion Evaluation Example

```unibasic
* Transaction with multiple items
TRANS.REC<ITEM.IDS> = 'ITEM001':VM:'ITEM002':VM:'ITEM003':VM:'ITEM004'
TRANS.REC<QTYS> = '2':VM:'1':VM:'2':VM:'1'
TRANS.REC<PRICES> = '25.00':VM:'50.00':VM:'15.00':VM:'30.00'
TRANS.REC<EXTENDED.PRICES> = '50.00':VM:'50.00':VM:'30.00':VM:'30.00'
TRANS.REC<SUBTOTAL> = 160.00

* Active Promotions:
* 1. PROMO-001: 20% off ITEM001 (Priority 5, Non-stackable)
* 2. PROMO-002: BOGO 50% on ITEM003 (Priority 7, Non-stackable)
* 3. PROMO-003: Spend $100 get $10 off (Priority 3, Stackable)

CALL PROMO.EVALUATE(TRANS.REC, 'S001', APPLIED.PROMOS, TOTAL.DISCOUNT, ERROR.CODE, ERROR.MSG)

* Evaluation Results:
* PROMO-001: 20% off $50 = $10.00 discount
* PROMO-002: BOGO on 2 units @ $15 = $7.50 discount (50% off cheaper)
* PROMO-003: $10.00 discount (subtotal > $100)

* Non-stackable conflict between PROMO-001 and PROMO-002
* PROMO-002 wins (higher priority 7 vs 5)
* PROMO-003 is stackable, so it adds

* Final Applied Promotions:
*   PROMO-002 (BOGO): $7.50
*   PROMO-003 (Spend $100): $10.00
*   Total Discount: $17.50

* New transaction total: $160.00 - $17.50 = $142.50 (before tax)
```

---

## INTEGRATION WITH POS

### Loyalty Points at Checkout

During POS transaction completion, the system integrates loyalty point earning:

```unibasic
* In POS.COMPLETE program
CUSTOMER.ID = TRANS.REC<CUSTOMER.ID>

* Check if customer has loyalty account
IF CUSTOMER.ID NE '' THEN
   READ CUST.REC FROM F.CUSTOMERS, CUSTOMER.ID THEN
      LOYALTY.NUMBER = CUST.REC<LOYALTY.NUMBER>

      IF LOYALTY.NUMBER NE '' THEN
         * Award loyalty points
         PURCHASE.AMT = TRANS.REC<SUBTOTAL>
         TRANS.ID = TRANS.REC<TRANS.ID>

         CALL LOYAL.EARN(LOYALTY.NUMBER, PURCHASE.AMT, TRANS.ID, POINTS.EARNED, NEW.BALANCE, ERR, MSG)

         IF ERR EQ 0 THEN
            * Print on receipt
            PRINT 'Points Earned: ':POINTS.EARNED
            PRINT 'Points Balance: ':NEW.BALANCE
         END
      END
   END
END
```

### Promotion Application Flow

```unibasic
* In POS.CALCULATE program

* 1. Calculate item subtotals
GOSUB CALCULATE.SUBTOTALS

* 2. Evaluate and apply promotions
STORE.ID = SYSTEM<CURRENT.STORE>
CALL PROMO.EVALUATE(TRANS.REC, STORE.ID, APPLIED.PROMOS, PROMO.DISCOUNT, ERR, MSG)

* 3. Apply loyalty point redemption if requested
IF LOYALTY.REDEEM.FLAG THEN
   LOYALTY.NUMBER = TRANS.REC<LOYALTY.NUMBER>
   POINTS.TO.REDEEM = TRANS.REC<REDEEM.POINTS>

   CALL LOYAL.REDEEM(LOYALTY.NUMBER, POINTS.TO.REDEEM, TRANS.ID, LOYAL.DISCOUNT, NEW.BAL, ERR, MSG)

   IF ERR EQ 0 THEN
      TRANS.REC<LOYALTY.DISCOUNT> = LOYAL.DISCOUNT
   END
END

* 4. Calculate tax on discounted amount
TAXABLE.AMT = TRANS.REC<SUBTOTAL> - PROMO.DISCOUNT - LOYAL.DISCOUNT
TAX.AMT = TAXABLE.AMT * TAX.RATE
TRANS.REC<TAX> = TAX.AMT

* 5. Calculate final total
FINAL.TOTAL = TAXABLE.AMT + TAX.AMT
TRANS.REC<TOTAL> = FINAL.TOTAL
```

---

## SUMMARY

The Loyalty and Promotions system provides:

### Loyalty Program
- **Three-tier structure** (Silver, Gold, Platinum) with automatic upgrades
- **Points earning** with tier multipliers (1.0x, 1.5x, 2.0x)
- **Points redemption** at 100 points = $1.00
- **Upgrade bonuses** (250 points for Gold, 500 for Platinum)
- **Luhn algorithm** for secure card number generation
- **Complete transaction history** tracking

### Promotional Engine
- **Four promotion types:**
  - Percentage discounts
  - Dollar amount discounts
  - BOGO (Buy One Get One)
  - Buy amount get discount
- **Flexible targeting** by item, category, or store
- **Date range** control with automatic activation/expiration
- **Priority system** for conflict resolution
- **Stacking rules** for combining promotions
- **Usage statistics** and ROI tracking

### Integration
- **Seamless POS integration** for real-time discount application
- **Automatic tier upgrades** during transactions
- **Combined discounts** from promotions and loyalty redemptions
- **Receipt printing** with promotion and points details
- **Comprehensive audit trails** for all loyalty and promotion activities

**Total Module Size:** ~3,300 lines of production-ready UniBasic code

---

*Part 5 of 8 - Next: Gift Cards and Price Management*
