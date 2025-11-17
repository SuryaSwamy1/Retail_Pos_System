# RETAIL POS SYSTEM - COMPLETE FUNCTIONALITY GUIDE

## PART 4: EMPLOYEE AND HR MANAGEMENT

---

## TABLE OF CONTENTS - PART 4

1. [Employee Management](#1-employee-management)
   - 1.1 Create Employee
   - 1.2 Read Employee
   - 1.3 Update Employee
   - 1.4 Terminate Employee

2. [Time and Attendance](#2-time-and-attendance)
   - 2.1 Time Card Management
   - 2.2 Overtime Calculation
   - 2.3 PTO Accrual

3. [Scheduling](#3-scheduling)
   - 3.1 Create Schedule
   - 3.2 Shift Management
   - 3.3 Conflict Detection

4. [Commission Tracking](#4-commission-tracking)
   - 4.1 Commission Calculation
   - 4.2 Tier Bonuses
   - 4.3 Reporting

---

## 1. EMPLOYEE MANAGEMENT

The Employee Management module handles complete employee lifecycle from hiring through termination, with comprehensive HR data tracking.

### 1.1 Create Employee (EMP.CREATE)

**Purpose:** Create new employee records with encrypted sensitive data

**Program:** `BP/EMP.CREATE`

**Input Parameters:**
```unibasic
EMP.REC        ; Employee record
EMP.ID.OUT     ; Generated employee ID (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Employee Record Fields:**

#### Personal Information:
| Field | Required | Validation | Encrypted |
|-------|----------|------------|-----------|
| FIRST.NAME | Yes | 2-50 characters | No |
| LAST.NAME | Yes | 2-50 characters | No |
| SSN | Yes | XXX-XX-XXXX format | **Yes** |
| DATE.OF.BIRTH | Yes | Valid date, 18+ years | No |
| EMAIL | Yes | Valid email format | No |
| PHONE | Yes | US phone format | No |
| ADDRESS | Yes | | No |
| CITY | Yes | | No |
| STATE | Yes | 2 characters | No |
| ZIP | Yes | 5 or 9 digits | No |

#### Employment Information:
| Field | Required | Options |
|-------|----------|---------|
| HIRE.DATE | Yes | Cannot be future |
| POSITION | Yes | CASHIER, SUPERVISOR, MANAGER, etc. |
| DEPARTMENT | No | SALES, OPERATIONS, ADMIN |
| STORE.ID | Yes | Must exist |
| EMPLOYMENT.TYPE | Yes | FULL.TIME, PART.TIME, SEASONAL |
| PAY.TYPE | Yes | HOURLY, SALARY, COMMISSION |
| PAY.RATE | Yes | Must be >= minimum wage |
| SECURITY.LEVEL | Yes | 1-10 |

#### Benefits Information:
| Field | Required |
|-------|----------|
| BENEFITS.ELIGIBLE | Auto-calculated |
| HEALTH.INSURANCE | Enrollment status |
| DENTAL.INSURANCE | Enrollment status |
| VISION.INSURANCE | Enrollment status |
| RETIREMENT.401K | Enrollment status |
| RETIREMENT.PCT | Contribution % |

#### Banking Information:
| Field | Required | Encrypted |
|-------|----------|-----------|
| BANK.NAME | If direct deposit | No |
| BANK.ROUTING | If direct deposit | **Yes** |
| BANK.ACCOUNT | If direct deposit | **Yes** |
| BANK.ACCOUNT.TYPE | If direct deposit | No |

**SSN Encryption:**
```unibasic
* Encrypt sensitive data before storing
PLAIN.SSN = '123-45-6789'
CALL UTILS.COMMON('ENCRYPT.STRING', PLAIN.SSN, ENCRYPTED.SSN)
EMP.REC<SSN> = ENCRYPTED.SSN

* When reading:
CALL UTILS.COMMON('DECRYPT.STRING', EMP.REC<SSN>, PLAIN.SSN)
```

**Process Flow:**
```
1. Validate all required fields
2. Validate SSN format and uniqueness
3. Check age requirement (18+)
4. Validate pay rate vs minimum wage
5. Generate Employee ID (EMP-YYYYMMDD-XXX)
6. Encrypt SSN and bank account
7. Calculate benefits eligibility
8. Set default values
9. Create employee record
10. Create onboarding tasks
11. Send welcome email
```

**Benefits Eligibility:**
```unibasic
* Full-time employees (30+ hours/week)
IF EMPLOYMENT.TYPE = 'FULL.TIME' THEN
   BENEFITS.ELIGIBLE = 'Y'
   EFFECTIVE.DATE = HIRE.DATE + 90  ; 90-day waiting period
END ELSE
   BENEFITS.ELIGIBLE = 'N'
END
```

**Default Security Levels:**
```
Position       Default Level
--------       -------------
CASHIER        Level 2
SENIOR CASHIER Level 4
SUPERVISOR     Level 6
MANAGER        Level 8
DIRECTOR       Level 9
EXECUTIVE      Level 10
```

**Example Usage:**
```unibasic
EMP.REC = ''
EMP.REC<FIRST.NAME> = 'Jane'
EMP.REC<LAST.NAME> = 'Doe'
EMP.REC<SSN> = '123-45-6789'
EMP.REC<DATE.OF.BIRTH> = '19900115'
EMP.REC<EMAIL> = 'jane.doe@company.com'
EMP.REC<PHONE> = '555-123-4567'
EMP.REC<HIRE.DATE> = SYSTEM.DATE
EMP.REC<POSITION> = 'CASHIER'
EMP.REC<DEPARTMENT> = 'SALES'
EMP.REC<STORE.ID> = 'STORE-001'
EMP.REC<EMPLOYMENT.TYPE> = 'FULL.TIME'
EMP.REC<PAY.TYPE> = 'HOURLY'
EMP.REC<PAY.RATE> = 15.00
EMP.REC<SECURITY.LEVEL> = 2

CALL EMP.CREATE(EMP.REC, EMP.ID, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Employee created: ' : EMP.ID
   CRT 'Benefits eligible: ' : EMP.REC<BENEFITS.ELIGIBLE>
END
```

---

### 1.2 Read Employee (EMP.READ)

**Purpose:** Retrieve employee record with calculated fields

**Program:** `BP/EMP.READ`

**Input Parameters:**
```unibasic
EMP.ID         ; Employee ID to read
EMP.REC        ; Employee record (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Calculated Fields:**

#### Tenure Calculation:
```unibasic
HIRE.DATE = EMP.REC<HIRE.DATE>
TENURE.DAYS = SYSTEM.DATE - HIRE.DATE
TENURE.YEARS = INT(TENURE.DAYS / 365)
TENURE.MONTHS = INT((TENURE.DAYS - (TENURE.YEARS * 365)) / 30)
```

#### PTO Accrual:
```unibasic
* Accrual based on tenure:
Years     Hours/Year
-----     ----------
0-1       80 hours (2 weeks)
2-4       120 hours (3 weeks)
5-9       160 hours (4 weeks)
10+       200 hours (5 weeks)

* Calculate current balance:
ACCRUED = calculate_accrued(HIRE.DATE, SYSTEM.DATE)
USED = sum_of_pto_taken()
AVAILABLE = ACCRUED - USED
```

#### Benefits Value:
```unibasic
* Calculate total benefits value
HEALTH.VALUE = health_insurance_value
DENTAL.VALUE = dental_insurance_value
VISION.VALUE = vision_insurance_value
RETIREMENT.MATCH = calculate_401k_match()

TOTAL.BENEFITS = HEALTH.VALUE + DENTAL.VALUE + VISION.VALUE + RETIREMENT.MATCH
```

#### Age and Service:
```unibasic
CURRENT.AGE = calculate_age(DATE.OF.BIRTH, SYSTEM.DATE)
YEARS.OF.SERVICE = TENURE.YEARS
```

**Example Usage:**
```unibasic
CALL EMP.READ('EMP-001', EMP.REC, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Employee: ' : EMP.REC<FIRST.NAME> : ' ' : EMP.REC<LAST.NAME>
   CRT 'Tenure: ' : EMP.REC<TENURE.YEARS> : ' years'
   CRT 'PTO Available: ' : EMP.REC<PTO.AVAILABLE> : ' hours'
   CRT 'Benefits Value: $' : EMP.REC<BENEFITS.VALUE>
END
```

---

### 1.3 Update Employee (EMP.UPDATE)

**Purpose:** Update employee information with change tracking

**Program:** `BP/EMP.UPDATE`

**Input Parameters:**
```unibasic
EMP.ID         ; Employee ID to update
EMP.REC        ; Updated employee record
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Updateable Fields:**
- Contact information (email, phone, address)
- Position and department (with authorization)
- Pay rate (with authorization and history)
- Benefits enrollment
- Security level (with authorization)
- Status (active, leave, terminated)

**Authorization Requirements:**

| Change Type | Required Level |
|-------------|----------------|
| Contact info | Employee or HR |
| Position change | Level 8+ |
| Pay rate increase < 10% | Level 7+ |
| Pay rate increase >= 10% | Level 9+ |
| Security level | Level 10 only |
| Status change | Level 8+ |

**Pay Rate Change Tracking:**
```unibasic
* Store pay history
OPEN 'PAY.HISTORY' TO F.PAY.HIST

HISTORY.REC = ''
HISTORY.REC<1> = EMP.ID
HISTORY.REC<2> = OLD.PAY.RATE
HISTORY.REC<3> = NEW.PAY.RATE
HISTORY.REC<4> = CHANGE.REASON
HISTORY.REC<5> = EFFECTIVE.DATE
HISTORY.REC<6> = AUTHORIZED.BY
HISTORY.REC<7> = SYSTEM.DATE

HISTORY.ID = EMP.ID : '*' : SYSTEM.DATE : '*' : SYSTEM.TIME
WRITE HISTORY.REC TO F.PAY.HIST, HISTORY.ID
```

**Status Change Logic:**
```unibasic
* Valid status transitions:
ACTIVE → ON.LEAVE
ACTIVE → TERMINATED
ON.LEAVE → ACTIVE
ON.LEAVE → TERMINATED

* Cannot reactivate terminated employees
IF OLD.STATUS = 'TERMINATED' THEN
   ERROR.MSG = 'Cannot reactivate terminated employee'
   RETURN
END
```

---

### 1.4 Terminate Employee (EMP.TERMINATE)

**Purpose:** Process employee termination with final pay calculation

**Program:** `BP/EMP.TERMINATE`

**Input Parameters:**
```unibasic
EMP.ID         ; Employee ID to terminate
TERM.DATE      ; Termination date
TERM.REASON    ; Termination reason
TERM.TYPE      ; VOLUNTARY or INVOLUNTARY
FINAL.PAY.INFO ; Final pay details (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Termination Reasons:**
- **RESIGNATION** - Employee resigned
- **RETIREMENT** - Employee retired
- **PERFORMANCE** - Performance issues
- **MISCONDUCT** - Policy violation
- **LAYOFF** - Business reasons
- **END.OF.SEASON** - Seasonal employee
- **OTHER** - Other reasons

**Process Flow:**
```
1. Validate employee exists and is active
2. Validate termination authorization (Level 8+)
3. Calculate final pay:
   a. Regular hours worked
   b. Overtime hours
   c. Unused PTO payout
   d. Commission owed
   e. Final reimbursements
4. Process benefits termination
5. Revoke system access
6. Update employee status
7. Create termination audit
8. Generate final paycheck
9. Create COBRA notification
10. Send exit paperwork
```

**Final Pay Calculation:**
```unibasic
* Components of final pay
REGULAR.PAY = hours_worked * pay_rate
OVERTIME.PAY = ot_hours * pay_rate * 1.5
PTO.PAYOUT = unused_pto_hours * pay_rate
COMMISSION.OWED = calculate_commission()
REIMBURSEMENTS = pending_reimbursements

GROSS.FINAL.PAY = REGULAR.PAY + OVERTIME.PAY + PTO.PAYOUT + COMMISSION.OWED + REIMBURSEMENTS

* Deductions
TAXES = calculate_taxes(GROSS.FINAL.PAY)
BENEFITS = final_benefits_deductions

NET.FINAL.PAY = GROSS.FINAL.PAY - TAXES - BENEFITS
```

**PTO Payout Rules:**
```unibasic
* Payout unused PTO based on reason:
IF TERM.TYPE = 'VOLUNTARY' AND TERM.REASON = 'RESIGNATION' THEN
   * Requires 2-week notice for full payout
   IF notice_given >= 14 THEN
      PTO.PAYOUT = unused_pto * pay_rate
   END ELSE
      PTO.PAYOUT = unused_pto * pay_rate * 0.5  ; 50% payout
   END
END ELSE IF TERM.REASON = 'RETIREMENT' THEN
   PTO.PAYOUT = unused_pto * pay_rate  ; Full payout
END ELSE IF TERM.TYPE = 'INVOLUNTARY' THEN
   * Check reason
   IF TERM.REASON = 'MISCONDUCT' THEN
      PTO.PAYOUT = 0  ; No payout
   END ELSE
      PTO.PAYOUT = unused_pto * pay_rate  ; Full payout
   END
END
```

**Example Usage:**
```unibasic
* Employee resignation with 2-week notice
CALL EMP.TERMINATE('EMP-001', SYSTEM.DATE + 14, 'RESIGNATION', 'VOLUNTARY', FINAL.PAY, ERROR.CODE, ERROR.MSG)

IF ERROR.CODE = ERR.SUCCESS THEN
   CRT 'Employee termination processed'
   CRT 'Final pay: $' : FINAL.PAY<NET.FINAL.PAY>
   CRT 'Last day: ' : OCONV(SYSTEM.DATE + 14, 'D4/')
END
```

---

## 2. TIME AND ATTENDANCE

Time and attendance tracking manages employee work hours, overtime, and PTO.

### 2.1 Time Card Management (EMP.TIMECARD)

**Purpose:** Record employee clock in/out with overtime calculation

**Program:** `BP/EMP.TIMECARD`

**Input Parameters:**
```unibasic
EMP.ID         ; Employee ID
ACTION         ; 'CLOCK.IN' or 'CLOCK.OUT'
TIMECARD.REC   ; Time card record (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Time Card Record:**
```unibasic
TIMECARD.REC<1> = employee_id
TIMECARD.REC<2> = work_date
TIMECARD.REC<3> = clock_in_time
TIMECARD.REC<4> = clock_out_time
TIMECARD.REC<5> = regular_hours
TIMECARD.REC<6> = overtime_hours
TIMECARD.REC<7> = break_time
TIMECARD.REC<8> = total_hours
TIMECARD.REC<9> = status (OPEN, CLOSED, APPROVED)
```

**Clock In Process:**
```
1. Validate employee is active
2. Check for existing open time card
3. Create new time card record
4. Record clock in time
5. Set status to OPEN
```

**Clock Out Process:**
```
1. Find open time card for employee
2. Record clock out time
3. Calculate break time (if applicable)
4. Calculate regular hours
5. Calculate overtime hours
6. Calculate total hours
7. Set status to CLOSED
```

**Hours Calculation:**
```unibasic
* Calculate total time
CLOCK.IN = TIMECARD.REC<3>
CLOCK.OUT = TIMECARD.REC<4>
TOTAL.MINUTES = (CLOCK.OUT - CLOCK.IN) / 60

* Subtract break time (30 min for 6+ hour shifts)
IF TOTAL.MINUTES >= 360 THEN
   BREAK.MINUTES = 30
END ELSE
   BREAK.MINUTES = 0
END

WORK.MINUTES = TOTAL.MINUTES - BREAK.MINUTES
TOTAL.HOURS = WORK.MINUTES / 60

* Round to nearest 0.25 hour
TOTAL.HOURS = INT((TOTAL.HOURS * 4) + 0.5) / 4
```

---

### 2.2 Overtime Calculation

**Overtime Rules:**
- **Daily OT:** Hours over 8 in a day = 1.5x
- **Weekly OT:** Hours over 40 in a week = 1.5x
- **Double Time:** Hours over 12 in a day = 2x

**Weekly Overtime Calculation:**
```unibasic
* Sum hours for week (Sunday-Saturday)
WEEK.START = calculate_week_start(WORK.DATE)
WEEK.END = WEEK.START + 6

* Get all time cards for week
WEEK.HOURS = 0
FOR D = WEEK.START TO WEEK.END
   TIMECARD.ID = EMP.ID : '*' : D
   READ TC.REC FROM F.TIMECARDS, TIMECARD.ID ELSE CONTINUE

   WEEK.HOURS += TC.REC<TOTAL.HOURS>
NEXT D

* Calculate overtime
IF WEEK.HOURS > 40 THEN
   REGULAR.HOURS = 40
   OVERTIME.HOURS = WEEK.HOURS - 40
END ELSE
   REGULAR.HOURS = WEEK.HOURS
   OVERTIME.HOURS = 0
END
```

---

### 2.3 PTO Accrual

**Accrual Rules:**

| Years of Service | Annual PTO | Accrual Rate |
|------------------|------------|--------------|
| 0-1 years | 80 hours | 6.67 hrs/month |
| 2-4 years | 120 hours | 10 hrs/month |
| 5-9 years | 160 hours | 13.33 hrs/month |
| 10+ years | 200 hours | 16.67 hrs/month |

**Monthly Accrual Process:**
```unibasic
* Calculate accrual rate based on tenure
TENURE.YEARS = calculate_tenure(HIRE.DATE, SYSTEM.DATE)

BEGIN CASE
   CASE TENURE.YEARS >= 10
      MONTHLY.ACCRUAL = 16.67
   CASE TENURE.YEARS >= 5
      MONTHLY.ACCRUAL = 13.33
   CASE TENURE.YEARS >= 2
      MONTHLY.ACCRUAL = 10.00
   CASE 1
      MONTHLY.ACCRUAL = 6.67
END CASE

* Add to employee balance
EMP.REC<PTO.BALANCE> += MONTHLY.ACCRUAL
```

---

## 3. SCHEDULING

Employee scheduling with shift management and conflict detection.

### 3.1 Create Schedule (EMP.SCHEDULE)

**Purpose:** Create employee work schedules

**Program:** `BP/EMP.SCHEDULE`

**Input Parameters:**
```unibasic
SCHEDULE.REC   ; Schedule information
SCHEDULE.ID    ; Generated schedule ID (output)
ERROR.CODE     ; Error code (output)
ERROR.MSG      ; Error message (output)
```

**Schedule Record:**
```unibasic
SCHEDULE.REC<1> = employee_id
SCHEDULE.REC<2> = work_date
SCHEDULE.REC<3> = shift_start_time
SCHEDULE.REC<4> = shift_end_time
SCHEDULE.REC<5> = position
SCHEDULE.REC<6> = location
SCHEDULE.REC<7> = break_time
SCHEDULE.REC<8> = notes
```

**Validation:**
```
1. Employee is active
2. No conflicting shifts
3. Not exceeding max hours per week
4. Meets minimum rest period (8 hours between shifts)
5. Within labor law limits
```

---

### 3.2 Shift Management

**Shift Types:**
- **OPENING:** 6:00 AM - 2:00 PM
- **MID:** 10:00 AM - 6:00 PM
- **CLOSING:** 2:00 PM - 10:00 PM
- **SPLIT:** Multiple periods
- **ON.CALL:** As needed

---

### 3.3 Conflict Detection

**Conflict Types:**
- **Double Booking:** Same employee, overlapping times
- **Insufficient Rest:** < 8 hours between shifts
- **Overtime Risk:** Would exceed 40 hours/week
- **Availability Conflict:** Employee marked unavailable
- **Max Hours:** Exceeds weekly limit

---

## 4. COMMISSION TRACKING

Commission calculation and reporting for sales employees.

### 4.1 Commission Calculation

**Commission Structure:**
```
Base Commission Rate: Set per employee (typically 1-5%)
Plus Tier Bonuses based on monthly sales:

Monthly Sales    Bonus Rate
-------------    ----------
< $5,000        0%
$5,000-$9,999   0.5%
$10,000-$24,999 1.0%
$25,000-$49,999 1.5%
$50,000+        2.0%
```

**Calculation:**
```unibasic
* Get employee's base commission rate
BASE.RATE = EMP.REC<COMMISSION.RATE>  ; e.g., 3%

* Calculate monthly sales
MONTHLY.SALES = sum_sales_for_month(EMP.ID, MONTH)

* Calculate base commission
BASE.COMMISSION = MONTHLY.SALES * (BASE.RATE / 100)

* Calculate tier bonus
IF MONTHLY.SALES >= 50000 THEN
   TIER.BONUS = MONTHLY.SALES * 0.02
END ELSE IF MONTHLY.SALES >= 25000 THEN
   TIER.BONUS = MONTHLY.SALES * 0.015
END ELSE IF MONTHLY.SALES >= 10000 THEN
   TIER.BONUS = MONTHLY.SALES * 0.01
END ELSE IF MONTHLY.SALES >= 5000 THEN
   TIER.BONUS = MONTHLY.SALES * 0.005
END ELSE
   TIER.BONUS = 0
END

* Total commission
TOTAL.COMMISSION = BASE.COMMISSION + TIER.BONUS
```

---

*Document Version: 1.0*
*Last Updated: 2025-10-06*
*Part: 4 of 8*
