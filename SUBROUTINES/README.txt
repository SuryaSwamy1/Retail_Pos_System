RETAIL POS SYSTEM - SUBROUTINES DIRECTORY
==========================================

This directory contains reusable subroutine code for the POS system.

SUBROUTINE ORGANIZATION:
------------------------
Subroutines are organized by functional area and stored in the BP directory
with the source code. Compiled subroutines are stored in this directory.

COMMON SUBROUTINES:
-------------------
The following subroutines are used across multiple programs:

1. UTILS.COMMON
   - Common utility functions (date formatting, string manipulation, etc.)
   - Called by: All programs

2. DB.CONNECT
   - Database connection management
   - Called by: All database programs

3. VALIDATION
   - Input validation routines
   - Called by: All data entry programs

4. FORMATTING
   - Output formatting routines
   - Called by: All display and report programs

5. SECURITY
   - Security and permission checking
   - Called by: All programs requiring authentication

6. TAX.CALC
   - Tax calculation routines
   - Called by: POS programs

7. DISCOUNT.CALC
   - Discount calculation routines
   - Called by: POS programs

8. INVENTORY.UTILS
   - Inventory-related utility functions
   - Called by: Inventory programs

9. CUSTOMER.UTILS
   - Customer-related utility functions
   - Called by: Customer programs

USAGE:
------
To call a subroutine from a program:

  CALL SUBROUTINE.NAME(PARAM1, PARAM2, PARAM3, RESULT)

Example:
  CALL UTILS.COMMON('FORMAT.DATE', INPUT.DATE, OUTPUT.DATE)

COMPILATION:
------------
Subroutines must be compiled and cataloged before use:

  BASIC BP SUBROUTINE.NAME
  CATALOG BP SUBROUTINE.NAME

BEST PRACTICES:
---------------
- Keep subroutines focused on a single responsibility
- Document all parameters and return values
- Handle errors gracefully and return status codes
- Use consistent naming conventions
- Test subroutines independently before integration
