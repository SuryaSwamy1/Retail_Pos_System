RETAIL POS SYSTEM - PROGRAMS DIRECTORY
======================================

This directory contains compiled object code for UniBasic programs.

COMPILATION:
------------
Programs are compiled from source files in the BP directory.

To compile a program:
  BASIC BP PROGRAM.NAME
  CATALOG BP PROGRAM.NAME

COMPILED PROGRAMS:
------------------
When you compile programs from the BP directory, the compiled object code
will be stored in this directory automatically.

PROGRAM CATEGORIES:
-------------------
- POS Programs: POS.START, POS.ADD.ITEM, POS.PAYMENT, POS.COMPLETE, etc.
- Inventory Programs: INV.CREATE, INV.UPDATE, INV.READ, INV.ADJUST, etc.
- Customer Programs: CUST.CREATE, CUST.UPDATE, CUST.READ, CUST.SEARCH, etc.
- Employee Programs: EMP.CREATE, EMP.UPDATE, EMP.READ, etc.
- Batch Programs: BATCH.EOD, BATCH.EOM, BATCH.REORDER, etc.
- Report Programs: RPT.SALES.DAILY, RPT.INVENTORY.STATUS, etc.
- Utility Programs: UTILS.COMMON, DB.CONNECT, etc.

EXECUTION:
----------
To execute a compiled program:
  RUN BP PROGRAM.NAME
  or
  EXECUTE PROGRAM.NAME

NOTE:
-----
This directory is automatically managed by the UniBasic compiler.
Do not manually edit or delete files in this directory unless you know
what you're doing.

To recompile all programs:
  cd BP
  BASIC *
  CATALOG *
