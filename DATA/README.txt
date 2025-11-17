RETAIL POS SYSTEM - DATA DIRECTORY
===================================

IMPORTANT: MultiValue Database Files
-------------------------------------

This directory should contain HASHED DATABASE FILES, not text files.

In a production UniVerse/UniData environment, you would have:

ACTUAL DATABASE FILES (Binary format):
--------------------------------------
- CUSTOMERS (hashed file + data frames)
- INVENTORY (hashed file + data frames)
- EMPLOYEES (hashed file + data frames)
- POS.TRANS (hashed file + data frames)
- PURCHASE.ORDERS (hashed file + data frames)
- VENDORS (hashed file + data frames)
- STORES (hashed file + data frames)
- PROMOTIONS (hashed file + data frames)
- LOYALTY.TRANS (hashed file + data frames)
- GIFTCARDS (hashed file + data frames)

Each database file consists of:
- A primary file (contains data and overflow frames)
- Data frames (D.* files for larger databases)

CREATING DATABASE FILES:
------------------------
Use the CREATE.FILES.sh utility script:

  cd UTILITIES
  ./CREATE.FILES.sh

This will execute commands like:
  CREATE.FILE DATA CUSTOMERS 1 101 TYPE=UD
  CREATE.FILE DATA INVENTORY 1 101 TYPE=UD
  CREATE.FILE DATA EMPLOYEES 1 101 TYPE=UD
  etc.

Parameters explained:
- DATA = Account/Directory
- CUSTOMERS = File name
- 1 = Modulo (number of groups)
- 101 = Separation (records per group)
- TYPE=UD = UniData dynamic file

SAMPLE DATA FILES:
------------------
The SAMPLE.*.txt files are NOT the actual database files.
They are import files in pipe-delimited format for loading test data.

To import sample data into actual database files:

  cd UTILITIES
  ./IMPORT.DATA.sh ../DATA/SAMPLE.CUSTOMERS.txt CUSTOMERS
  ./IMPORT.DATA.sh ../DATA/SAMPLE.INVENTORY.txt INVENTORY
  ./IMPORT.DATA.sh ../DATA/SAMPLE.EMPLOYEES.txt EMPLOYEES
  etc.

MULTIVALUE DATABASE STRUCTURE:
------------------------------
MultiValue files store records with:
- Record ID (key)
- Multiple attributes (fields) separated by field marks (ASCII 254)
- Multiple values within attributes separated by value marks (ASCII 253)
- Sub-values separated by sub-value marks (ASCII 252)

Example internal format:
CUST001þJohn SmithþRETAILþ555-1234ý555-5678þjohn@email.comþ...

Where:
- þ (ASCII 254) = Field mark
- ý (ASCII 253) = Value mark (for multivalued fields like phone numbers)
- ü (ASCII 252) = Sub-value mark

FILE ACCESS FROM PROGRAMS:
--------------------------
UniBasic programs open files using:

  OPEN 'DATA','CUSTOMERS' TO F.CUSTOMERS ELSE
    PRINT "Cannot open CUSTOMERS file"
    STOP
  END

Then read/write records:

  READ CUST.REC FROM F.CUSTOMERS, CUST.ID ELSE
    PRINT "Customer not found"
  END

  WRITE CUST.REC TO F.CUSTOMERS, CUST.ID

BACKUP AND RESTORE:
-------------------
Database files should be backed up regularly using:
  cd UTILITIES
  ./BACKUP.sh

This creates compressed backups of the actual database files.

MIGRATION FROM SAMPLE DATA:
---------------------------
For production deployment:

1. Install UniVerse or UniData database
2. Create the account/directory structure
3. Run CREATE.FILES.sh to create database files
4. Run IMPORT.DATA.sh to load sample data (optional)
5. Compile all programs: cd UTILITIES && ./COMPILE.ALL.sh
6. Configure system: Edit files in CONFIG directory
7. Start using the system via MENU.MAIN

NOTE FOR DEVELOPMENT:
---------------------
Since this is a code repository (not a live database), the actual
binary database files are NOT included. Only the schema (DICT files)
and sample import data are provided.

To deploy this system, you need:
1. A server with UniVerse or UniData installed
2. Run the setup utilities to create the database structure
3. Import or create your actual data
