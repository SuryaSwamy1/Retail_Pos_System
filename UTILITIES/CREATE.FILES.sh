#!/bin/bash
# Create Database Files Script
# This script creates all required database files

SYSTEM_DIR="C:\Users\surya.mukka\Desktop\Cursor\AI-Repos\Unibasic AI Repo\RETAIL-POS-SYSTEM"
DATA_DIR="$SYSTEM_DIR/DATA"

echo "======================================"
echo "RETAIL POS SYSTEM - CREATE FILES"
echo "======================================"
echo "Started at: $(date)"
echo ""

cd "$SYSTEM_DIR" || exit 1

# Create main data files
echo "Creating database files..."

echo "  - CUSTOMERS"
CREATE.FILE DATA CUSTOMERS 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo "  - INVENTORY"
CREATE.FILE DATA INVENTORY 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo "  - EMPLOYEES"
CREATE.FILE DATA EMPLOYEES 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo "  - POS.TRANS"
CREATE.FILE DATA POS.TRANS 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo "  - PURCHASE.ORDERS"
CREATE.FILE DATA PURCHASE.ORDERS 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo "  - VENDORS"
CREATE.FILE DATA VENDORS 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo "  - STORES"
CREATE.FILE DATA STORES 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo "  - PROMOTIONS"
CREATE.FILE DATA PROMOTIONS 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo "  - LOYALTY.TRANS"
CREATE.FILE DATA LOYALTY.TRANS 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo "  - GIFTCARDS"
CREATE.FILE DATA GIFTCARDS 1,1 TYPE=DIR
if [ $? -eq 0 ]; then echo "    ✓ Created"; else echo "    ✗ Failed"; fi

echo ""
echo "======================================"
echo "Database files created successfully"
echo "======================================"
echo "Completed at: $(date)"

exit 0
