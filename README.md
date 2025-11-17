# Retail Point-of-Sale & Distribution System

## Overview
Enterprise-grade retail management system built with UniBasic for MultiValue databases (D3, Universe, UniData).

## System Architecture

### Core Modules
1. **Customer Management** - Customer profiles, loyalty, credit management
2. **Inventory Management** - Stock control, warehousing, transfers
3. **Vendor & Purchasing** - Purchase orders, receiving, vendor management
4. **Point-of-Sale** - Multi-store POS, returns, exchanges
5. **Warehouse & Distribution** - Order fulfillment, shipping, logistics
6. **Employee & HR** - Staff management, commissions, time tracking
7. **Loyalty & Promotions** - Rewards, discounts, campaigns
8. **Reporting & Analytics** - Sales analysis, inventory reports, dashboards

### Database Files
- CUSTOMERS - Customer master
- INVENTORY - Product master
- VENDORS - Vendor master
- POS.TRANS - Point-of-sale transactions
- PURCHASE.ORDERS - Purchase orders
- INVENTORY.TRANS - Inventory movements
- EMPLOYEES - Employee master
- STORES - Store master
- LOYALTY.TRANS - Loyalty transactions
- PROMOTIONS - Promotion definitions

### Directory Structure
```
BP/          - UniBasic source code
DICT/        - Dictionary definitions
DATA/        - Sample data files
LOGS/        - Application logs
REPORTS/     - Report definitions
CONFIG/      - Configuration files
BATCH/       - Batch processing programs
SUBROUTINES/ - Reusable subroutines
PROGRAMS/    - Main programs
MENUS/       - Menu definitions
UTILITIES/   - Utility programs
```

## Features
- Multi-store support with centralized inventory
- Real-time inventory tracking across locations
- Comprehensive POS with barcode scanning
- Automated reorder point management
- Commission calculation and tracking
- Customer loyalty points system
- Promotional pricing engine
- Comprehensive reporting suite
- Batch processing for EOD/EOM operations

## Technical Specifications
- Language: UniBasic
- Database: MultiValue (D3/Universe/UniData compatible)
- Lines of Code: ~70,000+
- Modules: 8 core modules
- Programs: 100+ programs and subroutines
