#!/bin/bash
# Database Backup Script
# This script backs up all database files

SYSTEM_DIR="C:\Users\surya.mukka\Desktop\Cursor\AI-Repos\Unibasic AI Repo\RETAIL-POS-SYSTEM"
DATA_DIR="$SYSTEM_DIR/DATA"
BACKUP_DIR="$SYSTEM_DIR/BACKUP"
DATE_STAMP=$(date +%Y%m%d)
TIME_STAMP=$(date +%H%M%S)
BACKUP_NAME="BACKUP_${DATE_STAMP}_${TIME_STAMP}"

echo "======================================"
echo "RETAIL POS SYSTEM - BACKUP"
echo "======================================"
echo "Started at: $(date)"
echo ""

# Create backup directory if it doesn't exist
mkdir -p "$BACKUP_DIR/$BACKUP_NAME"

echo "Backup destination: $BACKUP_DIR/$BACKUP_NAME"
echo ""

# List of files to backup
FILES=(
    "CUSTOMERS"
    "INVENTORY"
    "EMPLOYEES"
    "POS.TRANS"
    "PURCHASE.ORDERS"
    "VENDORS"
    "STORES"
    "PROMOTIONS"
    "LOYALTY.TRANS"
    "GIFTCARDS"
)

# Backup each file
for FILE in "${FILES[@]}"; do
    echo "Backing up $FILE..."
    cp -r "$DATA_DIR/$FILE" "$BACKUP_DIR/$BACKUP_NAME/" 2>&1
    if [ $? -eq 0 ]; then
        echo "  ✓ Success"
    else
        echo "  ✗ Failed"
    fi
done

# Compress the backup
echo ""
echo "Compressing backup..."
cd "$BACKUP_DIR" || exit 1
tar -czf "${BACKUP_NAME}.tar.gz" "$BACKUP_NAME" 2>&1

if [ $? -eq 0 ]; then
    echo "  ✓ Compression successful"
    # Remove uncompressed backup
    rm -rf "$BACKUP_NAME"

    # Get backup size
    SIZE=$(du -h "${BACKUP_NAME}.tar.gz" | cut -f1)
    echo ""
    echo "======================================"
    echo "Backup completed successfully"
    echo "File: ${BACKUP_NAME}.tar.gz"
    echo "Size: $SIZE"
    echo "======================================"
else
    echo "  ✗ Compression failed"
    exit 1
fi

# Clean up old backups (keep last 30 days)
echo ""
echo "Cleaning up old backups..."
find "$BACKUP_DIR" -name "BACKUP_*.tar.gz" -mtime +30 -delete
echo "  ✓ Old backups removed"

echo "Completed at: $(date)"
exit 0
