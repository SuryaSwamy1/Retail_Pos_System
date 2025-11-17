#!/bin/bash
# Database Restore Script
# This script restores database files from a backup

SYSTEM_DIR="C:\Users\surya.mukka\Desktop\Cursor\AI-Repos\Unibasic AI Repo\RETAIL-POS-SYSTEM"
DATA_DIR="$SYSTEM_DIR/DATA"
BACKUP_DIR="$SYSTEM_DIR/BACKUP"

echo "======================================"
echo "RETAIL POS SYSTEM - RESTORE"
echo "======================================"
echo ""

# Check if backup file is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <backup_file.tar.gz>"
    echo ""
    echo "Available backups:"
    ls -lh "$BACKUP_DIR"/*.tar.gz 2>/dev/null
    exit 1
fi

BACKUP_FILE="$1"

# Check if backup file exists
if [ ! -f "$BACKUP_FILE" ]; then
    echo "Error: Backup file not found: $BACKUP_FILE"
    exit 1
fi

echo "WARNING: This will overwrite existing data!"
echo "Backup file: $BACKUP_FILE"
echo ""
read -p "Are you sure you want to continue? (yes/no): " CONFIRM

if [ "$CONFIRM" != "yes" ]; then
    echo "Restore cancelled"
    exit 0
fi

echo ""
echo "Starting restore at: $(date)"
echo ""

# Create temporary directory
TEMP_DIR="$BACKUP_DIR/temp_restore"
mkdir -p "$TEMP_DIR"

# Extract backup
echo "Extracting backup..."
tar -xzf "$BACKUP_FILE" -C "$TEMP_DIR" 2>&1

if [ $? -ne 0 ]; then
    echo "  ✗ Extraction failed"
    rm -rf "$TEMP_DIR"
    exit 1
fi
echo "  ✓ Extraction successful"

# Find the extracted directory
EXTRACTED_DIR=$(find "$TEMP_DIR" -maxdepth 1 -type d -name "BACKUP_*" | head -1)

if [ -z "$EXTRACTED_DIR" ]; then
    echo "Error: Could not find backup data in extracted files"
    rm -rf "$TEMP_DIR"
    exit 1
fi

# Restore files
echo ""
echo "Restoring files..."

for FILE in "$EXTRACTED_DIR"/*; do
    FILENAME=$(basename "$FILE")
    echo "Restoring $FILENAME..."

    # Backup current file if it exists
    if [ -d "$DATA_DIR/$FILENAME" ]; then
        mv "$DATA_DIR/$FILENAME" "$DATA_DIR/${FILENAME}.OLD"
    fi

    # Restore file
    cp -r "$FILE" "$DATA_DIR/" 2>&1

    if [ $? -eq 0 ]; then
        echo "  ✓ Success"
        # Remove old backup
        rm -rf "$DATA_DIR/${FILENAME}.OLD"
    else
        echo "  ✗ Failed"
        # Restore old file if restore failed
        if [ -d "$DATA_DIR/${FILENAME}.OLD" ]; then
            mv "$DATA_DIR/${FILENAME}.OLD" "$DATA_DIR/$FILENAME"
        fi
    fi
done

# Clean up
echo ""
echo "Cleaning up..."
rm -rf "$TEMP_DIR"

echo ""
echo "======================================"
echo "Restore completed successfully"
echo "======================================"
echo "Completed at: $(date)"

exit 0
