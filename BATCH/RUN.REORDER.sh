#!/bin/bash
# Automatic Reorder Batch Process Runner
# This script executes the inventory reorder batch process

SYSTEM_DIR="C:\Users\surya.mukka\Desktop\Cursor\AI-Repos\Unibasic AI Repo\RETAIL-POS-SYSTEM"
LOG_DIR="$SYSTEM_DIR/LOGS"
DATE_STAMP=$(date +%Y%m%d)
TIME_STAMP=$(date +%H%M%S)

echo "Starting Reorder Batch Process at $(date)"

# Execute the reorder batch program
cd "$SYSTEM_DIR"
EXECUTE BATCH.REORDER > "$LOG_DIR/REORDER.$DATE_STAMP.$TIME_STAMP.log" 2>&1

if [ $? -eq 0 ]; then
    echo "Reorder Batch Process completed successfully at $(date)"
else
    echo "Reorder Batch Process failed at $(date)"
    exit 1
fi
