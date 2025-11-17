#!/bin/bash
# Data Synchronization Batch Process Runner
# This script executes the data sync batch process

SYSTEM_DIR="C:\Users\surya.mukka\Desktop\Cursor\AI-Repos\Unibasic AI Repo\RETAIL-POS-SYSTEM"
LOG_DIR="$SYSTEM_DIR/LOGS"
DATE_STAMP=$(date +%Y%m%d)
TIME_STAMP=$(date +%H%M%S)

echo "Starting Data Sync Batch Process at $(date)"

# Execute the sync batch program
cd "$SYSTEM_DIR"
EXECUTE BATCH.SYNC > "$LOG_DIR/SYNC.$DATE_STAMP.$TIME_STAMP.log" 2>&1

if [ $? -eq 0 ]; then
    echo "Data Sync Batch Process completed successfully at $(date)"
else
    echo "Data Sync Batch Process failed at $(date)"
    exit 1
fi
