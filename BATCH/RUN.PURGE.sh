#!/bin/bash
# Data Purge Batch Process Runner
# This script executes the old data purge batch process

SYSTEM_DIR="C:\Users\surya.mukka\Desktop\Cursor\AI-Repos\Unibasic AI Repo\RETAIL-POS-SYSTEM"
LOG_DIR="$SYSTEM_DIR/LOGS"
DATE_STAMP=$(date +%Y%m%d)
TIME_STAMP=$(date +%H%M%S)

echo "Starting Data Purge Batch Process at $(date)"

# Execute the purge batch program
cd "$SYSTEM_DIR"
EXECUTE BATCH.PURGE > "$LOG_DIR/PURGE.$DATE_STAMP.$TIME_STAMP.log" 2>&1

if [ $? -eq 0 ]; then
    echo "Data Purge Batch Process completed successfully at $(date)"
else
    echo "Data Purge Batch Process failed at $(date)"
    exit 1
fi
