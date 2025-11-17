#!/bin/bash
# End of Day Batch Process Runner
# This script executes the EOD batch process

SYSTEM_DIR="C:\Users\surya.mukka\Desktop\Cursor\AI-Repos\Unibasic AI Repo\RETAIL-POS-SYSTEM"
LOG_DIR="$SYSTEM_DIR/LOGS"
DATE_STAMP=$(date +%Y%m%d)
TIME_STAMP=$(date +%H%M%S)

echo "Starting EOD Batch Process at $(date)"

# Execute the EOD batch program
cd "$SYSTEM_DIR"
EXECUTE BATCH.EOD > "$LOG_DIR/EOD.$DATE_STAMP.$TIME_STAMP.log" 2>&1

if [ $? -eq 0 ]; then
    echo "EOD Batch Process completed successfully at $(date)"
else
    echo "EOD Batch Process failed at $(date)"
    exit 1
fi
