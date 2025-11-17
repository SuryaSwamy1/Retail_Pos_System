#!/bin/bash
# End of Month Batch Process Runner
# This script executes the EOM batch process

SYSTEM_DIR="C:\Users\surya.mukka\Desktop\Cursor\AI-Repos\Unibasic AI Repo\RETAIL-POS-SYSTEM"
LOG_DIR="$SYSTEM_DIR/LOGS"
DATE_STAMP=$(date +%Y%m%d)
TIME_STAMP=$(date +%H%M%S)

echo "Starting EOM Batch Process at $(date)"

# Execute the EOM batch program
cd "$SYSTEM_DIR"
EXECUTE BATCH.EOM > "$LOG_DIR/EOM.$DATE_STAMP.$TIME_STAMP.log" 2>&1

if [ $? -eq 0 ]; then
    echo "EOM Batch Process completed successfully at $(date)"
else
    echo "EOM Batch Process failed at $(date)"
    exit 1
fi
