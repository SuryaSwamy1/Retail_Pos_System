#!/bin/bash
# Compile All Programs Script
# This script compiles all UniBasic programs in the BP directory

SYSTEM_DIR="C:\Users\surya.mukka\Desktop\Cursor\AI-Repos\Unibasic AI Repo\RETAIL-POS-SYSTEM"
BP_DIR="$SYSTEM_DIR/BP"
LOG_DIR="$SYSTEM_DIR/LOGS"
DATE_STAMP=$(date +%Y%m%d)
TIME_STAMP=$(date +%H%M%S)

echo "======================================"
echo "RETAIL POS SYSTEM - COMPILE ALL"
echo "======================================"
echo "Started at: $(date)"
echo ""

cd "$BP_DIR" || exit 1

# Count total programs
TOTAL=$(ls -1 | wc -l)
CURRENT=0
SUCCESS=0
FAILED=0

echo "Found $TOTAL programs to compile"
echo ""

# Compile each program
for PROGRAM in *; do
    CURRENT=$((CURRENT + 1))
    echo "[$CURRENT/$TOTAL] Compiling $PROGRAM..."

    # Compile the program
    BASIC BP "$PROGRAM" 2>&1

    if [ $? -eq 0 ]; then
        # Catalog if compilation succeeded
        CATALOG BP "$PROGRAM" 2>&1
        if [ $? -eq 0 ]; then
            SUCCESS=$((SUCCESS + 1))
            echo "  ✓ Success"
        else
            FAILED=$((FAILED + 1))
            echo "  ✗ Catalog failed"
        fi
    else
        FAILED=$((FAILED + 1))
        echo "  ✗ Compilation failed"
    fi
    echo ""
done

echo "======================================"
echo "COMPILATION SUMMARY"
echo "======================================"
echo "Total Programs:    $TOTAL"
echo "Successful:        $SUCCESS"
echo "Failed:            $FAILED"
echo "======================================"
echo "Completed at: $(date)"

# Exit with error code if any failures
if [ $FAILED -gt 0 ]; then
    exit 1
fi

exit 0
