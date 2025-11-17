RETAIL POS SYSTEM - UTILITIES
==============================

This directory contains utility scripts for system administration and maintenance.

AVAILABLE UTILITIES:
--------------------

1. COMPILE.ALL.sh
   Purpose: Compile all UniBasic programs in the BP directory
   Usage: ./COMPILE.ALL.sh
   When to use: After adding or modifying programs, or during initial setup

2. CREATE.FILES.sh
   Purpose: Create all required database files
   Usage: ./CREATE.FILES.sh
   When to use: During initial system setup

3. BACKUP.sh
   Purpose: Create a compressed backup of all database files
   Usage: ./BACKUP.sh
   When to use: Before major changes, or scheduled nightly backups
   Output: BACKUP/BACKUP_YYYYMMDD_HHMMSS.tar.gz

4. RESTORE.sh
   Purpose: Restore database files from a backup
   Usage: ./RESTORE.sh <backup_file.tar.gz>
   When to use: After data corruption or to restore to a previous state
   Example: ./RESTORE.sh BACKUP/BACKUP_20251007_020000.tar.gz

5. IMPORT.DATA.sh
   Purpose: Import data from text files into database
   Usage: ./IMPORT.DATA.sh <data_file> <target_table>
   When to use: Loading sample data or migrating data
   Example: ./IMPORT.DATA.sh SAMPLE.CUSTOMERS.txt CUSTOMERS

SCHEDULED TASKS:
----------------
The following utilities should be scheduled to run automatically:

- BACKUP.sh: Daily at 2:00 AM
- COMPILE.ALL.sh: After deployments

PERMISSIONS:
------------
Ensure these scripts have execute permissions:
chmod +x *.sh

LOGGING:
--------
All utilities write output to the console.
For scheduled tasks, redirect output to log files:
./BACKUP.sh > LOGS/backup.$(date +%Y%m%d).log 2>&1

TROUBLESHOOTING:
----------------
If a utility fails:
1. Check that the system directory path is correct
2. Ensure proper file permissions
3. Verify database connectivity
4. Check available disk space
5. Review error messages in the console output

BEST PRACTICES:
---------------
- Always test utilities in a development environment first
- Keep regular backups before running maintenance tasks
- Schedule backups during low-activity periods
- Monitor disk space for backup storage
- Test restore procedures regularly
- Document any custom modifications to these scripts
