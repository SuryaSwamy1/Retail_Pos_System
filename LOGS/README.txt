RETAIL POS SYSTEM - LOGS DIRECTORY
===================================

This directory stores all system log files.

LOG FILES:
----------
- EOD.YYYYMMDD.HHMMSS.log - End of Day batch process logs
- EOM.YYYYMMDD.HHMMSS.log - End of Month batch process logs
- SYNC.YYYYMMDD.HHMMSS.log - Data synchronization logs
- REORDER.YYYYMMDD.HHMMSS.log - Automatic reorder process logs
- PURGE.YYYYMMDD.HHMMSS.log - Data purge process logs
- ERROR.YYYYMMDD.log - Daily error logs
- TRANSACTION.YYYYMMDD.log - Transaction logs
- AUDIT.YYYYMMDD.log - Audit trail logs
- SYSTEM.YYYYMMDD.log - General system logs

LOG RETENTION:
--------------
- Transaction logs: 1 year
- Audit logs: 7 years (compliance requirement)
- Error logs: 90 days
- Batch process logs: 30 days
- System logs: 30 days

LOG ROTATION:
-------------
Logs are automatically rotated daily at midnight.
Old logs are compressed and archived.

TROUBLESHOOTING:
----------------
Check the ERROR log first for any system issues.
Review AUDIT logs for security-related investigations.
Transaction logs can be used for transaction disputes.
