rem Batch File to Run a Scenario 
rem The 'rem' keyword indicates that this is a remark

rem Xcopy /E /I 0_SETUP_FILES output\input_files\ previous folder structure

call landis-ii-7 scenario.txt
rem Always use the 'call' keyword before invoking landis-ii in a batch file. The call keyword is necessary because the landis-ii command is itself a batch file.

rem copy Landis-log.txt output\Landis-log.txt previous folder structure

rem Xcopy /E /I Metadata output\metadata\ previous folder structure

pause
rem Add a pause so that you can assess whether the scenario ran to completion or whether it encountered input parameter errors or any other error.