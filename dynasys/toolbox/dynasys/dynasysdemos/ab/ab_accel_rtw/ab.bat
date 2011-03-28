@echo off
set MATLAB=C:\Program Files\MATLAB\R2009a
"%MATLAB%\bin\win32\gmake" -f ab.mk  GENERATE_REPORT=0
