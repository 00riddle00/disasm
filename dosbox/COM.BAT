@echo off
REM script to assemble and link .asm file
REM full name: 'ASSEMBLE.BAT'
REM alias name: 'COM.BAT' (easier to type)
REM usage: COM FILE (without .asm extension)

if [%1]==[] goto err_args

tasm -zi %1.asm
if errorlevel 1 goto err_asm

tlink -v %1.obj
if errorlevel 1 goto err_link

type msg_success.txt
goto :eof

:err_args
    echo [ERROR] no arguments
    echo .
    goto :eof

:err_asm
    REM type msg_fail.txt
    echo [ERROR] assembler fail
    echo .
    goto :eof

:err_link
    REM type msg_fail.txt
    echo [ERROR] linker fail
    echo. 
    goto :eof

:eof
