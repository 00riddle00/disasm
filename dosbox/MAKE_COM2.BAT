REM interim process - assembling to EXE
REM this is for a resulting .com file to
REM retain debug info
tasm -zi %1.asm -w2
tlink -v %1.obj
tdstrip -s -c %1.exe %1.com

