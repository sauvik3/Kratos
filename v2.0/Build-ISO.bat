@echo off

DEL .\Floppy\boot.bin > nul 2>nul
DEL .\Files\KRATOS.SYS > nul 2>nul
DEL .\Images\KRATOS.IMA > nul 2>nul
DEL .\Images\KRATOS.ISO > nul 2>nul
if not exist ".\Tools\nasm.exe" goto fail
if not exist ".\Tools\bfi.exe" goto fail
if not exist ".\Tools\mkisofs.exe" goto fail

echo.
echo Assembling Bootloader ...
if not exist ".\SRC\boot\bootldr.asm" goto fail

.\Tools\nasm.exe .\SRC\boot\bootldr.asm -f bin -o .\Floppy\boot.bin

echo.
echo Done !!!

echo.
echo Assembling System ...
if not exist ".\SRC\sys\ldrstart.asm" goto fail

.\Tools\nasm.exe .\SRC\sys\ldrstart.asm -f bin -o .\Floppy\Files\KRATOS.SYS

echo.
echo Done !!!

echo.
echo Building Floppy Image ...
echo.
if not exist ".\Floppy\BOOT.BIN" goto fail
if not exist ".\Floppy\Files\KRATOS.SYS" goto fail

.\Tools\bfi.exe -b=Floppy\BOOT.BIN -t=6 -o=Floppy\Files\KRATOS.SYS -f=Images\KRATOS.IMA -l=KRATOS Floppy\Files -v

echo.
echo Done !!!

echo.
echo Building CD Image ...
echo.
if not exist ".\Images\KRATOS.IMA" goto fail

.\Tools\mkisofs.exe -J -N -l -v -b KRATOS.IMA -volid "KRATOS" -o Images\KRATOS.iso Images

echo.
echo Done !!!
goto ok

:fail
echo. Failed !!!
:ok
@pause>nul
exit