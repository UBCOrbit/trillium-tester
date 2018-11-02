#!/bin/sh
cp -f ../.stack-work/install/*/bin/trillium.exe .
cp -f /mingw64/bin/libusb-1.0.dll .
curl -LO https://github.com/raspberrypi/usbboot/raw/master/win32/redist/wdi-simple.exe
