## Regarding the modification of com2tcp

I won't go into detail about com2tcp here, so please refer to the [official website](https://sourceforge.net/projects/com0com/files/com2tcp/).

First, I tested whether the version compiled with gcc on msys2 worked, and after confirming there were no particular issues, I added several features.  
The goal is to achieve wireless communication via the PC's COM port. However, standard com2tcp does not collect information such as the baud rate, bit width, parity, and stop bits set on the COM port, making it insufficient for a complete wireless communication conversion of the COM port.

Additionally, due to the diverse conditions such as connection delays or unexpected disconnections caused by the state of the MCU board relaying between the WiFi and the terminal UART, there is a possibility that connection may become permanently impossible in some cases.

We have modified the functionality to interpolate those conditions so that it can be specified via the '--terminal' option.

Note that the Visual Studio project included in the original source code remains intact, so there's no need to go out of your way to compile it with gcc.

## Requirement

- Windows PC
- msys2 or GCC Developer Lite(GDLBasalPack & GDLWin64Pack)

The included build.cmd assumes GCC Developer Lite and contains simplified compilation instructions.

## Usage about additional features

com2tcp provides a ‘--terminal \<type\>’ option, but the original version shows no sign of using it. I added two types there.
- pusr  
Supports [PUSR's proprietary implementation](https://www.pusr.com/download/T24/USR-TCP232-T24-EN%20V3.2.5.pdf). Bits for baud rate, bit width, parity, and stop bit are combined and injected into the stream.
- lsrmst  
Send the stream with [IOCTL_SERIAL_LSRMST_INSERT](https://learn.microsoft.com/windows/win32/api/winioctl/ni-winioctl-ioctl_serial_lsrmst_insert) enabled as-is.

Select and use the appropriate one depending on the MCU you are connecting to.

## Licence

[GNU General Public License v3.0](https://github.com/mukyokyo/Modified-com2tcp/blob/main/LICENSE)
