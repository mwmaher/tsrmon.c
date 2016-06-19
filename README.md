# tsrmon.c
This is a Terminate Stay Resident (TSR) program for MS-DOS that monitors the usage of the computer's keyboard.
It is an interesting and clever bit of programming.

A terminate and stay resident (TSR) program is a computer program that uses a system call in MS-DOS operating systems to return control of the computer to the operating system, as though the program has quit, but stays resident in computer memory so it can be reactivated by a hardware or software interrupt. This technique partially overcame the MS-DOS operating system limitation of executing only one program at a time. TSR is unique to MS-DOS and not used in Windows.

Some terminate and stay resident programs were utility programs that a computer user might call up several times a day, while working in another program, using a hotkey. Borland Sidekick was an early and popular example of this type. Other TSRs serve as device drivers for hardware that the operating system did not directly support.
