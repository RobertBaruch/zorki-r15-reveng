# zorki-r15-reveng
Reverse engineering of Zork I z-machine interpreter

## Tools:
  * HxD
  * Ghidra
  * AppleWin
  * MicroM8

* Download from: https://archive.org/details/ZorkI_r15_4amCrack

## Extracting sector 0:
* Get HxD, Tools > Open Disk Image, open dsk, specify 256 bytes per sector
* Edit > Select Block, start offset 0 length 100
* Right click on highlighted area, Copy
* File > New
* Edit > Paste
* File > Save As... zorki-r15-boot1.bin

## Ghidra for sector 0:
* File > Import File, the bin file, select Raw Binary, and 65C02 for language
* Options...:
  * Block name: BOOT1
  * Base addr: 0x800
* Select no for analyze now.

* In the disassembly, scroll down to 0801 (this is where BOOT1 starts).
* On 0801, right click, Disassemble.

## Understanding BOOT1

* According to Beneath Apple DOS, p8-3, 0x801-0x84C loads BOOT2, including RWTS, and jumps to it.
* 0x81F is the instruction to read the sector in track 0 to read for BOOT2. For this, it reads 0x8FF, which is 9.
* 0x8FE contains the memory page to read sectors into (0x2200). Since we read from sector 9 down to sector 0,
  we will read sector 9 into 0x2B00, sector 8 into 0x2A00, and so on to sector 0 into 0x2200.
* After loading, we call SETKBD, SETVID, and INIT [PR#0 and IN#0], and finally call BOOT2 at 0x2300.

## Extract BOOT2 via HxD:
* Edit > Select Block, start offset 0 length A00 (10 sectors).
* Save as zorki-r15-boot2.bin
* HOWEVER! There is a sector translation table at 84D that must be obeyed. Thus, "track N sector 1" is actually track N sector 13 on disk.
  * 00 0D 0B 09 07 05 03 01 0E 0C 0A 08 06 04 02 0F

2200: Sector 0 -> 0  (0, 0)
2300: Sector 1 -> 13 (0,13)
2400: Sector 2 -> 11 (0,11)
2500: Sector 3 -> 9  (0, 9)
2600: Sector 4 -> 7  (0, 7)
2700: Sector 5 -> 5  (0, 5)
2800: Sector 6 -> 3  (0, 3)
2900: Sector 7 -> 1  (0, 1)
2A00: Sector 8 -> 14 (0, 14)
2B00: Sector 9 -> 12 (0, 12)

* HOWEVER! Somehow this table is not obeyed when loading BOOT2,
and the sectors
instead are read in their physical order. I do not understand
how this happens.

Possibly the dsk image is a "DOS-order image". This would have to
mean that accessing physical sector 13 actually accesses disk
image sector 1, so the table of physical -> image is:

  * 00 07 0E 06 0D 05 0C 04 0B 03 0A 02 09 01 08 0F

Thus, logical -> physical -> image is a no-op.

I wrote a Python program (extract.py) which extracts consecutive physical sectors from the entire disk image, using the
physical->image table, with or without the mapping table.
BOOT2 is obtained by not using the mapping table.

## Load into Ghidra:
* File > Add to Program, the boot2.bin file.
* Under Options...
  * Block name: BOOT2
  * Start addr 0x2200

* Scroll to 2300, right click, Disassemble.

## Reveng

I set a breakpoint at 0x83F and ran, to see what was in 8FD: 00 23 FF. So the last instruction will jump to 2300, which is the
beginning of sector 1.

Initial IOB at 0x23C0 is set to:
* track 1
* sector 0
* DCT addr 0x23D1
* buff addr 0x0800
* command READ

There are 16 sectors per track.

After the read, we increment sector_count (at 0x2343), buff_addr page, sector, and track (if necessary), stop if the sector_count
reaches 0x1A, otherwise read again. Thus, we will read 26 sectors starting from track 1 sector 0 into 0x0800 - 0x21FF. However,

Then we jump to 0x0800.

I set this as "boot3.bin".

0800: Sector 16 (1, 0) -> 16 (1, 0)
0900: Sector 17 (1, 1) -> 29 (1,13)
0A00: Sector 18 (1, 2) -> 27 (1,11)
0B00: Sector 19 (1, 3) -> 25 (1, 9)
0C00: Sector 20 (1, 4) -> 23 (1, 7)
0D00: Sector 21 (1, 5) -> 21 (1, 5)
0E00: Sector 22 (1, 6) -> 19 (1, 3)
0F00: Sector 23 (1, 7) -> 17 (1, 1)
1000: Sector 24 (1, 8) -> 30 (1,14)
1100: Sector 25 (1, 9) -> 28 (1,12)
1200: Sector 26 (1,10) -> 26 (1,10)
1300: Sector 27 (1,11) -> 24 (1, 8)
1400: Sector 28 (1,12) -> 22 (1, 6)
1500: Sector 29 (1,13) -> 20 (1, 4)
1600: Sector 30 -> 18 (1, 2)
1700: Sector 31 -> 31 (1, 15)
1800: Sector 32 -> 32 (2, 0)
1900: Sector 33 -> 45 (2, 13)
1A00: Sector 34 ->
1B00: Sector 35
1C00: Sector 36
1D00: Sector 37
1E00: Sector 38
1F00: Sector 39
2000: Sector 40
2100: Sector 41

BOOT3:
Read T3 S0 -> 2C00 [02 00 00 0F 47 6C 48 59...]
Read T3 S1 -> 2D00 [00 59 00 5B 00 5D ...]
Read T3 S2 -> 2E00 [17 D8 00 0C ...]
Read T3 S3 -> 2F00
...
Read T4 S0 -> 3C00
...
Read T7 S7 -> 7300 (read 72 (0x48) sectors)

z-machine?

02 = interpreter version number
00 = flags
00 0F = release number (15)
47 6C = base of high memory (6C47? 476C?)
48 59 = initial PC (5948? 4859?)
...
55 47 33 41 55 35 = serial code (UG3AU5)


Use a hacked up version of dcc6502 (https://github.com/tcarmelveilleux/dcc6502) to output
bare 6502 disassembly.