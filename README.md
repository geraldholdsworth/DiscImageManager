# Disc Image Manager
Disc Image Manager is an application used to load a retro disc image, read the catalogue and output any required files. In addition, it will also add files to the image, delete, and rename files and directories. So far, the following formats are supported:<br>
• Acorn DFS (Acorn and Watford formats) - Read and Write<br>
• Acorn ADFS, floppy formats S, M, L, D, E, E+, F, F+ and Hard Discs - Read and Write<br>
• Acorn Cassette Filing System (CFS) - Read and Write<br>
• !SparkFS - Read and Write (ZIP Format)<br>
• !PackDir - Read only<br>
• Acorn File Server Level 2 and 3, including ADFS Hybrids - Read and Write<br>
• DOS Plus (BBC Master 512) - Read and Write.<br>
• MS-DOS FAT12, FAT16 and FAT32 - Read and Write, including Long FileName support<br>
• Commodore 1541 (not 40 track) - Read and Write<br>
• Commodore 1571 - Read and Write<br>
• Commodore 1581 - Read and Write<br>
• Commodore AmigaDOS floppy and hard discs - OFS and FFS: Read and Write (not fully tested with FFS). No Directory cache or international character support as yet, and no support for rigid disk stuctures.<br>
Currently working on:<br>
• Making Commodore AmigaDOS writable<br>
• Sinclair Spectrum +3 and Amstrad images<br>
• SJ Research MDFS<br>
• Multiple images open simultaenously<br>
<br>
<a href="https://github.com/geraldholdsworth/DiscImageManager/blob/main/Documentation/Disc%20Image%20Manager%20User%20Guide.pdf">Full instructions, in PDF format</a>, are provided within the documentation folder.
<br>
For images in HFE format, I recommend you use <a href="https://sourceforge.net/projects/hxcfloppyemu/">HxC Floppy Emulator</a> to export the file in a format that Disc Image Manager understands.<br>
<H3>Note for ARM macOS users</H3>
For some reason, despite my best attempts, running the ARM version of Disc Image Manager on an ARM Mac results in <i>"Disc Image Manager" is damaged and can't be opened. You should eject the disc image</i>. To get around this, Extract the application from the DMG, then go to Terminal and enter:<br>
<B>cd path/to/application</B><br>
<B>xattr -cr "Disc Image Manager.app"</B><br>
<hr>
Another project that may interest users of Disc Image Manager is a test tool I wrote for UEF files - <a href="https://github.com/geraldholdsworth/UEFReader">UEF Reader</a>. The results of this project paved the way for UEF files to be read by Disc Image Manager.<br>
<br>
In order to compile Disc Image Manager, you will require the <a href="https://github.com/geraldholdsworth/SpriteToBitmap">TSpriteFile class</a> (specifically the files SpriteFile.pas and SpriteFilePalettes.pas), and the <a href="https://github.com/geraldholdsworth/GJHCustomComponents">TGJHCustomComponents class</a> (specifically the files GJHCustomComponents.pas, ButtonGraphics.pas, PointerGraphics.pas, RadioBoxGraphics.pas and TickBoxGraphics.pas).<br>
<br>
  Project was written in <a href="https://www.lazarus-ide.org">Lazarus</a>. Binaries are available for macOS 32 bit, 64 bit &amp; ARM, Windows 32 &amp; 64 bit, and Linux 32 bit, 64 bit, ARM 64 bit &amp; ARM 32 bit. Full source is available if you wish to compile for other systems.<br>
<br>
You might like to also check out the thread on <a href="https://stardot.org.uk/forums/viewtopic.php?f=12&t=21252">Stardot</a> concerning this project. I have also put this onto the <a href="http://eab.abime.net/index.php">English Amiga Board</a>.<br>
<br>
If you want, you can support this project, and others, by buying me a coffee (or a tea/beer/rum/etc.): https://ko-fi.com/geraldholdsworth<br>
