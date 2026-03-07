# Contributing

## Folder structure
There are five folders contained in the top level of this repository:<br>
* Blank Images : as it says, blank disc images. You may place any blank images of any formats not already included here.<br>
* Disc Image Reader : the original, read only, version of Disc Image Manager, written in Delphi.<br>
* Documentation : any relevant documentation for Disc Image Manager. Should not included reference material, with the exception of Guide To Disc Formats.<br>
* Graphics : any graphics used in the GUI side of Disc Image Manager.<br>
* LazarusSource : the source code.<br>
  
## Layout of code
The code for dealing with the actual images is in the DiscImage unit, which is split into further files where they are then included into the main DiscImage unit. The main DiscImage unit just contains the class definitions, including the procedure/functions and properties.<br>
<br>
General procedures and functions, which are part of the classes (as this includes the TSpark class as well as the main TDiscImage class) are kept in either DiscImagePrivate (for private procedures/functions) or DiscImagePublished (for procedures/functions available to the calling code). The other files in the DiscImage family of files are for each filing system which is supported.<br>
<br>
Code is invariably duplicated across these filing system files just to keep it all separate and easier to maintain.<br>
<br>
The other files are normal Lazarus units with lfm form definition files. Disc Image Manager version 1 can also be started up in console mode, and this is dealt with by the DiscImageManager.lpr file. There is another file, containing a class, used for the Console Mode - ConsoleAppUnit. When in Console Mode, the code calls routines in MainUnit so, therefore, there is also a MainUnit_Console include file.<br>

## Other code
There are other files, which were taken (with the author's permission) from Disk Image Manager used for reading in the DSK format (for Amstrad CPC and Sinclair Spectrum +3). These files are:<br>
* Comparers.pas<br>
* DSKFormat.pas<br>
* DskImage.pas<br>
* filesystem.pas<br>
* FormatAnalysis.pas<br>
* Header.pas<br>
* Utils.pas<br>
The only alterations undertaken with these should be with the commented licence header, or to comment out procedures/functions or units 'used' if they are not required (i.e., if they deal with the GUI).<br>
<br>
However, I have written my own code to deal with the DSK format, which will be getting added in due course.<br>
