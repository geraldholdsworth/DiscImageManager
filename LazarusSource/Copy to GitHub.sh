#!/bin/sh
#
# ------------ Setup --------------
#
# Common variables for the script
#
userdir='/./Users/geraldholdsworth'
folder="$userdir/Library/Mobile Documents/com~apple~CloudDocs/Programming/Lazarus/DiscImageManager"
gitfolder="$userdir/Documents/GitHub/DiscImageManager"
appname='Disc Image Manager'
appfile='DiscImageManager'
clifile='DiscImageManagerCLI'
iconfile='Icon'
#
# ------------ Function definitions ------------
#
function ZIPfiles ()
{
 local app="$appfile"
 local cli="$clifile"
 if [ "$4" == "Windows" ]
  then
   local app="$appfile.exe"
   local cli="$clifile.exe"
 fi
 if [ -e "$folder/lib/Release/$2/$app" ]
  then
   echo "*********************************************"
   echo "$1"
   echo "*********************************************"
   if [ -e "$folder/lib/Release/$2/$3.zip" ]
    then
     rm "$folder/lib/Release/$2/$3.zip"
   fi
   cd "$folder/lib/Release/$2"
   zip -v "$folder/lib/Release/$2/$3.zip" "$app"
   if [ -e "$folder/lib-cli/Release/$2/$cli" ]
    then
     cd "$folder/lib-cli/Release/$2"
     zip -v "$folder/lib/Release/$2/$3.zip" "$cli"
   fi
   cd "$folder/Documentation/User Guide"
   zip -v "$folder/lib/Release/$2/$3.zip" "Disc Image Manager User Guide.pdf"
   cd "$folder"
   mv -v "$folder/lib/Release/$2/$3.zip" "$userdir/Desktop"
 fi
}
function CreateDMG ()
{
 # Application folder name
 local appfolder="$appname.app"
 # If it already exists, remove it
 if [ -e "$appfolder" ]
  then
   rm -r "$appfolder"
 fi
 # macOS folder name
 local macosfolder="$userdir/Desktop/$appfolder/Contents/MacOS"
 # macOS plist filename
 local plistfile="$userdir/Desktop/$appfolder/Contents/Info.plist"
 if [ -e "lib/Release/$2/$appfile" ] 
  then
   echo "*********************************************"
   echo "Mac OS $1"
   echo "*********************************************"
   echo "Creating $appfolder..."
   mkdir "$userdir/Desktop/$appfolder"
   mkdir "$userdir/Desktop/$appfolder/Contents"
   mkdir "$userdir/Desktop/$appfolder/Contents/MacOS"
   mkdir "$userdir/Desktop/$appfolder/Contents/Frameworks"  # optional, for including libraries or frameworks
   mkdir "$userdir/Desktop/$appfolder/Contents/Resources"
   PkgInfoContents="APPLMAG#"
   cp "lib/Release/$2/$appfile" "$macosfolder/$appname"
   #
   # Copy the resource files to the correct place
   #
   if [ -e "$iconfile.icns" ]
    then
     cp "$iconfile.icns" "$userdir/Desktop/$appfolder/Contents/Resources"
   fi
   #
   # Create PkgInfo file.
   #
   echo $PkgInfoContents >"$userdir/Desktop/$appfolder/Contents/PkgInfo"
   #
   # Create information property list file (Info.plist).
   #
   echo '<?xml version="1.0" encoding="UTF-8"?>' >>"$plistfile"
   echo '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' >>"$plistfile"
   echo '<plist version="1.0">' >>"$plistfile"
   echo ' <dict>' >>"$plistfile"
   echo '  <key>CFBundleDevelopmentRegion</key>' >>"$plistfile"
   echo '  <string>English</string>' >>"$plistfile"
   echo '  <key>CFBundleExecutable</key>' >>"$plistfile"
   echo '  <string>'$appname'</string>' >>"$plistfile"
   echo '  <key>CFBundleIconFile</key>' >>"$plistfile"
   echo '  <string>'$iconfile'.icns</string>' >>"$plistfile"
   echo '  <key>CFBundleIdentifier</key>' >>"$plistfile"
   echo '  <string>com.geraldholdsworth.'$appname'</string>' >>"$plistfile"
   echo '  <key>CFBundleInfoDictionaryVersion</key>' >>"$plistfile"
   echo '  <string>6.0</string>' >>"$plistfile"
   echo '  <key>CFBundlePackageType</key>' >>"$plistfile"
   echo '  <string>APPL</string>' >>"$plistfile"
   echo '  <key>CFBundleSignature</key>' >>"$plistfile"
   echo '  <string>MAG#</string>' >>"$plistfile"
   #Extensions we can deal with
   declare -a local exts=("SSD" "DSD" "ADS" "ADM" "ADL" "ADF" "DAT" "HDF" "D64" "D71" "D81" "DSK" "UEF" "MMB" "AFS" "ZIP" "IMG" "FAT12" "FAT16" "FAT32")
   echo '  <key>CFBundleDocumentTypes</key>' >>"$plistfile"
   echo '  <array>' >>"$plistfile"
   echo '   <dict>' >>"$plistfile"
   echo '    <key>CFBundleTypeName</key>' >>"$plistfile"
   echo '    <string>Disc Image</string>' >>"$plistfile"
   echo '    <key>CFBundleTypeExtensions</key>' >>"$plistfile"
   echo '    <array>' >>"$plistfile"
   for i in "${exts[@]}"
   do
    echo '     <string>'$i'</string>' >>"$plistfile"
   done
   echo '    </array>' >>"$plistfile"
   echo '   </dict>' >>"$plistfile"
   echo '  </array>' >>"$plistfile"
   echo '  <key>CFBundleVersion</key>' >>"$plistfile"
   echo '  <string>'$appversion'</string>' >>"$plistfile"
   echo ' </dict>' >>"$plistfile"
   echo '</plist>' >>"$plistfile"
   xattr -cr "$userdir/Desktop/$appfolder"
   echo "Application $appname created"
   echo "Creating DMG"
   #
   # Create DMG
   #
   APP=`echo "$appfolder" | sed "s/\.app//"`
   DATE=`date "+%d%m%Y"`
   appdesc=" $appversion macOS $1"
   VOLUME="${APP}$appdesc" #_${DATE}"
   echo "Application name: ${APP}"
   echo "Volume name: ${VOLUME}"
   if [ -r "${APP}*.dmg.sparseimage" ]
    then
     rm "${APP}*.dmg.sparseimage"
   fi
   if [ -e "${VOLUME}.dmg" ]
    then
     rm "${VOLUME}.dmg"
   fi
   hdiutil create -size 45M -type SPARSE -volname "${VOLUME}" -fs HFS+ "${VOLUME}.dmg"
   hdiutil attach "${VOLUME}.dmg.sparseimage"
   cp -R "$userdir/Desktop/${APP}.app" "/Volumes/${VOLUME}/"
   cp "Documentation/User Guide/Disc Image Manager User Guide.pdf" "/Volumes/${VOLUME}/"
   if [ -e "lib-cli/Release/$2/$clifile" ]
    then
     cp "lib-cli/Release/$2/$clifile" "/Volumes/${VOLUME}/"
   fi
   hdiutil detach -force "/Volumes/${VOLUME}"
   hdiutil convert "${VOLUME}.dmg.sparseimage" -format UDBZ -o "${VOLUME}.dmg" -ov -imagekey zlib-level=9
   rm "${VOLUME}.dmg.sparseimage"
   #
   # Now we move the files
   #
   mv -v "$userdir/Desktop/$appfolder" "lib/Release/$2/$appfolder"
   mv -v "${VOLUME}.dmg" "$userdir/Desktop"
 fi 
}
# ------------ CODE STARTS HERE ------------
#
# Check to ensure the source folder exists
#
if [ -e "$folder" ]
 then
  cd "$folder"
 else
  echo "$folder does not exist"
  exit
fi
echo "*********************************************"
echo "Create application bundles and copy to GitHub"
echo "$appname"
echo "*********************************************"
#
# Ask the user for the version number
#
echo "Enter the version number"
read appversion
#
# ------------ macOS 64 bit --------------
#
CreateDMG "64 bit Intel" "x86_64-darwin"
#
# ------------ macOS 32 bit --------------
#
CreateDMG "32 bit Intel" "i386-darwin"
#
# ------------ macOS ARM --------------
#
CreateDMG "64 bit ARM" "aarch64-darwin"
#
# ------------ Linux 64 bit --------------
#
ZIPfiles "Linux 64 bit" "x86_64-linux" "$appname $appversion Linux 64 bit Intel" "Linux"
#
# ------------ Linux 32 bit --------------
#
ZIPfiles "Linux 32 bit" "i386-linux" "$appname $appversion Linux 32 bit Intel" "Linux"
#
# ------------ Linux ARM 32 bit --------------
#
ZIPfiles "Linux 32 bit ARM" "arm-linux" "$appname $appversion Linux 32 bit ARM" "Linux"
#
# ------------ Linux ARM 64 bit --------------
#
ZIPfiles "Linux 64 bit ARM" "aarch64-linux" "$appname $appversion Linux 64 bit ARM" "Linux"
#
# ------------ Windows 64 bit --------------
#
ZIPfiles "Windows 64 bit" "x86_64-win64" "$appname $appversion Windows 64 bit Intel" "Windows"
#
# ------------ Windows 32 bit --------------
#
ZIPfiles "Windows 32 bit" "i386-win32" "$appname $appversion Windows 32 bit Intel" "Windows"
#
# Now copy the source files to GIT
#
echo "*********************************************"
echo "Copying source files to GIT"
echo "*********************************************"
cp -v *\.pas "$gitfolder/LazarusSource"
cp -v *\.lfm "$gitfolder/LazarusSource"
cp -v "$appfile.lpi" "$gitfolder/LazarusSource"
cp -v "$appfile.lpr" "$gitfolder/LazarusSource"
cp -v "$appfile.lps" "$gitfolder/LazarusSource"
cp -v "$appfile.res" "$gitfolder/LazarusSource"
cp -v "Documentation/Changes.txt" "$gitfolder/Documentation"
cp -v "Documentation/ToDo.txt" "$gitfolder/Documentation"
cp -v "Documentation/User Guide/Disc Image Manager User Guide.pdf" "$gitfolder/Documentation"
cp -v "Documentation/User Guide/Disc Image Manager User Guide.docx" "$gitfolder/Documentation"
cp -v -R "Graphics" "$gitfolder"