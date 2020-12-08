#!/bin/sh
#
echo "========================================================"
echo "    Bundle creation script"
echo "========================================================"
#Set this to point to your Lazarus folder
cd "/Users/geraldholdsworth/OneDrive/Programming/Lazarus/Disc Image Reader/lib/ApplicationBundle"
#
# Creates the bundle
#

appname='Disc Image Reader'
appfolder="$appname.app"
macosfolder="$appfolder/Contents/MacOS"
plistfile="$appfolder/Contents/Info.plist"
appfile=DiscImageReader

PkgInfoContents="APPLMAG#"

#
if ! [ -e "../x86_64-darwin/$appfile" ]
then
  echo "$appfile does not exist"
elif [ -e "$appfolder" ]
then
  echo "$appfolder already exists"
else
  echo "Creating $appfolder..."
  mkdir "$appfolder"
  mkdir "$appfolder/Contents"
  mkdir "$appfolder/Contents/MacOS"
  mkdir "$appfolder/Contents/Frameworks"  # optional, for including libraries or frameworks
  mkdir "$appfolder/Contents/Resources"

#
# For a debug bundle,
# Instead of copying executable into .app folder after each compile,
# simply create a symbolic link to executable.
#
cp "../x86_64-darwin/$appfile" "$macosfolder/$appname"

# Copy the resource files to the correct place
  cp icon.icns "$appfolder/Contents/Resources"
#
# Create PkgInfo file.
  echo $PkgInfoContents >"$appfolder/Contents/PkgInfo"
#
# Create information property list file (Info.plist).
  echo '<?xml version="1.0" encoding="UTF-8"?>' >"$plistfile"
  echo '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' >>"$plistfile"
  echo '<plist version="1.0">' >>"$plistfile"
  echo '<dict>' >>"$plistfile"
  echo '  <key>CFBundleDevelopmentRegion</key>' >>"$plistfile"
  echo '  <string>English</string>' >>"$plistfile"
  echo '  <key>CFBundleExecutable</key>' >>"$plistfile"
  echo '  <string>'$appname'</string>' >>"$plistfile"
  echo '  <key>CFBundleIconFile</key>' >>"$plistfile"
  echo '  <string>icon.icns</string>' >>"$plistfile"
  echo '  <key>CFBundleIdentifier</key>' >>"$plistfile"
  echo '  <string>com.geraldholdsworth.discimagereader</string>' >>"$plistfile"
  echo '  <key>CFBundleInfoDictionaryVersion</key>' >>"$plistfile"
  echo '  <string>6.0</string>' >>"$plistfile"
  echo '  <key>CFBundlePackageType</key>' >>"$plistfile"
  echo '  <string>APPL</string>' >>"$plistfile"
  echo '  <key>CFBundleSignature</key>' >>"$plistfile"
  echo '  <string>MAG#</string>' >>"$plistfile"
  echo '  <key>CFBundleVersion</key>' >>"$plistfile"
  echo '  <string>1.4</string>' >>"$plistfile"
  echo '</dict>' >>"$plistfile"
  echo '</plist>' >>"$plistfile"
# Create DMG
bash app2dmg.sh "$appfolder" 15
fi