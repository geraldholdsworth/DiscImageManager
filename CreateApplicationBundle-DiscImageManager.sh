#!/bin/sh
#
echo "========================================================"
echo "    Bundle and DMG creation script"
echo "========================================================"
echo "Enter your projects folder"
folder='OneDrive/Programming/Lazarus/DiscImageReader'
#read folder
echo $folder
if [ -e "$folder" ]
 then
  cd "$folder"
 else
  echo "$folder does not exist"
  exit
fi
#
# Creates the bundle
#
# Ask the user for the application name and version
echo "Enter your application name"
appname='Disc Image Manager'
#read appname
echo $appname
echo "Enter your application version"
appversion='1.05'
#read appversion
echo $appversion
#
# Ask the user for the icon filename
echo "Enter your application's icon file (.icns)"
iconfile='Icon'
#read iconfile
echo $iconfile
if ! [ -e "$iconfile.icns" ]
 then
  echo "$iconfile.icns does not exist"
  exit
fi
#
# Application folder name
appfolder="$appname.app"
# If it already exists, remove it
if [ -e "$appfolder" ]
 then
  rm -r "$appfolder"
fi
#
# macOS folder name
macosfolder="$appfolder/Contents/MacOS"
#
# macOS plist filename
plistfile="$appfolder/Contents/Info.plist"
#
# Ask the user for the project's binary
echo "Enter your project's bin filename"
appfile='DiscImageReader'
#read appfile
echo $appfile
# Make sure it exists
if ! [ -e "lib/x86_64-darwin/$appfile" ]
 then
  echo "$appfile does not exist"
  exit
fi

echo "Creating $appfolder..."
mkdir "$appfolder"
mkdir "$appfolder/Contents"
mkdir "$appfolder/Contents/MacOS"
mkdir "$appfolder/Contents/Frameworks"  # optional, for including libraries or frameworks
mkdir "$appfolder/Contents/Resources"

PkgInfoContents="APPLMAG#"

cp "lib/x86_64-darwin/$appfile" "$macosfolder/$appname"

# Copy the resource files to the correct place
cp "$iconfile.icns" "$appfolder/Contents/Resources"
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
echo '  <string>'$iconfile'.icns</string>' >>"$plistfile"
echo '  <key>CFBundleIdentifier</key>' >>"$plistfile"
echo '  <string>com.geraldholdsworth.'$appname'</string>' >>"$plistfile"
echo '  <key>CFBundleInfoDictionaryVersion</key>' >>"$plistfile"
echo '  <string>6.0</string>' >>"$plistfile"
echo '  <key>CFBundlePackageType</key>' >>"$plistfile"
echo '  <string>APPL</string>' >>"$plistfile"
echo '  <key>CFBundleSignature</key>' >>"$plistfile"
echo '  <string>MAG#</string>' >>"$plistfile"
echo '  <key>CFBundleVersion</key>' >>"$plistfile"
echo '  <string>'$appversion'</string>' >>"$plistfile"
echo '</dict>' >>"$plistfile"
echo '</plist>' >>"$plistfile"

echo "Application $appname created"
echo "Creating DMG"
#
# Create DMG
APP=`echo "$appfolder" | sed "s/\.app//"`
DATE=`date "+%d%m%Y"`
VOLUME="${APP}" #_${DATE}"

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

hdiutil create -size 15M -type SPARSE -volname "${VOLUME}" -fs HFS+ "${VOLUME}.dmg"

hdiutil attach "${VOLUME}.dmg.sparseimage"
cp -R "${APP}.app" "/Volumes/${VOLUME}/"
cp README_FIRST.TXT "/Volumes/${VOLUME}/"
hdiutil detach -force "/Volumes/${VOLUME}"

hdiutil convert "${VOLUME}.dmg.sparseimage" -format UDBZ -o "${VOLUME}.dmg" -ov -imagekey zlib-level=9
rm "${VOLUME}.dmg.sparseimage"
