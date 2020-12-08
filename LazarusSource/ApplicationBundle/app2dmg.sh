#!/bin/sh

if [ "${1}" = "" ]
  then
      echo "Usage: app2dmg.sh app_name.app disk_size_in_mb"
      exit
fi

if [ "${2}" = "" ]
  then
      echo "Usage: app2dmg.sh app_name.app disk_size_in_mb"
      exit
fi

if [ ! -r "${1}" ]
  then
    echo "${1} is not in the current directory!"
    exit
fi

APP=`echo "${1}" | sed "s/\.app//"`
DATE=`date "+%d%m%Y"`
VOLUME="${APP}" #_${DATE}"

echo "Application name: ${APP}"
echo "Volume name: ${VOLUME}"

if [ -r "${APP}*.dmg.sparseimage" ]
  then
    rm "${APP}*.dmg.sparseimage"
fi

hdiutil create -size ${2}M -type SPARSE -volname "${VOLUME}" -fs HFS+ "${VOLUME}.dmg"

hdiutil attach "${VOLUME}.dmg.sparseimage"
cp -R "${APP}.app" "/Volumes/${VOLUME}/"
cp README_FIRST.TXT "/Volumes/${VOLUME}/"
hdiutil detach -force "/Volumes/${VOLUME}"

hdiutil convert "${VOLUME}.dmg.sparseimage" -format UDBZ -o "${VOLUME}.dmg" -ov -imagekey zlib-level=9
rm "${VOLUME}.dmg.sparseimage"