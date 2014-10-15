#!/bin/bash

set -e
set -x

dry_run=echo

if [[ $# != 5 ]]; then
  >&2 echo "usage: $0 <user@server> <album name> <download dir> <feed path> <mp3 name>"
  >&2 echo "  The mp3 name should be in the format e.g. 2014-09-07-10:00_John Doe_The Title.mp3"
  exit 1
fi

# parse out the fields from the filename

server="$1"
album="$2"
download_dir="$3"
feed_path="$4"
full_name="$5"

name="$(basename "$full_name")"

format='\([^-]*\)-\([^-]*\)-\([^-]*\)-\([^:]*\):\([^_]*\)_\([^_]*\)_\([^.]*\)\.mp3'

function get {
  echo "$name" | sed "s/$format/\\$1/"
}

year="$(get 1)"
month="$(get 2)"
day="$(get 3)"
hour="$(get 4)"
minute="$(get 5)"
performer="$(get 6)"
title="$(get 7)"

if [ "$title" == "$name" ]; then
  >&2 echo "invalid format: $name"
  exit 1
fi

# generate the full title, e.g. "September 14 - The Title"

function month-day {
  if [ "$(uname)" == "Linux" ]; then
    date -d "$1" '+%B %-d'
  else # BSD date, we assume
    date -j -f '%Y-%m-%d %H:%M' "$1" '+%B %-d'
  fi
}

full_title="$(month-day "$year-$month-$day $hour:$minute") - $title"

# determine the duration in minutes and seconds

duration="$(mp3info -p %m "$name"):$(printf %02d $(mp3info -p %s "$name"))"

# set id3 tags according to above fields

id3v2 --TIT2 "$full_title" --TALB "$album" --TCOM "$performer" --TPE1 "$performer" --TYER "$year" --TDAT "$day$month" --TIME "$hour$minute" "$full_name"

# get next item identifier from server

let id="$(ssh "$server" ls "$download_dir" | sort -n | tail -n 1) + 1"

# download feed.xml from server

scp "$server:$feed_path" old.xml

# edit feed.xml, adding a new item and removing the oldest one

runhaskell update-feed.hs < old.xml > new.xml

# make new directory on server

ssh "$server" mkdir "$download_dir/$id"

# copy file to server under new name

ln "$full_name" "$full_title.mp3"
$dry_run scp "$full_title.mp3" "$server:$download_dir/$id/"
rm "$full_title.mp3"

# upload new feed.xml to server

$dry_run scp new.xml "$server:$feed_path"

# log in to wordpress add entry to series engine

# todo


