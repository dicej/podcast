#!/bin/bash

# This file, intended to be sourced from another script, receives its
# configuration via environment variables, generates file names,
# titles, recording duration, etc. according to certain conventions,
# and provides functions for uploading the file, updating an RSS feed,
# and publishing to Series Engine.  The enclosing script can then call
# those functions in order to actually publish the recording.

set -e

for x in server http_server album link_uri download_uri feed_path download_dir file date time performer title; do
  if [[ -z "${!x}" ]]; then
    >&2 echo "$x must be set in environment"
    exit 1
  fi
done

dateArray=(${date//-/ })
year=${dateArray[0]}
month=${dateArray[1]}
day=${dateArray[2]}

timeArray=(${time//:/ })
hour=${timeArray[0]}
minute=${timeArray[1]}

# generate the full title, e.g. "September 14 - The Title"

function month-day {
  if [ "$(uname)" == "Linux" ]; then
    date -d "$1" '+%B %-d'
  else # BSD date, we assume
    date -j -f '%Y-%m-%d %H:%M' "$1" '+%B %-d'
  fi
}

full_title="$(month-day "$year-$month-$day $hour:$minute") - $title"

# generate the podcast title, e.g.
# "September 14th, 2014 - The Title - Joe Performer"

function month-day-year {
  if [ "$(uname)" == "Linux" ]; then
    date -d "$1" '+%B %-d, %Y'
  else # BSD date, we assume
    date -j -f '%Y-%m-%d %H:%M' "$1" '+%B %-d, %Y'
  fi
}

function pretty-day {
  echo "$1" \
    | sed "s/11,/11th,/" \
    | sed "s/12,/12th,/" \
    | sed "s/13,/13th,/" \
    | sed "s/1,/1st,/" \
    | sed "s/2,/2nd,/" \
    | sed "s/3,/3rd,/" \
    | sed "s/\([0-9]\),/\\1th,/"
}

podcast_title="$(pretty-day "$(month-day-year "$year-$month-$day $hour:$minute")") - $title - $performer"

# determine the duration in minutes and seconds

duration="$(mp3info -p %m "$file"):$(printf %02d $(mp3info -p %s "$file"))"

# determine file size in bytes

function file-size {
  if [ "$(uname)" == "Linux" ]; then
    du -b "$1" | cut -f 1
  else # BSD, we assume
    stat -f%z "$1"
  fi
}

size="$(file-size "$file")"

# set id3 tags according to above fields

id3v2 --TIT2 "$full_title" --TALB "$album" --TCOM "$performer" --TPE1 "$performer" --TYER "$year" --TDAT "$day$month" --TIME "$hour$minute" "$file"

# add image (todo: make this configurable)

eyeD3 --add-image=teaching.png:FRONT_COVER "$file"

if [ -z "$id" ]; then
  # get next item identifier from server
  let id="$(do-ssh "$server" ls "$download_dir" | sort -n | tail -n 1) + 1"
fi

# generate download url

function url-encode {
  perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$1"
}

function sanitize {
  echo "$1" | sed 's/?//g'
}

full_title_sanitized="$(sanitize "$full_title")"

download_url="$http_server/$download_uri/$id/$(url-encode "$full_title_sanitized").mp3"

function upload-file {
  # make new directory on server

  $dry_run do-ssh "$server" mkdir "$download_dir/$id"

  # copy file to server under new name

  ln "$file" "$full_title_sanitized.mp3"
  $dry_run do-scp "$full_title_sanitized.mp3" "$server:$download_dir/$id/"
  rm "$full_title_sanitized.mp3"
}

function update-feed {
  # download feed.xml from server

  do-scp "$server:$feed_path" old.xml

  # edit feed.xml, adding a new item and removing the oldest one

  runhaskell update-feed.hs "$podcast_title" "$http_server/$link_uri" "$download_url" "$size" "$duration" "$performer" "$(date +'%a, %d %b %Y %H:%M:%S %z')" "$id at $http_server"  < old.xml > new.xml

  # upload new feed.xml to server

  $dry_run do-scp new.xml "$server:$feed_path"
}

function encode-url-param-array {
  for x in $2; do echo -n "&$1=$x"; done
}

function update-series {
  # log in to wordpress add entry to series engine

  for x in series_list speaker wordpress_user wordpress_password location; do
    if [[ -z "${!x}" ]]; then
      >&2 echo "$x must be set in environment"
      exit 1
    fi
  done

  agent='Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.120 Safari/537.36'

  encoding='Accept-Encoding: gzip,deflate'

  $dry_run curl -A "$agent" -D cookie.txt -H "Referer: $http_server/wp-login.php?redirect_to=$(url-encode $http_server)%2Fwp-admin%2F&reauth=1" -H 'Cookie: wordpress_test_cookie=WP+Cookie+check' -H "$encoding" -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8" -H "Accept-Language: en-US,en;q=0.8" -H "Origin: $http_server" -d "log=$(url-encode $wordpress_user)&pwd=$(url-encode "$wordpress_password")&rememberme=forever&testcookie=1" "$http_server/wp-login.php"

  series_title="$title - $performer - $location"

  series_encoded="$(encode-url-param-array series%5B%5D "$series_list")"
  topics_encoded="$(encode-url-param-array topics%5B%5D "$topics_list")"

  $dry_run curl -A "$agent" -L -D foo.txt -b cookie.txt -H "$encoding" -H "Referer: http://www.denverchurch.org/wp-admin/admin.php?page=seriesengine_plugin/seriesengine_plugin.php&enmse_action=new" -d "message_title=$(url-encode "$series_title")&message_speaker=$speaker&speaker_first_name=First&speaker_last_name=Last&message_date=$year-$month-$day&message_description=&message_audio_url=$(url-encode "$download_url")&message_embed_code=$series_encoded&topic_name=$topics_encoded&message_alternate_date=&message_thumbnail=&message_alternate_toggle=No&message_alternate_label=&message_alternate_embed=&message_length=&message_audio_url_dummy=$(url-encode "$download_url")&message_audio_file_size=&message_video_length=&message_video_url=&message_video_file_size=&file_name=&file_url=&file_username=$wordpress_user&Submit=Add+New+Message" "$http_server/wp-admin/admin.php?page=seriesengine_plugin/seriesengine_plugin.php&enmse_action=new" >/dev/null
}
