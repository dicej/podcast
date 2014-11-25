#!/bin/bash

set -e

for x in server http_server album link_uri download_uri feed_path download_dir full_name; do
  if [[ -z "${!x}" ]]; then
    >&2 echo "$x must be set in environment"
    exit 1
  fi
done

# parse out the fields from the filename

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

duration="$(mp3info -p %m "$full_name"):$(printf %02d $(mp3info -p %s "$full_name"))"

# determine file size in bytes

size=$(du -b "$full_name" | cut -f 1)

# set id3 tags according to above fields

id3v2 --TIT2 "$full_title" --TALB "$album" --TCOM "$performer" --TPE1 "$performer" --TYER "$year" --TDAT "$day$month" --TIME "$hour$minute" "$full_name"

if [ -z "$id" ]; then
  # get next item identifier from server
  let id="$(ssh "$server" ls "$download_dir" | sort -n | tail -n 1) + 1"
fi

# generate download url

function url-encode {
  perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$1"
}

download_url="$http_server/$download_uri/$id/$(url-encode "$full_title").mp3"

function update-feed {
  # download feed.xml from server

  scp "$server:$feed_path" old.xml

  # edit feed.xml, adding a new item and removing the oldest one

  runhaskell update-feed.hs "$full_title" "$http_server/$link_uri" "$download_url" "$size" "$duration" "$performer" "$(date +'%a, %d %b %Y %H:%M:%S %z')" "$id at $http_server"  < old.xml > new.xml

  # make new directory on server

  $dry_run ssh "$server" mkdir "$download_dir/$id"

  # copy file to server under new name

  ln "$full_name" "$full_title.mp3"
  $dry_run scp "$full_title.mp3" "$server:$download_dir/$id/"
  rm "$full_title.mp3"

  # upload new feed.xml to server

  $dry_run scp new.xml "$server:$feed_path"
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

  $dry_run curl -A "$agent" -D cookie.txt -H "Referer: $http_server/wp-login.php?redirect_to=$(url-encode $http_server)%2Fwp-admin%2F&reauth=1" -H 'Cookie: wordpress_test_cookie=WP+Cookie+check' -H "$encoding" -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8" -H "Accept-Language: en-US,en;q=0.8" -H "Origin: $http_server" -d "log=$(url-encode $wordpress_user)&pwd=$(url-encode $wordpress_password)&rememberme=forever&testcookie=1" "$http_server/wp-login.php"

  series_title="$title - $performer - $location"

  series_encoded="$(encode-url-param-array series%5B%5D "$series_list")"
  topics_encoded="$(encode-url-param-array topics%5B%5D "$topics_list")"

  $dry_run curl -A "$agent" -L -D foo.txt -b cookie.txt -H "$encoding" -H "Referer: http://www.denverchurch.org/wp-admin/admin.php?page=seriesengine_plugin/seriesengine_plugin.php&enmse_action=new" -d "message_title=$(url-encode "$series_title")&message_speaker=$speaker&speaker_first_name=First&speaker_last_name=Last&message_date=$year-$month-$day&message_description=&message_audio_url=$(url-encode "$download_url")&message_embed_code=$series_encoded&topic_name=$topics_encoded&message_alternate_date=&message_thumbnail=&message_alternate_toggle=No&message_alternate_label=&message_alternate_embed=&message_length=&message_audio_url_dummy=$(url-encode "$download_url")&message_audio_file_size=&message_video_length=&message_video_url=&message_video_file_size=&file_name=&file_url=&file_username=$wordpress_user&Submit=Add+New+Message" "$http_server/wp-admin/admin.php?page=seriesengine_plugin/seriesengine_plugin.php&enmse_action=new" >/dev/null
}
