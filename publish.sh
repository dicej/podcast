#!/bin/bash

# This script takes a filename, title, performer name, time, location,
# date, and passphrase and publishes the file to denverchurch.org
# according to the configuration listed below.

export PATH=$PATH:/usr/local/bin

file="$1"
title="$2"
performer="$3"
time="$4"
location="$5"
date="$6"
passphrase="$7"

# match the series IDs in the WordPress database:
function series {
  case "$1" in
    "Acts") echo 2;;
    "Uptown") echo 4;;
    "Wash Park") echo 5;;
    "Sunday Teaching") echo 6;;
  esac
}

# match the topic IDs in the WordPress database:
function topics {
  case "$1" in
    "Acts") echo 8;;
  esac
}

# match the speaker IDs in the WordPress database:
function speakers {
  case "$1" in
    "Eric Murrell") echo 1;;
    "Michael Hidalgo") echo 2;;
    "Landon Lynch") echo 3;;
    "Carl Medearis") echo 4;;
    "Kathy Escobar") echo 5;;
    "Fran Blomberg") echo 6;;
    "Hugh Halter") echo 7;;
    "Dave Terpstra") echo 8;;
    "Jon Gettings") echo 9;;
    "Dave Neuhausel") echo 10;;
    "Nick Elio") echo 11;;
    "Ryan Taylor") echo 12;;
    "Brian Gray") echo 13;;
    "Amanda Pennington") echo 14;;
    "Serve Global Forum") echo 15;;
    "Scott Oppliger") echo 16;;
    "Leah Kostamo") echo 17;;
    "Paul Young") echo 18;;
    "Guest Speaker") echo 20;;
    "Amanda Lum") echo 21;;
  esac
}

server=u40002018@denverchurch.org
http_server=http://www.denverchurch.org
album="Denver Community Church"
link_uri=teaching
download_uri=audio/download
feed_path=clickandbuilds/dccmain1/audio/feed/feed.xml
download_dir=clickandbuilds/dccmain1/audio/download
series_list="$(series "$location") $(series "Sunday Teaching")"
topics_list=""
wordpress_user=dccmain1
wordpress_password="$(gpg --batch --passphrase "$passphrase" < wordpress.gpg)"
ssh_key=$(mktemp /tmp/temp.XXXXXX)
speaker="$(speakers "$performer")"
#dry_run=echo

trap "rm -f $ssh_key" EXIT
gpg --batch --passphrase "$passphrase" < id_rsa.gpg > $ssh_key

function do-ssh {
  ssh -o StrictHostKeyChecking=no -i $ssh_key "$@"
}

function do-scp {
  scp -o StrictHostKeyChecking=no -i $ssh_key "$@"
}

source podcast.sh

upload-file
update-feed
update-series
