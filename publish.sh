#!/bin/bash

file="$1"
title="$2"
performer="$3"
time="$4"
location="$5"
date="$6"
passphrase="$7"

# match the series IDs in the WordPress database:
declare -A series
series["Acts"]=2
series["Uptown"]=4
series["Wash Park"]=5
series["Sunday Teaching"]=6

# match the topic IDs in the WordPress database:
declare -A topics
topics["Acts"]=8

# match the speaker IDs in the WordPress database:
declare -A speakers
speakers["Eric Murrell"]=1
speakers["Michael Hidalgo"]=2
speakers["Landon Lynch"]=3
speakers["Carl Medearis"]=4
speakers["Kathy Escobar"]=5
speakers["Fran Blomberg"]=6
speakers["Hugh Halter"]=7
speakers["Dave Terpstra"]=8
speakers["Jon Gettings"]=9
speakers["Dave Neuhausel"]=10
speakers["Nick Elio"]=11

server=u40002018@denverchurch.org
http_server=http://www.denverchurch.org
album="Denver Community Church"
link_uri=teaching
download_uri=audio/download
feed_path=clickandbuilds/dccmain1/audio/feed/feed.xml
download_dir=clickandbuilds/dccmain1/audio/download
series_list="${series[$location]} ${series[Sunday Teaching]}"
topics_list=""
wordpress_user=dccmain1
wordpress_password="$(gpg --batch --passphrase "$passphrase" < wordpress.gpg)"
dry_run=echo

speaker=${speakers[$performer]}

source podcast.sh

gpg --batch --passphrase "$passphrase" < id_rsa.gpg | ssh-add /dev/stdin

update-feed
update-series
