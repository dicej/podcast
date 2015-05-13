The Podcastinator - Publish a podcast to WordPress (Series Engine) and RSS
==========================================================================

This is a collection of scripts and a basic Mac OS UI for uploading
MP3s to a website, adding them to an RSS feed, and finally publishing
them as WordPress Series Engine messages.  It is configured by default
to publish denverchurch.org recordings and reflects the nature of that
particular site's technology stack, but could also be adapted to work
with other websites.

The user specifies the title, speaker, date, time, location, and MP3
file, and this app uses that information to do the following:

  1. Add ID3v2 tags to the MP3 with the specified info
  2. Upload the file to the website
  3. Download the RSS feed XML file from the website
  4. Add the MP3 and its metadata to the feed
  5. Upload the modified feed to the website
  6. Add a new message with the uploaded MP3 URL to Series Engine


Files
-----

  * podcast.sh: calculates and derives publishing metadata, and
    defines functions for uploading the file, updating the feed, and
    adding the Series Engine message

  * publish.sh: contains configuration data specific to
    denverchurch.org and uses podcast.sh to actually publish the
    recording

  * update-feed.hs: edits an RSS feed.xml to add a new entry and
    remove any entries older than one year

  * wordpress.gpg (not distributed): a GPG-encrypted file containing
    the WordPress admin password

  * id_rsa.gpg (not distributed): a GPG-encrypted copy of the private
    key used to SSH to the webserver


Installing Dependencies
-----------------------

  (first, install Homebrew from http://brew.sh)

    brew doctor
    brew update
    brew install mp3info id3v2 cabal-install ghc gpg
    cabal update
    cabal install cabal-install hxt


Building and Installing
-----------------------

    make
    cp -a build/Release/Podcastinator.app /Applications/
