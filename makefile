files = update-feed.hs podcast.sh

.PHONY: build
build: xcode $(files)
	cp $(files) build/Release/Podcastinator.app/Contents/Resources/

.PHONY: xcode
xcode:
	xcodebuild build