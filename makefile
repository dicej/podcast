files = update-feed.hs podcast.sh id_rsa.gpg wordpress.gpg config.js publish.sh teaching.png

.PHONY: build
build: xcode $(files)
	cp $(files) build/Release/Podcastinator.app/Contents/Resources/

.PHONY: xcode
xcode:
	xcodebuild build

.PHONY: run
run: build
	./build/Release/Podcastinator.app/Contents/MacOS/Podcastinator

.PHONY: clean
clean:
	rm -rf build
