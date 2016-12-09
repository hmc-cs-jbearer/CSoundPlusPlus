#! /bin/bash

set -ev

# Install python3
brew update
brew install python3

# Install CSound
CSOUND_PKG="csound6.06-OSX-universal"

CURL_MAX_RETRIES=5
try=1
until [[ $try == $CURL_MAX_RETRIES ]]; do
    curl -L -m 900 "https://sourceforge.net/projects/csound/files/csound6/Csound6.06/$CSOUND_PKG.dmg/" > "$CSOUND_PKG.dmg" && break
    echo "curl failed ($?). Retrying in 1 minute."
    let try=$try+1
    sleep 60
done
if [[ $try == $CURL_MAX_RETRIES ]]; then
    echo "Failed to download CSound binary. Failed $CURL_MAX_RETRIES attempts."
    exit 1
fi

sudo hdiutil attach "$CSOUND_PKG.dmg"
sudo installer -package "/Volumes/Csound 6.06/$CSOUND_PKG.pkg" -target /
sudo hdiutil detach "/Volumes/Csound 6.06"

# Install sbt
which sbt || true
mkdir -p /Users/travis/bin
sudo curl -s https://raw.githubusercontent.com/paulp/sbt-extras/master/sbt > /Users/travis/bin/sbt
sudo chmod 0755 /Users/travis/bin/sbt
which sbt || true
