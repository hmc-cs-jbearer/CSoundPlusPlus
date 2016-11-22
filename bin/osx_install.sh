#! /bin/bash

set -ev

# Install python3
brew update
brew install python3

# Install CSound
CSOUND_PKG="csound6.06-OSX-universal"
curl -sL "https://sourceforge.net/projects/csound/files/csound6/Csound6.06/$CSOUND_PKG.dmg/" > "$CSOUND_PKG.dmg"
sudo hdiutil attach "$CSOUND_PKG.dmg"
sudo installer -package "/Volumes/Csound 6.06/$CSOUND_PKG.pkg" -target /
sudo hdiutil detach "/Volumes/Csound 6.06"

# Install sbt
which sbt || true
mkdir -p /Users/travis/bin
sudo curl -s https://raw.githubusercontent.com/paulp/sbt-extras/master/sbt > /Users/travis/bin/sbt
sudo chmod 0755 /Users/travis/bin/sbt
which sbt || true

# We're running with the sbt launcher in the project root directory, but the integration tests call
# sbt expecting it to be on the path. So we fix that with a bit of a hack here.

#export PATH="`pwd`:$PATH"
echo $PATH
