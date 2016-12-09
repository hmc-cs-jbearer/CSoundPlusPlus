# CSoundPlusPlus
_A high-level domain-specific language for digital sound generation._

[![Build Status](https://travis-ci.org/hmc-cs-jbearer/CSoundPlusPlus.svg?branch=master)](https://travis-ci.org/hmc-cs-jbearer/CSoundPlusPlus)
[![Coverage Status](https://coveralls.io/repos/github/hmc-cs-jbearer/CSoundPlusPlus/badge.svg?branch=master)](https://coveralls.io/github/hmc-cs-jbearer/CSoundPlusPlus?branch=master)

---

Welcome to CSound++, a high-level language for digital music synthesis! If you want to, take some
time to look over the
[documentation](https://github.com/hmc-cs-jbearer/CSoundPlusPlus/wiki/Documentation) and
[example programs](https://github.com/hmc-cs-jbearer/CSoundPlusPlus/tree/master/resources/examples).
Or, just follow the instructions below to get started right away!

---

## Installing dependencies

In order for CSound++ to work properly, you must install the following programs and tools. You can
find installation instructions for each by following the links.

* [Python 3](https://wiki.python.org/moin/BeginnersGuide/Download)
(make sure you get Python3, not Python2)
* [Scala](https://github.com/hmc-cs111-fall2016/hmc-cs111-fall2016.github.io/wiki/Installing-Scala)
(you only need to follow steps 1 through 3)
* [CSound](http://csound.github.io/download.html)

## Installing CSound++

Before beginning this step, make sure you've installed all of the dependencies listed above. Once
you've done that, go ahead and download a copy of the CSound++ source by entering the following
commands in a terminal:
```bash
cd ~
curl -sL https://github.com/hmc-cs-jbearer/CSoundPlusPlus/archive/master.zip > CSoundPlusPlus-master.zip
unzip CSoundPlusPlus-master
cd CSoundPlusPlus-master
```

Next, run:
```bash
sbt assembly
```
This will run some tests to ensure that CSound++ was installed properly, and then it will build the
`cspp` program, which is your entry-point to the CSound++ language. This process takes a few
minutes. Once it completes, open the file `~/.bashrc` and add the following lines at the bottom:
```bash
export CSPPTREE="$HOME/CSoundPlusPlus-master"
export PYTHONPATH="$CSPPTREE/src/main/python:$PYTHONPATH"
export PATH="$CSPPTREE/bin:$PATH"
alias cspp='java -jar "$CSPPTREE/bin/cspp.jar"'
```

Then, either restart your terminal or run
```bash
source ~/.bashrc
```

Finally, you are ready to run `cspp`:
```bash
cspp --help
```

## Usage

At this point, you're ready to start writing some programs. Say you've written a CSound++ program
called `mysynth.csp`. There are two steps to run this program. First, compile it to an executable:
```bash
cspp compile mysynth.csp -o mysynth
```
Then, create a MIDI score &mdash; `myscore.mid`, for example &mdash; and play the score with your
new synth, by running either:
```bash
cspp play mysynth myscore.mid
```
or, as a shortcut
```bash
./mysynth myscore.mid
```

You can also write the output to a WAV file, instead of playing it over the speakers, by using the
`-o` option, as in
```
./mysynth myscore.mid -o mysong.wav
```
