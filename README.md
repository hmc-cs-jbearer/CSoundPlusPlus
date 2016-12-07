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

## Usage

Before beginning to use CSound++, you must
[install the latest version of CSound](http://csound.github.io/download.html). CSound is another
audio signal processing DSL, which CSound++ uses internally.

Once you've done that, go ahead and download a copy of the CSound++ source by entering the following
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
minutes, after which you are ready to run `cspp`:
```bash
java -jar bin/cspp.jar --help
```

At this point, you're ready to start writing some programs. Say you've written a CSound++ program
called `mysynth.csp`. There are two steps to run this program. First, compile it to an executable:
```bash
java -jar bin/cspp.jar compile mysynth.csp -o mysynth
```
Then, create a MIDI score &mdash; `myscore.mid`, for example &mdash; and play the score with your
new synth, by running either:
```bash
java -jar bin/cspp.jar play mysynth myscore.mid
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
