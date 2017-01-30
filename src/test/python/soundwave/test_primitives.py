import os
import sys
from unittest import TestCase

from soundwave.analysis import analyze, chiSquared
from soundwave.errors import CompileError
from soundwave.files import uniqueFile, tempPath
from soundwave.player import play
import soundwave.lang as lang
from soundwave import midi

class Amplitude(TestCase):
    def setUp(self):
        self.score = uniqueFile()
        self.velocities = [i for i in range(1, 127)]
        midi.writeFile(self.score) \
            .interpolate(len(self.velocities)).fromNote(64, 1, 32).toNote(64, 127, 32) \
        .end()

    def tearDown(self):
        os.remove(self.score)

    def play(self, orcFile):
        # Dump WAV output to a file that will soon be destroyed
        with tempPath() as outFile:
            for line in play(orcFile, self.score, outFile):
                # CSound output will be shown if the test fails
                print(line)
                yield line

    def doTest(self, source):
        with tempPath() as orcFile:
            try:
                lang.compile(orcFile, source, '-d')
            except CompileError as e:
                self.fail('SoundWave compile error: ' + e.getMessage() + '\nSource was:\n' + source)

            data = analyze(self.play(orcFile))

        self.assertEqual(len(data.amps), len(self.velocities))

        # Amplitude should be a linear function which is 0 at 0 velocity and 1 at max velocity
        x2 = chiSquared(zip(self.velocities, data.amps), lambda v: v / 127.0)
        self.assertLess(x2, 1)

    def testSine(self):
        self.doTest('instr(1) = sine(amp, freq)')

    def testAverage4(self):
        self.doTest('''
            instr(1) = {
                parallel {
                    sine(amp, freq)
                    sine(amp, freq * 2)
                    sine(amp, freq * 3)
                    sine(amp, freq * 4)
                }
                average
            }
        ''')
