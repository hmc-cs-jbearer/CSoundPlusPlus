import os
from unittest import TestCase

from csoundpp.analysis import analyze, chiSquared
from csoundpp.files import uniqueFile, tempPath
from csoundpp.player import play
import csoundpp.lang as lang
from csoundpp import midi

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
            lang.compile(orcFile, source, '-d')
            data = analyze(self.play(orcFile))

        self.assertEqual(len(data.amps), len(self.velocities))

        # Amplitude should be a linear function which is 0 at 0 velocity and 1 at max velocity
        x2 = chiSquared(zip(self.velocities, data.amps), lambda v: v / 127.0)
        self.assertLess(x2, 1)

    def testSine(self):
        self.doTest('instr(1) = sine(amp, freq)')
