import signal
from subprocess import Popen, PIPE
import sys

from csoundpp.errors import SubprocessError
from csoundpp.files import tempPath

def play(programFile, scoreFile, outFile):
    """
    Play a compiled CSound++ program.
    programFile: the path to the compiled program.
    scoreFile: the path to a MIDI file containg the score to perform.
    outFile: the file to which to write the output. If outFile == 'dac', then output is written to
        computer's digital-analog converter. Otherwise, output is written in WAV format to the file
        specified.

    output: the output produced during program execution.

    Raises Subprocess error if CSound terminates with a nonzero exit code.
    """

    # Read in the program and strip the shebang from the program
    with open(programFile) as f:
        line = f.readline()
        if line[0:2] == "#!":
            line = ""
        program = line + f.read()

    # Write the processed program to a temporary file which is unique to this process
    with tempPath() as orcFile:
        with open(orcFile, 'w') as f:
            f.write(program)

        # Set up platform specific process signals
        if sys.platform.startswith("win"):
            from subprocess import CREATE_NEW_PROCESS_GROUP
            kwargs = {"creationflags": CREATE_NEW_PROCESS_GROUP}
            killSig = signal.CTRL_C_EVENT
        else:
            kwargs = {}
            killSig = signal.SIGINT

        # Set up platform specific shared library extensions
        if sys.platform.startswith("win"):
            libExt = "dll"
        elif sys.platform.startswith("darwin"):
            libExt = "dylib"
        else:
            libExt = "so"

        argv = ["csound",
                "--opcode-lib=lib/libcsppstd." + libExt,
                "--orc",
                "-F", scoreFile,
                "-o", outFile,
                orcFile]
        csound = Popen(argv, stderr=PIPE, *kwargs)

        ranToCompletion = False

        for line in csound.stderr:
            line = line.decode("utf-8")[:-1] # Strip off the trailing newline
            END_OF_SCORE = "end of midi track"
            if line[0:len(END_OF_SCORE)] == END_OF_SCORE:
                # CSound just hangs once the MIDI track is finished, so we have to manually kill it
                ranToCompletion = True
                csound.send_signal(killSig)
            else:
                yield line

        csound.communicate()

    if not ranToCompletion and csound.returncode != 0:
        raise SubprocessError(argv, csound.returncode)

    return
