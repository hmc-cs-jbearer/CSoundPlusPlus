import subprocess

from soundwave.errors import CompileError
from soundwave.files import tempPath

def compile(file, program, *options):
    with tempPath() as programFile:
        with open(programFile, 'w') as f:
            f.write(program)

        argv = ['java', '-jar', 'bin/soundwave.jar', 'compile', programFile, '-o', file] + list(options)
        proc = subprocess.Popen(argv, stderr=subprocess.PIPE)
        _, err = proc.communicate()
        status = proc.returncode

    if status != 0:
        raise CompileError(err.decode('utf-8'))
