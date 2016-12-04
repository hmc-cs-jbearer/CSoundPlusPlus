import subprocess

from csoundpp.errors import CompileError
from csoundpp.files import tempPath

def compile(file, program, *options):
    with tempPath() as programFile:
        with open(programFile, 'w') as f:
            f.write(program)

        argv = 'sbt "run compile {} {} -o {}"'.format(programFile, ' '.join(options), file)
        proc = subprocess.Popen(argv, stderr=subprocess.PIPE, shell=True)
        _, err = proc.communicate()
        status = proc.returncode

    if status != 0:
        raise CompileError(err.decode('utf-8'))
