import subprocess

from csoundpp.files import tempPath

def compile(file, program, *options):
    with tempPath() as programFile:
        with open(programFile, 'w') as f:
            f.write(program)

        argv = ['java', '-jar', 'bin/cspp.jar', 'compile', programFile, '-o', file] + list(options)
        status = subprocess.call(argv)

    assert status == 0, 'Failed to compile {}.'.format(program)
