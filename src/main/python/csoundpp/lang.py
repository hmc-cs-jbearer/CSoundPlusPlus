import subprocess

from csoundpp.files import tempPath

def compile(file, program, *options):
    with tempPath() as programFile:
        with open(programFile, 'w') as f:
            f.write(program)
        status = subprocess.call(['sbt', 'run compile {} {} -o {}'.format(
            programFile, ' '.join(options), file)])

    assert status == 0, 'Failed to compile {}.'.format(program)
