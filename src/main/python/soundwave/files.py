import os

def uniqueFile():
    file = ".tmp" + str(os.getpid()) + str(uniqueFile._pathsCreated)
    uniqueFile._pathsCreated += 1

    # Touch the file
    with open(file, 'a'):
        pass

    return file

uniqueFile._pathsCreated = 0

class tempPaths:

    _pathsCreated = 0

    def __init__(self, num):
        self._paths = [uniqueFile() for _ in range(num)]

    def __enter__(self):
        # Don't return a list of one element
        return self._paths if len(self._paths) > 1 else self._paths[0]

    def __exit__(self, *_):
        for path in self._paths:
            os.remove(path)

def tempPath():
    return tempPaths(1)
