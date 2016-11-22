class CsppError(Exception):
    def __init__(self, msg):
        Exception.__init__(self, msg)
        self._msg = msg

    def getMessage(self):
        return self._msg

    def __repr__(self):
        return self.getMessage()

class SubprocessError(CsppError):
    def __init__(self, argv, code, msg=''):
        CsppError.__init__(self, '{}: subprocess exited with error code {}{}'.format(
            ' '.join(argv), code, ': ' + msg if msg else ''))
        self._code = code

    def code(self):
        return self._code
