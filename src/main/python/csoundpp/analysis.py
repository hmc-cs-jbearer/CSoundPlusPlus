#! /usr/bin/env python3

import sys

class ProgramData:
    def __init__(self, amps=[], freqs=[]):
        self.freqs = freqs
        self.amps = amps

def analyzeLogEvent(info, result):
    if info['name'] == 'note_on':
        result.freqs.append(float(info['freq']))
        result.amps.append(float(info['amp']))

def analyze(output):
    '''
    Parse the output of a run of a CSound++ program to extract useful information.
    Returns a ProgramData instance
    '''
    data = ProgramData()

    for line in output:
        line = line.strip()
        if line.startswith('CSPPLOG '):
            fields = [field.strip() for field in line.split(' ')[1:]] # Chop off the "CSPPLOG" token
            info = { 'level': fields[0] }                             # level token is positional
            for field in fields[1:]:                                  # chop off level token
                k, v = [item.strip() for item in field.split('=')]    # other tokens are k=v pairs
                info[k] = v

            analyzeLogEvent(info, data)

    return data

def chiSquared(samples, expected):
    '''
    Calculate the chi squared statistic for the given data.
    This function has two modes:
    If samples is a list of pairs and expected is a function, then samples is treated as a list of
    pairs of independent, dependent variable values. To obtain the predicted value for each sample,
    expected is applied to the independent variable.
    If samples and expected are flat lists, then samples is treated as a list of obervations and
    expected is treated as a list of predictions.
    '''
    if callable(expected):
        independent, observations = zip(*samples)
        predictions = [expected(x) for x in independent]
    else:
        observations = samples
        predictions = expected

    if len(observations) != len(predictions):
        raise ValueError(
            'Samples and predictions of unequal lengths: {} samples and {} predictions'.format(
                len(observations), len(predictions)))

    x2 = sum([(o - e)**2 / e for o, e in zip(observations, predictions) if e != 0])

    if x2 < sys.float_info.epsilon:
        raise ValueError('Chi squared not distinguishable from zero.')
    return x2
