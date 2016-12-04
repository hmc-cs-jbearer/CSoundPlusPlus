#include "average.h"

int average(CSOUND *csound, AVERAGE *avg)
{
    uint32_t sample, signal, nsigs = avg->INCOUNT;
    MYFLT sum, *aout = avg->aout, **asigs = avg->asigs;

    for (sample = 0; sample < csound->GetKsmps(csound); ++sample) {
        sum = 0;
        for (signal = 0; signal < nsigs; ++signal) {
            sum += asigs[signal][sample];
        }
        aout[sample] = sum;
    }

    return OK;
}
