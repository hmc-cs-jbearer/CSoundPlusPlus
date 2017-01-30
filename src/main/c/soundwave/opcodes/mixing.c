#include "mixing.h"

#define concat1(x, y) x ## y
#define concat(x, y) concat1(x, y)
#define unused(t) t concat(unused, __COUNTER__) __attribute__((unused))

typedef MYFLT(*mixer)(uint32_t, uint32_t, MYFLT, va_list);

static int mix(uint32_t nsigs, uint32_t ksmps, MYFLT **asigs, MYFLT *aout, mixer f, ...)
{
    for (uint32_t sample = 0; sample < ksmps; ++sample) {
        MYFLT sum = 0;
        for (uint32_t signal = 0; signal < nsigs; ++signal) {
            va_list args;
            va_start(args, f);
            sum += f(signal, sample, asigs[signal][sample], args);
            va_end(args);
        }
        aout[sample] = sum;
    }

    return OK;
}

static MYFLT mix_average(unused(uint32_t), unused(uint32_t), MYFLT val, va_list args)
{
    uint32_t n = va_arg(args, uint32_t);
    return val / n;
}

static MYFLT mix_ident(unused(uint32_t), unused(uint32_t), MYFLT val, unused(va_list))
{
    return val;
}

int average(CSOUND *csound, MIX *avg)
{
    uint32_t nsigs = avg->INCOUNT, ksmps = csound->GetKsmps(csound);
    MYFLT *aout = avg->aout, **asigs = avg->asigs;

    return mix(nsigs, ksmps, asigs, aout, mix_average, nsigs);
}

int sum(CSOUND *csound, MIX *sum)
{
    uint32_t nsigs = sum->INCOUNT, ksmps = csound->GetKsmps(csound);
    MYFLT *aout = sum->aout, **asigs = sum->asigs;

    return mix(nsigs, ksmps, asigs, aout, mix_ident);
}
