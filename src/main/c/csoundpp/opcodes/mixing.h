#include "csdl.h"

typedef struct {
    OPDS    h;
    MYFLT * aout;
    MYFLT * asigs[VARGMAX];
} MIX;

extern int average(CSOUND *csound, MIX *avg);
extern int sum(CSOUND *csound, MIX *sum);
