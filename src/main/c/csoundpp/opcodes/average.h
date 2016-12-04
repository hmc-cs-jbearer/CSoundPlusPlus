#include "csdl.h"

typedef struct {
    OPDS    h;
    MYFLT * aout;
    MYFLT * asigs[VARGMAX];
} AVERAGE;

extern int average(CSOUND *csound, AVERAGE *avg);
