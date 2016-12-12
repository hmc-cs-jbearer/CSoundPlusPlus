#include "csdl.h"

typedef struct {
    // Information about the opcode instance
    OPDS        h;

    // Output signal
    MYFLT       *aout;

    // Inputs: the channel from which to source audio
    MYFLT       *id;

    // Internal state
    MYFLT       **buffer;
    uint32_t    *buffer_sz;
} CHANNEL;

typedef struct {
    OPDS        h;
    MYFLT       *asig;
    MYFLT       *id;
    MYFLT       **buffer;
    uint32_t    *buffer_sz;
} CHANNEL_OUT;

extern int channel_init(CSOUND *csound, CHANNEL *channel);
extern int channel(CSOUND *csound, CHANNEL *channel);
extern int channel_out_init(CSOUND *csound, CHANNEL_OUT *channel);
extern int channel_out(CSOUND *csound, CHANNEL_OUT *channel);
