#include "average.h"
#include "channel.h"
#include "opcodes.h"

static OENTRY localops[] = {
    /* opcode dspace flags thread outarg inargs isub ksub asub */
    CSPP_PUBLIC("average", S(AVERAGE), 0, 4, "a", "y", NULL, NULL, (SUBR)average),
    CSPP_PUBLIC("channel", S(CHANNEL), 0, 5, "a", "i", (SUBR)channel_init, NULL, (SUBR)channel),
    CSPP_PRIVATE("chout", S(CHANNEL_OUT), 0, 5, "", "ai", (SUBR)channel_out_init, NULL, (SUBR)channel_out)
};

LINKAGE
