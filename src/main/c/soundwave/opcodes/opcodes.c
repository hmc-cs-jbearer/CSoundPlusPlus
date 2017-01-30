#include "channel.h"
#include "mixing.h"
#include "opcodes.h"

static OENTRY localops[] = {
    /* opcode dspace flags thread outarg inargs isub ksub asub */
    SW_PUBLIC("average", S(MIX), 0, 4, "a", "y", NULL, NULL, (SUBR)average),
    SW_PUBLIC("sum", S(MIX), 0, 4, "a", "y", NULL, NULL, (SUBR)sum),
    SW_PRIVATE("channel", S(CHANNEL), 0, 5, "a", "i", (SUBR)channel_init, NULL, (SUBR)channel),
    SW_PRIVATE("chout", S(CHANNEL_OUT), 0, 5, "", "ai", (SUBR)channel_out_init, NULL, (SUBR)channel_out)
};

LINKAGE
