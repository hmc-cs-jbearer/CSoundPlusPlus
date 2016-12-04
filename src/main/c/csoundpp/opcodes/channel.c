#include "channel.h"

static MYFLT * channel_buffers[16] = { NULL };

int channel_init(CSOUND * csound, CHANNEL * channel)
{
    uint32_t id;

    if (UNLIKELY(*channel->id >= 16 || *channel->id < 0)) {
        // Error
        return NOTOK;
    }

    id = *(uint32_t *)channel->id;
    channel->buffer = &channel_buffers[id];

    if (UNLIKELY(!*channel->buffer)) {
        *channel->buffer = (MYFLT *)malloc(sizeof(MYFLT) * csound->GetKsmps(csound));
    }

    return OK;
}

int channel(CSOUND * csound, CHANNEL * channel)
{
    MYFLT *aout = channel->aout, *buffer = *channel->buffer;

    memcpy(aout, buffer, sizeof(MYFLT) * csound->GetKsmps(csound));
    return OK;
}

int channel_out_init(CSOUND * csound, CHANNEL_OUT * channel)
{
    uint32_t id;

    if (UNLIKELY(*channel->id >= 16 || *channel->id < 0)) {
        // Error
        return NOTOK;
    }

    id = *(uint32_t *)channel->id;
    channel->buffer = &channel_buffers[id];

    if (UNLIKELY(!*channel->buffer)) {
        *channel->buffer = (MYFLT *)malloc(sizeof(MYFLT) * csound->GetKsmps(csound));
    }

    return OK;
}

int channel_out(CSOUND * csound, CHANNEL_OUT * channel)
{
    MYFLT *asig = channel->asig, *buffer = *channel->buffer;
    memcpy(buffer, asig, sizeof(MYFLT) * csound->GetKsmps(csound));
    return OK;
}
