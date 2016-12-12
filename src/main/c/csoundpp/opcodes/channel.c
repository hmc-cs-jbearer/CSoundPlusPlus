#include "channel.h"

// These will be allocated an initialized lazily, since we don't know how much to allocate until we
// have a CSOUND struct, which contains ksmps
static MYFLT *channel_buffers[16] = { NULL };
static uint32_t channel_buffer_sizes[16] = { 0 };

int channel_init(CSOUND *csound, CHANNEL *channel)
{
    if (UNLIKELY(*channel->id >= 16 || *channel->id < 0)) {
        csound->InitError(csound, "channel must be between 0 and 16");
        return NOTOK;
    }

    uint32_t id = (uint32_t)(*channel->id);
    channel->buffer = &channel_buffers[id];
    channel->buffer_sz = &channel_buffer_sizes[id];

    if (UNLIKELY(!*channel->buffer)) {
        uint32_t ksmps = csound->GetKsmps(csound);
        *channel->buffer = (MYFLT *)malloc(sizeof(MYFLT) * ksmps);
        *channel->buffer_sz = ksmps;
    }

    return OK;
}

int channel(CSOUND *csound, CHANNEL *channel)
{
    if (UNLIKELY(!*channel->buffer)) {
        csound->PerfError(csound, channel->h.insdshead,
            "channel buffer %d has not been allocated", (uint32_t)*channel->id);
        return NOTOK;
    }
    if (UNLIKELY(*channel->buffer_sz != csound->GetKsmps(csound))) {
        csound->PerfError(csound, channel->h.insdshead,
            "channel buffer %d was allocated %d samples, but ksmps is %d",
            (uint32_t)*channel->id, *channel->buffer_sz, csound->GetKsmps(csound));
        return NOTOK;
    }

    uint32_t sz = *channel->buffer_sz;
    MYFLT *aout = channel->aout, *buffer = *channel->buffer;
    memcpy(aout, buffer, sizeof(MYFLT) * sz);
    return OK;
}

int channel_out_init(CSOUND * csound, CHANNEL_OUT * channel)
{
    if (UNLIKELY(*channel->id >= 16 || *channel->id < 0)) {
        csound->InitError(csound, "channel must be between 0 and 16");
        return NOTOK;
    }

    uint32_t id = (uint32_t)(*channel->id);
    channel->buffer = &channel_buffers[id];
    channel->buffer_sz = &channel_buffer_sizes[id];

    if (UNLIKELY(!*channel->buffer)) {
        uint32_t ksmps = csound->GetKsmps(csound);
        *channel->buffer = (MYFLT *)malloc(sizeof(MYFLT) * ksmps);
        *channel->buffer_sz = ksmps;
    }

    return OK;
}

int channel_out(CSOUND * csound, CHANNEL_OUT * channel)
{
    if (UNLIKELY(!*channel->buffer)) {
        csound->PerfError(csound, channel->h.insdshead,
            "channel buffer %d has not been allocated", (uint32_t)*channel->id);
        return NOTOK;
    }
    if (UNLIKELY(*channel->buffer_sz != csound->GetKsmps(csound))) {
        csound->PerfError(csound, channel->h.insdshead,
            "channel buffer %d was allocated %d samples, but ksmps is %d",
            (uint32_t)*channel->id, *channel->buffer_sz, csound->GetKsmps(csound));
        return NOTOK;
    }

    uint32_t sz = *channel->buffer_sz;
    MYFLT *asig = channel->asig, *buffer = *channel->buffer;
    memcpy(buffer, asig, sizeof(MYFLT) * sz);
    return OK;
}
