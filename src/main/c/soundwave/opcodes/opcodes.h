/**
 * For each user-visible opcode in the standard library, we export two symbols to CSound. One is an
 * unmangled opcode which allows this library to be used in a convenient way by human CSound
 * programmers. The other is a mangled opcode which follows standard library naming conventions and
 * which may be visible to the SoundWave programmer.
 */
#define SW_PUBLIC(opcode, dspace, flags, thread, outarg, inargs, isub, ksub, asub) \
    { (opcode), (dspace), (flags), (thread), (outarg), (inargs), (isub), (ksub), (asub) }, \
    { ("_" opcode), (dspace), (flags), (thread), (outarg), (inargs), (isub), (ksub), (asub) }

/**
 * Private symbols do not have a prefix, so we just export one symbol.
 */
#define SW_PRIVATE(opcode, dspace, flags, thread, outarg, inargs, isub, ksub, asub) \
    { (opcode), (dspace), (flags), (thread), (outarg), (inargs), (isub), (ksub), (asub) }

#define S(x) sizeof(x)
