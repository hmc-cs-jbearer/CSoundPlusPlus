/**
 * For each opcode in the CSound++ library, we export two symbols to CSound.
 * One is an unmangled opcode which allows this library to be used in a convenient way by humans
 * CSound programmers.
 * The other is a mangled opcode which will follows CSound++ standard library naming conventions and
 * which may be visible to the CSound++ programmer.
 * In the case of public opcodes, this mangling is necessary, because opcodes are only visible to
 * the CSound++ programmer if they are prefixed with cspp__.
 * In the case of private opcodes (which carry an __cspp_ prefix) the name-mangling is merely a
 * matter of convention.
 */
#define CSPP_PUBLIC(opcode, dspace, flags, thread, outarg, inargs, isub, ksub, asub) \
    { (opcode), (dspace), (flags), (thread), (outarg), (inargs), (isub), (ksub), (asub) }, \
    { ("_" opcode), (dspace), (flags), (thread), (outarg), (inargs), (isub), (ksub), (asub) }

#define CSPP_PRIVATE(opcode, dspace, flags, thread, outarg, inargs, isub, ksub, asub) \
    { (opcode), (dspace), (flags), (thread), (outarg), (inargs), (isub), (ksub), (asub) }

#define S(x) sizeof(x)
