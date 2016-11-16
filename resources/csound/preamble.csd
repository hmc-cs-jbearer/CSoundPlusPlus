; This file is prepended to every compiled CSound++ program. It contains global setup code as well
; as CSound opcodes defining the built-in CSound++ features.

sr = 44100  ; Sampling rate
ksmps = 32  ; Control rate (sr / 32)
nchnls = 1  ; Mono output (hopefully stereo sound will be supported in the future)
0dbfs  = 1  ; The amplitude of the loudest possible sound

; Force all MIDI channel mappings to be made exlicitly (via future calls to massign)
massign 0, 0

; Built in opcodes. All opcodes with a cspp_ prefix are visible to users via CSound++. For example,
; a reference to `fm` in CSound++ will compile to a call to the `cspp_fm` opcode.
; Opcodes prefixed with __cspp_ are not visible to the user, but are used in the implementation of
; the user-facing opcodes.

; fm:
;   A basic frequency modulated source.
; Inputs:
;   ifreq: the fundamental frequency in Hz of the signal to produce.
;   iamp: the amplitude of the signal, on a scale from 0 to 1.
;   index: the index of modulation.
; Outputs:
;   asig: the resulting signal.
opcode cspp_fm, a, iii
ifreq, iamp, index xin
asig foscil iamp, 1, ifreq, ifreq, index
xout asig
endop

; fm:
;   A basic compressor.
; Inputs:
;   asig: the signal to compress
;   ithresh: the threshold at which to begin compression (0 to 1).
;   iratio: the denominator of the compression ratio. For example, if iratio = 3, then signals
;       with amplitude greater than ithresh will be compressed by a factor of 3.
; Outputs:
;   asig: the resulting signal.
opcode cspp_compress, a, aii
asig, ithresh, iratio xin
asig compress asig, asig, 0, ithresh, ithresh, iratio, 0.01, 0.1, 0.1
xout asig
endop

; The extra two lines here are necessary to separate the preamble from the begining of the user's code

