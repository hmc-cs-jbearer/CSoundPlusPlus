; This file is prepended to every compiled CSound++ program. It contains global setup code as well
; as CSound opcodes defining the built-in CSound++ features.

sr = 44100  ; Sampling rate
ksmps = 32  ; Control rate (sr / 32)
nchnls = 1  ; Mono output (hopefully stereo sound will be supported in the future)
0dbfs  = 1  ; The amplitude of the loudest possible sound

; Force all MIDI channel mappings to be made exlicitly (via future calls to massign)
massign 0, 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Built in opcodes. All opcodes with a cspp_ prefix are visible to users via CSound++. For example,
; a reference to `fm` in CSound++ will compile to a call to the `cspp_fm` opcode.
; Opcodes prefixed with __cspp_ are not visible to the user, but are used in the implementation of
; the user-facing opcodes.
;
; The following are low-level opcodes, which are visible to the user, but are primarily inteneded to
; be used in the implementation of library routines. They give access to CSound-level components.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; foscil:
;   A basic frequency modulated source.
; Inputs:
;   iamp: the amplitude of the signal, on a scale from 0 to 1.
;   ifcar: the frequency in Hz of the carrier oscillator.
;   ifmod: the frequency in Hz of the modulating oscillator.
;   index: the index of modulation.
; Outputs:
;   asig: the resulting signal.
opcode cspp_foscil, a, iiii
iamp, icar, imod, index xin
asig foscil iamp, 1, icar, imod, index
xout asig
endop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following are higher-level opcodes, which are implemented in CSound but use some of CSound's
; higher-level features, and are intended for use directly by the user.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; fm:
;   A basic compressor.
; Inputs:
;   asig: the signal to compress
;   iloknee: the threshold (0 to 1) at which to begin compression. Or, the highest input amplitude
;       for which the slope of the compression function is 1.
;   ihiknee: the threshold (0 to 1) at which the slope of the compression function is first equal
;       to 1 / iratio.
;   iratio: the compression ratio. This is the denominator of the slope of the compression funciton
;       above ihiknee.
;   iatt: the attack time in seconds.
;   irel: the release time in seconds.
; Outputs:
;   asig: the resulting signal.
opcode cspp_compress, a, aiiiii
asig, iloknee, ihiknee, iratio, iatt, irel xin

; The lowest level which will be allowed through the compressor. If set above 0, the compressor
; starts to act as a noise reducer as well. We just want this opcode to be a compressor, so we set
; the threshold to 0.
ithresh = 0

; The lookahead time of the compressor. This is an implementation detail and so is hidden from the
; user. There is a tradeoff between small values, which result in small delays between input and
; output, and large values, which improve the performance of the peak detection. 0.05 is the value
; recommended by the CSound documentation (http://www.csounds.com/manual/html/compress.html).
ilook = 0.05

asig compress asig, asig, ithresh, iloknee, ihiknee, iratio, iatt, irel, ilook
xout asig
endop

; adsr:
;   An ADSR envelope generator. The input signal will have its amplitude attenuated by a time
;   varying amount, as determined by the four ADSR parameters.
; Inputs:
;   asig: the signal to modify.
;   iatt: the attack time of the envelope in seconds.
;   idec: the decay time in seconds.
;   isus: the sustain level, on a scale from 0 to 1.
;   irel: the release time in second.
opcode cspp_adsr, a, aiiii
asig, iatt, idec, isus, irel xin

kenv madsr iatt, idec, isus, irel
xout kenv*asig
endop
