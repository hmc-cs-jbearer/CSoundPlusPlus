#! /usr/bin/env csound-play
; This file is prepended to every compiled CSound++ program. It contains global setup code as well
; as CSound opcodes defining the built-in CSound++ features.

sr = 44100  ; Sampling rate
ksmps = 32  ; Control rate (sr / 32)
nchnls = 1  ; Mono output (hopefully stereo sound will be supported in the future)
0dbfs  = 1  ; The amplitude of the loudest possible sound

; Force all MIDI channel mappings to be made exlicitly (via future calls to massign)
massign 0, 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wave tables used by built-in opcodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gipulse ftgen 0, 0, 2048, 10, 1, 1, 1, 1, .7, .5, .3, .1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Built in opcodes. All opcodes with an _ prefix are visible to users via CSound++. For example, a
; reference to `fm` in CSound++ will compile to a call to the `x_fm` opcode. Opcodes without this
; prefix are not visible to the user, but are used in the implementation of the user-facing opcodes.
;
; The following are low-level opcodes, which are visible to the user, but are primarily inteneded to
; be used in the implementation of library routines. They give access to CSound-level components.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sine:
;   A sine wave source.
; Inputs:
;   iamp: the amplitude of the signal, on a scale from 0 to 1
;   ifreq: the frequency of the signal in Hz.
; Outputs:
;   asig: a sine wave.
opcode _sine, a, ii
iamp, ifreq xin
asig oscil iamp, ifreq
xout asig
endop

; pulse:
;   A pulse train source.
; Inputs:
;   iamp: the amplitude of the signal, 0 to 1
;   ifreq: the frequency of the signal in Hz.
; Outputs:
;   asig: a pulse train.
opcode _pulse, a, ii
iamp, ifreq xin
asig oscil iamp, ifreq, gipulse
xout asig
endop

; foscil:
;   A basic frequency modulated source.
; Inputs:
;   iamp: the amplitude of the signal, on a scale from 0 to 1.
;   ifcar: the frequency in Hz of the carrier oscillator.
;   ifmod: the frequency in Hz of the modulating oscillator.
;   index: the index of modulation.
; Outputs:
;   asig: the resulting signal.
opcode _foscil, a, iiii
iamp, icar, imod, index xin
asig foscil iamp, 1, icar, imod, index
xout asig
endop

; filt:
;   The common implementation for all CSound++ filter types.
; Inputs:
;   imode: an integer representing the type of filtering to perform.
;       0: Resonant low-pass filter
;       1: Resonant high-pass filter
;       2: Band-pass filter
;       3: Band-reject filter
;       4: Peaking filter
;       5: Low shelf
;       6: High shelf
;   ifreq: the cutoff frequency (modes 0, 1, 5, 6) or center frequency (modes 2 - 4) in Hz.
;   ilvl: the amount of boost or cut. Must be > 0. ilvl == 1 results in a flat frequency response.
;   iq: the filter quality (ifreq / bandwidth)
opcode _filt, a, aiiii
asig, imode, ifreq, ilvl, iq xin
denorm asig

; Csound has two main EQ filter types: rbjeq and pareq. pareq is easier to use but only supports
; peaking and shelving filters. Thus we use it for those and use rbjeq for low pass, high pass
; band pass, and band reject filters.
if imode < 4 then
    aflt rbjeq asig, ifreq, ilvl, iq, 0, imode * 2
else
    aflt pareq asig, ifreq, ilvl, iq, imode - 4
endif

xout aflt
endop

; transpose
;   Scale the frequency of the input signal.
; Inputs:
;   iscl: a multiplicative factor by which to scale the frequencies of the given signal. So, for
;       example, iscl = 2 would result in transposition up one octave, iscl = 0.5 would result in
;       transposition down one octave, etc.
opcode _transpose, a, ai
asig, iscl xin

fsig  pvsanal   asig, 1024, 256, 1024, 1 ; transform the signal to the frequency domain
ftps  pvscale   fsig, iscl, 1            ; transpose it keeping formants
atps  pvsynth   ftps                     ; transform back to time domain

xout atps
endop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following are higher-level opcodes, which are implemented in CSound but use some of CSound's
; higher-level features, and are intended for use directly by the user.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sidechain_compress:
;   An effect which compresses the dynamic range of one signal based on the amplitude of a different
;   signal.
; Inputs:
;   asig: the signal to compress
;   acmp: the signal to trigger compression
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
opcode _sidechain_compress, a, aaiiiii
asig, acmp, iloknee, ihiknee, iratio, iatt, irel xin

; The lowest level which will be allowed through the compressor. If set above 0, the compressor
; starts to act as a noise reducer as well. We just want this opcode to be a compressor, so we set
; the threshold to 0.
ithresh = 0

; The lookahead time of the compressor. This is an implementation detail and so is hidden from the
; user. There is a tradeoff between small values, which result in small delays between input and
; output, and large values, which improve the performance of the peak detection. 0.05 is the value
; recommended by the CSound documentation (http://www.csounds.com/manual/html/compress.html).
ilook = 0.05

asig compress asig, acmp, ithresh, iloknee, ihiknee, iratio, iatt, irel, ilook
xout asig
endop

; compress:
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
opcode _compress, a, aiiiii
asig, iloknee, ihiknee, iratio, iatt, irel xin
asig cspp_sidechain_compress asig, asig, iloknee, ihiknee, iratio, iatt, irel
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
opcode _adsr, a, aiiii
asig, iatt, idec, isus, irel xin

kenv madsr iatt, idec, isus, irel
xout kenv*asig
endop

; delay
;   Simple delay line with feedback.
; Inputs:
;   asig: the signal to delay
;   idlt: the delay time in seconds
;   ifb:  the feedback level. Must be in [0, 1)
opcode _delay, a, aii
asig, idlt, ifb xin
adel init 0
adel delay asig + (adel*ifb), idlt
xout adel
endop

; reverb
;   Reverberates an input signal with a “natural room” frequency response.
; Inputs:
;   irvt:
opcode _reverb, a, ai
asig, irvt xin
asig reverb asig, irvt
xout asig
endop

; scale
;   Scale the input signal by the given factor.
; Inputs:
;   ilvl: the multiplicative factor by which to scale.
opcode _scale, a, ai
asig, ilvl xin
xout asig * ilvl
endop

; lopass_sweep
;   Temporarily in place for a demo, will be removed when control rate functions are added.
opcode _lopass_sweep, a, aiiiiii
ain, ia, iadur, ib, irel, iz, iq xin
k1 expsegr ia, iadur, ib, irel, iz
ksweep = k1 - ia
aout butterlp ain, ksweep
xout aout
endop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sqrt: compute the square root of a nonnegative number
opcode _sqrt, i, i
input xin
xout sqrt(input)
endop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; User generated code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
