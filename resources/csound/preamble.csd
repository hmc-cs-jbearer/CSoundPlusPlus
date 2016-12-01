#! bin/csound-play
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

; sine:
;   A sine wave source.
; Inputs:
;   iamp: the amplitude of the signal, on a scale from 0 to 1
;   ifreq: the frequency of the signal in Hz.
; Outputs:
;   asig: a sine wave.
opcode cspp_sine, a, ii
iamp, ifreq xin
asig oscil iamp, ifreq
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
opcode cspp_foscil, a, iiii
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
;   is: the shelf slope for modes 5 and 6. Must be > 0. is == 1 results in the steepest possible
;       slope without resonance. is > 1 results in resonances.
opcode __cspp_filt, a, aiiiii
asig, imode, ifreq, ilvl, iq, is xin
denorm asig
asig rbjeq asig, ifreq, ilvl, iq, is, imode * 2
xout asig
endop

; Filter modes
#define CSPP_FILT_LOPASS        #0#
#define CSPP_FILT_HIPASS        #1#
#define CSPP_FILT_BANDPASS      #2#
#define CSPP_FILT_BANDREJECT    #3#
#define CSPP_FILT_PEAKING       #4#
#define CSPP_FILT_LOSHELF       #5#
#define CSPP_FILT_HISHELF       #6#

; Implementation for a low pass, high pass, band pass, or band reject filter.
; Macro parameters:
;   name: the name of the opcode to be generated.
;   mode: the imode parameter of __cspp_filt.
; Inputs:
;   ifreq
;   ilvl
;   iq
; The inputs have the same meaning as the corresponding inputs to __cspp_filt
#define CSPP_FILT_IMPL(name' mode') #
opcode cspp_$name, a, aiii
asig, ifreq, ilvl, iq xin
asig __cspp_filt asig, $mode, ifreq, ilvl, iq, 1
xout asig
endop
#


$CSPP_FILT_IMPL(lopass' $CSPP_FILT_LOPASS')
$CSPP_FILT_IMPL(hipass' $CSPP_FILT_HIPASS')
$CSPP_FILT_IMPL(bandpass' $CSPP_FILT_BANDPASS')
$CSPP_FILT_IMPL(bandreject' $CSPP_FILT_BANDREJECT')

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
opcode cspp_sidechain_compress, a, aaiiiii
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
opcode cspp_compress, a, aiiiii
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
opcode cspp_adsr, a, aiiii
asig, iatt, idec, isus, irel xin

kenv madsr iatt, idec, isus, irel
xout kenv*asig
endop

; average
;   The pointwise average of two input signals.
opcode cspp_average, a, aa
a1, a2 xin
avg = (a1 + a2) / 2
xout avg
endop
