0dbfs = 1
sr = 44100
kr = 4410
ksmps =10
nchnls=1

opcode fx, a, a
asig    xin

; Inserts:
;   Compressor
ains    compress    asig, asig, 0, 40, 60, 3, 0.1, 0.01, 0.05

;   EQ
ains    pareq       ains, 200, 1.1, sqrt(0.5), 1  ; Low shelf
ains    pareq       ains, 500, 1.2, sqrt(0.5), 0  ; Peaking boost
ains    rbjeq       ains, 1000, 0, sqrt(0.5), 0      ; Low pass

; We lost some energy through filtering, need to scale up
ains    =           ains * 1.5

; ; Sends:
; ;   Delay (with feedback)
adel    init        0
adel    delay       ains + (adel*0.5), 0.2

;   Reverb
asnd    reverb      ains, 1.5

; Mix
amix = 0.9*ains + 0.1*asnd

xout amix
endop

; Wave tables for oscillators
gipulse ftgen 0, 0, 2048, 10, 1, 1, 1, 1, .7, .5, .3, .1
gisine  ftgen 0, 0, 1024, 10, 1

; A bass synth
instr 1
  ; Input parameters
  ifrq   cpsmidi
  iamp   ampmidi 1

  ; Control for filter sweep
  k2    expsegr 3000, 0.08, 9000, 0.1, 1
  ksweep =k2-3000

  ; Oscillators using wave tables defined above
  a1    oscil    iamp*0.40, ifrq*0.998-.12, gipulse
  a2    oscil    iamp*0.40, ifrq*1.002-.12, gisine
  a3    oscil    iamp*0.40, ifrq*1.002-.12, gipulse
  a4    oscil    iamp*0.70, ifrq-.24      , gisine
  aall= a1+a2+a3+a4 / 4

  ; Filters
  a6    butterlp  aall,ksweep
  a8    butterlp  a6, ksweep
  a9    butterhp  a8, 65
  a10   butterhp  a9, 65
  a11   butterlp  a10,1000

  ; Amplitude envelope
  asig  linenr    a11, 0.02, 0.01, 0.01

  ; Effects chain
  asig  fx        asig

  ; Output
  out asig
endin
