(ns overtone-test.guitar
  (:use overtone.core))
(connect-external-server 57110)
(definst guitar
  []
  (sound-in 0))

(guitar)

(inst-fx guitar fx-distortion2)
(inst-fx guitar fx-reverb2)
(inst-fx guitar fx-echo)
(def screamer (inst-fx guitar fx-distortion-tubescreamer))
(def lowpass (inst-fx guitar fx-rlpf))

(ctl screamer :gain 5)
(ctl lowpass :cutoff 1000)

(volume 50)

(clear-fx guitar)

(stop)
