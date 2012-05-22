(ns overtone-test.rm
  (:use [overtone.core]))
(connect-external-server 57110)
(use 'overtone.inst.synth)

(definst rm-test
  [carrier 440 modulation 55]
  (let [csig (sin-osc carrier)
        msig (sin-osc modulation)
        mamp 0.5]
    (* csig (* mamp msig))))

(definst simple-fm
  [carrier 440 modulation 6]
  (let [msig (sin-osc modulation)
        mamp 0.5
        csig (sin-osc (+ carrier msig))]
    csig))

(definst sin-osc-inst
  [freq 440]
  (sin-osc freq))

(rm-test 220 660)

(simple-fm)

(sin-osc-inst 440)

(stop)
