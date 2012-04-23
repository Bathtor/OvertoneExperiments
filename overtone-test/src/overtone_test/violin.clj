(ns overtone-test.violin
  (:use [overtone.core]))
(connect-external-server 57110)
(use 'overtone.inst.synth)

(demo 5
      (comb-l
       (rlpf (* 0.05 (lf-pulse (mul-add (f-sin-osc:kr 0.05 0) 80 160) 0 0.4))
             (mul-add (f-sin-osc:kr [0.6 0.7] 0) 3600 4000)
             0.2)
       0.3 [0.2 0.25] 2))

(stop)
