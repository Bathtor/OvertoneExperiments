(ns overtone-test.violin
  (:use [overtone.core]))
(connect-external-server 57110)
(use 'overtone.inst.synth)
(use 'overtone.sc.ugens)
(use 'overtone.sc.envelope)
(use 'overtone.sc.defcgen)
(volume 50)

(definst dampedwave
  [in 440 dampening 0.5]
  (let [sig (sin-osc in)
        delay (reciprocal in)
        delayed-sig (delay-n sig delay delay)]
    (resonz (+ sig delayed-sig) 440 dampening)))

(defcgen guitar-string
  "Single guitar string"
  [note {:default 60 :doc "note played on that string"}
   velocity {:default 80 :doc "midi type velocity"}]
  "Models a single guitar string using physical modeling."
  (let [freq (midicps note)
        vol (/ velocity 127)
        stroke (env-gen (adsr 0.02 3.005 1.0 vol) 1 :action FREE)
        length (/ (reciprocal freq) 2)
        [fb1 fb2] (local-in 2)
        up-delay (delay-l fb1 0.05 length)
        down-delay (delay-l fb2 0.05 length)
        disp-filt (apf down-delay freq)
        loss-filt (one-pole (* disp-filt 0.99) 0.55)
        up-refl (neg up-delay)
        down-refl (neg loss-filt)
        string-vel (+ down-refl up-refl)]
    (local-out (+ stroke [down-refl up-refl]))
    (resonz down-refl 440 0.5)))

(definst guitar
  [note 60 velocity 80]
   (let [freq (midicps note)
        vol (/ velocity 127)
        stroke (env-gen (adsr 0.02 3.005 1.0 vol) 1 :action FREE)
        length (/ (reciprocal freq) 2)
        [fb1 fb2] (local-in 2)
        up-delay (delay-l fb1 0.05 length)
        down-delay (delay-l fb2 0.05 length)
        disp-filt (apf down-delay freq)
        loss-filt (one-pole (* disp-filt 0.99) 0.55)
        up-refl (neg up-delay)
        down-refl (neg loss-filt)
        string-vel (+ down-refl up-refl)]
    (local-out (+ stroke [down-refl up-refl]))
    (resonz down-refl 440 0.5)))

(guitar 63)

(stop)

(dampedwave 220 0.01)

(definst violin
  [note 60]
  (let [freq (midicps note)
        sig (sin-osc freq)
        delay (/ (reciprocal freq) 1)
        [fb1 fb2] (local-in 2)
        neck-delay (delay-l (+ fb1 sig) 0.05 delay)
        bridge-delay (delay-l (+ fb2 sig) 0.05 delay)
        neck-refl (neg neck-delay)
        bridge-refl (neg bridge-delay)]
    (local-out (* [bridge-refl neck-refl] 1))
    (+ bridge-delay neck-delay)))

(violin 69)

(bowed 69)

(stop)
