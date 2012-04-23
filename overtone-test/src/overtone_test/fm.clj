(ns overtone-test.fm
  (:use [overtone.core]))

(use 'overtone.inst.synth)

(definst fm [carrier 440 divisor 2.0 depth 1.0]
  (let [modulator (/ carrier divisor)
        mod-env (env-gen (lin-env 1 0 1))
        amp-env (env-gen (lin-env 0 1 1))]
    (* amp-env
       (sin-osc (+ carrier
                   (* mod-env (* carrier depth) (sin-osc modulator)))))))

(fm 440 (/ 7 5) 2)

(definst violin
  [note 60 velocity 80 gate 1 amp 1
   bow-offset 0 bow-slope 0.5 bow-position 0.75 vib-freq 6.127 vib-gain 0.2]
  (let [freq         (midicps note)
        velocity     (/ velocity 127)
        beta-ratio   (+ 0.027236 (* 0.2 bow-position))
        base-delay   (reciprocal freq)
        [fb1 fb2]    (local-in 2)
        vibrato      (* (sin-osc vib-freq) vib-gain)
        neck-delay   (+ (* base-delay (- 1 beta-ratio)) (* base-delay vibrato))
        neck         (delay-l fb1 0.05 neck-delay)
        nut-refl     (neg neck)
        bridge       (delay-l fb2 0.025 (* base-delay beta-ratio))
        string-filt  (one-pole (* bridge 0.95) 0.55)
        bridge-refl  (neg string-filt)
        adsr         (* amp (env-gen (adsr 0.02 3.005 1.0 0.01) gate :action FREE))
        string-vel   (+ bridge-refl nut-refl)
        vel-diff     (- adsr string-vel)
        slope        (- 5.0 (* 4 bow-slope))
        bow-table    (clip:ar (pow (abs (+ (* (+ vel-diff bow-offset) slope) 0.75 )) -4) 0 1)
        new-vel       (* vel-diff bow-table)]
   (local-out (+ [bridge-refl nut-refl] new-vel))
   (resonz (* bridge 0.5) 500 0.85)))

(violin)

(stop)
