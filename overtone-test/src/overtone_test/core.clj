(ns foo
  (:use [overtone.core]))
(connect-external-server 57110)
(use 'overtone.inst.synth)

(def pi-digits '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9))
(def tau-digits '(6 2 8 3 1 8 5 3 0 7 1 7 9 5 8 6 4 7 6 9 2 5 2 8 6 7 6 6 5 5 9))

(defn scalepos->interval
  [scalepos scale]
  (cond
    (nil? scalepos) nil
    (< scalepos 8) (degree->interval scalepos scale)
    :default (+ 12 (scalepos->interval (- scalepos 8) scale))))

(defn scaleposis->pitches
  [scaleposis scale root]
  (let [root (note root)]
    (when (nil? root)
      (throw (IllegalArgumentException. (str "root resolved to nil. that sucks!"))))
     (map (fn [scalepos]
             (cond
               (coll? scalepos) (scaleposis->pitches scalepos scale root)
               (nil? scalepos) nil
               :default (if-let [interval (scalepos->interval scalepos scale)]
                          (+ root interval))))
          scaleposis)))

(def pi-pitches (scaleposis->pitches pi-digits :melodic-minor-asc :C4))
(def tau-pitches (scaleposis->pitches tau-digits :melodic-minor-asc :C4))

(defn play
  [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (piano note)))
    (let [next-time (+ time sep)]
      (apply-at next-time play [next-time (rest notes) sep]))))

(play (now) pi-pitches 200)
(play (now) tau-pitches 200)

(let [t (+ 500 (now))]
  (play t (cycle (reverse pi-pitches)) 200)
  (play t (cycle (reverse tau-pitches)) 200))

(stop)
