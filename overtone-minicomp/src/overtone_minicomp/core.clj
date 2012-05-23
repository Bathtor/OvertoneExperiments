(ns overtone-minicomp.core
  (:use overtone.core))
;;(connect-external-server "192.168.0.1" 57110)
(connect-external-server 57110)
(use 'overtone.inst.piano)

(def note-length [32 16 8 4 2 1])
(def note-value-prob [0.03 0.2 0.25 0.25 0.17 0.1])

(reduce + note-value-prob)

(defn in32s
  [n frac]
  (let [ratio (/ 32 frac)]
    (if (>= ratio 1)
      (* n ratio)
      (throw (IllegalArgumentException. (str "input frac " frac " not divisable by 32!"))))))

(defn choose-value
  [bound]
  (if (= 1 bound)
    32
      (let [value (weighted-choose note-length note-value-prob)]
        (if (<= (in32s 1 value) bound)
          value
          (recur bound)))))

(defn not-silence?
 ([pattern]
    (let [[degrees value] pattern]
      (not-silence? true degrees)))
 ([val degrees]
    (if (empty? degrees)
      false
      (let [degree (first degrees)]
        (if degree
          true
          (recur val (rest degrees)))))))

(defn gen-pattern
  ([scale length frac]
     (let [pattern (gen-pattern (resolve-scale scale) length frac [] [])]
       (if (not-silence? pattern)
         pattern
         (recur scale length frac))))
  ([scale length frac notes values]
     (let [remain (in32s length frac)
           value (in32s 1 (choose-value remain))
           scale-size (count scale)
           pause (weighted-coin 0.25)
           degree (if pause nil (rand-int scale-size))
           remainder (- remain value)
           nextlength (if (= 1 remainder) 0 remainder)
           nextnotes (conj notes degree)
           nextvalues (conj values (if (= 1 remainder) (+ 1 value) value))]
       (if (= 0 nextlength)
         [nextnotes nextvalues]
         (recur scale nextlength 32 nextnotes nextvalues)))))

(defn resolve-pattern
  [scale root pattern]
  (let [[degrees values] pattern
        notes (degrees->pitches degrees scale root)]
    [notes values]))





(gen-pattern :diatonic 1 4)

(defn player
  ([t bpm phrase]
     (let [quarterlength (beat-ms 1 bpm)
           thirtytwolength (/ quarterlength 8)
           [notes values] phrase
           control (atom true)]
       (player t thirtytwolength notes values control)
       control))
  ([t length notes values control]
     (if (true? @control)
       (let [note (first notes)
           value (first values)
           notelength (* length value)]
         (when note
           (at t (piano note)))
         (let [next-time (+ t notelength)]
           (apply-at next-time player [next-time length (rest notes) (rest values) control])))
       control)))

(defn player-stop
  [control]
  (reset! control false))

(defn pattern-cycle
  [pattern]
  (let [[notes values] pattern]
    [(cycle notes) (cycle values)]))

(def rh (player (now) 120 (pattern-cycle (resolve-pattern :melodic-minor-asc :E2 (gen-pattern :melodic-minor-asc 2 8)))))
(def mel (player (now) 120 (pattern-cycle (resolve-pattern :melodic-minor-asc :E2 (gen-pattern :melodic-minor-asc 8 4)))))

(player-stop rh)
(player-stop mel)

(stop)
