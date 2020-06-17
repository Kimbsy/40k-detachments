(ns detachments.utils
  (:require [clojure.math.combinatorics :as combo]))

(defn combos
  "All possible combinations of units."
  [units]
  (mapcat (fn [i]
            (combo/combinations units i))
          (range 1 (inc (count units)))))

(defn clean
  "Get rid of the uuids from units in an army."
  [army]
  (update army :detachments (fn [detachments]
                              (map (fn [detachment]
                                     (update detachment :units (fn [units]
                                                                 (map (fn [unit]
                                                                        (dissoc unit :uuid))
                                                                      units))))
                                   detachments))))
