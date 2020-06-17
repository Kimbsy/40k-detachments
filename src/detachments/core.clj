(ns detachments.core
  (:require [detachments.battle-forged :as bf]
            [detachments.utils :as u]))

;;; Define units

(defn unit
  [name type points power-level]
  {:name   name
   :type   type
   :points points
   :pl     power-level
   :uuid   (str (java.util.UUID/randomUUID))})

(defn unit-type
  [[type units]]
  (pmap (fn [[name points pl]]
         (unit name type points pl))
       units))

(defn all-units
  [units]
  (mapcat unit-type units))

(def units-data (all-units {:hq [["Baharroth" 110 6]
                                 ["Illic Nightspear" 65 4]
                                 ["Spritseer" 55 3]
                                 ["Wraithseer" 135 10]
                                 ["Farseer" 115 6]
                                 ["Farseer" 115 6]]
                            :troops [["Dire Avengers" 61 3]
                                     ["Dire Avengers" 61 3]
                                     ["Guardian Defenders" 80 5]]
                            :elites [["wraithblades" 175 10]
                                     ["Wraithguard" 165 11]]
                            :fast-attack [["Swooping Hawks" 65 3]
                                          ["Vypers" 58 4]
                                          ["Windriders" 69 4]]
                            :heavy-support [["War Walkers" 49 4]
                                            ["Wraithlord" 142 8]]
                            :flyer []
                            :dedicated-transport []
                            :fortifications []
                            :lord-of-war [["Wraithknight" 350 27]]}))

(def beth-units (all-units {:hq [["Master of Possesion" 88 5]
                                 ["Master of Possesion" 88 5]]
                            :troops [["Chaos Space Marines" 132 8]
                                     ["Chaos Space Marines" 132 8]
                                     ["Chaos Space Marines" 132 8]]
                            :elites [["Greater Possesed" 120 8]]
                            :fast-attack []
                            :heavy-support [["Obliterators" 190 12]
                                            ["Venomcrawler" 115 7]]
                            :flyer [["Heldrake" 140 9]]
                            :dedicated-transport []
                            :fortifications []
                            :lord-of-war []}))

(def army-targets [750 1000 1500 1750 100000])


;;; Transducers

(defn only-type
  "Returns a transducer to filter units by a set of types."
  [types]
  (filter #(types (:type %))))

(defn costing-less
  "Returns a transducer which filters for elements under a specified cost."
  [cost]
  (filter #(<= (:cost %) cost)))

(defn costing-close-to
  "Returns a transducer which filters for elements close to a specified cost withing a configurable range."
  [target & {:keys [plus minus] :or {plus 0 minus 100}}]
  (filter #(<= (- target minus) (:cost %) (+ target plus))))

(defn extract-cost
  "Returns a transducer to extract the cost of units."
  [cost-key]
  (map cost-key))

;;; Combinatorics

(defn cost
  "Calculate the cost of a collection of units."
  [units & {:keys [cost-key] :or {cost-key :points}}]
  (transduce (extract-cost cost-key) + units))

(defn cost-combos
  "Costs of all unit combinations."
  [units & {:keys [cost-key] :or {cost-key :points}}]
  (->> (u/combos units)
       (pmap (fn [combo]
               {:cost (cost combo :cost-key cost-key)
                :units combo}))
       (sort-by :cost)))

(defn max-combo
  "Determine the combination of units with the highest cost under a specified limit."
  [units cost-limit & {:keys [cost-key] :or {cost-key :points}}]
  (last (transduce (costing-less cost-limit)
                   conj
                   (cost-combos units :cost-key cost-key))))

(defn max-army
  [target & {:keys [cost-key] :or {cost-key :points}}]
  (max-combo units-data target :cost-key cost-key))

(defn max-armies
  [& {:keys [targets] :or {targets army-targets}}]
  (pmap (partial max-combo units-data)
        targets))

(defn close-combos
  [units cost-target & {:keys [plus minus cost-key] :or {plus 0 minus 100 cost-key :points}}]
  (transduce (costing-close-to cost-target :plus plus :minus minus)
             conj
             (cost-combos units :cost-key cost-key)))

(defn close-armies
  [units cost-target & {:keys [plus minus cost-key] :or {plus 0 minus 100 cost-key :points}}]
  (close-combos units cost-target :plus plus :minus minus :cost-key cost-key))

(comment
  (->> (close-armies beth-units 1000)
       (pmap bf/optimize-detachments)
       (sort-by :total-cp)
       (take-last 5)
       reverse
       (map u/clean)))
