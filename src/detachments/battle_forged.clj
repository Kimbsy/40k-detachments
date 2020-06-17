(ns detachments.battle-forged
  (:require [detachments.utils :as u]
            [detachments.battle-forged :as bf]
            [clojure.set :as set]))

; @TODO: how do dedicated transports work?? maybe add them in in find-place-for-unit ?

(defn count-type
  [t units]
  (count (filter #(= t (:type %)) units)))

;;; Detachment constraint predicates

(defn patrol?
  [units]
  (and (<= 2 (count units) 11)
       (every? #(#{:hq :troops :elites :fast-attack :heavy-support} (:type %)) units)
       (<= 1 (count-type :hq units) 2)
       (<= 1 (count-type :troops units) 3)
       (<= 0 (count-type :elites units) 2)
       (<= 0 (count-type :fast-attack units) 2)
       (<= 0 (count-type :heavy-support units) 2)))

(defn battalion?
  [units]
  (and (<= 5 (count units) 23)
       (every? #(#{:hq :troops :elites :fast-attack :heavy-support :flyer} (:type %)) units)
       (<= 2 (count-type :hq units) 3)
       (<= 3 (count-type :troops units) 6)
       (<= 0 (count-type :elites units) 3)
       (<= 0 (count-type :fast-attack units) 3)
       (<= 0 (count-type :heavy-support units) 3)
       (<= 0 (count-type :flyer units) 2)))

(defn brigade?
  [units]
  (and (<= 18 (count units) 37)
       (every? #(#{:hq :troops :elites :fast-attack :heavy-support :flyer} (:type %)) units)
       (<= 3 (count-type :hq units) 5)
       (<= 6 (count-type :troops units) 12)
       (<= 3 (count-type :elites units) 8)
       (<= 3 (count-type :fast-attack units) 5)
       (<= 3 (count-type :heavy-support units) 5)
       (<= 0 (count-type :flyer units) 2)))

(defn vanguard?
  [units]
  (and (<= 4 (count units) 17)
       (every? #(#{:hq :troops :elites :fast-attack :heavy-support :flyer} (:type %)) units)
       (<= 1 (count-type :hq units) 2)
       (<= 0 (count-type :troops units) 3)
       (<= 3 (count-type :elites units) 6)
       (<= 0 (count-type :fast-attack units) 2)
       (<= 0 (count-type :heavy-support units) 2)
       (<= 0 (count-type :flyer units) 2)))

(defn outrider?
  [units]
  (and (<= 4 (count units) 17)
       (every? #(#{:hq :troops :elites :fast-attack :heavy-support :flyer} (:type %)) units)
       (<= 1 (count-type :hq units) 2)
       (<= 0 (count-type :troops units) 3)
       (<= 0 (count-type :elites units) 2)
       (<= 3 (count-type :fast-attack units) 6)
       (<= 0 (count-type :heavy-support units) 2)
       (<= 0 (count-type :flyer units) 2)))

(defn spearhead?
  [units]
  (and (<= 4 (count units) 17)
       (every? #(#{:hq :troops :elites :fast-attack :heavy-support :flyer} (:type %)) units)
       (<= 1 (count-type :hq units) 2)
       (<= 0 (count-type :troops units) 3)
       (<= 0 (count-type :elites units) 2)
       (<= 0 (count-type :fast-attack units) 2)
       (<= 3 (count-type :heavy-support units) 6)
       (<= 0 (count-type :flyer units) 2)))

(defn supreme-command?
  [units]
  (and (<= 3 (count units) 7)
       (every? #(#{:hq :elites :lord-of-war} (:type %)) units)
       (<= 3 (count-type :hq units) 5)
       (<= 0 (count-type :elites units) 1)
       (<= 0 (count-type :lord-of-war units) 1)))

(defn super-heavy?
  [units]
  (and (<= 3 (count units) 5)
       (every? #(= :lord-of-war (:type %)) units)))

(defn super-heavy-auxiliary?
  [units]
  (and (= 1 (count units))
       (= :lord-of-war (:type (first units)))))

(defn air-wing?
  [units]
  (and (<= 3 (count units) 5)
       (every? #(= :flyer (:type %)) units)))

(defn fortification-network?
  [units]
  (and (<= 1 (count units) 3)
       (every? #(= :fortifications (:type %)) units)))

(defn auxiliary-support?
  [units]
  (and
   (= 1 (count units))
   (#{:hq :troops :elites :fast-attack :heavy-support :flyer :dedicated-transport} (:type (first units)))))


;;; Detachment superset predicates

(defn contains-patrol?
  [units]
  (and (<= 2 (count units))
       (<= 1 (count-type :hq units))
       (<= 1 (count-type :troops units))))

(defn contains-battalion?
  [units]
  (and (<= 5 (count units))
       (<= 2 (count-type :hq units))
       (<= 3 (count-type :troops units))))

(defn contains-brigade?
  [units]
  (and (<= 18 (count units))
       (<= 3 (count-type :hq units))
       (<= 6 (count-type :troops units))
       (<= 3 (count-type :elites units))
       (<= 3 (count-type :fast-attack units))
       (<= 3 (count-type :heavy-support units))))

(defn contains-vanguard?
  [units]
  (and (<= 4 (count units))
       (<= 1 (count-type :hq units))
       (<= 3 (count-type :elites units))))

(defn contains-outrider?
  [units]
  (and (<= 4 (count units))
       (<= 1 (count-type :hq units))
       (<= 3 (count-type :fast-attack units))))

(defn contains-spearhead?
  [units]
  (and (<= 4 (count units))
       (<= 1 (count-type :hq units))
       (<= 3 (count-type :heavy-support units))))

(defn contains-supreme-command?
  [units]
  (and (<= 3 (count units))
       (<= 3 (count-type :hq units))))

(defn contains-super-heavy?
  [units]
  (and (<= 3 (count units))
       (<= 3 (count-type :lord-of-war units))))

(defn contains-super-heavy-auxiliary?
  [units]
  (and (<= 1 (count units))
       (<= 1 (count-type :lord-of-war units))))

(defn contains-air-wing?
  [units]
  (and (<= 3 (count units))
       (<= 3 (count-type :flyer units))))

(defn contains-fortification-network?
  [units]
  (and (<= 1 (count units))
       (<= 1 (count-type :fortifications units))))

(defn contains-auxiliary-support?
  [units]
  (<= 1 (count units)))


;;; Detachment extractors

(defn take-by-type
  [unit-type n units]
  (take n (filter #(= unit-type (:type %)) units)))

(defn extract-minimal-brigade
  [units-vector]
  (let [units                  (set units-vector)
        required-hqs           (take-by-type :hq 3 units)
        required-troops        (take-by-type :troops 6 units)
        required-elites        (take-by-type :elites 3 units)
        required-fast-attack   (take-by-type :fast-attack 3 units)
        required-heavy-support (take-by-type :heavy-support 3 units)        
        remainder              (set/difference units
                                               required-hqs
                                               required-troops
                                               required-elites
                                               required-fast-attack
                                               required-heavy-support)]
    {:detachment {:detachment :brigade :cp 9 :units (vec (concat required-hqs
                                                                 required-troops
                                                                 required-elites
                                                                 required-fast-attack
                                                                 required-heavy-support))}
     :remainder  remainder}))

(defn extract-minimal-battalion
  [units-vector]
  (let [units           (set units-vector)
        required-hqs    (take-by-type :hq 2 units)
        required-troops (take-by-type :troops 3 units)
        remainder       (set/difference units
                                        required-hqs
                                        required-troops)]
    {:detachment {:detachment :battalion :cp 3 :units (vec (concat required-hqs
                                                                   required-troops))}
     :remainder  remainder}))

(defn extract-minimal-patrol
  [units-vector]
  (let [units           (set units-vector)
        required-hqs    (take-by-type :hq 1 units)
        required-troops (take-by-type :troops 1 units)
        remainder       (set/difference units
                                        required-hqs
                                        required-troops)]
    {:detachment {:detachment :patrol :cp 1 :units (vec (concat required-hqs
                                                                required-troops))}
     :remainder  remainder}))

(defn extract-minimal-vanguard
  [units-vector]
  (let [units           (set units-vector)
        required-hqs    (take-by-type :hq 1 units)
        required-elites (take-by-type :elites 3 units)
        remainder       (set/difference units
                                        required-hqs
                                        required-elites)]
    {:detachment {:detachment :vanguard :cp 1 :units (vec (concat required-hqs
                                                                  required-elites))}
     :remainder  remainder}))

(defn extract-minimal-outrider
  [units-vector]
  (let [units                (set units-vector)
        required-hqs         (take-by-type :hq 1 units)
        required-fast-attack (take-by-type :fast-attack 3 units)
        remainder            (set/difference units
                                             required-hqs
                                             required-fast-attack)]
    {:detachment {:detachment :outrider :cp 1 :units (vec (concat required-hqs
                                                                  required-fast-attack))}
     :remainder  remainder}))

(defn extract-minimal-spearhead
  [units-vector]
  (let [units                  (set units-vector)
        required-hqs           (take-by-type :hq 1 units)
        required-heavy-support (take-by-type :heavy-support 4 units)
        remainder              (set/difference units
                                               required-hqs
                                               required-heavy-support)]
    {:detachment {:detachment :spearhead :cp 1 :units (vec (concat required-hqs
                                                                   required-heavy-support))}
     :remainder  remainder}))

(defn extract-minimal-supreme-command
  [units-vector]
  (let [units        (set units-vector)
        required-hqs (take-by-type :hq 3 units)
        remainder    (set/difference units
                                     required-hqs)]
    {:detachment {:detachment :supreme-command :cp 1 :units (vec required-hqs)}
     :remainder  remainder}))

(defn extract-super-heavy
  [units-vector]
  (let [units                 (set units-vector)
        required-lord-of-wars (take-by-type :lord-of-war 5 units)
        remainder             (set/difference units
                                              required-lord-of-wars)]
    {:detachment {:detachment :super-heavy :cp 1 :units (vec required-lord-of-wars)}
     :remainder  remainder}))

(defn extract-super-heavy-auxiliary
  [units-vector]
  (let [units                 (set units-vector)
        required-lord-of-wars (take-by-type :lord-of-war 1 units)
        remainder             (set/difference units
                                              required-lord-of-wars)]
    {:detachment {:detachment :super-heavy-auxiliary :cp 0 :units (vec required-lord-of-wars)}
     :remainder  remainder}))

(defn extract-air-wing
  [units-vector]
  (let [units           (set units-vector)
        required-flyers (take-by-type :flyer 5 units)
        remainder       (set/difference units
                                        required-flyers)]
    {:detachment {:detachment :air-wing :cp 1 :units (vec required-flyers)}
     :remainder  remainder}))

(defn extract-fortification-network
  [units-vector]
  (let [units           (set units-vector)
        required-flyers (take-by-type :flyer 3 units)
        remainder       (set/difference units
                                        required-flyers)]
    {:detachment {:detachment :fortification-network :cp 0 :units (vec required-flyers)}
     :remainder  remainder}))


;;; Detachment optimization

(defn add-detachment
  [army detachment]
  (update army :detachments #(conj % detachment)))

(defn auxiliary-support-detachment
  [unit]
  {:detachment :auxiliary-support :cp -1 :units [unit]})

(defn valid-detachment?
  [{:keys [units] :as detachment}]
  (let [pred (case (:detachment detachment)
               :brigade               brigade?               
               :battalion             battalion?             
               :patrol                patrol?                
               :vanguard              vanguard?              
               :outrider              outrider?              
               :spearhead             spearhead?             
               :supreme-command       supreme-command?       
               :super-heavy           super-heavy?           
               :super-heavy-auxiliary super-heavy-auxiliary? 
               :air-wing              air-wing?              
               :fortification-network fortification-network? 
               :auxiliary-support     auxiliary-support?)]
    (when (pred units)
      detachment)))

(defn insert-unit
  [unit detachment]
  (when-let [enriched-detachment (valid-detachment? (update detachment :units #(conj % unit)))]
    enriched-detachment))

(defn find-place-for-unit
  [acc unit]
  (if-let [{:keys [detachment i]} (->> (get-in acc [:enriched-army :detachments])
                                       (map-indexed (fn [i d]
                                                      (when-let [inserted (insert-unit unit d)]
                                                        {:detachment inserted :i i})))
                                       (filter some?)
                                       first)]
    (-> acc
        (assoc-in [:enriched-army :detachments i] detachment)
        (update :inserted-units #(conj % unit)))
    acc))

(defn enrich-army
  "Insert units into avaiable spaces in existing detachments."
  [army units]
  (let [{:keys [enriched-army inserted-units]}
        (reduce find-place-for-unit {:enriched-army army :inserted-units []} units)

        remainder         (set/difference units inserted-units)
        extra-detachments (map auxiliary-support-detachment remainder)
        enriched-army (-> enriched-army
                          (update :detachments #(concat % extra-detachments)))]
    (assoc enriched-army :total-cp (reduce + (map :cp (:detachments enriched-army))))))

(defn optimize-detachments
  ([{:keys [units cost]}]
   (optimize-detachments {:cost cost :detachments []} units))
  ([army units]
   (cond
     (empty? units)
     (assoc army :total-cp (reduce + (map :cp (:detachments army))))

     (contains-brigade? units)
     (let [{:keys [detachment remainder]} (extract-minimal-brigade units)]
       (recur (add-detachment army detachment) remainder))

     (contains-battalion? units)
     (let [{:keys [detachment remainder]} (extract-minimal-battalion units)]
       (recur (add-detachment army detachment) remainder))

     (contains-patrol? units)
     (let [{:keys [detachment remainder]} (extract-minimal-patrol units)]
       (recur (add-detachment army detachment) remainder))

     (contains-vanguard? units)
     (let [{:keys [detachment remainder]} (extract-minimal-vanguard units)]
       (recur (add-detachment army detachment) remainder))

     (contains-outrider? units)
     (let [{:keys [detachment remainder]} (extract-minimal-outrider units)]
       (recur (add-detachment army detachment) remainder))

     (contains-spearhead? units)
     (let [{:keys [detachment remainder]} (extract-minimal-spearhead units)]
       (recur (add-detachment army detachment) remainder))

     (contains-supreme-command? units)
     (let [{:keys [detachment remainder]} (extract-minimal-supreme-command units)]
       (recur (add-detachment army detachment) remainder))

     (contains-super-heavy? units)
     (let [{:keys [detachment remainder]} (extract-super-heavy units)]
       (recur (add-detachment army detachment) remainder))

     (contains-super-heavy-auxiliary? units)
     (let [{:keys [detachment remainder]} (extract-super-heavy-auxiliary units)]
       (recur (add-detachment army detachment) remainder))

     (contains-air-wing? units)
     (let [{:keys [detachment remainder]} (extract-air-wing units)]
       (recur (add-detachment army detachment) remainder))

     (contains-fortification-network? units)
     (let [{:keys [detachment remainder]} (extract-fortification-network units)]
       (recur (add-detachment army detachment) remainder))

     :else
     (enrich-army army units))))

;; (def test-bat [{:name "Dire Avengers", :type :troops, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab1"} {:name "Dire Avengers", :type :troops, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab2"} {:name "Dire Avengers", :type :troops, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab3"} {:name "Illic Nightspear", :type :hq, :points 65, :pl 4, :uuid "bb2c76db-a2e8-4e15-9b72-8f8f7c2410f6"} {:name "Spritseer", :type :hq, :points 55, :pl 3, :uuid "d4a33d02-17f8-4ff8-b54d-ec15f1e6e91a"}])

;; (def test-bat+ (conj test-bat {:name "something", :type :fortifications, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab4"}))

;; (def test-750 [{:name "Dire Avengers", :type :troops, :points 61, :pl 3, :uuid "34debc2c-a3b0-4825-80ec-362a38eb697a"} {:name "Dire Avengers", :type :troops, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab0"} {:name "Guardian Defenders", :type :troops, :points 80, :pl 5, :uuid "4a6bdaac-1a23-44c6-82d0-06f871ad3825"} {:name "Vypers", :type :fast-attack, :points 58, :pl 4, :uuid "7b0ba911-c1ad-44b3-aa5f-75ea180261ea"} {:name "Windriders", :type :fast-attack, :points 69, :pl 4, :uuid "139b93b2-93e4-427b-9d19-3bfa0955a2c0"} {:name "Baharroth", :type :hq, :points 110, :pl 6, :uuid "a3b5f6a8-bc0d-49bf-9fe6-1eee65f46663"} {:name "Illic Nightspear", :type :hq, :points 65, :pl 4, :uuid "bb2c76db-a2e8-4e15-9b72-8f8f7c2410f6"} {:name "Spritseer", :type :hq, :points 55, :pl 3, :uuid "d4a33d02-17f8-4ff8-b54d-ec15f1e6e91a"} {:name "War Walkers", :type :heavy-support, :points 49, :pl 4, :uuid "6ea78c27-2540-47d0-aa7e-c87769966dce"} {:name "Wraithlord", :type :heavy-support, :points 142, :pl 8, :uuid "9d2c02b0-ebf3-4a7c-abcd-aac40d206563"}])

;; (def test-bat-van [{:name "Dire Avengers", :type :troops, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab1"} {:name "Dire Avengers", :type :troops, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab2"} {:name "Dire Avengers", :type :troops, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab3"} {:name "Wraithguard", :type :elites, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab4"} {:name "Wraithguard", :type :elites, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab5"} {:name "Wraithguard", :type :elites, :points 61, :pl 3, :uuid "9c1eecab-d141-4c0b-a9bc-9b385557aab6"} {:name "Illic Nightspear", :type :hq, :points 65, :pl 4, :uuid "bb2c76db-a2e8-4e15-9b72-8f8f7c2410f6"} {:name "Baharroth", :type :hq, :points 65, :pl 4, :uuid "bb2c76db-a2e8-4e15-9b72-8f8f7c2410f7"} {:name "Spritseer", :type :hq, :points 55, :pl 3, :uuid "d4a33d02-17f8-4ff8-b54d-ec15f1e6e91a"}])

;; (contains-battalion? test-750)
;; (extract-minimal-battalion test-750)

;; (optimize-detachments test-bat-van)



