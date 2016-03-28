(ns cards-clojure.core
  (:gen-class))

(def suits [:clubs :spades :hearts :diamonds])

(def ranks (range 1 14))
(def rank-names {1 :ace 11 :jack 12 :queen 13 :king})
(def rank-num {:ace 1 :jack 11 :queen 12 :king 13})

(defn create-deck []
  (set
    (for [suit suits
          rank ranks]
      {:suit suit
       :rank (get rank-names rank rank)})))
      
(defn create-hands [deck]
  (set
    (for [c1 deck
          c2 (disj deck c1)
          c3 (disj deck c1 c2)
          c4 (disj deck c1 c2 c3)]
      #{c1 c2 c3 c4})))

(defn flush? [hand]
  (= 1 (count (set (map :suit hand)))))

(defn straight? [hand]
  (let [
        hand-check (sort
                     (set
                       (for [card hand]
                        (get rank-num (:rank card) (:rank card)))))]
       (and (= 4 (count hand-check))
            (= 3 (- (last hand-check) (first hand-check))))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))
  

(defn four-of-kind? [hand]
  (= 1 (count (set (map :rank hand)))))

(defn three-of-kind? [hand]
  (some #(= 3 %) (vals (frequencies (map :rank hand)))))

(defn two-pair? [hand]
  (and (= 2 (count (vals (frequencies (map :rank hand)))))  
       (= (first (vals (frequencies (map :rank hand)))) (last (vals (frequencies (map :rank hand))))))) 

(defn -main []
  (time
    (let [deck (create-deck)
          hands (create-hands deck)
          check-hands (filter two-pair? hands)]
      (count check-hands))))
    
  
