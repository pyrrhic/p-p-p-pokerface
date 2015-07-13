(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank-char _] card
        rank-string (if (Character/isDigit rank-char)
                      (str rank-char)
                      (str (get rank-map rank-char)))]
    (Integer/valueOf rank-string)))

(defn suit [card]
  (let[[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [max-frequency (apply max (vals (frequencies (map rank hand))))]
    (= max-frequency 2)))

(defn three-of-a-kind? [hand]
  (let [max-frequency (apply max (vals (frequencies (map rank hand))))]
    (= max-frequency 3)))

(defn four-of-a-kind? [hand]
  (let [max-frequency (apply max (vals (frequencies (map rank hand))))]
    (= max-frequency 4)))

(defn flush? [hand]
  (let [max-frequency (apply max (vals (frequencies (map suit hand))))]
    (= max-frequency 5)))

(defn full-house? [hand]
  (let [suit-frequencies (vals (frequencies (map rank hand)))
        has-three-of-kind? (if (= (some #{3} suit-frequencies) 3) true false)
        has-two-of-kind? (if (= (some #{2} suit-frequencies) 2) true false)]
    (and has-three-of-kind? has-two-of-kind?)))

(defn two-pairs? [hand]
  (let [suit-frequencies (vals (frequencies (map rank hand)))
        num-of-pairs (count (filter #{2} suit-frequencies))]
    (or
      (= num-of-pairs 2)
      (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [sorted-ranks-assume-ace-last (sort (map rank hand))
        range-for-ace-last (range 
                             (first sorted-ranks-assume-ace-last) 
                             (inc (last sorted-ranks-assume-ace-last)))
        sorted-ranks-assume-ace-first (sort (replace {14 1} sorted-ranks-assume-ace-last))
        range-for-ace-first (range 
                             (first sorted-ranks-assume-ace-first) 
                             (inc (last sorted-ranks-assume-ace-first)))]
    (cond
      (= sorted-ranks-assume-ace-last range-for-ace-last) true
      (= sorted-ranks-assume-ace-first range-for-ace-first) true
      (four-of-a-kind? hand) true
      :else false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
 (let [checkers #{[high-card? 0]  [pair? 1]
                  [two-pairs? 2]  [three-of-a-kind? 3]
                  [straight? 4]   [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}
       applicable-hands (filter 
                          (fn [matcher-value] ((first matcher-value) hand)) 
                          checkers)
       applicable-hand-values (map second applicable-hands)]
   (apply max applicable-hand-values)))
