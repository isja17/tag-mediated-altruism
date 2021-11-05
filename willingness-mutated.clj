;; gorilla-repl.fileformat = 1

;; **
;;; # Evolution of Tag-Mediated altruism with mutated willingnesses
;;; I take my first project and include mutations for the willingness of individuals.
;;; 
;; **

;; @@
(ns tag-mediated-altruism-mutated-willingness
  (:require [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def pop-size 100)
(def cost 0.1)
(def benefit 1.0)
(def pairings 3)
(def mutation-rate 1)
(def gens 30000)
(def minimum-tolerance 0)
(def donation-chance 0.5)
;these are used as global values
(def donations (atom 0))
(def failures (atom 0))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/failures</span>","value":"#'tag-mediated-altruism-mutated-willingness/failures"}
;; <=

;; @@
(defn population
  "Returns the tag and tolerance of an individual as a vector."
  []
  {:score 0 :tag (rand) :tolerance (rand) :willingness (rand)}) 

(defn new-population
  "Returns a vector of pop-size agents"
  []
  (repeatedly pop-size #(population)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/new-population</span>","value":"#'tag-mediated-altruism-mutated-willingness/new-population"}
;; <=

;; @@
(defn maybe-donate
  "Returns the result of a single donation attempt"
  [[donor recipient]] ;takes in a vector, seq, etc w two elements
  (if (and (>= (:willingness donor) (:willingness recipient)) ;if willing and are similar enough
           (<= (Math/abs (- (:tag donor) (:tag recipient)))
               (:tolerance donor)))
    (do (swap! donations inc) ;then inc donations
      [(assoc donor :score (- (:score donor) cost)) 
       (assoc recipient :score (+ (:score recipient) benefit))]) ;then associate donor and recipient with their new scores
    (do (swap! failures inc) ;else inc failures
      [donor recipient]))) ;then return donor and recipient unchanged
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/maybe-donate</span>","value":"#'tag-mediated-altruism-mutated-willingness/maybe-donate"}
;; <=

;; @@
(defn one-round-donations
  "Returns the population after one round of donation attemps"
  [population]
  (let [paired (partition 2 population) ;creates a sequence of sequences that contain two indivuals in the pop
        post-donations (map maybe-donate paired)] ; runs maybe-donate through all of the pairs
    (apply concat post-donations)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/one-round-donations</span>","value":"#'tag-mediated-altruism-mutated-willingness/one-round-donations"}
;; <=

;; @@
(defn all-donations
  "Returns the population after all rounds of pairings and donations"
  [population]
  (reset! donations 0) ;put these to 0 at the start of each tournament 
  (reset! failures 0)
  (loop [round 0
         p population] 
    (if (>= round pairings) ;if all pairings have been done
      p ;return p
      (do (recur (inc round) ;else inc round
               (shuffle (one-round-donations p))))))) ;then shuffle the population with updated scores after donations. Shuffle happens so that new individuals get paired together
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/all-donations</span>","value":"#'tag-mediated-altruism-mutated-willingness/all-donations"}
;; <=

;; @@
;this is how the tolerance of the agents are mutated in Riolio et al.
(defn gaussian-noise-factor
  "Returns gaussian noise of mean 0, std dev 1."
  []
  (*' (Math/sqrt (*' -2.0 (Math/log (rand))))
      (Math/cos (*' 2.0 Math/PI (rand)))))

(defn noise
  "Returns gaussian noise of mean 0, std dev 0.01."
  []
  (* 0.01 (gaussian-noise-factor)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/noise</span>","value":"#'tag-mediated-altruism-mutated-willingness/noise"}
;; <=

;; @@
(defn reproduce
  "Returns a population of children produced from the given population."
  [population]
  (mapv (fn [a] ;maps a function that takes an individual from population and compaires it to a random person. 
          (let [competitor (rand-nth population) 
                winner (if (>= (:score a) (:score competitor)) 
                         a
                         competitor)
                tag-mutated (if (<= (rand) mutation-rate)
                              (assoc winner :tag (rand)) ;tags are mutated w a new rand num.
                              winner)
                tolerance-mutated (if (<= (rand) mutation-rate) 
                                    (assoc tag-mutated ;then associate tag-mutated to keep the mutation 
                                      :tolerance ;alter the tolerance to be
                                      (max minimum-tolerance 
                                           (min 1 ;makes sure tolerance doesnt go below 0 or above 1
                                                (+ (:tolerance tag-mutated) ;mutate the tolerance to be the gaussian-noise
                                                   (noise)))))
                                    tag-mutated) ;else return tag-mutated unchanged
                willingness-mutated (if (<= (rand) mutation-rate)
                                      (assoc tolerance-mutated :willingness (rand))
                                      tolerance-mutated)] ;else return tolerance-mutated unchanged
            (assoc willingness-mutated :score 0)))
        population))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/reproduce</span>","value":"#'tag-mediated-altruism-mutated-willingness/reproduce"}
;; <=

;; @@
(defn run-simulation
  "Runs a simulation for my model."
  []
  (reset! donations 0)
  (reset! failures 0)
  (loop [generation 0
         p (new-population)
         rates []]
    (if (> generation gens)
      rates
      (let [next-population (-> p
                                (all-donations)
                                (reproduce))
            rate (float (/ @donations (+ @donations @failures)))] ;rate of donations using the atoms. @ accesses the atoms.
        (recur (inc generation)
               next-population
               (conj rates rate))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/run-simulation</span>","value":"#'tag-mediated-altruism-mutated-willingness/run-simulation"}
;; <=

;; @@
(plot/list-plot (run-simulation))
;; @@
;; =>
;; <=

;; @@
(defn mean
  [coll]
  (/ (apply + coll) (count coll)))

(defn variance
  [coll]
  (/ (apply + (map #(Math/abs (float (- (mean coll) %)))
                   coll))
     (- (count coll) 1)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/variance</span>","value":"#'tag-mediated-altruism-mutated-willingness/variance"}
;; <=

;; @@
(defn run-simulation-with-willingness-deviation
  "Runs a simulation for my model."
  []
  (reset! donations 0)
  (reset! failures 0)
  (loop [generation 0
         p (new-population)
         variances []]
    (if (> generation gens)
      variances
      (let [next-population (-> p
                                (all-donations)
                                (reproduce))
            get-tag (mapv :willingness p)
            variant (variance get-tag)] ;rate of donations using the atoms. @ accesses the atoms.
        
        (recur (inc generation)
               next-population
               (conj variances variant))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/run-simulation-with-willingness-deviation</span>","value":"#'tag-mediated-altruism-mutated-willingness/run-simulation-with-willingness-deviation"}
;; <=

;; @@
(plot/list-plot (run-simulation-with-willingness-deviation))
;; @@
;; =>
;; <=

;; @@
(mean (run-simulation-with-willingness-deviation))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.25083896983828824</span>","value":"0.25083896983828824"}
;; <=

;; @@
(defn run-simulation-with-tag-deviation
  "Runs a simulation for my model."
  []
  (reset! donations 0)
  (reset! failures 0)
  (loop [generation 0
         p (new-population)
         variances []]
    (if (> generation gens)
      variances
      (let [next-population (-> p
                                (all-donations)
                                (reproduce))
            get-tag (mapv :tag p)
            variant (variance get-tag)] ;rate of donations using the atoms. @ accesses the atoms.
        
        (recur (inc generation)
               next-population
               (conj variances variant))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/run-simulation-with-tag-deviation</span>","value":"#'tag-mediated-altruism-mutated-willingness/run-simulation-with-tag-deviation"}
;; <=

;; @@
(plot/list-plot (run-simulation-with-tag-deviation))
;; @@
;; =>
;; <=

;; @@
(mean (run-simulation-with-tag-deviation))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.2507880722722372</span>","value":"0.2507880722722372"}
;; <=

;; @@
(defn run-simulation-with-tolerance-deviation
  "Runs a simulation for my model."
  []
  (reset! donations 0)
  (reset! failures 0)
  (loop [generation 0
         p (new-population)
         variances []]
    (if (> generation gens)
      variances
      (let [next-population (-> p
                                (all-donations)
                                (reproduce))
            get-tag (mapv :tolerance p)
            variant (variance get-tag)] ;rate of donations using the atoms. @ accesses the atoms.
        
        (recur (inc generation)
               next-population
               (conj variances variant))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism-mutated-willingness/run-simulation-with-tolerance-deviation</span>","value":"#'tag-mediated-altruism-mutated-willingness/run-simulation-with-tolerance-deviation"}
;; <=

;; @@
(plot/list-plot (run-simulation-with-tolerance-deviation))
;; @@
;; =>
;; <=

;; @@
(mean (run-simulation-with-tolerance-deviation))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.02205010838611727</span>","value":"0.02205010838611727"}
;; <=

;; @@
;In this test, the donation rate went down from a little over 30% to about 8%, whereas my original simulations dropped from 45% to about 12%, which shows donations happen at an even lower rate with mutated willingnesses. 
;Moreover, the willingness in the original model rapidly found its way to nearly 0 within a few generations, whereas these simulations kept the williness between .2 and .3. Interestingly, the tolerance and tag stayed about the same in this model, but we did see a very slight increase in tolerance. 
;Thus, both adding in and mutating the willingness shows very little cooperation in the population. Riolo's model, using the same population size and generations, showed a 70% cooperation rate, whereas my models show a rate of about 10%. This suggest that in his model he is "forcing" donations, i.e. every individual is forced to donate if someone is similar, whereas here individuals must be willing to donate AND the person must be similar enough. 
;; @@
