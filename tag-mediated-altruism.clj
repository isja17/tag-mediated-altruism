;; gorilla-repl.fileformat = 1

;; **
;;; # Evolution of Tag-Mediated altruism
;;; "A long-standing problem in biological and social sciences is to understand the conditions required for the emergence and maintenance of cooperation in evolving populations." (Riolo et al.) In this project, I take ideas from Rolio and expand on them to test the conditions in which altruism evolves.
;;; 
;;; In Riolo's model, to test altruism, we give each agent a tag and tolerence. Both of these are a random floating # between 0 and 1. Starting with a score of zero, two agents are paired, the difference of their tags are taken and compaired to the tolerance of the potential donor. If the difference is lte the tolerence, then the donation happenes. A small cost (0.1) is deduced from the donors score, and a benefit (1.0) is added to the recipients score. 
;;; 
;;; By finding the difference of the tag, we are finding the similarity between the two agents. The tolerance represents ones willingness to give. If the difference between the two agents is lower than the potential donors tolerance to give, the agent will donate.
;;; 
;;; This model seems to miss the mark on what altruism is. In this, someone MUST donate if the agent they are paired with is similar to them. This seems to miss that altruism is a voluntary act, meaning the potential donor must be willing to give to someone before they consider similarity. I take Riolo's model and add a willingness to the agents. If the willingness of the potential donor is greater than the recievers and they are similar, a donation happens. 
;; **

;; @@
(ns tag-mediated-altruism
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/failures</span>","value":"#'tag-mediated-altruism/failures"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/new-population</span>","value":"#'tag-mediated-altruism/new-population"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/maybe-donate</span>","value":"#'tag-mediated-altruism/maybe-donate"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/one-round-donations</span>","value":"#'tag-mediated-altruism/one-round-donations"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/all-donations</span>","value":"#'tag-mediated-altruism/all-donations"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/noise</span>","value":"#'tag-mediated-altruism/noise"}
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
                                    tag-mutated)] ;else return tag-mutated unchanged
            (assoc tolerance-mutated :score 0)))
        population))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/reproduce</span>","value":"#'tag-mediated-altruism/reproduce"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/run-simulation</span>","value":"#'tag-mediated-altruism/run-simulation"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/variance</span>","value":"#'tag-mediated-altruism/variance"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/run-simulation-with-willingness-deviation</span>","value":"#'tag-mediated-altruism/run-simulation-with-willingness-deviation"}
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
;;; {"type":"html","content":"<span class='clj-double'>4.846110284421496E-5</span>","value":"4.846110284421496E-5"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/run-simulation-with-tag-deviation</span>","value":"#'tag-mediated-altruism/run-simulation-with-tag-deviation"}
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
;;; {"type":"html","content":"<span class='clj-double'>0.2507825114522635</span>","value":"0.2507825114522635"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tag-mediated-altruism/run-simulation-with-tolerance-deviation</span>","value":"#'tag-mediated-altruism/run-simulation-with-tolerance-deviation"}
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
;;; {"type":"html","content":"<span class='clj-double'>0.01824618039322999</span>","value":"0.01824618039322999"}
;; <=

;; @@
;After running tests to find the deviation for the tolerance, tag, and willingness of individuals over 30,000 generations, it seems that the individuals with a low willingness had the highest scores. Because of this, the population evolved from having a a donation rate of over 90% to between 0 and 12% over just a few generations. This suggests that populations will not evolve to act altruistically, and that Riolo's model made a terrible assumption. But, I also did not mutate the willingness of individuals here, so I am in the other script in these files.
;; @@
