(defn makeBoard [] '((0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0)))

(defn switchPlayer [player]
    (+ 1 (mod player 2)))

(defn elem [board lin col]
    (nth (nth board lin) col))

(defn getRight [board lin col]
    (list
    (elem board lin col)
    (elem board lin (+ col 1))
    (elem board lin (+ col 2))
    (elem board lin (+ col 3))))

(defn getDown [board lin col]
    (list 
    (elem board lin col)
    (elem board (+ lin 1) col)
    (elem board (+ lin 2) col)
    (elem board (+ lin 3) col)))

(defn getDownRight [board lin col]
    (list
    (elem board lin col)
    (elem board (+ lin 1) (+ col 1))
    (elem board (+ lin 2) (+ col 2))
    (elem board (+ lin 3) (+ col 3))))

(defn getUpRight [board lin col]
    (list
    (elem board lin col)
    (elem board (- lin 1) (+ col 1))
    (elem board (- lin 2) (+ col 2))
    (elem board (- lin 3) (+ col 3))))

(defn equals [player list]
    (apply #(= player %1 %2 %3 %4) list))

(defn checkRight [player board]
    (loop [lin 0 col 0]
        (cond
           (>= lin (count board))
           false

           (>= col (- (count (first board)) 3))
           (recur (inc lin) 0) 

           (equals player (getRight board lin col))
           true

           :else
           (recur lin (inc col)))))

(defn checkRight [player board]
    (loop [lin 0 col 0]
        (cond
           (>= lin (count board))
           false

           (>= col (- (count (first board)) 3))
           (recur (inc lin) 0) 

           (equals player (getRight board lin col))
           true

           :else
           (recur lin (inc col)))))

(defn checkDown [player board]
    (loop [lin 0 col 0]
        (cond
           (>= col (count (first board)))
           false

           (>= lin (- (count board) 3))
           (recur 0 (inc col)) 

           (equals player (getDown board lin col))
           true

           :else
           (recur (inc lin) col))))

(defn checkDownRight [player board]
    (loop [lin 0 col 0]
        (cond
           (>= lin (- (count board) 3))
           false

           (>= col (- (count (first board)) 3))
           (recur (inc lin) 0) 

           (equals player (getDownRight board lin col))
           true

           :else
           (recur lin (inc col)))))

(defn checkUpRight [player board]
    (loop [lin (- (count board) 1) col 0]
        (cond
           (< lin 3)
           false

           (>= col (- (count (first board)) 3))
           (recur (dec lin) 0) 

           (equals player (getUpRight board lin col))
           true

           :else
           (recur lin (inc col)))))

(defn match [player board]
    (apply #(or %1 %2 %3 %4)
        (list
        (checkRight player board)
        (checkDown player board)
        (checkDownRight player board)
        (checkUpRight player board))))

(defn printBoard [board]
    (loop [lin 0]
    (println (nth board lin))
    (if (< lin (dec (count board)))
        (recur (inc lin)))))

(defn verificaEmpate [board]
    (= (* (count board) (count (first board)))
        (loop [lin 0 col 0 aux 0]
            (cond 
                (= lin (count board))
                aux

                (= col (count (first board)))
                (recur (inc lin) 0 aux)

                (= 0 (elem board lin col))
                (recur lin (inc col) aux)

                :else
                (recur lin (inc col) (inc aux))))))

(defn placeInLine [line player col]
    (loop [aux 0 return '()]
        (cond
            (>= aux (count line))
            (reverse return)

            (= aux col)
            (recur (inc aux) (conj return player))

            :else
            (recur (inc aux) (conj return (nth line aux))))))


(defn placeToken [player board]
    (let [col (dec (read-string (str (read))))]
        (if (and (>= col 0) (< col (count (first board))))
            (loop [lin (dec (count board)) return '() notPlaced true]
                (cond
                    (< lin 0)
                    return

                    (and (zero? (elem board lin col)) notPlaced)
                    (recur (dec lin) (conj return (placeInLine (nth board lin) player col)) false)

                    :else
                    (recur (dec lin) (conj return (nth board lin)) notPlaced))))))

(defn addScore [winner score]
    (cond
        (= winner 0)
        (update score :draw inc)

        (= winner 1)
        (update score :score1 inc)

        (= winner 2)
        (update score :score2 inc)))

(defn printScore [winner score]
    (if (= winner 0)
        (println "Empate")
        (println (str "Jogador " winner " vence")))

    (println (str "Jogador 1: " (:score1 score) " vitórias"))
    (println (str "Jogador 2: " (:score2 score) " vitórias"))
    (println (str "empates: " (:draw score))))

(defn turn []
    (printBoard (makeBoard))
    (loop [player 1 board (makeBoard)]
        (let [newBoard (placeToken player board)]
            (printBoard newBoard)
            (cond
                (match player newBoard)
                player

                (verificaEmpate newBoard)
                0

                :else
                (recur (switchPlayer player) newBoard)))))

(defn newGame []
    (println "Jogar novamente?")
    (= (str (read)) "s"))

(defn game []
    (loop [score {:score1 0 :score2 0 :draw 0}]
        (let [winner (turn) newScore (addScore winner score)]
            (printScore winner newScore)
            (if (newGame)
                (recur newScore)))))

(game)
