namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) 
            (gameOver:'Game -> bool) (moveGenerator: 'Game -> seq<'Move>) 
            (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax game perspective =
                NodeCounter.Increment()

                // Check game state
                let leafNode = gameOver game 

                // Check current player 
                let currentPlayer = getTurn game 

                // If the game is over, do not return a Move 
                if leafNode then (None, heuristic game perspective) else 
                    // Generate sequence of possible moves 
                    moveGenerator game
                    // Create new game state for each possible move 
                    |> Seq.map (fun move -> move, applyMove game move)
                    // Run MiniMax for each new state 
                    |> Seq.map (fun (move,outcome) -> Some move, snd (MiniMax outcome perspective))
                    // Return the best move for the maximising player and worst for the minimising player
                    |> if currentPlayer = perspective then Seq.maxBy snd else Seq.minBy snd

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) 
            (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) 
            (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search 
            //tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()

                // Check game state 
                let leafNode = gameOver oldState

                // Check current player 
                let currentPlayer = getTurn oldState

                // If the game is over, do not return a Move 
                if leafNode then (None, heuristic oldState perspective) else 
                    // Generate sequence of possible moves 
                    moveGenerator oldState
                    // Create new game state for each possible move 
                    |> Seq.map (fun move -> move, applyMove oldState move)
                    // Run MiniMax for each new state 
                    |> Seq.map (fun (move,outcome) -> Some move, snd (MiniMax alpha beta outcome perspective))
                    // Return the best move for the maximising player and worst for the minimising player
                    |> if currentPlayer = perspective then Seq.maxBy snd else Seq.minBy snd
                    //|> if currentPlayer = perspective then max(fst alpha) else min (fst beta)
                    // Determine whether or not to purne 
                    //|> if beta <= alpha then break

            NodeCounter.Reset()
            MiniMax
