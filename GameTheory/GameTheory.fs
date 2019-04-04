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

                if leafNode then None, heuristic game currentPlayer 
                else None, heuristic game currentPlayer 
                //if turn = perspective then (None, heuristic game turn) //maximise 
                     //let score = heuristic game turn 
                     //let move = moveGenerator game 

                
                //else // minimise 

                // If game over 
                    // Break? 
                // If maximising player (check current player against perspective)
                    // For each child node -- take max score 
                    // Return value 
                // Else minimising player 
                    // For each child node -- take min score 
                    // Return value 
                

                raise (System.NotImplementedException("MiniMax"))

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) 
            (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) 
            (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search 
            //tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()

                raise (System.NotImplementedException("Alpha Beta Pruning"))
            NodeCounter.Reset()
            MiniMax
