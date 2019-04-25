namespace QUT

    module FSharpImpureTicTacToeModel =

        type Player = Nought | Cross  

        type Move = 
            { row: int; col: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        type GameState = 
            { mutable turn: Player; size: int; board: System.Collections.Generic.Dictionary<Move, Player> } 
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = 
                    let move = {row = row; col = col} 
                    let value = this.board.TryGetValue move
                    match value with 
                    | true, Player.Cross -> "X"
                    | true, Player.Nought -> "O"
                    | false, _ -> ""

        let CreateMove row col = 
            { Move.row = row; Move.col = col }

        let ChangePlayerFrom player = 
            match player with 
            | Nought -> Cross
            | Cross -> Nought

        let ApplyMove game move = 
            // Update the current player 
            let currentPlayer = game.turn 
            let nextPlayer = ChangePlayerFrom currentPlayer
            game.turn <- nextPlayer

            // Add the move to the board dictionary 
            let currentBoard = game.board
            game.board.Add (move,currentPlayer)

            // Return the game state 
            game

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        let Lines (size: int) : seq<seq<int*int>> = 
            // Create vertical lines 
            let vertical = 
                seq { 
                    for col in 0..size-1 do 
                        yield seq {
                            for row in 0..size-1 do 
                                yield row, col } }

            // Create horizontal lines 
            let horizontal = 
                seq { 
                    for row in 0..size-1 do
                        yield seq { 
                            for col in 0..size-1 do 
                                yield row, col } }

            // Create diagonal lines 
            let diagonal = 
                seq { 
                    yield seq { 
                        for row in 0..size-1 do
                            for col in 0..size-1 do 
                                if row + col = size-1 then yield row, col } 
                    yield seq { 
                        for row in 0..size-1 do
                            for col in 0..size-1 do 
                                  if row = col then yield row, col } }

            //Concat 
            Seq.concat [vertical; horizontal; diagonal]

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game: GameState) (line: seq<int*int>) : TicTacToeOutcome<Player> = 
            // Create sequence of pieces from sequence of coords  
            let pieces = 
                line 
                // Create a Move object using coordinates in tuple 
                |> Seq.map (fun (row,col) -> CreateMove row col)
                // Use move object to determine pieces on board 
                |> Seq.map (fun move -> game.board.TryGetValue move)
                // Ignore the Boolean value, just return players 
                |> Seq.map (fun (result,player) -> if result then Some player else None)
                //|> Seq.map (fun (result,player) -> player)

            // Check if line contains all Cross pieces and return Cross as winner  
            if Seq.forall (fun piece -> piece = Some Cross) pieces then TicTacToeOutcome.Win(Cross, line) else 
            // Check if line contains all Nought pieces and return Nought as winner
            if Seq.forall (fun piece -> piece = Some Nought) pieces then TicTacToeOutcome.Win(Nought, line) else 
            // Check for both pieces in the line 
            if Seq.contains (Some Cross) pieces && Seq.contains (Some Nought) pieces then TicTacToeOutcome.Draw else 
            // Any other outcome means the game is still undecided for this line 
            TicTacToeOutcome.Undecided

        let GameOutcome game = 
            // Generate all lines line game 
            let size = game.size 
            let lines = Lines size 

            // Return most important outcome - Win takes precedence over Undecided, Undecided over win
            let scorer = function 
            | Win(_,_) -> 2 
            | Undecided -> 1
            | Draw -> 0 

            // Check outcome for each line 
            lines 
            |> Seq.map (fun line -> CheckLine game line) 
            |> Seq.maxBy scorer 

        // MiniMax helper functions 
        let Heuristic game player = 
            let outcome = GameOutcome game 
            match outcome with 
            | Win(winner, _) when winner = player -> 1
            | Win(winner, _) when winner <> player -> -1
            | _ -> 0

        let GetTurn game = 
            game.turn

        let GameOver game = 
            let outcome = GameOutcome game
            match outcome with 
            | TicTacToeOutcome.Undecided -> false 
            | _ -> true 

        let MoveGenerator game =  
            let size = game.size-1 
            // Generate a sequence of all board coords 
            seq { 
                for row in 0..size do 
                    for col in 0..size do 
                        yield (row, col) }

            // Return all empty cells to create a list of possible moves remaining
            |> Seq.map (fun (row,col) -> CreateMove row col)
            |> Seq.filter (fun move -> game.board.ContainsKey(move) |> not)

        let MiniMax = 
            GameTheory.MiniMaxGenerator Heuristic GetTurn GameOver MoveGenerator ApplyMove

        let FindBestMove game = 
            NodeCounter.Reset()
            let (bestMove,bestScore) = MiniMax game game.turn
            match bestMove with 
            | Some move -> move 
            | None -> raise (System.Exception("Game is over."))

        let GameStart first size = 
           { turn = first; size = size; board = new System.Collections.Generic.Dictionary<Move, Player>() }

        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game move
                member this.FindBestMove(game)           = FindBestMove game