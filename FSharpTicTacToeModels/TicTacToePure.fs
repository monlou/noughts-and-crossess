namespace QUT

    module FSharpPureTicTacToeModel =
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { row: int; col: int } 
            interface ITicTacToeMove with
                member this.Row with get() = this.row 
                member this.Col with get() = this.col 

        // type to represent the current state of the game, including the size of the game (NxN), 
        // whose turn it is and the pieces on the board
        type GameState = 
            { turn: Player; size: int; board: Map<Move, Player> }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size 
                member this.getPiece(row, col) = 
                    let move = {row = row; col = col} 
                    let cell = Map.tryFind move this.board
                    match cell with 
                    | Some Player.Cross -> "X"
                    | Some Player.Nought -> "O"
                    | None -> ""

        let CreateMove row col = 
            { Move.row = row; Move.col = col }

        let ApplyMove (oldState: GameState) (move: Move) = 
            let currentPlayer = oldState.turn  
            let nextPlayer = if currentPlayer = Nought then Cross else Nought
            let oldBoard = oldState.board
            let newBoard = oldBoard.Add(move, currentPlayer)
            { oldState with turn = nextPlayer; board = newBoard } 

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        let Lines (size: int) : seq<seq<int*int>> = 
            seq { 
                // Create vertical lines 
                for col=0 to size-1 do 
                    yield seq {
                        for row in 0..size-1 do 
                            yield row, col } 

                // Create horizontal lines 
                for row in 0..size-1 do
                    yield seq { 
                        for col in 0..size-1 do 
                            yield row, col } 

                // Create diagonal line from top left to bottom right  
                yield seq { 
                    for row in 0..size-1 do
                        for col in 0..size-1 do 
                            if row = col then yield row, col } 
                // Create diagonal line from top right to bottom left 
                yield seq { 
                    for row in 0..size-1 do
                        for col in 0..size-1 do 
                              if row + col = size-1 then yield row, col } }

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game: GameState) (line: seq<int*int>) : TicTacToeOutcome<Player> = 
            // Create sequence of pieces from sequence of coords  
            let pieces = 
                line 
                // Create a Move object using coordinates in tuple 
                |> Seq.map (fun (row,col) -> CreateMove row col)
                // Use move object to determine pieces on board 
                |> Seq.map (fun move -> game.board.TryFind move)

            // Check if line contains all Cross pieces and return Cross as winner  
            if Seq.forall (fun piece -> piece = Some Cross) pieces then Win(Cross, line) else 
            // Check if line contains all Nought pieces and return Nought as winner
            if Seq.forall (fun piece -> piece = Some Nought) pieces then Win(Nought, line) else 
            // Check for both pieces in the line 
            if Seq.contains (Some Cross) pieces && Seq.contains (Some Nought) pieces then Draw else 
            // Any other outcome means the game is still undecided for this line 
            Undecided

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

        let GameStart firstPlayer size = 
            { GameState.turn = firstPlayer; GameState.size = size; GameState.board = Map.empty }

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
                   
        // Game AI 
        let MiniMax = 
            GameTheory.MiniMaxGenerator Heuristic GetTurn GameOver MoveGenerator ApplyMove 

        let MiniMaxWithPruning = 
            GameTheory.MiniMaxWithAlphaBetaPruningGenerator Heuristic GetTurn GameOver MoveGenerator ApplyMove

        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = 
                NodeCounter.Reset()
                let (bestMove,bestScore) = MiniMax game game.turn
                match bestMove with 
                | Some move -> move 
                | None -> raise (System.Exception("Game is over."))

        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = 
                NodeCounter.Reset()
                let alpha = 1
                let beta = -1
                let (bestMove,bestScore) = MiniMaxWithPruning alpha beta game game.turn
                match bestMove with 
                | Some move -> move 
                | None -> raise (System.Exception("Game is over.")) 