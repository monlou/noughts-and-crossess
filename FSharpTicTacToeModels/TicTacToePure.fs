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

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { turn: Player; size: int; board: Map<Move, Player> }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size 
                member this.getPiece(row, col) = 
                    let cell = Map.tryFind {row = row; col = col} this.board
                    match cell with 
                    | Some Player.Cross -> "X"
                    | Some Player.Nought -> "O"
                    | None -> ""

        let CreateMove row col = //raise (System.NotImplementedException("CreateMove"))
            { Move.row = row; Move.col = col }

        let ApplyMove (oldState:GameState) (move: Move) = //raise (System.NotImplementedException("CreateMove"))
            let currentPlayer = oldState.turn          
            let oldBoard = oldState.board
            let newBoard = oldBoard.Add(move, currentPlayer)
            { oldState with board = newBoard } 

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
        let Lines (size: int) : seq<seq<int*int>> = //raise (System.NotImplementedException("Lines"))
            // Create vertical lines 
            let vertical: seq<seq<int*int>> = 
                seq { for col in 0..size-1 do 
                    seq { for row in 0..size-1 
                        do yield row, col } }

            // Create horizontal lines 
            let horizontal: seq<seq<int*int>> = 
                seq { for row in 0..size-1 do
                    seq { for col in 0..size-1 
                        do yield row, col } }

            // Create diagonal lines 
            let diagonal: seq<seq<int*int>> = 
                seq { for row in 0..size-1 do
                    seq { for col in 0..size-1 do 
                        if row = col then yield row, col else 
                        if row + col = size then yield row, col } }

            //Concat 
            Seq.concat [vertical; horizontal; diagonal]

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game: GameState) (line: seq<int*int>) : TicTacToeOutcome<Player> = raise (System.NotImplementedException("CheckLine"))
            //let pieces = 
                //seq { for cell in line do yield game.getPiece(cell.row, cell.col) }
            //for pieces in cell do 
                //if pieces = "" then TicTacToeOutcome.Undecided else 
                //if pieces are all equal tokens 
                
                // If all cells are filled, 
                // Check if all pieces are the same 
                    // If yes, return winning piece 
                    // Else return Draw 
                // Else return undetermined 





            //let rec check = 

            //for piece in pieces-1 do 
                //if piece != piece+1 then 
                    //TicTacToeOutcome.Draw
                //else if piece = piece+1 then 
                     

        let GameOutcome game = raise (System.NotImplementedException("GameOutcome"))
            //let size = game.size 
            //let lines = Lines size 
            //for line in lines do 
                //let outcome = CheckLine game line
                //if player returned, game over 
                //{ game, game.turn, outcome }     

        let GameStart (firstPlayer:Player) size = //raise (System.NotImplementedException("GameStart"))
            { GameState.turn = firstPlayer; GameState.size = size; GameState.board = Map.empty }

        // Helper functions 
        let heuristic game player = 
            raise (System.NotImplementedException("heuristic"))

        let getTurn game = 
            game.turn

        let gameOver game = 
            GameOutcome game

        let moveGenerator game = raise (System.NotImplementedException(" move"))
            //seq { for move in game.board do
                    //if game.getPiece(move) = "" then yield move }

        let applyMove game = 
            raise (System.NotImplementedException("apply move"))

        // Game AI 
        let MiniMax game = //raise (System.NotImplementedException("MiniMax")
            GameTheory.MiniMaxGenerator heuristic getTurn gameOver moveGenerator applyMove 

        let MiniMaxWithPruning game = //raise (System.NotImplementedException("MiniMaxWithPruning"))
            GameTheory.MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver moveGenerator applyMove 
            

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
            override this.FindBestMove(game) = raise (System.NotImplementedException("FindBestMove"))


        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = raise (System.NotImplementedException("FindBestMove"))