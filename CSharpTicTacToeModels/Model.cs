namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Cross; 
        public Player Nought => Nought;
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }

        public Player NextPlayer(Player currentPlayer)
        {
            if (currentPlayer == Player.Cross) {
                return Player.Nought; 
            }
            return Player.Cross; 
        }

        public Game ApplyMove(Game game, Move move)
        {
            Player currentPlayer = game.Turn;
            Player nextPlayer = NextPlayer(currentPlayer); 

            game.Board.Add(move, currentPlayer);
            game.Turn = nextPlayer;
            return game;
        }

        public Move CreateMove(int row, int col)
        {
            Move move = new Move
            {
                Row = row,
                Col = col
            };

            return move;
        }

        public Move MiniMax(Game game)
        {
            throw new System.NotImplementedException("MiniMax");
        }

        public Move FindBestMove(Game game)
        {
            throw new System.NotImplementedException("FindBestMove");
        }

        //public array<int, int> Lines(int size)
        //{
        //    for(int row=0; row < size; row++) {
        //        for (int col = 0; col < size; col++)
        //        {

        //        }
        //    }
        //}

        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            throw new System.NotImplementedException("GameOutcome");
        }

        public Game GameStart(Player first, int size)
        {
            Game game = new Game
            {
                Size = size,
                Turn = first,
                Board = new System.Collections.Generic.Dictionary<Move, Player>()
            };

            return game; 
        }
    }
}