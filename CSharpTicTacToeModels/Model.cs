using System;
using System.Linq; 
using System.Collections.Generic;

namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.Cross; 
        public Player Nought => Player.Nought;
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }

        public Game GameStart(Player first, int size)
        {
            Game game = new Game
            {
                Size = size,
                Turn = first,
                Board = new Dictionary<Move,Player>()
            };

            return game;
        }

        public Player NextPlayer(Player currentPlayer)
        {
            if (currentPlayer == Player.Cross) {
                return Player.Nought;
            } else {
                return Player.Cross;
            }
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

        public Game ApplyMove(Game game, Move move)
        {
            Player currentPlayer = game.Turn;
            Player nextPlayer = NextPlayer(currentPlayer);

            // Add the new Move to the Board 
            game.Board.Add(move, currentPlayer);

            // Change players 
            game.Turn = nextPlayer;

            return game;
        }

        public List<List<Tuple<int, int>>> Lines(int size)
        {
            // List containing all lines 
            List<List<Tuple<int, int>>> lines = new List<List<Tuple<int, int>>>();

            // Temporary lists used to create different lines types 
            List<Tuple<int, int>> horizontal = new List<Tuple<int, int>>();
            List<Tuple<int, int>> vertical = new List<Tuple<int, int>>();
            List<Tuple<int, int>> diagonalLeftRight = new List<Tuple<int, int>>();
            List<Tuple<int, int>> diagonalRightLeft = new List<Tuple<int, int>>();

            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < size; j++)
                {
                    // Add horizontal lines
                    horizontal.Add(Tuple.Create(i, j));

                    // Add vertical lines 
                    vertical.Add(Tuple.Create(j, i));

                    // Add diagonal lines from left to right
                    if (i == j)
                    {
                        diagonalLeftRight.Add(Tuple.Create(i, j));
                    }

                    // Add diagonal lines from right to left 
                    if (i + j == size - 1)
                    {
                        diagonalRightLeft.Add(Tuple.Create(i, j));
                    }
                }

                // Add horizontal lines to master list after each inner loop
                lines.Add(new List<Tuple<int, int>>(horizontal));
                lines.Add(new List<Tuple<int, int>>(vertical));


                // Clear temporary lists 
                horizontal.Clear();
                vertical.Clear();
            }

            // Add diagonal lines to master list after final outer loop execution  
            lines.Add(new List<Tuple<int, int>>(diagonalRightLeft));
            lines.Add(new List<Tuple<int, int>>(diagonalLeftRight));

            // Return list of all lines
            return lines;
        }

        public TicTacToeOutcome<Player> CheckLine(Game game, List<Tuple<int, int>> line)
        {
            // Create a list of player pieces 
            List<Player> pieces = new List<Player>();
            foreach ((int row, int col) in line)
            {
                Move move = CreateMove(row, col); 

                if(game.Board.TryGetValue(move, out Player player)) 
                {
                    pieces.Add(player); 
                } 
            }

            // Determine line outcome 
            if (pieces.TrueForAll(piece => piece.Equals(Player.Cross)))
            {
                return TicTacToeOutcome<Player>.Win.NewWin(Cross, line);
            }

            if (pieces.TrueForAll(piece => piece.Equals(Player.Nought)))
            {
                return TicTacToeOutcome<Player>.Win.NewWin(Nought, line);
            }

            // If both players have a piece in the line, the line cannot be won
            if (pieces.Contains(Player.Cross) && pieces.Contains(Player.Nought))
            {
                return TicTacToeOutcome<Player>.Draw;
            }

            // In any other situation the line is still undecided 
            return TicTacToeOutcome<Player>.Undecided;
        }


        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            List<List<Tuple<int, int>>> lines = Lines(game.Size);
            List<TicTacToeOutcome<Player>> outcomes = new List<TicTacToeOutcome<Player>>(); 

            foreach (List<Tuple<int, int>> line in lines)
            {
                TicTacToeOutcome<Player> outcome = CheckLine(game, line);
                outcomes.Add(outcome); 
            }

            if (outcomes.TrueForAll(outcome => outcome == TicTacToeOutcome<Player>.Draw)) 
            {
                return TicTacToeOutcome<Player>.Draw; 
            }  

            if(outcomes.Contains(TicTacToeOutcome<Player>.Undecided)) 
            {
                return TicTacToeOutcome<Player>.Undecided; 
            } 

            else 
            {
                return outcomes.Find(outcome => outcome.GetType() == typeof(TicTacToeOutcome<Player>.Win)); 
            }
        }

        public int Heuristic(Game game, Player player)
        {
            TicTacToeOutcome<Player> outcome = GameOutcome(game);

            var win = outcome as TicTacToeOutcome<Player>.Win;

            if (win.winner == player)
            {
                return 1;
            } 

            if (win.winner != player) 
            {
                return -1;
            }

            else 
            {
                return 0; 
            }
        }

        public bool GameOver(Game game)
        {
            TicTacToeOutcome<Player> outcome = GameOutcome(game);

            if (outcome == TicTacToeOutcome<Player>.Undecided) 
            {
                return false;
            }

            else {
                return true;
            }
        }

        public List<Move> MoveGenerator(Game game)
        {
            // List of remaining possible moves to be returned 
            List<Move> moves = new List<Move>();

            // Number of rows and columns to loop through  
            int boardSize = game.Size - 1; 

            // Check each board coord for a piece and reutrn any empty squares 
            for(int row=0; row < boardSize; row++)
            {
                for (int col = 0; col < boardSize; col++)
                {
                    // Create a Move object 
                    Move move = CreateMove(row, col); 

                    if(game.Board.TryGetValue(move, out Player player) == false) 
                    {
                        moves.Add(move);
                    }
                }
            }

            return moves; 
        }

        public (Move, int) MiniMax(Game game, Player perspective)
        {
            // Check if the game is over 
            bool over = GameOver(game); 
            if(over)
            {
                return (null, Heuristic(game, perspective)); 
            } 

            // Generate all possible game outcomes 
            List<Move> moves = MoveGenerator(game);
            List<Game> games = new List<Game>();
            List<(Move, int)> gameOutcomes = new List<(Move, int)>(); 

            foreach (Move outcome in moves) {
                games.Add(ApplyMove(game, outcome));
            }

            foreach (Game gameOptions in games)
            {
                gameOutcomes.Add(MiniMax(gameOptions, perspective));
            }

            gameOutcomes.OrderByDescending(score => score.Item2).ToList();

            if (game.Turn == perspective)
            {
                return gameOutcomes.First(); 
            }
            else 
            {
                return gameOutcomes.Last();
            }
        }

        public Move FindBestMove(Game game)
        {
            //throw new System.NotImplementedException("FindBestMove");
            (Move move, int score) = Minimax(game, game.Turn);

            return move; 
        }
    }
}