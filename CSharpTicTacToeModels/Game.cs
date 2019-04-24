using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public int Size { get => Size; set => this.Size = Size; }
        public Player Turn { get => Turn; set => this.Turn = Turn; }
        public Dictionary<Move, Player> Board { get => Board; set => this.Board = Board; }
        public string getPiece(int row, int col)
        {
            // Create Move object for use as dictionary key 
            Move move = new Move
            {
                Row = row,
                Col = col
            };

            // Try to find player at given Move object 
            if (Board.TryGetValue(move, out Player player)) 
            {
                // Return X or O, depending on player found 
                if (player == Player.Cross)
                {
                    return "X";
                } if (player == Player.Nought)
                {
                    return "O";
                }
            // If false, return empty string 
            } else 
            {
                return "";
            }

            // Final path returns empty string 
            return ""; 
        }
    }
}