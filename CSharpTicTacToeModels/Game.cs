using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public int Size => Size;
        public Player Turn => Turn; 
        public string getPiece(int row, int col)
        {
            throw new System.NotImplementedException("getPiece");
        }
    }
}