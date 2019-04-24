namespace QUT.CSharpTicTacToe
{
    public class Move : ITicTacToeMove
    {
        public int Row { get => Row; set => this.Row = Row; }
        public int Col { get => Col; set => this.Col = Col; }
    }
}
