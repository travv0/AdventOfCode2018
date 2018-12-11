using System;
using System.Collections.Generic;
using System.Text;

namespace day10a
{
    internal class Position
    {
        public int X { get; set; }
        public int Y { get; set; }

        public int DistanceToPosition(Position point)
        {
            return Math.Abs(this.X - point.X) + Math.Abs(this.Y - point.Y);
        }
    }
}