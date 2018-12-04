using System;
using System.Linq;
using System.Text.RegularExpressions;

namespace day3a
{
    internal class Program
    {
        private static readonly int _clothSize = 2000;

        private static void Main(string[] args)
        {
            var input = "";
            int[,] cloth = new int[_clothSize, _clothSize];
            var overlaps = 0;

            while ((input = Console.ReadLine()) != null)
            {
                ParseFromString(input, out int x, out int y, out int width, out int height);
                foreach (var i in Enumerable.Range(x, width))
                {
                    foreach (var j in Enumerable.Range(y, height))
                    {
                        if (++cloth[i, j] == 2)
                        {
                            overlaps++;
                        }
                    }
                }
            }

            Console.WriteLine(overlaps);
        }

        private static void ParseFromString(string input, out int x, out int y, out int width, out int height)
        {
            var r = new Regex(@"#\d+ @ (\d+),(\d+): (\d+)x(\d+)", RegexOptions.Compiled);
            var match = r.Match(input);
            if (match.Success)
            {
                x = int.Parse(match.Groups[1].Value);
                y = int.Parse(match.Groups[2].Value);
                width = int.Parse(match.Groups[3].Value);
                height = int.Parse(match.Groups[4].Value);
            }
            else
            {
                throw new FormatException("Invalid input format.  Example: \"#1 @ 912,277: 27x20\"");
            }
        }
    }
}