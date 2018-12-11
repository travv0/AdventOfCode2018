using System;
using System.Linq;
using System.Text.RegularExpressions;

namespace day3b
{
    internal class Program
    {
        private static readonly int _clothSize = 2000;
        private static readonly int _idCountMax = 2000;

        private static void Main(string[] args)
        {
            var input = "";
            int[,] cloth = new int[_clothSize, _clothSize];
            bool[] overlaps = new bool[_idCountMax];
            for (int i = 0; i < overlaps.Length; i++)
            {
                overlaps[i] = true;
            }

            while ((input = Console.ReadLine()) != null)
            {
                var overlap = false;
                ParseFromString(input, out int id, out int x, out int y, out int width, out int height);
                foreach (var i in Enumerable.Range(x, width))
                {
                    foreach (var j in Enumerable.Range(y, height))
                    {
                        if (cloth[i, j] != 0)
                        {
                            overlap = true;
                            overlaps[cloth[i, j]] = true;
                            overlaps[id] = true;
                        }
                        else
                        {
                            cloth[i, j] = id;
                        }
                    }
                }
                if (!overlap)
                {
                    overlaps[id] = false;
                }
            }

            Console.WriteLine("Number of non-overlapping claims: " + overlaps.Count(b => b == false));
            Console.WriteLine("First non-overlapping claim ID: " + Array.IndexOf(overlaps, false));
        }

        private static void ParseFromString(string input, out int id, out int x, out int y, out int width, out int height)
        {
            var r = new Regex(@"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)", RegexOptions.Compiled);
            var match = r.Match(input);
            if (match.Success)
            {
                id = int.Parse(match.Groups[1].Value);
                x = int.Parse(match.Groups[2].Value);
                y = int.Parse(match.Groups[3].Value);
                width = int.Parse(match.Groups[4].Value);
                height = int.Parse(match.Groups[5].Value);
            }
            else
            {
                throw new FormatException("Invalid input format.  Example: \"#1 @ 912,277: 27x20\"");
            }
        }
    }
}