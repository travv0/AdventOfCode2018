using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace day10
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            var input = "";
            var lights = new List<Light>();

            while ((input = Console.ReadLine()) != null)
            {
                ParseFromString(input, out Position pos, out Velocity vel);
                lights.Add(new Light { Position = pos, Velocity = vel });
            }

            int i = 0;
            while (true)
            {
                var area = Light.AreaOfLights(lights);
                var newArea = Light.ShiftLights(lights);

                if (area.Item1 < newArea.Item1 && area.Item2 < newArea.Item2)
                {
                    Light.UnshiftLights(lights);
                    break;
                }
                i++;
            }

            Light.PrintLights(lights);
            Console.WriteLine($"It would've taken {i} seconds for the message to form.");
        }

        private static void ParseFromString(string input, out Position pos, out Velocity vel)
        {
            var r = new Regex(@"position=<(.*?),(.*?)> velocity=<(.*?),(.*?)>", RegexOptions.Compiled);
            var match = r.Match(input);
            if (match.Success)
            {
                pos = new Position { X = int.Parse(match.Groups[1].Value), Y = int.Parse(match.Groups[2].Value) };
                vel = new Velocity { Horizonal = int.Parse(match.Groups[3].Value), Vertical = int.Parse(match.Groups[4].Value) };
            }
            else
            {
                throw new FormatException("Invalid input format.  Example: \"position=< 7,  6> velocity=<-1, -1>\"");
            }
        }
    }
}