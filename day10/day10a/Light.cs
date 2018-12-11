using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace day10a
{
    internal class Light
    {
        public Position Position { get; set; }
        public Velocity Velocity { get; set; }

        public static Tuple<int, int> AreaOfLights(List<Light> lights)
        {
            var minX = lights.Min(light => light.Position.X);
            var minY = lights.Min(light => light.Position.Y);
            var maxX = lights.Max(light => light.Position.X);
            var maxY = lights.Max(light => light.Position.Y);

            return new Tuple<int, int>(maxX - minX, maxY - minY);
        }

        public static Tuple<int, int> ShiftLights(List<Light> lights)
        {
            foreach (var light in lights)
            {
                light.Position.X += light.Velocity.Horizonal;
                light.Position.Y += light.Velocity.Vertical;
            }

            return AreaOfLights(lights);
        }

        public static void UnshiftLights(List<Light> lights)
        {
            foreach (var light in lights)
            {
                light.Position.X -= light.Velocity.Horizonal;
                light.Position.Y -= light.Velocity.Vertical;
            }
        }

        public static void PrintLights(List<Light> lights)
        {
            var xOffset = lights.Min(light => light.Position.X);
            var yOffset = lights.Min(light => light.Position.Y);
            var width = lights.Max(light => light.Position.X) - xOffset + 1;
            var height = lights.Max(light => light.Position.Y) - yOffset + 1;

            char[,] sky = new char[width, height];
            for (int y = 0; y < height; y++)
            {
                for (int x = 0; x < width; x++)
                {
                    sky[x, y] = '.';
                }
            }

            foreach (var light in lights)
            {
                sky[light.Position.X - xOffset, light.Position.Y - yOffset] = '#';
            }

            for (int y = 0; y < height; y++)
            {
                for (int x = 0; x < width; x++)
                {
                    Console.Write(sky[x, y]);
                }
                Console.WriteLine();
            }
        }
    }
}