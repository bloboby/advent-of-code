defmodule Day09 do
  require Integer

  defp parse(line) do
    line |> String.split(",") |> Enum.map(&String.to_integer/1)
  end

  defp area({[a, b], [c, d]}) do
    (abs(c - a) + 1) * (abs(d - b) + 1)
  end

  defp interior_intersects_edge?({[a, b], [c, d]}, edges) do
    Enum.any?(edges, fn {[e, f], [g, h]} ->
      crosses_x = min(e, g) < max(a, c) and max(e, g) > min(a, c)
      crosses_y = min(f, h) < max(b, d) and max(f, h) > min(b, d)
      crosses_x and crosses_y
    end)
  end

  defp midpoint_inside_polygon?({[a, b], [c, d]}, edges) do
    {x_mid, y_mid} = {(a + c) / 2, (b + d) / 2}

    on_edge =
      Enum.any?(edges, fn {[e, f], [g, h]} ->
        cond do
          e == x_mid and g == x_mid and min(f, h) <= y_mid and y_mid <= max(f, h) -> true
          f == y_mid and h == y_mid and min(e, g) <= x_mid and x_mid <= max(e, g) -> true
          true -> false
        end
      end)

    odd_crossings_from_infinity =
      edges
      |> Enum.filter(fn {[e, f], [g, h]} ->
        f == h and min(e, g) <= x_mid and max(e, g) >= x_mid
      end)
      |> Enum.count(fn {[_, y], _} -> y < y_mid end)
      |> Integer.is_odd()

    on_edge or odd_crossings_from_infinity
  end

  def part1(contents) do
    tiles = contents |> String.split("\n") |> Enum.map(&parse/1)

    for(a <- tiles, b <- tiles, a < b, do: {a, b})
    |> Enum.map(&area/1)
    |> Enum.max()
  end

  def part2(contents) do
    tiles = [t | ts] = contents |> String.split("\n") |> Enum.map(&parse/1)
    edges = Enum.zip([tiles, ts ++ [t]])

    for(a <- tiles, b <- tiles, a < b, do: {a, b})
    |> Enum.reject(&interior_intersects_edge?(&1, edges))
    |> Enum.filter(&midpoint_inside_polygon?(&1, edges))
    |> Enum.map(&area/1)
    |> Enum.max()
  end
end
