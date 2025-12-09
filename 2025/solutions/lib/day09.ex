defmodule Day09 do
  defp parse(line) do
    line |> String.split(",") |> Enum.map(&String.to_integer/1)
  end

  defp area({[a, b], [c, d]}) do
    abs(c - a + 1) * abs(d - b + 1)
  end

  def part1(contents) do
    tiles = contents |> String.split("\n") |> Enum.map(&parse/1)
    for(a <- tiles, b <- tiles, a < b, do: {a, b}) |> Enum.map(&area/1) |> Enum.max()
  end
end
