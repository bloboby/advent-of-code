defmodule Day04 do
  defp parse_to_rolls(contents) do
    contents
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, row} ->
      line
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.map(fn {char, col} -> {{row, col}, char} end)
    end)
    |> Enum.filter(fn {_pos, char} -> char == ?@ end)
    |> MapSet.new(fn {pos, _char} -> pos end)
  end

  defp accessible?({x, y}, rolls) do
    Enum.count(
      [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}],
      fn {dx, dy} -> MapSet.member?(rolls, {x + dx, y + dy}) end
    ) < 4
  end

  defp remove(rolls) do
    new_rolls = MapSet.reject(rolls, &accessible?(&1, rolls))
    if rolls == new_rolls, do: rolls, else: remove(new_rolls)
  end

  def part1(contents) do
    rolls = parse_to_rolls(contents)
    Enum.count(rolls, &accessible?(&1, rolls))
  end

  def part2(contents) do
    rolls = parse_to_rolls(contents)
    removed_rolls = remove(rolls)
    Enum.count(rolls) - Enum.count(removed_rolls)
  end
end
