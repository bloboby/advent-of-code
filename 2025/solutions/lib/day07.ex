defmodule Day07 do
  defp parse_to_set(line) do
    line
    |> String.to_charlist()
    |> Enum.with_index()
    |> Enum.filter(fn {c, _idx} -> c != ?. end)
    |> MapSet.new(fn {_c, idx} -> idx end)
  end

  defp split_beams(splitters, {beams, count}) do
    split = fn x ->
      if MapSet.member?(splitters, x), do: [x - 1, x + 1], else: [x]
    end

    count = count + Enum.count(beams, &MapSet.member?(splitters, &1))
    beams = beams |> Enum.flat_map(split) |> Enum.dedup()
    {beams, count}
  end

  defp split_time(splitters, beam_counts) do
    split = fn {x, count} ->
      if MapSet.member?(splitters, x), do: [{x - 1, count}, {x + 1, count}], else: [{x, count}]
    end

    beam_counts
    |> Enum.flat_map(split)
    |> Enum.reduce(%{}, fn {k, v}, acc -> Map.update(acc, k, v, &(&1 + v)) end)
  end

  def part1(contents) do
    [start | splitters] = contents |> String.split("\n") |> Enum.map(&parse_to_set/1)

    splitters
    |> Enum.reduce({start, 0}, &split_beams/2)
    |> elem(1)
  end

  def part2(contents) do
    [start | splitters] = contents |> String.split("\n")
    s = start |> String.to_charlist() |> Enum.find_index(&(&1 == ?S))

    splitters
    |> Enum.map(&parse_to_set/1)
    |> Enum.reduce(%{s => 1}, &split_time/2)
    |> Map.values()
    |> Enum.sum()
  end
end
