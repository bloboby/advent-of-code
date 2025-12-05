defmodule Day05 do
  defp parse(contents) do
    [ranges, ids] = contents |> String.split("\n\n")

    ranges =
      ranges
      |> String.split("\n")
      |> Enum.map(fn range ->
        range
        |> String.split("-")
        |> Enum.map(&String.to_integer/1)
        |> List.to_tuple()
      end)

    ids = ids |> String.split("\n") |> Enum.map(&String.to_integer/1)

    {ranges, ids}
  end

  defp merge_one({a, b}, [{prev_a, prev_b} | acc]) do
    if a <= prev_b + 1 do
      [{prev_a, max(prev_b, b)} | acc]
    else
      [{a, b} | [{prev_a, prev_b} | acc]]
    end
  end

  defp merge([r | ranges]) do
    Enum.reduce(ranges, [r], &merge_one/2)
  end

  def part1(contents) do
    {ranges, ids} = parse(contents)
    ids |> Enum.count(&Enum.any?(ranges, fn {a, b} -> a <= &1 and &1 <= b end))
  end

  def part2(contents) do
    {ranges, _} = parse(contents)
    ranges |> Enum.sort() |> merge() |> Enum.sum_by(fn {a, b} -> b - a + 1 end)
  end
end
