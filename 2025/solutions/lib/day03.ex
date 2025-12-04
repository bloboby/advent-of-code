defmodule Day03 do
  defp max_digit(line) do
    line
    |> to_charlist()
    |> Enum.with_index()
    |> Enum.max_by(fn {val, idx} -> {val, -idx} end)
  end

  defp max_joltage(_line, 0), do: 0

  defp max_joltage(line, n) do
    {val, idx} = line |> String.slice(0..-n//1) |> max_digit()
    {_, rest} = String.split_at(line, idx + 1)
    (val - ?0) * 10 ** (n - 1) + max_joltage(rest, n - 1)
  end

  def part1(contents) do
    contents |> String.split("\n") |> Enum.sum_by(&max_joltage(&1, 2))
  end

  def part2(contents) do
    contents |> String.split("\n") |> Enum.sum_by(&max_joltage(&1, 12))
  end
end
