defmodule Day10 do
  def parse(line) do
    [diagram | words] = line |> String.split(" ")
    [joltage | buttons] = Enum.reverse(words)

    diagram =
      diagram
      |> String.slice(1..-2//1)
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.filter(fn {c, _idx} -> c == ?# end)
      |> Enum.map(fn {_c, idx} -> idx end)
      |> MapSet.new()

    buttons =
      buttons
      |> Enum.map(fn word ->
        word |> String.slice(1..-2//1) |> String.split(",") |> Enum.map(&String.to_integer/1)
      end)

    joltage =
      joltage |> String.slice(1..-2//1) |> String.split(",") |> Enum.map(&String.to_integer/1)

    {diagram, buttons, joltage}
  end

  def combinations([]), do: [[]]

  def combinations([x | xs]) do
    cs = combinations(xs)
    cs ++ Enum.map(cs, &[x | &1])
  end

  def valid?(combination, diagram) do
    result =
      combination
      |> List.flatten()
      |> Enum.frequencies()
      |> Enum.filter(fn {_btn, count} -> rem(count, 2) == 1 end)
      |> Enum.map(fn {btn, _} -> btn end)
      |> MapSet.new()

    result == diagram
  end

  def fewest_presses_for_diagram({diagram, buttons, _}) do
    buttons
    |> combinations()
    |> Enum.filter(&valid?(&1, diagram))
    |> Enum.map(&length/1)
    |> Enum.min()
  end

  def part1(contents) do
    contents
    |> String.split("\n")
    |> Enum.map(&parse/1)
    |> Enum.sum_by(&fewest_presses_for_diagram/1)
  end
end
