defmodule Utils do
  defp parse(line) do
    line |> String.split() |> Enum.map(&String.to_integer/1)
  end

  def read_2d_ints(filename) do
    {:ok, contents} = File.read("Documents/advent-of-code/2024/inputs/#{filename}.txt")

    contents
    |> String.split("\n", trim: true)
    |> Enum.map(&parse/1)
  end
end