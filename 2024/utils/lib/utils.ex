defmodule Utils do
  defp parse_ints(line) do
    line |> String.split() |> Enum.map(&String.to_integer/1)
  end

  def read_2d_ints(filename) do
    {:ok, contents} = File.read("#{__DIR__}/../../inputs/#{filename}.txt")

    contents
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_ints/1)
  end
end
