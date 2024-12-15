defmodule Utils do
  defp read_contents(filename) do
    {:ok, contents} = File.read("#{__DIR__}/../../inputs/#{filename}.txt")
    contents
  end

  defp parse_ints(line) do
    line |> String.split() |> Enum.map(&String.to_integer/1)
  end

  def read_2d_ints(filename) do
    read_contents(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_ints/1)
  end

  def parse_grid(lines) do
    n = length(lines)
    m = lines |> hd |> String.length()
    oob? = fn {x, y} -> x < 0 or y < 0 or x >= n or y >= m end

    grid =
      lines
      |> Enum.map(&String.to_charlist/1)
      |> Enum.map(&Enum.with_index/1)
      |> Enum.with_index()
      |> Enum.flat_map(fn {row, x} -> Enum.map(row, fn {c, y} -> {{x, y}, c} end) end)

    {grid, oob?}
  end

  def read_grid(filename) do
    filename
    |> read_contents()
    |> String.split("\n", trim: true)
    |> parse_grid()
  end
end
