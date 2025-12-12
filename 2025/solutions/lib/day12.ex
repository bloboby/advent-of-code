defmodule Day12 do
  defp fits_naively?(line, sizes) do
    [_, x, y, counts] = Regex.run(~r/(\d+)x(\d+):((?:\s\d+)+)/, line)
    [x, y] = [x, y] |> Enum.map(&String.to_integer/1)
    counts = counts |> String.split(" ", trim: true) |> Enum.map(&String.to_integer/1)
    total_size = sizes |> Enum.zip_with(counts, &(&1 * &2)) |> Enum.sum()

    cond do
      div(x, 3) * div(y, 3) >= Enum.sum(counts) -> true
      x * y < total_size -> false
    end
  end

  def part1(contents) do
    {presents, [regions]} = contents |> String.split("\n\n") |> Enum.split(-1)
    sizes = presents |> Enum.map(&String.count(&1, "#"))
    regions |> String.split("\n") |> Enum.count(&fits_naively?(&1, sizes))
  end
end
