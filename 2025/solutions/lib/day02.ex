defmodule Day02 do
  defp parse(contents) do
    contents
    |> String.split(",")
    |> Enum.map(fn x -> x |> String.split("-") |> Enum.map(&String.to_integer/1) end)
  end

  defp num_digits(n) do
    :math.log10(n + 1) |> ceil()
  end

  defp repeat(x, 1), do: x
  defp repeat(x, k), do: x <> repeat(x, k - 1)

  defp repeats?(n, k) do
    len = num_digits(n)

    if rem(len, k) != 0 do
      false
    else
      num = Integer.to_string(n)
      repeated = binary_part(num, 0, div(len, k)) |> repeat(k)
      num == repeated
    end
  end

  defp repeats?(n) when n < 10, do: false
  defp repeats?(n), do: Enum.any?(for k <- 2..num_digits(n), do: repeats?(n, k))

  defp sum_doubles([a, b]) do
    Enum.sum(for n <- a..b, repeats?(n, 2), do: n)
  end

  defp sum_invalid([a, b]) do
    Enum.sum(for n <- a..b, repeats?(n), do: n)
  end

  def part1(contents) do
    contents |> parse() |> Enum.map(&sum_doubles/1) |> Enum.sum()
  end

  def part2(contents) do
    contents |> parse() |> Enum.map(&sum_invalid/1) |> Enum.sum()
  end
end
