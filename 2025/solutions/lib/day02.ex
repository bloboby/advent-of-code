defmodule Day02 do
  # Iterate through every ID and check if it is valid.
  # O(n*d) where n = #IDs and d = #digits.

  defp parse(contents) do
    contents
    |> String.split(",")
    |> Enum.map(fn x -> x |> String.split("-") |> Enum.map(&String.to_integer/1) end)
  end

  defp num_digits(n) do
    :math.log10(n + 1) |> ceil()
  end

  defp repeats?(n, k) do
    len = num_digits(n)

    if rem(len, k) != 0 do
      false
    else
      num = Integer.to_string(n)
      repeated = binary_part(num, 0, div(len, k)) |> String.duplicate(k)
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
    contents |> parse() |> Enum.sum_by(&sum_doubles/1)
  end

  def part2(contents) do
    contents |> parse() |> Enum.sum_by(&sum_invalid/1)
  end
end
