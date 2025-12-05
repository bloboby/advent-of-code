defmodule Day02b do
  # This is a faster alternative solution.
  # For each k <= #digits, use a closed-form expression to get the sum of all
  # IDs that repeat k times. Then use inclusion-exclusion on k to get the final sum.
  # O(d) where d = #digits.

  defp parse(contents) do
    contents
    |> String.split(",")
    |> Enum.map(fn x -> x |> String.split("-") end)
  end

  defp next(n, k) do
    d = byte_size(n)
    key = binary_part(n, 0, div(d, k))

    cond do
      rem(d, k) != 0 -> 10 ** div(d, k)
      String.duplicate(key, k) >= n -> String.to_integer(key)
      true -> String.to_integer(key) + 1
    end
  end

  defp prev(n, k) do
    d = byte_size(n)
    key = binary_part(n, 0, div(d, k))

    cond do
      rem(d, k) != 0 -> 10 ** div(d, k) - 1
      String.duplicate(key, k) <= n -> String.to_integer(key)
      true -> String.to_integer(key) - 1
    end
  end

  defp num_digits(n) do
    :math.log10(n + 1) |> ceil()
  end

  defp triangle(n) do
    div(n * (n - 1), 2)
  end

  defp sum_repeats([lo, hi], k) do
    {a, b} = {next(lo, k), prev(hi, k)}
    d = num_digits(a)

    cond do
      b < a ->
        0

      num_digits(b) != d ->
        :not_implemented

      true ->
        coeff = Enum.sum(for pow <- 0..(k - 1), do: 10 ** (pow * d))
        coeff * (triangle(b + 1) - triangle(a))
    end
  end

  defp sum_repeats(range = [_lo, hi]) do
    if byte_size(hi) <= 10 do
      Enum.sum_by([2, 3, 5, 7], &sum_repeats(range, &1)) -
        Enum.sum_by([6, 10], &sum_repeats(range, &1))
    else
      :not_implemented
    end
  end

  def part1(contents) do
    contents |> parse() |> Enum.sum_by(&sum_repeats(&1, 2))
  end

  def part2(contents) do
    contents |> parse() |> Enum.sum_by(&sum_repeats(&1))
  end
end
