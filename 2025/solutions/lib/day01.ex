defmodule Day01 do
  defp parse(contents) do
    contents
    |> String.split("\n")
    |> Enum.map(fn <<op, n::binary>> -> {op, String.to_integer(n)} end)
  end

  defp rotate(pos, op, n) do
    cond do
      op == ?L -> pos - n
      op == ?R -> pos + n
    end
    |> Integer.mod(100)
  end

  defp final_crossing(pos, op, n) do
    cond do
      pos == 0 -> 0
      op == ?L and pos <= n -> 1
      op == ?R and 100 - pos <= n -> 1
      true -> 0
    end
  end

  defp count_final_zeroes({op, n}, {pos, count}) do
    pos = rotate(pos, op, n)
    count = if pos == 0, do: count + 1, else: count
    {pos, count}
  end

  defp count_all_zeroes({op, n}, {pos, count}) do
    count = count + div(n, 100) + final_crossing(pos, op, rem(n, 100))
    pos = rotate(pos, op, n)
    {pos, count}
  end

  def part1(contents) do
    contents |> parse() |> Enum.reduce({50, 0}, &count_final_zeroes/2) |> elem(1)
  end

  def part2(contents) do
    contents |> parse() |> Enum.reduce({50, 0}, &count_all_zeroes/2) |> elem(1)
  end
end
