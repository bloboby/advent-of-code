defmodule Day06 do
  defp parse(strlist) do
    {nums, [op]} = strlist |> Enum.split(-1)
    nums = nums |> Enum.map(&String.to_integer/1)
    {nums, op}
  end

  defp emit_on_space(col, acc) do
    if Enum.all?(col, &(&1 == ?\s)) do
      {:cont, acc, []}
    else
      {:cont, [col | acc]}
    end
  end

  defp parse_chunks(chunks) do
    {nums, op} = chunks |> List.to_string() |> String.split_at(-1)
    nums = nums |> String.split(" ", trim: true) |> Enum.map(&String.to_integer/1)
    {nums, op}
  end

  defp compute({nums, op}) do
    case op do
      "*" -> Enum.product(nums)
      "+" -> Enum.sum(nums)
    end
  end

  def part1(contents) do
    contents
    |> String.split("\n")
    |> Enum.map(&String.split(&1, " ", trim: true))
    |> Enum.zip_with(& &1)
    |> Enum.map(&parse/1)
    |> Enum.sum_by(&compute/1)
  end

  def part2(contents) do
    contents
    |> String.split("\n")
    |> Enum.map(&String.to_charlist/1)
    |> Enum.zip_with(& &1)
    |> Enum.chunk_while([], &emit_on_space/2, &{:cont, &1, nil})
    |> Enum.map(&parse_chunks/1)
    |> Enum.sum_by(&compute/1)
  end
end
