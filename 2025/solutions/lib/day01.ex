defmodule Day01 do
  defp rotate(location, direction, distance) do
    cond do
      direction == "L" -> location - distance
      direction == "R" -> location + distance
    end
    |> Integer.mod(100)
  end

  defp final_crossing(location, direction, distance) do
    remainder = Integer.mod(distance, 100)

    cond do
      location == 0 -> 0
      direction == "L" and location <= remainder -> 1
      direction == "R" and 100 - location <= remainder -> 1
      true -> 0
    end
  end

  defp count_final_zeroes(command, {location, count}) do
    <<direction::binary-size(1), distance::binary>> = command
    distance = String.to_integer(distance)

    location = rotate(location, direction, distance)
    count = if location == 0, do: count + 1, else: count
    {location, count}
  end

  defp count_all_zeroes(command, {location, count}) do
    <<direction::binary-size(1), distance::binary>> = command
    distance = String.to_integer(distance)

    count =
      count + Integer.floor_div(distance, 100) + final_crossing(location, direction, distance)

    location = rotate(location, direction, distance)

    {location, count}
  end

  def part1(contents) do
    contents |> String.split("\n") |> Enum.reduce({50, 0}, &count_final_zeroes/2) |> elem(1)
  end

  def part2(contents) do
    contents |> String.split("\n") |> Enum.reduce({50, 0}, &count_all_zeroes/2) |> elem(1)
  end
end
