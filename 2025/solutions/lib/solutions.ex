defmodule Solutions do
  defp _run(day, part, prefix) do
    suffix = day |> Integer.to_string() |> String.pad_leading(2, "0")

    {:ok, contents} =
      __DIR__ |> Path.dirname() |> Path.join("input/#{prefix}#{suffix}.txt") |> File.read()

    module = Module.concat(["Day#{suffix}" |> String.to_atom()])

    cond do
      part == 1 -> apply(module, :part1, [contents])
      part == 2 -> apply(module, :part2, [contents])
    end
  end

  def test(day, part) do
    _run(day, part, "test")
  end

  def run(day, part) do
    _run(day, part, "day")
  end
end
