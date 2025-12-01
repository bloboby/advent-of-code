defmodule Solutions do
  defp _run(day, prefix) do
    suffix = day |> Integer.to_string() |> String.pad_leading(2, "0")

    {:ok, contents} =
      __DIR__ |> Path.dirname() |> Path.join("input/#{prefix}#{suffix}.txt") |> File.read()

    module = Module.concat(["Day#{suffix}" |> String.to_atom()])
    apply(module, :run, [contents])
  end

  def test(day) do
    _run(day, "test")
  end

  def run(day) do
    _run(day, "day")
  end
end
