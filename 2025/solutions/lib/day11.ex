defmodule Day11 do
  defp adjacency_graph(contents) do
    contents
    |> String.split("\n")
    |> Enum.map(fn line -> String.split(line, [":", " "], trim: true) end)
    |> Map.new(fn [x | ys] -> {x, ys} end)
  end

  defp topological_sort(_, incoming, ["out"], output) do
    if incoming |> Map.values() |> Enum.max() != 0, do: :error, else: ["out" | output]
  end

  defp topological_sort(adj, incoming, [c | candidates], output) do
    incoming = Enum.reduce(adj[c], incoming, &Map.update!(&2, &1, fn x -> x - 1 end))
    new_candidates = adj[c] |> Enum.filter(&(incoming[&1] == 0))
    topological_sort(adj, incoming, candidates ++ new_candidates, [c | output])
  end

  defp num_paths(_, [], _, _), do: 0

  defp num_paths(adj, [x | order], acc, target) do
    cond do
      x == target ->
        Map.get(acc, x, 0)

      not Map.has_key?(acc, x) ->
        num_paths(adj, order, acc, target)

      true ->
        acc = Enum.reduce(adj[x], acc, &Map.update(&2, &1, acc[x], fn v -> v + acc[x] end))
        num_paths(adj, order, acc, target)
    end
  end

  defp num_paths_via(adj, order, [x, y]) do
    a = num_paths(adj, order, %{"svr" => 1}, x)
    b = num_paths(adj, order, %{x => 1}, y)
    c = num_paths(adj, order, %{y => 1}, "out")
    a * b * c
  end

  def part1(contents) do
    adj = contents |> adjacency_graph()
    incoming = adj |> Map.values() |> List.flatten() |> Enum.frequencies()
    candidates = adj |> Map.keys() |> Enum.reject(&Map.has_key?(incoming, &1))
    order = topological_sort(adj, incoming, candidates, []) |> Enum.reverse()

    num_paths(adj, order, %{"you" => 1}, "out")
  end

  def part2(contents) do
    adj = contents |> adjacency_graph()
    incoming = adj |> Map.values() |> List.flatten() |> Enum.frequencies()
    candidates = adj |> Map.keys() |> Enum.reject(&Map.has_key?(incoming, &1))
    order = topological_sort(adj, incoming, candidates, []) |> Enum.reverse()

    midpoints = order |> Enum.filter(&(&1 in ["dac", "fft"]))
    num_paths_via(adj, order, midpoints)
  end
end
