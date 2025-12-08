defmodule UnionFind do
  # Union-find implemented over two maps.
  defstruct [:parent, :size]

  def new(nodes) do
    p = nodes |> Map.new(fn n -> {n, n} end)
    s = nodes |> Map.new(fn n -> {n, 1} end)
    %UnionFind{parent: p, size: s}
  end

  defp root(a, parent) do
    cond do
      not Map.has_key?(parent, a) -> :error
      parent[a] == a -> a
      true -> root(parent[a], parent)
    end
  end

  def union({a, b}, uf = %UnionFind{parent: parent, size: size}) do
    {a_root, b_root} = {root(a, parent), root(b, parent)}

    if a_root == b_root do
      uf
    else
      p = parent |> Map.put(b_root, a_root)
      s = size |> Map.put(a_root, size[a_root] + size[b_root])
      %UnionFind{parent: p, size: s}
    end
  end

  def sizes(%UnionFind{parent: parent, size: size}) do
    parent |> Map.keys() |> Enum.map(&root(&1, parent)) |> MapSet.new() |> Enum.map(&size[&1])
  end

  def connected?(%UnionFind{size: size}) do
    size |> Map.values() |> Enum.max() == size |> Enum.count()
  end
end

defmodule Day08 do
  defp parse(line) do
    line |> String.split(",") |> Enum.map(&String.to_integer/1)
  end

  defp dist({a, b}) do
    Enum.zip_with(a, b, fn x, y -> (x - y) ** 2 end) |> Enum.sum()
  end

  def part1(contents) do
    boxes = contents |> String.split("\n") |> Enum.map(&parse/1)

    for(a <- boxes, b <- boxes, a < b, do: {a, b})
    |> Enum.sort_by(&dist/1)
    |> Enum.take(1000)
    |> Enum.reduce(UnionFind.new(boxes), &UnionFind.union/2)
    |> UnionFind.sizes()
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.product()
  end

  defp union_until_connected(edge, acc) do
    uf = UnionFind.union(edge, acc)
    if UnionFind.connected?(uf), do: {:halt, edge}, else: {:cont, uf}
  end

  def part2(contents) do
    boxes = contents |> String.split("\n") |> Enum.map(&parse/1)

    {[x1, _, _], [x2, _, _]} =
      for(a <- boxes, b <- boxes, a < b, do: {a, b})
      |> Enum.sort_by(&dist/1)
      |> Enum.reduce_while(UnionFind.new(boxes), &union_until_connected/2)

    x1 * x2
  end
end
