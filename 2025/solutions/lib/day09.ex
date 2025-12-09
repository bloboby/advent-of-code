defmodule Day09 do
  defp parse(line) do
    line |> String.split(",") |> Enum.map(&String.to_integer/1)
  end

  defp area({[a, b], [c, d]}) do
    (abs(c - a) + 1) * (abs(d - b) + 1)
  end

  defp rotate(xs, 0), do: xs
  defp rotate([x | xs], n), do: rotate(xs ++ [x], n - 1)

  defp intersects?(_rect = {[a, b], [c, d]}, _edge = {[e, f], [g, h]}) do
    crosses_x = min(e, g) < max(a, c) and max(e, g) > min(a, c)
    crosses_y = min(f, h) < max(b, d) and max(f, h) > min(b, d)
    crosses_x and crosses_y
  end

  defp edge_intersects_interior?(rect, edges) do
    Enum.any?(edges, &intersects?(rect, &1))
  end

  defp dir(_origin = [a, b], [c, d]) do
    cond do
      a == c and d > b -> :N
      b == d and c > a -> :E
      a == c and d < b -> :S
      b == d and c < a -> :W
      true -> nil
    end
  end

  defp classify_quadrants({prev, this, next}, {acc, prev_quadrants}) do
    # Q1 | Q2
    # ---+--- becomes [Q1, Q2, Q3, Q4]
    # Q3 | Q4
    case {dir(this, prev), dir(this, next), prev_quadrants} do
      # lr
      # ll
      {:N, :E, {l, r}} -> {Map.put(acc, this, [l, r, l, l]), {r, l}}
      # lr
      # rr
      {:N, :W, {l, r}} -> {Map.put(acc, this, [l, r, r, r]), {l, r}}
      # du
      # dd
      {:E, :N, {u, d}} -> {Map.put(acc, this, [d, u, d, d]), {d, u}}
      # uu
      # ud
      {:E, :S, {u, d}} -> {Map.put(acc, this, [u, u, u, d]), {u, d}}
      # ll
      # lr
      {:S, :E, {l, r}} -> {Map.put(acc, this, [l, l, l, r]), {l, r}}
      # rr
      # lr
      {:S, :W, {l, r}} -> {Map.put(acc, this, [r, r, l, r]), {r, l}}
      # ud
      # dd
      {:W, :N, {u, d}} -> {Map.put(acc, this, [u, d, d, d]), {u, d}}
      # uu
      # du
      {:W, :S, {u, d}} -> {Map.put(acc, this, [u, u, d, u]), {d, u}}
    end
  end

  defp choose_a_side(quadrants) do
    freq =
      quadrants
      |> Map.values()
      |> Enum.map(fn qs -> if Enum.frequencies(qs)[:a_side] == 1, do: :a_side, else: :b_side end)
      |> Enum.frequencies()

    cond do
      freq[:a_side] == freq[:b_side] + 4 -> :a_side
      freq[:b_side] == freq[:a_side] + 4 -> :b_side
      true -> :error
    end
  end

  defp any_point_inside_polygon?({a, b}, _, _) when a == b, do: false

  defp any_point_inside_polygon?({[a, b], [c, d]}, quadrants, inside) do
    [q1, q2, q3, q4] = quadrants[[a, b]]

    case {dir([a, b], [a, d]), dir([a, b], [c, b])} do
      # the nils are basically wrong
      {nil, _} -> true
      {_, nil} -> true
      {:N, :W} -> q1 == inside
      {:W, :N} -> q1 == inside
      {:N, :E} -> q2 == inside
      {:E, :N} -> q2 == inside
      {:S, :W} -> q3 == inside
      {:W, :S} -> q3 == inside
      {:E, :S} -> q4 == inside
      {:S, :E} -> q4 == inside
    end
  end

  def part1(contents) do
    tiles = contents |> String.split("\n") |> Enum.map(&parse/1)

    for(a <- tiles, b <- tiles, a < b, do: {a, b})
    |> Enum.map(&area/1)
    |> Enum.max()
  end

  def part2(contents) do
    tiles = contents |> String.split("\n") |> Enum.map(&parse/1)
    edges = Enum.zip([tiles, rotate(tiles, 1)])

    quadrants =
      Enum.zip([tiles, rotate(tiles, 1), rotate(tiles, 2)])
      |> Enum.reduce({%{}, {:a_side, :b_side}}, &classify_quadrants/2)
      |> elem(0)

    for(a <- tiles, b <- tiles, a < b, do: {a, b})
    |> Enum.reject(&edge_intersects_interior?(&1, edges))
    |> Enum.filter(&any_point_inside_polygon?(&1, quadrants, choose_a_side(quadrants)))
    |> Enum.map(&area/1)
    |> Enum.max()
  end
end
