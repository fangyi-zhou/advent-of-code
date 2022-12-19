defmodule AdventOfCode.Day18 do
  defp test do
    "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"
  end

  defp parse_point(line) do
    [x, y, z] = String.split(line, ",")
    {x, _} = Integer.parse(x)
    {y, _} = Integer.parse(y)
    {z, _} = Integer.parse(z)
    {x, y, z}
  end

  def part1(_args) do
    # input = test()
    input = AdventOfCode.Input.get!(18)
    lines = String.split(input, "\n", trim: true)
    points = MapSet.new(Enum.map(lines, &parse_point/1))

    Enum.reduce(points, 0, fn {x, y, z}, acc ->
      neighbours =
        Enum.map(
          directions(),
          fn {dx, dy, dz} ->
            {x + dx, y + dy, z + dz}
          end
        )

      acc + Enum.count(neighbours, fn pt -> not MapSet.member?(points, pt) end)
    end)
  end

  defp directions do
    [
      {1, 0, 0},
      {-1, 0, 0},
      {0, 1, 0},
      {0, -1, 0},
      {0, 0, 1},
      {0, 0, -1}
    ]
  end

  #  defp reachable(_, _, [], _), do: :no
  #  defp reachable(_, _, [{pt, history} | _], target) when pt == target, do: {:yes, history}
  #
  #  defp reachable(points, visited, [{curr, history} | rest], target) do
  #    if MapSet.member?(visited, curr) do
  #      reachable(points, visited, rest, target)
  #    else
  #      visited = MapSet.put(visited, curr)
  #      {x, y, z} = curr
  #
  #      neighbours =
  #        Enum.filter(
  #          Enum.map(directions(), fn {dx, dy, dz} ->
  #            {{x + dx, y + dy, z + dz}, [curr | history]}
  #          end),
  #          fn {pt, _} -> not MapSet.member?(points, pt) end
  #        )
  #
  #      reachable(points, visited, rest ++ neighbours, target)
  #    end
  #  end
  #
  #  def surrounded(points, acc, pt) do
  #    case reachable(points, points, [{pt, []}], {0, 0, 0}) do
  #      :no -> {false, acc}
  #      {:yes, history} -> {true, MapSet.union(acc, MapSet.new(history))}
  #    end
  #  end

  def part2(_args) do
    #     # input = test()
    #     input = AdventOfCode.Input.get!(18)
    #     lines = String.split(input, "\n", trim: true)
    #     points = MapSet.new(Enum.map(lines, &parse_point/1))
    # 
    #     {ans, _} =
    #       Enum.reduce(points, {0, MapSet.new()}, fn {x, y, z}, {acc, reachable_pts} ->
    #         neighbours =
    #           Enum.map(
    #             directions(),
    #             fn {dx, dy, dz} ->
    #               {x + dx, y + dy, z + dz}
    #             end
    #           )
    # 
    #         Enum.reduce(neighbours, {acc, reachable_pts}, fn pt, {acc, reachable_pts} ->
    #           if not MapSet.member?(points, pt) do
    #             if MapSet.member?(reachable_pts, pt) do
    #               {acc + 1, reachable_pts}
    #             else
    #               {is_surrounded, reachable_pts} = surrounded(points, reachable_pts, pt)
    #               {if(is_surrounded, do: acc + 1, else: acc), reachable_pts}
    #             end
    #           else
    #             {acc, reachable_pts}
    #           end
    #         end)
    #       end)
    # 
    #     ans
  end
end
