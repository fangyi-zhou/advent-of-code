defmodule AdventOfCode.Day08 do
  defp line_to_map(chars, x, y, acc) do
    case chars do
      [] ->
        {acc, y}

      [hd | tl] ->
        acc = Map.put(acc, {x, y}, hd - ?0)
        line_to_map(tl, x, y + 1, acc)
    end
  end

  defp to_map(lines, x, y, acc) do
    case lines do
      [] ->
        {acc, x, y}

      [hd | tl] ->
        chars = String.to_charlist(hd)
        {acc, y} = line_to_map(chars, x, 0, acc)
        to_map(tl, x + 1, y, acc)
    end
  end

  defp horizontal_lookups_h(map, x, y, i, j, left, right) do
    cond do
      i == x ->
        {left, right}

      j == y ->
        horizontal_lookups_h(map, x, y, i + 1, 0, left, right)

      j == 0 ->
        curr_l = Map.get(map, {i, j})
        left = Map.put(left, {i, j}, curr_l)
        curr_r = Map.get(map, {i, y - j - 1})
        right = Map.put(right, {i, y - j - 1}, curr_r)
        horizontal_lookups_h(map, x, y, i, j + 1, left, right)

      true ->
        curr_l = Map.get(map, {i, j})
        max_l = Map.get(left, {i, j - 1})
        left = Map.put(left, {i, j}, max(curr_l, max_l))
        curr_r = Map.get(map, {i, y - j - 1})
        max_r = Map.get(right, {i, y - j})
        right = Map.put(right, {i, y - j - 1}, max(curr_r, max_r))
        horizontal_lookups_h(map, x, y, i, j + 1, left, right)
    end
  end

  defp vertical_lookups_h(map, x, y, i, j, top, bottom) do
    cond do
      j == y ->
        {top, bottom}

      i == x ->
        vertical_lookups_h(map, x, y, 0, j + 1, top, bottom)

      i == 0 ->
        curr_t = Map.get(map, {i, j})
        top = Map.put(top, {i, j}, curr_t)
        curr_b = Map.get(map, {x - i - 1, j})
        bottom = Map.put(bottom, {x - i - 1, j}, curr_b)
        vertical_lookups_h(map, x, y, i + 1, j, top, bottom)

      true ->
        curr_t = Map.get(map, {i, j})
        max_t = Map.get(top, {i - 1, j})
        top = Map.put(top, {i, j}, max(curr_t, max_t))
        curr_b = Map.get(map, {x - i - 1, j})
        max_b = Map.get(bottom, {x - i, j})
        bottom = Map.put(bottom, {x - i - 1, j}, max(curr_b, max_b))
        vertical_lookups_h(map, x, y, i + 1, j, top, bottom)
    end
  end

  defp make_height_lookups(map, x, y) do
    {left, right} = horizontal_lookups_h(map, x, y, 0, 0, Map.new(), Map.new())
    {top, bottom} = vertical_lookups_h(map, x, y, 0, 0, Map.new(), Map.new())
    {left, right, top, bottom}
  end

  defp find_visible(map, height_lookups, i, j, x, y, acc) do
    cond do
      i == x ->
        acc

      j == y ->
        find_visible(map, height_lookups, i + 1, 0, x, y, acc)

      i == 0 or j == 0 or i == x - 1 or j == y - 1 ->
        find_visible(map, height_lookups, i, j + 1, x, y, acc + 1)

      true ->
        curr = Map.get(map, {i, j})
        {left, right, top, bottom} = height_lookups
        left = Map.get(left, {i, j - 1})
        right = Map.get(right, {i, j + 1})
        top = Map.get(top, {i - 1, j})
        bottom = Map.get(bottom, {i + 1, j})
        # IO.puts("#{i} #{j} #{curr} #{left} #{right} #{top} #{bottom}")

        acc =
          if curr > left or curr > right or curr > top or curr > bottom, do: acc + 1, else: acc

        find_visible(map, height_lookups, i, j + 1, x, y, acc)
    end
  end

  defp scene_score(map, limit, x, y, i, j, dx, dy, acc) do
    cond do
      i < 0 or j < 0 or i >= x or j >= y ->
        acc - 1

      true ->
        curr = Map.get(map, {i, j})

        if curr < limit do
          scene_score(map, limit, x, y, i + dx, j + dy, dx, dy, acc + 1)
        else
          acc
        end
    end
  end

  defp find_best(map, i, j, x, y, acc) do
    cond do
      i == x ->
        acc

      j == y ->
        find_best(map, i + 1, 0, x, y, acc)

      i == 0 or j == 0 or i == x - 1 or j == y - 1 ->
        find_best(map, i, j + 1, x, y, acc)

      true ->
        curr = Map.get(map, {i, j})
        left = scene_score(map, curr, x, y, i, j - 1, 0, -1, 1)
        right = scene_score(map, curr, x, y, i, j + 1, 0, 1, 1)
        top = scene_score(map, curr, x, y, i - 1, j, -1, 0, 1)
        bottom = scene_score(map, curr, x, y, i + 1, j, 1, 0, 1)

        find_best(map, i, j + 1, x, y, max(left * right * top * bottom, acc))
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(8)
    input = String.trim(input)
    lines = String.split(input, "\n")
    {map, x, y} = to_map(lines, 0, 0, Map.new())
    height_lookups = make_height_lookups(map, x, y)
    find_visible(map, height_lookups, 0, 0, x, y, 0)
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(8)
    input = String.trim(input)
    lines = String.split(input, "\n")
    {map, x, y} = to_map(lines, 0, 0, Map.new())
    find_best(map, 0, 0, x, y, 0)
  end
end
