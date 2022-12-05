defmodule AdventOfCode.Day05 do
  defp update_queue(queues, splitted, curr) do
    case splitted do
      ["", "", "", "" | rest] ->
        update_queue(queues, rest, curr + 1)

      [stuff | rest] ->
        item = String.at(stuff, 1)
        {_, queues} = Map.get_and_update(queues, curr, fn q -> {q, [item | q]} end)
        update_queue(queues, rest, curr + 1)

      [] ->
        queues
    end
  end

  defp update_queues(queues, lines, buckets) do
    case lines do
      [] ->
        queues

      [line | rest] ->
        splitted = String.split(line, " ")
        queues = update_queue(queues, splitted, 1)
        update_queues(queues, rest, buckets)
    end
  end

  defp parse_queues([hd | rest]) do
    buckets = Enum.count(String.split(hd, " "), fn s -> s != "" end)
    queues = Map.new(1..buckets, fn id -> {id, []} end)
    update_queues(queues, rest, buckets)
  end

  defp parse_instructions(lines) do
    Enum.map(lines, fn line ->
      [_move, nums, _from, origin, _to, dest] = String.split(line, " ")
      {nums, _} = Integer.parse(nums)
      {origin, _} = Integer.parse(origin)
      {dest, _} = Integer.parse(dest)
      {nums, origin, dest}
    end)
  end

  defp parse(lines, acc) do
    case lines do
      ["" | rest] ->
        {parse_queues(acc), parse_instructions(rest)}

      [hd | rest] ->
        parse(rest, [hd | acc])
    end
  end

  defp perform_instr(queues, {nums, origin, dest}) do
    if nums == 0 do
      queues
    else
      {item, queues} = Map.get_and_update(queues, origin, fn [item | q] -> {item, q} end)
      {_, queues} = Map.get_and_update(queues, dest, fn q -> {q, [item | q]} end)
      perform_instr(queues, {nums - 1, origin, dest})
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(5)
    input = String.trim(input)
    lines = String.split(input, "\n")
    {queues, instructions} = parse(lines, [])
    result = Enum.reduce(instructions, queues, fn instr, queue -> perform_instr(queue, instr) end)

    Enum.reduce(
      Enum.map(result, fn {_, [item | _]} -> item end),
      fn x, y -> y <> x end
    )
  end

  def part2(_args) do
  end
end
