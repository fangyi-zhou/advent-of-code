defmodule AdventOfCode.Day05 do
  defp update_queue(queues, ["", "", "", "" | rest], curr) do
    update_queue(queues, rest, curr + 1)
  end

  defp update_queue(queues, [stuff | rest], curr) do
    item = String.at(stuff, 1)
    {_, queues} = Map.get_and_update(queues, curr, fn q -> {q, [item | q]} end)
    update_queue(queues, rest, curr + 1)
  end

  defp update_queue(queues, [], _), do: queues

  defp update_queues(queues, [], _), do: queues

  defp update_queues(queues, [line | rest], buckets) do
    splitted = String.split(line, " ")
    queues = update_queue(queues, splitted, 1)
    update_queues(queues, rest, buckets)
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

  defp parse(["" | rest], acc) do
    {parse_queues(acc), parse_instructions(rest)}
  end

  defp parse([hd | rest], acc) do
    parse(rest, [hd | acc])
  end

  defp perform_instr_1(queues, {0, _, _}), do: queues

  defp perform_instr_1(queues, {nums, origin, dest}) do
    {item, queues} = Map.get_and_update(queues, origin, fn [item | q] -> {item, q} end)
    {_, queues} = Map.get_and_update(queues, dest, fn q -> {q, [item | q]} end)
    perform_instr_1(queues, {nums - 1, origin, dest})
  end

  defp perform_instr_2(queues, {nums, origin, dest}) do
    {items, queues} =
      Map.get_and_update(queues, origin, fn q ->
        Enum.split(q, nums)
      end)

    {_, queues} = Map.get_and_update(queues, dest, fn q -> {q, items ++ q} end)
    queues
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(5)
    input = String.trim(input)
    lines = String.split(input, "\n")
    {queues, instructions} = parse(lines, [])

    result =
      Enum.reduce(instructions, queues, fn instr, queue -> perform_instr_1(queue, instr) end)

    Enum.reduce(
      Enum.map(result, fn {_, [item | _]} -> item end),
      fn x, y -> y <> x end
    )
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(5)
    input = String.trim(input)
    lines = String.split(input, "\n")
    {queues, instructions} = parse(lines, [])

    result =
      Enum.reduce(instructions, queues, fn instr, queue -> perform_instr_2(queue, instr) end)

    Enum.reduce(
      Enum.map(result, fn {_, [item | _]} -> item end),
      fn x, y -> y <> x end
    )
  end
end
