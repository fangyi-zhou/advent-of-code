defmodule AdventOfCode.Day11 do
  defp test do
    "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"
  end

  defp parse([monkey, starting, operation, test, if_true, if_false]) do
    [_monkey, id] = String.split(monkey, " ", trim: true)
    [_starting, _items | items] = String.split(starting, [" ", ","], trim: true)
    [_operation, _new, _eq, operand1, op, operand2] = String.split(operation, " ", trim: true)
    [_test, _divisible, _by, divisible] = String.split(test, " ", trim: true)
    [_if, _true, _throw, _to, _monkey, monkey_true] = String.split(if_true, " ", trim: true)
    [_if, _false, _throw, _to, _monkey, monkey_false] = String.split(if_false, " ", trim: true)
    {id, _} = Integer.parse(id)

    items =
      Enum.map(items, fn item ->
        {item, _} = Integer.parse(item)
        item
      end)

    operation = fn x ->
      operand1 =
        case operand1 do
          "old" ->
            x

          _ ->
            {num, _} = Integer.parse(operand1)
            num
        end

      operand2 =
        case operand2 do
          "old" ->
            x

          _ ->
            {num, _} = Integer.parse(operand2)
            num
        end

      case op do
        "+" -> operand1 + operand2
        "*" -> operand1 * operand2
      end
    end

    {divisible, _} = Integer.parse(divisible)
    {monkey_true, _} = Integer.parse(monkey_true)
    {monkey_false, _} = Integer.parse(monkey_false)

    {id, {items, operation, divisible, monkey_true, monkey_false}}
  end

  defp throw_item(monkeys, throw_to, item) do
    {:ok, monkeys} =
      Map.get_and_update(monkeys, throw_to, fn
        {items, operation, divisible, monkey_true, monkey_false} ->
          {:ok, {items ++ [item], operation, divisible, monkey_true, monkey_false}}
      end)

    monkeys
  end

  defp process_items(monkeys, curr, [], _, _, _, _, _) do
    {:ok, monkeys} =
      Map.get_and_update(monkeys, curr, fn
        {_, operation, divisible, monkey_true, monkey_false} ->
          {:ok, {[], operation, divisible, monkey_true, monkey_false}}
      end)

    monkeys
  end

  defp process_items(
         monkeys,
         curr,
         [item | items],
         operation,
         divisible,
         monkey_true,
         monkey_false,
         worry_fn
       ) do
    worry = worry_fn.(operation.(item))

    monkeys =
      throw_item(
        monkeys,
        if(rem(worry, divisible) == 0, do: monkey_true, else: monkey_false),
        worry
      )

    process_items(monkeys, curr, items, operation, divisible, monkey_true, monkey_false, worry_fn)
  end

  defp single_round(monkeys, counts, curr, total, _) when curr == total do
    {monkeys, counts}
  end

  defp single_round(monkeys, counts, curr, total, worry_fn) do
    {items, operation, divisible, monkey_true, monkey_false} = Map.get(monkeys, curr)
    counts = Map.update(counts, curr, length(items), fn old -> old + length(items) end)

    monkeys =
      process_items(
        monkeys,
        curr,
        items,
        operation,
        divisible,
        monkey_true,
        monkey_false,
        worry_fn
      )

    single_round(monkeys, counts, curr + 1, total, worry_fn)
  end

  defp go(_, 0, counts, _, _) do
    [x, y | _] = Enum.sort(Map.values(counts), :desc)
    x * y
  end

  defp go(monkeys, rounds, counts, total, worry_fn) do
    {monkeys, counts} = single_round(monkeys, counts, 0, total, worry_fn)
    go(monkeys, rounds - 1, counts, total, worry_fn)
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(11)
    lines = String.split(input, "\n", trim: true)
    monkeys = Map.new(Enum.map(Enum.chunk_every(lines, 6), &parse/1))
    monkeys_count = map_size(monkeys)
    go(monkeys, 20, Map.new(), monkeys_count, fn worry -> div(worry, 3) end)
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(11)
    lines = String.split(input, "\n", trim: true)
    monkeys = Map.new(Enum.map(Enum.chunk_every(lines, 6), &parse/1))
    divisibles = Enum.map(Map.values(monkeys), fn {_, _, divisible, _, _} -> divisible end)
    modular = Enum.reduce(divisibles, 1, fn x, y -> x * y end)
    monkeys_count = map_size(monkeys)
    go(monkeys, 10000, Map.new(), monkeys_count, fn worry -> rem(worry, modular) end)
  end
end
