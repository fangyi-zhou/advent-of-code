defmodule AdventOfCode.Day07 do
  defp ls(lines, dirs, work_dir) do
    case lines do
      [] ->
        {dirs, lines}

      [hd | tl] ->
        cond do
          String.starts_with?(hd, "$") ->
            {dirs, lines}

          true ->
            {:dir, path, subs, dir_size} = Map.get(dirs, work_dir)
            [info, name] = String.split(hd, " ")

            case info do
              "dir" ->
                new_dir = {:dir, name, Map.new(), :unknown}
                subs = Map.put(subs, name, {:dir, name})
                dirs = Map.put(dirs, [name | work_dir], new_dir)
                curr = {:dir, path, subs, dir_size}
                dirs = Map.put(dirs, work_dir, curr)
                ls(tl, dirs, work_dir)

              size ->
                {size, _} = Integer.parse(size)
                subs = Map.put(subs, name, {:file, name, size})
                curr = {:dir, path, subs, dir_size}
                dirs = Map.put(dirs, work_dir, curr)
                ls(tl, dirs, work_dir)
            end
        end
    end
  end

  defp process(lines, dirs, work_dir) do
    curr = Map.get(dirs, work_dir)

    case lines do
      [] ->
        dirs

      [hd | tl] ->
        cond do
          String.starts_with?(hd, "$ ls") ->
            {dirs, rest} = ls(tl, dirs, work_dir)
            process(rest, dirs, work_dir)

          String.starts_with?(hd, "$ cd") ->
            [_dollar, _cd, dir] = String.split(hd, " ")

            case dir do
              ".." ->
                {:dir, _, _, _} = curr
                [_ | upper_dirs] = work_dir
                process(tl, dirs, upper_dirs)

              "/" ->
                process(tl, dirs, [])

              dir ->
                new_work_dir = [dir | work_dir]
                process(tl, dirs, new_work_dir)
            end
        end
    end
  end

  defp compute_size(dirs, path) do
    case Map.get(dirs, path) do
      {:dir, sub_path, subs, :unknown} ->
        {dirs, size} =
          Enum.reduce(subs, {dirs, 0}, fn {_, sub}, {dirs, acc} ->
            case sub do
              {:file, _, size} ->
                {dirs, acc + size}

              {:dir, name} ->
                case Map.get(dirs, [name | path]) do
                  {:dir, _, _, :unknown} ->
                    dirs = compute_size(dirs, [name | path])

                    case Map.get(dirs, [name | path]) do
                      {:dir, _, _, size} ->
                        {dirs, acc + size}
                    end

                  {:dir, _, _, size} ->
                    {dirs, acc + size}
                end
            end
          end)

        Map.put(dirs, path, {:dir, sub_path, subs, size})

      _ ->
        dirs
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(7)
    input = String.trim(input)
    lines = String.split(input, "\n")
    root = {:dir, "/", Map.new(), :unknown}
    dirs = Map.new()
    dirs = Map.put(dirs, [], root)
    dirs = process(lines, dirs, [])
    dirs = compute_size(dirs, [])

    Enum.reduce(dirs, 0, fn {_, {:dir, _, _, size}}, acc ->
      if size <= 100_000, do: size + acc, else: acc
    end)
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(7)
    input = String.trim(input)
    lines = String.split(input, "\n")
    root = {:dir, "/", Map.new(), :unknown}
    dirs = Map.new()
    dirs = Map.put(dirs, [], root)
    dirs = process(lines, dirs, [])
    dirs = compute_size(dirs, [])

    {:dir, _, _, size} = Map.get(dirs, [])
    size_to_reclaim = size - 40_000_000

    candidates =
      Enum.filter(dirs, fn {key, node} ->
        case node do
          {:dir, _, _, size} ->
            Map.get(dirs, [key])
            size >= size_to_reclaim
        end
      end)

    Enum.reduce(candidates, 99_999_999, fn {_, {:dir, _, _, size}}, acc ->
      min(acc, size)
    end)
  end
end
