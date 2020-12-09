defmodule Day3 do
  @moduledoc """
  Computes the answer for AoC day 3
  """

  def part1 do
    File.stream!("input.txt")
    |> Stream.map(&String.split(&1, "\n", trim: true))
    |> count_trees({3, 1})
  end

  def part2 do
    forest =
      File.stream!("input.txt")
      |> Stream.map(&String.split(&1, "\n", trim: true))

    [{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}]
    |> Enum.map(fn slope -> count_trees(forest, slope) end)
    |> Enum.reduce(1, &*/2)
  end

  def count_trees(forest, {d_x, d_y}) do
    {trees, _} =
      forest
      |> Enum.take_every(d_y)
      |> Enum.reduce({0, 0}, fn row, {tree_count, x} ->
        char_is_tree? =
          row
          |> Stream.flat_map(&String.split(&1, "", trim: true))
          |> Stream.cycle()
          |> Enum.at(x)
          |> tree?

        if char_is_tree? do
          {tree_count + 1, x + d_x}
        else
          {tree_count, x + d_x}
        end
      end)

    trees
  end

  def tree?(char) do
    char == "#"
  end
end

IO.puts "Part 1: #{Day3.part1}"
IO.puts "Part 2: #{Day3.part2}"
