Code.require_file "test_helper.exs", __DIR__

defmodule LogicTest do
  use ExUnit.Case, async: true

  test "_eval with :nomatch" do
    assert Logic._eval(:nomatch, 89) == "No solution for case #89"
  end
  test "_eval with :match" do
    assert Logic._eval({:match, {11, 23}}, 88) == "Case #88: 11 23"
  end
  test "_solve with zero credit" do
    assert Logic._solve([{1,722},{2,54},{3,47}], 0) == :nomatch
  end
  test "_solve with empty items list" do
    assert Logic._solve([], 77) == :nomatch
  end
  test "_solve with a single element items list" do
    assert Logic._solve([{1,722}], 78) == :nomatch
  end
  test "_solve with proper args" do
    assert Logic._solve([{1,722},{2,600},{3,905},{4,54},{5,47}], 101)
      == {:match,{4,5}}
  end
end
