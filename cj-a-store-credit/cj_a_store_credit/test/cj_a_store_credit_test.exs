Code.require_file "test_helper.exs", __DIR__

defmodule CjAStoreCreditTest do
  use ExUnit.Case, async: true

  test "items extracted correctly" do
    assert CjAStoreCredit._items(3, ["101\n","3\n","722 54 47\n"])
      == [{1,722},{2,54},{3,47}]
  end
  test "_eval with :nomatch" do
    assert CjAStoreCredit._eval(:nomatch, 89) == "No solution for case #89"
  end
  test "_eval with :match" do
    assert CjAStoreCredit._eval({:match, {11, 23}}, 88) == "Case #88: 11 23"
  end
  test "_solve with zero credit" do
    assert CjAStoreCredit._solve(0, [{1,722},{2,54},{3,47}]) == :nomatch
  end
  test "_solve with empty items list" do
    assert CjAStoreCredit._solve(77, []) == :nomatch
  end
  test "_solve with a single element items list" do
    assert CjAStoreCredit._solve(78, [{1,722}]) == :nomatch
  end
  test "_solve with proper args" do
    assert CjAStoreCredit._solve(101, [{1,722},{2,600},{3,905},{4,54},{5,47}])
      == {:match,{4,5}}
  end
  test "process_task with proper args" do
    CjAStoreCredit.process_task(9, ["101\n","5\n","722 600 905 54 47\n"], self)
    receive do
       message -> assert message == "Case #9: 4 5"
    end
  end
end
