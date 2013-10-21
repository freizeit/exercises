Code.require_file "test_helper.exs", __DIR__

defmodule CjAStoreCreditTest do
  use ExUnit.Case, async: true

  test "items extracted correctly" do
    assert CjAStoreCredit._items("722 54 47\n")
      == [{1,722},{2,54},{3,47}]
  end
  test "process_task with proper args" do
    CjAStoreCredit.process_task(9, ["101\n","5\n","722 600 905 54 47\n"], self)
    receive do
       message -> assert message == "Case #9: 4 5"
    end
  end
end
