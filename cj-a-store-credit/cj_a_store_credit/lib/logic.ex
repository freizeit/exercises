defmodule Logic do


  @doc """
  Solution to the google code jam 'Store Credit' problem.
  Please see
      http://code.google.com/codejam/contest/dashboard?c=351101#s=p0
  for details. For each test case there will be:
    - one line containing the value C, the amount of credit you have at the
      store.
    - one line containing the value I, the number of items in the store.
    - one line containing a space separated list of I integers. Each integer P
      indicates the price of an item in the store.
  Example:
    200
    7
    150 24 79 50 88 345 3

  Solution:
    "Case #2: 1 4"
  """
  def solve(prices, credit, task_num) do
    _solve(prices, credit) |> _eval(task_num)
  end


  # The unsolvable cases: zero credit and a store with less than 2 item prices
  def _solve(_, 0) do :nomatch end
  def _solve([], _) do :nomatch end
  def _solve([_], _) do :nomatch end
  @doc """
  Look for 2 prices whose sum is equal to the credit.
  """
  def _solve([{idx, price} | rest], credit) do
    outcome = Enum.drop_while(
      rest, fn { _, other } -> price + other != credit end)
    case outcome do
      [] -> _solve(rest, credit) # no solution for this item/price
      [{ other_idx, _ } | _] -> {:match, { idx, other_idx}}
    end
  end


  def _eval(result, task_num) do
    case result do
      :nomatch -> "No solution for case \##{task_num}"
      {:match, {idx1, idx2}} -> "Case \##{task_num}: #{idx1} #{idx2}"
    end
  end
end
