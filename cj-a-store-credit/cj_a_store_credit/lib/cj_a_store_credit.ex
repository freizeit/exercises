defmodule CjAStoreCredit do

  @doc """
  Solution to the google code jam 'Store Credit' problem.
  Please see
      http://code.google.com/codejam/contest/dashboard?c=351101#s=p0
  for details.
  """
  def main(args) do
    {opts, _, _} = OptionParser.parse(args)
    # IO.inspect args
    {_, path} = List.keyfind(opts, :file, 0)
    { :ok, device } = File.open(path)
    source = IO.stream(device, :line)
    num_rec = Enum.take(source, 1) |> Enum.first |> _s2i
    # IO.inspect num_rec

    # process the file in a separate process
    spawn(CjAStoreCredit, :process_file, [source, 1, self])

    process_results(num_rec, 0)
  end


  @doc """
  Convert a string to integer.
  """
  def _s2i(str) do
    {value, _} = String.to_integer(str); value
  end


  @doc """
  Processes a file where blocks of 3 lines define a task or test case.
  For each test case there will be:
    - one line containing the value C, the amount of credit you have at the
      store.
    - one line containing the value I, the number of items in the store.
    - one line containing a space separated list of I integers. Each integer P
      indicates the price of an item in the store.
  Each test case will have exactly one solution.
  Please note: each test case is processed in a separate process.
  """
  def process_file(source, task_num, rppid) do
    # IO.puts "> process_file, tn: #{task_num}"
    bo3 = Enum.take(source, 3)
    case bo3 do
      [_,_,_] ->
        spawn(CjAStoreCredit, :process_task, [task_num, bo3, rppid])
        process_file(source, task_num + 1, rppid)
      [] -> :ok
    end
  end


  @doc """
  Extract the item prices and search for a solution.
  """
  def process_task(task_num, [l1, _, l3], rppid) do
    result = l3 |> _items |> _solve(_s2i(l1)) |> _eval(task_num)
    rppid <- result
  end


  @doc """
  Converts a string with item pricess to a list with 2-tuples where the first
  datum is a (one-based) index and the second is an item price.
  Input: "5 75 25"
  Output: [{1, 5}, {2, 75}, {3, 25}]
  """
  def _items(l3) do
    is = l3 |> String.split
            |> Enum.filter(fn(x) -> String.length(x) > 0 end)
            |> Enum.map(&_s2i/1)
            |> Enum.with_index
    lc {v, i} inlist is, do: {i+1, v}
  end


  # The unsolvable cases: zero credit and a store with less than 2 items
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
      [{ oidx, _ } | _] -> {:match, { idx, oidx}}
    end
  end


  def _eval(result, task_num) do
    case result do
      :nomatch -> "No solution for case \##{task_num}"
      {:match, {i1, i2}} -> "Case \##{task_num}: #{i1} #{i2}"
    end
  end


  @doc """
  Keep receiving and printing results until all is done.
  """
  def process_results(total, processed) do
    # IO.puts "total: #{total}, done: #{processed}"
    case processed < total do
      false ->
        # IO.puts "done!"
        :ok
      true ->
        receive do
           message -> IO.puts message
           process_results(total, processed + 1)
        end
    end
  end
end
