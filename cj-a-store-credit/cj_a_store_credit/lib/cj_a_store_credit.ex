defmodule CjAStoreCredit do

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
  def main(args) do
    {opts, _, _} = OptionParser.parse(args)
    # IO.inspect args
    {_, path} = List.keyfind(opts, :file, 0)
    { :ok, device } = File.open(path)
    source = IO.stream(device, :line)
    num_rec = Enum.take(source, 1) |> Enum.at(0) |> _s2i
    # IO.inspect num_rec

    # process the file in a separate process
    spawn(CjAStoreCredit, :process_file, [source, 1, self])

    process_results(num_rec, 0)
  end


  @doc """
  Convert a string to integer.
  """
  def _s2i(str) do
    {value, _} = Integer.parse(str); value
  end


  @doc """
  Processes a file where blocks of 3 lines define a task or test case.
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
    result = l3 |> _items |> Logic.solve(_s2i(l1), task_num)
    send rppid, result
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
