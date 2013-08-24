defmodule CjAStoreCredit do
  def main(args) do
    {opts, _, _} = OptionParser.parse(args)
    # IO.inspect args
    {_, path} = List.keyfind(opts, :file, 0)
    { :ok, device } = File.open(path)
    source = IO.stream(device, :line)
    _num_rec = Stream.take(source, 1)
    # IO.inspect _num_rec
    {num_rec, _} = String.to_integer(Enum.at(_num_rec, 0))
    # IO.inspect num_rec

    # process the file in a separate process
    process_file(source, self)

    process_results(num_rec, 0)
  end

  def process_file(source, rppid) do
    spawn(CjAStoreCredit, :_process_file, [source, 1, rppid])
  end

  def _process_file(source, task_num, rppid) do
    IO.puts "> process_file, tn: #{task_num}"
    l3 = Stream.take(source, 3) |> Enum.to_list
    case l3 do
      [] -> :ok
      [_,_,_] ->
        args = [task_num, l3, rppid]
        pid = spawn(CjAStoreCredit, :process_task, args)
        _process_file(source, task_num + 1, rppid)
    end
  end

  def process_task(task_num, l3, rppid) do
    {credit, _} = String.to_integer(Enum.at(l3, 0))
    {item_count, _} = String.to_integer(Enum.at(l3, 1))
    is = _items(item_count, l3)
    result = _solve(credit, is) |> _eval(task_num)
    rppid <- result
  end

  def _items(item_count, l3) do
    is = Enum.at(l3, 2) |> String.split |>
         Enum.filter(fn(x) -> String.length(x) > 0 end)
    # IO.inspect is
    iis = Enum.map(is, fn i -> {ii, _} = String.to_integer(i); ii end)
    Enum.zip(1..item_count, iis)
  end

  def _eval(result, task_num) do
    case result do
      :nomatch -> "No solution for case \##{task_num}"
      {:match, {i1, i2}} -> "Case \##{task_num}: #{i1} #{i2}"
    end
  end

  # The unsolvable cases: zero credit and a store with less than 2 is
  def _solve(0, _) do :nomatch end
  def _solve(_, []) do :nomatch end
  def _solve(_, [_]) do :nomatch end
  def _solve(credit, [{idx, i} | is]) do
    rest = Enum.drop_while(is,
            fn { _, oi } -> i + oi != credit end)
    case rest do
      [] -> _solve(credit, is) # no solution for this prefix
      [{ oidx, _ } | _] -> {:match, { idx, oidx}}
    end
  end

  def process_results(result_count, processed) do
    IO.puts "total: #{result_count}, done: #{processed}"
    case processed >= result_count do
      true ->
        IO.puts "done!"
        :ok
      false ->
        receive do
           message -> IO.puts message
           process_results(result_count, processed + 1)
        end
    end
  end
end
