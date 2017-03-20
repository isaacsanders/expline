defmodule Expline.Matrix do
  @moduledoc false

  @enforce_keys [:n_rows, :m_cols, :internal]
  defstruct [:n_rows, :m_cols, :internal]

  @type t() :: %Expline.Matrix{ n_rows: pos_integer(),
                                m_cols: pos_integer(),
                                internal: internal() }
  @typep internal() :: tuple()

  @spec zeros(pos_integer(), pos_integer()) :: Expline.Matrix.t()
  def zeros(n_rows, m_cols) do
    construct(n_rows, m_cols, fn (_, _) -> 0.0 end)
  end

  @spec identity(pos_integer()) :: Expline.Matrix.t()
  def identity(n) do
    construct(n, n, fn
       (i, i) -> 1.0
       (_i, _j) -> 0.0
    end)
  end

  @spec construct(pos_integer(), pos_integer(), (non_neg_integer(), non_neg_integer() -> float())) :: Expline.Matrix.t()
  def construct(n_rows, m_cols, elem_fn)
  when n_rows > 0
  and m_cols > 0
  and is_function(elem_fn, 2) do
    internal = 0..(n_rows - 1)
    |> Enum.reduce({}, fn (i, matrix) ->
      row = 0..(m_cols - 1)
      |> Enum.reduce({}, fn (j, row) ->
        Tuple.append(row, elem_fn.(i, j))
      end)
      Tuple.append(matrix, row)
    end)
    %Expline.Matrix{ n_rows: n_rows, m_cols: m_cols, internal: internal }
  end

  @spec at(Expline.Matrix.t(), non_neg_integer(), non_neg_integer()) :: float()
  def at(%Expline.Matrix{ n_rows: n_rows, m_cols: m_cols, internal: internal }, i, j)
  when is_integer(i)
  and i < n_rows
  and is_integer(j)
  and j < m_cols do
    internal
    |> elem(i)
    |> elem(j)
  end

  @spec transpose(Expline.Matrix.t()) :: Expline.Matrix.t()
  def transpose(%Expline.Matrix{} = matrix) do
    construct(matrix.m_cols, matrix.n_rows, fn
       (i, j) -> at(matrix, j, i)
    end)
  end

  @spec symmetric?(Expline.Matrix.t()) :: boolean()
  def symmetric?(%Expline.Matrix{} = matrix) do
    matrix == transpose(matrix)
  end

  @spec lower_triangular?(Expline.Matrix.t()) :: boolean()
  def lower_triangular?(%Expline.Matrix{ n_rows: n_rows, m_cols: m_cols } = matrix) do
    for i <- 0..(n_rows-1), j <- 0..(m_cols-1), i < j do
      at(matrix, i, j)
    end |> Enum.all?(fn (0.0) -> true; (_) -> false end)
  end

  @spec upper_triangular?(Expline.Matrix.t()) :: boolean()
  def upper_triangular?(%Expline.Matrix{ n_rows: n_rows, m_cols: m_cols } = matrix) do
    for i <- 0..(n_rows-1), j <- 0..(m_cols-1), i > j do
      at(matrix, i, j)
    end |> Enum.all?(fn (0.0) -> true; (_) -> false end)
  end

  @spec cholesky_decomposition(Expline.Matrix.t()) :: {:ok, Expline.Matrix.t()}
                                                        | {:error, :not_square}
                                                        | {:error, :not_symmetric}
                                                        | {:error, :not_positive_definite}
  def cholesky_decomposition(%Expline.Matrix{ n_rows: n_rows, m_cols: m_cols } = matrix)
  when n_rows == m_cols do
    if symmetric?(matrix) do
      l_internal = 0..(n_rows - 1)
      |> Enum.reduce(matrix.internal, fn (i, internal) ->
        orig_row = elem(internal, i)
        new_row = 0..(m_cols - 1)
        |> Enum.reduce(orig_row, fn (j, row) ->
          cond do
            i == j ->
              summation = for k <- 0..(j-1), k >= 0, k <= (j-1) do
                elem(row, k) |> :math.pow(2)
              end |> Enum.sum
              put_elem(row, j, :math.sqrt(elem(orig_row, j) - summation))

            i > j ->
              summation = for k <- 0..(j-1), k >= 0, k <= (j-1) do
                upper_row = elem(internal, i)
                elem(row, k) * elem(upper_row, k)
              end |> Enum.sum

              put_elem(row, j, (elem(orig_row, j) - summation) / (internal |> elem(j) |> elem(j)))

            i < j -> put_elem(row, j, 0.0)
          end
        end)
        put_elem(internal, i, new_row)
      end)
      {:ok, %{ matrix | internal: l_internal }}
    else
      {:error, :not_symmetric}
    end
  end
  def cholesky_decomposition(%Expline.Matrix{}), do: {:error, :not_square}

  @spec product(Expline.Matrix.t(), Expline.Matrix.t()) :: {:ok, Expline.Matrix.t()}
                                                         | {:error, :dimension_mismatch}
  def product(%Expline.Matrix{ n_rows: a_rows, m_cols: a_cols, internal: a_internal },
              %Expline.Matrix{ n_rows: b_rows, m_cols: b_cols } = b)
  when a_cols == b_rows do
    b_internal = transpose(b).internal
    c = construct(a_rows, b_cols, fn
       (i, j) ->
         as = elem(a_internal, i) |> Tuple.to_list
         bs = elem(b_internal, j) |> Tuple.to_list

         Enum.zip(as, bs)
         |> Enum.map(fn ({a_ik, b_kj}) -> a_ik * b_kj end)
         |> Enum.sum
    end)
    {:ok, c}
  end
  def product(%Expline.Matrix{}, %Expline.Matrix{}), do: {:error, :dimension_mismatch}

  @spec forward_substitution(Expline.Matrix.t(), Expline.Vector.t()) :: {:ok, Expline.Vector.t()}
                                                                      | {:error, :dimension_mismatch}
                                                                      | {:error, :not_lower_triangular}
  def forward_substitution(%Expline.Matrix{ n_rows: n_rows } = matrix,
                           %Expline.Vector{ n_slots: n_slots } = vector)
  when n_rows == n_slots do
    if lower_triangular?(matrix) do
      solution = do_forward_substitution(matrix, vector, 0, {})
      {:ok, solution}
    else
      {:error, :not_lower_triangular}
    end
  end
  def forward_substitution(%Expline.Matrix{}, %Expline.Vector{}), do: {:error, :dimension_mismatch}

  defp do_forward_substitution(_matrix, %Expline.Vector{ n_slots: n_slots }, _row, solution)
  when n_slots == tuple_size(solution) do
    Expline.Vector.construct(tuple_size(solution), fn
       (i) -> elem(solution, i)
    end)
  end
  defp do_forward_substitution(matrix, vector, nth_row, solution) do
    summation = for i <- 0..(nth_row-1), i >= 0, i <= nth_row-1 do
      at(matrix, nth_row, i) * elem(solution, i)
    end |> Enum.sum
    new_solution = (Expline.Vector.at(vector, nth_row) - summation) / at(matrix, nth_row, nth_row)
    do_forward_substitution(matrix, vector, nth_row + 1, Tuple.append(solution, new_solution))
  end

  @spec backward_substitution(Expline.Matrix.t(), Expline.Vector.t()) :: {:ok, Expline.Vector.t()}
                                                                       | {:error, :dimension_mismatch}
                                                                       | {:error, :not_upper_triangular}

  def backward_substitution(%Expline.Matrix{ n_rows: n_rows } = matrix,
                           %Expline.Vector{ n_slots: n_slots } = vector)
  when n_rows == n_slots do
    if upper_triangular?(matrix) do
      sln_buffer = (1..n_rows) |> Enum.reduce({}, fn (_, t) -> Tuple.append(t, 0.0) end)
      solution = do_backward_substitution(matrix, vector, n_rows - 1, sln_buffer)
      {:ok, solution}
    else
      {:error, :not_upper_triangular}
    end
  end
  def backward_substitution(%Expline.Matrix{}, %Expline.Vector{}), do: {:error, :dimension_mismatch}

  defp do_backward_substitution(_matrix, _vector, -1, solution) do
    Expline.Vector.construct(tuple_size(solution), fn
       (i) -> elem(solution, i)
    end)
  end
  defp do_backward_substitution(matrix, vector, nth_row, solution) do
    summation = for i <- nth_row..(matrix.n_rows-1),
                    i >= 0,
                    i <= matrix.n_rows do
      at(matrix, nth_row, i) * elem(solution, i)
    end |> Enum.sum
    new_solution = (Expline.Vector.at(vector, nth_row) - summation) / at(matrix, nth_row, nth_row)
    do_backward_substitution(matrix, vector, nth_row - 1, put_elem(solution, nth_row, new_solution))
  end

  @spec disaugment(Expline.Matrix.t()) :: {:ok, {Expline.Matrix.t(), Expline.Vector.t()}}
                                        | {:error, :dimension_mismatch}
  def disaugment(%Expline.Matrix{ n_rows: n_rows, m_cols: m_cols, internal: internal } = matrix)
  when m_cols > 1 do
    augment = Expline.Vector.construct(n_rows, fn
      (i) ->
        at(matrix, i, m_cols-1)
    end)
    disaugmented_matrix = construct(n_rows, m_cols - 1, fn
       (i, j) ->
         at(matrix, i, j)
    end)

    {:ok, disaugmented_matrix, augment}
  end
end

defimpl Inspect, for: Expline.Matrix do
  import Inspect.Algebra

  def inspect(%Expline.Matrix{ internal: internal }, opts) do
    internal
    |> Tuple.to_list
    |> Enum.map(&(to_doc(&1, opts)))
    |> Enum.intersperse(break("\n"))
    |> concat
  end
end
