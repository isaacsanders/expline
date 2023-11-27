defmodule Expline.Matrix do
  @moduledoc false

  @enforce_keys [:n_rows, :m_cols, :internal]
  defstruct [:n_rows, :m_cols, :internal]

  @type t() :: %__MODULE__{n_rows: pos_integer(), m_cols: pos_integer(), internal: internal()}

  @type vector() :: Expline.Vector.t()

  @typep internal() :: tuple()
  @typep binary_op() :: (float(), float() -> float())
  @typep unary_op() :: (float() -> float())

  @spec zeros(pos_integer(), pos_integer()) :: __MODULE__.t()
  def zeros(n_rows, m_cols) do
    construct(n_rows, m_cols, fn _, _ -> 0.0 end)
  end

  @spec identity(pos_integer()) :: __MODULE__.t()
  def identity(n) do
    construct(n, n, fn
      i, i -> 1.0
      _i, _j -> 0.0
    end)
  end

  @spec sub(__MODULE__.t(), __MODULE__.t()) ::
          __MODULE__.t() | {:error, :dimension_mismatch}
  def sub(%__MODULE__{} = a, %__MODULE__{} = b), do: do_binary_op(a, b, &Kernel.-/2)

  @spec add(__MODULE__.t(), __MODULE__.t()) ::
          __MODULE__.t() | {:error, :dimension_mismatch}
  def add(%__MODULE__{} = a, %__MODULE__{} = b), do: do_binary_op(a, b, &Kernel.+/2)

  @spec do_binary_op(__MODULE__.t(), __MODULE__.t(), binary_op()) ::
          __MODULE__.t() | {:error, :dimension_mismatch}
  defp do_binary_op(
         %__MODULE__{n_rows: n_rows, m_cols: m_cols} = a,
         %__MODULE__{n_rows: n_rows, m_cols: m_cols} = b,
         op
       )
       when is_function(op, 2) do
    construct(n_rows, m_cols, fn
      i, j -> op.(at(a, i, j), at(b, i, j))
    end)
  end

  defp do_binary_op(%__MODULE__{}, %__MODULE__{}, _op),
    do: {:error, :dimension_mismatch}

  @spec scale(__MODULE__.t(), float()) :: __MODULE__.t()
  def scale(%__MODULE__{} = matrix, scalar)
      when is_float(scalar) do
    transform(matrix, &(scalar * &1))
  end

  @spec transform(__MODULE__.t(), unary_op()) :: __MODULE__.t()
  def transform(%__MODULE__{n_rows: n_rows, m_cols: m_cols} = matrix, op)
      when is_function(op, 1) do
    construct(n_rows, m_cols, fn
      i, j ->
        matrix |> at(i, j) |> op.()
    end)
  end

  @spec construct(pos_integer(), pos_integer(), (non_neg_integer(), non_neg_integer() -> float())) ::
          __MODULE__.t()
  def construct(n_rows, m_cols, elem_fn)
      when n_rows > 0 and
             m_cols > 0 and
             is_function(elem_fn, 2) do
    internal =
      0..(n_rows - 1)
      |> Enum.reduce({}, fn i, matrix ->
        row =
          0..(m_cols - 1)
          |> Enum.reduce({}, fn j, row ->
            Tuple.append(row, elem_fn.(i, j))
          end)

        Tuple.append(matrix, row)
      end)

    %__MODULE__{n_rows: n_rows, m_cols: m_cols, internal: internal}
  end

  @spec at(__MODULE__.t(), non_neg_integer(), non_neg_integer()) :: float()
  def at(%__MODULE__{n_rows: n_rows, m_cols: m_cols, internal: internal}, i, j)
      when is_integer(i) and
             i < n_rows and
             is_integer(j) and
             j < m_cols do
    internal
    |> elem(i)
    |> elem(j)
  end

  @spec transpose(__MODULE__.t()) :: __MODULE__.t()
  def transpose(%__MODULE__{} = matrix) do
    construct(matrix.m_cols, matrix.n_rows, fn
      i, j -> at(matrix, j, i)
    end)
  end

  @spec symmetric?(__MODULE__.t()) :: boolean()
  def symmetric?(%__MODULE__{} = matrix) do
    matrix == transpose(matrix)
  end

  @spec lower_triangular?(__MODULE__.t()) :: boolean()
  def lower_triangular?(%__MODULE__{n_rows: n_rows, m_cols: m_cols} = matrix) do
    for i <- 0..(n_rows - 1), j <- 0..(m_cols - 1), i < j do
      at(matrix, i, j)
    end
    |> Enum.all?(fn
      floating_zero when floating_zero in [+0.0, -0.0] -> true
      _ -> false
    end)
  end

  @spec upper_triangular?(__MODULE__.t()) :: boolean()
  def upper_triangular?(%__MODULE__{n_rows: n_rows, m_cols: m_cols} = matrix) do
    for i <- 0..(n_rows - 1), j <- 0..(m_cols - 1), i > j do
      at(matrix, i, j)
    end
    |> Enum.all?(fn
      floating_zero when floating_zero in [+0.0, -0.0] -> true
      _ -> false
    end)
  end

  @spec positive_definite?(__MODULE__.t()) :: boolean()
  def positive_definite?(%__MODULE__{n_rows: n, m_cols: n} = matrix) do
    case cholesky_decomposition(matrix) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @spec cholesky_decomposition(__MODULE__.t()) ::
          {:ok, __MODULE__.t()}
          | {:error, :not_square}
          | {:error, :not_symmetric}
          | {:error, :not_positive_definite}
  def cholesky_decomposition(%__MODULE__{n_rows: n, m_cols: n} = matrix) do
    if symmetric?(matrix) do
      l_internal =
        0..(n - 1)
        |> Enum.reduce_while(matrix.internal, fn i, mat_l ->
          row_a_i = elem(matrix.internal, i)
          row_l_i = elem(mat_l, i)

          new_row =
            0..(n - 1)
            |> Enum.reduce_while(row_l_i, fn j, row_l_i ->
              cond do
                i == j ->
                  summation =
                    for k <- 0..(j - 1), k >= 0, k <= j - 1 do
                      l_jk = elem(row_l_i, k)
                      :math.pow(l_jk, 2)
                    end
                    |> Enum.sum()

                  a_jj = elem(row_a_i, j)

                  case a_jj - summation do
                    value when value < 0.0 ->
                      {:halt, {:error, :not_positive_definite}}

                    value ->
                      new_row = put_elem(row_l_i, j, :math.sqrt(value))
                      {:cont, new_row}
                  end

                i > j ->
                  summation =
                    for k <- 0..(j - 1), k >= 0, k <= j - 1 do
                      row_l_j = elem(mat_l, j)
                      l_ik = elem(row_l_i, k)
                      l_jk = elem(row_l_j, k)
                      l_ik * l_jk
                    end
                    |> Enum.sum()

                  a_ij = elem(row_a_i, j)
                  l_jj = mat_l |> elem(j) |> elem(j)
                  new_row = put_elem(row_l_i, j, (a_ij - summation) / l_jj)
                  {:cont, new_row}

                # i < j
                true ->
                  {:cont, put_elem(row_l_i, j, 0.0)}
              end
            end)

          case new_row do
            {:error, :not_positive_definite} ->
              {:halt, new_row}

            _row ->
              {:cont, put_elem(mat_l, i, new_row)}
          end
        end)

      case l_internal do
        {:error, :not_positive_definite} ->
          {:error, :not_positive_definite}

        _ ->
          {:ok, %{matrix | internal: l_internal}}
      end
    else
      {:error, :not_symmetric}
    end
  end

  def cholesky_decomposition(%__MODULE__{}), do: {:error, :not_square}

  @spec product(__MODULE__.t(), __MODULE__.t()) ::
          {:ok, __MODULE__.t()}
          | {:error, :dimension_mismatch}
  def product(
        %__MODULE__{n_rows: a_rows, m_cols: a_cols, internal: a_internal},
        %__MODULE__{n_rows: b_rows, m_cols: b_cols} = b
      )
      when a_cols == b_rows do
    b_internal = transpose(b).internal

    c =
      construct(a_rows, b_cols, fn
        i, j ->
          as = elem(a_internal, i) |> Tuple.to_list()
          bs = elem(b_internal, j) |> Tuple.to_list()

          Enum.zip(as, bs)
          |> Enum.map(fn {a_ik, b_kj} -> a_ik * b_kj end)
          |> Enum.sum()
      end)

    {:ok, c}
  end

  def product(%__MODULE__{}, %__MODULE__{}), do: {:error, :dimension_mismatch}

  @spec forward_substitution(__MODULE__.t(), vector()) ::
          {:ok, vector()}
          | {:error, :dimension_mismatch}
          | {:error, :not_lower_triangular}
  def forward_substitution(
        %__MODULE__{n_rows: n_rows} = matrix,
        %Expline.Vector{n_slots: n_slots} = vector
      )
      when n_rows == n_slots do
    if lower_triangular?(matrix) do
      solution = do_forward_substitution(matrix, vector, 0, {})
      {:ok, solution}
    else
      {:error, :not_lower_triangular}
    end
  end

  def forward_substitution(%__MODULE__{}, %Expline.Vector{}), do: {:error, :dimension_mismatch}

  @spec do_forward_substitution(__MODULE__.t(), vector(), integer(), tuple()) :: vector()
  defp do_forward_substitution(_matrix, %Expline.Vector{n_slots: n_slots}, _row, solution)
       when n_slots == tuple_size(solution) do
    Expline.Vector.construct(tuple_size(solution), fn
      i -> elem(solution, i)
    end)
  end

  defp do_forward_substitution(matrix, vector, nth_row, solution) do
    summation =
      for i <- 0..(nth_row - 1), i >= 0, i <= nth_row - 1 do
        at(matrix, nth_row, i) * elem(solution, i)
      end
      |> Enum.sum()

    new_solution = (Expline.Vector.at(vector, nth_row) - summation) / at(matrix, nth_row, nth_row)
    do_forward_substitution(matrix, vector, nth_row + 1, Tuple.append(solution, new_solution))
  end

  @spec backward_substitution(__MODULE__.t(), vector()) ::
          {:ok, vector()}
          | {:error, :dimension_mismatch}
          | {:error, :not_upper_triangular}
  def backward_substitution(
        %__MODULE__{n_rows: n_rows} = matrix,
        %Expline.Vector{n_slots: n_slots} = vector
      )
      when n_rows == n_slots do
    if upper_triangular?(matrix) do
      sln_buffer = 1..n_rows |> Enum.reduce({}, fn _, t -> Tuple.append(t, 0.0) end)
      solution = do_backward_substitution(matrix, vector, n_rows - 1, sln_buffer)
      {:ok, solution}
    else
      {:error, :not_upper_triangular}
    end
  end

  def backward_substitution(%__MODULE__{}, %Expline.Vector{}), do: {:error, :dimension_mismatch}

  @spec do_backward_substitution(__MODULE__.t(), vector(), integer(), tuple()) :: vector()
  defp do_backward_substitution(_matrix, _vector, -1, solution) do
    Expline.Vector.construct(tuple_size(solution), fn
      i -> elem(solution, i)
    end)
  end

  defp do_backward_substitution(matrix, vector, nth_row, solution) do
    summation =
      for i <- nth_row..(matrix.n_rows - 1),
          i >= 0,
          i <= matrix.n_rows do
        at(matrix, nth_row, i) * elem(solution, i)
      end
      |> Enum.sum()

    new_solution = (Expline.Vector.at(vector, nth_row) - summation) / at(matrix, nth_row, nth_row)

    do_backward_substitution(
      matrix,
      vector,
      nth_row - 1,
      put_elem(solution, nth_row, new_solution)
    )
  end

  @spec disaugment(__MODULE__.t()) ::
          {:ok, {__MODULE__.t(), vector()}}
          | {:error, :dimension_mismatch}
  def disaugment(%__MODULE__{n_rows: n_rows, m_cols: m_cols} = matrix)
      when m_cols > 1 do
    augment =
      Expline.Vector.construct(n_rows, fn
        i ->
          at(matrix, i, m_cols - 1)
      end)

    disaugmented_matrix =
      construct(n_rows, m_cols - 1, fn
        i, j ->
          at(matrix, i, j)
      end)

    {:ok, {disaugmented_matrix, augment}}
  end

  def disaugment(%__MODULE__{}), do: {:error, :dimension_mismatch}
end

defimpl Inspect, for: Expline.Matrix do
  import Inspect.Algebra

  def inspect(%Expline.Matrix{internal: internal}, opts) do
    internal
    |> Tuple.to_list()
    |> Enum.map(&to_doc(&1, opts))
    |> Enum.intersperse(break("\n"))
    |> concat
  end
end
