defmodule Expline.Mixfile do
  use Mix.Project

  @app :explinex
  @version "0.2.4"

  def project do
    [
      app: @app,
      version: @version,
      elixir: "~> 1.8",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Hex parameters
      description: description(),
      package: package(),

      # Type-checking flags
      dialyzer: dialyzer(),

      # Generated documentation parameters
      name: "Expline",
      source_url: "https://github.com/am-kantox/#{@app}",
      docs: docs()
    ]
  end

  def application do
    [extra_applications: []]
  end

  def description do
    """
    A cubic spline interpolation library for Elixir.

    Includes a GenServer. All required Linear Algebra modules are built for the
    library, but may be extracted into an independent package at some point.
    """
  end

  def package do
    [
      organization: "kantox",
      name: @app,
      files: ["lib", "mix.exs", "README*", "LICENSE*"],
      licenses: ["MIT"],
      maintainers: ["Isaac Sanders", "Aleksei Matiushkin"],
      links: %{"GitHub" => "https://github.com/am-kantox/#{@app}"}
    ]
  end

  def dialyzer do
    []
  end

  def docs do
    [main: "Expline", extras: ["README.md"]]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.14", only: :dev, runtime: false},
      {:dialyxir, "~> 1.0", only: :dev, runtime: false},
      {:quixir, "~> 0.9", only: :test}
    ]
  end
end
