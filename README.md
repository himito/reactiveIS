# ReactiveIS

Reactive programming language for writing and executing interactive multimedia scenarios. The underlying 
theory of the language was published in

    Arias, J., Desainte-Catherine, M., Olarte, C., & Rueda, C. (2015). Foundations for Reliable and 
    Flexible Interactive Multimedia Scores. In T. Collins, D. Meredith, & A. Volk (Eds.), 5th 
    International Conference on Mathematics and Computation in Music, MCM 2015, London, UK, June 
    22-25, 2015 (Vol. 9110, pp. 29–41). Springer. https://doi.org/10.1007/978-3-319-20603-5_3

## Dependencies

We recommend to install the following dependencies using the package manager [opam](https://opam.ocaml.org).

```Bash
  brew install opam
```

* Ocaml
* Ocamlgraph
* Ocamllex
* Menhir

## Build
  * `make`

## Execution
  * `./reactiveIS.native examples/score.ris`

## Notes
  * Utils folder contains the necessary files for syntax highlighting.
