1. Motivation
    - We do this why? Because we can?
- Ohua
    - Stateful functions = atomic units of code
    - Algorithm decomposition
- Yauhau
    - Base transformation
    - Naivety explained
    - Motivate coming chapter
- If transformation
- smap transformation
- Context generalisation
    - Full transform
        - Finding order
        - stepwise stateful transformations
- Transformation implementation decisions
    - naivety and simplicity
        - redundant operators
    - subsequent IR optimisations
        - Find patterns of redundant operators and remove them
- ? If semantics and differences to Haxl
    - Branches generally computed *after* the condition
    - can `seq` be used to force precomputing? (does it lead to a dependency circle? it shouldn't, because afaik the if transform inserts `value` operators on the branches if they consist of just a binding)

        ```clojure
        (defalgo does-this-crash? []
          (let [data1 (compute-data)
                data2 (compute-data)
                mycond (seq (compute-cond) data1 data2)]
            (if mycond data1 data2)))
        ```

- ? Subgraph prebatching optimisations for if and smap
- ? Subgraph batching for handling of arbitrary loops
- Code generator expansions
- Experiments
