---
id: zinnia
aliases: []
tags: []
---

- literals (nums => in IR | vecs => (into) json)
- types
    - int<n> => register<n>
    ```
    x : int<8>

    let x = 8'3;

    let x: int<8> = 8'3;
    ```
    - bools  => register<1>
    - vecs - `vec<T, N>` ~= `sizeof(N * T)`
- primitives
    - scan
    - filter
    - map
- 1d conv?
