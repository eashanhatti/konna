# References and Inspiration

This is a list of prior art that have influenced Konna's design and implementation in major ways.

### Two Level Type Theory and Staging

[Two-Level Type Theory and Applications](https://arxiv.org/pdf/1705.03307.pdf)
> We define and develop two-level type theory (2LTT), a version of Martin-Löf type theory which combines two different type theories.

[Staged](https://github.com/AndrasKovacs/staged)
> Experimental staged language with dependent types

[MetaML: Multi-Stage Programming with Explicit Annotations](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.422&rep=rep1&type=pdf)
> We introduce MetaML, a practically-motivated, statically typed multi-stage programming language. MetaML allows the programmer to construct, combine, and execute code fragments in a type-safe manner.

### Contextual Type Theory

[Moebius: Metaprogramming using Contextual Types](https://arxiv.org/abs/2111.08099)
> We describe the foundation of the metaprogramming language, Moebius, which supports the generation of polymorphic code and, more importantly the analysis of polymorphic code via pattern matching.

[Contextual Modal Type Theory](https://www.cs.cmu.edu/~fp/papers/tocl07.pdf)
> Contextual modal type theory provides an elegant, uniform foundation for understanding meta-variables and explicit substitutions. We sketch some applications in functional programming and logical frameworks

### Elaboration

[Elaborating dependent (co)pattern matching](https://dl.acm.org/doi/pdf/10.1145/3236770)
> We present an algorithm elaborating definitions by dependent copattern matching to a core language with inductive datatypes, coinductive record types, an identity type, and constants defined by well-typed case trees.

[Elaboration Zoo](https://github.com/AndrasKovacs/elaboration-zoo)
> Minimal implementations for dependent type checking and elaboration

### Similar Projects
Projects exploring a similar design space - leveraging the compile time vs runtime separation - but from perspectives other than that of two level type theory.

[Basil](https://github.com/basilTeam/basil)
> Lisp dialect featuring highly flexible syntax, arbitrary compile-time evaluation, and static types!

[Spiral](https://github.com/mrakgr/The-Spiral-Language)
> Functional language with intensional polymorphism and first-class staging.

### Structured Editing

[Sapling](https://github.com/kneasle/sapling)
> A highly experimental code editor where you edit code, not text.

[Alfa](https://cth.altocumulus.org/~hallgren/Alfa/)
> Alfa is a successor of the proof editor ALF, i.e., an editor for direct manipulation of proof objects in a logical framework based on Per Martin-Löf's Type Theory.

[Hazel](https://hazel.org/)
> Hazel is a live functional programming environment that is able to typecheck, manipulate, and even run incomplete programs, i.e. programs with holes.