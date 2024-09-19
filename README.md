# A Maxima package for operator algebra

This is a package for handling operator algebra within the Maxima Computer Algebra System, with a particular emphasis on quantum mechanical operators.

### Installation

To install the `operator_algebra` package:

1. Copy the `operator_algebra` folder to a location where Maxima can find it.
2. If needed, add new entries to the `file_search_maxima` and `file_search_lisp` lists so that Maxima can locate the package.
3. Load the package manually using the following command:

```
(%i1)  load("operator_algebra")$
```

### Documentation

The user documentation is located in the doc folder. Additionally, a demo file is included. To run this demo, append the path to the `operator_algebra` package to `file_search_demo`. Now you 
should be able to run the demo using the commands:
```
(%i1) load(operator_algebra)$
(%i2) batch(demo_qm_operators,'demo);
```

### Basic usage

Operator composition is denoted by `.`, which is Maxima's generic noncommutative multiplication operator, and the composition of an operator with itself is denoted by `^^`, which
is  Maxima's generic noncommutative exponentiation operator.

To declare that a symbol is an operator, use the Maxima function `declare`. Additionally, we will declare some constants:

```
(%i1)	declare([f,g],operator)$
(%i2)	declare([α,β], constant)$
```
Now we can compute some commutators:
```
(%i3)	commutator(α*f, β*g);
(%o3)	α*β*(f . g) - α*β*(g . f)

(%i4)	commutator(f, f);
(%o4)	0

(%i5)	commutator(f,f^^2);
(%o5)	0

(%i6)	commutator(f + g, g);
(%o6)	-(g . (g + f)) + g^^2 + f . g

(%i7)	expand(%);
(%o7)	f . g - g . f
```
In the last example, you need to manually apply expand to simplify the result.
Alternatively, the user can automatically expand results by setting the option variable dotdistrib to `true:`
```
(%i8)	block([dotdistrib : true], commutator(f + g, g));
(%o8)	f . g - g . f
```

To assign a formula to an operator,  use `put`. For example, to define an operator `Dx`
that differentiates with respect to `x` and an operator `X` that multiplies an expression 
by `x`, we first need to declare `Dx` and `X` as operators. Next, we define the functions for these operators with put:
```
(%i1)	declare(Dx, operator, X, operator)$

(%i2)	put(Dx, lambda([q], diff(q,x)), 'formula);
(%o2)	lambda([q],'diff(q,x,1))

(%i3)	put(X, lambda([q],  x*q), 'formula);
(%o3)	lambda([q],x*q)
```
The function `operator_apply` applies a function to an argument:
```
(%i4)	operator_apply(Dx, x^2);
(%o4)	Dx(x^2)
```
To use the formula for `Dx`, apply the function `operator_express`:
```
(%i5)	operator_express(%);
(%o5)	2*x
```
Here is an example that uses both operators `Dx` and `X`:
```
(%i6)	operator_apply(X.Dx.X, x^2);
(%o6)	X(Dx(X(x^2)))

(%i7)	operator_express(%);
(%o7)	3*x^3
```

In output `(%o6)` above, we see that `operator_apply` effectively changes the dotted notation
for function composition (in this case `X.Dx.X` to traditional parenthesized function
notation `X(Dx(X(x^2)))`. The traditional notation allows the use of the `simplifying` 
package to define operators as simplifying functions. 

We will start by loading the simplifying package and defining predicates that detect if the main operator
of an expression is `Dx` or `X`. 
```
(%i1)	load(simplifying)$

(%i2)	Dx_p(e) := not mapatom(e) and inpart(e,0) = 'Dx$

(%i3)	X_p(e) := not mapatom(e) and inpart(e,0) = 'X$
```
After that, we can define a simplification function for `Dx` that applies the rule 
`Dx . X = Dx + X . Dx`. Our code is
```
(%i4)	simp_Dx (e) := block([],
	    /* Dx . X = Dx + X.Dx */
	    if X_p(e) then (
	        Dx(first(e)) + X(Dx(first(e))))
	   else simpfuncall(Dx,e))$
	    
(%i5)	simplifying('Dx, 'simp_Dx)$
```
Some simple examples:
```
(%i6)	operator_simp(Dx . X);
(%o6)	X . Dx + Dx

(%i7)	block([dotdistrib : true], operator_simp(Dx . X^^2 - X^^2 . Dx));
(%o7)	2*(X . Dx) + Dx
```


