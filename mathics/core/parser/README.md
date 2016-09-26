# Notes on the Mathics parser

## Introduction

The Mathics parser is an operator precedence parser that implements the
precedence climbing method.

The Wolfram language is surprisingly complex and has quite a few subtleties;
this document attempts to cover the major ones. The language is documented on
[the Wolfram website](https://reference.wolfram.com/language/tutorial/OperatorInputForms.html)
although there are a few errors.

One primary feature of the Wolfram language is a large number of operators with
precedences. It is this characteristic that makes operator precedence parsing
well suited to the language. There are however a few special language features
that cannot be parsed by the standard precedence climbing algorithm.

## Precedence Climbing

Instead of giving an introduction to  precedence climbing, here are two
references that explain the algorithm better than I could:
- [Wikipedia: Operator-precedence parser](https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method)
- [Eli Bendersky's blog entry: Parsing expressions by precedence climbing](http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing)

### Precedence

This algorithm has a natural application to the Wolfram language. Every
operator has an inbuilt precedence level which can be accessed with e.g.
`Precedence[Plus]`.

### Grouping

In addition to precedences, operators in the Wolfram language have a grouping
attribute. Consider trying to parse the code snippet `a + b + c`. There is
some ambiguity in what order the operations should be applied. Should we use
left associativity `Plus[Plus[a + b] + c]` or right associativity
`Plus[a, Plus[b + c]]`?

#### Flat associativity

In fact the addition operator is usually considered fully associative, that is
`Plus[a, b, c]`. In the Wolfram language this is referred to as 'flat'
associativity because the expression tree is flat.

Flat associativity is not a large issue in practice as the as flat operators
can be first treated as left or right associative, whichever is more efficient,
and flattened once the AST is constructed.

One thing to be aware of is that parenthesis prevent the flattening of
expressions at parse time, `a * (b * c)` parses as `Times[a, Times[b, c]]` but
is flattened immediately afterwards.

#### Non-associativity
In addition to left/right associativity some operators are non-associative.
That is, an operator `?` is non-associative if `a ? b ? c` is not valid syntax.

The only non associative operator in the Wolfram language is `PatternTest`.

#### Implementation of binary operators

The relevant code snippet for binary operators is:


```python
def parse_binary(self, expr1, token, p):    # called with expr1
    'expr <- expr1 BINARY expr2'
    tag = token.tag                         # name of BINARY token

    q = binary_ops[tag]                     # lookup precedence of operator
    if q < p:
        return None

    self.consume()                          # consume BINARY token

    if tag not in right_binary_ops:         # left/right grouping
        q += 1

    expr2 = self.parse_exp(q)               # parse expr2

                                            # handle nonassoc operators
    if tag in nonassoc_binary_ops and expr1.get_head_name() == tag and not expr1.parenthesised:
        self.tokeniser.sntx_message(token.pos)
        raise InvalidSyntaxError()

    result = Node(tag, expr1, expr2)        # construct the result: `BINARY[expr1, expr2]`

    if tag in flat_binary_ops:              # flatten the tree if required
        result.flatten()

    return result
```

#### Special case: Implicit times
In the Wolfram language the expression `a b` should parse as `Times[a b]`. This
seemingly simple rule actually creates lots of headaches.

In the context of precedence climbing we use the rule `E_n <- E_n E_n` where
`n = 400` is the precedence of `Times`.

### Unary operators

The Wolfram language has two types of unary operators, prefix and postfix. As
the names suggest, prefix operators come before the expression, e.g. `- 2` and
postfix operators come after e.g. `10 !`.

#### Special case: unary minus
Consider the code `- - a`.  One might expect that this parses as
`Minus[Minus[a]]` but in fact the unary minus is handled at parse time as `-x`
becomes `Times[-1, x]`. One might expect that the answer is
`Times[-1, Times[-1, a]]` but in fact this `Times` is flattened at parse time
and the answer is `Times[-1, -1, a]`. We can recover the expected answer by
imposing parenthesis to prevent flattening, that is `-(-a)`.

#### Special case: unary plus
Unary plus on the other hand is ignored. `+a` is the same syntactically as `a`.

### Ternary operators

The last type of standard operator in the Wolfram language are ternary
operators. There are two ternary operators, `Infix` and `Span` and both are
special cases. In general, they are treated like binary operators.

#### Special case: Infix

The simplest ternary operator in the Wolfram language is `Infix`, for example,
`a ~ b ~ c` becomes `b[a, c]`. We treat this like a binary operator but
override the rule to consume an extra `~ E`. The relevant annotated code
snippet is:


```python
def e_Infix(self, expr1, token, p):     # called with expr1
    'expr <- expr1 '~' expr2 '~' expr3'
    q = ternary_ops['Infix']            # lookup precedence of Infix
    if q < p:
        return None
    self.consume()                      # consume first '~'
    expr2 = self.parse_exp(q + 1)       # consume expr2
    self.expect('Infix')                # consume second '~'
    expr3 = self.parse_exp(q + 1)       # consume expr3
    return Node(expr2, expr1, expr3)    # return expr2[expr1, expr3]
```

#### Special case: Span

See the section on backtracking.

### Special case: Integrate

The `Integrate` rule is `E <- Integrate expr1 DifferentialD expr2`. Similarly
this can be handled by treating `Integrate` as a prefix operator that consumes
an extra token and expression. Unlike `Infix`, the precedences for the inner
(`expr1`) and outer (`expr2`) expressions differ.

To quote the Wolfram docs:

> Forms such as `'Integral' expr1 'DifferentialD' expr2` have an "outer"
precedence just below `Power`, as indicated in the table above, but an
"inner" precedence just above `Sum`. The outer precedence determines when
`expr2` needs to be parenthesized; the inner precedence determines when
`expr1` needs to be parenthesized.

This is simple to handle with precedence climbing:

```python
def p_Integral(self, token):
    `expr <- Integral expr1 DifferentialD expr2
    self.consume()                          # consume 'Integral'

    inner_prec = all_ops['Sum'] + 1         # lookup inner
    outer_prec = all_ops['Power'] - 1       # and outer prec

    expr1 = self.parse_exp(inner_prec)      # consume expr1
    self.expect('DifferentialD')            # consume 'DifferentialD'
    expr2 = self.parse_exp(outer_prec)      # consume expr2
    return Node('Integrate', expr1, expr2)
```

## Mathics implementation

### Precedence Climbing
All the Mathics operators are specified in `mathics/core/parser/operators.py`.
The precedence climbing algorithm is implemented in
`mathics/core/parser/parser.py`. All the special cases are implemented as
additional rules.

#### P Rules

Methods beginning with `p_TAG` declare what to do when the `TAG` token is
encountered at the beginning of an expression. For example, the first token
after an open parenthesis. These rules define all the atomics `E <- A`, prefix
operators, `E <- PREFIX E` and also brackets `E <- ( E )`.

#### E Rules

Methods beginning with `e_TAG` are called when one expression is already
present and we encounter the `TAG` token. This covers binary operators
`E <- E BINARY E`, postfix operators `E <- E POSTFIX`, ternary operators,
`E <- E TERNARY1 E TERNARY2 E`, and functions `E <- E [ SEQUENCE ]`.

#### B Rules

Methods beginning with `b_TAG` are used for parsing boxes and can be ignored
on first reading of the parser code.

#### Backtracking
Most of the Wolfram language can be parsed with precedence climbing but there
are a few special language features that require something more. The `Span`
operator is one example. Both `a ;; b` and `a ;;` are valid syntax, the latter
is equivalent to `a ;; All`.

Consider the example: `a;;!b`. There are four options for parsing this:
 - `Span[a, Not[b]]`
 - `Factorial[Span[a, Null]]`
 - `Times[Span[a, All], Not[b]]`
 - `Times[Factorial[Span[a, All]], b]`

`a ! b` parses as `Times[Factorial[a], b]` which might suggest option 4 is
correct but the precedence of `Span` is lower than that of `Times` so it is
parsed first, top down. We might then think that option 1 is correct, but `Not`
has a lower precedence than `Span` so the `E <- E ;; E` rule cannot be applied
here and we must use the postfix form of `Span`. Since the precedence of
`Factorial` is higher than that of `Span` we can apply the postfix rule for `!`
and option 3 turns out to be correct.

The problem with `Span`, and also `CompoundExpression` is that they require
arbitrary lookahead to see if the right hand side has lower precedence. It's
not a large issue for `CompoundExpression` which has very low precedence but
nevertheless this language quirk requires backtracking in the parser.

## Recursive descent parsers in Python

The lack of tail call optimisation (TCO) causes some problems when writing
recursive descent (RD) parsers in python. It's possible to write RD parsers in
an iterative style but it's not nice. One can use trampolining to mitigate this
limitation. In practice Python provides 1000 recursions by default which should
be enough for most parse trees. Try parsing `1 + 2 + ... 1000`!

## References

1. http://www.antlr.org/papers/Clarke-expr-parsing-1986.pdf

   Clarke 1986, 'The top-down parsing of expressions'

2. http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm

   Recursive descent parsers, shunting yard algorithm, precedence climbing, and
   efficient parsers with large number of operator precedences.

3. http://eli.thegreenplace.net/2009/03/14/some-problems-of-recursive-descent-parsers

   How to handle right/left associativity in recursive descent parsers and how
   to write efficient of RD parsers.

4. https://reference.wolfram.com/language/tutorial/OperatorInputForms.html

   MMA docs with grouping and relative precedences.

5. http://home.pipeline.com/~hbaker1/CheneyMTA.html

   Henry Baker 1994, 'CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.[1]',
   unpublished note on implementing TCO with trampolines.
