# The Idea

FokShell's Parser is at core - modular.
A `Node` is a class you can define on your own datatype,
it requires methods such as `parse`, `nodeLen`, `getRawData`, `makeTask` etc.
For reference, see [Parser.hs](../src/Language/Parser.hs)
FokShell's JobManager simply parser user input using provided ParserModule,
using `makeTask` to convert the Node into an appropriate task,
spawning it as a job. Currently there is not a lot built-in Nodes, or Job utils.
-# refer to roadmap for more info on what's planned.

## Parsers

FokShell chains parser in a specific way.
`parse` is a function that given a lower-level parser, generates a higher-level parser.
For example, `pcallParser` (Process Call parser) in default configuration receives
`primitiveParser` (NodeString parser) - using which it parser the executable and following arguments.
Currently, the default parser looks like this:

```hs
r0 = primitives empty -- no lower level than primitive, thus pass in empty
r1 = pcall r0 <|> r0  -- r1 is the process call parser
r2 = pipes r1 <|> r1  -- r2 adds pipes to the chain
r3 = chains r2 <|> r2 -- r3 adds chain expressions, continuing the parser chain
```

## The Defaults

Currently FokShell comes with these Nodes:

* ChainExp - such as `&&`, `;` or `||`. Consists of 2 nodes,
* PipeExp - such as `>>`, `>`, `>2`, `>2>`, `<` or `|`. Consists of 2 nodes,
* ProcessCall - call to a process consisting of an executable and arguments,
* Array - not yet supported/TBD,
* Primitive - the most basic element - strings.
