# Security

## What Daybreak is exposed to

Daybreak is a single-file Shiny application with two file inputs and
one optional network client. Uploaded data is read for computation,
and a saved session is a size-capped RDS bundle read back with
`readRDS`, not `load`, so opening one cannot introduce a variable into
the running R session on its own.

## What the code does about it

A saved session is treated as input to validate, not state to trust:
it is rejected outright above 200 MB, read with a reference hook that
refuses any external reference the file might carry, and checked
afterward against its own schema and required fields before anything
in it is used.

The optional local model is fixed to `http://127.0.0.1:11434` and
receives the finished explanation, not the uploaded data: a fixed set
of fields already decided by the app's own computation. A returned
rewrite is checked twice before it is shown, once for the five
required section headings and once for exact agreement between every
number in the source explanation and every number in the rewrite;
either check failing discards the rewrite outright and the
deterministic explanation stands unchanged. What is shown is built
through Shiny's own tag functions, which escape text content by
construction, rather than assembled as raw HTML.

## Reporting a problem

Open a private security advisory through the repository, or open a
normal issue if the problem is not sensitive. Please include the
version, what you did, and what you saw, and, if a saved session file
is involved, one with any real data replaced, since a session file
can carry the data it was built from.

## Scope

In scope: anything that causes a saved session file to affect the
running session beyond the data it declares, that lets a local model
rewrite change a number or bypass the structure check, or that renders
uploaded content or model output as unescaped HTML. Out of scope:
Ollama itself, which is reported to its own maintainers, and anything
that requires an attacker to already be running code on the same
machine.
