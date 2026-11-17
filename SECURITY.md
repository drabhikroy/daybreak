# Security

## What Daybreak is exposed to

Daybreak is a Shiny application with two file inputs, one optional
network client on the local machine, and one optional download.
Uploaded data is read for computation, and a saved session is a
size-capped RDS bundle read back with `readRDS`, not `load`, so opening
one cannot introduce a variable into the running R session on its own.
The page and the R process run on the same computer, and the app
listens on the loopback address.

## What the code does about it

A saved session is treated as input to validate, not state to trust:
it is rejected outright above 200 MB, read with a reference hook that
refuses any external reference the file might carry, and checked
afterward against its own schema and required fields before anything
in it is used.

Session files and `.rds` or `.RData` data files are refused on R
releases before 4.4.0, where reading such a file can run code hidden
inside it (CVE-2024-27322).

Column names are data. Every name written into an exported R script is
quoted as a string or a backticked name by one pair of functions, so a
name that contains quotes or backticks cannot close its quoting and run
as code. Every name and every other value taken from the data is
escaped before it is written into a Markdown, HTML, Word, or PDF
report.

The optional local model is fixed to `http://127.0.0.1:11434` and
receives the finished explanation, not the uploaded data: a fixed set
of fields already decided by the app's own computation. A returned
rewrite is checked twice before it is shown, once for the five
required section headings and once for exact agreement between every
number in the source explanation and every number in the rewrite;
either check failing discards the rewrite outright and the
deterministic explanation stands unchanged. A reply that repeats a
heading, carries a heading from the instructions, or runs far longer
than the text it was given is treated as the model handing back its
prompt and is discarded the same way. What is shown is built
through Shiny's own tag functions, which escape text content by
construction, rather than assembled as raw HTML.

Model names are checked against one pattern before they are used. The
pattern cannot begin with a dash, so a name cannot be read as an
option, and it admits no spaces or shell characters. Models are
downloaded by starting the Ollama program with an argument vector and
no shell, and removed through the local HTTP interface.

Daybreak can keep its own copy of Ollama, and this is the only place
the app reaches the internet on its own behalf. It happens only when
the reader presses the button for it. The archive is fetched over HTTPS
from the Ollama release page on GitHub, its SHA-256 is compared with
the checksum published in the same release, and a file that does not
match is deleted without being unpacked. A matching checksum shows the
file arrived intact. It does not vouch for the release itself, which
is trusted as the vendor's. The copy is unpacked into a folder that
belongs to Daybreak, it is started listening on `127.0.0.1` only, and
removing it deletes that one folder.

The hardware check behind the model suggestions reads the operating
system, the amount of memory, the processor core count, and whether
the processor is Apple silicon or an NVIDIA tool is present. It runs
only when asked, and the values are kept for the session and sent
nowhere.

## Reporting a problem

Open a private security advisory through the repository, or open a
normal issue if the problem is not sensitive. Please include the
version, what you did, and what you saw, and, if a saved session file
is involved, one with any real data replaced, since a session file
can carry the data it was built from.

## Scope

In scope: anything that causes a saved session file to affect the
running session beyond the data it declares, that lets a local model
rewrite change a number or bypass the structure check, that renders
uploaded content or model output as unescaped HTML, that lets a column
name or a model name run as code, or that lets the private copy of
Ollama be installed from a file that fails its checksum. Out of scope:
Ollama itself, which is reported to its own maintainers, and anything
that requires an attacker to already be running code on the same
machine.
