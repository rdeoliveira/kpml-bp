*** Introduction ***

The Hal script was implemented approximately at the same time as the first implementation of the BR-Portuguese grammar for KPML. The purpose of Hal is to enable easy text-form implementation of grammars for KPML. Grammars in Hal have less of a code aspect, which invites non-programmers to implement grammars for KPML. When raw source code is needed, for example, for implementing inquiry implementations, Hal uses Java, a modern language. Hal has a 'translator', which compiles the entire work into KPML's coding language: LISP. Thus, this grammar was the first 'from-scratch' test for the script, which revealed some incompatibilities between the original implementation of the script (Hal) and the target generation system (KPML). These are listed below.

*** Understanding Hal ***

What does each file do, once the Hal package is loaded?

1) RunExamples.java

The (only non-script) Java file that launches generation within a Java IDE (e.g. Eclipse). It also contains the inquiry implementations (in Java).

2) Texting.hals

Contains SPL expressions, i.e. KPML's .examples file.

3) Wording.hals

Substitutes both the .systems and .choosers files of KPML.

4) Meaning.hals

Substitutes the .inquiries files of KPML.

*** From Hal to KPML ***

Some features that were allowed in Hal were not necessarily allowed in the original KPML; also, translation from Hal or Java to LISP was not flawless.

* Features *

1) Hal permitted lexicalization of non-classified functions; these were all generated with [NIL] in KPML. This was fixed by classifying functions and including features in lexical items.

2) An idea for Hal/KPML in the future: itt will be *extremely* helpful for debugging, if there is a debugging window, where the association table gets updated after each step! Many times, the problem in implementing grammars is knowing what semantics is passed on to inquiries.

* Bugs *

1) Because of the irrelevance of function classification in Hal, when we came to KPML, classifications had to be included as realization statements of some systems. If a system's chooser requires lexicalization from the SPL (with term-resolution), classification takes place after lexicalization, which returns a warning, but does not stop generation.

2) Hal permitted easy implementation of new SPL variables, such as 'actor', 'hasModification' etc. In KPML, a new variable 'quantity' provokes an 'invalid keyword' warning, although it does not stop generation.
