# AI Usage

Up to commit [`5107f82`](https://github.com/guilhermeg2k/dusk-lang/commit/5107f82f4d2a873a75f0c8d83054d3fdb7577254)
(2026-10-14), Dusk was written by hand, with AI only used for small things
(e.g. the benchmark script, which is marked as such).

**After that commit, AI tools started to be used in the development of Dusk.**

## How AI is used here

- AI is a tool, not an author. I'm responsible for every line that lands in
  the repository, whoever (or whatever) typed it.
- I review and understand every AI-assisted change before it's committed.
  If I can't explain it, it doesn't get merged.
- AI-assisted commits are marked with a `Co-Authored-By` trailer, so the
  history shows where AI was involved.
- Design decisions (syntax, semantics, roadmap) stay mine.

This approach is inspired by how Mitchell Hashimoto handles AI in
Ghostty ([AI policy](https://github.com/ghostty-org/ghostty/blob/main/AI_POLICY.md)): be open about it,
and keep a human accountable for the result.
