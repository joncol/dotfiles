---
applyTo: "doc/changelog.d/*.yaml"
---
Create a changelog file containing `section`, `title`, and `description`.

The file itself should be named according to the ticket number, which can be
found via the first commit description on the current branch.

With `jujutsu`, this can be found using:

`jj log -r@ -T builtin_log_detailed`

For the `section`, use one of the sections names that can be found in the file
`doc/changelog.d/CHANGELOG-template.md.j2`.

For smaller tickets, `description` is not necessary.

Please limit line length in descriptions at 80 characters.
