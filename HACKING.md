Unless otherwise documented:

- `A`/`X`/`Y` are caller-save.
- `temp*` zero-page locations are scratch space, caller-save, and not for
use from NMI.

Edge cases to test:

- Long NKIs (5 lines of text)
- Starting new board when NKI text is showing
- Finding kitten when NKI text is showing (two adjacent NKIs)
- Two adjacent NKIs drawing text on opposite sides of screen
- Two adjacent NKIs with different numbers of text lines
- NROM build
