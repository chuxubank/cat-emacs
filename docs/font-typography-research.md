# Font and typography research

## Emacs font selection

A face does not natively accept an ordered list for `:family`: the value is one
family-name string.  A fontset does accept ordered font specifications for
character ranges, charsets, and scripts, and its name can be used wherever
Emacs accepts a font name.  Cat therefore keeps physical font candidates in
`cat-font-stacks` and compiles one named fontset per semantic role.  Each stack
owns both its ASCII and CJK candidates; stack inheritance shares CJK choices
between related serif or monospace stacks without a separate mapping table.

Cat applies each role fontset to a face through both `:font` and `:fontset`.
`:font` resolves the first available ASCII candidate into the face's concrete
Latin family, while the following `:fontset` attribute preserves the role's
CJK, mathematical, emoji, and other script mappings.  Using only `:fontset`
would leave the face's Latin family inherited from the default face; using
only `:font` would discard the script mappings.  See the GNU manuals on
[modifying fontsets](https://www.gnu.org/software/emacs/manual/html_node/emacs/Modifying-Fontsets.html)
and [`set-fontset-font`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Fontsets.html).
The latter can replace, prepend, or append specifications for a character
target, so multiple fonts there are a real ordered glyph fallback chain.  The
[corresponding Emacs source change](https://lists.gnu.org/archive/html/emacs-diffs/2022-04/msg00656.html)
documents the same overwrite/prepend/append semantics.

Nerd Icons uses the same path.  Cat reads the private-use character ranges
from `nerd-icons-set-font` and adds them to every role fontset, leaving the
package as the single source of truth for those evolving ranges.

Consequences for this configuration:

- Keep physical ASCII and CJK candidates together in reusable font stacks.
- Keep semantic roles responsible for selecting a stack, size, weight, and
  other typography attributes.
- Create and apply every role fontset before its scripts are first displayed,
  because Emacs can cache script font selection.
- Share mathematical and emoji rules across role fontsets, and retain the same
  rules in the default fontset for faces outside Cat's role system.

In short: a font stack answers both "which installed Latin family can fulfill
this role?" and "which font should render this character without losing that
role's serif, sans serif, or monospace character?".

## Typography principles

The [Apple typography guidance](https://developer.apple.com/design/human-interface-guidelines/typography)
recommends minimizing the number of typefaces and expressing hierarchy mainly
through size, weight, and color.  It also discourages light weights for small
text and suggests looser leading for long passages.  This supports one main
family per reading experience, with a separate monospace family only where the
content requires it.

[Google Design's font-selection guide](https://design.google/library/choosing-web-fonts-beginners-guide)
describes two reliable pairing strategies: deliberate contrast between roles,
or cohesion within a superfamily.  A distinctive display face should be
balanced by a quieter body face, and every candidate should be tested with the
actual scripts and character inventory.

[Carbon's typography system](https://carbondesignsystem.com/elements/typography/overview/)
models typography as semantic tokens rather than raw font sizes.  Its
productive styles are compact; expressive styles are larger and more
editorial.  Carbon uses regular text and semibold section headings, reserving
semibold from long body copy.  [Material 3](https://developer.android.com/develop/ui/compose/designsystems/material3)
similarly separates display, headline, title, body, and label roles.  These are
good precedents for keeping `title`, `heading`, `body`, `ui`, and `code` roles
independent of specific modes.

For prose buffers, typography is more than the family.  The
[U.S. Web Design System's typesetting guidance](https://designsystem.digital.gov/components/typography/)
targets 45-90 characters per line (66 for long text), gives long passages more
leading than headings, and places more space above a heading than below it.
Emacs supports buffer-local
[`line-spacing`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Line-Height.html),
so a practical preview scale is title `1.50-1.60`, first heading `1.25-1.30`,
second heading `1.14-1.18`, body `1.0`, with regular body text and semibold
headings.  Start long-form prose around 68-76 visible columns and `0.12-0.18`
additional line spacing; keep code denser.  These are preview values to tune,
not universal accessibility thresholds.

## Local font audit

An `fc-list` audit on this Mac on 2026-08-02 found all families used by the
preview PDFs: `Big Caslon`, `Iowan Old Style`, `DIN Condensed`, `STIX Two
Text`, `Avenir Next`, `Menlo`, and `SF Mono`.  Other useful installed families
include `STIX Two Math`, `Inter`, `Inter Display`, `SF Pro`, `Iosevka Term`,
`Iosevka Etoile`, `Iosevka Aile`, `Maple Mono`, `JetBrains Mono`, `Roboto
Serif`, `Roboto Mono`, `IBM Plex Mono`, and `LXGW WenKai`.  `IBM Plex Sans` and
`Chaparral` were not found, so they should not be primary families in PDFs
meant to represent the current machine.

## Recommended preview presets

### Artistic / editorial

- Preview PDF: `Big Caslon` medium for the title, `Iowan Old Style` for
  headings and prose, `Avenir Next` for UI labels, and `Menlo` for code and
  tables.
- Suggested hierarchy: title `1.55` medium; heading levels `1.25`, `1.15`, and
  `1.08` bold; body `1.0` regular with the loosest leading of the three sets.
- Candidate stacks: title `Big Caslon -> Iowan Old Style -> Hoefler Text`;
  prose `Iowan Old Style -> Charter -> Hoefler Text`.

This follows Google Design's deliberate-contrast strategy: an expressive
display serif is confined to a large, short title while a quieter old-style
serif carries sustained reading.  Avenir and Menlo stay in utility roles, so
the reading surface does not become a collage of typefaces.

### Technology / scientific

- Preview PDF: `DIN Condensed` bold for the title, `Avenir Next` for headings
  and UI labels, `STIX Two Text` for prose, and `SF Mono` for code and tables.
- Suggested hierarchy: title `1.50` bold; heading levels `1.25`, `1.15`, and
  `1.08` bold; body `1.0` regular with medium leading.
- Mathematical script in every role fontset: `STIX Two Math`, followed by
  `DejaVu Math TeX Gyre`.

The condensed industrial title gives the set a technical signal without
sacrificing long-form readability.  The
[STIX project](https://www.stixfonts.org/) is maintained to cover scientific
and engineering manuscript-to-publication needs; matching STIX text and math
gives formulas and prose a coherent scholarly color.  Avenir and SF Mono keep
editor chrome and source code neutral instead of forcing the scholarly serif
into every role.  A quieter implementation alternative is STIX Two Text for
both title and prose; an open-source code alternative is
[JetBrains Mono](https://www.jetbrains.com/lp/mono/), whose official specimen
emphasizes distinct ambiguous glyphs and code-specific ligatures.

### Modern / minimal

- Preview PDF: `Avenir Next` for title, headings, prose, and UI; `SF Mono` for
  code and tables.
- Suggested hierarchy: title `1.50` bold; headings `1.25`, `1.15`, and `1.08`
  demi-bold; body `1.0` regular with moderate leading.

This is the cohesive-family strategy.  The preset stays minimal by expressing
hierarchy through Avenir Next's scale and weight rather than introducing a
second proportional family.  `Inter Display` plus `Inter` is an installed,
open-source alternative: Inter's designer describes it as a
[screen-oriented workhorse](https://rsms.me/inter/) whose text optical design
uses a tall x-height and contrast-enhancing details, while its display design
has cleaner, more delicate forms.

### Mono / editorial

- Preview PDF: `Charter` for title and headings, `Iosevka Term` for body text,
  `Avenir Next` for UI labels, and `Maple Mono` for code and tables.
- Suggested hierarchy: title `1.50` roman; headings `1.25`, `1.15`, and `1.08`
  bold; body `1.0` regular; UI and code `0.95` regular.
- Keep long-form text near 68-76 visible cells and use slightly more leading
  than the proportional presets.

This preset treats the monospaced grid as a deliberate editorial texture
rather than merely a coding convention.  Charter interrupts the grid at
structural boundaries, Avenir keeps editor chrome quiet, and Maple Mono gives
source code a texture distinct from the Iosevka prose.  Role fontsets keep the
CJK style aligned with those semantic choices while sharing mathematical,
emoji, and symbol coverage.

## PDF comparison method

The generated previews use two identically structured pages per preset.  The
first simulates a reading buffer with title, heading, body, code, UI labels,
and a role panel.  The second compares the scale, texture, rationale, and
fallback model.  Page size, margins, content structure, and line lengths stay
fixed, while each preset receives a palette appropriate to its voice.

The PDFs focus on Latin typography.  CJK, mathematical, emoji, and symbol
coverage should be tested separately because CJK families vary by semantic
role while mathematical, emoji, and symbol fallbacks remain shared.
