# Font system and typography research

## Current Cat font model

The implementation in [`modules/ui/+font.el`](../modules/ui/+font.el)
separates physical font fallback, semantic typography, and mode-specific
application.  It does not use `face-font-family-alternatives` as configuration
input.

### Font stacks and roles

`cat-font-stacks` owns the physical candidates.  Each entry has this shape:

```elisp
(STACK
 :ascii (ASCII-FAMILY ...)
 :cjk (CJK-FAMILY ...)
 :extends PARENT-STACK)
```

`:extends` supplies properties omitted by the child.  Defining `:ascii` or
`:cjk` on the child replaces that inherited property; it does not append to
the parent's list.  This lets related stacks share CJK choices while keeping
their Latin candidates independent.  For example, `monospace-code` inherits
the CJK list from `monospace-narrow` but replaces its ASCII list.

`cat-font-preset` maps semantic roles to those stacks:

```elisp
(ROLE
 :stack STACK
 :extends PARENT-ROLE
 :fonts (PREFERRED-ASCII-FAMILY ...)
 FACE-ATTRIBUTE VALUE ...)
```

A role inherits and then overrides its parent role.  `:fonts` prepends
role-specific ASCII candidates to the selected stack; CJK candidates always
come from the stack.  Remaining properties such as `:height` and `:weight`
become face attributes.  This keeps language-specific code roles on one CJK
monospace fallback while allowing `JetBrains Mono`, `Cascadia Code`, or another
Latin family to take priority for a particular language group.

The current roles form this hierarchy:

- `default` selects the base frame font and absolute height.
- `title` extends `heading`; both use the serif stack.
- `documentation` extends `body`; `body`, `prose`, and `ui` remain separate
  reading, prose, and interface choices.
- `metadata-label`, `metadata-value`, `mono`, `code`, and `table` cover compact
  structural content.
- `code-*` and `terminal` extend their general roles and prepend a specialized
  ASCII family.

### Role fontsets

A face does not natively accept an ordered list for `:family`: the value is one
family-name string.  A fontset does accept ordered specifications for
character ranges, charsets, and scripts, and its name can be used wherever
Emacs accepts a font name.  Cat therefore compiles one named fontset per role
from three inputs:

1. The role's ordered ASCII candidates.
2. The role stack's ordered CJK candidates for Han, Kana, Hangul, Bopomofo,
   and miscellaneous CJK characters.
3. Shared `cat-font-script-rules` plus the private-use ranges owned by Nerd
   Icons.

Cat obtains the Nerd Icons ranges by temporarily intercepting the
`set-fontset-font` calls made by `nerd-icons-set-font`.  It caches those range
descriptions and applies them with `nerd-icons-font-family` to every role
fontset.  Nerd Icons remains the source of truth when its private-use ranges
change, and Cat does not maintain a duplicate range list.

The resolved inputs also form a signature.  Existing role fontsets are rebuilt
only when their signature changes.  The default fontset has a separate
signature cache because it is shared by faces outside the role system.  Font
setup runs after initialization, for new graphical frames, after theme
refreshes, and immediately when the module is loaded into an already
initialized graphical session.

Cat applies each role fontset to its role face through both `:font` and
`:fontset`.  `:font` resolves the first available ASCII candidate into the
face's concrete Latin font, while `:fontset` preserves the CJK, mathematical,
emoji, and icon mappings.  Using only `:fontset` would leave the Latin family
inherited from the default face; using only `:font` would discard the script
mappings.

See the GNU manuals on
[modifying fontsets](https://www.gnu.org/software/emacs/manual/html_node/emacs/Modifying-Fontsets.html)
and [`set-fontset-font`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Fontsets.html).
The latter can replace, prepend, or append specifications for a character
target, so multiple fonts there are a real ordered glyph fallback chain.  The
[corresponding Emacs source change](https://lists.gnu.org/archive/html/emacs-diffs/2022-04/msg00656.html)
documents the same overwrite/prepend/append semantics.

### Mode and face rules

`cat-mode-font-rules` applies the role system buffer-locally.  The first rule
whose `:modes` or `:buffer-name` matches is used:

- `:font` selects the buffer's base role or concrete family list.
- `:faces` remaps individual faces.  A face name ending in `*`, such as
  `org-level-*`, expands to every currently defined face with that prefix.
- Extra face attributes override the selected role's attributes.
- `:rescale` installs a buffer-local `face-font-rescale-alist`.

Cat records every remapping cookie and the previous rescale state, so changing
major mode or forcing a refresh removes only the settings owned by this
module.  It also leaves a `buffer-face-mode` installed by other configuration
alone.  Theme and font refreshes reapply active mode rules to live buffers.

In short: a stack answers which physical Latin and CJK fonts are available, a
role describes their semantic typography, and a mode rule decides where that
role is used.

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
