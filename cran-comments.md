## Resubmission (1.1.0 → 1.1.1)

Changes requested by CRAN auto-check:

* Bumped version to 1.1.1 (1.1.0 was already on CRAN)
* Revised Title so it no longer starts with the package name
* Quoted technical terms in DESCRIPTION ('PRIDIT', 'RIDITs', 'autoplot',
  'coef', 'tidymodels') to suppress spell-check warnings

## Test environments

* Local: macOS Tahoe, R 4.5.2
* win-builder: R-devel (2026-05-28 r90085) — 0 errors, 0 warnings, 2 notes
* win-builder: R-release — 0 errors, 0 warnings, 2 notes

## R CMD check results

0 errors | 0 warnings | 2 notes

Notes:
* "unable to verify current time": intermittent NTP issue on the check
  server, not related to the package
* "Non-standard file found at top level: Clone1": resolved in this
  submission (directory removed)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Changes since version 1.1.0

This is a patch release fixing CRAN submission issues only. No changes
to package functionality.
