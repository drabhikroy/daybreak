# Release identity ---------------------------------------------------------
#
# Four places need the version string: the sidebar footer, the session file
# header, the Markdown and bundle report footers, and the Rmd template. The
# constant lives in its own file so that app.R, the report template, and the
# test helper all read the same value without any of them sourcing the others,
# and so a release only ever changes one line.

DAYBREAK_VERSION <- "0.10.1"

# Shown beside the version in the sidebar footer and the session panel.
DAYBREAK_RELEASE_DATE <- "2026-08-02"
