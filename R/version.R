# Release identity ---------------------------------------------------------
#
# Four places need the version string: the sidebar footer, the session file
# header, the Markdown and bundle report footers, and the Rmd template. An
# earlier release kept the number in app.R under a different name, so the
# exports and the session writer referred to a constant that was never
# defined and every one of those downloads failed at the point of use. The
# constant lives in its own file so that app.R, the report template, and the
# test helper can all read the same value without any of them sourcing the
# others.

DAYBREAK_VERSION <- "0.10.0"

# Shown beside the version in the sidebar footer and the session panel.
DAYBREAK_RELEASE_DATE <- "2026-08-02"
