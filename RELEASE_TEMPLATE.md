# Release checklist

Steps for cutting a Daybreak release, in order.

## Before tagging

- [ ] `DAYBREAK_VERSION` in `R/version.R` matches the version you are cutting.
- [ ] `DESCRIPTION` and `CITATION.cff` carry the same version.
- [ ] `CHANGELOG.md` has a section headed with that version and today's date.
- [ ] `source("tests/run_tests.R")` passes with the full dependency set installed.
- [ ] `docs/CODE_REVIEW.md` states what was actually run for this release.
- [ ] The app opens, the three landing samples run, and the downloads produce files.

## Tag and push

```sh
git add -A
git commit -m "Daybreak <version>: <one line summary>"
git push
git tag -a v<version> -m "Daybreak <version>"
git push origin v<version>
```

Pushing the tag starts the release workflow, which extracts the matching
`CHANGELOG.md` section and drafts the release page.

## Release page

Title: `Daybreak <version>`

Body: the workflow fills this from the changelog. Add a short opening paragraph
above it describing what the release is for.

Attach `daybreak.zip` so people can download a working copy without cloning.

## After publishing

- [ ] The Releases sidebar shows the new version.
- [ ] The attached zip unpacks and runs.
- [ ] `git ls-remote --tags origin` lists the tag.
