# Release checklist

Steps for cutting a Daybreak release, in order.

## Before tagging

- [ ] `DAYBREAK_VERSION` in `R/version.R` matches the version you are cutting.
- [ ] `DESCRIPTION` and `CITATION.cff` carry the same version.
- [ ] `CHANGELOG.md` has a section headed with that version and today's date.
- [ ] `source("tests/run_tests.R")` passes with the full dependency set installed.
- [ ] `docs/METHOD_AUDIT.md` names this release and its test column is current.
- [ ] The app opens, the three landing samples run, and the downloads produce files.
- [ ] `docs/screenshots/screenshot-operator-1.png` shows this version in the sidebar footer.

## Tag and push

```sh
git add -A
git commit -m "Daybreak <version>: <one line summary>"
git push
git tag -a v<version> -m "Daybreak <version>"
git push origin v<version>
```

Pushing the tag starts the release workflow, which drafts the release page.
The body comes from `docs/release-notes/<version>.md` when that file exists,
and from the matching `CHANGELOG.md` section otherwise.

## Release page

Title: `Daybreak <version>`

Body: a release that introduces the project to new readers needs a real
introduction, since the changelog entry reads as a diff against something they
have not seen. Write it in `docs/release-notes/<version>.md` before tagging and
the workflow uses it as the body. Smaller releases can leave that file out and
let the changelog section stand.

Attach `daybreak.zip` so people can download a working copy without cloning.

## After publishing

- [ ] The Releases sidebar shows the new version.
- [ ] The attached zip unpacks and runs.
- [ ] `git ls-remote --tags origin` lists the tag.
