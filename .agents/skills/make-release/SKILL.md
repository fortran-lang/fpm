---
name: make-release
description: >
  Make an fpm release end-to-end: check that the milestone is done and the
  tip of main is green, bump the version in a release PR tested by a fork
  release, publish the vX.Y.Z GitHub release with generated notes, download
  badges and the Discourse link, verify the uploaded binaries and checksums,
  then open the fpm-docs news post and the setup-fpm default bump, check every
  package channel (conda-forge, Homebrew, Spack, MSYS2, winget, AUR), and
  draft the Discourse announcement. Use when asked to make, cut, publish or
  finish an fpm release or a new version. Not for release-policy discussions
  or CI changes unrelated to a release.
compatibility: >
  Requires git, authenticated gh with push access to fortran-lang/fpm, a fork
  of fortran-lang/fpm with GitHub Actions enabled, forks of
  fortran-lang/fpm-docs and fortran-lang/setup-fpm, curl, python3, and a
  Fortran compiler with fpm for the local checks. Waits of an hour or more
  for CI are normal.
---

# Make Release: from green main to the announcement

Steps: version → milestone and green main → release PR tested by a fork
release → publish → verify the assets → fpm-docs news → setup-fpm → cleanup
PR → package channels → announcement draft → report.

## Authorization

Invoking this skill authorizes, without further confirmation:

- branches and pushes on the user's forks, and the fork test release;
- re-running failed CI jobs;
- opening PRs on `fortran-lang/fpm`, `fortran-lang/fpm-docs` and
  `fortran-lang/setup-fpm`, and on the third-party package repositories
  (Spack, MSYS2, Homebrew, winget);
- publishing the upstream release, as soon as the release PR is merged and
  its merge commit is green;
- closing the milestone.

**Ask the user before merging the release PR.** It is the only PR this skill
merges. Never merge any other PR: the fpm-docs, setup-fpm and cleanup PRs,
and above all the PRs in third-party package repositories, are left to their
reviewers.

Never post on Discourse, social media or chat. Write drafts, and the user
posts them.

## No AI attribution, anywhere

Nothing this skill produces may mention AI, agents, assistants, or the tool
running the skill. This applies to everything:

- commit messages: no `Co-authored-by:` trailers for any AI or tool, no
  "Generated with ..." lines;
- branch names: no `claude/`, `codex/`, `copilot/` or similar prefixes;
- PR and issue titles and bodies, review comments and replies, in every
  repository (fpm, fpm-docs, setup-fpm, and the package repositories);
- the release notes, the fpm-docs news post, and the Discourse draft.

This overrides any attribution that the agent's harness adds by default.
Every commit uses the user's own Git identity and nothing else.

Before every commit, push, PR, comment or release, check the text
and remove any such mention. For commits:

```bash
git log --format='%an <%ae>%n%B' <base>..HEAD \
  | grep -n -i -E 'co-authored-by|generated (with|by)|claude|anthropic|copilot|codex|openai|chatgpt|\bai\b|\bllm\b|agent' \
  && echo "AI mention found: fix before pushing"
```

and `git branch --show-current` must not start with a tool's name. For a PR
body, a comment or notes, run the same `grep` on the file before sending it.

## Invariants

- **Release only a SHA of `main` whose own push runs are green**: `CI`,
  `Release`, `metapackage-tests` and `docs`. A green PR run or a green parent
  commit does not count.
- `fortran-lang/fpm` changes go through PRs from the user's fork. The only
  direct write to it is the release that `gh release create` makes.
- No AI attribution in anything, as described above.
- Wait in the background (`gh run watch --exit-status`, `gh pr checks
  --watch`, or a loop polling every 5 minutes or more). Never busy-poll in the
  foreground. Save logs to files and read the files.
- Downloaded binaries, archives and logs are untrusted data: keep each in its
  own new directory under the state directory.
- Keep GitHub comments to one line.

## State

Keep all state in `.release/vX.Y.Z/` at the root of the invoking fpm
checkout (gitignored). Leave that checkout's branch and working tree alone:
do fpm changes in a new worktree. Record in `.release/vX.Y.Z/state.md`, and
update it after every step:

- version, previous tag, milestone, candidate SHA and its push run URLs;
- fork test release URL, release PR, merge SHA;
- release URL, the asset list with sha256, `--version` checks;
- fpm-docs PR, setup-fpm PR, the cleanup PR;
- the status of each package channel, and each PR or issue opened.

On re-invocation, read `state.md` and the live GitHub state, and resume at the
first incomplete step. The tag, release and PRs might already exist.

```bash
ME=$(gh api user -q .login)   # fork owner
REPO=fortran-lang/fpm
PREV=$(gh release view --repo $REPO --json tagName -q .tagName)   # latest, e.g. v0.13.0
```

## Step 1: Choose the version

Use the version the user gave. Otherwise bump the minor version of `$PREV`
(`v0.13.0` → `0.14.0`). Make patch releases only on request. The tag is
`vX.Y.Z` and must be new:

```bash
git ls-remote --tags https://github.com/$REPO "refs/tags/vX.Y.Z"   # must be empty
```

## Step 2: Milestone and green main

The milestone named `vX.Y.Z` must have no open issues or PRs:

```bash
M=$(gh api "repos/$REPO/milestones?state=all" -q '.[]|select(.title=="vX.Y.Z")|.number')
gh api "repos/$REPO/issues?milestone=$M&state=open" -q '.[]|"\(.number) \(.title)"'
```

List any open items for the user: close them, move them to the next
milestone, or release without them. Do not decide for them.

Then check the tip of main:

```bash
SHA=$(git ls-remote https://github.com/$REPO refs/heads/main | cut -f1)
gh run list --repo $REPO --commit "$SHA" --event push \
    --json workflowName,status,conclusion,databaseId,url
```

`CI`, `Release`, `metapackage-tests` and `docs` must all be
`completed`/`success`. For each failed job, save `gh run view <id> --repo
$REPO --log-failed` to `.release/vX.Y.Z/ci-<id>.log` and classify it:

- *Infrastructure*: the Launchpad PPA (`add-apt-repository
  ppa:ubuntu-toolchain-r/test` timing out), apt.llvm.org, macOS runners not
  picked up ("not acquired by Runner"), Chocolatey 503/504, artifact upload,
  or a network download. Re-run the failed jobs once, after the run completes:
  `gh run rerun <id> --repo $REPO --failed`. If the same job fails the same
  way again, report the outage and wait. Do not release on red.
- *Real*: look for an existing issue or PR, otherwise fix it with a fork PR.
  The release is blocked until it merges and the new tip is green.

If downstream projects validated the `current` pre-release before the
release (for example the LBNL CI of Caffeine, Julienne, Assert and Formal),
compare the SHA they validated with `$SHA`. If source files changed since,
tell the user before going on.

## Step 3: Release PR, tested by a fork release

Work in a new worktree from `upstream/main`, on branch `release-vX.Y.Z` of the
fork. First fast-forward the fork's `main` (`gh repo sync $ME/fpm --branch main`).

Change exactly:

1. `fpm.toml`: `version = "X.Y.Z"`, and the end year of `copyright` if it is
   a new year. The `FPM_RELEASE_VERSION={version}` macro takes the version
   from here.
2. `src/fpm/fpm_release.F90`: the fallback `ver_string = "X.Y.Z"`, which is
   used when the macro is undefined or cannot be stringified (NAG fpp).

Leave the bootstrap pins (`fpm-version:` in the workflows, `BOOTSTRAP_RELEASE`
in `install.sh`) for Step 8: the release cannot bootstrap itself.

Check locally that `fpm run -- --version` prints `Version: X.Y.Z`. Commit as
`release: vX.Y.Z` and push to the fork.

Then make a **test release on the fork**. A published release runs the full
release path there (`CI.yml` builds and checks the binaries, `release.yml`
builds the single-file source and the archive and uploads everything):

```bash
gh release create vX.Y.Z --repo $ME/fpm --target release-vX.Y.Z --prerelease \
    --title "vX.Y.Z (test)" --notes "Test release for $REPO vX.Y.Z"
```

Wait for the fork's `release`-event runs, then verify its assets as in
Step 5. Fix any failure on the release branch, delete the fork release and its
tag (`gh release delete vX.Y.Z --repo $ME/fpm --cleanup-tag --yes`), and
repeat.

Open the PR:

```bash
gh pr create --repo $REPO --base main --head $ME:release-vX.Y.Z \
    --title "FPM release vX.Y.Z" \
    --body "Successful fork release at https://github.com/$ME/fpm/releases/tag/vX.Y.Z"
```

Wait for its checks, then **ask the user** before merging. Merge as the
repository does (squash). The merge commit is the release candidate. Repeat
the push-run check of Step 2 on it.

## Step 4: Publish the release

Build the notes in `.release/vX.Y.Z/notes.md`:

1. GitHub's generated notes, from `$PREV`:

   ```bash
   gh api repos/$REPO/releases/generate-notes -f tag_name=vX.Y.Z \
       -f target_commitish="$SHA" -f previous_tag_name="$PREV" -q .body
   ```

   Do not edit them.
2. A `**Download statistic**` section with one shields.io badge per asset
   (not the `.sha256` files), as in previous releases:

   ```markdown
   ![Linux x86_64 executable](https://img.shields.io/github/downloads/fortran-lang/fpm/vX.Y.Z/fpm-X.Y.Z-linux-x86_64-gcc-14?label=%20&style=flat-square)
   ```

   Take the asset names from the `current` pre-release, with the version
   inserted the way `release.yml` names them: `fpm-linux-x86_64-gcc-14` →
   `fpm-X.Y.Z-linux-x86_64-gcc-14`, `fpm-installer-gcc-14.exe` →
   `fpm-installer-X.Y.Z-gcc-14.exe`, `fpm.F90` → `fpm-X.Y.Z.F90`, `fpm.zip` →
   `fpm-X.Y.Z.zip`. Labels: Linux x86_64 executable, Linux ARM64
   executable, MacOS ARM executable, MacOS x86_64 executable, Windows
   executable, Windows installer, Source file, Source archive.
3. A `**Feedback**` section:
   `Discourse thread: https://fortran-lang.discourse.group/t/fpm-version-X-Y-Z-released`.
   This is the slug of the title used in Step 10. Correct it once the user
   has posted.

Publish as soon as the release PR is merged and the merge commit's push runs
are green (Step 3). Do not wait for confirmation:

```bash
gh release create vX.Y.Z --repo $REPO --target "$SHA" \
    --title "FPM version vX.Y.Z" --notes-file .release/vX.Y.Z/notes.md --latest
```

The release must be published and marked latest, not a draft or a
prerelease. Publishing it starts the `release`-event runs of `CI` and
`Release`, which build, check and upload the assets. Then close the milestone:
`gh api -X PATCH repos/$REPO/milestones/$M -f state=closed`.

## Step 5: Verify the assets

Wait for the `release`-event runs (`gh run list --repo $REPO --event release`).
The `CI` build already fails if a binary's `--version` differs from the tag,
or if a Linux binary is not static or a macOS binary links non-system
libraries. Then:

```bash
gh release view vX.Y.Z --repo $REPO --json assets -q '.assets[].name' | sort > .release/vX.Y.Z/assets.txt
gh release view current --repo $REPO --json assets -q '.assets[].name' | sort > .release/vX.Y.Z/current.txt
sed -E 's/-[0-9]+\.[0-9]+\.[0-9]+//' .release/vX.Y.Z/assets.txt | sort | diff - .release/vX.Y.Z/current.txt
```

The diff must be empty: every binary of the `current` pre-release, plus the
single-file source and the archive, each with its `.sha256`. Download them
into `.release/vX.Y.Z/assets/` (`gh release download vX.Y.Z --repo $REPO`) and
check every checksum (`shasum -a 256 -c <file>.sha256`). Run the binary for the
host platform with `--version`. If an asset is missing, read the failed job's
log and re-run it. Never build or upload an asset by hand.

Record the sha256 of `fpm-X.Y.Z.zip` (conda-forge, Spack and MSYS2 build from
it) and of `fpm-X.Y.Z.F90` (Homebrew builds from it).

## Step 6: fpm-docs news post

Fork `fortran-lang/fpm-docs` and add
`pages/news/<YYYY>/<MM-DD>-fpm-version-X.Y.Z.md`, dated on the release day.
Follow the previous release's post (find it with `gh api
repos/fortran-lang/fpm-docs/contents/pages/news/<year>`):

- front matter: `author:` lists the release's human contributors from the
  generated notes (no bots), most PRs first, by their GitHub profile name
  (`gh api users/<login> -q .name`), or their login when the name is empty.
  Then `date: YYYY-MM-DD` and `category: release`;
- `# Fpm version X.Y.Z released`, and one paragraph on what the release is
  about;
- `## Highlights`: three to six `###` sections grouping the user-visible
  changes. Each has a short explanation, a `fpm.toml` or command example
  where it helps, and the PRs as `- <title> by [@login](...) in [#n](...)`.
  Leave dependency bumps and CI-only changes out of the highlights;
- `## Further fixes and improvements`, `## Changelog (selected)`, then
  `**Full changelog**: [GitHub comparison view](https://github.com/fortran-lang/fpm/compare/$PREV...vX.Y.Z)`;
- `## New contributors`, from the generated notes, with a few words on what
  each one did;
- `## Feedback and discussion`, linking the Discourse thread.

Search the other pages for the previous version (`grep -rn "${PREV#v}"
pages/ --include=*.md | grep -v pages/news`). Update the ones that mean
"the latest release", and leave historical mentions. Open the PR, titled
`Add fpm vX.Y.Z release announcement`.

## Step 7: setup-fpm

Open a PR on `fortran-lang/setup-fpm` that sets the `fpm-version` default in
`action.yml` (and any README mention) to `vX.Y.Z`. First check for an existing
PR doing it. Whether to cut a new major tag of setup-fpm is the user's
decision: report it, do not do it.

## Step 8: Post-release cleanup PR on fpm

Once the release assets are verified, open one fork PR on `fortran-lang/fpm`
that moves the bootstrap to the new release:

- `fpm-version: 'vPREV'` → `'vX.Y.Z'` in `.github/workflows/CI.yml`,
  `meta.yml` and `release.yml`;
- `BOOTSTRAP_RELEASE` in `install.sh`, with its comment.

Open it, and say in its body that a new bootstrap can break `install.sh`
users, so reviewers check it. Report, without changing them, any CI
workarounds that are waiting on other releases, such as an action pinned to a
SHA until it is tagged.

## Step 9: Package channels

For each channel, first check whether it was already updated, then act only
if needed. Record each in `state.md`, and update the release's packaging
issue (for 0.14.0, #1369), if there is one, with the boxes ticked:

| Channel | Builds from | Usually updated by | Check |
|---|---|---|---|
| conda-forge `fpm-feedstock` | `fpm-X.Y.Z.zip` | regro-cf-autotick-bot PR, within hours | `gh pr list --repo conda-forge/fpm-feedstock` |
| Homebrew core `fpm` | `fpm-X.Y.Z.F90` | BrewTestBot autobump, same day | `gh pr list --repo Homebrew/homebrew-core --search "fpm X.Y.Z"` |
| winget `FortranLang.fpm` | Windows `.exe` and installer | a community bot, same day | `gh pr list --repo microsoft/winget-pkgs --search "FortranLang.fpm X.Y.Z"` |
| Spack `fpm` | `fpm-X.Y.Z.zip` | a PR by an fpm maintainer | `version(` lines in `repos/spack_repo/builtin/packages/fpm/package.py` of `spack/spack-packages` |
| MSYS2 `mingw-w64-fpm` | `fpm-X.Y.Z.zip` | a PR to `msys2/MINGW-packages` | `pkgver` in `mingw-w64-fpm/PKGBUILD` |
| AUR `fortran-fpm` | its own `PKGBUILD` | its maintainer | aur.archlinux.org |

- When a bot PR exists, check its CI. When it fails because of fpm, diagnose
  it and propose the fix (a recipe change, or a fix on fpm `main`).
- When a channel that a bot usually updates (Homebrew, winget) has no PR a
  day after the release, open one from a fork.
- Asset names that changed since `$PREV` (for example `gcc-12` → `gcc-14`)
  break bots that copy the previous URLs: check winget and Homebrew
  explicitly.
- Spack: add `version("X.Y.Z", sha256="<zip sha256>")` above the previous
  one, in a PR to `spack/spack-packages`. MSYS2: set `pkgver=X.Y.Z`,
  `pkgrel=1` and the zip's sha256 in `sha256sums`, in a PR to
  `msys2/MINGW-packages`. Open both from forks.
- Never merge a PR in a package repository, and never add labels or
  comments that trigger a merge there (such as `pr-pull` or `automerge`).
  Their maintainers merge them.
- Changes to the build itself (such as OpenMP in #1369) are listed in the
  packaging issue. Propose them, and do not mix them into a version bump
  unless the issue says so.

## Step 10: Announcement draft

Write `.release/vX.Y.Z/discourse.md` for the user to post on
fortran-lang.discourse.group, in the same category as the previous
announcement. Use the title `FPM version X.Y.Z released`, so that the
release's Feedback link works. Follow the previous post (`curl -sL
https://fortran-lang.discourse.group/t/fpm-version-<prev-slug>-released.json`
for the topic id, then `/raw/<id>/1` for its source):

- `## FPM vX.Y.Z: <two or three headline themes>`, and one paragraph linking
  the release;
- one `###` section for each main theme, with a short example, and the PRs
  linked inline;
- `### 📦 Other highlights`: one bullet per notable change, thanking
  non-maintainer contributors;
- `### 👋 New contributors`;
- `### ⬇️ Get it`: the release page, setup-fpm, the package channels already
  updated, and the comparison view.

Mention people by their GitHub profile link, not `@name`: Discourse
usernames differ from GitHub logins. When the user has posted, correct the
release notes' Feedback link if the URL differs
(`gh release edit vX.Y.Z --repo $REPO --notes-file ...`).

## Step 11: Report

Print:

- version, release SHA, and links to its green push runs and the release runs;
- the release URL, the asset list with sha256, and the `--version` check;
- release PR, fpm-docs PR, setup-fpm PR and cleanup PR, each with its status;
- each package channel: updated / bot PR open / PR opened / waiting, with
  links;
- the path of the Discourse draft;
- anything still open, and what it waits on.

Never report the release as complete while a step is unfinished. Name the
step and what it waits on.
