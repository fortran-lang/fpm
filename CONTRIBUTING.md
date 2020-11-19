# Contributing to the Fortran Package Manager

Thank you for considering contributing to the Fortran Package Manager (*fpm*).
Please review and follow these guidelines to make the contribution process
simple and effective for all involved. It will help communicate that you
respect the time of the community developers. In return, the community will
help address your problem, evaluate changes, and guide you through your pull
requests.

By contributing to *fpm*, you certify that you own or are allowed to share the
content of your contribution under the
[fpm license](https://github.com/fortran-lang/fpm/blob/master/LICENSE).

* [Style](#style)
* [Reporting a bug](#reporting-a-bug)
* [Suggesting a feature](#suggesting-a-feature)
* [Workflow](#workflow)
* [General guidelines](#general-guidelines)
* [For new contributors](#for-new-contributors)

## Style

Please follow the
[Fortran stdlib style guide](https://github.com/fortran-lang/stdlib/blob/master/STYLE_GUIDE.md)
for any Fortran code that you contribute.
This allows us to focus on substance rather than style.

## Reporting a bug

A bug is a *demonstrable problem* caused by the code in this repository.
Good bug reports are extremely valuable to us—thank you!

Before opening a bug report:

1. Check if the issue has already been reported
   ([issues](https://github.com/fortran-lang/fpm/issues)).
2. Check if it is still an issue or it has been fixed?
   Try to reproduce it with the latest version from the master branch.
3. Isolate the problem and create a minimal test case.

A good bug report should include all information needed to reproduce the bug.
Please be as detailed as possible:

1. Which version of *fpm* are you using? Please be specific.
2. What are the steps to reproduce the issue?
3. What is the expected outcome?
4. What happens instead?

This information will help the community diagnose the issue quickly and with
minimal back-and-forth.

## Suggesting a feature

Before suggesting a new feature, take a moment to find out if it fits the scope
of the project, or if it has already been discussed. It is up to you to provide
a strong argument to convince the community of the benefits of this feature.
Please provide as much detail and context as possible. If applicable, include a
mocked-up snippet of what the output or behavior would look like with this
feature implemented. “Crazy”, out-of-the-box ideas are especially welcome.
It’s quite possible that we are not considering an unusually creative solution.

## Workflow

*fpm* is a community project. There is no one single person making final
decisions. This is the workflow that we follow:

1. Open a [new issue](https://github.com/fortran-lang/fpm/issues/new) to
   describe a bug or propose a new feature.
   Refer to the earlier sections on how to write a good bug report or feature
   request.
2. Discuss with the community and reach majority consensus about what should be
   done about the bug or feature request.
   We define “majority” loosely as 80%.
   This means that at least 4 of 5 people engaged in the discussion should be
   able to agree on the next step.
   This allows us to have the community mostly agree while not getting stuck if
   one person disagrees.
   At this stage, the scope of the fix/feature, its behavior, and API if
   applicable should be defined.
   Only when you have community consensus on these items you should proceed to
   writing code and opening a PR.
   **When actively working on code towards a PR, please assign yourself to the
   issue on GitHub.**
   This is good collaborative practice to avoid duplicated effort and also
   inform others what you are currently working on.
3. Open a new Pull Request (PR) with your contribution.
   The body of the PR should at least include a bullet-point summary of the
   changes, and a detailed description is encouraged.
   If the PR completely addresses the issue you opened in step 1, include in
   the PR description the following line: `Fixes #<issue-number>`.
4. Request reviewers to your PR.
   For small bug fixes or documentation improvements, 1 to 2 reviewers is
   sufficient.
   For implementation of bigger features, request 3 to 4 or more reviewers.
   Ideally, request reviewers that participated in step 2.
5. If your PR implements a feature that adds or changes the behavior of *fpm*,
   your PR must also include appropriate changes to the documentation.

This workflow can evolve and change over time as we learn how best to work
together. If you have an idea on how to improve the workflow itself, please
open an issue and we’ll discuss it.

## General guidelines

* A PR should implement *only one* feature or bug fix.
* Do not commit changes to files that are irrelevant to your feature or bug fix.
* Smaller PRs are better than large PRs, and will lead to a shorter review and
  merge cycle
* Add tests for your feature or bug fix to be sure that it stays functional and useful
* Be open to constructive criticism and requests for improving your code.
* Again, please follow the
  [Fortran stdlib style guide](https://github.com/fortran-lang/stdlib/blob/master/STYLE_GUIDE.md).

## For new contributors

If you have never created a pull request before, welcome :tada:.
You can learn how from
[this great tutorial](https://egghead.io/series/how-to-contribute-to-an-open-source-project-on-github).

Don’t know where to start?
You can start by looking through the list of
[open issues](https://github.com/fortran-lang/fpm/issues).
