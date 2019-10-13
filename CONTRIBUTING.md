First off, thanks for taking the time to contribute!

The following is a set of guidelines for contributing to Beagle which is hosted at https://github.com/tokenmill/beagle. These are just guidelines, not rules, use your best judgment and feel free to propose changes to this document in a pull request.

This project adheres to the Contributor Covenant code of conduct. By participating, you are expected to uphold this code. Please report unacceptable behavior to info@tokenmill.lt.
Issues & Pull requests

Issues and Pull requests welcome!

We do ask that before submitting a pull request you open an issue tracking the bug of enhancement you'd like to fix or submit. This makes it easier to discuss changes in the abstract, before focusing on a particular solution.

Furthermore, please be diligent about submitting pull requests which only make one essential change at a time. While formatting changes and code cleanups are welcome, they should be separate from features and a pull request should only introduce one logical feature at a time. When adding new features, please ensure there are accompanying tests.

Commit Messages

Commit messages should be well formed, according to the guidelines outlined by Tim Pope: http://karma-runner.github.io/4.0/dev/git-commit-msg.html

When fixing an existing issue, add - fixes #xxx somewhere in the commit message: this has the dual purpose of closing the issue when your patch is merged to master as well as automatically providing a link in to the related issue.

Change Log

Pull requests are required to update the changelog. Changelog entries should mention and link to any issues or tickets involved in the change, and should provide a short summary description of the particular changes of the patch.

Include the issue number (#xxx) which will link back to the originating issue in Github. Commentary on the change should appear as a nested, unordered list.

Whitespace & Linting

Beagle is maintained with fairly strict whitespace and style standards.

Gitlab CI jobs will fail if the clj-kondo rules are violated, or the source format doesnt match the default cljfmt style guidelines. Hence, patches must be formatted and whitespace linted before they will be accepted.
