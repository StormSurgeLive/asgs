The ASGS Release Process

VERSION HISTORY

03/24/2025 - created by Brett Estrade

CONTENTS

1. Introduction 2. Description of Tiers 2.1 Feature Branch
2.2 Development 2.3 Release Candidate 2.4 Release 2.5 Stable 3
Recommended Release Steps

INTRODUCTION

This document is the general description of the manual release
process. It is still a moving target. Once it is more rote, we will
automate it.

2 DESCRIPTION OF TIERS

Release Tiers, ordered from most changing to most:

2.1 Feature Branch

Ad hoc personal branches made by developers, these should be used
only by the developers who created or currently "own" them. The
should not be considered suitable for operational use.

2.2 Development

The is the "master" branch, the commits made into this branch
are done so (usually) after a proper review process and approval
by a code maintainer other than the one who may be making the
changes. Code maintainers have the discretion to by-pass the rewiew
process, but only after careful consideration of the impacts of
the update on other development. This code should be considered
okay for operational use by operators who are extremely familiar
with the code only.

2.3 Release Candidate

A few times a year, and certainly before the Atlantic Hurricane
season begins; a version is officially released that contains all
vetted features that have been merged into the Development version
(master). The purpose of the release candidate is for downstream
operational users to preview the intended next "release" version
of ASGS.

The name of the release candidate will be of the form,

  YEAR.RELEASE_COUNT.rc-ITERATION

e.g., "2025.1.rc-0" - which means that this is the first release
candidate for the eventual first release of the year 2025. If this
RC was used and generally accepted by the downstream operational
users, then it would be promoted to a release.

If a new release candidate was needed because issues might have
been found during user testing, then the last number in the version
would be incremented by one.

e.g.,

2025.1.rc-0 -> 2025.1.rc-1 -> 2025.1.rc-2 -> ...

This process would continue until, adding only fixes until it was
deemed time to make a full release.

2.4 Release

A full release would be made once an RC was accepted, the name of
the release would be of the form,

  YEAR.RELEASE_COUNT.release-ITERATION

e.g., "2025.1.release-0"; and while it should be very rare, updates
to releases would cause the ITERATION to increase.

e.g.,

2025.1.release-0 -> 2025.1.release-1 -> ...

But because each "release" needs to get through an "rc" stage,
it is very unlikely that we would be updating the release version
except in cases that were required for operational use (e.g.,
adding support for a specific mesh or scenario parameter package.)

2.5 Stable

Stable releases are where release versions go to make room for a
new release version. It indicates that the code in them has stopped
changing, more or less the version is now set in stone. Changes
are high they would ever need to be modified.

The name of the stable version would be of the form,

  YEAR.RELEASE_COUNT.stable

e.g., "2025.1.stable; any updates to a stable version will be
exceedingly rare, so there is no accounting for the "ITERATION" that
is present in the release and release candidate versions. If such
a situation arises, the code maintainers would have to decide how
to denote that the stable version has been updated. No suggestions
are offered here since it would be such a highly unusual situation.

3 RECOMMENDED RELEASE STEPS

There should be 3 branches:

a. master b. release-candidate c. release d. stable

All new commits and changes must flow from master, and in this
ordering described above. It acts as a filter and protects the
"stability" of the code - i.e., how often or infrequently the
code itself changes (this doesn't refer to the "stability" of ASGS
as software.)

3.1 merging features into the master branch

The master branch is the merge target of all feature branches,
so there should be no commits going into the master branch that
are not complete and reviewed features.

3.2 Updating release-candatate From master

The code maintainers should decide what features from master should
go into the release for which they are creating or updating the
candidate. All commits coming from master should be cherry-picked
(and signed w/ "-xs") into the release-candidate branch.

When creatign a release candidate, a tag should be made in the
release-candidate branch; it makes no sense to create a tag in the
master branch. Once the release branch has been tagged, one may
now use the Github 'release tool' to create a release candidate.
Note the naming scheme, and take into account the ITERATION, if
this is the update of an existing release candidate.

a. cherry pick commits from master to release-candidate b. update
the VERSION file with the version, "2025.1.rc-1" (make commit)
c. tag release candidate with version, e.g., 2025.1.rc-1 d. generate
a release-candidate using Github's release tool

3.3 Promoting release-candidate to release

All releases must come from an official release-candidate.

Once a release-candidate is ready to be promoted, the following
procedure should be followed:

a. tag the release-candidate branch with the appropriate release
name, e.g. (say we're on our 3rd release-candidate and decide we
want to make a release):

  2025.1.rc-2 -> 2025.1.release-0

b. merge release-candidate branch into release branch c. update
VERSION file in the release branch, commit it d. tag the release
branch with the new release version e. create a new release using
the Github release tool

3.4 Promoting a release to a stable version

The stable release comes from the release branch in a similar manner:

a. tag release branch with the stable version,

  2025.1.release-1 -> 2025.1.stable

b. merge the release branch into the stable branch c. update VERSION
file with stable version, commit it d. tag the stable branch with the
stable version e. create new release using the Github release tool

3.5 Moving an ASGS Stable Version to "EOL" (End of Life)

Effectively this means it is not supported in any way, recommended
approach is as follows:

a. git checkout the stable version to EOL b. git tag the version
replacing "stable" with EOL:

  git tag 2024.1.stable-EOL

c. update the the Github release of 2024.1.stable with a note that
it is EOL and no longer supported; ask the reader to please upgrade
to the latest stable version
