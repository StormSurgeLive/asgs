The Automated Solution Generation System (ASGS) provides software infrastructure
for automating coastal ocean modelling for real time decision support, and provides
a variety of standalone command line tools _for_ pre and _post_-processing.

It includes `asgsh`, an extremely powerful interactive shell geared towards operational
storm surge forecasting, managing multiple ADCIRC installations, and facilitating ADCIRC
development workflows and testing.

See https://github.com/StormSurgeLive/asgs/releases for more information on releases.

To reach out via Chat, visit our Discord server: https://discord.gg/jFbacxrUf9

For a **Quick Start**, see:

https://github.com/StormSurgeLive/asgs/wiki/ASGS-Cheat-Sheet

# Release Notes

1. Before May 15 (what everyone must do)
   * Test all workflows against 2025.1.release-N (future stable)
   * Test all workflows against 2025.2.rc-N (future 2026 ops)
   * Re-run past storms and non-live workflows on both versions
   * Report any failures before May 15
   * Assume no major fixes after May 15
2. On May 15 (what becomes what)
   * 2025.2.rc-N → 2026.1.release-0 (operational version)
   * 2025.1.release-N → 2025.stable (failsafe)
   * All hurricane season runs use 2026.1.release-N
   * 2025.stable is fallback only
3. Fix / backport policy, Before May 15
   * Fixes go:
   * dev → 2025.2.rc-N
   * Critical fixes may also go: → 2025.1.release-N
   * Goal: both future ops and stable are fully validated
4. During hurricane season (after May 15)
   * Ops (2026.1.release-N)
     * Critical fixes only
     * Backported from dev
   * Stable (2025.stable)
     * Fix only if needed as fallback
     * Minimal changes
   * Dev
     * All new features
     * No stability guarantees
5. One-line summary
   * Before May 15: validate both versions
   * After May 15: run .1.release, protect it, and only backport critical fixes
