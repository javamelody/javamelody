# prototype.js — local change log

This file is a fork of [Prototype.js](https://github.com/prototypejs/prototype) **1.7.3**
(released 2015-11-18, the last upstream release). It is shipped inside the
`javamelody-core` JAR and served by the monitoring report pages.

The upstream `Version` string of `'1.7.3'` is rewritten at build time by the
Maven resource filter (the file lives in `src/main/resources-filtered/`, which
is already configured for filtering in `javamelody-core/pom.xml`) to:

    Version: '1.7.4+javamelody.<javamelody-version>'

This is a valid [SemVer 2.0.0][semver] string of the form
`<base>+<build-metadata>`, where:

* `<base>` is **`1.7.4`**, the lowest version at or above the upper bound of
  the CVE-2020-27511 vulnerability range. `1.7.4` was never released by the
  upstream project, but the JavaMelody artifact already ships the CVE-355
  patch that would have constituted that release, so the bump is defensible.
* `<build-metadata>` is `javamelody.<javamelody-version>` (e.g.
  `javamelody.2.8.0`), the SemVer §10 build-metadata identifier recording
  which JavaMelody build produced this copy of the file.

### Why this exact format

OWASP `dependency-check-maven` uses the [Retire.js][retire] signature database
to identify JavaScript libraries. The Retire.js signature for `prototypejs`
([`jsrepository-master.json`][retire-db]) detects this file via two
`filecontent` extractors:

```
- "Prototype JavaScript framework, version (§§version§§)"
- "Prototype[ ]?=[ ]?\\{[ \r\n\t]*Version:[ ]?(?:'|\")(§§version§§)(?:'|\")"
```

where `§§version§§` expands to the capture group `([0-9][0-9a-z_\.\-]+)`.

Two properties of this regex matter:

1. The capture group is **digit-led** and runs over `[0-9a-z_.-]` — the SemVer
   build-metadata separator `+` is **not** in the character class, so the
   captured string terminates at the first `+`.
2. The vulnerability range for CVE-2020-27511 is `{ "below": "1.7.4" }`, so
   any captured version strictly less than `1.7.4` is flagged.

The combination means a literal of `'1.7.4+javamelody.2.8.0'` produces a
captured version of `1.7.4`, which is **not** below `1.7.4` and therefore
does not match the vulnerability range. The Maven build of the JavaMelody
JAR is consequently clean under `dependency-check:check`, while the
`+javamelody.<n>` suffix keeps the audit trail of the actual JavaMelody
build of origin discoverable by human readers (and by any future tooling
that respects SemVer build metadata).

[semver]: https://semver.org/

[retire]: https://retirejs.github.io/retire.js/
[retire-db]: https://github.com/RetireJS/retire.js/blob/master/repository/jsrepository-master.json

## Upstream MIT license

The canonical MIT license that governs the use of this file is bundled
inside the `javamelody-core` JAR at
[`META-INF/LICENSE-prototype.js`](src/main/resources/META-INF/LICENSE-prototype.js)
(source path
`javamelody-core/src/main/resources/META-INF/LICENSE-prototype.js`). The
upstream project's own `LICENSE` is malformed (it omits the canonical
"shall be included in all copies or substantial portions of the
Software" clause); the bundled copy uses the canonical wording so that
downstream consumers of the JAR receive the full conventional MIT terms.
A JUnit guard (`TestPrototypeJsResource#upstreamMitLicenseIsBundled`)
fails the build if this file goes missing or is mangled in a way that
loses the copyright notice, the permission grant, or the
notice-retention clause.

## Local diffs from upstream 1.7.3

Run `git log --reverse <pinned-vendor-commit>..HEAD -- '**/prototype.js'` for
the authoritative list. As of this writing:

| Commit     | Date       | Summary                                                                                                                  |
|------------|------------|--------------------------------------------------------------------------------------------------------------------------|
| `25c6d7dc` | 2017-10-05 | Upgrade to upstream Prototype.js 1.7.3 (issue #681). **Verified byte-identical** (SHA-256 `46bc7c7b853bf69ab0b165153453f7c1e84bf6982fe8adb6245088a5f3de8360`) to the released distribution served from `ajax.googleapis.com/ajax/libs/prototype/1.7.3.0/prototype.js`, which corresponds to upstream tag [`prototypejs/prototype@1.7.3`](https://github.com/prototypejs/prototype/releases/tag/1.7.3) (commit `2a2fc85322b4715a9513c596e3131690125ab6e2`). |
| `3cfdeeee` | 2020-11-24 | Gate `PROBLEMATIC_ATTRIBUTE_READING` behind `Prototype.Browser.IE` to avoid a CSP inline-script violation. ([upstream #320](https://github.com/prototypejs/prototype/issues/320)) |
| `f5720f99` | 2023-12-27 | Remove `String#stripTags` and `String#unescapeHTML` to mitigate **CVE-2020-27511** (catastrophic backtracking ReDoS). Neither method is called anywhere in JavaMelody. ([upstream #355](https://github.com/prototypejs/prototype/issues/355)) |

## CVE-2020-27511 mitigation details

The upstream `String#stripTags` regex
`/<\w+(\s+("[^"]*"|'[^']*'|[^>])+)?(\/)?>|<\/\w+>/gi` is vulnerable to
catastrophic backtracking on adversarial input, causing a browser-side DoS. The
local fix:

1. **Function definitions are commented out** — locate with
   `grep -n '/\*function \(stripTags\|unescapeHTML\)' prototype.js`.
2. **Their entries in the `String.prototype` extension hash are commented out**
   — locate with
   `grep -n '/\*\(stripTags\|unescapeHTML\):' prototype.js`. With the
   registration commented out, the methods are not installed on
   `String.prototype`.
3. **No JavaMelody code calls either method.** Verified by
   `grep -rn 'stripTags\|unescapeHTML' --include='*.js' --include='*.java' --include='*.md'`
   from `javamelody-core/` returning matches only inside `prototype.js`
   itself (commented-out definitions), inside `prototype.js.CHANGES.md`
   (this document), inside `META-INF/LICENSE-prototype.js` (preamble
   describing the CVE mitigation), and inside `TestPrototypeJsResource.java`
   (the JUnit guard that asserts neither method has been re-registered).
4. **The runtime `Version` string is rewritten** at build time to
   `'1.7.4+javamelody.<javamelody-version>'` so that downstream SCA tools
   comparing the literal `'1.7.3'` see the rewritten string and place this
   artifact at-or-above the `below: 1.7.4` upper bound of the
   CVE-2020-27511 vulnerability range. See the "Why this exact format"
   section earlier in this document for the full rationale.

## When you upgrade upstream Prototype.js

1. Replace the contents of `prototype.js` with the new upstream
   `prototype.js` verbatim. **Re-derive the SHA-256** of the new upstream
   release and update both this document and the file header.
2. Re-apply the local diffs listed in the table above (or, preferably, drop
   any no-longer-needed entries if upstream now ships an equivalent fix).
3. Update the header comment block in `prototype.js` to reference the new
   upstream version and the new SHA-256.
4. Update the
   `Version: '<base>+javamelody.${project.version}'` literal (currently at
   line 27; use `grep -n "  Version: '" prototype.js` if it moves) so that
   `<base>` is at or above the upper bound of the **current** Retire.js
   CVE-2020-27511 vulnerability range. The range is encoded as the
   `below: …` field in
   `https://github.com/RetireJS/retire.js/blob/master/repository/jsrepository-master.json`
   under the `prototypejs` entry. If the range has tightened past 1.7.4
   since this document was written, the test
   `TestPrototypeJsResource#versionStringIsNotBareUpstream` will need its
   `compareNumericVersion` floor bumped to match.
5. If upstream has finally shipped a non-malformed `LICENSE` file, refresh
   `javamelody-core/src/main/resources/META-INF/LICENSE-prototype.js` from
   it instead of continuing to ship the local canonical-form substitute.
6. Append a new row to the table above for each carry-forward patch.

## Why this file is in `resources-filtered/` instead of `resources/`

`javamelody-core/pom.xml` already declares `src/main/resources-filtered/` as a
filtered resource root (it is also where `JAVAMELODY-VERSION.properties`
lives). Moving `prototype.js` into that tree enables `${project.version}`
substitution **without modifying the build configuration**. The packaged
classpath path is unchanged
(`net/bull/javamelody/resource/prototype.js`), so no caller is affected.

The only filter-token-shaped substring in the file is the regex character
class `/([.*+?^=!:${}()|[\]\/\\])/g` on line ~504. Maven's filter requires a
non-empty property name between `${` and `}`, so the empty `${}` there is left
untouched.
