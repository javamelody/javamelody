/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

/**
 * Guards the local fork of prototype.js against (a) silently reverting to the
 * upstream 1.7.3 string that SCA scanners flag for CVE-2020-27511, and
 * (b) reintroducing the vulnerable {@code stripTags} / {@code unescapeHTML}
 * functions. See {@code prototype.js.CHANGES.md} at the module root for
 * background.
 */
class TestPrototypeJsResource {

	private static final String RESOURCE_PATH = "/net/bull/javamelody/resource/prototype.js";

	private static final String LICENSE_PATH = "/META-INF/LICENSE-prototype.js";

	/** Lower bound for the size of the packaged prototype.js, in bytes. Upstream 1.7.3
	 *  is ~177 KB; an empty or severely truncated copy would mean the filter clobbered
	 *  the file or the resource lookup is hitting the wrong path, which would cause
	 *  every body-substring assertion below to pass vacuously. */
	private static final int PROTOTYPE_JS_MIN_BYTES = 100_000;

	/** Lower bound for the size of the bundled MIT license. The full canonical text is
	 *  ~1100 bytes; anything dramatically smaller means the file has been mangled. */
	private static final int LICENSE_MIN_BYTES = 800;

	private String readResource(String classpath, int minBytes) throws IOException {
		try (InputStream in = getClass().getResourceAsStream(classpath)) {
			assertNotNull(in, "resource missing from classpath: " + classpath);
			try (BufferedReader r = new BufferedReader(
					new InputStreamReader(in, StandardCharsets.UTF_8))) {
				final String body = r.lines().collect(Collectors.joining("\n"));
				assertTrue(body.length() >= minBytes,
						"resource " + classpath + " is suspiciously small ("
								+ body.length() + " bytes < " + minBytes
								+ ") — Maven filtering or resource copy is broken");
				return body;
			}
		}
	}

	/** Splits a numeric "x.y.z" string and compares it segment-by-segment to another
	 *  such string. Returns the same sign convention as {@link Comparable#compareTo}.
	 *  Used instead of {@link String#compareTo} so that bumps past single-digit
	 *  components (e.g. 1.7.10) do not silently regress the version-floor assertion. */
	private static int compareNumericVersion(String a, String b) {
		final String[] aParts = a.split("\\.");
		final String[] bParts = b.split("\\.");
		final int len = Math.max(aParts.length, bParts.length);
		for (int i = 0; i < len; i++) {
			final int av = i < aParts.length ? Integer.parseInt(aParts[i]) : 0;
			final int bv = i < bParts.length ? Integer.parseInt(bParts[i]) : 0;
			if (av != bv) {
				return Integer.compare(av, bv);
			}
		}
		return 0;
	}

	@Test
	void versionStringIsNotBareUpstream() throws IOException {
		final String body = readResource(RESOURCE_PATH, PROTOTYPE_JS_MIN_BYTES);

		// Strategy: a valid SemVer 2.0.0 string of the form
		//   <base>+javamelody.<javamelody-version>
		// where <base> is >= 1.7.4 (the upper bound of the CVE-2020-27511
		// vulnerability range in the Retire.js signature for prototypejs).
		// Retire.js's version-extractor regex is [0-9][0-9a-z_\.\-]+ and
		// excludes '+', so it captures only the <base> portion. As long as
		// <base> is at or above 1.7.4, OWASP dependency-check will not flag
		// the file. The '+javamelody.<n>' build-metadata identifier records
		// the JavaMelody build of origin for auditors.
		//
		// Retire.js has *two* extractors for prototypejs: the Version literal
		// inside the Prototype object, and the file-header "Prototype
		// JavaScript framework, version <v>" comment. Both must be rewritten
		// for the SCA flag to clear, so both are asserted below.

		assertFalse(body.contains("'1.7.3'"),
				"prototype.js contains a bare upstream '1.7.3' string literal "
						+ "somewhere in the file — a future contributor may have "
						+ "re-vendored upstream and forgotten to re-apply the local "
						+ "version rewrite; SCA scanners will flag CVE-2020-27511");

		// Match the Version literal anchored to the Prototype object so that a
		// stray '...Version: 1.7.3...' in an upstream comment or test fixture
		// further down the file can't shadow the real assignment.
		final Matcher versionMatch = Pattern.compile(
				"Prototype\\s*=\\s*\\{[^}]*?Version:\\s*'([^']+)'",
				Pattern.DOTALL).matcher(body);
		assertTrue(versionMatch.find(),
				"Prototype.Version literal not found in prototype.js");
		final String version = versionMatch.group(1);

		// Maven resource filtering must have resolved the placeholder. The
		// regex below would happily accept a literal ${project.version} via
		// its '.+' tail, so this is a separate explicit check.
		assertFalse(version.contains("${"),
				"Prototype.Version literal '" + version + "' contains an "
						+ "unresolved Maven placeholder — the file is in "
						+ "src/main/resources/ (unfiltered) instead of "
						+ "src/main/resources-filtered/, or the pom's filter "
						+ "configuration is broken");

		assertTrue(version.matches("^[0-9]+\\.[0-9]+\\.[0-9]+\\+javamelody\\..+"),
				"Prototype.Version literal '" + version + "' is not in the "
						+ "expected '<x.y.z>+javamelody.<jm-version>' SemVer form");
		final String base = version.substring(0, version.indexOf('+'));
		assertTrue(compareNumericVersion(base, "1.7.4") >= 0,
				"Prototype.Version base '" + base + "' must be >= 1.7.4 so "
						+ "Retire.js / OWASP dependency-check do not flag this "
						+ "file for CVE-2020-27511 (vulnerability range is "
						+ "below 1.7.4)");

		// Header extractor: Retire.js also matches the literal substring
		// "Prototype JavaScript framework, version <v>" in the file header.
		// If the header line still ends in "version 1.7.3" the scanner flags
		// the file regardless of what the runtime Version literal says.
		final Matcher headerMatch = Pattern.compile(
				"Prototype JavaScript framework, version ([^,\\s]+)").matcher(body);
		assertTrue(headerMatch.find(),
				"header 'Prototype JavaScript framework, version <v>' line is "
						+ "missing from prototype.js — the Retire.js header "
						+ "extractor and the human-readable provenance both rely "
						+ "on it");
		final String headerVersion = headerMatch.group(1);
		assertEquals(version, headerVersion,
				"header version '" + headerVersion + "' does not match the "
						+ "runtime Prototype.Version literal '" + version
						+ "' — both must be rewritten by Maven filtering or the "
						+ "scanner will still see the unpatched header");
	}

	@Test
	void upstreamMitLicenseIsBundled() throws IOException {
		final String raw = readResource(LICENSE_PATH, LICENSE_MIN_BYTES);
		// Collapse runs of whitespace so wrapped clauses still match.
		final String body = raw.replaceAll("\\s+", " ");
		assertTrue(body.contains("Copyright (c) 2005-2010 Sam Stephenson"),
				"bundled LICENSE-prototype.js does not preserve the upstream "
						+ "copyright notice");
		assertTrue(body.contains("Permission is hereby granted, free of charge"),
				"bundled LICENSE-prototype.js does not preserve the MIT "
						+ "permission grant");
		assertTrue(body.contains(
				"shall be included in all copies or substantial portions"),
				"bundled LICENSE-prototype.js does not preserve the MIT "
						+ "notice-retention clause");
		assertTrue(body.contains("THE SOFTWARE IS PROVIDED \"AS IS\""),
				"bundled LICENSE-prototype.js does not preserve the MIT "
						+ "warranty-disclaimer clause");
		assertTrue(body.contains("WITHOUT WARRANTY OF ANY KIND"),
				"bundled LICENSE-prototype.js does not preserve the MIT "
						+ "no-warranty clause");
	}

	@Test
	void stripTagsAndUnescapeHtmlRemainRemoved() throws IOException {
		final String body = readResource(RESOURCE_PATH, PROTOTYPE_JS_MIN_BYTES);
		// CVE-2020-27511 mitigation: these methods must not be installed on
		// String.prototype. The vulnerable upstream code installed them via
		// `Object.extend(String.prototype, ...)` with `stripTags: stripTags`
		// inside that hash, so the local fork comments those entries out.
		// Guard against the upstream form *and* against a future
		// re-introduction in a different shape (quoted keys, direct
		// String.prototype assignment, Object.defineProperty).
		final Pattern liveStripTags = Pattern.compile(
				"(?m)^\\s*['\"]?stripTags['\"]?\\s*[:=]"
						+ "|String\\.prototype\\.stripTags\\s*="
						+ "|defineProperty\\s*\\(\\s*String\\.prototype\\s*,\\s*['\"]stripTags['\"]");
		final Pattern liveUnescape = Pattern.compile(
				"(?m)^\\s*['\"]?unescapeHTML['\"]?\\s*[:=]"
						+ "|String\\.prototype\\.unescapeHTML\\s*="
						+ "|defineProperty\\s*\\(\\s*String\\.prototype\\s*,\\s*['\"]unescapeHTML['\"]");
		assertFalse(liveStripTags.matcher(body).find(),
				"stripTags appears to be registered on String.prototype — "
						+ "re-introduces CVE-2020-27511");
		assertFalse(liveUnescape.matcher(body).find(),
				"unescapeHTML appears to be registered on String.prototype — "
						+ "re-introduces CVE-2020-27511 (unescapeHTML internally "
						+ "calls stripTags)");
	}

	@Test
	void changesManifestExistsAtDocumentedPath() {
		// The prototype.js header and the bundled LICENSE-prototype.js
		// preamble both direct readers to prototype.js.CHANGES.md at the
		// javamelody-core module root for the full local diff and the
		// audit-trail justification. If that file is renamed or removed,
		// the SCA-mitigation paper trail breaks. Surefire runs each module's
		// tests with that module's directory as user.dir, so a relative path
		// resolves correctly here.
		final File changes = new File("prototype.js.CHANGES.md");
		assertTrue(changes.isFile(),
				"prototype.js.CHANGES.md not found at " + changes.getAbsolutePath()
						+ " — both the prototype.js header and the bundled MIT "
						+ "license preamble reference this file; if it has moved, "
						+ "those references need updating");
	}
}
