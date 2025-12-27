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
package net.bull.javamelody.internal.common;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.security.NoSuchAlgorithmException;

import org.junit.jupiter.api.Test;

/**
 * Test unitaire de la classe MessageDigestPasswordEncoder.
 * @author Emeric Vernat
 */
class TestMessageDigestPasswordEncoder {
	/** Test.
	 * @throws NoSuchAlgorithmException e */
	@Test
	void testEncodePassword() throws NoSuchAlgorithmException {
		final String algorithm = "SHA-256";
		final String password = "password";
		final String hash = new MessageDigestPasswordEncoder(algorithm).encodePassword(password);
		final String expectedHash = "{SHA-256}c33d66fe65ffcca1f2260e6982dbf0c614b6ea3ddfdb37d6142fbec0feca5245";
		assertEquals(expectedHash, hash, "encodePassword");
	}
}
