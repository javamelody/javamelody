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

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Encode (hash) passwords to prevent writing them as clear texts.
 * The hash algorithm SHA-256 with a salt is theoretically not reversible in human time.
 * @author Emeric Vernat
 */
public class MessageDigestPasswordEncoder {
	private static final byte[] SALT = "javamelody".getBytes(StandardCharsets.UTF_8);

	private static final char[] HEX_ARRAY = "0123456789abcdef".toCharArray();

	private final String algorithm;

	/**
	 * Constructor.
	 * @param algorithm String like <code>SHA-256</code> (recommended), <code>SHA-512</code>, <code>SHA-384</code>
	 *        or (not recommended) <code>SHA-224</code>, <code>SHA-1</code>, <code>MD5</code>
	 *        or more in <a href="https://docs.oracle.com/en/java/javase/11/docs/specs/security/standard-names.html#messagedigest-algorithms">Java 11</a>
	 */
	public MessageDigestPasswordEncoder(String algorithm) {
		super();
		this.algorithm = algorithm;
	}

	/**
	 * To be used from commande line to hash password(s) using SHA-256
	 * @param args String[]
	 * @throws NoSuchAlgorithmException Not possible for SHA-256
	 */
	public static void main(String[] args) throws NoSuchAlgorithmException {
		if (args.length == 0) {
			print("No password to hash in arguments");
		} else {
			final String algorithm = "SHA-256";
			final MessageDigestPasswordEncoder messageDigestPasswordEncoder = new MessageDigestPasswordEncoder(
					algorithm);
			for (int i = 0; i < args.length; i++) {
				if (i > 0) {
					print(" ");
				}
				print(messageDigestPasswordEncoder.encodePassword(args[i]));
			}
		}
	}

	private static void print(String msg) {
		System.out.print(msg); // NOPMD
	}

	/**
	 * Encode (hash) a password.
	 * @param password String like <code>password</code>
	 * @return String like <code>{SHA-256}c33d66fe65ffcca1f2260e6982dbf0c614b6ea3ddfdb37d6142fbec0feca5245</code>
	 * @throws NoSuchAlgorithmException In case algorithm in constructor is not supported
	 */
	public String encodePassword(String password) throws NoSuchAlgorithmException {
		// compute digest of the password
		final MessageDigest messageDigest = MessageDigest.getInstance(algorithm);
		messageDigest.update(password.getBytes(StandardCharsets.UTF_8));
		// we need a SALT against rainbow tables
		messageDigest.update(SALT);
		final byte[] digest = messageDigest.digest();
		// hexadecimal encoding of the digest
		return "{" + algorithm + '}' + hexa(digest);
	}

	private String hexa(byte[] digest) {
		final char[] chars = new char[digest.length * 2];
		for (int j = 0; j < digest.length; j++) {
			final int v = digest[j] & 0xFF;
			chars[j * 2] = HEX_ARRAY[v >>> 4];
			chars[j * 2 + 1] = HEX_ARRAY[v & 0x0F];
		}
		return new String(chars);
	}
}
