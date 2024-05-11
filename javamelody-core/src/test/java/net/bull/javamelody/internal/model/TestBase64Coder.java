// Tests for the Base64Coder class.

package net.bull.javamelody.internal.model;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Base64;
import java.util.Random;

import org.junit.Test;

/**
 * Test for Base64Coder.
 * <br/>
 * Project home page: <a href="http://www.source-code.biz/base64coder/java/">www.source-code.biz/base64coder/java</a><br>
 * Author: Christian d'Heureuse, Inventec Informatik AG, Zurich, Switzerland<br>
 * Multi-licensed: EPL / LGPL / GPL / AL / BSD / MIT.
 * Changed on format and java syntax by Emeric Vernat
 */
public class TestBase64Coder {

	/**
	 * Test Base64Coder with constant strings.
	 */
	@Test
	public void test1() {
		check("Aladdin:open sesame", "QWxhZGRpbjpvcGVuIHNlc2FtZQ=="); // example from RFC 2617
		check("", "");
		check("1", "MQ==");
		check("22", "MjI=");
		check("333", "MzMz");
		check("4444", "NDQ0NA==");
		check("55555", "NTU1NTU=");
		check("abc:def", "YWJjOmRlZg==");
	}

	private static void check(String plainText, String base64Text) {
		final String s1 = Base64Coder.encodeString(plainText);
		final String s2 = Base64Coder.decodeString(base64Text);
		if (!s1.equals(base64Text) || !s2.equals(plainText)) {
			fail("Check failed for \"" + plainText + "\" / \"" + base64Text + "\".");
		}
	}

	/**
	 * Test Base64Coder against java.util.Base64 Encoder/Decoder with random data.
	 * Line length below 76.
	 */
	@Test
	public void test2() {
		final int maxLineLen = 76 - 1; // the Base64 Mime encoder adds a CR/LF when a line is longer
		final int maxDataBlockLen = maxLineLen * 3 / 4;
		final Random rnd = new Random(0x538afb92);
		for (int i = 0; i < 100; i++) {
			final int len = rnd.nextInt(maxDataBlockLen + 1);
			final byte[] b0 = new byte[len];
			rnd.nextBytes(b0);
			final String e1 = new String(Base64Coder.encode(b0));
			final String e2 = Base64.getEncoder().encodeToString(b0);
			assertEquals("test2", e2, e1);
			final byte[] b1 = Base64Coder.decode(e1);
			final byte[] b2 = Base64.getDecoder().decode(e2);
			assertArrayEquals("test2", b0, b1);
			assertArrayEquals("test2", b0, b2);
		}
	}

	/**
	 * Test Base64Coder line encoding/decoding against java.util.Base64 Mime Encoder/Decoder
	 * with random data.
	 */
	@Test
	public void test3() {
		final int maxDataBlockLen = 512;
		final Random rnd = new Random(0x39ac7d6e);
		for (int i = 0; i < 100; i++) {
			final int len = rnd.nextInt(maxDataBlockLen + 1);
			final byte[] b0 = new byte[len];
			rnd.nextBytes(b0);
			// Base64Coder uses System.lineSeparator() but Mime Encoder uses CR LF (\r \n),
			// so we replace for linux
			final String e1 = Base64Coder.encodeLines(b0).replace(System.lineSeparator(), "\r\n");
			final String e2 = Base64.getMimeEncoder().encodeToString(b0);
			assertEquals("test3", e2, e1.trim());
			final byte[] b1 = Base64Coder.decodeLines(e1);
			final byte[] b2 = Base64.getMimeDecoder().decode(e2);
			assertArrayEquals("test3", b0, b1);
			assertArrayEquals("test3", b0, b2);
		}
	}

} // end class TestBase64Coder
