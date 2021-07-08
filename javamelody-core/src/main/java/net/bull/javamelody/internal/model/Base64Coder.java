// Copyright 2003-2010 Christian d'Heureuse, Inventec Informatik AG, Zurich, Switzerland
// www.source-code.biz, www.inventec.ch/chdh
//
// This module is multi-licensed and may be used under the terms
// of any of the following licenses:
//
//  EPL, Eclipse Public License, V1.0 or later, http://www.eclipse.org/legal
//  LGPL, GNU Lesser General Public License, V2.1 or later, http://www.gnu.org/licenses/lgpl.html
//  GPL, GNU General Public License, V2 or later, http://www.gnu.org/licenses/gpl.html
//  AL, Apache License, V2.0 or later, http://www.apache.org/licenses
//  BSD, BSD License, http://www.opensource.org/licenses/bsd-license.php
//  MIT, MIT License, http://www.opensource.org/licenses/MIT
//
// Please contact the author if you need another license.
// This module is provided "as is", without warranties of any kind.

package net.bull.javamelody.internal.model;

import java.util.Arrays;

/**
 * A Base64 encoder/decoder.
 *
 * <p>
 * This class is used to encode and decode data in Base64 format as described in RFC 1521.
 *
 * <p>
 * Project home page: <a href="http://www.source-code.biz/base64coder/java/">www.source-code.biz/base64coder/java</a><br>
 * Author: Christian d'Heureuse, Inventec Informatik AG, Zurich, Switzerland<br>
 * Multi-licensed: EPL / LGPL / GPL / AL / BSD / MIT.
 * Changed on format and java syntax by Emeric Vernat
 */
public final class Base64Coder {

	// The line separator string of the operating system.
	private static final String SYSTEM_LINE_SEPARATOR = System.lineSeparator();

	// Mapping table from 6-bit nibbles to Base64 characters.
	private static final char[] MAP1 = new char[64];

	static {
		int i = 0;
		for (char c = 'A'; c <= 'Z'; c++) {
			MAP1[i++] = c;
		}
		for (char c = 'a'; c <= 'z'; c++) {
			MAP1[i++] = c;
		}
		for (char c = '0'; c <= '9'; c++) {
			MAP1[i++] = c;
		}
		MAP1[i++] = '+';
		MAP1[i++] = '/';
	}

	// Mapping table from Base64 characters to 6-bit nibbles.
	private static final byte[] MAP2 = new byte[128];

	static {
		Arrays.fill(MAP2, (byte) -1);
		for (int i = 0; i < 64; i++) {
			MAP2[MAP1[i]] = (byte) i;
		}
	}

	// Dummy constructor.
	private Base64Coder() {
	}

	/**
	* Encodes a string into Base64 format.
	* No blanks or line breaks are inserted.
	* @param s  A String to be encoded.
	* @return   A String containing the Base64 encoded data.
	*/
	public static String encodeString(String s) {
		return new String(encode(s.getBytes()));
	}

	/**
	* Encodes a byte array into Base 64 format and breaks the output into lines of 76 characters.
	* This method is compatible with <code>sun.misc.BASE64Encoder.encodeBuffer(byte[])</code>.
	* @param in  An array containing the data bytes to be encoded.
	* @return    A String containing the Base64 encoded data, broken into lines.
	*/
	public static String encodeLines(byte[] in) {
		return encodeLines(in, 0, in.length, 76, SYSTEM_LINE_SEPARATOR);
	}

	/**
	* Encodes a byte array into Base 64 format and breaks the output into lines.
	* @param in            An array containing the data bytes to be encoded.
	* @param iOff          Offset of the first byte in <code>in</code> to be processed.
	* @param iLen          Number of bytes to be processed in <code>in</code>, starting at <code>iOff</code>.
	* @param lineLen       Line length for the output data. Should be a multiple of 4.
	* @param lineSeparator The line separator to be used to separate the output lines.
	* @return              A String containing the Base64 encoded data, broken into lines.
	*/
	public static String encodeLines(byte[] in, int iOff, int iLen, int lineLen,
			String lineSeparator) {
		final int blockLen = lineLen * 3 / 4;
		if (blockLen <= 0) {
			throw new IllegalArgumentException();
		}
		final int lines = (iLen + blockLen - 1) / blockLen;
		final int bufLen = (iLen + 2) / 3 * 4 + lines * lineSeparator.length();
		final StringBuilder buf = new StringBuilder(bufLen);
		int ip = 0;
		while (ip < iLen) {
			final int l = Math.min(iLen - ip, blockLen);
			buf.append(encode(in, iOff + ip, l));
			buf.append(lineSeparator);
			ip += l;
		}
		return buf.toString();
	}

	/**
	* Encodes a byte array into Base64 format.
	* No blanks or line breaks are inserted in the output.
	* @param in  An array containing the data bytes to be encoded.
	* @return    A character array containing the Base64 encoded data.
	*/
	public static char[] encode(byte[] in) {
		return encode(in, 0, in.length);
	}

	//	/**
	//	* Encodes a byte array into Base64 format.
	//	* No blanks or line breaks are inserted in the output.
	//	* @param in    An array containing the data bytes to be encoded.
	//	* @param iLen  Number of bytes to process in <code>in</code>.
	//	* @return      A character array containing the Base64 encoded data.
	//	*/
	//	public static char[] encode(byte[] in, int iLen) {
	//		return encode(in, 0, iLen);
	//	}

	/**
	* Encodes a byte array into Base64 format.
	* No blanks or line breaks are inserted in the output.
	* @param in    An array containing the data bytes to be encoded.
	* @param iOff  Offset of the first byte in <code>in</code> to be processed.
	* @param iLen  Number of bytes to process in <code>in</code>, starting at <code>iOff</code>.
	* @return      A character array containing the Base64 encoded data.
	*/
	public static char[] encode(byte[] in, int iOff, int iLen) { // NOPMD
		final int oDataLen = (iLen * 4 + 2) / 3; // output length without padding
		final int oLen = (iLen + 2) / 3 * 4; // output length including padding
		final char[] out = new char[oLen];
		int ip = iOff;
		final int iEnd = iOff + iLen;
		int op = 0;
		while (ip < iEnd) {
			final int i0 = in[ip++] & 0xff;
			final int i1 = ip < iEnd ? in[ip++] & 0xff : 0;
			final int i2 = ip < iEnd ? in[ip++] & 0xff : 0;
			final int o0 = i0 >>> 2;
			final int o1 = (i0 & 3) << 4 | i1 >>> 4;
			final int o2 = (i1 & 0xf) << 2 | i2 >>> 6;
			final int o3 = i2 & 0x3F;
			out[op++] = MAP1[o0];
			out[op++] = MAP1[o1];
			out[op] = op < oDataLen ? MAP1[o2] : '=';
			op++;
			out[op] = op < oDataLen ? MAP1[o3] : '=';
			op++;
		}
		return out;
	}

	/**
	* Decodes a string from Base64 format.
	* No blanks or line breaks are allowed within the Base64 encoded input data.
	* @param s  A Base64 String to be decoded.
	* @return   A String containing the decoded data.
	*/
	public static String decodeString(String s) {
		return new String(decode(s));
	}

	/**
	* Decodes a byte array from Base64 format and ignores line separators, tabs and blanks.
	* CR, LF, Tab and Space characters are ignored in the input data.
	* This method is compatible with <code>sun.misc.BASE64Decoder.decodeBuffer(String)</code>.
	* @param s  A Base64 String to be decoded.
	* @return   An array containing the decoded data bytes.
	*/
	public static byte[] decodeLines(String s) {
		final char[] buf = new char[s.length()];
		int p = 0;
		for (int ip = 0; ip < s.length(); ip++) {
			final char c = s.charAt(ip);
			if (c != ' ' && c != '\r' && c != '\n' && c != '\t') {
				buf[p++] = c;
			}
		}
		return decode(buf, 0, p);
	}

	/**
	* Decodes a byte array from Base64 format.
	* No blanks or line breaks are allowed within the Base64 encoded input data.
	* @param s  A Base64 String to be decoded.
	* @return   An array containing the decoded data bytes.
	*/
	public static byte[] decode(String s) {
		return decode(s.toCharArray());
	}

	/**
	* Decodes a byte array from Base64 format.
	* No blanks or line breaks are allowed within the Base64 encoded input data.
	* @param in  A character array containing the Base64 encoded data.
	* @return    An array containing the decoded data bytes.
	*/
	public static byte[] decode(char[] in) {
		return decode(in, 0, in.length);
	}

	/**
	* Decodes a byte array from Base64 format.
	* No blanks or line breaks are allowed within the Base64 encoded input data.
	* @param in    A character array containing the Base64 encoded data.
	* @param iOff  Offset of the first character in <code>in</code> to be processed.
	* @param iLen  Number of characters to process in <code>in</code>, starting at <code>iOff</code>.
	* @return      An array containing the decoded data bytes.
	*/
	// CHECKSTYLE:OFF
	public static byte[] decode(char[] in, int iOff, int iLen) { // NOPMD
		// CHECKSTYLE:ON
		if (iLen % 4 != 0) {
			throw new IllegalArgumentException(
					"Length of Base64 encoded input string is not a multiple of 4.");
		}
		int iLength = iLen;
		while (iLength > 0 && in[iOff + iLength - 1] == '=') {
			iLength--;
		}
		final int oLen = iLength * 3 / 4;
		final byte[] out = new byte[oLen];
		int ip = iOff;
		final int iEnd = iOff + iLength;
		int op = 0;
		while (ip < iEnd) {
			final int i0 = in[ip++];
			final int i1 = in[ip++];
			final int i2 = ip < iEnd ? in[ip++] : 'A';
			final int i3 = ip < iEnd ? in[ip++] : 'A';
			if (i0 > 127 || i1 > 127 || i2 > 127 || i3 > 127) {
				throw new IllegalArgumentException("Illegal character in Base64 encoded data.");
			}
			final int b0 = MAP2[i0];
			final int b1 = MAP2[i1];
			final int b2 = MAP2[i2];
			final int b3 = MAP2[i3];
			if (b0 < 0 || b1 < 0 || b2 < 0 || b3 < 0) {
				throw new IllegalArgumentException("Illegal character in Base64 encoded data.");
			}
			final int o0 = b0 << 2 | b1 >>> 4;
			final int o1 = (b1 & 0xf) << 4 | b2 >>> 2;
			final int o2 = (b2 & 3) << 6 | b3;
			out[op++] = (byte) o0;
			if (op < oLen) {
				out[op++] = (byte) o1;
			}
			if (op < oLen) {
				out[op++] = (byte) o2;
			}
		}
		return out;
	}
} // end class Base64Coder
