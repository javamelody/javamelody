/*
 * Copyright 2008-2012 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

//20091201 dhartford
//20100519 dhartford adjustments for UTF-8, however did not have an impact so removed.
//20100520 dhartford adjustments for reader/inputstream.
//20110206 evernat   refactoring

/**
 * Simple Wrapper class to get the payload for GWT RPC method name retrieval.
 * @author dhartford
 */
class GWTRequestWrapper extends HttpServletRequestWrapper {
	private final byte[] originalPayload; //this is a bit of a memory hog for just profiling, but no other way.
	private String gwtRpcMethodName;
	private ServletInputStream inputStream;
	private BufferedReader reader;

	/**
	 * Constructor.
	 * @param request the original HttpServletRequest
	 * @throws IOException In case of issues.
	 */
	GWTRequestWrapper(HttpServletRequest request) throws IOException {
		super(request);
		//Intent is to get the 7th (0-based array 6th) pipe delimited value.
		//If there is any room for optimization, it would be here and cross-check with MonitoringFilter.
		final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		TransportFormat.pump(request.getInputStream(), byteArrayOutputStream);
		originalPayload = byteArrayOutputStream.toByteArray();

		//decode the specified array of bytes using the platform's default charset
		final String payload = new String(originalPayload);
		//parse the gwt rpc method name
		parseGwtRpcMethodName(payload);
	}

	private void parseGwtRpcMethodName(String payload) {
		//commented out code uses GWT-user library for a more 'proper' approach.
		//GWT-user library approach is more future-proof, but requires more dependency management.
		//				RPCRequest decodeRequest = RPC.decodeRequest(readLine);
		//				gwtmethodname = decodeRequest.getMethod().getName();

		final String[] split = payload.split("\\|"); //pipe delimited
		if (split.length > 6 && split[6].length() > 0) {
			gwtRpcMethodName = split[6];
		}
	}

	/** {@inheritDoc} */
	@Override
	public BufferedReader getReader() throws IOException {
		if (reader == null) {
			// use character encoding as said in the API
			final String characterEncoding = this.getCharacterEncoding();
			if (characterEncoding == null) {
				reader = new BufferedReader(new InputStreamReader(this.getInputStream()));
			} else {
				reader = new BufferedReader(new InputStreamReader(this.getInputStream(),
						characterEncoding));
			}
		}
		return reader;
	}

	/** {@inheritDoc} */
	@Override
	public ServletInputStream getInputStream() throws IOException {
		if (inputStream == null) {
			final ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(
					originalPayload);
			inputStream = new ServletInputStream() {
				/** {@inheritDoc} */
				@Override
				public int read() throws IOException {
					return byteArrayInputStream.read();
				}
			};
		}
		return inputStream;
	}

	String getGwtRpcMethodName() {
		return gwtRpcMethodName;
	}
}
