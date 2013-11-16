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

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.regex.Pattern;

import javax.servlet.ServletInputStream;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

//20091201 dhartford GWTRequestWrapper
//20100519 dhartford adjustments for UTF-8, however did not have an impact so removed.
//20100520 dhartford adjustments for reader/inputstream.
//20110206 evernat   refactoring
//20131111 roy.paterson   SOAP request wrapper
//20131111 evernat   refactoring

/**
 * Simple Wrapper class to inspect payload for name.
 * @author dhartford, roy.paterson, evernat
 */
public class PayloadNameRequestWrapper extends HttpServletRequestWrapper {
	private static final Pattern GWT_RPC_SEPARATOR_CHAR_PATTERN = Pattern.compile(Pattern
			.quote("|"));

	/**
	 * Name of request, or null if we don't know based on payload @null
	 */
	private String name;

	/**
	 * Type of request if name != null, or null if we don't know based on the payload @null
	 */
	private String requestType;

	private BufferedInputStream bufferedInputStream;
	private ServletInputStream inputStream;
	private BufferedReader reader;

	/**
	 * Constructor.
	 * @param request the original HttpServletRequest
	 * @throws IOException In case of issues.
	 */
	public PayloadNameRequestWrapper(HttpServletRequest request) throws IOException {
		super(request);

		initialize(request);
	}

	protected void initialize(HttpServletRequest request) throws IOException {
		//name on a best-effort basis
		name = null;
		requestType = null;

		final String contentType = request.getContentType();
		if (contentType == null) {
			//don't know how to handle this content type
			return;
		}

		if (!"POST".equalsIgnoreCase(request.getMethod())) {
			//no payload
			return;
		}

		//Try look for name in payload on a best-effort basis...
		try {
			if (contentType.startsWith("text/x-gwt-rpc")) {
				//parse GWT-RPC method name
				bufferedInputStream = createBufferedInputStream();
				name = parseGwtRpcMethodName(bufferedInputStream, getCharacterEncoding());
				requestType = "GWT-RPC";
			} else if (contentType.startsWith("application/soap+xml") //SOAP 1.2
					|| contentType.startsWith("text/xml") //SOAP 1.1
					&& request.getHeader("SOAPAction") != null) {
				//parse SOAP method name
				bufferedInputStream = createBufferedInputStream();
				name = parseSoapMethodName(bufferedInputStream, getCharacterEncoding());
				requestType = "SOAP";
			} else {
				//don't know how to name this request based on payload
				//(don't parse if text/xml for XML-RPC, because it is obsolete)
				name = null;
				requestType = null;
			}
		} catch (final Exception e) {
			LOG.debug("Error trying to parse payload content for request name", e);

			//best-effort - couldn't figure it out
			name = null;
			requestType = null;
		} finally {
			//reset stream so application is unaffected
			if (bufferedInputStream != null) {
				bufferedInputStream.reset();
			}
		}
	}

	private BufferedInputStream createBufferedInputStream() throws IOException {
		//workaround Tomcat issue with form POSTs
		//see http://stackoverflow.com/questions/18489399/read-httpservletrequests-post-body-and-then-call-getparameter-in-tomcat
		final ServletRequest request = getRequest();
		request.getParameterMap();

		//buffer the payload so we can inspect it
		final BufferedInputStream result = new BufferedInputStream(request.getInputStream());
		// and mark to allow the stream to be reset
		result.mark(Integer.MAX_VALUE);
		return result;
	}

	/**
	 * Try to parse GWT-RPC method name from request body stream.  Does not close the stream.
	 *
	 * @param stream GWT-RPC request body stream @nonnull
	 * @param charEncoding character encoding of stream, or null for platform default @null
	 * @return GWT-RPC method name, or null if unable to parse @null
	 */
	@SuppressWarnings("resource")
	private static String parseGwtRpcMethodName(InputStream stream, String charEncoding) {
		//commented out code uses GWT-user library for a more 'proper' approach.
		//GWT-user library approach is more future-proof, but requires more dependency management.
		//				RPCRequest decodeRequest = RPC.decodeRequest(readLine);
		//				gwtmethodname = decodeRequest.getMethod().getName();

		try {
			final Scanner scanner;
			if (charEncoding == null) {
				scanner = new Scanner(stream);
			} else {
				scanner = new Scanner(stream, charEncoding);
			}
			scanner.useDelimiter(GWT_RPC_SEPARATOR_CHAR_PATTERN); //AbstractSerializationStream.RPC_SEPARATOR_CHAR

			//AbstractSerializationStreamReader.prepareToRead(...)
			scanner.next(); //stream version number
			scanner.next(); //flags

			//ServerSerializationStreamReader.deserializeStringTable()
			scanner.next(); //type name count

			//ServerSerializationStreamReader.preapreToRead(...)
			scanner.next(); //module base URL
			scanner.next(); //strong name

			//RPC.decodeRequest(...)
			scanner.next(); //service interface name
			return "." + scanner.next(); //service method name

			//note we don't close the scanner because we don't want to close the underlying stream
		} catch (final NoSuchElementException e) {
			LOG.debug("Unable to parse GWT-RPC request", e);

			//code above is best-effort - we were unable to parse GWT payload so
			//treat as a normal HTTP request
			return null;
		}
	}

	/**
	 * Scan xml for tag child of the current element
	 *
	 * @param reader reader, must be at "start element" @nonnull
	 * @param tagName name of child tag to find @nonnull
	 * @return if found tag
	 * @throws XMLStreamException on error
	 */
	private static boolean scanForChildTag(XMLStreamReader reader, String tagName)
			throws XMLStreamException {
		assert reader.isStartElement();

		int level = -1;
		while (reader.hasNext()) {
			//keep track of level so we only search children, not descendants
			if (reader.isStartElement()) {
				level++;
			} else if (reader.isEndElement()) {
				level--;
			}
			if (level < 0) {
				//end parent tag - no more children
				break;
			}

			reader.next();

			if (level == 0 && reader.isStartElement() && reader.getLocalName().equals(tagName)) {
				return true; //found
			}
		}
		return false; //got to end of parent element and not found
	}

	/**
	 * Try to parse SOAP method name from request body stream.  Does not close the stream.
	 *
	 * @param stream SOAP request body stream @nonnull
	 * @param charEncoding character encoding of stream, or null for platform default @null
	 * @return SOAP method name, or null if unable to parse @null
	 */
	private static String parseSoapMethodName(InputStream stream, String charEncoding) {
		try {
			final XMLInputFactory factory = XMLInputFactory.newFactory();
			final XMLStreamReader xmlReader;
			if (charEncoding != null) {
				xmlReader = factory.createXMLStreamReader(stream, charEncoding);
			} else {
				xmlReader = factory.createXMLStreamReader(stream);
			}

			//best-effort parsing

			//start document, go to first tag
			xmlReader.nextTag();

			//expect first tag to be "Envelope"
			if (!"Envelope".equals(xmlReader.getLocalName())) {
				LOG.debug("Unexpected first tag of SOAP request: '" + xmlReader.getLocalName()
						+ "' (expected 'Envelope')");
				return null; //failed
			}

			//scan for body tag
			if (!scanForChildTag(xmlReader, "Body")) {
				LOG.debug("Unable to find SOAP 'Body' tag");
				return null; //failed
			}

			xmlReader.nextTag();

			//tag is method name
			return "." + xmlReader.getLocalName();
		} catch (final XMLStreamException e) {
			LOG.debug("Unable to parse SOAP request", e);
			//failed
			return null;
		}
	}

	/** {@inheritDoc} */
	@Override
	public BufferedReader getReader() throws IOException {
		if (bufferedInputStream == null) {
			return super.getReader();
		}
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
		if (bufferedInputStream == null) {
			return super.getInputStream();
		}
		if (inputStream == null) {
			final BufferedInputStream myBufferedInputStream = bufferedInputStream;
			inputStream = new ServletInputStream() {
				/** {@inheritDoc} */
				@Override
				public int read() throws IOException {
					return myBufferedInputStream.read();
				}
			};
		}
		return inputStream;
	}

	/**
	 * @return name of request, or null if we can't figure out a good name based on
	 *   the request payload @null
	 */
	public String getPayloadRequestName() {
		return name;
	}

	/**
	 * Get type of request.  If {@link #getPayloadRequestName()} returns non-null then
	 * this method also returns non-null.
	 *
	 * @return type of request if or null if don't know @null
	 */
	public String getPayloadRequestType() {
		return requestType;
	}
}
