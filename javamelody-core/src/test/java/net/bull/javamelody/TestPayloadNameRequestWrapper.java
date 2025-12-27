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

import static org.easymock.EasyMock.expect;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;import java.util.HashMap;
import java.util.Map;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.easymock.Capture;
import org.easymock.EasyMock;
import org.easymock.EasyMockSupport;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.servlet.ReadListener;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Unit test for {@link PayloadNameRequestWrapper}.
 * @author rpaterson
 */
class TestPayloadNameRequestWrapper extends EasyMockSupport {
	private static final String CONTENT_TYPE_TEXT_XML = "text/xml";

	HttpServletRequest request;

	String httpMethod;
	String contentType;
	Map<String, String> headers;
	String queryString;
	String body;

	/**
	 * Setup.
	 * @throws IOException never
	 */
	@BeforeEach
	void setUp() throws IOException {
		request = createNiceMock(HttpServletRequest.class);

		//method
		httpMethod = "POST";
		expect(request.getMethod()).andAnswer(() -> httpMethod).anyTimes();

		//content type
		contentType = "text/html";
		expect(request.getContentType()).andAnswer(() -> contentType).anyTimes();

		//headers
		headers = new HashMap<>();
		final Capture<String> headerName = Capture.newInstance();
		expect(request.getHeader(EasyMock.capture(headerName)))
				.andAnswer(() -> headers.get(headerName.getValue())).anyTimes();

		//query string
		queryString = null;
		expect(request.getQueryString()).andAnswer(() -> queryString).anyTimes();

		//body
		body = "";
		expect(request.getInputStream()).andAnswer(this::createServletOutputStream);

		replayAll();
	}

	ServletInputStream createServletOutputStream() {
		final ByteArrayInputStream stream = new ByteArrayInputStream(body.getBytes());
		// CHECKSTYLE:OFF
		return new ServletInputStream() {
			// CHECKSTYLE:ON
			@Override
			public int read() throws IOException {
				return stream.read();
			}

			@Override
			public boolean isFinished() {
				return false;
			}

			@Override
			public boolean isReady() {
				return false;
			}

			@Override
			public void setReadListener(ReadListener readListener) {
				// nothing
			}
		};
	}

	private static String slurp(InputStream stream) throws IOException {
		final StringBuilder buffer = new StringBuilder();
		final Reader reader = new InputStreamReader(stream, StandardCharsets.UTF_8);
		int c = reader.read();
		while (c != -1) {
			buffer.append((char) c);
			c = reader.read();
		}
		return buffer.toString();
	}

	/**
	 * Test that normal GET request is passed through unchanged.
	 * @throws IOException on error
	 */
	@Test
	void testHttpGet() throws IOException {
		httpMethod = "GET";
		contentType = "text/html";
		body = "";
		queryString = "key=value1&key=value2";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertNull(wrapper.getPayloadRequestName(), "Should not have found name for HTTP GET");

		assertEquals(body, slurp(wrapper.getInputStream()), "Content was changed");
	}

	@Test
	void testNoContentType() throws IOException {
		contentType = null;
		body = "";
		queryString = "key=value1&key=value2";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertNull(wrapper.getPayloadRequestName(), "Should not have found name for HTTP GET");

		assertEquals(body, slurp(wrapper.getInputStream()), "Content was changed");
	}

	/**
	 * Test parsing of GWT-RPC request.
	 * @throws IOException on error
	 */
	@Test
	void testGwtRpc() throws IOException {
		contentType = "text/x-gwt-rpc";
		body = "7|0|4|http://site/path/com.example.GwtModuleName/|148050F6A52E484A068EAD552E9A6F2A|com.example.GwtRpcApi|gwtRpcMethodName|1|2|3|4|0|";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertEquals(".gwtRpcMethodName", wrapper.getPayloadRequestName(),
				"Could not parse GWT-RPC request");
		assertEquals("GWT-RPC", wrapper.getPayloadRequestType(), "GWT-RPC request type unrecognized");

		assertEquals(body, slurp(wrapper.getInputStream()), "Content was changed");
	}

	/**
	 * Test parsing of SOAP 1.1 request.
	 * @throws IOException on error
	 */
	@Test
	void testSoap11() throws IOException {
		//example request from SOAP spec
		//http://www.w3.org/TR/2000/NOTE-SOAP-20000508/#_Toc478383490
		contentType = CONTENT_TYPE_TEXT_XML;
		headers.put("SOAPAction", "Some-URI");
		body = "<SOAP-ENV:Envelope                                                        \n"
				+ "  xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"         \n"
				+ "  SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">\n"
				+ "   <SOAP-ENV:Body>                                                     \n"
				+ "       <m:GetLastTradePrice xmlns:m=\"Some-URI\">                      \n"
				+ "           <symbol>DIS</symbol>                                        \n"
				+ "       </m:GetLastTradePrice>                                          \n"
				+ "   </SOAP-ENV:Body>                                                    \n"
				+ "</SOAP-ENV:Envelope>                                                    ";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertEquals(".GetLastTradePrice", wrapper.getPayloadRequestName(),
				"Could not parse SOAP 1.1 request");
		assertEquals("SOAP", wrapper.getPayloadRequestType(), "SOAP request type unrecognized");

		assertEquals(body, slurp(wrapper.getInputStream()), "Content was changed");

		assertNotNull(wrapper.getReader(), "getReader");
	}

	/**
	 * Test parsing of SOAP 1.1 request.
	 * @throws IOException on error
	 */
	@Test
	void testSoap11WithoutSOAPAction() throws IOException {
		//example request from SOAP spec
		//http://www.w3.org/TR/2000/NOTE-SOAP-20000508/#_Toc478383490
		contentType = CONTENT_TYPE_TEXT_XML;
		headers.put("notSOAPAction", "Some-URI");
		body = "<SOAP-ENV:Envelope                                                        \n"
				+ "  xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"         \n"
				+ "  SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">\n"
				+ "   <SOAP-ENV:Body>                                                     \n"
				+ "       <m:GetLastTradePrice xmlns:m=\"Some-URI\">                      \n"
				+ "           <symbol>DIS</symbol>                                        \n"
				+ "       </m:GetLastTradePrice>                                          \n"
				+ "   </SOAP-ENV:Body>                                                    \n"
				+ "</SOAP-ENV:Envelope>                                                    ";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertNull(wrapper.getPayloadRequestName(), "Could not parse SOAP 1.1 request");
		assertNull(wrapper.getPayloadRequestType(), "SOAP request type unrecognized");

		assertEquals(body, slurp(wrapper.getInputStream()), "Content was changed");
	}

	/**
	 * Test parsing of SOAP 1.1 request with "mandatory header".
	 * @throws IOException on error
	 */
	@Test
	void testSoap11MandatoryHeader() throws IOException {
		//example request from SOAP spec
		//http://www.w3.org/TR/2000/NOTE-SOAP-20000508/#_Toc478383539

		contentType = CONTENT_TYPE_TEXT_XML;
		headers.put("SOAPAction", "Some-URI");
		body = "<SOAP-ENV:Envelope                                                         \n"
				+ "  xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"          \n"
				+ "  SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">\n"
				+ "   <SOAP-ENV:Header>                                                    \n"
				+ "       <t:Transaction                                                   \n"
				+ "           xmlns:t=\"some-URI\"                                         \n"
				+ "           SOAP-ENV:mustUnderstand=\"1\">                               \n"
				+ "               5                                                        \n"
				+ "       </t:Transaction>                                                 \n"
				+ "   </SOAP-ENV:Header>                                                   \n"
				+ "   <SOAP-ENV:Body>                                                      \n"
				+ "       <m:GetLastTradePrice xmlns:m=\"Some-URI\">                       \n"
				+ "           <symbol>DEF</symbol>                                         \n"
				+ "       </m:GetLastTradePrice>                                           \n"
				+ "   </SOAP-ENV:Body>                                                     \n"
				+ "</SOAP-ENV:Envelope>                                                     ";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertEquals(".GetLastTradePrice", wrapper.getPayloadRequestName(),
				"Could not parse SOAP 1.1 request with 'mandatory header'");
		assertEquals("SOAP", wrapper.getPayloadRequestType(), "SOAP request type unrecognized");

		assertEquals(body, slurp(wrapper.getInputStream()), "Content was changed");
	}

	/**
	 * Test parsing of SOAP 1.1 request with multiple request parameters.
	 * @throws IOException on error
	 */
	@Test
	void testSoap11MultipleRequestParameters() throws IOException {
		//example request from SOAP spec
		//http://www.w3.org/TR/2000/NOTE-SOAP-20000508/#_Ref477795992

		contentType = CONTENT_TYPE_TEXT_XML;
		headers.put("SOAPAction", "Some-URI");
		body = "<SOAP-ENV:Envelope                                                        \n"
				+ "  xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"         \n"
				+ "  SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">\n"
				+ "   <SOAP-ENV:Body>                                                     \n"
				+ "       <m:GetLastTradePriceDetailed xmlns:m=\"Some-URI\">              \n"
				+ "           <Symbol>DEF</Symbol>                                        \n"
				+ "           <Company>DEF Corp</Company>                                 \n"
				+ "           <Price>34.1</Price>                                         \n"
				+ "       </m:GetLastTradePriceDetailed>                                  \n"
				+ "   </SOAP-ENV:Body>                                                    \n"
				+ "</SOAP-ENV:Envelope>                                                    ";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertEquals(".GetLastTradePriceDetailed", wrapper.getPayloadRequestName(),
				"Could not parse SOAP 1.1 request with mutliple request parameters");
		assertEquals("SOAP", wrapper.getPayloadRequestType(), "SOAP request type unrecognized");

		assertEquals(body, slurp(wrapper.getInputStream()), "Content was changed");
	}

	/**
	 * Test parsing of SOAP 1.2 request.
	 * @throws IOException on error
	 */
	@Test
	void testSoap12() throws IOException {
		//example request from SOAP spec
		//http://www.w3.org/TR/soap12-part0/#Ref47748839611

		contentType = "application/soap+xml";
		body = "<?xml version='1.0' ?>                                                        \n"
				+ "	<env:Envelope xmlns:env=\"http://www.w3.org/2003/05/soap-envelope\" >     \n"
				+ " <env:Header>                                                              \n"
				+ "   <t:transaction                                                          \n"
				+ "           xmlns:t=\"http://thirdparty.example.org/transaction\"           \n"
				+ "           env:encodingStyle=\"http://example.com/encoding\"               \n"
				+ "           env:mustUnderstand=\"true\" >5</t:transaction>                  \n"
				+ " </env:Header>                                                             \n"
				+ " <env:Body>                                                                \n"
				+ "  <m:chargeReservation                                                     \n"
				+ "     env:encodingStyle=\"http://www.w3.org/2003/05/soap-encoding\"         \n"
				+ "          xmlns:m=\"http://travelcompany.example.org/\">                   \n"
				+ "   <m:reservation xmlns:m=\"http://travelcompany.example.org/reservation\">\n"
				+ "    <m:code>FT35ZBQ</m:code>                                               \n"
				+ "   </m:reservation>                                                        \n"
				+ "    <o:creditCard xmlns:o=\"http://mycompany.example.com/financial\">      \n"
				+ "     <n:name xmlns:n=\"http://mycompany.example.com/employees\">           \n"
				+ "           Ã…ke JÃ³gvan Ã˜yvind                                               \n"
				+ "     </n:name>                                                             \n"
				+ "     <o:number>123456789099999</o:number>                                  \n"
				+ "     <o:expiration>2005-02</o:expiration>                                  \n"
				+ "    </o:creditCard>                                                        \n"
				+ "   </m:chargeReservation                                                   \n"
				+ "  </env:Body>                                                              \n"
				+ "</env:Envelope>                                                             ";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertEquals(".chargeReservation", wrapper.getPayloadRequestName(),
				"Could not parse SOAP 1.2 request");
		assertEquals("SOAP", wrapper.getPayloadRequestType(), "SOAP request type unrecognized");

		assertEquals(body, slurp(wrapper.getInputStream()), "Content was changed");

		assertNotNull(wrapper.getReader(), "getReader");
	}

	/**
	 * Test scan for child tag.
	 * @throws XMLStreamException on error
	 */
	@Test
	void testScanForChildTag() throws XMLStreamException {
		assertTrue(scanForChildTag("child", "<parent><child/></parent>"), "Could not find child tag");

		assertFalse(scanForChildTag("notChild", "<parent><child/></parent>"), "Found wrong tag");

		//don't find descendant tags
		assertFalse(scanForChildTag("descendant", "<parent><child><descendant/></child></parent>"),
				"Found descendant tag");
	}

	private boolean scanForChildTag(String tagName, String xml) throws XMLStreamException {
		final XMLInputFactory factory = XMLInputFactory.newInstance();
		final XMLStreamReader reader = factory
				.createXMLStreamReader(new ByteArrayInputStream(xml.getBytes()));

		//advance to first tag (reader starts at "begin document")
		reader.next();

		final boolean found = PayloadNameRequestWrapper.scanForChildTag(reader, tagName);

		if (found) {
			assertEquals(tagName, reader.getLocalName(), "Found wrong tag");
		}

		reader.close();

		return found;
	}
}
