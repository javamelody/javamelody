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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ReadListener;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpUtils;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.easymock.Capture;
import org.easymock.EasyMock;
import org.easymock.EasyMockSupport;
import org.easymock.IAnswer;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit test for {@link PayloadNameRequestWrapper}.
 * @author rpaterson
 */
@SuppressWarnings("deprecation")
public class TestPayloadNameRequestWrapper extends EasyMockSupport {
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
	@Before
	public void setUp() throws IOException {
		request = createNiceMock(HttpServletRequest.class);

		//method
		httpMethod = "POST";
		expect(request.getMethod()).andAnswer(new IAnswer<String>() {
			@Override
			public String answer() throws Throwable {
				return httpMethod;
			}
		}).anyTimes();

		//content type
		contentType = "text/html";
		expect(request.getContentType()).andAnswer(new IAnswer<String>() {
			@Override
			public String answer() throws Throwable {
				return contentType;
			}
		}).anyTimes();

		//headers
		headers = new HashMap<String, String>();
		final Capture<String> headerName = Capture.newInstance();
		expect(request.getHeader(EasyMock.capture(headerName))).andAnswer(new IAnswer<String>() {
			@Override
			public String answer() throws Throwable {
				return headers.get(headerName.getValue());
			}
		}).anyTimes();

		//query string
		queryString = null;
		expect(request.getQueryString()).andAnswer(new IAnswer<String>() {
			@Override
			public String answer() throws Throwable {
				return queryString;
			}
		}).anyTimes();

		//body
		body = "";
		expect(request.getInputStream()).andAnswer(new IAnswer<ServletInputStream>() {
			@Override
			public ServletInputStream answer() throws Throwable {
				return createServletOutputStream();
			}
		});

		//params
		expect(request.getParameterMap()).andAnswer(new IAnswer<Map<String, String[]>>() {

			Map<String, String[]> parameterMap;

			@Override
			public Map<String, String[]> answer() throws Throwable {

				if (parameterMap == null) {

					parameterMap = getParameterMap();
				}

				return parameterMap;
			}
		}).anyTimes();

		replayAll();
	}

	Map<String, String[]> getParameterMap() throws IOException {
		final Map<String, String[]> parameterMap = new HashMap<String, String[]>();

		if (request.getQueryString() != null) {
			final Map<String, String[]> queryParams = HttpUtils
					.parseQueryString(request.getQueryString());
			parameterMap.putAll(queryParams);
		}

		if (request.getContentType() != null
				&& request.getContentType().startsWith("application/x-www-form-urlencoded")) {
			//get form params from body data
			//note this consumes the inputstream!  But that's what happens on Tomcat
			final Map<String, String[]> bodyParams = HttpUtils.parsePostData(body.length(),
					request.getInputStream());

			//merge body params and query params
			for (final String key : bodyParams.keySet()) {

				final String[] queryValues = parameterMap.get(key);
				final String[] bodyValues = bodyParams.get(key);

				final List<String> values = new ArrayList<String>();
				if (queryValues != null) {
					values.addAll(Arrays.asList(queryValues));
				}

				values.addAll(Arrays.asList(bodyValues));

				parameterMap.put(key, values.toArray(new String[0]));
			}
		} //end if form-encoded params in request body

		return parameterMap;
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
		final Reader reader = new InputStreamReader(stream);
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
	public void testHttpGet() throws IOException {
		httpMethod = "GET";
		contentType = "text/html";
		body = "";
		queryString = "key=value1&key=value2";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertEquals("Should not have found name for HTTP GET", null,
				wrapper.getPayloadRequestName());

		assertEquals("Content was changed", body, slurp(wrapper.getInputStream()));
	}

	@Test
	public void testNoContentType() throws IOException {
		contentType = null;
		body = "";
		queryString = "key=value1&key=value2";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertEquals("Should not have found name for HTTP GET", null,
				wrapper.getPayloadRequestName());

		assertEquals("Content was changed", body, slurp(wrapper.getInputStream()));
	}

	/**
	 * Test parsing of GWT-RPC request.
	 * @throws IOException on error
	 */
	@Test
	public void testGwtRpc() throws IOException {
		contentType = "text/x-gwt-rpc";
		body = "7|0|4|http://site/path/com.example.GwtModuleName/|148050F6A52E484A068EAD552E9A6F2A|com.example.GwtRpcApi|gwtRpcMethodName|1|2|3|4|0|";

		final PayloadNameRequestWrapper wrapper = new PayloadNameRequestWrapper(request);
		wrapper.initialize();

		assertEquals("Could not parse GWT-RPC request", ".gwtRpcMethodName",
				wrapper.getPayloadRequestName());
		assertEquals("GWT-RPC request type unrecognized", "GWT-RPC",
				wrapper.getPayloadRequestType());

		assertEquals("Content was changed", body, slurp(wrapper.getInputStream()));
	}

	/**
	 * Test parsing of SOAP 1.1 request.
	 * @throws IOException on error
	 */
	@Test
	public void testSoap11() throws IOException {
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

		assertEquals("Could not parse SOAP 1.1 request", ".GetLastTradePrice",
				wrapper.getPayloadRequestName());
		assertEquals("SOAP request type unrecognized", "SOAP", wrapper.getPayloadRequestType());

		assertEquals("Content was changed", body, slurp(wrapper.getInputStream()));

		assertNotNull("getReader", wrapper.getReader());
	}

	/**
	 * Test parsing of SOAP 1.1 request.
	 * @throws IOException on error
	 */
	@Test
	public void testSoap11WithoutSOAPAction() throws IOException {
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

		assertNull("Could not parse SOAP 1.1 request", wrapper.getPayloadRequestName());
		assertNull("SOAP request type unrecognized", wrapper.getPayloadRequestType());

		assertEquals("Content was changed", body, slurp(wrapper.getInputStream()));
	}

	/**
	 * Test parsing of SOAP 1.1 request with "mandatory header".
	 * @throws IOException on error
	 */
	@Test
	public void testSoap11MandatoryHeader() throws IOException {
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

		assertEquals("Could not parse SOAP 1.1 request with 'mandatory header'",
				".GetLastTradePrice", wrapper.getPayloadRequestName());
		assertEquals("SOAP request type unrecognized", "SOAP", wrapper.getPayloadRequestType());

		assertEquals("Content was changed", body, slurp(wrapper.getInputStream()));
	}

	/**
	 * Test parsing of SOAP 1.1 request with multiple request parameters.
	 * @throws IOException on error
	 */
	@Test
	public void testSoap11MultipleRequestParameters() throws IOException {
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

		assertEquals("Could not parse SOAP 1.1 request with mutliple request parameters",
				".GetLastTradePriceDetailed", wrapper.getPayloadRequestName());
		assertEquals("SOAP request type unrecognized", "SOAP", wrapper.getPayloadRequestType());

		assertEquals("Content was changed", body, slurp(wrapper.getInputStream()));
	}

	/**
	 * Test parsing of SOAP 1.2 request.
	 * @throws IOException on error
	 */
	@Test
	public void testSoap12() throws IOException {
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

		assertEquals("Could not parse SOAP 1.2 request", ".chargeReservation",
				wrapper.getPayloadRequestName());
		assertEquals("SOAP request type unrecognized", "SOAP", wrapper.getPayloadRequestType());

		assertEquals("Content was changed", body, slurp(wrapper.getInputStream()));

		assertNotNull("getReader", wrapper.getReader());
	}

	/**
	 * Test scan for child tag.
	 * @throws XMLStreamException on error
	 */
	@Test
	public void testScanForChildTag() throws XMLStreamException {
		assertTrue("Could not find child tag",
				scanForChildTag("child", "<parent><child/></parent>"));

		assertFalse("Found wrong tag", scanForChildTag("notChild", "<parent><child/></parent>"));

		//don't find descendant tags
		assertFalse("Found descendant tag",
				scanForChildTag("descendant", "<parent><child><descendant/></child></parent>"));
	}

	private boolean scanForChildTag(String tagName, String xml) throws XMLStreamException {
		final XMLInputFactory factory = XMLInputFactory.newInstance();
		final XMLStreamReader reader = factory
				.createXMLStreamReader(new ByteArrayInputStream(xml.getBytes()));

		//advance to first tag (reader starts at "begin document")
		reader.next();

		final boolean found = PayloadNameRequestWrapper.scanForChildTag(reader, tagName);

		if (found) {
			assertEquals("Found wrong tag", tagName, reader.getLocalName());
		}

		reader.close();

		return found;
	}
}
