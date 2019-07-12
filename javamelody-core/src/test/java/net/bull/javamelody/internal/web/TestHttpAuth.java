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
package net.bull.javamelody.internal.web;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.model.Base64Coder;

/**
 * Test unitaire de la classe HttpAuth.
 * @author Emeric Vernat
 */
public class TestHttpAuth {
	private static final String USER_PWD = "user:pwd";
	private static final String REMOTE_ADDR = "127.0.0.1"; // NOPMD

	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testIsAllowed() throws IOException {
		assertTrue("no auth", isAllowed());
		setProperty(Parameter.ALLOWED_ADDR_PATTERN, REMOTE_ADDR);
		assertTrue("addr pattern", isAllowed());
		setProperty(Parameter.ALLOWED_ADDR_PATTERN, "none");
		assertFalse("addr pattern", isAllowed());
		setProperty(Parameter.ALLOWED_ADDR_PATTERN, null);
		setProperty(Parameter.AUTHORIZED_USERS, USER_PWD);
		assertFalse("authorized users", isAllowed(null));
		assertFalse("authorized users", isAllowed("not BASIC"));
		assertTrue("authorized users", isAllowed("BASIC " + Base64Coder.encodeString(USER_PWD)));
		setProperty(Parameter.AUTHORIZED_USERS, "none");
		assertFalse("authorized users", isAllowed("BASIC " + Base64Coder.encodeString(USER_PWD)));

		// check lock
		final HttpAuth httpAuth = new HttpAuth();
		setProperty(Parameter.AUTHORIZED_USERS, USER_PWD);
		// 20 > HttpAuth.AUTH_FAILURES_MAX
		for (int i = 0; i < 20; i++) {
			assertFalse("lock",
					isAllowed(httpAuth, "BASIC " + Base64Coder.encodeString("notuser:notpwd")));
		}
		assertFalse("lock", isAllowed(httpAuth, "BASIC " + Base64Coder.encodeString(USER_PWD)));
	}

	private boolean isAllowed() throws IOException {
		return isAllowed(null);
	}

	private boolean isAllowed(String authorization) throws IOException {
		return isAllowed(new HttpAuth(), authorization);
	}

	private boolean isAllowed(HttpAuth httpAuth, String authorization) throws IOException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		expect(request.getRemoteAddr()).andReturn(REMOTE_ADDR).anyTimes();
		expect(request.getHeader("Authorization")).andReturn(authorization).anyTimes();

		replay(request);
		replay(response);
		final boolean result = httpAuth.isAllowed(request, response);
		verify(request);
		verify(response);
		return result;
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
