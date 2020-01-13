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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.internal.model.SessionInformations;

/**
 * Test unitaire de la classe SessionListener.
 * @author Emeric Vernat
 */
public class TestSessionListener {
	private SessionListener sessionListener;

	private HttpSession createSession() {
		return new SessionTestImpl(true);
	}

	private HttpSessionEvent createSessionEvent(String id, boolean serializable, long lastAccess) {
		return new HttpSessionEvent(new SessionTestImpl(id, serializable, lastAccess));
	}

	private HttpSessionEvent createSessionEvent() {
		return new HttpSessionEvent(createSession());
	}

	private void clearSessions() {
		final ServletContext servletContext = createNiceMock(ServletContext.class);
		final ServletContextEvent servletContextEvent = new ServletContextEvent(servletContext);
		replay(servletContext);
		sessionListener.contextDestroyed(servletContextEvent);
		verify(servletContext);
	}

	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
		try {
			final Field field = SessionListener.class.getDeclaredField("instanceCreated");
			field.setAccessible(true);
			field.set(null, false);
		} catch (final IllegalAccessException e) {
			throw new IllegalStateException(e);
		} catch (final NoSuchFieldException e) {
			throw new IllegalStateException(e);
		}
		sessionListener = new SessionListener();
		clearSessions();
	}

	/** Test. */
	@Test
	public void testGetSessionCount() {
		sessionListener.sessionCreated(createSessionEvent());
		if (SessionListener.getSessionCount() != 1) {
			fail("getSessionCount");
		}
	}

	/** Test. */
	@Test
	public void testGetSessionAgeSum() {
		sessionListener.sessionCreated(createSessionEvent());
		if (SessionListener.getSessionAgeSum() < 0) {
			fail("getSessionAgeSum");
		}
	}

	/** Test. */
	@Test
	public void testInvalidateAllSessions() {
		final SessionTestImpl session = new SessionTestImpl(true);
		sessionListener.sessionCreated(new HttpSessionEvent(session));
		SessionListener.invalidateAllSessions();
		if (!session.isInvalidated()) {
			fail("invalidateAllSessions");
		}
	}

	/** Test. */
	@Test
	public void testInvalidateSession() {
		final SessionTestImpl session = new SessionTestImpl(true);
		sessionListener.sessionCreated(new HttpSessionEvent(session));
		SessionListener.invalidateSession(session.getId());
		if (!session.isInvalidated()) {
			fail("invalidateAllSessions");
		}
	}

	/** Test. */
	@Test
	public void testGetAllSessionsInformations() {
		final long now = System.currentTimeMillis();
		sessionListener.sessionCreated(createSessionEvent("1", true, now));
		sessionListener.sessionCreated(createSessionEvent("2", true, now + 2));
		sessionListener.sessionCreated(createSessionEvent("3", true, now));
		sessionListener.sessionCreated(createSessionEvent("4", true, now - 2));
		sessionListener.sessionCreated(createSessionEvent("5", true, now));
		if (SessionListener.getAllSessionsInformations().size() != 5) {
			fail("getAllSessions");
		}
	}

	/** Test. */
	@Test
	public void testGetSessionInformationsBySessionId() {
		final HttpSessionEvent sessionEvent = createSessionEvent();
		sessionListener.sessionCreated(sessionEvent);
		final SessionInformations sessionInformations = SessionListener
				.getSessionInformationsBySessionId(sessionEvent.getSession().getId());
		assertEquals("getSessionInformationsBySessionId", sessionEvent.getSession().getId(),
				sessionInformations.getId());
		assertNull("getSessionInformationsBySessionId",
				SessionListener.getSessionInformationsBySessionId("n'importe quoi"));
	}

	/** Test. */
	@Test
	public void testContextInitialized() {
		ServletContext servletContext = createNiceMock(ServletContext.class);
		expect(servletContext.getServerInfo()).andReturn("Mock").anyTimes();
		expect(servletContext.getMajorVersion()).andReturn(2).anyTimes();
		expect(servletContext.getMinorVersion()).andReturn(5).anyTimes();
		ServletContextEvent servletContextEvent = new ServletContextEvent(servletContext);
		replay(servletContext);
		sessionListener.contextInitialized(servletContextEvent);
		sessionListener.contextDestroyed(servletContextEvent);
		verify(servletContext);

		Utils.setProperty(Parameter.NO_DATABASE, "true");
		servletContext = createNiceMock(ServletContext.class);
		expect(servletContext.getServerInfo()).andReturn("Mock").anyTimes();
		expect(servletContext.getMajorVersion()).andReturn(2).anyTimes();
		expect(servletContext.getMinorVersion()).andReturn(5).anyTimes();
		servletContextEvent = new ServletContextEvent(servletContext);
		replay(servletContext);
		sessionListener.contextInitialized(servletContextEvent);
		sessionListener.contextDestroyed(servletContextEvent);
		verify(servletContext);
	}

	/** Test. */
	@Test
	public void testContextDestroyed() {
		final ServletContext servletContext = createNiceMock(ServletContext.class);
		final ServletContextEvent servletContextEvent = new ServletContextEvent(servletContext);
		replay(servletContext);
		sessionListener.sessionCreated(createSessionEvent());
		sessionListener.contextDestroyed(servletContextEvent);
		verify(servletContext);
		if (!SessionListener.getAllSessionsInformations().isEmpty()) {
			fail("contextDestroyed");
		}
		if (SessionListener.getSessionCount() != 0) {
			fail("contextDestroyed");
		}
	}

	/** Test. */
	@Test
	public void testSessionCreated() {
		final HttpSessionEvent sessionEvent = createSessionEvent();
		sessionListener.sessionCreated(sessionEvent);
		if (SessionListener.getSessionCount() != 1) {
			fail("sessionCreated");
		}
		if (SessionListener.getAllSessionsInformations().isEmpty()) {
			fail("sessionCreated");
		}
		sessionListener.sessionCreated(sessionEvent);
		if (SessionListener.getSessionCount() != 1) {
			fail("sessionCreated");
		}
	}

	/** Test. */
	@Test
	public void testSessionDestroyed() {
		sessionListener.sessionCreated(createSessionEvent());
		sessionListener.sessionDestroyed(createSessionEvent());
		if (SessionListener.getSessionCount() != 0) {
			fail("sessionDestroyed");
		}
		if (!SessionListener.getAllSessionsInformations().isEmpty()) {
			fail("sessionDestroyed");
		}
	}

	/** Test. */
	@Test
	public void testSessionDidActivate() {
		sessionListener.sessionDidActivate(createSessionEvent());
		if (SessionListener.getSessionCount() != 1) {
			fail("sessionDidActivate");
		}
		if (SessionListener.getAllSessionsInformations().isEmpty()) {
			fail("sessionDidActivate");
		}
	}

	/** Test. */
	@Test
	public void testSessionWillPassivate() {
		sessionListener.sessionDidActivate(createSessionEvent());
		sessionListener.sessionWillPassivate(createSessionEvent());
		if (SessionListener.getSessionCount() != 0) {
			fail("sessionWillPassivate");
		}
		if (!SessionListener.getAllSessionsInformations().isEmpty()) {
			fail("sessionWillPassivate");
		}
	}

	/** Test. */
	@Test
	public void testWithInstanceDisabled() {
		final SessionListener sessionListener1 = new SessionListener();
		final SessionListener sessionListener2 = new SessionListener();
		final ServletContext servletContext = createNiceMock(ServletContext.class);
		final ServletContextEvent servletContextEvent = new ServletContextEvent(servletContext);
		expect(servletContext.getContextPath()).andReturn("/test").anyTimes();
		expect(servletContext.getServerInfo()).andReturn("Glassfish").anyTimes();
		expect(servletContext.getMajorVersion()).andReturn(2).anyTimes();
		expect(servletContext.getMinorVersion()).andReturn(5).anyTimes();
		replay(servletContext);
		sessionListener1.contextInitialized(servletContextEvent);
		sessionListener2.contextInitialized(servletContextEvent);
		sessionListener2.sessionCreated(null);
		sessionListener2.sessionWillPassivate(null);
		sessionListener2.sessionDidActivate(null);
		sessionListener2.sessionDestroyed(null);
		sessionListener2.contextDestroyed(servletContextEvent);
		verify(servletContext);
	}

	/** Test. */
	@Test
	public void registerSessionIfNeeded() {
		final HttpSession session = createSession();
		sessionListener.registerSessionIfNeeded(session);
		sessionListener.registerSessionIfNeeded(session);
		sessionListener.registerSessionIfNeeded(null);

		sessionListener.unregisterInvalidatedSessions();

		sessionListener.unregisterSessionIfNeeded(session);
		sessionListener.unregisterSessionIfNeeded(session);
		sessionListener.unregisterSessionIfNeeded(null);
	}

	/** Test. */
	@Test
	public void testToString() {
		final String string = sessionListener.toString();
		assertNotNull("toString not null", string);
		assertFalse("toString not empty", string.isEmpty());
	}
}
