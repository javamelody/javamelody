/*
 * Copyright 2008-2010 by Emeric Vernat
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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;

import org.junit.Before;
import org.junit.Test;

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
		sessionListener.contextDestroyed(null);
	}

	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
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
		expect(servletContext.getServerInfo()).andReturn("Mock");
		ServletContextEvent servletContextEvent = new ServletContextEvent(servletContext);
		replay(servletContext);
		sessionListener.contextInitialized(servletContextEvent);
		sessionListener.contextDestroyed(servletContextEvent);
		verify(servletContext);

		Utils.setProperty(Parameter.NO_DATABASE, "true");
		servletContext = createNiceMock(ServletContext.class);
		expect(servletContext.getServerInfo()).andReturn("Mock");
		servletContextEvent = new ServletContextEvent(servletContext);
		replay(servletContext);
		sessionListener.contextInitialized(servletContextEvent);
		sessionListener.contextDestroyed(servletContextEvent);
		verify(servletContext);
	}

	/** Test. */
	@Test
	public void testContextDestroyed() {
		sessionListener.sessionCreated(createSessionEvent());
		sessionListener.contextDestroyed(null);
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
		sessionListener.sessionCreated(createSessionEvent());
		if (SessionListener.getSessionCount() != 1) {
			fail("sessionCreated");
		}
		if (SessionListener.getAllSessionsInformations().isEmpty()) {
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
	public void testToString() {
		final String string = sessionListener.toString();
		assertNotNull("toString not null", string);
		assertFalse("toString not empty", string.isEmpty());
	}
}
