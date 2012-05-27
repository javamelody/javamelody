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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionActivationListener;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

/**
 * Listener de session http pour le monitoring.
 * C'est la classe de ce listener qui doit être déclarée dans le fichier web.xml de la webapp.
 * Ce listener fait également listener de contexte de servlet
 * et listener de passivation/activation de sessions.
 * @author Emeric Vernat
 */
public class SessionListener implements HttpSessionListener, HttpSessionActivationListener,
		ServletContextListener, Serializable {
	private static final String SESSION_ACTIVATION_KEY = "javamelody.sessionActivation";
	private static final long serialVersionUID = -1624944319058843901L;
	// au lieu d'utiliser un int avec des synchronized partout, on utilise un AtomicInteger
	private static final AtomicInteger SESSION_COUNT = new AtomicInteger();

	@SuppressWarnings("all")
	private static final List<String> CONTEXT_PATHS = new ArrayList<String>();

	// attention : this est mis en session, cette map doit donc restée statique
	@SuppressWarnings("all")
	private static final ConcurrentMap<String, HttpSession> SESSION_MAP_BY_ID = new ConcurrentHashMap<String, HttpSession>();

	private static boolean instanceCreated;

	private boolean instanceEnabled;

	static final class SessionInformationsComparator implements Comparator<SessionInformations>,
			Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		@Override
		public int compare(SessionInformations session1, SessionInformations session2) {
			if (session1.getLastAccess().before(session2.getLastAccess())) {
				return 1;
			} else if (session1.getLastAccess().after(session2.getLastAccess())) {
				return -1;
			} else {
				return 0;
			}
		}
	}

	/**
	 * Constructeur.
	 */
	public SessionListener() {
		super();
		if (instanceCreated) {
			// ce listener a déjà été chargé précédemment et est chargé une 2ème fois donc on désactive cette 2ème instance
			// (cela peut arriver par exemple dans glassfish v3 lorsque le listener est déclaré dans le fichier web.xml
			// et déclaré par ailleurs dans le fichier web-fragment.xml à l'intérieur du jar)
			// mais il peut être réactivé dans contextInitialized (issue 193)
			instanceEnabled = false;
		} else {
			instanceEnabled = true;
			setInstanceCreated(true);
		}
	}

	/**
	 * Constructeur.
	 * @param instanceEnabled boolean
	 */
	public SessionListener(boolean instanceEnabled) {
		super();
		this.instanceEnabled = instanceEnabled;
		setInstanceCreated(true);
	}

	private static void setInstanceCreated(boolean newInstanceCreated) {
		instanceCreated = newInstanceCreated;
	}

	static int getSessionCount() {
		if (!instanceCreated) {
			return -1;
		}
		// nous pourrions nous contenter d'utiliser SESSION_MAP_BY_ID.size()
		// mais on se contente de SESSION_COUNT qui est suffisant pour avoir cette valeur
		// (SESSION_MAP_BY_ID servira pour la fonction d'invalidateAllSessions entre autres)
		return SESSION_COUNT.get();
	}

	static long getSessionAgeSum() {
		if (!instanceCreated) {
			return -1;
		}
		final long now = System.currentTimeMillis();
		long result = 0;
		for (final HttpSession session : SESSION_MAP_BY_ID.values()) {
			try {
				result += now - session.getCreationTime();
			} catch (final Exception e) {
				// Tomcat can throw "java.lang.IllegalStateException: getCreationTime: Session already invalidated"
				continue;
			}
		}
		return result;
	}

	static void invalidateAllSessions() {
		for (final HttpSession session : SESSION_MAP_BY_ID.values()) {
			try {
				session.invalidate();
			} catch (final Exception e) {
				// Tomcat can throw "java.lang.IllegalStateException: getLastAccessedTime: Session already invalidated"
				continue;
			}
		}
	}

	static void invalidateSession(String sessionId) {
		final HttpSession session = SESSION_MAP_BY_ID.get(sessionId);
		if (session != null) {
			session.invalidate();
		}
	}

	static List<SessionInformations> getAllSessionsInformations() {
		final Collection<HttpSession> sessions = SESSION_MAP_BY_ID.values();
		final List<SessionInformations> sessionsInformations = new ArrayList<SessionInformations>(
				sessions.size());
		for (final HttpSession session : sessions) {
			try {
				sessionsInformations.add(new SessionInformations(session, false));
			} catch (final Exception e) {
				// Tomcat can throw "java.lang.IllegalStateException: getLastAccessedTime: Session already invalidated"
				continue;
			}
		}
		sortSessions(sessionsInformations);
		return Collections.unmodifiableList(sessionsInformations);
	}

	static List<SessionInformations> sortSessions(List<SessionInformations> sessionsInformations) {
		if (sessionsInformations.size() > 1) {
			Collections.sort(sessionsInformations,
					Collections.reverseOrder(new SessionInformationsComparator()));
		}
		return sessionsInformations;
	}

	static SessionInformations getSessionInformationsBySessionId(String sessionId) {
		final HttpSession session = SESSION_MAP_BY_ID.get(sessionId);
		if (session == null) {
			return null;
		}
		return new SessionInformations(session, true);
	}

	/** {@inheritDoc} */
	@Override
	public void contextInitialized(ServletContextEvent event) {
		final String contextPath = Parameters.getContextPath(event.getServletContext());
		if (!instanceEnabled) {
			if (!CONTEXT_PATHS.contains(contextPath)) {
				// si jars dans tomcat/lib, il y a plusieurs instances mais dans des webapps différentes (issue 193)
				instanceEnabled = true;
			} else {
				return;
			}
		}
		CONTEXT_PATHS.add(contextPath);
		// lecture de la propriété système java.io.tmpdir uniquement
		// pour lancer une java.security.AccessControlException si le SecurityManager est activé,
		// avant d'avoir une ExceptionInInitializerError pour la classe Parameters
		System.getProperty("java.io.tmpdir");

		Parameters.initialize(event.getServletContext());

		LOG.debug("JavaMelody listener init started");

		// on initialise le monitoring des DataSource jdbc même si cette initialisation
		// sera refaite dans MonitoringFilter au cas où ce listener ait été oublié dans web.xml
		final JdbcWrapper jdbcWrapper = JdbcWrapper.SINGLETON;
		jdbcWrapper.initServletContext(event.getServletContext());
		if (!Parameters.isNoDatabase()) {
			jdbcWrapper.rebindDataSources();
		}

		LOG.debug("JavaMelody listener init done");
	}

	/** {@inheritDoc} */
	@Override
	public void contextDestroyed(ServletContextEvent event) {
		// nettoyage avant le retrait de la webapp au cas où celui-ci ne suffise pas
		SESSION_MAP_BY_ID.clear();
		SESSION_COUNT.set(0);
		LOG.debug("JavaMelody listener destroy done");
	}

	// Rq : avec les sessions, on pourrait faire des statistiques sur la durée moyenne des sessions
	// (System.currentTimeMillis() - event.getSession().getCreationTime())
	// ou le délai entre deux requêtes http par utilisateur
	// (System.currentTimeMillis() - httpRequest.getSession().getLastAccessedTime())

	/** {@inheritDoc} */
	@Override
	public void sessionCreated(HttpSessionEvent event) {
		if (!instanceEnabled) {
			return;
		}
		// pour être notifié des passivations et activations, on enregistre un HttpSessionActivationListener (this)
		final HttpSession session = event.getSession();
		// Since tomcat 6.0.21, because of https://issues.apache.org/bugzilla/show_bug.cgi?id=45255
		// when tomcat authentication is used, sessionCreated is called twice for 1 session
		// and each time with different ids, then sessionDestroyed is called once.
		// So we do not count the 2nd sessionCreated event and we remove the id of the first event
		if (session.getAttribute(SESSION_ACTIVATION_KEY) == this) {
			for (final Map.Entry<String, HttpSession> entry : SESSION_MAP_BY_ID.entrySet()) {
				final String id = entry.getKey();
				final HttpSession other = entry.getValue();
				// si la map des sessions selon leurs id contient une session dont la clé
				// n'est plus égale à son id courant, alors on l'enlève de la map
				// (et elle sera remise dans la map avec son nouvel id ci-dessous)
				if (!id.equals(other.getId())) {
					SESSION_MAP_BY_ID.remove(id);
				}
			}
		} else {
			session.setAttribute(SESSION_ACTIVATION_KEY, this);

			// pour getSessionCount
			SESSION_COUNT.incrementAndGet();
		}

		// pour invalidateAllSession
		SESSION_MAP_BY_ID.put(session.getId(), session);
	}

	/** {@inheritDoc} */
	@Override
	public void sessionDestroyed(HttpSessionEvent event) {
		if (!instanceEnabled) {
			return;
		}
		final HttpSession session = event.getSession();

		// plus de removeAttribute
		// (pas nécessaire et Tomcat peut faire une exception "session already invalidated")
		//		session.removeAttribute(SESSION_ACTIVATION_KEY);

		// pour getSessionCount
		SESSION_COUNT.decrementAndGet();

		// pour invalidateAllSession
		SESSION_MAP_BY_ID.remove(session.getId());
	}

	/** {@inheritDoc} */
	@Override
	public void sessionDidActivate(HttpSessionEvent event) {
		if (!instanceEnabled) {
			return;
		}
		// pour getSessionCount
		SESSION_COUNT.incrementAndGet();

		// pour invalidateAllSession
		SESSION_MAP_BY_ID.put(event.getSession().getId(), event.getSession());
	}

	/** {@inheritDoc} */
	@Override
	public void sessionWillPassivate(HttpSessionEvent event) {
		if (!instanceEnabled) {
			return;
		}
		// pour getSessionCount
		SESSION_COUNT.decrementAndGet();

		// pour invalidateAllSession
		SESSION_MAP_BY_ID.remove(event.getSession().getId());
	}

	// pour hudson/Jenkins/jira/confluence/bamboo
	void registerSessionIfNeeded(HttpSession session) {
		if (session != null) {
			synchronized (session) {
				if (!SESSION_MAP_BY_ID.containsKey(session.getId())) {
					sessionCreated(new HttpSessionEvent(session));
				}
			}
		}
	}

	// pour hudson/Jenkins/jira/confluence/bamboo
	void unregisterSessionIfNeeded(HttpSession session) {
		if (session != null) {
			try {
				session.getCreationTime();
			} catch (final IllegalStateException e) {
				// session.getCreationTime() lance IllegalStateException si la session est invalidée
				synchronized (session) {
					sessionDestroyed(new HttpSessionEvent(session));
				}
			}
		}
	}

	// pour hudson/Jenkins/jira/confluence/bamboo
	void unregisterInvalidatedSessions() {
		for (final HttpSession session : SESSION_MAP_BY_ID.values()) {
			unregisterSessionIfNeeded(session);
		}
		// issue 198: in JIRA 4.4.*, sessionCreated is called two times with different sessionId
		// but with the same attributes in the second than the attributes added in the first,
		// so SESSION_COUNT is periodically counted again
		SESSION_COUNT.set(SESSION_MAP_BY_ID.size());
	}

	void removeAllActivationListeners() {
		for (final HttpSession session : SESSION_MAP_BY_ID.values()) {
			try {
				session.removeAttribute(SESSION_ACTIVATION_KEY);
			} catch (final Exception e) {
				// Tomcat can throw "java.lang.IllegalStateException: xxx: Session already invalidated"
				continue;
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[sessionCount=" + getSessionCount() + ']';
	}
}
