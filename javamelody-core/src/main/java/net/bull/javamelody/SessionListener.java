/*
 * Copyright 2008-2017 by Emeric Vernat
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

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.SessionInformations;

/**
 * Listener de session http ({@link HttpSessionListener}) pour le monitoring.
 * C'est la classe de ce listener qui doit être déclarée dans le fichier web.xml de la webapp.
 * Ce listener fait également listener de contexte de servlet ({@link ServletContextListener})
 * et listener de passivation/activation de sessions ({@link HttpSessionActivationListener}).
 * @author Emeric Vernat
 */
public class SessionListener implements HttpSessionListener, HttpSessionActivationListener,
		ServletContextListener, Serializable {
	public static final String CSRF_TOKEN_SESSION_NAME = "javamelody."
			+ HttpParameter.TOKEN.getName();
	public static final String SESSION_COUNTRY_KEY = "javamelody.country";
	public static final String SESSION_REMOTE_ADDR = "javamelody.remoteAddr";
	public static final String SESSION_REMOTE_USER = "javamelody.remoteUser";
	public static final String SESSION_USER_AGENT = "javamelody.userAgent";

	private static final String SESSION_ACTIVATION_KEY = "javamelody.sessionActivation";

	private static final long serialVersionUID = -1624944319058843901L;
	// au lieu d'utiliser un int avec des synchronized partout, on utilise un AtomicInteger
	private static final AtomicInteger SESSION_COUNT = new AtomicInteger();

	@SuppressWarnings("all")
	private static final List<String> CONTEXT_PATHS = new ArrayList<String>();

	// attention : this est mis en session, cette map doit donc restée statique
	@SuppressWarnings("all")
	private static final ConcurrentMap<String, HttpSession> SESSION_MAP_BY_ID = new ConcurrentHashMap<String, HttpSession>();

	private static final ThreadLocal<HttpSession> SESSION_CONTEXT = new ThreadLocal<HttpSession>();

	private static boolean instanceCreated;

	private boolean instanceEnabled;

	static final class SessionInformationsComparator
			implements Comparator<SessionInformations>, Serializable {
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

	public static int getSessionCount() {
		if (!instanceCreated) {
			return -1;
		}
		// nous pourrions nous contenter d'utiliser SESSION_MAP_BY_ID.size()
		// mais on se contente de SESSION_COUNT qui est suffisant pour avoir cette valeur
		// (SESSION_MAP_BY_ID servira pour la fonction d'invalidateAllSessions entre autres)
		return SESSION_COUNT.get();
	}

	public static long getSessionAgeSum() {
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

	// méthode conservée pour compatibilité ascendante
	// (notamment https://wiki.jenkins-ci.org/display/JENKINS/Invalidate+Jenkins+HTTP+sessions)
	static void invalidateAllSessions() {
		invalidateAllSessionsExceptCurrentSession(null);
	}

	// since 1.49
	public static void invalidateAllSessionsExceptCurrentSession(HttpSession currentSession) {
		for (final HttpSession session : SESSION_MAP_BY_ID.values()) {
			try {
				if (currentSession != null && currentSession.getId().equals(session.getId())) {
					// si l'utilisateur exécutant l'action a une session http,
					// on ne l'invalide pas
					continue;
				}
				session.invalidate();
			} catch (final Exception e) {
				// Tomcat can throw "java.lang.IllegalStateException: getLastAccessedTime: Session already invalidated"
				continue;
			}
		}
	}

	public static void invalidateSession(String sessionId) {
		final HttpSession session = getSessionById(sessionId);
		if (session != null) {
			// dans Jenkins notamment, une session invalidée peut rester un peu dans cette map
			try {
				session.invalidate();
			} catch (final Exception e) {
				// Tomcat can throw "java.lang.IllegalStateException: getLastAccessedTime: Session already invalidated"
				return;
			}
		}
	}

	private static HttpSession getSessionById(String sessionId) {
		final HttpSession session = SESSION_MAP_BY_ID.get(sessionId);
		if (session == null) {
			// In some cases (issue 473), Tomcat changes id in session withtout calling sessionCreated.
			// In servlet 3.1, HttpSessionIdListener.sessionIdChanged could be used.
			for (final HttpSession other : SESSION_MAP_BY_ID.values()) {
				if (other.getId().equals(sessionId)) {
					return other;
				}
			}
		}
		return session;
	}

	private static void removeSessionsWithChangedId() {
		for (final Map.Entry<String, HttpSession> entry : SESSION_MAP_BY_ID.entrySet()) {
			final String id = entry.getKey();
			final HttpSession other = entry.getValue();
			if (!id.equals(other.getId())) {
				SESSION_MAP_BY_ID.remove(id);
			}
		}
	}

	private static void addSession(final HttpSession session) {
		SESSION_MAP_BY_ID.put(session.getId(), session);
	}

	private static void removeSession(final HttpSession session) {
		final HttpSession removedSession = SESSION_MAP_BY_ID.remove(session.getId());
		if (removedSession == null) {
			// In some cases (issue 473), Tomcat changes id in session withtout calling sessionCreated.
			// In servlet 3.1, HttpSessionIdListener.sessionIdChanged could be used.
			removeSessionsWithChangedId();
		}
	}

	public static List<SessionInformations> getAllSessionsInformations() {
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

	public static List<SessionInformations> sortSessions(
			List<SessionInformations> sessionsInformations) {
		if (sessionsInformations.size() > 1) {
			Collections.sort(sessionsInformations,
					Collections.reverseOrder(new SessionInformationsComparator()));
		}
		return sessionsInformations;
	}

	public static SessionInformations getSessionInformationsBySessionId(String sessionId) {
		final HttpSession session = getSessionById(sessionId);
		if (session == null) {
			return null;
		}
		// dans Jenkins notamment, une session invalidée peut rester un peu dans cette map
		try {
			return new SessionInformations(session, true);
		} catch (final Exception e) {
			// Tomcat can throw "java.lang.IllegalStateException: getLastAccessedTime: Session already invalidated"
			return null;
		}
	}

	/**
	 * Définit la session http pour le thread courant.
	 * @param session HttpSession
	 */
	public static void bindSession(HttpSession session) {
		if (session != null) {
			SESSION_CONTEXT.set(session);
		}
	}

	/**
	 * Retourne la session pour le thread courant ou null.
	 * @return HttpSession
	 */
	public static HttpSession getCurrentSession() {
		return SESSION_CONTEXT.get();
	}

	/**
	 * Enlève le lien entre la session et le thread courant.
	 */
	public static void unbindSession() {
		SESSION_CONTEXT.remove();
	}

	/** {@inheritDoc} */
	@Override
	public void contextInitialized(ServletContextEvent event) {
		final long start = System.currentTimeMillis(); // NOPMD
		// lecture de la propriété système java.io.tmpdir uniquement
		// pour lancer une java.security.AccessControlException si le SecurityManager est activé,
		// avant d'avoir une ExceptionInInitializerError pour la classe Parameters
		System.getProperty("java.io.tmpdir");

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

		Parameters.initialize(event.getServletContext());

		LOG.debug("JavaMelody listener init started");

		// on initialise le monitoring des DataSource jdbc même si cette initialisation
		// sera refaite dans MonitoringFilter au cas où ce listener ait été oublié dans web.xml
		final JdbcWrapper jdbcWrapper = JdbcWrapper.SINGLETON;
		jdbcWrapper.initServletContext(event.getServletContext());
		if (!Parameters.isNoDatabase()) {
			jdbcWrapper.rebindDataSources();
		}

		final long duration = System.currentTimeMillis() - start;
		LOG.debug("JavaMelody listener init done in " + duration + " ms");
	}

	/** {@inheritDoc} */
	@Override
	public void contextDestroyed(ServletContextEvent event) {
		if (!instanceEnabled) {
			return;
		}
		// nettoyage avant le retrait de la webapp au cas où celui-ci ne suffise pas
		SESSION_MAP_BY_ID.clear();
		SESSION_COUNT.set(0);

		// issue 665: in WildFly 10.1.0, the MonitoringFilter may never be initialized neither destroyed.
		// For this case, it is needed to stop here the JdbcWrapper initialized in contextInitialized
		JdbcWrapper.SINGLETON.stop();

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
			// si la map des sessions selon leurs id contient une session dont la clé
			// n'est plus égale à son id courant, alors on l'enlève de la map
			// (et elle sera remise dans la map avec son nouvel id ci-dessous)
			removeSessionsWithChangedId();
		} else {
			session.setAttribute(SESSION_ACTIVATION_KEY, this);

			// pour getSessionCount
			SESSION_COUNT.incrementAndGet();
		}

		// pour invalidateAllSession
		addSession(session);
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
		removeSession(session);
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
		addSession(event.getSession());
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
		removeSession(event.getSession());
	}

	// pour Jenkins/jira/confluence/bamboo
	void registerSessionIfNeeded(HttpSession session) {
		if (session != null) {
			synchronized (session) {
				if (!SESSION_MAP_BY_ID.containsKey(session.getId())) {
					sessionCreated(new HttpSessionEvent(session));
				}
			}
		}
	}

	// pour Jenkins/jira/confluence/bamboo
	void unregisterSessionIfNeeded(HttpSession session) {
		if (session != null) {
			try {
				session.getCreationTime();

				// https://issues.jenkins-ci.org/browse/JENKINS-20532
				// https://bugs.eclipse.org/bugs/show_bug.cgi?id=413019
				session.getLastAccessedTime();
			} catch (final IllegalStateException e) {
				// session.getCreationTime() lance IllegalStateException si la session est invalidée
				synchronized (session) {
					sessionDestroyed(new HttpSessionEvent(session));
				}
			}
		}
	}

	// pour Jenkins/jira/confluence/bamboo
	void unregisterInvalidatedSessions() {
		for (final Map.Entry<String, HttpSession> entry : SESSION_MAP_BY_ID.entrySet()) {
			final HttpSession session = entry.getValue();
			if (session.getId() != null) {
				unregisterSessionIfNeeded(session);
			} else {
				// damned JIRA has sessions with null id, when shuting down
				final String sessionId = entry.getKey();
				SESSION_MAP_BY_ID.remove(sessionId);
			}
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
