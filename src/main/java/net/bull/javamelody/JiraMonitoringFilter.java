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

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 * Filter of monitoring JavaMelody for JIRA/Bamboo/Confluence with security check for system administrator.
 * @author Emeric Vernat
 */
public class JiraMonitoringFilter extends PluginMonitoringFilter {
	private static final boolean PLUGIN_AUTHENTICATION_DISABLED = Boolean.parseBoolean(System
			.getProperty("javamelody.plugin-authentication-disabled"));
	// valeur de com.atlassian.jira.security.Permissions.SYSTEM_ADMIN
	private static final int SYSTEM_ADMIN = 44;
	// valeur de DefaultAuthenticator.LOGGED_IN_KEY
	private static final String LOGGED_IN_KEY = "seraph_defaultauthenticator_user";
	private static Class<?> jiraUserClass;
	// initialisation ici et non dans la méthode init, car on ne sait pas très bien
	// quand la méthode init serait appelée dans les systèmes de plugins
	private final boolean jira = isJira();
	private final boolean confluence = isConfluence();
	private final boolean bamboo = isBamboo();

	/** {@inheritDoc} */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		if (!(request instanceof HttpServletRequest)) {
			super.doFilter(request, response, chain);
			return;
		}
		final HttpServletRequest httpRequest = (HttpServletRequest) request;
		final HttpServletResponse httpResponse = (HttpServletResponse) response;

		if (httpRequest.getRequestURI().equals(getMonitoringUrl(httpRequest))
				&& hasNotPermission(httpRequest, httpResponse)) {
			return;
		}

		super.doFilter(request, response, chain);
	}

	private boolean hasNotPermission(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse) throws IOException {
		return !PLUGIN_AUTHENTICATION_DISABLED
				&& (jira && !checkJiraAdminPermission(httpRequest, httpResponse) || confluence
						&& !checkConfluenceAdminPermission(httpRequest, httpResponse) || bamboo
						&& !checkBambooAdminPermission(httpRequest, httpResponse));
	}

	private boolean checkJiraAdminPermission(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse) throws IOException {
		// only the administrator can view the monitoring report
		final Object user = getUser(httpRequest);
		if (user == null) {
			// si non authentifié, on redirige vers la page de login en indiquant la page
			// d'origine (sans le contexte) à afficher après le login
			final String destination = getMonitoringUrl(httpRequest).substring(
					httpRequest.getContextPath().length());
			httpResponse.sendRedirect("login.jsp?os_destination=" + destination);
			return false;
		}
		if (!hasJiraSystemAdminPermission(user)) {
			// si authentifié mais sans la permission system admin, alors Forbidden
			httpResponse.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return false;
		}
		return true;
	}

	private boolean checkConfluenceAdminPermission(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse) throws IOException {
		// only the administrator can view the monitoring report
		final Object user = getUser(httpRequest);
		if (user == null) {
			// si non authentifié, on redirige vers la page de login en indiquant la page
			// d'origine (sans le contexte) à afficher après le login
			final String destination = getMonitoringUrl(httpRequest).substring(
					httpRequest.getContextPath().length());
			httpResponse.sendRedirect("login.action?os_destination=" + destination);
			return false;
		}
		if (!hasConfluenceAdminPermission(user)) {
			// si authentifié mais sans la permission system admin, alors Forbidden
			httpResponse.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return false;
		}
		return true;
	}

	private boolean checkBambooAdminPermission(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse) throws IOException {
		// only the administrator can view the monitoring report
		final Object user = getUser(httpRequest);
		if (user == null) {
			// si non authentifié, on redirige vers la page de login en indiquant la page
			// d'origine (sans le contexte) à afficher après le login
			final String destination = getMonitoringUrl(httpRequest).substring(
					httpRequest.getContextPath().length());
			httpResponse.sendRedirect("userlogin!default.action?os_destination=" + destination);
			return false;
		}
		if (!hasBambooAdminPermission(user)) {
			// si authentifié mais sans la permission admin, alors Forbidden
			httpResponse.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return false;
		}
		return true;
	}

	private static boolean hasJiraSystemAdminPermission(Object user) {
		try {
			final Class<?> managerFactoryClass = Class.forName("com.atlassian.jira.ManagerFactory");
			final Class<?> userClass = getJiraUserClass();
			// on travaille par réflexion car la compilation normale introduirait une dépendance
			// trop compliquée et trop lourde à télécharger pour maven
			final Object permissionManager = managerFactoryClass.getMethod("getPermissionManager")
					.invoke(null);
			final Boolean result = (Boolean) permissionManager.getClass()
					.getMethod("hasPermission", Integer.TYPE, userClass)
					.invoke(permissionManager, SYSTEM_ADMIN, user);
			return result;
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		//		return user != null
		//				&& com.atlassian.jira.ManagerFactory.getPermissionManager().hasPermission(
		//						SYSTEM_ADMIN, (com.opensymphony.user.User) user);
	}

	private static synchronized Class<?> getJiraUserClass() throws ClassNotFoundException { // NOPMD
		if (jiraUserClass == null) {
			try {
				// before JIRA 5:
				jiraUserClass = Class.forName("com.opensymphony.user.User");
			} catch (final ClassNotFoundException e) {
				// since JIRA 5:
				jiraUserClass = Class.forName("com.atlassian.crowd.embedded.api.User");
			}
		}
		return jiraUserClass;
	}

	private static boolean hasConfluenceAdminPermission(Object user) {
		try {
			final Class<?> containerManagerClass = Class
					.forName("com.atlassian.spring.container.ContainerManager");
			final Class<?> userClass = Class.forName("com.atlassian.user.User");
			// on travaille par réflexion car la compilation normale introduirait une dépendance
			// trop compliquée et trop lourde à télécharger pour maven
			final Object permissionManager = containerManagerClass.getMethod("getComponent",
					String.class).invoke(null, "permissionManager");
			final Boolean result = (Boolean) permissionManager.getClass()
					.getMethod("isConfluenceAdministrator", userClass)
					.invoke(permissionManager, user);
			return result;
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		//		return user != null
		//				&& com.atlassian.spring.container.ContainerManager.getComponent("permissionManager").
		//					isConfluenceAdministrator((com.opensymphony.user.User) user);
	}

	private static boolean hasBambooAdminPermission(Object user) {
		try {
			final Class<?> containerManagerClass = Class
					.forName("com.atlassian.spring.container.ContainerManager");
			// on travaille par réflexion car la compilation normale introduirait une dépendance
			// trop compliquée et trop lourde à télécharger pour maven
			final Object bambooPermissionManager = containerManagerClass.getMethod("getComponent",
					String.class).invoke(null, "bambooPermissionManager");
			final Class<?> globalApplicationSecureObjectClass = Class
					.forName("com.atlassian.bamboo.security.GlobalApplicationSecureObject");
			final Object globalApplicationSecureObject = globalApplicationSecureObjectClass
					.getField("INSTANCE").get(null);
			final Boolean result = (Boolean) bambooPermissionManager
					.getClass()
					.getMethod("hasPermission", String.class, String.class, Object.class)
					.invoke(bambooPermissionManager, user.toString(), "ADMIN",
							globalApplicationSecureObject);
			return result;
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		//		return user != null
		//				&& com.atlassian.spring.container.ContainerManager.getComponent("bambooPermissionManager").
		//					hasPermission(username, "ADMIN", GlobalApplicationSecureObject.INSTANCE);
	}

	private static Object getUser(HttpServletRequest httpRequest) {
		// ceci fonctionne dans JIRA et dans Confluence (et Bamboo ?)
		final HttpSession session = httpRequest.getSession(false);
		if (session == null) {
			return null;
		}
		return session.getAttribute(LOGGED_IN_KEY);
	}

	private static boolean isJira() {
		try {
			Class.forName("com.atlassian.jira.ManagerFactory");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	private static boolean isConfluence() {
		try {
			Class.forName("com.atlassian.confluence.security.PermissionManager");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	private static boolean isBamboo() {
		try {
			Class.forName("com.atlassian.bamboo.security.BambooPermissionManager");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}
}
