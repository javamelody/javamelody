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

import java.io.IOException;
import java.util.Timer;
import java.util.TimerTask;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 * Abstract Filter of monitoring JavaMelody for Hudson/Jenkins/JIRA/Bamboo/Confluence with emulated session listener.
 * @author Emeric Vernat
 */
public abstract class PluginMonitoringFilter extends MonitoringFilter {
	private final SessionListener emulatedSessionListener = new SessionListener();

	/** {@inheritDoc} */
	@Override
	public void init(FilterConfig config) throws ServletException {
		super.init(config);

		final TimerTask sessionTimerTask = new TimerTask() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					unregisterInvalidatedSessions();
				} catch (final Throwable t) { // NOPMD
					LOG.warn("exception while checking sessions", t);
				}
			}
		};
		final int resolutionSeconds = Parameters.getResolutionSeconds();
		final int periodMillis = resolutionSeconds * 1000;
		final Timer timer = getFilterContext().getTimer();
		timer.schedule(sessionTimerTask, periodMillis - 5 * 1000, periodMillis);
	}

	/** {@inheritDoc} */
	@Override
	public void destroy() {
		// évitons ClassNotFoundException: net.bull.javamelody.SessionListener,
		// au redémarrage de Tomcat, cf 8344 dans bugs Hudson/Jenkins:
		// http://issues.jenkins-ci.org/browse/JENKINS-8344
		emulatedSessionListener.removeAllActivationListeners();
	}

	/** {@inheritDoc} */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		if (!(request instanceof HttpServletRequest)) {
			super.doFilter(request, response, chain);
			return;
		}
		final HttpServletRequest httpRequest = (HttpServletRequest) request;

		registerSessionIfNeeded(httpRequest);
		super.doFilter(request, response, chain);
		// si logout on prend en compte de suite la destruction de la session
		unregisterSessionIfNeeded(httpRequest);
	}

	private void registerSessionIfNeeded(HttpServletRequest httpRequest) {
		// rq: cette session peut-être dors et déjà invalide et c'est pourquoi on vérifie
		// isRequestedSessionIdValid
		if (httpRequest.isRequestedSessionIdValid()) {
			final HttpSession session = httpRequest.getSession(false);
			emulatedSessionListener.registerSessionIfNeeded(session);
		}
	}

	private void unregisterSessionIfNeeded(HttpServletRequest httpRequest) {
		final HttpSession session = httpRequest.getSession(false);
		emulatedSessionListener.unregisterSessionIfNeeded(session);
	}

	void unregisterInvalidatedSessions() {
		emulatedSessionListener.unregisterInvalidatedSessions();
	}
}
