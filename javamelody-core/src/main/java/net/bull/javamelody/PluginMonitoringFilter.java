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

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Abstract Filter of monitoring JavaMelody for Jenkins/JIRA/Bamboo/Confluence with emulated {@link SessionListener}.
 * @author Emeric Vernat
 */
public abstract class PluginMonitoringFilter extends MonitoringFilter {
	private final SessionListener emulatedSessionListener = new SessionListener(true);

	/** {@inheritDoc} */
	@Override
	public void init(FilterConfig config) throws ServletException {
		super.init(config);

		final FilterContext filterContext = getFilterContext();
		if (filterContext != null) {
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
			final Timer timer = filterContext.getTimer();
			timer.schedule(sessionTimerTask, periodMillis - 5 * 1000, periodMillis);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void destroy() {
		// évitons ClassNotFoundException: net.bull.javamelody.SessionListener,
		// au redémarrage de Tomcat, cf 8344 dans bugs Jenkins:
		// http://issues.jenkins-ci.org/browse/JENKINS-8344
		emulatedSessionListener.removeAllActivationListeners();
		super.destroy();
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

	public static void logForDebug(String msg) {
		LOG.debug(msg);
	}
}
