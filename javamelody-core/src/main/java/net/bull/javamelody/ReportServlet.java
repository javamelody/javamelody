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

import jakarta.servlet.ServletConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.web.HttpAuth;
import net.bull.javamelody.internal.web.MonitoringController;

/**
 * Alternative report servlet.<br/>
 * Note that this servlet is rarely needed, because the .../monitoring url of the {@link MonitoringFilter} is enough for the reports of most webapps.
 * @author petersky
 * @author Emeric Vernat
 */
public class ReportServlet extends HttpServlet {

	static final String FILTER_CONTEXT_KEY = "javamelody.filterContext";

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private ServletConfig servletConfig;

	@SuppressWarnings("all")
	private transient HttpAuth httpAuth;

	/** {@inheritDoc} */
	@Override
	public void init(ServletConfig config) {
		this.servletConfig = config;
		httpAuth = new HttpAuth();
		LOG.debug("JavaMelody report servlet initialized");
	}

	/** {@inheritDoc} */
	@Override
	public void destroy() {
		servletConfig = null;
	}

	@Override
	protected void doGet(HttpServletRequest httpRequest, HttpServletResponse httpResponse)
			throws ServletException, IOException {
		final FilterContext filterContext = (FilterContext) servletConfig.getServletContext()
				.getAttribute(FILTER_CONTEXT_KEY);
		assert filterContext != null;

		if (!httpAuth.isAllowed(httpRequest, httpResponse)) {
			return;
		}
		final Collector collector = filterContext.getCollector();
		final MonitoringController monitoringController = new MonitoringController(collector, null);
		try {
			monitoringController.doActionIfNeededAndReport(httpRequest, httpResponse,
					servletConfig.getServletContext());
		} catch (final Exception e) {
			LOG.warn(e.toString(), e);
			httpResponse.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Internal server error");
			return;
		}

		// Ici ReportServlet comme MonitoringFilter#doMonitoring
		// par exemple pour serveur de collecte branché sur le management endpoint de Spring Boot (#1121).
		if ("stop".equalsIgnoreCase(HttpParameter.COLLECTOR.getParameterFrom(httpRequest))) {
			// on a été appelé par un serveur de collecte qui fera l'aggrégation dans le temps,
			// le stockage et les courbes, donc on arrête le timer s'il est démarré
			// et on vide les stats pour que le serveur de collecte ne récupère que les deltas
			for (final Counter counter : collector.getCounters()) {
				counter.clear();
			}

			if (!collector.isStopped()) {
				LOG.debug(
						"Stopping the javamelody collector in this webapp, because a collector server from "
								+ httpRequest.getRemoteAddr()
								+ " wants to collect the data itself");
				filterContext.stopCollector();
			}
		}
	}
}
