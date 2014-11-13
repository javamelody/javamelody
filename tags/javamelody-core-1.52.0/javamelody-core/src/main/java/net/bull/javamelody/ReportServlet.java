/*
 * Copyright 2008-2014 by Emeric Vernat
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
import java.util.regex.Pattern;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Alternative report servlet.<br/>
 * Note that this servlet is rarely needed, because the .../monitoring url of the MonitoringFilter is enough for the reports of most webapps.
 * @author petersky
 * @author Emeric Vernat
 */
public class ReportServlet extends HttpServlet {

	static final String FILTER_CONTEXT_KEY = "javamelody.filterContext";

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private ServletConfig servletConfig;

	private Pattern allowedAddrPattern;

	/** {@inheritDoc} */
	@Override
	public void init(ServletConfig config) {
		this.servletConfig = config;
		if (Parameters.getParameter(Parameter.ALLOWED_ADDR_PATTERN) != null) {
			allowedAddrPattern = Pattern.compile(Parameters
					.getParameter(Parameter.ALLOWED_ADDR_PATTERN));
		}
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
		if (isRequestNotAllowed(httpRequest)) {
			LOG.debug("Forbidden access to monitoring from " + httpRequest.getRemoteAddr());
			httpResponse.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return;
		}

		final FilterContext filterContext = (FilterContext) servletConfig.getServletContext()
				.getAttribute(FILTER_CONTEXT_KEY);
		final Collector collector = filterContext.getCollector();
		final MonitoringController monitoringController = new MonitoringController(collector, null);

		monitoringController.doActionIfNeededAndReport(httpRequest, httpResponse,
				servletConfig.getServletContext());
	}

	private boolean isRequestNotAllowed(HttpServletRequest httpRequest) {
		return allowedAddrPattern != null
				&& !allowedAddrPattern.matcher(httpRequest.getRemoteAddr()).matches();
	}
}