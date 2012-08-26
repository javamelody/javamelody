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
