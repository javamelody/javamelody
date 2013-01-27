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

import javax.servlet.FilterConfig;
import javax.servlet.ServletException;

/**
 * Filter of monitoring JavaMelody for Liferay.
 * @author Emeric Vernat
 */
public class LiferayMonitoringFilter extends PluginMonitoringFilter {
	/** {@inheritDoc} */
	@Override
	public void init(FilterConfig config) throws ServletException {
		super.init(config);

		LOG.debug("JavaMelody is monitoring Liferay");
	}

	// comme pour JIRA, on pourrait essayer de vérifier les permissions :
	//	/** {@inheritDoc} */
	//	@Override
	//	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	//			throws IOException, ServletException {
	//		if (!(request instanceof HttpServletRequest)) {
	//			super.doFilter(request, response, chain);
	//			return;
	//		}
	//		final HttpServletRequest httpRequest = (HttpServletRequest) request;
	//		final HttpServletResponse httpResponse = (HttpServletResponse) response;
	//		if (httpRequest.getRequestURI().equals(getMonitoringUrl(httpRequest))
	//				&& hasNotPermission(httpRequest, httpResponse)) {
	//			return;
	//		}
	//
	//		super.doFilter(request, response, chain);
	//	}
}
