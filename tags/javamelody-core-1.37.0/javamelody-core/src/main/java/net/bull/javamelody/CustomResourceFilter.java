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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet filter to customize resources in html reports, such as css file.<br/>
 * For example, add the following, before the monitoring filter, in the web.xml file of your webapp,
 * in order to use your own css or icons:
 * <pre>
 * &lt;filter>
 *	&lt;filter-name>customResourceFilter&lt;/filter-name>
 *	&lt;filter-class>net.bull.javamelody.CustomResourceFilter&lt;/filter-class>
 *	&lt;init-param>
 *		&lt;param-name>monitoring.css&lt;/param-name>
 *		&lt;param-value>/customMonitoring.css&lt;/param-value>
 *	&lt;/init-param>
 *	&lt;init-param>
 *		&lt;param-name>bullets/green.png&lt;/param-name>
 *		&lt;param-value>/static/bullets/red.png&lt;/param-value>
 *	&lt;/init-param>
 * &lt;/filter>
 * &lt;filter-mapping>
 *	&lt;filter-name>customResourceFilter&lt;/filter-name>
 *	&lt;url-pattern>/monitoring&lt;/url-pattern>
 * &lt;/filter-mapping>
 * </pre>
 * Then add files "customMonitoring.css" and "static/bullets/red.png" at the root of the web content in your webapp.<br/>
 * You can replace every web resource in <a href='http://code.google.com/p/javamelody/source/browse/#svn%2Ftrunk%2Fjavamelody-core%2Fsrc%2Fmain%2Fresources%2Fnet%2Fbull%2Fjavamelody%2Fresource'>
 * this directory and sub-directories</a>
 *
 * @author Emeric Vernat
 */
public class CustomResourceFilter implements Filter {
	private final Map<String, String> customResources = new HashMap<String, String>();

	/** {@inheritDoc} */
	@SuppressWarnings("unchecked")
	public void init(FilterConfig filterConfig) throws ServletException {
		final List<String> parameterNames = Collections.list(filterConfig.getInitParameterNames());
		for (final String parameterName : parameterNames) {
			customResources.put(parameterName, filterConfig.getInitParameter(parameterName));
		}
	}

	/** {@inheritDoc} */
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		final String resource = request.getParameter("resource");
		if (resource != null && customResources.get(resource) != null) {
			final HttpServletResponse httpResponse = (HttpServletResponse) response;
			httpResponse.addHeader("Cache-Control", "max-age=3600");
			httpResponse.setContentType("text/css");

			final String customResource = customResources.get(resource);
			request.getRequestDispatcher(customResource).include(request, response);
		} else {
			chain.doFilter(request, response);
		}
	}

	/** {@inheritDoc} */
	public void destroy() {
		// nothing
	}
}
