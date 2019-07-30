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

import net.bull.javamelody.internal.web.MonitoringController;

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
 * You can replace every web resource in <a href='https://github.com/javamelody/javamelody/tree/master/javamelody-core/src/main/resources/net/bull/javamelody/resource'>
 * this directory and sub-directories</a>
 *
 * @author Emeric Vernat
 */
public class CustomResourceFilter implements Filter {
	private final Map<String, String> customResources = new HashMap<String, String>();

	/** {@inheritDoc} */
	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
		final List<String> parameterNames = Collections.list(filterConfig.getInitParameterNames());
		for (final String parameterName : parameterNames) {
			customResources.put(parameterName, filterConfig.getInitParameter(parameterName));
		}
	}

	/** {@inheritDoc} */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		final String resource = request.getParameter("resource");
		if (resource != null && customResources.get(resource) != null) {
			final String customResource = customResources.get(resource);

			final HttpServletResponse httpResponse = (HttpServletResponse) response;
			MonitoringController.addHeadersForResource(httpResponse, customResource);

			if (customResources.get("useForward") == null) {
				request.getRequestDispatcher(customResource).include(request, response);
			} else {
				request.getRequestDispatcher(customResource).forward(request, response);
			}
		} else {
			chain.doFilter(request, response);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void destroy() {
		// nothing
	}
}
